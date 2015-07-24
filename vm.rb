require_relative 'vm/atom'
require_relative 'vm/int'
require_relative 'vm/byte_array'
require_relative 'vm/pair'
require_relative 'vm/empty_list'
require_relative 'vm/bool_true'
require_relative 'vm/bool_false'

class VM
  class CallStackTooDeep < StandardError; end
  class VariableUndefined < StandardError; end

  INSTRUCTIONS = [
    ['PUSH_ATOM',    1],
    ['PUSH_NUM',     1],
    ['PUSH_STR',     1],
    ['PUSH_TRUE',    0],
    ['PUSH_FALSE',   0],
    ['PUSH_CAR',     0],
    ['PUSH_CDR',     0],
    ['PUSH_CONS',    0],
    ['PUSH_LIST',    0],
    ['PUSH_LOCAL',   1],
    ['PUSH_REMOTE',  1],
    ['PUSH_ARG',     0],
    ['PUSH_ARGS',    0],
    ['PUSH_FUNC',    0],
    ['POP',          0],
    ['ADD',          0],
    ['SUB',          0],
    ['CMP_GT',       0],
    ['CMP_GTE',      0],
    ['CMP_LT',       0],
    ['CMP_LTE',      0],
    ['CMP_EQ',       0],
    ['CMP_NULL',     0],
    ['DUP',          0],
    ['ENDF',         0],
    ['INT',          1],
    ['JUMP',         1],
    ['JUMP_IF_TRUE', 1],
    ['LABEL',        1],
    ['CALL',         0],
    ['RETURN',       0],
    ['SET_LOCAL',    1],
    ['SET_ARGS',     0],
    ['HALT',         0],
    ['DEBUG',        0],
    ['VAR_NAMES',    1],
    ['UNQUOTE',      0]
  ]

  INSTRUCTIONS.each_with_index do |(name, _arity), index|
    const_set(name.to_sym, index)
  end

  INT_PRINT     = 1
  INT_PRINT_VAL = 2

  attr_reader :stack, :heap, :stdout, :ip

  def initialize(instructions = [], args: [], stdout: $stdout, libraries: {})
    @ip = 0
    @stack = []          # operand stack
    @call_stack = []     # call frame stack
    @call_stack << { locals: {}, args: args }
    @heap = []           # a heap "address" is an index into this array
    @labels = {}         # named labels -- a prepass over the code stores these and their associated IP
    @call_args = []      # used for next CALL
    @stdout = stdout
    libraries.each do |_name, code|
      load_code(code, execute: true)
    end
    load_code(instructions)
    @var_names = []
    @unquote = 0
  end

  def execute(instructions = nil, debug: 0)
    if instructions
      @ip = @heap.size
      @heap += instructions
    else
      @ip = 0
    end
    build_labels
    while (instruction = fetch)
      case instruction
      when PUSH_ATOM
        name = fetch
        push_val(Atom.new(name))
      when PUSH_NUM
        num = fetch
        push_val(Int.new(num))
      when PUSH_STR
        str = fetch
        push_val(ByteArray.new(str))
      when PUSH_TRUE
        push_true
      when PUSH_FALSE
        push_false
      when PUSH_CAR
        pair = pop_val
        push(pair.address)
      when PUSH_CDR
        pair = pop_val
        push(pair.next_node)
      when PUSH_CONS
        cdr = pop
        car = pop
        pair = build_pair(car, cdr)
        push_val(pair)
      when PUSH_LIST
        count = pop_raw
        address = last = empty_list
        count.times do
          arg = pop
          address = alloc
          @heap[address] = build_pair(arg, last)
          last = address
        end
        push(address)
      when PUSH_LOCAL
        var = fetch
        address = locals[var]
        fail VariableUndefined, "#{var} is not defined" unless address
        push(address)
      when PUSH_REMOTE
        var = fetch
        frame_locals = @call_stack.reverse.lazy.map { |f| f[:locals] }.detect { |l| l[var] }
        fail VariableUndefined, "#{var} is not defined" unless frame_locals
        address = frame_locals.fetch(var)
        push(address)
      when PUSH_ARG
        address = args.shift
        push(address)
      when PUSH_ARGS
        last = empty_list
        address = nil
        while arg = args.pop
          address = alloc
          @heap[address] = build_pair(arg, last)
          last = address
        end
        push(address)
      when PUSH_FUNC
        push(@ip)
        fetch_func_body # discard
      when POP
        pop
      when ADD
        num2 = pop_val
        num1 = pop_val
        push_val(num1 + num2)
      when SUB
        num2 = pop_val
        num1 = pop_val
        push_val(num1 - num2)
      when CMP_GT
        num2 = pop_val
        num1 = pop_val
        num1 > num2 ? push_true : push_false
      when CMP_GTE
        num2 = pop_val
        num1 = pop_val
        num1 >= num2 ? push_true : push_false
      when CMP_LT
        num2 = pop_val
        num1 = pop_val
        num1 < num2 ? push_true : push_false
      when CMP_LTE
        num2 = pop_val
        num1 = pop_val
        num1 <= num2 ? push_true : push_false
      when CMP_EQ
        num2 = pop_val
        num1 = pop_val
        num1 == num2 ? push_true : push_false
      when CMP_NULL
        val = pop_val
        val == EmptyList.instance ? push_true : push_false
      when DUP
        val = peek
        push(val)
      when INT
        func = fetch
        case func
        when INT_PRINT
          address = peek
          stdout_print(address)
        when INT_PRINT_VAL
          if (address = pop)
            val = resolve(address)
            stdout_print(val)
          else
            stdout_print(nil)
          end
        end
      when JUMP
        label = fetch
        @ip = @labels.fetch(label)
      when JUMP_IF_TRUE
        val = pop
        label = fetch
        @ip = @labels.fetch(label) if val != bool_false
      when CALL
        @call_stack << { return: @ip, locals: {}, args: @call_args }
        fail CallStackTooDeep, 'call stack too deep' if @call_stack.size > 1000
        @ip = pop
      when RETURN
        @ip = @call_stack.pop.fetch(:return)
      when LABEL
        fetch # noop
      when SET_LOCAL
        index = fetch
        locals[index] = pop
      when SET_ARGS
        count = pop_raw
        @call_args = (0...count).map { pop }.reverse
      when HALT
        return
      when DEBUG
        print_debug
      when UNQUOTE
        @unquote += 1
        label = "unquote_#{@unquote}".to_sym
        instr = Compiler.new(pop_val).compile(jump: label)
        new_ip = @heap.size
        @heap.concat(instr)
        @labels[label] = @ip
        @ip = new_ip
      end
      if debug > 0
        print((@ip - 1).to_s.ljust(5))
        (name, _arg_count) = INSTRUCTIONS.fetch(instruction)
        print name.ljust(15)
        case name
        when 'SET_ARGS'
          print 'args:   '
          p @call_args.each_with_object({}) { |a, h| h[a] = resolve(a) }
        when 'SET_LOCAL'
          print 'locals: '
          p locals.each_with_object({}) { |(n, a), h| h[n] = resolve(a) }
        when /^JUMP/
          print 'ip:     '
          p @ip
        else
          print 'stack:  '
          p @stack.each_with_object({}) { |a, h| h[a] = resolve(a) }
        end
      end
    end
  end

  def fetch
    instruction = @heap[@ip]
    @ip += 1
    instruction
  end

  def resolve(address)
    fail 'cannot lookup nil' if address.nil?
    @heap[address] || fail('invalid address')
  end

  def push(address)
    @stack.push(address)
  end

  def push_val(val)
    address = alloc
    @heap[address] = val
    push(address)
  end

  def bool_true
    @true_address ||= begin
      address = alloc
      @heap[address] = BoolTrue.instance
      address
    end
  end

  def push_true
    push(bool_true)
  end

  def bool_false
    @false_address ||= begin
      address = alloc
      @heap[address] = BoolFalse.instance
      address
    end
  end

  def push_false
    push(bool_false)
  end

  def empty_list
    @empty_list_address ||= begin
      address = alloc
      @heap[address] = EmptyList.instance
      address
    end
  end

  def pop
    @stack.pop
  end

  def pop_val
    address = pop
    resolve(address)
  end

  def pop_raw
    address = pop
    val = resolve(address)
    @heap[address] = nil
    val.raw
  end

  def peek
    @stack.last
  end

  def stack_values
    @stack.map do |address|
      resolve(address)
    end
  end

  def alloc
    @heap.index(nil) || @heap.size
  end

  def locals
    @call_stack.last[:locals]
  end

  def args
    @call_stack.last[:args]
  end

  def build_pair(car, cdr)
    Pair.new(car, cdr, heap: @heap)
  end

  def local_values
    locals.each_with_object({}) do |(name, address), hash|
      hash[name] = resolve(address)
    end
  end

  def stdout_print(val)
    stdout.print(val.to_s)
  end

  # TODO: does this ever need to return anything, or just burn through the function body?
  def fetch_func_body
    body = []
    while (instruction = fetch) != ENDF
      (_name, arity) = INSTRUCTIONS[instruction]
      body << instruction
      body << fetch_func_body + [ENDF] if instruction == PUSH_FUNC
      arity.times { body << fetch } # skip args
    end
    body.flatten
  end

  def load_code(instructions, execute: false)
    ip_was = @ip
    @ip = @heap.size
    @heap += instructions
    self.execute if execute
    @ip = ip_was
  end

  def build_labels
    ip_was = @ip
    while (instruction = fetch)
      if instruction == LABEL
        label = fetch
        @labels[label] = @ip
      else
        (_name, arity) = INSTRUCTIONS[instruction]
        arity.times { fetch } # skip args
      end
    end
    @ip = ip_was
  end

  def print_debug
    puts
    puts 'op stack --------------------'
    @stack.each do |address|
      puts "#{address} => #{resolve(address) rescue 'error'}"
    end
    puts
    puts 'call stack ------------------'
    @call_stack.each do |frame|
      p frame
    end
    puts
  end
end
