require_relative 'vm/int'
require_relative 'vm/byte_array'
require_relative 'vm/list_node'

class VM
  class CallStackTooDeep < StandardError; end
  class VariableUndefined < StandardError; end

  INSTRUCTIONS = [
    ['PUSH_NUM',     1],
    ['PUSH_STR',     1],
    ['PUSH_LIST',    0],
    ['PUSH_LOCAL',   1],
    ['PUSH_REMOTE',  1],
    ['PUSH_ARG',     0],
    ['PUSH_FUNC',    0],
    ['POP',          0],
    ['ADD',          0],
    ['SUB',          0],
    ['CMP_GT',       0],
    ['CMP_GTE',      0],
    ['CMP_LT',       0],
    ['CMP_LTE',      0],
    ['CMP_EQ',       0],
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
    ['VAR_NAMES',    1]
  ]

  INSTRUCTIONS.each_with_index do |(name, _arity), index|
    const_set(name.to_sym, index)
  end

  INT_PRINT     = 1
  INT_PRINT_VAL = 2

  attr_reader :stack, :heap, :stdout, :ip

  def initialize(instructions = [], args: [], stdout: $stdout)
    @ip = 0
    @stack = []          # operand stack
    @call_stack = []     # call frame stack
    @call_stack << { locals: {}, args: args }
    @heap = instructions # a heap "address" is an index into this array
    @labels = {}         # named labels -- a prepass over the code stores these and their associated IP
    @call_args = []      # used for next CALL
    @stdout = stdout
    @var_names = []
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
      when PUSH_NUM
        num = fetch
        push_val(Int.new(num))
      when PUSH_STR
        str = fetch
        push_val(ByteArray.new(str))
      when PUSH_LIST
        count = pop_raw
        last = nil
        address = nil
        count.times do
          arg = pop
          address = alloc
          @heap[address] = ListNode.new(arg, next_node: last, heap: @heap)
          last = address
        end
        push(address)
      when PUSH_LOCAL
        var_num = fetch
        address = locals[var_num]
        fail VariableUndefined, "#{@var_names[var_num.to_i]} is not defined" unless address
        push(address)
      when PUSH_REMOTE
        var_num = fetch
        frame_locals = @call_stack.reverse.lazy.map { |f| f[:locals] }.detect { |l| l[var_num] }
        fail VariableUndefined, "#{@var_names[var_num.to_i]} is not defined" unless frame_locals
        address = frame_locals.fetch(var_num)
        push(address)
      when PUSH_ARG
        address = args.shift
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
        result = num1 > num2 ? 1 : 0
        push_val(VM::Int.new(result))
      when CMP_GTE
        num2 = pop_val
        num1 = pop_val
        result = num1 >= num2 ? 1 : 0
        push_val(VM::Int.new(result))
      when CMP_LT
        num2 = pop_val
        num1 = pop_val
        result = num1 < num2 ? 1 : 0
        push_val(VM::Int.new(result))
      when CMP_LTE
        num2 = pop_val
        num1 = pop_val
        result = num1 <= num2 ? 1 : 0
        push_val(VM::Int.new(result))
      when CMP_EQ
        num2 = pop_val
        num1 = pop_val
        result = num1 == num2 ? 1 : 0
        push_val(VM::Int.new(result))
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
        val = pop_val
        label = fetch
        @ip = @labels.fetch(label) if val.is_a?(ByteArray) || val.raw == 1
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
      when VAR_NAMES
        @var_names = fetch.split
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
