require_relative 'vm/int'
require_relative 'vm/byte_array'
require_relative 'vm/list_node'

class VM
  INSTRUCTIONS = [
    ['PUSH_NUM',     1],
    ['PUSH_STR',     1],
    ['PUSH_LIST',    0],
    ['PUSH_LOCAL',   1],
    ['PUSH_FUNC',   -1], # determined by ENDF
    ['PUSH_ARG',     1],
    ['POP',          0],
    ['ADD',          0],
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
    ['DEBUG',        0]
  ]

  INSTRUCTIONS.each_with_index do |(name, _arity), index|
    const_set(name.to_sym, index)
  end

  INT_PRINT     = 1
  INT_PRINT_VAL = 2

  attr_reader :stack, :heap, :stdout, :ip

  def initialize(instructions = [], stdout: $stdout)
    @ip = 0
    @stack = []          # operand stack
    @call_stack = []     # call frame stack
    @call_stack << { locals: [], args: [] }
    @heap = instructions # a heap "address" is an index into this array
    @labels = {}         # named labels -- a prepass over the code stores these and their associated IP
    @call_args = []      # used for next CALL
    @stdout = stdout
  end

  def execute(instructions = nil, debug: false)
    if instructions
      @ip = @heap.size
      @heap += instructions
    else
      @ip = 0
    end
    build_labels
    while (instruction = fetch)
      puts "#{(@ip - 1).to_s.ljust(10)} #{INSTRUCTIONS[instruction][0]}" if debug
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
        address = locals[fetch]
        push(address)
      when PUSH_FUNC
        push(@ip)
        fetch_func_body # discard
      when PUSH_ARG
        address = args[fetch]
        push(address)
      when POP
        pop
      when ADD
        num1 = pop_val
        num2 = pop_val
        push_val(num1 + num2)
      when CMP_GT
        num1 = pop_val
        num2 = pop_val
        result = num1 > num2 ? 1 : 0
        push_val(VM::Int.new(result))
      when CMP_GTE
        num1 = pop_val
        num2 = pop_val
        result = num1 >= num2 ? 1 : 0
        push_val(VM::Int.new(result))
      when CMP_LT
        num1 = pop_val
        num2 = pop_val
        result = num1 < num2 ? 1 : 0
        push_val(VM::Int.new(result))
      when CMP_LTE
        num1 = pop_val
        num2 = pop_val
        result = num1 <= num2 ? 1 : 0
        push_val(VM::Int.new(result))
      when CMP_EQ
        num1 = pop_val
        num2 = pop_val
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
          print(address)
        when INT_PRINT_VAL
          if (address = peek)
            val = resolve(address)
            print(val)
          else
            print(nil)
          end
        end
      when JUMP
        label = fetch
        @ip = @labels[label]
      when JUMP_IF_TRUE
        val = pop_val
        label = fetch
        @ip = @labels[label] if val.is_a?(ByteArray) || val.raw == 1
      when CALL
        @call_stack << { return: @ip, locals: {}, args: @call_args }
        @ip = pop
      when RETURN
        @ip = @call_stack.pop[:return]
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
    free = @heap.each_with_index.detect { |(slot, _index)| slot.nil? }
    return free[1] if free
    @heap << nil
    @heap.size - 1
  end

  def locals
    @call_stack.last[:locals]
  end

  def args
    @call_stack.last[:args]
  end

  def local_values
    locals.map do |address|
      resolve(address)
    end
  end

  def print(val)
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
  end
end
