require_relative 'vm/int'
require_relative 'vm/byte_array'

class VM
  INSTRUCTIONS = [
    ['PUSH_NUM',     1],
    ['PUSH_STR',     1],
    ['PUSH_LOCAL',   1],
    ['POP',          0],
    ['ADD',          0],
    ['CMP_GT',       0],
    ['CMP_GTE',      0],
    ['CMP_LT',       0],
    ['CMP_LTE',      0],
    ['DUP',          0],
    ['INT',          1],
    ['JUMP',         1],
    ['JUMP_IF_TRUE', 1],
    ['LABEL',        1],
    ['CALL',         1],
    ['RETURN',       0],
    ['SET_LOCAL',    1]
  ]

  INSTRUCTIONS.each_with_index do |(name, _arity), index|
    const_set(name.to_sym, index)
  end

  INT_PRINT     = 1
  INT_PRINT_VAL = 2

  attr_reader :stack, :heap, :stdout, :ip, :locals

  def initialize(instructions = [], stdout: $stdout)
    @ip = 0
    @instructions = instructions
    @stack = []        # operand stack
    @call_stack = []   # call frame stack -- only stores the return IP for now
    @locals = []       # local variables, stored by index -- not per frame, because scoping is handled by the compiler
    @heap = []         # a heap "address" is an index into this array
    @labels = {}       # named labels -- a prepass over the code stores these and their associated IP
    @stdout = stdout
  end

  def execute(instructions = nil, debug: false)
    @instructions = instructions if instructions
    build_labels
    @ip = 0
    while (instruction = fetch)
      puts INSTRUCTIONS[instruction][0] if debug
      case instruction
      when PUSH_NUM
        num = fetch
        push_val(Int.new(num))
      when PUSH_STR
        str = fetch
        push_val(ByteArray.new(str))
      when PUSH_LOCAL
        address = locals[fetch]
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
      when DUP
        val = peek
        push(val)
      when INT
        func = fetch
        case func
        when INT_PRINT
          val = peek
          print(val)
        when INT_PRINT_VAL
          address = peek
          val = @heap[address]
          print(val)
        end
      when JUMP
        label = fetch
        @ip = @labels[label]
      when JUMP_IF_TRUE
        val = pop_val
        label = fetch
        @ip = @labels[label] if val.is_a?(ByteArray) || val.raw == 1
      when CALL
        label = fetch
        new_ip = @labels[label]
        @call_stack.push(
          return: @ip
        )
        @ip = new_ip
      when RETURN
        @ip = @call_stack.pop[:return]
      when LABEL
        fetch # noop
      when SET_LOCAL
        index = fetch
        locals[index] = pop
      end
    end
  end

  def fetch
    instruction = @instructions[@ip]
    @ip += 1
    instruction
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
    @heap[address]
  end

  def peek
    @stack.last
  end

  def stack_values
    @stack.map do |address|
      @heap[address]
    end
  end

  def alloc
    free = @heap.each_with_index.detect { |(slot, _index)| slot.nil? }
    return free[1] if free
    @heap << nil
    @heap.size - 1
  end

  def local_values
    locals.map do |address|
      @heap[address]
    end
  end

  def print(val)
    stdout.print(val.to_s)
  end

  def build_labels
    @ip = 0
    while (instruction = fetch)
      if instruction == LABEL
        label = fetch
        @labels[label] = @ip
      else
        (_name, arity) = INSTRUCTIONS[instruction]
        arity.times { fetch } # skip args
      end
    end
  end
end
