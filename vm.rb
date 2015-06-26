require_relative 'vm/int'
require_relative 'vm/byte_array'

class VM
  INSTRUCTIONS = %w(
    PUSH_NUM
    PUSH_STR
    POP
    ADD
    CMP_GT
    CMP_GTE
    CMP_LT
    CMP_LTE
    DUP
    INT
    JUMP
    JUMP_IF_TRUE
    LABEL
    CALL
    RETURN
  )

  INSTRUCTIONS.each_with_index do |name, index|
    const_set(name.to_sym, index)
  end

  INT_PRINT     = 1
  INT_PRINT_VAL = 2

  attr_reader :stack, :heap, :stdout, :ip

  def initialize(instructions = [], stdout: $stdout)
    @ip = 0
    @instructions = instructions
    @stack = []
    @call_stack = []
    @heap = [] # simulate a heap - just store references to objects here
               # an "address" is simply an index into this array
    @labels = {}
    @stdout = stdout
  end

  def execute(instructions = nil, debug: false)
    @instructions = instructions if instructions
    build_labels
    @ip = 0
    while (instruction = fetch)
      puts INSTRUCTIONS[instruction] if debug
      case instruction
      when PUSH_NUM
        num = fetch
        push_val(Int.new(num))
      when PUSH_STR
        str = fetch
        push_val(ByteArray.new(str))
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
        @call_stack.push(@ip)
        @ip = new_ip
      when RETURN
        @ip = @call_stack.pop
      when LABEL
        fetch # noop
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

  def print(val)
    stdout.print(val.to_s)
  end

  def build_labels
    @ip = 0
    while (instruction = fetch)
      next if instruction != LABEL
      label = fetch
      @labels[label] = @ip
    end
  end
end
