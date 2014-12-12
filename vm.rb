class VM
  LOCALS_SIZE = 32
  STACK_SIZE = 128
  MEM_SIZE = 1024 * 1024 # 1 MiB

  # all recognized op code names (leave room for extras later)
  INSTRUCTIONS = %w(
    NOOP  PUSH   POP  DUP  PUTS _ _
    ADD   SUB    MULT DIV  _    _ _
    EQ    GT     LT   NOT  JIF  _ _
    ALLOC ASSIGN RETR _    _    _ _
    FUNC  ENDF   _    _    _    _ _
    CALL  RETURN _    _    _    _ _
  )

  INSTRUCTIONS.each_with_index do |name, index|
    const_set(name.to_sym, index) unless name.start_with?('_')
  end

  Frame = Struct.new(:locals, :ret_addr)
  Closure = Struct.new(:function_addr, :locals)

  attr_reader :stack, :sp, :frames, :fp, :heap, :free, :ip, :ret_val
  attr_accessor :stdout, :stderr

  def initialize(program=nil)
    load(program) if program
  end

  def load(program)
    @stdout = ""
    @stderr = ""
    @stack = Array.new(STACK_SIZE)          # stack for operations and function calls
    @sp = @stack.size - 1                   # stack pointer
    @frames = Array.new(STACK_SIZE)         # stack for frames (function call context)
    @fp = @frames.size - 1                  # frame pointer
    @frames[@fp] = Frame.new([])            # top-level (global) frame
    @heap = Array.new(MEM_SIZE)             # the general heap
    program.each_with_index do |val, index| # copy the program to the heap (not safe?)
      @heap[index] = val
    end
    @free = [program.size..MEM_SIZE]        # a list of free memory ranges
    @ip = 0                                 # instruction pointer
    @ret_val = nil                          # return value / if truthy, the program ends
  end

  def frame
    @frames[@fp]
  end

  def execute
    step until @ret_val
  end

  def debug_execute
    puts "\n#{'instructions'.ljust(30)} #{'stack arguments'.ljust(30)} #{'stack result'.ljust(30)} frames"
    loop do
      break if @ret_val
      @debug_instr = [INSTRUCTIONS[@heap[@ip]]]
      @debug_stack_args = []
      step(:debug)
      puts "#{@debug_instr.inspect.ljust(30)} #{@debug_stack_args.inspect.ljust(30)} #{stack.compact.inspect.ljust(30)} #{STACK_SIZE - @fp}"
    end
  end

  def step(debug=false)
    op = @heap[@ip]
    @ip += 1
    case op
    when nil
      p(ip: @ip, heap_up_to_ip: @heap[0..@ip])
      raise "trying to execute from invalid heap location"
    when NOOP
      # nothing
    when PUSH
      val = advance
      @debug_instr << val if debug
      push(val)
    when POP
      pop
    when DUP
      val = peek
      @debug_stack_args << val if debug
      push(val)
    when PUTS
      count = pop
      @debug_stack_args << count if debug
      count.times do
        value = pop.chr
        @debug_stack_args << value if debug
        @stdout << value
      end
    when ADD
      val1 = pop
      val2 = pop
      @debug_stack_args += [val1, val2] if debug
      push(val1 + val2)
    when SUB
      val1 = pop
      val2 = pop
      @debug_stack_args += [val1, val2] if debug
      push(val2 - val1)
    when MULT
      val1 = pop
      val2 = pop
      @debug_stack_args += [val1, val2] if debug
      push(val1 * val2)
    when DIV
      val1 = pop
      val2 = pop
      @debug_stack_args += [val1, val2] if debug
      push(val2 / val1)
    when EQ
      val1 = pop
      val2 = pop
      @debug_stack_args += [val1, val2] if debug
      push(val2 == val1 ? 1 : 0)
    when NOT
      val = pop
      @debug_stack_args << val if debug
      push(val == 1 ? 0 : 1)
    when JIF
      val = pop
      @debug_stack_args << val if debug
      jump_count = advance
      @debug_instr << jump_count if debug
      if val == 1
        @ip += (jump_count - 1)
      end
    when GT
      val2 = pop
      val1 = pop
      @debug_stack_args += [val1, val2] if debug
      push(val1 > val2 ? 1 : 0)
    when LT
      val2 = pop
      val1 = pop
      @debug_stack_args += [val1, val2] if debug
      push(val1 < val2 ? 1 : 0)
    when ALLOC
      index = pop
      size = pop
      @debug_stack_args += [index, size] if debug
      addr = alloc(size)
      frame.locals[index] = addr
    when ASSIGN
      index = pop
      value = pop
      @debug_stack_args += [index, value] if debug
      addr = frame.locals[index]
      @heap[addr] = value
    when RETR
      index = pop
      @debug_stack_args << index if debug
      addr = frame.locals[index]
      value = @heap[addr]
      push(value)
    when FUNC
      addr = alloc(1)
      index = pop
      @debug_stack_args << index if debug
      frame.locals[index] = addr
      heap[addr] = Closure.new(@ip, frame.locals)
      nested = 0
      loop do
        case advance
        when FUNC
          nested += 1
        when ENDF
          break if nested <= 0
          nested -= 1
        end
      end
    when CALL
      index = pop
      @debug_stack_args << index if debug
      args = []
      addr = frame.locals[index]
      closure = heap[addr]
      @fp -= 1
      @frames[@fp] = Frame.new(closure.locals.dup, @ip)
      @ip = closure.function_addr
    when RETURN
      value = pop
      @debug_stack_args << value if debug
      assert !value.nil?, 'return value cannot be nil'
      @ip = frame.ret_addr
      @frames[@fp] = nil
      @fp += 1
      if frame
        push(value)
      else
        @ret_val = value
      end
    else
      raise "unknown op: #{op}"
    end
  end

  def push(val)
    @sp -= 1
    @stack[@sp] = val
  end

  def pop
    @stack[@sp].tap do
      @stack[@sp] = nil
      @sp += 1
    end
  end

  def peek
    @stack[@sp]
  end

  def step_frame
    step until [CALL, RETURN].include?(@heap[@ip])
  end

  def advance
    @heap[@ip].tap do
      @ip += 1
    end
  end

  def alloc(size)
    (block, index) = @free.each_with_index.find { |block, _index| block.size >= size }
    raise 'not enough heap' unless block
    (start, stop) = [block.first, block.last]
    @free[index] = (start + size)..stop
    start
  end

  private

  def assert(cond, message)
    unless cond
      raise message
    end
  end
end
