class VM
  LOCALS_SIZE = 32
  STACK_SIZE = 128
  MEM_SIZE = 1024 * 1024 # 1 MiB

  PUSH   = 0
  PUTS   = 1
  ADD    = 2
  SUB    = 3
  MULT   = 4
  DIV    = 5
  EQ     = 6
  GT     = 7
  LT     = 8
  NOT    = 9
  JIF    = 10
  ALLOC  = 16
  ASSIGN = 17
  RETR   = 18
  FUNC   = 19
  ENDF   = 20
  CALL   = 21
  RETURN = 22

  TYPE_INT = 0

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
    @ret_val = nil                          # return value / if truthy, the current frame ends
  end

  def frame
    @frames[@fp]
  end

  def execute
    step until @ret_val
  end

  def step
    op = @heap[@ip]
    @ip += 1
    case op
    when nil
      p(ip: @ip, heap_up_to_ip: @heap[0..@ip])
      raise "trying to execute from invalid heap location"
    when PUSH
      val = advance
      push(val)
    when PUTS
      count = pop
      count.times do
        @stdout << pop.chr
      end
    when ADD
      val1 = pop
      val2 = pop
      push(val1 + val2)
    when SUB
      val1 = pop
      val2 = pop
      push(val2 - val1)
    when MULT
      val1 = pop
      val2 = pop
      push(val1 * val2)
    when DIV
      val1 = pop
      val2 = pop
      push(val2 / val1)
    when EQ
      val1 = pop
      val2 = pop
      push(val2 == val1 ? 1 : 0)
    when NOT
      val = pop
      push(val == 1 ? 0 : 1)
    when JIF
      val = pop
      jump_count = advance
      if val == 1
        @ip += (jump_count - 1)
      end
    when GT
      val1 = pop
      val2 = pop
      push(val1 > val2 ? 1 : 0)
    when LT
      val1 = pop
      val2 = pop
      push(val1 < val2 ? 1 : 0)
    when ALLOC
      index = pop
      size = pop
      addr = alloc(size)
      frame.locals[index] = addr
    when ASSIGN
      index = pop
      value = pop
      addr = frame.locals[index]
      @heap[addr] = value
    when RETR
      index = pop
      addr = frame.locals[index]
      value = @heap[addr]
      push(value)
    when FUNC
      addr = alloc(1)
      index = pop
      frame.locals[index] = addr
      heap[addr] = Closure.new(@ip, frame.locals)
      while advance != ENDF; end
    when CALL
      index = pop
      args = []
      addr = frame.locals[index]
      closure = heap[addr]
      @fp -= 1
      @frames[@fp] = Frame.new(closure.locals, @ip)
      @ip = closure.function_addr
    when RETURN
      value = pop
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

require 'rspec'

describe VM do

  describe 'PUSH' do
    before do
      subject.load([
        VM::PUSH, 1,
        VM::RETURN
      ])
    end

    it 'pushes a value onto the stack' do
      expect { subject.step }.to change(subject, :sp).by(-1)
      expect(subject.peek).to eq(1)
    end
  end

  describe 'PUTS' do
    context 'with a single value' do
      before do
        subject.load([
          VM::PUSH, 'x'.ord,
          VM::PUSH, 1,
          VM::PUTS,
          VM::PUSH, 0,
          VM::RETURN
        ])
      end

      it 'prints the value from the stack' do
        subject.execute
        expect(subject.stdout).to eq('x')
      end
    end

    context 'with more than one value' do
      before do
        subject.load([
          VM::PUSH, 'z'.ord,
          VM::PUSH, 'y'.ord,
          VM::PUSH, 'x'.ord,
          VM::PUSH, 3,
          VM::PUTS,
          VM::PUSH, 0,
          VM::RETURN
        ])
      end

      it 'prints the values from the stack' do
        subject.execute
        expect(subject.stdout).to eq('xyz')
      end
    end
  end

  describe 'ADD' do
    before do
      subject.load([
        VM::PUSH, 3,
        VM::PUSH, 2,
        VM::ADD,
        VM::RETURN
      ])
      subject.step_frame
    end

    it 'adds two values and pushes the result onto the stack' do
      expect(subject.peek).to eq(5)
    end
  end

  describe 'SUB' do
    before do
      subject.load([
        VM::PUSH, 3,
        VM::PUSH, 2,
        VM::SUB,
        VM::RETURN
      ])
      subject.step_frame
    end

    it 'subtracts two values and pushes the result onto the stack' do
      expect(subject.peek).to eq(1)
    end
  end

  describe 'MULT' do
    before do
      subject.load([
        VM::PUSH, 3,
        VM::PUSH, 2,
        VM::MULT,
        VM::RETURN
      ])
      subject.step_frame
    end

    it 'multiplies two values and pushes the result onto the stack' do
      expect(subject.peek).to eq(6)
    end
  end

  describe 'DIV' do
    before do
      subject.load([
        VM::PUSH, 6,
        VM::PUSH, 2,
        VM::DIV,
        VM::RETURN
      ])
      subject.step_frame
    end

    it 'divides two values and pushes the result onto the stack' do
      expect(subject.peek).to eq(3)
    end
  end

  describe 'ALLOC' do
    before do
      subject.load([
        VM::PUSH, 3,
        VM::PUSH, 0,
        VM::ALLOC,
        VM::PUSH, 0,
        VM::RETURN
      ])
      subject.step_frame
    end

    it 'allocates a block from the heap of the given size and assigns the address into the locals index given' do
      expect(subject.frame.locals[0]).to eq(8)
    end

    it 'updates the free list' do
      expect(subject.free.size).to eq(1)
      block = subject.free.first
      expect(block.first).to eq(11)
      expect(block.last).to eq(VM::MEM_SIZE)
    end
  end

  describe 'ASSIGN' do
    before do
      subject.load([
        VM::PUSH, 10,
        VM::PUSH, 0,
        VM::ASSIGN,
        VM::PUSH, 0,
        VM::RETURN
      ])
      subject.frame.locals[0] = 6
      subject.step_frame
    end

    it 'stores the top value on the stack in the address represented by the local variable index' do
      expect(subject.heap[6]).to eq(10)
    end
  end

  describe 'RETR' do
    before do
      subject.load([
        VM::PUSH, 0,
        VM::RETR,
        VM::RETURN
      ])
      subject.heap[10] = 11
      subject.frame.locals[0] = 10
      subject.step_frame
    end

    it 'retrieves the value from the address represented by the local variable index and pushes it onto the stack' do
      expect(subject.peek).to eq(11)
    end
  end

  describe 'FUNC' do
    before do
      subject.load([
        VM::PUSH, 0,
        VM::FUNC,
        VM::PUSH, 'y'.ord,
        VM::PUSH, 1,
        VM::PUTS,
        VM::PUSH, 0,
        VM::RETURN,
        VM::ENDF,

        VM::PUSH, 0,
        VM::RETURN
      ])
      subject.step_frame
    end

    it 'allocates space on the heap' do
      expect(subject.frame.locals[0]).to eq(15)
      expect(subject.free.size).to eq(1)
      block = subject.free.first
      expect(block.first).to eq(16)
      expect(block.last).to eq(VM::MEM_SIZE)
    end

    it 'stores (pointer to) the function (struct) on the heap' do
      addr = subject.frame.locals[0]
      expect(subject.heap[addr]).to be_a(VM::Closure)
    end
  end

  describe 'CALL' do
    before do
      subject.load([
        VM::PUSH, 2, # arg1
        VM::PUSH, 1, # arg count
        VM::PUSH, 1, # index of function
        VM::CALL,
        VM::RETURN
      ])
      # manually load the instructions for a function
      subject.heap[20..27] = [
        VM::PUSH, 'x'.ord,
        VM::PUSH, 1,
        VM::PUTS,
        VM::PUSH, 0,
        VM::RETURN
      ]
      subject.heap[40] = VM::Closure.new(20, subject.frame.locals)
      subject.heap[100] = 'y'.ord
      subject.frame.locals[0] = 200 # location of top-level variable
      subject.frame.locals[1] = 40  # location of closure
      # subject.frame.locals[2] = 100 # location of argument to function
    end

    it 'creates a new frame with access to the closure' do
      subject.step_frame
      expect { subject.step }.to change(subject, :fp).by(-1)
      new_frame = subject.frame
      expect(new_frame).to be
      expect(subject.stack.compact).to eq([1, 2]) # arg count
      expect(new_frame.locals.compact).to eq([200, 40])
      expect(new_frame.locals.compact).to eq([200, 40])
    end

    it 'executes the instructions' do
      subject.step_frame
      subject.step
      expect(subject.ip).to eq(20)
      expect { subject.execute }.to change(subject, :fp).by(2)
      expect(subject.stdout).to eq('x')
    end

    it 'pushes the function return value onto the top level frame stack' do
      subject.step_frame # step up to CALL
      subject.step       # step into function
      subject.step_frame # step through function
      subject.step       # step back up to parent frame
      expect(subject.peek).to eq(0)
      subject.step       # process final return
      expect(subject.ret_val).to eq(0)
    end
  end

  describe 'EQ' do
    context 'given two equal values' do
      before do
        subject.load([
          VM::PUSH, 3,
          VM::PUSH, 3,
          VM::EQ,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 1 onto the stack' do
        expect(subject.peek).to eq(1)
      end
    end

    context 'given two values are not equal' do
      before do
        subject.load([
          VM::PUSH, 3,
          VM::PUSH, 2,
          VM::EQ,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 0 onto the stack' do
        expect(subject.peek).to eq(0)
      end
    end
  end

  describe 'NOT' do
    context 'given a 1' do
      before do
        subject.load([
          VM::PUSH, 1,
          VM::NOT,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 0 onto the stack' do
        expect(subject.peek).to eq(0)
      end
    end

    context 'given a 0' do
      before do
        subject.load([
          VM::PUSH, 0,
          VM::NOT,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 1 onto the stack' do
        expect(subject.peek).to eq(1)
      end
    end
  end

  describe 'GT' do
    context 'given first value greater than the second' do
      before do
        subject.load([
          VM::PUSH, 1,
          VM::PUSH, 2,
          VM::GT,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 1 onto the stack' do
        expect(subject.peek).to eq(1)
      end
    end

    context 'given first value not greater than the second' do
      before do
        subject.load([
          VM::PUSH, 2,
          VM::PUSH, 1,
          VM::GT,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 0 onto the stack' do
        expect(subject.peek).to eq(0)
      end
    end
  end

  describe 'LT' do
    context 'given first value less than the second' do
      before do
        subject.load([
          VM::PUSH, 2,
          VM::PUSH, 1,
          VM::LT,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 1 onto the stack' do
        expect(subject.peek).to eq(1)
      end
    end

    context 'given first value not less than the second' do
      before do
        subject.load([
          VM::PUSH, 1,
          VM::PUSH, 2,
          VM::LT,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 0 onto the stack' do
        expect(subject.peek).to eq(0)
      end
    end
  end

  describe 'JIF' do
    context 'value on stack is 1' do
      before do
        subject.load([
          VM::PUSH, 1,
          VM::JIF, 4,
          VM::PUSH, 10,
          VM::RETURN,
          VM::PUSH, 20,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'jumps the number of instructions specified' do
        expect(subject.peek).to eq(20)
      end
    end

    context 'value on stack is 0' do
      before do
        subject.load([
          VM::PUSH, 0,
          VM::JIF, 4,
          VM::PUSH, 10,
          VM::RETURN,
          VM::PUSH, 20,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'does not jump' do
        expect(subject.peek).to eq(10)
      end
    end
  end

  # describe 'fibonacci' do
  #   # fib = [n] { if n < 2,
  #   #                n,
  #   #                { (fib n - 1) + (fib n - 2) } }
  #   before do
  #     subject.load([
  #       VM::PUSH, 8, # TODO   
  #       VM::PUSH, 0,
  #       VM::ALLOC,

  #       # fib
  #       VM::PUSH, 0,
  #       VM::LOAD, 8,
  #       VM::PUSH, 2,
  #       VM::RETR, 0, # arg1
  #       VM::LT,
  #       VM::NOT,
  #       VM::JIF, 4,  # if arg1 >= 2, jump down
  #       # return 2
  #       VM::PUSH, 2,
  #       VM::RETURN,
  #       # reduce
  #       VM::RETR, 0, # arg1
  #       VM::PUSH, 1,
  #       VM::SUB,     # arg1 - 1
  #       # call self
  #       VM::PUSH, 
  #       VM::CALL, 
  #     ])
  #   end
  # end

end

RSpec.configure do |c|
  c.filter_run focus: true
  c.run_all_when_everything_filtered = true
end
