require_relative '../vm'

describe VM do

  describe 'NOOP' do
    before do
      subject.load([
        VM::NOOP,
        VM::RETURN
      ])
    end

    it 'does nothing' do
      expect { subject.step }.not_to change { [subject.sp, subject.fp, subject.stack.compact, subject.heap.compact] }
    end
  end

  describe 'PUSHNUM' do
    before do
      subject.load([
        VM::PUSHNUM, 1,
        VM::RETURN
      ])
    end

    it 'pushes a number value onto the stack' do
      expect { subject.step }.to change(subject, :sp).by(-1)
      expect(subject.peek).to eq(1)
    end
  end

  describe 'POP' do
    before do
      subject.load([
        VM::POP,
        VM::RETURN
      ])
      subject.push(5)
    end

    it 'pops a value from the stack and discards it' do
      expect { subject.step }.to change(subject, :sp).by(1)
      expect(subject.peek).to be_nil
    end
  end

  describe 'DUP' do
    before do
      subject.load([
        VM::DUP,
        VM::RETURN
      ])
      subject.push(5)
    end

    it 'duplicates the top value on the stack' do
      subject.step
      expect(subject.stack.compact).to eq([5, 5])
    end
  end

  describe 'PUTS' do
    context 'with a single value' do
      before do
        subject.load([
          VM::PUSHNUM, 'x'.ord,
          VM::PUSHNUM, 1,
          VM::PUTS,
          VM::PUSHNUM, 0,
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
          VM::PUSHNUM, 'z'.ord,
          VM::PUSHNUM, 'y'.ord,
          VM::PUSHNUM, 'x'.ord,
          VM::PUSHNUM, 3,
          VM::PUTS,
          VM::PUSHNUM, 0,
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
        VM::PUSHNUM, 3,
        VM::PUSHNUM, 2,
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
        VM::PUSHNUM, 3,
        VM::PUSHNUM, 2,
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
        VM::PUSHNUM, 3,
        VM::PUSHNUM, 2,
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
        VM::PUSHNUM, 6,
        VM::PUSHNUM, 2,
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
        VM::PUSHNUM, 3,
        VM::PUSHNUM, 0,
        VM::ALLOC,
        VM::PUSHNUM, 0,
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
        VM::PUSHNUM, 10,
        VM::PUSHNUM, 0,
        VM::ASSIGN,
        VM::PUSHNUM, 0,
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
        VM::PUSHNUM, 0,
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
    context 'single function' do
      before do
        subject.load([
          VM::PUSHNUM, 0,
          VM::FUNC,
          VM::PUSHNUM, 'y'.ord,
          VM::PUSHNUM, 1,
          VM::PUTS,
          VM::PUSHNUM, 0,
          VM::RETURN,
          VM::ENDF,

          VM::PUSHNUM, 0,
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

    context 'nested function' do
      before do
        subject.load([
          VM::PUSHNUM, 0,
          VM::FUNC,    # top function
          VM::PUSHNUM, 1,
          VM::FUNC,    # nested function
          VM::PUSHNUM, 1,
          VM::RETURN,
          VM::ENDF,    # end nested function
          VM::PUSHNUM, 2,
          VM::RETURN,
          VM::ENDF,    # end top function

          VM::PUSHNUM, 3,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'creates a single function' do
        expect(subject.frame.locals[0]).to be_a(Fixnum)
        expect(subject.frame.locals[1]).to be_nil
        expect(subject.peek).to eq(3)
      end
    end
  end

  describe 'CALL' do
    before do
      subject.load([
        VM::PUSHNUM, 2, # arg1
        VM::PUSHNUM, 1, # arg count
        VM::PUSHNUM, 1, # index of function
        VM::CALL,
        VM::RETURN
      ])
      # manually load the instructions for a function
      subject.heap[20..27] = [
        VM::PUSHNUM, 'x'.ord,
        VM::PUSHNUM, 1,
        VM::PUTS,
        VM::PUSHNUM, 0,
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
          VM::PUSHNUM, 3,
          VM::PUSHNUM, 3,
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
          VM::PUSHNUM, 3,
          VM::PUSHNUM, 2,
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
          VM::PUSHNUM, 1,
          VM::NOT,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 0 onto the stack' do
        expect(subject.peek).to eq(VM::Value.new(VM::NUM, 0))
      end
    end

    context 'given a 0' do
      before do
        subject.load([
          VM::PUSHNUM, 0,
          VM::NOT,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 1 onto the stack' do
        expect(subject.peek).to eq(VM::Value.new(VM::NUM, 1))
      end
    end
  end

  describe 'GT' do
    context 'given first value greater than the second' do
      before do
        subject.load([
          VM::PUSHNUM, 2,
          VM::PUSHNUM, 1,
          VM::GT,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 1 onto the stack' do
        expect(subject.peek).to eq(VM::Value.new(VM::NUM, 1))
      end
    end

    context 'given first value not greater than the second' do
      before do
        subject.load([
          VM::PUSHNUM, 1,
          VM::PUSHNUM, 2,
          VM::GT,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 0 onto the stack' do
        expect(subject.peek).to eq(VM::Value.new(VM::NUM, 0))
      end
    end
  end

  describe 'LT' do
    context 'given first value less than the second' do
      before do
        subject.load([
          VM::PUSHNUM, 1,
          VM::PUSHNUM, 2,
          VM::LT,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 1 onto the stack' do
        expect(subject.peek).to eq(VM::Value.new(VM::NUM, 1))
      end
    end

    context 'given first value not less than the second' do
      before do
        subject.load([
          VM::PUSHNUM, 2,
          VM::PUSHNUM, 1,
          VM::LT,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'pushes a 0 onto the stack' do
        expect(subject.peek).to eq(VM::Value.new(VM::NUM, 0))
      end
    end
  end

  describe 'JIF' do
    context 'value on stack is 1' do
      before do
        subject.load([
          VM::PUSHNUM, 1,
          VM::JIF, 4,
          VM::PUSHNUM, 10,
          VM::RETURN,
          VM::PUSHNUM, 20,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'jumps the number of instructions specified' do
        expect(subject.peek).to eq(VM::Value.new(VM::NUM, 20))
      end
    end

    context 'value on stack is 0' do
      before do
        subject.load([
          VM::PUSHNUM, 0,
          VM::JIF, 4,
          VM::PUSHNUM, 10,
          VM::RETURN,
          VM::PUSHNUM, 20,
          VM::RETURN
        ])
        subject.step_frame
      end

      it 'does not jump' do
        expect(subject.peek).to eq(VM::Value.new(VM::NUM, 10))
      end
    end
  end

end

RSpec.configure do |c|
  c.filter_run focus: true
  c.run_all_when_everything_filtered = true
end
