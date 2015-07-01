require_relative './spec_helper'
require 'stringio'

describe VM do
  let(:stdout) { StringIO.new }

  subject do
    described_class.new(
      stdout: stdout
    )
  end

  describe 'PUSH_NUM' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::HALT
      ])
    end

    it 'allocates memory, stores the number, and pushes address onto the stack' do
      expect(subject.stack_values).to eq([
        VM::Int.new(1)
      ])
    end
  end

  describe 'PUSH_STR' do
    before do
      subject.execute([
        VM::PUSH_STR, 'hello world',
        VM::HALT
      ])
    end

    it 'allocates memory, stores the string, and pushes address onto the stack' do
      expect(subject.stack_values).to eq([
        VM::ByteArray.new('hello world')
      ])
    end
  end

  describe 'PUSH_LIST' do
    before do
      subject.execute([
        VM::PUSH_NUM, 5,
        VM::PUSH_NUM, 7,
        VM::PUSH_NUM, 9,
        VM::PUSH_NUM, 3,
        VM::PUSH_LIST,
        VM::HALT
      ])
    end

    it 'creates a linked-list and pushes the first node address onto the stack' do
      expect(subject.stack_values.size).to eq(1)
      node = subject.stack_values.first
      expect(node).to be_a(VM::ListNode)
      expect(node.to_s).to eq('[5, 7, 9]')
    end
  end

  describe 'PUSH_LOCAL' do
    before do
      address = subject.alloc
      subject.heap[address] = VM::Int.new(9)
      subject.locals[0] = address
      subject.execute([
        VM::PUSH_LOCAL, 0,
        VM::HALT
      ])
    end

    it 'pushes the address of the local variable onto the stack' do
      expect(subject.stack_values).to eq([
        VM::Int.new(9)
      ])
    end
  end

  describe 'SET_ARGS and PUSH_ARG' do
    before do
      address = subject.alloc
      subject.heap[address] = VM::Int.new(9)
      subject.locals[0] = address
      subject.execute([
        VM::PUSH_FUNC,
        VM::PUSH_ARG, 0,             # first arg
        VM::INT, VM::INT_PRINT_VAL,
        VM::PUSH_ARG, 1,             # second arg
        VM::INT, VM::INT_PRINT_VAL,
        VM::RETURN,
        VM::ENDF,
        VM::SET_LOCAL, 0,

        VM::PUSH_NUM, 2,             # first arg
        VM::PUSH_NUM, 3,             # second arg
        VM::PUSH_NUM, 2,             # arg count
        VM::SET_ARGS,
        VM::PUSH_LOCAL, 0,
        VM::CALL,
        VM::HALT
      ])
    end

    it 'pushes the address of the argument onto the stack' do
      stdout.rewind
      expect(stdout.read).to eq('23')
    end
  end

  describe 'PUSH_FUNC and ENDF' do
    context 'given a single function' do
      before do
        subject.execute([
          VM::PUSH_FUNC,
          VM::PUSH_NUM, '1',
          VM::RETURN,
          VM::ENDF,
          VM::HALT
        ])
      end

      it 'pushes the function address onto the stack' do
        expect(subject.stack).to eq([
          1
        ])
      end
    end

    context 'given a function within a function' do
      before do
        subject.execute([
          VM::PUSH_FUNC,
          VM::PUSH_NUM, '1',
          VM::PUSH_FUNC,
          VM::RETURN,
          VM::ENDF,
          VM::RETURN,
          VM::ENDF,
          VM::HALT
        ])
      end

      it 'pushes the function address onto the stack' do
        expect(subject.stack).to eq([
          1
        ])
      end
    end
  end

  describe 'ADD' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::ADD,
        VM::HALT
      ])
    end

    it 'adds the last 2 numbers on the stack' do
      expect(subject.stack_values).to eq([
        VM::Int.new(3)
      ])
    end
  end

  describe 'POP' do
    before do
      subject.execute([
        VM::POP,
        VM::HALT
      ])
    end

    it 'pops the last value from the stack' do
      expect(subject.stack).to eq([])
    end
  end

  describe 'INT' do
    context 'given arg of INT_PRINT' do
      before do
        subject.execute([
          VM::PUSH_NUM, '123',
          VM::INT, VM::INT_PRINT,
          VM::HALT
        ])
      end

      it 'prints the address of the last item on the stack' do
        stdout.rewind
        expect(stdout.read).to eq('5') # length_of_program + 1
      end
    end

    context 'given arg of INT_PRINT_VAL' do
      before do
        subject.execute([
          VM::PUSH_STR, 'hello world',
          VM::INT, VM::INT_PRINT_VAL,
          VM::HALT
        ])
      end

      it 'prints the memory pointed to by the address in last item on the stack' do
        stdout.rewind
        expect(stdout.read).to eq('hello world')
      end
    end
  end

  describe 'JUMP' do
    before do
      subject.execute([
        VM::JUMP, :skip,
        VM::PUSH_NUM, 1,
        VM::LABEL, :skip,
        VM::PUSH_NUM, 2,
        VM::HALT
      ])
    end

    it 'skips over the intermediate code, to the given label' do
      expect(subject.stack_values).to eq([
        VM::Int.new(2)
      ])
    end
  end

  describe 'CALL' do
    before do
      subject.execute([
        VM::PUSH_FUNC,
        VM::PUSH_STR, 'yo',
        VM::INT, VM::INT_PRINT_VAL,
        VM::POP,
        VM::RETURN,
        VM::ENDF,
        VM::DUP,
        VM::CALL,
        VM::CALL,
        VM::HALT
      ])
    end

    it 'calls the function' do
      subject.stdout.rewind
      expect(subject.stdout.read).to eq('yoyo')
    end
  end

  describe 'CMP_GT' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::CMP_GT,
        VM::PUSH_NUM, '2',
        VM::PUSH_NUM, '2',
        VM::CMP_GT,
        VM::PUSH_NUM, '3',
        VM::PUSH_NUM, '2',
        VM::CMP_GT,
        VM::HALT
      ])
    end

    it 'removes both values and puts a 1 or 0 on the stack' do
      expect(subject.stack_values).to eq([
        VM::Int.new(1),
        VM::Int.new(0),
        VM::Int.new(0)
      ])
    end
  end

  describe 'CMP_GTE' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::CMP_GTE,
        VM::PUSH_NUM, '2',
        VM::PUSH_NUM, '2',
        VM::CMP_GTE,
        VM::PUSH_NUM, '3',
        VM::PUSH_NUM, '2',
        VM::CMP_GTE,
        VM::HALT
      ])
    end

    it 'removes both values and puts a 1 or 0 on the stack' do
      expect(subject.stack_values).to eq([
        VM::Int.new(1),
        VM::Int.new(1),
        VM::Int.new(0)
      ])
    end
  end

  describe 'CMP_LT' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::CMP_LT,
        VM::PUSH_NUM, '2',
        VM::PUSH_NUM, '2',
        VM::CMP_LT,
        VM::PUSH_NUM, '3',
        VM::PUSH_NUM, '2',
        VM::CMP_LT,
        VM::HALT
      ])
    end

    it 'removes both values and puts a 1 or 0 on the stack' do
      expect(subject.stack_values).to eq([
        VM::Int.new(0),
        VM::Int.new(0),
        VM::Int.new(1)
      ])
    end
  end

  describe 'CMP_LTE' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::CMP_LTE,
        VM::PUSH_NUM, '2',
        VM::PUSH_NUM, '2',
        VM::CMP_LTE,
        VM::PUSH_NUM, '3',
        VM::PUSH_NUM, '2',
        VM::CMP_LTE,
        VM::HALT
      ])
    end

    it 'removes both values and puts a 1 or 0 on the stack' do
      expect(subject.stack_values).to eq([
        VM::Int.new(0),
        VM::Int.new(1),
        VM::Int.new(1)
      ])
    end
  end

  describe 'DUP' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::DUP,
        VM::HALT
      ])
    end

    it 'duplicates the last value on the stack' do
      expect(subject.stack_values).to eq([
        VM::Int.new(1),
        VM::Int.new(1)
      ])
    end
  end

  describe 'JUMP_IF_TRUE' do
    before do
      subject.execute([
        VM::PUSH_NUM, '0',
        VM::LABEL, :loop,
        VM::INT, VM::INT_PRINT_VAL,
        VM::PUSH_NUM, '1',
        VM::ADD,
        VM::DUP,
        VM::PUSH_NUM, '10',
        VM::CMP_GT,
        VM::JUMP_IF_TRUE, :loop,
        VM::HALT
      ])
    end

    it 'jumps to the label if the last value on the stack is truthy' do
      subject.stdout.rewind
      expect(subject.stdout.read).to eq('0123456789')
    end
  end

  describe 'SET_LOCAL' do
    before do
      subject.execute([
        VM::PUSH_FUNC,
        VM::PUSH_STR, 'func.',
        VM::SET_LOCAL, 0,
        VM::PUSH_LOCAL, 0,
        VM::INT, VM::INT_PRINT_VAL,
        VM::POP,
        VM::RETURN,
        VM::ENDF,
        VM::SET_LOCAL, 1,

        VM::PUSH_LOCAL, 1,
        VM::CALL,

        VM::PUSH_STR, 'main.',
        VM::SET_LOCAL, 0,
        VM::PUSH_LOCAL, 0,
        VM::INT, VM::INT_PRINT_VAL,
        VM::POP,

        VM::PUSH_LOCAL, 1,
        VM::CALL,

        VM::HALT
      ])
    end

    it 'stores the stack value in given variable index' do
      expect(subject.local_values[0]).to eq(
        VM::ByteArray.new('main.')
      )
    end

    it 'keeps locals from different call frames separate' do
      subject.stdout.rewind
      expect(subject.stdout.read).to eq('func.main.func.')
    end
  end
end
