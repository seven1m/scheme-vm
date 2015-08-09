require_relative './spec_helper'
require 'stringio'

describe VM do
  let(:stdout) { StringIO.new }

  subject do
    described_class.new(
      stdout: stdout
    )
  end

  describe 'PUSH_ATOM' do
    before do
      subject.execute([
        VM::PUSH_ATOM, 'foo',
        VM::HALT
      ])
    end

    it 'allocates memory, stores the atom, and pushes address onto the stack' do
      expect(subject.stack_values).to eq([
        VM::Atom.new('foo')
      ])
    end
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

  describe 'PUSH_TRUE' do
    before do
      subject.execute([
        VM::PUSH_TRUE,
        VM::HALT
      ])
    end

    it 'allocates memory, stores the boolean, and pushes address onto the stack' do
      expect(subject.stack_values).to eq([
        VM::BoolTrue.instance
      ])
    end
  end

  describe 'PUSH_FALSE' do
    before do
      subject.execute([
        VM::PUSH_FALSE,
        VM::HALT
      ])
    end

    it 'allocates memory, stores the boolean, and pushes address onto the stack' do
      expect(subject.stack_values).to eq([
        VM::BoolFalse.instance
      ])
    end
  end

  describe 'PUSH_CAR' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::PUSH_NUM, '3',
        VM::PUSH_NUM, 3,
        VM::PUSH_LIST,
        VM::PUSH_CAR,
        VM::HALT
      ])
    end

    it 'allocates memory, stores the first element from the pair, and pushes address onto the stack' do
      expect(subject.stack_values).to eq([
        VM::Int.new('1')
      ])
    end
  end

  describe 'PUSH_CDR' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::PUSH_NUM, '3',
        VM::PUSH_NUM, 3,
        VM::PUSH_LIST,
        VM::PUSH_CDR,
        VM::HALT
      ])
    end

    it 'allocates memory, stores the first element from the pair, and pushes address onto the stack' do
      expect(subject.stack_values.first.to_a).to eq([
        VM::Int.new(2),
        VM::Int.new(3)
      ])
    end
  end

  describe 'PUSH_CONS' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::PUSH_NUM, '3',
        VM::PUSH_NUM, 2,
        VM::PUSH_LIST,
        VM::PUSH_CONS,
        VM::HALT
      ])
    end

    it 'allocates memory, creates a new pair with the two stack arguments, and pushes the address onto the stack' do
      expect(subject.stack_values.first.to_a).to eq([
        VM::Int.new(1),
        VM::Int.new(2),
        VM::Int.new(3)
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

    it 'creates a list and pushes the first node address onto the stack' do
      expect(subject.stack_values.size).to eq(1)
      node = subject.stack_values.first
      expect(node).to be_a(VM::Pair)
      expect(node.to_s).to eq('(5 7 9)')
    end
  end

  describe 'PUSH_LOCAL' do
    context do
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

    context 'pushing self' do
      let(:instructions) do
        [
          VM::PUSH_FUNC,
          VM::PUSH_REMOTE, 'my_func',
          VM::INT, VM::INT_PRINT,
          VM::RETURN,
          VM::ENDF,
          VM::SET_LOCAL, 'my_func',
          VM::PUSH_LOCAL, 'my_func',
          VM::CALL,
          VM::HALT
        ]
      end

      before do
        subject.execute(instructions)
      end

      it 'pushes the address of the current function onto the stack' do
        stdout.rewind
        expect(stdout.read.to_i).to eq(
          subject.heap.size - instructions.size + 1
        )
      end
    end

    context 'pushing undefined local variable' do
      it 'raises an error' do
        expect {
          subject.execute([VM::PUSH_LOCAL, 'x', VM::HALT])
        }.to raise_error(VM::VariableUndefined, 'x is not defined')
      end
    end

    context 'pushing undefined remote variable' do
      it 'raises an error' do
        expect {
          subject.execute([VM::PUSH_REMOTE, 'x', VM::HALT])
        }.to raise_error(VM::VariableUndefined, 'x is not defined')
      end
    end
  end

  describe 'PUSH_REMOTE' do
    context 'pushing a local defined prior to this function (closure)' do
      before do
        subject.execute([
          VM::PUSH_NUM, 10,
          VM::SET_LOCAL, 0, # x

          VM::PUSH_FUNC,
          VM::PUSH_REMOTE, 0, # x
          VM::INT, VM::INT_PRINT_VAL,
          VM::RETURN,
          VM::ENDF,
          VM::SET_LOCAL, 1, # my_func

          VM::PUSH_LOCAL, 1,
          VM::CALL,
          VM::HALT
        ])
      end

      it 'pushes the address of the variable defined outside this function onto the stack' do
        stdout.rewind
        expect(stdout.read).to eq('10')
      end
    end
  end

  describe 'SET_ARGS and PUSH_ARG and PUSH_ARGS' do
    before do
      address = subject.alloc
      subject.heap[address] = VM::Int.new(9)
      subject.locals[0] = address
      subject.execute([
        VM::PUSH_FUNC,
        VM::PUSH_ARG,
        VM::SET_LOCAL, 1,            # first arg
        VM::PUSH_ARG,
        VM::SET_LOCAL, 2,            # second arg
        VM::PUSH_ARGS,
        VM::SET_LOCAL, 3,            # list containing third and fourth args
        VM::PUSH_LOCAL, 1,
        VM::INT, VM::INT_PRINT_VAL,
        VM::PUSH_LOCAL, 2,
        VM::INT, VM::INT_PRINT_VAL,
        VM::PUSH_LOCAL, 3,
        VM::INT, VM::INT_PRINT_VAL,
        VM::RETURN,
        VM::ENDF,
        VM::SET_LOCAL, 0,

        VM::PUSH_NUM, 2,             # first arg
        VM::PUSH_NUM, 3,             # second arg
        VM::PUSH_NUM, 4,             # third arg
        VM::PUSH_NUM, 5,             # fourth arg
        VM::PUSH_NUM, 4,             # arg count
        VM::SET_ARGS,
        VM::PUSH_LOCAL, 0,
        VM::CALL,
        VM::HALT
      ])
    end

    it 'sets the arguments as locals inside the function' do
      stdout.rewind
      expect(stdout.read).to eq('23(4 5)')
    end
  end

  describe 'PUSH_FUNC and ENDF' do
    context 'given a single function' do
      let(:instructions) do
        [
          VM::PUSH_FUNC,
          VM::PUSH_NUM, '1',
          VM::RETURN,
          VM::ENDF,
          VM::HALT
        ]
      end

      before do
        subject.execute(instructions)
      end

      it 'pushes the function address onto the stack' do
        expect(subject.stack).to eq([
          subject.heap.size - instructions.size + 1
        ])
      end
    end

    context 'given a function within a function' do
      let(:instructions) do
        [
          VM::PUSH_FUNC,
          VM::PUSH_NUM, '1',
          VM::PUSH_FUNC,
          VM::RETURN,
          VM::ENDF,
          VM::RETURN,
          VM::ENDF,
          VM::HALT
        ]
      end

      before do
        subject.execute(instructions)
      end

      it 'pushes the function address onto the stack' do
        expect(subject.stack).to eq([
          subject.heap.size - instructions.size + 1
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

  describe 'SUB' do
    before do
      subject.execute([
        VM::PUSH_NUM, '3',
        VM::PUSH_NUM, '2',
        VM::SUB,
        VM::HALT
      ])
    end

    it 'subtracts the last number from the next-to-last number on the stack' do
      expect(subject.stack_values).to eq([
        VM::Int.new(1)
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
        address = stdout.read.to_i
        value = subject.resolve(address)
        expect(value).to eq(VM::Int.new(123))
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

    context 'given arg of INT_INCLUDE' do
      before do
        subject.execute([
          VM::PUSH_STR, '../spec/fixtures/include-test',
          VM::INT, VM::INT_INCLUDE,
          VM::HALT
        ])
      end

      it 'includes the file and executes it' do
        stdout.rewind
        expect(stdout.read).to eq('hello from include-test')
      end
    end
  end

  describe 'JUMP' do
    before do
      subject.execute([
        VM::JUMP, 3,
        VM::PUSH_NUM, 1,
        VM::PUSH_NUM, 2,
        VM::HALT
      ])
    end

    it 'skips the given number of instructions' do
      expect(subject.stack_values).to eq([
        VM::Int.new(2)
      ])
    end
  end

  describe 'JUMP_IF_FALSE' do
    before do
      subject.execute([
        VM::PUSH_FALSE,
        VM::JUMP_IF_FALSE, 3,
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::PUSH_TRUE,
        VM::JUMP_IF_FALSE, 3,
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::HALT
      ])
    end

    it 'jumps the given number of instructions if the top value on the stack is #f' do
      expect(subject.stack_values).to eq([
        VM::Int.new(2),
        VM::Int.new(1),
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

  describe 'APPLY' do
    before do
      subject.execute([
        VM::PUSH_FUNC,
        VM::PUSH_ARG,
        VM::SET_LOCAL, 'x',
        VM::PUSH_ARG,
        VM::SET_LOCAL, 'y',
        VM::PUSH_ARG,
        VM::SET_LOCAL, 'z',
        VM::PUSH_LOCAL, 'x',
        VM::INT, VM::INT_PRINT_VAL,
        VM::PUSH_LOCAL, 'z',
        VM::INT, VM::INT_PRINT_VAL,
        VM::PUSH_LOCAL, 'y',
        VM::INT, VM::INT_PRINT_VAL,
        VM::RETURN,
        VM::ENDF,
        VM::SET_LOCAL, 'foo',
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::PUSH_NUM, 2,
        VM::PUSH_LIST,
        VM::PUSH_NUM, '3',
        VM::PUSH_NUM, '4',
        VM::PUSH_NUM, 2,
        VM::PUSH_LIST,
        VM::PUSH_NUM, 2, # arg count
        VM::SET_ARGS,
        VM::PUSH_LOCAL, 'foo',
        VM::APPLY,
        VM::HALT
      ])
    end

    it 'calls the function, applying the list as arguments' do
      subject.stdout.rewind
      expect(subject.stdout.read).to eq('(1 2)43')
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

    it 'removes both values and puts a #t or #f on the stack' do
      expect(subject.stack_values).to eq([
        VM::BoolFalse.instance,
        VM::BoolFalse.instance,
        VM::BoolTrue.instance
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

    it 'removes both values and puts a #t or #f on the stack' do
      expect(subject.stack_values).to eq([
        VM::BoolFalse.instance,
        VM::BoolTrue.instance,
        VM::BoolTrue.instance
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

    it 'removes both values and puts a #t or #f on the stack' do
      expect(subject.stack_values).to eq([
        VM::BoolTrue.instance,
        VM::BoolFalse.instance,
        VM::BoolFalse.instance
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

    it 'removes both values and puts a #t or #f on the stack' do
      expect(subject.stack_values).to eq([
        VM::BoolTrue.instance,
        VM::BoolTrue.instance,
        VM::BoolFalse.instance
      ])
    end
  end

  describe 'CMP_EQ' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '1',
        VM::CMP_EQ,
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::CMP_EQ,
        VM::PUSH_TRUE,
        VM::PUSH_TRUE,
        VM::CMP_EQ,
        VM::PUSH_TRUE,
        VM::PUSH_FALSE,
        VM::CMP_EQ,
        VM::HALT
      ])
    end

    it 'removes both values and puts a #t or #f on the stack' do
      expect(subject.stack_values).to eq([
        VM::BoolTrue.instance,
        VM::BoolFalse.instance,
        VM::BoolTrue.instance,
        VM::BoolFalse.instance
      ])
    end
  end

  describe 'CMP_EQ_NUM' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '1',
        VM::CMP_EQ_NUM,
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::CMP_EQ_NUM,
        VM::PUSH_TRUE,
        VM::PUSH_TRUE,
        VM::CMP_EQ_NUM,
        VM::PUSH_TRUE,
        VM::PUSH_FALSE,
        VM::CMP_EQ_NUM,
        VM::HALT
      ])
    end

    it 'removes both values and puts a #t or #f on the stack' do
      expect(subject.stack_values).to eq([
        VM::BoolTrue.instance,
        VM::BoolFalse.instance,
        VM::BoolFalse.instance,
        VM::BoolFalse.instance
      ])
    end
  end

  describe 'CMP_NULL' do
    before do
      subject.execute([
        VM::PUSH_NUM, 0,
        VM::PUSH_LIST,
        VM::CMP_NULL,
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::PUSH_NUM, 2,
        VM::PUSH_LIST,
        VM::CMP_NULL,
        VM::HALT
      ])
    end

    it 'removes the value and puts a #t or #f on the stack' do
      expect(subject.stack_values).to eq([
        VM::BoolTrue.instance,
        VM::BoolFalse.instance
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
