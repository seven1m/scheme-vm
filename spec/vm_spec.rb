require_relative './spec_helper'
require 'stringio'

describe VM do
  let(:stdout) { StringIO.new }

  subject do
    described_class.new(
      stdout: stdout
    )
  end

  describe 'NOOP' do
    before do
      subject.execute([
        VM::NOOP,
        VM::HALT
      ])
    end

    it 'does nothing' do
      expect(subject.stack_values).to eq([])
    end
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

  describe 'PUSH_TYPE' do
    before do
      subject.execute([
        VM::PUSH_NUM, 1,
        VM::PUSH_TYPE,
        VM::HALT
      ])
    end

    it 'pushes an int representing the type' do
      expect(subject.stack_values).to eq([
        VM::Int.new(6)
      ])
    end
  end

  describe 'PUSH_UNDEF' do
    before do
      subject.execute([
        VM::PUSH_UNDEF,
        VM::HALT
      ])
    end

    it 'pushes undefined onto the stack' do
      expect(subject.stack_values).to eq([
        nil
      ])
    end
  end

  describe 'RAW' do
    before do
      subject.execute([
        VM::PUSH_CHAR, 'a',
        VM::RAW,
        VM::HALT
      ])
    end

    it 'pushes the raw value' do
      expect(subject.stack_values).to eq([
        VM::Int.new(97)
      ])
    end
  end

  describe 'STR_REF' do
    before do
      subject.execute([
        VM::PUSH_STR, 'hello world',
        VM::PUSH_NUM, '4',
        VM::STR_REF,
        VM::HALT
      ])
    end

    it 'allocates memory, stores the character, and pushes address onto the stack' do
      expect(subject.stack_values).to eq([
        VM::Char.new('o')
      ])
    end
  end

  describe 'STR_LEN' do
    before do
      subject.execute([
        VM::PUSH_STR, 'hello world',
        VM::STR_LEN,
        VM::HALT
      ])
    end

    it 'allocates memory, stores the length of the string, and pushes address onto the stack' do
      expect(subject.stack_values).to eq([
        VM::Int.new(11)
      ])
    end
  end

  describe 'LIST_TO_STR' do
    before do
      subject.execute([
        VM::PUSH_CHAR, 'a',
        VM::PUSH_CHAR, 'b',
        VM::PUSH_NUM, 2,
        VM::PUSH_LIST,
        VM::LIST_TO_STR,
        VM::HALT
      ])
    end

    it 'allocates memory, stores the new string, and pushes address onto the stack' do
      expect(subject.stack_values).to eq([
        VM::ByteArray.new('ab')
      ])
    end
  end

  describe 'APPEND' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::PUSH_NUM, 2,
        VM::PUSH_LIST,
        VM::PUSH_NUM, '3',
        VM::PUSH_NUM, '4',
        VM::PUSH_NUM, 2,
        VM::PUSH_LIST,
        VM::PUSH_NUM, 2,
        VM::APPEND,
        VM::HALT
      ])
    end

    it 'allocates memory, stores the new list, and pushes address onto the stack' do
      expect(subject.stack_values.first.to_s).to eq('(1 2 3 4)')
    end
  end

  describe 'PUSH_CHAR' do
    before do
      subject.execute([
        VM::PUSH_CHAR, 'c',
        VM::HALT
      ])
    end

    it 'allocates memory, stores the character, and pushes address onto the stack' do
      expect(subject.stack_values).to eq([
        VM::Char.new('c')
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
      expect(subject.stack_values.first.to_s).to eq('(2 3)')
    end
  end

  describe 'PUSH_CONS' do
    context 'given the first element is an int' do
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
        expect(subject.stack_values.first.to_s).to eq('(1 2 3)')
      end
    end

    context 'given the first element is a list' do
      before do
        subject.execute([
          VM::PUSH_NUM, '1',
          VM::PUSH_NUM, '2',
          VM::PUSH_NUM, 2,
          VM::PUSH_LIST,
          VM::PUSH_NUM, '3',
          VM::PUSH_NUM, '4',
          VM::PUSH_NUM, 2,
          VM::PUSH_LIST,
          VM::PUSH_CONS,
          VM::HALT
        ])
      end

      it 'allocates memory, creates a new pair with the two stack arguments, and pushes the address onto the stack' do
        expect(subject.stack_values.first.to_s).to eq('((1 2) 3 4)')
      end
    end

    context 'given the last element is an int' do
      before do
        subject.execute([
          VM::PUSH_NUM, '1',
          VM::PUSH_NUM, '2',
          VM::PUSH_CONS,
          VM::HALT
        ])
      end

      it 'allocates memory, creates a new pair with the two stack arguments, and pushes the address onto the stack' do
        pair = subject.stack_values.first
        expect(pair.car).to eq(VM::Int.new(1))
        expect(pair.cdr).to eq(VM::Int.new(2))
        expect(pair.to_s).to eq('(1 . 2)')
      end
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

  describe 'PUSH_VAR' do
    context do
      before do
        address = subject.alloc
        subject.heap[address] = VM::Int.new(9)
        subject.locals[0] = address
        subject.execute([
          VM::PUSH_VAR, 0,
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
          VM::PUSH_VAR, 'my_func',
          VM::RETURN,
          VM::ENDF,
          VM::DEFINE_VAR, 'my_func',
          VM::PUSH_VAR, 'my_func',
          VM::CALL,
          VM::HALT
        ]
      end

      before do
        subject.execute(instructions)
      end

      it 'pushes the address of the current function onto the stack' do
        expect(subject.stack).to eq([
          subject.heap.size - instructions.size + 1
        ])
      end
    end

    context 'pushing undefined local variable' do
      it 'raises an error' do
        expect do
          subject.execute([VM::PUSH_VAR, 'x', VM::HALT])
        end.to raise_error(VM::VariableUndefined, 'x is not defined')
      end
    end

    context 'pushing undefined remote variable' do
      it 'raises an error' do
        expect do
          subject.execute([VM::PUSH_VAR, 'x', VM::HALT])
        end.to raise_error(VM::VariableUndefined, 'x is not defined')
      end
    end
  end

  describe 'PUSH_VAR' do
    context 'referencing variable in lexical scope' do
      before do
        subject.execute([
          VM::PUSH_NUM, '9',
          VM::DEFINE_VAR, 'x',

          VM::PUSH_FUNC,

          VM::PUSH_NUM, '10',
          VM::SET_VAR, 'x',

          VM::PUSH_NUM, '11',
          VM::DEFINE_VAR, 'y',

          VM::PUSH_NUM, '12',
          VM::DEFINE_VAR, 'z',

          VM::PUSH_FUNC,
          VM::PUSH_VAR, 'x',

          VM::PUSH_NUM, '13',
          VM::DEFINE_VAR, 'z',

          VM::PUSH_FUNC,
          VM::PUSH_VAR, 'y',
          VM::PUSH_VAR, 'z',
          VM::RETURN,
          VM::ENDF,

          VM::RETURN,
          VM::ENDF,

          VM::RETURN,
          VM::ENDF,

          VM::CALL, # call the outer function
          VM::CALL, # call the middle function

          VM::PUSH_NUM, '12',
          VM::DEFINE_VAR, 'y',

          VM::CALL, # call the inner function

          VM::HALT
        ])
      end

      it 'captures the variable' do
        expect(subject.stack_values).to eq([
          VM::Int.new(10),
          VM::Int.new(11),
          VM::Int.new(13)
        ])
      end
    end
  end

  describe 'SET_ARGS and PUSH_ARG and PUSH_ARGS' do
    before do
      subject.execute([
        VM::PUSH_FUNC,
        VM::PUSH_ARG,
        VM::DEFINE_VAR, 'x',            # first arg
        VM::PUSH_ARG,
        VM::DEFINE_VAR, 'y',            # second arg
        VM::PUSH_ARGS,
        VM::DEFINE_VAR, 'z',            # list containing third and fourth args
        VM::PUSH_VAR, 'x',
        VM::INT, VM::INT_WRITE,
        VM::PUSH_VAR, 'y',
        VM::INT, VM::INT_WRITE,
        VM::PUSH_VAR, 'z',
        VM::INT, VM::INT_WRITE,
        VM::RETURN,
        VM::ENDF,
        VM::DEFINE_VAR, 'fn',

        VM::PUSH_NUM, 2,             # first arg
        VM::PUSH_NUM, 3,             # second arg
        VM::PUSH_NUM, 4,             # third arg
        VM::PUSH_NUM, 5,             # fourth arg
        VM::PUSH_NUM, 4,             # arg count
        VM::SET_ARGS,
        VM::PUSH_VAR, 'fn',
        VM::CALL,
        VM::HALT
      ])
    end

    it 'sets the arguments as locals inside the function' do
      stdout.rewind
      expect(stdout.read).to eq('23(4 5)')
    end
  end

  describe 'SET_CAR' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::PUSH_CONS,
        VM::DUP,
        VM::PUSH_NUM, '3',
        VM::SET_CAR,
        VM::HALT
      ])
    end

    it 'changes the car of the pair' do
      pair = subject.pop_val
      expect(pair).to be_a(VM::Pair)
      expect(subject.heap[pair.address]).to eq(VM::Int.new(3))
      expect(subject.heap[pair.next_node]).to eq(VM::Int.new(2))
    end
  end

  describe 'SET_CDR' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::PUSH_CONS,
        VM::DUP,
        VM::PUSH_NUM, '3',
        VM::SET_CDR,
        VM::HALT
      ])
    end

    it 'changes the car of the pair' do
      pair = subject.pop_val
      expect(pair).to be_a(VM::Pair)
      expect(subject.heap[pair.address]).to eq(VM::Int.new(1))
      expect(subject.heap[pair.next_node]).to eq(VM::Int.new(3))
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
    context 'given arg of INT_WRITE' do
      before do
        subject.execute([
          VM::PUSH_STR, 'hello world',
          VM::INT, VM::INT_WRITE,
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
        VM::INT, VM::INT_WRITE,
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
    context 'given an empty list' do
      before do
        subject.execute([
          VM::PUSH_FUNC,
          VM::PUSH_ARGS,
          VM::DEFINE_VAR, 'x',
          VM::PUSH_VAR, 'x',
          VM::INT, VM::INT_WRITE,
          VM::RETURN,
          VM::ENDF,
          VM::DEFINE_VAR, 'foo',
          VM::PUSH_NUM, 0,
          VM::PUSH_LIST,
          VM::PUSH_NUM, 1, # arg count
          VM::SET_ARGS,
          VM::PUSH_VAR, 'foo',
          VM::APPLY,
          VM::HALT
        ])
      end

      it 'calls the function, applying the list as arguments' do
        subject.stdout.rewind
        expect(subject.stdout.read).to eq('()')
      end
    end

    context 'given one lists' do
      before do
        subject.execute([
          VM::PUSH_FUNC,
          VM::PUSH_ARG,
          VM::DEFINE_VAR, 'x',
          VM::PUSH_ARG,
          VM::DEFINE_VAR, 'y',
          VM::PUSH_VAR, 'x',
          VM::INT, VM::INT_WRITE,
          VM::PUSH_VAR, 'y',
          VM::INT, VM::INT_WRITE,
          VM::RETURN,
          VM::ENDF,
          VM::DEFINE_VAR, 'foo',
          VM::PUSH_NUM, '1',
          VM::PUSH_NUM, '2',
          VM::PUSH_NUM, 2,
          VM::PUSH_LIST,
          VM::PUSH_NUM, 1, # arg count
          VM::SET_ARGS,
          VM::PUSH_VAR, 'foo',
          VM::APPLY,
          VM::HALT
        ])
      end

      it 'calls the function, applying the list as arguments' do
        subject.stdout.rewind
        expect(subject.stdout.read).to eq('12')
      end
    end

    context 'given two lists' do
      before do
        subject.execute([
          VM::PUSH_FUNC,
          VM::PUSH_ARG,
          VM::DEFINE_VAR, 'x',
          VM::PUSH_ARG,
          VM::DEFINE_VAR, 'y',
          VM::PUSH_ARG,
          VM::DEFINE_VAR, 'z',
          VM::PUSH_VAR, 'x',
          VM::INT, VM::INT_WRITE,
          VM::PUSH_VAR, 'z',
          VM::INT, VM::INT_WRITE,
          VM::PUSH_VAR, 'y',
          VM::INT, VM::INT_WRITE,
          VM::RETURN,
          VM::ENDF,
          VM::DEFINE_VAR, 'foo',
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
          VM::PUSH_VAR, 'foo',
          VM::APPLY,
          VM::HALT
        ])
      end

      it 'calls the function, passing the first argument as-is and applying the last argument' do
        subject.stdout.rewind
        expect(subject.stdout.read).to eq('(1 2)43')
      end
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

  describe 'CMP_EQV' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::PUSH_NUM, 2,
        VM::PUSH_LIST,
        VM::DUP,
        VM::CMP_EQV,
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::PUSH_NUM, 2,
        VM::PUSH_LIST,
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::PUSH_NUM, 2,
        VM::PUSH_LIST,
        VM::CMP_EQV,
        VM::HALT
      ])
    end

    it 'removes both values and puts a #t or #f on the stack' do
      expect(subject.stack_values).to eq([
        VM::BoolTrue.instance,
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

  describe 'DEFINE_VAR' do
    before do
      subject.execute([
        VM::PUSH_FUNC,
        VM::PUSH_STR, 'func.',
        VM::DEFINE_VAR, 'x',
        VM::PUSH_VAR, 'x',
        VM::INT, VM::INT_WRITE,
        VM::POP,
        VM::RETURN,
        VM::ENDF,
        VM::DEFINE_VAR, 'fn',

        VM::PUSH_VAR, 'fn',
        VM::CALL,

        VM::PUSH_STR, 'main.',
        VM::DEFINE_VAR, 'x',

        VM::PUSH_VAR, 'x',
        VM::INT, VM::INT_WRITE,
        VM::POP,

        VM::PUSH_VAR, 'fn',
        VM::CALL,

        VM::HALT
      ])
    end

    it 'stores the stack value in given variable index' do
      expect(subject.local_values['x']).to eq(
        VM::ByteArray.new('main.')
      )
    end

    it 'keeps locals from different call frames separate' do
      subject.stdout.rewind
      expect(subject.stdout.read).to eq('func.main.func.')
    end
  end

  describe 'SET_VAR' do
    before do
      subject.execute([
        VM::PUSH_FUNC,
        VM::PUSH_STR, 'func.',
        VM::SET_VAR, 'x',
        VM::PUSH_VAR, 'x',
        VM::INT, VM::INT_WRITE,
        VM::POP,
        VM::RETURN,
        VM::ENDF,
        VM::DEFINE_VAR, 'fn',

        VM::PUSH_STR, 'main.',
        VM::DEFINE_VAR, 'x',

        VM::PUSH_VAR, 'x',
        VM::INT, VM::INT_WRITE,
        VM::POP,

        VM::PUSH_VAR, 'fn',
        VM::CALL,

        VM::PUSH_VAR, 'x',
        VM::INT, VM::INT_WRITE,
        VM::POP,

        VM::HALT
      ])
    end

    it 'stores the stack value in given variable index' do
      expect(subject.local_values['x']).to eq(
        VM::ByteArray.new('func.')
      )
    end

    it 'overwrites the value in the frame above' do
      subject.stdout.rewind
      expect(subject.stdout.read).to eq('main.func.func.')
    end
  end

  describe 'tail call elimination' do
    context 'when the return is in the true position of an if' do
      before do
        c = Compiler.new(<<-END, filename: 'tce.scm')
          (import (only (scheme base) >= - define if))
          (import (only (scheme process-context) exit))
          (define (fn n)
            (exit)
            (if (>= n 1)
              (fn (- n 1))
              n))
          (fn 2)
        END
        instr = c.compile(keep_last: true)
        subject.execute(instr)
      end

      it 'reuses the same stack frame' do
        expect(subject.call_stack.size).to eq(2)
        subject.execute
        expect(subject.call_stack.size).to eq(2)
        subject.execute
        expect(subject.call_stack.size).to eq(2)
        subject.execute
        expect(subject.call_stack.size).to eq(1)
        expect(subject.pop_val).to eq(VM::Int.new(0))
      end
    end

    context 'when the return is in the false position of an if' do
      before do
        c = Compiler.new(<<-END, filename: 'tce.scm')
          (import (only (scheme base) < - define if))
          (import (only (scheme process-context) exit))
          (define (fn n)
            (exit)
            (if (< n 1)
              n
              (fn (- n 1))))
          (fn 2)
        END
        instr = c.compile(keep_last: true)
        subject.execute(instr)
      end

      it 'reuses the same stack frame' do
        expect(subject.call_stack.size).to eq(2)
        subject.execute
        expect(subject.call_stack.size).to eq(2)
        subject.execute
        expect(subject.call_stack.size).to eq(2)
        subject.execute
        expect(subject.call_stack.size).to eq(1)
        expect(subject.pop_val).to eq(VM::Int.new(0))
      end
    end

    context 'inside nested ifs' do
      before do
        c = Compiler.new(<<-END, filename: 'tce.scm')
          (import (only (scheme base) >= - define if))
          (import (only (scheme process-context) exit))
          (define (fn n)
            (exit)
            (if #t
              (if (>= n 1)
                (fn (- n 1))
                n)
              #f))
          (fn 2)
        END
        instr = c.compile(keep_last: true)
        subject.execute(instr)
      end

      it 'reuses the same stack frame' do
        expect(subject.call_stack.size).to eq(2)
        subject.execute
        expect(subject.call_stack.size).to eq(2)
        subject.execute
        expect(subject.call_stack.size).to eq(2)
        subject.execute
        expect(subject.call_stack.size).to eq(1)
        expect(subject.pop_val).to eq(VM::Int.new(0))
      end
    end

    context 'when the call uses apply' do
      before do
        c = Compiler.new(<<-END, filename: 'tce.scm')
          (import (only (scheme base) >= - define if apply list))
          (import (only (scheme process-context) exit))
          (define (fn n)
            (exit)
            (if (>= n 1)
              (apply fn (list (- n 1)))
              n))
          (fn 2)
        END
        instr = c.compile(keep_last: true)
        subject.execute(instr)
      end

      it 'reuses the same stack frame' do
        expect(subject.call_stack.size).to eq(2)
        subject.execute
        expect(subject.call_stack.size).to eq(2)
        subject.execute
        expect(subject.call_stack.size).to eq(2)
        subject.execute
        expect(subject.call_stack.size).to eq(1)
        expect(subject.pop_val).to eq(VM::Int.new(0))
      end
    end
  end

  describe 'SET_LIB and IMPORT_LIB' do
    before do
      subject.execute([
        VM::SET_LIB, 'my-lib',

        VM::PUSH_STR, 'foo',
        VM::DEFINE_VAR, 'foo',

        VM::PUSH_STR, 'bar',
        VM::DEFINE_VAR, 'private',

        VM::PUSH_FUNC,
        VM::PUSH_VAR, 'private',
        VM::RETURN,
        VM::ENDF,
        VM::DEFINE_VAR, 'bar-fn',

        VM::ENDL,
        VM::HALT # pause here
      ])
    end

    it 'stores the library context and locals defined within' do
      expect(subject.libs['my-lib']).to be
      expect(subject.libs['my-lib'][:locals].keys).to eq(['foo', 'private', 'bar-fn'])
    end

    it 'imports the named binding into the local frame' do
      expect(subject.locals).to eq({})
      subject.execute([
        VM::IMPORT_LIB, 'my-lib', 'foo', 'foo',
        VM::PUSH_VAR, 'foo',
        VM::HALT
      ])
      expect(subject.stack_values).to eq([
        VM::ByteArray.new('foo')
      ])
      expect(subject.locals.keys).to eq(['foo'])
    end

    it 'allows an imported binding to be renamed' do
      expect(subject.locals).to eq({})
      subject.execute([
        VM::IMPORT_LIB, 'my-lib', 'foo', 'my-foo',
        VM::PUSH_VAR, 'my-foo',
        VM::HALT
      ])
      expect(subject.stack_values).to eq([
        VM::ByteArray.new('foo')
      ])
      expect(subject.locals.keys).to eq(['my-foo'])
    end

    it 'allows an imported binding to reference a non-imported one' do
      expect(subject.locals).to eq({})
      subject.execute([
        VM::IMPORT_LIB, 'my-lib', 'bar-fn', 'bar-fn',
        VM::PUSH_VAR, 'bar-fn',
        VM::CALL,
        VM::HALT
      ])
      expect(subject.stack_values).to eq([
        VM::ByteArray.new('bar')
      ])
      expect(subject.locals.keys).to eq(['bar-fn'])
    end
  end
end
