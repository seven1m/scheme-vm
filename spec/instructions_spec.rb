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
        VM::PUSH_NUM, '1'
      ])
    end

    it 'allocates memory, stores the number, and pushes address onto the stack' do
      expect(subject.stack_values).to eq([
        VM::Int.new(1)
      ])
    end
  end

  describe 'ADD' do
    before do
      subject.execute([
        VM::PUSH_NUM, '1',
        VM::PUSH_NUM, '2',
        VM::ADD
      ])
    end

    it 'adds the last 2 numbers on the stack' do
      expect(subject.stack_values).to eq([
        VM::Int.new(3)
      ])
    end
  end

  describe 'PUSH_STR' do
    before do
      subject.execute([
        VM::PUSH_STR, 'hello world'
      ])
    end

    it 'allocates memory, stores the string, and pushes address onto the stack' do
      expect(subject.stack_values).to eq([
        VM::ByteArray.new('hello world')
      ])
    end
  end

  describe 'POP' do
    before do
      subject.execute([
        VM::POP
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
          VM::INT, VM::INT_PRINT
        ])
      end

      it 'prints the address of the last item on the stack' do
        stdout.rewind
        expect(stdout.read).to eq('0')
      end
    end

    context 'given arg of INT_PRINT_VAL' do
      before do
        subject.execute([
          VM::PUSH_STR, 'hello world',
          VM::INT, VM::INT_PRINT_VAL
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
        VM::PUSH_NUM, 2
      ])
    end

    it 'skips over the intermediate code, to the given label' do
      expect(subject.stack_values).to eq([
        VM::Int.new(2)
      ])
    end
  end

  describe 'CALL and RETURN' do
    before do
      subject.execute([
        VM::JUMP, :main,
        VM::LABEL, :func,
        VM::PUSH_STR, 'yo',
        VM::INT, VM::INT_PRINT_VAL,
        VM::RETURN,
        VM::LABEL, :main,
        VM::CALL, :func,
        VM::CALL, :func
      ])
    end

    it 'jumps to the label and returns' do
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
        VM::CMP_GT
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
        VM::CMP_GTE
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
        VM::CMP_LT
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
        VM::CMP_LTE
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
        VM::DUP
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
        VM::JUMP_IF_TRUE, :loop
      ])
    end

    it 'jumps to the label if the last value on the stack is truthy' do
      subject.stdout.rewind
      expect(subject.stdout.read).to eq('0123456789')
    end
  end
end
