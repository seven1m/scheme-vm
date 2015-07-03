require_relative './spec_helper'

describe Compiler do
  def d(instructions)
    [].tap do |pretty|
      while instructions.any?
        if (instruction = instructions.shift)
          (name, arity) = VM::INSTRUCTIONS[instruction]
          pretty << "VM::#{name}"
          arity.times { pretty << instructions.shift }
        else
          pretty << nil
        end
      end
    end
  end

  describe '#compile' do
    context 'literal' do
      before do
        @result = subject.compile([
          '1'
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::POP'
        ])
      end
    end

    context 'def' do
      before do
        @result = subject.compile([
          ['def', 'x', '1']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::SET_LOCAL', 0
        ])
      end
    end

    context 'fn' do
      context 'not storing in a variable or passing to a function' do
        before do
          @result = subject.compile([
            ['fn', [],
              ['def', 'x', '1']]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_NUM', '1',
            'VM::SET_LOCAL', 0,
            'VM::RETURN',
            'VM::ENDF',
            'VM::POP'
          ])
        end
      end

      context 'storing in a variable' do
        before do
          @result = subject.compile([
            ['def', 'myfn',
              ['fn', [],
                ['def', 'x', '1']]]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_NUM', '1',
            'VM::SET_LOCAL', 1,
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 0
          ])
        end
      end

      context 'mixed variable storage' do
        before do
          @result = subject.compile([
            ['def', 'one',
              ['fn', [],
                ['fn', [], '1'],
                ['def', 'two',
                  ['fn', [], '2']],
                ['fn', [], '3']]]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',       # (fn
            'VM::PUSH_FUNC',       #   (fn
            'VM::PUSH_NUM', '1',   #     1
            'VM::POP',             #   <pop 1>
            'VM::RETURN',          #   <return>
            'VM::ENDF',            #   )
            'VM::POP',             #   <pop (fn 1)>
            'VM::PUSH_FUNC',       #   (fn
            'VM::PUSH_NUM', '2',   #     2
            'VM::POP',             #   <pop 2>
            'VM::RETURN',          #   <return>
            'VM::ENDF',            #   )
            'VM::SET_LOCAL', 1,    #   (def two ...)
            'VM::PUSH_FUNC',       #   (fn
            'VM::PUSH_NUM', '3',   #     3
            'VM::POP',             #   <pop 3>
            'VM::RETURN',          #   <return>
            'VM::ENDF',            #   )
            'VM::POP',             #   <pop (fn 3)>
            'VM::RETURN',          # <return>
            'VM::ENDF',            # )
            'VM::SET_LOCAL', 0     # (def one ...)
          ])
        end
      end
    end

    context 'call' do
      context 'without args' do
        before do
          @result = subject.compile([
            ['def', 'x',
              ['fn', [],
                '1']],
            ['x']
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_NUM', '1',
            'VM::POP',
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 0,
            'VM::PUSH_LOCAL', 0,
            'VM::CALL'
          ])
        end
      end

      context 'with args' do
        before do
          @result = subject.compile([
            ['def', 'x',
              ['fn', ['y'],
                ['def', 'z', 'y']]],
            ['x', '2']
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_LOCAL', 2,
            'VM::SET_LOCAL', 1,
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 0,

            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', 1,
            'VM::SET_ARGS',
            'VM::PUSH_LOCAL', 0,
            'VM::CALL'
          ])
        end
      end

      context 'calling self' do
        before do
          @result = subject.compile([
            ['def', 'x',
              ['fn', [],
                ['x']]]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_LOCAL', 0,
            'VM::CALL',
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 0
          ])
        end
      end
    end

    context 'list' do
      before do
        @result = subject.compile([
          ['list', '1', '2']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', 2, # arg count
          'VM::PUSH_LIST',
          'VM::POP'
        ])
      end
    end

    context '==' do
      before do
        @result = subject.compile([
          ['==', '1', '1']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '1',
          'VM::CMP_EQ',
          'VM::POP'
        ])
      end
    end

    context 'if' do
      before do
        @result = subject.compile([
          ['if', ['==', '1', '1'], '2', '3']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '1',
          'VM::CMP_EQ',
          'VM::JUMP_IF_TRUE', :if_0_true,
          'VM::JUMP', :if_0_false,
          'VM::LABEL', :if_0_true,
          'VM::PUSH_NUM', '2',
          'VM::LABEL', :if_0_false,
          'VM::PUSH_NUM', '3',
          'VM::POP'
        ])
      end
    end
  end
end
