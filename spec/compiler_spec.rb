require_relative './spec_helper'

describe Compiler do
  def d(instructions)
    subject.pretty_format(instructions)
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
          'VM::HALT'
        ])
      end
    end

    context 'variable' do
      before do
        @result = subject.compile([
          'n'
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_LOCAL', 0,
          'VM::HALT'
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
          'VM::SET_LOCAL', 0,
          'VM::HALT'
        ])
      end
    end

    context 'print' do
      before do
        @result = subject.compile([
          ['print', '1']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::INT', VM::INT_PRINT_VAL,
          'VM::HALT'
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
            'VM::HALT'
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
            'VM::SET_LOCAL', 0,
            'VM::HALT'
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
            'VM::RETURN',          #   <return>
            'VM::ENDF',            #   )
            'VM::POP',             #   <pop (fn 1)>
            'VM::PUSH_FUNC',       #   (fn
            'VM::PUSH_NUM', '2',   #     2
            'VM::RETURN',          #   <return>
            'VM::ENDF',            #   )
            'VM::SET_LOCAL', 1,    #   (def two ...)
            'VM::PUSH_FUNC',       #   (fn
            'VM::PUSH_NUM', '3',   #     3
            'VM::RETURN',          #   <return>
            'VM::ENDF',            #   )
            'VM::RETURN',          # <return>
            'VM::ENDF',            # )
            'VM::SET_LOCAL', 0,    # (def one ...)
            'VM::HALT'
          ])
        end
      end

      context 'return value' do
        before do
          @result = subject.compile([
            ['def', 'one',
              ['fn', [],
                '1']],
            ['print', ['one']]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_NUM', '1',
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 0,
            'VM::PUSH_LOCAL', 0,
            'VM::CALL',
            'VM::INT', VM::INT_PRINT_VAL,
            'VM::HALT'
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
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 0,
            'VM::PUSH_LOCAL', 0,
            'VM::CALL',
            'VM::HALT'
          ])
        end
      end

      context 'with args' do
        before do
          @result = subject.compile([
            ['def', 'x',
              ['fn', ['y', 'z'], []]],
            ['x', '2', '4']
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_ARG',
            'VM::SET_LOCAL', 1,
            'VM::PUSH_ARG',
            'VM::SET_LOCAL', 2,
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 0,

            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '4',
            'VM::PUSH_NUM', 2,    # arg count
            'VM::SET_ARGS',
            'VM::PUSH_LOCAL', 0,
            'VM::CALL',
            'VM::HALT'
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
            'VM::SET_LOCAL', 0,
            'VM::HALT'
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
          'VM::HALT'
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
          'VM::HALT'
        ])
      end
    end

    context 'if' do
      before do
        @result = subject.compile([
          ['if', ['==', '1', '1'], '2', '3'],
          ['if', ['==', '1', '2'], '2', '3']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '1',
          'VM::CMP_EQ',
          'VM::JUMP_IF_TRUE', :if_1_true,
          'VM::JUMP', :if_1_false,
          'VM::LABEL', :if_1_true,
          'VM::PUSH_NUM', '2',
          'VM::JUMP', :if_1_end,
          'VM::LABEL', :if_1_false,
          'VM::PUSH_NUM', '3',
          'VM::LABEL', :if_1_end,
          'VM::POP',
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::CMP_EQ',
          'VM::JUMP_IF_TRUE', :if_2_true,
          'VM::JUMP', :if_2_false,
          'VM::LABEL', :if_2_true,
          'VM::PUSH_NUM', '2',
          'VM::JUMP', :if_2_end,
          'VM::LABEL', :if_2_false,
          'VM::PUSH_NUM', '3',
          'VM::LABEL', :if_2_end,
          'VM::HALT'
        ])
      end
    end
  end
end
