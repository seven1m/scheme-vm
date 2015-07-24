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

    context '#t' do
      before do
        @result = subject.compile([
          '#t'
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_TRUE',
          'VM::HALT'
        ])
      end
    end

    context '#f' do
      before do
        @result = subject.compile([
          '#f'
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_FALSE',
          'VM::HALT'
        ])
      end
    end

    context 'car' do
      before do
        @result = subject.compile([
          ['car', ['list', '1', '2', '3']]
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', '3',
          'VM::PUSH_NUM', 3,
          'VM::PUSH_LIST',
          'VM::PUSH_CAR',
          'VM::HALT'
        ])
      end
    end

    context 'cdr' do
      before do
        @result = subject.compile([
          ['cdr', ['list', '1', '2', '3']]
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', '3',
          'VM::PUSH_NUM', 3,
          'VM::PUSH_LIST',
          'VM::PUSH_CDR',
          'VM::HALT'
        ])
      end
    end

    context 'cons' do
      before do
        @result = subject.compile([
          ['cons', '1', ['list', '2', '3']]
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', '3',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',
          'VM::PUSH_CONS',
          'VM::HALT'
        ])
      end
    end

    context 'null?' do
      before do
        @result = subject.compile([
          ['null?', ['list']],
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', 0,
          'VM::PUSH_LIST',
          'VM::CMP_NULL',
          'VM::HALT'
        ])
      end
    end

    context 'local variable' do
      before do
        @result = subject.compile([
          ['define', 'n', '10'],
          ['define', 'x', '11'],
          'n'
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '10',
          'VM::SET_LOCAL', 'n',
          'VM::PUSH_NUM', '11',
          'VM::SET_LOCAL', 'x',
          'VM::PUSH_LOCAL', 'n',
          'VM::HALT'
        ])
      end
    end

    context 'remote variable' do
      before do
        @result = subject.compile([
          ['define', 'n', '10'],
          ['lambda', [],
            'n']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '10',
          'VM::SET_LOCAL', 'n',
          'VM::PUSH_FUNC',
          'VM::PUSH_REMOTE', 'n',
          'VM::RETURN',
          'VM::ENDF',
          'VM::HALT'
        ])
      end
    end

    context 'quote' do
      context 'given a list' do
        before do
          @result = subject.compile([
            ['quote', ['foo', '2', '3', ['print', '4']]]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_ATOM', 'foo',
            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '3',
            'VM::PUSH_ATOM', 'print',
            'VM::PUSH_NUM', '4',
            'VM::PUSH_NUM', 2, # arg count
            'VM::PUSH_LIST',
            'VM::PUSH_NUM', 4, # arg count
            'VM::PUSH_LIST',
            'VM::HALT'
          ])
        end
      end

      context 'given an atom' do
        before do
          @result = subject.compile([
            ['quote', 'foo']
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_ATOM', 'foo',
            'VM::HALT'
          ])
        end
      end
    end

    context 'quasiquote' do
      context 'given a simple list' do
        before do
          @result = subject.compile([
            ['quasiquote', ['foo', '2', '3', ['print', '4']]]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_ATOM', 'foo',
            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '3',
            'VM::PUSH_ATOM', 'print',
            'VM::PUSH_NUM', '4',
            'VM::PUSH_NUM', 2, # arg count
            'VM::PUSH_LIST',
            'VM::PUSH_NUM', 4, # arg count
            'VM::PUSH_LIST',
            'VM::HALT'
          ])
        end
      end

      context 'given a list containing an unquote expression' do
        before do
          @result = subject.compile([
            ['quasiquote', ['list', '1', ['unquote', ['+', '2', '3']]]]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_ATOM', 'list',
            'VM::PUSH_NUM', '1',
            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '3',
            'VM::ADD',
            'VM::PUSH_NUM', 3,   # arg count
            'VM::PUSH_LIST',     # ['list', '1', '5']
            'VM::HALT'
          ])
        end
      end

      context 'given a list containing an unquote-splicing expression' do
        before do
          @result = subject.compile([
            ['quasiquote', ['list', '1', ['unquote-splicing', ['list', '2', '3']]]]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_ATOM', 'list',
            'VM::PUSH_NUM', '1',
            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '3',
            'VM::PUSH_NUM', 4,   # arg count
            'VM::PUSH_LIST',     # ['list', '1', '2', '3']
            'VM::HALT'
          ])
        end
      end
    end

    context 'define' do
      before do
        @result = subject.compile([
          ['define', 'x', '1']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::SET_LOCAL', 'x',
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

    context 'lambda' do
      context 'not storing in a variable or passing to a function' do
        before do
          @result = subject.compile([
            ['lambda', [],
              ['define', 'x', '1']]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_NUM', '1',
            'VM::SET_LOCAL', 'x',
            'VM::RETURN',
            'VM::ENDF',
            'VM::HALT'
          ])
        end
      end

      context 'storing in a variable' do
        before do
          @result = subject.compile([
            ['define', 'myfn',
              ['lambda', [],
                ['define', 'x', '1']]]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_NUM', '1',
            'VM::SET_LOCAL', 'x',
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 'myfn',
            'VM::HALT'
          ])
        end
      end

      context 'mixed variable storage' do
        before do
          @result = subject.compile([
            ['define', 'one',
              ['lambda', [],
                ['lambda', [], '1'],
                ['define', 'two',
                  ['lambda', [], '2']],
                ['lambda', [], '3']]]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',        # (lambda
            'VM::PUSH_FUNC',        #   (lambda
            'VM::PUSH_NUM', '1',    #     1
            'VM::RETURN',           #   <return>
            'VM::ENDF',             #   )
            'VM::POP',              #   <pop (lambda 1)>
            'VM::PUSH_FUNC',        #   (lambda
            'VM::PUSH_NUM', '2',    #     2
            'VM::RETURN',           #   <return>
            'VM::ENDF',             #   )
            'VM::SET_LOCAL', 'two', #   (def two ...)
            'VM::PUSH_FUNC',        #   (lambda
            'VM::PUSH_NUM', '3',    #     3
            'VM::RETURN',           #   <return>
            'VM::ENDF',             #   )
            'VM::RETURN',           # <return>
            'VM::ENDF',             # )
            'VM::SET_LOCAL', 'one', # (def one ...)
            'VM::HALT'
          ])
        end
      end

      context 'return value' do
        before do
          @result = subject.compile([
            ['define', 'one',
              ['lambda', [],
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
            'VM::SET_LOCAL', 'one',
            'VM::PUSH_LOCAL', 'one',
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
            ['define', 'x',
              ['lambda', [],
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
            'VM::SET_LOCAL', 'x',
            'VM::PUSH_LOCAL', 'x',
            'VM::CALL',
            'VM::HALT'
          ])
        end
      end

      context 'with args' do
        before do
          @result = subject.compile([
            ['define', 'x',
              ['lambda', ['y', 'z'], []]],
            ['x', '2', '4']
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_ARG',
            'VM::SET_LOCAL', 'y',
            'VM::PUSH_ARG',
            'VM::SET_LOCAL', 'z',
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 'x',

            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '4',
            'VM::PUSH_NUM', 2,    # arg count
            'VM::SET_ARGS',
            'VM::PUSH_LOCAL', 'x',
            'VM::CALL',
            'VM::HALT'
          ])
        end
      end

      context 'with variable args' do
        before do
          @result = subject.compile([
            ['define', 'x',
              ['lambda', 'args', []]],
            ['x', '2', '4']
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_ARGS',
            'VM::SET_LOCAL', 'args',
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 'x',

            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '4',
            'VM::PUSH_NUM', 2,    # arg count
            'VM::SET_ARGS',
            'VM::PUSH_LOCAL', 'x',
            'VM::CALL',
            'VM::HALT'
          ])
        end
      end

      context 'with destructuring args' do
        before do
          @result = subject.compile([
            ['define', 'x',
              ['lambda', ['first', 'second', '.', 'rest'], []]],
            ['x', '2', '3', '4', '5']
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_ARG',
            'VM::SET_LOCAL', 'first',
            'VM::PUSH_ARG',
            'VM::SET_LOCAL', 'second',
            'VM::PUSH_ARGS',
            'VM::SET_LOCAL', 'rest',
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 'x',

            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '3',
            'VM::PUSH_NUM', '4',
            'VM::PUSH_NUM', '5',
            'VM::PUSH_NUM', 4,    # arg count
            'VM::SET_ARGS',
            'VM::PUSH_LOCAL', 'x',
            'VM::CALL',
            'VM::HALT'
          ])
        end
      end

      context 'calling immediately' do
        before do
          @result = subject.compile([
            [['lambda', ['x'], 'x'], '1']
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_NUM', '1',
            'VM::PUSH_NUM', 1,
            'VM::SET_ARGS',
            'VM::PUSH_FUNC',
            'VM::PUSH_ARG',
            'VM::SET_LOCAL', 'x',
            'VM::PUSH_LOCAL', 'x',
            'VM::RETURN',
            'VM::ENDF',
            'VM::CALL',
            'VM::HALT'
          ])
        end
      end

      context 'calling self' do
        before do
          @result = subject.compile([
            ['define', 'x',
              ['lambda', [],
                ['x']]]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_REMOTE', 'x',
            'VM::CALL',
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 'x',
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

    context '=' do
      before do
        @result = subject.compile([
          ['=', '1', '1']
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
          ['if', ['=', '1', '1'], '2', '3'],
          ['if', ['=', '1', '2'], '2', '3']
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
