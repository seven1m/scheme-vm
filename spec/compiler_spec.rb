require_relative './spec_helper'

describe Compiler do
  def d(instructions)
    subject.pretty_format(instructions)
  end

  describe '#compile' do
    context 'nil' do
      before do
        @result = subject.compile([
          'foo',
          nil,
          'bar'
        ])
      end

      it 'ignores nil' do
        expect(d(@result)).to eq([
          'VM::PUSH_REMOTE', 'foo',
          'VM::POP',
          'VM::PUSH_REMOTE', 'bar',
          'VM::HALT'
        ])
      end
    end

    context 'identifier' do
      before do
        @result = subject.compile([
          'foo',
          'bar baz'
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_REMOTE', 'foo',
          'VM::POP',
          'VM::PUSH_REMOTE', 'bar baz',
          'VM::HALT'
        ])
      end
    end

    context 'number' do
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
          '#t',
          '#true'
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_TRUE',
          'VM::POP',
          'VM::PUSH_TRUE',
          'VM::HALT'
        ])
      end
    end

    context '#f' do
      before do
        @result = subject.compile([
          '#f',
          '#false'
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_FALSE',
          'VM::POP',
          'VM::PUSH_FALSE',
          'VM::HALT'
        ])
      end
    end

    context '"a string"' do
      before do
        @result = subject.compile([
          '"a string"'
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_STR', 'a string',
          'VM::HALT'
        ])
      end
    end

    context 'a . pair' do
      before do
        @result = subject.compile([
          ['quote', ['1', '.', '2']]
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_CONS',
          'VM::HALT'
        ])
      end
    end

    context 'character #\c' do
      before do
        @result = subject.compile([
          '#\c',
          '#\space',
          '#\newline'
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_CHAR', 'c',
          'VM::POP',
          'VM::PUSH_CHAR', ' ',
          'VM::POP',
          'VM::PUSH_CHAR', "\n",
          'VM::HALT'
        ])
      end
    end

    context 'begin' do
      before do
        @result = subject.compile([
          ['begin', ['write', '1'], ['write', '2']]
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::INT', VM::INT_WRITE,
          'VM::PUSH_NUM', '2',
          'VM::INT', VM::INT_WRITE,
          'VM::HALT'
        ])
      end
    end

    context 'string-ref' do
      before do
        @result = subject.compile([
          ['string-ref', '"hello world"', '4']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_STR', 'hello world',
          'VM::PUSH_NUM', '4',
          'VM::STR_REF',
          'VM::HALT'
        ])
      end
    end

    context 'string-length' do
      before do
        @result = subject.compile([
          ['string-length', '"hello world"']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_STR', 'hello world',
          'VM::STR_LEN',
          'VM::HALT'
        ])
      end
    end

    context 'list->string' do
      before do
        @result = subject.compile([
          ['list->string', ['list', '#\a', '#\b']]
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_CHAR', 'a',
          'VM::PUSH_CHAR', 'b',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',
          'VM::LIST_TO_STR',
          'VM::HALT'
        ])
      end
    end

    context 'append' do
      before do
        @result = subject.compile([
          ['append', ['list', '1', '2'], ['list', '3', '4']]
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',
          'VM::PUSH_NUM', '3',
          'VM::PUSH_NUM', '4',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',
          'VM::PUSH_NUM', 2,
          'VM::APPEND',
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
          ['null?', ['list']]
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

    context 'variables' do
      before do
        @result = subject.compile([
          ['define', 'y', '10'],
          ['set!', 'y', '11'],
          ['set!', 'x', '9'],
          'y',
          'x'
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '10',
          'VM::SET_LOCAL', 'y',
          'VM::PUSH_NUM', '11',
          'VM::SET_LOCAL', 'y',
          'VM::PUSH_NUM', '9',
          'VM::SET_REMOTE', 'x',
          'VM::PUSH_LOCAL', 'y',
          'VM::POP',
          'VM::PUSH_REMOTE', 'x',
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
            ['quote', ['foo', '2', '3', ['write', '4']]]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_ATOM', 'foo',
            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '3',
            'VM::PUSH_ATOM', 'write',
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
            ['quasiquote', ['foo', '2', '3', ['write', '4']]]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_ATOM', 'foo',
            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '3',
            'VM::PUSH_ATOM', 'write',
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

      context 'given a list containing an unquoted variable' do
        before do
          @result = subject.compile([
            ['define', 'foo', '2'],
            ['quasiquote', ['list', '1', ['unquote', 'foo']]]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_NUM', '2',
            'VM::SET_LOCAL', 'foo',
            'VM::PUSH_ATOM', 'list',
            'VM::PUSH_NUM', '1',
            'VM::PUSH_LOCAL', 'foo',
            'VM::PUSH_NUM', 3,   # arg count
            'VM::PUSH_LIST',     # ['list', '1', '2']
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

    context 'write' do
      before do
        @result = subject.compile([
          ['write', '1']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::INT', VM::INT_WRITE,
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
            ['write', ['one']]
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
            'VM::INT', VM::INT_WRITE,
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

    context 'apply' do
      before do
        @result = subject.compile([
          ['apply', 'foo', ['list', '1', '2']]
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', 2, # arg count
          'VM::PUSH_LIST',
          'VM::PUSH_NUM', 1, # arg count
          'VM::SET_ARGS',
          'VM::PUSH_REMOTE', 'foo',
          'VM::APPLY',
          'VM::HALT'
        ])
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

    context 'eq?' do
      before do
        @result = subject.compile([
          ['eq?', '1', '1']
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

    context 'eqv?' do
      before do
        @result = subject.compile([
          ['eqv?', '1', '1']
        ])
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '1',
          'VM::CMP_EQV',
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
          'VM::CMP_EQ_NUM',
          'VM::HALT'
        ])
      end
    end

    context 'if' do
      context 'given value is not used' do
        before do
          @result = subject.compile([
            ['if', '#t', '2', '3'],
            '0'
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_TRUE',
            'VM::JUMP_IF_FALSE', 5,
            'VM::PUSH_NUM', '2',
            'VM::JUMP', 3,
            'VM::PUSH_NUM', '3',
            'VM::POP',
            'VM::PUSH_NUM', '0',
            'VM::HALT'
          ])
        end
      end

      context 'given value is used' do
        before do
          @result = subject.compile([
            ['write', ['if', '#t', '2', '3']]
          ])
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_TRUE',
            'VM::JUMP_IF_FALSE', 5,
            'VM::PUSH_NUM', '2',
            'VM::JUMP', 3,
            'VM::PUSH_NUM', '3',
            'VM::INT', VM::INT_WRITE,
            'VM::HALT'
          ])
        end
      end
    end

    context 'include' do
      context 'given a library name' do
        before do
          @result = subject.compile([
            ['include', '"list"']
          ])
        end

        it 'includes the code' do
          expect(@result.size).to be > 20
        end
      end
    end
  end
end
