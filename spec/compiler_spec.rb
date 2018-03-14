require_relative './spec_helper'

describe Compiler do
  let(:ast)     { [] }
  let(:out)     { StringIO.new }
  let(:program) { Program.new('', stdout: out) }

  subject { described_class.new(ast, filename: __FILE__, program: program) }

  before do
    @result = subject.compile
  end

  describe '#compile' do
    context 'number' do
      let(:ast) do
        [
          '1'
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context '#t' do
      let(:ast) do
        [
          '#t',
          '#true'
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_TRUE',
          'VM::POP',
          'VM::PUSH_TRUE',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context '#f' do
      let(:ast) do
        [
          '#f',
          '#false'
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_FALSE',
          'VM::POP',
          'VM::PUSH_FALSE',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context '"a string"' do
      let(:ast) do
        [
          '"a string"'
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_STR',
          'a string',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'a . pair' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['quote', ['1', '.', '2']]
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::CONS',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'character #\c' do
      let(:ast) do
        [
          '#\c',
          '#\space',
          '#\newline',
          '#\alarm',
          '#\backspace',
          '#\delete',
          '#\escape',
          '#\null',
          '#\return',
          '#\tab'
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', 99,  'VM::TO_CHAR', 'VM::POP',
          'VM::PUSH_NUM', 32,  'VM::TO_CHAR', 'VM::POP',
          'VM::PUSH_NUM', 10,  'VM::TO_CHAR', 'VM::POP',
          'VM::PUSH_NUM', 7,   'VM::TO_CHAR', 'VM::POP',
          'VM::PUSH_NUM', 8,   'VM::TO_CHAR', 'VM::POP',
          'VM::PUSH_NUM', 127, 'VM::TO_CHAR', 'VM::POP',
          'VM::PUSH_NUM', 27,  'VM::TO_CHAR', 'VM::POP',
          'VM::PUSH_NUM', 0,   'VM::TO_CHAR', 'VM::POP',
          'VM::PUSH_NUM', 13,  'VM::TO_CHAR', 'VM::POP',
          'VM::PUSH_NUM', 9,   'VM::TO_CHAR', 'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'list->string' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['list->string', ['list', '#\a', '#\b']]
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', 97,
          'VM::TO_CHAR',
          'VM::PUSH_NUM', 98,
          'VM::TO_CHAR',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',
          'VM::TO_STR',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'append' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['append', ['list', '1', '2'], ['list', '3', '4']]
        ]
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
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'car' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['car', ['list', '1', '2', '3']]
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', '3',
          'VM::PUSH_NUM', 3,
          'VM::PUSH_LIST',
          'VM::PUSH_NUM', 1,
          'VM::SET_ARGS',
          'VM::PUSH_VAR', 'car',
          'VM::CALL',
          'VM::HALT'
        ])
      end
    end

    context 'cdr' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['cdr', ['list', '1', '2', '3']]
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', '3',
          'VM::PUSH_NUM', 3,
          'VM::PUSH_LIST',
          'VM::PUSH_NUM', 1,
          'VM::SET_ARGS',
          'VM::PUSH_VAR', 'cdr',
          'VM::CALL',
          'VM::HALT'
        ])
      end
    end

    context 'cons' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['cons', '1', ['list', '2', '3']]
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', '3',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',
          'VM::CONS',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'set-car!' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['set-car!', ['quote', ['1', '.', '2']], '3']
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::CONS',
          'VM::PUSH_NUM', '3',
          'VM::SET_CAR',
          'VM::HALT'
        ])
      end
    end

    context 'set-cdr!' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['set-cdr!', ['quote', ['1', '.', '2']], '3']
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::CONS',
          'VM::PUSH_NUM', '3',
          'VM::SET_CDR',
          'VM::HALT'
        ])
      end
    end

    context 'null?' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['null?', ['list']]
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', 0,
          'VM::PUSH_LIST',
          'VM::CMP_NULL',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'variables' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['define', 'x', '8'],
          [['lambda', [],
            ['define', 'y', '10'],
            ['set!', 'y', '11'],
            ['set!', 'x', '9'],
            'y']],
          'x'
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '8',
          'VM::DEFINE_VAR', 'x',
          'VM::PUSH_FUNC',
          'VM::PUSH_NUM', '10',
          'VM::DEFINE_VAR', 'y',
          'VM::PUSH_NUM', '11',
          'VM::SET_VAR', 'y',
          'VM::PUSH_NUM', '9',
          'VM::SET_VAR', 'x',
          'VM::PUSH_VAR', 'y',
          'VM::RETURN',
          'VM::ENDF',
          'VM::CALL',
          'VM::PUSH_VAR', 'x',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'variable out of scope' do
      let(:erroring_ast) do
        [
          [VM::Atom.new('include', __FILE__), '"./fixtures/library-test.scm"'],
          ['lambda', [],
            'n'],
          ['define', 'n', '10']
        ]
      end

      it 'fails to compile' do
        compiler = described_class.new(erroring_ast, filename: __FILE__, program: program)
        expect { compiler.compile }.to raise_error(VM::VariableUndefined)
      end
    end

    context 'quote' do
      context 'given a list' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['quote', ['foo', '2', '3', ['write', '4']]]
          ]
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
            'VM::POP',
            'VM::HALT'
          ])
        end
      end

      context 'given an empty list' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['quote', []]
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_NUM', 0,
            'VM::PUSH_LIST',
            'VM::POP',
            'VM::HALT'
          ])
        end
      end

      context 'given an atom' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['quote', 'foo']
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_ATOM', 'foo',
            'VM::POP',
            'VM::HALT'
          ])
        end
      end
    end

    context 'quasiquote' do
      context 'given a simple list' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['quasiquote', ['foo', '2', '3', ['write', '4']]]
          ]
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
            'VM::POP',
            'VM::HALT'
          ])
        end
      end

      context 'given a list containing an unquote expression' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['quasiquote', ['list', '1', ['unquote', ['+', '2', '3']]]]
          ]
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
            'VM::POP',
            'VM::HALT'
          ])
        end
      end

      context 'given a list containing an unquote-splicing expression' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['quasiquote', ['list', '1', ['unquote-splicing', ['list', '2', '3']]]]
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_ATOM', 'list',
            'VM::PUSH_NUM', '1',
            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '3',
            'VM::PUSH_NUM', 4,   # arg count
            'VM::PUSH_LIST',     # ['list', '1', '2', '3']
            'VM::POP',
            'VM::HALT'
          ])
        end
      end

      context 'given a list containing an unquoted variable' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['define', 'foo', '2'],
            ['quasiquote', ['list', '1', ['unquote', 'foo']]]
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_NUM', '2',
            'VM::DEFINE_VAR', 'foo',
            'VM::PUSH_ATOM', 'list',
            'VM::PUSH_NUM', '1',
            'VM::PUSH_VAR', 'foo',
            'VM::PUSH_NUM', 3,   # arg count
            'VM::PUSH_LIST',     # ['list', '1', '2']
            'VM::POP',
            'VM::HALT'
          ])
        end
      end
    end

    context 'define' do
      context '<variable> <expression>' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['define', 'x', '1']
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_NUM', '1',
            'VM::DEFINE_VAR', 'x',
            'VM::HALT'
          ])
        end
      end

      context '(<variable> <formals>) <body>' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['define', ['fn', 'x', 'y'],
              ['list', 'x', 'y']]
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_ARG',
            'VM::NAME_ARG', 'x',
            'VM::PUSH_ARG',
            'VM::NAME_ARG', 'y',
            'VM::PUSH_VAR', 'x',
            'VM::PUSH_VAR', 'y',
            'VM::PUSH_NUM', 2,
            'VM::PUSH_LIST',
            'VM::RETURN',
            'VM::ENDF',
            'VM::DEFINE_VAR', 'fn',
            'VM::HALT'
          ])
        end
      end

      context '(<variable> . <formal>) <body>' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['define', ['fn', '.', 'x'],
              'x']
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_ARGS',
            'VM::NAME_ARG', 'x',
            'VM::PUSH_VAR', 'x',
            'VM::RETURN',
            'VM::ENDF',
            'VM::DEFINE_VAR', 'fn',
            'VM::HALT'
          ])
        end
      end
    end

    context 'lambda' do
      context 'not storing in a variable or passing to a function' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['lambda', [],
              ['define', 'x', '1']]
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_NUM', '1',
            'VM::DEFINE_VAR', 'x',
            'VM::RETURN',
            'VM::ENDF',
            'VM::POP',
            'VM::HALT'
          ])
        end
      end

      context 'storing in a variable' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['define', 'myfn',
              ['lambda', [],
                ['define', 'x', '1']]]
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_NUM', '1',
            'VM::DEFINE_VAR', 'x',
            'VM::RETURN',
            'VM::ENDF',
            'VM::DEFINE_VAR', 'myfn',
            'VM::HALT'
          ])
        end
      end

      context 'mixed variable storage' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['define', 'one',
              ['lambda', [],
                ['lambda', [], '1'],
                ['define', 'two',
                  ['lambda', [], '2']],
                ['lambda', [], '3']]]
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',         # (lambda
            'VM::PUSH_FUNC',         #   (lambda
            'VM::PUSH_NUM', '1',     #     1
            'VM::RETURN',            #   <return>
            'VM::ENDF',              #   )
            'VM::POP',               #   <pop (lambda 1)>
            'VM::PUSH_FUNC',         #   (lambda
            'VM::PUSH_NUM', '2',     #     2
            'VM::RETURN',            #   <return>
            'VM::ENDF',              #   )
            'VM::DEFINE_VAR', 'two', #   (def two ...)
            'VM::PUSH_FUNC',         #   (lambda
            'VM::PUSH_NUM', '3',     #     3
            'VM::RETURN',            #   <return>
            'VM::ENDF',              #   )
            'VM::RETURN',            # <return>
            'VM::ENDF',              # )
            'VM::DEFINE_VAR', 'one', # (def one ...)
            'VM::HALT'
          ])
        end
      end

      context 'with return value' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['lambda', [],
              '1',
              '2']
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_NUM', '1',
            'VM::POP',
            'VM::PUSH_NUM', '2',
            'VM::RETURN',
            'VM::ENDF',
            'VM::POP',
            'VM::HALT'
          ])
        end
      end
    end

    context 'call' do
      context 'without args' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['define', 'x',
              ['lambda', [],
                '1']],
            ['x']
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_NUM', '1',
            'VM::RETURN',
            'VM::ENDF',
            'VM::DEFINE_VAR', 'x',
            'VM::PUSH_VAR', 'x',
            'VM::CALL',
            'VM::HALT'
          ])
        end
      end

      context 'with args' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['define', 'x',
              ['lambda', ['y', 'z'], []]],
            ['x', '2', '4']
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_ARG',
            'VM::NAME_ARG', 'y',
            'VM::PUSH_ARG',
            'VM::NAME_ARG', 'z',
            'VM::RETURN',
            'VM::ENDF',
            'VM::DEFINE_VAR', 'x',

            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '4',
            'VM::PUSH_NUM', 2,    # arg count
            'VM::SET_ARGS',
            'VM::PUSH_VAR', 'x',
            'VM::CALL',
            'VM::HALT'
          ])
        end
      end

      context 'with variable args' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['define', 'x',
              ['lambda', 'args', []]],
            ['x', '2', '4']
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_ARGS',
            'VM::NAME_ARG', 'args',
            'VM::RETURN',
            'VM::ENDF',
            'VM::DEFINE_VAR', 'x',

            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '4',
            'VM::PUSH_NUM', 2,    # arg count
            'VM::SET_ARGS',
            'VM::PUSH_VAR', 'x',
            'VM::CALL',
            'VM::HALT'
          ])
        end
      end

      context 'with destructuring args' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['define', 'x',
              ['lambda', ['first', 'second', '.', 'rest'], []]],
            ['x', '2', '3', '4', '5']
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_ARG',
            'VM::NAME_ARG', 'first',
            'VM::PUSH_ARG',
            'VM::NAME_ARG', 'second',
            'VM::PUSH_ARGS',
            'VM::NAME_ARG', 'rest',
            'VM::RETURN',
            'VM::ENDF',
            'VM::DEFINE_VAR', 'x',

            'VM::PUSH_NUM', '2',
            'VM::PUSH_NUM', '3',
            'VM::PUSH_NUM', '4',
            'VM::PUSH_NUM', '5',
            'VM::PUSH_NUM', 4,    # arg count
            'VM::SET_ARGS',
            'VM::PUSH_VAR', 'x',
            'VM::CALL',
            'VM::HALT'
          ])
        end
      end

      context 'calling immediately' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            [['lambda', ['x'], 'x'], '1']
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_NUM', '1',
            'VM::PUSH_NUM', 1,
            'VM::SET_ARGS',
            'VM::PUSH_FUNC',
            'VM::PUSH_ARG',
            'VM::NAME_ARG', 'x',
            'VM::PUSH_VAR', 'x',
            'VM::RETURN',
            'VM::ENDF',
            'VM::CALL',
            'VM::HALT'
          ])
        end
      end

      context 'calling self' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['define', 'x',
              ['lambda', [],
                ['x']]]
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_VAR', 'x',
            'VM::CALL',
            'VM::RETURN',
            'VM::ENDF',
            'VM::DEFINE_VAR', 'x',
            'VM::HALT'
          ])
        end
      end
    end

    context 'call/cc' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['call/cc', ['lambda', ['k'], '1']]
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_FUNC',
          'VM::PUSH_ARG',
          'VM::NAME_ARG', 'k',
          'VM::PUSH_NUM', '1',
          'VM::RETURN',
          'VM::ENDF',
          'VM::CALL_WITH_CC',
          'VM::HALT'
        ])
      end
    end

    context 'apply' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['define', ['foo']],
          ['apply', 'foo', ['list', '1', '2']]
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_FUNC',
          'VM::RETURN',
          'VM::ENDF',
          'VM::DEFINE_VAR', 'foo',
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', 2, # arg count
          'VM::PUSH_LIST',
          'VM::PUSH_NUM', 1, # arg count
          'VM::SET_ARGS',
          'VM::PUSH_VAR', 'foo',
          'VM::APPLY',
          'VM::HALT'
        ])
      end
    end

    context 'list' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['list', '1', '2']
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', 2, # arg count
          'VM::PUSH_LIST',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'eq?' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['eq?', '1', '1']
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '1',
          'VM::CMP_EQ',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'if' do
      context 'given value is not used' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['if', '#t', '2', '3'],
            '0'
          ]
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
            'VM::POP',
            'VM::HALT'
          ])
        end
      end

      context 'given value is used' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['write-string', ['if', '#t', '2', '3']]
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to end_with([
            'VM::PUSH_TRUE',
            'VM::JUMP_IF_FALSE', 5,
            'VM::PUSH_NUM', '2',
            'VM::JUMP', 3,
            'VM::PUSH_NUM', '3',
            'VM::PUSH_NUM', 1,
            'VM::SET_ARGS',
            'VM::PUSH_VAR', 'write-string',
            'VM::CALL',
            'VM::HALT'
          ])
        end
      end

      context 'inside a function body' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['lambda', [],
              ['if', '#t', '2', '3']]
          ]
        end

        it 'compiles into vm instructions, optimizing out the JUMP with a RETURN' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_TRUE',
            'VM::JUMP_IF_FALSE', 5,
            'VM::PUSH_NUM', '2',
            'VM::RETURN',
            'VM::NOOP',
            'VM::PUSH_NUM', '3',
            'VM::RETURN',
            'VM::ENDF',
            'VM::POP',
            'VM::HALT'
          ])
        end
      end

      context 'without else' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['if', '#f', '#f']
          ]
        end

        it 'compiles into vm instructions, adding PUSH_UNDEF for else' do
          expect(d(@result)).to eq([
            'VM::PUSH_FALSE',
            'VM::JUMP_IF_FALSE', 4,
            'VM::PUSH_FALSE',
            'VM::JUMP', 2,
            'VM::PUSH_UNDEF',
            'VM::POP',
            'VM::HALT'
          ])
        end
      end
    end

    context 'define-syntax' do
      context 'at the top level' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['define', 'local-foo', '1'],
            ['define-syntax', 'macro-foo', ['syntax-rules', [], []]],

            ['define-syntax', 'and',
              ['syntax-rules', [],
                [['and'], '#t']]],
            ['and']
          ]
        end

        it 'stores the transformer in the top-level syntax accessor' do
          expect(subject.syntax['and']).to include(
            transformer: ['syntax-rules', [], [['and'], '#t']]
          )
        end

        it 'it stores bindings from the current scope and itself' do
          expect(subject.syntax['and']).to include(
            locals: include('local-foo', 'macro-foo', 'and')
          )
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_NUM', '1',
            'VM::DEFINE_VAR', 'local-foo',
            'VM::PUSH_TRUE',
            'VM::POP',
            'VM::HALT'
          ])
        end
      end

      context 'inside a lambda' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            ['lambda', [],
              ['define-syntax', 'my-and',
                ['syntax-rules', [],
                  [['my-and'], '#t']]],
              ['and']]
          ]
        end

        it 'does not store the transformer in the top-level syntax accessor' do
          expect(subject.syntax['my-and']).to be_nil
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_TRUE',
            'VM::RETURN',
            'VM::ENDF',
            'VM::POP',
            'VM::HALT'
          ])
        end
      end
    end

    context 'include' do
      context 'given a path' do
        let(:ast) do
          [
            [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
            [VM::Atom.new('include', __FILE__), '"./fixtures/include-test.scm"'],
            '"hello from main"'
          ]
        end

        it 'includes the code' do
          expected = [
            'VM::IMPORT_LIB', 'scheme/base', 'write-string', 'write-string',
            'VM::PUSH_STR', 'hello from include-test',
            'VM::PUSH_NUM', 1,
            'VM::SET_ARGS',
            'VM::PUSH_VAR', 'write-string',
            'VM::CALL',
            'VM::PUSH_STR', 'hello from main',
            'VM::POP',
            'VM::HALT'
          ]
          expect(d(@result, skip_libs: false)[-expected.size..-1]).to eq(expected)
        end
      end

      context 'given a path to a library' do
        let(:erroring_ast) do
          [
            [VM::Atom.new('include', __FILE__), '"./fixtures/library-test.scm"'],
            ['macro']
          ]
        end

        it 'does not import macros into the current namespace' do
          compiler = described_class.new(erroring_ast, filename: __FILE__, program: program)
          expect { compiler.compile }.to raise_error(VM::VariableUndefined)
        end
      end
    end

    context 'exit' do
      context 'given no arguments' do
        let(:ast) do
          [
            [VM::Atom.new('import', __FILE__), ['only', ['scheme', 'process-context'], 'exit']],
            ['exit']
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::HALT',
            'VM::HALT'
          ])
        end
      end

      context 'given an integer argument' do
        let(:ast) do
          [
            [VM::Atom.new('import', __FILE__), ['only', ['scheme', 'process-context'], 'exit']],
            ['exit', '10']
          ]
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_NUM', '10',
            'VM::HALT',
            'VM::HALT'
          ])
        end
      end
    end

    context 'macro expansion' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['define-syntax', 'foo',
            ['syntax-rules', [],
              [['foo', [['name1', 'val1'], '...']],
                ['list',
                  ['list', '"names"', ['quote', VM::Atom.new('name1')]], '...',
                  ['list', '"vals"', ['quote', VM::Atom.new('val1')]], '...']]]],
          ['foo', [['x', '"foo"'], ['y', '"bar"'], ['z', '"baz"']]]
        ]
      end

      it 'expands the macro' do
        expect(d(@result)).to eq([
          'VM::PUSH_STR', 'names',
          'VM::PUSH_ATOM', 'x',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',

          'VM::PUSH_STR', 'names',
          'VM::PUSH_ATOM', 'y',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',

          'VM::PUSH_STR', 'names',
          'VM::PUSH_ATOM', 'z',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',

          'VM::PUSH_STR', 'vals',
          'VM::PUSH_STR', 'foo',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',

          'VM::PUSH_STR', 'vals',
          'VM::PUSH_STR', 'bar',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',

          'VM::PUSH_STR', 'vals',
          'VM::PUSH_STR', 'baz',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',

          'VM::PUSH_NUM', 6,
          'VM::PUSH_LIST',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'define-library and import' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['define-library', [VM::Atom.new('my-lib', filename: ''), '1'],
            ['import', ['only', ['scheme', 'base'], 'define', 'define-syntax']],
            ['begin',
              ['define', 'foo', '"foo"'],
              ['define-syntax', 'macro1',
                ['syntax-rules', [],
                  [['macro1'], '1']]]],
            ['export', 'foo', 'macro1']],
          ['define-library', [VM::Atom.new('my-lib', filename: ''), '2'],
            ['import', ['only', ['scheme', 'base'], 'define', 'define-syntax']],
            ['import', ['my-lib', '1']],
            ['begin',
              ['define', 'baz', '"baz"'],
              ['define-syntax', 'macro2',
                ['syntax-rules', [],
                  [['macro2'], 'macro1']]]],
            ['export', 'foo', 'baz', 'macro2', ['rename', 'foo', 'bar']]],
          [VM::Atom.new('import', filename: ''), ['only', ['scheme', 'base'], 'define']],
          [VM::Atom.new('import', filename: ''), ['my-lib', '1'], ['my-lib', '2']],
          [VM::Atom.new('import', filename: ''), ['only', ['my-lib', '1'], 'foo']],
          [VM::Atom.new('import', filename: ''), ['only', ['my-lib', '1'], 'bar']], # wrong name
          [VM::Atom.new('import', filename: ''), ['prefix', ['my-lib', '1'], 'my-']],
          [VM::Atom.new('import', filename: ''), ['rename', ['my-lib', '1'], ['foo', 'baz']]],
          [VM::Atom.new('import', filename: ''), ['except', ['my-lib', '2'], 'bar']],
          [VM::Atom.new('import', filename: ''), ['rename',
                                                  ['prefix', ['except', ['my-lib', '2'], 'bar'], 'my-'],
                                                  ['my-foo', 'my-baz']]]
        ]
      end

      it 'records export names for the libraries' do
        expect(subject.libs['my-lib/1'][:syntax]['macro1']).to include(
          locals: Array,
          transformer: [
            'syntax-rules', [],
            [['macro1'], '1']
          ]
        )
        expect(subject.libs['my-lib/1'][:bindings]).to eq(
          'foo'    => 'foo',
          'macro1' => 'macro1'
        )
        expect(subject.libs['my-lib/2'][:syntax]['macro2']).to include(
          locals: Array,
          transformer: [
            'syntax-rules', [],
            [['macro2'], 'macro1']
          ]
        )
        expect(subject.libs['my-lib/2'][:bindings]).to eq(
          'foo'    => 'foo',
          'baz'    => 'baz',
          'bar'    => 'foo',
          'macro2' => 'macro2'
        )
      end

      it 'compiles into vm instructions' do
        expected = [
          'VM::SET_LIB', 'my-lib/1',
          'VM::PUSH_STR', 'foo',
          'VM::DEFINE_VAR', 'foo',
          'VM::ENDL',

          'VM::SET_LIB', 'my-lib/2',
          'VM::IMPORT_LIB', 'my-lib/1', 'foo', 'foo',
          'VM::PUSH_STR', 'baz',
          'VM::DEFINE_VAR', 'baz',
          'VM::ENDL',

          'VM::IMPORT_LIB', 'my-lib/1', 'foo', 'foo',
          'VM::IMPORT_LIB', 'my-lib/2', 'foo', 'foo',
          'VM::IMPORT_LIB', 'my-lib/2', 'baz', 'baz',
          'VM::IMPORT_LIB', 'my-lib/2', 'foo', 'bar',
          'VM::IMPORT_LIB', 'my-lib/1', 'foo', 'foo',
          'VM::IMPORT_LIB', 'my-lib/1', 'foo', 'my-foo',
          'VM::IMPORT_LIB', 'my-lib/1', 'foo', 'baz',
          'VM::IMPORT_LIB', 'my-lib/2', 'foo', 'foo',
          'VM::IMPORT_LIB', 'my-lib/2', 'baz', 'baz',
          'VM::IMPORT_LIB', 'my-lib/2', 'foo', 'my-baz',
          'VM::IMPORT_LIB', 'my-lib/2', 'baz', 'my-baz',

          'VM::HALT'
        ]
        expect(d(@result, skip_libs: false)[-expected.size..-1]).to eq(expected)
      end
    end
  end

  context 'define-syntax and syntax-rules' do
    context 'given a template with two arguments and a nested template' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['define-syntax', 'listify',
            ['syntax-rules', [],
             [['listify', 'first', 'second'], ['list', ['list', VM::Atom.new('first')], ['list', VM::Atom.new('second')]]]]],
          ['listify', '1', '2']
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', 1,
          'VM::PUSH_LIST',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', 1,
          'VM::PUSH_LIST',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'given multiple templates, recursive expansion' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['define-syntax', 'and',
            ['syntax-rules', [],
              [['and'], '#t'],
              [['and', 'test'], 'test'],
              [['and', 'test1', 'test2', '...'],
                 ['if', VM::Atom.new('test1'), ['and', VM::Atom.new('test2'), '...'], '#f']]]],
          ['and', '1', '2', '3']
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::JUMP_IF_FALSE', 12,
          'VM::PUSH_NUM', '2',
          'VM::JUMP_IF_FALSE', 5,
          'VM::PUSH_NUM', '3',
          'VM::JUMP', 2,
          'VM::PUSH_FALSE',
          'VM::JUMP', 2,
          'VM::PUSH_FALSE',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'given multiple templates, recursive expansion' do
      let(:ast) do
        [
          [VM::Atom.new('import', filename: ''), ['scheme', 'base']],
          ['define-syntax', 'let',
            ['syntax-rules', [],
              [['let', [['name', 'val'], '...'], 'body1', 'body2', '...'],
                [['lambda', [VM::Atom.new('name'), '...'], VM::Atom.new('body1'), VM::Atom.new('body2'), '...'],
                VM::Atom.new('val'), '...']]]],
          ['let', [['x', '1'], ['y', '2'], ['z', '3']], ['list', 'x', 'y', 'z']]
        ]
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', '3',
          'VM::PUSH_NUM', 3,
          'VM::SET_ARGS',
          'VM::PUSH_FUNC',
          'VM::PUSH_ARG',
          'VM::NAME_ARG', 'x',
          'VM::PUSH_ARG',
          'VM::NAME_ARG', 'y',
          'VM::PUSH_ARG',
          'VM::NAME_ARG', 'z',
          'VM::PUSH_VAR', 'x',
          'VM::PUSH_VAR', 'y',
          'VM::PUSH_VAR', 'z',
          'VM::PUSH_NUM', 3,
          'VM::PUSH_LIST',
          'VM::RETURN',
          'VM::ENDF',
          'VM::CALL',
          'VM::HALT'
        ])
      end
    end
  end
end
