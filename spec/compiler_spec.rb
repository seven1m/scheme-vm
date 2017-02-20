require_relative './spec_helper'

describe Compiler do
  before(:all) do
    base_compiler = described_class.new(filename: __FILE__)
    base_compiler.compile('(import (scheme base))')
    @compiler_cache = Marshal.dump(base_compiler)
  end

  subject { Marshal.load(@compiler_cache) }

  describe '#compile' do
    context 'number' do
      before do
        @result = subject.compile(<<-END)
          1
        END
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
      before do
        @result = subject.compile(<<-END)
          #t
          #true
        END
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
      before do
        @result = subject.compile(<<-END)
          #f
          #false
        END
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
      before do
        @result = subject.compile(<<-END)
          "a string"
        END
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
      before do
        @result = subject.compile(<<-END)
          (quote (1 . 2))
        END
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
      before do
        @result = subject.compile(<<-END)
          #\\c
          #\\space
          #\\newline
        END
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', 99,
          'VM::TO_CHAR',
          'VM::POP',
          'VM::PUSH_NUM', 32,
          'VM::TO_CHAR',
          'VM::POP',
          'VM::PUSH_NUM', 10,
          'VM::TO_CHAR',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'list->string' do
      before do
        @result = subject.compile(<<-END)
          (list->string (list #\\a #\\b))
        END
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
      before do
        @result = subject.compile(<<-END)
          (append (list 1 2) (list 3 4))
        END
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
      before do
        @result = subject.compile(<<-END)
          (car (list 1 2 3))
        END
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', '3',
          'VM::PUSH_NUM', 3,
          'VM::PUSH_LIST',
          'VM::CAR',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'cdr' do
      before do
        @result = subject.compile(<<-END)
          (cdr (list 1 2 3))
        END
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '2',
          'VM::PUSH_NUM', '3',
          'VM::PUSH_NUM', 3,
          'VM::PUSH_LIST',
          'VM::CDR',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'cons' do
      before do
        @result = subject.compile(<<-END)
          (cons 1 (list 2 3))
        END
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
      before do
        @result = subject.compile(<<-END)
          (set-car! (quote (1 . 2)) 3)
        END
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
      before do
        @result = subject.compile(<<-END)
          (set-cdr! (quote (1 . 2)) 3)
        END
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
      before do
        @result = subject.compile(<<-END)
          (null? (list))
        END
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
      before do
        @result = subject.compile(<<-END)
          (define x 8)
          ((lambda ()
            (define y 10)
            (set! y 11)
            (set! x 9)
            y))
          x
        END
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
      it 'fails to compile' do
        expect {
          subject.compile(<<-END)
            (lambda ()
              n)
            (define n 10)
          END
        }.to raise_error(VM::VariableUndefined)
      end
    end

    context 'quote' do
      context 'given a list' do
        before do
          @result = subject.compile(<<-END)
            (quote (foo 2 3 (write 4)))
          END
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
        before do
          @result = subject.compile(<<-END)
            (quote ())
          END
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
        before do
          @result = subject.compile(<<-END)
            (quote foo)
          END
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
        before do
          @result = subject.compile(<<-END)
            (quasiquote (foo 2 3 (write 4)))
          END
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
        before do
          @result = subject.compile(<<-END)
            (quasiquote (list 1 (unquote (+ 2 3))))
          END
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
        before do
          @result = subject.compile(<<-END)
            (quasiquote (list 1 (unquote-splicing (list 2 3))))
          END
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
        before do
          @result = subject.compile(<<-END)
            (define foo 2)
            (quasiquote (list 1 (unquote foo)))
          END
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
        before do
          @result = subject.compile(<<-END)
            (define x 1)
          END
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
        before do
          @result = subject.compile(<<-END)
            (define (fn x y)
              (list x y))
          END
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
        before do
          @result = subject.compile(<<-END)
            (define (fn . x)
              x)
          END
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
        before do
          @result = subject.compile(<<-END)
            (lambda ()
              (define x 1))
          END
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
        before do
          @result = subject.compile(<<-END)
            (define myfn
              (lambda ()
                (define x 1)))
          END
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
        before do
          @result = subject.compile(<<-END)
            (define one
              (lambda ()
                (lambda () 1)
                (define two
                  (lambda () 2))
                (lambda () 3)))
          END
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
        before do
          @result = subject.compile(<<-END)
            (lambda ()
              1
              2)
          END
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
        before do
          @result = subject.compile(<<-END)
            (define x
              (lambda ()
                1))
            (x)
          END
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
        before do
          @result = subject.compile(<<-END)
            (define x
              (lambda (y z) ()))
            (x 2 4)
          END
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
        before do
          @result = subject.compile(<<-END)
            (define x
              (lambda args ()))
            (x 2 4)
          END
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
        before do
          @result = subject.compile(<<-END)
            (define x
              (lambda (first second . rest) ()))
            (x 2 3 4 5)
          END
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
        before do
          @result = subject.compile(<<-END)
            ((lambda (x) x) 1)
          END
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
        before do
          @result = subject.compile(<<-END)
            (define x
              (lambda ()
                (x)))
          END
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
      before do
        @result = subject.compile(<<-END)
          (call/cc (lambda (k) 1))
        END
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
      before do
        @result = subject.compile(<<-END)
          (define (foo))
          (apply foo (list 1 2))
        END
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
      before do
        @result = subject.compile(<<-END)
          (list 1 2)
        END
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
      before do
        @result = subject.compile(<<-END)
          (eq? 1 1)
        END
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
        before do
          @result = subject.compile(<<-END)
            (if #t 2 3)
            0
          END
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
        before do
          @result = subject.compile(<<-END)
            (write-string (if #t 2 3))
          END
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
        before do
          @result = subject.compile(<<-END)
            (lambda ()
              (if #t 2 3))
          END
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
        before do
          @result = subject.compile(<<-END)
            (if #f #f)
          END
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
        before do
          @result = subject.compile(<<-END)
            (define local-foo 1)
            (define-syntax macro-foo (syntax-rules () ()))

            (define-syntax and
              (syntax-rules ()
                ((and) #t)))
            (and)
          END
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
        before do
          @result = subject.compile(<<-END)
            (lambda ()
              (define-syntax my-and
                (syntax-rules ()
                  ((my-and) #t)))
              (and))
          END
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
        before do
          @result = subject.compile(<<-END)
            (include "./fixtures/include-test")
            "hello from main"
          END
        end

        it 'includes the code' do
          expect(d(@result)).to eq([
            'VM::IMPORT_LIB', 'scheme/base', 'write-string', 'write-string',
            'VM::PUSH_STR', 'hello from include-test',
            'VM::PUSH_NUM', 1,
            'VM::SET_ARGS',
            'VM::PUSH_VAR', 'write-string',
            'VM::CALL',
            'VM::PUSH_STR', 'hello from main',
            'VM::POP',
            'VM::HALT'
          ])
        end
      end

      context 'given a path to a library' do
        it 'does not import macros into the current namespace' do
          expect {
            subject.compile('(include "./fixtures/library-test") (macro)')
          }.to raise_error(VM::VariableUndefined)
        end
      end
    end

    context 'exit' do
      context 'given no arguments' do
        before do
          @result = subject.compile(<<-END)
            (import (only (scheme process-context) exit))
            (exit)
          END
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::HALT',
            'VM::HALT'
          ])
        end
      end

      context 'given an integer argument' do
        before do
          @result = subject.compile(<<-END)
            (import (only (scheme process-context) exit))
            (exit 10)
          END
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
      before do
        @result = subject.compile(<<-END)
          (define-syntax foo
            (syntax-rules ()
              ((foo ((name1 val1) ...))
                (list
                  (list "names" (quote name1)) ...
                  (list "vals" (quote val1)) ...))))
          (foo ((x "foo") (y "bar") (z "baz")))
        END
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
      before do
        @result = subject.compile(<<-END)
          (define-library (my-lib 1)
            (import (only (scheme base) define define-syntax))
            (begin
              (define foo "foo")
              (define-syntax macro1
                (syntax-rules ()
                  ((macro1) 1))))
            (export foo macro1))
          (define-library (my-lib 2)
            (import (only (scheme base) define define-syntax))
            (import (my-lib 1))
            (begin
              (define baz "baz")
              (define-syntax macro2
                (syntax-rules ()
                  ((macro2) macro1))))
            (export foo baz macro2 (rename foo bar)))
          (import (only (scheme base) define))
          (import (my-lib 1) (my-lib 2))
          (import (only (my-lib 1) foo))
          (import (only (my-lib 1) bar)) ; wrong name
          (import (prefix (my-lib 1) my-))
          (import (rename (my-lib 1) (foo baz)))
          (import (except (my-lib 2) bar))
          (import (rename (prefix (except (my-lib 2) bar) my-) (my-foo my-baz)))
        END
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
        expect(d(@result, skip_libs: false)).to eq([
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
        ])
      end
    end
  end

  context 'define-syntax and syntax-rules' do
    context 'given a template with two arguments and a nested template' do
      before do
        @result = subject.compile(<<-END)
          (define-syntax listify
            (syntax-rules ()
              ((listify first second) (list (list first) (list second)))))
          (listify 1 2)
        END
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
      before do
        @result = subject.compile(<<-END)
          (define-syntax and
            (syntax-rules ()
              ((and) #t)
              ((and test) test)
              ((and test1 test2 ...)
                (if test1 (and test2 ...) #f))))
          (and 1 2 3)
        END
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
      before do
        @result = subject.compile(<<-END)
          (define-syntax let
            (syntax-rules ()
              ((let ((name val) ...) body1 body2 ...)
                ((lambda (name ...) body1 body2 ...)
                val ...))))
          (let ((x 1) (y 2) (z 3)) (list x y z))
        END
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
