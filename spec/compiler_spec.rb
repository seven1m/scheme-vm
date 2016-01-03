require_relative './spec_helper'

describe Compiler do
  subject { described_class.new(filename: __FILE__) }

  def d(instructions)
    subject.pretty_format(instructions)
  end

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
          'VM::PUSH_STR', 'a string',
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
          'VM::PUSH_CONS',
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
          'VM::PUSH_CHAR', 'c',
          'VM::POP',
          'VM::PUSH_CHAR', ' ',
          'VM::POP',
          'VM::PUSH_CHAR', "\n",
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'string-ref' do
      before do
        @result = subject.compile(<<-END)
          (string-ref "hello world" 4)
        END
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_STR', 'hello world',
          'VM::PUSH_NUM', '4',
          'VM::STR_REF',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'string-length' do
      before do
        @result = subject.compile(<<-END)
          (string-length "hello world")
        END
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_STR', 'hello world',
          'VM::STR_LEN',
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
          'VM::PUSH_CHAR', 'a',
          'VM::PUSH_CHAR', 'b',
          'VM::PUSH_NUM', 2,
          'VM::PUSH_LIST',
          'VM::LIST_TO_STR',
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
          'VM::PUSH_CAR',
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
          'VM::PUSH_CDR',
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
          'VM::PUSH_CONS',
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
          'VM::PUSH_CONS',
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
          'VM::PUSH_CONS',
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
          (define y 10)
          (set! y 11)
          (set! x 9)
          y
          x
        END
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
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context 'remote variable' do
      before do
        @result = subject.compile(<<-END)
          (define n 10)
          (lambda ()
            n)
        END
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '10',
          'VM::SET_LOCAL', 'n',
          'VM::PUSH_FUNC',
          'VM::PUSH_REMOTE', 'n',
          'VM::RETURN',
          'VM::ENDF',
          'VM::POP',
          'VM::HALT'
        ])
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

      context 'given an atom' do
        before do
          @result = subject.compile(<<-END)
            (quote foo)
          END
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
            'VM::SET_LOCAL', 'foo',
            'VM::PUSH_ATOM', 'list',
            'VM::PUSH_NUM', '1',
            'VM::PUSH_LOCAL', 'foo',
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
            'VM::SET_LOCAL', 'x',
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
            'VM::SET_LOCAL', 'x',
            'VM::PUSH_ARG',
            'VM::SET_LOCAL', 'y',
            'VM::PUSH_LOCAL', 'x',
            'VM::PUSH_LOCAL', 'y',
            'VM::PUSH_NUM', 2,
            'VM::PUSH_LIST',
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 'fn',
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
            'VM::SET_LOCAL', 'x',
            'VM::PUSH_LOCAL', 'x',
            'VM::RETURN',
            'VM::ENDF',
            'VM::SET_LOCAL', 'fn',
            'VM::HALT'
          ])
        end
      end
    end

    context 'write' do
      before do
        @result = subject.compile(<<-END)
          (write 1)
        END
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
          @result = subject.compile(<<-END)
            (lambda ()
              (define x 1))
          END
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_FUNC',
            'VM::PUSH_NUM', '1',
            'VM::SET_LOCAL', 'x',
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
            'VM::SET_LOCAL', 'x',
            'VM::PUSH_LOCAL', 'x',
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
          @result = subject.compile(<<-END)
            (define x
              (lambda ()
                (x)))
          END
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
        @result = subject.compile(<<-END)
          (apply foo (list 1 2))
        END
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

    context 'eqv?' do
      before do
        @result = subject.compile(<<-END)
          (eqv? 1 1)
        END
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '1',
          'VM::CMP_EQV',
          'VM::POP',
          'VM::HALT'
        ])
      end
    end

    context '=' do
      before do
        @result = subject.compile(<<-END)
          (= 1 1)
        END
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::PUSH_NUM', '1',
          'VM::PUSH_NUM', '1',
          'VM::CMP_EQ_NUM',
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
            (write (if #t 2 3))
          END
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
            locals: ['local-foo', 'macro-foo', 'and']
          )
        end

        it 'compiles into vm instructions' do
          expect(d(@result)).to eq([
            'VM::PUSH_NUM', '1',
            'VM::SET_LOCAL', 'local-foo',
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
              (define-syntax and
                (syntax-rules ()
                  ((and) #t)))
              (and))
          END
        end

        it 'does not store the transformer in the top-level syntax accessor' do
          expect(subject.syntax).to eq({})
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
            (print "hello from main")
          END
        end

        it 'includes the code' do
          expect(d(@result)).to eq([
            'VM::PUSH_STR', 'hello from include-test',
            'VM::INT', 1,
            'VM::PUSH_STR', 'hello from main',
            'VM::PUSH_NUM', 1,
            'VM::SET_ARGS',
            'VM::PUSH_REMOTE', 'print',
            'VM::CALL',
            'VM::HALT'
          ])
        end
      end
    end

    context 'exit' do
      context 'given no arguments' do
        before do
          @result = subject.compile(<<-END)
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

    context 'define-library' do
      before do
        @result = subject.compile(<<-END)
          (define-library (my-lib 1)
            (begin
              (define foo "foo"))
            (export foo))
        END
      end

      it 'records export names for the library' do
        expect(subject.libs).to eq({
          'my-lib/1' => ['foo']
        })
      end

      it 'compiles into vm instructions' do
        expect(d(@result)).to eq([
          'VM::SET_LIB', 'my-lib/1',
          'VM::PUSH_STR', 'foo',
          'VM::SET_LOCAL', 'foo',
          'VM::ENDL',
          'VM::HALT'
        ])
      end
    end
  end
end
