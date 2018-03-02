require_relative './spec_helper'
require 'stringio'

describe Program do
  let(:stdout) { StringIO.new }

  describe '#run' do
    subject do
      described_class.new(
        code,
        filename: __FILE__,
        stdout: stdout
      )
    end

    let(:import_exit) { '(import (only (scheme process-context) exit)) ' }

    context 'when the program successfully finishes' do
      let(:code) { '(import (only (scheme base) +)) (+ 1 1)' }

      it 'returns 0' do
        expect(subject.run).to eq(0)
      end
    end

    context 'when the program writes a string' do
      let(:code) do
        <<-END
          (import (only (scheme base) write-string))
          (write-string 1)
        END
      end

      it 'writes to stdout' do
        subject.run
        stdout.rewind
        expect(stdout.read).to eq('1')
      end
    end

    context 'when the program imports other libraries' do
      let(:code) do
        <<-END
          (define-library (base)
            (export
             define
             define-syntax
             if
             write-string)

            (begin
              (--define-native apply base_apply)
              (--define-native car base_car)
              (--define-native cdr base_cdr)
              (--define-native define base_define)
              (--define-native define-syntax base_define_syntax)
              (--define-native empty? base_null?)
              (--define-native if base_if)
              (--define-native lambda base_lambda)

              (define-syntax begin
                (syntax-rules ()
                  ((begin exp ...)
                  ((lambda () exp ...)))))

              (define not
                (lambda (condition)
                  (if condition
                      #f
                      #t)))

              (--define-native write write) ; don't export this

              (define (newline)
                (write #\\newline))

              (define (write-string . args)
                (if (not (empty? args))
                    (begin
                      (write (car args))
                      (apply write-string (cdr args)))))
            ))

          (define-library (my-lib 1)
            (import (base))
            (begin
              (define foo "foo")
              (define-syntax macro1
                (syntax-rules ()
                  ((macro1) (write-string "from my-lib 1")))))
            (export foo macro1))

          (define-library (my-lib 2)
            (import (base))
            (import (my-lib 1))
            (begin
              (define baz "baz")
              (define-syntax macro2
                (syntax-rules ()
                  ((macro2) (macro1)))))
            (export foo baz macro2 (rename foo bar)))

          (import (my-lib 2))

          (macro2)
        END
      end

      it 'allows one library to reference another one' do
        subject.run
        stdout.rewind
        expect(stdout.read).to eq('from my-lib 1')
      end
    end

    context 'when the program fails' do
      let(:code) { 'foo' }

      it 'returns 1' do
        expect(subject.run).to eq(1)
      end
    end

    describe 'exit' do
      context 'when the program calls (exit)' do
        let(:code) { import_exit + '(exit)' }

        it 'returns 0' do
          expect(subject.run).to eq(0)
        end
      end

      context 'when the program calls (exit #t)' do
        let(:code) { import_exit + '(exit #t)' }

        it 'returns 0' do
          expect(subject.run).to eq(0)
        end
      end

      context 'when the program calls (exit 10)' do
        let(:code) { import_exit + '(exit 10)' }

        it 'returns 10' do
          expect(subject.run).to eq(10)
        end
      end

      context 'when the program calls (exit #f)' do
        let(:code) { import_exit + '(exit #f)' }

        it 'returns 1' do
          expect(subject.run).to eq(1)
        end
      end
    end

    context 'when the stack overflows' do
      let(:code) do
        "(import (only (scheme base) define if < + -))\n" \
        "(define (fib n)\n" \
        "  (if (< n 2)\n" \
        "      n\n" \
        "      (+\n" \
        "        (fib (- n 1))\n" \
        "        (fib (- n 2)))))\n" \
        '(fib 3)'
      end

      it 'sets the exit code to 2' do
        expect(subject.run).to eq(2)
      end

      it 'returns 2 and prints the stack' do
        stub_const('VM::MAX_CALL_DEPTH', 3)
        subject.run
        stdout.rewind
        expect(stdout.read).to eq(
          "Error: call stack too deep\n" \
          "#{__FILE__}#6\n" \
          "          (fib (- n 1))\n" \
          "           ^\n" \
          "#{__FILE__}#6\n" \
          "          (fib (- n 1))\n" \
          "           ^\n" \
          "#{__FILE__}#8\n" \
          "  (fib 3)\n" \
          "   ^\n"
        )
      end
    end

    context 'when an undefined variable is referenced' do
      let(:code) do
        "; undefined variable\n" \
        '(foo)'
      end

      it 'sets the exit code to 1' do
        expect(subject.run).to eq(1)
      end

      it 'shows the filename, line and column of the error' do
        subject.run
        stdout.rewind
        expect(stdout.read).to eq(
          "Error: foo is not defined\n\n" \
            "#{__FILE__}#2\n\n" \
            "  ; undefined variable\n" \
            "  (foo)\n" \
            "   ^ foo is not defined\n"
        )
      end
    end

    context 'when an undefined variable is referenced on line 1 with weird spacing' do
      let(:code) do
        ' ( foo)'
      end

      it 'shows the cursor at the proper position' do
        subject.run
        stdout.rewind
        expect(stdout.read).to eq(
          "Error: foo is not defined\n\n" \
            "#{__FILE__}#1\n\n" \
            "   ( foo)\n" \
            "     ^ foo is not defined\n"
        )
      end
    end

    context 'exception in macro in another file' do
      let(:code) do
        '(include "./fixtures/bad-macro") ' \
        '(bad-macro)'
      end

      it 'shows the filename, line and column number' do
        subject.run
        stdout.rewind
        expect(stdout.read).to eq(
          "Error: foo is not defined\n\n" \
            "./fixtures/bad-macro.scm#5\n\n" \
            "    (syntax-rules ()\n" \
            "      ((bad-macro) (foo))))\n" \
            "                    ^ foo is not defined\n"
        )
      end
    end

    context 'when a syntax error occurs' do
      let(:code) do
        "(foo)\n" \
        "\n" \
        '      (()'
      end

      it 'sets the exit code to 3' do
        expect(subject.run).to eq(3)
      end

      it 'shows the filename, line and column of the error' do
        subject.run
        stdout.rewind
        expected = ["\"", "#;", "#|", "'", "(", ")", ",", ",@", ";", "[ \t\n]", "[^() \t\n[]{}|\"]", "`", "|"]
        expect(stdout.read).to eq(
          "Syntax Error:\n\n" \
            "#{__FILE__}#3\n\n" \
            "  \n" \
            "        (()\n" \
            "           ^ expected one of: #{expected.inspect}\n"
        )
      end
    end
  end
end
