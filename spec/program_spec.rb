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

    context do
      let(:code) do
        <<-END
          (import (only (scheme base) write-string))
          (write-string 1)
        END
      end

      it 'runs the program' do
        subject.run
        stdout.rewind
        expect(stdout.read).to eq('1')
      end
    end

    context 'when the stack overflows' do
      let(:code) do
        <<-END
          (import (only (scheme base) define if < + -))
          (define (fib n)
            (if (< n 2)
                n
                (+
                  (fib (- n 1))
                  (fib (- n 2)))))
          (fib 1000)
        END
      end

      it 'returns 2 and prints the stack' do
        result = subject.run
        expect(result).to eq(2)
        stdout.rewind
        out = stdout.read
        expect(out).to match(/\AError: call stack too deep\n.+program_spec\.rb#\d+\n\s+\(fib \(- n 1\)\)\n\s+\^/)
      end
    end

    context 'exception in program code' do
      let(:code) do
        "; undefined variable\n" \
        '(foo)'
      end

      it 'shows the filename, line and column number' do
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

    context 'exception in macro in another file' do
      let(:code) do
        '(include "./fixtures/bad-macro")' \
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
  end
end
