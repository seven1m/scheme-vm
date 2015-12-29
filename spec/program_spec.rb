require_relative './spec_helper'
require 'stringio'

describe Program do
  let(:stdout) { StringIO.new }

  describe '#run' do
    subject do
      described_class.new(
        code,
        filename: 'program_spec.rb',
        stdout: stdout
      )
    end

    context do
      let(:code) do
        <<-END
          (write 1)
        END
      end

      it 'runs the program' do
        subject.run
        stdout.rewind
        expect(stdout.read).to eq('1')
      end
    end

    context 'exception' do
      let(:code) do
        "; undefined variable\n" \
        '(foo)'
      end

      it 'shows the filename, line and column number' do
        subject.run
        stdout.rewind
        expect(stdout.read).to eq(
          "Error: foo is not defined\n\n" \
            "program_spec.rb#2\n\n" \
            "  ; undefined variable\n" \
            "  (foo)\n" \
            "   ^ foo is not defined\n"
        )
      end
    end
  end
end
