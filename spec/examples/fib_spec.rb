require_relative '../spec_helper'

describe 'Fib' do
  let(:stdout) { StringIO.new }

  let(:code) do
    <<-END
      (def fib
        (fn (n)
          (if (< n 2)
              n
              (+
                (fib (- n 1))
                (fib (- n 2))))))
      (print (fib 8))
    END
  end

  let(:subject) { Program.new(code, stdout: stdout) }

  before { subject.run }

  it 'prints 21' do
    stdout.rewind
    expect(stdout.read).to eq('21')
  end
end
