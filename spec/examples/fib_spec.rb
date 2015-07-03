require_relative '../spec_helper'

describe 'Fib' do
  let(:code) do
    <<-END
      (def fib
        (fn (n)
          (if (< n 2)
              n,
              (+
                (fib (- n 1))
                (fib (- n 2))))))
      (fib 8)
    END
  end

  it 'returns 21' do
    result = Program.new(code).run
    expect(result).to eq(21)
  end
end
