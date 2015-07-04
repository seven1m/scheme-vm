require_relative './spec_helper'

describe Parser do
  describe '#parse' do
    before do
      @result = subject.parse('(if (< 1 2)) n (foo (bar (baz quz)))')
    end

    it 'parses s-expressions' do
      expect(@result).to eq([
        ['if', ['<', '1', '2']], 'n', ['foo', ['bar', ['baz', 'quz']]]
      ])
    end
  end
end
