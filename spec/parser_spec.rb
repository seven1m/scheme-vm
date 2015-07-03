require_relative './spec_helper'

describe Parser do
  describe '#parse' do
    before do
      @result = subject.parse('(foo (bar (baz quz)))')
    end

    it 'parses s-expressions' do
      expect(@result).to eq([
        ['foo', ['bar', ['baz', 'quz']]]
      ])
    end
  end
end
