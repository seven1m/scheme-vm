require_relative '../spec_helper'

describe Compiler::Pattern do
  describe '#match' do
    it 'matches if length is the same' do
      expect(described_class.new(['and']).match(['and'])).to eq({})
    end

    it 'does not match if length is different' do
      expect(described_class.new(['and']).match(['and', 'foo'])).to be_nil
    end

    it 'matches without a sexp' do
      expect(described_class.new(['assert', 'test']).match(['assert', 'foo'])).to eq('test' => 'foo')
    end

    it 'does not error if pattern contains a sexp but expression does not' do
      pattern = described_class.new(['assert', ['x', 'y', 'z']])
      expect(pattern.match(['assert', 'foo'])).to be_nil
    end

    it 'matches if length is different but pattern allows for variability' do
      pattern = described_class.new(['and', 'var1', '...'])
      expect(pattern.match(['and'])).to eq('var1' => nil, 'var1...' => [])
      expect(pattern.match(['and', 'foo'])).to eq('var1' => 'foo', 'var1...' => [])
      expect(pattern.match(['and', 'foo', 'bar'])).to eq('var1' => 'foo', 'var1...' => ['bar'])
      expect(pattern.match(['and', 'foo', 'bar', 'baz'])).to eq('var1' => 'foo', 'var1...' => ['bar', 'baz'])
    end

    it 'matches sub patterns' do
      pattern = described_class.new(['let', [['name', 'val'], '...']])
      expect(pattern.match(['let', [['foo', 'bar'], ['baz', 'quz']]])).to eq(
        'name'    => 'foo',
        'val'     => 'bar',
        'name...' => ['baz'],
        'val...'  => ['quz']
      )
      pattern = described_class.new(['let', [['name1', 'val1'], ['name2', 'val2'], '...']])
      expect(pattern.match(['let', [['foo', 'bar']]])).to eq(
        'name1'    => 'foo',
        'val1'     => 'bar',
        'name2'    => nil,
        'val2'     => nil,
        'name2...' => [],
        'val2...'  => []
      )
      pattern = described_class.new(['let', []])
      expect(pattern.match(['let', [[]]])).to eq({})
    end

    it 'matches literals literally' do
      pattern = described_class.new(
        ['assert', ['expected', 'eq?', 'actual']],
        literals: ['eq?']
      )
      result = pattern.match(
        ['assert', ['foo', 'eq?', 'bar']]
      )
      expect(result).to eq(
        'expected' => 'foo',
        'actual' => 'bar'
      )
    end

    it 'does not match if literal does not match' do
      pattern = described_class.new(
        ['assert', ['expected', 'eq?', 'actual']],
        literals: ['eq?']
      )
      result = pattern.match(
        ['assert', ['foo', '==', 'bar']]
      )
      expect(result).to be_nil
    end
  end
end
