class Pattern
  def initialize(pattern)
    @pattern = pattern
  end

  def match(expr)
    return if @pattern.first != expr.first
    match_sub(expr.dup[1..-1], @pattern[1..-1])
  end

  private

  def match_sub(expr, pattern)
    hash = {}
    previous = nil
    while (identifier = pattern.shift)
      if identifier == '...'
        if previous.is_a?(Array)
          previous.each_with_index.each do |pr, index|
            hash[pr + identifier] = [expr.flatten].map { |e| e[index] }.compact
          end
        else
          hash[previous + identifier] = expr.dup
        end
        expr.clear
      elsif identifier.is_a?(Array)
        hash.merge!(match_sub(expr.shift, identifier.dup))
      else
        value = expr.shift
        return if value.nil? && pattern.first != '...'
        hash[identifier] = value
      end
      previous = identifier
    end
    return if expr.any?
    hash
  end
end

# describe Pattern do
#   it 'matches if length is the same' do
#     expect(Pattern.new(['and']).match(['and'])).to eq({})
#   end

#   it 'does not match if length is different' do
#     expect(Pattern.new(['and']).match(['and', 'foo'])).to eq(nil)
#   end

#   it 'matches if length is different but pattern allows for variability' do
#     pattern = Pattern.new(['and', 'var1', '...'])
#     expect(pattern.match(['and'])).to eq('var1' => nil, 'var1...' => [])
#     expect(pattern.match(['and', 'foo'])).to eq('var1' => 'foo', 'var1...' => [])
#     expect(pattern.match(['and', 'foo', 'bar'])).to eq('var1' => 'foo', 'var1...' => ['bar'])
#     expect(pattern.match(['and', 'foo', 'bar', 'baz'])).to eq('var1' => 'foo', 'var1...' => ['bar', 'baz'])
#   end

#   it 'matches sub patterns' do
#     pattern = Pattern.new(['let', [['name', 'val'], '...']])
#     expect(pattern.match(['let', [['foo', 'bar'], ['baz', 'quz']]])).to eq(
#       'name'    => 'foo',
#       'val'     => 'bar',
#       'name...' => ['baz'],
#       'val...'  => ['quz']
#     )
#   end
# end
