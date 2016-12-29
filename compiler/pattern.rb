class Compiler
  class Pattern
    def initialize(pattern, literals: [])
      @pattern = pattern
      @literals = literals.map(&:to_s)
    end

    def match(expr)
      return if @pattern.first != expr.first
      match_sub(expr.dup[1..-1], @pattern[1..-1])
    end

    private

    def match_sub(expr, pattern)
      hash = {}
      return hash if expr.flatten.empty? && pattern.flatten.empty?
      previous = nil
      while (identifier = pattern.shift)
        if @literals.include?(identifier)
          keyword = expr.shift
          return if identifier != keyword
        elsif identifier == '...'
          match_dotted_sub(identifier, expr, previous, hash)
        elsif identifier.is_a?(Array)
          return unless match_sub_array(identifier, expr, hash)
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

    def match_sub_array(identifier, expr, hash)
      sub_expr = expr.shift
      if sub_expr.nil?
        match = Hash[identifier.zip(Array.new(identifier.size))]
      elsif sub_expr.is_a?(Array)
        match = match_sub(sub_expr.dup, identifier.dup)
      end
      match && hash.merge!(match)
    end

    def match_dotted_sub(identifier, expr, previous, hash)
      if previous.is_a?(Array)
        previous.each_with_index.each do |pr, index|
          hash[pr + identifier] = expr.map { |e| e[index] }
        end
      else
        hash[previous + identifier] = expr.dup
      end
      expr.clear
    end
  end
end
