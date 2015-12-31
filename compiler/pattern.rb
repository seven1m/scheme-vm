class Compiler
  class Pattern
    def initialize(pattern, literals: [])
      @pattern = deatomize(pattern)
      @literals = literals.map(&:to_s)
    end

    def match(expr)
      expr = deatomize(expr)
      return if @pattern.first != expr.first
      match_sub(expr.dup[1..-1], @pattern[1..-1])
    end

    private

    def match_sub(expr, pattern)
      hash = {}
      previous = nil
      while (identifier = pattern.shift)
        if @literals.include?(identifier)
          keyword = expr.shift
          return if identifier != keyword
        elsif identifier == '...'
          match_dotted_sub(identifier, expr, previous, hash)
        elsif identifier.is_a?(Array)
          sub_expr = expr.shift
          return unless sub_expr.is_a?(Array)
          if (sub_match = match_sub(sub_expr, identifier.dup))
            hash.merge!(sub_match)
          else
            return
          end
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

    def deatomize(sexp)
      if sexp.is_a?(Array)
        sexp.map { |s| deatomize(s) }
      elsif sexp.is_a?(Parslet::Slice)
        sexp.to_s
      else
        sexp
      end
    end
  end
end
