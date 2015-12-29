class Pattern
  def initialize(pattern)
    @pattern = deatomize(pattern)
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
      if identifier == '...'
        if previous.is_a?(Array)
          previous.each_with_index.each do |pr, index|
            hash[pr + identifier] = expr.map { |e| e[index] }
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
