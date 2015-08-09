class Parser
  PARENS_RE = /(\((?>[^()]|\g<1>)*\)|[^\(\)]+)/
  STRING_RE = /".*?"|[^" ]+/

  def initialize(code = nil)
    @code = code
  end

  def parse(code = @code)
    code.scan(PARENS_RE).flat_map do |(sexp)|
      if sexp[0] == '('
        [parse(sexp[1..-1])]
      else
        sexp.strip.scan(STRING_RE)
      end
    end
  end
end
