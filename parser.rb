class Parser
  PARENS_RE = /(\((?>[^()]|\g<1>)*\))/

  def initialize(code = nil)
    @code = code
  end

  def parse(code = @code)
    code.scan(PARENS_RE).map do |(sexp)|
      body = sexp[1..-1]
      [body.match(/\A[^\(\)]+/)].compact.map { |atom| atom.to_s.strip } +
      parse(body) +
      [body.match(/[^\)\)]+\z/)].compact.map { |atom| atom.to_s.strip }
    end
  end
end
