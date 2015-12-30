require 'parslet'

module LISP
  class Parser < Parslet::Parser
    rule(:whitespace) do
      match('[ \t\n]').repeat
    end

    rule(:escape) do
      str('\\') >> any
    end

    rule(:string) do
      (str('"') >> (match('[^"]') | escape).repeat >> str('"')).as(:string)
    end

    rule(:atom) do
      quote.maybe >> (
        str('|') >> match('[^|]').repeat(1).as(:atom) >> str('|') |
        match('[^\(\) \t\n\[\]\{\}]').repeat(1).as(:atom)
      )
    end

    rule(:sexp) do
      quoted_sexp | simple_sexp
    end

    rule(:quote) do
      (str("'") | str(',@') | str(',') | str('`')).as(:quote)
    end

    rule(:quoted_sexp) do
      quote >> simple_sexp
    end

    rule(:simple_sexp) do
      (str('(') >> expression.repeat >> str(')')).as(:sexp)
    end

    rule(:comment) do
      block_comment | line_comment | datum_comment.as(:comment)
    end

    rule(:block_comment) do
      str('#|') >> (str('|#').absent? >> any).repeat >> str('|#')
    end

    rule(:line_comment) do
      str(';') >> match('[^\n]').repeat
    end

    rule(:datum_comment) do
      str('#;') >> str(' ').maybe >> (atom | sexp)
    end

    rule(:expression) do
      (string | comment | atom | sexp) >> whitespace
    end

    rule(:program) do
      expression.repeat(1)
    end

    root(:program)
  end

  class Transform < Parslet::Transform
    QUOTE_METHOD = {
      "'"  => 'quote',
      ',@' => 'unquote-splicing',
      ','  => 'unquote',
      '`'  => 'quasiquote'
    }

    rule(comment: subtree(:comment))

    rule(quote: simple(:quote), atom: simple(:atom)) do
      method = QUOTE_METHOD[quote.to_s]
      [method, atom]
    end

    rule(atom: simple(:atom)) do
      atom
    end

    rule(string: simple(:string)) do
      string
    end

    rule(quote: simple(:quote), sexp: subtree(:sexp)) do
      method = QUOTE_METHOD[quote.to_s]
      [method] + sexp
    end

    rule(sexp: subtree(:sexp)) do
      sexp
    end
  end
end

class Parser
  def initialize(code = nil)
    @code = code
    @parser = LISP::Parser.new
    @transform = LISP::Transform.new
  end

  def parse(code = @code)
    code.strip!
    return [] if code.empty?
    result = @parser.parse(code)
    @transform.apply(result)
  end
end
