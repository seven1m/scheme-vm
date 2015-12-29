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
      quote.maybe >> match('[^\(\) \t\n\[\]\{\}]').repeat(1).as(:atom)
    end

    rule(:sexp) do
      quoted_sexp | simple_sexp
    end

    rule(:quote) do
      (match("'") | match(',@') | match(',') | match('`')).as(:quote)
    end

    rule(:quoted_sexp) do
      quote >> simple_sexp
    end

    rule(:simple_sexp) do
      (str('(') >> expression.repeat >> str(')')).as(:sexp)
    end

    rule(:comment) do
      str(';') >> match('[^\n]').repeat
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
    result = @parser.parse(code.strip)
    @transform.apply(result)
  end
end
