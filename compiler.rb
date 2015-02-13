require_relative './vm'

class Compiler

  attr_accessor :expressions

  def initialize(expressions=[])
    @expressions = expressions
  end

  def compiled
    locals = []
    expressions.flat_map do |expr|
      compile(expr, locals)
    end +
    [VM::PUSHNUM, 0, VM::RETURN]
  end

  def compile(expr, locals)
    head = expr.shift
    case head
    when :def
      name = expr.shift
      value = expr.shift
      locals << name unless locals.include?(name)
      index = locals.index(name)
      [
        VM::PUSHNUM, 1,
        VM::PUSHNUM, index,
        VM::ALLOC,
        VM::PUSHNUM, value,
        VM::PUSHNUM, index,
        VM::ASSIGN
      ]
    end
  end

end
