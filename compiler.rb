class Compiler
  def initialize(sexps = nil)
    @sexps = sexps
    @locals = {}
  end

  def compile(sexps = @sexps, locals: @locals)
    sexps.flat_map do |sexp|
      compile_sexp(sexp, locals: locals)
    end.flatten.compact
  end

  private

  def compile_sexp(sexp, locals: {}, use: false)
    return compile_literal(sexp, use: use) unless sexp.is_a?(Array)
    (name, *args) = sexp
    if (var_num = locals[name])
      call(var_num, args, locals: locals, use: use)
    else
      send(name, args, locals: locals, use: use)
    end
  end

  def compile_literal(literal, use: false)
    [
      VM::PUSH_NUM,
      literal,
      use ? nil : VM::POP
    ]
  end

  def def((name, val), locals:, use:)
    index = locals[name] || locals[name] = locals.values.size
    [
      compile_sexp(val, use: true),
      VM::SET_LOCAL, index
    ]
  end

  def fn(body, locals:, use:)
    body = compile(body, locals: {})
    [
      VM::PUSH_FUNC,
      body,
      VM::RETURN,
      VM::ENDF,
      use ? nil : VM::POP
    ]
  end

  def call(var_num, args, locals:, use:)
    [
      VM::PUSH_LOCAL, var_num,
      VM::CALL
    ]
  end
end
