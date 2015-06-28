class Compiler
  def initialize(sexps = nil)
    @sexps = sexps
    @locals = {}
  end

  def compile(sexps = nil)
    @sexps = sexps if sexps
    @sexps.flat_map do |sexp|
      (reader, *args) = sexp
      case reader
      when :def
        (name, value) = args
        index = @locals[name] || @locals[name] = @locals.values.size
        [
          VM::PUSH_NUM, value,
          VM::SET_LOCAL, index
        ]
      end
    end
  end
end
