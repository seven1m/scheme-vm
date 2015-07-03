require_relative 'parser'
require_relative 'compiler'

class Program
  def initialize(code)
    @sexps = Parser.new(code).parse
    @instr = Compiler.new(@sexps).compile
  end

  def run
    @instr
  end
end
