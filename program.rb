require_relative 'parser'
require_relative 'compiler'

class Program
  def initialize(code, stdout: $stdout)
    @sexps = Parser.new(code).parse
    @instr = compiler.compile
    @stdout = stdout
  end

  def run(debug: 0)
    compiler.pretty_print(@instr) if debug >= 2
    vm.execute(nil, debug: debug)
  end

  private

  def compiler
    @compiler ||= Compiler.new(@sexps)
  end

  def vm
    @vm ||= VM.new(@instr, stdout: @stdout)
  end
end
