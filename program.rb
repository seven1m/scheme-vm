require_relative 'parser'
require_relative 'compiler'

class Program
  def initialize(code, args: [], stdout: $stdout)
    @sexps = Parser.new(code).parse
    @instr = compiler.compile
    @args = args
    @stdout = stdout
  end

  def run(debug: 0)
    compiler.pretty_print(@instr) if debug >= 2
    vm.execute(nil, debug: debug)
  end

  def compiler
    @compiler ||= Compiler.new(@sexps)
  end

  def vm
    @vm ||= VM.new(@instr, stdout: @stdout, args: @args)
  end
end
