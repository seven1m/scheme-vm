require_relative 'parser'
require_relative 'compiler'

class Program
  def initialize(code, args: [], stdout: $stdout, libraries: [])
    @sexps = Parser.new(code).parse
    @instr = compiler.compile
    @args = args
    @stdout = stdout
    @libraries = libraries
  end

  def run(debug: 0)
    compiler.pretty_print(@instr) if debug >= 2
    vm.execute(nil, debug: debug)
  end

  def compiler
    @compiler ||= Compiler.new(@sexps)
  end

  def vm
    @vm ||= VM.new(@instr, stdout: @stdout, args: @args, libraries: @libraries)
  end
end
