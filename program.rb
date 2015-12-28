require_relative 'parser'
require_relative 'compiler'

class Program
  LIBRARIES = %w(
    list
    pair
    bool
    string
    logic
  )

  def initialize(code, args: [], stdout: $stdout, libraries: [])
    @args = args
    @stdout = stdout
    @syntax = {}
    @libraries = LIBRARIES + libraries
    load_libraries
    @sexps = Parser.new(code).parse
    @instr = Compiler.new(@sexps, syntax: @syntax).compile
  end

  def run(debug: 0)
    Compiler.new.pretty_print(@instr) if debug >= 2
    vm.execute(@instr, debug: debug)
  end

  def vm
    @vm ||= VM.new(stdout: @stdout, args: @args)
  end

  def load_libraries
    @libraries.each do |name|
      result = vm.load_library(name)
      @syntax.merge!(result[:syntax])
    end
  end
end
