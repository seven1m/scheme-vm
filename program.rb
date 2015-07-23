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

  private

  def compiler
    @compiler ||= Compiler.new(lib_sexps + @sexps)
  end

  def vm
    @vm ||= VM.new(@instr, stdout: @stdout, args: @args)
  end

  def lib_sexps
    libs = [
      lib_code('list.scm')
    ]
    Parser.new(libs.join).parse
  end

  ROOT_PATH = File.expand_path("..", __FILE__)

  def lib_code(filename)
    File.read(File.join(ROOT_PATH, 'lib', filename))
  end
end
