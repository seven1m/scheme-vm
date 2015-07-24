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
    @compiler ||= Compiler.new(@sexps)
  end

  def vm
    @vm ||= VM.new(@instr, stdout: @stdout, args: @args, libraries: libraries)
  end

  def libraries
    {
      'list' => Compiler.new(lib_sexps('list.scm')).compile(halt: false)
    }
  end

  def lib_sexps(lib)
    code = lib_code(lib)
    Parser.new(code).parse
  end

  ROOT_PATH = File.expand_path("..", __FILE__)

  def lib_code(filename)
    File.read(File.join(ROOT_PATH, 'lib', filename))
  end
end
