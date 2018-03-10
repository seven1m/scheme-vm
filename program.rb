require_relative 'parser'
require_relative 'compiler'
require 'time'

class Program
  EXIT_CODE_SYNTAX_ERROR   = 1
  EXIT_CODE_VAR_UNDEFINED  = 2
  EXIT_CODE_STACK_TOO_DEEP = 3
  EXIT_CODE_FATAL_ERROR    = 4

  def initialize(code, filename: '(unknown)', args: [], stdout: $stdout)
    @filename = filename
    @args = args
    @stdout = stdout
    @source = {}
    @code = code
    @debug = 0
  end

  attr_accessor :debug

  def run
    @ast = parse(@code)
    @compiler = Compiler.new(@ast, filename: @filename, program: self)
    @instr = @compiler.compile
    VM::PrettyPrinter.new(@instr, grouped: true, ip: true).print if @debug >= 1
    vm.debug = @debug
    vm.execute(@instr)
    vm.return_value
  rescue Parser::ParseError => e
    print_syntax_error(e, @code)
    EXIT_CODE_SYNTAX_ERROR
  rescue VM::VariableUndefined => e
    print_general_error(e)
    EXIT_CODE_VAR_UNDEFINED
  rescue VM::CallStackTooDeep => e
    print_fatal_error(e)
    EXIT_CODE_STACK_TOO_DEEP
  rescue VM::FatalError => e
    print_fatal_error(e)
    EXIT_CODE_FATAL_ERROR
  end

  def filename=(f)
    @filename = f
    @compiler.filename = f
  end

  def stdout=(io)
    @stdout = io
    vm.stdout = io
  end

  def parse(code, filename: @filename)
    @source[filename] = code
    Parser.new(code, filename: filename).parse
  end

  private

  def vm
    @vm ||= VM.new(stdout: @stdout, args: @args)
  end

  def print_general_error(e)
    code = @source[e.filename]
    VM::SourceCodeErrorPrinter.new(
      title: "Error: #{e.message}",
      code: code,
      error: e
    ).print(@stdout)
  end

  def print_syntax_error(e, code)
    VM::SourceCodeErrorPrinter.new(
      title: "Syntax Error:",
      code: code,
      error: e
    ).print(@stdout)
  end

  def print_fatal_error(e)
    VM::CallStackPrinter.new(
      title: "Error: #{e.message}",
      call_stack: e.call_stack,
      source: @source,
      message: e.message
    ).print(@stdout)
  end
end
