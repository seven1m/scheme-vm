require_relative 'parser'
require_relative 'compiler'
require 'time'

class Program
  EXIT_CODE_VAR_UNDEFINED  = 1
  EXIT_CODE_STACK_TOO_DEEP = 2
  EXIT_CODE_SYNTAX_ERROR   = 3
  EXIT_CODE_FATAL_ERROR    = 4

  def initialize(code, filename: '(unknown)', args: [], stdout: $stdout)
    @filename = filename
    @args = args
    @stdout = stdout
    start_parse = Time.now
    @compiler = Compiler.new(code, filename: filename)
    @total_parse = Time.now - start_parse
  rescue Parser::ParseError => e
    print_syntax_error(e, code)
    @error_parsing = true
  end

  def run(code: nil, debug: 0)
    return EXIT_CODE_SYNTAX_ERROR if @error_parsing
    start_compile = Time.now
    @instr = @compiler.compile(code)
    @total_compile = Time.now - start_compile
    VM::PrettyPrinter.new(@instr, grouped: true, ip: true).print if debug >= 1
    vm.debug = debug
    start_execute = Time.now
    vm.execute(@instr)
    @total_execute = Time.now - start_execute
    print_timings if ENV['PRINT_TIMINGS']
    vm.return_value
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

  private

  def vm
    @vm ||= VM.new(stdout: @stdout, args: @args)
  end

  def print_general_error(e)
    code = @compiler.source[e.filename]
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
      compiler: @compiler,
      message: e.message
    ).print(@stdout)
  end

  def print_timings
    puts "parse:   #{@total_parse}"
    puts "compile: #{@total_compile}"
    puts "execute: #{@total_execute}"
  end
end
