require_relative 'parser'
require_relative 'compiler'
require 'time'

class Program
  EXIT_CODE_VAR_UNDEFINED  = 1
  EXIT_CODE_STACK_TOO_DEEP = 2
  EXIT_CODE_SYNTAX_ERROR   = 3

  def initialize(code, filename: '(unknown)', args: [], stdout: $stdout)
    @filename = filename
    @args = args
    @stdout = stdout
    @code = code
    start_parse = Time.now
    @compiler = Compiler.new(code, filename: filename)
    @total_parse = Time.now - start_parse
  rescue Parser::ParseError => e
    print_syntax_error(e)
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
    print_variable_undefined_error(e)
    EXIT_CODE_VAR_UNDEFINED
  rescue VM::CallStackTooDeep => e
    print_call_stack_too_deep_error(e)
    EXIT_CODE_STACK_TOO_DEEP
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

  def print_variable_undefined_error(e)
    message = "Error: #{e.message}"
    message += error_details_to_s(e)
    @stdout.puts(message)
  end

  def error_details_to_s(e, code = nil)
    return '' unless e.filename && e.filename != ''
    return '' unless (code ||= @compiler.source[e.filename])
    lines_range = (e.line - 2)..(e.line - 1)
    code = code.split("\n")[lines_range].map { |l| "  #{l}" }.join("\n")
    line = "#{e.filename}##{e.line}"
    pointer = " #{' ' * e.column}^ #{e.message}"
    "\n\n#{line}\n\n#{code}\n#{pointer}"
  end

  def print_call_stack_too_deep_error(e)
    message = "Error: #{e.message}"
    @stdout.puts(message)
    e.call_stack.reverse.each do |frame|
      next unless (name = frame[:name])
      @stdout.puts "#{name.filename}##{name.line}"
      code = @compiler.source[name.filename].split("\n")[name.line - 1]
      @stdout.puts "  #{code}"
      @stdout.puts " #{' ' * name.column}^"
    end
  end

  def print_syntax_error(e)
    message = 'Syntax Error:' + error_details_to_s(e, @code)
    @stdout.puts(message)
  end

  def print_timings
    puts "parse:   #{@total_parse}"
    puts "compile: #{@total_compile}"
    puts "execute: #{@total_execute}"
  end
end
