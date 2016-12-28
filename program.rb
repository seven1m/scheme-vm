require_relative 'parser'
require_relative 'compiler'

class Program
  def initialize(code, filename: '(unknown)', args: [], stdout: $stdout)
    @filename = filename
    @args = args
    @stdout = stdout
    @compiler = Compiler.new(code, filename: filename)
  end

  def run(code: nil, debug: 0)
    @instr = @compiler.compile(code)
    VM::PrettyPrinter.new(@instr, grouped: true, ip: true).print if debug >= 1
    vm.debug = debug
    vm.execute(@instr)
    vm.return_value
  rescue VM::VariableUndefined => e
    print_variable_undefined_error(e)
    1
  rescue VM::CallStackTooDeep => e
    print_call_stack_too_deep_error(e)
    2
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

  def error_details_to_s(e)
    return '' unless e.filename && @compiler.source[e.filename]
    lines_range = (e.line - 2)..(e.line - 1)
    code = @compiler.source[e.filename].split("\n")[lines_range].map { |l| "  #{l}" }.join("\n")
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
end
