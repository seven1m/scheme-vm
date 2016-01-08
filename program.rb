require_relative 'parser'
require_relative 'compiler'

class Program
  def initialize(code, filename: '(unknown)', args: [], stdout: $stdout)
    @filename = filename
    @args = args
    @stdout = stdout
    @compiler = Compiler.new(code, filename: filename)
  end

  def run(debug: 0)
    @instr = @compiler.compile
    @compiler.pretty_print(@instr) if debug >= 1
    vm.execute(@instr, debug: debug)
    vm.return_value
  rescue VM::VariableUndefined => e
    print_error_message(e)
    1
  end

  def vm
    @vm ||= VM.new(stdout: @stdout, args: @args)
  end

  def print_error_message(e)
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
end
