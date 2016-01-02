require_relative 'parser'
require_relative 'compiler'

class Program
  INCLUDES = %w(
    begin
    let
    logic
    cond
    list
    pair
    bool
    string
  )

  def initialize(code, filename: '(unknown)', args: [], stdout: $stdout)
    @filename = filename
    @args = args
    @stdout = stdout
    @code = code
    @sexps = Parser.new(code).parse
    @compiler = Compiler.new(filename: filename)
    @compiler.include_code(INCLUDES)
    @instr = @compiler.compile(@sexps)
  end

  def run(debug: 0)
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
    details = error_details(e)
    return '' unless details
    lines_range = (details[:line] - 2)..(details[:line] - 1)
    code = @code.split("\n")[lines_range].map { |l| "  #{l}" }.join("\n")
    line = "#{@filename}##{details[:line]}"
    pointer = " #{' ' * details[:column]}^ #{e.message}"
    "\n\n#{line}\n\n#{code}\n#{pointer}"
  rescue
    ''
  end

  def error_details(e)
    source = @compiler.source[@filename]
    nearest = source.keys.reverse.find { |i| i <= e.ip }
    source[nearest]
  end
end
