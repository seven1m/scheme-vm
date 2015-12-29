require_relative 'parser'
require_relative 'compiler'

class Program
  LIBRARIES = %w(
    list
    pair
    let
    logic
    bool
    string
  )

  def initialize(code, filename: nil, args: [], stdout: $stdout)
    @filename = filename
    @args = args
    @stdout = stdout
    @syntax = {}
    load_libraries
    @code = code
    @sexps = Parser.new(code).parse
    @compiler = Compiler.new(@sexps, filename: filename, syntax: @syntax)
    @instr = @compiler.compile
  end

  def run(debug: 0)
    @compiler.pretty_print(@instr) if debug >= 2
    vm.execute(@instr, debug: debug)
  rescue => e
    print_error_message(e)
  end

  def vm
    @vm ||= VM.new(stdout: @stdout, args: @args)
  end

  def load_libraries
    LIBRARIES.each do |name|
      result = vm.load_library(name, syntax: @syntax)
      @syntax.merge!(result[:syntax])
    end
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
  end

  def error_details(e)
    source = @compiler.source[@filename]
    nearest = source.keys.reverse.find { |i| i <= e.ip }
    source[nearest]
  end
end
