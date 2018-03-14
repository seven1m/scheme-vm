class VM
  class SourceCodeErrorPrinter
    def initialize(title: 'Error', code:, error:)
      @title = title
      @code = code
      @error = error
    end

    def to_s
      return @title unless @error.filename && @error.filename != ''
      return @title unless @code
      lines_range = (@error.line - 2)..(@error.line - 1)
      lines = @code.split("\n")[lines_range]
      code = lines && lines.map { |l| "  #{l}" }.join("\n")
      line = "#{@error.filename}##{@error.line}"
      pointer = " #{' ' * @error.column}^ #{@error.message}"
      "#{@title}\n\n#{line}\n\n#{code}\n#{pointer}"
    end

    def print(stdout)
      stdout.puts(to_s)
    end
  end
end
