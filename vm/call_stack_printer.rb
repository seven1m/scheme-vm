class VM
  class CallStackPrinter
    def initialize(title: 'Error', call_stack:, compiler:, message:)
      @title = title
      @call_stack = call_stack
      @compiler = compiler
      @message = message
    end

    def to_s
      out = []
      @call_stack.reverse.each do |frame|
        next unless (name = frame[:name] || frame[:orig_name])
        out << "#{name.filename}##{name.line}"
        code = @compiler.source[name.filename].split("\n")[name.line - 1]
        out << "  #{code}"
        out << " #{' ' * name.column}^#{' ' + @message if @message}"
      end
      @title + "\n\n" + out.join("\n")
    end

    def print(stdout)
      stdout.puts(to_s)
    end
  end
end
