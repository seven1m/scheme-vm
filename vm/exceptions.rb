require_relative 'atom'

class VM
  class NoStackValue < StandardError; end

  class VariableUndefined < StandardError
    attr_reader :name, :filename, :line, :column

    def initialize(name)
      @name = name
      return unless @name.is_a?(VM::Atom)
      @filename = @name.filename
      @line = @name.line
      @column = @name.column
    end

    def message
      "#{unmangled_name} is not defined"
    end

    def mangled?
      !(@name =~ /\A#.+\.v\d+\z/).nil?
    end

    def unmangled_name
      return @name unless mangled?
      @name.match(/\A#(.+)\.v\d+\z/)[1]
    end
  end

  class CallStackTooDeep < StandardError
    attr_reader :call_stack

    def initialize(call_stack)
      @call_stack = call_stack
    end

    def message
      'call stack too deep'
    end
  end
end
