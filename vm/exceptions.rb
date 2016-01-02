require_relative 'atom'

class VM
  class CallStackTooDeep < StandardError; end
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
end
