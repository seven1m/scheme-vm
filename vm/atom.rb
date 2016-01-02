require 'parslet'

class VM
  class Atom < String
    attr_reader :filename, :line, :column

    def initialize(name, filename: nil, line: nil, column: nil)
      super(name.to_s)
      @filename = filename
      @line = line
      @column = column
    end

    def raw
      self
    end

    def to_ruby
      to_s
    end

    def copy_and_rename(name)
      self.class.new(name, filename: filename, line: line, column: column)
    end

    def sub(*_args)
      new_name = super
      copy_and_rename(new_name)
    end
  end
end
