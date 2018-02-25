class VM
  class Atom < String
    attr_reader :filename, :offset, :line, :column

    def initialize(name, filename = nil, offset = nil, line = nil, column = nil)
      super(name.to_s)
      @filename = filename
      @offset = offset
      @line = line
      @column = column
    end

    def raw
      self
    end

    alias eq? ==

    def to_ruby
      to_s
    end

    def mangle(version)
      name = "##{self}.v#{version}"
      self.class.new(name, filename, offset, line, column)
    end
  end
end
