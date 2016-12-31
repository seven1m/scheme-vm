class VM
  class Char
    def initialize(code)
      @code = code
    end

    def self.from_string(str)
      new(str.ord)
    end

    def raw
      @code
    end

    def ==(other)
      return false unless other.is_a?(Char)
      raw == other.raw
    end

    alias eq? ==

    def to_s
      @code.chr
    end

    def to_ruby
      @code.chr
    end
  end
end
