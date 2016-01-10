class VM
  class Char
    def initialize(char)
      @byte = char.ord
    end

    def raw
      @byte
    end

    def ==(other)
      return false unless other.is_a?(Char)
      raw == other.raw
    end

    def eq?(other)
      return false unless other.is_a?(Char)
      raw == other.raw
    end

    def eqv?(other)
      return false unless other.is_a?(Char)
      raw == other.raw
    end

    def to_s
      @byte.chr
    end

    def to_ruby
      @byte.chr
    end
  end
end
