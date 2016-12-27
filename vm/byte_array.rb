class VM
  class ByteArray
    def initialize(string)
      string.force_encoding('binary')
      @bytes = string.bytes
      @size = @bytes.size
    end

    attr_reader :size

    def raw
      @bytes
    end

    def to_s
      raw.map(&:chr).join
    end

    alias to_ruby to_s

    alias eq? equal?
    alias eqv? equal?

    def ==(other)
      other.is_a?(ByteArray) && raw == other.raw
    end
  end
end
