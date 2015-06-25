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

    def ==(other)
      raw == other.raw
    end

    def to_s
      raw.map(&:chr).join
    end
  end
end
