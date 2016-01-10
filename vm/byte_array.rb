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
      return false unless other.is_a?(ByteArray)
      raw == other.raw
    end

    alias_method :eq?, :==

    def to_s
      raw.map(&:chr).join
    end
  end
end
