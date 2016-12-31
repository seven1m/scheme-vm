class VM
  class Int
    include Comparable

    MIN = 2**63 * -1
    MAX = 2**63 - 1

    def initialize(num)
      @num = num.to_i
      raise 'integer out of range' if @num < MIN || @num > MAX
    end

    def raw
      @num
    end

    def eq?(other)
      return false unless other.is_a?(Int)
      raw == other.raw
    end

    alias == eq?

    def +(other)
      Int.new(raw + other.raw)
    end

    def -(other)
      Int.new(raw - other.raw)
    end

    def *(other)
      Int.new(raw * other.raw)
    end

    def /(other)
      Int.new(raw / other.raw)
    end

    def %(other)
      Int.new(raw % other.raw)
    end

    def to_s
      raw.to_s
    end

    def to_ruby
      @num
    end

    def <=>(other)
      raw <=> other.raw
    end
  end
end
