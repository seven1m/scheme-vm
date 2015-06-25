class VM
  class Int
    include Comparable

    MIN = 2**63 * -1
    MAX = 2**63 - 1

    def initialize(num)
      @num = num.to_i
      fail 'integer out of range' if @num < MIN || @num > MAX
    end

    def raw
      @num
    end

    def ==(other)
      raw == other.raw
    end

    def +(other)
      Int.new(raw + other.raw)
    end

    def to_s
      raw.to_s
    end

    def <=>(other)
      raw <=> other.raw
    end
  end
end
