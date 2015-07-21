class VM
  class Atom
    def initialize(name)
      @name = name
    end

    def raw
      @name
    end

    def ==(other)
      return false unless other.is_a?(Atom)
      raw == other.raw
    end

    def to_s
      raw.to_s
    end
  end
end
