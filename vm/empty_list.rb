require 'singleton'

class VM
  class EmptyList
    include Singleton

    def raw
      []
    end

    def to_s
      '()'
    end

    def to_a
      []
    end
  end
end
