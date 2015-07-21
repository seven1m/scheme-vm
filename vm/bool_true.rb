require 'singleton'

class VM
  class BoolTrue
    include Singleton

    def raw
      true
    end

    def to_s
      raw.to_s
    end
  end
end
