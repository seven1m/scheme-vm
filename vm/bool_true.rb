require 'singleton'

class VM
  class BoolTrue
    include Singleton

    def raw
      true
    end

    def to_s
      '#t'
    end
  end
end
