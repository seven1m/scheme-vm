require 'singleton'

class VM
  class BoolFalse
    include Singleton

    def raw
      false
    end

    def to_s
      raw.to_s
    end
  end
end
