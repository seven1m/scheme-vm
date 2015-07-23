require 'singleton'

class VM
  class BoolFalse
    include Singleton

    def raw
      false
    end

    def to_s
      '#f'
    end
  end
end
