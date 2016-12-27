require 'singleton'

class VM
  class BoolFalse
    include Singleton

    def raw
      false
    end

    def to_ruby
      raw
    end

    def to_s
      '#f'
    end

    alias eq? ==
    alias eqv? ==
  end
end
