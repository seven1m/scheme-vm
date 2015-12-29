require 'singleton'

class VM
  class BoolTrue
    include Singleton

    def raw
      true
    end

    def to_ruby
      raw
    end

    def to_s
      '#t'
    end

    alias_method :eq?, :==
  end
end
