class VM
  class Unspecified
    include Singleton

    def to_s
      '#<unspecified>'
    end

    def eq?(other)
      self == other
    end

    def to_ruby
      nil
    end
  end
end
