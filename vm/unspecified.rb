class VM
  class Unspecified
    include Singleton

    def to_s
      '#<unspecified>'
    end

    def eq?(other)
      other == Unspecified.instance
    end
  end
end
