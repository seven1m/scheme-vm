class VM
  class Continuation
    def initialize(ip, call_stack)
      @ip = ip
      @call_stack = call_stack.map(&:dup)
    end

    attr_reader :ip

    def to_s
      '#<continuation>'
    end

    def call_stack
      @call_stack.map(&:dup)
    end
  end
end
