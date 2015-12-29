class VM
  class CallStackTooDeep < StandardError; end
  class NoStackValue < StandardError; end

  class VariableUndefined < StandardError
    attr_reader :name, :ip

    def initialize(name, ip)
      @name = name
      @ip = ip
    end

    def message
      "#{@name} is not defined"
    end
  end
end
