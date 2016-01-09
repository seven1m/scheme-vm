require 'pp'

class VM
  class PrettyPrinter
    def initialize(instructions, grouped: false, ip: false)
      @instructions = instructions.dup.flatten.compact
      @grouped = grouped
      @ip = ip
    end

    def format
      count = 0
      [].tap do |pretty|
        while @instructions.any?
          group = []
          group << count if @ip
          if (instruction = @instructions.shift)
            (name, arity) = VM::INSTRUCTIONS[instruction]
            group << "VM::#{name}"
            arity.times { group << @instructions.shift }
            pretty << group
            count += group.size - 1
          else
            pretty << nil
          end
        end
      end.send(@grouped ? :to_a : :flatten)
    end

    def print
      pp(format)
    end
  end
end
