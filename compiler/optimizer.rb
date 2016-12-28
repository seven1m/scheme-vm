require_relative '../vm'

class Compiler
  class Optimizer
    def initialize(instructions)
      @instructions = instructions.dup
    end

    def optimize
      jump_to_return
      @instructions
    end

    private

    # changes JUMP -> RETURN if the JUMP would land on a RETURN,
    # or if JUMP would land on a JUMP that would land on a RETURN, and so on,
    # allowing tail-call elimination to work with if expressions
    def jump_to_return
      jumps.reverse.each do |ip|
        hops = @instructions[ip + 1]
        if @instructions[ip + hops + 1] == VM::RETURN
          # add a NOOP so other JUMPs don't break
          @instructions[ip, 2] = [VM::RETURN, VM::NOOP]
        end
      end
    end

    def jumps
      ip = 0
      jumps = []
      while ip < @instructions.size
        instruction = @instructions[ip]
        (_name, arity) = VM::INSTRUCTIONS.fetch(instruction)
        jumps << ip if instruction == VM::JUMP
        ip += arity + 1
      end
      jumps
    end
  end
end
