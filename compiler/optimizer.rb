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
    # allowing tail-call elimination to work with if expressions
    def jump_to_return
      ip = 0
      while ip < @instructions.size
        instruction = @instructions[ip]
        (_name, arity) = VM::INSTRUCTIONS.fetch(instruction)
        if instruction == VM::JUMP
          hops = @instructions[ip + 1]
          if @instructions[ip + hops + 1] == VM::RETURN
            # add a NOOP so other JUMPs don't break
            @instructions[ip, 2] = [VM::RETURN, VM::NOOP]
          end
        end
        ip += arity + 1
      end
    end
  end
end
