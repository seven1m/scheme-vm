class VM
  class DebugOutput
    def initialize(vm, instruction, debug_level)
      @vm = vm
      @instruction = instruction
      @debug_level = debug_level
    end

    def print_debug
      (name, _arg_count) = VM::INSTRUCTIONS.fetch(@instruction)
      print name.ljust(15)
      case name
      when 'SET_ARGS'
        print_set_args
      when 'SET_LOCAL'
        print_set_local
      when /^JUMP/
        print_jump
      else
        print_stack
      end
      puts "#{' ' * 20}heap:   #{@vm.heap.inspect}\n\n" if @debug_level >= 3
    end

    private

    def print_set_args
      print 'args:   '
      p @vm.call_args.each_with_object({}) { |a, h| h[a] = @vm.heap[a] }
    end

    def print_set_local
      print 'locals: '
      p @vm.locals.each_with_object({}) { |(n, a), h| h[n] = @vm.heap[a] }
    end

    def print_jump
      print 'ip:     '
      p @vm.ip
    end

    def print_stack
      print 'stack:  '
      puts @vm.stack.map { |a| @vm.heap[a].inspect }.join("\n" + (' ' * 28))
    end
  end
end
