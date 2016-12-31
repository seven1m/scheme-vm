class VM
  class DebugOutput
    def initialize(vm, instruction, debug_level)
      @vm = vm
      @instruction = instruction
      @debug_level = debug_level
    end

    def print_ip
      print((@vm.ip - 1).to_s.ljust(5))
    end

    def print_debug
      (name, _arg_count) = VM::INSTRUCTIONS.fetch(@instruction)
      print name.ljust(15)
      case name
      when 'SET_ARGS'
        print_set_args
      when 'NAME_ARG'
        print_name_arg
      when 'DEFINE_VAR'
        print_define_var
      when 'SET_VAR'
        print_set_var
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
      p @vm.call_args.each_with_object({}) { |a, h| h[a] = a && @vm.heap[a] }
    end

    def print_name_arg
      print 'nargs:  '
      p @vm.named_args.each_with_object({}) { |(n, a), h| h[n] = a && @vm.heap[a] }
    end

    def print_define_var
      print 'locals: '
      p @vm.locals.each_with_object({}) { |(n, a), h| h[n] = a && @vm.heap[a] }
    end

    def print_set_var
      print 'locals: '
      p @vm.closure[:locals].each_with_object({}) { |(n, a), h| h[n] = a && @vm.heap[a] }
    end

    def print_jump
      print 'ip:     '
      p @vm.ip
    end

    def print_stack
      print 'stack:  '
      puts @vm.stack.map { |a| a ? @vm.heap[a].inspect : a.inspect }.join("\n" + (' ' * 28))
      return unless (atom = @vm.last_atom)
      print 'code:  '.rjust(28)
      puts "#{atom.filename}##{atom.line}:#{atom.column}"
    end
  end
end
