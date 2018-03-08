class VM
  module Operations
    def do_push_atom
      name = fetch
      push_val(Atom.new(name))
    end

    def do_push_num
      num = fetch
      push_val(Int.new(num))
    end

    def do_push_str
      str = fetch
      push_val(ByteArray.new(str))
    end

    def do_call_with_cc
      new_ip = pop
      continuation = Continuation.new(@ip, @call_stack)
      address = alloc
      @heap[address] = continuation
      @call_args = [address]
      do_call(new_ip)
    end

    def do_to_atom
      name = pop_val
      push_val(Atom.new(name))
    end

    def do_to_char
      code = pop_raw
      push_val(Char.new(code))
    end

    def do_push_true
      push_true
    end

    def do_push_false
      push_false
    end

    def do_type
      val = pop_val
      push_type(val)
    end

    def do_car
      pair = pop_val
      push(pair.address)
    end

    def do_cdr
      pair = pop_val
      push(pair.next_node)
    end

    def do_cons
      cdr = pop
      car = pop
      pair = build_pair(car, cdr)
      push_val(pair)
    end

    def do_push_arg
      address = args.shift
      push(address)
    end

    def do_push_local
      name = fetch
      address = named_args[name] || locals[name]
      raise VariableUndefined, name unless address
      push(address)
    end

    def do_push_var
      name = fetch
      @last_atom = name if name.is_a?(Atom)
      push(find_address_for_name(name, raise_if_not_found: true))
    end

    def do_push_args
      last = empty_list
      until args.empty?
        arg = args.pop
        address = alloc
        @heap[address] = build_pair(arg, last)
        last = address
      end
      push(address || empty_list)
    end

    def do_push_func
      push(@ip)
      @closures[@ip] = { locals: {}, parent: closure }
      fetch_until(ENDF) # discard
    end

    def do_push_undef
      push(nil)
    end

    def do_set_lib
      name = fetch
      @closures[name] = { locals: {} }
      @call_stack << { func: name, lib: name }
    end

    def do_endl
      frame = @call_stack.pop
      @libs[frame[:lib]] = @closures[frame[:func]]
    end

    def do_import_lib
      lib = fetch
      internal_name = fetch
      binding = fetch
      locals[binding] = @libs[lib][:locals][internal_name]
    end

    def do_str_ref
      index = pop_val.raw
      str = pop_val
      push_val(Char.new(str.raw[index]))
    end

    def do_str_len
      str = pop_val
      push_val(Int.new(str.size))
    end

    def do_to_str
      value = pop_val
      case value
      when Pair
        chars = value.to_ruby.map(&:to_s)
        push_val(ByteArray.new(chars.join))
      when EmptyList
        push_val(ByteArray.new(''))
      when Atom
        push_val(ByteArray.new(value.to_ruby))
      else
        raise "unknown value type: #{value.inspect}"
      end
    end

    def do_call(new_ip = pop)
      raise VM::FatalError.new(@call_stack, "tried to call undefined value: #{@last_atom}") if new_ip.nil?
      raise "ip #{new_ip.inspect} is invalid" unless new_ip.is_a?(Integer)
      return do_call_continuation(new_ip) if heap[new_ip].is_a?(Continuation)
      name = @last_atom if find_address_for_name(@last_atom) == new_ip
      if @heap[@ip] == RETURN
        @call_stack.last[:func] = new_ip
        @call_stack.last[:args] = @call_args
        @call_stack.last[:name] = name
      else
        @call_stack << { name: name, func: new_ip, return: @ip, args: @call_args, named_args: {} }
      end
      raise CallStackTooDeep.new(@call_stack) if @call_stack.size > MAX_CALL_DEPTH
      @ip = new_ip
    end

    def do_call_continuation(address)
      continuation = heap[address]
      @ip = continuation.ip
      @call_stack = continuation.call_stack
      push(@call_args.first)
      @call_args = []
    end

    def do_apply
      pair = resolve(@call_args.pop)
      while pair != EmptyList.instance
        @call_args.push(pair.address)
        pair = @heap[pair.next_node]
      end
      do_call
    end

    def do_return(debug)
      frame = @call_stack.pop
      @ip = frame.fetch(:return)
      VM::GC.new(self).run(debug: debug)
    end

    def do_push_list
      count = pop_raw_and_delete
      address = last = empty_list
      count.times do
        arg = pop
        address = alloc
        @heap[address] = build_pair(arg, last)
        last = address
      end
      push(address)
    end

    def do_pop
      pop
    end

    def do_add
      num2 = pop_val
      num1 = pop_val
      push_val(num1 + num2)
    end

    def do_sub
      num2 = pop_val
      num1 = pop_val
      push_val(num1 - num2)
    end

    def do_mul
      num2 = pop_val
      num1 = pop_val
      push_val(num1 * num2)
    end

    def do_div
      num2 = pop_val
      num1 = pop_val
      push_val(num1 / num2)
    end

    def do_mod
      num2 = pop_val
      num1 = pop_val
      push_val(num1 % num2)
    end

    def do_cmp_gt
      num2 = pop_val
      num1 = pop_val
      num1 > num2 ? push_true : push_false
    end

    def do_cmp_lt
      num2 = pop_val
      num1 = pop_val
      num1 < num2 ? push_true : push_false
    end

    def do_cmp_eq
      obj2 = pop_val
      obj1 = pop_val
      obj1.eq?(obj2) ? push_true : push_false
    end

    def do_cmp_null
      val = pop_val
      val == EmptyList.instance ? push_true : push_false
    end

    def do_dup
      val = peek
      push(val)
    end

    def do_int
      func = fetch
      case func
      when INT_WRITE
        val = pop_val
        stdout_print(val)
      end
    end

    def do_jump
      count = fetch.to_i
      @ip += (count - 1)
    end

    def do_jump_if_false
      val = pop
      count = fetch.to_i
      @ip += (count - 1) if val == bool_false
    end

    def do_append
      count = pop_raw_and_delete
      if count.zero?
        push(empty_list)
      else
        raw = (0...count).map { pop_val }.reverse.map(&:to_a).inject(&:+)
        last = empty_list
        while (arg = raw.pop)
          address = alloc
          @heap[address] = build_pair(arg, last)
          last = address
        end
        push(address || empty_list)
      end
    end

    def do_raw
      raw = pop_raw
      case raw
      when Integer
        push_val(VM::Int.new(raw))
      else
        raise "unknown raw value type #{raw.inspect}"
      end
    end

    def do_define_var
      name = fetch
      locals[name] = pop
    end

    def do_set_var
      name = fetch
      if (frame = find_call_stack_frame_with_symbol(name))
        frame[:named_args][name] = pop
      elsif (c = find_closure_with_symbol(name))
        c[:locals][name] = pop
      else
        raise VariableUndefined, name
      end
    end

    def do_set_args
      count = pop_raw_and_delete
      raise NoStackValue, "stack size is #{@stack.size}, but you tried to use #{count}" if @stack.size < count
      @call_args = (0...count).map { pop }.reverse
    end

    def do_name_arg
      address = pop
      name = fetch
      named_args[name] = address
    end

    def do_set_car
      new_car = pop
      pair = pop_val
      pair.address = new_car
    end

    def do_set_cdr
      new_cdr = pop
      pair = pop_val
      pair.next_node = new_cdr
    end
  end
end
