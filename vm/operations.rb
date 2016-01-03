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

    def do_push_char
      char = fetch
      push_val(Char.new(char))
    end

    def do_push_true
      push_true
    end

    def do_push_false
      push_false
    end

    def do_push_type
      val = pop_val
      push_type(val)
    end

    def do_push_car
      pair = pop_val
      push(pair.address)
    end

    def do_push_cons
      cdr = pop
      car = pop
      pair = build_pair(car, cdr)
      push_val(pair)
    end

    def do_push_arg
      address = args.shift
      push(address)
    end

    def do_push_cdr
      pair = pop_val
      push(pair.next_node)
    end

    def do_push_local
      name = fetch
      address = locals[name]
      fail VariableUndefined, name unless address
      push(address)
    end

    def do_push_remote
      name = fetch
      func = @call_stack.last[:func]
      closure_locals = func ? @closures[func][:locals] : {}
      if closure_locals.key?(name)
        address = closure_locals.fetch(name)
      else
        frame_locals = @call_stack.reverse.lazy.map { |f| f[:locals] }.detect { |l| l.key?(name) }
        fail VariableUndefined, name unless frame_locals
        address = frame_locals.fetch(name)
      end
      push(address)
    end

    def do_push_args
      last = empty_list
      while (arg = args.pop)
        address = alloc
        @heap[address] = build_pair(arg, last)
        last = address
      end
      push(address || empty_list)
    end

    def do_push_func
      push(@ip)
      @closures[@ip] = { locals: locals }
      fetch_until(ENDF) # discard
    end

    def do_set_lib
      name = fetch
      @call_stack << { lib: name, locals: {} }
    end

    def do_endl
      frame = @call_stack.pop
      @libs[frame[:lib]] = { locals: frame[:locals] }
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

    def do_list_to_str
      list = pop_val
      chars = list.to_ruby.map(&:to_s)
      push_val(ByteArray.new(chars.join))
    end

    def do_call
      new_ip = pop
      if @heap[@ip] == RETURN
        @call_stack.last[:func] = new_ip
        @call_stack.last[:args] = @call_args
      else
        @call_stack << { func: new_ip, return: @ip, locals: {}, args: @call_args }
      end
      fail CallStackTooDeep, 'call stack too deep' if @call_stack.size > MAX_CALL_DEPTH
      @ip = new_ip
    end

    def do_apply
      pair = resolve(@call_args.pop)
      while pair != EmptyList.instance
        @call_args.push(pair.address)
        pair = @heap[pair.next_node]
      end
      new_ip = pop
      @call_stack << { func: new_ip, return: @ip, locals: {}, args: @call_args }
      fail CallStackTooDeep, 'call stack too deep' if @call_stack.size > MAX_CALL_DEPTH
      @ip = new_ip
    end

    def do_return(debug)
      frame = @call_stack.pop
      @ip = frame.fetch(:return)
      VM::GC.new(self).run(debug: debug)
    end

    def do_push_list
      count = pop_raw
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

    def do_cmp_gt
      num2 = pop_val
      num1 = pop_val
      num1 > num2 ? push_true : push_false
    end

    def do_cmp_gte
      num2 = pop_val
      num1 = pop_val
      num1 >= num2 ? push_true : push_false
    end

    def do_cmp_lt
      num2 = pop_val
      num1 = pop_val
      num1 < num2 ? push_true : push_false
    end

    def do_cmp_lte
      num2 = pop_val
      num1 = pop_val
      num1 <= num2 ? push_true : push_false
    end

    def do_cmp_eq
      obj2 = pop_val
      obj1 = pop_val
      obj1.eq?(obj2) ? push_true : push_false
    end

    def do_cmp_eqv
      obj2 = pop_val
      obj1 = pop_val
      obj1.eqv?(obj2) ? push_true : push_false
    end

    def do_cmp_eq_num
      num2 = pop_val
      num1 = pop_val
      num1.is_a?(Int) && num1.eq?(num2) ? push_true : push_false
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
        if (address = pop)
          val = resolve(address)
          stdout_print(val)
        else
          stdout_print(nil)
        end
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
      count = pop_raw
      if count == 0
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

    def do_set_local
      name = fetch
      locals[name] = pop
    end

    def do_set_remote
      name = fetch
      frame_locals = @call_stack.reverse.lazy.map { |f| f[:locals] }.detect { |l| l.key?(name) }
      fail VariableUndefined.new(name) unless frame_locals
      frame_locals[name] = pop
    end

    def do_set_args
      count = pop_raw
      @call_args = (0...count).map { pop }.reverse
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
