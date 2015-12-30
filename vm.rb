require_relative 'vm/atom'
require_relative 'vm/int'
require_relative 'vm/byte_array'
require_relative 'vm/char'
require_relative 'vm/pair'
require_relative 'vm/empty_list'
require_relative 'vm/bool_true'
require_relative 'vm/bool_false'
require_relative 'vm/exceptions'
require_relative 'vm/gc'
require_relative 'vm/debug_output'
require_relative 'parser'
require_relative 'compiler'
require 'pry'

class VM
  ROOT_PATH = File.expand_path('..', __FILE__)

  MAX_CALL_DEPTH = 50

  INSTRUCTIONS = [
    ['PUSH_ATOM',     1],
    ['PUSH_NUM',      1],
    ['PUSH_STR',      1],
    ['PUSH_CHAR',     1],
    ['PUSH_TRUE',     0],
    ['PUSH_FALSE',    0],
    ['PUSH_TYPE',     0],
    ['PUSH_CAR',      0],
    ['PUSH_CDR',      0],
    ['PUSH_CONS',     0],
    ['PUSH_LIST',     0],
    ['PUSH_LOCAL',    1],
    ['PUSH_REMOTE',   1],
    ['PUSH_ARG',      0],
    ['PUSH_ARGS',     0],
    ['PUSH_FUNC',     0],
    ['STR_REF',       0],
    ['STR_LEN',       0],
    ['LIST_TO_STR',   0],
    ['APPEND',        0],
    ['POP',           0],
    ['ADD',           0],
    ['SUB',           0],
    ['CMP_GT',        0],
    ['CMP_GTE',       0],
    ['CMP_LT',        0],
    ['CMP_LTE',       0],
    ['CMP_EQ',        0],
    ['CMP_EQV',       0],
    ['CMP_EQ_NUM',    0],
    ['CMP_NULL',      0],
    ['DUP',           0],
    ['ENDF',          0],
    ['INT',           1],
    ['JUMP',          1],
    ['JUMP_IF_FALSE', 1],
    ['CALL',          0],
    ['APPLY',         0],
    ['RETURN',        0],
    ['SET_LOCAL',     1],
    ['SET_REMOTE',    1],
    ['SET_ARGS',      0],
    ['HALT',          0],
    ['DEBUG',         0]
  ]

  INSTRUCTIONS.each_with_index do |(name, _arity), index|
    const_set(name.to_sym, index)
  end

  INT_WRITE   = 1
  INT_INCLUDE = 3

  TYPES = [
    VM::BoolTrue,
    VM::BoolFalse,
    VM::ByteArray,
    VM::Char,
    VM::EmptyList,
    VM::Int,
    VM::Pair
  ]

  attr_reader :stack, :heap, :stdout, :ip, :call_stack, :call_args

  def initialize(instructions = [], args: [], stdout: $stdout)
    @ip = 0              # instruction pointer
    @stack = []          # operand stack
    @call_stack = []     # call frame stack
    @call_stack << { locals: {}, args: args }
    @heap = []           # a heap "address" is an index into this array
    @call_args = []      # used for next CALL
    @stdout = stdout
    @executable = []     # ranges of executable heap (w^x)
    load_code(instructions)
  end

  # rubocop:disable Metrics/PerceivedComplexity, Metrics/MethodLength, Metrics/CyclomaticComplexity, Metrics/AbcSize
  def execute(instructions = nil, debug: 0)
    @start = 0
    if instructions
      @start = @heap.size + 1
      load_code(instructions)
    end
    while (instruction = fetch)
      print((@ip - 1).to_s.ljust(5)) if debug > 0
      case instruction
      when PUSH_ATOM
        name = fetch
        push_val(Atom.new(name))
      when PUSH_NUM
        num = fetch
        push_val(Int.new(num))
      when PUSH_STR
        str = fetch
        push_val(ByteArray.new(str))
      when PUSH_CHAR
        char = fetch
        push_val(Char.new(char))
      when PUSH_TRUE
        push_true
      when PUSH_FALSE
        push_false
      when PUSH_TYPE
        val = pop_val
        push_type(val)
      when PUSH_CAR
        pair = pop_val
        push(pair.address)
      when PUSH_CDR
        pair = pop_val
        push(pair.next_node)
      when PUSH_CONS
        cdr = pop
        car = pop
        pair = build_pair(car, cdr)
        push_val(pair)
      when PUSH_LIST
        push_list
      when PUSH_LOCAL
        push_local
      when PUSH_REMOTE
        push_remote
      when PUSH_ARG
        address = args.shift
        push(address)
      when PUSH_ARGS
        push_args
      when PUSH_FUNC
        push(@ip)
        fetch_func_body # discard
      when STR_REF
        str_ref
      when STR_LEN
        str_len
      when LIST_TO_STR
        list_to_str
      when APPEND
        append
      when POP
        pop
      when ADD
        num2 = pop_val
        num1 = pop_val
        push_val(num1 + num2)
      when SUB
        num2 = pop_val
        num1 = pop_val
        push_val(num1 - num2)
      when CMP_GT
        num2 = pop_val
        num1 = pop_val
        num1 > num2 ? push_true : push_false
      when CMP_GTE
        num2 = pop_val
        num1 = pop_val
        num1 >= num2 ? push_true : push_false
      when CMP_LT
        num2 = pop_val
        num1 = pop_val
        num1 < num2 ? push_true : push_false
      when CMP_LTE
        num2 = pop_val
        num1 = pop_val
        num1 <= num2 ? push_true : push_false
      when CMP_EQ
        obj2 = pop_val
        obj1 = pop_val
        obj1.eq?(obj2) ? push_true : push_false
      when CMP_EQV
        obj2 = pop_val
        obj1 = pop_val
        obj1.eqv?(obj2) ? push_true : push_false
      when CMP_EQ_NUM
        num2 = pop_val
        num1 = pop_val
        num1.is_a?(Int) && num1.eq?(num2) ? push_true : push_false
      when CMP_NULL
        val = pop_val
        val == EmptyList.instance ? push_true : push_false
      when DUP
        val = peek
        push(val)
      when INT
        int
      when JUMP
        count = fetch.to_i
        @ip += (count - 1)
      when JUMP_IF_FALSE
        val = pop
        count = fetch.to_i
        @ip += (count - 1) if val == bool_false
      when CALL
        do_call
      when APPLY
        do_apply
      when RETURN
        do_return(debug)
      when SET_LOCAL
        set_local
      when SET_REMOTE
        set_remote
      when SET_ARGS
        set_args
      when HALT
        break
      when DEBUG
        binding.pry # rubocop:disable Lint/Debugger
      end
      DebugOutput.new(self, instruction, debug).print_debug if debug > 0
    end
  end
  # rubocop:enable Metrics/PerceivedComplexity, Metrics/MethodLength, Metrics/CyclomaticComplexity, Metrics/AbcSize

  def fetch
    fail "heap[#{@ip}] is not executable" unless executable?(@ip)
    instruction = @heap[@ip]
    @ip += 1
    instruction
  end

  def executable?(address)
    @executable.any? { |range| range.include?(address) }
  end

  def writable?(address)
    !executable?(address)
  end

  def writable_heap
    @executable.each_with_object(@heap.dup) do |range, writable_heap|
      writable_heap[range] = [nil] * range.size
    end
  end

  def resolve(address)
    fail 'cannot lookup nil' if address.nil?
    @heap[address] || fail("invalid address #{address}")
  end

  def push(address)
    @stack.push(address)
  end

  def push_val(val)
    address = alloc
    @heap[address] = val
    push(address)
  end

  def bool_true
    @true_address ||= begin
      address = alloc
      @heap[address] = BoolTrue.instance
      address
    end
  end

  def push_true
    push(bool_true)
  end

  def push_type(val)
    num = TYPES.index(val.class)
    push_val(Int.new(num))
  end

  def push_local
    var = fetch
    address = locals[var]
    fail VariableUndefined.new(var, @ip - @start) unless address
    push(address)
  end

  def push_remote
    name = fetch
    frame_locals = @call_stack.reverse.lazy.map { |f| f[:locals] }.detect { |l| l[name] }
    fail VariableUndefined.new(name, @ip - @start) unless frame_locals
    address = frame_locals.fetch(name)
    push(address)
  end

  def push_args
    last = empty_list
    while (arg = args.pop)
      address = alloc
      @heap[address] = build_pair(arg, last)
      last = address
    end
    push(address || empty_list)
  end

  def str_ref
    index = pop_val.raw
    str = pop_val
    push_val(Char.new(str.raw[index]))
  end

  def str_len
    str = pop_val
    push_val(Int.new(str.size))
  end

  def list_to_str
    list = pop_val
    chars = list.to_ruby.map(&:to_s)
    push_val(ByteArray.new(chars.join))
  end

  def do_call
    if @heap[@ip] == RETURN
      @call_stack.last[:args] = @call_args
    else
      @call_stack << { return: @ip, locals: {}, args: @call_args }
    end
    fail CallStackTooDeep, 'call stack too deep' if @call_stack.size > MAX_CALL_DEPTH
    @ip = pop
  end

  def do_apply
    pair = resolve(@call_args.pop)
    while pair != EmptyList.instance
      @call_args.push(pair.address)
      pair = @heap[pair.next_node]
    end
    @call_stack << { return: @ip, locals: {}, args: @call_args }
    fail CallStackTooDeep, 'call stack too deep' if @call_stack.size > MAX_CALL_DEPTH
    @ip = pop
  end

  def do_return(debug)
    frame = @call_stack.pop
    @ip = frame.fetch(:return)
    VM::GC.new(self).run(debug: debug)
  end

  def set_local
    name = fetch
    locals[name] = pop
  end

  def set_remote
    name = fetch
    frame_locals = @call_stack.reverse.lazy.map { |f| f[:locals] }.detect { |l| l[name] }
    fail VariableUndefined.new(var, @ip - @start) unless frame_locals
    frame_locals[name] = pop
  end

  def set_args
    count = pop_raw
    @call_args = (0...count).map { pop }.reverse
  end

  def bool_false
    @false_address ||= begin
      address = alloc
      @heap[address] = BoolFalse.instance
      address
    end
  end

  def push_false
    push(bool_false)
  end

  def push_list
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

  def empty_list
    @empty_list_address ||= begin
      address = alloc
      @heap[address] = EmptyList.instance
      address
    end
  end

  def pop
    @stack.pop
  end

  def int
    func = fetch
    case func
    when INT_WRITE
      if (address = pop)
        val = resolve(address)
        stdout_print(val)
      else
        stdout_print(nil)
      end
    when INT_INCLUDE
      load_library(pop_val.to_s)
    end
  end

  def append
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

  def pop_val
    address = pop
    fail NoStackValue, 'no value on stack' unless address
    resolve(address)
  end

  def pop_raw
    address = pop
    val = resolve(address)
    @heap[address] = nil
    val.raw
  end

  def peek
    @stack.last
  end

  def peek_val
    address = @stack.last
    fail 'no value on stack' unless address
    resolve(address)
  end

  def stack_values
    @stack.map do |address|
      resolve(address)
    end
  end

  def alloc
    @heap.index(nil) || @heap.size
  end

  def locals
    @call_stack.last[:locals]
  end

  def args
    @call_stack.last[:args]
  end

  def build_pair(car, cdr)
    Pair.new(car, cdr, heap: @heap)
  end

  def local_values
    locals.each_with_object({}) do |(name, address), hash|
      hash[name] = resolve(address)
    end
  end

  def stdout_print(val)
    stdout.print(val.to_s)
  end

  def fetch_func_body
    body = []
    while (instruction = fetch) != ENDF
      (_name, arity) = INSTRUCTIONS[instruction]
      body << instruction
      body << fetch_func_body + [ENDF] if instruction == PUSH_FUNC
      arity.times { body << fetch } # skip args
    end
    body.flatten
  end

  def load_code(instructions, execute: false)
    return if instructions.empty?
    ip_was = @ip
    @ip = @heap.size
    @heap += instructions
    @executable << (@ip..(@heap.size - 1))
    return unless execute
    self.execute
    @ip = ip_was
  end

  def load_library(name, syntax: {})
    filename = "#{name}.scm"
    compiler = Compiler.new(lib_sexps(filename), filename: filename, syntax: syntax)
    code = compiler.compile
    load_code(code, execute: true)
    { code: code, syntax: compiler.syntax }
  end

  def lib_sexps(lib)
    code = lib_code(lib)
    Parser.new(code).parse
  end

  def lib_code(filename)
    File.read(File.join(ROOT_PATH, 'lib', filename))
  end
end
