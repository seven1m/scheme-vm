require_relative 'parser'
require_relative 'vm/atom'
require_relative 'vm/bool_false'
require_relative 'vm/bool_true'
require_relative 'vm/byte_array'
require_relative 'vm/call_stack_printer'
require_relative 'vm/char'
require_relative 'vm/continuation'
require_relative 'vm/debug_output'
require_relative 'vm/empty_list'
require_relative 'vm/exceptions'
require_relative 'vm/gc'
require_relative 'vm/int'
require_relative 'vm/operations'
require_relative 'vm/pair'
require_relative 'vm/pretty_printer'
require_relative 'vm/source_code_error_printer'
require_relative 'vm/unspecified'
require 'pry'

class VM
  include Operations

  ROOT_PATH = File.expand_path('..', __FILE__)

  MAX_CALL_DEPTH = 10

  INSTRUCTIONS = [
    ['ADD',           0],
    ['APPEND',        0],
    ['APPLY',         0],
    ['CALL',          0],
    ['CALL_WITH_CC',  0],
    ['CAR',           0],
    ['CDR',           0],
    ['CMP_EQ',        0],
    ['CMP_GT',        0],
    ['CMP_LT',        0],
    ['CMP_NULL',      0],
    ['CONS',          0],
    ['DEBUG',         0],
    ['DEFINE_VAR',    1],
    ['DIV',           0],
    ['DUP',           0],
    ['ENDF',          0],
    ['ENDL',          0],
    ['HALT',          0],
    ['IMPORT_LIB',    3],
    ['INT',           1],
    ['JUMP',          1],
    ['JUMP_IF_FALSE', 1],
    ['MOD',           0],
    ['MUL',           0],
    ['NAME_ARG',      1],
    ['NOOP',          0],
    ['POP',           0],
    ['PUSH_ARG',      0],
    ['PUSH_ARGS',     0],
    ['PUSH_ATOM',     1],
    ['PUSH_FALSE',    0],
    ['PUSH_FUNC',     0],
    ['PUSH_LIB',      0],
    ['PUSH_LIST',     0],
    ['PUSH_NUM',      1],
    ['PUSH_STR',      1],
    ['PUSH_TRUE',     0],
    ['PUSH_UNDEF',    0],
    ['PUSH_VAR',      1],
    ['RAW',           0],
    ['RETURN',        0],
    ['SET_ARGS',      0],
    ['SET_CAR',       0],
    ['SET_CDR',       0],
    ['SET_LIB',       1],
    ['SET_VAR',       1],
    ['STR_LEN',       0],
    ['STR_REF',       0],
    ['SUB',           0],
    ['TO_ATOM',       0],
    ['TO_CHAR',       0],
    ['TO_STR',        0],
    ['TYPE',          0]
  ].freeze

  INSTRUCTIONS.each_with_index do |(name, _arity), index|
    const_set(name.to_sym, index)
  end

  INT_WRITE = 1

  TYPES = [
    VM::Atom,
    VM::BoolTrue,
    VM::BoolFalse,
    VM::ByteArray,
    VM::Char,
    VM::EmptyList,
    VM::Int,
    VM::Pair
  ].freeze

  attr_reader :stack, :heap, :ip, :call_stack, :closures, :call_args, :libs, :last_atom
  attr_accessor :stdout, :debug

  def initialize(instructions = [], args: [], stdout: $stdout)
    @ip = 0              # instruction pointer
    @stack = []          # operand stack
    @call_stack = []     # call frame stack
    @call_stack << { args: args, named_args: {} }
    @libs = {}           # library definitions
    @heap = []           # a heap "address" is an index into this array
    @call_args = []      # used for next CALL
    @closures = {}       # store function ip and locals available
    @closures[:global] = { locals: {} } # global variables
    @stdout = stdout
    @executable = []     # ranges of executable heap (w^x)
    @debug = 0
    load_code(instructions)
  end

  def execute(instructions = nil)
    load_code(instructions) if instructions
    while (instruction = fetch)
      break if instruction == HALT
      execute_instruction(instruction)
    end
  end

  def execute_instruction(instruction)
    debug_output = DebugOutput.new(self, instruction, debug) if debug >= 2
    debug_output.print_ip if debug >= 2
    case instruction
    when NOOP
      :noop
    when RETURN
      do_return(debug)
    when DEBUG
      binding.pry # rubocop:disable Lint/Debugger
    else
      name = INSTRUCTIONS[instruction].first
      send("do_#{name.downcase}")
    end
    debug_output.print_debug if debug >= 2
  end

  def fetch
    raise VM::FatalError.new(@call_stack, "#{resolve(@ip)} is not callable") unless executable?(@ip)
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
    return Unspecified.instance if address.nil?
    @heap[address] || raise("invalid address #{address}")
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

  def empty_list
    @empty_list_address ||= begin
      address = alloc
      @heap[address] = EmptyList.instance
      address
    end
  end

  def push_type(val)
    num = TYPES.index(val.class)
    push_val(Int.new(num))
  end

  def pop
    @stack.pop
  end

  def pop_val
    address = pop
    resolve(address)
  end

  def pop_raw
    address = pop
    val = resolve(address)
    val.raw
  end

  def pop_raw_and_delete
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
    resolve(address)
  end

  def stack_values
    @stack.map do |address|
      address && resolve(address)
    end
  end

  def alloc
    @heap.index(nil) || @heap.size
  end

  def closure
    key = @call_stack.last[:func] || :global
    @closures[key]
  end

  def locals
    closure[:locals]
  end

  def find_closure_with_symbol(name)
    c = closure
    loop do
      break unless c
      return c if c[:locals].key?(name)
      c = c[:parent]
    end
  end

  def find_address_for_name(name, raise_if_not_found: false)
    if (frame = find_call_stack_frame_with_symbol(name))
      frame[:named_args][name]
    elsif (c = find_closure_with_symbol(name))
      c[:locals][name]
    elsif (m = mangled_local(name))
      (lib, name) = m
      @libs[lib][:locals][name]
    elsif raise_if_not_found
      raise VariableUndefined, name
    end
  end

  def mangled_local(name)
    return unless name && (match = name.match(/\A#([^:]+):(.+)\z/))
    match[1..2]
  end

  def args
    @call_stack.last[:args]
  end

  def named_args
    @call_stack.last[:named_args]
  end

  def find_call_stack_frame_with_symbol(name)
    @call_stack.reverse.detect { |f| f[:named_args] && f[:named_args].key?(name) }
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

  def fetch_until(end_instruction)
    body = []
    while (instruction = fetch) != end_instruction
      (_name, arity) = INSTRUCTIONS[instruction]
      body << instruction
      body << fetch_until(ENDF) + [ENDF] if instruction == PUSH_FUNC
      body << fetch_until(ENDL) + [ENDL] if instruction == PUSH_LIB
      arity.times { body << fetch } # skip args
    end
    body.flatten
  end

  def load_code(instructions)
    return if instructions.empty?
    @ip = @heap.size
    @heap += instructions
    @executable << (@ip..(@heap.size - 1))
  end

  def return_value
    if peek
      val = pop_raw
      return val if val.is_a?(Integer)
      return 1 if val == false
    end
    0
  end
end
