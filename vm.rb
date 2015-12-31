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
require_relative 'vm/operations'
require_relative 'parser'
require_relative 'compiler'
require 'pry'

class VM
  include Operations

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
    ['SET_CAR',       0],
    ['SET_CDR',       0],
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

  def execute(instructions = nil, debug: 0)
    @start = 0
    if instructions
      @start = @heap.size + 1
      load_code(instructions)
    end
    while (instruction = fetch)
      break if instruction == HALT
      execute_instruction(instruction, debug: debug)
    end
  end

  def execute_instruction(instruction, debug: 0)
    debug_output = DebugOutput.new(self, instruction, debug) if debug >= 2
    debug_output.print_ip if debug >= 2
    case instruction
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

  def pop
    @stack.pop
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
