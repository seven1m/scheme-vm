require_relative 'vm'
require 'pp'

class Compiler
  def initialize(sexps = nil, arguments: {})
    @sexps = sexps
    @variables = {}
    @arguments = arguments
  end

  attr_reader :variables, :arguments

  def compile(sexps = @sexps, halt: true)
    instructions = compile_sexps(sexps)
    instructions + \
    (halt ? [VM::HALT] : [])
  end

  def pretty_format(instructions, grouped: false, ip: false)
    instructions = instructions.dup.flatten.compact
    count = 0
    [].tap do |pretty|
      while instructions.any?
        group = []
        group << count if ip
        if (instruction = instructions.shift)
          (name, arity) = VM::INSTRUCTIONS[instruction]
          group << "VM::#{name}"
          arity.times { group << instructions.shift }
          pretty << group
          count += group.size - 1
        else
          pretty << nil
        end
      end
    end.send(grouped ? :to_a : :flatten)
  end

  def pretty_print(instructions)
    pp pretty_format(instructions, grouped: true, ip: true)
  end

  private

  def compile_sexps(sexps, options = {})
    options[:locals] ||= {}
    sexps.each_with_index.flat_map do |sexp, index|
      compile_sexp(sexp, options.merge(use: index == sexps.size - 1))
    end.flatten.compact
  end

  def compile_sexp(sexp, options = { use: false, locals: {} })
    return compile_literal(sexp, options) unless sexp.is_a?(Array)
    return [] if sexp.empty?
    (name, *args) = sexp
    if options[:quote] || options[:quasiquote]
      if name =~ /unquote(\-splicing)?/ && options[:quasiquote] # FIXME move to a method
        expr = compile_sexp(args.first, options.merge(quasiquote: false))
        if name == 'unquote-splicing'
          fail "can only use unquote-splicing with a list" if expr.compact.last != VM::PUSH_LIST
          ['splice', expr.first]
        else
          expr
        end
      else
        list(sexp, options)
      end
    elsif name.is_a?(Array)
      call(sexp, options)
    elsif respond_to?(name.gsub(/[a-z]-/, '$1_'), :include_private)
      send(name, args, options)
    else
      call(sexp, options)
    end
  end

  # TODO rename this
  def compile_literal(literal, options = { use: false, locals: {} })
    case literal
    when /\A[a-z]/
      if options[:quote] || options[:quasiquote]
        [VM::PUSH_ATOM, literal]
      else
        [
          push_var(literal, options),
          pop_maybe(options)
        ]
      end
    when /\A#t/
      [VM::PUSH_TRUE, pop_maybe(options)]
    when /\A#f/
      [VM::PUSH_FALSE, pop_maybe(options)]
    else
      [
        VM::PUSH_NUM,
        literal,
        pop_maybe(options)
      ]
    end
  end

  def car((arg, *_rest), options)
    [
      compile_sexp(arg, options.merge(use: true)),
      VM::PUSH_CAR,
      pop_maybe(options)
    ]
  end

  def cdr((arg, *_rest), options)
    [
      compile_sexp(arg, options.merge(use: true)),
      VM::PUSH_CDR,
      pop_maybe(options)
    ]
  end

  def cons(args, options)
    fail 'cons expects exactly 2 arguments' if args.size != 2
    [
      args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
      VM::PUSH_CONS,
      pop_maybe(options)
    ]
  end

  def null?((arg, *_rest), options)
    [
      compile_sexp(arg, options.merge(use: true)),
      VM::CMP_NULL,
      pop_maybe(options)
    ]
  end

  def quote((arg, *_rest), options)
    if arg.is_a?(Array)
      compile_sexp(arg, options.merge(quote: true))
    else
      compile_literal(arg, options.merge(quote: true))
    end
  end

  def quasiquote((arg, *_rest), options)
    if arg.is_a?(Array)
      compile_sexp(arg, options.merge(quasiquote: true))
    else
      compile_literal(arg, options.merge(quasiquote: true))
    end
  end

  def define((name, val), options)
    options[:locals][name] = true
    [
      compile_sexp(val, options.merge(use: true)),
      VM::SET_LOCAL, name
    ]
  end

  def lambda((args, *body), options)
    arg_locals = {}
    if args.is_a?(Array)
      if args.include?('.')
        (named, _dot, rest) = args.slice_when { |i, j| [i, j].include?('.') }.to_a
        arg_locals = (named + rest).each_with_object({}) { |arg, hash| hash[arg] = true }
        args = named.map { |name| push_arg(name) } + push_all_args(rest.first)
      else
        arg_locals = args.each_with_object({}) { |arg, hash| hash[arg] = true }
        args = args.map { |name| push_arg(name) }
      end
    else
      arg_locals = { args => true }
      args = push_all_args(args)
    end
    [
      VM::PUSH_FUNC,
      args,
      compile_sexps(body, locals: arg_locals),
      VM::RETURN,
      VM::ENDF,
      pop_maybe(options)
    ]
  end

  def call((lambda, *args), options)
    [
      args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
      args.any? ? [VM::PUSH_NUM, args.size, VM::SET_ARGS] : nil,
      compile_sexp(lambda, options.merge(use: true)),
      VM::CALL
    ]
  end

  def apply((lambda, *args), options)
    fail 'apply expects at least 2 arguments' if args.empty?
    [
      args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
      VM::PUSH_NUM, args.size,
      VM::SET_ARGS,
      compile_sexp(lambda, options.merge(use: true)),
      VM::APPLY
    ]
  end

  def list(args, options)
    members = args.flat_map do |arg|
      expr = compile_sexp(arg, options.merge(use: true))
      if expr.first == 'splice'
        expr.last
      else
        [expr]
      end
    end
    [
      members,
      VM::PUSH_NUM, members.size,
      VM::PUSH_LIST,
      pop_maybe(options)
    ]
  end

  def if((condition, true_body, false_body), options)
    true_instr  = compile_sexp(true_body, options.merge(use: true)).flatten.compact
    false_instr = compile_sexp(false_body, options.merge(use: true)).flatten.compact
    [
      compile_sexp(condition, options.merge(use: true)),
      VM::JUMP_IF_FALSE, true_instr.size + 3,
      true_instr,
      VM::JUMP, false_instr.size + 1,
      false_instr,
      pop_maybe(options)
    ]
  end

  {
    '>'  => VM::CMP_GT,
    '>=' => VM::CMP_GTE,
    '<'  => VM::CMP_LT,
    '<=' => VM::CMP_LTE,
    '='  => VM::CMP_EQ,
    '+'  => VM::ADD,
    '-'  => VM::SUB
  }.each do |name, instruction|
    define_method(name) do |args, options|
      compare(instruction, args, options)
    end
  end

  def compare(instruction, (arg1, arg2), options)
    [
      compile_sexp(arg1, options.merge(use: true)),
      compile_sexp(arg2, options.merge(use: true)),
      instruction,
      pop_maybe(options)
    ]
  end

  def print(args, options)
    [
      args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
      VM::INT, VM::INT_PRINT_VAL
    ]
  end

  def push_var(name, options)
    [
      options[:locals][name] ? VM::PUSH_LOCAL : VM::PUSH_REMOTE,
      name
    ]
  end

  def push_arg(name, _options = {})
    [
      VM::PUSH_ARG,
      VM::SET_LOCAL, name
    ]
  end

  def push_all_args(name, _options = {})
    [
      VM::PUSH_ARGS,
      VM::SET_LOCAL, name
    ]
  end

  def pop_maybe(options)
    return VM::POP unless options[:use]
  end
end
