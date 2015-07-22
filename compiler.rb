require_relative 'vm'
require 'pp'

class Compiler
  def initialize(sexps = nil, arguments: {})
    @sexps = sexps
    @variables = {}
    @arguments = arguments
  end

  attr_reader :variables, :arguments

  def compile(sexps = @sexps, jump: nil)
    instructions = compile_sexps(sexps)
    (@variables.any? ? [VM::VAR_NAMES, @variables.keys.join(' ')] : []) + \
    instructions + \
    (jump ? [VM::JUMP, jump] : [VM::HALT])
  end

  def pretty_format(instructions, grouped: false, ip: false)
    instructions = instructions.dup
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

  def compile_sexps(sexps)
    locals = {}
    sexps.each_with_index.flat_map do |sexp, index|
      compile_sexp(sexp, use: index == sexps.size - 1, locals: locals)
    end.flatten.compact
  end

  def compile_sexp(sexp, options = { use: false, locals: {} })
    return compile_literal(sexp, options) unless sexp.is_a?(Array)
    return [] if sexp.empty?
    (name, *args) = sexp
    if options[:quote] || options[:quasiquote]
      if name =~ /unquote(\-splicing)?/ && options[:quasiquote]
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
      fail "#{name} is not a lambda"
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
    index = var_num(name)
    options[:locals][index] = true
    [
      compile_sexp(val, options.merge(use: true)),
      VM::SET_LOCAL, index
    ]
  end

  def lambda((args, *body), options)
    if args.is_a?(Array)
      if args.include?('.')
        (named, _dot, rest) = args.slice_when { |i, j| [i, j].include?('.') }.to_a
        args = named.map { |name| push_arg(name) } + push_all_args(rest.first)
      else
        args = args.map { |name| push_arg(name) }
      end
    else
      args = push_all_args(args)
    end
    [
      VM::PUSH_FUNC,
      args,
      compile_sexps(body),
      VM::RETURN,
      VM::ENDF,
      pop_maybe(options)
    ]
  end

  def call((name, *args), options)
    [
      args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
      args.any? ? [VM::PUSH_NUM, args.size, VM::SET_ARGS] : nil,
      push_var(name, options),
      VM::CALL
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
    @ifs ||= 0
    @ifs += 1
    true_label = "if_#{@ifs}_true".to_sym
    false_label = "if_#{@ifs}_false".to_sym
    end_label = "if_#{@ifs}_end".to_sym
    [
      compile_sexp(condition, options.merge(use: true)),
      VM::JUMP_IF_TRUE, true_label,
      VM::JUMP, false_label,
      VM::LABEL, true_label,
      compile_sexp(true_body, options.merge(use: true)),
      VM::JUMP, end_label,
      VM::LABEL, false_label,
      compile_sexp(false_body, options.merge(use: true)),
      VM::LABEL, end_label,
      pop_maybe(options)
    ]
  end

  {
    '>'  => VM::CMP_GT,
    '>=' => VM::CMP_GTE,
    '<'  => VM::CMP_LT,
    '<=' => VM::CMP_LTE,
    '==' => VM::CMP_EQ,
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
    num = var_num(name)
    [
      options[:locals][num] ? VM::PUSH_LOCAL : VM::PUSH_REMOTE,
      num
    ]
  end

  def push_arg(name, _options = {})
    [
      VM::PUSH_ARG,
      VM::SET_LOCAL, var_num(name)
    ]
  end

  def push_all_args(name, _options = {})
    [
      VM::PUSH_ARGS,
      VM::SET_LOCAL, var_num(name)
    ]
  end

  def pop_maybe(options)
    return VM::POP unless options[:use]
  end

  def var_num(name)
    variables[name] || variables[name] = variables.values.size
  end
end
