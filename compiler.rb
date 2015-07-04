require_relative 'vm'
require 'pp'

class Compiler
  def initialize(sexps = nil, arguments: {})
    @sexps = sexps
    @locals = {}
    @arguments = arguments
  end

  attr_reader :locals, :arguments

  def compile(sexps = @sexps)
    compile_sexps(sexps) + [VM::HALT]
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
    sexps.each_with_index.flat_map do |sexp, index|
      compile_sexp(sexp, use: index == sexps.size - 1)
    end.flatten.compact
  end

  def compile_sexp(sexp, options = { use: false })
    return compile_literal(sexp, options) unless sexp.is_a?(Array)
    return [] if sexp.empty?
    (name, *args) = sexp
    if respond_to?(name, :include_private)
      send(name, args, options)
    else
      call(sexp, options)
    end
  end

  def compile_literal(literal, options = { use: false })
    if literal =~ /\A[a-z]/
      [
        push_var(literal, options),
        pop_maybe(options)
      ]
    else
      [
        VM::PUSH_NUM,
        literal,
        pop_maybe(options)
      ]
    end
  end

  def def((name, val), options)
    index = local_num(name)
    [
      compile_sexp(val, options.merge(use: true)),
      VM::SET_LOCAL, index
    ]
  end

  def fn((args, *body), options)
    args = args.map do |name|
      [
        VM::PUSH_ARG,
        VM::SET_LOCAL, local_num(name)
      ]
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
    [
      args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
      VM::PUSH_NUM, args.size,
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

  def push_var(name, _options)
    num = local_num(name)
    [VM::PUSH_LOCAL, num]
  end

  def pop_maybe(options)
    return VM::POP unless options[:use]
  end

  def local_num(name)
    locals[name] || locals[name] = locals.values.size
  end
end
