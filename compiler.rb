require_relative 'vm'

class Compiler
  def initialize(sexps = nil, arguments: {})
    @sexps = sexps
    @locals = {}
    @arguments = arguments
  end

  attr_reader :locals, :arguments

  def compile(sexps = @sexps)
    sexps.flat_map do |sexp|
      compile_sexp(sexp)
    end.flatten.compact
  end

  private

  def compile_sexp(sexp, options = { use: false })
    return compile_literal(sexp, options) unless sexp.is_a?(Array)
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
    args = args.each_with_index.each_with_object({}) { |(a, i), h| h[a] = i }
    body = compile(body)
    [
      VM::PUSH_FUNC,
      body,
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
    [
      compile_sexp(condition, options.merge(use: true)),
      VM::JUMP_IF_TRUE, :if_0_true,
      VM::JUMP, :if_0_false,
      VM::LABEL, :if_0_true,
      compile_sexp(true_body, options.merge(use: true)),
      VM::LABEL, :if_0_false,
      compile_sexp(false_body, options.merge(use: true)),
      pop_maybe(options)
    ]
  end

  {
    '>'  => VM::CMP_GT,
    '>=' => VM::CMP_GTE,
    '<'  => VM::CMP_LT,
    '<=' => VM::CMP_LTE,
    '==' => VM::CMP_EQ,
    '+'  => VM::ADD
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
