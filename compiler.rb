class Compiler
  def initialize(sexps = nil, arguments: {})
    @sexps = sexps
    @locals = {}
    @arguments = arguments
  end

  def compile(sexps = @sexps, locals: @locals, arguments: @arguments)
    sexps.flat_map do |sexp|
      compile_sexp(sexp, locals: locals, arguments: arguments)
    end.flatten.compact
  end

  private

  def compile_sexp(sexp, options = { locals: {}, arguments: {}, use: false })
    return compile_literal(sexp, options) unless sexp.is_a?(Array)
    (name, *args) = sexp
    if options[:locals][name] || options[:arguments][name]
      call(sexp, options)
    else
      send(name, args, options)
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
    index = options[:locals][name] || options[:locals][name] = options[:locals].values.size
    [
      compile_sexp(val, options.merge(use: true)),
      VM::SET_LOCAL, index
    ]
  end

  def fn((args, *body), options)
    args = args.each_with_index.each_with_object({}) { |(a, i), h| h[a] = i }
    body = compile(body, locals: {}, arguments: args)
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

  def push_var(name, options)
    local_num = options[:locals][name]
    arg_num = options[:arguments][name]
    fail "cannot find #{name}" unless local_num || arg_num
    if local_num
      [VM::PUSH_LOCAL, local_num]
    else
      [VM::PUSH_ARG, arg_num]
    end
  end

  def pop_maybe(options)
    return VM::POP unless options[:use]
  end
end
