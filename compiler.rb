require_relative 'vm'
require_relative 'compiler/macro'
require_relative 'compiler/optimizer'
require 'pp'

class Compiler
  ROOT_PATH = VM::ROOT_PATH

  def initialize(sexps = [], filename: nil, arguments: {})
    @sexps = sexps
    @variables = {}
    @filename = filename
    @arguments = arguments
    @syntax = {}              # macro transformers
    @mangled_identifiers = {} # used for macro hygiene
    @source = {}              # records line and column numbers for identifiers
  end

  attr_reader :variables, :filename, :arguments, :syntax, :source

  def compile(sexps = [], keep_last: false, halt: true)
    @sexps += sexps
    instructions = compile_sexps(@sexps, { syntax: @syntax }, filename: filename, keep_last: keep_last)
    instructions << VM::HALT if halt
    optimize(instructions)
  end

  def include_code(paths)
    paths.map do |path|
      filename = "#{path}.scm"
      @sexps += parse_file(filename)
    end
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

  def mangle_identifier(name)
    @mangled_identifiers[name] ||= 0
    version = @mangled_identifiers[name] += 1
    "##{name}.v#{version}"
  end

  def built_in_function?(name)
    !built_in_function_name(name).nil?
  end

  def built_in_function_name(name)
    underscored_name = 'do_' + name.to_s.gsub('->', '_to_').gsub(/([a-z])-/, '\1_')
    underscored_name if respond_to?(underscored_name, :include_private)
  end

  private

  def compile_sexps(sexps, options = {}, filename:, keep_last: false)
    options[:locals] ||= {}
    source[filename] = {}
    sexps
      .each_with_index
      .map { |s, i| compile_sexp(s, options.merge(use: i == sexps.size - 1 && keep_last)) }
      .flatten
      .compact
      .each_with_index
      .map do |instr, index|
        stringify_and_record_location(instr, index)
      end
  end

  def optimize(instructions)
    Optimizer.new(instructions).optimize
  end

  def stringify_and_record_location(instr, index)
    if instr.is_a?(Parslet::Slice)
      (line, column) = instr.line_and_column
      @source[filename][index] = {
        line: line,
        column: column
      }
      instr.to_s
    else
      instr
    end
  end

  # rubocop:disable Metrics/PerceivedComplexity
  def compile_sexp(sexp, options = { use: false, locals: {} })
    sexp = sexp.to_ruby if sexp.is_a?(VM::Pair)
    return compile_literal(sexp, options) unless sexp.is_a?(Array)
    sexp.compact! # datum comments #;(...) come in as nil due to our parser :-(
    return [] if sexp.empty?
    (name, *args) = sexp
    if options[:quote] || options[:quasiquote]
      compile_quoted_sexp(sexp, options)
    elsif name.is_a?(Array) || name.is_a?(VM::Pair)
      call(sexp, options)
    elsif (built_in_name = built_in_function_name(name))
      send(built_in_name, args, options)
    elsif (macro = find_syntax(name.to_s, options))
      compile_macro_sexp(name, sexp, macro, options)
    else
      call(sexp, options)
    end
  end
  # rubocop:enable Metrics/PerceivedComplexity

  def compile_quoted_sexp(sexp, options)
    (name, *_args) = sexp
    if name.to_s =~ /unquote(\-splicing)?/ && options[:quasiquote]
      compile_quasiquoted_sexp(sexp, options)
    elsif sexp.size == 3 && sexp[1] == '.'
      do_pair(sexp, options)
    else
      do_list(sexp, options)
    end
  end

  def compile_quasiquoted_sexp(sexp, options)
    (name, *args) = sexp
    expr = compile_sexp(args.first, options.merge(quasiquote: false))
    if name.to_s == 'unquote-splicing'
      fail 'can only use unquote-splicing with a list' if expr.compact.last != VM::PUSH_LIST
      ['splice', expr.first]
    else
      expr
    end
  end

  def compile_literal(literal, options = { use: false, locals: {} })
    case literal.to_s.strip
    when /\A[a-z]|\A#[a-z].*\.v\d+$/
      compile_atom(literal, options)
    when '#t', '#true', '#f', '#false'
      compile_boolean(literal, options)
    when /\A#\\(.+)\z/
      compile_character($1, options)
    when /\A"(.*)"\z/
      compile_string($1, options)
    when ''
      []
    else
      compile_number(literal, options)
    end
  end

  def compile_atom(name, options)
    if options[:quote] || options[:quasiquote]
      [VM::PUSH_ATOM, name]
    else
      [
        push_var(name, options),
        pop_maybe(options)
      ]
    end
  end

  def compile_boolean(name, options)
    case name
    when '#t', '#true'
      [VM::PUSH_TRUE, pop_maybe(options)]
    when '#f', '#false'
      [VM::PUSH_FALSE, pop_maybe(options)]
    end
  end

  def compile_character(name, options)
    char = {
      'space'   => ' ',
      'newline' => "\n"
    }.fetch(name, name[0])
    [VM::PUSH_CHAR, char, pop_maybe(options)]
  end

  def compile_string(string, _options)
    [VM::PUSH_STR, string]
  end

  def compile_number(number, options)
    [
      VM::PUSH_NUM,
      number,
      pop_maybe(options)
    ]
  end

  def compile_macro_sexp(_name, sexp, macro, options)
    sexp = Macro.new(macro, self).compile(sexp)
    compile_sexp(sexp, options)
  end

  def do_exit((arg, *_rest), _option)
    [
      arg && compile_sexp(arg, use: true),
      VM::HALT
    ]
  end

  def do_car((arg, *_rest), options)
    [
      compile_sexp(arg, options.merge(use: true)),
      VM::PUSH_CAR,
      pop_maybe(options)
    ]
  end

  def do_cdr((arg, *_rest), options)
    [
      compile_sexp(arg, options.merge(use: true)),
      VM::PUSH_CDR,
      pop_maybe(options)
    ]
  end

  def do_cons(args, options)
    fail 'cons expects exactly 2 arguments' if args.size != 2
    [
      args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
      VM::PUSH_CONS,
      pop_maybe(options)
    ]
  end

  def do_set_car!((pair, new_car), options)
    [
      compile_sexp(pair, options.merge(use: true)),
      compile_sexp(new_car, options.merge(use: true)),
      VM::SET_CAR
    ]
  end

  def do_set_cdr!((pair, new_cdr), options)
    [
      compile_sexp(pair, options.merge(use: true)),
      compile_sexp(new_cdr, options.merge(use: true)),
      VM::SET_CDR
    ]
  end

  def do_append(args, options)
    [
      args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
      VM::PUSH_NUM, args.size,
      VM::APPEND,
      pop_maybe(options)
    ]
  end

  def do_null?((arg, *_rest), options)
    [
      compile_sexp(arg, options.merge(use: true)),
      VM::CMP_NULL,
      pop_maybe(options)
    ]
  end

  {
    'pair?'   => VM::Pair,
    'string?' => VM::ByteArray
  }.each do |name, type|
    define_method "do_#{name}" do |(arg, *_rest), options|
      [
        compile_sexp(arg, options.merge(use: true)),
        VM::PUSH_TYPE,
        VM::PUSH_NUM, VM::TYPES.index(type),
        VM::CMP_EQ_NUM,
        VM::JUMP_IF_FALSE, 4,
        VM::PUSH_TRUE,
        VM::JUMP, 2,
        VM::PUSH_FALSE,
        pop_maybe(options)
      ]
    end
  end

  def do_string_ref((string, index), options)
    [
      compile_sexp(string, options.merge(use: true)),
      compile_sexp(index, options.merge(use: true)),
      VM::STR_REF,
      pop_maybe(options)
    ]
  end

  def do_string_length((string, *_rest), options)
    [
      compile_sexp(string, options.merge(use: true)),
      VM::STR_LEN,
      pop_maybe(options)
    ]
  end

  def do_list_to_string((list, *_rest), options)
    [
      compile_sexp(list, options.merge(use: true)),
      VM::LIST_TO_STR,
      pop_maybe(options)
    ]
  end

  def do_quote((arg, *_rest), options)
    if arg.is_a?(Array)
      compile_sexp(arg, options.merge(quote: true))
    else
      compile_literal(arg, options.merge(quote: true))
    end
  end

  def do_quasiquote((arg, *_rest), options)
    if arg.is_a?(Array)
      compile_sexp(arg, options.merge(quasiquote: true))
    else
      compile_literal(arg, options.merge(quasiquote: true))
    end
  end

  def do_debug(_args, _options)
    [VM::DEBUG]
  end

  def do_define((name, *body), options)
    if name.is_a?(Array)
      (name, *args) = name
      args = args.last if args.size == 2 && args.first == '.'
      options[:locals][name.to_s] = true
      do_lambda([args, *body], options.merge(use: true)) + [
        VM::SET_LOCAL, name
      ]
    else
      options[:locals][name.to_s] = true
      [
        compile_sexp(body.first, options.merge(use: true)),
        VM::SET_LOCAL, name
      ]
    end
  end

  def do_set!((name, val), options)
    op = options[:locals][name.to_s] ? VM::SET_LOCAL : VM::SET_REMOTE
    [
      compile_sexp(val, options.merge(use: true)),
      op, name
    ]
  end

  def do_lambda((args, *body), options)
    (locals, args) = compile_lambda_args(args)
    body = compile_lambda_body(body, locals, options)
    [
      VM::PUSH_FUNC,
      args,
      body,
      VM::RETURN,
      VM::ENDF,
      pop_maybe(options)
    ]
  end

  def compile_lambda_args(args)
    if args.is_a?(Array)
      compile_lambda_args_many(args)
    else
      compile_lambda_args_single(args)
    end
  end

  def compile_lambda_args_many(args)
    (named, _dot, rest) = args.slice_when { |i, j| [i, j].include?('.') }.to_a
    locals = (Array(named) + Array(rest)).each_with_object({}) do |arg, hash|
      hash[arg] = true
    end
    args = Array(named).map { |name| push_arg(name) }
    args += push_all_args(rest.first) if rest
    [locals, args]
  end

  def compile_lambda_args_single(arg)
    [
      { arg => true },
      push_all_args(arg)
    ]
  end

  def compile_lambda_body(body, locals, options)
    body_opts = options.merge(use: true, locals: locals, syntax: {}, parent_options: options)
    body.each_with_index.map do |sexp, index|
      compile_sexp(sexp, body_opts.merge(use: index == body.size - 1))
    end
  end

  def call((lambda, *args), options)
    [
      args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
      args.any? ? [VM::PUSH_NUM, args.size, VM::SET_ARGS] : nil,
      compile_sexp(lambda, options.merge(use: true)),
      VM::CALL
    ]
  end

  def do_apply((lambda, *args), options)
    fail 'apply expects at least 2 arguments' if args.empty?
    [
      args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
      VM::PUSH_NUM, args.size,
      VM::SET_ARGS,
      compile_sexp(lambda, options.merge(use: true)),
      VM::APPLY
    ]
  end

  def do_pair((car, _, cdr), options)
    [
      compile_sexp(car, options.merge(use: true)),
      compile_sexp(cdr, options.merge(use: true)),
      VM::PUSH_CONS,
      pop_maybe(options)
    ]
  end

  def do_list(args, options)
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

  def do_define_syntax((name, transformer), options)
    options[:syntax][name.to_s] = {
      locals: options[:locals].keys + options[:syntax].keys + [name.to_s],
      transformer: transformer
    }
    []
  end

  def do_if((condition, true_body, false_body), options)
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
    '+'    => VM::ADD,
    '-'    => VM::SUB,
    '>'    => VM::CMP_GT,
    '>='   => VM::CMP_GTE,
    '<'    => VM::CMP_LT,
    '<='   => VM::CMP_LTE,
    '='    => VM::CMP_EQ_NUM,
    'eq?'  => VM::CMP_EQ,
    'eqv?' => VM::CMP_EQV
  }.each do |name, instruction|
    define_method('do_' + name) do |args, options|
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

  def do_include(paths, options)
    paths.map do |path|
      fail "include expects a string, but got #{path.inspect}" unless path.to_s =~ /\A"(.+)?"\z/
      filename = "#{$1}.scm"
      sexps = parse_file(filename)
      compile_sexps(sexps, { syntax: options[:syntax], locals: options[:locals] }, filename: filename)
    end
  end

  def do_write(args, options)
    [
      args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
      VM::INT, VM::INT_WRITE
    ]
  end

  def push_var(name, options)
    [
      options[:locals][name.to_s] ? VM::PUSH_LOCAL : VM::PUSH_REMOTE,
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

  def parse_file(filename)
    code = File.read(File.join(ROOT_PATH, 'lib', filename))
    Parser.new(code).parse
  end

  # walk up the chain of options looking for a syntax definition
  def find_syntax(name, options)
    loop do
      return options[:syntax][name] if options[:syntax].key?(name)
      break unless (options = options[:parent_options])
    end
    nil
  end
end
