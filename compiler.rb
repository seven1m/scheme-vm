require_relative 'vm'
require_relative 'compiler/macro'
require_relative 'compiler/optimizer'
require_relative 'compiler/lib/scheme/base'
require_relative 'compiler/lib/scheme/process_context'
require 'pp'

class Compiler
  ROOT_PATH = VM::ROOT_PATH
  LOAD_PATH = [File.join(ROOT_PATH, 'lib'), File.join(ROOT_PATH, 'spec')]

  include Compiler::Lib::Scheme::Base
  include Compiler::Lib::Scheme::ProcessContext

  def initialize(code = nil, filename:, includes: [], arguments: {}, load_path: LOAD_PATH)
    @variables = {}
    @filename = filename
    @arguments = arguments
    @load_path = load_path
    @syntax = {}              # macro transformers
    @libs = {}                # loaded libraries
    @mangled_identifiers = {} # used for macro hygiene
    @source = {}              # store source code for each file compiled
    @source[filename] = code
    @sexps = []
    include_code(includes)
    @sexps += Parser.new(code, filename: filename).parse if code
  end

  attr_reader :variables, :filename, :arguments, :syntax, :source, :libs

  def compile(code = nil, keep_last: false, halt: true)
    @sexps += Parser.new(code, filename: filename).parse if code
    compile_sexps(@sexps, options: { syntax: @syntax }, halt: halt, keep_last: keep_last)
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
    name.sub(/.+/, "##{name}.v#{version}")
  end

  def built_in_function?(name)
    !built_in_function_name(name).nil?
  end

  def built_in_function_name(name)
    underscored_name = 'do_' + name.gsub('->', '_to_').gsub(/([a-z])-/, '\1_').gsub(/^--/, '')
    underscored_name if respond_to?(underscored_name, :include_private)
  end

  private

  def compile_sexps(sexps, options: {}, halt: false, keep_last: false)
    options[:locals] ||= {}
    instructions = sexps
                   .each_with_index
                   .map { |s, i| compile_sexp(s, options.merge(use: i == sexps.size - 1 && keep_last)) }
                   .flatten
                   .compact
    instructions << VM::HALT if halt
    optimize(instructions)
  end

  def optimize(instructions)
    Optimizer.new(instructions).optimize
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
    elsif name == 'include'
      do_include(args, name.filename, options)
    elsif name == 'import'
      do_import(args, name.filename, options)
    elsif (built_in_name = built_in_function_name(name))
      send(built_in_name, args, options)
    elsif (macro = find_syntax(name, options))
      compile_macro_sexp(sexp, macro, options)
    else
      call(sexp, options)
    end
  end
  # rubocop:enable Metrics/PerceivedComplexity

  def compile_quoted_sexp(sexp, options)
    (name, *_args) = sexp
    if name =~ /unquote(\-splicing)?/ && options[:quasiquote]
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
    if name == 'unquote-splicing'
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

  def compile_macro_sexp(sexp, macro, options)
    if (method_name = macro[:native_transformer])
      send(method_name, sexp[1..-1], options)
    else
      sexp = Macro.new(macro, self).compile(sexp)
      compile_sexp(sexp, options)
    end
  end

  def do_define_native((name, method_name), options)
    options[:syntax][name] = {
      locals: options[:locals].keys + options[:syntax].keys + [name],
      native_transformer: method_name
    }
    []
  end

  def do_debug(_args, _options)
    [VM::DEBUG]
  end

  def do_define((name, *body), options)
    if name.is_a?(Array)
      (name, *args) = name
      args = args.last if args.size == 2 && args.first == '.'
      options[:locals][name] = true
      do_lambda([args, *body], options.merge(use: true)) + [
        VM::DEFINE_VAR, name
      ]
    else
      options[:locals][name] = true
      [
        compile_sexp(body.first, options.merge(use: true)),
        VM::DEFINE_VAR, name
      ]
    end
  end

  def do_set!((name, val), options)
    [
      compile_sexp(val, options.merge(use: true)),
      VM::SET_VAR,
      name
    ]
  end

  def do_lambda((args, *body), options)
    (locals, args) = compile_lambda_args(args)
    body = compile_lambda_body(body, options[:locals].merge(locals), options)
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
    options[:syntax][name] = {
      locals: options[:locals].keys + options[:syntax].keys + [name],
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

  def do_include(paths, relative_to, options)
    paths.map do |path|
      fail "include expects a string, but got #{path.inspect}" unless path =~ /\A"(.+)?"\z/
      filename = "#{$1}.scm"
      sexps = parse_file(filename, relative_to: relative_to)
      compile_sexps(sexps, options: { syntax: options[:syntax], locals: options[:locals] })
    end
  end

  def do_import((*sets), relative_to, options)
    sets.map do |set|
      import_set(set, relative_to, options)
    end
  end

  def import_set(set, relative_to, options)
    (include, bindings) = import_set_bindings(set, relative_to, options)
    bindings.map(&:last).each { |name| options[:locals][name] = true }
    [
      include,
      bindings.map do |binding|
        [VM::IMPORT_LIB, binding]
      end
    ]
  end

  # This method and import_set_all below return an array [include, bindings];
  # bindings is an array that looks like this:
  #
  #     [library_name, internal_binding_name, external_binding_name]
  #
  # which is shortened as:
  #
  #     [n, i, e]
  #
  def import_set_bindings(set, relative_to, options)
    return import_set_all(set, relative_to, options) unless set[1].is_a?(Array)
    (directive, source, *identifiers) = set
    (include, bindings) = import_set_bindings(source, relative_to, options)
    available = bindings.each_with_object({}) { |(n, i, e), h| h[e] = [n, i, e] }
    case directive
    when 'only'
      bindings = available.values_at(*identifiers)
    when 'except'
      bindings = available.values_at(*(available.keys - identifiers))
    when 'prefix'
      prefix = identifiers.first
      bindings = bindings.map { |(n, i, e)| [n, i, prefix + e] }
    when 'rename'
      renamed = Hash[identifiers]
      bindings = bindings.map do |name, internal_name, external_name|
        [name, internal_name, renamed[external_name] || external_name]
      end
    else
      fail "unknown import directive #{directive}"
    end
    [include, bindings]
  end

  def import_set_all(set, relative_to, options)
    name = set.join('/')
    include = include_library_if_needed(name, relative_to, options)
    [
      include,
      @libs[name].map do |external_name, internal_name|
        [name, internal_name, external_name]
      end
    ]
  end

  def include_library_if_needed(name, relative_to, options)
    return [] if @libs.key?(name)
    do_include(["\"#{name}\""], relative_to, options)
  end

  def do_define_library((name, *declarations), options)
    exports = @libs[name.join('/')] = {}
    begins = []
    declarations.each do |(type, *args)|
      case type
      when 'export'
        exports.merge!(library_exports_as_hash(args))
      when 'begin'
        begins += args
      end
    end
    [
      VM::SET_LIB, name.join('/'),
      begins.map { |s| compile_sexp(s, options) },
      VM::ENDL
    ]
  end

  def library_exports_as_hash(exports)
    exports.each_with_object({}) do |export, hash|
      if export.is_a?(Array)
        (_, old_name, new_name) = export
        hash[new_name] = old_name
      else
        hash[export] = export
      end
    end
  end

  def do_write(args, options)
    [
      args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
      VM::INT, VM::INT_WRITE
    ]
  end

  def push_var(name, options)
    fail VM::VariableUndefined, name unless options[:locals][name]
    [
      VM::PUSH_VAR,
      name
    ]
  end

  def push_arg(name, _options = {})
    [
      VM::PUSH_ARG,
      VM::SET_ARG, name
    ]
  end

  def push_all_args(name, _options = {})
    [
      VM::PUSH_ARGS,
      VM::SET_ARG, name
    ]
  end

  def pop_maybe(options)
    return VM::POP unless options[:use]
  end

  def parse_file(filename, relative_to: nil)
    if filename =~ /\A\./ && relative_to
      path = File.join(File.dirname(relative_to), filename)
    else
      path = @load_path.map { |p| File.join(p, filename) }.detect { |p| File.exist?(p) }
    end
    fail "File #{filename} not found in load path #{@load_path.join(';')}" unless path
    code = File.read(path)
    @source[filename] = code
    Parser.new(code, filename: filename).parse
  end

  # walk up the chain of options looking for a syntax definition
  def find_syntax(name, options)
    loop do
      return options[:syntax][name] if options[:syntax].key?(name)
      break unless (options = options[:parent_options])
    end
    nil
  end

  def lispify(sexp)
    if sexp.is_a?(Array)
      '(' + sexp.map { |s| lispify(s) }.join(' ') + ')'
    else
      sexp
    end
  end
end
