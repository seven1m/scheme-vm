require_relative 'vm'
require_relative 'compiler/macro'
require_relative 'compiler/optimizer'
require_relative 'compiler/libraries'
require_relative 'compiler/lib/scheme/base'
require_relative 'compiler/lib/scheme/process_context'
require_relative 'compiler/lib/scheme/write'
require 'pp'
require 'pry'

class Compiler
  ROOT_PATH = VM::ROOT_PATH
  LOAD_PATH = [File.join(ROOT_PATH, 'lib'), File.join(ROOT_PATH, 'spec')].freeze

  include Compiler::Libraries
  include Compiler::Lib::Scheme::Base
  include Compiler::Lib::Scheme::ProcessContext
  include Compiler::Lib::Scheme::Write

  def initialize(code = nil, filename:, arguments: {}, load_path: LOAD_PATH)
    @variables = {}
    @filename = filename
    @arguments = arguments
    @load_path = load_path
    @syntax = {}              # macro transformers
    @locals = {}              # top-level locals (globals)
    @libs = {}                # loaded libraries
    @mangled_identifiers = {} # used for macro hygiene
    @source = {}              # store source code for each file compiled
    @source[filename] = code
    @sexps = []
    @sexps += Parser.new(code, filename: filename).parse if code
  end

  attr_reader :variables, :arguments, :syntax, :source, :libs
  attr_accessor :filename

  def compile(code = nil, keep_last: false, halt: true)
    if code
      @source[@filename] = code
      @sexps = Parser.new(code, filename: filename).parse
    end
    compile_sexps(@sexps, options: { syntax: @syntax, locals: @locals }, halt: halt, keep_last: keep_last)
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
    underscored_name = 'do_' + name.gsub(/([a-z])-/, '\1_').gsub(/^--/, '')
    underscored_name if respond_to?(underscored_name, :include_private)
  end

  private

  def compile_sexps(sexps, options:, halt: false, keep_last: false)
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

  def compile_sexp(sexp, options = { use: false, locals: {} })
    sexp = sexp.to_ruby if sexp.is_a?(VM::Pair)
    return compile_literal(sexp, options) unless sexp.is_a?(Array)
    sexp.compact! # datum comments #;(...) come in as nil due to our parser :-(
    return [] if sexp.empty?
    dispatch(sexp, options)
  end

  def dispatch(sexp, options)
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
    elsif (macro = options[:syntax][name])
      compile_macro_sexp(sexp, macro, options)
    elsif options[:locals][name]
      call(sexp, options)
    else
      raise VM::VariableUndefined, name
    end
  end

  def compile_quoted_sexp(sexp, options)
    (name, *_args) = sexp
    if name =~ /unquote(\-splicing)?/ && options[:quasiquote]
      compile_quasiquoted_sexp(sexp, options)
    elsif sexp.size == 3 && sexp[1] == '.'
      compile_pair(sexp, options)
    else
      base_list(sexp, options)
    end
  end

  def compile_quasiquoted_sexp(sexp, options)
    (name, *args) = sexp
    expr = compile_sexp(args.first, options.merge(quasiquote: false))
    if name == 'unquote-splicing'
      raise 'can only use unquote-splicing with a list' if expr.compact.last != VM::PUSH_LIST
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

  def compile_string(string, options)
    [
      VM::PUSH_STR,
      string,
      pop_maybe(options)
    ]
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
      sexp = Macro.new(macro, self).expand(sexp)
      compile_sexp(sexp, options)
    end
  end

  def do_debug(_args, _options)
    [VM::DEBUG]
  end

  def do_debug_compile(_args, options)
    binding.pry # rubocop:disable Lint/Debugger
    []
  end

  def call((lambda, *args), options)
    [
      args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
      args.any? ? [VM::PUSH_NUM, args.size, VM::SET_ARGS] : nil,
      compile_sexp(lambda, options.merge(use: true)),
      VM::CALL
    ]
  end

  def compile_pair((car, _, cdr), options)
    [
      compile_sexp(car, options.merge(use: true)),
      compile_sexp(cdr, options.merge(use: true)),
      VM::PUSH_CONS,
      pop_maybe(options)
    ]
  end

  def push_var(name, options)
    raise VM::VariableUndefined, name unless options[:locals][name]
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
    path = if filename.start_with?('.') && relative_to
             File.join(File.dirname(relative_to), filename)
           else
             @load_path.map { |p| File.join(p, filename) }.detect { |p| File.exist?(p) }
           end
    raise "File #{filename} not found in load path #{@load_path.join(';')}" unless path
    code = File.read(path)
    @source[filename] = code
    Parser.new(code, filename: filename).parse
  end
end
