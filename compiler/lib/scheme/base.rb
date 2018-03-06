class Compiler
  module Lib
    module Scheme
      module Base
        def base_append(args, options)
          [
            args.map { |arg| compile_sexp_use(arg, options) },
            VM::PUSH_NUM, args.size,
            VM::APPEND,
            pop_maybe(options)
          ]
        end

        def base_apply((lambda, *args), options)
          raise 'apply expects at least 2 arguments' if args.empty?
          [
            args.map { |arg| compile_sexp_use(arg, options) },
            VM::PUSH_NUM, args.size,
            VM::SET_ARGS,
            compile_sexp_use(lambda, options),
            VM::APPLY
          ]
        end

        def base_define((name, *body), options)
          if name.is_a?(Array)
            (name, *args) = name
            args = args.last if args.size == 2 && args.first == '.'
            options[:locals][name] = true
            base_lambda([args, *body], options.merge(use: true)) + [
              VM::DEFINE_VAR, name
            ]
          else
            options[:locals][name] = true
            [
              compile_sexp_use(body.first, options),
              VM::DEFINE_VAR, name
            ]
          end
        end

        def base_define_syntax((name, transformer), options)
          options[:syntax][name] = {
            locals: options[:locals].keys + options[:syntax].keys + [name],
            transformer: transformer,
            syntax: options[:syntax],
            lib: options[:lib]
          }
          []
        end

        def base_call_cc((fn), options)
          [
            compile_sexp_use(fn, options),
            VM::CALL_WITH_CC
          ]
        end

        def base_car((arg, *_rest), options)
          [
            compile_sexp_use(arg, options),
            VM::CAR,
            pop_maybe(options)
          ]
        end

        def base_cons(args, options)
          raise 'cons expects exactly 2 arguments' if args.size != 2
          [
            args.map { |arg| compile_sexp_use(arg, options) },
            VM::CONS,
            pop_maybe(options)
          ]
        end

        def base_cdr((arg, *_rest), options)
          [
            compile_sexp_use(arg, options),
            VM::CDR,
            pop_maybe(options)
          ]
        end

        def base_if((condition, true_body, false_body), options)
          true_instr  = compile_sexp_use(true_body, options).flatten.compact
          false_instr = compile_sexp_use(false_body, options).flatten.compact
          false_instr = [VM::PUSH_UNDEF] if false_instr.empty?
          [
            compile_sexp_use(condition, options),
            VM::JUMP_IF_FALSE, true_instr.size + 3,
            true_instr,
            VM::JUMP, false_instr.size + 1,
            false_instr,
            pop_maybe(options)
          ]
        end

        def base_lambda((args, *body), options)
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
          body_opts = options.merge(locals: locals, syntax: options[:syntax].dup)
          compile_sexps_use_last(body, options: body_opts)
        end

        def base_let_syntax((bindings, *body), options)
          compile_let_syntax_body(bindings, body, {}, options)
        end

        def base_letrec_syntax((bindings, *body), options)
          initial_bindings = bindings.each_with_object({}) do |(name), hash|
            hash[name] = true
          end
          compile_let_syntax_body(bindings, body, initial_bindings, options)
        end

        def compile_let_syntax_body(bindings, body, initial_bindings, options)
          body_opts = options.merge(
            use: true,
            locals: options[:locals].dup,
            syntax: options[:syntax].merge(initial_bindings)
          )
          bindings.each do |name, transformer|
            body_opts[:syntax][name] = {
              locals: body_opts[:locals].keys + body_opts[:syntax].keys + [name],
              transformer: transformer
            }
          end
          compile_sexps_use_last(body, options: body_opts)
        end

        def base_list(args, options)
          members = args.flat_map do |arg|
            expr = compile_sexp_use(arg, options)
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

        def base_list_to_string((list, *_rest), options)
          [
            compile_sexp_use(list, options),
            VM::TO_STR,
            pop_maybe(options)
          ]
        end

        def base_symbol_to_string((symbol, *_rest), options)
          [
            compile_sexp_use(symbol, options),
            VM::TO_STR,
            pop_maybe(options)
          ]
        end

        def base_null?((arg, *_rest), options)
          [
            compile_sexp_use(arg, options),
            VM::CMP_NULL,
            pop_maybe(options)
          ]
        end

        def base_quote((arg, *_rest), options)
          if arg.is_a?(Array)
            compile_sexp(arg, options.merge(quote: true))
          else
            compile_literal(arg, options.merge(quote: true))
          end
        end

        def base_quasiquote((arg, *_rest), options)
          if arg.is_a?(Array)
            compile_sexp(arg, options.merge(quasiquote: true))
          else
            compile_literal(arg, options.merge(quasiquote: true))
          end
        end

        def base_set_car!((pair, new_car), options)
          [
            compile_sexp_use(pair, options),
            compile_sexp_use(new_car, options),
            VM::SET_CAR
          ]
        end

        def base_set!((name, val), options)
          [
            compile_sexp_use(val, options),
            VM::SET_VAR,
            name
          ]
        end

        def base_set_cdr!((pair, new_cdr), options)
          [
            compile_sexp_use(pair, options),
            compile_sexp_use(new_cdr, options),
            VM::SET_CDR
          ]
        end

        def base_string(chars, options)
          [
            chars.map { |char| compile_sexp_use(char, options) },
            VM::PUSH_NUM, chars.size,
            VM::PUSH_LIST,
            VM::TO_STR,
            pop_maybe(options)
          ]
        end

        def base_string_length((string, *_rest), options)
          [
            compile_sexp_use(string, options),
            VM::STR_LEN,
            pop_maybe(options)
          ]
        end

        def base_string_ref((string, index), options)
          [
            compile_sexp_use(string, options),
            compile_sexp_use(index, options),
            VM::STR_REF,
            pop_maybe(options)
          ]
        end

        def base_char_to_integer((char), options)
          [
            compile_sexp_use(char, options),
            VM::RAW
          ]
        end

        def base_integer_to_char((int), options)
          [
            compile_sexp_use(int, options),
            VM::TO_CHAR
          ]
        end

        {
          'char?'    => VM::Char,
          'integer?' => VM::Int,
          'pair?'    => VM::Pair,
          'string?'  => VM::ByteArray,
          'symbol?'  => VM::Atom
        }.each do |name, type|
          define_method "base_#{name}" do |(arg, *_rest), options|
            [
              compile_sexp_use(arg, options),
              VM::TYPE,
              VM::PUSH_NUM, VM::TYPES.index(type),
              VM::CMP_EQ,
              VM::JUMP_IF_FALSE, 4,
              VM::PUSH_TRUE,
              VM::JUMP, 2,
              VM::PUSH_FALSE,
              pop_maybe(options)
            ]
          end
        end

        [
          ['+', VM::ADD, 0],
          ['-', VM::SUB, 0],
          ['*', VM::MUL, 1],
          ['/', VM::DIV, 1]
        ].each do |name, instruction, default_arg_value|
          define_method('base_' + name) do |args, options|
            default_arg_count = [(2 - args.size), 0].max
            args = ([default_arg_value.to_s] * default_arg_count) + args
            arith(instruction, args, options)
          end
        end

        def arith(instruction, args, options)
          args = args.map { |arg| compile_sexp_use(arg, options) }
          first_two = args.shift(2)
          rest = args.zip([instruction] * args.size)
          [
            first_two,
            instruction,
            rest,
            pop_maybe(options)
          ]
        end

        {
          'modulo' => VM::MOD,
          '>'      => VM::CMP_GT,
          '<'      => VM::CMP_LT,
          'eq?'    => VM::CMP_EQ
        }.each do |name, instruction|
          define_method('base_' + name) do |args, options|
            compare(instruction, args, options)
          end
        end

        def compare(instruction, args, options)
          raise "wrong number of arguments (expected 2, got #{args.size})" if args.size != 2
          [
            args.map { |arg| compile_sexp_use(arg, options) },
            instruction,
            pop_maybe(options)
          ]
        end
      end
    end
  end
end
