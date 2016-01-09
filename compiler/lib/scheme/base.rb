class Compiler
  module Lib
    module Scheme
      module Base
        def base_append(args, options)
          [
            args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
            VM::PUSH_NUM, args.size,
            VM::APPEND,
            pop_maybe(options)
          ]
        end

        def base_apply((lambda, *args), options)
          fail 'apply expects at least 2 arguments' if args.empty?
          [
            args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
            VM::PUSH_NUM, args.size,
            VM::SET_ARGS,
            compile_sexp(lambda, options.merge(use: true)),
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
              compile_sexp(body.first, options.merge(use: true)),
              VM::DEFINE_VAR, name
            ]
          end
        end

        def base_car((arg, *_rest), options)
          [
            compile_sexp(arg, options.merge(use: true)),
            VM::PUSH_CAR,
            pop_maybe(options)
          ]
        end

        def base_cons(args, options)
          fail 'cons expects exactly 2 arguments' if args.size != 2
          [
            args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
            VM::PUSH_CONS,
            pop_maybe(options)
          ]
        end

        def base_cdr((arg, *_rest), options)
          [
            compile_sexp(arg, options.merge(use: true)),
            VM::PUSH_CDR,
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
          body_opts = options.merge(use: true, locals: locals, syntax: {}, parent_options: options)
          body.each_with_index.map do |sexp, index|
            compile_sexp(sexp, body_opts.merge(use: index == body.size - 1))
          end
        end

        def base_list(args, options)
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

        def base_list_to_string((list, *_rest), options)
          [
            compile_sexp(list, options.merge(use: true)),
            VM::LIST_TO_STR,
            pop_maybe(options)
          ]
        end

        def base_null?((arg, *_rest), options)
          [
            compile_sexp(arg, options.merge(use: true)),
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
            compile_sexp(pair, options.merge(use: true)),
            compile_sexp(new_car, options.merge(use: true)),
            VM::SET_CAR
          ]
        end

        def base_set!((name, val), options)
          [
            compile_sexp(val, options.merge(use: true)),
            VM::SET_VAR,
            name
          ]
        end

        def base_set_cdr!((pair, new_cdr), options)
          [
            compile_sexp(pair, options.merge(use: true)),
            compile_sexp(new_cdr, options.merge(use: true)),
            VM::SET_CDR
          ]
        end

        def base_string_length((string, *_rest), options)
          [
            compile_sexp(string, options.merge(use: true)),
            VM::STR_LEN,
            pop_maybe(options)
          ]
        end

        def base_string_ref((string, index), options)
          [
            compile_sexp(string, options.merge(use: true)),
            compile_sexp(index, options.merge(use: true)),
            VM::STR_REF,
            pop_maybe(options)
          ]
        end

        {
          'pair?'   => VM::Pair,
          'string?' => VM::ByteArray
        }.each do |name, type|
          define_method "base_#{name}" do |(arg, *_rest), options|
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
      end
    end
  end
end
