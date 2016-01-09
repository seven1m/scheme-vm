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
