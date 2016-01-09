class Compiler
  module Lib
    module Scheme
      module Base
        def base_car((arg, *_rest), options)
          [
            compile_sexp(arg, options.merge(use: true)),
            VM::PUSH_CAR,
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
      end
    end
  end
end
