class Compiler
  module Lib
    module Scheme
      module ProcessContext
        def process_exit((arg, *_rest), options)
          [
            arg && compile_sexp(arg, options.merge(use: true)),
            VM::HALT
          ]
        end
      end
    end
  end
end
