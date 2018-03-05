class Compiler
  module Lib
    module Scheme
      module ProcessContext
        def process_exit((arg, *_rest), options)
          [
            arg && compile_sexp_use(arg, options),
            VM::HALT
          ]
        end
      end
    end
  end
end
