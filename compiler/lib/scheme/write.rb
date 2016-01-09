class Compiler
  module Lib
    module Scheme
      module Write
        def write(args, options)
          [
            args.map { |arg| compile_sexp(arg, options.merge(use: true)) },
            VM::INT, VM::INT_WRITE
          ]
        end
      end
    end
  end
end
