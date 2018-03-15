require_relative '../vm'
require_relative '../loader'
require_relative './import'

class Compiler
  module Libraries
    def do_define_native((name, method_name), options)
      options[:syntax][name] = {
        locals: options[:locals].keys + options[:syntax].keys + [name],
        native_transformer: method_name,
        syntax: options[:syntax],
        lib: options[:lib]
      }
      []
    end

    def do_include(paths, relative_to, options)
      paths.map do |path|
        raise "include expects a string, but got #{path.inspect}" unless path =~ /\A"(.+)?"\z/
        filename = $1
        sexps = load_and_parse_file(filename, relative_to: relative_to)
        compile_sexps(sexps, options: options)
      end
    end

    def load_and_parse_file(filename, relative_to:)
      loader = Loader.new(filename, relative_to: relative_to)
      loader.load
      @program.parse(loader.code, filename: loader.path)
    end

    def do_import((*sets), relative_to, options)
      import = Import.new(import_sets: sets, options: options, relative_to: relative_to, compiler: self)
      import.compile
    end

    def include_library_if_needed(library_name, relative_to, options)
      return [] if @libs.key?(library_name)
      filename = library_name.tr('.', '/') + '.scm'
      do_include(["\"#{filename}\""], relative_to, options)
    end

    def do_define_library((name, *declarations), options)
      name_as_string = name.join('.')
      exports = @libs[name_as_string] = {
        syntax: {},
        bindings: {}
      }
      imports = []
      begins = []
      lib_opts = options.merge(syntax: exports[:syntax], lib: name_as_string)
      declarations.each do |(type, *args)|
        case type
        when 'export'
          exports[:bindings].merge!(library_exports_as_hash(args))
        when 'import'
          imports = do_import(args, name.first.filename, lib_opts)
        when 'begin'
          begins += args
        end
      end
      [
        VM::SET_LIB, name_as_string,
        imports,
        begins.map { |s| compile_sexp_discard(s, lib_opts) },
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
  end
end
