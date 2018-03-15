require_relative '../vm'
require_relative '../loader'
require_relative './import_binding'

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
      sets.map do |set|
        import_set(set, relative_to, options)
      end
    end

    def import_set(set, relative_to, options)
      (include, bindings) = import_set_bindings(set, relative_to, options)
      [
        include,
        bindings.map do |binding|
          if binding.syntax
            options[:syntax][binding.external_name] = binding.syntax
            []
          else
            options[:locals][binding.external_name] = true
            [VM::IMPORT_LIB, binding.library_name, binding.internal_name, binding.external_name]
          end
        end
      ]
    end

    def import_set_bindings(set, relative_to, options)
      return import_set_all(set, relative_to, options) unless set[1].is_a?(Array)
      (directive, source, *identifiers) = set
      (include, bindings) = import_set_bindings(source, relative_to, options)
      available = bindings.each_with_object({}) { |binding, hash| hash[binding.external_name] = binding }
      case directive
      when 'only'
        bindings = available.values_at(*identifiers).compact
      when 'except'
        bindings = available.values_at(*(available.keys - identifiers)).compact
      when 'prefix'
        prefix = identifiers.first
        bindings.each { |binding| binding.prefix = prefix }
      when 'rename'
        renamed = Hash[identifiers]
        bindings.each do |binding|
          next unless (new_name = renamed[binding.external_name])
          binding.external_name = new_name
        end
      else
        raise "unknown import directive #{directive}"
      end
      [include, bindings]
    end

    def import_set_all(set, relative_to, _options)
      name = set.join('/')
      isolated_options = { locals: {}, syntax: {} }
      include = include_library_if_needed(name, relative_to, isolated_options)
      [
        include,
        @libs[name][:bindings].map do |external_name, internal_name|
          ImportBinding.new(
            library_name: name,
            internal_name: internal_name,
            external_name: external_name,
            syntax: @libs[name][:syntax][internal_name]
          )
        end
      ]
    end

    def include_library_if_needed(name, relative_to, options)
      return [] if @libs.key?(name)
      do_include(["\"#{name}.scm\""], relative_to, options)
    end

    def do_define_library((name, *declarations), options)
      name_as_string = name.join('/')
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
