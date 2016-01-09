require_relative '../vm'

class Compiler
  module Libraries
    def do_define_native((name, method_name), options)
      options[:syntax][name] = {
        locals: options[:locals].keys + options[:syntax].keys + [name],
        native_transformer: method_name
      }
      []
    end

    def do_include(paths, relative_to, options)
      paths.map do |path|
        fail "include expects a string, but got #{path.inspect}" unless path =~ /\A"(.+)?"\z/
        filename = "#{$1}.scm"
        sexps = parse_file(filename, relative_to: relative_to)
        compile_sexps(sexps, options: options)
      end
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
        bindings.map do |(library_name, internal_name, external_name, syntax)|
          if syntax
            options[:syntax][external_name] = syntax
            []
          else
            options[:locals][external_name] = true
            [VM::IMPORT_LIB, library_name, internal_name, external_name]
          end
        end
      ]
    end

    # This method and import_set_all below return an array [include, bindings];
    # bindings is an array that looks like this:
    #
    #     [library_name, internal_binding_name, external_binding_name, syntax]
    #
    # which is shortened as:
    #
    #     [n, i, e, s]
    #
    def import_set_bindings(set, relative_to, options)
      return import_set_all(set, relative_to, options) unless set[1].is_a?(Array)
      (directive, source, *identifiers) = set
      (include, bindings) = import_set_bindings(source, relative_to, options)
      available = bindings.each_with_object({}) { |(n, i, e, s), h| h[e] = [n, i, e, s] }
      case directive
      when 'only'
        bindings = available.values_at(*identifiers)
      when 'except'
        bindings = available.values_at(*(available.keys - identifiers))
      when 'prefix'
        prefix = identifiers.first
        bindings = bindings.map { |(n, i, e, s)| [n, i, prefix + e, s] }
      when 'rename'
        renamed = Hash[identifiers]
        bindings = bindings.map do |name, internal_name, external_name, syntax|
          [name, internal_name, renamed[external_name] || external_name, syntax]
        end
      else
        fail "unknown import directive #{directive}"
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
          [
            name,
            internal_name,
            external_name,
            @libs[name][:syntax][internal_name]
          ]
        end
      ]
    end

    def include_library_if_needed(name, relative_to, options)
      return [] if @libs.key?(name)
      do_include(["\"#{name}\""], relative_to, options)
    end

    def do_define_library((name, *declarations), options)
      exports = @libs[name.join('/')] = {
        syntax: {},
        bindings: {}
      }
      begins = []
      declarations.each do |(type, *args)|
        case type
        when 'export'
          exports[:bindings].merge!(library_exports_as_hash(args))
        when 'begin'
          begins += args
        end
      end
      sexp = [
        VM::SET_LIB, name.join('/'),
        begins.map { |s| compile_sexp(s, options) },
        VM::ENDL
      ]
      exports[:syntax] = options[:syntax]
      sexp
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
