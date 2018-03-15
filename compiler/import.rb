require_relative './import_binding'

class Import
  def initialize(import_sets:, options:, relative_to:, compiler:)
    @import_sets = import_sets
    @options = options
    @relative_to = relative_to
    @compiler = compiler
    @includes = []
  end

  attr_reader :import_sets, :options, :relative_to, :compiler

  def compile
    sets = import_sets.map do |set|
      import_set(set, options)
    end
    @includes + sets
  end

  private

  def import_set(set, options)
    bindings = import_set_bindings(set, options)
    bindings.map do |binding|
      if binding.syntax
        options[:syntax][binding.external_name] = binding.syntax
        []
      else
        options[:locals][binding.external_name] = true
        [VM::IMPORT_LIB, binding.library_name, binding.internal_name, binding.external_name]
      end
    end
  end

  def import_set_bindings(set, options)
    return import_set_all(set, options) unless set[1].is_a?(Array)
    (directive, source, *identifiers) = set
    bindings = import_set_bindings(source, options)
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
    bindings
  end

  def import_set_all(set, _options)
    name = set.join('.')
    @includes += compiler.include_library_if_needed(name, relative_to, options)
    @compiler.libs[name][:bindings].map do |external_name, internal_name|
      ImportBinding.new(
        library_name: name,
        internal_name: internal_name,
        external_name: external_name,
        syntax: @compiler.libs[name][:syntax][internal_name]
      )
    end
  end
end
