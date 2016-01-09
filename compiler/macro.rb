require_relative 'pattern'

class Compiler
  class Macro
    def initialize(macro, compiler)
      @macro = macro
      @locals = macro[:locals]
      (_, @literals, *@patterns) = macro[:transformer]
      @compiler = compiler
    end

    def expand(sexp)
      (@values, @template) = match_template(sexp)
      sexp = expand_template(@template)
      sexp = mangle_macro_bindings(sexp)
      sexp
    end

    private

    def match_template(sexp)
      @patterns.each do |pattern, template|
        values = Pattern.new(pattern, literals: @literals).match(sexp)
        return [values, template] if values
      end
      fail "Could not match any template for #{sexp.inspect}"
    end

    def expand_template(template)
      return @values[template] if @values.key?(template)
      return template unless template.is_a?(Array)
      ([nil] + template).each_cons(2).flat_map do |(prev, part)|
        if part == '...' && prev
          expand_elipsis_template(prev)
        else
          [expand_template(part)].compact
        end
      end
    end

    def expand_elipsis_template(template)
      if template.is_a?(Array)
        name = template.flatten.detect { |n| @values.key?("#{n}...") }
        (0...@values["#{name}..."].size).map do |index|
          duplicate_elipsis_template(template, index, @values)
        end
      else
        expand_template("#{template}...")
      end
    end

    def duplicate_elipsis_template(template, index, values)
      template.map do |part|
        if part.is_a?(Array)
          duplicate_elipsis_template(part, index, values)
        elsif values.key?(part)
          values["#{part}..."][index]
        else
          part
        end
      end
    end

    def mangle_macro_bindings(sexp)
      return sexp unless sexp.is_a?(Array) && @template.is_a?(Array)
      sexp.map do |part|
        if part.is_a?(Array)
          mangle_macro_bindings(part)
        elsif mangled_identifiers.key?(part)
          mangled_identifiers[part]
        else
          part
        end
      end
    end

    def mangled_identifiers
      @bindings ||= begin
        # FIXME: won't work with some identifiers and literal elipsis
        identifiers = @template.flatten.select { |name| name =~ /\A[a-z]/ }.uniq
        identifiers.each_with_object({}) do |identifier, hash|
          next if known_identifier?(identifier)
          hash[identifier] = @compiler.mangle_identifier(identifier)
        end
      end
    end

    def known_identifier?(name)
      @locals.include?(name) || @compiler.built_in_function?(name)
    end
  end
end
