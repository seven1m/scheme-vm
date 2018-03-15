class ImportBinding
  def initialize(library_name:, internal_name:, external_name:, syntax: nil)
    @library_name = library_name
    @internal_name = internal_name
    @external_name = external_name
    @syntax = syntax
    @prefix = ''
  end

  attr_reader :library_name, :internal_name, :syntax
  attr_accessor :prefix

  def external_name
    "#{@prefix}#{@external_name}"
  end

  def external_name=(name)
    @prefix = ''
    @external_name = name
  end

  def inspect
    "<ImportBinding library_name=\"#{library_name}\", " \
      "external_name=\"#{external_name}\", internal_name=\"#{internal_name}\">"
  end
end
