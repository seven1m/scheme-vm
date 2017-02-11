require 'fiddle'
require_relative 'vm/atom'

class Parser
  class ParseError < StandardError
    attr_reader :line

    def initialize(line)
      @line = line
    end

    def message
      "error parsing at line #{line}"
    end
  end

  def initialize(code = nil, filename: nil)
    @code = code
    @filename = filename
  end

  def parse
    raise 'This method is implemented in Rust.'
  end
end

library = Fiddle::dlopen('target/release/libscheme_vm.dylib')
init_parser = Fiddle::Function.new(library['init_parser'], [], Fiddle::TYPE_VOID)
init_parser.call
