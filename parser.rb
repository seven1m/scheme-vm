require 'fiddle'
require_relative 'vm/atom'
require 'pathname'

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
    @filename = filename || ''
  end

  def parse
    parse_native
  end
end

LIB_PATHS = %w[
  target/release/libscheme_vm.so
  target/debug/libscheme_vm.so
  target/release/libscheme_vm.dylib
  target/debug/libscheme_vm.dylib
]

root = Pathname.new(File.expand_path('../', __FILE__))
library_path = LIB_PATHS.detect { |path| root.join(path).exist? }
library = Fiddle::dlopen(library_path)
init_parser = Fiddle::Function.new(library['init_parser'], [], Fiddle::TYPE_VOID)
init_parser.call
