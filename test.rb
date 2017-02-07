require 'fiddle'

library = Fiddle::dlopen('target/release/libscheme_vm.dylib')
init_parser = Fiddle::Function.new(library['init_parser'], [], Fiddle::TYPE_VOID)
init_parser.call

p Parser.parse(ARGV.first)
