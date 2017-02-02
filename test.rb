require 'fiddle'

library = Fiddle::dlopen('target/release/libscheme_vm.dylib')
is_ok = Fiddle::Function.new(library['is_ok'], [Fiddle::TYPE_VOIDP], Fiddle::TYPE_SHORT)

p is_ok.call(ARGV.first) == 1
