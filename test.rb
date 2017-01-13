require 'fiddle'

library = Fiddle::dlopen('target/release/libscheme_vm.dylib')

Fiddle::Function.new(library['initialize_string'], [], Fiddle::TYPE_VOIDP).call

p ''.weird?
