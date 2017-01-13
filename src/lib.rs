#[macro_use]
extern crate ruru;

use ruru::{Boolean, Class, Object, RString};

methods!(
   RString,
   itself,

   fn string_is_weird() -> Boolean {
       Boolean::new(false)
   }
);

#[no_mangle]
pub extern fn initialize_string() {
    Class::from_existing("String").define(|itself| {
        itself.def("weird?", string_is_weird);
    });
}
