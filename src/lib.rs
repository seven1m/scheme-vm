#[macro_use]
extern crate ruru;

use ruru::{Boolean, Class, Object, RString};

mod values;
mod tests;

mod lisp {
    include!(concat!(env!("OUT_DIR"), "/lisp.rs"));
}

methods!(
   RString,
   itself,

   fn string_test_rule() -> Boolean {
       Boolean::new(lisp::program(&itself.to_string()).is_ok())
   }
);

#[no_mangle]
pub extern fn initialize_string() {
    Class::from_existing("String").define(|itself| {
        itself.def("test_rule", string_test_rule);
    });
}
