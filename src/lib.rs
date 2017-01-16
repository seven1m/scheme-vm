#[macro_use]
extern crate ruru;

use ruru::{Boolean, Class, Object, RString};

mod lisp {
    include!(concat!(env!("OUT_DIR"), "/lisp.rs"));
}

use self::lisp::*;

methods!(
   RString,
   itself,

   fn string_is_whitespace() -> Boolean {
       Boolean::new(whitespace(&itself.to_string()).is_ok())
   }
);

#[no_mangle]
pub extern fn initialize_string() {
    Class::from_existing("String").define(|itself| {
        itself.def("whitespace?", string_is_whitespace);
    });
}
