#[macro_use]
extern crate ruru;

use ruru::{Boolean, Class, Object, RString};

mod values;

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

#[cfg(test)]
mod tests {
    use lisp;

    #[test]
    fn program() {
		let program = lisp::program("
			; comment
			'foo
			'(1 2)
			'()
			,foo
			,(foo bar) #; (baz) #;6
			#| this is a
			   multi-line comment |#
			(print |space in identifier|)
			(if (< 1 2) #;(2 3)
				x ; another comment
				(foo (bar (baz \"this is a string\"))))
		");
        assert!(program.is_ok());
    }
}

#[no_mangle]
pub extern fn initialize_string() {
    Class::from_existing("String").define(|itself| {
        itself.def("test_rule", string_test_rule);
    });
}
