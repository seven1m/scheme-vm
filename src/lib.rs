#[macro_use]
extern crate ruru;

use ruru::{Boolean, Class, Object, RString};

mod lisp {
    include!(concat!(env!("OUT_DIR"), "/lisp.rs"));
}

//use self::lisp::*;

methods!(
   RString,
   itself,

   fn string_test_rule() -> Boolean {
       Boolean::new(lisp::sexp(&itself.to_string()).is_ok())
   }
);

// temporary while we build up our rules
#[cfg(test)]
mod tests {
    use lisp;

    #[test]
    fn string() {
        assert!(lisp::string("\"foo\"").is_ok());
        assert!(lisp::string("\"\"").is_ok());
        assert!(lisp::string("\"quote\\\"in\\.the middle\"").is_ok());
        assert!(lisp::string("\"").is_err());
        assert!(lisp::string("").is_err());
    }

    #[test]
    fn escape() {
        assert!(lisp::escape("\\.").is_ok());
        assert!(lisp::escape("\\\"").is_ok());
        assert!(lisp::escape("\\").is_err());
        assert!(lisp::escape("").is_err());
    }

    #[test]
    fn atom() {
        assert!(lisp::atom("foo").is_ok());
        assert!(lisp::atom("*").is_ok());
        assert!(lisp::atom("[").is_err());
        assert!(lisp::atom("").is_err());
    }

    #[test]
    fn sexp() {
        assert!(lisp::sexp("(foo bar)").is_ok());
        assert!(lisp::sexp("(foo)").is_ok());
        assert!(lisp::sexp("()").is_ok());
        assert!(lisp::sexp("(").is_err());
        assert!(lisp::sexp("").is_err());
    }

    #[test]
    fn quote() {
        assert!(lisp::quote("'").is_ok());
        assert!(lisp::quote(",@").is_ok());
        assert!(lisp::quote(",").is_ok());
        assert!(lisp::quote("`").is_ok());
        assert!(lisp::quote("").is_err());
        assert!(lisp::quote("/").is_err());
    }
}

#[no_mangle]
pub extern fn initialize_string() {
    Class::from_existing("String").define(|itself| {
        itself.def("test_rule", string_test_rule);
    });
}
