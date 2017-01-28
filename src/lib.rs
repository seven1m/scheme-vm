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
       Boolean::new(lisp::sexp(&itself.to_string()).is_ok())
   }
);

// temporary while we build up our rules
#[cfg(test)]
mod tests {
    use lisp;
    use values::*;

    #[test]
    fn string() {
        assert!(lisp::string("\"foo\"").is_ok());
        assert!(lisp::string("\"\"").is_ok());
        assert!(lisp::string("\"quote\\\"in\\.the middle\"").is_ok());
        assert!(lisp::string("\"").is_err());
        assert!(lisp::string("").is_err());
        let str = lisp::string("\"foo\"").unwrap();
        match *str {
            Val::Str { val } => assert_eq!(val, "foo"),
            _                => panic!("not a Str")
        }
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
        let str = lisp::atom("foo").unwrap();
        match *str {
            Val::Atom { name } => assert_eq!(name, "foo"),
            _                  => panic!("not an Atom")
        }
    }

    #[test]
    fn sexp() {
        assert!(lisp::sexp("(foo \"bar\")").is_ok());
        assert!(lisp::sexp("(foo)").is_ok());
        assert!(lisp::sexp("()").is_ok());
        assert!(lisp::sexp("(").is_err());
        assert!(lisp::sexp("").is_err());
        let sexp = lisp::sexp("(foo \"bar\")").unwrap();
        match *sexp {
            Val::Arr { vals } => {
                assert_eq!(2, vals.len());
                let first = &vals[0];
                match **first {
                    Val::Atom { ref name } => assert_eq!("foo", name),
                    _                      => panic!("not an Atom")
                }
                let second = &vals[1];
                match **second {
                    Val::Str { ref val } => assert_eq!("bar", val),
                    _                    => panic!("not a Str")
                }
            }
            _ => panic!("not a sexp")
        }
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

    #[test]
    #[allow(unused_variables)]
    fn quoted_sexp() {
        assert!(lisp::quoted_sexp("'(foo)").is_ok());
        let sexp = lisp::quoted_sexp("'(foo \"bar\")").unwrap();
        match *sexp {
            Val::Arr { vals } => {
                assert_eq!(2, vals.len());
                let first = &vals[0];
                match **first {
                    Val::Atom { ref name } => assert_eq!("'", name),
                    _                      => panic!("not an Atom")
                }
                let second = &vals[1];
                match **second {
                    Val::Arr { ref vals } => {},
                    _                     => panic!("not an Arr")
                }
            }
            _ => panic!("not a sexp")
        }
    }

    #[test]
    fn expression() {
        assert!(lisp::expression("\"string\"").is_ok());
        assert!(lisp::expression("identifier").is_ok());
        assert!(lisp::expression("1").is_ok());
        assert!(lisp::expression("").is_err());
        assert!(lisp::expression("[").is_err());
    }
}

#[no_mangle]
pub extern fn initialize_string() {
    Class::from_existing("String").define(|itself| {
        itself.def("test_rule", string_test_rule);
    });
}
