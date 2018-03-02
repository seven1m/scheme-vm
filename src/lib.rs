extern crate libc;
extern crate ruby_sys;
#[macro_use]
extern crate lazy_static;

#[macro_use] mod rb;
use rb::{CallbackPtr, Value, RB_NIL};

mod atom;
mod quotes;

mod lisp {
    include!(concat!(env!("OUT_DIR"), "/lisp.rs"));
}

fn parse_native(rself: Value) -> Value {
    let code = rbstr2str!(&rb::ivar_get(&rself, "@code"));
    let filename_rbstr = rb::ivar_get(&rself, "@filename");
    let filename = rbstr2str!(&filename_rbstr);
    let newlines: Vec<usize> = code.match_indices("\n").map(|(i, _s)| i).collect();
    rb::gc_disable();
    match lisp::program(&code, &filename, &newlines) {
        Ok(ast) => {
            rb::gc_enable();
            ast
        },
        Err(err) => {
            raise_syntax_error(err, filename_rbstr);
            RB_NIL
        }
    }
}

fn raise_syntax_error(err: lisp::ParseError, filename_rbstr: Value) {
    let c_parser = rb::const_get("Parser", &RB_NIL);
    let c_parse_error = rb::const_get("ParseError", &c_parser);
    let line = int2rbnum!(err.line);
    let column = int2rbnum!(err.column);
    let mut expected = rb::ary_new();
    for token in err.expected {
        expected = rb::ary_push(expected, rb::str_new(token));
    }
    let error = rb::class_new_instance(&c_parse_error, vec![filename_rbstr, line, column, expected]);
    rb::gc_enable();
    rb::raise_instance(&error);
}

#[no_mangle]
pub extern fn init_parser() {
    let c_parser = rb::const_get("Parser", &RB_NIL);
    rb::define_method(&c_parser, "parse_native", parse_native as CallbackPtr, 0);
}
