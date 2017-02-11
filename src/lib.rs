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

fn parse(rself: Value) -> Value {
    let program_str = rbstr2str!(&rb::ivar_get(&rself, "@code"));
    match lisp::program(&program_str) {
        Ok(ast) => ast,
        Err(err) => {
            //let expected = rb::vec2rbarr(
                //err.expected.iter().cloned().map(|e| rb::str_new(&e.to_string())).collect()
            //);
            println!("{}", err.column);
            println!("{:?}", err.expected);
            let c_parser = rb::const_get("Parser", &RB_NIL);
            let c_parse_error = rb::const_get("ParseError", &c_parser);
            let line = int2rbnum!(err.line);
            let error = rb::class_new_instance(&c_parse_error, vec![line]);
            rb::raise_instance(&error);
            RB_NIL
        }
    }
}

#[no_mangle]
pub extern fn init_parser() {
    let c_parser = rb::const_get("Parser", &RB_NIL);
    rb::define_method(&c_parser, "parse", parse as CallbackPtr, 0);
}
