extern crate libc;
extern crate ruby_sys;

#[macro_use] mod rb;
use rb::{CallbackPtr, Value, RB_NIL};

mod tests;

mod lisp {
    include!(concat!(env!("OUT_DIR"), "/lisp.rs"));
}

fn parse(_rself: Value, program: Value) -> Value {
    let program_str = rbstr2str!(&program);
    match lisp::program(&program_str) {
        Ok(ast) => ast,
        Err(err) => {
            let expected = rb::vec2rbarr(
                err.expected.iter().cloned().map(|e| rb::str_new(&e.to_string())).collect()
            );
            // TODO: pass expected and other error info back in ruby exception
            rb::raise("foo".to_owned());
            RB_NIL
        }
    }
}

#[no_mangle]
pub extern fn init_parser() {
  let m_parser = rb::define_module("Parser");
  rb::define_singleton_method(&m_parser, "parse", parse as CallbackPtr, 1);
}
