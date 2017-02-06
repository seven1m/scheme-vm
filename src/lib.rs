extern crate libc;
extern crate ruby_sys;

mod values;
mod tests;

#[macro_use] mod rb;
use rb::{CallbackPtr, Value, RB_TRUE, RB_FALSE};

mod lisp {
    include!(concat!(env!("OUT_DIR"), "/lisp.rs"));
}

fn is_ok(_rself: Value, program: Value) -> Value {
    let program_str = rbstr2str!(&program);
    if lisp::program(&program_str).is_ok() {
        RB_TRUE
    } else {
        RB_FALSE
    }
}

#[no_mangle]
pub extern fn init_parser() {
  let m_parser = rb::define_module("Parser");
  rb::define_singleton_method(&m_parser, "ok?", is_ok as CallbackPtr, 1);
}
