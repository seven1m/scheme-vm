// borrowed from https://github.com/ubcsanskrit/sanscript.rb (MIT)

use ruby_sys::class;
use ruby_sys::value::RubySpecialConsts::{Nil, True, False};

pub use ruby_sys::types::{CallbackPtr, Value};

pub const RB_NIL: Value = Value { value: Nil as usize };
pub const RB_TRUE: Value = Value { value: True as usize };
pub const RB_FALSE: Value = Value { value: False as usize };

//
// Helper functions/macros for dealing with Ruby and CStrings
//

macro_rules! str2cstr {
  ($s:expr) => { ::std::ffi::CString::new($s).unwrap() }
}

macro_rules! str2cstrp {
  ($s:expr) => { str2cstr!($s).as_ptr() }
}

macro_rules! rbstr2cstrp {
  ($s:expr) => { ::ruby_sys::string::rb_string_value_cstr($s) }
}

macro_rules! rbstr2str {
  ($s:expr) => {
    unsafe { ::std::ffi::CStr::from_ptr(rbstr2cstrp!($s)).to_str().unwrap() }
  }
}

macro_rules! str2rbid {
  ($s:expr) => { ::ruby_sys::util::rb_intern(str2cstrp!($s)) }
}

macro_rules! str2sym {
  ($s:expr) => {
    unsafe { ::ruby_sys::symbol::rb_id2sym(str2rbid!($s)) }
  }
}

pub fn define_module(name: &str) -> Value {
  unsafe { class::rb_define_module(str2cstrp!(name)) }
}

pub fn define_module_under(parent: &Value, name: &str) -> Value {
  unsafe { class::rb_define_module_under(*parent, str2cstrp!(name)) }
}

pub fn define_method(module: &Value, name: &str, method: CallbackPtr, argc: i32) {
  unsafe { class::rb_define_method(*module, str2cstrp!(name), method, argc) }
}

pub fn define_singleton_method(module: &Value, name: &str, method: CallbackPtr, argc: i32) {
  unsafe { class::rb_define_singleton_method(*module, str2cstrp!(name), method, argc) }
}

pub fn extend_object(object: &Value, module: &Value) {
  unsafe { class::rb_extend_object(*object, *module) }
}
