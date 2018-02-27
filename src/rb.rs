// borrowed from https://github.com/ubcsanskrit/sanscript.rb (MIT)

use ruby_sys::{class, array, string};
use ruby_sys::types::{c_char, c_int, c_long};
use ruby_sys::util::{rb_const_get};
use ruby_sys::vm::{rb_raise};
use ruby_sys::value::RubySpecialConsts::{Nil, True, False};
use ruby_sys::rb_cObject;

pub use ruby_sys::types::{CallbackPtr, Value};

pub const RB_NIL: Value = Value { value: Nil as usize };

#[allow(dead_code)]
pub const RB_TRUE: Value = Value { value: True as usize };

#[allow(dead_code)]
pub const RB_FALSE: Value = Value { value: False as usize };

macro_rules! str2cstr {
    ($s:expr) => { ::std::ffi::CString::new($s).unwrap() }
}

macro_rules! str2cstrp {
    ($s:expr) => { str2cstr!($s).into_raw() }
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

#[allow(unused_macros)]
macro_rules! str2sym {
    ($s:expr) => {
        unsafe { ::ruby_sys::symbol::rb_id2sym(str2rbid!($s)) }
    }
}

macro_rules! int2rbnum {
    ($s:expr) => {
        unsafe { ::ruby_sys::fixnum::rb_int2inum($s as isize) }
    }
}

#[allow(dead_code)]
pub fn define_module(name: &str) -> Value {
    unsafe { class::rb_define_module(str2cstrp!(name)) }
}

#[allow(dead_code)]
pub fn define_class(name: &str, superclass: &Value) -> Value {
    let parent = if *superclass == RB_NIL {
        unsafe { rb_cObject }
    } else {
        *superclass
    };
    unsafe { class::rb_define_class(str2cstrp!(name), parent) }
}

#[allow(dead_code)]
pub fn define_class_under(outer: &Value, name: &str, superclass: &Value) -> Value {
    let parent = if *superclass == RB_NIL {
        unsafe { rb_cObject }
    } else {
        *superclass
    };
    unsafe { class::rb_define_class_under(*outer, str2cstrp!(name), parent) }
}

pub fn class_new_instance(klass: &Value, args: Vec<Value>) -> Value {
    let argc = args.len() as c_int;
    let instance = unsafe { class::rb_class_new_instance(argc, args.as_ptr(), *klass) };
    instance
}

#[allow(dead_code)]
pub fn ivar_set(object: &Value, name: &str, value: &Value) -> Value {
    unsafe { class::rb_ivar_set(*object, str2rbid!(name), *value) }
}

pub fn ivar_get(object: &Value, name: &str) -> Value {
    unsafe { class::rb_ivar_get(*object, str2rbid!(name)) }
}

#[allow(dead_code)]
pub fn define_module_under(parent: &Value, name: &str) -> Value {
    unsafe { class::rb_define_module_under(*parent, str2cstrp!(name)) }
}

pub fn define_method(module: &Value, name: &str, method: CallbackPtr, argc: i32) {
    unsafe { class::rb_define_method(*module, str2cstrp!(name), method, argc) }
}

#[allow(dead_code)]
pub fn define_singleton_method(module: &Value, name: &str, method: CallbackPtr, argc: i32) {
    unsafe { class::rb_define_singleton_method(*module, str2cstrp!(name), method, argc) }
}

#[allow(dead_code)]
pub fn extend_object(object: &Value, module: &Value) {
    unsafe { class::rb_extend_object(*object, *module) }
}

pub fn ary_new() -> Value {
    unsafe { array::rb_ary_new() }
}

pub fn ary_push(array: Value, item: Value) -> Value {
    let new_array = unsafe { array::rb_ary_push(array, item) };
    new_array
}

pub fn str_new(string: &str) -> Value {
    let str = string.as_ptr() as *const c_char;
    let len = string.len() as c_long;
    unsafe { string::rb_str_new(str, len) }
}

pub fn vec2rbarr(vec: Vec<Value>) -> Value {
    let mut arr = ary_new();
    for item in vec {
        arr = ary_push(arr, item);
    }
    arr
}

pub fn const_get(name: &str, class: &Value) -> Value {
    let parent = if *class == RB_NIL {
        unsafe { rb_cObject }
    } else {
        *class
    };
    unsafe { rb_const_get(parent, str2rbid!(name)) }
}

extern "C" {
    pub fn rb_exc_raise(object: Value);
    pub fn rb_gc_enable();
    pub fn rb_gc_disable();
}

#[allow(dead_code)]
pub fn raise(exception: &Value, message: &str) {
    unsafe { rb_raise(*exception, str2cstrp!(message)) };
}

pub fn raise_instance(object: &Value) {
    unsafe { rb_exc_raise(*object) };
}

pub fn gc_enable() {
    unsafe { rb_gc_enable() };
}

pub fn gc_disable() {
    unsafe { rb_gc_disable() };
}
