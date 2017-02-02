extern crate libc;

mod values;
mod tests;

mod lisp {
    include!(concat!(env!("OUT_DIR"), "/lisp.rs"));
}

#[no_mangle]
pub extern "C" fn is_ok(program_ptr: *const libc::c_char) -> libc::int8_t {
    let program = string_from_c_ptr(program_ptr);
    if lisp::program(&program).is_ok() {
        1
    } else {
        0
    }
}

fn string_from_c_ptr(c_ptr: *const libc::c_char) -> String {
    let c_str = unsafe {
        assert!(!c_ptr.is_null());
        std::ffi::CStr::from_ptr(c_ptr)
    };
    c_str.to_str().unwrap().to_string()
}
