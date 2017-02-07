#[cfg(test)]
mod tests {
    use lisp;
    use rb;
    use ruby_sys::vm::{ruby_init};
    use ruby_sys::fixnum::{rb_num2int};
    use ruby_sys::util::{rb_funcallv};

    #[test]
    fn program() {
        unsafe { ruby_init() };
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
        match program {
            Ok(value) => {
                let args = rb::ary_new();
                let size = unsafe {
                    rb_num2int(rb_funcallv(value, str2rbid!("size"), 0, &args))
                };
                assert_eq!(7, size);
            },
            Err(_) => panic!()
        }
    }
}
