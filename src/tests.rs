#[cfg(test)]
mod tests {
    use lisp;
    use values::*;

    #[test]
    fn program() {
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
        assert!(program.is_ok());
        match *program.unwrap() {
            Val::Arr { vals } => assert_eq!(7, vals.len()),
            _                 => panic!()
        }
    }
}
