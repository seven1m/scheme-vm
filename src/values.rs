use std::fmt;

pub trait Val {
}

#[derive(Debug)]
pub struct Str {
	pub value: String
}

impl Val for Str {
}

#[derive(Debug)]
pub struct Atom {
	pub name: String
}

impl Val for Atom {
}

pub struct Arr {
    pub values: Vec<Box<Val>>
}

impl Val for Arr {
}

impl fmt::Debug for Arr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Arr()")
    }
}

pub struct Cons {
	car: Box<Val>,
	cdr: Box<Cons>
}
