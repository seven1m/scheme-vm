pub enum Val {
    Str  { val: String },
    Atom { name: String },
    Arr  { vals: Vec<Box<Val>> },
    Cons { car: Box<Val>, cdr: Box<Val> }
}
