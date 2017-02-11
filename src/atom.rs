use rb;
use rb::{Value, RB_NIL};

pub fn atom(name: &str) -> Value {
    let name_rbstr = rb::str_new(&name.to_string());
    let vm_class = rb::const_get("VM", &RB_NIL);
    let atom_class = rb::const_get("Atom", &vm_class);
    rb::class_new_instance(&atom_class, vec![name_rbstr])
}
