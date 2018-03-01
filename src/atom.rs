use rb;
use rb::{Value, RB_NIL};

pub fn atom(name: &str, filename: &str, offset: usize, line: Value, column: Value) -> Value {
    let name_str = rb::str_new(&name.to_string());
    let filename_str = rb::str_new(&filename.to_string());
    let offset_num = int2rbnum!(offset);
    let vm_class = rb::const_get("VM", &RB_NIL);
    let atom_class = rb::const_get("Atom", &vm_class);
    rb::class_new_instance(&atom_class, vec![name_str, filename_str, offset_num, line, column])
}
