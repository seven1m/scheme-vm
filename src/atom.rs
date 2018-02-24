use rb;
use rb::{Value, RB_NIL};

pub fn atom(name: &str, filename: &str, offset: usize, input: &str) -> Value {
    let line = int2rbnum!(&input[..offset].matches("\n").count() + 1);
    let newline_index = *&input[..offset].match_indices("\n").last().unwrap_or((0, "\n")).0;
    let column = int2rbnum!(offset - newline_index);
    let name_str = rb::str_new(&name.to_string());
    let filename_str = rb::str_new(&filename.to_string());
    let offset_num = int2rbnum!(offset);
    let vm_class = rb::const_get("VM", &RB_NIL);
    let atom_class = rb::const_get("Atom", &vm_class);
    rb::class_new_instance(&atom_class, vec![name_str, filename_str, offset_num, line, column])
}
