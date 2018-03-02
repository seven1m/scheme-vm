use rb;
use rb::{Value, RB_NIL};

pub fn atom(name: &str, filename: &str, offset: usize, newlines: &Vec<usize>) -> Value {
    let lines_before: Vec<usize> = newlines.iter().take_while(|i| *i < &offset).map(|i| *i).collect();
    let line = int2rbnum!(lines_before.len() + 1);
    let column = match lines_before.last() {
        Some(i) => int2rbnum!(offset - i),
        None => int2rbnum!(offset + 1)
    };
    let name = rb::str_new(&name.to_string());
    let filename = rb::str_new(&filename.to_string());
    let vm = rb::const_get("VM", &RB_NIL);
    let atom = rb::const_get("Atom", &vm);
    rb::class_new_instance(&atom, vec![name, filename, line, column])
}
