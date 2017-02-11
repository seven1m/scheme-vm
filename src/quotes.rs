use std::collections::HashMap;

lazy_static! {
    pub static ref QUOTES: HashMap<&'static str, &'static str> = {
        let mut q = HashMap::new();
        q.insert("'",  "quote");
        q.insert(",@", "unquote-splicing");
        q.insert(",",  "unquote");
        q.insert("`",  "quasiquote");
        q
    };
}
