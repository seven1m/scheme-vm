extern crate libc;
extern crate ruby_sys;
#[macro_use]
extern crate lazy_static;
extern crate pest;
#[macro_use]
extern crate pest_derive;

#[macro_use] mod rb;
use rb::{CallbackPtr, Value, RB_NIL};

mod atom;
mod quotes;

use pest::Parser;
use atom::atom;
use quotes::QUOTES;

pub mod lisp {
    #[derive(Parser)]
    #[grammar = "lisp.pest"]
    pub struct LispParser;
}

use lisp::*;

fn build_ast(pair: pest::iterators::Pair<lisp::Rule>, filename: &str, newlines: &Vec<usize>) -> Option<Value> {
    match pair.as_rule() {
        lisp::Rule::program | lisp::Rule::simple_sexp | lisp::Rule::quoted_sexp | lisp::Rule::quoted_atom => {
            let mut array = rb::ary_new();
            for p in pair.into_inner() {
                match build_ast(p, filename, newlines) {
                    Some(ast) => array = rb::ary_push(array, ast),
                    None => {}
                }
            }
            Some(array)
        }
        lisp::Rule::simple_atom | lisp::Rule::delimited_identifier_inner => {
            let span = pair.into_span();
            let before: Vec<usize> = newlines.iter().take_while(|i| *i < &span.start()).map(|i| *i).collect();
            let line = int2rbnum!(before.len() + 1);
            let column = int2rbnum!(span.start() - before.last().unwrap_or(&0));
            Some(atom(span.as_str(), filename, span.start(), line, column))
        }
        lisp::Rule::string => Some(rb::str_new(pair.into_span().as_str())),
        lisp::Rule::quote => {
            let q = pair.into_span().as_str();
            Some(rb::str_new(&QUOTES.get(&q).unwrap().to_string()))
        }
        lisp::Rule::comment => None,
        _ => {
            println!("{:?} is unknown", pair.as_rule());
            unreachable!()
        }
    }
}

fn parse_native(rself: Value) -> Value {
    let code = rbstr2str!(&rb::ivar_get(&rself, "@code"));
    let filename = rbstr2str!(&rb::ivar_get(&rself, "@filename"));
    let newlines = code.match_indices("\n").map(|(i, s)| i).collect();
    rb::gc_disable();
    let pairs = LispParser::parse(lisp::Rule::program, code).unwrap_or_else(|e| panic!("{}", e));
    build_ast(pairs.into_iter().next().unwrap(), filename, &newlines).expect("error parsing")
}

#[no_mangle]
pub extern fn init_parser() {
    let c_parser = rb::const_get("Parser", &RB_NIL);
    rb::define_method(&c_parser, "parse_native", parse_native as CallbackPtr, 0);
}
