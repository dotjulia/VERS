#![allow(non_snake_case)]
extern crate vers_macro;
use vers::{Grammar, ParseGrammar, Rule};
use vers_macro::VΣRS;


fn main() {
    VΣRS! [G = (
        V = {S},
        Σ = {a = "(", b = ")"},
        R = {
            S -> aSb,
            S -> ab,
        },
        S = S
    )];
    G.print_normal_from_rules();
    println!("{}", "((()))".parse_grammar(&G).matches); // true
    println!("{}", "((())))".parse_grammar(&G).matches); // false
}
