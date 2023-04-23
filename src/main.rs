#![allow(non_snake_case)]
extern crate vers_macro;
use vers::{Grammar, ParseGrammar, Rule};
use vers_macro::VΣRS;


fn main() {
    VΣRS! [G = (
        V = {S, A, B},
        Σ = {a = "(", b = ")", ε = ""},
        R = {
            S -> aSb,
            S -> ε,
        },
        S = S
    )];
    G.print_normal_from_rules();
    println!("{}", "(())".parse_grammar(&G).matches); // true
    println!("{}", ("(".repeat(100) + ")".repeat(100).as_str()).as_str().parse_grammar(&G).matches); // true
    println!("{}", "((())))".parse_grammar(&G).matches); // false
}
