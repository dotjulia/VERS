#![allow(non_snake_case)]
#![feature(proc_macro_diagnostic)]
extern crate proc_macro;

use proc_macro::{
    token_stream::IntoIter, Delimiter, Diagnostic, Ident, Level, TokenStream, TokenTree,
};

fn consume(item: &mut IntoIter, expected: String, error_string: Option<String>) -> bool {
    let error_string = error_string.unwrap_or(format!("expected '{}'", expected));
    if let Some(i) = item.next() {
        if matches!(i.clone(), proc_macro::TokenTree::Ident(i) if i.to_string() == expected) {
            true
        } else if matches!(i.clone(), proc_macro::TokenTree::Punct(p) if p.as_char() == expected.chars().next().unwrap())
        {
            true
        } else {
            Diagnostic::spanned(i.span(), Level::Error, error_string).emit();
            false
        }
    } else {
        Diagnostic::spanned(
            proc_macro::Span::call_site(),
            Level::Error,
            format!("expected '{}'", expected),
        )
        .emit();
        false
    }
}

fn consume_group(item: &mut IntoIter, delim: Delimiter, error: Option<String>) -> Option<IntoIter> {
    let error = error.unwrap_or(format!("expected group with delimiter '{:?}'", delim));
    if let Some(i) = item.next() {
        if let proc_macro::TokenTree::Group(g) = i {
            if g.delimiter() == delim {
                Some(g.stream().into_iter())
            } else {
                Diagnostic::spanned(g.span(), Level::Error, error).emit();
                None
            }
        } else {
            Diagnostic::spanned(i.span(), Level::Error, error).emit();
            None
        }
    } else {
        Diagnostic::spanned(proc_macro::Span::call_site(), Level::Error, error).emit();
        None
    }
}

fn get_list_of_identifier(item: &mut IntoIter) -> Vec<(String, Option<Ident>)> {
    let mut list = Vec::new();
    while let Some(i) = item.next() {
        if let proc_macro::TokenTree::Ident(i) = i {
            if i.to_string().contains("𝑋") {
                Diagnostic::spanned(
                    i.span(),
                    Level::Error,
                    "Non terminals cannot contain '𝑋'. It's reserved for normalization.",
                )
                .emit();
            }
            list.push((i.to_string(), Some(i)));
        } else if let proc_macro::TokenTree::Punct(i) = i {
            if i.as_char() != ',' {
                Diagnostic::spanned(
                    i.span(),
                    Level::Error,
                    format!(
                        "Expected identifier or seperator ',' but got '{}'",
                        i.to_string()
                    ),
                )
                .emit();
            }
        } else {
            Diagnostic::spanned(
                i.span(),
                Level::Error,
                format!(
                    "Expected ident as nonterminal character got '{}'",
                    i.to_string()
                ),
            )
            .emit();
        }
    }
    list
}

fn get_ident_assign(item: &mut IntoIter) -> Vec<(Ident, String, String)> {
    let mut list = Vec::new();
    while let Some(terminal) = item.next() {
        if let proc_macro::TokenTree::Ident(terminal) = terminal {
            let ident = terminal.to_string();
            if ident.contains("𝑋") {
                Diagnostic::spanned(
                    terminal.span(),
                    Level::Error,
                    "Terminals cannot contain '𝑋'. It's reserved for normalization.",
                )
                .emit();
            }
            consume(
                item,
                "=".to_owned(),
                Some(format!(
                    "Expected '=' after ident got '{}'",
                    terminal.to_string()
                )),
            );
            if let Some(i) = item.next() {
                if let proc_macro::TokenTree::Literal(i) = i {
                    list.push((terminal, ident, i.to_string().trim_matches('"').to_owned()));
                } else {
                    Diagnostic::spanned(i.span(), Level::Error, "Expected a string literal").emit();
                }
            } else {
                Diagnostic::spanned(
                    proc_macro::Span::call_site(),
                    Level::Error,
                    "Expected ident as nonterminal character got EOF".to_owned(),
                )
                .emit();
            }
        }
    }
    list
}

fn get_string(iter: &mut IntoIter) -> String {
    let literal = iter.next();
    if let Some(TokenTree::Literal(literal)) = literal {
        literal.to_string()
    } else if let Some(TokenTree::Ident(ident)) = literal {
        ident.to_string()
    } else if let Some(TokenTree::Punct(punct)) = literal {
        punct.to_string()
    } else {
        if let Some(literal) = literal {
            Diagnostic::spanned(
                literal.span(),
                Level::Error,
                format!("Expected string literal got '{}'", literal.to_string()),
            )
            .emit();
        }
        Diagnostic::spanned(
            proc_macro::Span::call_site(),
            Level::Error,
            "Expected ident got EOF".to_owned(),
        )
        .emit();
        "".to_owned()
    }
}

fn parse_rules(
    iter: &mut IntoIter,
    nonterminals: &Vec<(String, Option<Ident>)>,
) -> Vec<(String, String, Option<Ident>)> {
    let mut ret_val = Vec::new();
    while let Some(TokenTree::Ident(nonterminal)) = iter.next() {
        if !nonterminals
            .iter()
            .any(|(s, _)| s == &nonterminal.to_string())
        {
            Diagnostic::spanned(
                nonterminal.span(),
                Level::Error,
                format!(
                    "Nonterminal '{}' is not defined in the grammar",
                    nonterminal.to_string()
                ),
            )
            .emit();
        }
        consume(iter, "-".to_owned(), None);
        consume(iter, ">".to_owned(), None);
        let mut cfg = String::new();
        loop {
            let str = get_string(iter);
            if str == "," {
                break;
            }
            cfg.push_str(&str);
        }
        ret_val.push((nonterminal.to_string(), cfg, Some(nonterminal)));
    }
    ret_val
}

fn eliminate_start_rhs(rules: &mut Vec<(String, Vec<String>)>, start_symbol: &mut String, x_count: &mut u32) {
    // if start symbol occurs on rhs
    if rules.iter().any(|(_, r)| r.contains(start_symbol)) {
        rules.push((format!("𝑋{}", x_count), vec![start_symbol.clone()]));
        *start_symbol = format!("𝑋{}", x_count);
        *x_count += 1;
    }
}

fn remove_null_productions(rules: &mut Vec<(String, Vec<String>)>, terminals: &Vec<(Ident, String, String)>) {
    // eliminate empty terminals
    if terminals.iter().any(|(_, _, t)| t == "") {
        let mut empty_terminals = Vec::new();
        for (_, t, literal) in terminals.iter() {
            if literal == "" {
                empty_terminals.push(t.clone());
            }
        }
        // find all nonterminals which could produce empty terminals
        let mut nullable_nonterminals: Vec<String> = Vec::new();
        for (nt, t) in rules.iter() {
            if empty_terminals.iter().any(|e| t.contains(e)) {
                nullable_nonterminals.push(nt.clone());
            }
        }

        let mut added_nullable = true;
        while added_nullable {
            added_nullable = false;
            for (nt, r) in rules.iter() {
                if !nullable_nonterminals.iter().any(|n| n == nt) {
                    if r.iter().all(|e| nullable_nonterminals.contains(e)) {
                        nullable_nonterminals.push(nt.clone());
                        added_nullable = true;
                    }
                }
            }
        }
        for nullable in nullable_nonterminals {
            let mut new_rules: Vec<(String, Vec<String>)> = Vec::new();
            for (nt, rule) in rules.iter() {
                // figure out all permutations
                let nullable_in_rule_cnt = rule.iter().filter(|e| **e == nullable).count();
                for cnt_nullable_to_remove in 1..=nullable_in_rule_cnt {
                    for j in 0..rule.len() {
                        if rule[j] != nullable {
                            continue;
                        }
                        if rule[j..].iter().filter(|f| **f == nullable).count() < cnt_nullable_to_remove {
                            continue;
                        }
                        let mut new_rule = Vec::new();
                        let mut cnt_added = 0;
                        for i in 0..j {
                            new_rule.push(rule[i].clone());
                        }
                        for i in j..rule.len() {
                            if nullable != rule[i] || cnt_added >= cnt_nullable_to_remove {
                                new_rule.push(rule[i].clone());
                                cnt_added += 1;
                            }
                        }
                        if new_rule.len() > 0 {
                            new_rules.push((nt.clone(), new_rule));
                        }
                    }
                }
            }
            rules.append(&mut new_rules);
        }
        rules.dedup();
        *rules = rules.iter().filter(|(_, r)| !empty_terminals.contains(&r.join(""))).map(|e| e.clone()).collect();
    }
}

fn eliminate_unit_rules(rules: &mut Vec<(String, Vec<String>)>, terminals: &Vec<(Ident, String, String)>) {
    let filter_condition = |(_, r): &&(String, Vec<String>)| r.len() == 1 && !terminals.iter().any(|(_, t, _)| t == &r[0]);
    let mut new_rules: Vec<(String, Vec<String>)> = Vec::new();
    for rule in rules.iter().filter(filter_condition) {
        for (nt, r) in rules.iter() {
            if nt == &rule.1[0] {
                new_rules.push((rule.0.clone(), r.clone()));
            }
        }
    }
    // *rules = rules.iter().filter(filter_condition).map(|e| e.clone()).collect();
    rules.append(&mut new_rules);
    rules.retain(|(_, r)| r.len() > 1 || terminals.iter().any(|t| t.1 == r[0]));
    rules.dedup();
}

fn maximum_two_rules(rules: &mut Vec<(String, Vec<String>)>, x_count: &mut u32) {
    let mut new_rules: Vec<(String, Vec<String>)> = Vec::new();
    for (_, r) in rules.iter_mut() {
        while r.len() > 2 {
            let new_nt = if let Some(existing_rule) = new_rules.iter().find(|(_, rtf)| rtf.len() == 2 && rtf[0] == r[0] && rtf[1] == r[1]) {
                existing_rule.0.clone()
            } else {
                let new_nt = format!("𝑋{}", x_count);
                *x_count += 1;
                new_rules.push((new_nt.clone(), vec![r[0].clone(), r[1].clone()]));
                new_nt
            };
            r[0] = new_nt;
            r.remove(1);
        }
    }
    rules.append(&mut new_rules);
}

fn eliminate_terminal_rules(rules: &mut Vec<(String, Vec<String>)>, terminals: &Vec<(Ident, String, String)>, x_count: &mut u32) {
    let mut new_rules: Vec<(String, Vec<String>)> = Vec::new();
    for (_, r) in rules.iter_mut() {
        if r.len() == 2 && terminals.iter().any(|(_, t, _)| t == &r[0] || t == &r[1]) {
            let terminal1 = terminals.iter().find(|(_, t, _)| t == &r[0]);
            let terminal2 = terminals.iter().find(|(_, t, _)| t == &r[1]);
            let mut replace_terminal = |terminal: String| {
                let replacing_rule = if let Some(existing_rule) = new_rules.iter().find(|(_, rtf)| rtf[0] == terminal) {
                    existing_rule.0.clone()
                } else {
                    let new_nt = format!("𝑋{}", x_count);
                    *x_count += 1;
                    new_rules.push((new_nt.clone(), vec![terminal.clone()]));
                    new_nt
                };
                if r[0] == terminal {
                    r[0] = replacing_rule;
                } else {
                    r[1] = replacing_rule;
                }
            };
            if let Some(terminal) = terminal1 {
                replace_terminal(terminal.1.clone());
            }
            if let Some(terminal) = terminal2 {
                replace_terminal(terminal.1.clone());
            }
        }
    }
    rules.append(&mut new_rules);
}

#[proc_macro]
#[allow(non_snake_case)]
pub fn VΣRS(item: TokenStream) -> TokenStream {
    let mut iter = item.into_iter();
    let grammar_name = get_string(&mut iter);
    let debug = grammar_name == "DEBUG";
    consume(&mut iter, "=".to_owned(), None);
    let mut iter = consume_group(
        &mut iter,
        Delimiter::Parenthesis,
        "Expected '(' as first argument to specif cfg tupel"
            .to_owned()
            .into(),
    )
    .unwrap();
    let error_v: Option<String> =
        "Expected 'V = {}' as first argument to specif nonterminal characters"
            .to_owned()
            .into();
    consume(&mut iter, "V".to_owned(), error_v.clone());
    consume(&mut iter, "=".to_owned(), error_v.clone());
    let mut iter_nonterminal = consume_group(&mut iter, Delimiter::Brace, error_v).unwrap();
    let nonterminals = get_list_of_identifier(&mut iter_nonterminal);
    nonterminals.iter().for_each(|(s, i)| {
        if !s.chars().all(|c| c.is_uppercase()) {
            Diagnostic::spanned(
                i.clone().unwrap().span(),
                Level::Warning,
                format!("Nonterminals should be uppercase '{}'", s),
            )
            .emit();
        }
    });
    consume(
        &mut iter,
        ",".to_owned(),
        Some("Expected ',' delimiter".to_owned()),
    );
    let error_v: Option<String> =
        "Expected 'Σ = {}' as second argument to specif terminal characters"
            .to_owned()
            .into();
    consume(&mut iter, "Σ".to_owned(), error_v.clone());
    consume(&mut iter, "=".to_owned(), error_v.clone());
    let mut iter_terminal = consume_group(&mut iter, Delimiter::Brace, error_v).unwrap();
    let terminals = get_ident_assign(&mut iter_terminal);
    terminals.iter().for_each(|(i, s, _)| {
        if !s.chars().all(|c| c.is_lowercase()) {
            Diagnostic::spanned(
                i.span(),
                Level::Warning,
                format!("Terminals should be lowercase '{}'", s),
            )
            .emit();
        }
    });

    consume(
        &mut iter,
        ",".to_owned(),
        Some("Expected ',' delimiter".to_owned()),
    );
    let error_r: Option<String> =
        "Expected 'R = {}' as first argument to specify V × (V ∪ Σ)* relations"
            .to_owned()
            .into();
    consume(&mut iter, "R".to_owned(), error_r.clone());
    consume(&mut iter, "=".to_owned(), error_r.clone());
    let mut iter_rules = consume_group(&mut iter, Delimiter::Brace, error_r).unwrap();
    let rules = parse_rules(&mut iter_rules, &nonterminals);
    consume(
        &mut iter,
        ",".to_owned(),
        Some("Expected ',' delimiter".to_owned()),
    );
    consume(
        &mut iter,
        "S".to_owned(),
        Some("Expected 'S' as start symbol".to_owned()),
    );
    consume(
        &mut iter,
        "=".to_owned(),
        Some("Expected '=' to assign start symbol".to_owned()),
    );
    let mut start_symbol = get_string(&mut iter);
    // this is the best code i've ever written, trust me
    let mut x_count = 0;

    let mut tokenized_rules = Vec::new();
    // tokenize rules
    for (nonterminal, rule, ident) in rules.iter() {
        let mut token = String::new();
        let iter = rule.chars();
        let mut nts = Vec::new();
        for char in iter {
            token.push(char);
            if let Some((_, _)) = nonterminals.iter().find(|(nt, _)| nt == &token) {
                nts.push(token);
                token = String::new();
            }
            if terminals.iter().any(|(_, s1, _)| s1 == &token) {
                nts.push(token);
                token = String::new();
            }
        }

        // throw error if not tokenized correctly
        if token.len() > 0 {
            if let Some(ident) = ident {
                Diagnostic::spanned(
                    ident.span(),
                    Level::Error,
                    format!("Unexpected token '{}'", token),
                )
                .emit();
            } else {
                Diagnostic::spanned(
                    proc_macro::Span::call_site(),
                    Level::Error,
                    format!("Unexpected token '{}'", token),
                );
            }
        }
        tokenized_rules.push((nonterminal.clone(), nts));
    }

    eliminate_start_rhs(&mut tokenized_rules, &mut start_symbol, &mut x_count);
    if debug {
        println!("Eliminate Start: {:?} S = {}", tokenized_rules, start_symbol);
    }
    remove_null_productions(&mut tokenized_rules, &terminals);
    if debug {
        println!("Eliminate Null Productions: {:?} S = {}", tokenized_rules, start_symbol);
    }
    eliminate_unit_rules(&mut tokenized_rules, &terminals);
    if debug {
        println!("Eliminate Unit Rules: {:?} S = {}", tokenized_rules, start_symbol);
    }
    maximum_two_rules(&mut tokenized_rules, &mut x_count);
    if debug {
        println!("RHS max 2: {:?} S = {}", tokenized_rules, start_symbol);
    }
    eliminate_terminal_rules(&mut tokenized_rules, &terminals, &mut x_count);
    if debug {
        println!("Eliminate mixed terminal rules: {:?} S = {}", tokenized_rules, start_symbol);
    }

    let transform_rule = |(nt, rule): (String, Vec<String>)| {
        if rule.len() == 2 {
            return format!(r#"("{}".to_owned(), Rule::NonTerminal("{}".to_owned(), "{}".to_owned()))"#, nt, rule[0], rule[1]);
        } else {
            return format!(r#"("{}".to_owned(), Rule::Terminal("{}".to_owned()))"#, nt, rule.join(""));
        }
    };


    format!(
        r#"let {}: Grammar = Grammar {{
        rules: vec![{}],
        terminals: vec![{}],
        start_symbol: "{}",
    }};"#,
        grammar_name,
        tokenized_rules
            .iter()
            .map(|m| transform_rule(m.clone()))
            .collect::<Vec<String>>()
            .join(","),
        terminals.iter().map(|(_, s1, s2)| format!(r#"("{}".to_owned(), "{}".to_owned())"#, s1, s2)).collect::<Vec<String>>().join(","),
        start_symbol
    )
    .parse()
    .unwrap()
}
