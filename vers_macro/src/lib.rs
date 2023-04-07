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

#[proc_macro]
#[allow(non_snake_case)]
pub fn VΣRS(item: TokenStream) -> TokenStream {
    let mut iter = item.into_iter();
    let grammar_name = get_string(&mut iter);
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
    let mut nonterminals = get_list_of_identifier(&mut iter_nonterminal);
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
    let mut rules = parse_rules(&mut iter_rules, &nonterminals);
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
    let start_symbol = get_string(&mut iter);
    let mut new_rules: Vec<(String, String)> = Vec::new();
    // this is the best code i've ever written, trust me
    let backup_rules = rules.clone();
    let mut x_count = 0;
    // replace all terminals in rules
    rules.iter_mut().for_each(|(_, rule, _)| {
        terminals.iter().for_each(|(_, t, _)| {
            if rule.contains(t){
                if rule.len() != t.len() {
                    if let Some((new_nt, _, _)) = backup_rules.iter().find(|(_, rule, _)| rule == t)
                    {
                        *rule = rule.replace(t, &new_nt.clone());
                        return;
                    }
                    if let Some((new_nt, _)) = new_rules.iter().find(|(_, rule)| rule == t) {
                        *rule = rule.replace(t, &new_nt.clone());
                        return;
                    }
                    let new_terminal = format!("𝑋{}", x_count);
                    x_count += 1;
                    nonterminals.push((new_terminal.clone(), None));
                    *rule = rule.replace(t, &new_terminal);
                    new_rules.push((new_terminal.clone(), t.clone()));
                }
            }
        });
    });
    new_rules.iter().for_each(|(nt, t)| {
        rules.push((nt.clone(), t.clone(), None));
    });
    // transform rules into a vector of tuples of the form (nonterminal, (nonterminal, nonterminal)) and (nonterminal, terminal)
    let mut nonterminalrules = Vec::<(String, (String, Option<String>))>::new();
    let mut terminalrules = Vec::<(String, String)>::new();
    for (nonterminal, rule, ident) in rules {
        if terminals.iter().any(|(_, t, _)| t == &rule) {
            terminalrules.push((nonterminal, rule.clone()));
            continue;
        }
        let mut token = String::new();
        let iter = rule.chars();
        let mut nts = Vec::new();
        for char in iter {
            token.push(char);
            if let Some((_, _)) = nonterminals.iter().find(|(nt, _)| nt == &token) {
                nts.push(token);
                token = String::new();
            }
        }
        if nts.len() == 1 {
            if let Some(ident) = ident.clone() {
                Diagnostic::spanned(
                    ident.span(),
                    Level::Error,
                    "Rhs of a rule should contain at least 2 nonterminals",
                )
                .emit();
            } else {
                Diagnostic::spanned(
                    proc_macro::Span::call_site(),
                    Level::Error,
                    "Rhs of a rule should contain at least 2 nonterminals",
                )
                .emit();
            }
        } else if nts.len() == 2 {
            nonterminalrules.push((nonterminal, (nts[0].clone(), Some(nts[1].clone()))));
        } else if nts.len() > 2 {
            // create new rule for 𝑋{x_count}
            while nts.len() > 2 {
                let new_nt = format!("𝑋{}", x_count);
                x_count += 1;
                nonterminals.push((new_nt.clone(), None));
                nonterminalrules.push((new_nt.clone(), (nts[0].clone(), Some(nts[1].clone()))));
                nts.remove(0);
                nts.remove(0);
                nts.insert(0, new_nt);
            }
            nonterminalrules.push((nonterminal, (nts[0].clone(), Some(nts[1].clone()))));
        }
        if token.len() > 0 {
            if let Some(ident) = ident {
                Diagnostic::spanned(
                    ident.span(),
                    Level::Error,
                    format!("non-terminal/terminal '{}' is not defined in the grammar", token),
                )
                .emit();
            } else {
                Diagnostic::spanned(
                    proc_macro::Span::call_site(),
                    Level::Error,
                    format!("non-terminal/terminal '{}' is not defined in the grammar", token),
                )
                .emit();
            }
        }
    }
    if nonterminalrules.iter().any(|(_, (n1, n2))| n1 == &start_symbol || n2 == &Some(start_symbol.clone())) {
        nonterminalrules.push((format!("𝑋{}", x_count), (start_symbol.clone(), None)));
        // x_count += 1;
    }
    if terminals.iter().any(|(_, _, t)| t == "") {
        //TODO: eliminate empty terminals
        // find nonterminals which produce empty terminals
        // find nonterminals which produce nonterminals which produce empty terminals
        // ...
        todo!();
    }
    let transform_nonterminalrule = |(nt, (nt1, nt2))| {
        let nt2: Option<String> = nt2;
        if nt2.is_some() {
            return format!(r#"("{}".to_owned(), Rule::NonTerminal("{}".to_owned(), Some("{}".to_owned())))"#, nt, nt1, nt2.unwrap());
        } else {
            return format!(r#"("{}".to_owned(), Rule::NonTerminal("{}".to_owned(), None))"#, nt, nt1);
        }
    };

    let transform_terminalrule = |(nt, t)| {
        return format!(r#"("{}".to_owned(), Rule::Terminal("{}".to_owned()))"#, nt, t);
    };

    format!(
        r#"let {}: Grammar = Grammar {{
        rules: vec![{}],
        terminals: vec![{}],
        start_symbol: "{}",
    }};"#,
        grammar_name,
        nonterminalrules
            .iter()
            .map(|m| transform_nonterminalrule(m.clone()))
            .chain(
                terminalrules
                    .iter()
                    .map(|m| transform_terminalrule(m.clone()))
            )
            .collect::<Vec<String>>()
            .join(","),
        terminals.iter().map(|(_, s1, s2)| format!(r#"("{}".to_owned(), "{}".to_owned())"#, s1, s2)).collect::<Vec<String>>().join(","),
        start_symbol
    )
    .parse()
    .unwrap()
}
