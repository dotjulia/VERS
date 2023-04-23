#[derive(Debug, Clone)]
pub enum Rule {
    Terminal(String),
    NonTerminal(String, String),
}

impl ToString for Rule {
    fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

pub struct Grammar {
    pub rules: Vec<(String, Rule)>,
    // add terminal list and use that for lookup in parsing
    pub terminals: Vec<(String, String)>,
    pub start_symbol: &'static str,
}

impl Grammar {
    pub fn print_normal_from_rules(&self) {
        println!(
            "G = (V = {{...}}, Σ = {{{}\n}}, R = {{{}\n}}, S = {})",
            self.terminals
                .iter()
                .map(|(s, d)| "\n\t".to_owned() + &s.to_owned() + " = '" + d + "'")
                .collect::<Vec<String>>()
                .join(", "),
            self.rules
                .iter()
                .map(|(s, d)| "\n\t".to_owned() + s
                    + " → "
                    + &match d {
                        Rule::Terminal(t) => t.clone(),
                        Rule::NonTerminal(nt1, nt2) => format!("{}{}", nt1, nt2),
                    })
                .collect::<Vec<String>>()
                .join(","),
            self.start_symbol
        );
    }
}

pub struct ParseResult {
    pub matches: bool,
}

pub trait ParseGrammar {
    fn parse_grammar(&self, g: &Grammar) -> ParseResult;
}

impl ParseGrammar for &str {
    fn parse_grammar(&self, g: &Grammar) -> ParseResult {
        let n = self.len();
        let mut table = vec![vec![Vec::<String>::new(); n]; n];
        for j in 0..n {
            if let Some((t, _)) = g.terminals.iter().find(|(_, c)| c == &self.chars().nth(j).unwrap().to_string()) {
                let mut added = false;
                g.rules.iter().for_each(|(lhs, rhs)| {
                    if let Rule::Terminal(t2) = rhs {
                        if t == t2 {
                            table[j][j].push(lhs.clone());
                            added = true;
                        }
                    }
                });
                if !added {
                    panic!("Invalid input: {}", self.chars().nth(j).unwrap());
                }
            } else {
                panic!("Invalid input: {}", self.chars().nth(j).unwrap());
            }
            // render_table(&table);
            // println!();
            // for (let i = j; i >= 0; i--) {
            for i in (0..=j).rev() {
                // for (let k = i; k <= j; k++) {
                for k in i..=j {
                    for (lhs, rhs) in g.rules.iter() {
                        if let Rule::NonTerminal(a, b) = rhs {
                            if table[i][k].contains(&a) && k + 1 < n && table[k + 1][j].contains(&b)
                            {
                                table[i][j].push(lhs.clone());
                            }
                        }
                    }
                }
            }
        }
        if table[0][n - 1].contains(&g.start_symbol.to_string()) {
            return ParseResult { matches: true };
        } else {
            return ParseResult { matches: false };
        }
    }
}

impl Grammar {
    pub fn grammar(&self) -> String {
        format!(
            "G = (\n\tR = {{ {} }},\n\tS = {}\n)",
            self.rules
                .iter()
                .map(|(s, d)| s.to_string() + " → " + &d.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            self.start_symbol
        )
    }
}
