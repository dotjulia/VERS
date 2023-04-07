# $VΣRS$ is a Context Free Grammar (CFG) parser in Rust
Given a context-free grammar (CFG) $G = (V, Σ, R, S)$ and a word $w$, VΣRS determines whether $w$ is a member of the language generated by $G$. $VΣRS$ first converts G to Chomsky Normal Form (CNF) at compile time and then applies the Cocke-Younger-Kasami (CYK) algorithm to $G$ in CNF form to decide $w$'s membership in the generated language.

```Rust
VΣRS! [G = (
    V = {S},
    Σ = {a = "(", b = ")"},
    R = {
        S -> aSb,
        S -> ab,
    },
    S = S
)];
"((()))".parse_grammar(&G).matches // true
```

## Chomsky Normal Form (CNF)
$VΣRS$ converts a CFG $G = (V, Σ, R, S)$ to CNF at compile time. The conversion is done by applying the following rules to $G$:
* Given a set of terminals $a \in \Sigma$, for all production rules containing more than one terminals or at least one non-terminal and a terminal, substitute these terminals with a newly defined non-terminal $A_a$.
* Subsitute the starting symbol $S$ with $S'$ and add the production rule $S' \rightarrow S$, if $S$ appears on the right-hand side of a production rule.
* For every production rule that involves more than two non-terminals denoted by $S \rightarrow ABC$, introduce a new symbol, $X_{AB}$, and replace the first two non-terminals with this symbol, while also adding the rule $X_{AB} \rightarrow AB$. This produces the revised rule $S \rightarrow X_{AB}C$.
* For every nullable production rule $A \rightarrow \epsilon$, substitute it with one or more equivalent rules. A production rule is said to be nullable if it is of the form $A \rightarrow \epsilon$, where $\epsilon$ denotes the empty word, or if it is of the form $A \rightarrow B$, where $B$ is a nullable production rule. Next, substitute every production rule with all its permutations while omitting any or all of the nullable production rules. (not implemented)

## Cocke-Younger-Kasami (CYK) Algorithm

## Is my grammar context free?
To check if a grammar is context free, the [pumping lemma for context free languages](https://en.wikipedia.org/wiki/Pumping_lemma_for_context-free_languages) can be used.

The pumping lemma states that if $L$ is a context free language, then there exists a positive integer $p \geq 1$ such that for all $w \in L$, $|w| \geq p$ and $w$ can be written as $w = uvsxy$, where $|vx| \geq 1$, $|vsx| \leq p$ and $uv^nsx^ny \in L$ for $n \geq 0   $. The pumping lemma can be used to check if a grammar is context free by checking if the language generated by the grammar is context free. If the language generated by the grammar is context free, then the grammar is context free.

All finite languages fulfill the pumping lemma trivially by choosing $p$ equal to the maximum length of the string. 

Most often the pumping lemma is used to proof that a language is not context free by presenting a contradiction to the definition above.

## Rust macros
$VΣRS$ uses Rust macros to convert a CFG to CNF and then parses the word at runtime using the CYK algorithm. Using rust macros allows giving useful compile-time errors to the user. For instance, if the user specifies a terminal that is not defined in the grammar, the compiler will throw an error. The following is an example of a compile-time error thrown by $VΣRS$:
```Rust
VΣRS! [G = (
    V = {S},
    Σ = {a = "(", b = ")"},
    R = {
        S -> aSbc,
        S -> ab,
    },
    S = S
)];
```
```Rust
error: non-terminal/terminal 'c' is not defined in the grammar
  --> src/main.rs:27:13
   |
27 |             S -> aSbc,
   |             ^
```

## Limitations:
* Grammar cannot generate the empty word ("" or $ε$) but production rules can contain the empty word.
For instance, assuming $S$ is the starting symbol, the production $S \rightarrow ε$ is considered invalid. However, the rule $B \rightarrow ε$ is considered a legitimate production rule.
* Multiple terminals which represent the empty word are not allowed. For instance, $Σ = \set{ε = "", empty = ""}$ would be considered invalid