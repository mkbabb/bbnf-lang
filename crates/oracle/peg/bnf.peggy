// sk-v27 LW3.ORC2 (ruling ORC-2, 2026-11-12a): the independent oracle of
// grammar/bnf/bnf.bbnf, a hand transcription into peggy (PEG), read by the
// pinned peggy 5.1.1 (crates/oracle/ts/pin.json, set "peggy").
// transcribes: grammar/bnf/bnf.bbnf sha256 8d4de97d9a54a1fef6f7ccccdeabd185d66c8614f167fc3ae28225b7994e4a1b
//
// Written from the grammar text and docs/bbnf alone; no bbnf-lang front end,
// generator or runtime reads it. BBNF -> PEG, by docs/bbnf/operators.md:
//   `a , b` sequence; `a | b` ordered choice; `x *`, `x +`, `x ?` greedy;
//   `a >> b` and `a << b` match both (the projection keeps one value, and the
//   oracle reads only the verdict); `x ?w` is `ws* x ws*` (operators.md §6),
//   with ws the six characters observation.md names for the default `?w`:
//   space, tab, line feed, vertical tab, form feed, carriage return; the
//   entry is root's registry entry `grammar`, and the whole input must be
//   read.
// A regex is an ECMAScript pattern read leftmost-first at the current
// position (docs/bbnf; ruling OR-1): each pattern here is transcribed into
// the PEG expression that reads the same match. None of these patterns can
// backtrack into a different match: every quantifier is greedy over a class
// the next part cannot start with. `.` (no `s` flag) is any code point but a
// line terminator (\n, \r, U+2028, U+2029).

grammar = (W rule W)*

// the default `?w` set
W = [ \t\n\v\f\r]*

// /[a-zA-Z_][a-zA-Z0-9_-]*/
identifier = [a-zA-Z_] [a-zA-Z0-9_-]*

// "\"" , /(\\.|[^"\\])*/ , "\""
terminal = '"' ("\\" [^\n\r\u2028\u2029] / [^"\\])* '"'

// "<" >> identifier << ">"
nonterminal = "<" identifier ">"

term = terminal / nonterminal

// (term , /[ \t]*/)+
expression = (term [ \t]*)+

// expression , ( /[ \t]*/ , "|" , /[ \t]*/ , expression )*
alternation = expression ([ \t]* "|" [ \t]* expression)*

rhs = alternation

lhs = nonterminal

// lhs , /[ \t]*/ , "::=" , /[ \t]*/ , rhs , /[ \t]*/ , /\n/ ?
rule = lhs [ \t]* "::=" [ \t]* rhs [ \t]* "\n"?
