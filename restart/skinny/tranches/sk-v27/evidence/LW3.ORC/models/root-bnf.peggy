// sk-v27 LW3.ORC2 evidence: root's reading of grammar/bnf/bnf.bbnf: RW before every rule, and ?w read as [ \t\n\r]*.
// Derived from crates/oracle/peg/bnf.peggy (the oracle transcription); not an oracle.
// Checked by ../ws_model.mjs against the arm's verdicts on every corpus case.

grammar = RW ((W rule W)*)
W = [ \t\n\r]*
identifier = RW ([a-zA-Z_] [a-zA-Z0-9_-]*)
terminal = RW ('"' ("\\" [^\n\r\u2028\u2029] / [^"\\])* '"')
nonterminal = RW ("<" identifier ">")
term = RW (terminal / nonterminal)
expression = RW ((term [ \t]*)+)
alternation = RW (expression ([ \t]* "|" [ \t]* expression)*)
rhs = RW (alternation)
lhs = RW (nonterminal)
rule = RW (lhs [ \t]* "::=" [ \t]* rhs [ \t]* "\n"?)
RW = [ \t\n\r]*
