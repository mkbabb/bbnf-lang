// sk-v27 LW3.ORC2 evidence: root's reading of grammar/misc/math.bbnf: RW before every rule.
// Derived from crates/oracle/peg/math.peggy (the oracle transcription); not an oracle.
// Checked by ../ws_model.mjs against the arm's verdicts on every corpus case.

number = RW ([0-9]* ("." [0-9]+)? ([eE] [-+]? [0-9]+)?)
expr = RW (term (("+" / "-") term)*)
term = RW (factor (("*" / "/") factor)*)
p = RW ("(")
pp = RW (p)
ppp = RW (pp)
pppp = RW (ppp)
ppppp = RW (pppp)
pppppp = RW (ppppp)
wrapped = RW (pppppp expr ")")
factor = RW (number / wrapped)
RW = [ \t\n\r]*
