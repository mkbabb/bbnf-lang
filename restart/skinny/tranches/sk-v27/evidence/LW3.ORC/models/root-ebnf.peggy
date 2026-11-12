// sk-v27 LW3.ORC2 evidence: root's reading of grammar/ebnf/ebnf.bbnf: RW = root's [ \t\n\r]* before every rule, and in place of the entry repetition's S.
// Derived from crates/oracle/peg/ebnf.peggy (the oracle transcription); not an oracle.
// Checked by ../ws_model.mjs against the arm's verdicts on every corpus case.

grammar = RW ((RW rule RW)*)
letter = RW ("A" / "B" / "C" / "D" / "E" / "F" / "G" / "H" / "I" / "J" / "K" / "L" / "M" / "N" / "O" / "P" / "Q" / "R" / "S" / "T" / "U" / "V" / "W" / "X" / "Y" / "Z" / "a" / "b" / "c" / "d" / "e" / "f" / "g" / "h" / "i" / "j" / "k" / "l" / "m" / "n" / "o" / "p" / "q" / "r" / "s" / "t" / "u" / "v" / "w" / "x" / "y" / "z")
digit = RW ("0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9")
symbol = RW ("[" / "]" / "{" / "}" / "(" / ")" / "<" / ">" / "'" / '"' / "=" / "|" / "." / "," / ";" / "-" / "+" / "*" / "?" / "\n" / "\t" / "\r" / "\f" / "\b" / "\\")
character = RW (letter / digit / symbol / "_" / " ")
identifier = RW (letter (letter / digit / "_")*)
S = [ \t\n\r\f]*
terminal = RW ("'" (!"'" character) (!"'" character)* "'" / '"' (!'"' character) (!'"' character)* '"')
terminator = RW (";" / ".")
term = RW ("(" S rhs S ")" / "[" S rhs S "]" / "{" S rhs S "}" / terminal / identifier)
factor = RW (term S "?" / term S "*" / term S "+" / term S "-" S term / term S)
concatenation = RW ((S factor S ","?)+)
alternation = RW ((S concatenation S "|"?)+)
rhs = RW (alternation)
lhs = RW (identifier)
rule = RW (lhs S "=" S rhs S terminator)
RW = [ \t\n\r]*
