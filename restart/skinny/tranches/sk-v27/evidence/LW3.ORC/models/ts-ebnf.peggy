// sk-v27 LW3.ORC2 evidence: the TS 0.3.0 package's reading of grammar/ebnf/ebnf.bbnf: a literal's \n \t \r \f \b is a backslash and a letter.
// Derived from crates/oracle/peg/ebnf.peggy (the oracle transcription); not an oracle.
// Checked by ../ws_model.mjs against the arm's verdicts on every corpus case.


grammar = (S rule S)*

letter = "A" / "B" / "C" / "D" / "E" / "F" / "G"
       / "H" / "I" / "J" / "K" / "L" / "M" / "N"
       / "O" / "P" / "Q" / "R" / "S" / "T" / "U"
       / "V" / "W" / "X" / "Y" / "Z" / "a" / "b"
       / "c" / "d" / "e" / "f" / "g" / "h" / "i"
       / "j" / "k" / "l" / "m" / "n" / "o" / "p"
       / "q" / "r" / "s" / "t" / "u" / "v" / "w"
       / "x" / "y" / "z"

digit = "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"

symbol = "[" / "]" / "{" / "}" / "(" / ")" / "<" / ">"
       / "'" / '"' / "=" / "|" / "." / "," / ";" / "-"
       / "+" / "*" / "?" / "\\n" / "\\t" / "\\r" / "\\f"
       / "\\b" / "\\"

character = letter / digit / symbol / "_" / " "

identifier = letter (letter / digit / "_")*

S = [ \t\n\r\f]*

terminal = "'" (!"'" character) (!"'" character)* "'"
         / '"' (!'"' character) (!'"' character)* '"'

terminator = ";" / "."

term = "(" S rhs S ")"
     / "[" S rhs S "]"
     / "{" S rhs S "}"
     / terminal
     / identifier

factor = term S "?"
       / term S "*"
       / term S "+"
       / term S "-" S term
       / term S

concatenation = (S factor S ","?)+

alternation = (S concatenation S "|"?)+

rhs = alternation

lhs = identifier

rule = lhs S "=" S rhs S terminator
