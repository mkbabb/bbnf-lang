// sk-v27 LW3.ORC2 (ruling ORC-2, 2026-11-12a): the independent oracle of
// grammar/ebnf/ebnf.bbnf, a hand transcription into peggy (PEG), read by the
// pinned peggy 5.1.1 (crates/oracle/ts/pin.json, set "peggy").
// transcribes: grammar/ebnf/ebnf.bbnf sha256 42ddc265529c5662cd667e44b051ba129bbe36bc3b8abc743c26b8826d58de52
//
// Written from the grammar text and docs/bbnf alone; no bbnf-lang front end,
// generator or runtime reads it. BBNF -> PEG, by docs/bbnf/operators.md:
//   `a , b` sequence; `a | b` ordered choice (peggy `/`); `x *`, `x +`, `x ?`
//   greedy repetition and option; `{ x }` is `( x ) *`; `a - b` "match A only
//   if B does not match at the same position" (`!b a`); the entry is root's
//   registry entry `grammar` (the harness's registry), and the whole input
//   must be read (peggy refuses unread input).
// A string literal reads its JavaScript escapes ("\n", "\t", "\r", "\f",
// "\b" backspace U+0008, "\\"): docs/bbnf is silent on literal escapes, and
// this is the reading of the TS front end and of a JavaScript string literal.

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
       / "+" / "*" / "?" / "\n" / "\t" / "\r" / "\f"
       / "\b" / "\\"

character = letter / digit / symbol / "_" / " "

identifier = letter (letter / digit / "_")*

// /[ \t\n\r\f]*/
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
