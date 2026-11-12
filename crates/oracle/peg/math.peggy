// sk-v27 LW3.ORC2 (ruling ORC-2, 2026-11-12a): the independent oracle of
// grammar/misc/math.bbnf, a hand transcription into peggy (PEG), read by the
// pinned peggy 5.1.1 (crates/oracle/ts/pin.json, set "peggy").
// transcribes: grammar/misc/math.bbnf sha256 04c7013a9ae394ae007242e51fc27533563fe795381f331893fa188e3bd67e31
//
// Written from the grammar text and docs/bbnf alone; no bbnf-lang front end,
// generator or runtime reads it. G1 reads math from root's registry entry,
// `number` (the harness's registry): the language is that rule's pattern,
// /(\d+)?(\.\d+)?([eE][-+]?\d+)?/, read leftmost-first (ECMAScript, ruling
// OR-1) and required to cover the whole input. Each group is optional and
// nothing after it can fail, so the pattern never backtracks into a group:
// the greedy PEG below reads the same match. `\d` is [0-9] (the u flag does
// not widen it). The whole grammar is transcribed below the entry, so the
// other rules are read too when an arm reads them.

number = [0-9]* ("." [0-9]+)? ([eE] [-+]? [0-9]+)?

// expr = term, { ("+" | "-"), term } ;
expr = term (("+" / "-") term)*

// term = factor, { ("*" | "/"), factor } ;
term = factor (("*" / "/") factor)*

p = "("
pp = p
ppp = pp
pppp = ppp
ppppp = pppp
pppppp = ppppp

wrapped = pppppp expr ")"

factor = number / wrapped
