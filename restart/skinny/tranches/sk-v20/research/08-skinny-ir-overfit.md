# Skinny IR Overfit Audit

Scope: `skinny/crates/ir/`, `skinny/crates/passes/`, `skinny/crates/grammar/`,
`skinny/crates/bbnf-regex/`, `skinny/crates/parse-that-regex/`.

---

## Summary Verdict

The skinny IR and passes crates carry five distinct categories of JSON-grammar
overfit embedded in generic, ostensibly grammar-agnostic code. Three of the five
are in the generic pipeline layer (`passes/src/lib.rs`) and would silently
misfire or produce nonsense for any grammar that is not structurally identical to
JSON. The other two live in the regex classifier (`bbnf-regex/src/lib.rs`) and
the low-level string primitive library (`parse-that-regex/src/lib.rs`). The
grammar crate also exports a named convenience wrapper (`parse_json_grammar`) in
a crate that should contain only a generic parser. No Lock-14 self-gate exists in
the skinny tree; the full-tree gate in `crates/ir/tests/lock14_grammar_generalization.rs`
covers `crates/ir` and `crates/analysis` but is absent from every skinny crate.

---

## Finding 1 — `derive_recognizers` hardcodes JSON's structural alphabet

**File:** `skinny/crates/passes/src/lib.rs:344`

```rust
if matches!(byte, b'{' | b'}' | b'[' | b']' | b',' | b':' | b'"') {
    present.insert(byte);
}
```

The function collects only single-byte literals that belong to JSON's structural
alphabet. Every other grammar's structural bytes (CSS `{`, `}` are coincidentally
included, but e.g. `(`, `)`, `;`, `<`, `>`, `=`, `#` are not) are silently
discarded. The second branch admits `b'"'` whenever the regex classifier reports
`QuotedString`, which again assumes `"` is the only valid string delimiter across
all grammars.

The correct generalisation is to admit every single-byte literal in the grammar
unconditionally; the SIMD engine does not need a hard upper bound on the alphabet
size. The current filter is pure JSON-shape laundering framed as "structural
bytes."

---

## Finding 2 — `derive_materialization_roles` is a structural description of JSON only

**File:** `skinny/crates/passes/src/lib.rs:1403–1491`

The role-derivation algorithm bakes in every semantic invariant of JSON:

- **Boolean detection** (`1419–1422`): a rule matches `bool_value` when its
  direct literals include both `b"true"` and `b"false"` — the exact two
  literals of JSON boolean. Any grammar that uses `yes`/`no`, `on`/`off`, or any
  other two-alternative literal pair is not matched.
- **Null detection** (`1430`): `literals.len() == 1 && literals[0] == b"null"` —
  hardcodes the JSON null keyword. A rule whose only literal is `""` (empty) or
  `"nil"` or `"none"` produces `null_value = None`.
- **Container detection** (`1437–1438`): `{` and `}` bracket pair only.
- **Sequence detection** (`1445–1446`): `[` and `]` bracket pair only.
- **Pair detection** (`1475`): colon separator only.
- **Value detection** (`1463`): a rule is the value root when it references at
  least four of the six JSON-role rules. The threshold `>= 4` is load-bearing
  only for JSON's six-item union; any other grammar's union cardinality will not
  satisfy the threshold unless it happens to have the same cardinality.

The accompanying test `materialization_derives_from_structure_not_rule_names`
(`passes/src/lib.rs:1872`) creates a grammar that is structurally identical to
JSON with different rule names and asserts the same roles are inferred. This
proves rule-name independence, not grammar-shape independence; the grammar in
the test is still JSON, just renamed. The test is named to suggest a stronger
guarantee than it provides. Calling this "structure-derived" is shape-laundering.

---

## Finding 3 — `shapes::derive_shape_facts` hardwires a JSON-shaped type vocabulary

**File:** `skinny/crates/passes/src/lib.rs:256–313`

The function generates a fixed six-variant enum (`Object`, `Array`, `String`,
`Number`, `Bool`, `Null`) plus six fixed structs and one `Root` struct regardless
of which grammar is being compiled. A grammar for CSV, TOML, XML, or any other
format will receive these six variants and their field names (`members`, `elements`,
`key`, `value`, `span`, `needs_unescape`) even if no rule in that grammar
corresponds to the shapes. The fallback strings (`"Object"`, `"Array"`, etc.) at
lines 277–283 make this more visible: when no materialization descriptor matches
`TapeKind::Container`, the shape name defaults to the bare string `"Object"` —
a JSON-domain noun emitted verbatim into generated code for an unrelated grammar.

---

## Finding 4 — `TapeKind` in `ir` is a JSON semantic enum, not a generic tape tag

**File:** `skinny/crates/ir/src/lib.rs:453–463`

```rust
pub enum TapeKind {
    Container,
    Sequence,
    KeyValuePair,
    StringValue,
    NumberValue,
    BoolValue,
    NullValue,
    Member,
    Element,
}
```

The nine variants cover precisely the nine node types a JSON tape emits:
`object`, `array`, `pair`, `string`, `number`, `bool`, `null`, `member`,
`element`. `Member` and `Element` are not used by `derive_materialization_plan`
at all (they appear only as uncovered enum arms in downstream pattern matches).
A general IR's tape tag should be a grammar-defined integer or opaque string.
The `TapeKind::StringValue`-triggered capacity policy at `passes/src/lib.rs:677`
and the `redress_72_evidence` gate at `passes/src/lib.rs:726–742` both
hard-route on `TapeKind::StringValue`, coupling a generic performance heuristic
to a JSON-specific tape variant.

The `redress_72_evidence` function additionally hardcodes the workload strings
`"direct"` and `"track2"` (`passes/src/lib.rs:731`) — JSON benchmark track
names presented as generic measurement workloads.

---

## Finding 5 — `bbnf-regex` `classify` is hard-wired to three JSON regex shapes

**File:** `skinny/crates/bbnf-regex/src/lib.rs:92–105`

```rust
fn classify(pattern: &str) -> RegexKind {
    if pattern == r"[ \t\n\r]*" {
        RegexKind::Whitespace
    } else if pattern.starts_with('"') {
        RegexKind::QuotedString
    } else if pattern.starts_with("-?(0|[1-9]")
        && (pattern.contains(r"\d") || pattern.contains("[0-9]"))
        && pattern.contains("[eE]")
    {
        RegexKind::Numeric
    } else {
        RegexKind::Unknown
    }
}
```

`RegexKind::Whitespace` is matched only against the exact pattern `[ \t\n\r]*`.
Any other whitespace pattern — `[\s]+`, `[ \t]+`, `\s*`, or patterns with
Unicode whitespace — falls to `Unknown`. CSS and other grammars commonly use
`\s*` or `[ \t\r\n\f]+`.

`RegexKind::QuotedString` is matched by `starts_with('"')` alone, so any
double-quoted-string regex from any grammar including a CSS `<string>` token
or a SQL string literal qualifies. That would be honest structural recognition,
except the `StringFacts` struct attached to every `QuotedString` match
hardcodes `delimiter: b'"'` and `escape: b'\\'` (`bbnf-regex/src/lib.rs:54–55`).
CSS `<string>` also admits single-quoted strings; any grammar with a non-backslash
escape character receives wrong `StringFacts` silently.

`RegexKind::Numeric` is matched by a pattern prefix and two substring checks
copied from the JSON number regex. A CSS `<number>` regex, an SQL integer regex,
or any numeric regex that does not start with `-?(0|[1-9]` or does not include
`[eE]` falls to `Unknown`. The `Unknown` fallthrough in `first_from_prefix`
then attempts a prefix parse that may or may not find a useful first-byte set.

The downstream consumers `passes::recognizers::derive_recognizers` and
`passes::extract::span_kind` treat `QuotedString`/`Numeric`/`Whitespace` as
grammar-class truth, so misclassification here propagates through role
assignment, recognizer alphabet, and capacity policy.

---

## Finding 6 — `parse_json_grammar` / `load_json_grammar` are named exports in a generic crate

**File:** `skinny/crates/grammar/src/lib.rs:15–26`

The grammar crate exposes `parse_json_grammar` and `load_json_grammar` as
top-level public functions alongside the generic `parse_grammar`. These are
thin wrappers (`parse_grammar("json", source)`), but their presence anchors
the crate's public API to JSON. Seven out of seven test functions in
`passes/src/lib.rs` that test the generic pipeline call
`grammar::parse_json_grammar` rather than `grammar::parse_grammar`. The
convenience wrappers should be removed; callers should pass the grammar name
explicitly. The bbnf facade crate (`skinny/crates/bbnf/src/lib.rs`) doubles
this overfit with `compile_json_source`, `compile_json_file`, and a `JsonGrammar`
struct exporting `json::JsonRoot`, `json::JsonObject`, etc. as re-exports — that
is appropriate in the bbnf facade, but the grammar crate is supposed to be
generic.

---

## Finding 7 — `parse-that-regex` doc-strings and test names anchor to JSON

**File:** `skinny/crates/parse-that-regex/src/lib.rs:163,176,187,236,296`

The string-matching primitives are generic byte-scanner code. Every public
function that scans quoted-string content carries a doc-comment anchoring it
to JSON: "Matches a JSON string…", "Returns the raw end offset for a JSON
string…", etc. The test `numbers_match_json_shape` (`lib.rs:1132`) and
`unescape_handles_all_json_escapes` (`lib.rs:1344`) name JSON explicitly.

These are doc / naming issues, not mechanical overfit — the scanner logic is
generic. However, the framing discourages reuse for CSS `<string>` or any
other quoted-string grammar, and the `validate_string_escape` function at
`lib.rs:414` encodes JSON's exact escape alphabet
(`'"'`, `'\\'`, `'/'`, `'b'`, `'f'`, `'n'`, `'r'`, `'t'`, `'u'`) without a
configurable escape table. CSS `<string>` has a different escape syntax
(`\hex`, `\newline`), so the function is not reusable as-is; the name and
docs do not flag this limitation.

---

## Absent Lock-14 Self-Gate

The full-tree crate `crates/ir/tests/lock14_grammar_generalization.rs` runs a
scan over every `.rs` file in `crates/ir/src/` and `crates/analysis/src/` for
grammar-name tokens in code lines. No equivalent gate covers
`skinny/crates/ir/src/`, `skinny/crates/passes/src/`, or
`skinny/crates/grammar/src/`. The findings above would all trip a skinny-tree
gate if one existed: `b"true"`, `b"false"`, `b"null"`, `b"{"`, `b"}"`, `b"["`,
`b"]"`, `b":"` literals in generic pass code, `"direct"` and `"track2"` workload
strings, `TapeKind::BoolValue`, `TapeKind::NullValue`, and `parse_json_grammar`
in the grammar crate.

---

## Recommended Transpositions

1. **`TapeKind`**: replace the nine-variant JSON-semantic enum with a grammar-
   defined opaque `u8` or `&'static str` tag. The IR carries the tag value
   opaquely; the grammar file declares it via an `@tape_kind` annotation or
   `->` projection. The passes read and emit the tag without interpreting it.

2. **`derive_materialization_roles`**: delete entirely. Role assignment belongs
   in the grammar via `->` projections that carry a typed materialization target.
   The pass reads projection targets, not structural heuristics.

3. **`derive_recognizers`**: remove the fixed-alphabet filter. Collect all
   single-byte literals in the grammar unconditionally. The SIMD engine's
   alphabet is not bounded by JSON's character set.

4. **`bbnf-regex::classify`**: replace the three-pattern heuristic with a proper
   NFA/DFA HIR that classifies regex structure without pattern-string matching.
   The `RegexKind` enum must be extensible or replaced by a richer `HirKind`
   with per-grammar configurability for `StringFacts` delimiter and escape byte.

5. **Grammar crate API**: delete `parse_json_grammar` and `load_json_grammar`
   from `skinny/crates/grammar`. All callers must pass the grammar name as a
   parameter. Add a skinny Lock-14 gate test analogous to the one at
   `crates/ir/tests/lock14_grammar_generalization.rs`.
