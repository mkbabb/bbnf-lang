# PROTO-1 — LOCK-7 pass-1, probe t1 (family f1): the §0 base-case wall

**Probe class:** RUNNING (RAN_MEASURED). Feasibility gate, not a perf claim — the measured
quantity is a deterministic parse-rejection **byte offset**, reproducible independent of load.

---

## Question

The FAMILY-SPECS §0 governing finding: 7A/7B/7D are three postures on ONE bootstrap ladder that
share ONE make-or-break gate — *does `emit_from_source("bbnf", bbnf.bbnf)` produce a parser that
reparses the grammar corpus?* Until GREEN, the bbnf leg of all three families is
BLOCKED-candidate on "the identical missing primitive." §0 framed that primitive as *a
general-descent emitter that lowers bbnf's own closure / `@import` / `@{}` / recursive-alternation
surface* — i.e. it presumed the front-end **parses** bbnf and the wall is downstream in
**lowering**.

**t1 asks the sharpest sub-question:** is the wall in the emitter (as §0 presumed), or earlier —
does skinny's hand front-end even *parse* bbnf's own surface? And at which byte does it reject?

## Method

Skinny (bbnf-lang), not parse-that — the pt-scratch recipe (R3 §8) does not apply. A read-only
inspection plus a 3-case running probe added as a `grammar`-crate integration test
(`skinny/crates/grammar/tests/probe_bbnf_selfhost.rs`), driving the real production entry
`grammar::parse_grammar` — the SEED that `emit_from_source` (`codegen/src/lib.rs:108`) and
`emit_typed_from_source` (`:141`) both call before any lowering. Each case reads the three
committed self-grammars (`grammar/bbnf/{bbnf,expressions,types}.bbnf`), runs `parse_grammar`, and
on rejection decodes the `GrammarError` byte offset to line/col + surrounding context.

- **Probe A** — literal §0 gate: `parse_grammar("bbnf", bbnf.bbnf)` verbatim (imports unresolved).
- **Probe B** — imports inlined: `bbnf ∪ expressions ∪ types` with `@import` lines stripped →
  self-contained, isolating pure parse from import machinery.
- **Probe C** — each file standalone (which of the three individually parse).

Build: `cargo test -p grammar --profile ax-iter --test probe_bbnf_selfhost` in an isolated
worktree `CARGO_TARGET_DIR`. Cold build 5.02s; test <0.01s. loadavg at run 13.00 / 12.90 / 12.23
(machine shared with other agents; irrelevant to a byte-offset verdict).

## What ran — the numbers

```
PROBE[A/literal-single-source] bbnf.bbnf:        REJECT @ byte 421 (line 9, col 40)
    err: BBNF-PARSE: unexpected token `-` at byte 421
    line: identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ -> Span ;
    ctx:  …_a-zA-Z0-9-]*/ ⟨HERE⟩-> Span ;…
PROBE[B/self-contained]  bbnf∪expr∪types:        REJECT @ byte 323 (line 7, col 40)  [same `->`]
PROBE[C/standalone] types.bbnf:                  PARSE OK — 1 rule accepted
PROBE[C/standalone] expressions.bbnf:            REJECT @ byte 115 — `-` of `-> i64` (int_lit)
PROBE[C/standalone] bbnf.bbnf:                   REJECT @ byte 323 — `-` of `-> Span` (identifier)
test result: ok. 3 passed
```

**The wall is the typed-projection arrow `->`, at the FIRST rule that uses it.** Not `@import`,
not `@{}`, not closures, not the recursive-alternation tower — the parser dies on `bbnf.bbnf`'s
*ninth line*, its first terminal `identifier = /…/ -> Span`. `types.bbnf` (the one self-grammar
with zero `->`) is the only one that parses.

### Why the RD parser cannot reach `->`
`Parser::parse_atom` (`grammar/src/lib.rs:1416-1452`) accepts only literal / regex / group / ref;
`parse_postfix` (`:1365-1414`) accepts only `* ? +` and **explicitly rejects `?w`**
(`:1387` → `BBNF-DIRECTIVE-NOT-IN-SKINNY`, "whitespace modifier ?w is not available in the skinny
compiler"). There is **no `->` token anywhere in the RD parser**, so the arrow's `-` falls to the
catch-all `Some(ch) => "unexpected token \`{ch}\`"` (`:1450`). skinny's GrammarIr-producing
front-end is, by construction, only the **untyped JSON subset**.

### Both entries refuse `->` — corroboration
- **RD parser** (`emit_from_source` / `emit_typed_from_source` path): hard parse error above.
- **Runtime construct-scanner** (`emit_runtime_from_request` path — the *other* §0 entry): the
  committed test `w5a_named_unsupported_constructs_are_source_located` (`:2042-2052`) asserts
  `root = "x" -> 0u8` yields `BBNF-UNSUPPORTED-PROJECTION` at the offset of `->`. The scanner
  *classifies* `->` but flags it unsupported; it never lowers it.

### The surface skinny never exercised
- `skinny/grammars/json.bbnf` is the **only `.bbnf` in the whole skinny tree, and has 0 `->`.**
- The CSS / "real-typed" path (`xtask:203-214`) injects typed projections via a **hand-written
  Rust `real_typed_schema::schema()` (`DirectSchemaSet`) over the same projection-free json.bbnf**
  — grammar `->` syntax is bypassed entirely.
- `->` counts in the self-grammars: bbnf.bbnf 7, expressions.bbnf 4, types.bbnf 0 (11 total);
  `?w` is pervasive; `@{` (term rule) and `|params|` rule-level closures also unimplemented in the
  RD parser. The self-grammar's *first line* already exceeds the front-end.

## Verdict

**VERDICT: the §0 base-case gate is RED — and the wall is EARLIER and more fundamental than §0
presumed.** `emit_from_source("bbnf", bbnf.bbnf)` does not produce a parser; it does not even
finish parsing. It rejects at **byte 421 (bbnf.bbnf line 9, col 40), the first `->`**, with
`BBNF-PARSE: unexpected token '-'`. The missing primitive is NOT "a general-descent emitter that
lowers bbnf's exotic surface" — the emitter is never reached. It is that **skinny's hand
front-end (`grammar::parse_grammar`, the SEED all of 7A/7B/7D inherit) implements only the untyped
JSON subset and cannot parse `->`, `?w`, `@{}`, or `|params|` closures** — the constructs bbnf
uses on nearly every rule. This is a designed-in boundary (the codebase ships a `bbnf-self.bbnf`
fixture whose test *asserts* `->` is unsupported), not an accident.

**Consequence for pass 2.** The shared §0 wall is confirmed and re-scoped: the first, mandatory,
larger-than-assumed obligation for the bbnf leg of every ladder posture is to **grow the front-end
parser** (`parse_atom`/`parse_postfix`/rule-rhs in `skinny/crates/grammar/src/lib.rs`) to accept
bbnf's own surface — `->` mapped-factor, `?w`, `@{}` host-capture, `|params|` closures,
grammar-function call-args — before "does the emitter lower it" is even askable. Per the standing
law (augment BBNF/the parser, never work around), this is parser-extension work, not a shim. The
lowering wall §0 described is **unreached-by-construction** and cannot be probed until the parse
wall falls. 7C remains the only family that needs none of this (orthogonal, in-tree).

## Fix-target (named, not implemented)
`skinny/crates/grammar/src/lib.rs`: `parse_postfix` (`:1365`, add `?w`), `parse_atom` (`:1416`,
add `@{`), and the rule-rhs chain feeding them (add `->` mapped-factor, `|params|` closure,
`ident(call_arg,…)` grammar-function calls). This is the T3-scale primitive the §0 gate hides.

---
*Deliverables: this file + `PROTO-1.patch` (the running probe test, 3 cases, exit 0).*
