# PROTO-1 — OPEN-1′: the builder-augment cascade on `general_descent`

LOCK-7 (Grammar Totality & Bootstrap), pass-2, probe 1. Family 7A/7B/7C/7D (shared make-or-break).

- **Measured HEAD:** `c93fc43143743ffaf54855c526f6aeab79227352`
- **BASE-PIN:** worktree fast-forwarded (`--ff-only c93fc4314`) from `d2b32f6e3`, which was an *ancestor* of the pin (behind, not divergent). Post-ff `git merge-base --is-ancestor ce51c36fc HEAD` = **OK**; `c93fc4314` = HEAD. No wrong-artifact-risk.
- **Verdict:** **FAIL (informative) — the cascade is real, RAN_MEASURED.** Named construct below.
- **loadavg at measurement:** 4.97 / 7.46 / 8.87 (leaf probe); build ran at 10→5.

---

## QUESTION (RED-unknown)

Does authoring `-> Span`/`-> T` builder projections on bbnf's structural rules **over the
`general_descent` recognizer emitter** reparse the `grammar/**/*.bbnf` corpus into
`ORACLE_REL_V1`-equivalent `GrammarIr` (within +1 SOFT `add_expr` / +1 HARD `resolve_refs` host
budget), regen∘regen byte-identical — or does it cascade, and at which construct?

## METHOD (what actually ran)

Two runnable probes, both on verbatim in-tree code paths — no SPEC-only reasoning:

1. **Stage-1 precondition census (RAN).** `skinny/crates/grammar/tests/oracle_selfparse_probe.rs`
   calls the hand-written ORACLE `grammar::parse_grammar` on one file per corpus family
   (bbnf / json / sheets / css-l4). `cargo test -p grammar --test oracle_selfparse_probe`.
2. **Stage-2 leaf cascade (RAN).** `scratchpad/leaf_probe.rs` lifts the **verbatim**
   `general_descent` leaf logic — `regex_first::{first_bytes,is_nullable}` (whole module) plus
   `emit.rs::{match_leaf, consume_quoted, is_text_byte}` — and exercises it against the *real*
   bbnf corpus leaf patterns and tokens. `rustc --edition 2021 -O` (std-only, zero target-dir
   contention). The recognizer's own byte offsets are the numbers.

The experiment never advanced to authoring the host `add_expr`/`resolve_refs` walk, planting the
Alt-reorder mutation, or running regen∘regen — because both preconditions fail upstream, below.

---

## THE NUMBERS

### Stage 1 — the ORACLE precondition is UNMET at HEAD

```
ORACLE CORPUS CENSUS (grammar::parse_grammar):
  grammar/bbnf/bbnf.bbnf            ERR  "unexpected token `-`" @ 662
  grammar/json/json.bbnf           ERR  "unexpected token `<`" @ 319
  grammar/google-sheets/…          Ok   (39 rules)
  grammar/css/l4/stylesheet.bbnf   ERR  "unexpected token `<`" @ 715
```

Source at each offset:

| file | offset | bytes | construct |
|---|---|---|---|
| bbnf.bbnf | 662 | `… "*/" ) ?w -> Span ;` | `->` after `?w` (big_comment, line 17) |
| json.bbnf | 319 | `… ) *)?w << "]" ;` | `<<` after `?w` |
| css l4 | 715 | `… ?w << "}" ;` | `<<` after `?w` |

**All three failures are one bug:** `parse_seq` (lib.rs:1361) consumes a standalone `?w` and
`continue`s, so a following postfix `->` or binary `<<`/`>>` is met at the head of the next atom and
rejected. This is precisely the brief's **Stage-1 fix #2** ("attach `?w` across `->`"), generalized to
`<<`/`>>`. It fires at line 17 of bbnf.bbnf — **before** the `term` rule (line 33+), so the anticipated
`parse_atom` `@{`/`[`/`{` gap (fix #1) is never even reached. Sheets is the lone `Ok` because it uses no
`?w`-then-operator sequence. **The three bounded Stage-1 fixes are genuinely un-landed at `c93fc4314`.**

### Stage 2 — the builder-augment path cascades at construct #1: the regex/quoted **leaf lexer**

`general_descent` defers typed leaves to a Sheets/JSON-tuned Span **delimiter-scan** (its own
docstring: *"typed leaves deferred to `Span`"*). Run against bbnf's actual leaves:

```
regex_first::first_bytes on bbnf structural leaf patterns
  identifier   /[_a-zA-Z][_a-zA-Z0-9-]*/     first={53 bytes}  nullable=false
  literal "    /(\.|[^"\\])*/                 first={'.'}       nullable=true
  regex body   /(\.|[^\/])+/                  first={'.'}       nullable=false
  big_comment  /[^\*]*/                       first={} EMPTY    nullable=true
  comment      /.*/                           first={} EMPTY    nullable=true

The cascade (match_leaf on real corpus spans):
  A) `regex` body leaf on "[_a-zA-Z][_a-zA-Z0-9-]*"
       => Err("head 0x5b '[' NOT in first set -> REJECT")   (oracle: consume all 23 bytes)
  B) `literal` body leaf on `a\"b`
       first={'.'} nullable=true => Ok(0) consumes 0 bytes   (oracle: 4)  -> closing-quote desync
  C) consume_quoted on source token "a\"b" (backslash escape)
       => end 4   (oracle backslash-aware end = 6)           MIS-TERMINATED
  D) identifier leaf on "foo-bar" (bbnf class has '-')
       => Ok(3)   (oracle: 7)   is_text_byte('-')=false      truncates at the hyphen
```

Root causes, with E-code:

- **`regex_first::head_bytes`/`split_top_level_alternation`** (regex_first.rs:81,90,120) never descend
  into a **nested** `(a|b)` group: `split_top_level_alternation` splits only at paren-depth 0, and
  `head_bytes` on a leading `(` reads only its *first* token. So bbnf's `regex` body `(\.|[^\/])+`
  collapses to first = `{'.'}`, non-nullable → the head-gate in `match_leaf` (emit.rs:175) **hard-rejects**
  every real `/.../ ` literal (head `[`, `.`, `\d`, letters…). This is the *first* regex literal in
  bbnf.bbnf (line 9) — a hard `ParseError`.
- The same collapse makes `literal`'s `(\.|[^"\\])*` first = `{'.'}` but **nullable**, so the head-gate
  passes vacuously (Ok, 0 bytes) and the *closing* `"` ByteLiteral desyncs one node later.
- **`consume_quoted`** (emit.rs:233) honors **doubled-quote** escapes (Sheets/CSV `""`), not bbnf's
  **backslash** escapes — `"a\"b"` closes at the escaped quote.
- **`is_text_byte`** (emit.rs:226) `[A-Za-z0-9_.$]` **excludes `-`** (bbnf identifier class) and
  **includes `.`/`$`** (not in it) — both over- and under-run bbnf identifiers.

---

## THE VERDICT LINE

**FAIL — it CASCADES. The construct is the `general_descent` regex/quoted-string LEAF LEXER**
(`regex_first::first_bytes` + `emit.rs::{match_leaf, consume_quoted, is_text_byte}`), which recognizes
only the Sheets/JSON leaf alphabet and cannot tokenize bbnf's `regex` / `literal` / `identifier` /
comment leaves. This is **upstream of the entire builder-projection question**: no `-> Span`/`-> T`
projection can materialize because the recognizer never produces correct leaf spans. **None** of the
brief's downstream candidate cascades is reached — not nested-Alt-inside-Seq offset underdetermination,
not the deferred `resolve_refs` seam, not `@{` atom recursion, not closure self-reference. The failure
is at the **lexical floor**, at the first regex literal.

Consequences for the PASS/FAIL rubric (all **moot / not reached**):
- host-builder LOC ≤ ½·2142 (≤ 1071): **not authored** — no point building over a recognizer that
  cannot tokenize the corpus.
- regen∘regen byte-identical: **not run**.
- planted Alt-reorder mutation trips: **not planted**.

Independently, **Stage 1 is un-landed**: the ORACLE itself rejects 3/4 corpus families on the
`?w`-across-operator bug (fix #2), which masks the `@{`/`[`/`{` atom gap (fix #1).

### What this says for LOCK-7

The builder-augment-over-`general_descent` route (OPEN-1′) is **refuted as posed**. `general_descent`'s
leaf layer is a grammar-family-specific Span approximation; bootstrapping bbnf through it requires a
**real regex leaf matcher** (the very thing the emitter's docstring defers), not builder projections
bolted onto the existing recognizer. Any bbnf self-host must first give `general_descent` a
grammar-neutral leaf evaluator that honors nested-alternation first-sets, backslash escapes,
`/`-delimited regex literals, and the bbnf identifier class — *then* the `-> Span`/`-> T` /
`add_expr` / `resolve_refs` question can even be posed. Absent that, the answer to the RED-unknown is
a clean **no, at the leaf**.

---

## Artifacts (code that ran)

- `skinny/crates/grammar/tests/oracle_selfparse_probe.rs` — Stage-1 ORACLE census (throwaway probe).
- `scratchpad/leaf_probe.rs` — verbatim `general_descent` leaf logic exercised on the bbnf corpus
  (delivered alongside this report as `leaf_probe.rs`).
