# PROTO-3 — the grammar-neutral LEAF EVALUATOR on `general_descent`

LOCK-7 (Grammar Totality & Bootstrap), pass-2, probe 1 — successor to the refuted
OPEN-1′ (PROTO-1). Family 7A / 7B-Plane-A / 7D-bbnf (shared bbnf-leg make-or-break).

- **Measured HEAD:** `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` (exact base-pin).
- **BASE-PIN:** `git merge-base --is-ancestor ce51c36fc HEAD` = **OK**; HEAD == pin, no
  fast-forward needed. No wrong-artifact-risk.
- **Verdict:** **BLOCKED — MISSING PRIMITIVE, not a bounded arm-swap. RAN_MEASURED.**
- **loadavg:** 4.28 / 4.31 / 4.69 at start; build+test 3.55s cold / 0.14s warm, load 4–5.

---

## QUESTION

Is giving `general_descent` a bbnf-capable leaf evaluator a **BOUNDED arm-swap** (route
`/regex/` leaves through the extant LOCK-5 compiled matcher) or a **MISSING PRIMITIVE**
equal to the original problem (re-author regex matching in the emitter)? PROTO-1 refuted
the delimiter-scan at the lexical floor; this decides whether 7A/7B-Plane-A/7D-bbnf are
reachable or BLOCKED on a LOCK-5/9C coupling (INC-5).

## METHOD (what actually ran)

One runnable probe, `skinny/crates/grammar/tests/leaf_matcher_probe.rs`, exercising the
**real in-tree crates** (`grammar`, `ir`, `bbnf-regex`, `parse-that-regex`) — no vendored
copies, no SPEC-only reasoning. `cargo test -p grammar --test leaf_matcher_probe --
--nocapture`, isolated `CARGO_TARGET_DIR` (no target-dir contention).

- **Wall-0 (landed).** `parse_seq` (`grammar/src/lib.rs:1361`) consumed a standalone `?w`
  and `continue`d, orphaning a following postfix `->` or binary `<<`/`>>` at the head of the
  next atom. Fix: extract the operator loop of `parse_postfix` into `parse_postfix_ops(expr,
  start)` and apply it to the `?w` annotation before pushing it. Bounded: +12 net lines, one
  shared method, two call-sites.
- **Wall-1 (the probe).** Rather than copy `emit.rs`/`regex_first.rs` and *pretend* a matcher
  exists, the probe asks the load-bearing question directly: **does a matcher target exist to
  route the leaf to?** It (A) censuses `parse_grammar` over the 4 corpus families, (B) pulls
  the oracle regex-leaf spans from the bbnf GrammarIr, and (C) attempts to route each of the 5
  named structural leaves through every extant "compiled matcher" primitive
  (`bbnf_regex::analyze`, `parse_that_regex::{match_string, validate_number}`).

---

## THE NUMBERS

### [A] Oracle census — Wall-0 lands; a NEW wall (`@import`-closure) appears

```
grammar::parse_grammar (single file), post parse_seq ?w-fix:
  bbnf     ERR  unresolved rule reference `value_expr`      <- @import closure, NOT ?w
  json     Ok   (10 rules)                                  <- was ERR @319 pre-fix
  css-l4   ERR  unresolved rule reference `declaration`     <- @import closure, NOT ?w
  sheets   Ok   (39 rules)
  => 2/4 single-file

bbnf import-closure (bbnf + expressions + types) parse_grammar == Ok (53 rules)
```

Wall-0 is genuinely fixed: **json flipped ERR→Ok**, and css-l4's pre-fix parse error
(`unexpected token '<'` @715, the `?w << "}"` site) is **gone** — it now fails *deeper*, at
`resolve_refs`, on an imported symbol. The bbnf/css single-file failures are the **`@import`
closure wall** (multi-file grammars; `parse_grammar` resolves refs against one file only), a
*distinct* wall from both Wall-0 and the leaf floor. Parsing the bbnf import-closure resolves
+ validates clean (53 rules), certifying every `?w`-across-operator site in `bbnf.bbnf`
(`big_comment`/`comment` lines 17–18, and `<<`/`>>` generalisation).

### [B] Oracle regex-leaf spans (bbnf GrammarIr, keyed by the 5 named rules)

```
  identifier   span [395..420] len 25  /[_a-zA-Z][_a-zA-Z0-9-]*/
  literal      span [451..466] len 15  /(\\.|[^"\\])*/
  literal      span [492..507] len 15  /(\\.|[^'\\])*/
  literal      span [532..547] len 15  /(\\.|[^`\\])*/
  regex        span [584..598] len 14  /(\\.|[^\/])+/
  big_comment  span [641..649] len  8  /[^\*]*/
  comment      span [691..695] len  4  /.*/
  => 7 regex leaves across the 5 named structural rules
```

### [C] Arm-swap TARGET census — there is nothing to route to

```
bbnf_regex::analyze -> RegexKind over the 7 named-rule leaves:
  identifier   kind=Unknown  first=Exact    nullable=false
  literal ×3   kind=Unknown  first=Unknown  nullable=true
  regex        kind=Unknown  first=Unknown  nullable=false
  big_comment  kind=Unknown  first=Exact    nullable=true
  comment      kind=Unknown  first=Exact    nullable=true
  histogram = { Unknown: 7 }   => 7/7 classify Unknown (no matcher template)

extant EXECUTABLE scanners on a real bbnf leaf token:
  parse_that_regex::match_string("foo-bar")            => Err(ExpectedString)  (needs leading '"')
  parse_that_regex::validate_number("[_a-zA-Z]...")    => false
  bbnf_regex::analyze(".*")                            => RegexFacts{kind=Unknown} — a classifier,
                                                          NO consume/match method on the type
```

The whole regex substrate in skinny is:
- **grammar tier** — `ExprKind::Regex{pattern}` / `ir::BackendExpr::RegexProgram{ pattern:
  String, span_kind }`: an **opaque interned pattern string**, not a compiled program
  (`RegexProgram` is a misnomer — there is no program, DFA, NFA, instruction, or transition
  table anywhere in `skinny/crates`; grep for `struct Dfa|Nfa|Program|Instruction|transition`
  is empty).
- **engine tier** — `bbnf-regex::analyze`: a **classifier** returning `RegexFacts` (nullable,
  first-set, and one of exactly four hand-recognised kinds Whitespace/QuotedString/Numeric/
  **Unknown**). It has no method that consumes input against an arbitrary pattern.
- **`parse-that-regex`** — hand-written **JSON-family scanners** (`match_string` keyed to `"`,
  `validate_number`, `skip_ascii_whitespace`), each bound to a fixed delimiter/shape.

bbnf's 5 structural leaves are all **Unknown** to the classifier and match **none** of the
JSON scanner shapes. There is no arm to swap the delimiter-scan *for*.

---

## THE VERDICT LINE

**BLOCKED — the leaf evaluator is a MISSING PRIMITIVE, not a bounded arm-swap.** The "extant
LOCK-5 compiled matcher" the brief hypothesises **does not exist** in skinny: the grammar tier
holds an opaque pattern string, the engine tier (`bbnf-regex`) is a first-set/nullability
**classifier** that buckets every bbnf structural leaf as `Unknown`, and the only executable
matchers (`parse-that-regex`) are delimiter-fixed JSON scanners. Routing `/[_a-zA-Z]...`,
`/(\\.|[^\/])+/`, `/[^\*]*/`, `/.*/` through them is impossible — **7/7 named-rule leaves have
no matcher target**, so the leaf→matcher routing produces **0** byte-equal spans. Giving
`general_descent` a bbnf-capable leaf evaluator therefore requires **authoring a general
regex-execution engine** (NFA/DFA over an arbitrary pattern, honouring nested-alternation
first-sets, backslash escapes, `/`-delimited bodies, negated classes, the bbnf identifier
class) — the *exact* thing PROTO-1's cascade named and the `regex_first.rs` docstring defers.
This is **equal to the original problem**.

PASS/FAIL rubric:
- (a) `parse_grammar==Ok` 4/4 — **NO (2/4)**; the 2 misses are the `@import`-closure wall, not
  Wall-0 or the leaf floor (the bbnf import-closure parses Ok, certifying Wall-0).
- (b) 5 structural leaves tokenize byte-equal to the oracle — **NO (0/7)**; no matcher exists
  to produce any span.
- (c) bounded arm-swap, no E0499-class escape — **MOOT**; there is no target primitive, so the
  "swap" is a re-authoring, not an arm-swap. (No borrow escape was reached because no swap
  compiled — the block is upstream of any borrow question.)

### The escaping construct + the required LOCK-5/9C primitive

- **Escaping construct:** a **general regex matcher** — `fn match_at(pattern, input, start) ->
  Option<end>` over an *arbitrary* leaf pattern. Absent at every tier.
- **Required primitive (INC-5 coupling):** stand up LOCK-5's **engine-tier executable matcher**
  (rich `Hir` → runnable NFA/DFA) as a callable leaf primitive, OR consume **LOCK-9/9C's
  SCANNER-AS-DFA** (REGISTRY XL-5: it "consumes the bootstrapped regex", i.e. it is a
  *downstream* consumer, not an available upstream leaf primitive). Either way the leaf
  evaluator is **gated on LOCK-5's two-tree substrate fork** (REGISTRY XL-2: skinny
  opaque-regex-leaf vs top-level interned) — not a free re-home.

### What this says for LOCK-7

7A / 7B-Plane-A / 7D-bbnf are **BLOCKED on a LOCK-5/9C coupling (INC-5)**: the shared bbnf-leg
make-or-break is not the builder augment (PROTO-1) and not a leaf arm-swap (this probe) but the
**absence of a general regex-execution primitive** under `general_descent`. Any bbnf self-host
must first give the emitter a real regex matcher — after which the `-> Span`/`-> T` / `add_expr`
/ `resolve_refs` questions (and the deferred `@import`-closure wall this probe newly surfaced)
can be posed. A secondary, cheaper finding banked in passing: **Wall-0 is a bounded, landed
fix** (json ERR→Ok, css parse-error eliminated), and the next parser wall after it is
`@import`-closure resolution, not the operator/atom gaps.

---

## Artifacts (code that ran)

- `skinny/crates/grammar/src/lib.rs` — Wall-0 fix (`parse_postfix_ops` extraction + `?w`
  operator attachment).
- `skinny/crates/grammar/Cargo.toml` — dev-deps `bbnf-regex`, `parse-that-regex` (probe only).
- `skinny/crates/grammar/tests/leaf_matcher_probe.rs` — the runnable census/leaf/target probe.
- `PROTO-3-leaf-evaluator.patch` — the full staged diff.
