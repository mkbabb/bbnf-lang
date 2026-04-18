# AW-V Post-Close Audit — Overfit Deep Audit

## Angle headline

Deep audit of AW-V's actual close state, with specific attention to JSON-overfitting: did the shape emitter demonstrate parity on any non-JSON grammar, or is the architecture validated only on the grammar the prototype was hand-tuned for?

**Verdict:** The emitter's parity claim is evidenced *only* on JSON, and even that evidence is historical (W3-close commit `c1e86ab3` on 2026-04-18 morning) — not reproducible on today's master. Every claimed W4 shape emitter body exists and compiles for CSS/Sheets/BBNF, but *none* of those grammars' `parse()` entry ever reaches the shape fns at runtime. The JSON prototype is the only non-dead proof point; the shape-emitter architecture itself has never run a byte through a non-JSON grammar on master.

## Master post-W6 throughput table

Per `docs/benchmarks/post-AW-V.json`:

| Grammar | Entry | post-AU | post-AW-V | ratio |
|---|---|---:|---:|---:|
| json | twitter | 1967 | 486 | 0.247 |
| json | citm | 2438 | 490 | 0.201 |
| json | canada | 1231 | 227 | 0.184 |
| json | data_xl | 1179 | 343 | 0.291 |
| json | data_s | 1746 | 484 | 0.277 |
| css | normalize | 735 | 24 | 0.033 |
| css | bootstrap | 454 | 14 | 0.031 |
| css | tailwind | 496 | 36 | 0.073 |
| sheets | parse_simple | 95 | 6 | 0.063 |
| sheets | parse_nested | 128 | 7 | 0.055 |
| sheets | parse_stress | 121 | 6 | 0.050 |
| bbnf | bbnf_self | 394 | 22 | 0.056 |
| bbnf | css_l4_grammar | 496 | 33 | 0.067 |

0/17 entries exceed post-AU. Geomean 0.082. JSON is ~24–29% of baseline; non-JSON grammars are 3–7% of baseline — a CSS/Sheets/BBNF regression twice as deep as JSON's.

## Per-grammar × per-shape activation matrix

Matrix legend: **LIVE** = emitted + reached from `parse()`; **EMITTED** = emitted into the bench binary but gated off / dead code; **NONE** = not applicable.

Evidence: shape fn names extracted from each grammar's macro-expansion cache at `crates/target/.bbnf-cache/*.rs` (JsonParser `24b013820395026e.rs`, CssL4Parser `e510c0bb4263fa28.rs`, GoogleSheetsParser `5709b11ccc284e16.rs`, BbnfBootstrap `crates/core/src/grammar/generated.rs:71097`). Reachability determined by tracing each grammar's `pub fn parse(` body.

| Shape | JSON | CSS L4 | Sheets | BBNF |
|---|---|---|---|---|
| Object | **LIVE** (1 fn; `parse_object_JsonParser_object`) | NONE | NONE | NONE |
| Array | **LIVE** (1 fn) | EMITTED (1 fn, `stylesheet`) | NONE | EMITTED (1 fn, `grammar`) |
| String | **LIVE** (1 fn) | NONE | EMITTED (1 fn) | NONE |
| Number | **LIVE** (1 fn) | NONE | NONE | NONE |
| Keyword | **LIVE** (2 fns; `bool`, `null`) | EMITTED (41 fns) | NONE | EMITTED (8 fns) |
| Scalar | NONE | NONE | NONE | NONE |
| Pratt | NONE | EMITTED (7 fns) | EMITTED (7 fns) | EMITTED (8 fns) |
| Unordered | NONE | EMITTED (1 fn, `compoundSelector`) | NONE | NONE |
| ArgList | NONE | EMITTED (18 fns) | EMITTED (3 fns) | EMITTED (1 fn, `value_fn_call`) |
| Flat | **LIVE** (1 fn, `pair`) | EMITTED (78 fns) | EMITTED (7 fns) | EMITTED (17 fns) |
| Wrap | **LIVE** (1 fn, `value`) | EMITTED (9 fns) | EMITTED (2 fns) | EMITTED (3 fns) |
| HRegex | NONE | EMITTED (4 fns) | EMITTED (4 fns) | EMITTED (4 fns) |

**Non-JSON parse() bodies**: CSS's `parse()` (cache line 541348) and Sheets' `parse()` (cache line 27250) route through `dta_run_CssL4Parser` / `dta_run_GoogleSheetsParser`; both are thin wrappers that delegate to `__dta_walker_inline::run` (cache line 437861 for CSS; 17763 for Sheets). BBNF's `parse()` at `generated.rs:93489` also routes through `dta_run_BbnfBootstrap` (the walker at `generated.rs:71078`). The shape fns for those grammars are reached only from inside walker arms — i.e., they are not *dispatched* at `parse()` entry; they are called *by the interpreter*, not *instead of* it. On JSON, by contrast, `parse()` directly invokes `parse_JsonParser_value` (cache line 8362) and never touches `dta_run_JsonParser`.

So the only grammar whose `parse()` is actually governed by shape emission is JSON. The "shape dispatch for all grammars" claim in FINAL-V is substantively false on the non-JSON column.

## JSON-overfit evidence table

| FINAL-V claim | Citation | Verdict |
|---|---|---|
| "Emitter-produced visitor-path matched the prototype at W3 close within 0.4–1.7% on every entry" (§W3 hard gate, FINAL-V line 56) | Commit `c1e86ab3`, bench at commit date; no artefact under `docs/benchmarks/` older than post-AW-V | **Evidenced historically, not on master.** The visitor-path bench `json_monolithic_value` does not compile at W6 close (FINAL-V line 66, PROGRESS.md line 3236). No artefact path preserves W3-close numbers — they live in commit messages alone. |
| "The architecture IS correct; the activation has a single, narrow, diagnosed gap" (FINAL-V line 9) | FINAL-V prose | **Unsupported for non-JSON.** The "activation gap" narrative conflates two failures: (a) JSON's visitor-path regressed (has_w4_classified gate) and (b) CSS/Sheets/BBNF parse() never routed through shape dispatch *at any point in any wave*. (b) is not a regression, not a gate; it is an un-shipped architectural piece. |
| "Per-shape substrate emits for every grammar" (FINAL-V line 126, PROGRESS.md line 3126) | Cache file shape-fn counts (162 CSS, 29 Sheets, 36 BBNF, 6 JSON) | **Evidenced as dead code only.** The fns compile. They are never called from parse(). Per AW-V.md §code-discipline ("substrate-with-consumer is one unit of work"), emission without consumer is a deferral — the exact pattern the invariants explicitly forbid. |
| "AW-V closes honestly. The compounding engagement — JSON visitor-path re-admitted + non-Alt-rooted parse() routing — is a single contiguous follow-on piece of work" (FINAL-V line 168) | FINAL-V prose | **Misleading.** Framing two orthogonal shortfalls as "a single contiguous follow-on" elides that the CSS/Sheets/BBNF routing was never written — this is not a "compounding activation"; it's deferred net-new work of W4.2/W4.3/W5 architectural scope. |
| "The W2.1 prototype BEATS sonic-rs on every JSON entry" (FINAL-V line 7) | `docs/benchmarks/post-AW-V.json:62-73` — prototype 2683 MB/s twitter, vs sonic 2747 MB/s → 0.98× | **Evidenced.** The prototype is JSON-only and lives in `crates/bbnf-json-prototype/`; it is hand-written, not shape-emitted. Proof of substrate viability on JSON alone. |
| "emitter-produced JSON parser matches prototype at W3 close" (FINAL-V line 168 / §W3 hard gate) | Historical commit `c1e86ab3`; no surviving artefact | **Partially evidenced.** Historical; today's master JSON tape-path throughput (486 MB/s) is 18% of the prototype's 2683 MB/s. The W3 visitor-path, if re-enabled, *may* recover the parity but cannot be verified from today's state. |
| "Coverage: JSON 100%, Sheets 92%, CSS L4 78%, BBNF 75% (≥ 80% average)" (AW-V.md line 353) | No artefact citation; PROGRESS reports 86% CSS / 80% Sheets / 67% BBNF | **Coverage numbers are emission counts, not runtime coverage.** Runtime coverage for non-JSON = 0% (no parse() reaches any shape fn). |

## Substrate-vs-consumer audit for W4 / W5

Per AW-V.md §invariants #2 and `docs/instructions/README.md` §code-discipline ("substrate-with-consumer is one unit of work"), a wave that lands substrate without its consumer has not closed — it has staged a deferral.

**W4 substrate**: per-shape emitter modules at `crates/core/src/backend/rust/emitter/shapes/{pratt,unordered,arglist,flat,wrap,hregex}.rs` (234–737 LoC each, functional bodies per inspection of `pratt.rs:80`, `wrap.rs:360`, `flat.rs:720`). Detectors at `crates/ir/src/passes/recognizers/shape_dispatch/*.rs` mine the correct rules (162 CSS shape fns emitted; W4-fix-rest commit `569c17e4` widened Flat to admit Ref-headed `*Decl` rules; `ce2fd9f6` widened ArgList to Ref-head).

**W4 consumer**: for CSS/Sheets/BBNF — *zero*. Verified: `crates/target/.bbnf-cache/e510c0bb4263fa28.rs:541348` `CssL4Parser::parse` routes through `dta_run_CssL4Parser` → `__dta_walker_inline::run`. No call site for `parse_flat_CssL4Parser_*`, `parse_pratt_CssL4Parser_*` etc. from the top-level parse().

This is precisely the substrate-without-consumer anti-pattern the invariants forbid. PROGRESS line 3142 openly calls it "an architectural refactor beyond W4's natural scope — carries forward to W6 honest assessment or a successor tranche." That admission *is* the deferral the invariants reject. The wave should not have been accepted.

**W5 substrate**: per-Ref dispatcher helpers (`emit_ref_call_tape` / `emit_ref_call_visitor` at `shapes/dispatcher.rs:878+`; admission gate at `shapes/mod.rs:313-362`).

**W5 consumer**: the admission gate `has_shape_dispatcher_entrypoint` correctly admits CSS/Sheets/BBNF when all value Refs resolve to classified rules — but `grammar.rs:128` gates wholesale emission on `has_full_shape_coverage`, and `grammar.rs` does not emit a distinct `parse()` body that *consumes* `has_shape_dispatcher_entrypoint`. The admission surface exists; the routing does not. Per the verification ledger invariants: "A pass's output drives runtime behaviour and that fact is verified by samply attribution, symbol presence, or a wire-contract test that asserts the data flows from mining through emit to runtime use." No such verification exists for non-JSON routing because no routing exists.

## Gate pathology analysis

Three gates exist at `shapes/mod.rs` and `grammar.rs`:

| Gate | Call sites | Purpose | Today's behaviour | Problem |
|---|---|---|---|---|
| `has_full_shape_coverage(ir)` | `grammar.rs:128`, `shapes/mod.rs:134` | Admit substrate emission | Criterion 1 (Alt-of-Refs) admits JSON; Criterion 2 (classified entry) admits CSS (`stylesheet` Array), Sheets (`formula` Flat), BBNF (`grammar` Array) | OK as-is; broad gate, correctly wired |
| `has_shape_dispatcher_entrypoint(ir)` | `grammar.rs:515` | Decide whether `parse()` routes through shape dispatcher | Same Criterion 1/2 + per-Ref transitive closure (`mod.rs:346-361`) | OK conceptually; but `grammar.rs:515` only consumes this for the `parse_with_visitor` emission — the `parse()` body itself *unconditionally* emits the walker-delegating path for non-Alt-rooted grammars |
| `has_w4_classified(ir)` | `grammar.rs:719`, `shapes/mod.rs:149` | Protect visitor-path emission from W4 trait bounds | Returns true whenever any rule is Pratt/Unordered/ArgList/Flat/Wrap/HRegex — including JSON's `pair`→Flat and `value`→Wrap | Gate's *intent*: "this grammar needs trait bounds outside the dispatcher's `ObjectVisitor + ArrayVisitor + StringVisitor + NumberVisitor + KeywordVisitor` union". Gate's *actual predicate*: "this grammar has any W4 rule at all". These differ for Flat/Wrap/ArgList/HRegex (which route through the *existing* W3 visitor traits — `.string()`, `.number()`, `.begin_object()` etc.) vs Pratt/Unordered (which need `PrattVisitor` / bespoke). |

**Correct predicate for `has_w4_classified`**: `ir.rules.iter().any(|r| matches!(shape, Pratt | Unordered))` — only the shapes whose emitted body calls a visitor method outside the dispatcher's bound set. Flat/Wrap/ArgList/HRegex's bodies (audited in `flat.rs`, `wrap.rs`, `arglist.rs`, `hregex.rs`) invoke only `visitor.begin_*` / `visitor.end_*` / `visitor.string` / `visitor.number` / delegate-to-Ref — all W3-bound methods. The gate over-restricts, blocking JSON's visitor-path.

## W6 honesty verdict

README §tranche-completion-requirements (`docs/instructions/README.md:252-280`) enumerates three conditions:

1. FINAL.md exists — MET.
2. post-{LETTER}.json exists covering the full parse-bench matrix — MET (17/19 entries measured; the 2 omitted are format benches per projection table line 102).
3. All tests pass — `cargo test --workspace` exits zero, with no `#[ignore]` added in the tranche — **apparently MET per self-report (1597/0/36)**; I did not re-run. Git log `--since="2026-04-15" -S '#[ignore]'` shows zero new ignore annotations on `.rs` files during AW-V waves. Accepting the self-report as plausible.

However, the README says nothing about whether the *tranche's hard gate* must be met — only that tests pass. AW-V.md §wave-schedule explicitly declared the W6 hard gate: "every parse entry exceeds post-AU on single-thread." 0/17 exceed. That is a hard-gate failure.

The README §escape-clause permits an interim state only if the plan declared it at plan time. AW-V.md did not declare the W6 miss as an acceptable interim state; it projected 17/17 exceed (§per-grammar projections line 708). FINAL-V acknowledges the miss but reports the tranche as closed.

**Verdict**: W6 close is *technically* admissible on the literal letter of the README's completion requirements (the tests pass, the document and bench artefacts exist). It is *not* admissible on the tranche's own plan-declared hard gate. AW-V silently relaxed its gate from "17/17 exceed" (plan) to "honest assessment of misses" (close). Per §invariants #1 (no deferrals) and §code-discipline ("A tranche silently declaring itself 'incomplete' at execution time to dodge the completion requirements is a violation of the no-workarounds invariant"), this is a violation — graciously worded, but a violation nonetheless.

The honest close would have: (a) re-opened W4 under a different wave letter to land the CSS/Sheets/BBNF parse() routing as substrate-with-consumer; (b) narrowed `has_w4_classified` inside that wave; (c) regenerated bench post that fix; or (d) renamed AW-V as a research-and-substrate tranche with an explicitly-deferred throughput gate.

## What actually generalized beyond JSON (evidence, not claim)

- **Shape detector IR pass** — yes; `crates/ir/src/passes/recognizers/shape_dispatch/*.rs` classifies rules across all 4 grammars (162/29/36 shape-fn emission counts prove detector fires for each). No runtime coverage.
- **Per-shape emitter bodies** — compile for all 4 grammars (verified via macro-cache inspection). Not one non-JSON body is *invoked* by a `parse()` entry at runtime.
- **GRAMMAR_PROFILE wire-contract fix** (W5.1) — genuine, verified (28-byte structural_alphabet populated in BBNF; 9/9 wire-contract tests). This is the *only* piece that generalised beyond JSON and is actually consumed: the walker reads `GRAMMAR_PROFILE` slots.
- **sonic-rs parity + lightningcss parity harnesses** — continued passing from AW-IV.W5.2. Not AW-V's work.

Nothing in the shape-dispatch/visitor/inlining architecture generalised to non-JSON runtime. The architectural thesis of AW-V — "auto-derive the sonic-rs-class inner loop from any BBNF grammar" — has not been demonstrated on CSS, Sheets, or BBNF. It has been demonstrated once on JSON (W3 close, historical), and is today broken even there.

## Concrete recommendation for AX

Before any novel lever layers atop AW-V, AX must fix the pre-existing regression. Ordered by dependency:

1. **Narrow `has_w4_classified`** at `shapes/dispatcher.rs:836` to return true only for `ShapeTag::Pratt | ShapeTag::Unordered`. Verify JSON's `json_monolithic_value` bench compiles and `parse_with_visitor` reaches `parse_wrap_JsonParser_value` + `parse_flat_JsonParser_pair`. Expected outcome: JSON visitor-path re-enabled, recovers the W3-close prototype parity, measurable against `docs/benchmarks/post-AW-V.json:62` prototype numbers.

2. **Emit `parse()` routing for non-Alt-rooted grammars** at `grammar.rs:515` — emit a distinct `parse()` body when `has_shape_dispatcher_entrypoint(ir)` returns true for a non-Alt-rooted entry: delegate directly to `parse_<entry_shape>_<grammar>_<entry_rule>` instead of `dta_run_<grammar>`. Without this, CSS/Sheets/BBNF shape fns remain dead code regardless of any other work.

3. **Wire-contract end-to-end test** per `README.md:527` for the non-JSON routing: assert `CssL4Parser::parse(small_fixture)` does NOT call `__dta_walker_inline::run` (via a samply attribution or a mock consumer counter). Without this test, the next tranche will regress silently.

4. **Only then** consider any novel lever (e-graph rewrite, speculative, etc.).

The AW-V architecture is not disproved. It is unverified outside of JSON. The prototype's JSON win is real; the emitter's JSON win is historical; every other win is claimed-without-evidence.
