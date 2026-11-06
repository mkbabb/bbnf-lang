# The parse-that LR/packrat GO/NO-GO dossier (2026-07-05)

Scope: the dated decision parse-that has been waiting on. parse-that keeps its packrat/WDM
left-recursion tier alive on a *provisional* keep, held open expressly for one hypothetical
consumer — bbnf-lang's grammar DSL. This dossier answers whether that consumer will ever
materialize, states what GO and NO-GO each mean for both repositories, gives the
recommendation with evidence, and prices the cost of leaving it undecided.

**Recommendation: NO-GO.** bbnf will not exercise parse-that's runtime packrat/WDM
left-recursion tier — now or on the forward roadmap — because bbnf already eliminates
left-recursion at compile time, in-pipeline, and its production grammar fleet expresses every
recursive/precedence construct with the PEG repetition idiom rather than left-recursion.

---

## 1. What the tier is, and what parse-that is waiting for

parse-that's `typescript/src/parse/packrat.ts` (488 LOC) implements the
Warth-Douglass-Millstein "Packrat Parsers Can Support Left Recursion" (PEPM '08) algorithm,
keyed on `(id, offset)` (`packrat.ts:6-13`). It layers a runtime seed-and-grow machinery
(`setupLR`/`growLR`, `LR` markers, `HEADS`/`GROWING` tables, `packrat.ts:26-49`) on top of a
plain memoization core, all behind the `PACKRAT_ARMED` zero-alloc latch (S.H1;
`packrat.ts:137-156`) that costs nothing until a `memoize()`/`mergeMemos()` wrapper is
constructed. The default `parse()` — parse-that's SOTA-performance artifact — "omits
left-recursion / packrat entirely" (`packrat.ts:12-13`).

The recorded keep is explicitly conditional. `docs/future-research.md:214-220`: "The
WDM/LR left-recursion tier keep is PROVISIONAL … The tier is kept pending the bbnf-lang
LR-consumer question — bbnf-lang is the one grammar-DSL that would exercise it … If no LR
consumer materializes, a future cut may retire the tier." The 1.0.0 non-goals list Squirrel
LR alongside token streams and incremental parsing (`future-research.md:200-204`). The
VALUEJS-PT-E letter (2026-07-05) confirms no other neighbor drives it: value.js benefits only
from packrat *arming* as a pure win at 82 ns/parse and records Pratt/precedence as dormant,
"does not clear a KISS/DRY bar" (`VALUEJS-PT-E-2026-07-05.md:23-31`). bbnf is the sole
outstanding question.

---

## 2. The bbnf side — the fleet needs no runtime LR, and bbnf already handles LR at compile time

Two independent facts each suffice for NO-GO; together they are dispositive.

**(a) The production fleet uses the PEG repetition idiom, never left-recursion.** Every
precedence tower in the fleet is written as `head , ( op , head ) *` — the canonical way to
express left-associative operators without left recursion. The most complex expression
grammar, `grammar/google-sheets/google-sheets.bbnf`, builds a full seven-level tower
(comparison → concat → add → mul → exp → unary → postfix → primary) entirely this way, e.g.
`add_expr = mul_expr ?w , (add_op ?w , mul_expr ?w) *`. The BBNF value-expression grammar
does the same across six precedence levels (`grammar/bbnf/expressions.bbnf`,
`value_mul`/`value_add`/`value_cmp`/`value_and`/`value_or`), and `grammar/misc/math.bbnf`
uses `expr = term, { ("+"|"-"), term }`. None is left-recursive.

The single left-recursive grammar in all 646 `.bbnf` files is `grammar/misc/math-ambiguous.bbnf`
(`expression = expression , ((...)) ? | number`), and its name declares its purpose: a
demonstration of ambiguity. It is a **fixture, not a consumer** — referenced only by the
self-parity test `crates/core/tests/bbnf_self_parity.rs:170` (which asserts bbnf can *parse
the grammar text*, not *execute* a left-recursive parser) and the playground term registry
`playground/src/lib/termRegistry.ts:241`. No production parser is generated from it.

**(b) bbnf owns compile-time left-recursion elimination — the architecturally correct place
for a parser generator.** `crates/ir/src/passes/lr.rs` (321 LOC) implements standard direct
elimination (`A = Aα | β` → `A = βA'`, `A' = αA' | ε`, `lr.rs:1-27`) and indirect elimination
via Paull's algorithm over SCCs (`lr.rs:132-135`). Both are wired as **active, unconditional
pipeline passes**: `crates/core/src/pipeline/compile/pipeline.rs:95-99` runs
`eliminate_indirect_lr` then `eliminate_direct_lr` on every compile. The analysis layer
already detects left recursion via Tarjan SCC and reports it
(`crates/analysis/src/state/diagnostics/cycles.rs:10-21`).

The strategies are mutually exclusive by design. parse-that's packrat **executes** a
left-recursive PEG at runtime via memoized seed-and-grow. bbnf is a parser *generator*: it
**rewrites** left-recursion into right-recursion at IR-lowering time, then emits standalone
recursive-descent Rust/TS that never encounters left recursion and never allocates a memo
table. bbnf-generated parsers route through `dispatch()+regex`/`all`/`any`/closure combinators
with no packrat on the path (report 17 §3). A runtime seed-and-grow tier is not merely unused
by bbnf — it is the wrong tool for a codegen parser generator whose first-order mandate is
cold, zero-alloc dav1d-discipline parse speed. Packrat's per-parse Map allocation is
antithetical to that mandate.

---

## 3. What GO means

GO would assert that bbnf commits to a runtime path that exercises parse-that's packrat/WDM
tier — i.e. bbnf's TS backend would emit combinator code that leaves left-recursion *in place*
and relies on parse-that's runtime seed-and-grow to resolve it, rather than eliminating it via
`lr.rs`.

- **parse-that:** the WDM tier graduates from provisional to committed; its soundness surface
  (the `(id, offset)` fix `193854d`, the >1 MB Float64-safe memo key PT-Q2, the re-entrancy
  fix, the `PACKRAT_ARMED` latch) becomes a supported, permanently-tested API contract with a
  named downstream. The 1.0.0 "provisional keep" caveat resolves to "keep."
- **bbnf:** would have to *disable* its own `eliminate_direct_lr`/`eliminate_indirect_lr`
  passes for the TS target, generate combinator code targeting parse-that's memoized runtime,
  accept per-parse memo-Map allocation on the generated hot path, and carry the packrat design
  learnings report 17 §4 flagged as GO-conditional (the memo-key and latch designs). This
  contradicts the standalone-codegen model and the cold-parse performance charter.

GO has no live driver: no grammar in the fleet needs it, and adopting it would regress bbnf's
own architecture and performance posture.

---

## 4. What NO-GO retires

NO-GO tells parse-that the phantom consumer will not materialize, freeing a future cut to
retire the **left-recursion machinery specifically** — `setupLR`/`growLR`, the `LR` markers,
the `HEADS`/`GROWING` tables, the seed-and-grow re-entry logic (`packrat.ts:26-49, 109-135,
188+`). This is the bulk of `packrat.ts`'s complexity and its entire soundness-audit surface.

NO-GO does **not** necessarily retire packrat *memoization* itself: value.js is a live
consumer of packrat arming as a pure win (`VALUEJS-PT-E:22-24`), and the `PACKRAT_ARMED` latch
plus the plain memoize core may stand on that account. The precise retirement target is the
WDM left-recursion layer that sits atop the memo core — the layer held open for bbnf alone.
parse-that gets to collapse packrat to a memoization primitive without a left-recursion tier,
shedding the ongoing soundness-proof maintenance the tier demands each tranche.

bbnf retires nothing — it keeps `lr.rs` (its own compile-time elimination, which stands on its
own merit for any future left-recursive input) and simply records that it does not and will
not consume parse-that's runtime tier.

---

## 5. The cost of deciding late

The tier is cheap to *hold* (the `PACKRAT_ARMED` latch makes it zero-cost at runtime for
non-memoizing consumers) but not cheap to *maintain*. Each parse-that tranche re-audits the
WDM machinery's soundness: the arc `193854d` (soundness) → `7901314` (cross-input BLOCKER) →
`2c806fb` (re-entrancy + >1 MB memo-key) is three separate correctness fixes to a tier with no
live consumer (report 17 §1). Deferral means:

1. **Continued soundness-maintenance tax** on parse-that for a phantom downstream — every
   packrat touch re-proves left-recursion correctness (armed 2/2) that nothing exercises.
2. **The 1.0.0 "provisional" caveat never closes** — parse-that cannot cut a clean packrat
   API surface while the keep is conditional on an unanswered neighbor.
3. **A standing cross-repo ambiguity** that every future refresh (this is the second) must
   re-litigate from scratch, re-reading both trees to reconstruct that bbnf uses compile-time
   elimination — cost already paid twice.

The decision costs nothing to make now (the evidence is unambiguous and stable) and accrues
maintenance + audit drag every tranche it is left open. There is no scenario in which the
answer flips: bbnf's compile-time elimination is load-bearing and wired into every compile,
and no fleet grammar requests runtime LR.

---

## 6. Decision

**NO-GO, dated 2026-07-05.** bbnf-lang will not exercise parse-that's runtime packrat/WDM
left-recursion tier. bbnf handles left-recursion at compile time via
`eliminate_direct_lr`/`eliminate_indirect_lr` (`crates/ir/src/passes/lr.rs`, wired at
`crates/core/src/pipeline/compile/pipeline.rs:95-99`), and its production fleet uses the PEG
repetition idiom throughout; the sole left-recursive grammar is a named ambiguity fixture.
parse-that is released from the bbnf keep and may retire the WDM left-recursion machinery at
its convenience, retaining the packrat memoization core for value.js. SK-V21's SPEC should
record this as a dated outbound decision to parse-that (closing critic gap G3,
`research/32-completeness-critic.md:117-119`) and drop the GO-conditional packrat-design
fold-in (report 17 §4 item 3) to a recorded-but-not-carried note.
