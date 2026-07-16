# LOCK-7 pass-2 — Research Directives (DISTILLED)

Pass-2 research for the grammar-totality & bootstrap formulations. Answer the questions with
evidence and options — never conclusions. Every measurement names its HEAD (XL-H) and runs on
skinny@HEAD, the measurement substrate (XL-A).

## Shared facts to carry (given; do not re-derive, do not extrapolate beyond)
- A general, shape-dispatched (grammar-NEUTRAL) descent emitter exists: `crates/codegen/src/general_descent/` (~1206 LOC), wired via `xtask regen-sheets` for the Sheets operator-tower grammar. `bbnf.bbnf` has no `value` rule, so it shape-dispatches to the same branch (`runtime_generator.rs:98 !has_rule("value")`). It emits ONE `parse_<rule>` **recognizer** per rule — recognizers, not GrammarIr builders. Whether a builder augment over it round-trips is OPEN and unseeded; do not presume it from the legacy emitter.
- The hand builder parser handles `->` (`grammar/src/lib.rs:1404 parse_postfix` → `parse_projection:1481`). Three bounded parser gaps block routing `bbnf.bbnf` through the emitter: `?w`→`->` attachment (`parse_seq`), `@{}` atom (`parse_atom`), `|params|` closures.
- Construction census: node values are pure `->`; build mechanisms are host-bound — `add_expr` (append) and `resolve_refs` (cross-rule link, TreeFold); the host floor depends on a LOCK-1 inherited-attribute verdict.
- `parse-that/.../regex/mod.rs:34 parse_generated` is a PUBLIC producer (parse then `host::lower`), not a recognizer.
- Regex substrate is SETTLED (XL-C): two domain-fit types — grammar tier `Regex(StringId)` opaque, engine tier rich `Hir`, bridged via `simplify_hir`; vendored parse-that-regex in skinny is the measurement substrate, the top-level duplicate is legacy (archives at GN-0). The fast TS face is the build-time emitter (XL-D).

## STAGED-BOOTSTRAP (7A)
- Re-derive the `seed_LOC` headline on skinny and state the trade vs a total self-host; note whether a *building* seed inflates toward the hand-parser LOC. Verify the ~88s→0.5s datum is a legacy transplant (`grep regen-bbnf|fixpoint` in skinny xtask).
- Choose the fixpoint SUBJECT — generated `.rs` bytes vs GrammarIr equivalence — and show any byte-determinism evidence is non-vacuous (a fixed-const emit path stamps identical bytes for any input; triple-identical certifies nothing).
- Cost the legacy fixpoint-gate as RESCUE-and-port vs re-author (legacy archives at GN-0, XL-A); name the born-RED gate for the parser-augment.

## TOTAL-SELF-HOST (7B) — open scope questions
- Scope axis: does the middle-IR tower owner (LOCK-3) adopt the grammar-homoiousia route (node values `->`-expressible)? If yes, draft the joint LOCK-3⊗LOCK-7 IR-plane family and name `ir.bbnf`'s consumer; if no, name what remains.
- Structure-scoping: can codegen control-flow / dispatch / tape-assembly be expressed as grammar-plane data without a borrow-checker escape? Measure the data-vs-escape ratio on one emitter blob and give the NAMED kernel-escape failure signature. A research answer, not a route.

## HOST-FUNCTION-LATTICE (7C)
- Split independence: lock-free {kill `passes/src/lib.rs:~798` heuristic, lower `@{}` at `grammar/src/lib.rs:222`, name `resolve_refs`} vs LOCK-1-bound (typed signature into inference, tier off `&mut Ctx`, XL-3). Mark that the census/totality% deliverable needs bbnf to parse+build.
- Specify the totality% counter (leaves × branches) and a floor-gate as a GN-5 `check-name-gates` sibling, with a planted-breach falsification.
- Confirm the CONTEXT-tier zero-cost invariant is testable (`resolve_refs` round-trip; TOML `push_path`) before that tier is scheduled.

## META-CIRCULAR-ORACLE (7D)
- Codify the equivalence relation with `Empty ≡ Concat([])` and the shared-semantic-pass pivot (`resolve_refs`/`validate` on both sides or neither); pin the relation-id before quoting any parity%.
- On the settled regex substrate (XL-C), name which regex divergences are engine-tier `host.rs` obligations vs oracle verdicts; correct the record that `parse_generated` is a producer.
- Draft the promotion-ledger schema with a totality% column sourced from 7C's counter; specify golden-anchor (not majority-vote) + planted-mutation falsification.

## Shared open empirical question (unseeded — report RED-unknown)
Authoring `-> Span`/`-> T` builder projections on bbnf's structural rules over the recognizer emitter: does the emitted parser reparse the corpus into GrammarIr-equivalent structure (within the measured `add_expr`+`resolve_refs` host budget) while `regen∘regen` holds byte-identical — or does it cascade, and at which construct? Drive the `!has_rule("value")` branch, never `json_sink_direct`. Do not seed the verdict from the legacy emitter or from the emitter's mere existence.
