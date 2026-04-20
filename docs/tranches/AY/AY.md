# Tranche AY — Tape Substrate + Direct-to-Struct + Value API Eager Lane

AY is the performance + projection tranche. Successor to AX; predecessor to AY. AX closed the interpreter (W0b), landed the shape-emission-authoritative tape (W0a), reverted hand-coded Value duplicates (W1r.0), proved grammar-derived view surface via canonical-serialization byte equality (W1r.2/3a/4a/5), and audited the typed-accessor surface (W1r.6). Eight W1r sub-waves landed (`3429aaba`..`ab7c218d`), 13 parity + canonical harnesses green on master (247/1 ignored).

Six fresh audits (A1–A6, `docs/tranches/AX/audit/next-tranche/A{1..6}-*.md`) at master HEAD `9074a685` establish the performance floor: **JSON 5.5-8.2× slower than sonic-rs; CSS L4 beats lightningcss by 19-40% at scale; Sheets/BBNF stable**. The universal hot-path union across all four grammar families is `Columns::push_structural` (23-43% self-time) + `tape::finaliser::finalise` (12-27%) = **50-70% of every bench's runtime**. No scanner, no dispatch, no allocator is the primary loss — the tape substrate's write + post-pass IS the loss.

AX's W1r.1 scope-reveal that `TypeDesc::Named(_)` collapses before Rust emit on every grammar remains unaddressed; AX invariants 20/21 (shape-emission authority + grammar-derived view surface) require this to be surgically repaired. AX's 5 W0a/W0b-era wire-contract tests that retired when their predicates retired (invariant 14) remain on disk unbuildable. AX's bench matrix + FINAL.md remain uncaptured.

AY discharges all three. No deferrals. No hand-coded Value duplicates. No third-party comparator bridges. No new grammar directives (invariant 4). Every wave ships substrate + consumer (invariant 2). All AY invariants are AX's 1-21 plus three new architectural gates: tape substrate inline (22), Named preservation end-to-end (23), and Value API apples-to-apples work-matching (24).

## Architectural thesis

Four propositions:

1. **The tape substrate's write path is the universal floor.** Every grammar, every fixture, every profile points to `push_structural + finalise` as the dominant self-time. The fix is substrate-level: inline `push_structural` into per-rule emit call sites, fuse `finalise` back-patches into compound-close points. Both changes are semantics-preserving; the parity harnesses + generated.rs regen verify correctness. Expected delta: JSON gap 7-8× → 3-4× vs sonic-rs; CSS/Sheets/BBNF absolute throughput +20-40%.

2. **Direct-to-struct must reach emit, or AX's invariant 20/21 is unenforced.** The W1r.1 revelation — that `TypeDesc::Named(sid)` never reaches the Rust backend's `emit_direct_to_struct_projection` — means 18 months of architectural work (AS.2.3 → AU.4.2 → AW.0.5 → AW-III.W6.4 → AW-IV.W3.5a → AX.W1r.1) built a consumer-ready pathway for a declaration (`-> input : <Name>`) the pipeline silently discards. A6 narrows the collapse to two IR-pass hypotheses (egraph cost-guided extraction or alias/transparent stamping); AY empirically discriminates, ships the surgical fix, and gates with a wire-contract test enforcing invariant-14-style closure.

3. **Apples-to-apples comparison requires three lanes, not one.** Current bench matrices compare bbnf parse-only to sonic-rs parse+materialize (A1 §6) — measuring 11.15× bbnf/sonic on twitter via `json_monolithic_value` obscures which half is dispatch vs materialization. A5 proposes three matched lanes: (canonical-serialize, landed W1r.2); (lazy-cursor-to-lazy-cursor via sonic-rs `get_by_path`, new); (eager-to-eager via grammar-emitted `<Grammar>Value` + `parsed.to_value()`, new). Lane-3 clarifies the residual-gap story after W1 closes the substrate loss.

4. **Compile-time urgency halved since doc 06.** W1r.3a's `@pretty` refactor (`933d02fb`..`b930cf2c`) quietly removed a super-linear rustc codepath on CSS L4: doc 06 reported 5.81s wall + 877 MB RSS; A4 measures 1.81s + 636 MB on HEAD `9074a685` — a **69% wall drop + 27% RSS drop**. Compile-time levers remain worthwhile but no longer urgent; AY schedules them after the runtime + correctness waves.

## Invariants

All AX invariants 1-21 carry forward verbatim. AY adds three:

22. **Tape substrate inline.** `tape::columns::Columns::push_structural`, `tape::finaliser::finalise`-equivalent back-patch paths, and `TapeBuilder::push_leaf_with` inline at every emit call site. No cross-crate call-boundary overhead on hot-path record emission. Wave close verifies via `nm` + samply self-time shifting from the `tape` crate into per-rule `parse_<shape>_<grammar>_<rule>` functions.

23. **Named preservation end-to-end.** Every grammar-declared `-> input : <Name>` annotation (where `<Name>` is non-scalar per scalar-name table) reaches the Rust tape emitter as `TypeDesc::Named(sid)`. `emit_direct_to_struct_projection` admits the rule and emits the runtime shim + aggregate payload pathway. Enforced by per-grammar `named_type_preservation.rs` wire-contract test; pipeline-close assertion `for each rule with named projection: ir.types[rule.id] == TypeDesc::Named(<Name>_sid)`.

24. **Value API apples-to-apples.** Bench comparisons vs external comparators are work-matched along one of three lanes: (a) canonical-serialize text equality (W1r.2 template), (b) lazy-cursor-to-lazy-cursor per-field access (bbnf `NodeView::get` vs sonic-rs `get_by_path`), (c) eager-to-eager materialized-tree (bbnf `parsed.to_value::<T>()` vs sonic-rs `from_str::<Value>`). Mixed-work comparisons forbidden in reported headline ratios.

## Operational posture

1. **Bench-checkpoint every wave.** `cargo bench` at wave close, saved to `docs/benchmarks/post-AY-W<N>.json`. Regression ≥ 5% triggers re-plan per AX invariant 10.

2. **Fresh profile-prepare at mid-tranche.** Re-run `scripts/prepare-profile-wave.sh` at W3 close (half-tranche) to re-ground W4+ agent profiles. Stale-profile prevention per `docs/tranches/AX/audit/next-tranche/00-session-recap.md` §5.

3. **Wire-contract tests for every new invariant.** Invariant 22 → `docs/benchmarks/post-AY-W1-close-nm.txt` + samply shift document. Invariant 23 → `crates/core/tests/named_type_preservation.rs`. Invariant 24 → `crates/core/tests/value_api_apples_to_apples.rs`.

4. **Scope-reveal Absorb.** Per SPEC. Wave scope-reveal reopens as sub-wave in place; no letter pivot mid-execution.

5. **No grammar DSL additions.** Invariant 4 preserved. Every change is IR pipeline / tape substrate / codegen / bench-harness.

## Wave summary

Eight waves.

| Wave | Spec | Headline | Opens after |
|------|------|----------|-------------|
| **AY.W0** | [waves/W0.md](waves/W0.md) | Stale-test retirement + ebnf_prettify diagnosis + AX.FINAL (bench + close doc) | tranche open |
| **AY.W1** | [waves/W1.md](waves/W1.md) | Tape substrate inline + finalise fusion (universal hot-path) | W0 |
| **AY.W2** | [waves/W2.md](waves/W2.md) | Named-type preservation + direct-to-struct activation + wire-contract | W1 |
| **AY.W3** | [waves/W3.md](waves/W3.md) | Grammar-emitted `<Grammar>Value` + `parsed.to_value()` + eager bench lane | W2 |
| **AY.W4** | [waves/W4.md](waves/W4.md) | Regex-scan specialisation (byte-class pre-filter + PHF) | W1 |
| **AY.W5** | [waves/W5.md](waves/W5.md) | CSS L4 @import split + DFA hoist + shared PHF (compile A/B/D) | W3 |
| **AY.W6** | [waves/W6.md](waves/W6.md) | parse_that de-generic-ify + ax-iter profile config (compile C/E) | W5 |
| **AY.W7** | [waves/W7.md](waves/W7.md) | FINAL — bench matrix + FINAL.md + AY handoff | W6 |

W4 depends on W1 but not W2/W3 — regex work is independent of Named + Value lanes. All other waves are linearly dependent.

## AY → AY handoff contract

Six conditions must verify clean before AY opens:

1. `cargo test --workspace` green (AX's 5 stale tests retired in W0; ebnf_prettify fixed in W0).
2. `post-AY.json` bench matrix captured (invariant 10).
3. AY-W2 `named_type_preservation.rs` test passes for every grammar with `-> input : <Name>` annotations.
4. AY-W3 eager-lane bench: JSON `bbnf.to_value::<JsonValue>(twitter)` within 3× of `sonic_rs::from_str::<Value>(twitter)` (relaxed target; a 3× eager-to-eager gap is post-tape-fix acceptable floor).
5. `nm` on all 4 prebuilt bench binaries shows zero `__push_structural` / cross-crate tape symbol exports (invariant 22).
6. AY's Y0 substrate design locks against a stable shape emitter — `shape_emitter` + tape columns schema frozen.

## Defensible floor

Per the fresh audits, AY's defensible floor is five items:

1. **W0 stale-test retirement** — 5 files deleted, `cargo test --workspace` unblocked.
2. **W1 tape-inline** — push_structural `#[inline(always)]` on single cross-crate call site; profile shifts.
3. **W2 Named preservation** — single pass guard in metadata.rs or egraph cost function; wire-contract test + `.as_color()` fires on fresh profiles.
4. **W3 `<Grammar>Value` emission** — codegen-only addition; doesn't require W1/W2 success to ship.
5. **W7 FINAL artefacts** — bench matrix + FINAL.md + AY handoff doc.

These five land even if W4/W5/W6 face scope-reveals. Historically (per `docs/tranches/AX/audit/next-tranche/01-prior-tranche-archaeology.md` §1 chronic-debt patterns), W1+W2 are high-confidence substrate landings; W3 is medium; W4/W5/W6 are lever-portfolio items subject to samply-verified attribution pre-admission.

## Post-tranche review candidates

Decision at AY.W7 close, not mid-wave:

- **CSS `calc()` semantic evaluator** (W1r.3a scope-reveal) — if bootstrap/tailwind byte-parity becomes plan-priority, schedule dedicated workstream in AY or a follow-on letter. Not AY scope.
- **Cranelift JIT per-schema** (AY.Y5 per existing AY.md draft) — move to AY as planned.
- **W4 Regex engine Bounded-HIR rewrite** — if W4's byte-class + PHF fails to land 8-15% CSS gain, defer to post-AY with bench-delta-gated decision.

## Indefatigability

When AY closes:

- Tape hot-path inline + finalise fused; no cross-crate call boundary on emit.
- Direct-to-struct projection fires for every grammar-declared `-> input : <Name>`.
- Apples-to-apples bench matrix populated across canonical-serialize + lazy-lazy + eager-eager lanes.
- JSON gap vs sonic-rs cut from 7-8× to 3-4× (substrate fix) + 1.5-2.5× (eager materialization via `to_value`).
- CSS L4 compile-time + cache-size reduction on top of W1r.3a's 69% drop.
- Five stale wire-contract tests retired; ebnf_prettify recognizer green; `cargo test --workspace` clean.
- AX FINAL.md written; AY handoff artefacts ready.
- No hand-coded Value duplicates; no third-party comparator bridges; no substrate-without-consumer landings; no placeholder variants.
- Invariants 22-24 + AX 1-21 all gated by per-wave wire-contract tests and samply attribution.
