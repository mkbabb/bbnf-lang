# BA.W4c — Legacy `parse_with.rs` Deletion

**Thesis** (the four `runtime/{json,bbnf,css_l4,google_sheets}/parse_with.rs` legacy lowering passes — ~480 LOC across four files — DELETE per `audit/CENSUS-2026-05-03.md:262`; the W4b unified `parse_with` surface in `crates/core/src/grammar/generated/<g>.rs` replaces them; per surgery #9, deletion lands AFTER the unified surface, not before). **Closer-gate** (`for f in json bbnf css_l4 google_sheets; do test ! -f crates/core/src/runtime/$f/parse_with.rs; done` — all pass).

## §1 — Deliverable

Hereupon the four legacy `runtime/<g>/parse_with.rs` files retire. Per surgery #9 ("Move `docs/tranches/BA/waves/W3.md:62-67` (parse_with deletion) into BA.W4 before `docs/tranches/BA/waves/W4.md:34-39` (parse_with reintroduction); remove the W3 `test(parse_with)` gate at `:148`"), the deletion sequencing is corrected: W3 does NOT delete; W4b emits the unified surface; W4c deletes the legacy files. The original W3.M5 surgery is repositioned to W4c per surgery #9's mandate.

The four files (per `audit/CENSUS-2026-05-03.md:262`, ~480 LOC total):

| Path | LOC | Purpose | FATE |
|---|---:|---|---|
| `crates/core/src/runtime/json/parse_with.rs` | 133 | Typed→legacy alphabet lowering bridge | DELETE per CENSUS:262 |
| `crates/core/src/runtime/bbnf/parse_with.rs` | 120 | same | DELETE |
| `crates/core/src/runtime/css_l4/parse_with.rs` | 113 | same | DELETE |
| `crates/core/src/runtime/google_sheets/parse_with.rs` | 114 | same | DELETE |

Each file's body manually lowers the typed alphabet (`use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment}; let mut legacy: Vec<LegacySegment<'_>> = Vec::with_capacity(path.len()); for owned in path.owned_segments() { legacy.push(lower(&owned.as_borrowed())?); } doc.get::<T>(LegacyPath::new(&legacy))` per CENSUS:253-256). After W3c retired the legacy `runtime/path.rs`, the manual lowering body's call to `LegacyPath::new(&legacy)` no longer compiles; W3c's M1 milestone rewrote each `parse_with.rs` import from `use crate::runtime::path::` to `use crate::path::ir::` IN PLACE, with the body still calling the manual-lowering helper (now reading from the typed alphabet). W4c deletes the entire file: the manual-lowering body is gone; the unified `parse_with` from W4b replaces the call surface.

The deletion sequencing per surgery #9: W4b emits unified `parse_with` for every grammar at `crates/core/src/grammar/generated/<g>.rs`; consumers' calls to `runtime::<g>::parse_with(input, &path)` redirect to the generated entry. W4c retires the four `runtime/<g>/parse_with.rs` files; the redirect's source code is the unified generated surface, not the manual lowering bridge.

The Era V failure mode is mitigated because W4c's substrate retirement (the four file deletions) has the same-wave consumer of every `runtime::<g>::parse_with` caller — those callers now resolve to the unified generated entry from W4b. Both substrates exist in-wave; W4c is the final retirement step, with the generated surface as same-wave consumer of the deletion's release.

The W3 close `test(parse_with)` gate at line 148 of the previous W3.md is REMOVED per surgery #9; W3c does not gate parse_with tests (those gate at W4b.M3 and W4c.M2).

## §2 — Milestones

> **M0 — Verify W4b unified surface lands per all 9 grammars**
>
> *Surface*: `crates/core/src/grammar/generated/{json,bbnf,css_l4,google_sheets,bnf,csv,ebnf,css_pretty,math}.rs`.
> *Action*: Pre-W4c gate: confirm the W4b-rewritten emitter has produced unified `pub fn parse_with(input, &path)` for all 9 grammars.
> *Gate*: `rg -n 'pub fn parse_with' crates/core/src/grammar/generated/` returns ≥ 9.
> *Exit-criteria*: `rg -n 'pub fn parse_with' /Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/ | wc -l | tr -d '\n'` returns ≥ 9.

> **M1 — Delete the four `runtime/<g>/parse_with.rs` files**
>
> *Surface*: per the §1 table; ~480 LOC total per CENSUS:262.
> *Action*: Delete each file; remove `pub mod parse_with;` declaration from each `runtime/<g>/mod.rs`. Per `audit/CENSUS-2026-05-03.md:262` ("once `runtime/path.rs` is unified, the manual lower vanishes"), the manual `Vec<LegacySegment>::with_capacity` + per-step lowering vanishes. Per `audit/RESTART-SKETCH-2026-05-03.md:213` (the wasted-cycles row 9), the deletion releases the typed-alphabet runtime as the only path-alphabet.
> *Gate*: four files gone; module declarations gone; the consumer pattern is W4b-ready.
> *Exit-criteria*: `for f in json bbnf css_l4 google_sheets; do test ! -f /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/$f/parse_with.rs || echo "MISS:$f"; done | wc -l | tr -d '\n'` returns `0`; `cargo check -p bbnf 2>&1 | rg -c 'error\[' | tr -d '\n'` returns `0`.

> **M2 — Run parse_with test cohort verification**
>
> *Surface*: `crates/core/tests/parse_with_*.rs` (post-W0.M5 inline-test migration; four files: `parse_with_json.rs`, `parse_with_bbnf.rs`, `parse_with_css_l4.rs`, `parse_with_google_sheets.rs`).
> *Action*: Run `cargo nextest run -p bbnf -E 'test(parse_with)' --profile ax-iter`; verify 100% pass-rate. Per surgery #9, the W3 close `test(parse_with)` gate moved to W4 (specifically W4b/W4c); W4c is the final pass-rate verification.
> *Gate*: 100% pass-rate across the parse_with test cohort.
> *Exit-criteria*: `cargo nextest run -p bbnf -E 'test(parse_with)' --profile ax-iter 2>&1 | rg 'test result: ok' | wc -l | tr -d '\n'` returns ≥ 1.

## §3 — Closer gate

```
for f in json bbnf css_l4 google_sheets; do
  test ! -f crates/core/src/runtime/$f/parse_with.rs
done                                                  ; expect: all pass
rg -n 'pub fn parse_with' crates/core/src/grammar/generated/ | wc -l
                                                      ; expect: ≥ 9
cargo nextest run -p bbnf -E 'test(parse_with)'        ; expect: 100% pass
cargo check -p bbnf                                    ; expect: 0 error[ lines
```

## §4 — Invariants

§I1. **Lock 3 close** (cursor + byte-skip unified, one parse impl). The legacy bridge files retire; the unified `parse_with` from W4b is the single source of truth.

§I2. **No backward compat** (per `feedback_no_backward_compat`). The four legacy files delete; no transitional alias survives.

§I3. **No silent epsilon** (per `feedback_no_silent_epsilon`). The W4b unified `parse_with` carries explicit error returns; no silent fallback to legacy lowering survives.

§I4. **System cohesion** (per `feedback_system_cohesion`). The deletion folds into the existing per-grammar `runtime/<g>/` directory structure; the directory continues with `arena.rs`, `builder.rs`, `document.rs`, `mod.rs`, `value.rs`, `view.rs` (per CENSUS:439-507's runtime cohort inventory) — `parse_with.rs` is the retiring file.

§I5. **Generated LOC unchanged** (per BA-G10 + surgery G06-3). W4c does NOT regen any grammar; the four file deletions retire ~480 LOC of source (NOT generated) code.

## §5 — Risks specific to this wave

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| The four legacy `parse_with.rs` files carry behaviour the four production tests at `crates/core/tests/parse_with_*.rs` exercise | Medium | M2 close: `cargo nextest run -p bbnf -E 'test(parse_with)'` | The eight inline tests at W0.M5 already migrate to `crates/core/tests/parse_with_*.rs`; the four legacy `runtime/<g>/parse_with.rs` files are pure lowering bridges with no test surface — their deletion is mechanical |
| Some non-test consumer imports the legacy `runtime::<g>::parse_with` (e.g. analysis, lsp) | Medium | `rg -n 'use bbnf::runtime::.*parse_with' crates/` post-W4c | The consumer rewrites at W4c sub-commit time: `use bbnf::grammar::generated::<g>::parse_with` replaces `use bbnf::runtime::<g>::parse_with` |
| `pub mod parse_with;` declarations linger in `runtime/<g>/mod.rs` after deletion | Low | M1 sub-commit: `cargo check -p bbnf` | M1 includes the module declaration removal in the action; the gate verifies |
| The unified W4b `parse_with` body in `crates/core/src/grammar/generated/<g>.rs` is incomplete for some grammar (CSS L4's specialised builder) | Medium | W4b.M2 already gated this; W4c re-verifies via M2 | W4b.M2 is the per-grammar `Document::get<T>` reroute gate; W4c.M2 re-runs the cohort |

## §6 — Cross-references

- **Honours Lock 3** (final phase) per BA.md §13-Lock honoured row L3.
- **Carry-tags produced**: none direct to BB; W4c's outputs are consumed by W5 (which the unified `parse_with` body for JSON rewrites direct-to-struct).
- **Preceding wave**: BA.W4b (public wrappers).
- **Following wave**: BA.W5 (JSON direct-to-struct codegen first cut).
- **Routed-carry**: none specific to W4c.

## §7 — Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target | Notes |
|---|---|---|---|
| `cargo check --workspace --profile ax-iter` | ≤ 22 s | error count: 0 | Post-W4c check |
| `cargo nextest run -p bbnf -E 'test(parse_with)' --profile ax-iter` | ≤ 18 s | 100% | parse_with test cohort |
| `for f in json bbnf css_l4 google_sheets; do test ! -f crates/core/src/runtime/$f/parse_with.rs; done` | < 1 s | all pass | Deletion verification |

## §8 — Verification artefacts

W4c produces no audit artefact directly; the closer-gate's filesystem checks are the verification surface.

## §9 — Audit lane forecast

Lane 02 sequencing: same-wave consumer (post-W4b unified surface) verifies. Lane 03 cohesion: closes C03-1 (orphan parse_with gate retires). Lane 06 budget: source LOC reduces by ~480; generated unchanged.
