# S-P0 audit-overfit hardening V3 — CH5 HIDDEN-COUPLING (2nd-consecutive confirm, independent re-grep)

Lens (CH5 HIDDEN-COUPLING, V3): does the residual-overfit audit surface EVERY cross-surface
dependency the addenda gates rely on; can any gate read CLEAN only because it is silently coupled
to a leak-EXCLUDING scope; does any finding false-green another; is the PRUNE-sequencing's
revert/entry-gate graph a REAL dependency chain (not an asserted order). Per PASS-0-OVERFIT-AUDIT
§3 + ORCHESTRATOR §3W/§3Z. Subject: `a0`–`a3` + `SYNTHESIS-AUDIT-OVERFIT.md`. Every coupling and
the V1→V2 discharge re-grepped INDEPENDENTLY on disk this pass at live HEAD `83b66db42`.

V3 is the 2nd-consecutive confirm: the V1 CH5 carried ONE REVISE (R1-CH5 — the R16 recipe-pin's
named-nested-struct enumeration was itself a hidden coupling: it inlined ONE nested struct while
`RuntimeTarget` has TWO, and the `PartialEq`-derive cost of the cleanest mechanism was unstated);
V2 DISCHARGED it. V3 re-greps every dispositive witness from scratch to confirm the fold held and
introduced no new coupling.

## Independent disk re-verification (every CH5 load-bearing claim grepped from scratch this pass)

| Claim under test | Disk verdict (HEAD `83b66db42`) | Cite |
|---|---|---|
| `RuntimeTarget` derives ONLY `#[derive(Clone, Copy, Debug)]` (PartialEq genuinely ABSENT → the one-line cost is real, not over-stated) | CONFIRMED verbatim | `xtask/src/regen.rs:5` |
| `RuntimeTarget` has exactly 12 fields; TWO are nested structs | CONFIRMED (counted body-only) | `regen.rs:6-18` |
| `frontend_requirements` = field **#11** (`RuntimeFrontendRequirements`) | CONFIRMED (body-only ordinal count #11) | `regen.rs:17`; struct `grammar_provider.rs:46` |
| `output_labels` = field **#12** (`Option<RuntimeOutputLabels>`) | CONFIRMED (body-only ordinal count #12) | `regen.rs:18`; struct `grammar_provider.rs:92` |
| Both nested structs derive `PartialEq, Eq` (full-row `PartialEq` realization viable) | CONFIRMED — `RuntimeFrontendRequirements` `:45`, `RuntimeOutputLabels` `:91` both `#[derive(Clone, Copy, Debug, PartialEq, Eq)]` | `grammar_provider.rs:45,91` |
| `RuntimeEmitterKind = {CompiledLowering, RequestFacts}` grammar-family fork, dispatched on the `emitter` field | CONFIRMED — `:40` enum, `:41/:42` variants, `:33` field, `:110` live dispatch | `grammar_provider.rs:40-42,33,110` |
| Phantom `<G>` axis (default `AnyGrammar`); `K=AnyKind` is the SEPARATE real axis | CONFIRMED | `tape/mod.rs:175` |
| Lock-14 green-by-exclusion: `runtime_generator.rs` routed to the WEAK root, NOT the strict scan | CONFIRMED — `runtime_generator.rs` is line 2 of `SKV15_W2_EXTRA_COVERAGE_ROOTS:2442`; the strict `GENERIC_SCAN_ROOTS:2409` does NOT contain it; the weak set is chained at `:2511` | `lock14_baseline.rs:2409,2442,2443,2511,2463` |
| Witnesses emit from `runtime/`, NOT the codegen P4 scan root | CONFIRMED — `grammars/json/event_grammar_witness.rs`, `grammars/sheets_witness/event_grammar_witness.rs`, `tape/event_grammar_tests.rs` | `crates/runtime/src/…` |
| P1↔`checkasm_parity.rs` build coupling: 9 ACTIVE compile-coupled `bbnf_simd::x86_64::…::*_scalar(…)` call sites | CONFIRMED verbatim at `:458,:464,:467,:477,:478,:484,:493,:497,:502` | `bbnf-simd/tests/checkasm_parity.rs` |

## R1-CH5 discharge holds in BOTH parts (independently re-confirmed)

The V1 CH5 REVISE was that the R16 recipe-pin coupled the gate's invariant to a named-field list
NARROWER than the invariant itself (one nested struct of two), and under-stated the `PartialEq`
cost. Both halves are DISCHARGED on disk and re-confirmed this pass:

- **(i) The recipe pin enumerates BOTH nested-struct fields at the invariant's altitude.** All
  four surfaces now recurse into the FULLY-EXPANDED row: `a3:169-208` ("MUST recurse into EVERY
  nested-struct field, not just `output_labels` … BOTH `frontend_requirements` (field #11) AND
  `output_labels` (field #12)"); `a2:382-400` (§4a, "the config-tuple must be the FULLY-EXPANDED
  row … both nested structs, not one"); `a0:144-157` (§1-L2) + `a0:275-285` (§2.4);
  `SYNTHESIS:209-221` (§5 fact 5). No surviving surface recurses into one nested struct only — the
  named-field coupling is now matched to its own invariant altitude.
- **(ii) The `PartialEq`-derive cost is stated AND disk-true.** `RuntimeTarget` derives only
  `#[derive(Clone, Copy, Debug)]` (`regen.rs:5`, re-grepped this pass — `PartialEq` is genuinely
  absent), so the full-row mechanism's one-line cost is REAL, not a free claim. Both nested structs
  already derive `PartialEq, Eq` (`grammar_provider.rs:45/:91`) so the derive is viable; stated at
  `a3:189-195`, `a2:393-400`, `a0:153-156`, `SYNTHESIS:217-221`. The full-row derive is the
  PREFERRED realization precisely because it covers both nested structs automatically and CANNOT be
  coupled to a hand-rolled field list — i.e. the discharge mechanism itself eliminates the coupling
  it remediates.

**The false-green this polices is LIVE on disk (re-grepped this pass).** `frontend_requirements ==
REQUEST_FACTS_REQUIREMENTS` is UNIFORM across all 7 css_l4 rows (`regen_css.rs:47,65,83,101,119,137,155`)
while `output_labels.fact_schema` is 7 DISTINCT values (`css-l4-at-rules-media-facts-v1` …
`css-l4-visual-function-facts-v1`). This is the EXACT displaced-one-field-over hazard: a recipe
recursing into `output_labels` only would catch today's distinctness but MISS a future relocated
seam riding the currently-uniform `frontend_requirements`. The both-nested-struct / full-row
`PartialEq` recipe is the correct policing altitude — confirmed not theoretical but disk-grounded.

## Disposition ledger (each cross-surface coupling, re-verified)

- **(1) Lock-14 green-by-exclusion coupling — A3/R9/P4 (ACCEPT).** The canonical hidden coupling:
  the gate reads CLEAN only because the strict `GENERIC_SCAN_ROOTS:2409` is coupled to a
  leak-EXCLUDING root set — `runtime_generator.rs` rides the WEAK `SKV15_W2_EXTRA_COVERAGE_ROOTS`
  (`:2442`, confirmed at `:2443` line 2 of the array; chained at `:2511`), the x86 tree tagged
  `"diagnostic-x86"` (`:2463`). Surfaced (a0 §3 / a2 §1 P4 / a3 §2 / SYNTHESIS §3-A3 + §4 P4) AND
  de-coupled (move the leak files into the strict root; extend `FORBIDDEN_GENERIC_TOKENS:2420` with
  `CSS_`/`_RS`/`EventGrammar`; drop the x86 exclusion) AND a meaningfulness falsifier (re-inject a
  `JsonSink` token → gate RED, then revert; `lock14_gate_scans_codegen == true`). Sequencing
  obligation bound: P4 BEFORE the G2/G3 rebuild. No hidden coupling left un-policed.
- **(2) P4-before-rebuild sequencing coupling (ACCEPT).** SYNTHESIS §5 fact 2 / a2 §2 (1c) / a0 §7
  surface that the gate's TRUSTWORTHINESS is coupled to its landing ORDER, not its content — P4
  must land before the un-forked emitter is authored, or G2/G3 could re-leak a grammar-named branch
  under a blind gate. Bound as an entry-gate ("not a preference," a2 §2). Correct.
- **(3) S-P1-profile cross-pass coupling for G6 (ACCEPT).** SYNTHESIS §5 fact 4 / a0 §1-L6 / a1 §L6
  surface that the G6 NEON RETIRE branch is coupled to an S-P1 samply non-top-N MEASUREMENT — a
  cross-pass dependency that, if missed, would let G6 close by assertion. Bound profile-FIRST; the
  kernel-with-its-consumer-in-one-commit clause closes the inverse (orphan-kernel) coupling.
- **(4) No-second-substrate coupling — Lock 1 / G4 (ACCEPT).** a2 §5 (4c) / SYNTHESIS CLEAN-list /
  a0 §8: G3/G4 emit over the EXISTING `Tape`/`ValueRef`; an introduced
  `StructLayout`/`TapeStructBuilder`/`TapeCursor` is a Lock-1 type-ambivalence coupling (REJECT).
  The shared trait's existence is held INDEPENDENT of the `<G>` phantom (deleting `<G>` and defining
  the trait are separable, a2 §5/4a) — severing a coupling that would otherwise "manufacture the
  very phantom being deleted." Correct.
- **(5) R16 recipe-pin / P3↔G3 PRUNE-binds-GENERALIZE coupling + relocated-seam structural gate
  (ACCEPT, now full-row).** a2 §4/§4a / a0 §5 / SYNTHESIS §5 fact 3 surface the deepest non-obvious
  coupling: P3 (a PRUNE item) is NOT independent of G3 (a GENERALIZE item) — the
  `runtime_target_rows_collapsed` structural check is the ONLY mechanism that catches a per-grammar
  branch RELOCATED into a neutral-identifier `RuntimeTarget` data-table (the arm-census regex is
  syntactically incapable). The R1-CH5 fold WIDENED the structural recipe to the full row (both
  nested structs) — the relocated-seam coupling is now policed at the full-row altitude, closing the
  one-field-over displacement the V1 lens identified. A seam riding `frontend_requirements` can no
  longer slip a one-nested-struct recipe. Correct, and now full-row.

## Hidden-coupling sweep — couplings the audit did NOT miss (affirmative, re-confirmed)

- **Witness-emission scan-root coupling.** `JsonEventGrammar`/`SheetsEventGrammar` witnesses live
  in `runtime/` (disk-confirmed: `grammars/{json,sheets_witness}/event_grammar_witness.rs`), NOT the
  P4 codegen scan root. IF the un-forked generator emits a grammar-named `EventGrammar` literal it
  must be caught AT ITS EMIT SITE — surfaced as SYNTHESIS §2.1 obligation 2 + a1 §L3 + the
  `FORBIDDEN_GENERIC_TOKENS` `EventGrammar`/`*EventGrammar` extension (a2 §1 P4). The emit-site/
  scan-root mismatch is named and the extension is load-bearing, not decorative. NOT a residual.
- **Build-soundness coupling P1↔`checkasm_parity.rs`.** Deleting `src/x86_64/` breaks the build via
  9 active compile-coupled `bbnf_simd::x86_64::…::*_scalar(…)` call sites — re-grepped verbatim this
  pass at `:458,:464,:467,:477,:478,:484,:493,:497,:502`. Same-commit decouple-or-delete, exit gate
  `cargo test --no-run` clean — no intermediate broken-build state. Surfaced a2 §3 / a3 §2 / a0 §7
  P1. NOT a residual.
- **JSON-richness↔`<G>`-resolution coupling.** `json_rich_navigation_preserved == true` is a
  SEPARATE checked condition a ≥2 impl-count does NOT imply (a2 §5/4b, a0 §1-L4, SYNTHESIS §1
  hardening). The coupling that a phantom "resolution" could LCD-flatten JSON's `get(key)`/typed-
  `Kind`/visitor is severed (preserve-rich-ast). NOT a residual.

## REVISE (0) / REJECT (0)

The single V1 CH5 REVISE (R1-CH5) is DISCHARGED in BOTH parts and re-confirmed at every surface the
V1 lens named (a3 §3, a2 §4a, a0 §1-L2 + §2.4, SYNTHESIS §5 fact 5): the recipe pin enumerates BOTH
`frontend_requirements` (#11) AND `output_labels` (#12) at the full-expanded-row invariant altitude,
and the one-line `RuntimeTarget: PartialEq` derive cost is disk-verified (`regen.rs:5` carries only
`Clone, Copy, Debug`) and stated as the pin's preferred realization. The false-green it polices is
disk-LIVE (uniform `frontend_requirements`, 7-distinct `output_labels`). Every cross-surface
dependency the addenda gates rely on is surfaced and de-coupled or entry-gated; zero gate reads
CLEAN by a coupling left un-policed; zero finding false-greens another; the PRUNE-sequencing
revert/entry-gate graph is a real dependency chain (P4-before-rebuild, G1→G2/G3/G4/PROVE,
G3→PROVE, dual G2-entry on G1∧P3). No new hidden coupling introduced by the V2 fold. Zero orphan
REVISE; the V2+V3 pair is 100%×2 consecutive on CH5.

## Tally

ACCEPT 6 · REVISE 0 · REJECT 0 — **100%**. R1-CH5 DISCHARGED (recipe pin enumerates BOTH nested
structs at the invariant altitude; the one-line `RuntimeTarget: PartialEq` derive cost disk-verified
and stated; the policed false-green is disk-LIVE); the relocated-seam structural gate is policed at
the full-row altitude; every hidden coupling surfaced and de-coupled or entry-gated; no new coupling
from the fold.

TALLY accept=6 revise=0 reject=0
