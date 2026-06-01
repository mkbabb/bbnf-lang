---
lens: CH5
name: HIDDEN COUPLING
pass: T-P1-excavation
cycle: V2 (re-review of stale V1 verdict against V5-SKV18-totality inventories)
disposition: REVISE
input_commit: 4e4aa064835b0bf8f7e25113edb40f3a9e01b866
generated_at: 2026-06-01
prior_verdict_superseded: V2/CH5.md (2026-05-28, input_commit 2fcbc1dc8) — all-ACCEPT, citations now stale (147 commits behind HEAD)
files_audited:
  - restart/audit/totality/p1/1A-substrate-evidence.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1B-codegen-evidence.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1C-runtime-evidence.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1D-skinny-lessons.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1E-locks-evidence.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1F-coherence-scan.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1F-anti-pattern.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1F-past-corpora.md (cycle V5-SKV18)
spec_surfaces: [restart/ARCHITECTURE.md, restart/MASTER-PLAN.md, restart/locks/LOCKS.md]
live_spot_checks: 9 path:line rows re-grounded at HEAD with sed/rg
---

# CH5 Hidden Coupling — V2 (cycle V2 re-review)

## Verdict

REVISE.

LENS: CH5 HIDDEN-COUPLING. The substrate inventory (1A) must honour the Lock-1
union — no catalogued state may imply a parallel substrate, sidecar producer, or
renamed scanner; 1F must catch the live couplings; any Track-1≡Track-2 dishonesty
must surface.

Two facts dominate this re-review:

1. **The LIVE inventories (1A/1B/1C/1D/1E/1F at cycle V5-SKV18-totality) honour the
   CH5 lens correctly.** Spot-verified at HEAD (`4e4aa064`): the skinny substrate
   carries NO retained sidecar (`json/generated.rs:12-15 attach_structural_index`
   is a NO-OP `let _ = state;`); the ONE live retained probe — the totality
   `OnceCell<StructuralIndex>` on `ScanState` (`crates/core/src/grammar/generated/json.rs:701`)
   — is correctly catalogued by 1F-anti-pattern, 1E:158 and 1F COH18-015 as an
   *admissible per-parse `generated_function`* carry, NOT closed as clean. The
   scanner asymmetry (skinny `bbnf-simd` has ZERO `next_structural_at_or_after`;
   totality `crates/simd-scan` exports the retained-index probe) is caught
   (COH18-015). The relocated-seam (grammar-named ident table in the generic `ir`
   crate, `crates/ir/src/registry/strategy.rs:137-155`) is caught (COH18-005).
   Track-2 second-scanner/structural-stream temptations are recorded as REJECTED
   lessons (1D:155, Item 53). This live coverage is strong.

2. **The PRIOR V1 verdict in this very file (V2/CH5.md, 2026-05-28) is stale and
   paper-close.** It is an all-ACCEPT wave whose `input_commit` (`2fcbc1dc8`) is
   147 commits behind HEAD. Every CH5-V2-0x evidence cite lands on superseded
   content: e.g. CH5-V2-01 cites `1D:102,147,171,179` for a "CSS 24-row
   broadcast" that no longer exists in 1D (those lines now hold G6 profile,
   `PartialEq` derive, Lock-14 separation, the JSON guard baseline); `1E:92,106,
   131,151` now hold L12-archive / RuntimeEmitterKind-fork / LAC-1E-V5-06; the
   "1F-coherence:98" CSS-broadcast cite now lands on COH18-009 (x86). The V1
   verdict also misses the live CH5 surfaces (COH18-015 scanner asymmetry, the
   totality OnceCell carry, COH18-005 relocated-seam). An all-ACCEPT verdict on
   inventories it cannot have read at their current revision is itself the
   paper-close the cycle-V1 mandate names.

Net: the inventories are right; the carried-forward verdict is wrong. REVISE.

## Findings

| id | disposition | check | live evidence (spot-verified at HEAD) | correction / required action |
|---|---|---|---|---|
| CH5-V2-01 | ACCEPT | Skinny substrate carries NO retained structural sidecar; the scan plane is `local_temp_only`, no cross-call carry. | `json/generated.rs:12-15` `attach_structural_index` is `debug_assert_eq!(...)` then `let _ = state;` (verified verbatim); `json/scan.rs:22 scan_structurals`→`:47 structural_capacity_for` is the transient consumer. 1A-SUB-017 graded `impl_exceeds_spec`, directional conclusion HOLDS. | None for CH5. |
| CH5-V2-02 | ACCEPT | The one live retained probe (`OnceCell<StructuralIndex>`) is catalogued, NOT paper-closed, and correctly classified as admissible per-parse `generated_function`, not the REJECT cross-call class. | `crates/core/src/grammar/generated/json.rs:701` `structural_index: OnceCell<simd_scan::StructuralIndex>` on `ScanState`; `ScanState::new()` per parse at `json.rs:3442`; emitter self-diction "The probe substrate (OnceCell + helper)" at `.../dispatcher/support.rs:67` (verified verbatim). 1F-anti-pattern:43, 1E:158 carry it. | None for CH5. |
| CH5-V2-03 | ACCEPT | Scanner asymmetry / renamed-parallel-scanner check is surfaced: totality `crates/simd-scan` exports a retained-index probe API the skinny scanner lacks. | `crates/simd-scan/src/lib.rs:68 pub use index::{StructuralIndex, next_structural_at_or_after}`; `index.rs:74` impl; `rg next_structural_at_or_after skinny/crates/bbnf-simd/src` = 0 (verified empty). COH18-015 names the SK-V19 scanner-unification decision. | None for CH5. |
| CH5-V2-04 | ACCEPT | Track-1≡Track-2 dishonesty surfaces: the generator FORKS by grammar family despite the spec's single-generator claim, and the relocated-seam grammar-named table lives in the generic `ir` crate. | `grammar_provider.rs:40-42 RuntimeEmitterKind{CompiledLowering,RequestFacts}` (verified); `crates/ir/src/registry/strategy.rs:137-155` grammar-named `idents:&["JsonParser","JsonGrammar"]...` (verified). 1B Lock-5 row + COH18-003/004/005. | None for CH5. |
| CH5-V2-05 | ACCEPT | Lock-1 union holds at the runtime substrate (one `Tape`/`ValueRef`, no second cursor); divergence is codegen provenance, not substrate. | 1C-C11 grounds `CSS_GENERATED_RS` routing into the EXISTING offset tape (`use crate::tape::{OffsetFlags,Tape,TapeBuilder,ValueRef}`); 1A SPINE confirms CSS "Holds exactly the existing Tape — no second substrate" at `css_l4_declaration_values/generated.rs:257`. | None for CH5. |
| CH5-V2-06 | REVISE | The carried-forward V1 verdict (this file) cites `1D:102,147,171,179`; `1E:92,106,131,151`; `1F-coherence:98,131` for a "CSS 24-row broadcast." | At HEAD those exact lines hold unrelated content (G6 profile / PartialEq derive / Lock-14 separation / JSON guard baseline; L12-archive / RuntimeEmitterKind fork / LAC-1E-V5-06; COH18-009 x86). The cited finding is gone from the rewritten inventories. | File: `restart/audit/totality/p1/hardening/V2/CH5.md`. Correction: the V1 CH5-V2-01..08 citation block is dangling against the V5-SKV18 inventories; re-anchor CH5 coverage on the live rows (1F-anti-pattern:43 OnceCell, COH18-015, COH18-005, 1E:158, 1A-SUB-017) per this table. (This re-review IS that re-anchoring.) |
| CH5-V2-07 | REVISE | The carried-forward V1 verdict disposition is a blanket ACCEPT ("No CH5 hidden-coupling REVISE remains") on inventories at a 147-commit-stale `input_commit` (`2fcbc1dc8`). | The live inventories carry an OPEN, unclassified hidden-coupling burden: the totality `OnceCell<StructuralIndex>` probe + the `crates/simd-scan` `next_structural_at_or_after` API are explicitly "must be classified at SK-V19 adoption … do NOT close substrate-union 'BOTH trees' while this is unclassified" (1F-anti-pattern:43, 1E:158). A blanket ACCEPT contradicts the inventory's own open carry. | File: `restart/audit/totality/p1/hardening/V2/CH5.md`. Correction: disposition cannot be unqualified ACCEPT while the totality-tree retained-probe substrate is an unclassified SK-V19 reconcile burden. CH5 status for the *totality* half is CONDITIONAL-OPEN, not closed; only the *skinny* half is clean. |
| CH5-V2-08 | REVISE | The carried-forward V1 verdict frames the OnceCell as a generic "root structural-index sidecar" without the per-parse-vs-cross-call lifetime distinction that decides admissibility. | The live 1F-anti-pattern:43 + 1E:158 establish the load-bearing distinction: `retention_lifetime = generated_function` (per-parse `&mut ScanState`, NOT cross-call → ADMISSIBLE class, NOT the REJECT `retained-across-call-boundary` class at `LOCKS.md:139-149`). The undifferentiated V1 phrasing under- and over-states the risk simultaneously. | File: `restart/audit/totality/p1/hardening/V2/CH5.md`. Correction: replace the bare "sidecar" phrasing with the lifetime-classified statement — admissible per-parse probe scratch in the totality tree, NOT a proven Lock-1 violation, but an SK-V19 classification carry (mirror 1F-anti-pattern:43). |
| CH5-V2-09 | REJECT | False-precision cite recalled by the prior cycle: `Tape::id` at `:172`. | Disk truth at HEAD: `pub fn id(&self) -> TapeId` is at `skinny/crates/runtime/src/tape/mod.rs:170` (verified by sed). 1A:124 already flags "the prior cycle's `:172` re-cite is contradicted by disk." | The `:172` cite is a recalled/false line and is correctly overwritten by 1A-SUB at `:170`. Any verdict still resting on `:172` must drop it. |

## Spot-Verification Log (path:line re-grounded at HEAD 4e4aa064)

- `skinny/crates/runtime/src/tape/mod.rs:94` `Tape<'input>{source,offsets,flag_cursors,flag_values,...}` — CONFIRMED.
- `skinny/crates/runtime/src/tape/mod.rs:170` `pub fn id(&self)->TapeId` — CONFIRMED (not `:172`).
- `skinny/crates/runtime/src/tape/mod.rs:175,179` `ValueRef<'doc,'input,K,G:EventGrammar>` + `_grammar:PhantomData` — CONFIRMED.
- `skinny/crates/runtime/src/grammars/json/generated.rs:12-15` `attach_structural_index` NO-OP `let _ = state;` — CONFIRMED (no retained sidecar).
- `skinny/crates/runtime/src/grammars/json/scan.rs:22,47` transient `scan_structurals`/`structural_capacity_for` — CONFIRMED.
- `crates/core/src/grammar/generated/json.rs:701` `OnceCell<simd_scan::StructuralIndex>` on `ScanState`; `:3442` `ScanState::new()` per-parse — CONFIRMED (per-parse local, lazily filled once).
- `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:67` "The probe substrate (OnceCell + helper) emits for any non-empty alphabet" — CONFIRMED verbatim.
- `crates/simd-scan/src/lib.rs:68` exports `next_structural_at_or_after`; `rg` over `skinny/crates/bbnf-simd/src` = 0 — CONFIRMED asymmetry.
- `crates/ir/src/registry/strategy.rs:137-155` grammar-named `idents:&["JsonParser","JsonGrammar"]` etc. — CONFIRMED relocated-seam.
- `skinny/crates/codegen/src/grammar_provider.rs:40-42` `RuntimeEmitterKind{CompiledLowering,RequestFacts}` fork — CONFIRMED.
- `skinny/crates/codegen/src/runtime_generator.rs:701` `const CSS_GENERATED_RS: &str = r#"..."#` courier; `:91` `normalize(CSS_GENERATED_RS)` — CONFIRMED.

## Notes

- Static audit only. No source edits, builds, tests, staging, or commits.
- The REVISE rows (CH5-V2-06/07/08) target the PRIOR V1 verdict carried in this
  file, not the live inventories — the inventories' CH5 coverage is sound and
  ACCEPT-ed where spot-verified (CH5-V2-01..05). CH5-V2-09 REJECTs a recalled
  `:172` line already corrected on disk.
- Substantive CH5 residual for downstream waves: the totality-tree retained
  `OnceCell<StructuralIndex>` probe + the `crates/simd-scan` retained-index API
  are an UNCLASSIFIED SK-V19 substrate-union reconcile burden. Do not close the
  "BOTH trees honour Lock 1" claim until each totality `OnceCell<StructuralIndex>
  | ensure_structural_index | scan_structural | next_structural_at_or_after` hit
  is classified per `LOCKS.md:139-149`.

TALLY accept=5 revise=3 reject=1
