---
lens: CH5
name: HIDDEN COUPLING
pass: T-P1-excavation
cycle: V3 (SK-V18, cycle V3 re-review against V5-SKV18-totality inventories)
disposition: REVISE
input_commit_tree: dirty (HEAD per 1E live_truth_method = 4e4aa0648)
generated_at: 2026-06-01
prior_in_cycle: V1/CH5.md REVISE, V2/CH5.md REVISE (accept=5 revise=3 reject=1)
files_audited:
  - restart/audit/totality/p1/hardening/V3/CHALLENGE-CONTEXT.md
  - restart/audit/totality/p1/1A-substrate-evidence.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1B-codegen-evidence.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1C-runtime-evidence.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1D-skinny-lessons.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1E-locks-evidence.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1F-coherence-scan.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1F-anti-pattern.md (cycle V5-SKV18)
  - restart/audit/totality/p1/1F-past-corpora.md (cycle V5-SKV18)
spec_surfaces: [restart/ARCHITECTURE.md, restart/MASTER-PLAN.md, restart/locks/LOCKS.md]
live_spot_checks: 14 path:line rows re-grounded at HEAD with sed/rg/awk/find
score: "4/8 ACCEPT, 3/8 REVISE, 1/8 REJECT"
---

# CH5 Hidden Coupling — SK-V18 T-P1 V3

## Verdict

REVISE.

LENS: CH5 HIDDEN-COUPLING. The substrate inventory (1A) must honour the Lock-1
union — no catalogued state may imply a parallel substrate, sidecar producer, or
renamed scanner; 1F must catch the live couplings; any Track-1≡Track-2
dishonesty must surface.

The live V5-SKV18 inventories honour the CH5 lens at their CORE: the skinny
substrate carries NO retained structural sidecar, the CSS runtime routes into the
EXISTING tape with no second substrate, the scanner asymmetry is caught, and the
relocated-seam grammar-named ident table is caught. These were spot-verified
verbatim at HEAD and ACCEPT.

But close reading of the most load-bearing hidden-coupling rows turns up three
genuine corrections and one false dispatch premise:

1. The totality `OnceCell<StructuralIndex>` probe census **overstates its breadth
   by one grammar** — it is emitted into 8 of 9, not "all 9 / the 9 generated
   grammars" (`math.rs` carries only an inert `ScanState` shell, no live
   `ensure_structural_index`). The breadth figure of the single most important
   retained-probe coupling row is wrong on disk.

2. The FNV production coupling is **mis-framed as bench-only**. `input_fnv64` /
   `fnv64` is computed by the PRODUCTION `emit_full_parse` on the LIVE Track-1
   benched CSS recognition path (the very path that produces the 1.9–3.3×
   numbers), not "bench-only metadata" as 1D G-5 asserts; and the `1F-anti-pattern`
   fence cites a "prior-cycle V4 transcript" rather than the current production
   line numbers.

3. The V3 CHALLENGE-CONTEXT routes the two heaviest hidden-coupling fences
   (OnceCell lifetime, FNV hash-sidecar) to `1F-coherence-scan.md`, but the live
   carrier of both is `1F-anti-pattern.md:41,:43` — the file the same context
   declares "historical and superseded."

4. (REJECT) That same "historical and superseded / not live authority" claim
   about the 1F auxiliaries is FALSE against disk: `1F-anti-pattern.md` was
   regenerated this pass at `cycle: V5-SKV18-totality` and is the SOLE carrier of
   the load-bearing OnceCell Lock-1 classification that the indisputably-live
   `1E-locks-evidence.md:158` explicitly cross-references.

The core CH5 coverage is sound; the breadth count, the FNV framing, and the
auxiliary-authority routing need correction. Hence REVISE.

## Spot-Verification Log (path:line re-grounded at HEAD, dirty tree)

- `skinny/crates/runtime/src/grammars/json/generated.rs:12-15` `attach_structural_index`
  is `debug_assert_eq!(config::STRUCTURAL_BYTES, b"{}[],:\"")` then `let _ = state;`
  — CONFIRMED no retained sidecar.
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:257`
  `CssDocument` doc-comment "Holds exactly the existing `Tape` — no second
  substrate"; `:258` `pub struct CssDocument<'input> { tape: Tape<'input> }` —
  CONFIRMED verbatim (1A SPINE / 1C C11).
- `skinny/crates/codegen/src/runtime_generator.rs:702` (inside `CSS_GENERATED_RS`
  const) `use crate::tape::{OffsetFlags, Tape, TapeBuilder, ValueRef};` — CONFIRMED
  (CSS courier reuses the one tape).
- `crates/core/src/grammar/generated/json.rs:701` `structural_index:
  ::core::cell::OnceCell<::simd_scan::StructuralIndex>` on `ScanState`; `:711`
  `OnceCell::new()` per `ScanState::new`; `:719` `fn ensure_structural_index`;
  `:732` `::simd_scan::scan_structural(input, &alphabet)` — CONFIRMED (per-parse,
  lazy, not cross-call).
- Per-grammar `OnceCell<StructuralIndex>` field + `ensure_structural_index`
  census: bbnf/bnf/css_l4/css_pretty/csv/ebnf/google_sheets/json = 1 each;
  **math = 0** (`rg -c 'fn ensure_structural_index' crates/core/src/grammar/generated/math.rs`
  = 0; `math.rs:277-291` carries only a documented `ScanState` shell, never the
  populated field). Live probe breadth = **8/9**, not 9/9.
- `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:67` "The
  probe substrate (OnceCell + helper) emits for any non-empty alphabet"; the
  gate `ctns_probe_admits` (`:74-95`) returns false for empty/whitespace
  alphabets and admits only `12 <= len <= 24` — CONFIRMED conditional emission
  (math is gated out).
- `crates/simd-scan/src/lib.rs:68` `pub use index::{StructuralIndex,
  next_structural_at_or_after}`; `index.rs:74`/`:101` two `next_structural_at_or_after`
  defs; `rg -c next_structural_at_or_after|scan_structural skinny/crates/bbnf-simd/src`
  = 0 (no output, exit 1) — CONFIRMED scanner asymmetry (COH18-015).
- `crates/ir/src/registry/strategy.rs:137,143,149,155,...,185` — nine grammar-named
  `idents` rows in the generic `ir` crate — CONFIRMED relocated-seam (COH18-005).
- FNV PRODUCTION surface: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:393`
  `out.push_str("source\tinput_fnv64=")`, `:394` `push_hex64(&mut out,
  fnv64(input.as_bytes()))`, `:899` `fn fnv64`, emitted inside `emit_full_parse`
  (`:380`, "Diagnostic-only roll-up ... from the same tape"); `rg -lc input_fnv64
  css_l4_*/generated.rs` = **7 files**; template `skinny/crates/codegen/src/runtime_generator.rs:1093`
  + `:1599 fn fnv64` — CONFIRMED live in all 7 replicas + the generator template.
- FNV CALLER: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/parser.rs:42`
  `generated::emit_full_parse(input)` (production parser path), and
  `skinny/crates/bbnf-bench/src/bin/css_cold_harness.rs:130` `track1_full`
  = `emit_full_parse` (the MEASURED Track-1 recognition plane) — CONFIRMED NOT
  bench-quarantine-only.
- `1F-anti-pattern.md:4` `cycle: V5-SKV18-totality`; `:41` Generated CSS FNV row;
  `:43` Totality OnceCell row — CONFIRMED the auxiliary is a current-pass
  rewrite, not a superseded stub.
- `restart/audit/totality/p1/1E-locks-evidence.md:158` "cross-ref 1F-anti-pattern
  OnceCell row" — CONFIRMED the live 1E depends on the auxiliary.

## Findings

| id | disposition | finding | evidence (spot-verified at HEAD) | required fold |
|---|---|---|---|---|
| CH5-V3-001 | ACCEPT | Skinny substrate carries NO retained structural sidecar; the scan plane is `local_temp_only`, no cross-call carry. | `json/generated.rs:12-15` `attach_structural_index` NO-OP `let _ = state;` (verbatim); 1A-SUB-017 graded `impl_exceeds_spec`, directional conclusion holds; `scan.rs:22`→`:47` transient consumer. | None. |
| CH5-V3-002 | ACCEPT | Lock-1 union holds at the runtime substrate: one `Tape`/`ValueRef`, no second cursor; CSS routes into the EXISTING tape. The divergence is codegen provenance, not substrate. | `css_l4_declaration_values/generated.rs:257-259` "Holds exactly the existing `Tape` — no second substrate" + `CssDocument{ tape: Tape<'input> }`; `runtime_generator.rs:702` courier body `use crate::tape::{OffsetFlags,Tape,TapeBuilder,ValueRef}`; 1C-C11, 1A SPINE. | None. |
| CH5-V3-003 | REVISE | The totality `OnceCell<StructuralIndex>` probe census OVERSTATES breadth: `1F-anti-pattern.md:43` ("emitted into all 9 generated grammars") and `1E-locks-evidence.md:158` ("emitted into the 9 generated grammars") assert 9/9, but the live probe (populated `structural_index` field + `ensure_structural_index`) is in 8/9 — `math.rs` carries only a documented-but-inert `ScanState` shell (`math.rs:277-291`, 0 `ensure_structural_index`), gated out by `ctns_probe_admits` (`support.rs:74-95`, `12<=len<=24` non-whitespace alphabet). | `rg -c 'fn ensure_structural_index' crates/core/src/grammar/generated/{bbnf,bnf,css_l4,css_pretty,csv,ebnf,google_sheets,json}.rs` = 1 each; `math.rs` = 0; `OnceCell<…StructuralIndex` field present 8 grammars, absent in math. | Files: `1F-anti-pattern.md:43` and `1E-locks-evidence.md:158`. Correction: change "all 9 / the 9 generated grammars" to "8 of 9 (all but `math`, whose structural alphabet falls outside the `ctns_probe_admits` 12–24-byte window)". The `generated_function` per-parse classification is unchanged; only the breadth count is wrong. |
| CH5-V3-004 | ACCEPT | Scanner asymmetry / renamed-parallel-scanner check is surfaced and correctly fenced: totality `crates/simd-scan` exports a retained-index probe API the skinny `bbnf-simd` lacks; SK-V19 must unify or admit a renamed parallel scanner. | `crates/simd-scan/src/lib.rs:68` exports `next_structural_at_or_after`; `index.rs:74`/`:101`; `rg next_structural_at_or_after|scan_structural skinny/crates/bbnf-simd/src` = 0 (verified empty); COH18-015. | None. |
| CH5-V3-005 | REVISE | FNV production coupling is MIS-FRAMED as bench-only. `1D-skinny-lessons.md:196` (G-5) states "FNV … are bench-only metadata, never a runtime equality arbiter (SK-V15 W10 quarantine)" and `1D:36` lists "FNV bench-quarantine" in the proved floor — but `input_fnv64`/`fnv64` is computed by the PRODUCTION runtime `emit_full_parse` (`css_l4_declaration_values/generated.rs:393-394`, `:899`), CALLED by the production parser (`parser.rs:42 generated::emit_full_parse`) and by the MEASURED Track-1 plane (`css_cold_harness.rs:130 track1_full`). It is non-equality telemetry (correct), but NOT bench-quarantined. Separately, the `1F-anti-pattern.md:41` fence (the one inventory that does catalogue it) cites "prior-cycle V4 transcript" as basis, not the current production path:line. | Production: 7 css_l4 `generated.rs` carry `input_fnv64` + `fn fnv64`; template `runtime_generator.rs:1093,:1599`. Caller: `parser.rs:42`, `css_cold_harness.rs:130`. Inventory: `1F-anti-pattern.md:41` "prior-cycle V4 transcript"; `1D:196` G-5 "bench-only". | Files: `1D-skinny-lessons.md:196` (G-5) and `1F-anti-pattern.md:41`. Correction: (a) split G-5 — the BENCH-side FNV quarantine (`fnv_quarantine.rs`) is clean/KEEP, but the PRODUCTION `emit_full_parse` `input_fnv64` is live telemetry-output on the Track-1 path, fenced as non-equality / non-substrate / non-document-identity, NOT "bench-only"; (b) re-anchor `1F-anti-pattern.md:41` evidence on the current production path:line (`generated.rs:393,:394,:899`; template `:1093,:1599`; caller `parser.rs:42`) instead of the V4 transcript. |
| CH5-V3-006 | REVISE | The V3 CHALLENGE-CONTEXT (`§1`, `§2`) routes the OnceCell and FNV/source-sidecar hidden-coupling fences to `1F-coherence-scan.md`, and treats the 1F auxiliaries as out-of-scope. But on disk the OnceCell Lock-1 lifetime classification AND the FNV hash-sidecar fence live ONLY in `1F-anti-pattern.md:43`/`:41`; `1F-coherence-scan.md` carries the scanner asymmetry (COH18-015) and relocated-seam (COH18-005) but NOT the OnceCell-lifetime or FNV classifications. Routing the two heaviest hidden-coupling fences to a file that does not hold them under-states the live coverage and risks an audit-close that never re-reads the actual carrier. | `1F-coherence-scan.md` grep: no `OnceCell`-lifetime row, no `fnv` row (only COH18-015 scanner + COH18-005 seam); `1F-anti-pattern.md:41,:43` hold both; `1E:158` cross-refs `1F-anti-pattern`. | File: `restart/audit/totality/p1/hardening/V3/CHALLENGE-CONTEXT.md` (and the V3 fold roster). Correction: name `1F-anti-pattern.md:41,:43` as the live carrier of the FNV hash-sidecar and OnceCell-lifetime fences, OR fold those two rows into `1F-coherence-scan.md` so the "live six" set is self-contained; do not route a fence to a file that lacks it. |
| CH5-V3-007 | ACCEPT | The relocated-seam (a per-grammar branch moved into a neutral-identifier data table) is caught: a 9-grammar-named `idents` table in the generic `ir` crate, invisible to arm-grep, caught only by structural row-collapse. | `crates/ir/src/registry/strategy.rs:137,143,149,155,161,167,173,179,185` (`JsonParser`/`GoogleSheetsParser`/`CssL4Parser`/… verbatim); COH18-005/COH18-012; 1B Lock-10 side-table row (`grammar_provider.rs:32-37` `emitter` field). | None. |
| CH5-V3-008 | REJECT | RECALLED/FALSE premise in the V3 CHALLENGE-CONTEXT: it asserts the 1F auxiliaries "remain historical and superseded" and "are not live authority" (`§ "The two 1F auxiliaries remain historical and superseded"`, carried from the prior SK-V15 V3 context). That is contradicted by disk: `1F-anti-pattern.md` was REGENERATED this pass (`:4 cycle: V5-SKV18-totality`; `prior_cycle_dispositions_folded` rewrites the SK-V15-V2 stubs) and is the SOLE carrier of the OnceCell Lock-1 classification cross-referenced by the live `1E-locks-evidence.md:158` and of the FNV hash-sidecar fence. A superseded non-authority file cannot be the sole carrier the live 1E depends on. | `1F-anti-pattern.md:4` `cycle: V5-SKV18-totality`; `1F-coherence-scan.md:24` lists "`1F-anti-pattern.md` … SK-V15-V2 superseded stubs (rewritten this pass)"; `1E:158` "(cross-ref 1F-anti-pattern OnceCell row)". | The "historical / not live authority" framing of `1F-anti-pattern.md` is the false claim; the falsifying evidence is the V5-SKV18 regeneration header + the live 1E cross-reference. Treat `1F-anti-pattern.md` as a current-pass auxiliary whose OnceCell+FNV rows ARE live CH5 authority (subject to the CH5-V3-003/005 corrections above). |

## Track-1 / Track-2 Honesty (lens-mandated check)

No Track-1≡Track-2 dishonesty surfaces. RESULTS.md labels Track 1 (generated)
and Track 2 (independent hand/serde oracle) as DISTINCT planes with per-iter
equality (`RESULTS.md:5-25`, e.g. twitter Track 1 8349.290 vs Track 2 4558.264
vs sonic-rs strict 4913.095; the ">SOTA" 1D:62 cite compares Track 1 to
sonic-rs, not to Track 2). The Track-2 oracle (`skinny/crates/bbnf-bench/src/track2/json.rs`)
legitimately REUSES Track-1 runtime tape helpers (`use runtime::{grammars::json::{JsonRoot,
ParseError}, tape::{CapacityPlan,OffsetFlags,TapeBuilder}}`; `JsonRoot::from_tape`) —
this is shared-helper construction of an oracle, NOT a second retained substrate,
and is exactly what makes the per-iter equality honest. The live 1D fences the
second-substrate temptations as REJECTED lessons (Item 246 structural-stream
driver `1D:163`, Item 53 parser-local second scanner `1D:166`). This axis is
honoured; recorded ACCEPT-equivalent, no separate finding row needed.

## Notes

- Static audit only. No source edits, builds, tests, staging, or commits.
- The CORE skinny-substrate Lock-1 coverage (CH5-V3-001/002) and the
  scanner-asymmetry + relocated-seam catches (CH5-V3-004/007) are sound and
  spot-verified verbatim.
- The substantive downstream residual unchanged from V1/V2: the totality-tree
  retained `OnceCell<StructuralIndex>` probe (8/9 grammars, per-parse
  `generated_function`) + the `crates/simd-scan` `next_structural_at_or_after`
  retained-index API are an UNCLASSIFIED SK-V19 substrate-union reconcile burden.
  Do not close "BOTH trees honour Lock 1" until each totality
  `OnceCell<StructuralIndex>|ensure_structural_index|scan_structural|next_structural_at_or_after`
  hit is classified per `LOCKS.md:139-149`.
- Extend the CH5/1F sidecar+hash grep guard to
  `OnceCell<StructuralIndex>|ensure_structural_index|scan_structural|next_structural_at_or_after|input_fnv64|stream_fnv64|fn fnv64|fnv64\(`
  so future substrate-close checks cannot miss probe-substrate OR hash-sidecar
  surfaces in either tree.

TALLY accept=4 revise=3 reject=1
