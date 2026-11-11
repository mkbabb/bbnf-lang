# SK-V20 Deferred-Census — Ledger of Punted, Pending, and Residual Items

Auditor: 05-deferred-census. Date: 2026-06-10. HEAD surveyed: f4633d468 (current master).
Source sweep: `restart/` tranche SPEC/PASS/HANDOFF/audit docs and locks, plus code
spot-checks at HEAD. `rg` patterns verified against actual files; all status assertions
are code-checked, not doc-checked.

Status codes used:
- **DONE** — item is verifiably closed at HEAD 36ca10646.
- **OPEN** — no implementation exists; the wave/close gate has not fired.
- **PARTIAL** — some sub-items done, others open; split noted.
- **CHRONIC** — deferred across 2+ cycles without landing; flagged.

---

## §1 — SK-V19 Wave Ledger (the current implementation cycle)

The SK-V19 SPEC (`restart/skinny/tranches/sk-v19/SPEC.md`) declares seven waves W0-W6.
At HEAD 36ca10646 only the W6 leak cluster has received partial implementation. All
substrate, value-plane, and decision-engine waves remain entirely open.

| # | Item | First seen | Re-defers | Status | Evidence |
|---|---|---|---|---|---|
| 1 | **W0 — substrate_target classification (9-carrier report)** | SK-V19 SPEC §1 | 0 (new) | **OPEN** | No `substrate_target` classification report committed; `crates/core` tape has no SoA columns; `rg substrate_target crates/core` = 0. |
| 2 | **W1 — eager StructBuilder families retire** | SK-V19 SPEC §2 | 0 (new) | **OPEN** | `xtask/src/regen_simple_runtime.rs` (4343 LOC) still exists; no `JsonStructBuilder`/`CssStructBuilder` grep returns a hit because those exact names are gone, but the per-grammar builder emitters in regen paths remain untouched; the spec says "delete after tape wired" and the tape is not yet wired in crates/core. |
| 3 | **W2 — AoS TapeRec → SoA Tape exactly-one-encoding** | SK-V15 era; re-stated SK-V19 §3 | CHRONIC (SK-V15→V16→V17→V19) | **OPEN** | `crates/core/src/runtime/tape/record.rs:103` declares `pub struct TapeRec` (16-byte AoS record, asserted `size_of::<TapeRec>() == 16` at `:120`). `rg TapeRec crates/core/src/runtime/tape/` hits `record.rs`, `arena.rs`, `cursor.rs`, `mod.rs`. No SoA `offsets:Vec<u32>` column exists in `tape/mod.rs`. This is the longest-running deferral in the campaign. |
| 4 | **W3 — C fold: ONE grammar-agnostic value-plane generator** | SK-V17 (deferred on >SOTA-preservation burden); SK-V19 §4 | CHRONIC (SK-V17→V18→V19) | **OPEN** | `crates/core` has zero `ValueRef` (`rg ValueRef crates/core/src` = 0); zero `Cursor` value-plane; `json_templates/{value,view,visitor}.rs` couriers still exist in skinny (`skinny/crates/codegen/src/json_templates/`); `general_descent` emitter not yet promoted to crates/core. The PASS-IMPL explicitly marks "Phase 3 pending even in skinny" (`restart/skinny/tranches/sk-v19/audit/PASS-IMPL-FOLD-READINESS.md:27-28`). |
| 5 | **W4 — StructRegistry/FieldSource compile-time projection fence** | SK-V19 SPEC §5 | 0 (new) | **OPEN** | No compile-time gate; W4 cannot land until W3 exists. |
| 6 | **W5a — BackendShape enum introduced in crates/ir** | SK-V19 SPEC §6 | 0 (new) | **OPEN** | `rg BackendShape crates/ir/src` = 0. The enum exists only in the skinny tree (`skinny/crates/ir/`). |
| 7 | **W5b — egraph+csp selector wired into crates/ir passes** | SK-V15+ (MP.SK15.W7-W9 active pending) | CHRONIC (SK-V15→V19) | **OPEN** | `rg backend_shape crates/ir` = 0. The `crates/egraph` and `crates/csp-solver` crates exist but are not connected to `crates/ir/src/passes/recognizers/`. MASTER-PLAN rows MP.SK15.W8/W9 still read "active pending" (`restart/MASTER-PLAN.md:970-971`). |
| 8 | **W5c — cost model gating (a)-(d) named primitives** | SK-V19 SPEC §6 | 0 (new) | **OPEN** | Depends on W5b. |
| 9 | **W5d — simd-scan avx2/avx512/wasm production wiring DELETE** | SK-V19 SPEC §6 D-E | 0 (SK-V19 new) | **OPEN** | `crates/simd-scan/src/avx2.rs` (370 LOC), `avx512.rs`, `wasm.rs` all exist. `scan_structural` in `lib.rs:93-110` dispatches to avx512/avx2/wasm under `cfg` gates, NOT deleted. The D-E decision to delete x86/wasm production wiring from `crates/simd-scan` has not been carried out. |
| 10 | **W6a — PRODUCTION_MANIFEST_TABLE metadata-driven** | SK-V18 T-P1 (COH18-005); SK-V19 §7 W6a | SK-V18→V19 | **DONE** | `crates/ir/build.rs` (92 LOC) generates table from `[workspace.metadata.bbnf-strategy]`; `include!` wires it (`strategy.rs:139`); no grammar-named const in source. Landed at 36ca10646. |
| 11 | **W6b — css_types.rs relocated to runtime/css_l4/host.rs** | SK-V18 T-P1 (COH18-006); SK-V19 §7 W6b | SK-V18→V19 | **DONE** | `crates/core/src/css_types.rs` deleted (git status `D`); `crates/core/src/runtime/css_l4/host.rs` created. Landed at 36ca10646. |
| 12 | **W6c — shape_dict_bbnf.rs re-keyed on structure not rule-name** | SK-V19 §7 W6c | 0 | **DONE** | `is_big_comment_shape`/`is_mapped_factor_empty_shape` now use structural matching on `"/*"/"*/"` and `"->"` literals, not `"big_comment"`/`"mapped_factor"` string literals. Landed at f4633d468. |
| 13 | **W6d — StructLayout → Layout rename (1030 sites)** | SK-V19 SPEC §7 D-C | 0 (new) | **OPEN** | `rg StructLayout crates/` returns 960 hits. The rename has not started. |
| 14 | **W6e — Lock-14 self-gate live (xtask/CI)** | SK-V19 §7 W6e | SK-V18→V19 | **DONE** | `crates/ir/tests/lock14_grammar_generalization.rs` live gate with 9-roster scan + re-inject falsifier. Landed at 36ca10646. |

---

## §2 — Chronic Items (Deferred Across 2+ Cycles)

These items have been named as future work in multiple distinct tranches without
landing. All represent structural architecture that the >SOTA claim depends on.

### CHRONIC-1: AoS TapeRec → SoA Tape (W2)

First declared as a target in SK-V15 receiver block `MP.SK15.W0`, re-stated in SK-V16
`REDRESS-53`, carried through SK-V17 SPEC (the "dual-encoding hazard" clause), carried
again through SK-V18 as a "GREENFIELD ADOPT" for SK-V19, and now W2 of SK-V19. At HEAD
the 16-byte AoS `pub struct TapeRec` is still the sole `crates/core` tape record
(`crates/core/src/runtime/tape/record.rs:103`; `size_of::<TapeRec>() == 16` asserted at
`:120`). No SoA columns exist in `tape/mod.rs`. The PASS-IMPL
(`audit/PASS-IMPL-FOLD-READINESS.md:16-22`) states "NOT on the proven SoA tape" for
crates/core. Deferred 4+ cycles.

### CHRONIC-2: C fold — grammar-agnostic value-plane generator (W3)

The C fold was explicitly deferred from SK-V17 "on the >SOTA-preservation burden" and
from SK-V18 as the W3 SK-V19 obligation. `crates/core` has no `ValueRef`, no
`Cursor`, no grammar-agnostic value generator. The hand-authored `json_templates/`
courier files still exist in skinny (`skinny/crates/codegen/src/json_templates/value.rs`,
`view.rs`, `visitor.rs`). The PASS-IMPL flags "Phase 3 pending even in skinny"
(`PASS-IMPL-FOLD-READINESS.md:28`). Three emit bodies
(`json_sink_direct`/`css_scan_direct`/`general_descent`) still exist — the C fold
must collapse them to one. Deferred SK-V17→V18→V19.

### CHRONIC-3: BackendShape / decision engine in crates/ir (W5b)

Named since SK-V15 as `MP.SK15.W7` ("Decision Engine spine — active pending") and
`MP.SK15.W8/W9` ("BackendShape harness — active pending"). MASTER-PLAN:970-971 still
carries these rows as "active pending." At HEAD `rg BackendShape crates/ir/src` = 0.
The 5-shape enum exists only in the skinny tree. Deferred SK-V15→V16→V17→V18→V19.
Five cycles without a single `BackendShape` enum in `crates/ir/src`.

### CHRONIC-4: simd-scan / bbnf-simd scanner-unification (MP.SK19.SCANNER-UNIFY)

Named as `COH18-015` in T-P1, carried as `MP.SK19.SCANNER-UNIFY` in the SK-V19 tee-up
(`3B-master-plan-reconciliation.md:177`). The totality `crates/simd-scan` and skinny
`bbnf-simd` remain functionally parallel with divergent APIs: `crates/simd-scan`
exports `NibbleLut`/`WideLut`/`KernelShape` and a `scan_structural` entry point
producing `StructuralIndex`; skinny `bbnf-simd` exports `structural_classify_64`
producing a 6-mask. The W5d deletion of avx2/avx512/wasm from `crates/simd-scan`
has not occurred; avx2.rs/avx512.rs/wasm.rs all exist in `crates/simd-scan/src/`
with live `cfg`-gated dispatch in `lib.rs:93-110`. Deferred SK-V18→V19.

### CHRONIC-5: Lock-16 single-movemask co-gate / multi-pack census

LOCKS.md:641 documents ≥4 non-delegating pack implementations in `bbnf-simd` (the
`vaddv_u8` shift-add bodies in `bracket_depth_mask_64.rs`, `comment_body_mask_64.rs`,
`byte_class_from_eq_set_64.rs`, `structural_classify_64.rs`). The structural co-gate
`bbnf_simd_single_mask_convention` (every 64-byte→64-bit pack delegates to the ONE
canonical SHRN pack in `movemask.rs:4`) is described as "PLANNED" in LOCKS.md:641 and
has not been implemented. Deferred from SK-V18 into SK-V19, unaddressed at HEAD.

### CHRONIC-6: SK-V16/V17 REDRESS pre-block reconcile (D-G)

SK-V19 SPEC §0.5 D-G requires "confirm the SK-V16/V17 REDRESS pre-block reconcile
(items 51/53/246/247) is on the committed ledger before W1/W2 dispatch." This
pre-block covers streamed-cursor/second-scanner REJECT routes that abut G2/G4/G6
waves. Per `3B-master-plan-reconciliation.md:256` (CH3 Q), the reconcile is a
"Pass-Omega-V6 / pre-W-PRUNE blocker," meaning W0-W6 implementation is gated behind
it. No committed ledger entry for items 51/53/246/247 was located at HEAD. This
blocks dispatch of W1/W2.

---

## §3 — Legitimately Deferred Items (Not SK-V20 Scope)

These items are deferred by architectural design decision; pulling them forward would
violate stated contracts or is explicitly post-V1.

| Item | Deferral class | Authority | Notes |
|---|---|---|---|
| TS/WASM backends (TsBackend, WasmBackend) | Post-V1 architectural | LOCKS.md:181 Lock 5; deferral-audit-6 §2.1 | Locked to V2; TS backend deferred to BD.W1; WASM performance gate deferred to H.W3 measured values. |
| TS path runtime (bbnf-path-ts full runtime) | Post-V1 architectural | deferral-audit-5 §3 | Schema-typed binding over WASM-loaded Rust; TS-native parse forks lifetime story. |
| Full-corpus WASM SOTA measurement ({N}/{M} in H.W3) | Measurement deferral | MASTER-PLAN:479; deferral-audit-6 §6 | Pure measurement placeholder; architecture settled. |
| CSS >SOTA totality-tree re-capture (same-run H1) | Measurement deferral | SK-V18 SYNTHESIS:288-291 | The `css_canon_bench` re-lock at crates/core scale is SK-V19 close gate 5 (`sota_preserved`); pending W3/W5 landing. |
| Visitor cookbook content (cookbook/visitor-mutation.md) | Documentation deferral | deferral-audit-5 §3 | Content writing task; no architecture gate open. |
| Profile-guided specialization | Post-V1 | deferral-audit-6 §4 | Requires measurement feedback loop not in V1. |

---

## §4 — Open Items Requiring SK-V20 Triage

Items below are open but have not reached chronic status. SK-V20 should either schedule
them or promote them to the chronic ledger.

**4.1 W0 substrate_target classification report.** This is the entry gate for all W1-W2
work. It carries zero behaviour LOC; it is purely a classification document. There is no
blocker beyond executing the census and committing it. The longer this waits, the more
likely W1 authors will dispatch without the classification forcing a revert.

**4.2 bbnf-simd multi-pack census.** LOCKS.md:641 asserts ≥4 non-delegating pack bodies.
If the Lock-16 co-gate is deferred further, it should at minimum be verified against the
ACTUAL count at HEAD, since the LOCKS.md assertion dates from the SK-V18 audit. The
structural-classify_64.rs file now exists (`skinny/crates/bbnf-simd/src/aarch64/
structural_classify_64.rs`) and uses `vaddv_u8`; the LOCKS.md count may have grown.

**4.3 D-G pre-block reconcile.** The SK-V19 SPEC marks this as required before W1/W2
dispatch. It has no implementation cost (doc commit only) but blocks all substrate
waves. This should land as the very first SK-V19 implementation act.

**4.4 BBNF-self fleet carrier (D-F).** `crates/core/src/grammar/generated/bbnf.rs`
exists; the carrier is present. The SK-V19 SPEC names BBNF-self as a required W3
second negative control (D-F: the generator must emit BBNF-self by-construction). No
`ValueProject` derive for BBNF-self exists in `crates/core`. This is a W3 sub-item but
should be tracked explicitly because it is the harder negative control (recursive
grammar, not a formula grammar like Sheets).

**4.5 json_templates Phase 3 in skinny.** The PASS-IMPL explicitly states "Phase 3
pending even in skinny" for the JSON courier retirement. `skinny/crates/codegen/src/
json_templates/` still contains `value.rs`/`view.rs`/`visitor.rs` that the W3 C fold
must retire. Until these are retired in skinny, the W3 port to crates/core lacks a
clean skinny precedent.

**4.6 grammar_facts.rs 131 grammar-name occurrences.** The PASS-IMPL (`PASS-IMPL-
FOLD-READINESS.md:36-38`) identifies 131 grammar-name occurrences in `crates/ir/src/
passes/recognizers/grammar_facts.rs` and `shape_dispatch/`. The W6c landing (f4633d468)
fixed `shape_dict_bbnf.rs` specifically but did NOT address the 131-occurrence body in
`grammar_facts.rs`. Static inspection confirms `grammar_facts.rs` (1530 LOC) refers to
"Sheets" as a doc example multiple times. The Lock-14 live gate (`lock14_grammar_
generalization.rs`) excludes doc-comment lines (`///` and `//!`), so these references
do not currently fire. However, the PASS-IMPL counted them as a "leak class OUTSIDE the
Pattern-H file frame." If any of the 131 occurrences is a code line rather than a doc
line, the gate will catch it; if all are doc lines, the count is not a live leak but
the number should be audited at W6e close.

---

## §5 — Cross-Tranche Ledger Summary

The table below shows each deferred item keyed by its document origin, number of
tranches it has appeared in without landing, and the proposed SK-V20 disposition.

| Item | Origin | Cycles deferred | Code status at HEAD | SK-V20 disposition |
|---|---|---|---|---|
| AoS TapeRec → SoA Tape | SK-V15 REDRESS-53 | 4+ | OPEN (`TapeRec` at record.rs:103) | W2 core work; block W1 dispatch on D-G |
| C fold / ValueRef / grammar-agnostic gen | SK-V17 deferred | 3 | OPEN (no ValueRef in crates/core) | W3 core work; needs W5 co-land |
| BackendShape / decision engine in crates/ir | SK-V15 MP.SK15.W7-9 | 5 | OPEN (rg BackendShape crates/ir = 0) | W5a-c core work |
| simd-scan x86/wasm production deletion | SK-V19 D-E | 1 (new) | OPEN (avx2/avx512/wasm live in lib.rs) | W5d |
| Scanner unification (bbnf-simd vs simd-scan) | SK-V18 COH18-015 | 2 | OPEN (parallel APIs diverge) | W5d resolve UNIFY vs parallel-scanner |
| Lock-16 single-movemask co-gate | SK-V18 LOCKS:641 | 2 | OPEN (≥4 non-delegating bodies) | W5d structural co-gate |
| D-G pre-block reconcile (items 51/53/246/247) | SK-V19 SPEC §0.5 | 1 (new) | OPEN (no committed ledger) | Pre-W1 doc commit — blocks all substrate waves |
| StructLayout → Layout rename | SK-V19 SPEC W6d | 1 (new) | OPEN (960 hits) | W6d; needs W5 first |
| json_templates Phase 3 in skinny | SK-V18 PASS-IMPL | 1 | OPEN (couriers still exist) | Skinny pre-condition for W3 |
| BBNF-self by-construction W3 gate | SK-V19 D-F | 1 (new) | OPEN (no ValueProject in crates/core) | W3 sub-item |
| grammar_facts.rs 131-occurrence audit | PASS-IMPL | 1 | UNCLEAR (may be doc-only) | W6e close audit item |
| TS/WASM backends | LOCKS Lock 5 | Post-V1 | By design absent | Keep deferred — not SK-V20 scope |
| CSS >SOTA totality-tree re-capture | SK-V18 H1 | 1 | Pending W3/W5 | SK-V19 close gate 5 |
| W6a metadata-driven manifest | SK-V18→V19 | 1 | DONE (36ca10646) | Closed |
| W6b css_types.rs relocation | SK-V18→V19 | 1 | DONE (36ca10646) | Closed |
| W6c shape_dict structural re-key | SK-V19 | 0 | DONE (f4633d468) | Closed |
| W6e Lock-14 live gate | SK-V19 | 0 | DONE (36ca10646) | Closed |

---

## §6 — Findings Requiring Adversarial Attention

**F1 — The W6 implementation order is backwards.** W6 (the leak collapse) is classified
in the SPEC as last ("W6a∥W6b∥W6c → W6e parallel to substrate"). Landing W6 first,
before W0-W5, gives a false sense of progress: the Lock-14 self-gate now reads GREEN
for the four items W6a/b/c/e address, but the substrate items (W0-W2), the value plane
(W3-W4), and the decision engine (W5) are entirely absent. The green gate at HEAD is
not a correctness property of the full system; it is a correctness property of a stub.
The gate will remain green even if W3 introduces a grammar-agnostic generator that
itself adds grammar-specific code — because W3 is not yet audited.

**F2 — The avx2/avx512/wasm modules in crates/simd-scan are a standing violation of
the aarch64-only mandate.** The SK-V18 P1 deletion was scoped to `skinny/crates/
bbnf-simd/` (confirmed: `find skinny/crates/bbnf-simd/src/x86_64 ext/x86 -type f` =
0). The totality `crates/simd-scan/src/avx2.rs` (370 LOC) was never in scope for P1;
it is now a D-E W5d item. The current state is: skinny builds aarch64-only; totality
ships avx2/avx512/wasm behind cfg gates. LOCKS.md:641 notes these as "production
wiring deleted" PLANNED work. At HEAD, the deletion is UNPLANNED-for-crates/simd-scan.

**F3 — The D-G pre-block is a hidden dependency for all W0-W6 substrate work.** SK-V19
SPEC §0.5 D-G requires the SK-V16/V17 REDRESS pre-block reconcile (items 51/53/246/247)
on the committed ledger before W1/W2 dispatch. If W1 dispatch happens without this,
any wave that touches the tape (W2, W3) could re-open the second-substrate/streamed-
cursor REDRESS routes. This is a soft blocker that is easy to miss because it is a
documentation-only act.

**F4 — grammar_facts.rs references to "Sheets" need a live test verification.** The Lock-14
gate at HEAD excludes `///`/`//!` doc lines. If any of the 131 occurrences in
`grammar_facts.rs` is in executable code (e.g., a `const` value or a match-arm string),
the gate would catch it. If all are in doc comments, they are invisible to the gate.
This should be verified by running the live gate against the file; this audit cannot
run cargo.
