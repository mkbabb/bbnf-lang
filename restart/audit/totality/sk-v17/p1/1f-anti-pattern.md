---
agent: 1F
pass: T-P1-excavation
cycle: V5
generated_at: 2026-05-30T17:00:00Z
spec_surfaces_audited:
  - restart/locks/LOCKS.md
  - restart/ARCHITECTURE.md
  - restart/skinny/tranches/sk-v17/SPEC.md
files_audited_count: 14
live_truth_method: "grep over crates/core/src/runtime/{tape,json,css_l4}/, crates/ir/src/registry/struct.rs, crates/simd-scan/src/, crates/core/src/grammar/generated/*.rs, crates/core/src/backend/rust/emitter/shapes/; Read over the tape/builder/struct files; CH5 firewall = Lock 1 substrate-union + Lock 14 grammar-neutral; V5 re-verified live at HEAD 445925167154de73540e3ea3283d0170371de790: grep -c scan_structural over all 9 generated grammars (json/css_l4/ebnf/bnf/csv/css_pretty/google_sheets/bbnf = 1 each; math.rs = 0), grep -c OnceCell math.rs = 2 (both doc-comments :281/:285, no field — the 8-carrier OnceCell census excludes math.rs, matching CH1-V4-001), css_l4/builder.rs pending_* field-type re-read (6 Vec<T> :74-79 + 1 Option :71, none Vec<Vec>), TapeStructBuilder grep-zero outside runtime/tape/, REDRESS-53 named-line confirmation (:577/:657/:825/:839); no cargo/build mutation"
prior_cycle_dispositions_folded:
  accepted:
    - CH5-S3-no-cross-call-classifier-state
    - V4-CH5-S2-sidecar-OnceCell-all-8-AP17-004-held-synced-substrate_target-REDRESS-53
    - V4-CH5-S3-renamed-scanner-cross-call-state-retention-lifetime-REJECT-held-open
    - V4-AP17-001-parallel-substrate-firewall-clean-carry
    - V4-AP17-002-grammar-name-leak-Lock14-ALLOWED
    - V4-AP17-003-817-LOC-god-module-fold-deletion-target
    - V4-AP17-004-sidecar-OnceCell-CRITICAL-verified
    - V4-AP17-005-structregistry-hot-path-do-not-redrive
    - V4-CH7-1f-anti-pattern-OVERFIT-clean
  rejected: []
  revised:
    - CH1-V2-004-AP17-002-OnceCell-census-include-css_l4-all-8
    - CH1-V2-005-AP17-003-pending-count-six-Vec-plus-Option
    - CH3-R1-StructRegistry-hot-path-indirection-row-added
    - CH5-S4-pending-count
  first_cycle_additions:
    - AP17-001-parallel-substrate-firewall
    - AP17-002-grammar-name-leak-core
    - AP17-003-god-module-builders
    - AP17-004-sidecar-OnceCell-StructuralIndex
    - AP17-005-structregistry-hot-path-indirection
divergence_count:
  spec_claims_implemented: 0
  spec_claims_unimplemented: 0
  impl_exceeds_spec: 0
  unknown: 1
locks_amendment_candidates: 0
---

## Executive Summary

CH5 firewall scan over the SK-V17 tape/substrate/value-API/NEON surfaces of the
TOTALITY fold target (crates/core). The Lock-1 parallel-substrate question is the
spine. Finding: within crates/core there is exactly ONE retained tape construct
(`TapeStructBuilder` / `TapeRec` / `PayloadArena` / `TapeCursor`,
`crates/core/src/runtime/tape/`), and it is UNWIRED — no live parse path consumes
it (the eager `OpenFrame` builders are the live substrate). So there is no Track-1
≡ Track-2 dishonesty inside crates/core's tape: the tape is not a sidecar producer
beside a live tape, it is dead code awaiting the SK-V18 fold. The structural-scan
sidecar IS live: generated parsers hold `OnceCell<StructuralIndex>` fields
(`crates/core/src/grammar/generated/json.rs:686-732`) — a retained structural
projection that, under Lock 1, IS the tape if retained, but here it feeds eager
builders, not the tape. Grammar-name leaks: the per-grammar runtime modules
(`runtime/json/`, `runtime/css_l4/`) carry grammar-named builders/value enums, and
the emitter `substrate.rs` references `CssStructBuilder`/`JsonStructBuilder` by name
(`crates/core/src/backend/rust/emitter/shapes/substrate.rs:60,73`) — but these are
per-grammar runtime surfaces (Lock 14 ALLOWED), not generic-crate leaks. The
god-module risk is the CSS builder at 817 LOC (`crates/core/src/runtime/css_l4/builder.rs`),
flagged for the fold. No NEW retained substrate, no second tape, no parallel
producer proven within crates/core in this scan.

## Anti-Pattern Scan

| Anti-pattern | Live evidence | Verdict | verify_action |
|---|---|---|---|
| Parallel substrates / second tape (Lock 1, CH5 firewall) | crates/core holds ONE tape construct: `TapeStructBuilder` (`crates/core/src/runtime/tape/mod.rs:58`), `TapeRec` AoS (`record.rs:103`), `PayloadArena` (`arena.rs`), `TapeCursor` (`cursor.rs`). It is UNWIRED (grep-zero outside `tape/`). The live substrate is the eager `JsonStructBuilder`/`CssStructBuilder` `OpenFrame` (`json/builder.rs:9`, `css_l4/builder.rs:16`). So the tape is NOT a sidecar beside a live tape — it is the SK-V18 fold target sitting dormant. | NO parallel-substrate violation within crates/core; the AoS-vs-SoA cross-tree mismatch (COH17-001) is a fold-convergence question, NOT a same-tree second substrate. | Before any SK-V18 fold close: grep `TapeStructBuilder|TapeRec|PayloadArena|TapeCursor` over crates/core to confirm exactly one tape survives once wired; confirm `OpenFrame` builders are RETIRED not retained beside it. |
| Sidecar producer / retained structural projection (Lock 1) | Generated parsers retain `OnceCell<StructuralIndex>` initialized via `scan_structural` — in ALL 8 generated grammars (doc lines json.rs:686, css_l4.rs:15936, ebnf.rs:1335, bnf.rs:802, csv.rs:520, css_pretty.rs:1859, google_sheets.rs:3513, bbnf.rs:4797; json field+init :701,:711,:732; css_l4 field :15951, scan call :15982). The LARGEST grammar (css_l4) carries the identical retained index — the census is all 8, NOT a 4-grammar sample. The index is a retained structural projection. **The 8-carrier OnceCell census is exactly the 8 scan-wired grammars (`grep -c scan_structural` = 1 each); the 9th generated grammar `math.rs` is NOT a carrier — `grep -c scan_structural math.rs` = 0 and its two `OnceCell` mentions at `math.rs:281,285` are doc-comment text, not a `structural_index` field (the same symbol-vs-doc-comment class CH1-V4-001 corrected in 1C/1D). So the firewall scope is 8 carriers, never 9.** | Per Lock 1, "if structural offsets are retained, the structural projection IS the tape" (`ARCHITECTURE.md:1088`) — but here the retained index feeds the eager `OpenFrame` builders, NOT a tape; it is a retained scan cache, not the tape projection. This matches the prior SK-V14 1F COH-014 root-OnceCell coupling (`restart/audit/totality/p1/1F-coherence-scan.md:87`). Classify at fold: retained scan cache vs tape projection. | Under SK-V18, the `OnceCell<StructuralIndex>` must EITHER become the tape's `offsets` (Lock-1 union: index IS the tape) OR be re-classified as `local_temp_only`; a retained index parallel to a wired tape would be REDRESS-53 (SPEC §9 `:825`). |
| God module | `crates/core/src/runtime/css_l4/builder.rs` = 817 LOC with `OpenFrame` enum + **six `pending_*` Vecs (`:74-79`: pending_rules, pending_decls, pending_selectors, pending_values, pending_blocks, pending_components) plus one `pending_value: Option` (`:71`)** = SEVEN pending_ fields, NONE of them `Vec<Vec>`; `json/builder.rs` = 231 LOC. Lock 13 budget governs. The CSS builder's `pending_*` slabs are the eager-arena shape SK-V17 W1 deletes (SPEC `:7-8`, `:485`). | The 817-LOC CSS builder is the eager `OpenFrame`+`pending_*` machine SK-V17 PRUNEs; it is god-module-shaped (one file, many concerns) and is the fold-deletion target, not a permanent surface. (Count corrected per CH5-S4 / CH1-V2-005: six Vec + one Option, not "nine Vecs".) | At SK-V18 fold: the `OpenFrame`/`pending_*` builders collapse into the single `TapeStructBuilder` (`crates/core/src/runtime/tape/mod.rs:16-20` already names this as "the single generic StructBuilder impl … serves CSS, JSON, sheets, bbnf"). |
| Grammar-name leaks (Lock 14) | Per-grammar runtime modules `runtime/json/` + `runtime/css_l4/` carry grammar-named builders + value enums (`CssStructBuilder`, `CssTypedValue` `css_l4/value.rs:414`); emitter `substrate.rs` references both builders by literal path (`crates/core/src/backend/rust/emitter/shapes/substrate.rs:43,55,60,73`). | These are PER-GRAMMAR runtime surfaces (Lock 14 ALLOWED: "allowed CSS-specific surfaces: … per-grammar providers/templates", SPEC `:334`). The emitter consumes a `builder_path`/`document_path` from `EmitStrategy::StructDirect` as DATA (`substrate.rs:43,55`), not a grammar-name branch — Lock 14 honoured. The generic tape (`TapeStructBuilder`) carries NO route strings ("dispatches on the StructLayout … never on per-grammar route strings", `crates/core/src/runtime/tape/mod.rs:54-56`). | Confirm at fold: when the tape is wired, the per-grammar `*StructBuilder` route is replaced by `TapeStructBuilder` dispatching on `StructLayout`; the emitter `substrate_path` data-binding must survive as data, not collapse to a grammar-name branch. |
| StructRegistry / Arena<G> / Builder<G> hot-path indirection (SK-V17 W1 pre-block, do-not-redrive) | `StructRegistry` is `layouts: BTreeMap<RuleId, StructLayout>` (`crates/ir/src/registry/struct.rs:313-314`); `FieldSource` (`:84`) lives INSIDE `StructLayout` (`:202`) inside the registry. The runtime tape consumer `begin_compound(&StructLayout)` (`crates/core/src/runtime/tape/mod.rs:185`) takes a RESOLVED `&StructLayout` by reference. SK-V17 pre-blocks "StructRegistry / Arena<G> / Builder<G> hot-path indirection (28-65× bbnf/sheets, 983× css bootstrap, 10583× WATCHDOG tailwind). No registry lookup in the per-leaf hot path" (`restart/skinny/tranches/sk-v17/SPEC.md:794-795`; W1 row `:824`). | DO-NOT-REDRIVE FENCE: the SK-V18 value-projection FieldSource walk MUST be **compile-time projection-emission, resolved once** at codegen — NOT a per-leaf runtime `StructRegistry`/`FieldSource` walk. A naive per-leaf walk re-opens the 28-65×/983×/10583× regression. `begin_compound` taking a pre-resolved `&StructLayout` is correct; a per-leaf `StructRegistry::layout(rule)` call in the hot path is the REJECT shape. | Before SK-V18 fold close: grep the projection generator for any runtime `StructRegistry`/`registry.layout(`/`.field(` call inside the per-leaf emit loop; confirm the FieldSource walk is emitted statically per-rule (resolved once), citing SPEC `:794-795`. |
| Renamed-scanner / cross-call classifier state (Lock 1 v+1) | crates/core scan is `scan_structural(input, &alphabet)` (`crates/simd-scan/src/lib.rs:80`) producing a `StructuralIndex` per call; no cross-call classifier-state retention visible in the dispatch (`lib.rs:80-113`). | No cross-call retained classifier state proven in crates/core scan in this pass (Lock 1 v+1 `:137-149` REJECT class not tripped). The `OnceCell<StructuralIndex>` retains the OUTPUT index per-parse, not classifier carry state. | Before SK-V18 SIMD fold: grep `prev_state|carry|prefix_xor|retained` across `crates/simd-scan/src/` for cross-call state; confirm carry stays within a single scan call (Lock 1 v+1 `:141`). |

## CH5 Firewall Verdict (Track 1 ≡ Track 2 honesty)

No Track-1 ≡ Track-2 dishonesty surfaces within crates/core in this scan: the tape
is unwired dead code, not a producer masquerading as an independent oracle. The
only LIVE retained projection is the `OnceCell<StructuralIndex>` scan cache, which
under Lock 1 is admissible as "the structural projection IS the tape" ONLY once a
tape consumes it; today it feeds eager builders and is a retained scan cache. The
SK-V18 fold must collapse: (1) `OpenFrame`/`pending_*` eager builders → the single
`TapeStructBuilder`; (2) the `OnceCell<StructuralIndex>` cache → the tape's
`offsets` (index IS the tape) or `local_temp_only`. A fold that wires the tape but
retains the eager builders OR the parallel index is a Lock-1 second-substrate
violation (SPEC §9 `:483-485`, REDRESS-53 `:825`).

## Open Questions

| UNKNOWN | Blocking question | verify_action |
|---|---|---|
| U-AP17-001 | Is the `OnceCell<StructuralIndex>` in crates/core generated parsers a `local_temp_only` scratch or a `existing_tape` projection under the Lock 1 v+1 `substrate_target` manifest (`LOCKS.md:120-127`)? | T-P2 read ALL 8 generated `structural_index` fields (json.rs:701,711,732 + css_l4.rs:15951,15982 + ebnf/bnf/csv/css_pretty/google_sheets/bbnf siblings), NOT json alone, and classify against the four admitted `substrate_target` values; the fold must declare it before wiring the tape, else REDRESS-53 re-entry (SPEC `:825`). |
