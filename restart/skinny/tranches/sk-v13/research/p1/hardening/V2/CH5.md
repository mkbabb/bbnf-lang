# SK-V13 S-P1 V2 CH5: Hidden Coupling/Substrate Review

Pass: S-P1 Profile. Cycle: V2.
Date: 2026-05-21.
Scope: adversarial CH5 review of the six S-P1 V2 artifacts under `restart/skinny/tranches/sk-v13/research/p1/`.
Output: this file.
Lens: CH5 HIDDEN COUPLING - substrate union, Track 1/Track 2 separation, structural-scan-only treatment, direct/typed/CSS plane separation, and hot-leaf attribution sidecar/cursor implications.

## Disposition

ACCEPT.

V2 does not normalize independent observations into a sidecar substrate. The V1 shared-substrate inference has been removed from the V2 packet; direct profiling is now a measured Track 1/Track 2 product-plane lane, structural scan is measured as a separate mode-III probe, CSS is fenced as a separate telemetry/profile signal, and REDRESS 96/97/98 are cited as binding history against reopening the union-substrate route from scanner micro-results alone.

## Evidence

### CH5-HC-001 - Track 1 and Track 2 remain structurally separated

- Evidence: `p1a-samply-mode-1.md:5` scopes parse profiling as Track 1 generated JSON plus Track 2 independent hand JSON parser. `p1a-samply-mode-1.md:81`-`99` keeps separate Track 1 and Track 2 top-symbol columns, and `p1a-samply-mode-1.md:134` states that Track 1 leaves come from `runtime::generated_json::*` while Track 2 leaves come from `bbnf_bench::track2::json::*`.
- Evidence: `p1b-samply-mode-2.md:66`-`84` records direct Track 1 and Track 2 symbols in separate columns. Track 1 resolves to generated direct envelopes or runtime primitives; Track 2 resolves to `HandParser::*`, `parse_that_regex::unescape_string`, or timer noise.
- CH5 assessment: ACCEPT. V2 treats Track 2 as independent evidence, not as another view of generated runtime substrate. The only shared primitive called out across tracks is `parse_that_regex::unescape_string` for `unicode_escapes` (`p1b-samply-mode-2.md:81`, `p1b-samply-mode-2.md:113`-`115`), which is a named code leaf, not an implied substrate union.

### CH5-HC-002 - Direct, typed, and CSS planes are not collapsed into one substrate

- Evidence: `p1b-samply-mode-2.md:86`-`88` preserves typed as the V1 seven-row generated typed subset and explicitly refuses to invent the ten missing typed rows. `p1b-samply-mode-2.md:119`-`120` repeats that missing typed rows are product-surface gaps, not profiling omissions.
- Evidence: `p1f-results-delta.md:43`-`50` separates JSON parse, JSON direct, JSON typed, JSON mode III, CSS declaration-values, and the remaining CSS parity matrix in distinct rows. `p1f-results-delta.md:97`-`106` counts JSON and CSS inventory separately.
- Evidence: `p1e-hot-leaf-attribution.md:67`-`71` reports the CSS declaration-values profile as timer/fact-sink dominated, and `p1f-results-delta.md:117`-`120` says CSS V2 throughput is a hot-leaf/equality signal with `profile_signal_not_gate_admission` classification.
- CH5 assessment: ACCEPT. Direct evidence is not used to fill typed gaps, typed evidence stays generated-row-limited, and CSS is not counted as JSON profile convergence or as a parser substrate claim.

### CH5-HC-003 - Structural scan is measured directly and fenced from union-substrate reopening

- Evidence: `p1c-samply-mode-3.md:11` reports 17 JSON corpora x 5 captured probes, including structural scalar and structural SIMD. `p1c-samply-mode-3.md:98`-`106` contrasts V1 absence with V2 measured structural scalar/SIMD rows and routes unsupported probes explicitly.
- Evidence: `p1c-samply-mode-3.md:108`-`112` states that structural SIMD beating scalar scan is a scanner micro-result and does not by itself reopen REDRESS 96/97/98. `p1d-pmu-cycles.md:127`-`129` carries the same guardrail for PMU/counter interpretation.
- CH5 assessment: ACCEPT. V2 captures structural-scan-only evidence as its own mode-III lane. It does not backfill structural evidence from parse/direct/typed rows, and it does not treat the scanner result as a retained cursor, event vector, or second source scan.

### CH5-HC-004 - REDRESS 96/97/98 union-substrate history is preserved

- Evidence: `skinny/REDRESS.md:2797`-`2848` rejects the class-column plus move-consumed structural-index implementation after correctness-green measurement missed every W3 must-improve row and W10b maintain floor. `skinny/REDRESS.md:2852`-`2906` rejects the allocation-free streaming-cursor implementation after it also missed the measured gate.
- Evidence: `skinny/REDRESS.md:2910`-`2950` retires `G-W3-UNION-SUBSTRATE` for SK-V9: both faithful implementations falsified the thesis that retaining a SIMD structural index as the union substrate would improve throughput.
- Evidence: V2 cites that history instead of normalizing around it: `p1c-samply-mode-3.md:110`-`112`, `p1d-pmu-cycles.md:127`-`129`, and `p1e-hot-leaf-attribution.md:92`-`94`.
- CH5 assessment: ACCEPT. V2 records fresh structural SIMD/scalar facts without claiming a profitable union substrate or sidecar event lane.

### CH5-HC-005 - Symbol sidecars remain offline metadata, not substrate evidence

- Evidence: `p1a-samply-mode-1.md:75` says leaf samples are joined to `.json.syms.json` sidecars because saved profiles are not cleanly symbolicated. `p1b-samply-mode-2.md:62`-`64` and `p1e-hot-leaf-attribution.md:35`-`39` describe the same RVA-to-symbol resolution path and point to TSV outputs.
- Evidence: `p1c-samply-mode-3.md:58`-`62`, `p1c-samply-mode-3.md:121`-`122`, and `p1e-hot-leaf-attribution.md:97`-`99` make sidecar limitations explicit for ASM/system leaves.
- CH5 assessment: ACCEPT. The sidecars are used as offline symbol-resolution metadata. V2 does not describe them as parser events, retained cursor state, source-scan results, or substrate material.

## Track/Plane Checks

| Check | V2 result | Fold action |
|---|---|---|
| Track 1 vs Track 2 separation | ACCEPT. Parse and direct tables keep generated runtime symbols separate from Track 2 hand-parser symbols. | Preserve separate Track 1/Track 2 columns in any consolidated fold. Do not summarize shared primitive leaves as shared substrate cost. |
| Direct vs typed | ACCEPT. Direct is 17/17 non-panic profile evidence; typed remains seven generated rows with ten missing product surfaces. | Keep typed missingness as a product-surface gap. Do not use direct hot leaves to infer typed coverage. |
| CSS plane | ACCEPT. CSS is a separate declaration-values profile signal and not gate admission. | Keep CSS in its own plane with `profile_signal_not_gate_admission` unless a later prompt/gate extends the schema. |
| Structural scan | ACCEPT. Structural scalar/SIMD are captured as mode-III probes and explicitly fenced from REDRESS 96/97/98 reopening. | Use structural-scan facts as scanner micro-results only unless a future materially differentiated route proves row movement. |
| Sidecar substrate risk | ACCEPT. `.json.syms.json` sidecars are offline symbol metadata. | Preserve "symbol sidecar" wording as metadata only; avoid "event sidecar", "cursor sidecar", or "substrate sidecar" language. |

## Required Fold Actions

1. Carry V2's REDRESS 96/97/98 guardrail into the consolidated S-P1 fold: structural SIMD/scalar differentials are antecedent profile facts, not a union-substrate route.
2. Keep direct, typed, and CSS as distinct planes. Direct hot-leaf coverage does not fill typed gaps, and CSS declaration-values remains cross-plane profile/equality telemetry rather than JSON profile convergence.
3. Preserve Track 1/Track 2 symbol-path separation in every summary table; shared named leaf functions may be cited only as shared code leaves, not as proof of a shared substrate.
4. Label `.json.syms.json` files as offline symbol-resolution metadata wherever cited. They are not event vectors, retained cursors, parser state, or a second source scan.
