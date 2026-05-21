# SK-V13 S-P1 V3 CH5: Hidden Coupling/Substrate Review

Pass: S-P1 Profile. Cycle: V3.
Date: 2026-05-21.
Scope: adversarial CH5 review of the six S-P1 V3 fold artifacts under `restart/skinny/tranches/sk-v13/research/p1/`.
Output: this file.
Lens: CH5 HIDDEN COUPLING - substrate union, Track 1/Track 2 separation, structural-scan-only treatment, direct/typed/CSS plane separation, sidecar metadata, and union-substrate boundary hygiene.

## Disposition

ACCEPT.

V3 preserves the V2 CH5 boundary fixes. The fold does not normalize Track 2 as generated-runtime substrate, does not use direct profiles to fill typed product gaps, does not promote CSS telemetry into JSON parser evidence, does not convert structural-scan-only measurements into a retained cursor or event vector, and does not treat `.json.syms.json` sidecars as substrate material. The V3 evidence ledger strengthens the boundary by classifying all rows as `profile_signal_not_gate_admission` and by repeating the REDRESS guardrails for structural cursors, event sidecars, and orphan SIMD routes.

## Evidence

### CH5-HC-001 - Track 1 and Track 2 remain structurally separated

- Authority: `PASS-1-PROFILE.md:148`-`153` requires hot-leaf attribution not to imply a parallel substrate and states that Track 1 is generated runtime while Track 2 is structurally independent.
- Evidence: `p1a-samply-mode-1.md:5` scopes parse profiling as Track 1 generated JSON plus Track 2 independent hand JSON parser, and `p1a-samply-mode-1.md:139` states that Track 1 leaves come from `runtime::generated_json::*` while Track 2 leaves come from `bbnf_bench::track2::json::*`.
- Evidence: `p1b-samply-mode-2.md:72`-`90` keeps separate Track 1 and Track 2 direct columns. The shared `parse_that_regex::unescape_string` leaf on `unicode_escapes` is named as a shared code leaf at `p1b-samply-mode-2.md:119`-`121`, not as proof of a shared substrate.
- CH5 assessment: ACCEPT. V3 keeps Track 2 as independent product evidence and does not fold hand-parser symbols back into generated-runtime attribution.

### CH5-HC-002 - Direct, typed, and CSS planes are not collapsed

- Evidence: `p1b-samply-mode-2.md:92`-`94` preserves typed evidence as the V1 seven-row generated typed subset and says V2 did not invent rows for ten unsupported corpora. `p1b-samply-mode-2.md:125`-`126` repeats that missing typed rows are product-surface gaps, not profiling omissions.
- Evidence: `p1f-results-delta.md:48`-`55` separates JSON parse, direct, typed, mode-III, CSS declaration-values, and remaining CSS parity rows. `p1f-results-delta.md:102`-`111` counts JSON profile coverage and CSS rows separately.
- Evidence: `p1e-hot-leaf-attribution.md:79`-`83` reports CSS declaration-values as timer/fact-sink dominated, and `p1f-results-delta.md:122`-`125` says CSS V2 throughput is a hot-leaf/equality signal with `profile_signal_not_gate_admission` classification.
- Evidence: `support/evidence-ledger-v3.md:16`-`17` distinguishes `json-typed-only` from `css-profiled-nonparser-overhead`, and `support/evidence-ledger-v3.md:23` makes every ledger row `profile_signal_not_gate_admission`.
- CH5 assessment: ACCEPT. Direct hot leaves are not used as typed coverage, typed rows remain generated-product-limited, and CSS remains a separate telemetry/equality plane.

### CH5-HC-003 - Structural scan is measured directly and fenced from union-substrate reopening

- Evidence: `p1c-samply-mode-3.md:11` records 17/17 JSON corpora x 5 captured probes, including structural scalar and structural SIMD. `p1c-samply-mode-3.md:103`-`111` marks structural scalar/SIMD as measured probes, while unsupported PEXT and dispatch-table probes are explicitly routed out.
- Evidence: `p1c-samply-mode-3.md:115`-`117` says structural SIMD beating scalar scan is a scanner micro-result and does not by itself reopen REDRESS 96/97/98. `p1d-pmu-cycles.md:131`-`133` carries the same PMU-side guardrail.
- Evidence: `support/evidence-ledger-v3.md:90`-`91` classifies structural scalar/SIMD as JSON scan primitive candidates only, with structural SIMD limited to a scanner micro-result and REDRESS 96/97/98 not reopened.
- CH5 assessment: ACCEPT. Structural scan remains a mode-III measurement lane. V3 does not backfill parse/direct/typed evidence from it and does not claim a retained cursor, event sidecar, or second source scan.

### CH5-HC-004 - Sidecars remain offline symbol metadata

- Evidence: `p1a-samply-mode-1.md:80` describes joining saved profile leaf samples to matching `.json.syms.json` sidecars because save-only profiles are not cleanly symbolicated. `p1b-samply-mode-2.md:68`-`70` and `p1e-hot-leaf-attribution.md:47`-`51` describe the same offline RVA-to-symbol extraction path.
- Evidence: `support/evidence-ledger-v3.md:20` defines `function-only-sidecar` as a sidecar-resolved function name without source file:line, not precise primitive attribution. `support/evidence-ledger-v3.md:80`-`83` keeps mode-III rows function-only where the V2 sidecar lacks source file:line.
- Evidence: `p1e-hot-leaf-attribution.md:109`-`111` says ASM and system sidecar limitations are explicit, not silently resolved.
- CH5 assessment: ACCEPT. Sidecars are metadata for offline symbol resolution only. V3 does not describe them as parser events, retained cursor state, source-scan output, or substrate material.

### CH5-HC-005 - Union-substrate history remains binding

- Authority: `PASS-1-PROFILE.md:263`-`266` treats offset tape, lazy materialisation counters, and structural projection as one Lock 1 substrate that must not be split into hidden parallel producers.
- Evidence: `skinny/REDRESS.md:2797`-`2848` rejects the class-column plus move-consumed structural-index implementation, `skinny/REDRESS.md:2852`-`2906` rejects the allocation-free streaming-cursor implementation, and `skinny/REDRESS.md:2910`-`2950` retires `G-W3-UNION-SUBSTRATE` for SK-V9 after both faithful implementations regressed.
- Evidence: `p1a-samply-mode-1.md:141`-`145`, `p1b-samply-mode-2.md:127`-`134`, `p1c-samply-mode-3.md:128`-`133`, `p1e-hot-leaf-attribution.md:112`-`115`, and `p1f-results-delta.md:130`-`131` carry pre-block and zero-orphan route guards forward.
- Evidence: `support/evidence-ledger-v3.md:117`-`128` states that direct profile signals do not reopen REDRESS 119/120, dispatch/masking/tiny-string/unescape signals do not reopen parser-local structural cursors or event sidecars, and function-only ASM leaves do not create orphan SIMD primitives.
- CH5 assessment: ACCEPT. V3 records antecedent profile facts without turning them into a union-substrate route or a sidecar/cursor implementation plan.

## Track/Plane Checks

| Check | V3 result | Required carry-forward |
|---|---|---|
| Track 1 vs Track 2 separation | ACCEPT. Parse and direct artifacts retain separate generated-runtime and hand-parser symbol paths. | Preserve separate Track 1/Track 2 columns in consolidated folds. Shared named leaves are shared code, not shared substrate. |
| Direct vs typed | ACCEPT. Direct is 17/17 measured profile evidence; typed remains 7/17 generated product coverage with ten missing product surfaces. | Do not use direct hot-leaf coverage to infer typed coverage. |
| CSS plane | ACCEPT. CSS declaration-values is equality/profile telemetry and parser hot leaf remains unresolved. | Keep CSS as `profile_signal_not_gate_admission` unless a later gate changes the schema. |
| Structural scan | ACCEPT. Structural scalar/SIMD are mode-III scanner micro-results with REDRESS 96/97/98 explicitly preserved. | Use structural facts as measurements only unless a future materially different route proves row movement. |
| Sidecar metadata | ACCEPT. `.json.syms.json` sidecars are offline symbol-resolution metadata with explicit function-only limitations. | Avoid event-sidecar, cursor-sidecar, substrate-sidecar, or second-source-scan wording. |
| Union-substrate boundary | ACCEPT. V3 does not reopen the retired union-substrate thesis or create orphan SIMD authority from line-poor leaves. | Keep REDRESS 96/97/98 and REDRESS-126 guards attached to future structural/SIMD interpretations. |

## Required Fold Actions

1. Carry forward V3 ledger vocabulary exactly: every row remains `profile_signal_not_gate_admission` until a later gate-consuming wave changes that status.
2. Keep direct, typed, CSS, and mode-III structural facts in separate planes. Do not summarize them as one parser substrate cost.
3. Preserve Track 1/Track 2 symbol-path separation, including for shared `parse_that_regex` leaves.
4. Treat `.json.syms.json` sidecars as offline symbol metadata only. They are not event vectors, retained cursors, parser state, source scans, or substrate material.
5. Preserve REDRESS 96/97/98 and REDRESS-126 as binding history for structural cursor, union-substrate, and orphan SIMD interpretations.
