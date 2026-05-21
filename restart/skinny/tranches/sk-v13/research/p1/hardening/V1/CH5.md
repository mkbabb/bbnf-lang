# SK-V13 S-P1 V1 CH5: Hidden Coupling/Substrate Review

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-21.
Scope: adversarial CH5 review of the six S-P1 V1 artifacts under `restart/skinny/tranches/sk-v13/research/p1/`.
Output: this file.
Lens: CH5 HIDDEN COUPLING - substrate union, Track 1/Track 2 separation, structural-scan-only treatment, direct/typed/CSS plane separation, and hot-leaf attribution sidecar/cursor implications.

## Disposition

REVISE.

The V1 packet mostly preserves Track 1 generated-runtime evidence apart from Track 2 independent-parser evidence, and P1-C correctly refuses to paper-close missing mode III/structural-scan profiles. It is not clean enough for ACCEPT because one artifact normalizes a Track 1/Track 2 inversion through a "shared tape substrate" explanation without evidence, and the extraction/hot-leaf packet lets CSS/direct/typed telemetry sit close enough to JSON profile authority that the fold needs explicit plane guards.

## Findings

### CH5-HC-001 - Do not infer a shared tape substrate from Track 1/Track 2 c/B inversions

- Evidence: `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:170` says the `unicode_mixed` and `unicode_escapes` parse inversions "may separate generated parser overhead from shared tape substrate cost."
- Why this is CH5: Track 1 is generated runtime and Track 2 is structurally independent. A "shared tape substrate" explanation implies hidden substrate commonality across those tracks. The profile packet does not cite a measured shared tape surface, retained cursor, or event vector that would justify that normalization.
- Required fold action: replace the inference with a Lock 1 observation. The folded P1-D/P1-E wording should say only that the inversion is a measured Track 1/Track 2 difference requiring follow-up; it must not attribute the delta to shared tape cost unless a later artifact cites a concrete shared substrate path and file:line.

### CH5-HC-002 - CSS telemetry must remain a separate plane, not a JSON S-P1 admission/classification surface

- Evidence: `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:5` scopes P1-E as "JSON parse/direct/typed rows plus CSS L4 declaration-values measurement"; `p1e-hot-leaf-attribution.md:81`-`85` then records CSS as an unprofiled throughput row. `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:40`-`52` mixes the admitted CSS row into the extracted surface, and `p1f-results-delta.md:112`-`114` assigns it a fresh class `A` while also warning that the harness differs from the W1b Criterion gate.
- Why this is CH5: CSS is not Track 1 JSON parse/direct/typed substrate evidence. P1-E does keep the CSS row unresolved, and P1-F flags stale methodology, but the fold must prevent the CSS row from being normalized into JSON profile convergence or sidecar-equivalent hot-leaf coverage.
- Required fold action: in V2, move CSS to an explicitly labeled cross-plane appendix or mark it `CSS telemetry only; no S-P1 JSON profile admission`. Do not count CSS toward JSON corpus/profile convergence, and do not use schema-v3 JSON row classes for CSS unless the prompt explicitly extends the plane schema.

### CH5-HC-003 - Structural-scan-only is correctly treated as missing profile evidence, but the fold must preserve that boundary

- Evidence: `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:53`-`61` refuses to make hot-leaf claims for absent mode III profiles and records only source-definition facts. `p1c-samply-mode-3.md:87`-`89` says adjacent parse/direct/typed PMU data must not be promoted into P1-C coverage. `p1c-samply-mode-3.md:95`-`99` requires 17/17 structural/masking captures and names the Lock 1 risk if structural scan is treated as a separable sidecar.
- Why this is CH5: this is the correct substrate-boundary behavior. The structural scan is not normalized into a second source scan or sidecar event vector in V1.
- Required fold action: carry this boundary forward verbatim. V2 must capture structural-scan-only as its own lane with 17/17 artifact paths or keep it absent; it must not backfill structural evidence from parse/direct/typed rows.

### CH5-HC-004 - Hot-leaf sidecars are symbol-resolution aids only, not event vectors

- Evidence: `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:67` states that sidecars were used to resolve saved profile addresses because the profiles were not cleanly symbolicated. `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:37`-`46` describes joining leaf samples to `.json.syms.json` sidecars, and `p1e-hot-leaf-attribution.md:143`-`147` lists symbol sidecars as sources.
- Why this is CH5: the wording does not currently claim a sidecar event vector, retained cursor, or second source scan. The risk is fold-cycle drift: sidecar-derived symbols must not become a runtime substrate claim.
- Required fold action: V2 should label `.json.syms.json` as offline symbol-resolution metadata wherever cited. Do not describe sidecars as parser events, cursor state, or substrate evidence.

## Track/Plane Checks

| Check | V1 result | Required fold action |
|---|---|---|
| Track 1 vs Track 2 separation | Mostly preserved. P1-A separates `runtime::generated_json::*` from `bbnf_bench::track2::json::*` at `p1a-samply-mode-1.md:73`-`91`; P1-B states the plane separation at `p1b-samply-mode-2.md:136`-`138`. | Remove/qualify the P1-D shared-substrate inference in CH5-HC-001. |
| Parallel substrate normalization | One unsafe inference in P1-D; P1-C otherwise guards against this for structural scan. | Convert inferred shared substrate language into Lock 1 observation language. |
| Structural-scan-only treatment | Correctly missing, not paper-closed. | Preserve as missing until 17/17 structural-scan artifacts exist. |
| Direct/typed/CSS plane separation | Direct and typed are separated; CSS is separated locally but classified too close to JSON extraction authority. | Fence CSS as non-JSON telemetry and do not count it toward S-P1 JSON convergence. |
| Hot-leaf sidecar/cursor implication | No retained cursor claim found; sidecars are used for symbol lookup. | Keep sidecars explicitly offline metadata. |

## Required Fold Actions

1. Revise P1-D language around `unicode_mixed`/`unicode_escapes` c/B inversions so it does not imply shared tape substrate across Track 1 and Track 2 without measured evidence.
2. Add a CSS plane guard to P1-E/P1-F: CSS L4 measurement is cross-plane telemetry only in this S-P1 packet, not JSON hot-leaf coverage or JSON schema-v3 admission.
3. Preserve P1-C's structural-scan-only boundary: missing mode III/structural profiles stay missing until captured directly.
4. Label `.json.syms.json` sidecars as offline symbol-resolution metadata in the fold, with no event-vector or retained-cursor interpretation.
