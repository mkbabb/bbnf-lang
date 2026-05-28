# SK-V15 T-P1 V4 Hardening Consolidated

Cycle: T-P1 Excavation V4.
Date: 2026-05-28.
Input inventories: `restart/audit/totality/p1/1A-substrate-evidence.md`
through `1F-coherence-scan.md`.
Inventory fold commit: `0c79c2b43`.
Hardening root: `restart/audit/totality/p1/hardening/V4/`.

## Verdict

ACCEPT-RATE: 6 / 7 = 85.7%.

Cycle verdict: REVISE. V4 folds the V3 defects across cost carriers,
primitive/kernel receiver enumeration, FNV/hash coupling, Lock 5 wording, and
CSS fact-stream paper-close wording. CH2, CH3, CH4, CH5, CH6, and CH7 all
ACCEPT. CH1 correctly blocks the cycle on bounded mechanical correctness
defects: residual non-root-resolving shorthand citations in the live
inventories and two stale prose references that still call V4 inventories V3.
No lens returned REJECT.

## Lens Dispositions

| Lens | Disposition | Output | Fold surface |
|---|---|---|---|
| CH1 CORRECTNESS | REVISE | `V4/CH1.md` | Expand residual shorthand citations in `1A` and `1F`; replace stale V3 self-description in `1A` and `1B`. |
| CH2 GENERALITY | ACCEPT | `V4/CH2.md` | D9/D10 remain grammar-neutral Lock 14 failures with non-JSON receivers; primitive rows are not demoted to JSON-only lessons. |
| CH3 REGRESSION | ACCEPT | `V4/CH3.md` | `NEW-CH3-V5-01`, REDRESS-183/184/209..213, and delete/rebuild guards remain load-bearing. |
| CH4 COST | ACCEPT | `V4/CH4.md` | V4 receiver cost carrier and primitive/kernel receiver table discharge the V3 class-level cost gaps. |
| CH5 HIDDEN COUPLING | ACCEPT | `V4/CH5.md` | Current-source FNV census and expanded sidecar/hash grep guard are present without laundering hashes into value, substrate, or equality proof. |
| CH6 ANTI-PAPER-CLOSE | ACCEPT | `V4/CH6.md` | 1E L05 and 1C CSS fact-stream are scoped partials; generated provenance and UNKNOWN routes remain open. |
| CH7 OVERFIT-PRUNE / GATE-EXCLUSION | ACCEPT | `V4/CH7.md` | No stale W8R, x86, PMULL/CSSC, FNV, sidecar, gate-exclusion, primitive, or header-only close route is introduced. |

## Deduplicated V5 Fold Roster

| id | required fold | target files |
|---|---|---|
| T-P1-V5-F01 | Expand `json/scan.rs:1` and `json/sink.rs:1` into full repo-root path:line citations. | `1A-substrate-evidence.md` |
| T-P1-V5-F02 | Expand COH-016 FNV runtime/template citations so no `generated.rs:<line>` or bare `:<line>` citation remains. Either list all seven CSS runtime paths or cite a root-resolving transcript that proves all seven profiles share the line positions. | `1F-coherence-scan.md` |
| T-P1-V5-F03 | Replace stale V3 self-description in live V4 prose. | `1A-substrate-evidence.md`, `1B-codegen-evidence.md` |
| T-P1-V5-F04 | Re-run CH1 mechanical checks and require zero residual shorthand, brace-path, and stale V3-prose matches. | verification transcript in the V5 fold / CH context |

## Accepted V4 Surfaces To Preserve

- `P1-1B-D9` / `P1-1B-D10` remain grammar-neutral Lock 14 proof receivers,
  not JSON-only empirical lessons.
- The V4 receiver cost carrier `RC-01` through `RC-11` remains bounded by
  owner path/row, LOC range, risk, wave, hard cap, consumer/proof, and
  disposition.
- The primitive/kernel receiver table remains source-present and non-admitting:
  scalar oracle, aarch64 or scalar-delegate implementation, strict parity, and
  same-wave consumer proof are required before any SIMD/ASM admit.
- The FNV/hash census is telemetry/quarantine evidence only; it is not CSS
  Value API proof, retained document identity, same-substrate evidence, or a
  production equality arbiter.
- Generated-header and Pattern H evidence remains open until generated
  ownership plus delete/regenerate or check proof exists.

## Verification

Commands executed or reported during V4:

```sh
rg -n -o '`[^`]*(?:\.md|\.rs|\.toml):[0-9][^`]*`' restart/audit/totality/p1/1*.md \
  | rg -v ':`(restart|skinny|crates|xtask)/|:`Cargo\.toml:|:`skinny/Cargo\.toml:'

rg -n -o '`:[0-9][0-9]*(?:-[0-9][0-9]*)?`' restart/audit/totality/p1/1*.md

rg -n '`[^`]*\{[^`]*\}[^`]*\.(rs|md|toml):[0-9][^`]*`' restart/audit/totality/p1/1*.md

rg -n 'Cycle is V3|this V3 inventory' \
  restart/audit/totality/p1/1A-substrate-evidence.md \
  restart/audit/totality/p1/1B-codegen-evidence.md \
  restart/audit/totality/p1/1C-runtime-evidence.md \
  restart/audit/totality/p1/1D-skinny-lessons.md \
  restart/audit/totality/p1/1E-locks-evidence.md \
  restart/audit/totality/p1/1F-coherence-scan.md
```

Observed: CH1's first two checks still report the `1A` / `1F` shorthand cites
and stale V3 prose listed in `V4/CH1.md`. The brace-path check returns zero.
1A and 1F count reconciliation pass.

## Next Dispatch

Fold `T-P1-V5-F01` through `T-P1-V5-F04` into the live inventories, then run
a fresh CH1-CH7 V5 hardening cycle. Because V4 is REVISE, T-P1 cannot lock in
V5 unless the orchestration contract explicitly accepts an exceptional
single-cycle close; under the normal §3Z rule, V5 must at minimum close all V4
orphans and then the pass needs two consecutive clean cycles.
