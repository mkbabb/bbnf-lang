# Omega-A Coherence Audit - Pass Omega V5 W5R

Pass: Pass Omega V5.
Date: 2026-05-26.
Scope: REDRESS-209 / W5R amended SK-V14 wave graph.
Boundary: audit only; no V1 or SK-V14 dispatch surface is edited by this
artifact.

## Verdict

ACCEPT-WITH-REQUIRED-SURFACE-AMENDMENTS.

W5R resolves the current W5 contradiction by splitting generator capability
from provider/template deletion. W5A becomes the source-consuming runtime
generator contract and CSS source parser wave. W5B becomes the provider/template
deletion and Lock 14 baseline-close wave. W6 remains the Pattern H runtime
collapse after W5B.

## Pre-Amendment Gap

Current W5 requires:

- replacing eight per-grammar provider modules with one grammar-agnostic
  generator template consuming grammar source plus workspace metadata;
- deleting seven CSS provider modules and seven CSS template directories in the
  same replacement slice;
- running `cargo xtask regen-css`, seven `check-css-l4-*` companions, provider
  count zero, CSS template count zero, and Lock 14 grep gates.

Current HEAD cannot meet the source-consuming requirement:

- `skinny/xtask/src/regen.rs:18` and `:32` call
  `codegen::emit_runtime_profile(target.profile)`.
- `skinny/xtask/src/regen.rs:61-74` hashes source/metadata bytes but does not
  pass them to codegen.
- `skinny/crates/codegen/src/lib.rs:1-10` imports the static provider modules,
  and `:162-210` matches `RuntimeProvider` variants.
- `skinny/crates/grammar/src/lib.rs:80-99` accepts only `@import` and `@token`;
  `:196-231` has no value-projection or span-capture atom.
- CSS L4 source uses unsupported syntax, including
  `grammar/css/l4/values.bbnf:37` (`->`) and `values.bbnf:67-69` (`@{...}`).

Static centralization would remove path names but preserve hand-written
per-profile runtime bodies. That is not a Lock 14 close.

## Amended Graph

The proposed graph is acyclic:

`W0 -> W1 -> W2 -> W3 -> W4-ledger -> W5A-generator-contract -> W5B-provider-delete -> W6.0..W6.8 -> W7 -> W8/W9/W10 -> W11`

W8/W9/W10 remain globally blocked until PRUNE-1 through PRUNE-5 close. W9 and
W10 local entry gates must be clarified as local prerequisites only.

## Required Surface Amendments

| Surface | Disposition |
|---|---|
| `restart/skinny/tranches/sk-v14/SPEC.md` | Required. Split W5 into W5A/W5B, move provider/template deletion to W5B, make W6 conditional on W5B, and fix W8-W10 global block wording. |
| `restart/skinny/tranches/sk-v14/SYNTHESIS.md` | Required. R3/C-1/P-6 wording must distinguish generator capability from deletion close. |
| `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` | Required. R3 PRUNE-3 wording must match W5A/W5B. |
| `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` | Required. Add W5R guard and forward CH3/CH5 wave-graph-cycle addendum. |
| `restart/MASTER-PLAN.md` | Required. Update §13.3 W5/W6 sequencing. |
| `restart/HANDOFF.md` and `restart/MIGRATION.md` | Required. Record REDRESS-209 and route next move to amended W5A after G-Omega V5 CRUD. |
| `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER}.md` | Required limited wording alignment. BENCH/SUBSTRATE read no-op. |
| `restart/ARCHITECTURE.md` | Read/no-op. W5R changes wave ownership and generator capability sequencing, not architecture. |
| `restart/locks/LOCKS.md` | Read/no-op. Existing Lock 14 already requires the corrected shape. |

## Gate Binding

Until G-Omega V5 authorizes W5R, do not patch dispatch surfaces, do not delete
CSS provider/template directories, and do not dispatch another W5 implementation
attempt.
