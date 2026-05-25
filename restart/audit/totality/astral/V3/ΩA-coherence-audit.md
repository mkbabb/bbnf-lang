# Omega-A Coherence Audit - Pass Omega V3 W2R

Pass: Pass Omega V3.
Date: 2026-05-25.
Scope: W2R amended SK-V14 wave graph acyclicity and required receiver surfaces.
Boundary: audit only; no V1 or SK-V14 dispatch surface is edited by this artifact.

## Verdict

ACCEPT-WITH-REQUIRED-SURFACE-AMENDMENTS.

W2R removes the current SK-V14 graph cycle by moving root
`crates/core/src/runtime/css_l4/` generation out of W2 and into W6.0, after W5
creates the generic generator substrate. Current HEAD remains internally
contradictory until G-Omega authorizes the patch set and CRUD applies it.

## Pre-Amendment Cycle

Current W2 requires `cargo xtask regen-css` to emit both
`skinny/crates/runtime/src/grammars/css_l4_*` and
`crates/core/src/runtime/css_l4/` (`SPEC.md:484`-`:491`). The root CSS L4
runtime tree is currently seven hand-written Pattern H runtime files, part of
the 67-file Pattern H census (`ARCHITECTURE.md:1800`-`:1825`). MIGRATION routes
generated-provider collapse to W5 and Pattern H runtime-root collapse to W6
(`MIGRATION.md:44`-`:45`).

That creates this prerequisite cycle:

`W6 -> W2 -> W3 -> W4 -> W5 -> W6`

Evidence:

| Edge | Anchor | Reason |
|---|---|---|
| W6 -> W2 | `SPEC.md:484`-`:491`; `MIGRATION.md:44`-`:45`; `ARCHITECTURE.md:1800`-`:1825` | W2 requires a W6-owned root runtime generator. |
| W2 -> W3 | `SPEC.md:530`-`:533` | W3 requires W2 admitted. |
| W2 + W3 -> W4 | `SPEC.md:583`-`:586` | W4 requires W2 and W3 admitted. |
| W4 -> W5 | `SPEC.md:643`-`:647` | W5 requires W4 admitted. |
| W5 -> W6 | `SPEC.md:705`-`:708` | W6 requires W5 admitted. |

REDRESS-183 records the same failure: no current generator restores
`crates/core/src/runtime/css_l4/`, so W2 is rejected and no later SK-V14
implementation wave is legally dispatchable from that state
(`skinny/REDRESS.md:5090`-`:5093`).

## Amended Graph

W2R removes only the W6 -> W2 back-edge:

- W2 owns skinny-side `regen-css` only: `skinny/crates/runtime/src/grammars/css_l4_*`.
- W6.0 owns root CSS L4 runtime collapse: `crates/core/src/runtime/css_l4/`.
- W6 remains nine sub-waves under the existing per-sub-wave and aggregate caps.
- W4 still depends on W2, but W2 admission now means skinny-side generation only.

A dispatch-conservative topological order exists:

`W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6.0 -> W6.1..W6.8 -> W7 -> W8 -> W9 -> W10 -> W11`

The global PRUNE-before-new-admit rule still controls W8/W9/W10. Their local
technical entry gates do not authorize dispatch before PRUNE-1..PRUNE-5 close
(`SYNTHESIS.md:29`-`:33`, `SYNTHESIS.md:291`-`:294`).

## Required Surface Amendments

| Surface | Disposition |
|---|---|
| `restart/skinny/tranches/sk-v14/SPEC.md` | Required. Replace W2 dual-tree wording with skinny-only generation; replace the contradictory downstream note; renumber W6 sub-waves to W6.0..W6.8 with CSS L4 first; preserve W10 Stage-0 text. |
| `restart/skinny/tranches/sk-v14/SYNTHESIS.md` | Required. Replace stale C-1/C-3 text that still says 64 files / 8 sub-waves and dual-tree W2 round-trip with 67 files / 9 sub-waves and W2 skinny-side + W6.0 root-runtime split. |
| `restart/MASTER-PLAN.md` | Required. Update §13.3 W2 and W6 rows and add a W2R receiver note. |
| `restart/HANDOFF.md` | Required. Record the W2 rejection and block W2 rerun / W3+ dispatch until G-Omega authorizes W2R. |
| `restart/MIGRATION.md` | Required. Amend the Pattern H receiver row so CSS L4 root runtime is explicitly W6.0, not W2. |
| `restart/ARCHITECTURE.md` | No-op. It already records the 67-file Pattern H state and root CSS L4 runtime as hand-written. |
| `restart/locks/LOCKS.md` | No-op. W2R changes wave ownership, not lock semantics. |

## Gate Binding

Before any SPEC or V1 surface patch merges, G-Omega must present REDRESS-183,
W2R, CHALLENGE verdict, zero-delta locks diff, SPEC / MASTER / HANDOFF /
MIGRATION / skinny-corpus proposed diffs, and CRUD operations. Until G-Omega
authorizes the amendment, do not rerun W2 and do not dispatch W3 or later waves.
