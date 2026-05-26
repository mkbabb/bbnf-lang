# Omega-C Locks Amendments - Pass Omega V8 W5B-FRONTENDR

Date: 2026-05-26.
Scope: REDRESS-212 / W5B-FRONTENDR corrective route.
Disposition: NO LOCK AMENDMENT.

## Verdict

REDRESS-212 is a SPEC wave-graph and cap-accounting rejection, not a new locks
case. W5B-FRONTEND remains valid in goal but invalid in current execution shape:
the V2 plan needs formal W5B.0 through W5B.4 sub-waves and an honest maintain
gate authority before source redress can resume. No frontend/codegen/xtask source
redress was attempted or retained for W5B-FRONTEND
(`restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-redress.md:15`-`18`).

No amendment to `restart/locks/LOCKS.md` is required. The W5B-FRONTENDR
corrective packet explicitly routes the needed changes to SPEC, MASTER-PLAN,
dispatch, handoff, and skinny authority surfaces, while stating that LOCKS and
ARCHITECTURE remain read/no-op unless Omega-C finds an unexpected public syntax,
substrate, BackendShape, or Lock 14 amendment need
(`restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTENDR-corrective-packet.md:107`-`130`).
No such need was found.

## Audit Basis

- Pass Omega gives Ω-C authority to audit the 16 locks and produce a
  `LOCKS.md` amendment diff, while requiring G-Omega before any locks amendment
  merges (`restart/prompts/pass-contracts/PASS-OMEGA.md:30`,
  `restart/prompts/pass-contracts/PASS-OMEGA.md:67`,
  `restart/prompts/pass-contracts/PASS-OMEGA.md:98`-`103`).
- Pass Omega requires Lock 14 grammar-neutrality and the five-shape
  BackendShape canon to remain coherent, and keeps the 16-lock count accurate
  (`restart/prompts/pass-contracts/PASS-OMEGA.md:168`-`171`).
- V7 is direct precedent: REDRESS-211/W5B routing required executable SPEC and
  Lock 14 gate routing, but no `LOCKS.md` amendment
  (`restart/audit/totality/astral/V7/ΩC-locks-amendments.md:9`-`26`,
  `restart/audit/totality/astral/V7/locks-diff.md:3`-`22`).

## Invariant Checks

| Invariant | Result | Evidence |
|---|---:|---|
| 16-lock count | PASS | `grep -cE '^[0-9]+\\. \\*\\*' restart/locks/LOCKS.md` returned `16`; Lock 17 is still explicitly rejected as the CH7 carrier (`restart/locks/LOCKS.md:44`-`61`). |
| 5-shape BackendShape canon | PASS | Lock 1 says `FactStream` is a substrate-manifest category, not a sixth `BackendShape`, and names `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` as the search domain (`restart/locks/LOCKS.md:100`-`109`). Lock 10 repeats that a new `BackendShape`, directive, or BIR variant remains G-Omega gated (`restart/locks/LOCKS.md:269`-`274`). |
| Lock 14 generated-output / grammar-neutrality | PASS | Lock 14 already contains the generated-output allowance, regen round-trip binding, per-wave Lock 14 baseline gate, grammar-name/shape leak census, and non-JSON witness requirements (`restart/locks/LOCKS.md:349`-`390`). |
| No new public syntax | PASS | W5B-FRONTEND V2 challenge requires public-retirement tests for the full compatibility set, and the corrective packet lowers constructs into request-local facts without authorizing new public syntax (`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH2.md:17`-`24`, `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTENDR-corrective-packet.md:87`-`90`). |
| No new substrate, BIR, or BackendShape | PASS | The corrective packet's W5B.3 exit gate says no new public directive, BIR, BackendShape, or substrate variant; its risk statement says the correction does not change architecture, lock count, BackendShape canon, or W5C/W5D/W6 ownership (`restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTENDR-corrective-packet.md:89`, `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTENDR-corrective-packet.md:132`-`138`). |
| No W5B source owner-path delta retained | PASS | `git diff --exit-code HEAD -- skinny/crates/bbnf-bench/src/lock14_baseline.rs skinny/crates/grammar/src/lib.rs skinny/crates/codegen/src/grammar_provider.rs skinny/crates/codegen/src/lib.rs skinny/xtask/src/main.rs skinny/xtask/src/regen.rs skinny/xtask/src/regen_css.rs` returned clean. |
| No admit ledger or rolling-delta movement retained | PASS | `git diff --exit-code HEAD -- skinny/RESULTS.md restart/skinny/ROLLING-SOTA-DELTA.md` returned clean. |

## Lock 14 Disposition

The V2 challenge found missing Lock 14 execution detail, not missing Lock 14
text. CH1 requires exact W5B owner-path tests, W5C/W5D rejection, and
provider/template modification rejection
(`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH1.md:17`-`20`,
`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH1.md:40`-`46`).
CH2 requires a Lock 14 leak census over all W5B generic owner paths and
public-retirement tests for the full compatibility set
(`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH2.md:37`-`43`).
CH5 requires all `_templates` guards and a Lock14-only first checkpoint before
grammar/codegen/xtask frontend edits
(`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH5.md:30`-`38`).

Those are W5B.0/SPEC gate requirements. They do not widen Lock 14 because Lock
14 already requires grammar-neutral generated-output discipline and same-wave
baseline/leak-census enforcement for generic-crate, generated-provider,
template, decision-engine, runtime-root, and shared-consumer changes
(`restart/locks/LOCKS.md:377`-`390`).

## Required Non-Locks Fold

W5B-FRONTENDR must be handled outside Ω-C:

- Formalize W5B.0 through W5B.4 in SK-V14 SPEC with aggregate cap and final close
  semantics.
- Mirror the W5B sub-wave graph in MASTER-PLAN and dispatch/handoff surfaces.
- Dispatch W5B.0 LOCK14-GATE before any W5B frontend/codegen/xtask source owner
  path changes.
- Resolve the W5B maintain gate either by SPEC-authorized exact no-diff for this
  non-admit capability wave or by fresh full-table maintain evidence
  (`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/HARDENING-SKV14-W5B-FRONTEND-V2-CONSOLIDATED.md:39`-`58`,
  `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTENDR-corrective-packet.md:99`-`126`).

## Proposed Locks Diff

Zero delta. CRUD-3 is read/no-op.

```diff
# restart/locks/LOCKS.md
# no changes
```
