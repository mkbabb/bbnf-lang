# Pass Omega V4 Proposed Master / SPEC Diff

Status: proposed only.
Do not apply before G-Omega V4 authorization.

## Summary

REDRESS-184 rejects current W4 because provider deletion is sequenced before
the W5 replacement generator. The proposed amendment keeps W4 as the CSS
admit-ledger prune and moves CSS provider/template deletion into W5's existing
provider-collapse scope.

## `restart/MASTER-PLAN.md` §13.3

Replace W4/W5 row intent with:

```text
| W4 | §7 | PRUNE-2 — CSS L4 admit-ledger prune: restore rolling delta to
0/24 CSS L4 admitted and add 24 row-keyed REDRESS entries; no
provider/template deletion until W5 replacement exists. | Conditional on W2 +
W3 close | <=500 docs/ledger | <=90 min |
| W5 | §8 | PRUNE-3 — Lock-14 refactor plus CSS provider/template deletion:
trait dispatch + grammar-agnostic generator template; migrate regen_css.rs;
delete seven CSS provider modules and seven template dirs in the same commit as
the replacement; run regen-css and companions. | Conditional on W4 ledger close
| <=1.4k C-1 part-A source/test LOC | <=90 min |
```

Preserve W6 row from Pass Omega V3 W2R:

```text
W6.0 CSS L4 root-runtime collapse; W6.1-W6.8 remaining Pattern H dirs.
```

## `restart/skinny/tranches/sk-v14/SPEC.md`

Section 7 W4 changes:

- Owner paths become `restart/skinny/ROLLING-SOTA-DELTA.md`,
  `skinny/RESULTS.md` if a narrow status correction is needed,
  `skinny/REDRESS.md`, and W4 close docs.
- Remove provider/template/runtime-twin deletion from W4 tasks.
- Exit gate becomes CSS L4 0/24 in rolling delta, 24 REDRESS entries citing
  `v1 §1-6`, `skinny/RESULTS.md` retaining `AUDIT-FALSIFIED`, JSON rows
  maintained, and no CSS source/generator deletion.
- Downstream effect: W5 is unblocked only after W4 ledger close.

Section 8 W5 changes:

- Add owner paths:
  `skinny/crates/codegen/src/css_l4_*_provider.rs`,
  `skinny/crates/codegen/src/css_l4_*_templates/`,
  `skinny/crates/runtime/src/grammars/css_l4_*/`, and
  `skinny/xtask/src/regen_css.rs`.
- Add tasks:
  stand up generic provider path, migrate `regen_css.rs`, delete seven CSS
  provider modules and template dirs, regenerate/check seven CSS runtime
  profiles, and update Lock 14 baseline intentionally.
- Add exit gates:
  provider/template count zero, `cargo xtask regen-css` clean, all seven
  `check-css-l4-*` commands pass, and Lock 14 baseline passes.

## `restart/skinny/tranches/sk-v14/SYNTHESIS.md`

R3 and C-5 change from "PRUNE-2 delete 7 CSS templates + revert 24 CSS rows"
to:

```text
PRUNE-2 reverts the 24 CSS L4 admit claims in rolling delta and REDRESS.
PRUNE-3 deletes the seven CSS provider/template clusters only in the same wave
as the grammar-agnostic provider replacement.
```

P-1 remains unchanged as the anti-pattern; only the receiver wave changes.

## `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`

R3 PRUNE-2/PRUNE-3 wording follows the same split:

- PRUNE-2 = CSS row-ledger prune.
- PRUNE-3 = Lock 14 provider collapse plus CSS provider/template deletion.

## `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`

Add a W4R guard:

```text
Before W5 or any CSS provider/template deletion, verify Pass Omega V4
G-Omega closed, CRUD applied, and amended W4 ledger-only PRUNE closed.
```

Correct rolling-delta path references from `skinny/ROLLING-SOTA-DELTA.md` to
`restart/skinny/ROLLING-SOTA-DELTA.md` in the authority list and post-redress
update list.

## Handoff / Migration / Skinny Corpus

Patch obligations:

- `restart/HANDOFF.md`: replace stale "blocked until amended W2 re-admits" with
  W2/W3 admitted and W4R current blocker.
- `restart/MIGRATION.md`: add V4 W4R receiver and mark V3 W2 rejection block as
  superseded by amended W2 admission.
- `restart/skinny/{INDEX,WORKSPACE,HARDENING}.md`: align active authority,
  workspace deletion ownership, and hardening refusal posture.
- `skinny/REDRESS.md`: add a narrow REDRESS-183 supersession note; do not erase
  historical rejection evidence.
