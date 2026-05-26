# Pass Omega V5 Proposed Master / SPEC Diff

Status: proposed only.
Do not apply before G-Omega V5 authorization.

## Summary

REDRESS-209 rejects current W5 because the wave requires provider/template
deletion and Lock 14 closure before a source-consuming runtime generator exists.
The proposed amendment splits W5 into W5A generator capability and W5B
provider/template deletion.

## `restart/MASTER-PLAN.md` §13.3

Replace W5/W6 row intent with:

```text
| W5A | §8 | PRUNE-3A — source-consuming runtime generator contract: pass grammar source + workspace metadata into codegen, make CSS L4 source parseable for runtime generation, migrate regen-css to the new path; no provider/template deletion. | Conditional on W4 ledger close | <=1.4k C-1 part-A source/test LOC unless split narrower in SPEC | <=90 min |
| W5B | §8B | PRUNE-3B — provider/template deletion and Lock 14 baseline close: delete seven CSS provider modules and seven template dirs only after W5A replacement is load-bearing; retire old provider mesh; run regen-css and companions. | Conditional on W5A close | source/test LOC named by SPEC | <=90 min |
| W6 | §9 | PRUNE-4 — 9 sub-waves: W6.0 CSS L4 root-runtime collapse, then remaining Pattern H collapses. | Conditional on W5B close | <=2.0k C-1 part-B aggregate | <=90 min per sub-wave; aggregate <=810 min |
```

## `restart/skinny/tranches/sk-v14/SPEC.md`

Section 8 W5 changes:

- Rename W5 to PRUNE-3A generator capability.
- Keep owner paths to codegen, grammar parser/runtime-generation parser,
  `regen_css.rs`, and Lock 14 baseline temporary W5A guard.
- Remove provider/template deletion from W5A.
- Exit gate becomes source/metadata consumed by codegen, CSS L4 source surface
  parseable for runtime generation, at least one CSS L4 profile emitted through
  the source-consuming path with no static provider/template dependency,
  `regen-css` and seven companions passing through the migrated path, and no new
  provider/template directories added.

Add Section 8B W5B:

- Owner paths: old CSS providers/templates, old provider mesh, `lock14_baseline.rs`,
  and W5B close docs.
- Entry gate: W5A admitted.
- Tasks: delete seven CSS providers and seven template directories, retire old
  provider mesh once no profile consumes it, update Lock 14 baseline to post-W5
  forward invariant, run `regen-css` and all seven companions.
- Exit gate: provider count zero, CSS template dir count zero, Lock 14 grep
  clean, `regen-css` clean, all seven companions pass.

Section 9 W6 changes:

- Entry gate changes from W5 admitted to W5B admitted.
- W6.0 CSS L4 root-runtime collapse remains unchanged.

W9/W10 entry gates:

- Add "local prerequisite only; global PRUNE gate still applies" to W9 and W10.

Downstream effects:

- Replace any "W8/W9/W10 may proceed independently of W5" wording with:
  "W8/W9/W10 remain globally blocked until PRUNE-1 through PRUNE-5 close."

## `restart/skinny/tranches/sk-v14/SYNTHESIS.md`

R3/C-1/P-6 wording changes to:

```text
PRUNE-3 splits into W5A generator capability and W5B provider/template deletion.
W5A proves source-consuming runtime generation; W5B deletes provider/template
clusters only after the replacement generator is load-bearing. Static
centralization of hand-written CSS runtime bodies is rejected as P-6 recurrence.
```

## `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`

R3 PRUNE-3 wording follows the same split:

- PRUNE-3A = source-consuming generator contract + CSS source parser support +
  `regen-css` migration.
- PRUNE-3B = provider/template deletion + Lock 14 baseline close.

## `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`

Add a W5R guard:

```text
Before W5A, W5B, W6, W7, W8, W9, or W10 dispatch, verify Pass Omega V5
G-Omega closed, CRUD applied, and amended W5A/W5B sequencing is in SPEC.
Provider/template deletion is forbidden before W5A admits.
```

Add the forward procedural addenda:

```text
NEW-CH3-V4-01: CH3 must grep delete-target / rebuild-capability pairs and
assert rebuild capability precedes deletion.
NEW-CH5-V4-01: CH5 must treat provider/template/runtime deletion as coupled to
the code path compiling the same-wave consumer.
```

## Handoff / Migration / Skinny Corpus

Patch obligations:

- `restart/HANDOFF.md`: record REDRESS-209, W5R gate, and next dispatch W5A.
- `restart/MIGRATION.md`: add V5 W5R receiver and W5A/W5B routing.
- `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER}.md`: align active
  authority and refusal posture.
- `restart/skinny/{BENCH,SUBSTRATE}.md`: read/no-op unless local wording drift
  is found during CRUD.
- `skinny/REDRESS.md`: after authorization, add only a narrow supersession note
  for REDRESS-209; keep the historical rejection intact.
