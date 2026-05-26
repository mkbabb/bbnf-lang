# SK-V14 W5 CHALLENGE Consolidated

Date: 2026-05-26.
Wave: W5.
Phase: CHALLENGE between Plan and Redress.
Status: ACCEPT REDRESS.

## Context

W5 is mandatory-CHALLENGE per `DISPATCH-PROMPT.md:222` because PRUNE-3 is a
Lock 14 refactor touching the provider/generator substrate. The challenged plan
is `skv14-W5-plan.md`: reject the current SPEC-shaped implementation rather
than centralize static CSS provider/template bodies.

Challenge inputs:

- W5 research files `skv14-W5-A-provider-dispatch.md`,
  `skv14-W5-B-regen-css-consumer.md`,
  `skv14-W5-C-template-commonality.md`, and
  `skv14-W5-D-lock14-governance.md`.
- W5 plan file `skv14-W5-plan.md`.
- Read-only explorer findings:
  - code-level generator/parser gap;
  - wave-graph/governance contradictions;
  - Omega V3/V4 gate precedent.

## CH1 Interface / Owner Surface

ACCEPT.

The plan preserves W5 owner boundaries. It does not touch `runtime/`, because
SPEC §8 makes root runtime W6 work. It does not delete provider/template paths,
because current `regen-css` still compiles through them. It routes the needed
owner-surface amendment to Pass Omega instead of silently changing SPEC.

## CH2 Evidence / Executability

ACCEPT.

The rejection is executable, not inferred:

- provider leak count: `5`;
- provider module count: `8`;
- CSS template dir count: `7`;
- CSS `.bbnf` source count: `15`;
- `cargo xtask regen-css` exits 0 through the static provider/template mesh;
- `cargo test -p grammar rejects_non_skinny_directives -- --nocapture` passes;
- a temporary parse probe against `grammar/css/l4/values.bbnf` returns
  `BBNF-PARSE: unexpected token '-' at byte 1362`.

The evidence matches source citations in `skv14-W5-plan.md` and confirms that
source/metadata are freshness inputs only.

## CH3 Regression / Wave Graph

ACCEPT WITH FORWARD FIX.

Rejecting W5 blocks W6 and W7 exactly as SPEC §8 says. The plan also catches a
stale contradiction: SPEC §8's downstream note says W8-W10 may proceed
independently of W5, but the manifest and prune-before-new-admit chain globally
block W8/W9/W10 until PRUNE-1 through PRUNE-5 close. Pass Omega V5 must amend
that downstream text and mark W9/W10 local entry gates as local prerequisites
only.

Forward-lens note: this is the third SK-V14 wave-graph failure pattern after
W2R and W4R. Future T-P3 CH3 should grep for "delete X" / "X exists after wave
N" / "rebuild X in wave M" pairs and assert rebuild capability precedes
deletion.

## CH4 Cap / Scope Control

ACCEPT.

Forcing W5 to build a CSS L4 source-consuming parser/generator plus delete the
provider/template mesh inside the current W5 cap is not credible. Splitting the
capability wave from the deletion wave is the minimal honest scope change.
Static centralization would be smaller, but it would be a path-count workaround
and would not satisfy Lock 14.

## CH5 Hidden Coupling

ACCEPT WITH FORWARD FIX.

The hidden coupling is precise: `regen-css` claims grammar-source inputs in the
target roster, but `regen.rs` only hashes them. Runtime emission still receives
`target.profile` and dispatches to static providers. The plan identifies this
delete-target/rebuild-capability coupling and routes it to Omega.

Forward-lens note: future T-P3 CH5 should require any wave deleting a provider,
template, runtime twin, or generated-output source to cite the earlier wave that
made the replacement path load-bearing.

## CH6 Concurrency / Orchestration

ACCEPT.

The W5 plan uses disjoint research and read-only explorer outputs. No sub-agent
edited shared files. The main thread owns the write set for challenge/redress and
keeps unrelated dirty JSON artifacts untouched.

## CH7 Overfit-Prune

ACCEPT.

The plan refuses the overfit-prone close: moving hand-written CSS scanner bodies
into one provider file would preserve profile-specific behavior while removing
profile-specific filenames. That would pass the easiest greps and fail the
gestalt Lock 14 requirement. REDRESS-209 is the correct outcome.

## Verdict

Proceed to W5 redress:

- record REDRESS-209;
- write `skv14-W5-redress.md`;
- write `skv14-W5R-corrective-packet.md`;
- update tranche handoff to route the next move to Pass Omega V5;
- stop at G-Omega V5 before any SPEC/MASTER/LOCKS/HANDOFF CRUD patch is applied.
