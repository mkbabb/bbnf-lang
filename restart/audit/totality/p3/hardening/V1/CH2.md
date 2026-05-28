---
challenge_agent: CH2
name: GENERALITY
pass: T-P3-synthesis
cycle: V1
verdict: ACCEPT
generated_at: 2026-05-28T03:41:23-04:00
owned_output: restart/audit/totality/p3/hardening/V1/CH2.md
---

# CH2 GENERALITY

## Verdict

ACCEPT. Lock 14 holds across the V1 packet. The packet does not narrow the
spec to JSON, does not accept CSS-only evidence as fleet proof, and gives
concrete non-JSON receivers for CSS L4 plus Sheets or BBNF-self. The 3C lock
diff preserves the 16-lock and five-`BackendShape` canon and adds no directive,
BIR variant, substrate, public substrate API, retained sidecar, lock, lock
retirement, or sixth shape.

The ACCEPT is not a paper close: it is limited to the proposal packet. CSS,
Sheets, and BBNF-self remain receiving proof obligations for later gates, not
closed implementation evidence.

## Evidence Commands And Outputs

```sh
git show --stat --oneline 0a0508acd -- restart/audit/totality/p3
```

```text
0a0508acd docs(sk-v15-t-p3): add V1 synthesis packet
 .../audit/totality/p3/3A-architecture-synthesis.md |  92 ++++++++++++
 .../totality/p3/3B-master-plan-reconciliation.md   | 167 +++++++++++++++++++++
 .../audit/totality/p3/3C-locks-crystallisation.md  | 114 ++++++++++++++
 restart/audit/totality/p3/3C-locks-v+1-diff.md     |  76 ++++++++++
 restart/audit/totality/p3/3D-skinny-fold.md        |  93 ++++++++++++
 .../audit/totality/p3/3E-grammar-generalisation.md | 145 ++++++++++++++++++
 restart/audit/totality/p3/3F-migration-handoff.md  | 120 +++++++++++++++
 7 files changed, 807 insertions(+)
```

```sh
git diff --check 0a0508acd^ 0a0508acd -- restart/audit/totality/p3
```

```text
# no output; exit 0
```

```sh
awk '/^```diff$/{in_diff=1; next} in_diff && /^```$/{exit} in_diff {print}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v1.diff
git apply --check /tmp/tp3-locks-v1.diff
```

```text
# no output; exit 0
```

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
```

```text
16
```

```sh
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
```

```text
      67
```

```sh
git diff --name-only 0a0508acd^ 0a0508acd -- \
  restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/locks/LOCKS.md \
  restart/HANDOFF.md restart/MIGRATION.md
```

```text
# no output; target packet does not edit live V1 spec surfaces
```

```sh
rg -n 'enum BackendShape|BackendShape::|BackendShape' \
  skinny/crates/ir/src/lib.rs skinny/crates/ir/src/cost.rs
```

```text
skinny/crates/ir/src/cost.rs:333:pub const fn all_backend_shapes() -> [BackendShape; 5] {
skinny/crates/ir/src/cost.rs:335:        BackendShape::EagerTape,
skinny/crates/ir/src/cost.rs:336:        BackendShape::OffsetTape,
skinny/crates/ir/src/cost.rs:337:        BackendShape::EventTape,
skinny/crates/ir/src/cost.rs:338:        BackendShape::SinkOnly,
skinny/crates/ir/src/cost.rs:339:        BackendShape::CollapsedStage,
skinny/crates/ir/src/lib.rs:340:pub enum BackendShape {
```

```sh
awk '/^```diff$/{in_diff=1; next} in_diff && /^```$/{exit} in_diff && /^\+[^+]/{print NR ":" $0}' \
  restart/audit/totality/p3/3C-locks-v+1-diff.md |
  rg -n "BackendShape|directive|BIR|substrate|API|sidecar|FactStream|CSS|Sheets|BBNF|future grammar|JSON"
```

```text
# selected matching output
40:+This addendum ... preserves ... `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`; it adds no directive, BIR variant, substrate, public substrate API, retained sidecar, lock, lock retirement, or sixth shape.
42:+- Lock 1 ... `FactStream` remains an output-plane/admitted-product category only, not a sixth BackendShape and not a retained internal sidecar.
56:+- Lock 10 ... all-five gate over exactly `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`. Adding a sixth shape, new directive, or new BIR variant is not a fix and remains G-Omega gated.
60:+- Lock 14 ... Future grammar onboarding is source/metadata-only through CSS plus Sheets or BBNF-self witnesses; no new directive, BIR variant, sixth BackendShape, or generic code branch is admissible. CSS value parsing may reuse byte kernels below a CSS scalar oracle, but JSON string/number semantic APIs are not the CSS parser.
```

## Findings

| id | status | target evidence | corroborating evidence | CH2 assessment |
|---|---|---|---|---|
| CH2-F1 | PASS | `restart/audit/totality/p3/3A-architecture-synthesis.md:54`, `restart/audit/totality/p3/3A-architecture-synthesis.md:57`, `restart/audit/totality/p3/3A-architecture-synthesis.md:59`, `restart/audit/totality/p3/3A-architecture-synthesis.md:61` | `restart/locks/LOCKS.md:349`-`400`; `restart/audit/totality/p2/2C-grammar-neutrality.md:47`-`55` | 3A preserves Lock 14 by replacing JSON/CSS branches with generated-provider discipline, treating CSS fact streams as diagnostic/output-plane evidence, and refusing runtime-regex or sixth-shape repairs. |
| CH2-F2 | PASS | `restart/audit/totality/p3/3B-master-plan-reconciliation.md:93`-`100`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:114`-`123`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:135`-`138` | `restart/skinny/tranches/sk-v15/SPEC.md:206`-`217`; `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-`31` | 3B reclassifies stale CSS rows as historical/refuted or diagnostic, adds SK-V15 W0-W11 receivers, routes CSS through W1/W5/W6, and keeps all-five lowerer proof inside the existing canon. |
| CH2-F3 | PASS | `restart/audit/totality/p3/3C-locks-crystallisation.md:25`, `restart/audit/totality/p3/3C-locks-crystallisation.md:40`, `restart/audit/totality/p3/3C-locks-crystallisation.md:49`, `restart/audit/totality/p3/3C-locks-crystallisation.md:86`, `restart/audit/totality/p3/3C-locks-crystallisation.md:90` | `restart/audit/totality/p3/3C-locks-v+1-diff.md:40`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:56`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:60`; `skinny/crates/ir/src/lib.rs:339`-`345`; `skinny/crates/ir/src/cost.rs:333`-`339` | 3C aligns with 3E: every 2C Lock 14 candidate is folded into D-L14/D-L16, `FactStream` is not a `BackendShape`, and the exact five shapes remain the search domain. |
| CH2-F4 | PASS | `restart/audit/totality/p3/3D-skinny-fold.md:48`-`51`, `restart/audit/totality/p3/3D-skinny-fold.md:57`-`65`, `restart/audit/totality/p3/3D-skinny-fold.md:80` | `restart/audit/totality/p2/2C-grammar-neutrality.md:35`-`55`; `restart/skinny/tranches/sk-v15/SPEC.md:187`-`204` | 3D preserves JSON as a scoped guard baseline only, carries CSS as a rejection/repair input, and routes non-JSON receiver proof to CSS plus Sheets or BBNF-self. |
| CH2-F5 | PASS | `restart/audit/totality/p3/3E-grammar-generalisation.md:34`-`49`, `restart/audit/totality/p3/3E-grammar-generalisation.md:64`-`74`, `restart/audit/totality/p3/3E-grammar-generalisation.md:76`-`117` | `restart/audit/totality/p2/2C-grammar-neutrality.md:72`-`75`, `restart/audit/totality/p2/2C-grammar-neutrality.md:140`-`149`; `restart/locks/LOCKS.md:377`-`400` | 3E is concrete for CSS L4 plus Sheets and BBNF-self, includes a per-grammar five-shape matrix, and defines future onboarding as source/metadata-only with fail-closed checks. |
| CH2-F6 | PASS | `restart/audit/totality/p3/3F-migration-handoff.md:41`-`46`, `restart/audit/totality/p3/3F-migration-handoff.md:65`-`73`, `restart/audit/totality/p3/3F-migration-handoff.md:104`-`111` | `restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:55`-`68`; `restart/skinny/tranches/sk-v15/SPEC.md:246`-`465` | 3F keeps implementation blocked until Pass Omega/G-Omega and maps Lock 14/16, CSS provider, Decision/lowerer, Pattern H, and FNV work to receiving gates rather than closing them by prose. |

## JSON/CSS Overfit Check

The packet does not overfit JSON evidence because JSON is repeatedly framed as a
scoped guard, not a fleet proof: `restart/audit/totality/p3/3D-skinny-fold.md:48`
and `restart/audit/totality/p3/3D-skinny-fold.md:57` preserve JSON 51-row
evidence only with scope qualifiers, while
`restart/audit/totality/p3/3E-grammar-generalisation.md:64` rejects JSON-only
proof for generic surface claims.

The packet does not overfit CSS-only evidence because CSS is a positive
non-JSON receiver only after typed provider and same-workload retime proof:
`restart/audit/totality/p3/3E-grammar-generalisation.md:65` requires generated
typed value/document/view/visitor output and `cssparser` retime, and
`restart/audit/totality/p3/3E-grammar-generalisation.md:80`-`84` pairs CSS with
Sheets or BBNF-self negative controls.
`restart/audit/totality/p3/3B-master-plan-reconciliation.md:93` and
`restart/audit/totality/p3/3B-master-plan-reconciliation.md:135` demote old CSS
row-admit language and route CSS through W1/W5/W6.
`restart/audit/totality/p3/3C-locks-v+1-diff.md:60` makes the same rule part of
the proposed Lock 14 addendum.

## Repair Directives

None. No CH2 REVISE or REJECT finding is raised.

## Residual Risk

1. The first negative-control receiver remains a planning choice:
   `restart/audit/totality/p3/3E-grammar-generalisation.md:140` asks whether
   Sheets or BBNF-self should land first if only one fits W7. This is acceptable
   because `3E` and `3C` both require at least one non-CSS witness and fail
   fleet-wide wording without it.
2. This ACCEPT does not validate implementation. Later gates must still produce
   the generated CSS typed provider, Sheets or BBNF-self receiver fixture,
   full-surface Lock 14 scan, and all-five lowerer evidence.
3. The working tree had broad unrelated dirty runtime/doc changes during this
   audit. The required invariant checks still returned 16 locks and 67 Pattern H
   runtime files, and no live V1 spec surface was changed by the target packet.
