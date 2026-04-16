# Retrospective — Tranche AR

AR was "Discriminator Split, Payload Activation, Clone Reduction,
CSS Hardening." It landed 30 commits across nine phases, closed the
ten-tranche bootstrap deferral that had shadowed the project since
AC, and produced the first honest post-tranche bench numbers. It
was also the first tranche where in-flight audits drove a mid-plan
restructure rather than a post-hoc post-mortem.

## 1. Scope reality vs plan

The plan was scaffolded as 5 phases; the audit phase expanded it to
9 (`8fb3adb`). All 9 gates were claimed. **But AS later marked "AS
Phase 2 done — bootstrap loop closed in AR audit"** (`2dc6a20`): the
genuine closure of the chronic self-hosting debt (`0c6e011`,
"recover modifiers from Repeat(vi=0)") landed under AR's banner but
in AR's post-audit cleanup window — scope neither AR.md nor critique
forecast.

## 2. Silent vs declared deferrals

Phase 6 declared 8 scanner-generalization items and shipped 3
(AR.6.3 FAMILY_HELPER + partial HIR re-exports; symmetric kernel
moved to AS). AR.6.1/6.2/6.4/6.5/6.6 rolled silently into AS.5,
where AS.5.1 was then marked "Not applicable" and AS.5.2/5.4/5.5
"Not implemented / deferred / negligible" — classic defer-then-retire,
a direct `no-deferrals` violation. Phase 2 "activation" met its
functional gate but the AS pre-analysis pins canada −39% / data_xl
−20% on AR.1.1's `meta: Vec<u8>` side-channel — activation passed,
throughput regressed.

## 3. Orchestration friction

Four waves of six agents. Critical-files matrix lists `grammar.rs`,
`alt.rs`, and `mod.rs` in ≥2 phases. Duplicate commits `b0e4534` +
`6c889d5` (AR.7.1 span-text deletion) and `677a801` + `6074a4b`
(AR.3.1 egraph clones) are evidence of worktree racing — a direct
`agent-orchestration` edict violation. The commit-before-parallelise
remediation did not propagate to the Wave-3 / Wave-4 boundary.

## 4. Agent-layer friction

The six-audit pattern (prior, AQ-code, scanners, direct-struct,
self-hosting, sonic-gap) produced rich forensic material but
critique.md had to correct eight overstatements: structural dispatch
framed "dormant-ready" when it was already deleted; "lazy AST"
overclaim; "after AR (projected)" throughput tables. Audit-first
worked; trust-the-audit did not.

## 5. Edict adherence

`no-workarounds` held at code level (zero TODO / FIXME / HACK in
scope). `no-deferrals` was violated structurally by the AR.6→AS.5
roll. `inspect-generated-output` held (`cargo expand` cited
throughout). `accurate-perf-narrative` was partially violated —
post-AR.json landed with a known-regression disclaimer and
`css_l4: BROKEN` carried into AS Phase 1.

## 6. Chronic deferrals

AR's own `audit-prior` named four: **clean bootstrap regen** (open
ten tranches — **closed in AR audit phase** via `0c6e011`, the
single largest chronic item resolved); **global CSP solve**
(declared out-of-scope, deferred AGAIN to AW → AX); **cost-model
grid sweep** (deferred AGAIN); **BBNF_CSP_REPORT on trivial
components** (landed via AR.2.4, `c5133ee`). Score: 2 of 4 closed;
2 routed forward.

## 7. Mid-tranche restructuring

The four-audit surge (plus two later) *was* the planning
methodology. It caught real architectural errors: `lower_map_arrow`
rule_kind check, Alt-branch post-inline fragility, CSS classifier
escape-sequence gap. The critique cycle was high-leverage. The
failure mode: phase-expansion from 5→9 happened silently under the
same AR letter instead of opening a new letter — violating the
`new-tranche-new-doc` feedback. AR.md at scaffold and AR.md after
audit expansion are different documents.

## 8. Lessons

1. **Audit-driven replan must promote to a new tranche letter.**
   Silent phase inflation under one letter muddies what "AR landed."
2. **Activation gates must include regression gates.** AR.1.1
   shipped its meta_idx gate green alongside a −39% canada
   regression. A throughput-delta gate would have caught the
   side-channel cost before landing.
3. **Worktree racing on shared files reappeared.** Duplicate
   AR.7.1 and AR.3.1 commits prove commit-before-parallelise did
   not propagate to Wave-3/4. Re-assert at every wave boundary,
   not just at tranche start.
