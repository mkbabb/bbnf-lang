# Tranche AQ — Retrospective (R-AQ)

Span: `a217a3a1` (plan) … `a78c4a60` (AQ.9.3), 2026-04-13. Sources:
`AQ/AQ.md`, `AQ/audit.md`, AR audits, `post-AQ.json`. No `PROGRESS.md` /
`FINAL.md` under `AQ/`.

## 1. Scope reality vs plan

Plan declared nine mandatory phases across 4 waves × 6 agents = 24 parallel
sessions. Actual landed state is ~14 phase-scale commits in linear order on
master. Phases 2 (inspect), 3, 4 (deoverfit), 5 (delete structural), 6.A/B/C/D
(PayloadKind delete + TypeDesc expansion + aggregate layouts) landed as
source. Phase 7 (CSS ident routing + key dispatch hash) landed then was
reverted wholesale (`64a2cf9d` reverts `04bd0421`). Phase 8 reduced to AQ.8.2
(TapeBuilder pre-size). Phase 9 shipped 9.1/9.3/9.6; 9.4 (cost sweep) and
9.5 (global CSP) deferred.

## 2. Silent vs declared deferrals

**Declared** in post-AQ.json: clean bootstrap regen (1.2), cost-model grid
sweep (9.4), global CSP (9.5), CSS `selectorIdent` escape classification.
**Silent**: AQ.1 labelled HIGHEST PRIORITY "end the deferral chain" and
shipped as `5b06096` "restore structural attribute + span-text fallback" —
the opposite of the plan's hard gate (`grammar_roundtrip` un-ignored). Phase 6
typed-payload path landed as source but was functionally dormant: zero rules
in any production grammar receive an aggregate layout; the scalar path on
JSON `value` falls silent post-`fuse_single_use`. Code completeness
masqueraded as activation — a deferral no document names.

## 3. Orchestration friction

Plan stipulates 24 worktree-isolated agents across 4 waves with cherry-pick
onto master. Commit trail shows a single linear master history: no
`worktree-agent-*` merges, no cherry-pick batches, no wave boundaries. Waves
collapsed to sequential execution; 6-per-wave parallelism was ceremonial.

## 4. Agent-layer friction

AQ.7+8 revert exposes the canonical failure: identifier kernel routing and
length-bucketed hash interacted poorly with CSS escape patterns in
`selectorIdent`. Plan did not sequence the classifier extension
(`allows_escapes`) ahead of kernel routing; activation broke `var_*` and
`css_l4_simple_rule`, forcing wholesale rollback. AR carried this as 4.1-4.3.

## 5. Edict adherence

- **No workarounds**: violated — AQ.1 span-text fallback restoration is a
  workaround dressed as closure.
- **Plan-declared deferrals only**: violated on AQ.1, 7, 9.4, 9.5.
- **No god modules**: held — `passes::inspect/` split correctly.
- **One codegen path**: held — structural dispatch cleanly deleted (~400 LOC).
- **Hard grep gates**: held for `PayloadKind`, `structural_bytes`,
  `Json*`/`Css*` RegexClass, `sp_json_*`/`sp_css_*`.
- **Clean regen**: violated — `generated.rs` still hand-patched.
- **`#[ignore]` removed**: violated — `grammar_roundtrip` still ignored.

## 6. Chronic deferrals in vs out

**In** (inherited): clean bootstrap regen (AC→…→AQ, 9th tranche), global CSP
(AL→AO→AP→AQ), cost-model grid sweep (AM.6→AO→AP→AQ), instrumentation
(AN.6→AO→AP→AQ). **Out**: instrumentation SHIPPED at AQ.9.3 — first of four
to close. Regen, global CSP, cost sweep forwarded to AR/AT. One new chronic
seeded: CSS classifier escape extension (AR.4).

## 7. Mid-tranche restructuring

One pre-execution re-plan (`0597eccb` "delete structural, TypeDesc-driven
payload"): original draft activated structural dispatch; post-AP honest math
(-190µs on citm) flipped to DELETE. Clean plan-re-plan-before-execution. No
mid-wave restructuring; the AQ.7+8 revert was a same-day rollback, not a new
plan letter (which the new-tranche-new-doc edict would require).

## 8. Lessons

1. **Hard gates without execution discipline become aspirations.** Plan
   declared `grammar_roundtrip` un-`#[ignore]`-ed; tranche closed with
   `#[ignore]` intact. Gates must block commit-level landing, not merely
   the close document.
2. **Code completeness is not activation.** Phase 6 shipped full source
   (builder, layout planner, view decoder, prelude/epilogue) with zero
   payload writes in any production grammar. The tranche needed a
   population gate: `payload_layouts.len() > 0` for N grammars, verified
   via `cargo expand`. AR later adopted this as AR.2.5.
3. **Sequence enablers before consumers.** CSS ident routing (7.1/7.2)
   required classifier escape extension; plan listed them in the same
   phase. Agent waves collapsed to linear execution yet the cross-phase
   dependency was nowhere the agents would see. The revert was predictable.
