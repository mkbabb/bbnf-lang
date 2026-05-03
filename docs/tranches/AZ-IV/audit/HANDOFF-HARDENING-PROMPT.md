# Handoff Hardening Prompt — Refine BA/BB/BC

> **For the next agentic system.** This is a self-contained prompt that briefs a fresh orchestrator + 6-polymath audit cohort to further refine the BA/BB/BC tranche specifications produced by the prior 8-agent deep cohort. Pithy. Read-first paths are absolute. The cohort shape mirrors `docs/tranches/AZ-IV/audit/HARDENING-SYNTHESIS-2026-05-01-FINAL.md` exactly — 6 named polymath agents, ≤25 min HARD CAP per audit lane, paste-ready amendments per agent, one synthesis at the end.

## State

Repository: `/Users/mkbabb/Programming/bbnf-lang` at master `40e1835d` (after Phase 1 plan-surgery; ≈2583 commits total). AZ-IV closed `complete_with_misses` at `6de6ac0c`; the canonical post-AZ-IV ordering is **AZ → BA (direct-projection codegen) → BB (rule-discovery, un-subsumed) → BC (cleanup, repurposed) → BD+ (TS/WASM re-engineering or shared-ABI; future)** per `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md`. Phase 1 plan-surgery already archived old BA/BB/BC content (`historical/` + `orchestration-archive-2026-04-30/`), deleted superseded plan docs, refreshed GESTALT.md.

The 8-agent deep cohort (DEEPX-1 through DEEPX-8) just landed deep findings:

| Agent | Doc | Anchor finding |
|---|---|---|
| DEEPX-1 (SIMDJSON-WITNESS) | `audit/DEEPX-1-json.md` | 10 hardcoded `rule_type: TypeDesc::Span` emission sites; static REGISTRY at `json.rs:276` carries rich BoxedEnum types; `JsonParser::get` static entry is the load-bearing missing API |
| DEEPX-2 (LIGHTNINGCSS-WITNESS) | `audit/DEEPX-2-cssl4.md` | 1407 `checkpoint()` sites in `css_l4.rs` (107K LOC); 5545 declarations × ~7 failed branches = ≈38K clones/parse, ≈7.6 MB cloned per parse; bootstrap 606ms is the same `Vec<OpenFrame>::clone` mechanism |
| DEEPX-3 (SHEETS-WITNESS) | `audit/DEEPX-3-sheets.md` | Flat-shape early-bail mechanism named: consult `cursor.has_resolved()` between positions in Flat/Pratt/Wrap/Keyword/ArgList shape emitters; close compound, return success when terminal reached |
| DEEPX-4 (BBNF-WITNESS) | `audit/DEEPX-4-bbnf.md` | Recursive-projection mechanism: cycle-break `Box` driven by `ir.type_obligations`, one rule per SCC promoted to BoxedEnum; non-cyclic Refs project direct |
| DEEPX-5 (KNUTH-ARCHAEOLOGY) | `audit/DEEPX-5-projfail-archaeology.md` | The 3000-commit answer: each thesis-pivot reset substrate without retiring indirection; substrate-first/consumer-forward + no-orthogonal-codepaths preserves shim by deferring consumer; 32 zero-caller substrates surface as the cumulative tail |
| DEEPX-6 (TURING-ASSAY) | `audit/DEEPX-6-tranchehist.md` | Single-thesis tranche scope; reject "union" framing (AZ-IV's failure mode); audit-cohort cap 6+3+1=10 max |
| DEEPX-7 (VON-NEUMANN-PATH) | `audit/DEEPX-7-sotapath.md` | Unambiguous path: BA W0..W6 sequenced; bumpalo-arena-backed `<Grammar>Document`; cheap `(stack_depth, arena_count)` checkpoints; `path!` produces compile-time grammar-aware diagnostics no SOTA competitor can produce |
| DEEPX-8 (KNUTH-SPEC) | `audit/DEEPX-8-fullspec.md` + `tranches/{BA,BB,BC}/{<LETTER>.md, waves/W{0..6}.md}` | Full BA/BB/BC tranche specs at AZ-IV.md depth (see commit history for landed scope) |

DEEPX-8's output is the work product the next cohort hardens.

## User-Mandated Invariants (verbatim)

1. "NO quick solutions, NO workarounds: idiomatic, gestalt approaches"
2. "Architectural transpositions in the sake of elegance, simplicity, and performance above all are both necessary and desirable"
3. "NO legacy code"
4. "Ensure all substrate is wired and consumed — audit for any dead code, under utilized code, alongside deprecated, contrived, shim-like, complex, or legacy code"
5. "Any deferred items, or in particular chronically deferred items, must be noted and explicitly addressed"
6. "With all approaches: KISS. One path."
7. "Lettering must be canonical: AZ → BA → BB → BC → BD"
8. "Even without an explicit `->` annotation, we should use our type inference system to infer the type and project into a struct"
9. "Mirror sonic-rs / simdjson `get` API with superior ergonomics"
10. "Ignore TS and WASM backends" (route to BD+; shared-ABI question deferred)
11. "Triumvirate dispatch for scope increase/change as we implement"

## Mandate

Refine BA/BB/BC tranche specifications produced by DEEPX-8 to maximal clarity and minimal ambiguity. Specifically:

- **Stress-test BA's wave shape** against DEEPX-1..7's findings: does each wave's hard gate close the correct subset of the audit findings? Are scope items mis-attributed across waves? Are file bounds disjoint?
- **Fill missing fixtures** in BA's hard gates (path expressions, fixture inputs, expected outputs) using DEEPX-1/2/3/4's per-grammar fixture lists.
- **Verify the cycle-break Box discipline** (DEEPX-4) is bound to a wave; verify the Flat-shape early-bail (DEEPX-3) is bound to a wave; verify cheap-checkpoint mechanism (DEEPX-7) covers Pratt + Sheets compounds, not only JSON.
- **Surface chronic-deferral risks** specific to BA's plan: which BA gates are most likely to route forward at close? What discipline hardens against the AZ-IV → fictional-AZ-V failure mode?
- **Triumvirate triggers**: enumerate the conditions that fire mandatory triumvirate per wave; cite ORCHESTRATION.md §Triumvirate Auto-Triggers.
- **Deletion bias**: confirm every "delete" is a delete, not a rename or move; identify any "transpose" hiding a "delete needed" verdict.

## Cohort Shape — 6 Polymaths, 25 min HARD CAP each

| Agent | Lane | Read-first focus | Output |
|---|---|---|---|
| **EULER** | BA wave-coherence + hard-gate closure-mapping | `tranches/BA/BA.md` + `tranches/BA/waves/W*.md` + DEEPX-1..7 | `audit/HARDENING-BA-2026-05-XX-euler.md` |
| **GAUSS** | BB un-subsume coherence (rule-discovery scope vs current substrate) | `tranches/BB/BB.md` + `tranches/BB/waves/W*.md` + recycled-BA archive (`BA/historical/recycled-rule-discovery/`) | `audit/HARDENING-BB-2026-05-XX-gauss.md` |
| **NOETHER** | BC repurpose coherence (cleanup pass vs Audit-A TRANSPOSE bucket + AUDIT-B routed splits + worktree fixture symlink contract + samply 7-artefact canonicalization) | `tranches/BC/BC.md` + `tranches/BC/waves/W*.md` + `audit/POST-CLOSE-A-legacy.md` + `audit/POST-CLOSE-B-substrate.md` + `tranches/AZ-IV/audit/AUDIT-B-arch-2026-05-02.md` | `audit/HARDENING-BC-2026-05-XX-noether.md` |
| **POINCARE** | Cross-tranche orchestration discipline (triumvirate triggers; audit-cohort cap; per-wave agent allocation; worktree disjointness) + chronic-carry resistance | All BA/BB/BC + ORCHESTRATION.md + DEEPX-6 | `audit/HARDENING-ORCH-2026-05-XX-poincare.md` |
| **HILBERT** | Substrate-with-consumer audit on BA's new substrates + GESTALT invariant fidelity check (typed materialization, no orthogonal codepaths, direct-to-struct, grammar-authoritative) | BA/BB/BC + GESTALT.md §2 + `audit/POST-CLOSE-B-substrate.md` | `audit/HARDENING-SUBSTRATE-2026-05-XX-hilbert.md` |
| **EINSTEIN** | Performance-thesis hardening: does BA actually close 4196× sonic gap + 18/19 AU floor + ts_node_execute + Flat-shape lazy through ONE mechanism? Or are there sub-mechanisms hiding orthogonal codepaths? | BA + DEEPX-1/2/3/4/7 + samply traces under `.profiles/samply/post-AZ-IV/deep-B/` | `audit/HARDENING-PERF-2026-05-XX-einstein.md` |

After 6 polymaths land, **synthesise** at `audit/HARDENING-SYNTHESIS-2026-05-XX.md`. Synthesis must:
1. Triage findings per agent (ACCEPT / NARROW / REJECT).
2. Produce paste-ready amendment blocks per BA/BB/BC file.
3. Identify any cross-cutting theme that warrants thesis review.
4. Decide: is BA ready to dispatch W0, or does it need a 4th amendment pass?

## Dispatch Discipline (per ORCHESTRATION.md §Triumvirate)

Each polymath:
- Sibling worktree: `bbnf-wt-harden-{euler,gauss,noether,poincare,hilbert,einstein}` (orchestrator creates).
- Per-agent `CARGO_TARGET_DIR=<worktree>/target/harden-<name>` (avoid lock contention).
- Read-only audit; one commit per agent for the audit doc.
- HARD CAP 25 min audit-lane default; commit at 22.5 min if running long.
- Empty/no-evidence return → verbatim redispatch once → second empty triggers triumvirate.
- JSONL transcript quiet >15 min triggers orchestrator-side check.

## Self-Contained Brief for the Next Orchestrator

```
You are the orchestrator of a 6-polymath BA/BB/BC hardening cohort. Repo at /Users/mkbabb/Programming/bbnf-lang, master 40e1835d (after Phase 1 plan-surgery + 7 of 8 DEEPX cohort cherry-picks). Your job:

1. Read this prompt verbatim: docs/tranches/AZ-IV/audit/HANDOFF-HARDENING-PROMPT.md
2. Read DEEPX-1..7 audit docs (audit/DEEPX-{1..7}-*.md) and DEEPX-8 once it lands.
3. Create 6 sibling worktrees: bbnf-wt-harden-{euler,gauss,noether,poincare,hilbert,einstein}, each at master with `-b harden-<name>`.
4. Dispatch 6 polymaths in parallel using the Cohort Shape table above. Each gets the 25-min HARD CAP, the read-first list, and writes one audit doc.
5. After all 6 return, synthesise at audit/HARDENING-SYNTHESIS-2026-05-XX.md. Cherry-pick all 7 commits to master.
6. If synthesis recommends amendments, apply them as paste-ready blocks per agent. Re-validate the BA/BB/BC top-level + wave specs against the user's invariants (verbatim above).
7. If synthesis recommends a 4th hardening pass, dispatch one (3-agent ceiling for 4th pass).
8. Return a single completion message with: state of BA/BB/BC; commit hash of HARDENING-SYNTHESIS; whether Phase 2 (BA.W0 cleanup absorption) is ready to dispatch; routed carries to BB/BC/BD+ with named close criteria.

Non-negotiables: stay inside file bounds (audit/ + tranches/{BA,BB,BC}/); no source-code commits; KISS / no workarounds / one path; canonical AZ→BA→BB→BC→BD lettering. Cite specific file:line evidence in every finding.
```

This prompt is the handoff. The next agentic system reads it, executes the cohort, returns the synthesis. After that returns and the orchestrator re-validates, Phase 2 (BA.W0 cleanup absorption) opens.
