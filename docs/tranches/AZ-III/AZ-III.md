# Tranche AZ-III - AZ-II Continuation Close

AZ-III is the continuation of AZ-II. AZ-II closes as a handoff tranche:
it records the direct-to-struct cutover work that landed, names the
blocked or deferred close items without marking them green, and routes
the remaining AZ close work here. AZ-III does not relax the AZ thesis.
It finishes the tape abrogation, semantic parity, benchmark truth, and
grammar-general substrate authority work needed before BA or BB may open.

AZ-III opens from the AZ-II 2026-04-30 audit packet. O0 through O4
landed. O5 implementation landed in part, but its close packet is not
green because regen drift remains active and no refreshed O5 artifact
records a clean close. O6 and O7 did not run. The audit also identified
one legitimate continuation substrate: fact/type/CSP/projection authority
is still too recomputed, under-consumed, and fallback-heavy to support
full grammar-general semantic richness at SOTA performance.

## Thesis

There is one materialized parse form: the grammar-derived struct graph.
All semantic richness, projection, layout, recognizer strategy, and
backend generation must flow from grammar-derived facts, type inference,
CSP decisions, and e-graph facts that are durable enough to be consumed
by production emitters and tests. No tape runtime, `Parsed<R>`,
`TapeDirect`, generated tape view, compatibility bridge, or silent
`BoxedEnum` escape may survive as a production answer.

AZ-III has two duties:

1. finish the AZ-II terminal close work that was not honestly green;
2. land the grammar-general authority substrate that the audits show is
   required to stop recurring local fixes for EBNF, CSS, Sheets, BBNF,
   and future grammars.

## Invariants

1. **Continuation, not deferral.** AZ-II blockers do not disappear; they
   become named AZ-III gates with owners and evidence.
2. **No legacy code.** Dead tape/prototype/JIT/bootstrap surfaces delete
   or move to archive; no renamed tape runtime or compatibility view may
   replace them.
3. **Substrate with consumer.** Fact, type, CSP, or projection substrate
   lands with a production consumer and a test that fails without it.
4. **Grammar-general fixes only.** EBNF, CSS, Sheets, JSON, and BBNF
   redress must improve shared inference/projection/codegen paths unless
   a local grammar rule is the actual source of the defect.
5. **Evidence before optimization.** No BB optimization or SOTA claim may
   consume stale `post-AZ-II.json` values or placeholder bench rows.
6. **Clean orchestration.** Implementation dispatch begins only from a
   clean main worktree, explicit file bounds, sibling worktrees, and a
   commit plan with concrete scopes and evidence-bearing bodies.
7. **No silent fallback.** Unsolved cycles, heterogeneous alternations,
   unsupported StructDirect variants, and bootstrap bridges must produce
   a named error or a same-wave grammar-general implementation.

## Carried Work Ledger

| Origin | Item | AZ-III owner |
|---|---|---|
| AZ-II.O5 | O5 close packet, regen drift, no-default proof refresh, A1 residue | W1 - O5 Reclose |
| AZ-II.O6 | JSON sonic-rs, CSS lightningcss, Sheets, BBNF semantic parity | W2 - Semantic Parity and Bootstrap Canonicalization |
| AZ-II.O6 | 17-entry matrix, workspace health, structural audits, profile truth | W4 - Benchmark, Profile, and Workspace Truth |
| AZ-II.O7 | FINAL conversion and trajectory handoff | W5 - Terminal Close and Handoff |
| AZ-II audit | generated BBNF self-host canonicalization vs `bootstrap_parser` | W2 - Semantic Parity and Bootstrap Canonicalization |
| AZ-II audit | CSP `shape_dict` no-op / under-consumed global decisions | W3b - CSP Strategy Globalization |
| AZ-II audit | silent `BoxedEnum` fallback for cyclic and heterogeneous typing | W3a - Fact and Type Authority |
| AZ-II audit | durable egraph/node/projection fact authority | W3a - Fact and Type Authority |
| AZ-II audit | StructDirect fallback/deferred emitter surfaces | W3c - Projection Consumption and Registry Authority |
| AZ-II audit | pipeline registry holes (`MultiPathParser`, `ImportPrettyParser`, `SplitPrettyParser`) | W3a.0 research + W3c registry binding |
| REAUDIT 2026-04-30 | bench-iter profile, regen `--staged`, `make doctor`, profile redundancy, nextest partition | W0p - Throughput Substrate |
| REAUDIT 2026-04-30 | sibling-repo (parse-that, pprint, gorgeous, bbnf-buddy) red-state triage | W0.6 sub-unit (triage doc only; sibling source edits routed elsewhere) |
| REAUDIT 2026-04-30 | commit-body truth sample over the 68-commit AZ-III W0 rewrite span | W0.5 sub-unit (sample doc; orchestrator decides re-rewrite) |
| Prior tranches | substrate-only closes and stale benchmark consumption | W0 - Quarantine and Dispatch Repair; W4 - Benchmark, Profile, and Workspace Truth |

## Wave Table

| Wave | Agents | Closes on | Status |
|---|---:|---|---|
| [W0 - Quarantine and Dispatch Repair](waves/W0.md) | up to 6 parallel | clean state, commit/orchestration repair, AZ-II handoff docs, dispatch packets, W0.5 commit-body sample, W0.6 sibling triage | planned |
| [W0p - Throughput Substrate](waves/W0p.md) | up to 5 parallel | bench-iter profile, regen `--staged`, `make doctor`, profile redundancy resolved, nextest partition wired, 5-harness sweep measured | planned |
| [W1 - O5 Reclose](waves/W1.md) | up to 6 parallel | O5 close packet green: regen, no-default build, metadata, deletion scans | planned |
| [W2 - Semantic Parity and Bootstrap Canonicalization](waves/W2.md) | up to 6 parallel | semantic parity and canonical generated BBNF self-host (or same-tranche `bootstrap_parser.rs` removal commit) | planned |
| [W3a - Fact and Type Authority](waves/W3a.md) | up to 6 parallel | durable fact authority consumed by production passes; type obligation solver replaces silent `BoxedEnum` fallbacks; W3a.0 pipeline registry research doc | planned |
| [W3b - CSP Strategy Globalization](waves/W3b.md) | up to 6 parallel | CSP shape/layout/dispatch constraints installed and consumed; `shape_dict::install` no-op replaced | planned |
| [W3c - Projection Consumption and Registry Authority](waves/W3c.md) | up to 6 parallel | StructDirect emitter fallbacks deleted; pipeline registry holes resolved; EBNF/CSS/Sheets/BBNF projection tests fail without authority | planned |
| [W4 - Benchmark, Profile, and Workspace Truth](waves/W4.md) | up to 6 parallel | workspace, structural, profile, and 17-entry benchmark truth | planned |
| [W5 - Terminal Close and Handoff](waves/W5.md) | up to 6 parallel | terminal AZ close docs, BA/BB handoff, archive decisions | planned |

## Triumvirate Discipline

Any non-environmental stall, repeated empty return, three diagnostic
loops, unclear root cause, or scope reveal dispatches a triumvirate:
research, plan augment/synthesis, and redress. Redress may not edit
source until the wave spec has been amended. Broad implementation waves
also carry read-only hardening lanes for diff bounds, gate evidence,
dead/overfit substrate, and document-status reconciliation.

## Hard Gates

1. `cargo xtask regen --check` is green across the grammar fleet.
2. `cargo build -p bbnf --no-default-features --profile ax-iter` is
   green with no `crates/tape` package in metadata.
3. No production source or generated Rust exposes `Parsed<R>`,
   `TapeDirect`, generated tape views, `ValueRoot`, `TapeOffset`, or a
   public tape runtime.
4. JSON sonic-rs, CSS lightningcss, Sheets, and BBNF parity suites are
   green or the tranche remains blocked with exact source owners.
5. Generated BBNF self-hosting is canonical, or `bootstrap_parser.rs` is
   named as a terminal blocker with a same-wave removal plan.
6. CSP shape/layout/dispatch decisions are installed and consumed, not
   sidecar comments or no-op constraints.
7. Type inference has no silent fallback for unresolved production rule
   cycles or heterogeneous alternation joins.
8. The 17-entry matrix is refreshed in `docs/benchmarks/post-AZ-III.json`
   with commands, units, fixture names, and no placeholders.
9. `PROGRESS.md`, `FINAL.md`, wave statuses, remaining trajectory, and
   AZ-II handoff docs agree.

## Exclusions

BA typed path APIs, BB rewrite inference close, sibling-repo
modernization, and new optimization claims remain closed until AZ-III
publishes terminal evidence. AZ-III may prepare BA/BB handoff facts, but
it does not implement BA/BB features.
