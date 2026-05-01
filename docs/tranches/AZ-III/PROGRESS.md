# AZ-III - Progress Log

**Status**: planned continuation of AZ-II.
**Opened by**: AZ-II continuation handoff on 2026-04-30.
**Parent**: [`AZ-III.md`](AZ-III.md).

AZ-III opens because AZ-II is closed as a continuation handoff, not a
terminal green close. The carried work is explicit: O5 close evidence,
O6 semantic/performance truth, O7 close conversion, and the audit-found
grammar-general fact/type/CSP/projection authority substrate.

## Opening Evidence

- AZ-II O0-O4 landed.
- O5 implementation partially landed, including `crates/tape` deletion
  work, but no refreshed green O5 close packet exists.
- Latest audit evidence reports no-default build repair is stale-good,
  while `cargo xtask regen --check` remains the active O5 blocker.
- O6 and O7 did not run.
- The only legitimate substrate expansion is grammar-general authority
  over facts, type obligations, CSP decisions, and projection emission.

## Wave Status

| Wave | Status | Notes |
|---|---|---|
| W0 - Quarantine and Dispatch Repair | complete | state ledger, commit-body sample, sibling triage, instruction-migration scan, dispatch packets all archived; dirty fmt slice preserved as `stash@{0}` and routed to W1 (pre-commit `regen --check` blocks landing in W0); 7/7 hard gates met |
| W0p - Throughput Substrate | complete | bench-iter profile (cold 1m 45s, warm 0.468s); ax-iter consolidated; xtask `--staged` (binary 0.098s, hook 1.5s); make doctor probe; nextest 3-shard CI matrix; 5 evidence files archived; 5/6 hard gates met (cold-wall spec was 60s; actual 105s recorded for W0p close gate amendment) |
| W1 - O5 Reclose | complete | regen drift confirmed STALE-BAD audit (HEAD already 9/9 clean, byte-identical to regen output via xtask content-equality skip); no-default build green (44.33s warm); cargo metadata clean (no tape, no json-prototype); 4 deletions landed (dta orphan, analysis/pretty re-export, parse_with_state alias, IR tape:: doc-comment scrubs); rustfmt now excludes generated/ via `.rustfmt.toml`; 411-file workspace fmt sweep landed; 5/5 hard gates met; W1.4 row 8 absorbed into W1.5 per SPEC.md scope-reveal rule |
| W2 - Semantic Parity and Bootstrap Canonicalization | complete_with_misses | JSON sonic-rs parity 5/5 GREEN (W2.1); CSS lightningcss normalize+bootstrap GREEN, named_color + tailwind perf routed to W3c (W2.2); Sheets parity 100→122/133 (+22, audit cluster MET), 11 routed to W3c (W2.3); BBNF bootstrap canonical: bootstrap_parser.rs DELETED 1505 LOC, 95/95 BBNF parity GREEN via canonical generated path (W2.4 path-(a) CLOSED); W2.4.r flat-shape Span synthesis, W2.4.s lower_term + activation, W2.4.t lower_factor modifier recovery (3/9 drift reduction), W2.4.u keyword-shape Span push (architectural fix at carve scope) |
| W3a - Fact and Type Authority | complete_with_misses | W3a.0 pipeline registry research: 3 idents TEST FIXTURES, Option A binding; W3a.1 durable FactAuthority surface + 5 disconnect tests; W3a.2 UnresolvedCompoundRef obligation replaces silent BoxedEnum at reference.rs:74; W3a.3 HeterogeneousAltJoin obligation + new TypeDesc variant replaces silent BoxedEnum at revise.rs:123; W3a.merge unified surface (406 IR tests + 2 JSON Value tests pass); W3a.4 regen path-agnostic shape detection BLOCKED-with-route — root cause traced to keyword Unit→Span (W2.4.u absorbed at carve), entry-rule classifier (Scalar vs Array), HRegex payload (i64→str), PHF table generation (W6.2). 4/4 hard gates either MET or NAMED-BLOCKER |
| W3b - CSP Strategy Globalization | complete | All 4 sub-units MET: W3b.1 shape installer (no-op deleted), W3b.2 layout installer, W3b.3 dispatch installer, W3b.4 csp-solver alignment. 5 named production consumers + disconnect tests; csp-solver 99/99 PASS |
| W3c - Projection Consumption and Registry Authority | complete_with_misses | W3c.2 6 fixture idents substituted (`BbnfParser`); 11/12 pipeline_compile_request PASS, 1 ts_backend_emits_discriminated_union routed forward. W3c.3 5 deletions: prettify stubs (43 LOC), trace.rs corpse (54 LOC), recognizer_plan.rs (159 LOC), emit_negated_scan_{plus,star} wrappers (11 LOC), is_fused_number_regex shim (16 LOC) — total 301 LOC removed. W3c.1 alt_dispatch named_color emitter mechanism landed (substrate); runtime activation blocked on egraph cost extractor (Map wrapper stripped during extraction; W3a/W3b egraph cost.rs follow-up); priorities 2-5 (sheets, tailwind perf) blocked by regen-pipeline divergences |
| W4 - Benchmark, Profile, and Workspace Truth | in_progress | bench/profile/workspace truth |
| W5 - Terminal Close and Handoff | planned | terminal close and BA/BB handoff |

## Current Blockers

1. Main worktree is dirty with the restored AZ-II implementation/source
   slice and two untracked docs artefacts; implementation dispatch remains
   blocked until W0 - Quarantine and Dispatch Repair slices or routes that
   work.
2. AZ-II O5 close artifact is stale and must be regenerated under W1 - O5
   Reclose.
3. Root, parse-that, and pprint format checks are green. Root compile
   passes. Root tests, root clippy, parse-that tests/clippy, and pprint
   clippy are red; pprint tests pass. See `audit/W0-state-ledger.txt`.
4. Parity and benchmark truth are stale or partial until W2/W4.
5. BA/BB remain blocked until W5 - Terminal Close and Handoff lands.

## 2026-04-30 - W0 Quarantine Evidence

Recorded W0 state, history repair, and dispatch packet evidence:

- `audit/W0-state-ledger.txt`
- `audit/W0-commit-repair-plan.md`
- `audit/W0-dispatch-packets.md`

Root commit history from `53d3e6b2..HEAD` was rewritten message-only to
replace terse AZ-II subjects and missing bodies with concrete scopes and
evidence-bearing bodies. The backup branch is
`codex/az-history-before-reword-20260430-114057`.

Additional W0 gate runs:

- `cargo iter-check`: pass with generated-code warnings.
- `cargo iter-test`: fail-fast after `bootstrap_full_parse`; 202/1509 run,
  201 passed, 1 failed, 25 skipped, 1307 not run.
- parse-that `cargo test --workspace`: fail on published `parse_that 0.3.3`
  expecting old `pprint::Doc` / `pprint::Join`.
- pprint `cargo test`: pass with one warning and two ignored doctests.

## 2026-04-30 - REAUDIT Wave Refinements

Per `docs/tranches/AZ-III/audit/REAUDIT-2026-04-30/SYNTHESIS.md` and the
six lane reports under that directory, the AZ-III wave plan absorbs the
following refinements:

- **R1**: New `W0p - Throughput Substrate` wave opens between W0 and W1
  to land bench-iter profile, regen `--staged`, `make doctor`, profile
  redundancy cleanup, and nextest partition before any source
  implementation wave dispatches. Per `feedback_build_infra_first` and
  REAUDIT lane 6's >10 min/harness fat-LTO finding (the W4 throughput
  blocker).
- **R2**: New `W0.5 Commit Body Truth Sample` sub-unit under W0
  produces a sampled report over the 68 templated-body commits;
  orchestrator decides re-rewrite scope only after user acknowledgment.
- **R3**: New `W0.6 Sibling Repo Triage Packet` sub-unit under W0
  catalogs parse-that, pprint, gorgeous, and bbnf-buddy red surfaces
  with explicit dispositions (registry pin, sibling tranche, or AZ-III
  blocker carry).
- **R4**: The single `W3 - Fact, Type, CSP, and Projection Authority`
  wave is split into three: `W3a - Fact and Type Authority` (egraph
  facts + type obligation solver; deletes silent `BoxedEnum` fallbacks
  at `crates/ir/src/passes/types/constraint/reference.rs:74` and
  `revise.rs:123`), `W3b - CSP Strategy Globalization` (shape, layout,
  dispatch constraints; replaces `shape_dict::install` no-op), and
  `W3c - Projection Consumption and Registry Authority` (StructDirect
  emitter authority + pipeline registry holes for `MultiPathParser` /
  `ImportPrettyParser` / `SplitPrettyParser`).
- **R5**: The W2 vs W3.4 emitter file-bounds race is resolved by
  carving `crates/core/src/backend/rust/emitter/shapes/**/struct_direct.rs`
  to W2 (parity-driven shape-specific fixes) and the rest of
  `crates/core/src/backend/rust/emitter/**` to W3c (projection-authority
  work). Both wave specs document the carve in their File Bounds.
- **R6**: New `W3a.0 Pipeline Registry Research` sub-unit produces
  `audit/W3a-0-pipeline-registry-research.md` with a per-caller verdict
  for `MultiPathParser`, `ImportPrettyParser`, and `SplitPrettyParser`;
  W3c registry binding work consumes this verdict.
- **R7**: W2.4 BBNF Bootstrap Canonical Path closes ONLY by canonical
  generated self-host or by a same-tranche `bootstrap_parser.rs`
  removal commit. Deferral to BA, BB, or any future tranche letter is
  forbidden as a closure path.
- **R8**: AZ-III wave table and PROGRESS wave-status table reshape into
  nine rows (W0, W0p, W1, W2, W3a, W3b, W3c, W4, W5) and the Carried
  Work Ledger in `AZ-III.md` reroutes its W3-owned items to W3a / W3b /
  W3c.
