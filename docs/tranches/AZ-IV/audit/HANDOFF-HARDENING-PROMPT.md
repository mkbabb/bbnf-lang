# Handoff Hardening Prompt — Refine BA/BB/BC

> **For the next agentic system.** Self-contained brief that hands off the BA/BB/BC tranche specifications produced by the prior 8-agent deep cohort to a fresh orchestrator running an **8-lane meta-audit** patterned exactly after `docs/tranches/meta-audit/0{1..8}-*.md` — the 2026-04-22 bundle that fed B1, B7, AZ-I, AZ-II planning. Each lane has a discrete methodology, a discrete output, and a discrete verdict shape; the orchestrator then synthesises and decides Phase 2 (BA.W0 dispatch) readiness.

The polymath-name framing of the prior draft was decorative; the canonical meta-audit shape is **methodological**, not nominal. Each lane below maps the original meta-audit lane (purpose, methodology, evidence form, verdict shape) onto the BA/BB/BC hardening question.

## State

Repository: `/Users/mkbabb/Programming/bbnf-lang` at master `d4085e4e`. AZ-IV closed `complete_with_misses` at `6de6ac0c`; canonical post-AZ-IV ordering **AZ → BA (direct-projection codegen) → BB (rule-discovery, un-subsumed) → BC (cleanup, repurposed) → BD+ (TS/WASM re-engineering or shared-ABI; future)** per `docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md`. Phase 1 plan-surgery already archived prior BA/BB/BC content (`historical/` + `orchestration-archive-2026-04-30/`); DEEPX-8 landed full top-level + 21 wave specs; the residual "Phase 2 cleanup absorption" framing was retracted at `d4085e4e` (cleanup IS BA.W0). Total commits: ~2585.

The 8-agent DEEPX cohort outputs are the inputs:

| Cohort agent | Doc | Anchor |
|---|---|---|
| DEEPX-1 (JSON) | `audit/DEEPX-1-json.md` | 10 hardcoded `rule_type: TypeDesc::Span` emission sites; rich REGISTRY discarded at parse |
| DEEPX-2 (CSS L4) | `audit/DEEPX-2-cssl4.md` | 1407 `checkpoint()` sites + ≈7.6 MB cloned per bootstrap |
| DEEPX-3 (Sheets) | `audit/DEEPX-3-sheets.md` | Flat-shape early-bail mechanism named |
| DEEPX-4 (BBNF) | `audit/DEEPX-4-bbnf.md` | Cycle-break `Box` from `ir.type_obligations`; one rule per SCC promoted |
| DEEPX-5 (archaeology) | `audit/DEEPX-5-projfail-archaeology.md` | Each thesis-pivot reset substrate without retiring indirection |
| DEEPX-6 (tranche history) | `audit/DEEPX-6-tranchehist.md` | Reject "union" framing; audit-cohort cap 6+3+1=10 |
| DEEPX-7 (SOTA path) | `audit/DEEPX-7-sotapath.md` | bbnf's `path!` produces compile-time grammar-aware diagnostics SOTA cannot |
| DEEPX-8 (full spec) | `audit/DEEPX-8-fullspec.md` + `tranches/{BA,BB,BC}/{<LETTER>.md, waves/W{0..6}.md}` | BA/BB/BC fully specified |

## User-Mandated Invariants (verbatim)

1. NO quick solutions, NO workarounds; idiomatic, gestalt approaches
2. Architectural transpositions in the sake of elegance, simplicity, and performance above all are both necessary and desirable
3. NO legacy code
4. Ensure all substrate is wired and consumed — audit for any dead code, under-utilized code, deprecated, contrived, shim-like, complex, or legacy code
5. Any deferred items, or in particular chronically deferred items, must be noted and explicitly addressed
6. KISS. ONE PATH.
7. Lettering must be canonical: AZ → BA → BB → BC → BD
8. Even without an explicit `->` annotation, type inference should infer the type and project into a struct
9. Mirror sonic-rs / simdjson `get` API with superior ergonomics
10. Ignore TS and WASM backends (route to BD+; shared-ABI question deferred)
11. Triumvirate dispatch for scope increase/change as we implement

## Mandate

Execute an 8-lane meta-audit on the BA/BB/BC tranche specs, using `docs/tranches/meta-audit/0{1..8}-*.md` as the canonical methodological template. Each lane carries the original lane's *purpose* but *retargeted* at BA/BB/BC instead of AY-II/B0/B1. Synthesis decides whether BA is ready to dispatch W0 or needs another amendment pass.

## The 8 Audit Lanes — Canonical Mapping

Each lane's reference file in `docs/tranches/meta-audit/` is the methodological exemplar. Each retargeted lane below preserves: the methodology (offline JSONL scan / edict-vs-observed grep / wave-vs-PROGRESS drift / wall-clock measurement matrix / claim verification / git-archaeology era taxonomy / per-repo assay / KEEP-MODERNIZE-REPLACE-ABROGATE-FOLD verdict scheme).

### Lane 01 — BA/BB/BC Spec-Friction Mining
**Methodological exemplar**: `meta-audit/01-session-friction.md` (offline JSONL transcript scan; verbatim quote extraction; tool-use frequency tables; ranked friction patterns).

**Retargeted scope**: Mine the latest two long-session transcripts for orchestrator-side friction during BA/BB/BC spec authoring. Quantify: Bash-poll vs Monitor adoption, parallel-agent overlap, worktree contention, redispatch-after-empty rate. Identify 3-5 ranked friction patterns the BA dispatch will inherit and propose mechanism-level fixes (not policy fixes).

**Output**: `audit/HARDENING-2026-05-XX-01-spec-friction.md` (≤500 lines).

### Lane 02 — BA/BB/BC Edict Adherence
**Methodological exemplar**: `meta-audit/02-instruction-adherence.md` (edict-vs-observed grep; quoted edict + measured violation count; memory-proposal scoping).

**Retargeted scope**: For every edict in `docs/precepts/instructions/` (especially `tranche/SPEC.md`, `tranche/WAVE_SPEC.md`, `ORCHESTRATION.md`, `STYLE.md`, `LESSONS-LEARNED.md`), audit BA/BB/BC top-level + wave specs for adherence. Specific items: heavy-surface routine defaults; agent-cap (≤6 per wave); triumvirate auto-trigger thresholds; status-tick cadence; substrate-with-consumer; deletion bias; six-agent ceiling. Verbatim quote each violated edict + cite the BA/BB/BC line.

**Output**: `audit/HARDENING-2026-05-XX-02-edict-adherence.md` (≤700 lines).

### Lane 03 — BA/BB/BC Wave-Spec Drift
**Methodological exemplar**: `meta-audit/03-tranche-drift.md` (wave-spec vs PROGRESS.md vs FINAL.md drift; per-tranche table of doc state; D1-D8 numbered findings with fix proposals).

**Retargeted scope**: Audit consistency between (a) `<LETTER>.md` top-level and (b) `<LETTER>/waves/W*.md` and (c) cross-references to AZ-IV/FINAL.md, GESTALT.md, codegen-paths.md, and the DEEPX corpus. Specific items to verify: wave-status words match the `<LETTER>.md` wave table; hard gates referenced by waves match top-level numbering; file bounds disjoint across waves; carry-ledger items bound to a specific wave; non-routable carries enumerated with closure proof; cross-tranche dependencies (BA blocks BB blocks BC) cited honestly; deletion-bias targets surface in the wave that owns them, not aspirationally elsewhere.

**Output**: `audit/HARDENING-2026-05-XX-03-spec-drift.md` (≤700 lines).

### Lane 04 — BA Toolchain Pain Forecast
**Methodological exemplar**: `meta-audit/04-toolchain-pain.md` (wall-clock measurement matrix; pain-points ranked by friction × frequency; per-pain mechanism-level fix).

**Retargeted scope**: Forecast the toolchain pain BA's mechanism (direct-projection codegen + cheap-checkpoint + parse_with-as-value-API) will introduce or alleviate. Measure (where feasible): cold/warm walls for `cargo iter-check` after BA's emitter changes; regen wall after BA.W2's per-grammar typed-record emission; nextest wall after BA.W5's LegacyPath retirement. Identify 3-5 ranked pain points the wave specs underestimate and propose pre-W0 mitigations (e.g., per-grammar regen split if W2 emits 100K+ LOC of typed records).

**Output**: `audit/HARDENING-2026-05-XX-04-toolchain-forecast.md` (≤500 lines).

### Lane 05 — DEEPX Cohort Validation
**Methodological exemplar**: `meta-audit/05-validation.md` (verify prior audits' top claims against current state; cite verbatim grep / wc / sed evidence; identify stale or contradictory assertions).

**Retargeted scope**: Verify each of DEEPX-1..7's top claims against current master `d4085e4e` (post-DEEPX-8 spec landing + Phase 1 surgery). Sample: does DEEPX-1's "10 hardcoded `rule_type: TypeDesc::Span` emission sites" still grep at the named files? Has DEEPX-2's "1407 `checkpoint()` sites" count drifted? Does DEEPX-5's "Era 0 BumpArena<T> commit `f419b6d3`" still resolve via `git show`? Does DEEPX-7's claim "bbnf's `path!` produces compile-time grammar-aware diagnostics" hold against actual `crates/bbnf-path/src/path_macro.rs` behavior? Triage each claim **VALIDATED** / **NARROW** (claim correct but caveat surfaced) / **STALE** (claim drifted; correct or retract in BA spec).

**Output**: `audit/HARDENING-2026-05-XX-05-validation.md` (≤500 lines).

### Lane 06 — BA-Targeting Commit Archaeology
**Methodological exemplar**: `meta-audit/06-commit-archaeology.md` (1923 commits across all refs; six-era taxonomy; per-era thesis vs reversal vs cost; archaeology subdir for per-era deep dives).

**Retargeted scope**: DEEPX-5 already produced era-level archaeology; this lane produces **BA-targeting** archaeology — for each of the 24 BA hard gates, locate the prior commit (or commit cluster) that previously attempted to close it and either (a) succeeded then regressed, (b) failed and routed forward, or (c) never attempted. Produce a per-hard-gate row: `[gate # | prior attempt commit | outcome | what changed | what BA does differently]`. Surface gates that match category (a) or (b) more than once across history — these are the chronic risks BA must arm against (DEEP-D's "MASKED-DEFERRAL" pattern operationalised at the gate level).

**Output**: `audit/HARDENING-2026-05-XX-06-ba-archaeology.md` (≤700 lines).

### Lane 07 — Cross-Repo Appurtenant Posture
**Methodological exemplar**: `meta-audit/07-appurtenant-assay.md` (16 repos; per-repo posture record; cross-repo synthesis; toolchain harmonisation verdicts).

**Retargeted scope**: Re-assay the appurtenant ring (parse-that, pprint, csp-solver/csc411 sibling, wasm/, gorgeous, bbnf-buddy, precepts submodule, ffuzzy, etc.) against the BA mechanism. Specific questions: does BA.W2's typed-projection emitter cross-pollute parse-that's combinator surface? Does the `bbnf-regex` sub-crate-of-parse-that resolution (BC.W5 carry) require BA-time prep? Is the csp-solver canonical-source split current? Does BA introduce any new sibling-repo dependency (e.g., `bumpalo` per DEEPX-A's recommendation — which version, where pinned, does it cross with parse-that)? Verdict per repo: **READY** / **PRE-BA TOUCH** / **POST-BA TOUCH** / **OUT-OF-SCOPE-FOREVER**.

**Output**: `audit/HARDENING-2026-05-XX-07-appurtenant.md` (≤700 lines).

### Lane 08 — BA Substrate Abrogation Catalog
**Methodological exemplar**: `meta-audit/08-abrogation-catalog.md` (KEEP / KEEP-MODERNIZE / REPLACE / ABROGATE / FOLD-INTO-TOOLING verdicts; verdict counts table; per-verdict line-count delta).

**Retargeted scope**: For every substrate that BA touches or retires (per DEEPX-1..8's deletion-bias enumerations), assign one of five verdicts: **KEEP** (load-bearing, BA preserves), **KEEP-MODERNIZE** (BA refactors but preserves), **REPLACE** (BA emits a new substrate that supersedes), **ABROGATE** (BA deletes outright), **FOLD-INTO-TOOLING** (BA moves into xtask or scripts). Specific surfaces: `arena_template.rs` + `builder_template.rs`; per-grammar `arena.rs` + `builder.rs`; `__EAGER_EMPTY_PATH` LazyLock; `LegacyPath`/`LegacySegment` shim; `cursor.match_field` + `cursor.match_index` + `cursor.decide`; `Vec<OpenFrame>::clone` checkpoint; per-grammar `__path_plan` re-exports; the 32 zero-caller substrates (delete vs sanction-whitelist); `AscentStrategy` (DEEPX-A flagged consumer-less); `Option<&mut PathCursor>` parameter pattern. Per-verdict LOC delta + cumulative net delta.

**Output**: `audit/HARDENING-2026-05-XX-08-abrogation.md` (≤700 lines).

## Synthesis

After all 8 lanes return, synthesise at `audit/HARDENING-SYNTHESIS-2026-05-XX.md` with the structure of `HARDENING-SYNTHESIS-2026-05-01-FINAL.md` (the AZ-IV pre-flight synthesis): cohort table, disposition by lane (ACCEPT / NARROW / REJECT counts), cross-cutting themes, paste-ready amendment blocks per BA/BB/BC file, and a single decision: **BA W0 ready** (proceed) or **needs another amendment pass** (specify scope).

The synthesis is a CONTRACT: every paste-ready block must apply cleanly to the named file at the named line range, every cross-cutting theme must cite ≥2 lanes' findings, every retraction must name the prior claim verbatim and the new finding that supersedes it.

## Dispatch Discipline

- 8 sibling worktrees: `bbnf-wt-harden-{01..08}` (orchestrator creates with `git worktree add ... -b harden-NN master`)
- Per-agent `CARGO_TARGET_DIR=<worktree>/target/harden-NN`
- Read-only audit; one commit per lane for the audit doc
- HARD CAP per lane: lanes 01/04/05 → 25 min; lanes 02/03/06/07/08 → 35 min (these are wider-scope)
- Empty/no-evidence return → verbatim redispatch once → second empty triggers triumvirate
- JSONL transcript quiet >15 min triggers orchestrator-side check
- Every lane cites file:line evidence; no handwave; no aspiration

## Self-Contained Brief for the Next Orchestrator

```
You are the orchestrator of an 8-lane BA/BB/BC meta-audit hardening cohort.
Repo: /Users/mkbabb/Programming/bbnf-lang at master d4085e4e.

1. Read this prompt verbatim:
   docs/tranches/AZ-IV/audit/HANDOFF-HARDENING-PROMPT.md
2. Read the canonical meta-audit bundle as your methodological template:
   docs/tranches/meta-audit/{01..08}-*.md and docs/tranches/meta-audit/ARCHIVE.md
3. Read DEEPX-1..8 and DEEP-SYNTHESIS as the cohort inputs:
   docs/tranches/AZ-IV/audit/DEEPX-{1..8}-*.md
   docs/tranches/AZ-IV/audit/DEEP-SYNTHESIS.md
   docs/tranches/AZ-IV/audit/DEEP-{A,B,C,D}-*.md
4. Read BA/BB/BC current specs (the work-product to harden):
   docs/tranches/{BA,BB,BC}/{<LETTER>.md, waves/W{0..6}.md}
5. Create 8 sibling worktrees: bbnf-wt-harden-{01..08} at master with -b harden-NN.
6. Dispatch 8 lanes in parallel using the 8-Lane Mapping table above.
   Each lane gets the methodological exemplar, the retargeted scope, the
   output path, and the HARD CAP. Each writes one audit doc.
7. After all 8 return, synthesise at:
   audit/HARDENING-SYNTHESIS-2026-05-XX.md
   patterned exactly after audit/HARDENING-SYNTHESIS-2026-05-01-FINAL.md
8. Cherry-pick all 9 commits to master.
9. Apply paste-ready amendments to BA/BB/BC where the synthesis prescribes.
10. Re-validate cross-references with the user's 11 invariants verbatim.
11. Decide: BA.W0 ready, or 4th-pass amendment needed?
12. Return a single completion message: state of BA/BB/BC; commit hash of
    HARDENING-SYNTHESIS; BA.W0 readiness signal; routed carries to BB/BC/BD+
    with named close criteria.

Non-negotiables:
- File bounds: audit/ + tranches/{BA,BB,BC}/ only; no source-code commits.
- KISS / no workarounds / one path / canonical AZ→BA→BB→BC→BD.
- Cite file:line evidence in every finding.
- Triumvirate fires on the auto-triggers in
  docs/precepts/instructions/ORCHESTRATION.md §Triumvirate Auto-Triggers.
- Pre-existing memory at
  /Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/
  is read; do not duplicate; update only if new feedback surfaces.
```

This prompt is the handoff. The next agentic system reads it, executes the 8-lane cohort, returns the synthesis. After that returns and the orchestrator re-validates, BA.W0 dispatches.
