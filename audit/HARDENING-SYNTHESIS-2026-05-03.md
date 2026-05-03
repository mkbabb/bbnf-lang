# Hardening Synthesis — BA/BB/BC Audit

Date: 2026-05-03  
Repo: `/Users/mkbabb/Programming/bbnf-lang`  
Observed HEAD: `baf7df2d07cd130a5ad2b8f81fc339418406a3b3`  
Prompt baseline: `c5a6fab9` (stale)  
Scope: audit-only. No source, tranche spec, or arc-state mutation was performed.

## 1. Cohort Table

| Lane | Agent | Worktree | Audit doc | Commit | Lines |
|---:|---|---|---|---|---:|
| 01 Spec-Friction | Volta | main audit workspace | `audit/HARDENING-2026-05-03-01-spec-friction.md` | n/a | 265 |
| 02 Edict-Adherence | Wegener | main audit workspace | `audit/HARDENING-2026-05-03-02-edict-adherence.md` | n/a | 175 |
| 03 Spec-Drift | Harvey | main audit workspace | `audit/HARDENING-2026-05-03-03-spec-drift.md` | n/a | 351 |
| 04 Toolchain-Forecast | Banach | main audit workspace | `audit/HARDENING-2026-05-03-04-toolchain-forecast.md` | n/a | 139 |
| 05 Cohort-Validation | Kuhn | main audit workspace | `audit/HARDENING-2026-05-03-05-cohort-validation.md` | n/a | 45 |
| 06 Tranche-Archaeology | Halley | main audit workspace | `audit/HARDENING-2026-05-03-06-tranche-archaeology.md` | n/a | 85 |
| 07 Appurtenant-Posture | orchestrator-local | main audit workspace | `audit/HARDENING-2026-05-03-07-appurtenant.md` | n/a | 82 |
| 08 Substrate-Abrogation | orchestrator-local | main audit workspace | `audit/HARDENING-2026-05-03-08-abrogation.md` | n/a | 101 |

The brief's self-contained orchestrator block asks for sibling worktrees and later cherry-picks, but the same brief's audit-only contract forbids commits and arc mutation. The audit honoured the audit-only contract.

## 2. Disposition By Lane

| Lane | Disposition | Reason |
|---:|---|---|
| 01 | ACCEPT | Latest two long transcripts mined; Bash-tail polling, foreground cargo, empty-return, worktree contention, and status tick gaps quantified. |
| 02 | ACCEPT | Edict coverage is broad and cites violations; BB.W0 substrate-forward skeletons, BA HARD CAP extension, and missing generated-size budgets are blocking findings. |
| 03 | ACCEPT | Eight concrete drift findings with paste-ready amendments; D1/D2/D3/D8 block BA.W0 readiness. |
| 04 | NARROW | Lightweight wall clocks captured; full nextest/samply intentionally not run. The forecast still establishes a required BA.W0 toolchain baseline amendment. |
| 05 | NARROW | Twenty predecessor claims triaged; profile artefact directory was absent, so the 86.07% sample share is source-validated by mechanism but not independently reopened. |
| 06 | ACCEPT | All 24 BA hard gates mapped to prior attempts; archaeology supports BA's thesis but insists on strict gate preservation. |
| 07 | ACCEPT | Appurtenant topology checked; `bbnf-regex` endpoint drift and sibling dirty-state baseline are concrete BC findings. |
| 08 | ACCEPT | Deletion targets inventoried; `AscentStrategy` lacks an active wave owner and BA.W2 needs per-grammar arena/builder disposition. |

Counts: ACCEPT 6, NARROW 2, REJECT 0.

## 3. Cross-Cutting Themes

1. **Substrate-without-consumer remains the highest-risk pattern.** Lane 02 and Lane 03 independently flag BB.W0's rank/tier skeletons as W3-deferred consumers. Lane 08 confirms BA has many real deletion targets, so BB must not reintroduce the chronic pattern immediately after BA.
2. **BA's TS/WASM language leaks execution authority.** Lane 03 flags contradictory BA text that both punts TS/WASM to BD and implies BA may close `ts_node_execute` through TS aggregate emit. That violates the user's explicit punt and risks source edits outside BA's Rust thesis.
3. **Evidence machinery exists, but the plans under-budget it.** Lane 04 measured `cargo xtask regen --check` at 59.98 s, with CSS L4 `compile_paths_request` at 52.53 s. Lane 02 flags missing generated-size budgets. Lane 05 confirms substrate-audit still fails with 32 zero-caller substrates.
4. **The architectural defect is still live and correctly targeted by BA.** Lane 05 validates the generated parser still constructs empty parse-time layouts, `JsonParser::get<T>` still does not exist, eager get still records the 4196.5x gap, and checkpoint clone sites remain structurally present.
5. **Brief and canon diverged in bounded ways.** The baseline SHA is stale; the prompt's 10 hardcoded emitter sites are now 9; `crates/bbnf-path` now exists; `TypedPath` terminal type inference remains narrow; residual `AZ-V` references are historical only.

## 4. Paste-Ready Amendment Blocks

### `docs/tranches/BA/BA.md`

```md
9. **Failing-test census is canonical.** Workspace nextest is 100% pass at every BA wave close for BA-owned Rust surfaces. AZ-IV's RED `substrate_audit` test closes in BA.W0. AZ-IV's RED `ts_node_execute` is not BA-owned; BA close records it as `#[ignore]` or equivalent non-blocking status with owner `BD` / TS-WASM re-engineering, reason, and deadline, and does not edit TS or WASM emitters.
```

```md
| F5 TS Node-execute | Audit-C MASKED-DEFERRAL | routes to BD (TS/WASM) per user punt | BA close names BD with owner/reason/deadline; BA does not edit TS or WASM emitter code |
| F4 Tailwind regex_scan timeout | Audit-C CHRONIC-RISK | W2 disposition; closes in W2 only with profile evidence, otherwise routes to BB.W3 | direct-projection profile either proves the regex_scan path resolves here OR records a routed BB.W3 grammar-specific rule-discovery close criterion |
```

```md
11. If a write-authorized agent reaches N without a clean owned commit, it halts and returns current state; the orchestrator either narrows bounds, dispatches triumvirate, or redeploys with an amended plan. HARD CAP extension after overrun is forbidden.
14. Status ticks every ~5 minutes of orchestrator-silent wait while agents are running; each tick names live agents, worktrees, last transcript touch, and next decision point.
```

```md
22. **Worktree fixture symlink contract codified for BA-local fixtures**: `data/{json,css,bbnf,sheets}` materializes on worktree open via `xtask worktree-init`; the fleet-wide post-BB `rewrites/*.ron` closure remains BC.W2.
```

### `docs/tranches/BA/waves/W0.md`

```md
6. Worktree fixture symlink contract: codify `data/{json,css,bbnf,sheets}` materialization on worktree open via `xtask worktree-init` or equivalent. This installs the BA-local prerequisite for the W6.2 known miss; the fleet-wide post-BB closure remains BC.W2 after `grammar/<name>/rewrites/*.ron` exists.
```

```md
Add to Verification Artefacts:

- `docs/tranches/BA/audit/W0-toolchain-baseline.md` — records per-grammar `cargo xtask regen --check` timings, full nextest wall, generated warning count by lint/file, active cargo lock/process preflight, and CSS L4 `compile_paths_request` baseline. Later waves cite this when regen, nextest, warning volume, or target-lock behaviour changes.
```

### `docs/tranches/BA/waves/W2.md`

```md
Replace every `BB.W1` F4 destination with `BB.W3`.

F4 Tailwind regex_scan: profile under direct-projection. If the per-call layout-construction overhead is removed by mechanism, F4 closes here. If not, F4 routes to BB.W3 as a grammar-specific rule-discovery candidate, with the exact path-shape rewrite hypothesis and CSS L4 close-matrix row named in the W2 close commit body.
```

```md
Add to Verification Artefacts:

- `docs/tranches/BA/audit/W2-generated-size-budget.md` — one row per generated grammar output: pre-wave LOC, expected post-wave LOC window, warning-count delta, overflow disposition, and close artefact path.
- `docs/tranches/BA/audit/W2-arena-builder-disposition.md` — one row per grammar: `arena.rs` action, `builder.rs` action, consumer if retained, direct-projection replacement, and verification command.
```

### `docs/tranches/BA/waves/W3.md`

```md
Add to Verification Artefacts:

- `docs/tranches/BA/audit/W3-checkpoint-source-shape.txt` — output from `rg -n "stack\\.clone\\(|\\.stack\\.clone\\(|Vec<OpenFrame>::clone|fn checkpoint" crates/core/src/runtime crates/core/src/grammar/generated`, paired with the samply 7-artefact proof. The grep proves source-shape retirement; samply proves cost retirement.
```

### `docs/tranches/BB/waves/W0.md`

```md
Rank/tiering are not created in W0; W3 creates and consumes them in the same wave.

Delete W0.3 "Rank/Tier Skeletons" and remove `crates/ir/src/rewrites/rank.rs`, `crates/ir/src/rewrites/tiering.rs`, `ir/rewrites/rank-skeleton`, and `ir/rewrites/tiering-skeleton` from File Bounds, Disjointness, Worktree Plan, Agent Units, Hard Gate 1, and Commit Plan.

Hard Gate 1 becomes:

1. `crates/ir/src/rewrites/{mod,schema}.rs` exist and are functional; base RON rules validate; no W3-deferred rank/tier skeleton exists.
```

### `docs/tranches/BB/waves/W3.md`

```md
Add File Bounds:

| `crates/ir/src/rewrites/rank.rs` | create |
| `crates/ir/src/rewrites/tiering.rs` | create |

Hard Gate 1 becomes:

1. `crates/ir/src/rewrites/rank.rs` and `tiering.rs` are created, implemented, and consumed by the W3 run in the same wave.
```

### `docs/tranches/BB/waves/W4.md`

```md
Add to Verification Artefacts:

- `docs/tranches/BB/audit/W4-generated-size-budget.md` — one row per generated grammar output: pre-wave LOC, expected post-wave LOC window, rewrite-driven delta, overflow disposition, and close artefact path. The existing "one grammar shrinks >= 10 LOC" gate remains necessary but is not sufficient for the other generated artefacts.
```

### `docs/tranches/BB/BB.md`

```md
Replace the W0 wave-table close text with:

regen drift cleared; cost extractor live; `crates/ir/src/rewrites/{mod,schema}.rs` recreated clean; base RON rules land; no W3-deferred rank/tier skeleton; substrate_audit GREEN

Replace Critical Files row "Ranker + tiering" with:

| Ranker + tiering | W3 | `crates/ir/src/rewrites/{rank,tiering}.rs`, `crates/ir/tests/rank_*.rs` |
```

### `docs/tranches/BC/BC.md`

```md
Add to Carry Ledger:

| AUDIT-B `dta.rs` / `grammar_facts` split | AUDIT-B routed | W0 inventory -> W1 or W4 disposition | W0 names exact owner wave and close criterion; no item remains in placeholder state |
```

### `docs/tranches/BC/waves/W0.md`

```md
Add to Verification Artefacts:

- `docs/tranches/BC/audit/W0-sibling-baseline.txt` — captures `git rev-parse --short HEAD` and `git status --short` for parse-that, pprint, csc411/csp-solver, bbnf-buddy, and ffuzzy before BC edits. BC.W5 uses this baseline to separate pre-existing sibling dirt from W5 output.
- `docs/tranches/BC/audit/W0-ascent-strategy-disposition.md` — assigns `AscentStrategy` KEEP / KEEP-MODERNIZE / ABROGATE with a named production consumer or deletion owner.
```

### `docs/tranches/BC/waves/W5.md`

```md
Before W5 dispatch, run:

`test -d /Users/mkbabb/Programming/parse-that/rust/regex`
`test ! -d /Users/mkbabb/Programming/parse-that/rust/bbnf-regex`
`rg -n "bbnf-regex\\s*=.*parse-that/rust/regex|parse-that/rust/bbnf-regex" .cargo/config.toml docs/tranches/BC docs/GESTALT.md`

W5 must choose and document one endpoint. Either rename `parse-that/rust/regex` to `parse-that/rust/bbnf-regex` and update parse-that workspace membership plus bbnf-lang's path patch, or preserve the existing endpoint `parse-that/rust/regex` and amend BC.W5 / GESTALT references accordingly.
```

### `docs/GESTALT.md`

```md
### Transitional parse state after AZ-IV; BA closes one path

Post-AZ-IV has two entry surfaces that share generated code but not yet one value-API path: eager parse still materializes a document, while `parse_with` is path-driven. BA.W4 is the planned closure point where eager becomes `parse_with(input, &EMPTY_PATH)` and `Document::get<T>` / `<Grammar>Parser::get<T>` consume that single path.
```

### `docs/codegen-paths.md`

```md
Add a post-AZ-IV / pre-BA note near the Rust AOT parse-mode section:

Current forward state before BA.W4: generated parsers expose eager `parse` and lazy `parse_with`; BA.W4 is responsible for collapsing eager into `parse_with(input, &EMPTY_PATH)` and retiring `__EAGER_EMPTY_PATH`.
```

### Standard W0-W5 Commit-Plan Line

```md
Wave-close doc/status commit updates `PROGRESS.md`, this wave's `Status`, the parent wave table, and the named audit artefacts before the next wave opens.
```

Apply this line to BA/BB/BC W0-W5 commit plans.

## 5. Readiness Decision

BA.W0 is **not ready**; another amendment pass is required before execution, because D1/D2/D3/D8, Edict A2/A3/A4/A5, the BA.W0 toolchain baseline, and the prompt-level `AscentStrategy` owner gap affect opening authority or close evidence.

## 6. Brief Integrity

| Finding | Evidence | Disposition |
|---|---|---|
| B1. Baseline drift | Brief says audit baseline `c5a6fab9`; observed HEAD is `baf7df2d`. | Flagged. Future prompt should update §Audit subjects baseline. |
| B2. Mode-slip in self-contained orchestrator block | Brief begins with audit-only contract, but later instructs "Create 8 sibling worktrees", "Dispatch 8 lanes", "cherry-pick all 9 commits", and "Apply paste-ready amendments" (`docs/HARDENING-AUDIT-PROMPT.md:617-626`). | The audit-only contract is authoritative; the execution lines are voice-leak findings. |
| B3. Emitter-site count drift | Prompt/preamble cites 10 hardcoded Span emission sites; cohort validation finds current unique emitter count is 9, while generated parser empty-layout counts remain broader. | Amend future prompt to cite 9 current emitter sites plus generated empty-layout counts. |
| B4. SOTA-path state drift | Older predecessor claim that `crates/bbnf-path` does not exist is stale; it now exists, but terminal TypeDesc return inference remains narrow. | Future prompt should distinguish compile-time path diagnostics landed from return-type inference still open. |
| B5. `AscentStrategy` deletion target lacks wave owner | Prompt lists `AscentStrategy` as a current deletion target; active BA/BB/BC specs do not own it. | Assign BA.W5 or BC.W0, or remove from current-cycle deletion list. |

## 7. Routed Carries

| Carry | Destination | Close criterion |
|---|---|---|
| TS/WASM / `ts_node_execute` | BD | BA/BC FINAL name BD owner/reason/deadline; BA/BC do not edit TS/WASM emitters. |
| F4 Tailwind `regex_scan` if direct-projection does not close it | BB.W3 | Grammar-specific rule-discovery candidate has named path-shape rewrite hypothesis and CSS L4 close-matrix row. |
| Fleet-wide worktree fixture + `rewrites/*.ron` materialization | BC.W2 | `xtask worktree-init` materializes every grammar's data and rewrite fixtures after BB introduces `grammar/<name>/rewrites/*.ron`. |
| `bbnf-regex` endpoint reconciliation | BC.W5 | One canonical path is chosen and both parse-that workspace membership and bbnf-lang path patch agree. |
| `AscentStrategy` | BA.W5 or BC.W0, pending amendment | Named production consumer keeps it, or deletion owner removes it with tests. |
| Generated-size budgets | BA.W2 and BB.W4 | Per-generated-file LOC windows and warning deltas recorded before emitter/regen close. |
| 32 zero-caller substrates | BA.W0 | `substrate_audit` green; each row deleted or whitelisted with same-wave/later-wave consumer and rationale. |
| Sibling dirty-state ambiguity | BC.W0 | `W0-sibling-baseline.txt` captures sibling HEAD/status before cross-repo edits. |

No routed carry names a fictional successor letter.
