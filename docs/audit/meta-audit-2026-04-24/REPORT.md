# Meta-Audit Report — 2026-04-24

Audit target: `bbnf-lang` at `/Users/mkbabb/Programming/bbnf-lang`.
Audit scope: the audit-only brief now stored as `docs/META-AUDIT-PROMPT.md`,
the canonical tranche/docs corpus, the three Claude JSONL transcripts named
in that brief, and the live repo state at this audit pass.

This report is audit-only. It does not open tranches, dispatch agents,
rewrite canon, or authorize execution work.

## 1. Baseline Verification Ledger

| Surface | Status | Evidence |
|---|---|---|
| Git baseline | PASS with drift noted | Live HEAD is `a6f99cc1`, three commits beyond the brief's `56a67e2e` master baseline. Worktree was clean before this report file was added. |
| Meta-audit prompt | PASS | `docs/META-AUDIT-PROMPT.md` exists at HEAD and preserves the audit-only contract. |
| Meta-audit 2026-04-23 bundle | PASS | `docs/audit/meta-audit-2026-04-23/INDEX.md` plus axis 1 through axis 8 exist. |
| Canon docs | PASS with numeric drift | `docs/GESTALT.md` and `docs/RISK-PERF-MATRIX.md` exist. `GESTALT.md` headline counts are stale against current git counts; see finding D-3. |
| Runway tranche tree | PASS | `docs/tranches/B1`, `AY-II`, `AZ-I`, `AZ-II`, `BA`, and `BB` exist with wave specs and `PROGRESS.md`. |
| Instructions edicts | PASS | `docs/instructions/README.md`, `PROFILING.md`, `CHANGELOG.md`, and `docs/instructions/tranche/*` exist. |
| Benchmark corpus | PASS | `docs/benchmarks/post-*.json` exists through `post-B0.json`; B1/AZ/BA/BB bench artefacts are not expected yet. |
| Historical transcripts | PASS | The three named JSONL files exist. Simple string-user extraction returned 46 / 46 / 105 messages, close to the brief's 50 / 46 / 107 after sentinel and structured-content filtering. |
| Claude memory | PASS | `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md` exists. |
| Project worktree admin | DRIFT | `.claude/worktrees/` is not empty: `parse-that` and `pprint` remain. The brief expected empty or near-empty steady state. |

## 2. Invariant Findings

### I-1 — AZ-I Still Pre-Declares a CSS Tape Escape

Priority: P1

The hard invariant says AZ-I leaves tape retained for BBNF only, and the
brief treats `AZ-I scopes non-BBNF grammars (tape retained for BBNF only)` as
a baked decision. Current canon is contradictory:

- `docs/GESTALT.md:40-43`, `docs/GESTALT.md:514-520`, and
  `docs/GESTALT.md:1159-1163` say AZ-I moves JSON / CSS L4 / Sheets to
  direct-to-struct and retains tape for BBNF only.
- `docs/RISK-PERF-MATRIX.md:124-125` says W4 scopes tape to BBNF only, but the
  tranche-close floor says tape is retained for CSS + BBNF.
- `docs/RISK-PERF-MATRIX.md:258-263` repeats the CSS + BBNF tape floor.
- `docs/tranches/AZ-I/AZ-I.md:306-309` and `:386-396` explicitly allow CSS
  aggregates to remain tape-backed in a partial close.

This is failure-mode D1 / D2 reappearing in a new form: a hard direct-to-struct
decision has been softened into a pre-declared partial substrate floor. It also
breaks the AZ-I handoff contract to AZ-II, which assumes CSS L4 has already
left the tape.

Minimum redress: choose one canon. If the brief and GESTALT are authoritative,
remove the CSS tape-backed partial-close clause from `AZ-I.md` and
`RISK-PERF-MATRIX.md`; make CSS L4 partial mean typed-parity gap, not tape
retention.

### I-2 — B1 Toolchain Docs Still Contain Pre-Redress Linker Posture

Priority: P2

B1 redress decided that macOS lld is opt-in, uses the separate `lld` Homebrew
formula, and remains commented unless explicitly enabled. Current docs still
carry stale statements:

- `docs/tranches/B1/TOOLCHAIN-SOTA.md:394-395` recommends enabling lld by
  default on macOS arm64.
- `docs/tranches/B1/TOOLCHAIN-SOTA.md:676-685` repeats "Enable lld on macOS
  arm64 by default".
- `docs/tranches/B1/TOOLCHAIN-MIGRATION.md:97-104` says lld "Requires
  `brew install llvm`".
- The same migration doc later corrects this at `:365-381`, and
  `docs/tranches/B1/patches/config.toml.draft:135-163` correctly makes lld
  opt-in via `brew install lld`.

Minimum redress: normalize the SOTA recommendation and migration decision
ledger to match the redressed posture: default ld64, lld opt-in, separate
`brew install lld`, no `llvm` prerequisite.

### I-3 — `ir-rewrites` Module Decision Holds

Priority: none

No recurrence found. `docs/tranches/BB/BB.md:123-126` explicitly rejects a
standalone `ir-rewrites` crate, and BB wave specs consistently use
`crates/ir/src/rewrites/`.

### I-4 — `bbnf-tape-mini` Decision Holds

Priority: none

No recurrence found outside the audit prompt's historical quotations. AZ-II
canon says no shrunken-tape floor and requires wholesale `crates/tape/`
deletion (`docs/tranches/AZ-II/AZ-II.md:69-74`, `:281-292`).

## 3. Discipline Findings

### D-1 — Worktree Cleanup Drift

Priority: P3

`.claude/worktrees/` contains `parse-that` and `pprint`. The brief states this
directory is expected empty or near-empty in steady state and non-empty entries
are cleanup findings. This is administrative drift, not architecture drift.

Minimum redress: inspect whether those worktrees hold unmerged work; if not,
remove them in a separate cleanup pass.

### D-2 — Generated File Discipline Not Re-Audited by Build

Priority: informational

This audit did not run bootstrap regeneration or compile checks. The docs
preserve the rule that generated output must come from clean regen, but this
report does not prove `generated.rs` is byte-identical to fresh output.

Minimum redress: B1/W0 or AY-II/W0' should rerun the already-declared
double-regen gate and cite the artefact.

### D-3 — GESTALT Headline Counts Are Stale

Priority: P3

`docs/GESTALT.md:25-27` and `:63-67` report 1,890 master commits, 945
unpushed, and 24 feature branches. Live counts at audit time are:

- `git rev-list --count HEAD` → 1,898
- `git rev-list --count origin/master..HEAD` → 1,001
- local heads → 55

The doc says counts drift with every commit, so this is not a conceptual
contradiction. It is still current-state canon drift.

Minimum redress: either refresh the count rows or mark them explicitly as
synthesis-time counts rather than live canon.

## 4. Runway Drift Ledger

| Runway element | Status | Evidence |
|---|---|---|
| Sequence B1 → AY-II → AZ-I → AZ-II → BA → BB | PASS | `docs/GESTALT.md:37-59` and `docs/RISK-PERF-MATRIX.md:211-215` agree. |
| BB parallel with AZ-II where disjoint | PASS | `docs/GESTALT.md:684-690`; `docs/RISK-PERF-MATRIX.md:181-186`. |
| AZ-I tape retained for BBNF only | DRIFT | Contradicted by AZ-I partial-close floor; see I-1. |
| AZ-II required, no partial floor | PASS | `docs/RISK-PERF-MATRIX.md:286-298`; `docs/tranches/AZ-II/AZ-II.md:281-292`. |
| BA opens only after AZ-II close | PASS | `docs/RISK-PERF-MATRIX.md:162-166`; `docs/tranches/BA/BA.md` agrees. |
| B1 toolchain pin / divan / nextest | PARTIAL | Plans and patch drafts exist. Live root and sibling `rust-toolchain.toml` files are not yet present, which is expected because B1 is planned, not executed. |

## 5. Purview-Scaffold Populations

### 5.1 Gestalt Drift Ledger

| Element | Status | Evidence / gap |
|---|---|---|
| One grammar surface | PARTIAL | Grammar-authoritative doctrine is intact in `GESTALT.md:94-120`; full fleet closure waits on AZ-I/AZ-II. |
| One IR substrate | YES | `crates/ir` remains the declared analysis substrate; BB rewrites live under `crates/ir/src/rewrites/`. |
| One parse path / no `to_value()` reparse | PARTIAL | `crates/core/src/runtime/parsed.rs` documents no second parse; AY-II wave specs still own final proof. |
| Grammar-derived semantics | PARTIAL | Canon asserts the invariant; CSS/AZ-I partial tape escape weakens it until redressed. |
| CSP + e-graph as pluggable optimisation substrates | YES | GESTALT and BB specs preserve CSP/e-graph roles. |
| VM bounded oracle on residue | YES | BB owns VM as oracle, not runtime, in `docs/tranches/BB/BB.md`. |
| Backend emitters Rust / TS / WASM / Python where declared | PARTIAL | Rust is live; TS/Python/BA path macro remain planned. |
| Direct-to-struct with tape abrogated | PARTIAL | AZ-I/AZ-II plan exists; current runtime still has tape. AZ-I CSS escape is the main drift. |
| Competitor parity / superiority | PARTIAL | Harnesses exist; current performance is below AU and competitor parity for JSON value path. |
| Sibling repos pinned/path-patched | PARTIAL | Path patches are live in `.cargo/config.toml:28-42`; pin propagation is planned in B1.W2 but not live. |

### 5.2 Archaeology Table

| Era | Thesis | Commit range | Key pivot | Retired on exit | Landed on entry | Carry-forward lesson |
|---|---|---|---|---|---|---|
| I | LSP/TextMate prelude | 2023-03-03 → 2023-03-06 | Initial extension prototype | None | TextMate/LSP seed | Historically relevant, not architecturally continuous. |
| II | Monorepo + IR foundations | 2026-02-26 → 2026-03-15 | IR crate and grammar notation freeze | None | Workspace scaffold | Atomic commits before tranche protocol. |
| III | Optimiser substrate | 2026-03-16 → 2026-04-09 | CSP, e-graph, regex HIR, NodeId, IndexMap | GrammarAnalysis lattice | Pluggable optimisation substrate | Compile-time decisions must reach emitted constants. |
| IV | Tape-first runtime | 2026-04-10 → 2026-04-15 | AU baseline at `5281ec23` | EmissionTier/structural dispatch at AQ.5 | 17-entry AU baseline | Bench artefacts become tranche law. |
| V | DTA/PSI activation rut | 2026-04-15 → 2026-04-19 | AW-V 0/17 recovery, AX reckoning | DTA walker, PSI/interpreter substrate, ~78K LOC | Shape-emitter lessons, parity harnesses | Substrate without same-commit consumer is debt. |
| VI | Infra-truth restart | 2026-04-20 → present | AY-I column revert, AY-II pause, B1 prelude | Column experiment; `note_push` slated for retirement | B1/AZ-I/AZ-II/BA/BB runway | Verify dev loop and measurement before runtime execution. |

### 5.3 Performance Arc

| Grammar | AU baseline | Era V low / close | Current cited | Next close target | Competitor posture |
|---|---:|---:|---:|---:|---|
| JSON twitter | 1967 MB/s | AW-V 486 MB/s | AY-I.W1 688 MB/s | AZ-I.W2 ≥ 1967 | AY-I value path 3.995× sonic on twitter; visitor lane geomean 0.99× sonic, not current `to_value()` close. |
| JSON canada | 1231 MB/s | AW-V 227 MB/s | AY-I.W1 ~450 MB/s cited in matrix | AZ-I.W2 ≥ 1231 | Sonic/simdjson parity harnesses exist; current parity not achieved. |
| CSS normalize/bootstrap/tailwind | 735 / 454 / 496 MB/s | AW-V 24 / 14 / 36 MB/s | AY-I.W1 ~300 / ~200 / ~210 cited | AZ-I.W3 ≥ 735 / 600 / 496 | Lightningcss/csparser harnesses exist; full typed parity remains planned. |
| Sheets parse_simple | 95 MB/s | AW-V 6 MB/s | AY-I.W1 ~45 MB/s cited | AZ-I.W2 ≥ 95 | No external peer equivalent; AU recovery is the first gate. |
| BBNF self | correctness at AU; 394 MB/s in `post-AU.json` | AW-V 22 MB/s | functional tape substrate | AZ-II byte-identical + ≥ current + 10% | No external peer; self-hosting identity is the comparator. |

No warm-bench citation was found in canon during targeted searches. The main
performance finding remains not a hidden regression but the known unrecovered
gap: current cited JSON twitter is 35% of AU, and the competitor value path is
not closed.

### 5.4 Abandoned Paths Ledger

| Subsystem / approach | Era tried | Retired in | Successor / lesson |
|---|---|---|---|
| EmissionTier / structural dispatch | IV | AQ.5 (`2f7c1bd4`) | Collapse to `PayloadKind → TypeDesc`; no orthogonal codepaths. |
| DTA walker / PSI interpreter substrate | V | AX.W0b (`bc550d2c` / `a206b962` / `0adabb23`) | Direct codegen path; VM survives only as BB residue oracle. |
| Seven-column tape | IV/V | AY-I.W1 | Single `Vec<TapeRec>` + parallel `sib_skip`; column work not resumed. |
| `note_push` / `SIB_SKIP_STAMPED_BIT` | AY-I | Retired by AY-II plan | `FusedBuilder` + rollback/finaliser discipline. |
| `navigate_tape` dead consumer | AY-I | Routed to AY-II.W0 retirement | Same-path `to_value()` / path consumer only. |
| `bbnf-tape-mini` | Planning | Rejected post-audit | Full AZ-II tape deletion; no partial floor. |
| `ir-rewrites` crate | Planning | Redressed in BB | Module under `crates/ir/src/rewrites/`. |
| `bencher` harness | Current pre-B1 | B1 planned removal | Divan primary; iai-callgrind CI secondary. |

### 5.5 External Integration

Competitor posture:

| Competitor | Grammar | Target | Current delta / state | Evidence |
|---|---|---|---|---|
| sonic-rs | JSON | parity/beat | AY-I `to_value()` twitter 3.995× slower; visitor lane geomean 0.99× but not the fused close path | `docs/tranches/AY-I/FINAL.md:126-133` |
| simdjson / simd-json | JSON | parity/beat | Harness planned/live; no current superiority claim proven post-B1 | `crates/core/benches/json/competitors.rs` |
| lightningcss | CSS L4 | typed node-for-node parity | Harness exists; full CSS L4 typed parity remains AZ-I.W3/AY-II.W2 work | `docs/tranches/AZ-I/AZ-I.md:135-138` |
| cssparser | CSS L4 | reference harness | Present as comparator, not the top target | `crates/core/benches/css/competitors.rs` |

Sibling state:

| Sibling | Pin match | Path-patched | Modernisation state |
|---|---|---|---|
| `../parse-that` | NO live `rust-toolchain.toml` found | YES (`parse_that`, `bbnf-regex`) | B1.W2 plans pin propagation and nextest posture. |
| `../pprint` | NO live `rust-toolchain.toml` found | YES (`pprint`, `pprint_derive`) | B1.W2 plans pin propagation. |
| `gorgeous` sibling | N/A | Workspace-local `crates/gorgeous` | Sibling deletion decision appears respected; no external gorgeous path patch. |
| `csp-solver` | Workspace-local | YES (`crates/csp-solver`) | Wider sibling reconciliation remains documented, not live. |

### 5.6 Open Contradictions / Live Questions

| Question | Source | Load-bearing for | Status | Proposed resolution |
|---|---|---|---|---|
| Can AZ-I close with CSS aggregates tape-backed? | `AZ-I.md:386-396` vs GESTALT | AZ-I/AZ-II boundary | Blocking contradiction | Remove or explicitly re-authorize the escape; current brief says no. |
| Is lld default or opt-in? | `TOOLCHAIN-SOTA.md` vs migration/config draft | B1.W0 | Non-blocking doc drift | Normalize to opt-in `brew install lld`. |
| Are pin files absent because B1 is not executed? | B1 plan vs live root | B1.W0/W2 | Non-blocking | Treat as planned until B1 opens; do not call it drift now. |
| BA.W2.a research depth | User edict + BA plans | BA | Open | Keep as W0/W2 research gate; do not improvise bindings. |
| Cranelift activation | B1 docs | B1 dev-loop | Open | Component in draft, backend commented until measured. |
| Worktree admin residue | `.claude/worktrees/` | Orchestration hygiene | Non-blocking cleanup | Verify and remove stale admin worktrees in a separate pass. |

## 6. Failure-Mode Recurrences

| Pattern | Recurrence? | Evidence |
|---|---|---|
| D1/D2 softening hard decisions / escape as plan | YES | AZ-I CSS tape-backed partial floor. |
| D3 probability as hedge | PARTIAL | Probability tables are generally honest, but AZ-I floor uses probability language to preserve a forbidden substrate state. |
| D5 auditing the audit forever | NO | This pass found concrete contradictions; report stops here. |
| D6 zombie tasks | PARTIAL | `.claude/worktrees/` residue is a cleanup analogue, not a TaskList recurrence. |
| D7 hand-patching generated output | NOT VERIFIED | No direct recurrence found; regen not run. |
| D8 parsimonious output | NO | Prior prompt and this report are complete enough for redress. |
| D9 doc drift | YES | AZ-I floor and B1 lld posture drift across related docs. |
| D10 competitor gates deferred | PARTIAL | Gates are in specs, but current state remains below parity; no false current "beat" claim found. |
| D11 propose-execute coupling | NO | This report does not execute runway work. |
| D12 slow dev loop shrugged off | NO | B1 exists and owns it. |

## 7. Brief Integrity

The brief is mostly execution-clean. It repeatedly states audit-only scope,
and the imperative passages that remain are audit workflow instructions, not
tranche execution instructions.

One integrity note: the prompt intentionally contains `[auditor fills this
during audit]` scaffold markers. They are not prior-audit incompleteness; they
are the current auditor's work surface. This report populates those scaffolds.

## 8. Proposed Next Steps

1. Redress AZ-I canon first: remove the CSS tape-backed partial-close floor or
   obtain explicit user re-authorization. This is the only P1 finding.
2. Normalize B1 linker/toolchain docs so every B1 file says lld is opt-in,
   installed via `brew install lld`, and not default.
3. Refresh or timestamp `GESTALT.md` headline counts.
4. Inspect and clean `.claude/worktrees/parse-that` and `.claude/worktrees/pprint`
   if they contain no live work.
5. Do not execute B1 / AY-II / AZ-I from this report. Redress should be a
   separately authorized pass.
