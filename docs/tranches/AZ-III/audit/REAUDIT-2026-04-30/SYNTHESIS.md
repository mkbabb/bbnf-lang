# AZ-III REAUDIT 2026-04-30 - Synthesis

Six read-only audit lanes inspected the workspace at master HEAD `d5179b8a`
to validate or refute the prior 2026-04-30 SIX-AGENT-SYNTHESIS, the AZ-III
plan, the AZ-II terminal close, the precepts framework, and the dev-loop
posture. This synthesis sequences findings into accepted, narrowed, and
speculative dispositions, names the path forward, and carries the dispatch
packets for the redress wave.

The user claim driving the reaudit was concrete: "the last several tranches
have not landed quite properly," with explicit asks to confirm, fold
chronically deferred items, refine AZ-III, and validate precepts.

## Lane Inputs

| Lane | Deliverable | Headline |
|---|---|---|
| 1 - Failure Baseline | `01-failure-baseline.md` | 4/9 gates red at HEAD; 1446/1509 tests pass; 9/9 grammars regen-drifted; 42 clippy errors across 4 infra crates; sibling `parse-that` red on published `parse_that 0.3.3`; AZ-II FINAL.md O5 gate-1 status is stale-BLOCKED while no-default build is actually GREEN in 35.57 s |
| 2 - Future SOTA | `02-future-sota.md` | 17-entry matrix wave-stale and worktree-relative; sonic-rs/lightningcss/Sheets parity all RED; BBNF self-host bench routes through codegen path PARSE_FAILED; canada bench shows 50× divergence between two recent baselines; AZ-III.W4 gate cannot pass without bench-harness preflight; "BEAT lightningcss" remains binding repo policy |
| 3 - Substrate / Dead Code | `03-substrate-deadcode.md` | ~1893 LOC deletable; `crates/ir/src/dta/` orphan; `bootstrap_parser.rs` 1505 LOC still canonical; silent `BoxedEnum` fallbacks at `reference.rs:74` and `revise.rs:123`; `recognizer_plan.rs` self-confessed orphan; `parse_hex_color` host shim duplicated 16× across tests; `parser-trace` feature corpse |
| 4 - Instructions / Precepts | `04-instructions-process.md` | `AGENT_DISPATCH_TEMPLATE.md` lacks HARD CAP / worktree pin / read-size / no-polling / empty-return slots; `ORCHESTRATION.md` triumvirate spec lacks measurable auto-triggers; `WAVE_SPEC.md` lacks §3a Triumvirate / §4a Disjointness / §4b Worktree Plan; `SPEC.md` §Scope Reveal too lenient; `LESSONS-LEARNED.md` missing nine 2026-04-30 entries |
| 5 - Plan / Waves Drift | `05-plan-waves.md` | User claim CONFIRMED: nine consecutive tranches AU through AZ-I closed with carry-forward, AZ-II is the first to admit it. Sixteen items crossed two-or-more boundaries; five crossed five-or-more. AZ-III plan mostly well-formed but W3 over-broad, W2 vs W3.4 file-bound race, dev-loop infra mis-ordered, three AZ-II FINAL vs PROGRESS-SNAPSHOT mismatches |
| 6 - Throughput / Commit | `06-throughput-commit.md` | Iteration is bimodal: dev-loop excellent (`iter-check` 0.14 s warm / 17 s cold), bench compile catastrophic (>10 min/harness fat-LTO, >50 min sweep). `[profile.ax-iter]` defined twice with conflicting settings. 68/499 commits carry identical templated body from filter-repo rewrite; all 68 local-only and safe to re-rewrite. `make doctor` host-readiness gap |

## Accepted Findings

These claims are grounded in artefacts cited by their lanes and survive
challenge. They become AZ-III plan inputs.

### A1 - User claim is correct

Nine consecutive tranches (AU, AV, AW, AX, AY-I, AY-II-I, AY-III, AZ-I,
AZ-II) closed with recorded misses or continuation handoff. AZ-II is the
first close that admits the pattern as such rather than burying it.
Sixteen items crossed two-or-more tranche boundaries; five crossed five-
or-more (17-entry bench matrix AU→AZ-III, tape deletion AU→AZ-III, BBNF
self-host canonical AY→AZ-III, Sheets parity AU→AZ-III, direct-to-struct
admission AS→AZ-II.O2). Source: lane 5 §1, §2 with FINAL.md citations.

### A2 - AZ-II FINAL.md has stale and inconsistent close evidence

Three concrete mismatches between FINAL.md and PROGRESS-SNAPSHOT are
documented, plus one stale-BLOCKED status corrected by lane 1's HEAD
gate run. The "within ±2% of AZ-I close" claim has no citable bench
artefact. AZ-III.W1 cannot dispatch against an inconsistent reference.

### A3 - AZ-III plan thesis holds; refinements are mechanical

The two-duty thesis (close AZ-II terminal + grammar-general authority
substrate) survives the audit. No third axis emerges. The required
refinements are concrete patches:

- W3 (Fact + Type + CSP + Projection in one wave, eight directories
  modify-access) is a tranche, not a wave. Split into W3a / W3b / W3c.
- W2 vs W3.4 race on `crates/core/src/backend/rust/emitter/**`. Carve
  shape-specific `struct_direct.rs` files to W2; the rest to W3c.
- Dev-loop infra is mis-ordered: it must precede source work per
  `feedback_build_infra_first`. Either expand W0 scope or add a new
  W0p Throughput Substrate wave before W1.
- W2.4 BBNF bootstrap canonical close wording ("produce blocker proof")
  permits the AY/AZ-I close-with-miss pattern. Tighten to forbid
  `bootstrap_parser.rs` retention as a closure path.
- Three FINAL.md mismatches reconciled before W1 dispatch.

### A4 - Failure baseline is bounded and routable

63 failing tests are chronic AZ-II carry-overs, not new regressions
since `e11f3665`. Each cluster has a clear AZ-III owner wave:

- Regen drift on 9/9 grammars → W1 - O5 Reclose.
- Sheets corpus + serialize_roundtrip + sheets_parity (40 retries) →
  W2.3 - Sheets parity.
- CSS L4 typed-payload missing for hex / named colours / `:dir(ltr|rtl)`
  → W3 - typed-payload authority (specifically W3b once split).
- `pipeline_compile_request` panics at `crates/ir/src/registry/strategy.rs:257`
  for `MultiPathParser` / `ImportPrettyParser` / `SplitPrettyParser` →
  W3 - registry authority. Lane 1 recommends a research lane before W3
  dispatch on this cluster: are these test fixtures or real grammars?
- JSON Number-vs-U64 + canada coordinate divergence → W3 - JSON Value
  enum discrimination.
- 42 clippy errors on `bbnf-ser` / `csp-solver` / `egraph-derive` /
  `simd-scan` → W4 - workspace truth.

### A5 - 17-entry matrix is wave-stale and worktree-relative

The fresh JSON rows in `docs/benchmarks/post-AZ-II.json` were captured
in `/private/tmp/bbnf-worktrees/cutover-H2`, not at master, and
predate O2/O3/O4 deletion landings. Eleven rows are explicit
`cutover.E placeholder`. Five rows are SIGABRT/NOT_MEASURED. One row is
PARSE_FAILED. The 50× canada divergence between `post-AZ-II.json`
(4.078 ms) and `post-AY-az-ii-doc-baseline-json.txt` (219 ms) implies
either profile-conditioned or commit-conditioned drift. AZ-III.W4 owns
a clean recapture; the BBNF bench must reroute from the codegen path
to `bootstrap_parser` until self-host is canonical.

### A6 - Substrate violations are exact lines

Three findings have exact source coordinates and are immediately
actionable:

- `crates/ir/src/dta/` (~90 LOC) - zero `use bbnf_ir::dta` consumers;
  types appear only in test deny-strings. Delete in W1.
- Silent `BoxedEnum` fallback at `crates/ir/src/passes/types/constraint/reference.rs:74`
  swallows compound-Ref. Delete fallback in W3a, raise diagnostic.
- Silent `BoxedEnum` fallback at `crates/ir/src/passes/types/constraint/revise.rs:123`
  swallows heterogeneous-Alt joins. Delete fallback in W3a.
- `crates/core/src/backend/recognizer_plan.rs` (159 LOC) - self-confessed
  orphan with "downstream consumer count is currently zero" comment.
  Delete in W1 or hold until W3c proves a consumer.
- `parse_hex_color` host shim duplicated 16× across test files despite
  `crates/core/src/css_types.rs:15` claiming single source of truth.
  Mechanical fix in W2.2 - CSS parity.
- `parser-trace` feature corpse - 4× source references, 0× Cargo.toml
  declarations. Delete in W1.

`crates/core/src/grammar/bootstrap_parser.rs` (1505 LOC) is the named
AZ-III Hard Gate 5 blocker. W2.4 must produce the canonical self-host
proof or AZ-III stays blocked - this is the durable carry, not a
deletion target until canonicalization closes.

### A7 - Bench compile is the W4 throughput blocker

Fat-LTO `[profile.bench]` produces >10 min per harness; the 5-harness
17-entry sweep is >50 min. AZ-III.W4 hard gate (refreshed
`post-AZ-III.json` matrix) cannot be satisfied honestly without
W0-level bench-iter relief. Lane 6's proposed `[profile.bench-iter]`
(`lto=off`, `codegen-units=16`) reduces sweep wall to ~2.5 min.

### A8 - Profile redundancy is a measurement-truth risk

`[profile.ax-iter]` is defined twice (`Cargo.toml:125-129` and
`.cargo/config.toml:65-70`) with conflicting `debug` and
`codegen-units` settings. Single source of truth is required before
W4 measurement, otherwise the W4 evidence is profile-ambiguous.

### A9 - 68 commits carry templated bodies

The W0 message-only rewrite over span `53d3e6b2..HEAD` produced 68
commits with the identical body "Land the implementation slice named
in the subject as an explicit cutover checkpoint…". The subject
scopes are clean (`fix(emitter/wrap-tape)`, `refactor(lower/view-walk)`)
so the scope rule landed; the body rule landed only as templates.
All 68 are local-only (HEAD is 1397 ahead of origin/master, 0
behind). The user has acknowledged the rewrite landed; lanes 4 and 6
identify the residual quality gap. Do NOT re-rewrite without explicit
user authorization. Codify "evidence-bearing, not templated" as a
precepts addendum so future rewrites do not produce the same gap.

### A10 - Precepts framework needs five concrete patches

Lane 4's refinements are surgical:

- `AGENT_DISPATCH_TEMPLATE.md` - add HARD CAP, sibling-worktree pin,
  CARGO_TARGET_DIR, read-size preflight, empty-return rule, anti-polling.
- `ORCHESTRATION.md` - add `## Triumvirate Auto-Triggers` (JSONL quiet
  >15 min, first-pass no-commit, three diagnostic-loop iterations,
  scope-pivot reveal) and require artefact paths
  `audit/{COHORT}-{research,plan,redress}.md`.
- `WAVE_SPEC.md` - add §3a Triumvirate Dispatch, §4a Disjointness,
  §4b Worktree Plan.
- `SPEC.md` §Scope Reveal - tighten step 1 to absorb only when
  file-bound expansion is ≤2 paths and hard gate unchanged.
- `LESSONS-LEARNED.md` - append nine entries codifying patterns this
  audit confirmed.

The precepts repository is a submodule pinned at `e490e8e`. Edits land
inside the submodule, then the parent updates its pointer.

## Narrowed Findings

These claims were proposed by lanes but are tightened or restricted:

### N1 - Sibling-repo ownership

Lane 5 proposes a new W0.6 - Sibling Repo Triage Packet. Narrow this:
the failing surfaces are scoped (parse-that pinned to published 0.3.3
expecting old `pprint::Doc` / `pprint::Join`; pprint clippy red).
W0.6 owns the triage doc and registry-pin decision; actual sibling
repo source edits route to a sibling-tranche or remain documented
blockers for AZ-III close.

### N2 - W0p vs expanded W0

Lane 6 proposes either expanding W0 or adding W0p Throughput Substrate.
W0 is currently doc-only with explicit "no source code" file bounds
(`waves/W0.md:37`). Per `feedback_build_infra_first`, throughput must
land first - but W0 was written to keep dispatch repair clean of
source. Choose: open AZ-III.W0p between W0 and W1 with source-allowed
bounds limited to `Cargo.toml`, `.cargo/config.toml`, `Makefile`,
`scripts/**`, `xtask/**`. W1 reclose then runs against W0p's profile
and bench-iter setup.

### N3 - Pipeline registry research lane

Lane 1 recommends a research lane before W3 dispatch on the
`pipeline_compile_request` cluster (`MultiPathParser` /
`ImportPrettyParser` / `SplitPrettyParser`). Adopt as a W3.0 research
sub-unit feeding W3a planning, not as a separate triumvirate.

## Speculation - not gates

Per `CHALLENGE.md` synthesis rule, claims marked speculation cannot
become hard gates or implementation phases.

### S1 - "Templated bodies are bodyless equivalent"

Lane 6 implies the templated-body rewrite is functionally equivalent
to bodyless; lane 4 calls it half-broken. Both are inferences from
qualitative reading. The user has acknowledged the rewrite landed.
Treat as a precepts gap to codify (lesson-learned), not as a re-
rewrite trigger.

### S2 - "All 1893 LOC is deletable in W1"

Lane 3's deletable LOC includes `bootstrap_parser.rs` 1505, which is
the AZ-III Gate 5 blocker, not an immediate deletion target.
Subtract 1505 to get the immediate-deletable surface (~388 LOC). The
1505 deletes when W2.4 closes.

## Path Forward

AZ-II is closed as continuation handoff with three FINAL.md
reconciliations applied. AZ-III absorbs the audit-found refinements
into existing waves and adds W0p / W0.5 / W0.6 / W3.0. The precepts
submodule receives five patches.

### Sequence

1. Orchestrator writes this synthesis (done).
2. Wave Redress B (parallel, three agents):
   - **B1 - AZ-II Close Honesty Patches**. Reconcile FINAL.md vs
     PROGRESS-SNAPSHOT mismatches; correct stale-BLOCKED on O5 gate
     1; record AZ-III routing pointers across AZ-II docs.
   - **B2 - AZ-III Wave Spec Refinements**. Split W3 into W3a / W3b /
     W3c; resolve W2 vs W3.4 emitter race; add W0p Throughput
     Substrate; add W0.5 Commit Body Truth Sample; add W0.6 Sibling
     Repo Triage; add W3.0 Pipeline Registry Research; tighten W2.4
     bootstrap close clause; update AZ-III.md wave table; update
     PROGRESS.md.
   - **B3 - Precepts Submodule Refinements**. Apply lane 4's five
     patches inside the submodule; commit inside submodule; orchestrator
     updates parent pointer.
3. Orchestrator integrates, validates `git diff --check`, runs
   `cargo fmt --all -- --check` if any source touched (B1/B2/B3 are
   docs-only), and commits each B-lane's output as its own
   evidence-bearing commit.
4. AZ-III W0 close gate is then rerun with the audit folded in. W0p
   opens immediately after W0 close.

### Wave Redress Dispatch Bounds

| Lane | May modify | Must not touch |
|---|---|---|
| B1 | `docs/tranches/AZ-II/{FINAL.md,PROGRESS.md,PROGRESS-SNAPSHOT-2026-04-29.md}` | AZ-III, precepts, source, generated |
| B2 | `docs/tranches/AZ-III/{AZ-III.md,PROGRESS.md,waves/W*.md,audit/REAUDIT-2026-04-30/}` | AZ-II, precepts, source, generated |
| B3 | `docs/precepts/instructions/{tranche/AGENT_DISPATCH_TEMPLATE.md,tranche/WAVE_SPEC.md,tranche/SPEC.md,ORCHESTRATION.md,LESSONS-LEARNED.md}` (inside submodule) | AZ-II, AZ-III, source, parent .gitmodules pointer (orchestrator owns the parent pointer update) |

### Hard Caps

B1 - 30 min (mechanical reconciliation; no scope reveal expected).
B2 - 45 min (six wave-spec edits; one new wave file; AZ-III.md table).
B3 - 30 min (five doc edits; submodule commit).

Triumvirate auto-trigger: any B-lane that returns empty, exceeds cap
without commit, or reveals scope outside its bounds dispatches a
research / plan / redress triad; the synthesis pauses until the
triad lands an amended packet.

## Open Routes Carried Beyond AZ-III

Items that do not fit the two-duty thesis but surfaced in the audit:

- Sibling repo modernization (parse-that, pprint, gorgeous, bbnf-buddy)
  remains routed beyond AZ-III. AZ-III owns the triage packet only.
- BA path APIs and BB rewrite inference remain blocked until AZ-III
  W5 terminal close per `AZ-III.md` Exclusions clause.
- The generated bench output from the close matrix routes to a
  sibling docs commit alongside the W4 close, never folded into a
  source commit.

The audit confirms the AZ-III thesis. Implementation now resumes
under the refined plan.
