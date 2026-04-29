# AZ-II.cutover.O3a - Failure Baseline and Triumvirate Redress
**Opens after**: AZ-II.cutover.O2 close and O-wave documentation alignment
**Agents**: up to 10 parallel
**Hard gate**: all 84 post-O2 test failures and the failed JSON bench baseline are assigned to root-cause cohorts with research, plan, wave-spec, and redress ownership.
**Status**: in_progress

## Scope

1. Record the post-documentation test baseline:
   `scripts/test-tier.sh workspace --profile ax-iter --no-fail-fast`
   ran 1645 tests: 1561 passed, 84 failed, 25 skipped.
2. Record the post-documentation performance baseline attempt:
   `make ay-bench-close WAVE=az-ii-doc-baseline` failed in the JSON
   lane when `json_monolithic::data_xl` exceeded the 1s iteration cap
   (`2.478697958s`).
3. Partition every failed test into root-cause cohorts before any
   implementation resumes.
4. Dispatch research + plan + redress triumvirates for each failure
   cohort, with the plan agent responsible for creating or amending a
   wave spec before redress lands.
5. Feed cohort outcomes into O3, O4, O5, O6, or a new child wave with
   explicit file bounds and hard gates.
6. Dispatch from concrete child specs:
   [`O3a-J1`](O3a-J1.md), [`O3a-C1`](O3a-C1.md),
   [`O3a-S1`](O3a-S1.md), [`O3a-P1`](O3a-P1.md), and
   [`O3a-A1`](O3a-A1.md).
7. Keep AZ-III closed unless a cohort proves a new grammar-general
   inference/layout substrate that cannot honestly land inside AZ-II.

## File bounds

| File | Access |
|---|---|
| `docs/benchmarks/AZ-II/cutover/O3a-test-failures.txt` | create |
| `docs/benchmarks/post-AY-az-ii-doc-baseline-json.txt` | create |
| `docs/tranches/AZ-II/waves/cutover/O3.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O4.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O5.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O6.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O7.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/O3a-{J1,C1,S1,P1,A1}.md` | create |
| `docs/tranches/AZ-II/audit/O3a-six-agent-audit-synthesis-2026-04-29.md` | create |
| `docs/tranches/AZ-II/waves/cutover/README.md` | modify |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |
| `docs/tranches/AZ-II/audit/AZ-II-HARDENING-AUDIT-2026-04-29.md` | modify |

**Do NOT touch**: source code, generated parser files, manifests, or
benchmark harnesses in O3a. Redress agents may edit source only under a
subsequent cohort wave spec. Deployment invariant: all agents run in
sibling fully-contained worktrees seeded with `scripts/seed-worktree.sh`;
the orchestrator owns master, cohort sequencing, and integration.

## Phase sub-items

### AZ-II.cutover.O3a.1 Baseline Capture

Mechanism: preserve the full nextest failed-test list and the failed
JSON bench transcript as committed evidence.

Files touched:
`docs/benchmarks/AZ-II/cutover/O3a-test-failures.txt`,
`docs/benchmarks/post-AY-az-ii-doc-baseline-json.txt`.

Sub-gate: artifacts record the exact test count and the bench timeout.

### AZ-II.cutover.O3a.2 Cohort J1 - JSON Materialization and Throughput

Mechanism: triage JSON bool/null/scalar materialization, structural
tests, typed accessors, sonic-rs parity, canonical parity, and the
`json_monolithic::data_xl` bench timeout as one root-cause cohort unless
research proves a split.

Dispatch spec: [`O3a-J1.md`](O3a-J1.md).

Files touched by future redress wave: `crates/core/src/runtime/json/**`,
`crates/core/src/backend/rust/emitter/shapes/**`,
`crates/core/tests/{json_*,sonic_rs_parity,structural,typed_accessor_surface,wrap_compound_elision,serialize_roundtrip}.rs`,
`crates/core/benches/json/monolithic.rs`.

Sub-gate: research doc names whether the failures share branch-tag /
leaf-payload routing, `Value` projection, or serializer causes.

### AZ-II.cutover.O3a.3 Cohort C1 - CSS Admission and Payloads

Mechanism: triage CSS comment admission, bootstrap/tailwind parse,
hex/named-color payload materialization, pseudo-branch payloads,
selector payload loss, and lightningcss parity.

Dispatch spec: [`O3a-C1.md`](O3a-C1.md).

Files touched by future redress wave:
`grammar/css_l4/*.bbnf`, `crates/core/src/runtime/css_l4/**`,
`crates/core/src/backend/rust/emitter/shapes/**`,
`crates/core/tests/{css_l4*,lightningcss_parity,ax_w0a2s_real_css_probe}.rs`.

Sub-gate: research doc separates whitespace/comment admission failures
from typed color/pseudo payload failures if they do not share a root.

### AZ-II.cutover.O3a.4 Cohort S1 - Sheets Branch Payload and Serialization

Mechanism: triage Sheets error literal, boolean, operator, range,
unary, and serialize self-parity failures.

Dispatch spec: [`O3a-S1.md`](O3a-S1.md).

Files touched by future redress wave:
`crates/core/src/runtime/google_sheets/**`,
`crates/core/src/backend/rust/emitter/shapes/**`,
`crates/core/tests/{sheets_parity,sheets_self_parity,sheets_expr_parity}.rs`.

Sub-gate: research doc names whether failures are branch-tag routing,
literal payload materialization, or serializer emission.

### AZ-II.cutover.O3a.5 Cohort P1 - Projection Totality and Generated View Residue

Mechanism: triage `projection_totality_runtime_call_count` alongside
O3's generated tape-view / `ValueRoot` purge.

Dispatch spec: [`O3a-P1.md`](O3a-P1.md).

Files touched by future redress wave:
`crates/core/src/backend/rust/view/**`,
`crates/core/src/backend/rust/emitter/grammar.rs`,
`crates/core/src/backend/rust/emitter/shapes/value_materialize.rs`,
`crates/core/tests/projection_totality.rs`.

Sub-gate: plan doc states whether P1 lands inside O3 or creates a
separate O3b wave.

### AZ-II.cutover.O3a.6 Cohort A1 - Analysis, LSP, and Historical json-prototype

Mechanism: triage `bbnf-analysis::directives`, `bbnf-lsp::integration`,
and `json-prototype::corpus` failures. Determine whether
`json-prototype` is archived/deleted, fixture-seeded, or moved out of
workspace testing.

Dispatch spec: [`O3a-A1.md`](O3a-A1.md).

Files touched by future redress wave:
`crates/analysis/**`, `crates/lsp/**`, `crates/core/benches/json-prototype/**`,
workspace manifests if archival is chosen.

Sub-gate: plan doc names deletion/archive vs repair, with no
compatibility shim.

### AZ-II.cutover.O3a.7 Triumvirate Round 1 Dispatch

Mechanism: dispatch research + plan + redress triads for J1, C1, and
S1. HARD CAP: research 20 min, plan 15 min, redress 30 min. Redress
may commit probes and halt if the plan requires a new wave spec not yet
approved by the orchestrator.

Files touched: cohort audit/plan files under `docs/tranches/AZ-II/audit/`
and the existing child wave files named above.

Sub-gate: each triad returns a root-cause document, a plan/wave-spec
patch, and either a redress commit or a documented halt.

### AZ-II.cutover.O3a.8 Triumvirate Round 2 Dispatch

Mechanism: dispatch research + plan + redress triads for P1 and A1
after Round 1 is integrated, staying under the up-to-10 agent cap.

Files touched: cohort audit/plan files under `docs/tranches/AZ-II/audit/`
and the existing child wave files named above.

Sub-gate: P1/A1 destinations are integrated into O3/O4/O5/O6 or new
child wave specs.

### AZ-II.cutover.O3a.9 Cutover Item Augmentation

Mechanism: update O3-O7 specs with the cohort outcomes so every failure
has a wave owner before implementation continues.

Files touched: `docs/tranches/AZ-II/waves/cutover/O3.md`,
`docs/tranches/AZ-II/waves/cutover/O4.md`,
`docs/tranches/AZ-II/waves/cutover/O5.md`,
`docs/tranches/AZ-II/waves/cutover/O6.md`,
`docs/tranches/AZ-II/waves/cutover/O7.md`,
`docs/tranches/AZ-II/waves/cutover/README.md`.

Sub-gate: `rg 'UNASSIGNED|TBD|owner gap' docs/tranches/AZ-II/waves/cutover/O*.md`
returns no active failure owner gaps.

### AZ-II.cutover.O3a.10 Progress Boundary

Mechanism: record the baseline, triad dispatch results, and next active
implementation wave.

Files touched: `docs/tranches/AZ-II/PROGRESS.md`,
`docs/tranches/AZ-II/audit/AZ-II-HARDENING-AUDIT-2026-04-29.md`.

Sub-gate: PROGRESS cites the failed-test artifact, bench artifact, and
cohort wave owners.

## Hard gate

1. `docs/benchmarks/AZ-II/cutover/O3a-test-failures.txt` contains the
   post-O2 full nextest failure list and the summary `1561 passed, 84
   failed, 25 skipped`.
2. `docs/benchmarks/post-AY-az-ii-doc-baseline-json.txt` records the
   JSON bench baseline failure and `data_xl` timeout.
3. Every failed test is assigned to J1, C1, S1, P1, or A1.
4. J1/C1/S1 triads are dispatched with research, plan, and redress
   roles before source redress lands.
5. P1/A1 triads are queued or dispatched before O3 closes.
6. O3-O7 specs are amended with cohort child waves before
   implementation resumes.

## Verification artefacts

- `docs/benchmarks/AZ-II/cutover/O3a-test-failures.txt`.
- `docs/benchmarks/post-AY-az-ii-doc-baseline-json.txt`.
- `docs/tranches/AZ-II/audit/O3a-J1-*.md` after triad close.
- `docs/tranches/AZ-II/audit/O3a-C1-*.md` after triad close.
- `docs/tranches/AZ-II/audit/O3a-S1-*.md` after triad close.
- Future `docs/tranches/AZ-II/audit/O3a-P1-*.md` and
  `docs/tranches/AZ-II/audit/O3a-A1-*.md`.
- `docs/tranches/AZ-II/audit/O3a-six-agent-audit-synthesis-2026-04-29.md`.
- `docs/tranches/AZ-II/waves/cutover/O3a-{J1,C1,S1,P1,A1}.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.O2
- **Blocks**: AZ-II.cutover.O3 close, AZ-II.cutover.O4, AZ-II.cutover.O6 truth claims

## Archaeology

The post-O2 baseline exposed failures outside the immediate generated
view purge. O3a prevents those failures from becoming silent O6 debt by
forcing triumvirate research, plan, wave creation, and redress routing
before implementation continues.
