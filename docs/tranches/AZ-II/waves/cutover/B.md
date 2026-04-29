# AZ-II.cutover.B - Stage A/B Byte-Equal Gate
**Opens after**: AZ-II.cutover.A close
**Agents**: up to 10 parallel
**Hard gate**: Stage A and Stage B BBNF regen output are byte-equal and guarded by a permanent reproducibility test.
**Status**: complete

## Scope

1. Run Stage A BBNF regen with the post-cutover.A compiler.
2. Rebuild the candidate compiler from generated source.
3. Run Stage B BBNF regen from the candidate compiler.
4. Compare Stage A and Stage B generated BBNF output byte-for-byte.
5. Land `bbnf_bootstrap_reproducibility` as a permanent CI gate.
6. Record the byte-equal close in AZ-II progress docs.

## File bounds

| File | Access |
|---|---|
| `crates/core/src/grammar/generated/bbnf.rs` | modify |
| `crates/core/tests/bbnf_bootstrap_reproducibility.rs` | create |
| `docs/benchmarks/AZ-II/cutover/stage-a-bbnf.rs` | create |
| `docs/benchmarks/AZ-II/cutover/stage-b-bbnf.rs` | create |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |
| `docs/tranches/AZ-II/waves/cutover/README.md` | modify |

**Do NOT touch**: BBNF runtime substrate, BBNF consumer migration,
`crates/tape/` deletion, non-BBNF resolver arms. Deployment invariant:
all agent work occurs in sibling fully-contained worktrees; the
orchestrator owns the canonical regen and byte-diff window.

## Phase sub-items

### AZ-II.cutover.B.1 Stage A Capture

Mechanism: run `cargo xtask regen --grammar bbnf` and capture the
generated BBNF source as Stage A evidence.

Files touched: `crates/core/src/grammar/generated/bbnf.rs`,
`docs/benchmarks/AZ-II/cutover/stage-a-bbnf.rs`.

Sub-gate: Stage A generated source exists and is deterministic on
rerun.

### AZ-II.cutover.B.2 Candidate Rebuild

Mechanism: rebuild `bbnf` from the Stage A generated source under
`ax-iter`.

Files touched: no source edits.

Sub-gate: `cargo build -p bbnf --profile ax-iter` succeeds.

### AZ-II.cutover.B.3 Stage B Capture

Mechanism: rerun BBNF regen using the rebuilt candidate and capture the
Stage B generated source.

Files touched: `crates/core/src/grammar/generated/bbnf.rs`,
`docs/benchmarks/AZ-II/cutover/stage-b-bbnf.rs`.

Sub-gate: Stage B generated source exists.

### AZ-II.cutover.B.4 Byte-Equal Diff

Mechanism: compare Stage A and Stage B captures with `diff -u`.

Files touched: evidence only.

Sub-gate: the diff is empty.

### AZ-II.cutover.B.5 Permanent Reproducibility Test

Mechanism: encode the Stage A/B equality contract as
`bbnf_bootstrap_reproducibility`.

Files touched: `crates/core/tests/bbnf_bootstrap_reproducibility.rs`.

Sub-gate: `cargo nextest run -p bbnf --test bbnf_bootstrap_reproducibility --profile ax-iter`
passes.

### AZ-II.cutover.B.6 Progress Boundary

Mechanism: update AZ-II progress and cutover docs with the landed gate.

Files touched: `docs/tranches/AZ-II/PROGRESS.md`,
`docs/tranches/AZ-II/waves/cutover/README.md`.

Sub-gate: cutover.C opens only after the reproducibility gate is green.

## Hard gate

1. Stage A and Stage B BBNF generated files exist.
2. `diff -u stage-a-bbnf.rs stage-b-bbnf.rs` is empty.
3. `cargo xtask regen --check` passes.
4. `cargo nextest run -p bbnf --test bbnf_bootstrap_reproducibility --profile ax-iter`
   passes.

## Verification artefacts

- Commit `d6b0377a`.
- `crates/core/tests/bbnf_bootstrap_reproducibility.rs`.
- Stage A/B captures under `docs/benchmarks/AZ-II/cutover/`.

## Dependencies

- **Depends on**: AZ-II.cutover.A
- **Blocks**: AZ-II.cutover.C, AZ-II.cutover.G

## Archaeology

The original AZ-II close contract depended on byte-equal bootstrap
identity. cutover.B made that identity executable before later scope
reveals split tape deletion and consumer migration.
