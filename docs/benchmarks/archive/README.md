# Benchmarks Archive

Read-only historical evidence. Files in this directory predate the
benchmark-directory cleanup of 2026-05-01 and lived in
`docs/benchmarks/` (root or per-tranche subdirs) before
`docs/benchmarks/SPEC.md` codified the matrix-only policy.

## What's here

- **Per-tranche subdirs** (`AZ-I/`, `AZ-II/`, `AZ-III/`): wave evidence
  files (walls, parity proofs, deletion scans, audit notes) that were
  emitted during the named tranche's wave executions. These are
  *evidence*, not bench matrices — they were moved here because the
  SPEC restricts `docs/benchmarks/` root + tranche-close subdirs to
  matrix-only content, while wave evidence belongs at
  `docs/tranches/{LETTER}/audit/`.
- **Loose `post-{TAG}-W*-...` files** at the top of this directory:
  per-wave evidence that was emitted at root level prior to the
  cleanup. Examples: `post-AX-W0a*` (AX expand traces, predicate
  tables, refs probes, progress notes), `post-AY-W4-*-spot.txt` (A/B
  spot benches), `post-B*-W*.txt` (B-tranche walls/proofs), and
  similar.

## Citation policy

These files MAY be cited from closed tranche docs (`FINAL.md`, `audit/`)
as historical reference. They MUST NOT be cited from active or planned
tranche docs as authoritative numerics. Active tranches cite either:

- a current `docs/benchmarks/post-{X}.json` matrix at the parent
  benchmarks root, or
- a wave-evidence file under `docs/tranches/{LETTER}/audit/`.

If an archived file is needed for an active tranche claim, the active
tranche reproduces the measurement under its own profile and lands the
new matrix at the benchmarks root or wave evidence at
`docs/tranches/{LETTER}/audit/`.

## Inventory note

The bulk move happened in a single commit on 2026-05-01 alongside
`docs/benchmarks/SPEC.md` creation. Prior commits cite the original
paths (e.g. `docs/benchmarks/AZ-III/W1-no-default-build.txt`); active
tranche docs were updated in the same cleanup commit to use
`docs/benchmarks/archive/AZ-III/W1-no-default-build.txt` and
equivalent. Historical commits before that date carry the original
paths and should be read against pre-cleanup HEAD when needed.
