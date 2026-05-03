# BC.W1b — Regen-Equality Verification

**Name**: W1b — Regen-Equality Verification
**Opens after**: BC.W1a close (full Rust emitter refactor across nine grammars)
**Hard gate**: `cargo xtask regen --check` produces byte-identical output to BB close artefact for ALL nine grammars; `diff -r BB-close-snapshot/ crates/core/src/grammar/generated/` returns zero
**Status**: planned

## §1 Deliverable

Per BC02-1 surgery, W1b is the regen-equality byte-verification consumer of W1a's refactor. The refactor lands at W1a; the byte-equality gate lands here. The two-wave split mirrors the W0a (spec) → W0b (smoke) sequencing pattern.

W1b's only deliverable is verification: capture pre-W1a generated output as a snapshot fixture; run `cargo xtask regen --check` post-W1a; assert byte-identical equality across nine grammars; record evidence.

## §2 Milestones

### §2.1 Capture BB-close snapshot

Mechanism: capture `crates/core/src/grammar/generated/*.rs` (or post-BA.W3 path) as a snapshot fixture at W1b open. The fixture is the truth surface for byte-equality.

Files: `docs/tranches/BC/audit/W1b-bb-close-snapshot/` (create — directory of nine snapshot files).

Sub-gate: nine snapshot files captured; checksums recorded.

### §2.2 Run xtask regen --check post-W1a

Mechanism: run `cargo xtask regen --check` against the W1a-refactored lowerer; assert byte-identical to BB close snapshot.

Files: `docs/tranches/BC/audit/W1b-regen-equality.txt` (create).

Sub-gate: regen output byte-identical for nine grammars; record diff command output (must be empty).

### §2.3 Per-grammar diff verification

Mechanism: run `diff -u BB-close-snapshot/<g>.rs crates/core/src/grammar/generated/<g>.rs` for each of nine grammars; record empty output for each.

Files: `docs/tranches/BC/audit/W1b-per-grammar-diff.md` (create).

Sub-gate: per-grammar diff output empty for all nine; document records each grammar's verification status.

### §2.4 Cohort regen check

Mechanism: the five cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math) per `audit/MODULES-2026-05-03.md:625-628` are template-emitted at BB.W2; W1b verifies the template emission preserves byte-output through the typed-IR refactor.

Files: `docs/tranches/BC/audit/W1b-cohort-regen.md` (create).

Sub-gate: cohort grammars regen byte-identical to BB close; cross-cohort consistency verified.

### §2.5 Samply checkpoint

Mechanism: capture a samply 5K-sample profile at W1b close; verify the OpenFrame retiral from BA.W5 + BB.W1 is preserved (`Vec<OpenFrame>::clone` < 0.1% inclusive samples).

Files: `docs/tranches/BC/audit/W1b-samply.md` (create).

Sub-gate: samply profile records `Vec<OpenFrame>::clone` retired; BA-G2's heap-allocation invariant preserved; per `audit/RESTART-SKETCH-2026-05-03.md:154-220` the 86.07% pathology stays excised.

### §2.6 Tape-residue scrub continuity

Mechanism: per W1a §2.11, tape mentions stay zero post-refactor.

Files: `docs/tranches/BC/audit/W1b-tape-scrub.txt` (create).

Sub-gate: `rg -n 'TapeRec\|TapeCursor\|TapeBuilder\|TapeOffset' crates/core/src/codegen/` returns zero.

## §3 Closer gate

| ID | Gate | Verification |
|---|---|---|
| W1b-G1 | BB-close snapshot captured | `test -d docs/tranches/BC/audit/W1b-bb-close-snapshot && ls docs/tranches/BC/audit/W1b-bb-close-snapshot/*.rs \| wc -l` returns 9 |
| W1b-G2 | xtask regen byte-identical | `cargo xtask regen --check` returns clean exit; recorded in W1b-regen-equality.txt |
| W1b-G3 | Per-grammar diff empty | per-grammar diff command output empty; recorded in W1b-per-grammar-diff.md |
| W1b-G4 | Cohort grammars regen byte-identical | cohort grammars verified per W1b-cohort-regen.md |
| W1b-G5 | Samply OpenFrame retiral preserved | `Vec<OpenFrame>::clone` < 0.1% inclusive per W1b-samply.md |
| W1b-G6 | Tape residue zero | `rg` returns zero per W1b-tape-scrub.txt |

## §4 Invariants

§I1. **Regen-equality**: byte-for-byte BB close = W1b close.

§I2. **No behavioural change**: refactor invariant verified.

§I3. **Lock 1 (tape and columnar dead)**: continuity preserved.

§I4. **Lock 9 (slice-borrow primary)**: continuity preserved.

§I5. **No metalanguage** in evidence files.

## §5 Risks

| Risk | Likelihood | Mitigation |
|---|---|---|
| Regen produces different bytes for one grammar | Medium | W1b §2.3 per-grammar diff catches; rollback W1a or amend |
| Snapshot captured at wrong revision | Low | Snapshot is at W1b open, not arbitrary; checksum records the source |
| Samply capture is non-deterministic between runs | Medium | Multi-sample averaging; threshold 0.1% bounds typical noise |

## §6 Cross-references

### Carry-tags consumed

| Tag | From | Description |
|---|---|---|
| BC.W1a→BC.W1b | BC.W1a | Refactored Rust lowerer is the input to byte-equality verification |

### BC-G gates closed

| Gate | Closure |
|---|---|
| BC-G4 (verified) | regen-equality verified by mechanism (W1b-G2 + W1b-G3) |

### Carry-tags produced

| Tag | To | Description |
|---|---|---|
| BC.W1b→BC.W2 | BC.W2 | Verified Rust lowerer is the reference for TS + WASM scaffold consumers |
| BC.W1b→BC.W3 | BC.W3 | Refactored lowerer at `crates/core/src/codegen/rust/lower.rs` is the input to BC.W3c's relocation |

## §7 Iter-time check

| Activity | Pre-W1b wall | Post-W1b wall | Notes |
|---|---:|---:|---|
| `cargo xtask regen --check` | ≤ 23 s | ≤ 23 s | byte-identical run |
| `samply record cargo bench --bench json_twitter` | n/a | ~30 s | one-time samply capture |
| `cargo nextest run --workspace` | ~50 s | ~50 s | unchanged |

## §8 Dependencies

- **Depends on**: BC.W1a close (refactored Rust emitter).
- **Blocks**: BC.W2 (TS + WASM scaffolds), BC.W3 (core split).

## §9 Closing posture

W1b is the byte-equality consumer of W1a. The verification is mechanical: capture snapshot, run regen, diff, record. No new substrate; no behavioural change; only verification evidence.
