# LW3.PERF-1a: the cycle split that opens the record (DIG-K-B8)

DIG-K-B8: "LW3.PERF-1a's own stage-1 indexer opens with a committed cycle split that answers these by name": NEON digit parse, multi-block pipelining, a separate index prepass with no amortisation, one-shot presize, the DTA table walker. AMENDMENTS §G: the split covers stage 1, structural walk, strings, numbers, allocation and tape emit.

**Status: harness committed, reading owed.** No timing read was taken. The owner's site deadline held the machine until 09:00 local on 2026-11-12; at 09:00:01 the 1-minute load read 9.75 (X-7's bar is below 8), and no quiet window had been scheduled (DIG-QW: the orchestrator schedules it; a record outside it is NOISY and decides nothing). The reading is ask **S9-QW** in the ledger.

## The harness

parse-that `rust/simd/benches/stage1_split.rs` (lane `lw3/s9-perf` `07ed26c`, merged in PT `sk-v27-lw3` `cfe77fb`), built `--no-run` only. Arms, per corpus file (twitter, citm_catalog, canada, bootstrap, tailwind-output):

| Arm | What it times | The split it gives |
|---|---|---|
| `split::utf8` | `core::str::from_utf8` alone | the UTF-8 share a separate pass costs |
| `split::scan` | `scan_structural` alone (no UTF-8, no comments, no atoms) | structural classification and compaction |
| `split::separate` | `utf8` then `scan`: two passes | the "separate prepass with no amortisation" |
| `split::fused` | `stage1` on the selected tier | the fused pass: classification, UTF-8, comment mask, atom starts, compaction |
| `split::oracle` | `scalar::stage1` | the oracle (G-K1's denominator) |

`fused − scan` is the cost of the comment mask, the atom starts and fused UTF-8; `separate − fused` is the amortisation. Allocation is the index's growth from `len / 8 + 1` slots. Structural walk, strings, numbers and tape emit are stage 2's and belong to the parser that consumes the index (PERF-1b, the kernel as an LIR primitive, and LW4.PERF-2, the tape union); their split is taken when that consumer exists.

## The measured negatives, by name

| Negative (DIG §2 rows 169–170) | How stage 1 meets it |
|---|---|
| NEON digit parse | Stage 1 parses no digit. A number is an atom start (its first byte) and nothing more; digit parsing stays a stage-2 concern and stays refuted until new evidence. |
| Multi-block pipelining | Not used: one 64-byte block per iteration, one resolver step per block. |
| A separate index prepass with no amortisation | Stage 1 is a prepass, so it must amortise: it replaces std's UTF-8 pass and the separate comment and atom scans by one pass. `G-K1-S1-FUSED` (fused against separate) is the measurement; a LOSS is recorded against this row by name and opens a redesign unit (DIG-G1). |
| One-shot presize | Not used: the index grows from `len / 8 + 1` slots. |
| The DTA table walker | None. The resolver is word arithmetic per block; its only walk is over a block's quote and comment events, and only where there are several quote classes or comment classes. |
