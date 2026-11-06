# PROTO-2 — LOCK-2 THE GRAPH ENGINE · probe 2: PROVE-2-DISPATCH-CYCB-PAIRED

**Family 2A / 2D — extraction-as-decider, the DECIDING half of the master gate (§0.2b).**
The runtime cyc/B measurement PROTO-1 punted as "no scriptable PMU on this host."

- **BASE-PIN:** measured on HEAD `ce51c36fc` (`git log --oneline -1` →
  `ce51c36fc locks(sk-v24): pass-1 bbnf CLOSED …`). `git merge-base --is-ancestor
  ce51c36fc HEAD` → **true** (HEAD *is* the pin; clean descent, no fast-forward
  needed). All numbers below measured on this exact HEAD. **Not** wrong-artifact-risk.
- **loadavg:** 5.9 → 7.0 across the measured runs (recorded per run below; the
  paired design nulls load — see METHOD). host=`arm64`.
- **Verdict:** `RAN_MEASURED` — **PASS**. The N-crossover **TRACKS REAL RUNTIME
  COST.** Extraction-as-decider **EARNED**; 2A/2B/2D unblock on the deciding half.

> **Filename note.** This probe's brief names its deliverable `PROTO-2.md`, but the
> pass-2 directory already holds a `PROTO-2.md` for a *different* probe
> (PROVE-2-SUBSTRATE-RECONCILE, family 2B). To avoid clobbering that sibling
> evidence this report is delivered as **`PROTO-2-DISPATCH-CYCB.md`**. Orchestrator
> reconciles the naming.

---

## QUESTION

The master gate's DECIDING half (FAMILY-SPECS §0.2b): is DISPATCH-table lowering
(branch on the first byte) actually **FASTER** in *measured* cyc/B than
SEQUENTIAL-trial at large N in a **runnable** parser — 95% CI excluding 1.00×
same-direction at value-starts — **AND** does the direction **reverse** at small N
(N=2)? PROTO-1 proved only that a hand-authored cost model's `Alt` arm *reads* N and
crosses at `N*=3.0`; it never showed the crossover picks the genuinely faster
lowering. This probe supplies the runtime truth: if DISPATCH wins at N=8 and
SEQUENTIAL wins at N=2, the crossover tracks real cost → **extraction-as-decider
EARNED**; if the CI includes parity at N=8 or there is no reversal at N=2 →
**REFUTED** and LOCK-2 collapses to canonicalization + the `BackendShape` driver.

**PROTO-1's blocker is refuted.** cyc/B needs no `perf`/`instruments`/`dtrace`: the
canon benches read per-process `ri_cycles`/`ri_instructions` via
`proc_pid_rusage(RUSAGE_INFO_V5)` (`json_canon_bench.rs` PMU mode, the same counter
PROVE-12 used for cyc/B 0.81–0.98×). The blocker was a runnable lowering, not the
counter. This probe builds the runnable lowering.

## METHOD

Skinny worktree (`…/wf_fce2ec2b-53a-34/skinny`), base-pinned, HEAD named. The rule
under test is the JSON `value` shape generalised to N pairwise-disjoint-FIRST
alternatives: N keywords of equal length (`apple berry cocoa dates elder figgy grape
honey`), distinct first bytes `a..=h`. Corpus = round-robin of the first N keywords
so the matching alt is **uniformly distributed** over `0..N` (SEQUENTIAL's mean trial
depth `(N+1)/2`, DISPATCH's `1`); **every token is a value-start**, so the whole
64 KiB corpus is value-starts.

Three RUNNABLE lowerings, hand-authored to the exact shapes the two codegen paths
emit, all producing byte-identical parses (asserted `assert_eq!` on a checksum before
timing — same work, only the *select* differs):

- **match-dispatch** — `match first_byte { b'a'=>…, … }`, the literal shape
  `runtime/src/grammars/json/value.rs:33` already emits (LLVM lowers the contiguous
  8-arm byte match to a jump table). *The real generated dispatch.*
- **lut-dispatch** — the textbook 256-entry "branch on first byte" table, populated
  for the live N alternatives; cost is **N-independent** O(1) (one load, one sentinel
  test, one verify). *Sized to the rule — added to defeat the "fixed-arm match
  over-penalizes small N" critique.*
- **sequential** — `for k in 0..N { if b==KW[k][0] { verify; break } }`, ordered
  trial with fail-fast on first-byte mismatch. *The decision under test — no e-graph
  needed to emit it.*

**Paired cold, load-nulling:** per window, a block of `iters` match-dispatch parses,
then `iters` lut-dispatch, then `iters` sequential — **adjacent**, with an
`ri_cycles`/`ri_instructions` read straddling each block, so load/thermal drift hits
all three sides of every ratio inside one window. ≥20 windows/run give the paired
ratio distribution; 95% CI is the normal approx over windows (identical to
`json_canon_bench`'s `emit_paired`). **ins/B is the load-invariant parity check**
(instruction retirement is deterministic, load-immune); cyc/B is the load-sensitive
metric the CI guards. cyc/B is a *ratio* per window, so absolute cache warmth cancels
— the decision variable is confound-free even though a block loop keeps the 64 KiB
corpus resident (the canon PMU discipline, faithful to a real hot-parser workload).

## COMMANDS

- `crates/bbnf-bench/src/bin/dispatch_canon_bench.rs` — cloned the `ri_cycles`
  harness (`RusageInfoV5` + `proc_pid_rusage`) verbatim from `json_canon_bench.rs`.
- `cargo build --release -p bbnf-bench --bin dispatch_canon_bench` (worktree) →
  exit 0, 1m22s cold. *(Run as a `--bin`, not `--bench`: the brief said "clone
  `json_canon_bench.rs`", which is itself a `src/bin/` binary — a bench harness=false
  target would be a different, heavier scaffold for the same measurement.)*
- `target/release/dispatch_canon_bench <windows> <iters> <corpus_bytes>` — args
  default `20 64 65536`; runs N=2 then N=8 (bracketing PROTO-1's predicted `N*=3`).

## WHAT RAN — THE NUMBERS

Five measured runs (A–C match-only, D–E add the LUT shape). Ratio = **dispatch /
sequential**; `hi95 < 1.0` ⇒ DISPATCH faster & excludes parity; `lo95 > 1.0` ⇒
SEQUENTIAL faster & excludes parity (reversal).

### N=8 (large N) — cyc/B ratio dispatch/sequential

| run | shape | mean | lo95 | hi95 | excludes 1.00×? |
|-----|-------|------|------|------|-----------------|
| A | match | 0.6510 | 0.6376 | **0.6644** | yes — DISPATCH faster |
| B | match | 0.6473 | 0.6255 | **0.6692** | yes — DISPATCH faster |
| C | match | 0.7192 | 0.6857 | **0.7527** | yes — DISPATCH faster |
| D | match | 0.6441 | 0.6329 | **0.6552** | yes — DISPATCH faster |
| D | lut   | 0.6287 | 0.6190 | **0.6384** | yes — DISPATCH faster |
| E | match | 0.7403 | 0.7165 | **0.7642** | yes — DISPATCH faster |
| E | lut   | 0.6626 | 0.6375 | **0.6877** | yes — DISPATCH faster |

### N=2 (small N) — cyc/B ratio dispatch/sequential

| run | shape | mean | lo95 | hi95 | reversed (lo95>1.0)? |
|-----|-------|------|------|------|----------------------|
| A | match | 1.0209 | **1.0058** | 1.0359 | yes — SEQUENTIAL faster |
| B | match | 1.0912 | **1.0587** | 1.1237 | yes — SEQUENTIAL faster |
| C | match | 1.1792 | **1.1338** | 1.2247 | yes — SEQUENTIAL faster |
| D | match | 1.0391 | **1.0229** | 1.0552 | yes — SEQUENTIAL faster |
| D | lut   | 1.0373 | **1.0114** | 1.0631 | yes — SEQUENTIAL faster |
| E | match | 1.2170 | **1.1872** | 1.2467 | yes — SEQUENTIAL faster |
| E | lut   | 1.0968 | **1.0539** | 1.1397 | yes — SEQUENTIAL faster |

### Load-invariant ins/B corroboration (deterministic; run D shown, stable across all)

| N | match_ins/B | lut_ins/B | seq_ins/B | dispatch does fewer ins? |
|---|-------------|-----------|-----------|--------------------------|
| 2 | 4.671 | 4.839 | **4.254** | **no** — dispatch does MORE (both shapes) → SEQUENTIAL cheaper |
| 8 | 4.672 | 4.838 | **6.319** | **yes** — dispatch does FEWER → DISPATCH cheaper |

`match_ins/B` = 4.671 and `seq_ins/B` = 4.254/6.319 are byte-stable across all five
runs (instruction retirement is load-immune). The cyc/B direction matches the ins/B
direction **exactly** at both N — the cyc/B signal is real cost, not load noise.

## READING

- **Every** N=8 CI (7/7, both shapes, 5 runs) has `hi95 < 1.0` → DISPATCH strictly
  faster, parity excluded, same direction. At N=8 sequential trials the matching alt
  at mean depth 4.5; dispatch is O(1).
- **Every** N=2 CI (7/7, both shapes) has `lo95 > 1.0` → SEQUENTIAL faster, parity
  excluded — **the direction reverses.** At N=2 the dispatch table's fixed
  per-token machinery (bounds-checked jump / LUT load + sentinel test) is not
  amortized against sequential's 1.5-mean-compare trial.
- The **LUT** shape is decisive: it is N-*independent* O(1) and *sized to the rule*,
  yet still loses at N=2 (ins/B 4.84 > 4.25) and still wins at N=8 (4.84 < 6.32). The
  reversal is a genuine property of dispatch-table overhead vs N-scaled trial, not an
  artifact of a fixed-arm match.
- Crossover is bracketed to **2 < N* ≤ 8**, consistent with PROTO-1's cost-model
  `N*=3.0`. The two probe points establish the *direction reversal* the gate
  requires; the exact integer crossover was not swept (only N=2 and N=8 needed).

## VERDICT

**PASS — `RAN_MEASURED`.** At N=8 DISPATCH cyc/B 95% CI excludes 1.00× below
SEQUENTIAL, same-direction across ≥3 windows (20–30) at value-starts, in **7/7**
CIs over 5 runs and **both** dispatch shapes; AND at N=2 the direction reverses
(SEQUENTIAL < DISPATCH, `lo95 > 1.0`) in **7/7** CIs — corroborated by load-immune
ins/B at both N. **The N-crossover tracks real runtime cost. Extraction-as-decider is
EARNED. The master gate's deciding half PASSES; 2A/2B/2D unblock.** (This settles
§0.2b only — §0.2a crossover-*fitting* and §0.2c synthetic-consumer remain separate
open items; this probe proves the pick moves cyc/B at value-starts, which was the
gate the fit and consumer were blocked behind.)

## DELIVERABLES

- `PROTO-2-DISPATCH-CYCB.md` — this report.
- `PROTO-2-DISPATCH-CYCB.patch` — the runnable harness
  (`crates/bbnf-bench/src/bin/dispatch_canon_bench.rs`, new file, self-contained,
  `libc`-only, no e-graph). Reproduce: apply, `cargo build --release -p bbnf-bench
  --bin dispatch_canon_bench`, `target/release/dispatch_canon_bench 30 96 65536`.
