# PROVE-01 — PREFILTER (F1): shufti/truffle NEON prefilter into scan dispatch

**Verdict: PARTIAL.** Measured, parity-exact, integrated at the three-layer seam.
Dense-neutrality gate **MET** (twitter 1.05x / bootstrap 1.15x cyc — the prefilter
is neutral-to-faster on dense). Skip-heavy gate **UNMET at 15x**: measured **8.9x
shufti / 5.5x truffle** cyc against the fair same-output scalar baseline. The win
is real; the 15x bar is a baseline-calibration mismatch, quantified in section 5.

## 1. What was built

Ported the legacy shufti (nibble) and truffle (wide) NEON classify kernels from
`crates/simd-scan/src/{neon.rs,alphabet.rs}` (read-only source) into skinny as a
colocated prefilter module with a scalar parity oracle, and productionized them
behind a fused GATE=16 "find next member" primitive wired through the three-layer
seam R1 names.

- **bbnf-simd/src/scalar/prefilter.rs** (arch-neutral, +109) — NibbleLut/WideLut
  (ported per-singleton-bit encoding: lo[b&0xf] & hi[b>>4] != 0 iff member; EXACT,
  no false positives -> the prefilter needs no confirm step) + the executable-spec
  oracle classify_block_scalar / find_member_scalar.
- **bbnf-simd/src/aarch64/prefilter.rs** (NEON, +125) — classify_block_nibble
  (shufti: 2x vqtbl1q per 16B stripe, AND'd) and classify_block_wide (truffle:
  4x vqtbl1q over the paired table, OR'd), packed via the in-tree
  movemask::movemask_u8x16 (vshrn #4); plus any_member_64_{nibble,wide} — the
  cheap vmaxvq reject that carries NO movemask on the all-miss skip path.
- **bbnf-simd/src/lib.rs prim (+112)** — PrefilterSet::new(members) compiles the
  LUTs + selects the backend; find_member is the fused GATE=16 finder: Phase 1
  scalar-probes PREFILTER_GATE=16 bytes (dense fast path, zero vector setup);
  Phase 2, only on a run that crosses the gate, engages the NEON 64-byte
  cheap-reject loop (movemask only to locate the hit block); Phase 3 scalar tail.
- **bbnf-simd/src/dispatch.rs (+27)** — PrefilterBackend {Scalar,NeonNibble,
  NeonWide} + select_prefilter_backend (cost-driven from cardinality: <=8 shufti,
  <=16 truffle, else scalar — mirrors legacy KernelShape::select), the sibling
  switch to SelectedBackend.
- **ir/src/lib.rs (+4)** — SimdMode::Prefilter next to Exact (the IR signal a scan
  site skips long non-member runs rather than emitting every position).
- **bbnf-simd/tests/prefilter_parity.rs (+207)** — differential parity.
- **bbnf-bench/src/bin/prefilter_bench.rs (+314)** — the cold paired PMU harness.

Diff: **9 files, +901 / -0** (see patch.diff). No @generated file touched; no x86;
no stub/legacy alias. `cargo test -p bbnf-simd --test prefilter_parity` — 3/3 pass:
direct NEON-vs-scalar block classify (set sweep 1..=16, alignment sweep 0..64),
fused-finder vs oracle (dense/skip-heavy/boundary, both backends), and a full
twitter.json structural-position scan (byte-identical position vectors).

## 2. Measured table — cold paired PMU, N=200

Metric of record: PMU cyc/B (proc_pid_rusage V5 ri_cycles). Speedup =
scalar / fused (>1 => prefilter faster). Two back-to-back runs, loadavg per block.
Lanes are both real scan consumers producing the identical structural count
(parity-asserted before timing).

| corpus | lane | bytes | backend | fused cyc/B | scalar cyc/B | cyc speedup | wall speedup (95% CI) | loadavg |
|---|---|--:|---|--:|--:|--:|--:|--:|
| skip_heavy_shufti | fused vs scalar | 524288 | NeonNibble | 0.1154 | 1.0271 | **8.90x** | 8.86x (+/-0.20) | 12.7 |
| skip_heavy_truffle | fused vs scalar | 524288 | NeonWide | 0.1841 | 1.0230 | **5.56x** | 5.74x (+/-0.11) | 12.7 |
| twitter (dense JSON) | fused vs scalar | 631515 | NeonNibble | 1.6262 | 1.7021 | **1.05x** | 1.04x (+/-0.03) | 12.7 |
| bootstrap (dense CSS) | fused vs scalar | 232803 | NeonNibble | 2.3745 | 2.7213 | **1.15x** | 1.22x (+/-0.03) | 12.7 |

Run 2 corroborates (loadavg 12.7): shufti 7.1-9.1x, truffle 4.6-5.8x, twitter
1.04x, bootstrap 1.17x cyc. Skip-heavy = 512 KiB of non-member 'a' with a member
every 8192 B (long runs). Dense = the gate-named corpora (twitter via test-fixtures,
bootstrap-5.3.3.min.css). N=200 >= gate floor; paired per-sample interleaving nulls
load drift on the ratio.

## 3. Gate adjudication

- **Dense >=0.95x — MET.** twitter 1.05x and bootstrap 1.15x cyc (both >1.0x, both
  runs). GATE=16 keeps dense corpora in the scalar probe; the 64-byte cheap-reject
  bounds the per-gap cost so even twitter's long string-value gaps (member set
  includes ") do not regress. (An earlier 256-byte skip stride regressed twitter to
  0.82x by over-classifying run boundaries; the 64-byte-granular reject fixed it.)
- **Skip-heavy >=15x — UNMET.** Measured 8.90x shufti / 5.56x truffle cyc. Real,
  parity-exact, but below 15x. Root cause in section 5 (not a defect — a physical
  ceiling against this baseline).

## 4. Hot-path visual verification (otool -tV, the shufti reject loop inlined into find_member)

```
1000052d0  ldp   q6, q7, [x11]          ; load stripes 0,1 (32B of the 64B block)
1000052d4  and.16b   v16, v6, v23       ; low nibble  (v23 = 0x0F splat)
1000052d8  ushr.16b  v6, v6, #0x4        ; high nibble
1000052dc  tbl.16b   v16, { v4 }, v16    ; shufti lo LUT  (v4 = NibbleLut.lo)
1000052e0  tbl.16b   v6,  { v5 }, v6     ; shufti hi LUT  (v5 = NibbleLut.hi)
1000052e4  and.16b   v6,  v6, v16        ; member iff lo & hi != 0
   ... (stripes 1,2,3 identical: 8x tbl.16b total, 4 stripes x 2 nibble lookups) ...
100005328  orr.16b   v7,  v7, v17        ; OR-reduce the four stripe match-vectors
10000532c  orr.16b   v16, v18, v6
100005330  orr.16b   v7,  v7, v16
100005334  cmtst.16b v7,  v7, v7         ; nonzero lanes -> 0xFF
100005338  umaxv.16b b7,  v7             ; single-instruction ANY-member reject
10000533c  fmov      w11, s7             ; -> GPR, branch if zero: skip 64 bytes
```

Confirmed: the skip path is the branch-free ported nibble fan reduced to one umaxv
— NO shrn/addv movemask on the reject (movemask appears only in the classify_block
locate path, run once per member found). Disassembly-verified.

## 5. Why 8.9x not 15x — the baseline calibration (honest refutation of the bar, not the mechanism)

The >=15x figure is inherited from PROTO-PB-1, whose baseline was parse-that's
lazy-DFA self-loop — a multi-cycle-per-byte automaton (lazy state cache +
LazyStateID decode + match/quit checks per byte). This prover's fair same-output
baseline is a bare membership byte-scan, measured at ~1.03 cyc/B — already near
memory-streaming speed. Speedup scales linearly with the consumer's per-byte cost
against the fused floor:

- Fused prefilter floor, measured: **0.115 cyc/B** (shufti skip). This is the
  load-bearing number — the NEON block-skip is doing 64 B per ~7.4 cycles.
- Against a 1.03 cyc/B scalar -> 8.9x (the measured ceiling here).
- The same fused floor crosses **15x against any consumer >= ~1.75 cyc/B**, and
  20-40x against a 2-5 cyc/B string/escape-tracking or DFA scanner (which is what a
  real "skip to next structural" consumer costs, and what PROTO-PB-1 measured).

So the mechanism delivers the PROTO-PB-1-class skip floor (0.115 cyc/B); the 8.9x
headline is an artifact of comparing against an atypically cheap oracle. The
prefilter's value is realized when the accelerated consumer does real per-byte work
— which the wiring in section 6 exists to route.

## 6. Integration cost + the exact seam

The three-layer seam is wired and compiles: (a) prim::PrefilterSet at
bbnf-simd/src/lib.rs (the primitive facade, next to the other prim kernels);
(b) PrefilterBackend/select_prefilter_backend at dispatch.rs (the cost switch);
(c) SimdMode::Prefilter at ir/src/lib.rs:403 (the recognizer signal). Remaining to
reach a live end-to-end dense number through json_canon_bench/css_canon_bench: the
runtime scan consumers (runtime/src/grammars/json/scan.rs::scan_structurals,
neutralized on the live path — reachable only under BBNF_TAPE_CAPACITY=A|B|C per
R1) emit EVERY structural (SimdMode::Exact), not a skip-find, so they are not the
prefilter's consumer shape. The honest live consumer is a run-skipper (long
string-body / whitespace skip to the next structural); routing
passes::derive_recognizers to emit SimdMode::Prefilter for self-looping scan sites
and lowering it to PrefilterSet::find_member in codegen is the ~1-day integration
that converts the measured 0.115 cyc/B floor into an end-to-end parser win. Net new
surface is small and colocated (901 LOC, 2 kernels + oracle + fused finder + seam).

## 7. Reproduce

```
cd <worktree>/skinny
cargo test  -p bbnf-simd --test prefilter_parity        # 3/3 parity
cargo build --release -p bbnf-bench --bin prefilter_bench
./target/release/prefilter_bench 200                    # cold paired PMU
```
