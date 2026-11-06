# CRIT-PB-6 — SIMD mantissa accumulation probe (PT-R2.W4)

Adversarial critique of `prototypes/PROTO-PB-6.md` against the codex section it feeds
(`SPEC-DRAFT.md:486-497`, PT-R2.W4 + class-c close gate; grounding research/02:209-231, §1:30-63).
**Convergence: 91%.** GO stands; the residuals are close-gate wording, all wave-owned.

## Provenance verified (the prototype's citations are true)

- Baseline "copied verbatim from `number_simd.rs:31-102`" — confirmed: `count_leading_digits_neon`
  is lines 31-61, `accumulate_digits_neon` is 68-102 (read of the live parse-that file).
- "vaddv AND-weights anti-pattern at `:41-59`" — confirmed: the `[1,2,4,8,…]` weight vector +
  `vand_u8` + two `vaddv_u8` at 43-57. `:73-101` is exactly the three-way SWAR body
  (`count==16` / `<=8` / `9..=15`). The SPEC's `:31-61` / "SWAR today" framing (486-488) matches.
- No source drift: the SPEC's own PB-6 registry entry (747-753) states the same question/method as
  the prototype doc — no summary skew.

## The algorithm is correct — I re-derived the two load-bearing tricks

- **vshrn nibble movemask `tz>>2`.** `vshrn_n_u16(mask,4)` narrows each u16 lane to
  `(B_hi&0xF)<<4 | (B_lo>>4)`, so for 0x00/0xFF mask bytes every input byte m maps to bit position
  4m in the 64-bit result (even m via low nibble, odd m via high nibble — both land on 4m).
  First non-digit at byte k → `trailing_zeros=4k` → `>>2 = k`; all-digit `nd=0` → `tz=64 → 16`,
  branchless. The planted-trap falsifier (raw tz = 4×15 = 60) is exactly this identity. Sound.
- **Lemire right-align fold.** `SHUF[c][j]=(j-(16-c))` yields valid indices only in `0..c-1`
  (max `15-(16-c)=c-1≤15`; positives never exceed 15 → no accidental wrong-lane pull), and
  wrapped negatives land ≥128 → out of the `vqtbl1q` 0..15 range → 0. Digits right-align to the
  high lanes the `[10,1,…]/[100,1,…]/[10000,1,…]` fold expects; 16-digit max 1e16 fits u64, so the
  `wrapping_mul(1e8)` combine cannot overflow. The three vector folds + scalar combine are the
  standard parse-16 chain. (Prose nit: "4-stage pairwise fold" is 3 vector folds + 1 scalar
  combine; "negatives wrap ≥128" — the real out-of-range threshold is ≥16, not 128. Cosmetic.)

Correctness verdict: the YES follows. 3.8M random runs across counts 1..19 + full canada, with a
deterministic-dataflow kernel, is adequate saturation to catch any systematic shuffle/weight error
(failures here would be structural, not probabilistic).

## Evidence quality — honest, with the load defended correctly

- **Load defence is technically valid.** `proc_pid_rusage` `ri_cycles` counts on-core CPU cycles,
  not wall time, so descheduling under loadavg 11-24 does not inflate it; paired-interleaved
  (baseline then candidate, same loop, same P-state) cancels DVFS/contention in the RATIO. The
  certified metric — **ins/B 6.82→2.99 (2.28×), 5.26→1.69 (3.11×) on long runs** — is genuinely
  load- and cache-invariant (a retired-instruction count), so it survives the warm/loaded regime.
  This is the right thing to certify and the prototype certifies exactly it. Good.
- **Attribution honesty is strong and CONFIRMS the codex.** The decomp (Mula −40/−61%, vshrn
  count −4/−6%) matches research/02 §1's own pre-registered honesty: "settled +35% on the movemask
  primitive itself … mid-single-digit-to-teens percent whole-kernel win is the honest expectation"
  (02:60-63). The vshrn count is NOT over-sold anywhere in the codex — 02 scoped it low, PB-6
  validated it. The "highest materialization-leverage" phrase (SPEC 489) attaches to the
  *accumulate/Mula* clause, which is where PB-6 puts the lever. **No codex over-claim on numbers**:
  the SPEC cites no inflated percentage; the 53.8%/69.6% headlines live only in the prototype and
  are explicitly leverage-gated there.

## The two real residuals (wave-owned, hence not blocking convergence)

1. **WARM microbench vs the cold-only standing law — the close gate must re-specify.** inner=40
   passes over 222k stripes is deeply warm (fully-resident L1/L2); the cyc/B *absolutes* and the
   "53.8%" are warm/steady-state, which the standing precept forbids as a LANDING metric
   (no-warm-benches; PASS-ALPHA "cold-only"). The prototype is honest ("absolutes directional; the
   RATIO and ins/B are the certified findings") and a warm kernel isolate is a legitimate
   *instrument*. But the class-c close gate (SPEC 494-495) currently says only "paired cyc/byte
   delta under load accounting" — it must be tightened to bind a **COLD full-parse canada cyc/B**,
   NOT inherit PB-6's warm kernel figure. The prototype's own recommendation (line 116) says "a
   paired canada cyc/B window" without the cold qualifier — pin it.

2. **End-to-end materiality is unquantified — kernel win ≠ scanner win ≠ full-parse win.** PB-6
   measures the digit kernel in isolation; the live path adds dot/exponent/sign handling, tape
   emission, memory traffic, and the Eisel-Lemire `compute_f64` that *consumes* the mantissa (the
   prototype says so, 98-100). The decomp table only splits *within* the kernel (vshrn vs Mula) —
   there is no Amdahl figure for the kernel's *share* of number-scan or of full parse. A 53% kernel
   ratio could be a single-digit-percent full-parse delta. The "highest materialization-leverage"
   claim rests on research/02:226-228's stage-2-dominator citation (canada 1.847 c/B), which is
   grounding but not a PB-6 measurement. The close gate must therefore require the COLD end-to-end
   canada number improves — not merely the kernel ratio — else a real kernel win lands without
   confirming it moves the parse.

## Lesser notes (do not gate)

- **Corpus-specificity must ride forward as a caveat.** The win is gated on canada's long
  fractional runs; ≤2-digit corpora are a near-wash (7.6%, CI [.910,.939]). Honestly disclosed in
  PB-6, but the codex must carry it so no downstream synthesis reads "53.8%" as a universal
  number-parse win. A small-integer-heavy JSON corpus would see far less.
- **checkasm-liveness terminology is loose for an intrinsics kernel.** Per the sk-v21 adjudication
  "vshrn obviates hand asm," this kernel is `core::arch` intrinsics, not hand-asm, so it is not
  literally "inside the dav1d asm-gate" (02:57-59 and PB-6:117 both inherit this slip). Post-x86
  abrogation, aarch64 NEON is cfg-guaranteed live (no runtime dispatch), so the "liveness" half of
  the gate is near-vacuous — the binding that matters is SIMD-vs-scalar exact-value parity, which
  PB-6 already supplies. Keep the parity gate; drop or reword the asm-liveness framing for this
  specific kernel.
- **Wave name slightly over-weights vshrn.** "SIMD mantissa + vshrn" gives the −4/−6% count trick
  co-billing with the −40/−61% Mula lever. Accurate to keep both (branchless movemask is a KISS
  cleanliness win that also deletes the 3-way branch), but the perf narrative is Mula-carries-all.

## Verdict

The prototype is technically excellent, correctly derived, and scrupulously honest about the
warm/loaded regime; the verdict follows from the load-invariant ins/B evidence. The codex does not
over-claim beyond it — if anything research/02 pre-scoped the weak lever (vshrn) low and PB-6
confirmed it. The only open design question is the class-c close-gate wording: it must bind a COLD
end-to-end canada cyc/B + a kernel-share check, not the warm kernel isolate. That is wave-owned by
PT-R2.W4, so it does not block executability. **91% — GO, land under the cold end-to-end gate.**
