# CRIT-P4 — Critique of PROTO-P4 (measured KernelCostTable flips an extraction decision)

Subject: `restart/skinny/tranches/sk-v21/prototypes/PROTO-P4.md` (worktree
`wf_be85d0ab-073-10`); spec section under test: SPEC-DRAFT-P1.md §5 (the decision engine, TW4),
specifically §5.1 "the one net-new artifact is the measured `KernelCostTable` … never estimated"
and §5.2 "every hardcoded threshold demote to admissibility filters / table cells."

Verdict on the instrument: **the kernel cost curves are sound and the narrow null is refuted, but
the prototype tested the wrong selector, measured proxy kernels, and its two headline flips
exercise no live-fleet grammar** — so "flips a real extractor decision" is shown in logic, not in
the live consumer. Convergence 64.

## What is solid (verified against tree)

- The threshold ladder is real and exactly as quoted: `KernelShape::for_singleton_count`,
  `NIBBLE_LUT_MAX = 8`, `WIDE_LUT_MAX = 16` at `crates/ir/src/passes/recognizers/kernel_shape.rs:63,68,71-79`;
  the doc confirms NibbleLut/WideLut are "Same intrinsic count" (`:49`), so no cost flip lives at N=8.
- The harness is present and method-honest: `p4-kernelcost/src/main.rs` (422 LOC) runs a byte-exact
  parity gate against a scalar reference across the N-sweep (`:309-339`), min-of-repeats estimator
  (`:261-263`), all four kernels lifted verbatim. The PMU counter is the same cold-harness
  `ri_cycles` (`skinny/crates/bbnf-bench/src/bin/profile_direct.rs:45,53,56`).
- FLIP 3 is a genuine in-tree inconsistency: `movemask.rs:4-5` uses `vshrn_n_u16::<4>`;
  `byte_class_from_eq_set_64.rs:79-84` defines a *local* `movemask_u8x16` using `vaddv_u8`;
  `classify_tbl4.rs:5` imports the vshrn one on the hot path. Two costs for one named op — real,
  and correctly surfaced. It is, however, a bonus unrelated to the cost-table selection claim.
- The kernel physics are believable: LUT is O(1) (~14 cyc/64B), multipass is linear (~4.9/needle),
  crossover N*≈2. The claim that the LUT wins from N≥3 and MultipassCmpEq is never cost-optimal for
  a large alphabet is architecturally correct — a 64-entry low-6 `vqtbl4q` table can represent far
  more than 16 needles, so the N>16 boundary is collision-feasibility, not cardinality.

## Where the evidence does not carry the verdict

**1. Wrong selector: the flipped ladder has TESTS-ONLY consumers.** `for_singleton_count` /
`select_kernel_strategy` in `crates/ir` is consumed only by `crates/ir/tests/kernel_shape.rs` and
`structural_alphabet_extended.rs` (plus the `recognizers/mod.rs` re-export) — no emitter, codegen,
or scan path reads it; its own doc is future-tense ("At W6 the emitter *will* read the same
struct" — kernel_shape.rs:24-25). The LIVE scan dispatch is a *different* `KernelShape` at
`crates/simd-scan/src/alphabet.rs:103,118-124` with variants `Empty/NibbleLut/WideLut/MultiCmp`
selected by `KernelShape::select(alphabet)`. The prototype never examined it. The conclusion does
transfer — the live ladder is byte-identical (`1..=8 / 9..=16 / _`, alphabet.rs:120-123) — but that
transfer is this critique's verification, not the prototype's; flipping a tests-only selector is,
by the SPEC's own §1 master-pathology law, flipping substrate with no consumer.

**2. Proxy kernels, not the live ones.** The measured kernels are lifted from
`skinny/crates/bbnf-simd` (frozen skinny workspace); the live totality path dispatches to
`crates/simd-scan`'s `scan_nibble/scan_wide/scan_multi` (neon.rs:51-53). Cost-curve equivalence
between the skinny `classify_multipass` and totality `scan_multi` is assumed, never benched. On
avx2/wasm the live path even collapses `WideLut | MultiCmp → scan_multi` (avx2.rs:43), so FLIP 2's
"demote MultipassCmpEq" has no separate kernel to demote off-NEON.

**3. Neither headline flip exercises a current-fleet grammar.** FLIP 1 (N=1 → multipass) needs a
single-delimiter grammar — the report itself names only CSV, which is NOT in the fleet (SPEC §9
schedules the CSV oracle for TW6). FLIP 2 concedes "no grammar in the fleet should ever select
MultipassCmpEq." JSON (6-7 bytes) and CSS both land in NibbleLut/WideLut and do not flip. So the
cost table, as demonstrated, flips only *hypothetical* (CSV/single-delimiter) or *dead-branch*
(N>16) decisions. This inadvertently surfaces a real plan gap the SPEC does not flag: TW4's
KernelCostTable has no consumer that changes an outcome until TW6's third language lands — a
TW4-before-TW6 sequencing tension that violates §1's "no substrate without its consumer in the
same wave" unless the cost table's live flip is tied to a grammar that actually exercises it.

**4. Cited raw logs are absent.** PROTO-P4.md:148-149 cites `scratchpad/p4_run1.log` and
`p4_run2.log` as the numeric backing; neither exists in the worktree (an exhaustive `*.log` search
returns many other logs, not these two). The table's cyc/64B cells are therefore not reproducible
from cited artifacts — though the harness is present and re-runnable, so re-derivation is possible.

**5. Load and confidence.** loadavg 17.3-23.0 is disclosed prominently (`:46-51`), far from the
charter's quiet-<2. The ratio-robustness defense is reasonable for the *sign* of the crossover, but
the LUT (table loads + one SIMD↔GPR movemask crossing/lane) and multipass (register-only) plausibly
inflate non-uniformly under contention, so even N* could drift by a needle under quiet capture.
Confidence 82% is a touch high for a directional result whose crossover was located under 17-23
loadavg.

## Does the SPEC over-claim?

§5.1/§5.2 claim the cost table would *differ from* the thresholds — this the prototype supports
convincingly (the null "thresholds already match measurement" is refuted). The SPEC does NOT
over-state the physics. What it omits is the consumer-reality the prototype exposes: the table's
demonstrated flips are hypothetical or dead in the current fleet, so §5's "KernelCostTable first"
ordering (TW4) must either (a) bind its live consumer to TW6's third language, or (b) name a
current-fleet decision it actually changes, or the artifact is itself the master pathology it warns
against. That disposition is unwritten. FLIP 3 (movemask) is a real, immediately-actionable
in-tree fix the SPEC does not yet capture.

## Convergence: 64

The falsifiable question is answered and the kernel physics are trustworthy, but the answer lands
against a tests-only selector via proxy kernels, its two flips exercise no live grammar, the cited
logs are missing, and it surfaces an undispositioned TW4/TW6 sequencing gap. Another loop turn is
required: re-point the flip at `crates/simd-scan::KernelShape::select` with the live
`scan_nibble/scan_wide/scan_multi` kernels, name the current-fleet decision the table changes (or
bind the consumer to the third-language wave), fold FLIP 3 into TW0's honesty sweep, and re-capture
quiet.
