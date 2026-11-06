# G5/G6 Amendment — dispatch-targeting NEON redesign (ratified 2026-06-02)

## Finding (G5/G6 research, waves/g5g6/research.md)
The §8 inert-run-skip retarget is already wired live (G2: find_css_significant = the two-fan eq-set
NEON OR-reduce, called unconditionally on the hot scan). BUT measurement proves it barely helps: inert
runs are median 6-9 bytes / p90 17-33 (NEON block = 64); inert bytes in runs >=64 = animate 0.0% /
tailwind 0.7% / bootstrap 2.5% / material 24.6%. The 79.5% self-time is in the per-significant-byte
DISPATCH (membership test + match + load), NOT the inert gaps. Vectorizing the SKIP does not address
where the time goes.

## Amendment (ratified — redesign over the as-specified skip-wire)
G6 redesigns the NEON to target the DISPATCH (simdjson-stage-1 structural classification):
- A NEON kernel classifies N bytes (16/64) at once into {significant-bitmask, kind-per-position} in
  one vectorized pass (vqtbl/vceqq classification over the caller-supplied significant byte-set +
  the structural-kind table), grammar-NEUTRAL (the byte-set + kind-table are caller data).
- The scalar shell ITERATES the structural positions from the bitmask (count-trailing-zeros to the
  next significant byte) instead of testing every byte — eliminating the per-byte dispatch overhead.
  The shell still owns recursion / strings / comments / errors at each structural position.
- dav1d discipline: scalar-reference FIRST (the existing scalar scan is the reference) + the checkasm
  differential (classify-bitmask matches the scalar per-byte classification) over the REAL
  71KB-495KB corpora in checkasm_parity.rs (NOT the inline 5-toy-string micro-test) + aarch64
  NEON/dotprod ONLY (no x86, no SVE).
- The kernel reaches the hot path AT admission (the generated css_l4 scan / css_balanced_component_scan
  calls it live, non-test; the samply admission profile captured).
- Carry-forward: delete the genuinely-dead find_comment_close (test-only); G5 neutralize the
  zero-sampled json/scan.rs (keep the capacity probe).

## Gate (per SPEC §8 + the CH2-V10 timed-plane fold)
G6 pre-H1 gate = CORRECTNESS (checkasm differential scalar==vector, real corpora) + ADMISSION (live
caller census, samply-confirmed) + 9-field cssparser parity EXACT + no >SOTA regression. The speedup
MAGNITUDE figure comes from the corpus-in-timer symmetric harness (css_canon_bench cold), reported at
H1 honestly (the realism finding: the dispatch vectorization SHOULD help where the skip did not, but
the magnitude is measured, not asserted; if it does not improve >SOTA, that is the honest H1 close).

## Disposition
Wave-level redesign, ratified; folds into SPEC §8 at the next Pass Omega. Does NOT relax the dav1d
discipline or the neutrality; it retargets the NEON at the measured bottleneck (the dispatch) instead
of the as-specified inert skip (which the measurement proved ineffective).
