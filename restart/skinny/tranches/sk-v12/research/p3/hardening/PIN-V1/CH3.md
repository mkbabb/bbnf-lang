# SK-V12 S-P3 CH3 Regression/REDRESS - Cycle PIN-V1

Pass: S-P3 Synthesis-Plan CHALLENGE.
Cycle: PIN-V1.
Lens: CH3 regression, guard floors, REDRESS adjacency, and honest failure handling.
Date: 2026-05-20.

## Verdict

REVISE.

Confidence: 88%.

The packet is directionally regression-safe: CSS L4 is first, the CSS close bar is
`track1_mbps > lightningcss_mbps + 1`, JSON direct/typed guards are carried,
`parse_only` is diagnostic-only, REDRESS 96/97/98 and 88/89/90 are historical
evidence rather than category blockers, and W5 requires REDRESS evidence for
CSS miss / union miss / ASM miss. However, PIN-V1 contains three guard-shape
inconsistencies that can produce a hidden fallback trigger, a blocked correctness
prerequisite, or a paper "ADMIT" label below the CSS close bar.

## Findings

1. REVISE - W2 entry is inconsistent across the packet and can block the SIMD
   correctness prerequisite behind the CSS redress it is supposed only to
   precede for SIMD admission.

   - `SPEC.md:243` and `DISPATCH-PROMPT.md:74` make W2 conditional on W1a.
   - `SPEC.md:404-408` likewise uses W1a PASS as W2 entry.
   - `p3b-wave-sequencing.md:77` and `p3c-falsifiability-gates.md:250-253`
     require W1b to have a measured CSS row or CSS redress attempt before W2.

   Regression risk: the known `escape_mask_64` correctness blocker could remain
   unresolved longer than necessary, while a later plan could argue W2 was not
   eligible and route around it. The user pin only requires W2 before new SIMD
   admission, not before scalar CSS L4 baseline work.

   Required fold: choose one entry rule everywhere. CH3 recommends W2 entry =
   W1a PASS plus named falsifier/checkasm/corpus commands; W2 blocks W4 and any
   SIMD-backed W3/W1b plan, but does not require a prior W1b CSS redress attempt.

2. REVISE - P3-B has a fallback typo that can mis-trigger Sheets/BBNF-self off
   the wrong wave.

   - `p3b-wave-sequencing.md:98-104` says Sheets and BBNF-self may enter only
     after "W2 records a measured CSS L4 redress attempt."
   - The correct CSS redress source is W1b, as stated in `SPEC.md:369-388`,
     `p3c-falsifiability-gates.md:235-241`, and
     `DISPATCH-PROMPT.md:79-82`.

   Regression risk: W2 is the `escape_mask_64` correctness wave and cannot record
   the CSS L4 redress attempt. Leaving this wording creates either an impossible
   fallback gate or a hidden fallback after a non-CSS wave.

   Required fold: replace the P3-B fallback trigger with "after W1b records a
   measured CSS L4 BLOCKED/REJECTED redress attempt"; keep fallback out of W1b
   and out of the SK-V12 ADMIT close unless the user pin is amended.

3. REVISE - W3/W4 use "ADMIT" language for local primitive or JSON-guard success
   paths that do not necessarily satisfy the CSS L4 campaign ADMIT bar.

   - `p3c-falsifiability-gates.md:70-71` says W3/W4 "ADMIT" if they improve or
     maintain a selected CSS/guard row.
   - `p3c-falsifiability-gates.md:301-310` labels the W3 local microbench/guard
     path "ADMIT exit", including a JSON fallback consumer after CSS redress.
   - `p3c-falsifiability-gates.md:340-352` likewise labels W4's local primitive
     path "ADMIT exit", including JSON fallback guard maintenance.
   - `SPEC.md:456-459` and `SPEC.md:503-507` are stricter and correctly require
     CSS Track 1 to beat `lightningcss_mbps + 1` for ADMIT, with measured reject
     otherwise.

   Regression risk: a W3/W4 primitive could be recorded as "ADMIT" on a JSON guard
   maintain or local microbench gain, then W5 would have to clean up the mismatch.
   That is exactly the paper-close shape CH3 is meant to catch.

   Required fold: reserve "ADMIT" for the SK-V12 CSS close bar only. Rename the
   local W3/W4 success paths to `behavior PASS` or `FIXPOINT-credit measured
   reject/pass`, and state that JSON fallback consumers can only provide
   fixpoint evidence or guard evidence, never SK-V12 ADMIT.

4. REVISE - P3-A's candidate summary overstates CSS floor requirements for
   fixpoint-credit attempts.

   - `p3a-candidate-shortlist.md:319-320` requires C4 and C5-C7 to keep "CSS row
     floor above" for union/SIMD/ASM attempts.
   - `SPEC.md:65-82`, `p3c-falsifiability-gates.md:313-320`, and
     `p3c-falsifiability-gates.md:369-374` correctly allow measured, materially
     differentiated rejects to count toward FIXPOINT when CSS ADMIT is
     uncloseable.

   Regression risk: the shortlist table can block the very measured reject route
   required by the user-pin FIXPOINT clause, or force W3/W4 to paper over a miss
   as an admission.

   Required fold: split each candidate threshold into two rows: CSS ADMIT
   requires `track1_mbps > lightningcss_mbps + 1`; FIXPOINT credit requires
   measured source/microbench/row evidence, REDRESS material differential, guard
   disposition, and zero orphan state where applicable.

## Positive Checks

- CSS L4 first target and lightningcss strict bar are stated in `SPEC.md:37-49`,
  `SPEC.md:84-86`, `p3c-falsifiability-gates.md:214-231`, and
  `DISPATCH-PROMPT.md:147-151`.
- JSON direct/typed guard floors are present in `SPEC.md:186-210` and
  `p3c-falsifiability-gates.md:93-126`; demotion requires measured REDRESS.
- `parse_only` is diagnostic-only in `SPEC.md:179`, `SPEC.md:218`,
  `p3c-falsifiability-gates.md:122-126`, and `DISPATCH-PROMPT.md:180`.
- REDRESS 96/97/98 and 88/89/90 are treated as historical evidence, not category
  blockers, in `SPEC.md:565-575`, `p3e-preblocked-ledger.md:12-23`, and
  `DISPATCH-PROMPT.md:193-200`.
- W5 close correctly requires ADMIT or FIXPOINT evidence, including CSS attempt,
  union attempt, ASM-gen attempt, zero orphans, REDRESS misses, and close-doc
  agreement in `SPEC.md:512-545` and `p3c-falsifiability-gates.md:381-422`.

## Required Folds

1. Normalize W2 entry and dependency wording across P3-B, P3-C, SPEC, and
   DISPATCH-PROMPT.
2. Fix the P3-B fallback trigger from W2 to W1b.
3. Replace W3/W4 local "ADMIT" labels with non-close terminology unless the CSS
   L4 `> lightningcss_mbps + 1` bar is met.
4. Split P3-A C4/C5-C7 gates into ADMIT versus FIXPOINT-credit measured-reject
   forms.
5. Rerun CH3 on the folded packet before S-P3 can claim a clean cycle.
