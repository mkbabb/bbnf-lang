# SK-V9 S-P3 V2 CHALLENGE consolidation

Date: 2026-05-18.
Cycle: V2.
Inputs: `restart/skinny/tranches/sk-v9/research/p3/hardening/V2/CH{1..6}.md`.

## Verdict — V3 fold required (the V2 fold under-scoped)

| Lens | V1 | V2 | Verdict |
|---|---:|---:|---|
| CH1 CORRECTNESS | 62.5% | 73.7% | REVISE |
| CH2 GENERALITY | ~81% | 91.7% | ACCEPT (qualifying) |
| CH3 REGRESSION | ~83% | 93.3% | ACCEPT-WITH-NOTE |
| CH4 COST | ~37% | 59.4% | REJECT |
| CH5 HIDDEN COUPLING | 44% | 93.3% | ACCEPT-WITH-NOTE |
| CH6 ANTI-PAPER-CLOSE | 66.7% | 88.9% | NEAR |

## The root cause — the V2 fold was split and under-scoped

The V2 fold dispatched F-MAIN (re-author the two P3-F drafts) +
F-AUX (surgical P3-A/B/C). F-MAIN succeeded — **the SPEC + DISPATCH
drafts are now sound and canonical** (CH1: "the two P3-F drafts are
sound"; CH6: "zero `[INTEGRATE]` markers; the SPEC/DISPATCH are the
canonical, correct authority"). But F-AUX touched only P3-A/B and
cosmetically P3-C; **P3-C, P3-D, P3-E were never re-authored to the
unified manifest.** The result is the inverse of the V1 defect: the
SPEC is correct, the siblings lag it.

Surviving defects, all in un-refolded siblings:
- P3-C §1.4/§2 still gate the old two-wave manifest (W4 paired
  codec+string-block, W5 ASM); no W4a-d gate table.
- P3-D §3.2 still says "SPEC §0.3 names 7 / §0.4 names 31" (describing
  the superseded V1 SPEC).
- P3-E still uses the V1 lettered scheme (W-AC/W-RG/W-UE/W-UC/W-AS).
- P3-A/B/C/D/E still stamped `Cycle: V1`.

## The one genuinely new substantive defect (CH4)

**W4b — the `escape_codec_hex_unit` codec — is ~1,045 net LOC under a
single 75-min redress cap.** V2 sub-waved the W4 *bracket* (W4a-d) but
never sub-divided the *codec itself*. P2-E §7.4 sizes the codec at
~1,045 net across 11 slices, ~6.0 h aggregate. One 75/90-min redress
cannot land it. CH4 also flags W3 (~465-635 hand + ~120 regen) likely
overruns its redress cap, and the P2-A-warned MEDIUM→HIGH risk
escalation is unrecorded.

## V3 fold — ONE comprehensive integration agent

The V2 mistake was splitting the fold. V3 dispatches ONE agent owning
the full integration so the manifest is consistent across all five
files. The agent:

1. **Re-authors P3-C, P3-D, P3-E** to the unified W1-W5 / W4a-d
   manifest the SPEC carries. P3-C gains a W4a-d gate table; P3-D's
   ruling-prose goes past-tense (10-outcome / 36-field are now IN the
   SPEC, not "should be"); P3-E's lettered scheme maps to the numeric.
   Bump all P3-A..E to `Cycle: V3`.
2. **Sub-divides W4b** (the codec) along P2-E §7.4 slice seams into
   W4b-1 (scalar reference + checkasm harness), W4b-2 (fixed-width
   bodies + JSON `unescape_four_unicode_escapes` consumer), W4b-3
   (variable-width const-generic bindings + codegen). Each sub-wave
   fits a 75-min redress. Update the SPEC §2 manifest + §7 per-wave
   sections + the DISPATCH draft + P3-C's gate table consistently. The
   W4a+W4b pairing (codec closes zero rows without the string-block
   widening) is preserved — W4a pairs with W4b-2 (the JSON consumer
   sub-wave that actually moves the rows).
3. **W3 cap check** — verify W3's LOC against the 75-min redress; if it
   overruns, sub-divide W3 (W3a substrate + W3b consume_structural
   deletion) or record the cap honestly with a CHALLENGE-gated
   exception. Record the MEDIUM→HIGH risk escalation P2-A warned.
4. **Arithmetic corrections** — N1 `update_center` floor 14369→14370
   (`ceil(15806/1.10)`); N2 `gsoc-2018` no-regression base 21646→22184
   (live RESULTS:24); N3 W10b six-row floors use ONE rounding
   convention; N5 `SkV8ComparatorEvidence` 6→7 fields per the live
   struct.

## V3 path

1. Dispatch ONE comprehensive V3-fold integration agent (re-author
   P3-C/D/E + sub-divide W4b + W3 cap check + arithmetic).
2. Commit `docs(sk-v9-p3-v3): unify all seven artefacts + sub-divide W4b`.
3. Re-dispatch CHALLENGE V3 (all six lenses).
4. Expected: all six clear ≥95% — the SPEC is already canonical and
   sound; V3 brings the siblings into line and resolves the one real
   substantive defect (W4b cap).

## Convergence forecast

S-P3 V1 (fail, unintegrated draft) → V2 (fail, under-scoped fold) → V3
(one comprehensive fold) → V4 (second-consecutive confirm). The
content has been accepted since V1; the failures are integration
discipline. V3's single-agent scope prevents the V2 split-fold mistake
from recurring.
