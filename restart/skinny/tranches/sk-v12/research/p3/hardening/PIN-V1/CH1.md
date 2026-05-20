# SK-V12 S-P3 CH1 Correctness Review - Cycle PIN-V1

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V1.
Lens: CH1 correctness and gate consistency.
Date: 2026-05-20.
Disposition: REVISE.
Confidence: 92%.

## Scope

Reviewed the PIN-V1 S-P3 packet:

- `restart/skinny/tranches/sk-v12/SPEC.md`
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3f-spec-draft.md`

Checked against the user pin, pin-aware `SYNTHESIS.md`, `HANDOFF.md`,
S-P1/S-P2 convergence, audits, `RESULTS.md`, and `REDRESS.md`.

## Findings

### CH1-1 - Exact CSS L4 admission row is not consistently bound

The user-pin handoff requires S-P3 to name the exact CSS L4 row, output plane,
generated Track 1 path, fixture/input source, oracle/Track 2, lightningcss
comparator, equality command, benchmark command, gate command, and rollback
slice (`HANDOFF.md:111-118`). P3-A fixes C1 to
`css_l4/declaration_values/direct_to_struct/main`
(`p3a-candidate-shortlist.md:55-58`), but P3-C allows either
`direct_to_struct` or `real_typed_struct`
(`p3c-falsifiability-gates.md:203-209`), while SPEC W1b delegates the selected
CSS corpus/output plane to the later wave plan instead of binding the row in
the S-P3 packet (`SPEC.md:363-378`). DISPATCH likewise names W1b generically
without the exact row or output plane (`DISPATCH-PROMPT.md:72-77`).

This is a correctness issue because the CSS L4 admission bar is same-corpus,
same-output-plane, strict equality against lightningcss (`USER-PIN...md:29-34`;
`HANDOFF.md:113-119`). Leaving the output plane open lets a later plan shift
between direct and typed fact streams without S-P3 re-adjudicating comparator
legality.

Required fold: choose one exact CSS L4 admission row and output plane in SPEC,
DISPATCH, P3-C, and P3-F. If P3-A's
`css_l4/declaration_values/direct_to_struct/main` is authoritative, replace the
`{direct_to_struct|real_typed_struct}` allowance and make W1b's entry gate name
that row, fact stream, generated runtime path, fixture, oracle, lightningcss
harness, equality command, benchmark command, gate command, and rollback slice
or explicitly point to a required plan artifact that is itself part of S-P3
authority.

### CH1-2 - W2 topology is inconsistent across the packet

The pin-aware seed order in `SYNTHESIS.md` places W2
`escape_mask_64` correctness after W1b CSS L4 baseline/comparator
(`SYNTHESIS.md:265-270`). `HANDOFF.md` uses the same seed split
(`HANDOFF.md:135-140`). P3-B also makes W2 depend on W1b having a CSS measured
row or measured CSS baseline failure (`p3b-wave-sequencing.md:72-80`), and
P3-C requires W1b measured/admitted CSS before W2 entry
(`p3c-falsifiability-gates.md:248-253`).

SPEC and DISPATCH disagree: SPEC makes W2 conditional only on W1a
(`SPEC.md:238-245`, `SPEC.md:404-408`), and DISPATCH repeats W2 as
conditional on W1a before SIMD admission (`DISPATCH-PROMPT.md:69-77`). That
allows the SIMD correctness wave to dispatch before the CSS L4 row exists,
contrary to the rest of the S-P3 packet's "CSS first, then correctness unblock"
topology.

Required fold: align W2's entry gate everywhere. CH1 recommends matching
SYNTHESIS/P3-B/P3-C: W2 dispatches after W1b has produced a measured CSS row or
measured CSS redress attempt. If the intended topology is instead W2 after W1a,
then P3-B, P3-C, SYNTHESIS-derived references, and the CSS-first rationale need
to be rewritten to make that exception explicit and not silently reorder the
campaign.

### CH1-3 - P3-B contains wave-label errors that can misroute fallbacks and gates

P3-B's fallback section says Sheets/BBNF-self may enter only after "W2 records a
measured CSS L4 redress attempt" (`p3b-wave-sequencing.md:98-104`), but W2 is
the `escape_mask_64` correctness wave, not the CSS redress wave. The measured
CSS attempt is W1b in the same document (`p3b-wave-sequencing.md:27-31`,
`p3b-wave-sequencing.md:76-77`).

The same artifact has additional mislabels in its falsifiability binding: W1
should be W1a for the GrammarConfig gate (`p3b-wave-sequencing.md:128-135`);
the line saying W2 can legally emit CSS L4 should be W1b
(`p3b-wave-sequencing.md:134-135`); and a W3-sidecar failure is said to
falsify W4 (`p3b-wave-sequencing.md:157-168`). These are not harmless typos in
a dispatch packet: they attach CSS fallback, legality, and union failure
conditions to the wrong wave identifiers.

Required fold: correct all P3-B wave labels, then re-check SPEC and DISPATCH
for the same identifiers. Fallback eligibility must key on a measured W1b CSS
L4 redress result, not W2. GrammarConfig legality must be W1a. CSS emission
must be W1b. Sidecar/union-shape violations must falsify W3.

## Correctness Checks That Passed

- The packet consistently rejects CSS admission by `ceil(baseline_mbps * 1.01)`
  and uses strict `track1_mbps > lightningcss_mbps + 1` as the close bar
  (`SPEC.md:39-42`, `p3c-falsifiability-gates.md:214-231`,
  `p3d-telemetry-schema.md:116-128`).
- Strict equality, same-plane lightningcss evidence, and independent
  oracle/Track 2 are present in the CSS gate shape (`SPEC.md:43-49`,
  `p3d-telemetry-schema.md:118-128`).
- GrammarConfig before CSS emission is correctly represented as W1a and as a
  Lock 14 precondition (`SPEC.md:312-345`, `p3c-falsifiability-gates.md:162-199`).
- `escape_mask_64` is correctly treated as a correctness prerequisite before new
  SIMD/ASM admission (`SPEC.md:393-424`,
  `DISPATCH-PROMPT.md:147-158`).
- Union and ASM-gen are reopened only with material differential, CHALLENGE,
  same-wave consumer, and REDRESS evidence (`SPEC.md:565-575`,
  `p3e-preblocked-ledger.md:12-23`).
- Zero production aarch64 orphans are correctly required for ADMIT/FIXPOINT
  close (`SPEC.md:57-60`, `p3d-telemetry-schema.md:239-255`).

## Required Folds Before Acceptance

1. Bind one exact CSS L4 admission row/output plane across P3-A, P3-C, P3-F,
   SPEC, and DISPATCH.
2. Align W2 topology across SYNTHESIS/HANDOFF-derived seed order, P3-B, P3-C,
   SPEC, and DISPATCH.
3. Fix P3-B's wave-label drift for fallback eligibility, W1a legality, W1b CSS
   emission, and W3 union failure.

After those folds, CH1 expects the packet to be close to ACCEPT on correctness.
