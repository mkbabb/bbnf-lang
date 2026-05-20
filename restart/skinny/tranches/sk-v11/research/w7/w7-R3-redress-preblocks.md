# SK-V11 W7 R3 - REDRESS And Pre-Block Ledger

Pass: W7 Phase 1 research.
Agent: R3 REDRESS / pre-block ledger.
Date: 2026-05-20.
Scope: SPEC Section 11 output digest/hash host sink, C8 only.
Output: this file only.

## Verdict

W7 may still dispatch, but only as an output-plane sink/oracle wave. It may not
reuse W6's rejected `JsonDigestSink::*_source` decoded-byte fold, may not
change parser semantics, and may not turn digest/hash facts into generic parser
vocabulary.

The admissible W7 surface is narrow:

- a per-product host sink specialization in the SPEC Section 11 owner set;
- an output oracle/report/gate refinement for an existing product output plane;
- a non-JSON host-sink oracle only if prior W1b authority supplies a generated
  non-JSON baseline and same-plane oracle, which REDRESS 113 currently blocks;
- row movement only when fresh post-W6 profile evidence names digest/hash as a
  limiting hot leaf for the selected product-row subset.

Absent that fresh limiting profile, W7 should record BLOCKED before behavior
redress. C8 is not parser vocabulary.

## Binding Sources

- SPEC Section 11 (`restart/skinny/tranches/sk-v11/SPEC.md:648-692`):
  W7 candidate is C8 output digest/hash oracle or per-product host sink only.
  Entry requires W3-W6 dispositions plus CHALLENGE acceptance that digest/hash
  is an observed limiting hot leaf, with exact scalar fold/mix source, output
  plane, and independent oracle named. Exit requires selected direct rows to
  clear Section 0.4 floors on Track 1 and Track 2/oracle, or selected non-JSON
  host sink to improve at least 1.0% with strict output equality. Digest/hash
  state may not enter generic parser crates as parser semantics.
- P3-E ledger (`research/p3/p3e-preblocked-ledger.md:12`, `:41`, `:56`,
  `:196-200`): C8 is output oracle or product host sink only; it cannot become
  parser vocabulary, generic hash fact, hidden sidecar, or shared Track 1 ==
  Track 2 parser. Direct movement follows the REDRESS 100/101/109 contract.
- W6 redress (`research/w6/redress/w6-redress-entry-blocked.md:18-55`) and
  REDRESS 117 (`skinny/REDRESS.md:3436-3460`): W6 admits no escaped-segment
  primitive, x4 production consumer, source-method digest fold, non-JSON proof,
  or reusable scalar oracle. W7 carries REDRESS 54/55/66/69 plus 64/82/107/108,
  REDRESS 113, 116, and 117 forward.

## What W7 May Still Do

1. Product host sink specialization.

   W7 may specialize only a selected output sink owned by Section 11:
   `bbnf-bench/src/direct_struct.rs`, `generated_real_typed.rs`, report/gate
   code, JSON parity tests, selected non-JSON oracle/report files if W1b
   supplies baseline authority, `RESULTS.md` on measured PASS, and
   `REDRESS.md`. The implementation must leave parser control, generated
   runtime semantics, BIR/directives, `BackendShape`, generic crates, and
   substrate structure unchanged.

2. Output oracle/report refinement.

   W7 may add or tighten report/gate checks that consume product-output evidence
   in the same wave. The check must name `wave_id`, `redress_entry`, selected
   row, output plane, strict comparators, Track 2/oracle independence, floor,
   and same-wave consumer class. It cannot move a row by gate-only relabeling.

3. A new consumer representation.

   The only possible C8 source delta after REDRESS 54/55/69 is a representation
   change away from the current direct digest length/fingerprint streaming
   workload. A viable plan must show that the selected product sink consumes a
   different output representation or field-specific access pattern, then prove
   bit-exact equality against an independent oracle. Merely hashing decoded
   bytes earlier, in chunks, in one pass, or inside `JsonDigestSink` is blocked.

4. Track 2/oracle work.

   W7 may create an independent oracle for the selected product output, but the
   oracle cannot share generated parser control or the same hidden parser body.
   REDRESS 34/35/48 make the generated Track 1 versus independent Track 2
   boundary load-bearing. Track 2 may share a scalar output primitive only if it
   remains parser-independent and a stricter external oracle proves the output
   fields.

5. Non-JSON host sink only under unresolved precondition.

   SPEC Section 11 allows selected non-JSON oracle/report files if W1b uses
   digest output. REDRESS 113 says W1b did not create the generated non-JSON
   baseline, so W7 cannot invent the first non-JSON generated row under C8. A
   W7 non-JSON host-sink path is blocked unless a later governance revision
   first supplies generated Track 1 plus independent same-plane oracle authority.

## Hard Pre-Blocks

- Parser semantics through digest/hash. W7 cannot make digest, hash,
  semantic length, fingerprint, or string facts part of parser semantics, BIR,
  directives, `BackendShape`, runtime grammar contracts, generic crates, or a
  retained/direct substrate.
- Hidden Track 1 == Track 2 coupling. REDRESS 34 found the old direct workload
  dishonest because both tracks used the same bench-private parser. REDRESS 35
  and 48 close the honesty route by requiring generated Track 1 from lowered BIR
  and structurally separate Track 2/oracle evidence.
- Current direct digest decoded-byte/hash family. REDRESS 54 rejected
  sink-local exact decoded stats/hash; REDRESS 55 rejected quote-source fused
  streaming hash; REDRESS 66 rejected direct source-hook field-layout
  materialization; REDRESS 69 rejected semantic string fact hashing for the
  current direct digest workload. REDRESS 117 applies that history directly to
  W6's proposed `JsonDigestSink::*_source` decoded-byte fold.
- Parser-owned decoded scratch or materialization rewrites. REDRESS 67 rejected
  parser-owned reusable decoded scratch; REDRESS 68 rejected byte-output
  `unescape_json_string` materialization under the current `Cow<str>` API.
- Scalar parent fold as direct proof. REDRESS 93 rejected the hand Track 2
  scalar-parent fold candidate and kept digest evidence guard-plane only unless
  a later wave supplies checked gate, full maintain measurement, and an
  independent Track 2 arithmetic backstop.
- Cache hints / prefetch as proof. SPEC Section 11 and P3-E block PRFM/STNP or
  cache-hint-only changes as row movers. They are inventory unless fresh
  output-sink hot-leaf evidence plus row movement exists.
- Non-JSON close claim. REDRESS 113 blocks the non-JSON generated baseline.
  W7 may carry the block; it may not claim grammar-generalization closure.
- W5/W6 reusable proof. REDRESS 116 admits no span API or reusable scalar proof.
  REDRESS 117 admits no escaped-segment primitive, x4 production consumer,
  source-method digest fold, or reusable scalar oracle.
- Gate-only admission. REDRESS 100/101/109 require direct row movement to be
  strict measured-row evidence with generated Track 1, independent Track 2 or
  oracle, same-run sonic/serde comparator evidence, same-wave gate consumption,
  and guard floors. W7 cannot relabel a residual row from stale data.

## Material Differentials A W7 Plan Must Clear

1. Differential from REDRESS 54/55/66/69.

   The plan must prove it is not another sink-local decoded stats/hash route for
   the existing `JsonDirectDigest` length/fingerprint contract. It must name the
   new product representation or host-sink output shape, why the consumer is
   materially different, and why the cost mechanism is not simply "avoid
   allocation then hash decoded bytes."

2. Differential from REDRESS 34/35/48.

   The plan must prove Track 1 is generated output-plane work and Track 2/oracle
   is structurally independent. If a helper is shared, it must be output-only and
   backed by an external oracle that can catch helper bugs. No shared hidden
   parser, generated helper reuse, or benchmark-private parser can be the proof.

3. Differential from REDRESS 93.

   If W7 touches parent/child digest folding or scalar aggregation, it must avoid
   the hand Track 2 scalar-parent fold replay. It needs a W7-aware gate/report
   validator, full direct and typed guard measurement, and independent arithmetic
   oracle evidence before row movement.

4. Differential from REDRESS 100/101/109.

   Direct row admission must follow the existing direct contract: both generated
   Track 1 and independent Track 2/oracle clear the fixed floor in the same
   native run family; strict direct comparators are present; guard floors hold;
   report and `gate-json` consume the W7 provenance in the same wave.

5. Differential from REDRESS 113.

   Any non-JSON host sink must not be the first generated non-JSON baseline.
   Without a prior generated non-JSON Track 1 plus independent same-plane oracle,
   W7 can only record non-JSON as blocked background.

6. Differential from REDRESS 116/117.

   W7 cannot inherit W5 span proof or W6 escaped-segment proof. Any plan must
   start from fresh output-sink evidence, not from a rejected span/escape route,
   and must explicitly exclude `JsonDigestSink::*_source` decoded-byte folding
   as PASS evidence.

## Required Entry Evidence For Plan

A W7 plan should not reach redress unless it records:

- post-W6 `xctrace`/`samply` evidence naming digest/hash or a concrete host
  sink function as a limiting hot leaf for each selected row;
- exact owner paths inside SPEC Section 11 only;
- scalar fold/mix or output-oracle source, with a bit-exact equality contract;
- selected row floors, comparator source, direct/typed guard floors, and run
  family;
- a same-wave `gate.rs`/`report.rs` consumption plan for W7 provenance;
- a revert protocol that preserves `/tmp/skv11-waveW7-rejected.patch` and
  moves no `RESULTS.md` row unless all floors, parity, profile, and consumer
  proofs pass.

## Research Conclusion

W7 is not blocked by dispatch status, but most obvious digest/hash patches are
pre-blocked. The only admissible plan is a fresh, profile-backed product
host-sink/oracle intervention with a different output representation from the
current direct digest decoded length/fingerprint path, strict Track 1 and
Track 2/oracle independence, and same-wave gate consumption. If research cannot
name that representation and hot-leaf evidence, W7 should be blocked before
source redress rather than replay REDRESS 54/55/66/69 through C8.
