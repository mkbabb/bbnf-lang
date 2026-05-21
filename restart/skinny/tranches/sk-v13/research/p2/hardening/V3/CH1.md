# SK-V13 S-P2 V3 CH1: Correctness

Verdict: ACCEPT.

## Evidence

- The CH1 bar remains unchanged: every candidate primitive must trace to a named
  S-P1 hot leaf, SOTA claims must cite the correct comparator and strictness
  plane, and ISA claims must cite architecture authority
  (`restart/prompts/skinny/PASS-2-RESEARCH.md:95`-`:100`). The S-P1 input
  remains non-admissive profile evidence, not primitive admission:
  S-P1 V5 explicitly carries CSS profiling as timer/fact-sink dominated and
  keeps every profile candidate non-admitted until S-P2/S-P3 selection
  (`restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md:53`-`:61`),
  and the evidence ledger marks all rows `profile_signal_not_gate_admission`
  while naming the only CSS profile as nonparser overhead with parser hot leaf
  unresolved
  (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:20`-`:25`,
  `:100`-`:104`).

- V3 resolves the V2 CH1 blocker. P2-F adds `CSS-ROW-SCOPE-CONDITIONAL` to the
  verdict vocabulary and defines it as a generated CSS row/fact-stream scope,
  not a primitive admission; S-P3 may plan such rows only with a fresh narrow CSS
  parser profile or same-wave strict lightningcss/cssparser row movement
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:35`-`:47`).
  The six previously ambiguous CSS rows are now all stamped
  `CSS-ROW-SCOPE-CONDITIONAL`, not primitive eligibility: stylesheet/selector
  facts, declaration-value extension, visual functions, at-rules/media, nesting,
  and vendor/custom at-rule taxonomy all require the same fresh narrow CSS
  parser profile or same-wave strict row movement gate
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:67`-`:74`).

- P2-F repeats the row-scope boundary outside the table, so S-P3 has no
  reasonable path to read CSS rows 1-6 as immediate primitive authority. The
  grammar-neutrality section says CSS L4 row scopes are row-production work with
  strict lightningcss/cssparser equality plus fresh narrow CSS parser profile or
  same-wave row movement, and are not CH1 primitive hot-leaf evidence
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:121`-`:132`).
  The carry-forward list also separates eligible primitive/refactor families
  from "Conditional row-production scopes, not primitive eligibility: CSS rows
  1-6" and reiterates the narrow-profile / strict-row-movement gate
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:151`-`:161`).
  The V3 cross-read disposition explicitly names the same six CSS row scopes as
  row-production scopes rather than primitive hot-leaf admissions
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:238`-`:245`).

- The accepted V2 C7/B2/P2E-6 boundary did not regress. P2-A still says C7 is a
  conditional route-production candidate rather than a standalone P1-grounded
  parser primitive, requiring a fresh narrow CSS parser profile or same-wave CSS
  scan-block measurement with strict lightningcss equality
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:89`-`:94`).
  P2-B still gives B2 only a weaker antecedent from the SK-V12 W4 microbench plus
  CSS scanner need, not from S-P1 CSS self-time, and requires a named CSS
  generated scanner consumer with strict equality
  (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:64`-`:76`).
  P2-F's C7 mapping remains `CONDITIONAL-GRAMMAR-NEUTRAL route-production` with
  the same fresh CSS profile or same-wave scan-block row-movement requirement
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:108`-`:119`).

- The accepted V2 D1 and orphan-inventory boundaries did not regress. P2-D still
  marks lazy tape capacity policy `NOT-S-P3-ELIGIBLE` as a standalone behavior
  wave unless a later row-moving consumer names the exact row and hot leaf it
  moves (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:146`-`:185`).
  P2-C keeps EOR3 and standalone `byte_context` out of S-P3 eligibility without
  new hot-expression or consumer evidence, and preserves the inventory-only
  treatment for unsupported SIMD/refinement routes
  (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:24`-`:33`,
  `:42`-`:43`, `:75`-`:93`). P2-F carries the same S-P3 exclusion list for
  EOR3/BCAX, cache hints, standalone prefix/next/bulk bitmap primitives,
  standalone `byte_context`, and standalone D1
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:162`-`:166`,
  `:255`-`:260`).

- Comparator, ISA, dav1d lineage, REDRESS, and Lock 1 boundaries accepted in V2
  remain intact. P2-A continues to bind JSON admission to strict same-plane
  sonic-rs while treating simdjson/yyjson/asmjson as architecture pressure
  unless same-run same-plane sidecars are produced, and keeps CSS on strict
  lightningcss/cssparser equality (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:42`-`:59`,
  `:140`-`:148`). P2-C still narrows Arm claims to named ACLE/Neon feature and
  intrinsic anchors and keeps x86 as background only
  (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:16`-`:26`,
  `:69`-`:93`). P2-B still bounds dav1d to lineage context and forbids
  dav1d-specific gate text without future exact dav1d source anchors
  (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:12`-`:25`,
  `:143`-`:148`). P2-D still rejects sidecar vectors, parser-owned cursors,
  aux density tables, `UnionTape`, and parallel structural substrates
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:134`-`:137`,
  `:353`-`:363`, `:428`-`:434`).

## Blockers / Fold Requirements

None for CH1.

Carry-forward requirements for S-P3 remain binding:

- CSS rows 1-6 are conditional row-production scopes, not primitive eligibility.
- Any CSS row plan needs fresh narrow CSS parser profiling or same-wave strict
  lightningcss/cssparser row movement.
- Any SIMD primitive used inside a CSS row inherits scalar reference,
  checkasm/parity, same-wave consumer, REDRESS material-differential, and
  zero-orphan requirements.
- S-P3 must continue to read the `a64_ascii_set_run_skip` family through the
  C7/B2/P2E-6 route-production gate, not as proof that the existing CSS P1 sample
  isolated a parser hot leaf.

## Disposition

V3 resolves the V2 CH1 blocker without reopening the V2 accepted folds. CH1
accepts S-P2 V3 on correctness. This is the first accepted CH1 cycle after the
V2 revise; a later confirmation cycle is still required by the pass convergence
rule if the full V3 challenge set accepts.
