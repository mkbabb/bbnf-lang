# SK-V13 S-P2 V2 CH1: Correctness

Verdict: REVISE.

## Evidence

- The CH1 bar remains the S-P2 contract: every candidate primitive must trace to a
  named S-P1 hot leaf, comparator claims must use the correct comparator and
  strictness plane, and ISA claims must cite architecture authority
  (`restart/prompts/skinny/PASS-2-RESEARCH.md:95`-`:100`). S-P1 is valid input
  because V5 converged and explicitly keeps profile facts non-admissive until
  S-P2/S-P3 selection (`restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md:10`-`:15`,
  `:53`-`:61`). The canonical ledger names the available hot-leaf families:
  JSON direct envelopes, JSON parse envelopes, `unescape_string`,
  `read_hex_unit_scalar`, `match_tiny_plain_string_with_cap`, structural scan
  probes, JSON typed-only leaves, and CSS timer/fact-sink nonparser overhead
  (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:32`-`:104`).

- V2 resolves the main V1 CH1 blockers. P2-A now states that C7
  `ascii_set_member_find64_css` is a conditional route-production candidate, not
  a standalone P1-grounded parser primitive, and requires a fresh narrow CSS
  parser profile or same-wave CSS scan-block measurement with strict
  lightningcss equality (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:89`-`:94`).
  P2-B gives the same weaker antecedent for B2 and preserves REDRESS-126's
  route-production boundary (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:64`-`:76`).
  P2-F maps C7/B2/P2E-6/C-P2C-1 as `CONDITIONAL-GRAMMAR-NEUTRAL`
  route-production because CSS P1 is not yet a parser-hot-leaf proof
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:114`).

- V2 also resolves the D1 and orphan-inventory problems from V1. P2-D marks
  lazy tape capacity policy `NOT-S-P3-ELIGIBLE` as a standalone behavior wave,
  naming it a measurement question unless later micro-proof names the exact row
  and hot leaf it moves (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:146`-`:185`).
  P2-C stamps EOR3 and standalone `byte_context` as `NOT-S-P3-ELIGIBLE`, keeps
  `cache_hints` non-selectable without a later store/prefetch hot leaf, and
  tightens the S-P3 citation guidance for CSSC CTZ, PMULL, UDOT, TBL/TBX, EXT,
  and EOR3 (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:24`,
  `:30`-`:43`, `:65`, `:75`-`:93`). P2-F carries the same non-shortlist rule
  for EOR3/BCAX, cache hints, standalone prefix/next/bulk bitmap primitives,
  standalone `byte_context`, and standalone D1 (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:153`-`:157`).

- Comparator and strictness claims are correct for CH1. P2-A limits the binding
  JSON admission comparator to same-plane sonic-rs strict, treats simdjson,
  yyjson, and asmjson as architecture pressure unless same-run same-plane
  sidecars are wired, and explicitly rejects asmjson as a strict anchor for this
  host/scope because the located source is permissive (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:42`-`:59`,
  `:140`-`:143`). CSS is carried as strict same-plane lightningcss/cssparser
  equality, not as a JSON comparator (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:17`-`:21`,
  `:144`-`:148`).

- ISA claims no longer overclaim at the V1 level. P2-C narrows Arm claims to
  named ACLE feature macros and Neon intrinsic entries: `__ARM_FEATURE_CSSC`
  for CTZ availability, `vmull_p64` / `vmull_high_p64` for PMULL,
  `__ARM_FEATURE_DOTPROD` and `vdotq_u32` for UDOT, `vqtbl4q_u8` /
  `vqtbx4q_u8` for TBL/TBX, `vextq_u8` for EXT, and `__ARM_FEATURE_SHA3` /
  `veor3q_u8` for EOR3 (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:16`-`:24`,
  `:69`-`:93`). It also states that x86 is background only for SK-V13 and should
  not become implementation or benchmark scope (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:26`).

- The dav1d citation gap is resolved for CH1/CH6 purposes. P2-B now bounds the
  lineage claim to cited FFmpeg/VideoLAN checkasm process authority and says no
  dav1d-specific implementation or gate text may be copied into S-P3 without
  exact dav1d source-file anchors (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:12`-`:25`,
  `:143`-`:148`).

## Blockers / Fold Requirements

- P2-F still exposes non-hot-leaf CSS row/fact-stream work as immediate S-P3
  eligibility without a CH1 route stamp. Rows such as CSS stylesheet/selector
  facts, CSS visual functions, at-rules/media, nesting, and vendor/custom
  at-rule taxonomy cite missing CSS coverage, CSS feature gaps, or no P1 hot
  leaf, yet the table marks several as `ADMISSIBLE-GRAMMAR-NEUTRAL` or
  `CONDITIONAL-GRAMMAR-NEUTRAL` and the eligibility section says "CSS rows 1-6"
  are eligible (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:65`-`:70`,
  `:146`-`:152`). CH1 cannot accept those as candidate primitive authority
  because S-P1's only CSS profile remains timer/fact-sink dominated with parser
  hot leaf unresolved (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:100`-`:104`;
  `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md:55`-`:58`).

- Fold requirement: P2-F must either reclassify CSS rows 1-6 as non-primitive row
  production scopes outside CH1 primitive admission, or stamp each missing-hot-leaf
  CSS entry as conditional route-production with the same requirement already
  used for C7/B2/P2E-6: fresh narrow CSS parser profile or same-wave strict
  lightningcss/cssparser row movement. Until then, S-P3 must not read
  `ADMISSIBLE-GRAMMAR-NEUTRAL` on those rows as CH1 primitive eligibility.

## Disposition

V2 is close but not converged under CH1. The specific V1 fold actions for C7,
D1, orphan SIMD inventory, dav1d lineage, comparator strictness, and Arm ISA
overclaim are resolved. The remaining blocker is narrower: P2-F still lets
missing-CSS-hot-leaf row scopes appear in the immediate eligibility set without a
CH1 route-production or non-primitive stamp. Revise only that disposition surface;
do not reopen the accepted comparator, ISA, C7, D1, or orphan-inventory folds.
