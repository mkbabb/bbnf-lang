# SK-V13 S-P2 V1 CH1: Correctness

Verdict: REVISE.

## Evidence

- The CH1 contract is strict: every candidate primitive must trace to a named
  S-P1 hot leaf, comparator claims must cite the correct comparator and
  strictness plane, and ISA claims must cite architecture references
  (`restart/prompts/skinny/PASS-2-RESEARCH.md:95`-`:100`). S-P1 is a valid
  input because V5 closed `G-S-P1-CONVERGED` and authorized S-P2
  (`restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md:10`-`:15`).

- Most row-moving candidates are correctly grounded. The P1 ledger names the
  direct envelopes, `parse_that_regex::unescape_string`, parse-only
  `dispatch_value` / `match_tiny_plain_string_with_cap` /
  `read_hex_unit_scalar`, typed-only leaves, mode-III `scan_tail` /
  `scan_structurals`, and CSS timer/fact-sink status
  (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:32`-`:104`).
  P2-A's C1-C6/C8, P2-B's B1/B3/B4, P2-D's D2-D5, and P2-E's P2E-1..5/P2E-8
  each cite one of those antecedent families in their candidate bodies
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:72`-`:81`;
  `restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:37`-`:91`;
  `restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:183`-`:347`;
  `restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md:30`-`:83`,
  `:107`-`:116`).

- Comparator strictness is mostly correct. P2-A limits the binding JSON SOTA
  comparator to same-plane sonic-rs strict, treats asmjson/simdjson/yyjson as
  architecture pressure unless same-run sidecars are wired, and preserves the
  `RESULTS.md` warning that C++ sidecars are historical or absent
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:42`-`:59`,
  `:133`-`:136`; `skinny/RESULTS.md:149`). The CSS row is also cited as
  strict same-plane lightningcss/cssparser equality, not a JSON comparator
  (`skinny/RESULTS.md:94`, `:146`-`:148`).

- The AArch64 ISA claims in P2-C are sourced against primary Arm references:
  CSSC includes CTZ, DotProd gates UDOT, SHA3 gates EOR3, and the Neon
  reference maps `vqtbl4q_u8`, `vqtbx4q_u8`, `vdotq_u32`, `vmull_p64`, and
  `veor3q_u8` to the stated A64 instructions
  (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:16`-`:24`,
  `:64`-`:68`). I found no CH1 contradiction in the ISA/source claims.

## Blockers / Fold Requirements

- Inventory-only candidates remain in candidate tables. P2-C lists
  `eor3_string_mask_fusion` while stating there is no current S-P1
  three-input hot expression, and lists `byte_context_orphan_resolution` as
  close hygiene with no P1 production caller
  (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:24`,
  `:37`-`:38`). P2-F similarly lists `cache_hints`, EOR3/BCAX, and
  LD4/TBX/SMIN/SMAX as inventory/no-antecedent rows while later saying they are
  not eligible without new evidence (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:89`-`:91`,
  `:139`-`:141`). V2 must move these to non-candidates or stamp them
  `NOT-S-P3-ELIGIBLE` so S-P3 cannot shortlist speculative kernels.

- The CSS ASCII set run-skip route is useful but not yet a named S-P1 parser
  hot leaf. S-P1 classifies the only CSS profile as timer/fact-sink dominated
  with parser hot leaf unresolved (`restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md:100`-`:104`;
  `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:82`-`:87`).
  P2-B correctly admits B2 is justified by SK-V12 W4 microbench plus CSS
  scanner need, not by S-P1 self-time (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:51`-`:56`).
  V2 must either require a fresh narrow CSS parser profile before S-P3
  shortlist, or classify B2 / C-P2C-1 / P2E-6 / P2-A C7 as conditional
  route-production candidates rather than CH1-grounded primitives.

- P2-D D1 lazy tape capacity policy is grounded in `RESULTS.md` ratios and
  legal substrate reasoning, but not in a named S-P1 hot leaf. P2-D itself says
  current rows are dominated by dispatch envelopes, unicode/string decode, or
  output-plane work rather than payload/tape writers
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:53`-`:60`)
  and marks D1 as needing micro-proof before behavior-wave treatment
  (`restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md:179`-`:181`).
  V2 must demote D1 to a measurement question/non-candidate or name the exact
  S-P1 hot leaf and row it is expected to move.

- P2-B has a source-citation gap for dav1d-specific claims. It makes a
  dav1d/FFmpeg/VLC process lineage claim in findings
  (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:12`), but
  later states no specific dav1d source-file URL was verified
  (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:130`).
  V2 must add exact dav1d source anchors or narrow the claim to the cited
  FFmpeg/VideoLAN checkasm sources.

## Disposition

CH1 does not reject the candidate pool: the central string, unicode, numeric,
dispatch, structural, sink, and resolver candidates are grounded enough for a
fold cycle. It does require V2 cleanup so speculative inventory, weak CSS
microbench carryover, and uncited dav1d lineage do not enter S-P3 as accepted
candidate authority.
