# SK-V13 S-P2 V4 CH1: Correctness Confirmation

Verdict: ACCEPT.

## Evidence

- V4 confirms the V3 acceptance against the unchanged S-P2 packet. V2's sole
  CH1 blocker was narrow: P2-F still exposed CSS rows 1-6 as immediate S-P3
  eligibility despite CSS profiling being timer/fact-sink dominated rather than
  parser-hot-leaf proof
  (`restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:12`-`:19`).
  V3 records that this blocker was resolved and that V4 is the required
  confirmation cycle
  (`restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:10`-`:17`).

- P2-F now makes the CSS row boundary explicit in the verdict vocabulary:
  `CSS-ROW-SCOPE-CONDITIONAL` means generated CSS parity row/fact-stream scope,
  not primitive admission; S-P3 may plan it only with fresh narrow CSS parser
  profiling or same-wave strict lightningcss/cssparser row movement
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:35`-`:47`).
  The six CSS row scopes are each stamped with that verdict and each names a
  strict row consumer rather than a primitive admission path
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:67`-`:74`).

- P2-F repeats the same constraint in carry-forward form: CSS L4 row scopes are
  row-production work with strict lightningcss/cssparser equality and either a
  fresh narrow CSS parser profile or same-wave row movement; they are not CH1
  primitive hot-leaf evidence
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:123`-`:132`).
  The S-P3 carry-forward list keeps CSS rows 1-6 separate from eligible
  primitive/refactor families and requires scalar/checkasm/consumer gates for
  any SIMD used inside a CSS row
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:151`-`:166`).

- Comparator strictness remains correct. P2-A binds JSON admission to sonic-rs
  strict on the same plane by at least 1 Mbps, keeps CSS admission on strict
  lightningcss equality, and treats simdjson/yyjson/asmjson as architecture
  pressure unless same-run same-plane sidecars are produced
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:17`-`:21`,
  `:42`-`:59`). C7/B2/P2E-6 still requires strict lightningcss equality and
  same-wave CSS scan-block row movement, not another microbench-only helper
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:89`-`:94`;
  `restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:64`-`:76`;
  `restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md:90`-`:100`).

- Named hot-leaf evidence remains bounded. P2-A and P2-F both treat S-P1
  profile data as candidate evidence, not admission; the CSS profile is still
  described as timer/fact-sink dominated with parser hot leaf unresolved
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:23`-`:40`;
  `restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:20`-`:27`).
  The row scopes therefore cannot be read as primitive admissions merely because
  they are grammar-neutral row templates.

- ISA and checkasm claims stay accurate. P2-C narrows PMULL, CSSC CTZ, UDOT,
  TBL/TBX, EXT, and EOR3 to named Arm ACLE / Neon feature and intrinsic anchors,
  keeps x86 out of scope, and marks EOR3 inventory-only until a named
  three-input hot expression exists
  (`restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md:16`-`:26`,
  `:69`-`:93`). P2-B bounds dav1d to lineage context and gives S-P3 the usable
  checkasm process through FFmpeg/VideoLAN plus local strict-mode requirements,
  not unverified dav1d file-level gate authority
  (`restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md:12`-`:31`,
  `:137`-`:148`).

- The accepted V2 exclusions remain intact: D1 is not standalone S-P3 eligible,
  support-only SIMD/orphan inventory cannot be promoted without same-wave
  consumer evidence, and union/structural routes must cite REDRESS 88/89/96/97/98
  with a material differential
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:89`-`:95`,
  `:174`-`:202`; `restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md:112`-`:121`,
  `:144`-`:146`).

## Blockers / Fold Requirements

None for CH1.

Carry-forward requirements:

- S-P3 must preserve CSS rows 1-6 as conditional row-production scopes, not
  primitive eligibility.
- Any CSS row plan must name fresh narrow CSS parser profiling or same-wave
  strict lightningcss/cssparser row movement.
- Any SIMD primitive inside a CSS row must carry scalar reference,
  checkasm/parity, same-wave consumer wiring, REDRESS material-differential
  citation, and zero-orphan disposition.
- C7/B2/P2E-6 remains route-production only; the SK-V12 microbench is not a row
  admission and not proof that the current CSS profile isolated a parser hot
  leaf.

## Disposition

CH1 confirms V3 acceptance for V4. The unchanged V3 packet preserves
correctness, comparator strictness, named hot-leaf boundaries, Arm ISA accuracy,
and the CSS row-scope demotion that resolved the V2 blocker. No CH1 revise is
required.
