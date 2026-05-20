# SK-V12 S-P2 CHALLENGE V2 — CH6 Anti-Paper-Close

Pass: S-P2 Research CHALLENGE. Cycle: V2.
Date: 2026-05-20.
Lens: CH6 ANTI-PAPER-CLOSE.
Disposition: ACCEPT.

## Scope

This lens re-audits the V2 S-P2 packet for unsupported
"researched/designed" claims, future-wave placeholders, uncited comparator or
ISA claims, broken citations, and candidate claims missing present grounding.
Read set: `restart/prompts/skinny/PASS-2-RESEARCH.md` §3, all six S-P2
artifacts, V1 CH1/CH4/CH6 plus
`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`,
`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`,
`restart/skinny/tranches/sk-v12/HANDOFF.md`, `skinny/RESULTS.md`,
`skinny/REDRESS.md`, and `restart/locks/LOCKS.md`.

## Evidence

1. The V1 P2-A broken-citation defects are fixed. P2-A no longer cites
   out-of-range `ORCHESTRATOR.md:374` or `:474`; its local governance sources
   now cite live lines `restart/prompts/ORCHESTRATOR.md:104`, `:118`, `:197`,
   and `:205` (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:170`).
   The SK-V12 capture manifest and replay paths now point under
   `restart/skinny/tranches/sk-v12/research/p1/` and resolve at the cited
   files (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:179`-`:180`).
   The historical SK-V11 convergence path now includes `research/p2/hardening/`
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:184`).

2. The V1 P2-A candidate-grounding defect is fixed. P2-A §2 now opens with an
   explicit C1-C7 accounting table carrying scalar-reference status,
   checkasm/parity expectation, and same-wave consumer note
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:27`-`:37`),
   followed by scalar sketches for every candidate
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:39`-`:49`).
   The later candidate prose remains explanatory comparator grounding, not a
   substitute for the triad.

3. The V1 P2-D future-diagnostic defect is fixed. P2-D now states current
   selectable candidate count from SK-V12 S-P1 is zero, with three same-tape
   diagnostics and one rejected parallel-substrate route
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:67`-`:72`).
   `retained_cursor_skip_projection` remains listed only as
   diagnostic/ineligible under current S-P1, with no selectable-candidate claim
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:78`-`:83`).

4. P2-C's prior speculative ISA entries are no longer paper candidates. LD4 and
   SHA3/EOR3 are explicitly marked ISA inventory, not current candidates, with
   no sufficient P1 antecedent and no selectable same-wave consumer
   (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:38`-`:42`,
   `:55`-`:65`, `:122`-`:133`). This satisfies CH6 because unsupported ISA
   opportunity text is demoted rather than deferred for S-P3 to repair.

5. The remaining "future/later" wording is refusal or boundary language, not a
   placeholder. Examples: P2-A says numeric/container candidates must be
   re-proven before use (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:97`,
   `:159`); P2-E says the artifact selects no such wave and that any legal
   packet must include a same-wave generated consumer
   (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:147`,
   `:216`, `:278`); P2-C keeps PMULL/CTZ future use blocked unless a distinct
   narrow consumer proves itself (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:152`).
   None of these claims asks a future wave to supply missing evidence for a
   current S-P2 candidate.

6. External and ISA claims remain citable rather than asserted. P2-A's
   comparator claims cite commit-pinned primary sources for asmjson, sonic-rs,
   simdjson, and yyjson (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:186`-`:229`).
   P2-B's checkasm lineage and process claims cite VideoLAN, FFmpeg, and dav1d
   primary sources (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:83`-`:87`).
   P2-C's AArch64 claims cite Arm ACLE/Neon instruction sources
   (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:168`-`:178`).
   P2-F introduces no new external comparator or ISA claims
   (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:21`).

## Residual Guard

This ACCEPT is anti-paper-close only. It does not select a wave and does not
relax the S-P3 requirement that any shortlisted primitive replace the research
sketch with executable scalar reference, strict parity/checkasm where relevant,
and a same-wave generated/runtime consumer before implementation authority.

## Disposition

ACCEPT. The V1 CH6 blockers are folded: citations are followable, unsupported
candidate claims have either been grounded or demoted, diagnostic/future items
are not counted as current candidates, and no remaining S-P2 artifact closes on
an ungrounded future-wave promise.
