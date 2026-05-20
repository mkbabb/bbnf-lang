# SK-V12 S-P2 CHALLENGE V3 - CH1 Correctness

Disposition: ACCEPT.

Lens: CH1 CORRECTNESS.

Scope: convergence check at current commit `6b8be238`. Verify that no
correctness defect was introduced after the V2 all-ACCEPT cycle and that the
V2 CH1 acceptance remains valid over the same folded S-P2 research cohort.

## Basis

- PASS-2 binds CH1 to candidate antecedents, strictness-plane comparator
  sourcing, and ISA reference sourcing
  (`restart/prompts/skinny/PASS-2-RESEARCH.md:95`-`:100`).
- ORCHESTRATOR binds CH1 to resolving file:line / commit / RESULTS / REDRESS
  evidence and measurable gates, and §3Z requires consecutive ACCEPT cycles
  with no open REVISE (`restart/prompts/ORCHESTRATOR.md:81`-`:84`,
  `restart/prompts/ORCHESTRATOR.md:118`-`:120`).
- S-P1 convergence remains the authority for accepted hot-family antecedents
  and the generated non-JSON-first boundary
  (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:41`-`:63`).
- V2 was six-of-six ACCEPT, with CH1 explicitly accepting the folded
  candidate-accounting, comparator, ISA, and diagnostic demotion fixes
  (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:12`-`:21`).
- Drift check: `git diff --name-only f707b555..6b8be238 -- restart/skinny/tranches/sk-v12/research/p2/p2*.md`
  returned no paths. The only intervening p2 commit archived V2 hardening; the
  six folded S-P2 artifacts are unchanged since the V2 fold.

## Evidence

1. The V2 accepted boundary remains intact. V2 defines the S-P2 pool as
   research input, not a wave plan; P2-A C1-C7 are scalar/checkasm/consumer
   guarded shapes, P2-C has six current AArch64 candidates with LD4/SHA3 kept
   as inventory, P2-D has zero selectable tape-substrate candidates, P2-E has
   five parse-that gaps, and P2-F has six conditional parser/support families
   plus oracle/accounting-only families
   (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:23`-`:40`).

2. P2-A still satisfies CH1. Comparator strictness is separated by lane:
   asmjson is architecture/flaw-probe only, sonic-rs unchecked/lossy APIs are
   excluded, yyjson `YYJSON_READ_NOFLAG` is the strict lane, and simdjson's
   retained index is comparator evidence rather than import authority
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:14`-`:21`).
   C1-C7 carry scalar-reference status, checkasm/parity expectation, and
   same-wave consumer notes (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:29`-`:49`),
   and each candidate names accepted S-P1 antecedents before admission claims
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:55`,
   `:67`, `:79`, `:91`, `:103`, `:115`, `:127`). Primary comparator sources
   remain commit-pinned for asmjson, sonic-rs, simdjson, and yyjson
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:186`-`:229`).

3. P2-B still satisfies CH1. Its twelve gates are process/admission gates, not
   speculative kernels; each table entry names P1 antecedent, scalar status,
   strict parity/checkasm expectation, same-wave consumer rule, and admission
   boundary (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:36`-`:51`).
   Its external process citations are primary VideoLAN, FFmpeg, and dav1d
   sources (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:83`-`:87`).

4. P2-C still satisfies CH1. The packet keeps `Current candidate count: 6` and
   marks LD4 interleaved classification plus SHA3 ternary boolean fold as ISA
   inventory because S-P1 does not name an existing interleaved stream or
   three-input boolean-fold expression
   (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:38`-`:42`,
   `:55`-`:65`, `:122`-`:133`). The six current candidates name accepted P1
   antecedents and local scalar/source anchors (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:44`-`:53`,
   `:67`-`:76`, `:78`-`:87`, `:89`-`:98`, `:100`-`:109`, `:111`-`:120`).
   Arm ISA claims cite ACLE / Arm references for the named instructions and
   feature macros (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:168`-`:178`).

5. P2-D still satisfies CH1 by contributing no selectable candidate primitive
   from the current S-P1 surface. It records zero selectable candidates, three
   diagnostics, and one rejected parallel-substrate route
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:67`-`:72`).
   The three same-tape entries are explicitly diagnostic/ineligible, while
   `structural_class_lane_union` is rejected under REDRESS 96/97/98
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:74`-`:83`).

6. P2-E and P2-F remain CH1-correct. P2-E limits parse-that work to five
   candidate-carrying antecedents and keeps the other accepted leaves as
   constraints (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:12`-`:32`);
   its candidates carry scalar sketches, P1 antecedents, checkasm expectations,
   and same-wave consumer notes (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:50`-`:93`,
   `:284`-`:358`). P2-F adds no new external comparator or ISA claims, and its
   F1-F8 table preserves parser/support, oracle-only, and accounting-only
   eligibility boundaries (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:21`-`:40`).

## Revise List

None.

## Disposition

ACCEPT. No CH1 correctness defect was introduced after V2; the folded S-P2
artifacts are unchanged since the V2 fold, V2 all-ACCEPT remains valid, and
there is no open CH1 REVISE item.
