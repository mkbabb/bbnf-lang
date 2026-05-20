# SK-V12 S-P2 PIN-V2 CH1 Correctness Challenge

Pass: S-P2 Research CHALLENGE. Cycle: PIN-V2.
Lens: CH1 correctness.
Date: 2026-05-20.
Scope: folded Cycle V2 S-P2 packet after commit `31859478`.

## Verdict

ACCEPT.

Score: 96%.

## Blocking Findings

None.

## Findings

1. The folded packet is anchored to the user pin's strict CSS L4 comparator
   plane. P2-A states that CSS L4, not JSON parse-only, is the admission
   comparator and repeats the `lightningcss_mbps + 1`, same corpus, same output
   plane, same host, and strict-equality requirements
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:14`).
   P2-B carries the same strict comparator rule in its admission process
   (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:36`,
   `:90`). P2-D and P2-E reject JSON row movement as CSS proof and route CSS
   admission through generated Track 1 plus same-plane lightningcss/oracle
   evidence (`p2d-substrate-tape.md:71`, `:108`; `p2e-parse-that-gaps.md:12`,
   `:68`). P2-F preserves the CSS-first order and fallback boundary
   (`p2f-grammar-neutral.md:14`, `:65`, `:81`).

2. Selectable candidates trace to pin S-P1 hot-family antecedents. The six P2
   artifacts name the same ten accepted hot families in frontmatter, and P2-C's
   selectable set is limited to C1, C3, C4, C5, and C6 with explicit P1
   antecedents (`p2c-arch-esoterica.md:43`-`:59`). P2-A's C1-C7 table names a
   P1 antecedent for each comparator-derived candidate (`p2a-sota-teardown.md:27`-`:37`).
   P2-E maps parser candidates and output-plane rows to the same hot-family
   surface while treating JSON evidence as nomination-only (`p2e-parse-that-gaps.md:12`,
   `:31`-`:42`).

3. Nonselectable inventory is no longer treated as candidate authority. P2-C
   explicitly says only C1, C3, C4, C5, and C6 are selectable and classifies C2,
   C9, and C11 as inventory/drop while C7, C8, C10, and C12 are support-only or
   conditional inventory (`p2c-arch-esoterica.md:43`-`:59`). P2-F generalizes
   that boundary: rows marked `Inventory/drop`, `Support inventory`,
   `Support-only`, `Diagnostic-only`, or `Parser-candidate-ineligible` are
   outside the current S-P3 candidate pool unless a later folded pass adds fresh
   P1 evidence, scalar oracle, micro-proof, and same-wave consumer
   (`p2f-grammar-neutral.md:28`).

4. The stale pre-pin S-P2 hardening authority is demoted, not controlling.
   `HARDENING-S-P2-CONVERGED.md` now states that pre-pin convergence is
   superseded, records PIN-V1 CH1/CH4 REVISE, describes the folded V2 surface,
   and says S-P2 is not converged under the user pin until two consecutive clean
   challenge cycles after the last REVISE reset
   (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:1`-`:42`).

5. The corrected external anchors resolve. The old yyjson `#L271-L279` and
   dav1d `#L1127-L1227` anchors are absent from the active V2 packet. The
   current yyjson README lines 10-15 contain the ANSI C / no explicit SIMD /
   strict feature list cited by P2-A. The current dav1d `msac.c` ranges
   115-136, 156-175, 184-195, 203-215, and 228-250 contain cloned C/ASM state,
   `call_ref`, `call_new`, state/CDF comparison, and `bench_new`, matching P2-B.
   All active external URLs in P2-A/P2-B/P2-C returned HTTP 200 in the resolution
   sweep.

6. ISA claims are no longer unanchored speculation. P2-C keeps ARMv9.2/NEON
   surfaces tied to local code and official Arm ACLE references with section
   labels for TBL/TBX, UDOT/DotProd, shift/extract, LD4, and PMULL/CSSC/SHA3
   (`p2c-arch-esoterica.md:8`-`:13`, `:211`-`:219`). Inventory-only ISA routes
   lacking a current P1 expression are labeled inventory/drop or support-only,
   so they do not become S-P3 authority.

## Nonblocking Notes

- Archived pre-pin/V1-V3 hardening files still contain stale historical counts
  such as "six current AArch64 candidates" and "five parse-that gaps." They are
  archival context only; the current hardening status explicitly supersedes them
  and the active V2 packet does not depend on those counts.
- P2-C's Arm ACLE citations are broad page URLs plus section labels rather than
  stable line anchors. This is acceptable for CH1 because the referenced Arm
  pages are generated reference manuals and the artifact names the exact
  intrinsic families and feature macros, but S-P3 should preserve the section
  labels when carrying any ISA route into a wave plan.

## Fold Edits Required

None.

## Commands Used

```sh
git status --short
git rev-parse --short HEAD
sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md
sed -n '1,340p' restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md
sed -n '1,300p' restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md
sed -n '1,340p' restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md
sed -n '1,320p' restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md
sed -n '1,180p' restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md
sed -n '1,120p' restart/prompts/skinny/PASS-2-RESEARCH.md
sed -n '74,122p' restart/prompts/ORCHESTRATOR.md
rg -n 'Cycle: V2|Cycle: V1|pre-pin|stale|S-P2-CONVERGED|six current|five parse|L271-L279|L1127-L1227|ceil\(baseline|parse_only.*target|Sheets.*first|BBNF-self.*first' restart/skinny/tranches/sk-v12/research/p2 restart/skinny/tranches/sk-v12/research/p2/hardening
rg -n 'lightningcss_mbps \+ 1|CSS L4|parse_only|strict equality|same output plane|same-plane|D3|D4|D5|escape_mask_64|GrammarConfig' restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/HANDOFF.md restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md
rg -n 'P1 hot-leaf antecedents|Selectable candidate|Inventory/drop|Support-only|Support inventory|Diagnostic only|parser-candidate|No P2-D primitive|candidate-eligible|outside the current S-P3 candidate pool|lightningcss_mbps \+ 1|strict equality|same output plane' restart/skinny/tranches/sk-v12/research/p2/p2*.md
rg --no-filename -o 'https?://[^ )]+' restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md | sed 's/[,.]$//' | sort -u | while IFS= read -r url; do code=$(curl -L -s -o /dev/null -w '%{http_code}' --max-time 15 "$url"); printf '%s\t%s\n' "$code" "$url"; done
curl -L -s https://raw.githubusercontent.com/ibireme/yyjson/master/README.md | sed -n '10,15p'
curl -L -s https://raw.githubusercontent.com/videolan/dav1d/master/tests/checkasm/msac.c | awk 'NR>=115 && NR<=136 {print NR ":" $0} NR>=156 && NR<=175 {print NR ":" $0} NR>=184 && NR<=195 {print NR ":" $0} NR>=203 && NR<=215 {print NR ":" $0} NR>=228 && NR<=250 {print NR ":" $0}'
```
