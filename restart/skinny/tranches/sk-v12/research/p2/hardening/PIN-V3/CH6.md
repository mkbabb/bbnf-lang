# SK-V12 S-P2 PIN-V3 CH6 Anti-Paper-Close Challenge

Verdict: ACCEPT
Score: 97%
Lens: CH6 anti-paper-close
Scope: folded Cycle V3 S-P2 packet after commit `75233b2b`.

## Blocking Findings

None.

## Findings

1. The packet does not claim CSS L4 admission. P2-A states the current result
   surface has zero admitted CSS L4 rows and that comparator lessons are only
   primitive-shape evidence until generated CSS Track 1, same-plane
   lightningcss output, and strict equality exist
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:12`,
   `:14`). P2-F repeats that `skinny/RESULTS.md` is still JSON-only and that
   no `lightningcss` or `css_l4` row exists in the live table
   (`p2f-grammar-neutral.md:18`).

2. CSS L4 absence is routed rather than paper-closed. P2-D says no same-tape
   CSS-local union candidate is shortlist-grounded yet because the profile has
   no generated CSS runtime, lightningcss comparator, equality evidence, or CSS
   hot-leaf attribution (`p2d-substrate-tape.md:54`-`:66`). It then states the
   CSS-local union shape is a conditional post-baseline research aperture, not a
   current P3-A shortlist candidate (`p2d-substrate-tape.md:68`-`:78`).

3. Future-wave language is written as gate language, not a promise. P2-D uses
   "should not shortlist" and "only after" around W1a/W1b CSS evidence
   (`p2d-substrate-tape.md:64`-`:76`), P2-E requires CSS generated parser/fact
   stream consumers unless a measured CSS redress failure routes fallback work
   (`p2e-parse-that-gaps.md:12`), and P2-F says CSS L4 must be benchmarked
   first while Sheets/BBNF-self are fallback falsifiers only after CSS redress
   (`p2f-grammar-neutral.md:14`, `:87`, `:103`).

4. CH4 accounting does not become a paper admission. P2-A marks C6 as an
   output-plane/oracle contract and C7 as a legality surface, not standalone row
   movers (`p2a-sota-teardown.md:36`-`:37`). P2-E marks `pt_fact_event_emit` as
   parser-row-mover ineligible alone and `pt_fact_stream_digest` as
   parser-candidate-ineligible (`p2e-parse-that-gaps.md:41`-`:42`). P2-F's CH4
   supplement explicitly says it admits no row and exists to prevent
   support/oracle/accounting verdicts from becoming implicit candidate evidence
   (`p2f-grammar-neutral.md:61`-`:81`).

5. SIMD/ASM and union categories are not admitted by citation. P2-A requires
   REDRESS material differential plus scalar/parity/checkasm and same-wave
   consumer evidence for union/ASM plans, and blocks string-region SIMD on the
   `escape_mask_64` falsifier (`p2a-sota-teardown.md:57`-`:60`). P2-B frames
   `ESCAPE_MASK_64_FIX_GATE` as a correctness prerequisite with no close credit
   until strict parity is green, and treats support gates as N/A unless a real
   consumer lands (`p2b-dav1d-process.md:43`, `:50`-`:57`). P2-C similarly
   keeps inventory/drop and support-only ARM rows outside admission unless a
   named consumer and material differential exist (`p2c-arch-esoterica.md:47`-
   `:59`, `:186`-`:207`).

6. The stale pre-pin convergence claim is gone. The hardening marker says
   pre-pin convergence is superseded, Cycle V3 is the folded surface after
   PIN-V2, and S-P2 is not converged under the user pin until two consecutive
   clean challenge cycles after the last REVISE reset
   (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:5`,
   `:36`-`:62`).

## Nonblocking Notes

- P2-D still uses W1a/W1b labels for the future CSS baseline/comparator
  aperture. This is acceptable for CH6 because the text treats them as
  prerequisites and not as a closed route, but S-P3 should normalize those names
  if the final wave manifest changes.
- `P2-F:50` says generated FIRST-set dispatch is mandatory for the CSS L4
  baseline. That is not a paper-close in context because the same row requires
  scalar generated dispatch oracle and lightningcss fact-stream equality.

## Exact Fold Edits

N/A. ACCEPT.

## Commands Used

```sh
git status --short && git rev-parse --short HEAD && ps -axo pid,comm,args | rg 'cargo|rustc|xctrace|samply' | rg -v 'rg|Codex' || true
find restart/skinny/tranches/sk-v12/research/p2 -maxdepth 3 -type f | sort
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md
sed -n '1,280p' restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md
sed -n '1,300p' restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md
sed -n '1,320p' restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md
sed -n '1,320p' restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md
rg -n "admit|admission|GO|closed|converged|lightningcss|Track 1|Track1|strict equality|future|will|should|candidate-eligible|shortlist|N/A|parser-row-mover ineligible|parser-candidate-ineligible|outside the current S-P3 candidate pool|support-only|Inventory/drop|Diagnostic only|Do not shortlist|No P2-D primitive" restart/skinny/tranches/sk-v12/research/p2/*.md restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md restart/skinny/tranches/sk-v12/HANDOFF.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md
rg -n "Status: CONVERGED|CONVERGED UNDER USER PIN|pre-pin.*converged|CSS L4 row admits|admitted CSS|lightningcss_mbps.*PASS|G-Alpha.*PASS|S-P2 is converged|S-P2.*CONVERGED" restart/skinny/tranches/sk-v12/research/p2 restart/skinny/tranches/sk-v12/HANDOFF.md restart/skinny/tranches/sk-v12/SYNTHESIS.md restart/skinny/RESULTS.md skinny/RESULTS.md 2>/dev/null || true
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md | sed -n '1,130p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md | sed -n '1,150p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md | sed -n '1,120p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md | sed -n '1,180p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md | sed -n '1,120p'
```
