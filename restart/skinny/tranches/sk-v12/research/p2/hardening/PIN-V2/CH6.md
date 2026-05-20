# SK-V12 S-P2 PIN-V2 CH6 — Anti-Paper-Close

Pass: S-P2 Research CHALLENGE.
Cycle: PIN-V2.
Lens: CH6 anti-paper-close.
Date: 2026-05-20.
Artifact scope: review folded Cycle V2 S-P2 packet after commit `31859478`.
Output: this file.

## Verdict

Verdict: ACCEPT.
Score: 97%.

## Blocking Findings

None.

## Acceptance Basis

1. The folded packet does not claim CSS L4 admission. P2-A states that
   comparator lessons are primitive-shape evidence only until generated CSS L4
   Track 1, same-plane lightningcss output, and strict equality exist
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:14`).
   P2-B repeats that there is no generated CSS L4 runtime, no lightningcss
   same-plane comparator, and no strict equality oracle in the pin profile root
   (`p2b-dav1d-process.md:17`). P2-E says a candidate cannot satisfy the
   lightningcss target without generated CSS Track 1, independent oracle/Track
   2, same-plane lightningcss comparator, strict equality, and gate-consumed
   provenance (`p2e-parse-that-gaps.md:68`). P2-F records no admitted generated
   CSS L4 row and keeps `RESULTS.md` JSON-only (`p2f-grammar-neutral.md:18`).

2. CSS L4 absence is routed rather than paper-closed. P2-D explicitly says no
   same-tape CSS-local union candidate is shortlist-grounded yet, because CSS L4
   has no generated skinny parser or lightningcss comparator row
   (`p2d-substrate-tape.md:54`-`:66`). Its candidate table marks
   `css_fact_stream_same_tape_kind` as "Do not shortlist yet; conditional
   post-W1b aperture" and marks capacity/flag/retained-view shapes diagnostic or
   ineligible (`p2d-substrate-tape.md:102`-`:112`). This is a guardrail, not a
   current admission.

3. Future-wave language is bounded by evidence requirements. P2-F's note that a
   later CHALLENGE fold may revise dispositions is a governance caveat, not a
   promise of completion (`p2f-grammar-neutral.md:10`). P2-C splits selectable
   candidates from inventory/support rows and says C2 and C7-C12 remain
   inventory/support unless a later CSS-local profile and same-wave consumer make
   them measurable (`p2c-arch-esoterica.md:40`-`:59`). P2-F independently says
   rows marked inventory/drop, support-only, diagnostic-only, or
   parser-candidate-ineligible are outside the current S-P3 candidate pool unless
   a later folded pass adds fresh P1 evidence, scalar oracle, micro-proof, and
   same-wave consumer (`p2f-grammar-neutral.md:28`).

4. The packet carries the user-pin close bar rather than the rescinded baseline
   lift. P2-A uses generated CSS L4 > `lightningcss_mbps + 1` on the same
   corpus/output plane/host with strict equality (`p2a-sota-teardown.md:14`,
   `:53`). P2-B repeats the same strict comparator discipline
   (`p2b-dav1d-process.md:90`). P2-F makes Sheets/BBNF-self fallback-only after
   measured CSS redress, not substitutes for CSS (`p2f-grammar-neutral.md:14`,
   `:65`, `:81`).

5. The pre-pin convergence claim is not stale. The live S-P2 hardening status is
   explicitly `PRE-PIN CONVERGENCE SUPERSEDED; PIN S-P2 HARDENING IN PROGRESS`
   (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:5`).
   It says PIN-V2 CHALLENGE is the next required action and that S-P2 is not
   converged under the user pin until two clean cycles are recorded
   (`HARDENING-S-P2-CONVERGED.md:44`-`:46`).

6. Comparator and ISA claims are cited enough for anti-paper-close review. P2-A
   cites comparator sources for asmjson, sonic-rs, simdjson, yyjson, and
   lightningcss (`p2a-sota-teardown.md:80`-`:101`). P2-C cites local SIMD code,
   REDRESS history, and Arm ACLE reference sections for TBL/TBX, UDOT,
   shift/extract, LD4, PMULL, CSSC, and SHA3 (`p2c-arch-esoterica.md:212`-`:235`).
   CH6 did not find an uncited comparator/ISA assertion that the packet uses as
   current admission evidence.

## Nonblocking Notes

- P2-D's `css_fact_stream_same_tape_kind` wording is intentionally conditional.
  S-P3 should keep it behind W1a/W1b CSS legality/baseline evidence, as P2-D
  already requires.
- P2-F's "later CHALLENGE fold may still revise dispositions" sentence is
  acceptable because it does not close a candidate; it names unsettled review
  surfaces.
- P2-C retains broad ARM inventory in the packet, but the folded table now
  prevents that inventory from masquerading as selectable row-movers.

## Commands Used

```sh
git status --short && ps -axo pid,comm,args | rg '(cargo|rustc|xctrace|samply)' || true
find restart/skinny/tranches/sk-v12/research/p2 -maxdepth 3 -type f -print0 | xargs -0 stat -f '%Sm %N' | sort
git show --stat --oneline --decorate --no-renames 31859478
sed -n '1,220p' restart/prompts/skinny/PASS-2-RESEARCH.md
sed -n '1,220p' restart/prompts/ORCHESTRATOR.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/HANDOFF.md
nl -ba restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md | sed -n '1,220p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md | sed -n '1,240p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md | sed -n '1,260p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md | sed -n '1,320p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md | sed -n '1,280p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md | sed -n '1,320p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md | sed -n '1,320p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md | sed -n '1,260p'
rg -n '(?i)(CSS L4.*admit|admitted CSS|beats lightning|beat lightning|lightningcss_mbps|ceil\(baseline|baseline_mbps|preflight-equivalent|Sheets.*first|BBNF-self.*first|CONVERGED UNDER USER PIN|Status: CONVERGED|future wave will|will detail|TBD|TODO|placeholder|promise|admit.*without|no admitted generated CSS|does not prove CSS|cannot satisfy the lightningcss|must beat|fallback-only|not converged)' restart/skinny/tranches/sk-v12/research/p2 restart/skinny/tranches/sk-v12/HANDOFF.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md
rg -n '^Pass: S-P2 Research\. Cycle:|^# SK-V12 P2-|^Status:' restart/skinny/tranches/sk-v12/research/p2/p2*.md restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md
rg -n '(?i)(todo|tbd|placeholder|future wave will|will detail|will be detailed|admission.*from JSON|CSS.*admitted|admitted.*CSS|ceil\(baseline_mbps|baseline\*1\.01|preflight-equivalent substitutes|Sheets.*substitute|BBNF-self.*substitute|S-P2 is converged|Status: CONVERGED UNDER USER PIN)' restart/skinny/tranches/sk-v12/research/p2/*.md restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md
git log --oneline -5 --decorate && git status --short
```
