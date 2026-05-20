# SK-V12 S-P2 PIN-V2 CH3 Regression / REDRESS Challenge

Pass: S-P2 Research CHALLENGE.
Cycle: PIN-V2.
Lens: CH3 regression / REDRESS.
Date: 2026-05-20.
Scope: review the folded Cycle V2 S-P2 packet at commit `31859478` for
historical REDRESS integrity, user-pin D3/D4 unblocks, parse-only discipline,
and row-movement boundaries.
Output: this file.

## Verdict

ACCEPT.

Score: 97%.

## Blocking Findings

None.

## Regression Review

1. Union category unblocks preserve the historical REDRESS differentials. The
   user pin rescinds category-level union preblocks while preserving REDRESS
   96/97/98 as measured historical implementations; the V2 packet carries that
   distinction consistently. P2-D makes no union primitive shortlist-ready in
   this cycle and labels `css_fact_stream_same_tape_kind` conditional on a
   CSS baseline, same-plane oracle, REDRESS 96/97/98 citation, strict equality,
   JSON guard preservation, and a measured `lightningcss_mbps + 1` row
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:102`,
   `:108`). It also rejects retained structural cursor/class-lane shapes as
   not candidates (`:112`) and warns that "now CSS" is not a sufficient
   differential (`:149`-`:153`). P2-C and P2-F independently restate that
   REDRESS 96/97/98 remain measured failures and require no retained class
   column, no parser-owned cursor/list, no second scan stream, no source-free
   proof-only route, and a same-wave measured consumer
   (`p2c-arch-esoterica.md:196`-`:202`; `p2f-grammar-neutral.md:24`,
   `:56`, `:85`, `:89`).

2. ASM-gen category unblocks preserve REDRESS 88/89/90. P2-C narrows PMULL to
   `a64_pmull_prefix_xor_narrow_consumer`, support-only until W2 plus a named
   caller, and requires the material differential from REDRESS 88: not the
   production-default `bitmap_prefix_xor_64` body, not a parse-only JSON
   default path, feature-gated fallback, scalar parity, `escape_mask_64`
   boundary proof, and measured same-wave generated CSS/non-JSON or guard-row
   consumer (`p2c-arch-esoterica.md:54`, `:115`-`:122`, `:188`-`:194`). The
   CTZ route is likewise support-only and local to a named consumer, with no
   global next-bit/bulk-emitter replacement, no canary-as-row-movement claim,
   and no retained cursor/side table (`:55`, `:124`-`:131`, `:188`-`:194`).
   P2-B and P2-E repeat that D4 reopens categories, not old patches, and that
   proof-only SIMD cannot be promoted without scalar reference, checkasm,
   same-wave consumer, and material differential
   (`p2b-dav1d-process.md:82`-`:84`; `p2e-parse-that-gaps.md:64`).

3. `parse_only` remains diagnostic and cannot satisfy admission. P2-A states
   the generated CSS L4 row must beat `lightningcss_mbps + 1` on the same CSS
   fact-stream plane, and that JSON `parse_only` rows cannot stand in for that
   plane (`p2a-sota-teardown.md:53`, `:57`). P2-E states JSON rows can preserve
   guard context but cannot satisfy the lightningcss target without generated
   CSS Track 1, independent oracle/Track 2, same-plane comparator, strict
   equality, and gate-consumed provenance (`p2e-parse-that-gaps.md:68`). The
   handoff agrees: `parse_only` is diagnostic only and parse-only admission is
   a refusal condition (`restart/skinny/tranches/sk-v12/HANDOFF.md:35`,
   `:64`, `:155`).

4. Rejected routes are not reopened as implementation authority. The V2 packet
   routes string and escape proof-only work through REDRESS 106/107/108
   constraints (`p2c-arch-esoterica.md:94`, `:103`, `:204`; `p2b-dav1d-process.md:85`),
   keeps numeric JSON direct closure constrained by REDRESS 114
   (`p2b-dav1d-process.md:86`; `p2a-sota-teardown.md:61`), preserves the
   output-digest/host-sink block from REDRESS 118 (`p2a-sota-teardown.md:62`;
   `p2e-parse-that-gaps.md:60`), and keeps REDRESS 119/120 direct residuals as
   guard/fixpoint evidence rather than the SK-V12 close target
   (`p2b-dav1d-process.md:88`; `p2c-arch-esoterica.md:206`;
   `p2e-parse-that-gaps.md:68`).

5. S-P2 moves no rows and edits no implementation surface. Commit `31859478`
   changes only S-P2 research/hardening documents. A live diff check over
   `skinny/RESULTS.md`, `skinny/REDRESS.md`, `skinny/crates`, and
   `restart/prompts` returned `diff_rc=0`, so there is no uncommitted source,
   result, REDRESS, or prompt movement attributable to this CH3 review.

## Nonblocking Notes

- The V2 packet is conservative about support inventory. C2/C7-C12 in P2-C are
  retained for D4/D5 inventory and orphan disposition, but only C1/C3/C4/C5/C6
  are selectable from current S-P1 evidence (`p2c-arch-esoterica.md:40`-`:59`).
- The zero-orphan close rule is represented as a wave obligation, not an S-P2
  row movement. That is appropriate for S-P2: it names same-wave consumption or
  inventory demotion/removal for `bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
  `bulk_emit_positions_64`, `byte_context`, and `cache_hints`, but it does not
  claim close has already happened.
- CSS L4 remains the first target. Sheets/BBNF-self are only fallback
  falsifiers after CSS redress, matching the pin and handoff.

## Exact Fold Edits Required

None. ACCEPT.

## Commands Used

```sh
git status --short && git rev-parse --short HEAD && ps -axo pid,comm,args | rg '(cargo|rustc|xctrace|samply)' || true
find restart/skinny/tranches/sk-v12/research/p2 -maxdepth 3 -type f -print | sort | xargs ls -lt | head -40
git show --stat --oneline --decorate 31859478 --
sed -n '1,220p' restart/prompts/skinny/PASS-2-RESEARCH.md
sed -n '1,240p' restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/HANDOFF.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md
sed -n '2700,2935p' skinny/REDRESS.md
sed -n '2475,2605p' skinny/REDRESS.md
sed -n '3270,3565p' skinny/REDRESS.md
sed -n '1,220p' skinny/RESULTS.md
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md | sed -n '38,225p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md | sed -n '90,170p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md | sed -n '29,78p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md | sed -n '24,92p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md | sed -n '39,105p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md | sed -n '27,72p'
git show --name-only --pretty=format:'%h %s' 31859478 --
git diff --exit-code -- skinny/RESULTS.md skinny/REDRESS.md skinny/crates restart/prompts >/tmp/skv12-p2-pin-v2-ch3-source-diff.txt
rg -n "parse_only|diagnostic|REDRESS 8[89]|REDRESS 90|REDRESS 9[678]|category-unblocked|category unblocked|material differential|default body|global|sidecar|retained|same-wave|RESULTS.md|REDRESS 119|REDRESS 120|lightningcss|ceil\\(baseline|Sheets" restart/skinny/tranches/sk-v12/research/p2/*.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/HANDOFF.md
rg -n "REDRESS 88|REDRESS 89|REDRESS 90|REDRESS 96|REDRESS 97|REDRESS 98|REDRESS 106|REDRESS 107|REDRESS 108|REDRESS 109|REDRESS 110|REDRESS 111|REDRESS 112|REDRESS 113|REDRESS 114|REDRESS 115|REDRESS 116|REDRESS 117|REDRESS 118|REDRESS 119|REDRESS 120" restart/skinny/tranches/sk-v12/research/p2/*.md
```
