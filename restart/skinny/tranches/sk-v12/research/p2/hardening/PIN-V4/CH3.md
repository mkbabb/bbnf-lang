# SK-V12 S-P2 PIN-V4 CH3 Regression / REDRESS Challenge

Pass: S-P2 Research.
Cycle: PIN-V4.
Lens: CH3 regression / REDRESS.
Date: 2026-05-20.
Scope: second clean-cycle check over the unchanged folded Cycle V3 S-P2 packet
after research fold `75233b2b` and clean PIN-V3 record `b407583e`.
Output: this file.

## Verdict

ACCEPT.

Score: 97%.

## Blocking Findings

None.

## Review

1. The Cycle V3 packet remains a research packet and moves no row. HEAD is
   `b407583e`; the research fold is `75233b2b`. The six S-P2 research artifacts
   still declare `Pass: S-P2 Research. Cycle: V3.`, and `git diff --exit-code`
   over `skinny/RESULTS.md`, `skinny/REDRESS.md`, `skinny/crates`,
   `restart/prompts`, and the six folded P2 artifacts was clean before this
   artifact was written. This CH3 lens found no source, result, REDRESS, prompt,
   or benchmark movement to account for.

2. The `parse_only` diagnostic boundary is intact. The user pin and handoff
   keep `parse_only` diagnostic-only and require generated CSS L4 Track 1 to
   beat `lightningcss_mbps + 1` on the same corpus, same output plane, same
   host, and strict equality. P2-A treats comparator lessons as primitive-shape
   evidence until a CSS L4 generated Track 1, same-plane lightningcss output,
   and strict equality exist. P2-E and P2-F repeat that JSON rows are guard or
   primitive antecedents only; they cannot satisfy the CSS L4 admission plane.

3. USER PIN D3 is preserved as a category unblock, not as a REDRESS erasure.
   P2-D records REDRESS 96/97/98 as historical measured implementations and
   withholds any same-tape CSS union shortlist until a CSS generated baseline,
   same-plane oracle, lightningcss comparator, and hot-leaf evidence exist. Its
   only plausible aperture, `css_fact_stream_same_tape_kind`, is explicitly
   conditional and must prove a material differential: CSS fact-stream
   consumption inside the single retained tape, not JSON delimiter replacement
   through a retained sidecar. `parallel_structural_cursor_or_class_lane` remains
   "Not a candidate." P2-C and P2-F independently reject retained class lanes,
   parser-owned cursor lists, structural vectors, whitespace bitmaps, decoded
   byte sidecars, and `UnionTape` repeats unless a later folded plan proves a
   new material differential.

4. USER PIN D4 is preserved as a category unblock, not as support-kernel
   admission. P2-B and P2-C keep REDRESS 88 PMULL, REDRESS 89 CSSC CTZ/bulk,
   and REDRESS 90 canary hardening as measured history. PMULL is support-only
   inside a named string-region caller after `escape_mask_64` is green; it is
   not the default `bitmap_prefix_xor_64` body. CTZ is local next/first-bit
   support inside one measured consumer; it is not a global next-bit or bulk
   emitter rewrite. Canary hardening remains checkasm integrity, not row
   movement.

5. The PIN-V2 row-accounting fold did not reopen rejected support/kernel routes.
   P2-B, P2-E, and P2-F mark support, inventory, diagnostic, oracle, and
   accounting families with explicit micro-proof or N/A, same-wave consumer
   requirements, and consume/demote/remove orphan disposition. `pt_fact_event_emit`
   is output-plane/fact-stream work and parser-row-mover ineligible alone;
   `pt_fact_stream_digest` is parser-candidate-ineligible. Bitmap prefix/next,
   bulk emit, `byte_context`, `cache_hints`, widened string scan, x4 hex decode,
   UDOT digit helpers, and movemask support cannot move rows merely because a
   scalar/checkasm surface exists.

6. REDRESS 106-120 remain fenced. The packet carries string and escape proof
   boundaries from REDRESS 106/107/108, numeric direct closure rejection from
   REDRESS 114, container-tail rejection from REDRESS 115, escaped-segment
   production limits from REDRESS 117, digest/host-sink rejection from REDRESS
   118, and JSON direct residual fixpoint routing from REDRESS 119/120. None is
   promoted into current implementation authority. The only admissible future
   route is a CSS/generated same-wave consumer with strict equality, scalar
   oracle, micro-proof, and gate-consumed provenance.

7. `escape_mask_64` and zero-orphan discipline remain load-bearing. The folded
   packet consistently treats the xorshift `0xCAFEF00DBAADF00D` falsifier as a
   correctness prerequisite before any new string-region SIMD admission. It
   also preserves the close target of zero production aarch64 orphans by
   admission, removal, or explicit inventory demotion.

## Nonblocking Notes

- The older non-PIN hardening files remain historical context only. The live
  PIN sequence records PIN-V3 as the first clean cycle after the PIN-V2 reset;
  this CH3 result is compatible with PIN-V4 serving as the second clean-cycle
  check if the other lenses also accept.
- S-P3 still has to select concrete waves and gates. This ACCEPT only says the
  S-P2 research surface is REDRESS-clean; it does not admit CSS L4, union, ASM,
  or SIMD work by itself.

## Exact Fold Edits If REVISE/REJECT

None. CH3 accepts the unchanged folded Cycle V3 packet.

## Commands Used

```sh
git status --short && git log --oneline -8
ps -axo pid,comm,args | rg 'cargo|rustc|xctrace|samply|criterion|bbnf' || true
find restart/skinny/tranches/sk-v12/research/p2 -maxdepth 3 -type f -print | sort | tail -80
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V3/CONSOLIDATED.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md
sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md
sed -n '1,320p' restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md
sed -n '1,320p' restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md
sed -n '1,360p' restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md
sed -n '1,340p' restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V2/CH3.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V3/CH3.md
sed -n '1,180p' restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/HANDOFF.md
rg -n "REDRESS (88|89|90|96|97|98|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120)|^## REDRESS (88|89|90|96|97|98|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120)|parse_only|S / NO-GO|lightningcss|orphan|escape_mask_64|GrammarConfig" skinny/REDRESS.md skinny/RESULTS.md restart/skinny/tranches/sk-v12/research/p2/*.md restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md restart/prompts/skinny/PASS-2-RESEARCH.md restart/prompts/ORCHESTRATOR.md
ls -la restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V4 2>/dev/null || true
rg -n "Verdict|REVISE|REJECT|ACCEPT|blocking|REDRESS|parse_only|row movement|row-mover|candidate-ineligible|support-only|inventory/drop|category" restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V4 restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V3 || true
git status --short && git rev-parse --short HEAD && git show --stat --oneline --name-only 75233b2b -- && git show --stat --oneline --name-only b407583e --
git diff --exit-code HEAD -- skinny/RESULTS.md skinny/REDRESS.md skinny/crates restart/prompts restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md >/tmp/skv12-p2-pin-v4-ch3-source-diff.txt
rg -n "^Pass: S-P2 Research\\. Cycle: V3\\.|^Pass: S-P2 Research\\. Cycle: V[12]\\." restart/skinny/tranches/sk-v12/research/p2/p2{a,b,c,d,e,f}-*.md
rg -n "ceil\\(baseline|baseline_mbps|parse_only.*admi|Sheets.*fallback|BBNF-self.*fallback|category-unblocked|category unblocked|material differential|default body|global replacement|parser-candidate-ineligible|row-mover ineligible|Inventory/drop|Support-only|Diagnostic only|Do not shortlist|not a candidate|REDRESS 88|REDRESS 89|REDRESS 90|REDRESS 96|REDRESS 97|REDRESS 98|REDRESS 106|REDRESS 107|REDRESS 108|REDRESS 114|REDRESS 117|REDRESS 118|REDRESS 119|REDRESS 120|escape_mask_64|orphan" restart/skinny/tranches/sk-v12/research/p2/*.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/HANDOFF.md restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md
```
