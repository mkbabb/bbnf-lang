# SK-V12 S-P2 PIN-V4 CH1 Correctness Challenge

Pass: S-P2 Research CHALLENGE. Cycle: PIN-V4.
Date: 2026-05-20.
Scope: unchanged folded Cycle V3 S-P2 packet after commits `75233b2b` and `b407583e`.
Output: this file.

## Verdict

ACCEPT.

Score: 97%.

## Blocking Findings

None.

## Findings

1. The reviewed S-P2 packet is still the folded Cycle V3 packet. All six active
   P2 artifacts report `Pass: S-P2 Research. Cycle: V3`, and `git diff
   --name-only 75233b2b..b407583e -- restart/skinny/tranches/sk-v12/research/p2/p2*.md`
   returned no files. PIN-V3 added only hardening artifacts; it did not mutate the
   six research files reviewed here.

2. Stale pre-pin authority does not control the packet. The live S-P1
   convergence artifact is pin-aware, records the CSS L4 absence and the ten
   accepted hot-family antecedents, and routes JSON-only telemetry as nomination
   evidence rather than CSS proof. The current S-P2 hardening marker explicitly
   says pre-pin convergence is superseded and that PIN-V4 is the second clean
   cycle required after PIN-V3.

3. Comparator strictness matches the user pin. P2-A, P2-D, P2-E, and P2-F keep
   CSS L4 on the generated Track 1 / independent oracle / lightningcss same-plane
   fact-stream comparator, with the close bar fixed at `lightningcss_mbps + 1`.
   JSON parse-only remains diagnostic, JSON direct/typed rows are guard context,
   and Sheets/BBNF-self remain fallback falsifiers only after measured CSS L4
   redress.

4. Selectable candidates trace to pin S-P1 hot-family antecedents. P2-A C1-C7
   each names antecedent families in the candidate table. P2-C limits the
   selectable ARM set to C1, C3, C4, C5, and C6, each tied to accepted hot
   families; C2 and C7-C12 are inventory/support unless a later folded pass adds
   CSS-local profile evidence, scalar oracle, micro-proof, and a same-wave
   consumer. P2-E separates parser row-mover candidates from output-plane/oracle
   accounting rows, and P2-D has no shortlist-ready substrate primitive.

5. Nonselectable inventory is not treated as candidate authority. P2-C demotes
   LD4 interleaving, SHA3 ternary fold, and PRFM/STNP cache hints to
   inventory/drop when no P1 antecedent or real consumer exists, and marks PMULL,
   CSSC CTZ, byte-context, and UTF-8 block work as support-only pending named
   consumers. P2-F's accounting supplement repeats that support-only,
   inventory/drop, diagnostic-only, rejected, and parser-candidate-ineligible
   families are outside the current S-P3 candidate pool.

6. External and local citations resolve on spot check. The previously broken
   yyjson README anchor now resolves to the portable/no-explicit-SIMD/strict
   feature lines. The dav1d `msac.c` anchors resolve to cloned C/ASM state,
   `call_ref`, `call_new`, return/state/CDF comparison, and `bench_new`. The
   sampled asmjson, sonic-rs, simdjson, lightningcss, FFmpeg, dav1d, and Arm ACLE
   URLs returned HTTP 200. The Arm citations remain page-level plus named
   intrinsic/feature-macro sections, which is acceptable for CH1 because the
   artifact names the exact intrinsic or macro beside each reference.

7. No drift from PIN-V3 was found. PIN-V3 CH1 accepted the same folded packet at
   96%; PIN-V4 rechecked the same correctness axes after `b407583e` and found no
   new CH1 defect. This CH1 result is therefore a second clean-cycle correctness
   acceptance for the unchanged Cycle V3 packet.

## Nonblocking Notes

- Archived pre-pin and earlier hardening files still contain historical terms
  such as "six current AArch64 candidates" and "five parse-that gaps"; they are
  not controlling authority under the current hardening marker. Do not cite them
  as current packet state in S-P3.
- This ACCEPT is research correctness only. It does not admit a wave, CSS row,
  SIMD body, union route, or ASM-gen route.

## Exact Fold Edits If REVISE/REJECT

None.

## Commands Used

- `git status --short && git log --oneline -8`
- `rg -n "^Pass:|^Cycle:|^Status:|PIN-V3|PIN-V4|USER PIN|lightningcss|candidate|Inventory|Selectable|stale|pre-pin|skv12-p1" restart/skinny/tranches/sk-v12/research/p2 restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/HANDOFF.md restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md skinny/RESULTS.md skinny/REDRESS.md`
- `rg --files restart/skinny/tranches/sk-v12/research/p2 restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V3 restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V4`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md | sed -n '1,180p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md | sed -n '1,180p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md | sed -n '1,240p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md | sed -n '1,260p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md | sed -n '1,220p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md | sed -n '1,240p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V3/CH1.md | sed -n '1,220p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V3/CONSOLIDATED.md | sed -n '1,220p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md | sed -n '1,220p'`
- `nl -ba restart/prompts/skinny/PASS-2-RESEARCH.md | sed -n '1,170p'`
- `nl -ba restart/prompts/ORCHESTRATOR.md | sed -n '70,130p'`
- `rg -n 'Cycle: V[12]|L271-L279|L1127-L1227|ceil\(baseline|baseline_mbps|Sheets.*first|BBNF-self.*first|parse_only.*(target|admit)|Status: CONVERGED UNDER USER PIN|six current AArch64|five parse-that|SK-V12-open|/tmp/skv12-p1([^a-zA-Z0-9_-]|$)|/tmp/skv12-profile-target-50bd1648' restart/skinny/tranches/sk-v12/research/p2 restart/skinny/tranches/sk-v12/HANDOFF.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md || true`
- `rg -n 'Inventory/drop|Support inventory|Support-only|Diagnostic only|Not a candidate|Do not shortlist yet|parser-candidate-ineligible|Parser row-mover candidate|Selectable candidate|P1 antecedent|lightningcss_mbps \+ 1|strict equality|same output plane|same-plane|CSS L4' restart/skinny/tranches/sk-v12/research/p2/p2{a,b,c,d,e,f}-*.md`
- `curl -L -s -o /dev/null -w '%{http_code}\t%{url_effective}\n' <sampled P2-A/P2-B/P2-C source URL>`
- `curl -Ls https://raw.githubusercontent.com/ibireme/yyjson/master/README.md | nl -ba | sed -n '10,15p'`
- `curl -Ls https://raw.githubusercontent.com/videolan/dav1d/master/tests/checkasm/msac.c | nl -ba | sed -n '115,136p;156,175p;184,195p;203,215p;228,250p'`
- `git diff --name-only 75233b2b..b407583e -- restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `rg -n '^Pass: S-P2 Research\. Cycle: V3\.$|^Pass: S-P2 Research\. Cycle:' restart/skinny/tranches/sk-v12/research/p2/p2{a,b,c,d,e,f}-*.md`
- `ps -axo pid,comm,args | rg '(cargo|rustc|xctrace|samply)' || true; git status --short`
