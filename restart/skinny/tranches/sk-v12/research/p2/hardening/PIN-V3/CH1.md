# SK-V12 S-P2 PIN-V3 CH1 Correctness Challenge

Pass: S-P2 Research CHALLENGE. Cycle: PIN-V3.
Date: 2026-05-20.
Scope: folded Cycle V3 S-P2 packet after commit `75233b2b`.
Output: this file.

## Verdict

ACCEPT.

Score: 96%.

## Blocking Findings

None.

## Findings

1. The folded packet uses the pin-aware S-P1 authority, not stale pre-pin
   research. All six S-P2 artifacts are `Cycle: V3`; P2-A/P2-B/P2-C/P2-E
   frontmatter names the ten accepted S-P1 hot families, and P2-D/P2-F also
   carry the same family set or consume the current P2-B/C/D/E packet. The live
   hardening marker explicitly says pre-pin convergence is superseded and that
   PIN-V3 is the next required challenge action
   (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:5`,
   `:60`).

2. Comparator strictness matches the user pin. P2-A routes comparator lessons
   to primitive-shape evidence only until a generated CSS L4 Track 1 row,
   same-plane lightningcss output, and strict equality exist
   (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:12`-`:14`).
   P2-D and P2-E repeat that JSON movement is guard evidence, not CSS admission,
   and keep the close bar at `lightningcss_mbps + 1`
   (`p2d-substrate-tape.md:54`-`:78`; `p2e-parse-that-gaps.md:66`-`:68`).
   P2-F preserves CSS first and Sheets/BBNF-self fallback-only ordering
   (`p2f-grammar-neutral.md:14`, `:85`-`:87`). I found no lingering
   `ceil(baseline_mbps * 1.01)`, Sheets-first, BBNF-self-first, or parse-only
   admission claim in the folded packet.

3. Selectable candidates trace to S-P1 hot-family antecedents. P2-A C1-C7 each
   names concrete antecedent families in its §2 table
   (`p2a-sota-teardown.md:29`-`:37`). P2-C now says only C1, C3, C4, C5, and
   C6 are selectable; C2 and C7-C12 are inventory/support unless later CSS-local
   profile evidence and a same-wave consumer make them measurable
   (`p2c-arch-esoterica.md:40`-`:59`). P2-E's parser/support/oracle rows carry
   candidate class and consumer/ineligible status, including parser-candidate
   ineligibility for fact-stream digest accounting (`p2e-parse-that-gaps.md:31`-`:42`).
   P2-D has no shortlist-ready primitive in this cycle and marks same-tape union
   work as a post-W1b aperture, not current candidate authority
   (`p2d-substrate-tape.md:100`-`:112`).

4. Nonselectable inventory is not treated as candidate authority. P2-C demotes
   LD4 interleaving, SHA3 ternary fold, and PRFM/STNP cache hints to
   inventory/drop where no P1 antecedent or real consumer exists
   (`p2c-arch-esoterica.md:49`, `:56`, `:58`). P2-F's V3 CH4 supplement keeps
   bitmap, byte-context, cache-hint, output-digest, diagnostic tape-policy, and
   rejected/inventory shapes outside parser-row movement unless a later folded
   pass adds fresh evidence, scalar oracle, and same-wave consumer
   (`p2f-grammar-neutral.md:61`-`:81`).

5. The PIN-V2 CH4 fold did not introduce CH1 drift. It added row-level
   micro-proof, parity/N/A, same-wave proof, and orphan-disposition accounting
   to P2-B, P2-D, and P2-F without changing the strict CSS admission plane or
   promoting support rows into candidates (`p2b-dav1d-process.md:41`-`:57`;
   `p2d-substrate-tape.md:106`-`:112`; `p2f-grammar-neutral.md:67`-`:81`).

6. Source claims resolve on spot check. The previously fragile yyjson anchor now
   resolves to the portable/strict/no-explicit-SIMD feature lines, and the dav1d
   `msac.c` anchors resolve to cloned C/ASM state, `call_ref`, `call_new`,
   return/state/CDF comparison, and `bench_new`. Sonic README anchors resolve to
   the no-simdjson-two-stage SIMD note, direct-to-struct/no-temporary-structure
   note, RawNumber note, and UTF-8 validation note. The comparator, FFmpeg,
   dav1d, ACLE, and lightningcss source URLs tested returned HTTP 200.

## Nonblocking Notes

- P2-C's ACLE references are page-level plus named sections rather than stable
  line anchors. That is acceptable for CH1 because the artifact names the exact
  intrinsic or feature macro beside each broad Arm reference, but future folds
  should keep section labels attached if the Arm site changes anchors.
- This ACCEPT is research correctness only. It does not admit any wave, any CSS
  row, or any SIMD implementation.

## Exact Fold Edits If REVISE/REJECT

None.

## Commands Used

- `git status --short && git rev-parse --short HEAD`
- `rg -n "^(Pass:|Cycle:|Status:|## §|# |\\[.*\\]:|https://|lightningcss|sonic|candidate|Candidate|Inventory|S-P3 eligibility|PIN-V2|pre-pin|stale|SK-V12-open|/tmp/skv12)" restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `sed -n '1,220p' restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `sed -n '1,220p' restart/skinny/tranches/sk-v12/HANDOFF.md`
- `sed -n '1,220p' restart/prompts/skinny/PASS-2-RESEARCH.md`
- `sed -n '1,180p' restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`
- `sed -n '1,180p' restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`
- `sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `sed -n '1,240p' restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `sed -n '1,190p' restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`
- `sed -n '1,180p' restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `sed -n '1,80p' skinny/RESULTS.md`
- `sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V2/CH4.md`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md | sed -n '1,150p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md | sed -n '95,135p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md | sed -n '20,100p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md | sed -n '24,110p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md | sed -n '38,170p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md | sed -n '28,75p'`
- `rg -n "https?://[^ )]+" restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `find restart/skinny/tranches/sk-v12/research/p2/hardening -maxdepth 2 -type f | sort`
- `rg -n "Status:|PIN-V2|PIN-V3|ACCEPT|REVISE|Score|Disposition|Cycle" restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V2/*.md`
- `curl -L -s -o /dev/null -w '%{http_code}\\n' <external-source-url>` for representative P2-A/P2-B/P2-C source URLs.
- `curl -Ls https://raw.githubusercontent.com/ibireme/yyjson/master/README.md | nl -ba | sed -n '10,15p'`
- `curl -Ls https://raw.githubusercontent.com/videolan/dav1d/master/tests/checkasm/msac.c | nl -ba | sed -n '115,136p;156,175p;184,195p;203,215p;228,250p'`
- `curl -Ls https://raw.githubusercontent.com/cloudwego/sonic-rs/main/README.md | nl -ba | sed -n '60,66p;84,90p;130,135p;436,455p'`
- `rg -n 'Cycle: V[12]|L271-L279|L1127-L1227|ceil\\(baseline|baseline_mbps|Sheets.*first|BBNF-self.*first|parse_only.*(target|admit)|CONVERGED UNDER USER PIN|Status: CONVERGED|six current AArch64|five parse-that|Inventory/drop for current S-P2|Support-only until|parser-candidate-ineligible|Diagnostic only|Not a candidate|Do not shortlist yet|no CSS|CSS L4|lightningcss_mbps \\+ 1|lightningcss' restart/skinny/tranches/sk-v12/research/p2 restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `ps -axo pid,comm,args | rg '(cargo|rustc|xctrace|samply)' || true`
- `git status --short`
