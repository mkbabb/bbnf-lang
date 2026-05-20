# SK-V12 S-P2 PIN-V3 CH3 Regression / REDRESS Challenge

Pass: S-P2 Research.
Cycle: PIN-V3.
Lens: CH3 REGRESSION.
Date: 2026-05-20.
Verdict: ACCEPT.
Score: 96%.

## Blocking Findings

None.

## Review Scope

Reviewed the folded Cycle V3 S-P2 packet at HEAD `75233b2b`, with the user pin,
S-P1 convergence, `skinny/RESULTS.md`, and `skinny/REDRESS.md` as authorities.
CH3 asks whether the research packet reopens a previously rejected REDRESS
route, treats parse-only as admission, moves rows during S-P2, erases the
category-unblock material-differential requirements, or lets the PIN-V2 CH4
accounting fold promote support/kernel inventory into implementation authority.

## Findings

1. S-P2 still moves no row. `PASS-2-RESEARCH.md` keeps S-P2 read-only against
   source and says it produces design artifacts, not behavior source edits; the
   folded packet remains six research artifacts at Cycle V3. `git diff
   --exit-code HEAD -- skinny/RESULTS.md` is clean, and `RESULTS.md` still
   records the JSON-only `N-direct / NoGo` result surface with no admitted CSS
   L4 row.

2. The parse-only diagnostic boundary is intact. The user pin and handoff keep
   `parse_only` diagnostic-only and make generated CSS L4 greater than
   `lightningcss_mbps + 1` the admission target. P2-A frames comparator lessons
   as primitive-shape evidence only until CSS L4 Track 1, same-plane
   lightningcss output, and strict equality exist; P2-B rejects JSON
   parse-only and permissive/lossy rows as admission anchors; P2-C warns not to
   use JSON direct residuals as the SK-V12 close target; P2-F says CSS L4 must
   be benchmarked first and Sheets/BBNF-self are fallback falsifiers only.

3. USER PIN D3/D4 are honored as category unblocks, not history deletion.
   P2-B states REDRESS 96/97/98 and 88/89/90 remain measured history and require
   a material differential, micro-proof, checkasm/parity, and same-wave
   consumer before any new union or ASM-gen attempt. P2-C repeats the required
   differentials: PMULL cannot be the REDRESS 88 default prefix-XOR body, CTZ
   cannot be the REDRESS 89 global next-bit/bulk replacement, canary hardening
   cannot become row movement, and union work cannot repeat retained class
   columns, parser-owned cursor lists, or source-free class-lane proofs. P2-D
   allows only a future same-tape CSS fact-stream aperture after CSS baseline
   evidence; it marks parallel cursor/class-lane shapes as not candidates.

4. The PIN-V2 CH4 accounting fold did not reopen rejected support/kernel
   routes. P2-B now gives every support/oracle row a micro-proof or explicit
   N/A, same-wave consumer class, and orphan disposition. The risky rows remain
   fenced: bitmap prefix-XOR must be consumed under a named string-region caller
   or demoted and REDRESS 88 blocks default retention; bitmap next-bit/bulk
   emit must be local consumer support or demoted and REDRESS 89 blocks global
   replacement; `byte_context` and `cache_hints` are consume-or-demote support;
   output digest is parser-candidate-ineligible unless a concrete parser
   consumer also moves. P2-F's new CH4 supplement reinforces that support,
   inventory, diagnostic, oracle, and accounting families do not admit rows.

5. String, Unicode, numeric, and digest REDRESS boundaries remain closed.
   P2-C and P2-E carry REDRESS 106/107/108 as proof-only/string-caller
   boundaries, REDRESS 114 as the numeric direct rejection, REDRESS 117 as the
   escaped-segment production block, and REDRESS 118 as the digest/host-sink
   block. The packet requires scalar references, strict parity, caller-local
   micro-proof, and CSS/generated same-wave consumers before any such primitive
   can become S-P3 implementation authority.

6. The `escape_mask_64` correctness blocker and zero-orphan close rule remain
   load-bearing. P2-B, P2-C, P2-E, and P2-F all require resolving the xorshift
   `0xCAFEF00DBAADF00D` falsifier before new string-region SIMD admission.
   They also route the five carried aarch64 orphans through consume, remove, or
   explicit inventory demotion; no support kernel is admitted merely because
   checkasm exists.

## Nonblocking Notes

- The older `hardening/V3` and `HARDENING-S-P2-V3-CONSOLIDATED.md` files are
  pre-pin historical context. The live status file explicitly marks pre-pin
  convergence superseded and routes this PIN-V3 challenge over the folded Cycle
  V3 packet.
- The research packet can proceed to S-P3 only as a candidate pool. S-P3 must
  still select concrete waves, name gates, and preserve the user-pin order:
  GrammarConfig/CSS legality, generated CSS L4 baseline and lightningcss
  comparator, `escape_mask_64` correctness, then any material-differential
  union or ASM-gen attempt.

## Exact Fold Edits If REVISE/REJECT

None. CH3 accepts the folded Cycle V3 packet as regression-clean.

## Commands Used

- `git status --short`
- `git rev-parse --short HEAD`
- `pgrep -fl 'cargo|rustc|xctrace|samply'`
- `sed -n '1,220p' restart/prompts/skinny/PASS-2-RESEARCH.md`
- `sed -n '1,260p' restart/prompts/ORCHESTRATOR.md`
- `sed -n '1,240p' restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `sed -n '1,260p' restart/skinny/tranches/sk-v12/HANDOFF.md`
- `sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `sed -n '1,220p' skinny/RESULTS.md`
- `rg -n "REDRESS (88|89|90|96|97|98|106|107|108|109|110|111|112|113|114|115|116|117|118|119|120)" skinny/REDRESS.md`
- `sed -n '2488,2615p' skinny/REDRESS.md`
- `sed -n '2788,2958p' skinny/REDRESS.md`
- `sed -n '3138,3230p' skinny/REDRESS.md`
- `sed -n '3224,3295p' skinny/REDRESS.md`
- `sed -n '3272,3565p' skinny/REDRESS.md`
- `sed -n '1,260p' restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`
- `sed -n '1,300p' restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`
- `sed -n '1,340p' restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `sed -n '1,320p' restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `sed -n '1,340p' restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`
- `sed -n '1,340p' restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V2/CH4.md`
- `sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V2/CONSOLIDATED.md`
- `git show --stat --oneline --name-only 75233b2b`
- `rg -n "row moves|moves no|does not admit|parser-candidate-ineligible|Support-only|Support inventory|Inventory/drop|Diagnostic only|Do not shortlist|not shortlist|not a candidate|REDRESS 88|REDRESS 89|REDRESS 90|REDRESS 96|REDRESS 97|REDRESS 98|REDRESS 106|REDRESS 107|REDRESS 108|REDRESS 111|REDRESS 112|REDRESS 113|REDRESS 114|REDRESS 115|REDRESS 116|REDRESS 117|REDRESS 118|REDRESS 119|REDRESS 120|escape_mask_64|same-wave|orphan" restart/skinny/tranches/sk-v12/research/p2/p2{a,b,c,d,e,f}-*.md`
- `rg -n "css_l4|lightningcss|CSS L4" skinny/RESULTS.md restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md restart/skinny/tranches/sk-v12/HANDOFF.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `git diff --exit-code HEAD -- skinny/RESULTS.md`
- `rg -n "^Pass: S-P2 Research\\. Cycle: V3\\.|^Pass: S-P2 Research\\. Cycle: V[12]\\." restart/skinny/tranches/sk-v12/research/p2/p2{a,b,c,d,e,f}-*.md`
- `git status --short`
