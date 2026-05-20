# SK-V12 S-P2 PIN-V1 CH6 Anti-Paper-Close

Verdict: ACCEPT

Score: 96%

## Blocking Findings

None.

## Nonblocking Notes

1. Comparator claims are sourced to primary material rather than self-report. P2-A's comparator matrix cites source-backed asmjson, sonic-rs, simdjson, yyjson, and lightningcss material (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:18`-`:23`, `:80`-`:101`). Spot checks reached the cited asmjson classifier/runtime lines, sonic-rs README, simdjson structural indexer, yyjson README, and lightningcss API source. P2-B likewise grounds checkasm-process claims in VideoLAN, FFmpeg, and dav1d sources (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:22`-`:24`, `:94`-`:98`).

2. ISA claims have manual/source citations and are not treated as admission by prose. P2-C maps TBL/TBX, UDOT, shift/EXT, LD4, PMULL/CSSC/SHA3, PRFM/STNP, and related AArch64 surfaces to local code plus Arm ACLE/Neon sources (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:26`-`:32`, `:36`, `:210`-`:214`). The Arm ACLE source spot checks resolved instruction rows such as `vqtbl4q_u8`, `vld4q_u8`, `vextq_u8`, and feature macro sections for dotprod/CSSC/SHA3. Citation granularity could be improved with exact ACLE line anchors, but the current packet has source authority and does not close an ISA claim without a manual/source pointer.

3. Primitive claims include present scalar-reference sketches or explicit non-admission. P2-A now gives C1-C7 scalar sketches in its candidate table (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:27`-`:37`). P2-B carries scalar status, checkasm expectation, and same-wave consumer class for each gate (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:41`-`:57`). P2-C names executable refs, missing scalar oracles, and smoke-test gaps candidate by candidate (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:43`, `:52`, `:61`, `:70`, `:79`, `:88`, `:97`, `:106`, `:115`, `:124`, `:133`, `:142`). P2-D marks same-tape ideas diagnostic or rejected under current evidence (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:102`-`:112`). P2-E gives function-shaped scalar refs and micro-proof needs for parse-that gaps (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:31`-`:42`). P2-F folds the pool into legality classes with scalar/checkasm needs and drop routes (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:32`-`:59`).

4. CSS L4 absence is routed, not paper-closed. S-P1 records no generated CSS L4 Track 1 runtime, no same-plane lightningcss comparator, and no strict equality oracle, and routes the absence to S-P2/S-P3 (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:55`-`:57`, `:73`-`:76`). The user pin and handoff require generated CSS L4 first and `lightningcss_mbps + 1` on the same corpus/output plane/host with strict equality (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18`-`:34`; `restart/skinny/tranches/sk-v12/HANDOFF.md:54`-`:67`). P2-A, P2-D, P2-E, and P2-F repeat that absence as a prerequisite boundary rather than claiming CSS parity from JSON evidence (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:12`-`:14`; `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:54`-`:78`, `:90`-`:98`; `restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:12`, `:68`; `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:14`-`:20`).

5. Future-wave language is guardrail language, not load-bearing deferral. P2-D says no P2-D primitive is shortlist-ready and labels same-tape entries diagnostic/ineligible until CSS baseline evidence exists (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:102`-`:112`, `:144`-`:147`). P2-C demotes LD4 and SHA3 ternary fold when scalar oracle, P1 antecedent, or consumer is missing (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:49`-`:56`, `:112`-`:119`, `:189`). P2-F's note that a later CHALLENGE fold may revise dispositions is governance caveat, not a candidate promise (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:10`).

6. The current convergence summary is consistent with this CH6 audit: it states P2-A candidates are comparator-grounded only under scalar/checkasm/same-wave floors, P2-D has no selectable tape-substrate candidate, P2-E contributes scalar-sketch-backed gaps, and generated non-JSON baseline work remains first priority (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:23`-`:39`).

## Exact Fold Edits If REVISE/REJECT

N/A. Verdict is ACCEPT; no fold edits are required.

## Commands Used

- `pwd && rg --files restart/skinny | rg 'PASS-2-RESEARCH|ORCHESTRATOR|USER-PIN|HANDOFF|convergence|PIN-V1|p2|S-P1|artifacts|CH[0-9]|sk-v12'`
- `find restart/skinny/tranches/sk-v12 -maxdepth 8 -type f | sort`
- `git status --short`
- `rg --files | rg '(^|/)PASS-2-RESEARCH\.md$|(^|/)ORCHESTRATOR\.md$|USER-PIN|HANDOFF\.md$|HARDENING-S-P1-CONVERGED\.md$|HARDENING-S-P2-CONVERGED\.md$|restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V1/CH6\.md$'`
- `test -e restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V1/CH6.md && ls -l restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V1/CH6.md || true`
- `find restart/skinny/tranches/sk-v12/research/p2/hardening -maxdepth 2 -type f | sort`
- `rg -n '^#{1,6} |^§|CH6|3W|3Z' restart/prompts/skinny/PASS-2-RESEARCH.md restart/prompts/ORCHESTRATOR.md`
- `wc -l restart/prompts/skinny/PASS-2-RESEARCH.md restart/prompts/ORCHESTRATOR.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/HANDOFF.md restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `nl -ba restart/prompts/skinny/PASS-2-RESEARCH.md | sed -n '62,145p'`
- `nl -ba restart/prompts/ORCHESTRATOR.md | sed -n '74,128p'`
- `nl -ba restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `nl -ba restart/skinny/tranches/sk-v12/HANDOFF.md`
- `nl -ba restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/hardening/V1/CH6.md`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/hardening/V2/CH6.md`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/hardening/V3/CH6.md`
- `rg -n '\b(future|later|defer|placeholder|TODO|TBD|will detail|will fill|paper-close|paper close|shortlist-ready|inventory|diagnostic|fallback|fall back)\b' restart/skinny/tranches/sk-v12/research/p2/*.md`
- `rg -n '\[(A|S|J|Y|L|VLC|FF|D|ACLE)[^\]]*\]|Arm|ACLE|Neon|NEON|TBL|TBX|UDOT|LD4|PMULL|CSSC|SHA3|PRFM|STNP|EOR3|BCAX|simdjson|sonic|asmjson|yyjson|lightningcss' restart/skinny/tranches/sk-v12/research/p2/*.md`
- `rg -n 'scalar-ref|Scalar-ref|scalar reference|scalar loop|scalar oracle|Existing scalar|New scalar|Missing scalar|No scalar|x1 scalar|Scalar today|no-op|support-only|same-wave consumer|Same-wave consumer' restart/skinny/tranches/sk-v12/research/p2/*.md`
- Web source spot-checks with `web.open` / `web.find`: asmjson README, sonic-rs README, simdjson stage1 structural indexer, yyjson README, FFmpeg `checkasm.h`, dav1d `tests/checkasm/msac.c`, VideoLAN checkasm, Arm Neon Intrinsics Reference, and Arm C Language Extensions.
