# Pass Omega V9 Hardening V2 - CH5 Hidden Coupling / Overfit / Admission Honesty

Date: 2026-05-28.
Lens: CH5 hidden coupling, overfit, and admission honesty.
Source HEAD: `9d336c606`.
Disposition: ACCEPT.

## Verdict

ACCEPT. Folded V9 preserves the PASS-IMPL V1 overfit findings as blockers and
routes the repairs to SK-V15 W0-W11. I found no V9 source-packet path that
re-admits the CSS 24-row broadcast, `CSS_GENERATED_RS`, brace-counter /
`full_parse` mismatch, or fact-stream-only CSS parse as live proof. CSS typed
Value provider and same-workload `cssparser` retime remain W5/W6 work. Lock
14/16 gate restoration, codegen leak abrogation, Pattern H provenance, Decision
Engine activation, lowerers, and FNV quarantine remain SK-V15 wave obligations,
not Omega paper-close claims.

No folded V9 source creates a new escape hatch through source inventory, stale
comparators, x86 diagnostics, or SK-V16 deferral. Apple M5 Max / aarch64-only
admission and no-warm-bench constraints remain load-bearing.

## Sources Checked

- `restart/audit/totality/astral/V9/ΩA-coherence-audit.md`
- `restart/audit/totality/astral/V9/ΩB-skinny-lessons.md`
- `restart/audit/totality/astral/V9/ΩC-locks-amendments.md`
- `restart/audit/totality/astral/V9/ΩD-master-plan-reconciliation.md`
- `restart/audit/totality/astral/V9/ΩE-skinny-corpus.md`
- `restart/audit/totality/astral/V9/ΩF-migration-handoff.md`
- `restart/audit/totality/astral/V9/master-plan-diff.md`
- `restart/audit/totality/astral/V9/locks-diff.md`
- `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`
- `restart/skinny/tranches/sk-v15/SPEC.md`
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`

## Evidence Matrix

| Check | Result | Evidence |
|---|---|---|
| CSS broadcast is not live proof | ACCEPT | PASS-IMPL V1 identifies all 24 CSS L4 admits as one measurement broadcast (`CONSOLIDATED-AUDIT.md:21`-`25`). SK-V15 close requires no CSS 24-row broadcast admit (`SPEC.md:54`-`55`), marks W1 as CSS admission honesty with no W8R live admit (`SPEC.md:175`), and gives `DEP-W1-CSS-BROADCAST` diagnostic-only status (`SPEC.md:194`). V9 repeats this as diagnostic/open, not proof (`ΩB:21`, `ΩD:105`, `ΩF:85`-`86`, `master-plan-diff.md:142`). |
| `CSS_GENERATED_RS` is not admitted | ACCEPT | PASS-IMPL V1 classifies the CSS generator as a 646-line hand-written string literal (`CONSOLIDATED-AUDIT.md:31`). SK-V15 requires retirement from live CSS admission (`SPEC.md:56`-`58`) and routes the dependency to W6 after W5 typed proof (`SPEC.md:195`; `DISPATCH-PROMPT.md:82`). V9 refuses `CSS_GENERATED_RS` as live admission (`ΩD:105`-`107`, `ΩF:87`-`88`, `master-plan-diff.md:155`-`160`). |
| Brace-counter / `full_parse` mismatch and fact-stream-only CSS parse remain blocked | ACCEPT | PASS-IMPL V1 says `CssFullParseSummary` is only counters and CSS `parse()` returns fact-stream string, not a Value API (`CONSOLIDATED-AUDIT.md:29`, `:56`-`58`). SK-V15 requires retirement of `CssFullParseSummary`, fact-stream-only `parse()`, and brace-counter admission (`SPEC.md:56`-`58`) through `DEP-W6-CSS-SUMMARY-FACT-STREAM` (`SPEC.md:196`; `DISPATCH-PROMPT.md:83`). V9 carries the same refusal (`ΩE:207`-`210`, `ΩF:87`-`88`). |
| CSS typed Value provider and same-workload retime stay W5/W6 | ACCEPT | SK-V15 W5 is the CSS typed Value provider and W6 is same-workload retime / old-proof retirement (`SPEC.md:179`-`180`, `:358`-`380`). The dispatcher requires fresh typed `cssparser` comparator evidence in W6 (`DISPATCH-PROMPT.md:205`-`212`). V9 MASTER operations preserve W5 and W6 as active pending rows (`master-plan-diff.md:147`-`148`) and state that `lightningcss` only counts after comparable CSSOM/value output (`master-plan-diff.md:126`-`129`). |
| Lock 14/16, codegen leaks, Pattern H, Decision, lowerers, and FNV are not paper-closed | ACCEPT | SK-V15 routes W2 gate restoration, W3 codegen leak abrogation, W4 Pattern H, W7 Decision, W8/W9 lowerers, and W10 FNV (`SPEC.md:176`-`184`). Dependency rows bind the same work to `DEP-W3-W6-CSS-PROVIDER-TEMPLATE`, `DEP-W4-PATTERN-H-PROVENANCE`, `DEP-W7-DECISION-SPINE`, `DEP-W8-LOWERERS-A`, `DEP-W9-LOWERERS-B`, and `DEP-W10-FNV-QUARANTINE` (`SPEC.md:197`-`203`). V9 mirrors those rows in the active receiver table (`master-plan-diff.md:143`-`152`) and Omega-F's migration receiver (`ΩF:89`-`114`). |
| Source inventory cannot close primitives | ACCEPT | SK-V15 rejects source-present unwired primitives (`SPEC.md:121`-`122`) and requires scalar oracle, parity/checkasm, and same-wave consumer (`SPEC.md:140`-`142`). The V9 locks addendum says source inventory and macro names are not admission (`locks-diff.md:71`). Omega-D explicitly rejects source-present SIMD/ASM admission without consumer and row movement (`ΩD:111`-`112`). |
| Stale comparators and x86 diagnostics cannot close | ACCEPT | SK-V15 comparator classes make stale sidecars, W8R CSS tuple, `lightningcss` before comparable CSSOM output, and x86/AVX-512 diagnostics planning-only (`SPEC.md:90`-`92`). Section 1 says Apple M5 Max / aarch64 is the only admission host, x86/AVX-512 diagnostic only, and no warm benches (`SPEC.md:135`-`137`). V9 MASTER and Omega-D preserve this (`master-plan-diff.md:121`-`129`, `ΩD:72`-`78`, `:113`). |
| SK-V16 deferral is not a close route | ACCEPT | SK-V15 says SK-V16 routing is routed remainder after proof, not SK-V15 close evidence (`SPEC.md:82`-`84`, `:453`-`455`). V9 MASTER says missing dependency proof blocks the exit gate and does not route to SK-V16 as close evidence (`master-plan-diff.md:159`-`161`). Omega-D and Omega-F refuse SK-V16 deferral as close evidence (`ΩD:118`-`119`, `ΩF:128`-`130`). |
| Apple M5 Max / aarch64 and no warm benches preserved | ACCEPT | SK-V15 global close preserves JSON rows measured on native Apple M5 Max / aarch64 (`SPEC.md:52`-`53`) and Section 1 requires Apple M5 Max / aarch64-only admission plus no warm benches (`SPEC.md:135`-`137`). DISPATCH preflight verifies Apple M5 Max / aarch64 as the only admission target (`DISPATCH-PROMPT.md:56`) and rejects warm benches and x86 admission anchors (`DISPATCH-PROMPT.md:345`-`346`). V9 Omega-C and Omega-F carry the same aarch64-only posture (`ΩC:64`, `:128`-`130`; `ΩF:30`-`34`). |

## Findings

None.

## Notes

The prior V1 hardening reports still contain historical `T-P2 V5`,
`CRUD/SPEC`, and source-commit wording from the V1 REVISE cycle. Those hits are
not part of the folded V9 source packet audited here and do not create a CH5
admission-honesty defect.
