# S-P2 V6 CH6 - Anti-Paper-Close Review

Title: CH6 anti-paper-close review of the user-authorized exceptional S-P2 V6.

Scope: current HEAD `f20fbc46`, with primary review of
`restart/skinny/tranches/sk-v8/SYNTHESIS.md`,
`restart/skinny/tranches/sk-v8/SPEC.md`,
`restart/skinny/tranches/sk-v8/HANDOFF.md`, and
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`
against ORCHESTRATOR §3W/§3Z, the S-P2 CH6 rule, anti-paper-close,
strict-vs-strict comparator discipline, Lock 14 grammar-neutrality, Lock 1
substrate cardinality, no new directive/BIR/substrate, no deferrals, and no
automatic S-P3 dispatch from one V6 ACCEPT.

Verdict: ACCEPT.

Confidence: 94%.

## Blockers

None.

## Basis

1. **The exceptional V6 authorization is recorded without normalizing a V6 path.**
   ORCHESTRATOR requires two consecutive qualifying ACCEPT cycles or a user pin,
   bars next-pass dispatch before convergence, and says a pass that reaches V5
   without convergence escalates with the unresolved REVISE named
   (`restart/prompts/ORCHESTRATOR.md:118`-`restart/prompts/ORCHESTRATOR.md:127`).
   The S-P2 prompt repeats the same S-P3 gate and V5 ceiling
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:155`-`restart/prompts/skinny/PASS-2-RESEARCH.md:162`,
   `restart/prompts/skinny/PASS-2-RESEARCH.md:184`-`restart/prompts/skinny/PASS-2-RESEARCH.md:195`,
   `restart/prompts/skinny/PASS-2-RESEARCH.md:250`-`restart/prompts/skinny/PASS-2-RESEARCH.md:252`).
   Current HEAD now says the user revised the hard-ceiling instruction on
   2026-05-17 by authorizing an exceptional V6 challenge, but also says V6 does
   not by itself dispatch S-P3 and, if V6 ACCEPTs, is only the first qualifying
   cycle after V5 REVISE unless the user pins S-P2 final or explicitly authorizes
   another over-ceiling cycle
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:96`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:102`,
   `restart/skinny/tranches/sk-v8/SYNTHESIS.md:191`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:195`,
   `restart/skinny/tranches/sk-v8/SPEC.md:454`-`restart/skinny/tranches/sk-v8/SPEC.md:460`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:80`).

2. **No S-P3, W3, or implementation dispatch is paper-closed by the research
   nomination.** S-P2 selects nothing and sequences nothing
   (`restart/prompts/skinny/PASS-2-RESEARCH.md:3`-`restart/prompts/skinny/PASS-2-RESEARCH.md:11`).
   The packet preserves that boundary: W3 still requires W0/W1 closure, a fresh
   plan, exact owner paths, a same-wave production consumer, revert protocol,
   measurement thresholds, measured-path validation proof, and challenge
   acceptance
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:179`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:189`,
   `restart/skinny/tranches/sk-v8/SPEC.md:447`-`restart/skinny/tranches/sk-v8/SPEC.md:452`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:84`-`restart/skinny/tranches/sk-v8/HANDOFF.md:90`).
   `G-Alpha closed` remains W0-only, and W1-W6 require W0 closure plus plan
   augmentation before dispatch
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:267`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:273`,
   `restart/skinny/tranches/sk-v8/SPEC.md:590`-`restart/skinny/tranches/sk-v8/SPEC.md:596`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:5`-`restart/skinny/tranches/sk-v8/HANDOFF.md:7`).

3. **Strict-vs-strict comparator discipline is preserved.** The packet classifies
   same-run strict anchors separately from flaw probes and sidecar planning
   signals (`restart/skinny/tranches/sk-v8/SPEC.md:44`-`restart/skinny/tranches/sk-v8/SPEC.md:55`,
   `restart/skinny/tranches/sk-v8/SYNTHESIS.md:217`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:225`).
   The executable refusal rule rejects strict admission unless comparator plane,
   strictness, freshness/native-anchor status, and measured-row validation all
   hold; deferred strictness, view-boundary UTF-8, stale sidecars, C++ sidecar-only
   evidence, and plane mismatch are guard telemetry only
   (`restart/skinny/tranches/sk-v8/SPEC.md:117`-`restart/skinny/tranches/sk-v8/SPEC.md:123`).
   SYNTHESIS keeps same-run sonic-strict substrate-guard signals separate from
   simdjson DOM sidecars and historical SK-V6 rows
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:104`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:131`).

4. **Lock 14 grammar-neutrality is a current gate, not a deferred assertion.**
   SPEC requires public API, grammar-branch, primitive/table, role/fact-boundary,
   template/provider-boundary, and non-JSON proof scans for generic edits
   (`restart/skinny/tranches/sk-v8/SPEC.md:247`-`restart/skinny/tranches/sk-v8/SPEC.md:269`).
   SYNTHESIS states the S-P2 fold requires generated per-grammar byte-set tables
   plus fixed neutral structural-role ordinals, with grammar meaning remaining in
   generated grammar modules
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:157`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:167`,
   `restart/skinny/tranches/sk-v8/SYNTHESIS.md:209`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:215`).
   SC-6 exercises JSON, CSS L4, Sheets, and arbitrary user grammars through
   generated byte sets and opaque class ordinals, including reused punctuation,
   doubled-quote Sheets strings, and empty-alphabet grammars routing away from the
   retained structural representation
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:338`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:535`).

5. **Lock 1 cardinality and no-new-substrate boundaries remain intact.** SC-6
   treats SC-6-L1-R1 as a Pass Omega candidate that does not bind the V1 spec
   until ratified
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:247`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:261`).
   SYNTHESIS, SPEC, and SC-6 preserve the same discriminant: a retained structural
   projection passes only if it replaces the offset-tape as the one retained
   `Tape`; if it survives beside the old offset append path, it is a sidecar and
   fails Lock 1
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:133`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:167`,
   `restart/skinny/tranches/sk-v8/SPEC.md:441`-`restart/skinny/tranches/sk-v8/SPEC.md:492`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:145`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:236`).
   SC-6 and SYNTHESIS also bar `UnionTape`, a sixth `BackendShape`, BIR variant,
   BBNF directive, public substrate type, public generic grammar API, and
   grammar-name branch
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:157`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:164`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:301`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:328`,
   `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:657`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:666`).

## Disposition Of V5 CH4 Blocker

Closed for CH6.

V5 CH4's blocker was governance, not substrate design: the packet still budgeted
a normal post-V5 qualifying cycle even though S-P2 hard-capped at V5
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V5/CH4.md:13`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V5/CH4.md:16`,
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V5/CH4.md:35`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/V5/CH4.md:38`).
The V5 consolidation required user escalation: a user pin, an explicit
hard-ceiling override authorizing exceptional V6, or blocked S-P2
(`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:33`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V5-CONSOLIDATED.md:78`).
Current HEAD folds the second route exactly. It records the user authorization
for exceptional V6 and blocks automatic S-P3 unless the user pins S-P2 final or
authorizes another over-ceiling cycle
(`restart/skinny/tranches/sk-v8/SYNTHESIS.md:96`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:102`,
`restart/skinny/tranches/sk-v8/SYNTHESIS.md:191`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:195`,
`restart/skinny/tranches/sk-v8/SPEC.md:454`-`restart/skinny/tranches/sk-v8/SPEC.md:460`,
`restart/skinny/tranches/sk-v8/HANDOFF.md:71`-`restart/skinny/tranches/sk-v8/HANDOFF.md:80`).

## Residual Non-Blocking Risks

1. **Exceptional-cycle governance remains easy to mis-consolidate.** A V6 ACCEPT
   still would not be convergence by itself. The V6 consolidator must state that
   one V6 ACCEPT is only the first qualifying cycle after V5 REVISE unless the
   user pins final or authorizes a further over-ceiling cycle
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:191`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:195`;
   `restart/skinny/tranches/sk-v8/SPEC.md:454`-`restart/skinny/tranches/sk-v8/SPEC.md:460`).

2. **SC-6's Lock 14 grep line must not be reused as live implementation evidence.**
   SC-6 includes a Lock-14-style `rg` command and says it returns zero generic
   role leaks
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:508`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:535`).
   This is non-blocking because SPEC independently requires the actual scans and
   non-JSON proof when generic crates are edited
   (`restart/skinny/tranches/sk-v8/SPEC.md:247`-`restart/skinny/tranches/sk-v8/SPEC.md:269`),
   and because V6 authorizes challenge review, not implementation admission. A
   later W3 plan must run current workspace paths and exclude allowed generated
   per-grammar surfaces rather than cite SC-6 prose as verification.

3. **Pass Omega timing remains a real implementation hazard.** SC-6-L1-R1 is only
   a skinny-track Pass Omega candidate until ratified; W3 must either consume the
   ratified refinement or prove Lock 1 as written while routing the Omega
   residual explicitly
   (`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:685`-`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md:695`,
   `restart/skinny/tranches/sk-v8/SYNTHESIS.md:254`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:265`,
   `restart/skinny/tranches/sk-v8/SPEC.md:462`-`restart/skinny/tranches/sk-v8/SPEC.md:464`).

4. **Tier A can still be oversold later as string-plane closure.** The current
   packet blocks that by keeping Tier A to structural-class cursor migration and
   assigning string-boundary, quote/backslash/parity, CostFacts-template, and
   non-JSON production migration to Tier B
   (`restart/skinny/tranches/sk-v8/SYNTHESIS.md:133`-`restart/skinny/tranches/sk-v8/SYNTHESIS.md:155`,
   `restart/skinny/tranches/sk-v8/SPEC.md:431`-`restart/skinny/tranches/sk-v8/SPEC.md:439`).
   Later planners must preserve the split.

## Required Folds If REVISE

N/A. Verdict is ACCEPT.
