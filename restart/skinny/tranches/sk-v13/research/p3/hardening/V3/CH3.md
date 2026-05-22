# SK-V13 S-P3 V3 CH3 Regression / REDRESS

Pass: S-P3 Synthesis-Plan.
Cycle: V3 CHALLENGE.
Date: 2026-05-22.
Lens: CH3 REGRESSION / REDRESS.
Output: `restart/skinny/tranches/sk-v13/research/p3/hardening/V3/CH3.md`.

## Verdict

ACCEPT.

The current HEAD preserves the V2 CH3 acceptance. The substantive S-P3 fold is
still the packet folded at `9f8bbfce5`, with V2 challenge acceptance at
`b5f58b755`; the later HEAD `eb8051016` adds Omega V5 hardening files and does
not change the SK-V13 P3 authorities, SPEC, or DISPATCH. CH3 finds no reopened
REDRESS route, no silent admitted-row demotion path, and no support-only or
orphan-SIMD dispatch gap.

## Evidence

| Check | Evidence | CH3 reading | Disposition |
|---|---|---|---|
| V2 fold still controls | SPEC authority names P3-A through P3-F and `skinny/RESULTS.md` / `skinny/REDRESS.md` (`restart/skinny/tranches/sk-v13/SPEC.md:10`-`30`), and DISPATCH states P3-A through P3-E are current required inputs with P3-C owning formulas, P3-D telemetry/gate-json, and P3-E the REDRESS ledger (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:31`-`34`). | The V1 defect that treated P3-B through P3-E as absent remains folded. | ACCEPT |
| W1 maintain formula | P3-A records the admitted CSS guard as `Track1_after >= max(lightningcss_open + 1.0, 0.98 * SK-V13-open Track1)` with strict equality (`restart/skinny/tranches/sk-v13/research/p3/p3a-candidate-shortlist.md:119`-`123`). SPEC W1 carries the same formula for the declaration-values row (`restart/skinny/tranches/sk-v13/SPEC.md:462`-`468`). | The one admitted CSS row cannot silently degrade while W1 adds comparator/oracle harnesses. | ACCEPT |
| Telemetry gate consumption | P3-D says printing a field without a gate consumer is producer-only telemetry and rejects the wave (`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:55`-`59`). SPEC requires every emitted field to be consumed by `gate-json`, a CSS companion gate, or rolling SOTA delta in the same wave and rejects producer-only telemetry (`restart/skinny/tranches/sk-v13/SPEC.md:240`-`246`). DISPATCH requires telemetry fields, including `consumer_gate` and G-Omega status, in every wave packet (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:65`-`86`). | Telemetry cannot become a sidecar/prose producer. | ACCEPT |
| REDRESS/pre-block matrix | P3-E supplies route states and a wave-family pre-block matrix (`restart/skinny/tranches/sk-v13/research/p3/p3e-preblocked-ledger.md:58`-`101`, `:188`-`:202`). SPEC Section 20 requires each wave packet to copy the exact applicable row and lists route-state vocabulary plus wave-family blocks (`restart/skinny/tranches/sk-v13/SPEC.md:953`-`986`). DISPATCH mirrors those blocks (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:219`-`251`). | REDRESS constraints are dispatch-grade, not a global reminder. | ACCEPT |
| No silent demotions | SPEC G7 forbids silent demotion (`restart/skinny/tranches/sk-v13/SPEC.md:72`-`82`), rolling delta covers all JSON rows and CSS features and fails backward margin movement (`restart/skinny/tranches/sk-v13/SPEC.md:269`-`283`), and W15 requires rolling delta no silent demotion plus zero-orphan audit (`restart/skinny/tranches/sk-v13/SPEC.md:943`-`949`). DISPATCH requires no silent demotion confirmation on admit (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:253`-`260`). | Admitted JSON/CSS state cannot be downgraded without measured REDRESS or architectural-block/user re-pin evidence. | ACCEPT |
| Same-wave consumer | SKINNY-TRIUMVIRATE requires every primitive/kernel/generated path to include its hot-path caller in the same redress commit and rejects omitted consumers as orphans (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:177`-`187`). SPEC makes no primitive, kernel, generated path, resolver, union substrate, or telemetry producer legal without a same-wave measured consumer (`restart/skinny/tranches/sk-v13/SPEC.md:285`-`306`). DISPATCH makes same-wave consumer path mandatory and lists W10.N/W11.N/W13/W14.N consumer minimums (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:65`-`86`, `:203`-`:210`). | No row-moving wave can land support code without a production consumer. | ACCEPT |
| SIMD zero-orphan | SK-V13 close requires zero aarch64 production orphans (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:84`-`93`). P3-C requires scalar reference, strict checkasm, same-wave production consumer, symbol-path evidence when claiming hot-path movement, and `orphan_count_after = 0` (`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:294`-`349`). SPEC W9 applies the same same-wave zero-orphan predicates to C3/`bbnf-simd` union and forbids relying on later W12 cleanup (`restart/skinny/tranches/sk-v13/SPEC.md:741`-`751`); W12 repeats zero-orphan and checkasm-only rejection (`restart/skinny/tranches/sk-v13/SPEC.md:846`-`854`). DISPATCH mirrors the rule for any wave touching `bbnf-simd` (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:212`-`217`). | SIMD cannot create or retain an orphan for a later cleanup wave. | ACCEPT |
| Bracket accounting | P3-B says the folded SPEC/DISPATCH W0-W15 identifiers are canonical and that every real subwave counts against active skinny-bracket accounting (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:10`-`18`). SPEC makes W10.N/W11.N/W14.N real subwaves count when declared and brackets forward on overflow without dropping pinned rows (`restart/skinny/tranches/sk-v13/SPEC.md:314`-`320`, `:342`-`:345`). DISPATCH repeats the canonical accounting rule (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:184`-`194`). | The P3-B packing aliases cannot hide real wave count or demote pinned work. | ACCEPT |
| No support-only waves | SYNTHESIS invalidates support-only landings unless same-wave wired to a measured consumer (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:202`-`210`). SPEC non-negotiables forbid support-only behavior waves (`restart/skinny/tranches/sk-v13/SPEC.md:303`-`306`), and W5-W7 require row movement, admit, or measured architectural block rather than extraction/e-graph/cost/CSP scaffold (`restart/skinny/tranches/sk-v13/SPEC.md:584`-`593`, `:620`-`:630`, `:658`-`:667`). DISPATCH applies the anti-paper-close rule to W5-W8 (`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:196`-`201`). | Decision, policy, resolver, and scaffold waves cannot close as support plumbing. | ACCEPT |
| G-Omega / mutation block | HANDOFF blocks implementation waves, source changes, gate/report changes, and RESULTS/REDRESS edits before G-Omega (`restart/skinny/tranches/sk-v13/HANDOFF.md:54`-`74`, `:85`-`:91`). SPEC and DISPATCH additionally require S-P3 convergence or user pin before W0/later redress (`restart/skinny/tranches/sk-v13/SPEC.md:32`-`43`; `restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:36`-`47`). | Even with Omega V5 now in HEAD, S-P3 still gates implementation/ledger mutation until this pass converges or is pinned. | ACCEPT |

## Fold Items

None for CH3.

## Verification

- Reviewed current HEAD `eb80510167464d30f5d0cf55ac2c80c60d0445d1`, including the
  S-P3 fold commit `9f8bbfce5` and V2 accepted consolidation commit
  `b5f58b755`.
- Confirmed `git diff --` for the SK-V13 P3 authorities, `SPEC.md`, and
  `DISPATCH-PROMPT.md` was empty before writing this file.
- Confirmed this V3 CH3 file did not exist before creation.
- No source, generated runtime, `skinny/RESULTS.md`, `skinny/REDRESS.md`,
  staging, or commit action was performed.
