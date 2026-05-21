# Pass Omega V1 CH5 Hidden Coupling

| Field | Value |
|---|---|
| Pass | Pass Omega |
| Cycle | V1 CHALLENGE |
| Date | 2026-05-21 |
| Lens | CH5 Hidden Coupling |
| Output | `restart/audit/totality/astral/V1/hardening/CH5.md` |

## Verdict

ACCEPT.

The Omega V1 packet does not introduce a parallel substrate, renamed sidecar,
Track 1 / Track 2 dishonesty, public substrate API, new `BackendShape`, new BIR
variant, or Lock 1 violation under the CH5 lens. The highest-risk terms
(`fact_stream`, `union`, `Track 2`, imported scanner plans, decision engine, and
CSS telemetry) are fenced as proposal-only, output-plane, local/transient, or
G-Omega-gated evidence. CSS fact streams remain admitted output rows and do not
become retained runtime substrate.

## Evidence Table

| Check | Disposition | Evidence | CH5 finding |
|---|---|---|---|
| Governing CH5 scope | ACCEPT | PASS-OMEGA defines CH5 as the audit for parallel substrate, renamed sidecar, Track 1 / Track 2 dishonesty, and Lock 1 violations (`restart/prompts/pass-contracts/PASS-OMEGA.md:51`). ORCHESTRATOR defines CH5 as "No parallel substrate, sidecar producer, renamed-scanner Lock 1 violation, or Track 1 == Track 2 dishonesty; substrate union holds" (`restart/prompts/ORCHESTRATOR.md:81`-`88`) and separately makes "No new BIR variant" plus "No new substrate" CH5-enforced rules (`restart/prompts/ORCHESTRATOR.md:201`-`203`). | Scope is clear and applicable to Ω-C, `locks-diff.md`, `master-plan-diff.md`, and the sibling Omega artifacts. |
| Proposal-only / G-Omega fence | ACCEPT | Ω-C states the 16-lock count stays fixed and no new lock, directive, BIR variant, `BackendShape`, public substrate API, or retained sidecar is authorized (`restart/audit/totality/astral/V1/ΩC-locks-amendments.md:9`-`13`). `locks-diff.md` repeats that the patch is proposed-only and adds none of those surfaces (`restart/audit/totality/astral/V1/locks-diff.md:3`-`10`), then its governance footer forbids using proposed text as permission for source edits, RESULTS/REDRESS writes, new BIR variants, `BackendShape` expansion, public substrate APIs, retained sidecars, or SK-V13 W0 before G-Omega (`restart/audit/totality/astral/V1/locks-diff.md:391`-`401`). | The packet is not self-authorizing. Any future substrate/API/shape expansion remains outside this V1 CH5 acceptance and requires G-Omega plus explicit amendment. |
| Track 2 is not a second substrate | ACCEPT | Lock 1 Hunk 2 says Skinny Track 2 is a substrate-ceiling probe, not a second substrate, and it measures whether the same `runtime::tape` plus `bbnf-simd` APIs can reach SOTA without hidden runtime identity, parser-owned sidecars, or parallel representation (`restart/audit/totality/astral/V1/locks-diff.md:61`-`68`). | No Track 1 / Track 2 dishonesty found. Track 2 is framed as a measurement probe against the same APIs codegen will emit, not as a retained alternate runtime. |
| CSS fact streams stay output-plane | ACCEPT | Lock 1 Hunk 2 says fact streams are output-plane contracts, not retained internal sidecars, and that `css_l4_declaration_value_fact_stream` may be admitted only with comparator/oracle provenance and gate-consumed telemetry (`restart/audit/totality/astral/V1/locks-diff.md:77`-`82`). Lock 8 Hunk 5 says the CSS declaration-values row is a SK-V12 `PASS-ADMIT` row on that fact stream, not full CSS parity, universal grammar closure, or SK-V13 close authority (`restart/audit/totality/astral/V1/locks-diff.md:139`-`146`). Ω-B states the same scoped CSS truth (`restart/audit/totality/astral/V1/ΩB-skinny-lessons.md:45`-`49`). | CSS fact streams remain output rows and telemetry. They do not imply EventTape, retained runtime identity, or a new substrate. |
| Union/substrate candidate fence | ACCEPT | Lock 1 Hunk 2 requires every e-graph candidate, backend rewrite, imported scanner plan, union candidate, and SIMD consumer to declare `substrate_target`, `retention_lifetime`, and `policy_owner`; it rejects retained class/mask streams, parser-owned cursor/list state, public substrate APIs, `UnionTape`, or a second tape unless G-Omega explicitly amends Lock 1 (`restart/audit/totality/astral/V1/locks-diff.md:84`-`93`). It also keeps REDRESS 96/97/98 as binding substrate-ceiling history requiring material differential, proofs, same-wave consumer, strict gate, rollback path, and abrogate threshold before any reopen (`restart/audit/totality/astral/V1/locks-diff.md:95`-`101`). | The union language is tightly fenced. It does not smuggle in a retained sidecar or parallel substrate. |
| Master-plan union receiver wording | ACCEPT | Ω-D proposes MP.NW10 as "Fresh union-substrate variant or architectural block with material differential beyond REDRESS 96/97/98" (`restart/audit/totality/astral/V1/ΩD-master-plan-reconciliation.md:78`) and maps SK-V13 G3 to union admission/block evidence (`restart/audit/totality/astral/V1/ΩD-master-plan-reconciliation.md:90`). The companion diff uses the same receiver language (`restart/audit/totality/astral/V1/master-plan-diff.md:65`). Ω-F refusal conditions reject support-only primitives, union substrates, resolver infrastructure, or codegen paths without same-wave measured consumer (`restart/audit/totality/astral/V1/ΩF-migration-handoff.md:90`). | The "union" receiver is a future measured-admission-or-block wave, not a current Lock 1 amendment that creates a second substrate. |
| Five-shape and BIR fence | ACCEPT | Lock 10 Hunk 7 states the five `BackendShape` variants remain the V1 search domain and that a new `BackendShape`, directive, or BIR variant is not admitted by cost evidence and remains G-Omega-gated (`restart/audit/totality/astral/V1/locks-diff.md:193`-`201`). Ω-E requires the decision-engine fold to preserve five `BackendShape` values and introduce no directive, BIR variant, `BackendShape`, substrate, or grammar-specific generic branch (`restart/audit/totality/astral/V1/ΩE-skinny-corpus.md:21`). Ω-F says any downstream SPEC-local authorization of a new directive, BIR variant, `BackendShape`, public substrate API, or grammar-specific generic behavior must return REVISE (`restart/audit/totality/astral/V1/ΩF-migration-handoff.md:92`). | No new `BackendShape` or BIR route is admitted. Decision-engine work is constrained to the existing five-shape canon. |
| Comparator / Track honesty | ACCEPT | Lock 8 Hunk 5 requires comparator-plane provenance across same corpus, output plane, host, strictness, freshness, sidecar status, and gate-consumed artifact provenance (`restart/audit/totality/astral/V1/locks-diff.md:148`-`155`). It also requires Track 1, Track 2, serde, and sonic strict equality for the same semantic output plane before direct digest hashing or SIMD sub-hash acceleration can admit a row (`restart/audit/totality/astral/V1/locks-diff.md:166`-`171`). Ω-E routes Track 1 / Track 2 Mbps and output plane into the SK-V13 common telemetry schema (`restart/audit/totality/astral/V1/ΩE-skinny-corpus.md:19`). | Track 1 and Track 2 are not collapsed. The proposed telemetry makes the distinction explicit and gate-consumed. |
| Skinny corpus substrate alignment | ACCEPT | Ω-E proposes updating `SUBSTRATE.md` to carry SK-V13 obligations: fresh union admission or architectural block, zero aarch64 production orphans, no retained sidecar classifier state, grammar-owned quote/escape/control policy, same-wave measured SIMD/ASM consumer, and preserved Lock 1 one-substrate language (`restart/audit/totality/astral/V1/ΩE-skinny-corpus.md:26`). Its fold receiver for `SUBSTRATE.md` names single-substrate/output-plane taxonomy and zero-orphan obligations (`restart/audit/totality/astral/V1/ΩE-skinny-corpus.md:38`). | The skinny corpus receiver aligns with CH5 rather than weakening it. |

## Fold Actions

None required for CH5. No affected files.

## G-Omega Block

This CH5 lens does not block G-Omega presentation. Overall G-Omega remains
subject to the Pass Omega consolidated CHALLENGE result and any other open
REVISE dispositions under the convergence rules (`restart/prompts/pass-contracts/PASS-OMEGA.md:86`-`94`,
`restart/prompts/ORCHESTRATOR.md:118`-`123`).
