# CH6 Anti-Paper-Close Challenge - SK-V13 Alpha V2

Disposition: ACCEPT.

Alpha V2 closes the V1 CH6 blocker. The current packet no longer permits a
paper close by support-only work, future-tranche deferral, ordinary measured
reject/fixpoint language, weaker scoping prose, old GO/slack carry-forward, or
missing rolling-delta accounting. The downstream risk is now enforcement in
S-P3, not a defect in the Alpha V2 contract.

## Findings

### F1 - ACCEPT: close is full ADMIT or architectural-block only

`SYNTHESIS.md` now makes the addendum the conflict authority over any clause
that treats CSS as one-row complete, treats `parse_only` as diagnostic-only,
permits JSON below strict sonic-rs, or allows ordinary fixpoint close without
architectural-level intrinsic-block evidence (`SYNTHESIS.md:25-28`). The close
condition is explicit: every remaining row/feature must fully ADMIT or carry
architectural-block proof; implementation-limited misses are reopens and force
REJECT plus immediate SK-V14 bracketing (`SYNTHESIS.md:32-36`).

This blocks ordinary measured-reject and REDRESS-history close. Measured
rejections remain evidence for redress routing, not close authority.

### F2 - ACCEPT: support-only, scaffold-only, and deferred closures are blocked

The V1 weakness was inherited scoping prose: scaffold-only W0, deferred
recognizer/materialization work, optional CSS waves, and production-split
deferrals could be copied into S-P3. Alpha V2 now blocks that route in the
master contract. S-P3 must target G1-G7, every behavior wave must move a row or
record architectural-block proof, support-only landings are invalid unless
same-wave wired to a measured consumer, and pinned work cannot be deferred to a
future tranche except through the automatic Pass Alpha bracket after rejected
close (`SYNTHESIS.md:206-217`).

The handoff carries the same refusal condition: support-only primitives, union
substrates, resolver infrastructure, or codegen paths reject without same-wave
measured consumers (`HANDOFF.md:147-148`). It also rejects source/gate edits
without telemetry and rolling delta updates (`HANDOFF.md:158`).

### F3 - ACCEPT: weaker scoping prose is subordinate

Alpha V2 adds the missing precedence guard. `SYNTHESIS.md`, `HANDOFF.md`, and
the 2026-05-21 addendum override weaker scoping prose, and S-P3 must not inherit
labels such as optional, fallback, diagnostic, support-only, scaffold-only, or
future-tranche for pinned CSS/JSON/G2-G7 work. Those items must become admitted
row targets, architectural-block proofs, or user re-pin issues
(`SYNTHESIS.md:218-222`).

`HANDOFF.md` makes this operational by requiring REVISE for any downstream plan
that inherits those weaker labels instead of converting them to row targets,
architectural-block proofs, or user re-pin issues (`HANDOFF.md:149-152`).

### F4 - ACCEPT: old GO/slack/fixpoint outcomes are not counted as full-SOTA

Alpha-C now states that REDRESS-119/120 are historical evidence only and no
longer close JSON direct residuals or the parse-only plane (`alpha-C:13-21`).
It separately pre-blocks claiming production SIMD admission from the W4
microbench alone (`alpha-C:93-94`) and requires direct rows to reopen under
strict equality against sonic-rs with `Track 1 > sonic-rs strict + 1 Mbps`
(`alpha-C:100-105`).

The mandatory reopen/accounting set is explicit: 51 JSON rows, including 10
absent typed rows, plus the 23 CSS parity features remaining after the SK-V12
row. REDRESS history and old GO/slack outcomes are not closure authority under
the addendum (`alpha-C:217-221`).

### F5 - ACCEPT: rolling delta is a close gate

`SYNTHESIS.md` requires `restart/skinny/ROLLING-SOTA-DELTA.md` for every JSON
row/plane and every CSS feature, with `row`, `plane`, `T1_current`, `T1_sota`,
`margin`, and `tranche_admitted` columns (`SYNTHESIS.md:187-194`). The table is
defined as a close gate, not an appendix; negative margins remain open and
regressions fail G7 unless resolved by architectural-block/user re-pin
(`SYNTHESIS.md:196-200`).

`HANDOFF.md` also tells S-P3 to include rolling-delta production and gate
consumption in the concrete wave plan (`HANDOFF.md:133-136`).

### F6 - ACCEPT: Alpha-E fanout is bounded by row-consuming gates

Alpha-E still has broad candidate families, but the global gates now prevent
them from becoming paper waves. Every family requires full CSS parity, all 51
JSON rows above sonic-rs strict or architectural-block proof, no lossy/permissive
SOTA anchors, behavior waves that move a row or prove intrinsic block, and
same-wave consumers for every primitive, union route, resolver rule, or CSS
production (`alpha-E:23-46`).

Its cost/cap fold also makes W5 non-close-bearing infrastructure and requires
E2/E4/E5 to be consumed by CSS/JSON row gates (`alpha-E:448-455`). This is enough
for CH6: S-P3 may plan scaffolding as dependency work, but it cannot count that
work as behavior closure.

## Required Carry-Forward

No Alpha V2 file edit is required for CH6. S-P3 must preserve these checks
verbatim in `SPEC.md` / `DISPATCH-PROMPT.md`:

1. Behavior-wave exit requires a named row movement or architectural-block proof.
2. Scaffold/support/checkasm/resolver/codegen-only landings are not close-bearing.
3. Old A/GO, slack, measured-reject, and REDRESS-119/120 fixpoint results are
   baseline history only under the addendum.
4. Weaker scoping labels are non-authoritative for pinned CSS/JSON/G2-G7 work.
5. `ROLLING-SOTA-DELTA.md` production and gate consumption are mandatory close
   conditions.

With those carry-forward constraints, Alpha V2 CH6 accepts.
