# SK-V12 Pass Alpha Hardening V3 - CH6 Anti-Paper-Close

Date: 2026-05-20.
Pass: Pass Alpha SK-V11 -> SK-V12 under `USER-PIN-W1-CSS-L4-SOTA.md`.
Lens: CH6 anti-paper-close.

## Verdict

REVISE.

The current Alpha A/B/E/F, `SYNTHESIS.md`, and `HANDOFF.md` preserve the V2 CH6
folds: no S-P3 jump before G-Alpha/S-P1/S-P2, CSS L4 before Sheets/BBNF-self,
strict `generated_track1_mbps > lightningcss_mbps + 1`, gate-consumed telemetry,
and a usable G-Alpha seed table. The paper-close blocker is the standalone
G-Alpha presentation, which still claims V2 convergence and presents stale
pre-pin authority. That file must be revised before G-Alpha can stand.

## Findings

1. REVISE - `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:28`
   says the Pass Alpha packet "converged under V2 hardening" and
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:49`
   records `G-Alpha PASS`. That is a paper close: V2 consolidated disposition is
   still `REVISE` at
   `restart/skinny/tranches/sk-v12/research/alpha-hardening/V2/CONSOLIDATED.md:6`,
   states CH5 must fold before G-Alpha at
   `restart/skinny/tranches/sk-v12/research/alpha-hardening/V2/CONSOLIDATED.md:13`,
   and requires V3 folds at
   `restart/skinny/tranches/sk-v12/research/alpha-hardening/V2/CONSOLIDATED.md:27`.
   Orchestrator convergence does not advance while unresolved REVISE remains
   (`restart/prompts/ORCHESTRATOR.md:118`,
   `restart/prompts/ORCHESTRATOR.md:123`), and Pass Alpha requires G-Alpha only
   after CHALLENGE convergence
   (`restart/prompts/pass-contracts/PASS-ALPHA.md:167`,
   `restart/prompts/pass-contracts/PASS-ALPHA.md:182`).

2. REVISE - The same G-Alpha presentation reimports the obsolete baseline
   selection. It presents "generated non-JSON baseline first" as selectable from
   CSS L4, Sheets, or BBNF-self at
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:35` and
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:36`.
   The user pin requires CSS L4 first and makes Sheets/BBNF-self fallbacks only
   after a CSS L4 redress attempt
   (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:20`,
   `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:21`).
   The current contract folds this correctly in
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:77`,
   `restart/skinny/tranches/sk-v12/HANDOFF.md:64`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:35`,
   and `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:40`.

3. REVISE - The G-Alpha presentation omits the strict lightningcss admission
   floor and gate-consumed telemetry that Pass Alpha says must be presented at
   G-Alpha. The current G-Alpha body has no `> lightningcss_mbps + 1` row, no
   CSS output-plane/equality/telemetry schema, and no S-P1/S-P2/S-P3 plus W0-W5
   intervention table in
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:26`.
   Pass Alpha requires the goalset table and telemetry schema at G-Alpha
   (`restart/prompts/pass-contracts/PASS-ALPHA.md:171`,
   `restart/prompts/pass-contracts/PASS-ALPHA.md:173`). The sufficient table
   exists in the folded Alpha packet at
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:256` and
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:211`,
   with strict admission repeated at
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:273` and
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:228`.

4. REVISE - The G-Alpha presentation still says union/event/class-column/
   streaming-cursor/class-lane/sidecar substrate routes remain pre-blocked at
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:44`.
   The user pin unblocks union and ASM-gen categories at category level
   (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:39`,
   `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:58`), and the
   current folded contract carries that in
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:219`,
   `restart/skinny/tranches/sk-v12/HANDOFF.md:66`, and
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:47`.

## Checks That Passed In The Folded Alpha Packet

- No S-P3 jump: `restart/skinny/tranches/sk-v12/SYNTHESIS.md:237` and
  `restart/skinny/tranches/sk-v12/HANDOFF.md:105` require G-Alpha, then S-P1,
  S-P2, and S-P3 under the pin; Alpha-F matches at
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:148`.
- CSS redress before fallback: folded in `SYNTHESIS.md:77`,
  `HANDOFF.md:64`, `alpha-E-candidate-shortlist.md:349`, and
  `alpha-F-contract-draft.md:105`.
- Strict lightningcss gate: folded in
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:34`,
  `alpha-E-candidate-shortlist.md:117`, and
  `alpha-F-contract-draft.md:228`; equality at `+1` is explicitly a miss in
  `alpha-B-competitor-deltas.md:55`.
- Consumed telemetry: folded in `SYNTHESIS.md:183`, `HANDOFF.md:139`,
  `alpha-B-competitor-deltas.md:84`, and `alpha-F-contract-draft.md:173`.
- No future-phase close promise: Alpha-A and Alpha-B classify CSS evidence as
  absent/unmeasured at
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-A-results-extraction.md:52`
  and `restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:102`;
  routed SK-V13 language is conditional on measured FIXPOINT in
  `SYNTHESIS.md:90` and `alpha-F-contract-draft.md:119`.

## Required Fold

Replace the stale G-Alpha presentation with the pin-aware table and close
summary from the current `SYNTHESIS.md` / Alpha-F packet, remove the V2
convergence claim, and present G-Alpha only after V3 hardening convergence.
