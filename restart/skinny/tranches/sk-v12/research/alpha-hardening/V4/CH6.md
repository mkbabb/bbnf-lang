# SK-V12 Pass Alpha Hardening V4 - CH6 Anti-Paper-Close

Date: 2026-05-20.
Pass: Pass Alpha SK-V11 -> SK-V12 under `USER-PIN-W1-CSS-L4-SOTA.md`.
Lens: CH6 anti-paper-close.

## Verdict

PASS.

No CH6 blocker remains after the pin-aware G-Alpha fold. The current packet
does not authorize implementation waves, does not skip the G-Alpha -> S-P1 ->
S-P2 -> S-P3 sequence, does not substitute Sheets/BBNF-self for CSS before a
measured CSS redress attempt, and does not claim `G-Alpha PASS` before V4
hardening acceptance.

## Findings

1. PASS - No S-P3 jump before G-Alpha/S-P1/S-P2. The orchestrator says folding
   must complete before advancement and the next pass does not dispatch until
   convergence holds (`restart/prompts/ORCHESTRATOR.md:116`,
   `restart/prompts/ORCHESTRATOR.md:123`). PASS-ALPHA places SK-V12 profile
   work post-G-Alpha and says P1 dispatch follows G-Alpha closure
   (`restart/prompts/pass-contracts/PASS-ALPHA.md:165`,
   `restart/prompts/pass-contracts/PASS-ALPHA.md:178`,
   `restart/prompts/pass-contracts/PASS-ALPHA.md:182`). The folded packet
   matches: `SYNTHESIS.md` requires G-Alpha, then S-P1/S-P2/S-P3 before
   implementation authority (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:8`,
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:237`), `HANDOFF.md` repeats the
   same sequence (`restart/skinny/tranches/sk-v12/HANDOFF.md:105`,
   `restart/skinny/tranches/sk-v12/HANDOFF.md:170`), Alpha-F says no wave
   packet is authority before those passes (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:11`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:148`),
   and G-Alpha asks only to authorize that pass sequence
   (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:38`,
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:44`,
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:47`).

2. PASS - CSS redress remains before fallback. The user pin makes CSS L4 first
   and allows Sheets/BBNF-self only after a CSS L4 redress attempt fails
   (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:20`). The current
   packet preserves that ordering in `SYNTHESIS.md` ADMIT/FIXPOINT and blocked
   route text (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:77`,
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:211`,
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:249`), `HANDOFF.md`
   (`restart/skinny/tranches/sk-v12/HANDOFF.md:64`,
   `restart/skinny/tranches/sk-v12/HANDOFF.md:89`,
   `restart/skinny/tranches/sk-v12/HANDOFF.md:158`), Alpha-E
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:35`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:67`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:137`),
   Alpha-F (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:40`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:105`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:154`),
   and the G-Alpha close contract
   (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:56`,
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:77`).

3. PASS - The strict lightningcss gate and consumed telemetry are explicit.
   The packet rejects the old internal baseline lift and requires generated CSS
   Track 1 strictly `> lightningcss_mbps + 1` on the same corpus/output plane
   with strict equality (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:31`,
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:42`,
   `restart/skinny/tranches/sk-v12/HANDOFF.md:53`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:34`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:55`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:38`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:72`,
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:58`).
   The required CSS gate/provenance fields are not producer-only prose: they
   must be consumed by the gate or companion report, and missing/stale fields
   reject the wave (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:185`,
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:202`,
   `restart/skinny/tranches/sk-v12/HANDOFF.md:141`,
   `restart/skinny/tranches/sk-v12/HANDOFF.md:149`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:84`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:123`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:175`,
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:90`,
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:107`).

4. PASS - The packet does not use future-phase language as a close substitute.
   Alpha-A records that no generated CSS L4 row exists today and that the CSS
   floor remains unmeasured until W1 measures the same corpus/output plane with
   lightningcss and gate-consumed provenance
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-A-results-extraction.md:54`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-A-results-extraction.md:70`).
   Alpha-B likewise classifies missing CSS competitor data as `UNMEASURED`, not
   zero or pass (`restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:102`).
   The W5 "synthesize SK-V13 if close unmet" rows are bounded by measured ADMIT
   or measured FIXPOINT, not used as closure evidence
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:90`,
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:271`,
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:273`,
   `restart/skinny/tranches/sk-v12/HANDOFF.md:95`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:119`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:226`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:231`,
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:76`,
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:124`).

5. PASS - The G-Alpha table is sufficient for the presentation gate. PASS-ALPHA
   requires the user presentation to include targeted rows/interventions, LOC
   budget, hard caps, pre-blocked routes, goalset table, telemetry schema, and
   predicted close state (`restart/prompts/pass-contracts/PASS-ALPHA.md:169`,
   `restart/prompts/pass-contracts/PASS-ALPHA.md:171`,
   `restart/prompts/pass-contracts/PASS-ALPHA.md:173`). The folded G-Alpha
   presentation carries the close contract, telemetry required at G-Alpha, and
   the S-P1/S-P2/S-P3 plus W0-W5 seed table with target/role, hand LOC cap,
   minute caps, REDRESS adjacency, close contribution, and failure action
   (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:50`,
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:88`,
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:111`).
   It matches the corresponding Synthesis/Alpha-F seeds
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:260`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:215`).

6. PASS - G-Alpha no longer claims premature PASS. The presentation status is
   `PENDING V4 HARDENING`, explicitly says it is not a `G-Alpha PASS` record,
   and states no implementation wave is dispatchable until hardening accepts
   the presentation (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:7`,
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:9`,
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:128`,
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:130`).
   A direct scan finds no stale `G-Alpha PASS` or "converged under V2" claim in
   the file.

## Stable Carry-Forward

- The seed measurement surface remains SK-V11 close, not a new CSS admission:
  `skinny/RESULTS.md` carries JSON-only row identities and an overall
  `N-direct / NoGo` surface (`skinny/RESULTS.md:5`,
  `skinny/RESULTS.md:143`), while REDRESS 119/120 record the measured direct
  fixpoint and SK-V11 close without non-JSON generated admission
  (`skinny/REDRESS.md:3495`, `skinny/REDRESS.md:3531`,
  `skinny/REDRESS.md:3545`).
- Union and ASM-gen are correctly unblocked at category level, with REDRESS
  96/97/98 and 88/89/90 preserved as historical implementation evidence
  requiring material differential, CHALLENGE, proof/parity, microbench, and
  same-wave consumer (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:39`,
  `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:58`,
  `restart/skinny/tranches/sk-v12/SYNTHESIS.md:219`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:47`,
  `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:79`).

## Required Fold

None for CH6.
