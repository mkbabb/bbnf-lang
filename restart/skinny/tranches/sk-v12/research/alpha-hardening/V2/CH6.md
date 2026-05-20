# SK-V12 Pass Alpha Hardening V2 - CH6 Anti-Paper-Close

Date: 2026-05-20.
Pass: Pass Alpha SK-V11 -> SK-V12 under `USER-PIN-W1-CSS-L4-SOTA.md`.
Lens: CH6 next-tranche impact / anti-paper-close.

## Verdict

PASS.

The V2-folded Alpha packet prevents paper close under the user pin. It does not
authorize behavior source work, does not let SK-V12 jump to S-P3 before
G-Alpha/S-P1/S-P2, keeps CSS L4 as the mandatory first redress target, binds
admission to strict `> lightningcss_mbps + 1`, makes telemetry gate-consumed
and fail-closed, avoids future-phase CSS promises, and gives G-Alpha a
sufficient intervention table.

## Materials Checked

- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
- `restart/prompts/ORCHESTRATOR.md` Section 3Z.
- `restart/prompts/pass-contracts/PASS-ALPHA.md`.
- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CONSOLIDATED.md`.
- V2-folded `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- V2-folded `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- V2-folded Alpha A/B/E/F.
- `skinny/RESULTS.md`.
- Tail of `skinny/REDRESS.md` through REDRESS 120.

## Anti-Paper-Close Checks

| Check | Result | Evidence |
|---|---|---|
| No S-P3 jump before G-Alpha/S-P1/S-P2 | PASS | `SYNTHESIS.md` says Alpha is not implementation authority and, after G-Alpha, SK-V12 runs S-P1, S-P2, then S-P3 under the pin (`SYNTHESIS.md:5-11`, `:39-40`, `:237-254`). `HANDOFF.md` repeats "present G-Alpha, then run SK-V12 S-P1 Profile, S-P2 Research, and S-P3 Synthesis-Plan" and bars downstream S-P3 packet edits until those passes converge (`HANDOFF.md:105-108`, `:170-173`). Alpha-F matches this sequence (`alpha-F-contract-draft.md:11-16`, `:148-171`). |
| CSS redress attempt before fallbacks | PASS | The pin requires CSS L4 first. V2 keeps Sheets/BBNF-self fallback-only after a CSS L4 redress attempt records measured evidence (`SYNTHESIS.md:77-78`, `:175-177`, `:211-213`, `:249`; `HANDOFF.md:64-65`, `:89-90`, `:123`, `:157-159`; `alpha-F-contract-draft.md:40-43`, `:105-110`, `:154-156`, `:199-201`, `:228-231`; `alpha-E-candidate-shortlist.md:33-35`, `:343-345`). |
| Strict `> lightningcss` gate | PASS | Alpha-B defines the binding comparator as `css_l4_track1_mbps > lightningcss_mbps + 1` and says equality at `+1` is a miss (`alpha-B-competitor-deltas.md:34-58`, `:187-193`). SYNTHESIS, HANDOFF, Alpha-E, and Alpha-F use the same strict gate (`SYNTHESIS.md:42-47`, `:273-276`; `HANDOFF.md:53-57`, `:76-78`, `:117-118`; `alpha-E-candidate-shortlist.md:36-39`, `:114-126`, `:260-267`, `:321-331`; `alpha-F-contract-draft.md:71-84`, `:228-231`). Baseline-plus-1% text remains only as historical/rescinded/refusal language. |
| Telemetry consumed | PASS | The CSS L4 row must be in `skinny/RESULTS.md` or a same-wave gate-consumed companion report, and the gate rejects missing/stale provenance, comparator, equality, sample, host, profile, JSON guard, Lock 14/16, consumer, wave, and REDRESS fields (`SYNTHESIS.md:183-204`; `HANDOFF.md:139-151`; `alpha-F-contract-draft.md:173-192`). Alpha-B also requires row/comparator/version/output-plane/Mbps/delta/fixture/equality fields before any Alpha/G-Alpha delta claim (`alpha-B-competitor-deltas.md:84-105`). |
| No future-phase promises | PASS | Alpha-A and Alpha-B state that current CSS L4 evidence is absent/unmeasured, not a pass (`alpha-A-results-extraction.md:52-72`, `:235-240`; `alpha-B-competitor-deltas.md:102-127`, `:189-200`). References to a "future route/pass" are conditional blocked-route language for JSON residuals after the CSS priority resolves; they do not defer CSS L4, Sheets, BBNF-self, telemetry, or comparator evidence. |
| G-Alpha intervention table | PASS | SYNTHESIS and Alpha-F provide a presentation-ready table with S-P1/S-P2/S-P3 plus W0-W5, target/role, LOC cap, minute cap, REDRESS adjacency, close contribution, and failure action (`SYNTHESIS.md:256-276`; `alpha-F-contract-draft.md:211-233`). The table is sufficient for G-Alpha because it names CSS L4 first, W1a/W1b split, W2 SIMD correctness, W3 union, W4 ASM-gen/orphan disposition, and W5 close/fixpoint. |

## Fold Assessment

No V3 fold is required for CH6.

The V1 CH6 blockers are folded:

1. Pass order is restored to G-Alpha -> S-P1 -> S-P2 -> S-P3 -> waves.
2. Candidate caps are explicit: W1a/W1b/W2/W3/W4/W5 inherit 20/15/30 minute
   wave caps and carry LOC ceilings in the G-Alpha table.
3. CSS admission uses strict `generated_track1_mbps > lightningcss_mbps + 1`
   everywhere that can admit a CSS row.
4. Revert/rejected-patch paths are named for W1a, W1b, W2, W3, and W4, and W5
   routes close failure to measured FIXPOINT or SK-V13 synthesis only after the
   required CSS, union, ASM-gen, and orphan dispositions are measured.
5. G-Alpha has enough table detail to present without inventing unstated
   orchestrator policy.

Residual implementation risk remains downstream, not a Pass Alpha blocker:
S-P1/S-P2/S-P3 and later waves must still produce the fresh CSS L4 row,
lightningcss comparator, strict equality artifact, JSON guard state, and REDRESS
evidence before any ADMIT or FIXPOINT can close.
