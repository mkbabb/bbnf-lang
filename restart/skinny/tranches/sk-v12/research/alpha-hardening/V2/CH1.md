# SK-V12 Alpha Hardening V2 - CH1 Correctness

Date: 2026-05-20.

Scope: correctness review of the V2-folded Alpha packet at commit `c45ab80d`
under `USER-PIN-W1-CSS-L4-SOTA.md`. Checked strict lightningcss gate
semantics, CSS output-plane/equality consistency, pass order, close
conditions, evidence citations, and rollback facts.

## Verdict

PASS.

No blocking CH1 correctness findings remain. Required folds: none.

## Findings

1. Strict lightningcss gate semantics are folded. The user pin requires the
   generated CSS L4 parser to beat lightningcss on the same corpus, same output
   plane, with strict equality (`USER-PIN-W1-CSS-L4-SOTA.md:29-35`). V2
   consistently uses generated Track 1 strictly
   `> lightningcss_mbps + 1`: `SYNTHESIS.md` at `c45ab80d:39-47`,
   `HANDOFF.md` at `c45ab80d:53-58` and `c45ab80d:117-118`,
   `alpha-B-competitor-deltas.md` at `c45ab80d:37-58`,
   `alpha-E-candidate-shortlist.md` at `c45ab80d:36-39` and
   `c45ab80d:114-120`, and `alpha-F-contract-draft.md` at
   `c45ab80d:72-77` and `c45ab80d:228-230`.

2. CSS output-plane and equality binding are consistent. V2 defines one
   canonical CSS fact stream shared by generated Track 1, independent Track
   2/oracle, and lightningcss in `SYNTHESIS.md` at `c45ab80d:42-53`,
   `alpha-B-competitor-deltas.md` at `c45ab80d:84-113`,
   `alpha-E-candidate-shortlist.md` at `c45ab80d:77-84` and
   `c45ab80d:103-120`, and `alpha-F-contract-draft.md` at
   `c45ab80d:71-84`. Historical CSS measurements are explicitly non-closing,
   and missing CSS rows remain `UNMEASURED`, not wins
   (`alpha-B-competitor-deltas.md` at `c45ab80d:102-127`).

3. Pass order and close conditions match the contracts. V2 restores
   G-Alpha -> S-P1 -> S-P2 -> S-P3 before implementation authority
   (`SYNTHESIS.md` at `c45ab80d:39-41` and `c45ab80d:237-241`,
   `HANDOFF.md` at `c45ab80d:105-108` and `c45ab80d:168-173`,
   `alpha-F-contract-draft.md` at `c45ab80d:14-16` and `c45ab80d:146-171`),
   consistent with the G-Alpha gate in `PASS-ALPHA.md:167-178` and
   `ORCHESTRATOR.md:159-172`. ADMIT/FIXPOINT also preserve CSS-before-fallback
   and the measured union/ASM-gen FIXPOINT requirements (`SYNTHESIS.md` at
   `c45ab80d:72-94`; `HANDOFF.md` at `c45ab80d:87-95`;
   `alpha-F-contract-draft.md` at `c45ab80d:100-120`).

4. Evidence citations and rollback facts are adequate for CH1. Result-state
   summaries resolve to local `RESULTS.md`/`REDRESS.md` lines in Alpha-A,
   `SYNTHESIS.md`, `HANDOFF.md`, and Alpha-F
   (`alpha-A-results-extraction.md` at `c45ab80d:92-95` and
   `c45ab80d:171-174`; `SYNTHESIS.md` at `c45ab80d:110-140`;
   `HANDOFF.md` at `c45ab80d:44-47`; `alpha-F-contract-draft.md` at
   `c45ab80d:136-139`). Wave rollback/rejected-patch paths are specified for
   W1a-W4 in Alpha-E (`c45ab80d:133-136`, `c45ab80d:182-185`,
   `c45ab80d:221-223`, `c45ab80d:276-278`, `c45ab80d:335-339`) and summarized
   in the G-Alpha seed (`alpha-F-contract-draft.md` at `c45ab80d:220-226`).
