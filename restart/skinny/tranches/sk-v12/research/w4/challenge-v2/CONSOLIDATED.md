# SK-V12 W4 CHALLENGE V2 - Consolidated Disposition

Verdict: REVISE.

Accepted lenses: CH3 regression/REDRESS, CH6 anti-paper-close.

Rejected lenses: CH1 correctness, CH2 generality/Lock 14, CH4 cost, CH5 hidden
coupling.

PLAN-V2 fixed the V1 caller ambiguity and W4-current gate concept, but PLAN-V3
is required before redress.

## Required PLAN-V3 Changes

1. Add W4-current strict-equality artifacts to the production PASS branch:
   post-W4 Track 1, cssparser, and lightningcss fact artifacts; fact-stream
   digest; run id; input/source checksums; and gate-consumed equality status.

2. Name `checkasm_ascii_set_member_find_64` explicitly in required commands for
   both microbench-reject and production-pass branches.

3. Mark A5's layout `skip_ws_and_comments` / run-skip framing superseded by
   the V2/V3 delimiter member-find caller for W4 redress.

4. Add or route a narrow Lock 14 W4 owner authorization. If production wiring
   edits frozen CSS template/runtime roots, W4 must own
   `lock14_baseline.rs` changes and tests that authorize only the selected CSS
   W4 slice under `sk-v12-waveW4`. If W4 stays reject-only before frozen-root
   edits, state that no W4 Lock 14 parent authorization is needed.

5. Split cost explicitly:
   - default branch: caller checkasm/parity, microbench, orphan disposition,
     JSON no-touch guard, REDRESS measured reject, no production wiring;
   - rare microbench-pass branch: halt redress at 0.9x cap and route a
     follow-up production/gate slice, or explicitly budget a W4-current
     report/gate + Lock 14 authorization sub-slice before continuing.

6. Strengthen `orphan-disposition.md` required fields with per-row
   `orphan_status`, `consumer_path` / `no-production-consumer`,
   `lock16_status`, `redress_entry`, source grep evidence, test/checkasm
   evidence, REDRESS adjacency, material differential, final disposition, and
   final `orphan_count=0`.

The exact delimiter member-find route remains semantically valid if the above
conditions are met.
