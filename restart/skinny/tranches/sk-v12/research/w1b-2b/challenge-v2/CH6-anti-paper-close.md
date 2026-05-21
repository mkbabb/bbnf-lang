# SK-V12 W1b-2b PLAN-V2 CH6 - Anti-Paper-Close

Date: 2026-05-20.
Wave: W1b-2b - CSS L4 Lightningcss SOTA Report + Admission Gate.
Lens: CH6 anti-paper-close.
Owned artifact: `restart/skinny/tranches/sk-v12/research/w1b-2b/challenge-v2/CH6-anti-paper-close.md`.

## Authorities Read

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` §4.
- `restart/skinny/tranches/sk-v12/SPEC.md` §0.1 and §7.2.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN-V2.md`.
- Current W1b artifacts:
  `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json`,
  `restart/skinny/tranches/sk-v12/research/w1b/artifacts/strict-equality.txt`,
  `restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-strict-equality.txt`,
  and `restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-facts.txt`.
- W1b-2b research A3/A5 for Criterion consumption and REDRESS/RESULTS routing.

## Findings

1. Measured Criterion evidence is gate authority. PLAN-V2 requires
   `gate.rs`, not the serialized report, to read the three
   `nonjson_css_l4/*/new/` lanes for Track 1, cssparser oracle, and
   lightningcss. It requires `throughput.Bytes == 187`, finite positive
   `mean.point_estimate`, and `sample.json.iters.len() >= 30`; no `base/`,
   `change/`, hand-entered Mbps, or report-only throughput can admit.

2. The lightningcss comparator is same-plane and retained. W1b-2a artifacts
   include a `lightningcss_same_plane_fact_stream` Criterion row plus retained
   `lightningcss-facts.txt` for
   `css_l4/declaration_values/direct_to_struct/main` on
   `css_l4_declaration_value_fact_stream`. PLAN-V2 binds the W1b-2b report to
   those artifact paths and requires `lightningcss_sequence_status ==
   pass:ast_projection_matches_source_sidecar`.

3. The independent oracle and strict equality are consumed, not asserted.
   PLAN-V2 requires `track2_independence_status == independent_verified`,
   `strict_output_equality == pass`, and
   `three_way_equality == pass:track1=cssparser=lightningcss`. Missing or stale
   Track 2/cssparser evidence cannot become a lightningcss-only speed claim.

4. JSON guard state is explicit. PLAN-V2 requires `json_guard_state` in the
   report and requires the companion gate to run any requested JSON check
   against a populated accepted JSON Criterion root, not the CSS-only root. It
   also blocks write/probe flags and mixed companion reports, so the CSS report
   cannot paper over JSON guard drift.

5. The outcome split is honest. PLAN-V2 derives
   `threshold_mbps = lightningcss_mbps + 1` and
   `admission_margin_mbps = track1_mbps - threshold_mbps` in validation.
   `PASS-ADMIT-CANDIDATE` is allowed only for a strict positive margin;
   equality at the threshold or any lower Track 1 result routes to
   `PASS-MEASURED-BASELINE`, which records REDRESS evidence but does not move
   the CSS row in `skinny/RESULTS.md`.

6. Final campaign close is not smuggled into W1b-2b. PLAN-V2 can produce a CSS
   admit candidate or measured baseline for `G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA`,
   but SPEC §0.1 and later W4/W5 obligations still control final SK-V12 ADMIT
   or FIXPOINT. The plan does not claim that a W1b-2b report alone closes the
   campaign.

## Blocking Findings

None.

## Redress Reject Conditions

- Reject if `PASS-ADMIT-CANDIDATE` is recorded without recomputing Track 1,
  cssparser, and lightningcss Mbps from the three valid Criterion `new/` lanes.
- Reject if equality at or below `lightningcss_mbps + 1` admits instead of
  routing to `PASS-MEASURED-BASELINE`.
- Reject if strict three-way equality, cssparser oracle independence,
  lightningcss same-plane artifact identity, JSON guard state, REDRESS-125
  identity, or generated/source provenance is missing or producer-only.
- Reject if the companion CLI accepts write/probe flags, mixed companion
  reports, missing report paths, flag-as-path arguments, or a CSS-only
  Criterion root as JSON guard proof.
- Reject if `skinny/RESULTS.md` moves for `PASS-MEASURED-BASELINE`,
  `BLOCKED/FAIL`, stale evidence, or a future W3/W4/W5 promise.
- Reject if W1b-2b wording claims final SK-V12 ADMIT/FIXPOINT rather than a
  Section 7.2 gate disposition.

## Disposition

DISPOSITION: ACCEPT

PLAN-V2 is redressable under CH6. It prevents paper-close by making live
Criterion `new/` lanes the throughput source, requiring retained three-way
strict equality and independent oracle evidence, binding the same-plane
lightningcss comparator, consuming JSON guard state, and deriving the
`PASS-ADMIT-CANDIDATE` versus `PASS-MEASURED-BASELINE` decision from the
`lightningcss_mbps + 1` threshold.
