# SK-V11 S-P3 V1 CH1: Correctness And Measurable Row Gates

Pass: S-P3 Synthesis-Plan CHALLENGE.
Cycle: V1.
Date: 2026-05-20.
Lens: CH1 correctness.
Disposition: REVISE.

## Scope

This lens checks whether the V1 packet's candidates, wave gates, floors, owner
paths, dependencies, and strict-vs-strict claims are traceable to the accepted
S-P1/S-P2/SK-V10 close authority and internally consistent. It does not edit
source.

## Accepted Ground

- S-P1 is a valid authority for the SK-V11-open baseline: run id
  `sk-v9-open:criterion-fnv64-c8d7e0468358f98c`, host/toolchain/flags, 13
  residual direct rows, 7 typed guard rows, 4 direct guard rows, W0-clamped
  non-admissions, and the accepted hot-leaf families
  (`HARDENING-S-P1-CONVERGED.md:24-55`).
- S-P2 is converged and eligible for S-P3 consumption. The accepted pool is
  C1-C7 parser primitives, C8 oracle/host sink only, C9 accounting only, with
  W3 closed and non-JSON generality measured through generated direct/typed
  parsers (`HARDENING-S-P2-CONVERGED.md:7-32`).
- The primary direct residual floor table is traceable and strict-plane
  correct. `SYNTHESIS.md:101-124`, W0 `W0-open-baseline.md:36-57`,
  P3-C `p3c-falsifiability-gates.md:105-125`, and SPEC `SPEC.md:110-128`
  agree on `ceil(sonic-rs strict direct / 1.10)` and both-track admission.
- The packet correctly preserves the high-level close axes: direct closure or
  per-row measured proof, existing guard preservation, no parse-only SOTA, no
  W3 substrate retry, at least one benchmarked non-JSON generated parser
  intervention, and same-wave telemetry consumption (`SYNTHESIS.md:41-85`,
  `SPEC.md:26-59`, `DISPATCH-PROMPT.md:144-157`).

## Findings

1. Guard floors are internally inconsistent between P3-A and the gate/SPEC
   authority.

   P3-A says direct and typed guards use 99% maintain floors and publishes the
   higher floor tables (`p3a-candidate-shortlist.md:43-65`). P3-C later sets
   the binding formula to 98% plus same-run strict floor and publishes lower
   direct/typed maintain tables (`p3c-falsifiability-gates.md:127-158`);
   SPEC adopts the 98% tables (`SPEC.md:130-155`). P3-A candidates inherit
   "guard floors from §1" (`p3a-candidate-shortlist.md:123-126`,
   `p3a-candidate-shortlist.md:303-306`, `p3a-candidate-shortlist.md:411-416`),
   so a wave planned from P3-A can require different guard outcomes than the
   SPEC gate. CH1 cannot accept two maintain authorities for admitted rows.

   Required fold: make P3-A use the P3-C/SPEC 98% formula and tables, or make
   P3-C/SPEC explicitly adopt the stricter 99% P3-A tables. The final packet
   needs one guard-floor authority.

2. The required telemetry identifier set is not internally consistent, which
   weakens strict-admission measurability.

   SPEC says SK-V11 inherits a "36-field schema" and lists 36 identifiers, but
   omits `parse_utf8`, `escape_complete`, `flaw_probe`,
   `comparator_value_mbps`, and `comparator_source_artifact`
   (`SPEC.md:81-101`). P3-D says the required identifiers remain the
   gate-consumed set and explicitly includes those fields
   (`p3d-telemetry-schema.md:96-148`). P3-D also states these comparator fields
   are real evidence fields even if rendered in a folded cell
   (`p3d-telemetry-schema.md:144-148`). Because strict admission depends on
   measured validation path, strict comparator identity/value/source, and
   fail-closed gate consumption, the SPEC cannot be the dispatch authority
   while it carries a different required identifier inventory.

   Required fold: reconcile SPEC §0.3 with P3-D by listing the same required
   identifier set, or explicitly state which P3-D identifiers are derived
   aliases and how `gate-json` reconstructs and validates them.

3. The non-JSON baseline/floor dependency is split across W0, W1, and W2.

   P3-A says the non-JSON floor is unbound and must be materialized by W0/P3-D
   before behavior dispatch, or the candidate is unmeasurable
   (`p3a-candidate-shortlist.md:347-355`). P3-B instead makes the W2 entry
   depend on a W1 CSS baseline (`p3b-wave-sequencing.md:61-62`) and sets the
   W2 exit floor to `ceil(W1_css_baseline_mbps * 1.01)`
   (`p3b-wave-sequencing.md:94-96`). P3-C makes W1 responsible for the
   baseline harness and telemetry lane (`p3c-falsifiability-gates.md:170-174`)
   but its W2 gate uses `floor(W1_css_open_mbps * 1.01)`
   (`p3c-falsifiability-gates.md:79`). SPEC then says W2 may create the
   baseline and intervention in one wave if none exists and uses
   `floor(open_mbps * 1.01)` (`SPEC.md:341-362`), despite W1's exit already
   requiring renderable before/after baseline throughput (`SPEC.md:302-311`).

   This is a measurable-gate defect: the packet has three possible authorities
   for when the non-JSON baseline exists and two rounding rules for the 1%
   floor. The strict CH1 rule requires named rows plus concrete thresholds, not
   contingent prose.

   Required fold: make W1 the sole baseline-producing wave, make W2 consume a
   concrete W1 baseline with one rounding rule, and remove the W0/P3-D and W2
   same-wave-baseline alternatives. If W2 is allowed to capture the baseline,
   then W1's exit gate and P3-B dependency row must be rewritten accordingly.

4. The W5 string/unicode row set is inconsistent between P3-B/P3-C and SPEC.

   P3-B and P3-C include `y_string_unicode >= 3950` as a W5 string-heavy direct
   target (`p3b-wave-sequencing.md:99`, `p3c-falsifiability-gates.md:82`).
   SPEC W5 excludes `y_string_unicode` from the selected W5 target list and
   treats `unicode_escapes`, `unicode_mixed`, and `y_string_unicode` only as
   0.5% guard rows when not selected (`SPEC.md:510-515`). This matters because
   `y_string_unicode` is one of the 13 residual direct close rows, not an
   existing admitted guard (`SYNTHESIS.md:106-124`). A residual row cannot be
   silently demoted from a floor-bearing W5 target to a guard-only row without
   explaining which later gate owns its direct floor.

   Required fold: either restore `y_string_unicode` as a selectable W5 target
   with its 3950 floor in SPEC, or move it solely to W6/W8 with an explicit
   rationale and no "guard row" vocabulary that implies prior admission.

## Verdict

REVISE. The V1 packet is close to a measurable plan, and the accepted
S-P1/S-P2/SK-V10 authority is mostly carried correctly. The open defects are
internal consistency defects in the gates themselves: guard-floor authority,
telemetry identifier authority, non-JSON baseline ownership, and one residual
row's wave ownership. Redress should not dispatch from this packet until those
are folded into a single SPEC/P3-C/P3-A/P3-B story.
