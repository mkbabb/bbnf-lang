# SK-V11 S-P3 V2 CH1: Correctness And Measurable Row Gates

Pass: S-P3 Synthesis-Plan CHALLENGE.
Cycle: V2.
Date: 2026-05-20.
Lens: CH1 correctness.
Disposition: REVISE.

## Scope

This lens checks whether V2 folded the V1 CH1 defects into one internally
consistent dispatch packet: guard-floor authority, telemetry identifier
authority, non-JSON baseline ownership and rounding, `y_string_unicode`
ownership, floors, owner paths, dependencies, and strict-vs-strict claims. It
does not edit source.

## Accepted Ground

- SPEC, P3-C, P3-D, and DISPATCH now agree that JSON direct admission uses the
  SK-V11-open strict direct floor `ceil(sonic-rs strict direct Mbps / 1.10)`,
  with generated Track 1 and independent Track 2/oracle on the same output
  plane.
- SPEC and P3-D now carry the same telemetry authority, including
  `parse_utf8`, `escape_complete`, `flaw_probe`,
  `comparator_value_mbps`, and `comparator_source_artifact`, and preserve the
  fail-closed rule for producer-only telemetry.
- The main wave sequence now correctly splits non-JSON work: W1a owns the
  gate/report lane, W1b owns exactly one generated non-JSON baseline plus
  independent oracle, and W2 consumes that baseline with
  `ceil(W1b_css_baseline_mbps * 1.01)`.
- P3-B, P3-C, SPEC, and DISPATCH make `y_string_unicode/direct_to_struct` a
  residual row with floor 3950, selectable in W5 and W6 and finally owned by
  W8 if still open.
- Owner paths and dependencies are materially better than V1: W1a/W1b/W2 have
  distinct owner path families, generated output is capped to named inputs, and
  W2 blocks later generic primitive claims.
- Strict-vs-strict comparator discipline is folded: direct digest admission
  cannot use typed evidence, typed admission cannot use direct digest evidence,
  stale sidecars and flaw probes are planning only, and Track 2/oracle coupling
  fails closed.

## Findings

1. P3-A still carries older typed guard floors, so V2 does not yet have one
   guard-floor authority across the packet.

   P3-A opens with the V2/P3-C/SPEC typed guard table:
   `twitter 17385`, `citm_catalog 29928`, `apache_builds 8308`,
   `github_events 11633`, `update_center 11613`, `mesh 9214`,
   and `marine_ik 11552` for Track 1
   (`research/p3/p3a-candidate-shortlist.md:54-67`). Later P3-A reintroduces
   the old higher typed guard values: C4 says refreshed typed numeric guards
   include `mesh >= 9308` and `marine_ik >= 11670`
   (`p3a-candidate-shortlist.md:261-264`), and C7 says typed guards must
   maintain `17562 / 30233 / 8393 / 11752 / 11732 / 9308 / 11670`
   (`p3a-candidate-shortlist.md:388-391`). The §3 summary repeats the same old
   C7 floor set (`p3a-candidate-shortlist.md:409-410`).

   Required fold: update all P3-A candidate and summary floor references to the
   V2 P3-C/SPEC maintain table, or explicitly make the stricter P3-A values the
   packet authority everywhere. As written, a plan agent reading P3-A can select
   different typed guard thresholds than the SPEC gate.

2. P3-A's non-JSON summary still contradicts the W1a/W1b/W2 ownership and
   rounding fold.

   The P3A-C6 body is corrected: W1a creates the gate/report lane, W1b creates
   the baseline row, and W2 admits only at
   `ceil(W1b_css_baseline_mbps * 1.01)` with strict oracle equality
   (`p3a-candidate-shortlist.md:350-356`). But P3-A §3 still says the
   non-JSON floor is "currently unbound in `skinny/RESULTS.md`" and that
   "W0/P3-D must create the concrete Mbps floor before behavior dispatch"
   (`p3a-candidate-shortlist.md:402-409`). That is the V1 defect under a new
   packet: it leaves W0/P3-D as possible baseline/floor authority even though
   SPEC, P3-B, P3-C, and DISPATCH make W1b the only baseline authority and W2
   the first intervention consumer.

   Required fold: replace the P3-A §3 C6 summary with the W1b/W2 rule:
   W1b creates the concrete baseline Mbps and W2 uses
   `ceil(W1b_css_baseline_mbps * 1.01)`. P3-D may bind fields; it must not own
   the non-JSON performance floor.

3. `y_string_unicode` ownership is mostly folded, but SPEC still has one
   guard-vocabulary conflict that should be removed before dispatch authority.

   The binding gates are correct: P3-B lists `y_string_unicode >= 3950` as a
   W5 string-heavy target and a W6 escape/unicode target, and W8 owns every
   remaining residual row; P3-C and SPEC carry the same floor. SPEC W5 also
   explicitly says `unicode_escapes`, `unicode_mixed`, and
   `y_string_unicode` "are not admitted guards" if unselected and remain W6/W8
   residuals (`SPEC.md:570-576`). However, the W5 task list still says "Run
   Unicode rows as guards when the target is plain-string rows" (`SPEC.md:561-566`).
   That wording reopens the V1 CH1 ambiguity: residual Unicode rows are not
   guard rows merely because they are monitored alongside a plain-string target.

   Required fold: change the W5 task wording to residual monitoring or
   full-table maintain coverage, not "guards", while preserving the exit gate's
   floor-bearing residual ownership.

## Verdict

REVISE. V2 fixed the main SPEC/DISPATCH authority, telemetry identifiers,
non-JSON wave split, row dependencies, owner paths, and strict-vs-strict
discipline. It still does not converge under CH1 because P3-A remains part of
the committed V2 packet and carries contradictory typed guard floors plus a V1
non-JSON floor-ownership sentence. Clean those P3-A references and the SPEC W5
Unicode guard wording, then rerun CH1.
