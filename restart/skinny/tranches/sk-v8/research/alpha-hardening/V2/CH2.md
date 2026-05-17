# CH2 Generality Challenge - SK-V8 Alpha V2

Date: 2026-05-17.
Lens: CH2 Generality.
Overall disposition: REVISE.

## Read Set

- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/CH2.md`
- `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/CONSOLIDATED.md`

## Verdict

The final SK-V8 docs materially improve the V1 packet and do not trigger a
CH2 reject. They keep W0 telemetry-only, make CostFacts binding precede
behavior waves, say that Lock 14 and Lock 15 pass at every wave close, block
JSON policy in generic crates, and keep Pass Omega queued and separate.

The V1 generality revisions are not fully resolved. The final packet carries
the right intent, but several CH2 requirements remain too implicit or absent:
the Lock 14 gate is not made concrete per implementation-wave exit, telemetry
is still framed around `gate-json` and JSON rows rather than a grammar-aware
report contract, the domain comparator registry is not specified, and the
CSS L4, Sheets, and BBNF-self implications were not folded into the dispatch
gates.

Disposition: REVISE.

## Resolution Matrix

| Requirement | V2 disposition | Finding |
|---|---|---|
| Lock 14 per-wave gate | REVISE | `SPEC.md` Section 0.1 says Lock 14 and Lock 15 gates pass at every wave close, and Section 8 gives W5 reconciliation. V1 CH2 required W0 baseline allowlist plus concrete every-wave exit checks. W0, W2, W3, and W4 do not spell out the public API, branch, primitive, template, and non-JSON proof scans. |
| No JSON policy in generic crates | ACCEPT with hardening | The packet repeats this as a non-negotiable and W5 exit gate, and W1 requires grammar-neutral CostFacts paths. This resolves the top-level policy, but the enforcement should be attached to each generic-crate edit gate rather than left mostly to W5. |
| Grammar-neutral telemetry | REVISE | `SPEC.md` Section 0.4 adds useful telemetry, but it does not add `grammar_id`, `domain`, `comparator_id`, `comparator_plane`, or `comparator_strictness`, nor does it state that `gate-json` is only the JSON instance of a more general report contract. |
| Domain comparator registry posture | REVISE | The comparator classes are clear for current JSON rows, but there is no registry shape or extension point for non-JSON comparators such as lightning-css, formula engines, or BBNF self-host rows. Future domain comparators are mentioned only as sidecar planning signals. |
| CSS, Sheets, and BBNF-self implications | REVISE | The final docs do not carry the V1 CH2 non-JSON implications. There is no explicit CSS token/class-table proof, Sheets DirectSchema or formula/cell dry run, or BBNF-self terminology scan tied to generic CostFacts/codegen changes. |
| Pass Omega separation | ACCEPT | `SYNTHESIS.md` and `HANDOFF.md` keep Omega queued, separate, and non-blocking for G-Alpha/W0. A final hardening sentence should still say Omega may only add enforcement or clarification and cannot weaken Lock 14. |

## Remaining Blockers Before ACCEPT

1. Add a concrete Generality and Lock 14 gate to `SPEC.md`, or equivalent
   per-wave exit text, covering public API scans, grammar branch scans,
   primitive/table scans, template/provider boundaries, and a non-JSON proof
   requirement for any generic-crate edit.
2. Add the W0 Lock 14 baseline allowlist requested by V1 CH2. Allowed
   JSON-specific surfaces should be limited to grammar inputs, generated JSON
   output, per-grammar providers/templates, tests, and host/API schema facts.
3. Extend the telemetry/report contract with a grammar-aware comparator
   registry or equivalent fields: `grammar_id`, `domain`, `comparator_id`,
   `comparator_plane`, `comparator_strictness`, freshness, and run id.
   Keep rendered JSON columns if useful, but do not make them the generic
   report model.
4. State the domain comparator posture: JSON comparators are valid only for
   JSON rows unless refreshed under same-run same-plane rules; CSS, Sheets,
   and BBNF-self need their own domain anchors or explicit absence of strict
   comparator evidence.
5. Fold the CSS L4, Sheets, and BBNF-self implications into W1-W5 gates:
   generic CostFacts, codegen, runtime, SIMD, or parser-template changes must
   prove they do not require JSON structural roles to compile, lower, cost, or
   run.
6. Add the V1 CH2 Lock 14 residue cluster to the pre-block or audit scope:
   REDRESS 36, 37, and 38 concerns around generic-crate JSON residue,
   detached scanner surfaces, JSON structural alphabets, JSON binding helpers,
   and public `Json*` generic APIs.
7. Add explicit Omega no-weakening wording: Pass Omega may add lock
   enforcement, path cleanup, or canonical-surface refresh, but it cannot
   weaken Lock 14 or be used as a prerequisite for accepting a generic JSON
   policy leak.

## Reject Triggers

No reject trigger is present in the final docs. They do not authorize JSON
policy in generic crates, do not claim yyjson or twitter parse wins as generic
CSS/Sheets/BBNF-self evidence, and do not make Pass Omega a license to weaken
Lock 14. The open issues are specificity and missing non-JSON proof gates, so
the correct V2 outcome is REVISE rather than REJECT.
