# SK-V11 W5 CHALLENGE CH4 Cost

Date: 2026-05-20.
Lens: CH4 cost / measurement sufficiency.
Scope: W5 scalar bounded string-span plan for `random/direct_to_struct`.
Output: this file.
Disposition: REVISE.

## Authorities Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Sections 0.4, 0.5, 2.1, 2.2,
  and 9.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R3-simd-string-block.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R4-row-gates-measurement.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R1-parse-that-string-span.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R2-generated-consumers.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R5-grammar-neutral.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-R6-preblocked-risk.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-plan-string-span-implementation.md`.
- `restart/skinny/tranches/sk-v11/research/w5/w5-plan-gate-risk-matrix.md`.
- P1 profile/cost facts:
  `restart/skinny/tranches/sk-v11/research/p1/p1b-samply-mode-2.md`,
  `p1c-samply-mode-3.md`, `p1d-pmu-cycles.md`, and
  `p1e-hot-leaf-attribution.md`.
- Prior cost challenge precedent:
  `restart/skinny/tranches/sk-v11/research/w4/challenge/w4-CH4-cost.md` and
  `restart/skinny/tranches/sk-v11/research/w4/challenge-v2/w4-CH4-cost-v2.md`.
- Source spot-check only:
  `skinny/crates/codegen/src/sink_direct.rs`,
  `skinny/crates/runtime/src/grammars/json/generated.rs`,
  `skinny/crates/bbnf-bench/src/direct_struct.rs`, and
  `skinny/crates/bbnf-bench/src/track2/json.rs`.

## Verdict

REVISE. The plan has the right macro-boundaries: scalar-only per R3, one target
row, no SIMD body, Unicode residual monitors, direct and typed guards, and
gate/report provenance. CH4 still cannot authorize the source patch as written.

The cost hypothesis is too weak on the binding track. `random/direct_to_struct`
is string-heavy, so W5 is at the right row family, and the Track 1 floor miss is
small enough that a string-path change could plausibly move it. The blocker is
Track 2: it must move from 6949 to 7878 Mbps, a 929 Mbps lift, or 13.4%
throughput. That is about 0.136 ns/byte, about 69 us per full
`random.json` parse, or about 2.1 ns per string over the 33,005 string spans in
the fixture.

The proposed cap-8 scalar span factoring does not yet explain that saving. The
current generated Track 1 and hand Track 2 already have cap-8 tiny plain-string
loops, already return the raw end, and already borrow the raw slice without
decode on the hit path. Replacing a raw-end result with a span-shaped result at
the same cap does not change fast-path coverage, does not reduce fallback count,
and may add field movement unless the compiler erases it.

## Row Cost

Binding row:

| Row | W0 Track 1 | W0 Track 2 | sonic direct | Floor | Needed T1 | Needed T2 |
|---|---:|---:|---:|---:|---:|---:|
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 | +185 | +929 |

The Track 1 lift is 2.4% and could be funded by a tiny-string cleanup. The
Track 2 lift is the real cost proof. P1-B/P1-E attribute direct `random` to:

| Track | Hot leaves | PMU c/B |
|---|---|---:|
| Track 1 | 23.8% `tiny_string`, 17.9% `ws`, 6.6% `option_copied` | 5.564 |
| Track 2 | 20.2% `hand_tiny`, 16.9% `ws`, 8.5% `u64_add` | 5.980 |

If the Track 2 gain comes only from `hand_tiny`, W5 needs roughly two thirds of
that leaf removed. That is not credible from a same-cap raw-end-to-span
factoring. If the claim is broader, the plan must name the broader work source:
fallback avoidance, cap coverage, whitespace interaction, digest interaction,
or a distinct hand Track 2 local source delta.

A read-only fixture census reinforces the problem:

| `random.json` fact | Count |
|---|---:|
| Bytes | 510476 |
| Strings | 33005 |
| Strings with content length <= 7, current cap-8 hit class | 19256 |
| Strings outside the current cap-8 hit class | 13749 |
| Escapes/control bytes observed by the string census | 0 |

The selected cap is the current generated direct cap 8, which covers content
lengths 0 through 7 because the closing quote must appear inside the eight
bytes after the opener. Keeping that cap means W5 does not move the 13,749
longer strings out of the full matcher. A cap change might be a real cost
mechanism, but it is not this plan and would need fresh cap evidence plus a
REDRESS 72 material differential.

## Probe And Criterion Sufficiency

The probe-first structure is necessary but not sufficient as written. R4 and
the plan currently allow Criterion when at least one `random` track improves by
`>= 1.0%` and the other does not regress by more than `0.5%`. That trigger is
too weak for this row. It would allow a Track 1-only improvement while Track 2
remains far below 7878 Mbps, making the Criterion run predictably non-admitting.

Revise the probe trigger before redress:

1. Run repeated same-host old-vs-new `profile_direct` probes for
   `random` Track 1 and Track 2 with run id, host triple, build flags, selected
   cap, sample count, and patch identity.
2. Stop before Criterion unless both `random` tracks clear 7878 Mbps with
   enough noise margin to survive Criterion, or unless CHALLENGE records a
   narrower diagnostic-only probe that cannot move `RESULTS.md`.
3. Require the Track 2 probe to show the required 13.4% lift plus margin, not
   merely "no regression."
4. Probe direct guards, typed guards, and Unicode residual monitors before
   Criterion; any guard-threatening regression is a reject path.
5. Run binding Criterion only after the probes clear the floor-level trigger.
   Criterion remains the admission authority, but it should not be used to
   discover that the known Track 2 gap was not addressed.

The existing Criterion command shape is acceptable after that trigger: selected
row, direct guards, Unicode residual monitors, same-run sonic/serde comparators,
typed guards, and `gate-json --with-cost-facts --check-results`.

## Source Patch Boundary

Do not attempt the source patch as currently described. A revised W5 source
attempt may proceed only after the plan names a material Track 2 cost mechanism
and tightens the probe trigger above.

Acceptable revision directions:

- Keep cap 8, but show a concrete local source delta in both generated Track 1
  and independent hand Track 2 that plausibly removes real instructions from
  the bounded plain-string hit path. The plan must explain why the compiler
  will not reduce the new span shape back to the current raw-end shape.
- Select a cap or coverage change only under new CHALLENGE language, because
  cap transfer from retained/string-proof contexts is preblocked by REDRESS 72.
- If Track 2 is changed, keep it local to `direct_struct.rs` or the hand Track
  2 owner path. It must not call generated Track 1 helpers, generated typed
  helpers, or a hidden shared parser.
- Keep R3's scalar-only decision. No `bbnf-simd` source patch belongs in this
  W5 packet without a separate strict checkasm cell, caller microbench, and
  REDRESS 106 differential.
- Do not edit `parse-that-regex`, generic codegen behavior, or runtime outside
  generated JSON unless the same wave supplies the missing non-JSON generated
  string/literal proof. REDRESS 113 remains open.

## Guard Cost

The guard burden is appropriate and should stay. The plan correctly carries:

- direct guards: `citm_catalog`, `apache_builds`, `marine_ik`, and
  `unicode_basic`;
- typed guards: `twitter`, `citm_catalog`, `apache_builds`, `github_events`,
  `update_center`, `mesh`, and `marine_ik`;
- Unicode residual monitors: `unicode_escapes`, `unicode_mixed`, and
  `y_string_unicode`.

This guard set is not the CH4 problem. The problem is that the selected patch
does not yet describe enough work removal on `random` Track 2 to justify even a
source attempt, and the current probe trigger can fall through to Criterion
without addressing that known floor miss.

## Required Plan Changes

1. State that same-cap scalar factoring alone is low-confidence and not
   authorized as a source patch unless paired with a named Track 2 local cost
   mechanism.
2. Replace the `>= 1.0%` one-track probe trigger with a floor-level trigger:
   both `random` tracks must clear 7878 Mbps with noise margin before Criterion
   can be an admission run.
3. Explain the Track 2 cost budget in time terms: 929 Mbps, 13.4%, about
   0.136 ns/byte, about 69 us per `random.json` parse, or about 2.1 ns per
   string.
4. If a cap change is proposed, return to CHALLENGE with cap-specific evidence
   and REDRESS 72 differential. Do not smuggle a cap change into this cap-8
   plan.
5. Preserve all R3/R4 guard and provenance requirements, including scalar-only
   W5, Unicode residual monitoring, direct/typed guards, independent Track 2,
   same-run strict comparators, and gate/report consumption.

DISPOSITION: REVISE.
