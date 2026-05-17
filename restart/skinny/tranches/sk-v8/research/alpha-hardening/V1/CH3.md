# Pass Alpha SK-V8 CH3 Regression Challenge

Date: 2026-05-17.
Lens: CH3 Regression.
Scope: SK-V8 Pass Alpha V1 artifacts.

## Verdict

Overall disposition: REVISE.

The alpha packet correctly carries the main SK-V7 regression lesson: SK-V7
closed as `N-direct / NoGo`, W10c admitted only B6 stack-canary Stage 1, and
PMULL/CTZ bitmap body fills remain rejected. Alpha-C and Alpha-F also identify
most of the required pre-blocked route clusters from REDRESS 28+33, 50-55,
60-72, and 77-90.

The packet is not yet regression-safe enough for G-Alpha. The candidate
shortlist still allows two risky reopenings unless the final SPEC tightens the
entry gates:

- the retained parse "fusion-quality" candidate can become W5/W6/W4 under a
  new name unless it is explicitly post-W0 and hot-leaf-bound;
- the bitmap density-gated candidate intentionally reopens REDRESS 88 and 89
  and must be demoted to a post-W0 measurement envelope unless fresh profiles
  name bitmap prefix/next-bit as a row owner.

## Sources Read

- `restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md`
- `skinny/REDRESS.md`, especially entries 28+33, 50-55, 60-72, 77-90
- `git log` through `56e66ef5 feat(sk-v7-wave10c): admit B6 stack-canary Stage 1`

## Regression Dispositions

| Area | Disposition | Reason | Required fix |
|---|---|---|---|
| Alpha-A measured extraction | ACCEPT | It preserves the current authority: all 17 parse rows remain `K / NO-GO`, 11 direct rows remain `N-direct / NO-GO`, 6 direct rows are `A / GO`, and 4 real typed rows are `A / GO`. It does not claim rejected wave throughput. | None. Keep `RESULTS.md` as the opening authority and do not infer missing deltas. |
| Alpha-B competitor deltas | ACCEPT | Strict sonic is the same-run anchor, lossy sonic is a flaw probe, and sidecars are not treated as same-run strict anchors. | None. The final SPEC should retain the same strictness-plane language. |
| Alpha-C pre-block digest | REVISE | The digest covers the requested clusters and correctly says PMULL/CTZ remain rejected. It should make the reopen rule executable, not prose-only, before W2/W3/W4 candidates can dispatch. | Add a pre-redress checklist: current-head PC profile, row owner, same-wave consumer, scalar/checkasm where relevant, full-table maintain comparison, REDRESS citation explaining why the shape is different, and CHALLENGE acceptance. |
| Alpha-D validated/invalidated ledger | ACCEPT | It separates admitted substrate/harness work from rejected throughput routes and correctly demotes typed wins to product-plane wins. | None. Preserve the W10 honesty note. |
| Alpha-E candidate 1, twitter yyjson residual fusion | REVISE | This is a valid driver, but as written it can still reopen W5 StringBlock16, W6 value-byte carry, REDRESS 50 side tables, REDRESS 51/53 cursors, or REDRESS 64 per-quartet Unicode work under "fusion" language. | Reframe as post-W0 only. W0 must name the exact hot leaf and profile artifact. The final SPEC must ban local tiny-string wrappers, per-quartet helpers, object/key carry, aux side tables, parser-local structural cursors, separator/function-pointer/SWAR retries, and sidecar producers in this wave unless CH3 explicitly accepts a different shape. |
| Alpha-E candidate 2, RESULTS schema completion | ACCEPT | Telemetry completion does not reopen a rejected route and has a throughput-only split rule. | Keep the +/-1.0 percent telemetry-only guard and require malformed sidecar-manifest rejection. |
| Alpha-E candidate 3, Lock 14 residue audit | ACCEPT | It is an audit/relocation candidate with byte-identical generated output and zero throughput movement. | None. If source movement occurs, retain zero `RESULTS.md` diff and generated-output byte identity. |
| Alpha-E candidate 4, bitmap density-gated bodies | REVISE | This intentionally revisits REDRESS 88 and 89. The changed framing is plausible, but not sufficient before W0: REDRESS 88/89 require a narrow consumer or proven non-regression, and Alpha-F says bitmap routes have no target until a fresh profile names bitmap prefix/next-bit as hot owner. | Demote to a measurement-envelope candidate after W0. No production selection may land unless W0 names bitmap prefix/next-bit as a hot owner, scalar remains default outside the predicate, all W10/W10b falsifier rows stay within -1.0 percent Track 1/Track 2, all current GO rows keep verdicts, and full `bench-json`/`gate-json` data is attached. |
| Alpha-F global close condition | REVISE | The close condition has the right shape, but "previously passing row" is too narrow for regression control. W10b failed without verdict downgrades, so verdict-only protection is insufficient. | Define a machine-readable SK-V8 opening baseline and require every wave to compare all touched workload rows against it. Maintain gates must cover all 38 current rows, not only current GO rows, with wave-specific budgets and zero hidden stale Criterion roots. |
| Alpha-F W0 telemetry lock | ACCEPT | Profile-first entry is the correct regression barrier after W0b's unprofiled hot-leaf placeholders and missing baseline deltas. | None, except make W0 mandatory before W1-W4 dispatch. |
| Alpha-F W1 typed product expansion | REVISE | Typed product expansion is the safest admitted direction, but codegen/runtime changes can regress parse/direct guard rows. | Require all four current real typed rows to stay GO, the six current direct GO rows to stay GO, all non-target parse/direct rows within the declared Mbps budget, and no host/API schema hidden inside benchmark-private parsers or BBNF directives. |
| Alpha-F W2 parse candidate | REVISE | The draft says CHALLENGE must accept that the candidate is not pre-blocked, but it does not yet require full parse-table and direct/typed guard maintenance. | After W0, select at most one parse intervention family and at most three target rows. Require all 17 parse rows, all direct rows, and all real typed rows to be compared against the opening baseline; reject on any guard breach. |
| Alpha-F W3 direct guard triage | REVISE | It correctly distinguishes digest guard from product typed rows, but direct-string rejected families are dense and easy to reopen. | Explicitly ban REDRESS 54, 55, and 66-69 shapes under the current digest workload: sink-local decoded stats, quote-source streaming hash, source-hook folding, parser-owned scratch, byte-output unescape, and semantic string fact hashing. Reopen only if the output contract changes to product typed fields or a standalone decoded primitive first beats `unescape_string`. |
| Alpha-F W4 CostFacts gate | ACCEPT | It consumes admitted REDRESS 87 as evidence plumbing rather than a throughput admission. | Keep grammar-neutral CostFacts and require rejected alternatives with REDRESS references. |
| Alpha-F W5 Lock 14 audit | ACCEPT | It is a necessary regression guard after W7/W8 and does not reopen hot-path routes. | None. |
| Alpha-F W6 close/redress reconciliation | REVISE | The final close must prevent silent "routed" rejected candidates from escaping REDRESS. | Require every rejected W1-W4 candidate to produce a REDRESS entry or a wave artifact with thresholds, rows, and rollback status before G-Alpha close. |

## Pre-Block Completeness Check

Disposition: REVISE.

Alpha-C and Alpha-F correctly include these required blocks:

- REDRESS 28+33: active Class A NEON/TBL tiny-string wiring as a parse close.
- REDRESS 50: retained parse-time projection side tables.
- REDRESS 51+53: byte-class/EventCursor and parser-local structural-mask cursor.
- REDRESS 54+55: sink-local decoded stats and quote-source streaming hash.
- REDRESS 60: deleting the retained tiny-string probe.
- REDRESS 61+62: always-wide and delayed-wide retained trusted string scans.
- REDRESS 64: retained Unicode-escape run validator.
- REDRESS 65 and 84: object next-key/value-byte carry family.
- REDRESS 66-69: direct source-hook, parser-owned scratch, byte-output unescape,
  and semantic string fact hashing under the digest workload.
- REDRESS 70: hand-authored typed sink as DirectBuild proof.
- REDRESS 71: admitted only when host/API schema facts are explicit.
- REDRESS 72: generated-retained cap-16 only; no global, direct, or Track 2
  cap-16 policy.
- REDRESS 77-78: strict comparator and schema-v3 reporting are admitted but
  not throughput wins.
- REDRESS 80, 82, 83, 84: W2/W4/W5/W6 SK-V7 routes rejected.
- REDRESS 88-89: PMULL and CTZ/bulk production paths rejected.
- REDRESS 90: B6 Stage 1 admitted as harness hardening only.

Missing or under-specified pre-blocks to add to the final packet:

1. Add an explicit "renamed route" rule. Any candidate that touches the same
   local ownership boundary as a rejected item must cite the rejected item and
   explain the different hot owner before implementation.
2. Add REDRESS 73 as an adjacent standing block even though it is outside the
   emphasized range: generated retained array continuation shape must not be
   assumed to transfer to hand Track 2. This protects W2/W3 guard repair work.
3. Add a no-orphan primitive rule beside the bitmap route: scalar reference and
   checkasm are insufficient without a same-wave production consumer and
   full-row non-regression.
4. Add a digest-output-contract rule: direct digest misses are guard residuals
   unless W3 either closes them under the digest gate or routes them explicitly;
   they cannot be relabeled as product-plane wins.

## Candidate Reopen Risk

### Candidate 1: retained parse fusion

Disposition: REVISE.

This candidate is acceptable only as a profile-bound driver after W0. It must
not start from the old hypothesis that twitter is a tiny-string, object carry,
cursor, or Unicode materialization problem. Current alpha text already blocks
StringBlock16, value-byte carry, EventCursor, sidecar, PMULL, and CTZ, but the
final SPEC needs a stricter test:

- W0 must name the exact top hot leaf and profile artifact for `twitter
  parse_only`.
- The candidate must state which REDRESS entries share the touched boundary.
- If the touched boundary matches REDRESS 28/33, 50-55, 60-65, 82-84, or
  88-89, the plan is REJECT unless it proves a different owner and consumer.
- Guard rows must be the whole current table, not only six named parse rows.

Concrete final-SPEC text:

```text
W2 may not dispatch from the phrase "fusion-quality" alone. It dispatches only
after W0 records a non-placeholder hot leaf, profile artifact, c/B, and
opening-baseline delta for every parse row. If the selected patch touches a
boundary previously rejected in REDRESS, the W2 plan must include a CH3
accepted "different-shape proof" before redress begins.
```

### Candidate 4: bitmap density-gated bodies

Disposition: REVISE, bordering on REJECT if it remains pre-W0.

REDRESS 88 rejected PMULL as the default `bitmap_prefix_xor_64` hot body after
parse row regressions. REDRESS 89 rejected the CTZ/bulk production consumer
after six rows dropped beyond the maintain invariant. Alpha-E's density-gated
framing is meaningfully different from the default rewire, but it still risks
turning correctness-green asm into production code before row ownership is
established.

Concrete fixes:

- Move this out of the main SK-V8 wave manifest unless W0 profiles name bitmap
  prefix/next-bit as a hot owner on a target row.
- Split the first pass into measurement-only envelope work. It may produce
  primitive scan data and predicate evidence, but not production parser
  selection.
- If later promoted to production, require all W10 and W10b falsifier rows:
  `instruments`, `numbers`, `unicode_escapes`, `canada`, `citm_catalog`,
  `marine_ik`, and `mesh` to stay within -1.0 percent on both Track 1 and
  Track 2.
- Require all current `A / GO` direct and real typed rows to keep verdicts.
- Require an audit proving PMULL is not the default prefix-XOR body and CTZ/bulk
  is not the default bulk consumer outside the predicate.

## Maintain And Regression Gates

Disposition: REVISE.

The packet has useful row-level gates, but the final contract needs a single
maintain policy that every wave inherits. W10b is the reason: it failed by
multi-row Mbps drops even though verdict downgrades were not the only signal.

Required gate additions:

1. Establish `SK-V8-open` as a machine-readable baseline before W1.
2. Every implementation wave must compare all 38 current rows against
   `SK-V8-open`.
3. Wave-local target thresholds are not enough. A wave also needs:
   - no current GO row verdict downgrade;
   - no non-target Track 1 or Track 2 drop beyond the wave budget;
   - explicit handling for Criterion noise and target-dir provenance;
   - a row table in the wave artifact for every row that moved by more than
     1.0 percent.
4. Telemetry-only waves must fail if throughput moves by more than +/-1.0
   percent unless the wave is split and reclassified as performance work.
5. Primitive waves must require scalar reference, checkasm, asm proof where
   relevant, same-wave consumer, and full `bench-json`/`gate-json` evidence.
6. Direct/product waves must prevent product-plane relabeling: digest rows,
   typed rows, and parse rows need separate output-plane claims.

Concrete final-SPEC text:

```text
Maintain gate. Every admitted SK-V8 implementation wave must attach a
full-table comparison against SK-V8-open. A wave rejects if any non-target row
drops beyond its written budget, any current GO row loses GO, any stale
Criterion target is consumed, or any output-plane claim changes without a
matching schema and REDRESS entry.
```

## Concrete Patch List For Alpha-F

Disposition: REVISE.

Apply these content changes before G-Alpha:

1. In `Global close condition`, replace "No previously passing row regresses"
   with "No row regresses beyond the wave budget, and no current GO row loses
   GO."
2. In `W0`, say W0 is mandatory before W1-W4 and produces `SK-V8-open`.
3. In `W1`, add full-table maintain guard, not only the four typed rows.
4. In `W2`, require post-W0 exact owner paths and CH3 accepted
   different-shape proof for any boundary related to REDRESS 28/33, 50-55,
   60-72, 80, 82-84, 88, or 89.
5. In `W3`, add the direct-string/digest route bans from REDRESS 54, 55, and
   66-69.
6. In the bitmap candidate, add "measurement-only unless W0 names bitmap as hot
   owner" and preserve scalar defaults outside the predicate.
7. In `Pre-Blocked Routes`, add the renamed-route rule, no-orphan primitive
   rule, digest-output-contract rule, and the adjacent REDRESS 73 Track 2
   helper-transfer block.
8. In `G-Alpha Signoff`, require zero open CH3 REVISE items before dispatch.

## Final CH3 Disposition

REVISE.

The packet is close to regression-safe, but only after the final SK-V8 contract
makes W0 mandatory, converts candidate reopenings into post-profile gated
routes, strengthens full-table maintain gates, and makes the pre-block list
executable. No candidate should be rejected outright if those fixes land. If the
bitmap candidate remains a pre-W0 production route, CH3 changes that specific
candidate to REJECT.
