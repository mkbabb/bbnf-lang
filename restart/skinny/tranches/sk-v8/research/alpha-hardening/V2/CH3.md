# Pass Alpha SK-V8 V2 CH3 Regression Challenge

Date: 2026-05-17.
Lens: CH3 Regression.
Scope: final SK-V8 docs against V1 CH3 and V1 consolidated findings.

## Verdict

Overall disposition: REVISE.

The final SK-V8 packet resolves the critical V1 regression risks around W0,
pre-block reopening, bitmap reserve status, direct digest honesty, and dispatch
sequencing. It is close to ACCEPT, but one CH3 blocker remains: W2 typed product
expansion does not explicitly carry the full-table maintain gate required by V1
CH3.

This is not a REJECT. The remaining issue is a contract gap in `SPEC.md`
Section 5, not a reopened rejected route. If W2 inherits the same full-table
`SK-V8-open` comparison discipline as W0, W1, W3, and W4, CH3 can move to
ACCEPT.

## Sources Read

- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/CH3.md`
- `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/CONSOLIDATED.md`

## Regression Checks

| Requirement | Disposition | Finding |
|---|---|---|
| W0 mandatory `SK-V8-open` | ACCEPT | `SPEC.md` makes W0 capture `SK-V8-open`, populate required telemetry for all 38 current rows, reject placeholder/missing telemetry, and block W1-W6 on W0 rejection. `HANDOFF.md` and `DISPATCH-PROMPT.md` make W0 the only post-G-Alpha dispatch. |
| Executable pre-block reopen rules | ACCEPT | `SPEC.md` Section 10 and `HANDOFF.md` Section 7 require fresh W0 evidence, same-wave consumer, scalar/checkasm where relevant, no-regression gate, REDRESS citation, and challenge acceptance before a listed route can reopen. This is executable enough for CH3. |
| Full-table maintain gates | REVISE | W0, W1, W3, and W4 have full-table or all-row maintain wording, and W5 has zero-drift audit wording. W2 does not. Its exit gate protects existing real typed GO rows and typed-product proof, but it does not explicitly require all 38 current rows, the six direct GO rows, and non-target parse/direct rows to maintain against `SK-V8-open`. |
| Bitmap demoted to reserve | ACCEPT | `SYNTHESIS.md`, `HANDOFF.md`, and `SPEC.md` state PMULL prefix-XOR and CSSC CTZ/bulk remain rejected as defaults and may return only as reserve research after fresh profile evidence and challenge acceptance. |
| Direct-string/digest rejected families blocked | ACCEPT | `SPEC.md` Section 10 blocks REDRESS 50-55 and REDRESS 60-72, including sidecar producers and digest/Track 2 cap-16 routes. W4 keeps `N-direct` rows as digest guard rows or routed residuals, and direct digest rows are not product-plane proof. |
| No W1-W6 dispatch before plan update | ACCEPT | `HANDOFF.md` requires W1-W6 to have W0 admission plus a wave plan naming exact owner paths, row gates, pre-blocked routes, revert protocol, and same-wave consumer. `DISPATCH-PROMPT.md` says not to dispatch W1-W6 from the prompt alone and requires fresh research and plan artifacts before any conditional wave. |

## Remaining Blocker

### CH3-B1: W2 missing full-table maintain inheritance

Disposition: REVISE.

V1 CH3 explicitly required typed product expansion to preserve more than the
four current `real_typed_struct` GO rows. The final W2 section allows codegen,
bench, generated output, and named runtime consumers, so it can still regress
parse/direct guard rows while satisfying its current typed-only exit language.

Required fix in `SPEC.md` Section 5:

```text
W2 exit gate:
- Existing real_typed_struct GO rows maintain GO.
- Existing direct_to_struct GO rows maintain GO.
- All 38 current main rows compare against SK-V8-open.
- Non-target parse/direct rows stay within the W2 plan budget.
- Any row outside budget rejects W2 with REDRESS evidence.
```

The W2 plan should also name the full-table budget before implementation, not
only typed row thresholds.

## Final CH3 Disposition

REVISE.

No bitmap, direct-string, digest, W0, pre-block, or dispatch-sequencing blocker
remains. The only remaining CH3 blocker is the missing W2 full-table maintain
gate.
