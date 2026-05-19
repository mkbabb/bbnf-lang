# SK-V10 S-P3 P3-B Wave Sequencing

Date: 2026-05-19.
Scope: S-P3 sequencing artifact only. This file does not authorize source
implementation; it constrains the later `SPEC.md` and `DISPATCH-PROMPT.md`.

## Authorities Read

- `restart/audit/pass-3-runtime/PASS-3.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`
- `restart/skinny/tranches/sk-v10/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` entries 94-98

## Standing Gates

1. `G-W3-UNION-SUBSTRATE` is retired by REDRESS 98. No wave may reopen it
   through a class column, streaming cursor, structural cursor, `UnionTape`,
   renamed W3, or W4 cascade dependency.
2. `parse_only` remains 17 `S / NO-GO` rows and is never a SOTA admission or
   close target in SK-V10.
3. Row movement requires same-wave `gate-json` consumption, same-run run id,
   comparator strictness, validation path, output plane, and independent
   Track 2/oracle evidence where the ledger requires it.
4. All substrate/kernel candidates are micro-prove-first: scalar oracle,
   checkasm target where applicable, host flags, feature gates, representative
   corpus slices, current production caller, failure threshold, and same-wave
   consumer must exist before production wiring.
5. Lock 14 remains binding. Generic crate, codegen, or runtime-outside-JSON
   edits require grammar-neutral proof plus named CSS L4 / Sheets / BBNF-self
   evidence. JSON-only wins stay inside JSON/bench owner paths.
6. Every wave has a redress execution cap of 90 minutes. Pre-redress research
   and plan phases use the dispatch protocol caps of 30 minutes per agent, and
   CHALLENGE uses its 60-90 minute wall cap when required. Hitting the redress
   cap without the entry/exit evidence records a REDRESS rejection or
   proof-only close; it does not extend the wave.

## Owner Families

| Owner family | Surfaces |
|---|---|
| Gate/report | `bbnf-bench` report, metadata, gate, `xtask`, `RESULTS.md`, `REDRESS.md` |
| Direct output | direct digest/output contract, `direct_struct`, `json_parity`, direct row disposition |
| Typed product | `real_typed_schema`, `real_typed_struct`, generated typed output, typed comparator rows |
| Typed root/codegen | root model, typed renderer, schema-direct proof, generated typed fixture roots |
| Kernel proof | `bbnf-simd`, `parse-that-regex`, scalar/checkasm/microbench harnesses |
| Runtime consumer | current JSON string/unescape/direct callers only; PASS-3 tape/value/path/visitor identity is a consumer constraint, not a W3 substrate license |
| S-P3 governance | wave plan, refusal gates, W3/parse firewall, close accounting |

## Sequence

| Wave | Name | Depends on | Owner family | Entry gate | Redress cap / budget | Status |
|---|---|---|---|---|---|---|
| W0 | SK-V10-open telemetry freeze | SK-V10 handoff + S-P2 V1 accepted | Gate/report | Current state is reproduced: 17 parse `S / NO-GO`, 3 direct `A / GO`, 14 direct `N-direct / NO-GO`, 6 typed `A / GO`; any emitted schema field is consumed by `gate-json`. | 120-240 gate/report LOC; no parser/runtime source. | Gate-only; no row movement. |
| W1 | Direct output/control-path contract | W0 | Direct output + Gate/report | Direct output equivalence, independent Track 2/oracle status, sonic direct comparator semantics, row floors, and revert protocol are named. Digest rows are not typed proof. | 180-320 docs/gate LOC; no source optimization. | Proof-only contract; no row movement. |
| W2 | Direct row-table reclamation | W1 | Direct output + Gate/report | Fresh same-run direct Criterion rows satisfy the W1 contract. A row may move only if Track 1 and independent Track 2/oracle both meet `ceil(sonic_direct / 1.10)` under the same run id. | 120-240 gate/report LOC; zero behavior source. | Row-moving for direct rows only. |
| W3 | W3 and parse-only firewall | W2 | S-P3 governance + Gate/report | `rg`/plan audit shows no W3 alias, no `ParseStream`/union substrate route, no W4-through-W3 dependency, and no parse-only SOTA claim. | 80-160 docs/gate LOC; zero source. | Proof-only firewall; no row movement. |
| W4 | `instruments` typed product admission | W3 | Typed product + Gate/report | Full-fixture generated typed, serde_json typed, sonic typed, and Track 2/oracle checksums match; same-run typed comparator rows exist; six existing typed rows maintain `A / GO`. | 160-260 source/generated LOC plus 40-80 gate/report LOC. | Row-moving for `instruments/real_typed_struct` only. |
| W5 | Root-type typed generalization proof | W4 | Typed root/codegen | Root model represents `Vec<T>` and numeric-string map-entry roots without JSON policy in generic code; generated/serde/sonic checksum parity is proven on fixture roots. | 220-420 source/generated LOC plus 60-120 test/gate LOC. | Proof-only unless W6 consumes it. |
| W6 | Root typed row admission | W5 | Typed product + Gate/report | One root-unblocked corpus at a time (`github_events` before `gsoc-2018` unless CHALLENGE reverses order) has same-wave generated typed, Track 2/oracle, serde typed, sonic typed, checksum parity, and typed floor evidence. | Per corpus: 160-260 source/generated LOC plus 40-80 gate/report LOC. | Row-moving for typed rows only. |
| W7 | String primitive micro-proof | W3 | Kernel proof | Exactly one of `C4-tiny-string-proof` or `C5-full-string-proof` is selected; scalar oracle, representative slices, host flags, checkasm/microbench, failure threshold, and current caller are named. | 90-260 proof LOC; no production caller wiring. | Proof-only. |
| W8 | Escape/segment micro-proof | W7 | Kernel proof | Exactly one of `C6-hex-escape-proof` or `C7-string-segment-fold` is selected; JSON slash/`\u`/surrogate policy remains generated-template owned; scalar/checkasm/microbench proof passes. | 90-260 proof LOC; no production caller wiring. | Proof-only. |
| W9 | Existing-call-site kernel production | relevant W7 or W8 proof | Kernel proof + Runtime consumer + Gate/report | Only the relevant accepted W7 or W8 proof for the exact `C4`-`C7` primitive and caller can wire into `match_string_at_quote_trusted_utf8`, `validate_unicode_escape_run`, `decode_unicode_escape`, or `unescape_string`; W10b maintain floors hold for `canada`, `citm_catalog`, `instruments`, `marine_ik`, `mesh`, and `numbers`. | 220-420 source/bench/gate LOC; split if more than one primitive, caller, plane, or target set is needed. | Row-moving only for direct/typed rows with same-wave gates; parse-only stays `S / NO-GO`. |
| W10 | Direct residual behavior tranche | W2 + W3 | Direct output + Gate/report | One direct-output/control mechanism and at most three direct target rows are named; REDRESS 73 helper-transfer and REDRESS 93 scalar-parent-fold routes remain blocked; same-run Track 1, Track 2/oracle, and sonic direct evidence meet W1 contract. | 320 source/gate LOC, or 420 total only if CHALLENGE accepts the broader C1 cap. | Row-moving for direct rows only; otherwise REDRESS reject. |
| Close | SK-V10 close accounting | All dispatched waves closed | S-P3 governance + Gate/report | Every wave is admitted, proof-closed, or REDRESS-rejected; no open source patch; `RESULTS.md` row dispositions match the accepted evidence; `gate-json` rejects missing comparator/run-id/provenance evidence. | 80-160 docs/gate LOC; zero behavior source. | Gate-only close. |

## Dependency Notes

- W1 and W3 are mandatory before any direct behavior source wave. W2 may move
  direct rows only through report/gate evidence, not through parser changes.
- W4 can run after W3 because typed product admission does not depend on the
  direct contract, but it must preserve all existing typed `A / GO` rows.
- W5 must precede W6 because `github_events` and `gsoc-2018` are root-shape
  blocked. W5 by itself cannot edit `RESULTS.md`.
- W7 and W8 are proof-only micro waves. W9 cannot dispatch if the relevant
  W7 or W8 proof for the exact primitive and caller is missing, stale-host,
  scalar-only without checkasm where required, lacking a threshold-clearing
  caller microbench artifact, or lacking a current production caller. W8
  additionally depends on W7 only when its selected primitive names a string
  proof dependency.
- W10 is deliberately after the W3 firewall so direct residual work cannot
  inherit the retired W4 cascade-lock through W3.

## Row-Movement Boundaries

Direct row movement uses the P2-G floors and the W1 contract. Current target
floors are `twitter` 13840, `canada` 10977, `apache_builds` 10020,
`github_events` 14364, `update_center` 10160, `mesh` 8916, `random` 7734,
`gsoc-2018` 20980, `instruments` 11086, `numbers` 11788, `unicode_mixed`
9314, `unicode_escapes` 12527, `distinct_values` 10022, and
`y_string_unicode` 8027 Mbps. The already-admitted direct guard rows
`citm_catalog`, `marine_ik`, and `unicode_basic` are maintain rows, not typed
proof.

Typed row movement requires generated typed output, independent Track 2/oracle,
serde_json typed, sonic typed, checksum parity, same-run Criterion rows, and
`Track 1 <= 1.10x sonic typed time`. Existing typed maintain rows are
`twitter`, `citm_catalog`, `apache_builds`, `update_center`, `mesh`, and
`marine_ik`.

Kernel production can help direct or typed rows only after proof. A kernel wave
that improves a parse-only row still records `parse_only` as `S / NO-GO`; it is
not a SOTA admission.

## Refusal Conditions

Refuse or REDRESS-reject any proposed wave that:

- admits W3, a renamed union/event substrate, or parse-only SOTA;
- uses sidecar freshness, PMU, cycles, structural-scan, masking, or probe-only
  evidence as a behavior producer;
- moves Canada typed without full-fixture generated/serde/sonic parity;
- edits generic crates or runtime outside JSON without Lock 14 non-JSON proof;
- combines multiple primitive families, checkasm, microbench, and multiple
  production consumers in one 90-minute wave;
- emits telemetry not consumed by `gate-json` in the same wave.
