# SK-V11 W5 CH3: REDRESS Regression Challenge

Date: 2026-05-20.
Scope: CH3 regression / REDRESS lens for W5 Phase 2 plan artifacts:
`w5-plan-string-span-implementation.md` and
`w5-plan-gate-risk-matrix.md`.
This is research-only except this artifact.

Disposition: **ACCEPT - scalar JSON direct W5 redress authorized under the
guardrails below**.

## Read Set

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 9.
- W5 Phase 1 artifacts R1 through R6, especially
  `w5-R6-preblocked-risk.md`.
- W5 Phase 2 plan artifacts:
  `w5-plan-string-span-implementation.md` and
  `w5-plan-gate-risk-matrix.md`.
- `skinny/REDRESS.md` entries 28, 33, 49, 54, 55, 60, 61, 62, 66, 67, 68,
  69, 72, 83, and 106.
- Carried-forward SK-V11 context for REDRESS 113, 114, and 115, because W5
  must not claim non-JSON, numeric, or container-tail closure.

## Finding

The W5 plan does not repeat a preblocked REDRESS route as written. It selects a
scalar, JSON-direct, cap-8 bounded plain-string span consumed by generated
direct `parse_string_direct`, with one selected row:
`random/direct_to_struct`.

The material differential is load-bearing. W5 may return offsets and
decode-needed status only: `content_start`, `content_end`, `raw_end`, and
`needs_decode=false` on the bounded plain path. A miss must leave the cursor
unchanged and fall back to the existing trusted full matcher. W5 may not return
decoded bytes, decoded stats, hashes, semantic facts, parser-owned scratch, a
retained wrapper, or a widened retained scan.

This is accepted as a redress attempt, not as row admission. Admission still
requires same-run measured floors, independent Track 2/oracle equality on the
same digest output plane, same-wave gate/report consumption, and honest
recording of residual Unicode rows.

## Regression Checks

| Risk | CH3 result |
|---|---|
| REDRESS 28/33 tiny-string NEON/TBL replay | Acceptable as written. The plan is scalar-only, keeps generated direct cap 8, and does not wire the rejected 16-byte NEON/TBL tiny-string parser path. Any SIMD body, active tiny-string dispatch replay, or primitive-parity production claim returns to REVISE before implementation or REJECT if used as admission evidence. |
| REDRESS 60-62 retained trusted-string scans | Acceptable. The plan is generated direct `SinkOnly`, not retained parse. It preserves the full matcher fallback and does not delete the scalar short-string early-out, add a 64-byte retained scanner, or delay into a wide retained scan. Any parse-that retained widening or full-string scan route is REJECT. |
| REDRESS 72 cap transfer | Acceptable only because the selected cap is direct-plane cap 8. Generated retained cap-16 evidence does not transfer to generated direct, hand Track 2, typed, or non-JSON. Any cap-16 direct policy or mismatched Track 1/Track 2 cap semantics is REJECT. |
| REDRESS 83 / 106 string-block wrapper or primitive proof replay | Acceptable. The plan uses no `StringBlock16`, no `bbnf-simd` body, and no existing checkasm result as row proof. If native code appears, CH3 requires a fresh scalar oracle, strict parity, caller microbench, selected-row floors, and REDRESS 106 material differential before production wiring. |
| REDRESS 49/54/55 decoded visitor, exact stats, or streaming hash | Acceptable. The bounded helper returns spans only and preserves the existing source-hook/fallback materialization behavior. It must not become a no-allocation decoded visitor, exact decoded stats sink, quote-source streaming hash, or any replacement for the current allocate-then-contiguous-hash baseline. |
| REDRESS 66-69 direct materialization/scratch/facts | Acceptable. The plan does not fold source-hook receiver overhead, thread parser-owned decoded scratch, rewrite `unescape_json_string` byte output, or add semantic string facts/hash side channels. Any such addition is REJECT for W5. |
| REDRESS 113/114/115 carry-forward | Acceptable. W5 does not close the blocked non-JSON axis, W3 numeric direct rejection, or W4 container-tail rejection. Those must remain carried forward in gate/report and REDRESS text. |

## Guard Floors

The selected row is `random/direct_to_struct`. Admission requires Track 1 and
independent Track 2/oracle to both measure at least `7878` Mbps in the same
native Criterion root, with same-run sonic-rs and serde_json direct comparator
rows.

The Track 2 floor is not optional. The open Track 2 value is below the W5 floor,
so W5 cannot admit by improving generated Track 1 alone. If `direct_struct.rs`
changes, it must implement an independent local cap-8 scalar path with the
same semantic fixtures and must not call generated Track 1, generated helpers,
or a shared generated span symbol. If Track 2 remains unchanged and misses the
floor, the redress records rejection rather than lowering the floor or
substituting probe movement.

The direct guard floors from the plan remain mandatory:

| Row | Track 1 floor | Track 2 floor |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

The typed guard floors from the plan remain mandatory:

| Row | Track 1 floor | Track 2/oracle floor |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

Unicode residual rows are monitors, not W5 admissions. `unicode_escapes`,
`unicode_mixed`, and `y_string_unicode` must be measured honestly and must not
be silently admitted or demoted. If a later CHALLENGE selects
`y_string_unicode`, its `3950` Mbps floor becomes binding for W5; this plan does
not select it.

Probe movement remains a sub-gate only. It may permit Criterion when at least
one `random` track improves by `>= 1.0%` and the other does not regress by more
than `0.5%`; it cannot admit the row.

## Gate And Provenance Requirements

Gate/report consumption must land in the same redress slice. Any admission
requires strict measured-row evidence with:

- `same_wave_consumer_class=gate_json_direct_contract`;
- `wave_id=SK-V11-W5`;
- `redress_entry=REDRESS-116`, unless the ledger advances before W5 redress;
- `output_plane=digest`;
- independent Track 2/oracle equality on the same output plane.

`gate-json --with-cost-facts --check-results` must reject stale W2/W10/W4
provenance, `gate_only`, deferred validation, coupled Track 2, non-digest
output, missing REDRESS, wrong same-wave consumer class, and any false accept
below `7878` Mbps.

## Revert Completeness

The plan's revert protocol is complete for the accepted selected slice only if
the implementation stays inside its declared owners: `sink_direct.rs`,
regenerated `generated.rs`, independent `direct_struct.rs` changes,
`json_parity.rs`, gate/report files, `RESULTS.md`, and `REDRESS.md`.

On any parity failure, row-floor miss, guard regression, Unicode residual
regression, REDRESS replay, stale provenance accept, or Track 2 coupling, W5
must save the rejected implementation patch, restore the entire W5 touched
slice together, and preserve unrelated user or agent edits. Generated output
must never be hand-patched without the renderer source and `check-json`.

If implementation drifts into `parse-that-regex`, `bbnf-simd`, generic codegen,
runtime outside generated JSON, typed generated behavior, or a non-JSON proof
surface before a new CHALLENGE approval, the plan returns to REVISE. If such
edits nevertheless occur and measurement rejects the attempt, those touched
files are part of the same revert slice; they cannot be left behind as proof
scaffolding.

## Disposition

DISPOSITION: ACCEPT.

CH3 authorizes the W5 scalar JSON direct bounded string-span redress attempt
with cap 8, one generated direct `parse_string_direct` consumer, one selected
row `random/direct_to_struct`, independent Track 2/oracle enforcement, direct
and typed guard floors, Unicode residual monitoring, and same-wave gate/report
consumption.

Any second target row, typed generated consumer, generic parse-that API,
`bbnf-simd` body, non-JSON generated-parser claim, cap-16 transfer, decoded
materialization, semantic fact, coupled Track 2, primitive-only proof, or
incomplete revert converts this CH3 decision to REVISE or REJECT under the
specific conditions above.
