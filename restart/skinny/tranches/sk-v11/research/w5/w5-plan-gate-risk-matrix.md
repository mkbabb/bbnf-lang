# SK-V11 W5 Plan - Gate And Risk Matrix

Date: 2026-05-20.

Disposition: PLAN companion matrix for `G-W5-STRING-SPAN-DIRECT`.

## Gate Binding

W5 selects one row:

| Row | Track 1 open | Track 2 open | sonic direct | Binding floor |
|---|---:|---:|---:|---:|
| `random/direct_to_struct` | 7693 | 6949 | 8665 | 7878 |

Admission requires Track 1 and independent Track 2 >= 7878 Mbps in the same
native Criterion root with same-run sonic-rs and serde_json direct comparator
rows. Probe evidence may permit Criterion but cannot admit the row.

## Guard Block

Direct guard floors:

| Row | Track 1 floor | Track 2 floor |
|---|---:|---:|
| `citm_catalog/direct_to_struct` | 18191 | 17431 |
| `apache_builds/direct_to_struct` | 11028 | 9996 |
| `marine_ik/direct_to_struct` | 8759 | 9248 |
| `unicode_basic/direct_to_struct` | 2253 | 2182 |

Typed guard floors:

| Row | Track 1 floor | Track 2/oracle floor |
|---|---:|---:|
| `twitter/real_typed_struct` | 17385 | 15593 |
| `citm_catalog/real_typed_struct` | 29928 | 17321 |
| `apache_builds/real_typed_struct` | 8308 | 6754 |
| `github_events/real_typed_struct` | 11633 | 12029 |
| `update_center/real_typed_struct` | 11613 | 10150 |
| `mesh/real_typed_struct` | 9214 | 7739 |
| `marine_ik/real_typed_struct` | 11552 | 9894 |

Unicode residual monitors:

| Row | Floor | W5 status |
|---|---:|---|
| `unicode_escapes/direct_to_struct` | 3441 | monitor only |
| `unicode_mixed/direct_to_struct` | 2588 | monitor only |
| `y_string_unicode/direct_to_struct` | 3950 | monitor only |

## Non-JSON Route

REDRESS 113 remains blocked. W5 may state that the span shape is
grammar-neutral by parameterization, but it may not claim a generated non-JSON
parser intervention, non-JSON row movement, or close-condition satisfaction.

Any generic behavior change beyond the bounded scalar helper returns REVISE
unless it lands a same-wave generated CSS L4 / Sheets / BBNF-self parser proof
under new authority. This plan does not create that authority.

## Provenance Requirements On Admission

If `random` admits, gate/report must consume strict measured-row direct
evidence with:

- `same_wave_consumer_class=gate_json_direct_contract`
- `wave_id=SK-V11-W5`
- `redress_entry=REDRESS-116`
- `sk_v9_open_delta=bounded-string-span`
- `output_plane=digest`
- independent Track 2 verified on the same output plane

Gate/report must reject stale W2/W10/W4 provenance, `gate_only`, deferred
validation, coupled Track 2, non-digest output, missing REDRESS, or a row in
the false accept band below 7878 Mbps.

## Preblocked Routes

Reject any W5 plan or patch that reopens:

- REDRESS 28/33 tiny-string NEON/TBL parser wiring;
- REDRESS 60-62 retained trusted-string boundary collapse or wide retained
  scans;
- REDRESS 72 global cap transfer from retained parse to direct/Track 2;
- REDRESS 83 retained `StringBlock16` wrapper;
- REDRESS 106 primitive-parity-only string-block production;
- REDRESS 49/54/55 decoded visitor, exact stats sink, or streaming hash;
- REDRESS 66-69 direct source-hook receiver, parser-owned decoded scratch,
  byte-output materializer, or semantic string facts;
- W3 union/event/class-column/streaming cursor or parse-only row movement.

## Challenge Questions

1. Does the plan select exactly one string/key consumer and exactly one row?
2. Does the scalar helper return offsets only and leave decoded bytes,
   surrogate policy, hashes, semantic facts, and scratch outside the helper?
3. Does the selected cap have direct-plane evidence rather than retained
   parse-only evidence?
4. Does Track 2 remain independent from generated Track 1?
5. Is REDRESS 113 carried forward honestly?
6. Are Unicode rows monitored rather than silently admitted?
7. Does probe-first measurement stop the wave before Criterion when movement is
   too small?
8. Is the revert slice complete and saved before source restoration on reject?

## Disposition Rule

CHALLENGE should ACCEPT only if the plan remains scalar-only, row-limited to
`random`, probe-first, and provenance-consumed. It should REVISE if it adds a
second row, a SIMD body, a typed generated consumer, or generic non-JSON claims
without new measurement authority. It should REJECT if the plan relies on
primitive parity, PMU, parse-only, or prose generality as row admission.
