# SK-V6 R3c retained offset/tape emission and structural costs post-Candidate4

Date: 2026-05-14.
Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
Scope: retained Track 1 generated runtime after REDRESS 63 / Candidate-4 `ContainerNext` admission.
Repo edits: none. Pre-existing staged dirty state remained untouched: `skinny/crates/bbnf-bench/src/metadata.rs`, `skinny/xtask/src/bin/capacity_probe.rs`.

## Authority Read

Current `skinny/RESULTS.md` records retained parse G rows on `citm_catalog`, `random`, `instruments`, `distinct_values`, and `update_center`; `canada`, `marine_ik`, `mesh`, and `numbers` are retained GO controls. REDRESS item 63 admits the array `ContainerNext` / next-byte carry intervention and explicitly leaves parse-G open. The key Candidate-4 attribution result was that the old redundant boundary set `consume_container_next + parse_value_at + dispatch_value` fell from 24.97% to 14.51% on `citm_catalog` and from 27.37% to 6.48% on `canada`; the new `consume_array_next` helper is the replacement boundary, not residual re-entry.

I also read `restart/skinny/tranches/sk-v6/research/skv6-R2b-parser-dispatch.md`. That pre-Candidate4 report selected the next-byte carry candidate and pre-blocked retained side tables, structural cursors, whitespace cursors, eager tape, and second scanners. This report does not reopen those rejected routes.

`skinny/crates/runtime/src/grammars/json/generated.rs` now has the Candidate-4 array shape: `parse_array` parses the first value once, then loops on `consume_array_next`, which consumes comma/close and carries the next value byte into `dispatch_value`. `parse_object` still uses the older `consume_container_next -> parse_pair -> parse_key_colon -> parse_value_at` cadence.

## Method

Built the attribution binary in an isolated target dir:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CARGO_TARGET_DIR=/tmp/skv6-cargo/R3c
cargo build --release -p xtask --bin profile-lazy --features runtime/parse-attribution
```

Captured retained Track 1 profiles:

```bash
samply record --rate 4000 --main-thread-only --unstable-presymbolicate \
  --save-only --no-open \
  -o /tmp/skv6-R3c-profiles/<row>.profile.json.gz \
  /tmp/skv6-cargo/R3c/release/profile-lazy <iters> <row-or-path>
```

Profiles:

| Row | Profile path | Iters | Attribution Mbps | Samples |
|---|---|---:|---:|---:|
| `citm_catalog` | `/tmp/skv6-R3c-profiles/citm_catalog.profile.json.gz` | 3000 | 11301 | 14575 |
| `random` | `/tmp/skv6-R3c-profiles/random.profile.json.gz` | 8000 | 5422 | 24121 |
| `instruments` | `/tmp/skv6-R3c-profiles/instruments.profile.json.gz` | 20000 | 7701 | 18311 |
| `distinct_values` | `/tmp/skv6-R3c-profiles/distinct_values.profile.json.gz` | 50000 | 2932 | 82436 |
| `update_center` | `/tmp/skv6-R3c-profiles/update_center.profile.json.gz` | 12000 | 5617 | 36482 |

The Mbps values are attribution-build values, not replacements for `RESULTS.md`. Self-time was computed from leaf frames in the Gecko profiles, mapping RVAs through the samply `.syms.json` files.

## Requested Boundary Self-Time

Percentages are leaf self-sample percentages under `runtime/parse-attribution`.

| Row | `emit_plain_offset` | `patch_flags` | `consume_structural` | `skip_ws` | `parse_key_colon` | `parse_object+array` | `consume_container_next` | `consume_array_next` | `parse_value_at` | `dispatch_value` |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `citm_catalog` | 8.82 | 0.00 | 8.68 | 7.80 | 8.56 | 2.11 | 12.79 | 10.18 | 0.58 | 3.68 |
| `random` | 6.70 | 0.00 | 2.75 | 2.33 | 7.02 | 1.69 | 8.87 | 6.29 | 0.34 | 4.11 |
| `instruments` | 5.94 | 0.00 | 2.01 | 1.70 | 16.22 | 1.57 | 9.75 | 1.45 | 0.81 | 3.46 |
| `distinct_values` | 4.33 | 0.00 | 0.46 | 0.22 | 5.44 | 0.74 | 4.33 | 0.41 | 0.47 | 1.27 |
| `update_center` | 5.35 | 0.05 | 4.17 | 0.79 | 5.33 | 1.17 | 4.93 | 1.95 | 0.55 | 2.99 |

Adjacent hot boundaries:

| Row | `consume_quote_at_cursor` | `match_tiny_plain_string` | `match_string_at_quote` | `match_number_at_digit` | `parse_string` | `parse_number` |
|---|---:|---:|---:|---:|---:|---:|
| `citm_catalog` | 2.59 | 17.03 | 6.54 | 6.11 | 0.08 | 1.24 |
| `random` | 5.77 | 35.84 | 9.49 | 4.31 | 1.55 | 0.83 |
| `instruments` | 3.67 | 29.24 | 11.50 | 7.69 | 0.21 | 1.76 |
| `distinct_values` | 6.52 | 54.08 | 18.11 | 0.52 | 1.13 | 0.08 |
| `update_center` | 4.67 | 38.51 | 26.06 | 0.00 | 1.76 | 0.00 |

## Findings

1. Offset/tape emission is real but not the next high-ceiling lever.

   `emit_plain_offset` is 4.33-8.82% self-time on the sampled rows. `patch_flags` is effectively absent here, except 0.05% on `update_center`; this is not an escape-flag bottleneck. A tape writer intervention would be capped in the single digits on most remaining parse-G rows and would risk reopening already-rejected tape-width/eager-tape territory. The data does not justify a Wave 2 tape-representation candidate.

2. Structural byte consumption is row-specific and no longer a generic parser-dispatch failure.

   `consume_structural + skip_ws` is high on `citm_catalog` at 16.48%, moderate on `random` at 5.08% and `update_center` at 4.96%, and negligible on `distinct_values` at 0.68%. This is not enough to revive a structural sidecar, whitespace cursor, or parser-local structural-mask cursor; those are pre-blocked and would violate the single-substrate discipline unless they become the canonical substrate, which is broader than an R3c/Wave 2 intervention.

3. Candidate-4 removed value re-entry, not delimiter parsing.

   `parse_value_at` is now under 1% on all five rows, so the old redundant re-entry layer is gone. `dispatch_value` is only 1.27-4.11%. The remaining delimiter cost is split between `consume_container_next` and `consume_array_next`: 22.97% combined on `citm_catalog`, 15.16% on `random`, 11.20% on `instruments`, 4.74% on `distinct_values`, and 6.88% on `update_center`. That is real comma/close/whitespace/offset work, not a hidden function-call tax.

4. Object cadence is the only remaining non-string control surface.

   `parse_object` still uses `consume_container_next -> parse_pair -> parse_key_colon`. On object-heavy rows, `consume_container_next` remains 4.33-12.79% and `parse_key_colon` remains 5.33-16.22%. This is the only plausible non-string follow-up to Candidate-4. It is not an offset/tape candidate; it is an object loop/key-boundary candidate.

5. String matching dominates most remaining rows.

   `match_tiny_plain_string + match_string_at_quote` is 23.57% on `citm_catalog`, 45.33% on `random`, 40.74% on `instruments`, 72.19% on `distinct_values`, and 64.57% on `update_center`. This means an object-control intervention cannot close the remaining matrix by itself. It can at most recover object-heavy rows while leaving the string/unicode cluster for a separate, non-rejected shape.

## Candidate Decision

No admissible high-impact offset/tape or structural-substrate Wave 2 candidate remains after Candidate-4.

The only admissible non-string candidate left is narrow: **object next-key carry**. It would mirror Candidate-4 for objects without adding a sidecar: after comma/whitespace, return `ObjectNext::NextKey` or a carried first byte to a `parse_pair_at_current` helper, preserving quote validation and offset emission. It must not alter string scanning, add a structural cursor, add retained side tables, or change the tape representation.

Expected impact is modest. The falsifiability gate should therefore be strict:

| Row | Required delta |
|---|---:|
| `citm_catalog` | >= +3% production `profile-lazy` Mbps |
| `random` | >= +2% production `profile-lazy` Mbps |
| `instruments` | >= +2% production `profile-lazy` Mbps |
| `update_center` | >= +1.5% production `profile-lazy` Mbps |
| `distinct_values` | no regression >1%, because it is string-bound |

Attribution gate: `consume_container_next + parse_key_colon + parse_pair` must fall by at least 15% relative on `citm_catalog` and `instruments`; `emit_plain_offset + patch_flags` must not rise above 10% on any sampled row; `match_tiny_plain_string + match_string_at_quote` must not rise by more than 3% relative.

If that object candidate fails, R3c sees no remaining non-string retained parse route inside the current Wave 2 rules. The next candidate should move to the string/unicode cluster or to the Wave 3 direct-to-struct bridge, not to eager tape, sidecar cursors, structural prepasses, or offset-width churn.

## Pre-Blocked Routes Reconfirmed

- Retained side tables: rejected by REDRESS 50 and not supported by this profile.
- Parser-local structural mask cursor / structural sidecar: rejected by REDRESS 53; `consume_structural + skip_ws` is not broad enough here to justify reopening it.
- Eager tape / tape-width churn: previous NO-GO; `emit_plain_offset` is visible but not dominant enough to justify representation churn.
- New string wide-scan variants: REDRESS 60-62 already rejected the recent string routes; this R3c pass does not provide a new string candidate.

## Bottom Line

Post-Candidate4 retained Track 1 is no longer value re-entry bound. Offset emission is a measurable single-digit cost, structural consumption is row-specific, and object delimiter/key cadence is the sole remaining non-string parser-control surface. The only non-string Wave 2 candidate worth testing is object next-key carry, with a low-ceiling gate; otherwise the evidence says to stop spending Wave 2 effort on non-string substrate mechanics and return to string/unicode or direct materialization.
