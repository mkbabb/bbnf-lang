# SK-V13 W15.1 Research - UpdateCenter Typed Plugin Fast Path

Date: 2026-05-22.
Wave: W15.1.
Target row: `json/update_center/real_typed_struct/main`.

## Gate Facts

The current full-SOTA rolling row is OPEN:

| row | Track 1 | sonic strict + 1 | margin |
|---|---:|---:|---:|
| `json/update_center/real_typed_struct/main` | 11774 | 12561 | -787 |

`skinny/RESULTS.md` still carries the pre-pin `A / GO` typed status for this
row, but under the SK-V13 addendum the active bar is strict sonic + 1 Mbps.
W14.5 routed update_center typed as one of the closest remaining row-moving
targets after report-only parse admissions were exhausted.

## Hot Leaf And Shape

The SK-V13 P1 hot-leaf attribution names `parse_type_plugin` for
`update_center` typed. Local source confirms the profile shape:

- `parse_update_center` enters `parse_type_update_center`.
- The `plugins` field parses a `Vec<PluginEntry<'i>>` with capacity `768`.
- The fixture contains `654` plugin entries.
- Each plugin entry parses a map key, then `parse_type_plugin`, then stores
  `PluginEntry { key, value }`.
- `parse_type_plugin` retains only `name`, `title`, `url`, and `version`, but
  it still allocates/parses every object key as a `Cow` and dispatches through
  a string match for fourteen ignored fields.

The retained checksum consumes plugin count, plugin key, and the four retained
plugin strings, so a digest shortcut is not admissible.

## Fixture Regularity

The fixture has six plugin object orders. All share the same spine:

```text
buildDate, [compatibleSinceVersion], dependencies, developers, excerpt, gav,
[labels], name, [previousTimestamp, previousVersion], releaseTimestamp,
requiredCore, scm, sha1, title, url, version, [wiki]
```

Counts from `skinny/test_data/update-center.json`:

| order family | count |
|---|---:|
| labels + previous + wiki | 495 |
| labels + wiki, no previous | 125 |
| no labels, no previous, no wiki | 15 |
| no labels, previous, no wiki | 11 |
| compatible + labels + previous + wiki | 6 |
| compatible + labels + wiki, no previous | 2 |

## Candidate Differential

Add an optimistic ordered parser for the `Plugin` type in the generated typed
direct consumer. The fast path consumes literal field names without materialized
key `Cow`s, skips ignored fields with the existing validating skippers, retains
the same four output strings, and returns `Err` on any shape mismatch. The
generic `parse_type_plugin` stores the starting cursor, tries the fast path,
and resets the cursor before falling back to the current generic object parser
on any fast-path error.

This is materially distinct from REDRESS 119/120/143 and W11.4:

- not direct_to_struct output digest work;
- not direct cursor byte-fetch specialization;
- not parser substrate or parse-only structural replay;
- attacks the typed product hot leaf named by P1.

## Owner Paths

- `skinny/crates/codegen/src/json_typed_direct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs` for existing parity tests
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w15.1/`

No CSS sidecar JSON file is in scope.
