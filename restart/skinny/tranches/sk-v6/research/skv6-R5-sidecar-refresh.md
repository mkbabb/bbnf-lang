# SK-V6 Wave 1 R5 - Sidecar Comparator Refresh

Date: 2026-05-14 16:25 EDT.
Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
Authority read: `restart/skinny/audit/IMPLEMENTATION-AGENT-PROMPT-SK-V6.md`
sections 1, 2, Wave 1 R5, and 7; `skinny/RESULTS.md`;
`restart/skinny/audit/SK-V5-COHORT/skv5-B3-native-sidecars.md`;
`restart/skinny/audit/SK-V5-COHORT/skv5-A1-comparative.md`.

No tracked files were modified, staged, or committed. The worktree was already
dirty before this dispatch in:

- `skinny/crates/bbnf-bench/src/metadata.rs`
- `skinny/xtask/src/bin/capacity_probe.rs`

## Summary

The only sidecar source still present and buildable under the cap is
`/tmp/asmjson-research`. I rebuilt and reran its existing quick Criterion bench
with `CARGO_TARGET_DIR=/tmp/skv6-cargo/R5-asmjson`. That is a permissive
arm64 SWAR flaw probe only, not a strict SOTA target.

The strict 17-corpus rows available now are the current in-tree
`skinny/RESULTS.md` rows for:

- `sonic-rs` eager `Value` DOM anchor
- Rust `simd-json` borrowed and owned value anchors
- `sonic-rs` direct-to-struct
- `serde_json` direct-to-struct

The C++/C sidecar sources and binaries for `simdjson C++`, `yyjson`, and the
standalone `serde_json` profile driver are absent from `/tmp`, so those were not
rerun. Their profile artefacts remain available but are stale/profile-only:
`simdjson C++` covers 13 skinny corpora; `yyjson` covers 6 exact skinny corpora
plus one `unicode_heavy` analogue; standalone `serde_json` profile covers 6
corpora.

Current bbnf Track 1 remains `deferred / view-boundary / yes`, so ratios
against strict sidecars below are Wave 2/4 target deltas, not strict-vs-strict
win claims.

## Sidecar Availability

| Sidecar | Source now | Binary or harness now | Rows now | Rerun status | Strictness plane |
|---|---|---|---|---|---|
| `sonic-rs` in-tree harness | Upstream `/tmp/sonic-research` absent. Workspace harness present at `skinny/crates/bbnf-bench/benches/json_parity.rs`. | Criterion outputs under `skinny/target/criterion/json_*`; current rows folded into `skinny/RESULTS.md`. | 17 current eager `Value` DOM rows and 17 current direct-to-struct rows. Profile coverage for 9 rows in `skinny/profile/sonic-rs-expanded/`. | Not rerun; `RESULTS.md` is current authority. | strict / scan-boundary / escape-complete yes. |
| Rust `simd-json` in-tree harness | Workspace harness present at `skinny/crates/bbnf-bench/benches/json_parity.rs`. This is not C++ simdjson. | Criterion outputs under `skinny/target/criterion/json_*`; current rows folded into `skinny/RESULTS.md`. | 17 current borrowed rows and 17 current owned rows. | Not rerun; `RESULTS.md` is current authority. | strict / scan-boundary / escape-complete yes. |
| `simdjson C++` | `/tmp/simdjson-research` absent. | Driver and binaries absent. | 13 profile-only rows in `skinny/profile/simdjson-expanded/`. | Not rerun; source absent. Mark stale/profile-only. | strict / scan-boundary / escape-complete yes. |
| `yyjson` | `/tmp/yyjson-research` absent. | `yy_bench` absent. | 6 exact profile-only rows plus `unicode_heavy` analogue in `skinny/profile/yyjson/`. | Not rerun; source absent. Mark stale/profile-only. | strict default RFC 8259 / parse-boundary / escape-complete yes. |
| `asmjson` SWAR | `/tmp/asmjson-research` present. | Built and ran `/tmp/skv6-cargo/R5-asmjson/release/deps/parse-*`. | Synthetic `string_array`, `string_object`, `mixed` only; not row-aligned to 17 skinny corpora. | Refreshed in this dispatch. | permissive / no UTF-8 validation / flaw probe only. |
| `serde_json` standalone profile | `/tmp/serde_json-bench` absent. Workspace direct and DOM harnesses present in `json_parity.rs`. | Current direct rows in `skinny/RESULTS.md`; 6 stale profile rows in `skinny/profile/serde_json/`. | 17 current direct-to-struct rows; profile coverage for 6 DOM rows. | Not rerun; standalone source absent. | strict / scan-boundary via Rust string validation / escape-complete yes. |

## Current 17-Corpus Strict In-Tree Rows

Source: `skinny/RESULTS.md`, current gate output. API/output planes:

- `sonic-rs Value`: `sonic_rs::from_slice::<sonic_rs::Value>`, eager typed DOM.
- `simd-json borrowed`: `simd_json::to_borrowed_value`, borrowed value DOM.
- `simd-json owned`: `simd_json::to_owned_value`, owned value DOM.
- `sonic-rs direct`: `bbnf_bench::direct_struct::sonic_digest`, typed serde direct-to-struct digest.
- `serde_json direct`: `bbnf_bench::direct_struct::serde_digest`, typed serde direct-to-struct digest.

All rows in this table are strict sidecar rows (`strict / scan-boundary / yes`).

| Corpus | sonic-rs Value Mbps | simd-json borrowed Mbps | simd-json owned Mbps | sonic-rs direct Mbps | serde_json direct Mbps |
|---|---:|---:|---:|---:|---:|
| twitter | 21176 | 14658 | 12231 | 15614 | 11546 |
| citm_catalog | 25413 | 16532 | 14945 | 21874 | 14594 |
| canada | 13719 | 6275 | 6331 | 12606 | 7806 |
| apache_builds | 17453 | 15628 | 11987 | 11791 | 10568 |
| github_events | 23219 | 17915 | 14591 | 17217 | 13930 |
| update_center | 19835 | 12074 | 8890 | 12620 | 9107 |
| mesh | 11871 | 7311 | 7426 | 9691 | 7927 |
| random | 15451 | 9631 | 7431 | 10021 | 7129 |
| gsoc-2018 | 48816 | 24039 | 19236 | 24392 | 20179 |
| marine_ik | 9977 | 7028 | 6823 | 8809 | 7664 |
| instruments | 19714 | 12576 | 10721 | 13358 | 10704 |
| numbers | 13523 | 8810 | 9101 | 12583 | 8623 |
| unicode_mixed | 15681 | 8570 | 7658 | 11117 | 5299 |
| unicode_escapes | 19090 | 4697 | 4606 | 14427 | 5264 |
| unicode_basic | 15753 | 9373 | 7107 | 9647 | 6112 |
| distinct_values | 17828 | 11903 | 9058 | 13214 | 8825 |
| y_string_unicode | 13633 | 6385 | 5662 | 8877 | 7634 |

## C++/C Profile-Only Sidecars

Source: `skinny/profile/simdjson-expanded/PROFILE-REPORT.md`,
`skinny/profile/yyjson/PROFILE-REPORT.md`, and the SK-V5 B3 sidecar audit.
These rows are same-machine M5 Max profiles from 2026-05-12 but were not
refreshed because their source checkouts and binaries are absent.

| Corpus | simdjson C++ DOM Mbps | simdjson hot path | yyjson DOM Mbps | yyjson hot path | Profile path |
|---|---:|---|---:|---|---|
| twitter | 24518 | stage1 dominant | 30932 | `yyjson_read_opts` | `skinny/profile/simdjson-expanded/`, `skinny/profile/yyjson/` |
| citm_catalog | 35819 | stage1 dominant | 20954 | `yyjson_read_opts` | `skinny/profile/simdjson-expanded/`, `skinny/profile/yyjson/` |
| canada | 11491 | stage2 number dominant | 13002 | `yyjson_read_opts` | `skinny/profile/simdjson-expanded/`, `skinny/profile/yyjson/` |
| apache_builds | 36009 | stage1 dominant | 16273 | `yyjson_read_opts` | `skinny/profile/simdjson-expanded/`, `skinny/profile/yyjson/` |
| github_events | 39637 | stage1 dominant | 21423 | `yyjson_read_opts` | `skinny/profile/simdjson-expanded/`, `skinny/profile/yyjson/` |
| update_center | 30584 | stage1 near stage2 | 18536 | `yyjson_read_opts` | `skinny/profile/simdjson-expanded/`, `skinny/profile/yyjson/` |
| mesh | 9413 | stage2 number dominant | - | - | `skinny/profile/simdjson-expanded/` |
| random | 20635 | stage1 dominant plus UTF-8 checker | - | - | `skinny/profile/simdjson-expanded/` |
| gsoc-2018 | - | - | - | - | none |
| marine_ik | - | - | - | - | none |
| instruments | - | - | - | - | none |
| numbers | - | - | - | - | none |
| unicode_mixed | 13146 | stage2 string/UTF-8 dominant | 10302* | `yyjson_read_opts` | `skinny/profile/simdjson-expanded/`, `skinny/profile/yyjson/` |
| unicode_escapes | 5635 | stage2 string escape dominant | - | - | `skinny/profile/simdjson-expanded/` |
| unicode_basic | 16275 | stage1 plus UTF-8 checker | - | - | `skinny/profile/simdjson-expanded/` |
| distinct_values | 22817 | stage1 dominant | - | - | `skinny/profile/simdjson-expanded/` |
| y_string_unicode | 13624 | stage2 string escape dominant | - | - | `skinny/profile/simdjson-expanded/` |

`*` yyjson `unicode_heavy` is a 384 KiB synthesized analogue, not the exact
1 MiB `unicode_mixed` fixture. Keep it out of strict row claims unless Wave 4
restores the yyjson source and reruns exact fixtures.

## Refreshed asmjson SWAR Flaw Probe

Command:

```bash
cd /tmp/asmjson-research
CARGO_TARGET_DIR=/tmp/skv6-cargo/R5-asmjson \
  cargo bench --bench parse -- --quick --warm-up-time 1 --measurement-time 3
```

Values below are Criterion mean point estimates converted to Mbps
(`bytes * 8000 / ns`). These are not skinny 17-corpus rows; they are the
asmjson-shipped 10 MiB synthetic corpora. asmjson SWAR remains permissive and
is excluded from SOTA target selection.

| Synthetic workload | asmjson/u64 Mbps | simd-json Mbps | sonic-rs Mbps | serde_json Mbps | Plane |
|---|---:|---:|---:|---:|---|
| string_array | 25370 | 25696 | 50751 | 21040 | permissive flaw probe |
| string_object | 20347 | 13871 | 36016 | 5158 | permissive flaw probe |
| mixed | 4893 | 2907 | 3963 | 1826 | permissive flaw probe |

Hot path: no fresh `samply` PC profile was taken under the cap. Existing B3
attribution remains the only hot-leaf source: `parse_u64_*` about 75%,
`parse_string_u64` about 15%, inline classifier about 10%.

Compared with `skinny/profile/native-sidecars/asmjson/bench.log`, the refreshed
mean is lower on `string_array` and `mixed` and similar on `string_object`.
Because the workload is synthetic and permissive, this only refreshes the flaw
probe reality on this machine.

## Strict-vs-Strict Target Table

This table picks the highest strict sidecar target currently known for each
skinny corpus among `sonic-rs`, `simdjson C++`, and `yyjson`. `bbnf Track 1`
is included only to size the Wave 2/4 gap; current Track 1 strictness is still
deferred, so these ratios are not strict-vs-strict pass claims.

| Corpus | bbnf Track 1 Mbps | Strict target | API/output plane | Target Mbps | T1 / target | Freshness | Hot leaf/profile path |
|---|---:|---|---|---:|---:|---|---|
| twitter | 12303 | yyjson | `yyjson_read_opts` -> `yyjson_doc` DOM | 30932 | 39.8% | stale profile-only | `yyjson_read_opts`; `skinny/profile/yyjson/` |
| citm_catalog | 20775 | simdjson C++ | `dom::parser::parse` -> DOM | 35819 | 58.0% | stale profile-only | stage1 dominant; `skinny/profile/simdjson-expanded/` |
| canada | 17738 | sonic-rs | `from_slice::<Value>` -> eager typed DOM | 13719 | 129.3% | current RESULTS | no 17-row profile; closest expanded profile is number/Eisel-Lemire heavy |
| apache_builds | 12341 | simdjson C++ | `dom::parser::parse` -> DOM | 36009 | 34.3% | stale profile-only | stage1 dominant; `skinny/profile/simdjson-expanded/` |
| github_events | 13161 | simdjson C++ | `dom::parser::parse` -> DOM | 39637 | 33.2% | stale profile-only | stage1 dominant; `skinny/profile/simdjson-expanded/` |
| update_center | 9430 | simdjson C++ | `dom::parser::parse` -> DOM | 30584 | 30.8% | stale profile-only | stage1 near stage2; `skinny/profile/simdjson-expanded/` |
| mesh | 13411 | sonic-rs | `from_slice::<Value>` -> eager typed DOM | 11871 | 113.0% | current RESULTS | expanded profile available; number-heavy path |
| random | 7794 | simdjson C++ | `dom::parser::parse` -> DOM | 20635 | 37.8% | stale profile-only | stage1 plus UTF-8 checker; `skinny/profile/simdjson-expanded/` |
| gsoc-2018 | 21907 | sonic-rs | `from_slice::<Value>` -> eager typed DOM | 48816 | 44.9% | current RESULTS | no sidecar PC profile found |
| marine_ik | 12818 | sonic-rs | `from_slice::<Value>` -> eager typed DOM | 9977 | 128.5% | current RESULTS | no sidecar PC profile found |
| instruments | 11887 | sonic-rs | `from_slice::<Value>` -> eager typed DOM | 19714 | 60.3% | current RESULTS | no sidecar PC profile found |
| numbers | 18740 | sonic-rs | `from_slice::<Value>` -> eager typed DOM | 13523 | 138.6% | current RESULTS | no sidecar PC profile found |
| unicode_mixed | 8720 | sonic-rs | `from_slice::<Value>` -> eager typed DOM | 15681 | 55.6% | current RESULTS | expanded profile available; NEON string block plus UTF-8 validation |
| unicode_escapes | 12848 | sonic-rs | `from_slice::<Value>` -> eager typed DOM | 19090 | 67.3% | current RESULTS | expanded profile available; string escape/unicode codepoint path |
| unicode_basic | 10898 | simdjson C++ | `dom::parser::parse` -> DOM | 16275 | 67.0% | stale profile-only | stage1 plus UTF-8 checker; `skinny/profile/simdjson-expanded/` |
| distinct_values | 6097 | simdjson C++ | `dom::parser::parse` -> DOM | 22817 | 26.7% | stale profile-only | stage1 dominant; `skinny/profile/simdjson-expanded/` |
| y_string_unicode | 6084 | sonic-rs | `from_slice::<Value>` -> eager typed DOM | 13633 | 44.6% | current RESULTS | no sonic profile; simdjson profile has stage2 string escape dominant at 13624 Mbps |

Rows whose target comes from `simdjson C++` or `yyjson` need a Wave 4 rerun if
the source trees are restored. Rows whose target comes from `sonic-rs` are
current `skinny/RESULTS.md` rows on this machine.

## Direct Workload Target Notes

For direct-to-struct synthesis, `skinny/RESULTS.md` already contains the current
strict sidecar floor/target rows:

- SOTA target: `sonic_rs_direct_to_struct`, 17 rows, strict typed serde direct.
- Rust floor: `serde_json_direct_to_struct`, 17 rows, strict typed serde direct.
- No simdjson C++ direct visitor or yyjson typed direct row exists in current
  artefacts.

The direct rows that remain largest against strict `sonic-rs direct` are:

| Corpus | bbnf Track 1 direct Mbps | sonic-rs direct Mbps | T1 / sonic | serde_json direct Mbps |
|---|---:|---:|---:|---:|
| unicode_escapes | 5262 | 14427 | 36.5% | 5264 |
| unicode_mixed | 4633 | 11117 | 41.7% | 5299 |
| distinct_values | 6212 | 13214 | 47.0% | 8825 |
| y_string_unicode | 5006 | 8877 | 56.4% | 7634 |
| gsoc-2018 | 15115 | 24392 | 62.0% | 20179 |
| update_center | 8534 | 12620 | 67.6% | 9107 |
| github_events | 12411 | 17217 | 72.1% | 13930 |
| twitter | 11932 | 15614 | 76.4% | 11546 |

These direct targets are current and strict, but hot PC attribution for the
current direct sidecar rows was not found in the sidecar profile artefacts.

## Provenance

- Current 17-row authority: `skinny/RESULTS.md`.
- In-tree harness source: `skinny/crates/bbnf-bench/benches/json_parity.rs`.
- simdjson C++ stale profile: `skinny/profile/simdjson-expanded/PROFILE-REPORT.md`,
  `skinny/profile/simdjson-expanded/throughput.json`.
- yyjson stale profile: `skinny/profile/yyjson/PROFILE-REPORT.md`.
- sonic-rs expanded stale profile subset: `skinny/profile/sonic-rs-expanded/PROFILE-REPORT.md`,
  `skinny/profile/sonic-rs-expanded/throughput.json`.
- serde_json stale profile subset: `skinny/profile/serde_json/PROFILE-REPORT.md`.
- asmjson previous profile/report: `skinny/profile/native-sidecars/asmjson/NOTE.md`,
  `skinny/profile/native-sidecars/asmjson/bench.log`.
- asmjson refreshed Criterion output: `/tmp/skv6-cargo/R5-asmjson/criterion/`.

End of report.
