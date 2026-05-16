# SK-V6 Profiling Cohort C3 - Sidecar Comparator Planes

Date: 2026-05-15
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Scope: read-only repository audit. No repository files edited. Output written only to `/tmp/skv6-C3-sidecar-planes.md`.

## Executive finding

The current sidecar plane disclosure is not schema-v3-clean. `restart/skinny/BENCH.md` states that every SOTA row must disclose strictness, UTF-8 point, escape completeness, output plane, ownership plane, feature mask, API symbol, corpus hash, hardware, build flags, sidecar freshness, and primitive/checkasm status at lines 19-24, and later says missing v3 fields are invalid at lines 676-682. The implementation still emits schema v2: `skinny/crates/bbnf-bench/src/metadata.rs:7`, `skinny/target/criterion/json_twitter/sonic_rs_anchor/metadata.toml:1`, and the v2 metadata row lacks the v3 plane fields at `skinny/target/criterion/json_twitter/sonic_rs_anchor/metadata.toml:20-28`.

The strict-anchor blocker is `sonic-rs`: the active skinny bench dependency enables `utf8_lossy` at `skinny/crates/bbnf-bench/Cargo.toml:21`. With that feature compiled, sonic-rs applies `.utf8_lossy()` globally in `from_trait` at `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.8/src/serde/de.rs:1280-1283`. Therefore every current `sonic_rs::from_slice` row in the skinny bench is `lossy_utf8`, not `strict_bytes`, unless rebuilt without the feature and rerun. Current `RESULTS.md` uses `sonic-rs` as the retained S anchor on every corpus at `skinny/RESULTS.md:5-21`, so those strict S-anchor classifications are invalid as written.

## Files read

- `restart/skinny/BENCH.md`
- `skinny/RESULTS.md`
- `Cargo.toml`, `skinny/Cargo.toml`, `skinny/crates/bbnf-bench/Cargo.toml`, `crates/core/Cargo.toml`
- `restart/skinny/tranches/sk-v6/research/skv6-A3-comparator-planes.md`
- `restart/skinny/tranches/sk-v6/research/skv6-R5-sidecar-refresh.md`
- active bench/runtime sources under `skinny/crates/bbnf-bench`, `skinny/crates/runtime`, and `skinny/crates/parse-that-regex`
- existing profile reports under `skinny/profile/`

## Current row audit

| Row family | API symbol currently used | Strictness | UTF-8 boundary | Escape completeness | Output plane | Ownership plane | S-anchor status |
|---|---|---|---|---|---|---|---|
| BBNF Track 1 retained | `runtime::generated_json::parse(&str)` at `skinny/crates/bbnf-bench/benches/json_parity.rs:43-47` and `skinny/crates/runtime/src/grammars/json/parser.rs:47-51` | `strict_after_utf8_view`, not `strict_bytes` | `&str` prevalidation; byte entry exists at `parse_bytes` but retained bench uses `from_utf8` before timing at `json_parity.rs:16` | yes for JSON string controls/escapes via `match_json_string_at_quote_trusted_utf8` at `parse-that-regex/src/lib.rs:298-340` | `retained_tape_typed_root` | owns tape, borrows input | Eligible only against similarly prevalidated rows or marked deferred against byte parsers |
| BBNF Track 2 retained | `bbnf_bench::track2::json::parse(&str)` at `json_parity.rs:65-70` | same as Track 1 | same as Track 1 | same parse/string substrate | `retained_tape_typed_root` | owns tape, borrows input | Diagnostic substrate row, not external S anchor |
| sonic-rs Value | `sonic_rs::from_slice::<sonic_rs::Value>` at `json_parity.rs:87-90` and again for checked at `104-107` | `lossy_utf8` currently | feature-driven lossy parse, not strict scan-boundary | unescaped controls still rejected in parser code, but invalid UTF-8/UTF-16 may be replaced | `dom_value` | owns sonic Value/arena; input borrowed bytes | Not eligible until `utf8_lossy` removed and rows rerun |
| sonic-rs direct/real typed | `bbnf_bench::direct_struct::sonic_digest` at `json_parity.rs:225-240`; `real_typed_struct::sonic_typed` calls `sonic_rs::from_slice::<T>` at `real_typed_struct.rs:146-157` | `lossy_utf8` currently | same feature-driven lossy path | same caveat | `typed_serde_direct` / digest stressor | target-dependent; current real typed structs use `Cow<'a, str>` at `real_typed_struct.rs:15-40` | Not eligible as strict direct S anchor until rerun without lossy feature |
| Rust simd-json borrowed | `simd_json::to_borrowed_value(&mut bytes)` at `json_parity.rs:121-130` | `strict_bytes` if probes pass | parser-owned byte validation; registry source has invalid UTF-8 errors in stage/string paths, e.g. `simd-json-0.13.11/src/lib.rs:755-758`, `stringparse.rs:77-88` | yes; unescaped control mask at `simd-json-0.13.11/src/lib.rs:287-288` | `dom_value` borrowed | per-iteration `Vec<u8>` clone, mutable in-situ; output borrows mutated buffer | Eligible after metadata records mutation/clone/prevalidation |
| Rust simd-json owned | `simd_json::to_owned_value(&mut bytes)` at `json_parity.rs:142-150` | `strict_bytes` if probes pass | same as borrowed | yes | `dom_value` owned | per-iteration mutable cloned buffer; output owned | Eligible after metadata records mutation/clone/prevalidation |
| serde_json Value/direct/real typed | `serde_json::from_slice::<Value>` at `json_parity.rs:163-179`; direct at `243-259`; real typed at `real_typed_struct.rs:132-144` | `strict_bytes` | Rust/serde byte validation in parse scope; profile shows `core::str::converts::from_utf8` hot at `skinny/profile/serde_json/PROFILE-REPORT.md:35`, `61`, `107`, `131`, `155` | yes | DOM floor or typed serde floor/control | borrowed immutable bytes, owned `Value` or target-dependent borrowed/Cow typed output | Floor/control, not SOTA target |
| simdjson C++ DOM | native profile driver `dom::parser::parse(json)` at `skinny/profile/simdjson-expanded/PROFILE-REPORT.md:7`; symbol evidence at lines 78-96 | `strict_bytes` for DOM profile rows | scan-boundary; UTF-8 checker cost documented at lines 116-134 | yes; string parse/surrogate decode documented at lines 136-164 | native `dom_value` / tape DOM | native padded input, owned DOM/tape | Native reference only; stale/profile-only until rerun on exact 17 corpora |
| simdjson C++ On-Demand | not a current skinny row | `partial_ondemand` unless full traversal | scan/cursor dependent | depends on traversed workload | `ondemand_cursor` | cursor borrows parser/input | Advisory only until full-walk workload exists |
| yyjson DOM | `yyjson_read`/`yyjson_read_opts` driver at `skinny/profile/yyjson/PROFILE-REPORT.md:8`; top symbol at lines 35-40 | `strict_bytes` for default flags | parse-boundary | yes under default flags | native `dom_value` | owns `yyjson_doc`; string copy/arena behavior visible via `_platform_memmove` at lines 37-40 | Native reference only; profile-only/stale and incomplete 17-corpus coverage |
| asmjson | AVX-512 `parse_to_dom_zmm` / `parse_with_zmm` unavailable on arm64; SWAR profile at `skinny/profile/native-sidecars/asmjson/NOTE.md:7-25` | `permissive` / flaw probe | none for current SWAR note | no strict proof; BENCH says permissive SWAR accepts invalid classes at `restart/skinny/BENCH.md:643-647` | `sax_sink` or DOM depending path; current rows are synthetic sidecar benches | x86 AVX-512 or arm64 SWAR synthetic, not 17-corpus skinny | Not eligible for strict Apple Silicon S anchors |

## Sonic-rs utf8_lossy confirmation

- Active skinny bench dependency: `skinny/crates/bbnf-bench/Cargo.toml:21` has `sonic-rs = { version = "=0.5.8", default-features = false, features = ["sort_keys", "utf8_lossy"] }`.
- `BENCH.md` conflicts with itself: the §2.1 snippet omits `utf8_lossy` at `restart/skinny/BENCH.md:172-177`, but the later full bench sketch includes it at `restart/skinny/BENCH.md:1126-1130`.
- Upstream feature exists at `sonic-rs-0.5.8/Cargo.toml:44-51`.
- With the feature enabled, sonic-rs applies `.utf8_lossy()` automatically in `from_trait` at `sonic-rs-0.5.8/src/serde/de.rs:1280-1283`.
- The lossy behavior replaces invalid UTF-8 before `Value` parse at `sonic-rs-0.5.8/src/serde/de.rs:381-385` and tests accept invalid raw UTF-8 / lone surrogates as U+FFFD at `sonic-rs-0.5.8/src/serde/mod.rs:723-735` and `747-775`.
- Impact: `sonic_rs_anchor`, `sonic_rs_checked`, `sonic_rs_direct_to_struct`, and `sonic_rs_real_typed_struct` must be marked `strictness=lossy_utf8`, `s_anchor_eligible=false`, with flaw probes for invalid UTF-8 and invalid surrogate escapes. They cannot ratify strict BBNF anchors until rebuilt without the feature and rerun.

## Schema v3 gaps in implementation

1. `skinny/crates/bbnf-bench/src/metadata.rs:7` still sets `SCHEMA_VERSION = "2"`; `BENCH.md` requires v3 at `restart/skinny/BENCH.md:676-680` and sketches `"3"` at `BENCH.md:696-730`.
2. `RowMetadata` at `metadata.rs:20-54` lacks `strictness`, `parse_utf8`/`validation_boundary`, `escape_complete`, `flaw_probe`, `output_plane`, `ownership_plane`, `feature_mask`, `api_symbol`, `corpus_hash` as distinct from v2 input SHA, `hardware`, `build_flags`, `sidecar_freshness`, `primitive_status`, `input_mutated`, `clone_charged`, `prevalidation_charged`, and `s_anchor_eligible`.
3. `BenchFacts::competitor` assigns all competitors `parse_mode="from_slice"` and `source_ownership="owned"` at `metadata.rs:160-188`, which is wrong for sonic-rs/serde immutable byte rows and incomplete for simd-json in-situ mutation.
4. `json_parity.rs` writes only crate/version/materialisation for competitors through `write_competitor_row` at `json_parity.rs:457-481`; row-specific API symbols and feature masks are not captured.
5. `report.rs` hard-codes retained strictness/plane/flaw text at `report.rs:141-148` and workload strictness/plane/flaw text at `report.rs:164-180`; it does not render row metadata.
6. `gate.rs` validates only v2 metadata at `gate.rs:97-116`, then chooses the fastest anchor from raw timings at `gate.rs:192-201` without checking strictness, plane, ownership, freshness, or `s_anchor_eligible`.
7. `gate.rs:217-219` appends a note claiming `sonic-rs/simd-json/serde_json rows are strict / scan-boundary / yes`; this is false for the current sonic-rs build.
8. `BENCH.md:684-685` says the four original columns are emitted by a `Sidecar` trait in `bbnf-bench/src/lib.rs`, but `skinny/crates/bbnf-bench/src/lib.rs:1-10` exposes only modules; no such trait exists.
9. `RESULTS.md:224` repeats the strict sidecar claim and must be regenerated after metadata/gate changes.
10. Existing criterion metadata is schema v2 and missing required fields, e.g. `skinny/target/criterion/json_twitter/sonic_rs_anchor/metadata.toml:1-28`; all current rows should be treated as schema-v3 invalid, not merely advisory.

## Required schema v3 edits

No repository edits were made in this dispatch. Required edits when implementation resumes:

1. Remove `utf8_lossy` from the strict sonic-rs dependency in `skinny/crates/bbnf-bench/Cargo.toml:21`, or split lossy rows into separate advisory bench names. Align `restart/skinny/BENCH.md:172-185` and `BENCH.md:1126-1130` to the same feature policy.
2. Change `SCHEMA_VERSION` to `"3"` in `skinny/crates/bbnf-bench/src/metadata.rs:7` and extend `RowMetadata`/`BenchFacts` at `metadata.rs:20-86` with the v3 fields listed above.
3. Add row constructors or a `SidecarRow`/`PlaneFacts` table in `metadata.rs` or a new module. Populate exact values for each bench in `json_parity.rs:43-350`: retained Track 1/2, sonic Value, sonic checked, simd-json borrowed/owned, serde_json Value, digest rows, and real typed rows.
4. Record exact `api_symbol` values, not generic materialisation labels. Examples: `sonic_rs::from_slice::<sonic_rs::Value>`, `simd_json::to_borrowed_value`, `simd_json::to_owned_value`, `serde_json::from_slice::<serde_json::Value>`, `runtime::generated_json::parse`, `runtime::generated_json::parse_direct`, `bbnf_bench::real_typed_struct::sonic_typed`.
5. Split ownership fields: `input_ownership`, `output_ownership`, `input_mutated`, `clone_charged`, and `prevalidation_charged`. Do not keep the v2 `source_ownership="owned"` default for all competitors.
6. Add `feature_mask` from crate feature policy. For current sonic rows it must include `utf8_lossy`; strict rows must omit it. For simd-json include `serde_impl`; for serde_json include workspace features such as `preserve_order` where relevant.
7. Add sidecar freshness values: `current_same_run` for in-tree criterion rows after rerun, `stale_profile_only` for current simdjson C++/yyjson profile reports, `advisory` for asmjson SWAR synthetic rows, and `published_cross_arch` for asmjson Zen 4 AVX-512 anchors.
8. Update `RowMetadata::required_fields_present` at `metadata.rs:276-310` and `gate::validate_schema` at `gate.rs:97-116` so missing/empty v3 fields produce `Outcome::JSchemaFail`.
9. Update anchor selection in `gate.rs:138-201` and `Estimates::fastest_anchor` at `src/bin/gate.rs:406-416` to filter by same `plane`, compatible strictness/ownership, `sidecar_freshness`, and `s_anchor_eligible=true`. Strictness-disjoint rows should render as notes, not feed `S`.
10. Replace hard-coded Markdown in `report.rs:141-180` with metadata-driven rendering. `RESULTS.md` must show the comparator row’s own plane fields, not a single retained-row prose string.
11. Fix `BENCH.md` contradictions: `schema_version` example at `BENCH.md:602`, BBNF baseline value at `BENCH.md:663`, sonic baseline at `BENCH.md:664`, `Sidecar` trait claim at `BENCH.md:684-685`, duplicate/ambiguous native comparator wording at `BENCH.md:262-293`, and lossy sonic sketch at `BENCH.md:1128`.
12. Add conformance/flaw-probe emission for at least invalid UTF-8, lone surrogate, unescaped control in string, invalid escape, invalid number, and trailing junk per row. Current BBNF byte entry rejects invalid UTF-8 at `skinny/crates/runtime/src/grammars/json/parser.rs:55-66`; retained timed rows still use `&str` at `json_parity.rs:16`.
13. Rerun the bench after schema update and regenerate `skinny/RESULTS.md`; do not classify existing v2 criterion rows as strict v3 rows.

## Sidecar availability

Current `/tmp` state matches R5: `/tmp/asmjson-research` is present; `/tmp/sonic-research`, `/tmp/simdjson-research`, `/tmp/yyjson-research`, and `/tmp/serde_json-bench` are absent. Therefore native simdjson C++ and yyjson rows remain stale/profile-only until their source trees and drivers are restored and rerun. The asmjson row is available only as a synthetic/permissive advisory flaw probe, not a strict 17-corpus S anchor.

## Bottom line

Current strict retained anchors should be recomputed without sonic-rs unless sonic-rs is rebuilt without `utf8_lossy`. Current direct-to-struct sonic targets are likewise ineligible. The implementation must land schema v3 in metadata, report rendering, and gate filtering before any SK-V6 SOTA-beat or strict same-plane claim is defensible.
