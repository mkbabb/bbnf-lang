# SK-V14 W0C: Sidecar Freshness
Date: 2026-05-24.
Scope: W0 sidecar freshness, comparator strictness, and stale-anchor rejection.
Output: this file.

## §1 — Findings (concrete file:line cited)

1. **SK-V14 makes sidecar evidence planning-only unless strictness, freshness, and plane rules are executable.** Section 0.2 defines same-run strict anchors as the only admission class, while stale sidecars and pre-W1 `sonic_rs_anchor` fan-out evidence are sidecar planning signals only (`restart/skinny/tranches/sk-v14/SPEC.md:71-75`). The same section requires `xtask gate-json` to reject strict admission unless comparator plane matches output plane, `comparator_strictness=strict`, the comparator id is the admitted R1 anchor, `per_iter_equality` passes every iter, and validation occurs inside the measured row (`restart/skinny/tranches/sk-v14/SPEC.md:77-82`). It explicitly classifies stale sidecars, sidecar-only evidence, historical deltas, and plane mismatch as guard telemetry only (`restart/skinny/tranches/sk-v14/SPEC.md:84-86`).

2. **W0's sidecar exit gate is already sharper than the current report shape.** Section 3 requires W0 to add sidecar freshness/source validation and reject any same-run sidecar claim until a structured sidecar manifest parser exists (`restart/skinny/tranches/sk-v14/SPEC.md:343-348`). Its exit gate requires absent sidecar values to use `sidecar_freshness=absent:<reason>`, populated sidecars to be historical non-manifest planning signals, and `xtask gate-json` to reject malformed sidecar evidence plus `sidecar-same-run` without a structured manifest (`restart/skinny/tranches/sk-v14/SPEC.md:354-360`).

3. **The current comparator evidence fields are usable for freshness checks but do not yet carry the SK-V14 W0 admission schema.** `SkV8ComparatorEvidence` has `comparator_id`, `comparator_plane`, `comparator_strictness`, `comparator_freshness`, `sidecar_freshness`, `value_mbps`, and `source_artifact` (`skinny/crates/bbnf-bench/src/report.rs:34-44`). `TelemetryRow` still has no row-level `per_iter_equality`, `audit_overlay_verdict`, or `track2_entry_point` fields (`skinny/crates/bbnf-bench/src/report.rs:77-97`), even though Section 0.4 requires those fields and same-wave gate consumption (`restart/skinny/tranches/sk-v14/SPEC.md:126-159`).

4. **The rendered report exposes comparator evidence as an SK-V9 manifest string, so W0 must not treat schema presence as SK-V14 compliance.** `render_markdown` keeps the 26-column table and appends `## SK-V9 W0 Telemetry Manifest` with a single `Comparator evidence` cell (`skinny/crates/bbnf-bench/src/report.rs:3428-3503`). Comparator evidence is rendered as `id[plane=...,strictness=...,freshness=...,sidecar=...,mbps=...,source=...]` (`skinny/crates/bbnf-bench/src/report.rs:6407-6424`). This can carry freshness metadata, but it is not yet a structured SK-V14 gate-json payload for `per_iter_equality`, audit overlay, or admitted-anchor id.

5. **Current sidecar values are hard-coded historical SK-V7 planning signals, not same-run manifests.** `sidecar_comparators` hard-codes simdjson, yyjson, and RapidJSON MiB/s values by corpus and converts them to Mbps (`skinny/crates/bbnf-bench/src/bin/gate.rs:3676-3717`). `w0_comparator_evidence` marks populated sidecars as `historical:sk-v7-sidecar-profile` with source `sidecar-profile:sk-v7-cpp:{corpus}:{id}`, and missing sidecars as `absent:not-collected-for-{workload}` with an `absence:w0:...` source (`skinny/crates/bbnf-bench/src/bin/gate.rs:2779-2800`).

6. **The current validator already rejects malformed sidecar evidence, with one ambiguity to make explicit in SK-V14.** `validate_comparator_evidence` rejects duplicate ids, empty fields, invalid Mbps, unknown comparator ids, populated sidecars marked absent, absent sidecars without `absent:<reason>`, and missing sidecar slots (`skinny/crates/bbnf-bench/src/report.rs:4553-4638`). `validate_sidecar_comparator` requires sidecar plane `DOM`, strictness `strict`, matching comparator/sidecar freshness, expected historical/absence source shapes, and rejects `sidecar-same-run` without a structured manifest (`skinny/crates/bbnf-bench/src/report.rs:4676-4723`). The ambiguity: sidecar entries currently retain `comparator_strictness=strict`, so SK-V14 needs an explicit "strict parser but non-admissible planning signal" rule keyed by freshness/admission class.

7. **The stale parse-only sonic anchor is still live in the benchmark, report evidence, and gate plumbing.** The parse-only bench `sonic_rs_anchor` measures `sonic_rs::from_slice::<sonic_rs::Value>` and writes it as `eager_typed` (`skinny/crates/bbnf-bench/benches/json_parity.rs:87-102`). The gate reads that estimate as `estimates.sonic` (`skinny/crates/bbnf-bench/src/bin/gate.rs:430-445`), W0 comparator evidence maps parse_only `sonic_rs_strict` to source `criterion:json_{corpus}/sonic_rs_anchor/new/estimates.json` (`skinny/crates/bbnf-bench/src/bin/gate.rs:2734-2757`), and native-source validation still expects `sonic_rs_anchor` for parse_only (`skinny/crates/bbnf-bench/src/report.rs:4737-4743`). Section 0.2 names that exact pre-W1 single-lane anchor as the structural cause of the JSON misadmits and says W1 deletes it (`restart/skinny/tranches/sk-v14/SPEC.md:88-90`).

8. **Direct and typed sonic anchors are separated, but still need SK-V14 admitted-id binding before W1 can use them.** The direct bench measures `sonic_rs_direct_to_struct` via `direct_struct::sonic_digest` (`skinny/crates/bbnf-bench/benches/json_parity.rs:225-239`), whose implementation is `sonic_rs::from_slice(bytes)` into `JsonDirectDigest` (`skinny/crates/bbnf-bench/src/direct_struct.rs:423-429`). The typed bench measures `sonic_rs_real_typed_struct` via `real_typed_struct::sonic_typed` (`skinny/crates/bbnf-bench/benches/json_parity.rs:310-329`), and `sonic_typed` dispatches per corpus to typed `sonic_rs::from_slice::<Target>` bindings (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:690-730`). These are better separated than parse_only, but W1 still must promote them from parity/support evidence to SK-V14 admitted R1 anchor ids.

9. **`xtask gate-json` is a passthrough plus pre-check wrapper, not the owner of SK-V14-specific comparator logic today.** `skinny/xtask/src/main.rs` validates the optional `--check-results` snapshot, validates allowed passthrough args, and shells out to `cargo run -p bbnf-bench --bin gate -- ...` (`skinny/xtask/src/main.rs:242-257`). Its allowlist currently names SK-V12/SK-V13 report flags but no SK-V14 W0 manifest flag (`skinny/xtask/src/main.rs:265-302`). There is no `skinny/xtask/src/gate.rs` file in this checkout; the active comparator and sidecar validation lives in `bbnf-bench`.

10. **CSS L4 has a separate source-sidecar surface that must be quarantined from W0 strict admission.** Current CSS code writes a lightningcss equality artifact whose comparator string is `lightningcss-1.0.0-alpha.71:same-plane-source-sidecar` (`skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:1079-1084`). Fixture sidecar facts are built from static byte spans and fail closed on span mismatch/UTF-8 boundary errors (`skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:2691-2723`), with fixture-drift tests covering the fail-closed path (`skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:3463-3468`). SK-V14 R1 requires CSS strict admission to use lightningcss full-parse plus cssparser full-parse, not source-sidecar evidence alone (`restart/skinny/tranches/sk-v14/SPEC.md:73-75`).

## §2 — Recommendations (named falsifiability gates)

1. **G-W0C-SIDECAR-MANIFEST-ABSENCE.** `xtask gate-json` rejects any `sidecar-same-run` sidecar freshness unless a structured manifest parser has accepted a same-run manifest with corpus, workload, comparator id, command line, binary/version, input hash, output-plane, strictness, run id, host, and checksum. Until that parser exists, populated C++ sidecars must be `historical:*`, missing sidecars must be `absent:<reason>`, and both must carry the expected source shape.

2. **G-W0C-PLANNING-NONANCHOR.** Any comparator evidence with `sidecar_freshness` starting `historical:` or `absent:` is planning-only. Gate-json rejects a row if such an entry is used as the SOTA anchor, strict-admission comparator, `Delta vs SOTA` authority, or row-close justification.

3. **G-W0C-R1-ADMITTED-ID-WHITELIST.** Strict admission is allowed only for SK-V14 R1 admitted ids: parse_only uses a Skipper-class structural-skip anchor, direct uses a same-corpus strict struct-deser anchor, typed uses the per-corpus typed struct-deser anchor, and CSS uses lightningcss full-parse plus cssparser full-parse. Any `sonic_rs_anchor` source path or umbrella `sonic_rs_strict` id without a plane-specific admitted id is rejected for strict admission.

4. **G-W0C-SONIC-ANCHOR-QUARANTINE.** Before W1, any evidence sourced from `criterion:json_*/sonic_rs_anchor/new/estimates.json` is tagged `pre-W1-stale:planning-only` or rejected if the row claims `A/GO`, strict row admission, or parse_only SOTA victory. This prevents `sonic_rs::from_slice::<Value>` from standing in for the required Skipper-class comparator.

5. **G-W0C-OUTPUT-PLANE-LOCK.** For every strict admission row, gate-json requires `comparator_plane == Output plane` and rejects DOM sidecars on direct/typed planes, source-sidecars on fact-stream planes, and any row whose comparator plane is inherited from a different workload class.

6. **G-W0C-PER-ITER-EQUALITY-CONSUMED.** Gate-json rejects missing `per_iter_equality`, startup-only checksums, or `pass_all_iters=false`. For strict admission, the per-iter equality payload must name the row id, comparator id, iter count, sample count, measured-validation path, and whether equality ran inside the timed region.

7. **G-W0C-CSS-SOURCE-SIDECAR-QUARANTINE.** Any CSS comparator string containing `source-sidecar` is planning/support evidence only until W8 lands production-corpus full-parse lightningcss + cssparser evidence with same-plane equality and a gate-consumed manifest.

8. **G-W0C-XTASK-SKV14-PAYLOAD.** `xtask gate-json` should accept and require a SK-V14 W0 manifest/report payload before W1, rather than relying only on the SK-V9 comparator-evidence string. The payload must expose `comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, `track2_entry_point`, sidecar freshness, and admitted-anchor id as structured fields.

## §3 — Risks (REDRESS entries to pre-block)

1. **REDRESS-W0C-1: Historical C++ sidecar as SOTA anchor.** Pre-block any row that uses hard-coded SK-V7 simdjson/yyjson/RapidJSON/asmjson sidecar Mbps as an admission, margin, or strict SOTA authority without same-run structured manifest evidence.

2. **REDRESS-W0C-2: Parse-only `sonic_rs_anchor` reuse.** Pre-block reuse of `sonic_rs::from_slice::<Value>` / `sonic_rs_anchor` as the parse_only strict comparator. It may remain only as historical planning telemetry until replaced by the Skipper-class R1 anchor.

3. **REDRESS-W0C-3: DOM sidecar plane leakage.** Pre-block direct/typed row closure that compares digest or typed-direct output against DOM sidecar evidence, even when the sidecar entry says `strict`.

4. **REDRESS-W0C-4: Same-run sidecar without manifest parser.** Pre-block any `sidecar-same-run` claim unless the same commit adds and gates a structured sidecar manifest parser.

5. **REDRESS-W0C-5: CSS source-sidecar admission.** Pre-block CSS L4 row admission based on fixture source-sidecars, static spans, or lightningcss source-sidecar equality artifacts instead of full-parse lightningcss + cssparser evidence.

6. **REDRESS-W0C-6: Schema-only close.** Pre-block any W0/W1 close that adds telemetry producers but leaves `xtask gate-json` unable to reject missing `per_iter_equality`, missing audit overlay, missing `track2_entry_point`, stale sidecar strict claims, or unsupported admitted comparator ids.

7. **REDRESS-W0C-7: Umbrella comparator id ambiguity.** Pre-block row admission that reports only `sonic_rs_strict` without a plane-specific admitted id and source path, because the current umbrella id maps parse_only, direct, and typed to different benchmark semantics.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:66-90` — Section 0.2 comparator classes, sidecar planning-only status, strict-admission rejection requirements, and `sonic_rs_anchor` stale-anchor statement.
- `restart/skinny/tranches/sk-v14/SPEC.md:120-166` — Section 0.4 required telemetry fields and gate-json consumption requirements.
- `restart/skinny/tranches/sk-v14/SPEC.md:315-371` — Section 3 W0 owner paths, tasks, exit gate, same-wave consumer, and pre-blocked routes.
- `skinny/crates/bbnf-bench/src/report.rs:34-44` — current comparator evidence fields.
- `skinny/crates/bbnf-bench/src/report.rs:77-97` — current telemetry row surface.
- `skinny/crates/bbnf-bench/src/report.rs:3428-3503` and `skinny/crates/bbnf-bench/src/report.rs:6407-6424` — report rendering of telemetry manifest and comparator evidence.
- `skinny/crates/bbnf-bench/src/report.rs:4553-4638`, `skinny/crates/bbnf-bench/src/report.rs:4676-4788`, and `skinny/crates/bbnf-bench/src/report.rs:7240-7476` — current comparator, sidecar, native-source, and negative-test validation.
- `skinny/crates/bbnf-bench/src/bin/gate.rs:430-445`, `skinny/crates/bbnf-bench/src/bin/gate.rs:2710-2823`, and `skinny/crates/bbnf-bench/src/bin/gate.rs:3645-3717` — gate estimate reads, comparator evidence construction, and hard-coded sidecar values.
- `skinny/crates/bbnf-bench/benches/json_parity.rs:87-102`, `skinny/crates/bbnf-bench/benches/json_parity.rs:225-239`, and `skinny/crates/bbnf-bench/benches/json_parity.rs:310-329` — parse_only, direct, and typed sonic benchmark surfaces.
- `skinny/crates/bbnf-bench/src/direct_struct.rs:423-429` and `skinny/crates/bbnf-bench/src/real_typed_struct.rs:690-730` — direct digest and typed per-corpus sonic deserialization bindings.
- `skinny/xtask/src/main.rs:242-302` — `gate-json` wrapper and passthrough allowlist.
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:1079-1084`, `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:2691-2723`, and `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:3463-3468` — CSS L4 source-sidecar evidence and fail-closed drift tests.
