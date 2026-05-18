# SK-V8 W1 C: Strict Comparator And Admission Gate Binding

Date: 2026-05-18.
Scope: strict-admission/comparator fail-closed coverage, W1 binding gaps, and tests for rejecting sidecar, permissive, or historical admission evidence.
Output: `restart/skinny/tranches/sk-v8/research/skv8-W1-c-comparator-admission.md`.

## §1 — Findings (concrete, file:line cited)

1. W1 is the right dispatch point and must remain research/plan/redress separated.
   The triumvirate contract says research outputs are read-only artifacts under
   `restart/skinny/tranches/sk-v{N}/research/` and must contain findings,
   recommendations, risks, and sources; implementation belongs only to redress
   commits (restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:11,
   restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:27,
   restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:190). V12 closes W0 and
   explicitly dispatches W1 under SPEC Section 4
   (restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:19,
   restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:82).

2. The admission contract is stricter than the current rendered result labels.
   SPEC Section 0.2 allows strict admission only for same-run native strict
   anchors whose output plane matches the row and whose validation occurs in the
   measured row; flaw probes are planning-only, and sidecars remain planning-only
   unless freshness, strictness, and plane rules are satisfied
   (restart/skinny/tranches/sk-v8/SPEC.md:63,
   restart/skinny/tranches/sk-v8/SPEC.md:69,
   restart/skinny/tranches/sk-v8/SPEC.md:73). SPEC Section 1 restates that no
   stale sidecar, permissive, lossy, historical, or view-boundary evidence may
   serve as strict admission (restart/skinny/tranches/sk-v8/SPEC.md:203,
   restart/skinny/tranches/sk-v8/SPEC.md:204). Current `RESULTS.md` still
   reports all 38 W0 manifest rows with `Validation=view-boundary` and
   `CostFacts=none:pre-W1:none:pre-W1:none:pre-W1`; for example the first W0
   manifest row shows view-boundary validation, pre-W1 CostFacts, native strict
   anchors, one permissive lossy probe, and historical/absent sidecars
   (skinny/RESULTS.md:46, skinny/RESULTS.md:48).

3. Helper-level strict admission is already mostly fail-closed for field
   semantics. `StrictAdmissionEvidence` carries outcome, row strictness,
   `parse_utf8`, `escape_complete`, row/comparator planes, comparator
   strictness/freshness, sidecar freshness, and measured-validation path
   (skinny/crates/bbnf-bench/src/gate.rs:58). `validate_strict_admission`
   rejects unsupported outcomes, non-GO outcomes, non-strict rows, non-strict
   comparators, non-measured UTF-8, incomplete escape validation, plane mismatch,
   non-measured validation paths, stale/historical/absent freshness, and anything
   other than `comparator_freshness=same-run-native` with `sidecar_freshness=n/a`
   (skinny/crates/bbnf-bench/src/gate.rs:135, skinny/crates/bbnf-bench/src/gate.rs:145,
   skinny/crates/bbnf-bench/src/gate.rs:151, skinny/crates/bbnf-bench/src/gate.rs:157,
   skinny/crates/bbnf-bench/src/gate.rs:160, skinny/crates/bbnf-bench/src/gate.rs:163,
   skinny/crates/bbnf-bench/src/gate.rs:172). The gap is that the helper does not
   carry or check `comparator_id`; an evidence object with native-looking
   freshness/plane/strictness is not tied to an admitted comparator identity
   (skinny/crates/bbnf-bench/src/gate.rs:58).

4. The report layer already fail-closes W0 comparator inventory and sources.
   W0 telemetry stores `comparator_id`, `comparator_plane`,
   `comparator_strictness`, `comparator_freshness`, `sidecar_freshness`, Mbps
   value, and source artifact (skinny/crates/bbnf-bench/src/report.rs:32). W0
   validation rejects missing required SK-V8 telemetry fields, wrong
   grammar/domain, non-baseline wave/run ids, missing sample cost, profile/hot
   leaf drift, unsupported same-wave consumer class, and parse rows that escape
   substrate-guard non-admission (skinny/crates/bbnf-bench/src/report.rs:275,
   skinny/crates/bbnf-bench/src/report.rs:322, skinny/crates/bbnf-bench/src/report.rs:330,
   skinny/crates/bbnf-bench/src/report.rs:345, skinny/crates/bbnf-bench/src/report.rs:349,
   skinny/crates/bbnf-bench/src/report.rs:356, skinny/crates/bbnf-bench/src/report.rs:362).
   Comparator validation rejects empty/duplicate evidence, invalid Mbps,
   unsupported ids, missing native comparators, and missing sidecar slots
   (skinny/crates/bbnf-bench/src/report.rs:1135, skinny/crates/bbnf-bench/src/report.rs:1171,
   skinny/crates/bbnf-bench/src/report.rs:1179, skinny/crates/bbnf-bench/src/report.rs:1213,
   skinny/crates/bbnf-bench/src/report.rs:1220). Native strict comparators are
   pinned by workload to expected bench/source and plane: parse-only uses DOM,
   direct-to-struct uses digest, and real-typed-struct uses typed direct
   (skinny/crates/bbnf-bench/src/report.rs:1324, skinny/crates/bbnf-bench/src/report.rs:1337,
   skinny/crates/bbnf-bench/src/report.rs:1343, skinny/crates/bbnf-bench/src/report.rs:1349,
   skinny/crates/bbnf-bench/src/report.rs:1355, skinny/crates/bbnf-bench/src/report.rs:1361,
   skinny/crates/bbnf-bench/src/report.rs:1367).

5. Sidecars and permissive probes are currently report-consumed but not
   admission-capable. The report classifies native strict ids as
   `sonic_rs_strict` and `serde_json`, native flaw probes as `sonic_rs_lossy`,
   and sidecars as simdjson/yyjson/asmjson/RapidJSON slots
   (skinny/crates/bbnf-bench/src/report.rs:920, skinny/crates/bbnf-bench/src/report.rs:928).
   Flaw probes are valid only for parse-only, must be DOM/permissive/same-run
   native, and must have a `sonic_rs_lossy` Criterion source
   (skinny/crates/bbnf-bench/src/report.rs:1230, skinny/crates/bbnf-bench/src/report.rs:1242,
   skinny/crates/bbnf-bench/src/report.rs:1253). Sidecars must be DOM/strict, have
   matching comparator and sidecar freshness, reject `sidecar-same-run` without a
   structured manifest, and use either `sidecar-profile:sk-v7-cpp:...` or
   `absence:w0:...` sources (skinny/crates/bbnf-bench/src/report.rs:1263,
   skinny/crates/bbnf-bench/src/report.rs:1269, skinny/crates/bbnf-bench/src/report.rs:1275,
   skinny/crates/bbnf-bench/src/report.rs:1281, skinny/crates/bbnf-bench/src/report.rs:1287,
   skinny/crates/bbnf-bench/src/report.rs:1293). `RESULTS.md` confirms the
   intended W0 evidence shape: native Rust comparators are same-run, C++ sidecars
   are historical or absent, and never strict anchors in W0 (skinny/RESULTS.md:141).

6. W0 CostFacts are deliberately fail-closed as sentinels, not evidence. W0
   manifest semantics reject any CostFacts value other than
   `none:pre-W1`/`none:pre-W1`/`["none:pre-W1"]`, any non-`none` redress entry,
   or unverified Track 2 independence (skinny/crates/bbnf-bench/src/report.rs:1007,
   skinny/crates/bbnf-bench/src/report.rs:1009). V12 routes replacement of those
   sentinels to W1 before any behavior wave can cite route quality
   (restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:67).
   SPEC Section 4 makes this explicit: W1 must bind CostFacts ids, chosen shape,
   rejected alternatives, evidence source, wave id, and REDRESS reference into
   the gate report, then make `gate-json --with-cost-facts` reject missing
   evidence (restart/skinny/tranches/sk-v8/SPEC.md:398,
   restart/skinny/tranches/sk-v8/SPEC.md:402).

7. The W1 gap is binding the selected admission comparator, not merely carrying a
   comparator inventory. Current report rendering emits a combined comparator
   evidence cell from the telemetry manifest (skinny/crates/bbnf-bench/src/report.rs:575,
   skinny/crates/bbnf-bench/src/report.rs:581, skinny/crates/bbnf-bench/src/report.rs:608,
   skinny/crates/bbnf-bench/src/report.rs:1389). Current `gate-json` validates
   W0 schema and W0 telemetry, writes or compares `RESULTS.md`, then exits by
   outcome severity (skinny/crates/bbnf-bench/src/bin/gate.rs:319,
   skinny/crates/bbnf-bench/src/bin/gate.rs:329, skinny/crates/bbnf-bench/src/bin/gate.rs:341).
   W1 needs a strict-admission consumer that selects the comparator id used for
   admission and constructs `StrictAdmissionEvidence` from the rendered row plus
   that comparator. Without the selected-id binding, a row can carry safe native
   evidence and unsafe sidecar/permissive evidence side by side; the report
   inventory alone does not say which one admitted behavior.

8. Existing tests prove important negative cases, but W1 needs selected-id and
   CostFacts integration tests. Helper tests already reject unsupported ids as
   outcomes, non-GO outcomes including `N-direct` and `S`, deferred/view-boundary
   strict claims, plane mismatch, historical sidecar evidence, and
   `sidecar-same-run` without a structured manifest
   (skinny/crates/bbnf-bench/src/gate.rs:451, skinny/crates/bbnf-bench/src/gate.rs:459,
   skinny/crates/bbnf-bench/src/gate.rs:485, skinny/crates/bbnf-bench/src/gate.rs:501,
   skinny/crates/bbnf-bench/src/gate.rs:512). Report tests reject malformed
   sidecar evidence, unsupported strict/view-boundary claims, deferred-validation
   drift, native source mismatch, native semantic mismatch, sidecar source and
   freshness mismatch, unknown sidecar strict shapes, baseline drift, bad run id,
   strict hard-failure claims, and non-W0 CostFacts sentinels
   (skinny/crates/bbnf-bench/src/report.rs:1711, skinny/crates/bbnf-bench/src/report.rs:1760,
   skinny/crates/bbnf-bench/src/report.rs:1786, skinny/crates/bbnf-bench/src/report.rs:1812,
   skinny/crates/bbnf-bench/src/report.rs:1838, skinny/crates/bbnf-bench/src/report.rs:1876,
   skinny/crates/bbnf-bench/src/report.rs:1908, skinny/crates/bbnf-bench/src/report.rs:1959,
   skinny/crates/bbnf-bench/src/report.rs:2031, skinny/crates/bbnf-bench/src/report.rs:2041,
   skinny/crates/bbnf-bench/src/report.rs:2058). V12 evidence says `cargo test -p
   bbnf-bench strict`, `cargo test -p bbnf-bench sidecar_same_run`, full
   `cargo test -p bbnf-bench`, and `cargo xtask gate-json --advisory
   --check-results` passed (restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:49,
   restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:53).

## §2 — Recommendations (named falsifiability gates)

1. Gate C1: `strict_admission_requires_selected_native_comparator_id`.
   Extend the strict-admission evidence consumed by W1 to include
   `comparator_id`, and accept only same-run native strict anchors admitted by
   id for the row plane. In current terms that means `sonic_rs_strict` and
   `serde_json`, with workload-specific planes and Criterion sources already
   described by the report native comparator validator
   (skinny/crates/bbnf-bench/src/report.rs:1324). A selected `sonic_rs_lossy`,
   simdjson, yyjson, asmjson, RapidJSON, unknown id, or missing id must fail even
   if its other fields are forged to strict/native-looking values.

2. Gate C2: `strict_admission_uses_report_emitted_fields`.
   Build strict-admission evidence from `TelemetryRow` plus the selected
   `SkV8ComparatorEvidence`; do not accept a parallel side input. The gate should
   bind row strictness, `parse_utf8`, `escape_complete`, row output plane,
   comparator plane, comparator strictness, comparator freshness, sidecar
   freshness, and measured-validation path using the same fields rendered in the
   SK-V8 telemetry manifest (skinny/crates/bbnf-bench/src/report.rs:44,
   skinny/crates/bbnf-bench/src/report.rs:70,
   skinny/crates/bbnf-bench/src/report.rs:575).

3. Gate C3: `costfacts_and_comparator_admit_together`.
   `gate-json --with-cost-facts` must reject any row that still has
   `none:pre-W1` CostFacts, missing chosen shape, missing rejected alternatives,
   missing evidence source, missing wave id, or missing REDRESS reference, and
   strict-admission rows must also pass the selected comparator gate. This binds
   SPEC Section 4 tasks 1-3 into one same-wave consumer
   (restart/skinny/tranches/sk-v8/SPEC.md:398,
   restart/skinny/tranches/sk-v8/SPEC.md:418).

4. Gate C4: `no_planning_signal_admits_behavior`.
   Add negative tests where the selected admission comparator is:
   `sonic_rs_lossy` with same-run freshness, historical `simdjson_dom`,
   `sidecar-same-run` without a structured manifest, absent sidecar evidence,
   unknown sidecar id, or DOM sidecar evidence for a digest/typed row. Each must
   fail despite any native strict comparator also being present in the row
   inventory. This closes the ambiguity between "row has a valid native anchor"
   and "row was admitted by this invalid comparator".

5. Gate C5: `view_boundary_go_is_not_strict_admission`.
   Add W1 tests that mutate an otherwise GO row to `strict` while leaving
   `parse_utf8=view-boundary` or `measured_validation_path=view-boundary`; both
   must fail. Add the symmetric test where `measured_validation_path=measured-row`
   but `parse_utf8` remains view-boundary, because the helper already has both
   fields and should keep them independent (skinny/crates/bbnf-bench/src/gate.rs:151,
   skinny/crates/bbnf-bench/src/gate.rs:160).

6. Gate C6: `full_table_maintain_no_behavior_drift`.
   W1 should keep parser/generated behavior unchanged and maintain the full table
   within +/-1.0% of `SK-V8-open`, as SPEC Section 4 requires
   (restart/skinny/tranches/sk-v8/SPEC.md:404,
   restart/skinny/tranches/sk-v8/SPEC.md:416). Tests should verify generated JSON
   output diff remains empty and that a stale or mixed Criterion capture is still
   rejected by the existing run/profile/baseline checks
   (skinny/crates/bbnf-bench/src/report.rs:494,
   skinny/crates/bbnf-bench/src/bin/gate.rs:673).

## §3 — Risks (REDRESS entries to pre-block)

1. Producer-only telemetry risk. W1 can fail by adding comparator/CostFacts
   fields to the report without making `gate-json --with-cost-facts` and the
   strict-admission gate consume them. SPEC pre-blocks producer-only
   CostFacts/telemetry and requires same-wave consumption
   (restart/skinny/tranches/sk-v8/SPEC.md:418,
   restart/skinny/tranches/sk-v8/SPEC.md:421).

2. Comparator inventory confusion risk. Rows can carry native anchors, flaw
   probes, and sidecar planning signals in one cell. If W1 does not bind the
   selected comparator id, future behavior could be admitted by proximity to a
   valid native anchor while actually citing sidecar, permissive, or historical
   evidence. This directly reopens the global block on sidecar/permissive/lossy
   strict admission (restart/skinny/tranches/sk-v8/SPEC.md:768).

3. CostFacts-as-performance risk. V12 says W1 owns replacing pre-W1 sentinels
   before route quality claims, but CostFacts are evidence substrate, not
   behavior permission (restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:67).
   Keep SPEC Section 4's pre-block on CostFacts-as-performance claims and global
   route-fact policy ignoring rejected alternatives (restart/skinny/tranches/sk-v8/SPEC.md:421).

4. Historical sidecar freshness risk. Current W0 sidecar values are historical
   or absent by construction (skinny/RESULTS.md:48, skinny/RESULTS.md:141).
   W1 must not relax the W0 rule that `sidecar-same-run` rejects until a later
   accepted wave adds a structured manifest parser and gate
   (restart/skinny/tranches/sk-v8/SPEC.md:77,
   skinny/crates/bbnf-bench/src/report.rs:1287).

5. Behavior drift risk. W1 is authorized for CostFacts/report/gate/test binding,
   not parser/generated behavior. SPEC Section 4 blocks behavior changes and
   generated output drift (restart/skinny/tranches/sk-v8/SPEC.md:404,
   restart/skinny/tranches/sk-v8/SPEC.md:421). Any parser, scanner, SIMD,
   codegen output, product-plane, or direct behavior edit should be rejected or
   routed to a later challenged behavior wave.

## §4 — Sources (every external citation)

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`

No external web sources were used.
