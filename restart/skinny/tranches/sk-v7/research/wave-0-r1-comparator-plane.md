# SK-V7 W0 R1 comparator-plane research

Date: 2026-05-16.

Scope: read-only review of the SK-V7 Wave 0 comparator-plane state. No source,
benchmark, or results files were changed.

## Finding

Wave 0 is correctly scoped as a comparator repair, not a parser change. The SK-V7
packet says the tranche closes against a strict-rebuilt sonic-rs baseline after
Wave 0, and identifies the current comparator as `utf8_lossy` on the sonic-rs
row (`restart/skinny/tranches/sk-v7/SPEC.md:19`,
`restart/skinny/tranches/sk-v7/SPEC.md:27`). The Wave 0 owner path and task are
also explicit: remove `"utf8_lossy"` from `skinny/crates/bbnf-bench/Cargo.toml`
line 21, rerun the JSON bench, then rerun the gate report
(`restart/skinny/tranches/sk-v7/SPEC.md:100`,
`restart/skinny/tranches/sk-v7/SPEC.md:103`,
`restart/skinny/tranches/sk-v7/SPEC.md:109`,
`restart/skinny/tranches/sk-v7/SPEC.md:111`,
`restart/skinny/tranches/sk-v7/SPEC.md:112`).

The current dependency is still lossy. `bbnf-bench` pins sonic-rs `=0.5.8`,
disables default features, and enables both `sort_keys` and `utf8_lossy`
(`skinny/crates/bbnf-bench/Cargo.toml:21`). The library surface itself is only
module exports and carries no lossy-specific API dependency
(`skinny/crates/bbnf-bench/src/lib.rs:1`,
`skinny/crates/bbnf-bench/src/lib.rs:9`). The sonic call sites use
`sonic_rs::from_slice` for parse RSS, direct digest, and real typed output
(`skinny/crates/bbnf-bench/src/bin/gate.rs:641`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:642`,
`skinny/crates/bbnf-bench/src/direct_struct.rs:416`,
`skinny/crates/bbnf-bench/src/direct_struct.rs:417`,
`skinny/crates/bbnf-bench/src/real_typed_struct.rs:146`,
`skinny/crates/bbnf-bench/src/real_typed_struct.rs:151`). That makes the
feature removal the load-bearing comparator change.

The synthesis matches this reading. It says the comparator is currently a flaw
probe because `utf8_lossy` is enabled, and says removing that one feature should
drop sonic-rs by 3-8%, likely flipping `instruments` and `unicode_basic`
borderline parse rows while leaving strong direct misses unresolved
(`restart/skinny/tranches/sk-v7/SYNTHESIS.md:66`,
`restart/skinny/tranches/sk-v7/SYNTHESIS.md:67`,
`restart/skinny/tranches/sk-v7/SYNTHESIS.md:71`,
`restart/skinny/tranches/sk-v7/SYNTHESIS.md:73`,
`restart/skinny/tranches/sk-v7/SYNTHESIS.md:74`,
`restart/skinny/tranches/sk-v7/SYNTHESIS.md:75`,
`restart/skinny/tranches/sk-v7/SYNTHESIS.md:76`,
`restart/skinny/tranches/sk-v7/SYNTHESIS.md:77`).

## Expected W0 gate

The expected W0 gate is a strict-vs-strict rebuild with no Track 1 or Track 2
behavior delta. The spec requires every comparator row to report against
sonic-rs strict, simdjson C++ DOM and On Demand, yyjson default, asmjson SWAR as
flaw probe, RapidJSON as flaw probe, and serde_json strict
(`restart/skinny/tranches/sk-v7/SPEC.md:46`,
`restart/skinny/tranches/sk-v7/SPEC.md:48`,
`restart/skinny/tranches/sk-v7/SPEC.md:49`,
`restart/skinny/tranches/sk-v7/SPEC.md:50`,
`restart/skinny/tranches/sk-v7/SPEC.md:51`,
`restart/skinny/tranches/sk-v7/SPEC.md:52`,
`restart/skinny/tranches/sk-v7/SPEC.md:53`,
`restart/skinny/tranches/sk-v7/SPEC.md:54`). Its exit gate is concrete:
sonic-rs Mbps drops 3-8% on every row, `instruments` parse passes at >=100%
strict sonic, `unicode_basic` parse either passes or records a residual gap, and
Track 1/Track 2 do not regress because W0 changes only the comparator
(`restart/skinny/tranches/sk-v7/SPEC.md:116`,
`restart/skinny/tranches/sk-v7/SPEC.md:117`,
`restart/skinny/tranches/sk-v7/SPEC.md:118`,
`restart/skinny/tranches/sk-v7/SPEC.md:119`,
`restart/skinny/tranches/sk-v7/SPEC.md:120`).

Current parse rows show the two W0 borderlines exactly as advertised:
`instruments` parse is 18,163 Mbps Track 1 versus 19,737 Mbps sonic-rs, or
92.0%; `unicode_basic` parse is 12,193 Mbps Track 1 versus 13,304 Mbps sonic-rs,
or 91.7% (`skinny/RESULTS.md:15`, `skinny/RESULTS.md:19`). A 3-8% sonic-rs
drop would move these rows closer to PASS; `unicode_basic` needs the high end of
that band plus noise in its favor, so residual-gap documentation is a real W0
possibility.

The workload table confirms why Wave 0 should not be expected to fix direct
rows. `canada` direct is 83.6% of sonic, `mesh` direct is 91.8%, and
`instruments` direct is 93.5%; those misses are workload-path gaps, not
parse-comparator defects (`skinny/RESULTS.md:30`, `skinny/RESULTS.md:35`,
`skinny/RESULTS.md:39`). The SPEC routes those later: canada direct belongs to
W2, mesh and marine_ik typed rows to W3, and broad parse string work to W4/W5
(`restart/skinny/tranches/sk-v7/SPEC.md:30`,
`restart/skinny/tranches/sk-v7/SPEC.md:32`,
`restart/skinny/tranches/sk-v7/SPEC.md:33`,
`restart/skinny/tranches/sk-v7/SPEC.md:34`,
`restart/skinny/tranches/sk-v7/SPEC.md:35`,
`restart/skinny/tranches/sk-v7/SPEC.md:36`,
`restart/skinny/tranches/sk-v7/SPEC.md:37`,
`restart/skinny/tranches/sk-v7/SPEC.md:38`,
`restart/skinny/tranches/sk-v7/SPEC.md:39`,
`restart/skinny/tranches/sk-v7/SPEC.md:40`,
`restart/skinny/tranches/sk-v7/SPEC.md:41`).

## Current schema gap

The current `RESULTS.md` parse table has 17 columns: corpus, outcome, verdict,
strictness sidecar fields, Track 1/Track 2 Mbps, sonic-rs Mbps, two simd-json
columns, fastest-anchor fields, and ratios (`skinny/RESULTS.md:3`). The workload
table has 14 columns and only carries sonic-rs plus serde_json among competitors
(`skinny/RESULTS.md:25`). This does not satisfy the SK-V7 schema requirement,
which calls for a 24-column table with `sonic-rs strict`, `sonic-rs lossy`,
`simdjson DOM`, `simdjson OD`, `yyjson`, `asmjson SWAR`, `asmjson AVX-512`,
`RapidJSON`, `serde_json`, deltas, hot leaf, and signal
(`restart/skinny/tranches/sk-v7/SPEC.md:56`,
`restart/skinny/tranches/sk-v7/SPEC.md:58`,
`restart/skinny/tranches/sk-v7/SPEC.md:60`,
`restart/skinny/tranches/sk-v7/SPEC.md:61`,
`restart/skinny/tranches/sk-v7/SPEC.md:62`,
`restart/skinny/tranches/sk-v7/SPEC.md:63`,
`restart/skinny/tranches/sk-v7/SPEC.md:64`,
`restart/skinny/tranches/sk-v7/SPEC.md:65`,
`restart/skinny/tranches/sk-v7/SPEC.md:66`,
`restart/skinny/tranches/sk-v7/SPEC.md:69`,
`restart/skinny/tranches/sk-v7/SPEC.md:70`).

The report emitter explains the gap. `ReportRow` stores only one sonic value and
two simd-json values (`skinny/crates/bbnf-bench/src/report.rs:22`,
`skinny/crates/bbnf-bench/src/report.rs:23`,
`skinny/crates/bbnf-bench/src/report.rs:24`,
`skinny/crates/bbnf-bench/src/report.rs:25`,
`skinny/crates/bbnf-bench/src/report.rs:26`). `WorkloadReportRow` stores
sonic-rs and serde_json only (`skinny/crates/bbnf-bench/src/report.rs:45`,
`skinny/crates/bbnf-bench/src/report.rs:46`,
`skinny/crates/bbnf-bench/src/report.rs:47`,
`skinny/crates/bbnf-bench/src/report.rs:48`). The markdown renderer hard-codes
the same older headers (`skinny/crates/bbnf-bench/src/report.rs:141`,
`skinny/crates/bbnf-bench/src/report.rs:164`). The gate binary also reads only
`sonic_rs_anchor`, `simd_json_borrowed`, and `simd_json_owned` for parse
anchors, plus sonic/serde direct and typed workloads
(`skinny/crates/bbnf-bench/src/bin/gate.rs:35`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:38`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:39`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:40`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:43`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:44`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:47`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:48`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:419`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:423`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:424`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:425`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:428`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:429`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:432`,
`skinny/crates/bbnf-bench/src/bin/gate.rs:433`).

There is therefore a real W0 follow-up if the redress implementer wants the gate
to emit schema v3 automatically. The SPEC already anticipates this: manually add
missing schema v3 columns if the bench harness does not emit them, and mark a
Wave 0 follow-up if the harness remains incomplete
(`restart/skinny/tranches/sk-v7/SPEC.md:113`). The note at the bottom of current
`RESULTS.md` also admits the sidecar state: sonic-rs, simd-json, and serde_json
rows are strict-sidecar metadata, while asmjson and RapidJSON rows are absent and
must be rendered as permissive flaw probes when populated
(`skinny/RESULTS.md:224`).

## Implementation cautions

Keep the W0 code diff to `skinny/crates/bbnf-bench/Cargo.toml` unless choosing to
implement schema v3 emission as a separate, named follow-up. The instruction
precepts require scoped changes, evidence-backed gates, and no opportunistic
refactors (`docs/precepts/instructions/README.md:38`,
`docs/precepts/instructions/README.md:51`,
`docs/precepts/instructions/README.md:52`,
`docs/precepts/instructions/README.md:104`,
`docs/precepts/instructions/README.md:106`,
`docs/precepts/instructions/README.md:111`,
`docs/precepts/instructions/README.md:116`). The style precept points hard-gate
text toward plain evidence prose, with no decorative register
(`docs/precepts/instructions/STYLE.md:109`,
`docs/precepts/instructions/STYLE.md:110`,
`docs/precepts/instructions/STYLE.md:119`,
`docs/precepts/instructions/STYLE.md:120`).

Do not claim strict-vs-strict closure from the Cargo edit alone. Required
evidence is the feature tree showing sonic-rs without `utf8_lossy`, a fresh
`json_parity` bench, and a regenerated gate report
(`restart/skinny/tranches/sk-v7/SPEC.md:109`,
`restart/skinny/tranches/sk-v7/SPEC.md:110`,
`restart/skinny/tranches/sk-v7/SPEC.md:111`,
`restart/skinny/tranches/sk-v7/SPEC.md:112`). Also preserve the old lossy sonic
numbers in schema v3 or in the re-baseline report, because W0's falsifiability
depends on per-row sonic-rs Mbps dropping 3-8% versus current results
(`restart/skinny/tranches/sk-v7/SPEC.md:27`,
`restart/skinny/tranches/sk-v7/SPEC.md:117`).
