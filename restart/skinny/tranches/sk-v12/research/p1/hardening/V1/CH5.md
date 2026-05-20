# SK-V12 S-P1 Hardening V1 CH5 Hidden Coupling

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-20.
Lens: CH5 hidden coupling.
Output: this file only.

## Disposition

ACCEPT.

Critical findings: none.

The SK-V12 P1 packet keeps the CH5 boundary intact. It separates generated
Track 1, independent Track 2/oracle evidence, JSON product rows, generated
non-JSON baseline requirements, diagnostic PMU/profile evidence, and rejected
substrate/sidecar routes. The open caveat around missing exact SK-V12
inlined-frame percentages is an evidence-quality caveat for CH1/CH6, not a
hidden-coupling failure, because the packet does not use those missing
percentages for row admission or source-delta claims.

## Materials Read

- `restart/prompts/skinny/PASS-1-PROFILE.md`.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md`.
- `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md`.
- `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md`.
- `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md`.
- `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md`.
- `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md`.
- `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` through REDRESS 120.
- Relevant codegen/runtime/report/gate source:
  `skinny/crates/codegen/src/json_provider.rs`,
  `skinny/crates/codegen/src/lib.rs`,
  `skinny/crates/runtime/src/grammars/`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`,
  `skinny/crates/bbnf-bench/src/gate.rs`, and
  `skinny/crates/bbnf-bench/src/report.rs`.

## Findings

1. Track 1 and Track 2/oracle lanes remain separate. The CH5 contract requires
   Track 1 generated runtime and structurally independent Track 2 symbol paths
   (`restart/prompts/skinny/PASS-1-PROFILE.md:148`). P1-B defines `T1` as
   generated Track 1 and `T2` as independent Track 2/oracle, then keeps JSON
   digest rows and JSON typed guard rows separate from non-JSON baselines
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:131`,
   `:134`, `:136`). `skinny/RESULTS.md` also records Track 1 as
   `runtime::generated_json::parse` and Track 2 as an independent hand-coded
   parser over `runtime::tape` that never calls generated JSON parse
   (`skinny/RESULTS.md:143`, `:144`, `:145`). Source agrees: the JSON gate
   notes the same Track 1/Track 2 boundary
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:347`), and the RSS probe dispatch
   calls `runtime::generated_json::parse` only for `track1_generated` and
   `bbnf_bench::track2::json::parse` for `track2_handcoded`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:1986`,
   `:1992`).

2. Typed and direct output planes are not cross-admitted. P1-B explicitly says
   typed rows are output-plane specific, cannot admit a direct digest row, and
   direct digest evidence cannot admit a typed row
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:194`).
   It also keeps typed Track 2 as oracle/independence evidence for guard
   maintenance, not as a replacement for the SK-V12 non-JSON baseline
   requirement (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:196`).
   P1-E repeats the stronger rule: typed Track 2 leaves cannot prove generated
   typed Track 1 primitives or direct rows
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:351`).

3. The JSON report lane remains separate from a generated non-JSON baseline.
   SK-V12 requires one generated non-JSON direct or typed parser baseline before
   JSON-only micro-waves, with generated Track 1, independent Track 2/oracle,
   strict equality, same-run throughput, provenance, telemetry, and no JSON
   policy leak (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`,
   `:41`, `:44`). P1-B says no non-JSON product row exists in its capture set
   and that JSON product profiling does not substitute for the required
   generated non-JSON baseline
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:238`,
   `:240`). P1-F states the only admitted non-JSON surface is the REDRESS 111
   companion report lane for `--w1a-non-json-report`, not a generated baseline,
   admission row, or `skinny/RESULTS.md` movement
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:183`,
   `:184`, `:185`).

4. Source confirms the generated non-JSON blocker is real, not hidden by docs.
   `json_provider::ensure_runtime_profile` accepts only `grammar_name ==
   "json"` and errors otherwise (`skinny/crates/codegen/src/json_provider.rs:4`,
   `:5`, `:8`). Both untyped and typed emission call that JSON-only guard
   (`skinny/crates/codegen/src/lib.rs:108`, `:146`). P1-F's source read reaches
   the same conclusion: direct and typed emission still route through
   `json_provider::ensure_runtime_profile`, and runtime grammars contain
   generated `json` plus `sheets_witness`, with no generated `css_l4` or
   `css_l4_declaration_values` runtime
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:188`,
   `:190`, `:191`, `:192`).

5. The W1a non-JSON report validator is schema/gate-only and rejects admission
   coupling. The gate exits early when `--w1a-non-json-report` validates a
   report (`skinny/crates/bbnf-bench/src/bin/gate.rs:37`, `:40`, `:42`), and
   refuses to combine that flag with JSON result update or probe flags
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:404`, `:408`, `:415`). The report
   validator rejects W1a row admission by requiring `outcome_id == "S"` and
   `verdict == "NO-GO"` (`skinny/crates/bbnf-bench/src/report.rs:1814`), requires
   `same_wave_consumer_class == "non_json_gate_schema_only"` and
   `track2_independence_status == "independent_verified"`
   (`skinny/crates/bbnf-bench/src/report.rs:1821`, `:1823`), and requires the
   W1a oracle to be a same-plane `internal_oracle` with `same-run-oracle`
   freshness and `sidecar_freshness == "n/a"`
   (`skinny/crates/bbnf-bench/src/report.rs:1865`, `:1870`, `:1873`, `:1874`).
   The validator also rejects non-independent oracle sources
   (`skinny/crates/bbnf-bench/src/report.rs:1882`, `:1893`) and has regression
   tests for gate-only/admission-claim rejection
   (`skinny/crates/bbnf-bench/src/report.rs:2320`, `:2327`).

6. Source-delta claims do not smuggle a behavior producer. P1-A says the
   behavior-source diff from `3ce75df4` is limited to gate/report files and
   that `50bd1648..HEAD` is empty under `skinny/crates`, `skinny/Cargo.toml`,
   and `skinny/Cargo.lock`
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:24`,
   `:27`, `:29`). P1-F independently records no
   `skinny/RESULTS.md` or `skinny/REDRESS.md` diff from SK-V11 close, then
   narrows the source delta to `skinny/REDRESS.md`, `skinny/RESULTS.md`,
   `bbnf-bench` gate/report consumer code, and no parser-source changes under
   codegen/runtime/bbnf-simd/parse-that
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:197`,
   `:200`, `:203`). I re-ran those source filters and observed the same
   boundary: only `skinny/crates/bbnf-bench/src/bin/gate.rs` and
   `skinny/crates/bbnf-bench/src/report.rs` changed under the behavior-source
   filter from `3ce75df4`, and no behavior-source diff exists from `50bd1648`.

7. PMU, samply, and xctrace evidence is not coupled to row admission. P1-D
   limits PMU authority to `PROBE_RESULT` rows and states that cycles,
   instructions, c/B, and CPI do not move `skinny/RESULTS.md`, admit direct or
   typed rows, or change the opening `N-direct / NoGo` surface
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:81`,
   `:87`, `:88`). P1-E says result authority remains `skinny/RESULTS.md` plus
   REDRESS 119/120 and that PMU throughput/cycles are diagnostic planning facts,
   not row admission facts
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:29`).
   P1-F says the fresh capture artifacts are not consumed by `skinny/RESULTS.md`
   as row movement, hot-leaf symbol resolution, or direct/typed admission
   evidence (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:31`).
   The report validator also requires the W0 diagnostic status
   `structural_scan+masking_probes+pmu+cycles:nonproducer`
   (`skinny/crates/bbnf-bench/src/report.rs:396`), and P1-F confirms PMU,
   cycles, structural scan, masking probes, and Criterion slope metadata remain
   diagnostic unless a same-wave gate consumes them as admitted behavior
   evidence (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:225`,
   `:226`, `:227`).

8. Missing exact SK-V12 self-time percentages are fenced rather than papered
   over. P1-A, P1-B, and P1-E all say the samply profiles are
   `symbolicated=false`, `.json.syms.json` files carry symbol maps, and no fresh
   xctrace summary export exists, so they do not fabricate exact inline
   percentages (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:95`,
   `:97`, `:100`; `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:114`,
   `:117`, `:118`; `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:73`,
   `:76`, `:77`). P1-E frames its row attribution as source-map attribution
   plus fresh PMU/capture facts, not exact self-time evidence
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:67`,
   `:70`, `:71`, `:78`). That is not hidden coupling because the packet does
   not use those absent percentages to admit rows.

9. Sidecar/substrate routes stay pre-blocked. SK-V12 synthesis bans W3
   union/event/class-column/streaming-cursor/class-lane/sidecar substrates,
   parse-only SOTA admission, direct digest as typed proof, PMU/cycles and
   structural-scan evidence as behavior producers, JSON policy leakage, and new
   public/parser-owned substrate surfaces
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:230`, `:233`, `:237`,
   `:242`, `:244`, `:245`). P1-C carries the same pre-blocks and states that
   structural-scan observations cannot reopen a sidecar, retained vector,
   class-column, streaming cursor, `UnionTape`, or class-lane route
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:208`,
   `:210`, `:211`, `:227`). P1-E's pre-block table similarly keeps structural
   rediscovery, retained classes, sidecar vectors, parser-owned cursors,
   streaming cursor, class column, and `UnionTape` closed
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:302`,
   `:304`).

10. Sidecar comparator evidence is not a strict admission shortcut. The
    JSON report notes native Rust comparators are same-run while C++ sidecars
    are historical or explicitly absent and never strict anchors
    (`skinny/RESULTS.md:146`). The strict admission validator rejects stale,
    historical, or absent comparator/sidecar freshness and requires
    `same-run-native` plus `sidecar_freshness == "n/a"`
    (`skinny/crates/bbnf-bench/src/gate.rs:170`, `:177`, `:179`). The schema
    validator also rejects unstructured sidecar same-run claims
    (`skinny/crates/bbnf-bench/src/report.rs:1616`, `:1634`, `:1640`).

## Required Folds

These are carry-forward folds for the V1 consolidation, not REVISE blockers:

1. Preserve the CH5 lane fence verbatim: generated Track 1, independent
   Track 2/oracle, direct digest, typed direct, W1a non-JSON report lane, and
   future generated non-JSON baseline are separate authorities.
2. Record that PMU, cycles, samply, xctrace, structural-scan, masking-probe,
   sidecar-freshness, lazy-tape, and Criterion-slope evidence is diagnostic
   unless a later same-wave gate explicitly consumes it as behavior evidence.
3. Carry P1-E's percentage caveat into the consolidation: V1 accepts source-map
   attribution plus fresh PMU/capture facts for CH5, but it must not be restated
   as exact SK-V12 inline self-time percentage evidence.
4. Keep REDRESS 111 as a report/gate lane only. S-P2/S-P3 must still create a
   real generated non-JSON Track 1 baseline with an independent same-plane
   oracle or Track 2; the current JSON-only `json_provider` path is a blocker,
   not hidden baseline authority.
5. Do not reopen W3 substrate, sidecar, retained cursor/vector, parse-only
   SOTA, direct digest-as-typed, or JSON direct residual routes from the P1
   profile facts. JSON residual movement still needs the full SK-V12 reopen
   burden after the generated non-JSON priority resolves or honestly blocks.
