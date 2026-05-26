# Skinny Spec — Workspace + LOC Budget

## 0. Purpose And Boundary

This document specifies the workspace shape and LOC budget for the **JSON skinny**: a focused subset of the V1 architecture sufficient to validate the SOTA-viability premise before tranches A-J commit. The skinny exists to take *one* grammar (JSON) end-to-end through compiler -> codegen -> runtime -> bench, measure against `sonic-rs` / `simd-json`, and update SOTA-beat probabilities with measurement-grounded evidence.

The skinny is buildable in 2-4 weeks of focused work or it is not skinny. This file binds:

- The crate set (10 crates, no more).
- Per-crate LOC budgets (handwritten only; generated under `runtime/src/grammars/json/` is excepted from per-file caps but tracked under a separate generated-budget row).
- The workspace TOML.
- The directory layout.
- The build-and-test commands.
- The `xtask` runner that replaces a full `bbnf-cli`.
- The stub policy for skipped V1 crates.
- The migration parity matrix mapping skinny crates to V1 destinations.
- The build-time targets and the dev-iteration discipline.
- The explicit list of what the skinny omits and the impact of each omission on the SOTA-viability test.

**SK-V6 workspace fold-back (2026-05-15).** The workspace now treats
`bbnf-simd` and `bbnf-bench` as admission gates, not support code. `bbnf-simd`
must keep grammar-neutral primitive modules, scalar oracles, target dispatch,
and DAV1D-grade `primitive-checkasm`; JSON wrappers and generated `.data` stay
under `runtime/src/grammars/json/` or generated output. `bbnf-bench` must emit
schema v3 comparator-plane rows before any SOTA decision. Remaining Lock 14
cleanup includes splitting the `bbnf-simd` JSON god-module residue and
removing JSON-name logic from generic pass/codegen crates.

**Pass Omega V2 / SK-V14 workspace receiver (2026-05-24; post-CRUD-3 LOCKS
v+1 at `85a043224`, 779 lines).** The active workspace boundary is no longer
JSON-only validation; full cohort §3Z LOCK convergence (S-P2 `4c70b6f193` +
T-P1 `0a9c0fe65d` + S-P3 `626cb06cc1` + T-P2 `34a28f5c15` + T-P3
`69eea1c5c`) authorises the SK-V14 12-wave plan (W0..W11) per the α-E
candidate shortlist. `bbnf-bench` owns the grammar-neutral common telemetry
envelope (extended with four mandatory audit-overlay columns per LAC-1E-16:
`track2_entry_point`, `comparator_plane`, `per_iter_equality`,
`audit_overlay_verdict`), rolling SOTA delta production, JSON sonic-rs
strict comparators, CSS lightningcss/cssparser comparators, PMU/samply
capture references, and gate-consumed provenance. `bbnf-simd` owns
source-present primitive inventory only when each primitive is wired,
deleted, scalar-delegated, or architecturally blocked by the owning wave
under `BBNF_SIMD_STRICT=1` cohort-wide (LAC-2D / F-V3-CH4-A;
`restart/locks/LOCKS.md:295`). CSS L4 (15 sub-grammars per 3E
grammar-generalisation: `color`, `easing`, `filters`, `func-body`,
`gradients`, `keyframes`, `keywords`, `media`, `properties`, `selectors`,
`stylesheet`, `tokens`, `transforms`, `value-unit`, `values`) and all 51
JSON rows are SK-V14 close targets or architectural-block proofs, not
V1-H caveats. Pattern H = 67 hand-written runtime files per LAC-1E-15
per-tranche census (live find `find crates/core/src/runtime -mindepth 2
-type f -name '*.rs' \| wc -l`); `runtime_profiles() -> [&'static
GrammarProfile; 8]` static roster at `skinny/crates/codegen/src/grammar_profile.rs:100`-`110`
must be replaced by W5B-GEN and deleted by W5C-DELETE per the
generated-provider manifest receiver wave.

**Next-cycle dispatch posture** per Pass Omega V6 W5BR: REDRESS-183 is
historical after W2 admitted as skinny-side `regen-css` at `45568e669`, and
W3 production CSS corpus staging admitted at `b0a864f0b`. REDRESS-184 rejected
the original W4 provider-deletion gate; after V4 CRUD, W4 reruns as
ledger-only CSS L4 PRUNE with no CSS source/generator/provider/template
deletion. REDRESS-209 rejected the original monolithic W5 provider-collapse
gate. After V5 CRUD, W5A admitted the source-consuming request boundary at
`286233fa2`. REDRESS-210 rejected W5B deletion because live provider-backed
generation remained. After V6 CRUD, W5B-GEN owns the provider-free generator
body and W5C-DELETE owns CSS provider/template deletion only after W5B-GEN is
load-bearing.
`crates/core/src/runtime/css_l4/` remains Pattern H root-runtime work until
W6.0 after W5C-DELETE closes. W8/W9/W10 remain globally
blocked until PRUNE-1..PRUNE-5 close. **SK-V15 Pass Alpha re-entry handoff**
per F-V2-CH4-3E D06: the generated-fixture impl tail for CSS L4 onboarding
(5×15 sub-grammar matrix) enters as SK-V15 SPEC entry condition only if
SK-V14 R10 does not close it first (close anchor = SK-V14 W11 close per SPEC
§13:248).

This document records ownership and budget receivers only; it does not
authorize telemetry implementation, source edits, gates, `RESULTS.md`, or
`REDRESS.md` mutations.

Not in this file: substrate internals (`SUBSTRATE.md`), compiler pipeline internals (`COMPILER.md`), bench thresholds and reproducibility schema (`BENCH.md`).

### 0.1 Post-Iteration State (SK-V2)

The on-disk skinny prototype has iterated against this spec; the accepted and
rejected implementation decisions are recorded in `skinny/REDRESS.md`. The
load-bearing measurement findings:

- **`bbnf-bench` cap was redressed to 3,300 LOC after the final auditability gates and direct-to-struct workload landed**: `xtask lint-loc` now carries the fastest-anchor `S` report columns, subprocess RSS probes, persisted SIMD parity metadata, masking probes, and direct-to-struct rows inside the bench crate. The crate-local cap moved because the BENCH.md §7.9/§8.2 reporting contract plus the SK-V3 direct projection proof are now gate surfaces, not optional reports. The old 2,000/2,400 caps were too narrow for the full proof. Track 2 remains gated by substrate-API correspondence (per BENCH.md §10.6), not by an LOC cap. The total handwritten skinny envelope is redressed to 32,000 LOC.
- **Expanded parse corpus still has G/D/E rows; overall gate is
  N-direct / NoGo** (per `skinny/RESULTS.md`): the lazy tape/direct substrate
  validates on several shapes, while the expanded parse corpus currently has
  5 G rows, 4 A rows, 3 D rows, and 5 E rows. Canada structural scan is green
  against the 40000 Mbps floor after SK-V5 redress item 56. Direct-to-struct
  correctness is green. The sink-only `semantic_full_digest_stressor` direct
  rows pass five fixtures and miss 12; representative `real_typed_struct`
  rows pass for `twitter` and `update_center`. Codegen remains empirically
  separable from substrate; the current blocks are true BIR SinkOnly lowering,
  event-cursor/string/Unicode lowering coverage,
  structural-scan floor restoration, and exact float/string/Unicode
  materialization inside typed sinks.
- **Rejected routes remain recorded** (Lane 9 greenfield discipline): pair-token fusion (REDRESS §16), function-pointer dispatch table (REDRESS §17), 12-byte skipless token (REDRESS §18), structural-index typed parser prepass, NEON no-escape string matcher, separator elision, generic SWAR whitespace skipper, and width churn (REDRESS §25). The accepted path is lazy-offset tape with sparse flags, direct spare-capacity offset writes, SWAR digit/plain-string runs, delimiter fusion, `parse_value_at`, short plain-string fast path, cold errors, and Track 2 inline parity.
- **The host-call probe split (REDRESS §19)** disposes single-probe / 2-percent-threshold phrasing throughout this file: dispatch overhead passes (≤50ns); eager-decode bands MASK at 57.6%/77.2%/81.9% of Track 1 ns across corpora. The host-fn-free skinny is FAITHFUL only for a V1 path that keeps string decode lazy.

Per-section text below has been refreshed to match these findings; see §8.1 (deviation ledger), §10 (omission impact), and §11 (closure conditions) for the binding cells. The empirical LOC headroom is read as V1-destination-shape headroom, not as scope-wrong evidence: a per-crate budget overrun in the prototype is a Lens N graduation-mechanicality signal, not a Lens L scope-wrong signal — except for `bbnf-bench`, which is the only crate near its cap.

## 1. Skinny Crate Set

Exactly ten crates plus `xtask`. The list is closed. Adding another crate is a scope amendment, not a workspace change.

| # | Crate | Status | Role in skinny |
|---|---|---|---|
| 1 | `bbnf` | partial | Public facade. `Grammar` trait, borrowed `parse`, `parse_in`, and a cold `parse_owned` wrapper for JSON only. No LSP types; no `path` re-exports; minimal `Diagnostic`. |
| 2 | `grammar` | partial | BBNF parser sufficient for `grammars/json.bbnf`. Subset of the six-directive surface: only `@import` (and trivially `@token`); the main JSON grammar is host-fn-free by deliberate skinny deviation; no `@error(recover)`, no `@layout`, no `@pretty`. Metadata schema validation. |
| 3 | `ir` | partial | Grammar IR + Backend IR variants JSON exercises. Minimal pretty-printer. ID arenas. Validate. |
| 4 | `passes` | partial | `normalize`, `layout/types` (HM-only — no DK13, no GADT, no OutsideIn), minimal `shapes`, inlined recognizer-curation; bridge stubs only. Heaviest skinny crate by LOC because three would-be-separate concerns share it. |
| 5 | `codegen` | partial | `lower::bir` + `rust` backend. Templates for the JSON runtime. No `simd_cfg` directive emission; `bbnf-simd` dispatch is unconditional with scalar fallback wired in. No WASM, no TS. |
| 6 | `runtime` | partial | `tape`, `document`, `builder`, minimal `visitor`. The generated JSON parser lands at `runtime/src/grammars/json/`. |
| 7 | `parse-that-regex` | partial | Basic regex sufficient for JSON's string-escape and number scanners. No Unicode class algebra. Subset HIR; NFA / DFA / VM; literal helpers. |
| 8 | `bbnf-simd` | full | Replacement scanner crate for runtime and bench. NEON + scalar are mandatory; AVX2 stays available; AVX512/VBMI2 and handwritten ASM are host-target work under Lock 16, not a separate scanner crate. |
| 9 | `bbnf-bench` | partial | Criterion harness, reproducibility schema emitter, parity matrix runner, independent oracles, sidecar comparators, and the Track 2 handwritten substrate/direct probes (substrate-API correspondence per BENCH.md §10.6; measured against `runtime::tape::*`, runtime event/sink traits, and `bbnf_simd::*` calls). It must not own the Track 1 `SinkOnly` implementation; Track 1 direct is generated runtime/codegen in SK-V4. Owned by BENCH.md; this slice budgets its LOC only. |
| 10 | `test-fixtures` | partial | JSON corpora (`twitter`, `citm`, `canada`, plus malformed minicorpus) + manifest. |
|  + | `xtask` | dev | Tiny binary (≤650 LOC) replacing `bbnf-cli` for the skinny: `regen-json`, `check-json`, `check-conformance`, `lint-loc`, `bench-json`, `gate-json`, `primitive-checkasm`. |

### 1.1 V1 Crates Skipped In The Skinny

| Skipped V1 crate | Skinny disposition | Rationale |
|---|---|---|
| `bbnf-cli` | Replaced by `cargo test` and `xtask`. | Full CLI argument parsing, command discovery, workspace traversal, debug subcommands are scope creep for a JSON-only skinny. |
| `bbnf-language-server` | Skipped entirely. | LSP / DAP / incremental parse cannot influence the SOTA-beat measurement. Carried into V1 at tranche I (receiver: I.W2 per INDEX cross-references). |
| `vm` | Skipped entirely. | The VM is a debug-replay validator for BIR. The skinny validates BIR by running the Rust lowerer's output against the test-fixtures corpus directly. Carried into V1 at tranche E (receiver: E.W2 per INDEX cross-references). |
| `host` | Inlined as a 50-LOC private module in `bbnf::host_stubs`. | The main JSON grammar has no `@host fn` calls. BENCH.md still emits a one-host-fn probe to bound the `CallHost` registry cost before the skinny claims the cut is FAITHFUL. |
| `cost-model` | Skipped entirely. | The skinny's optimization choices are statically wired in `passes` (use SIMD scan for JSON structural; use simple recursive descent for value parse). BENCH.md bounds this cut with alternate-plan probes; a probe win routes to V1 H.W2/H.W3 rather than being called free. |
| `egraph` + `egraph-derive` | Skipped entirely. | No e-graph rewrites in the skinny; `passes::normalize` is plain. |
| `csp-solver` | Skipped entirely. | HM-only inference does not need a CSP. JSON's grammar is monomorphic. |
| `parse-that` | Skipped (legacy combinator core). | Not needed for JSON. The regex sub-crate (`parse-that-regex`) suffices. |
| `path` / `path-core` / `path-ts` | Skipped entirely. | Path DSL is irrelevant to the SOTA-beat test. JSON access in benches uses the generated `Document` view directly. |
| `error` | Inlined as a 100-LOC private module in `bbnf::diagnostic`. | Diagnostics in the skinny are simple `(span, code, message)`. The full `error` crate exists in V1 to share this between `bbnf` and the LSP; the skinny has no LSP. |
| `source` | Inlined as a 150-LOC private module in `passes::source_stub`. | Span + file ID + slice loader; no rope, no include graph (JSON imports nothing), no snapshots. |
| `pipeline` | Inlined as orchestration in two callers: `xtask/src/main.rs::regen` (regen path) and the bench harness path (currently `bbnf-bench/src/probes.rs`). A public `bbnf::compile` facade is deferred to V1 graduation; the skinny does not expose a pipeline as a stable surface. | The pipeline crate carries cache keys, scheduler topology, and stage DAGs in V1. The skinny pipeline is linear and compile-time fixed: `parse -> validate -> infer -> mine -> lower -> emit`. No caching. V1 graduation extracts the orchestrator into a `pipeline` crate; the two skinny call sites collapse into a single `bbnf::compile` consumer. |

The skinny's compatibility shims (`source_stub`, `host_stubs`, `diagnostic`) are intentionally named with a `_stub` or domain-suffix to make their migration to dedicated crates mechanical when the skinny graduates.

## 2. Per-Crate LOC Budget

The handwritten LOC budget for the skinny is **32,000 LOC** across ten crates. Generated output (`runtime/src/grammars/json/**/*.rs`) is budgeted separately at **≤ 4,000 LOC** for JSON, matching PASS-2's KEEP-MODIFY observation that the current `core/src/grammar/generated/json.rs` lands at 3,500 LOC plus 2 percent (`restart/audit/pass-2-codegen/PASS-2.md:432`). The delta from the original 31,400 ceiling is a BENCH-side redress: direct-to-struct proof rows and primitive admission reporting became mandatory falsifiability gates during SK-V3.

| Crate | Skinny LOC | Skinny subset rationale | Full V1 estimate (these crates) |
|---|---:|---|---:|
| `bbnf` | 600 | Facade with `Grammar` trait, borrowed `parse`, `parse_in`, cold `parse_owned` wrapper outside the SOTA path, inlined `Diagnostic` (~100 LOC) and `host_stubs` (~50 LOC). No `path` re-export. No metadata-only-change cache. | 1,500 |
| `grammar` | 3,500 | BBNF parser sufficient for `json.bbnf`, AST, validate, `metadata` schema with one grammar entry, bootstrap parse path. `@import` honored; the other five directives parse to a no-op trapdoor that errors with `BBNF-DIRECTIVE-NOT-IN-SKINNY`. | 10,000 |
| `ir` | 2,500 | Grammar IR variants JSON uses (`Seq`, `Alt`, `Repeat`, `Optional`, `Terminal`, `RegexProgram`, `RuleRef`); BIR variants the codegen consumes (`Match`, `Alt { mode }`, `Loop`, `CallRule`, `RegexProgram`, `SimdScan`, `Return`); IDs; validate; pretty. No `LayoutScope`, no `CallHost`, no Pratt nodes. | 8,000 |
| `passes` | 6,000 | `normalize/` (~1,000), `layout/types/` (HM-only) (~2,500), `shapes/` minimal (~800), inlined recognizer-curation under `recognizers/` (~1,200), `bridge/` stubs (~200), `source_stub/` (~150), test plumbing (~150). Heaviest skinny crate; this is the contradiction-flagged row — see §2.1. | 25,000 |
| `codegen` | 4,500 | `lower/` (~1,500), `rust/` backend (~2,000), `templates/json` (~700), `verify/` regen-equality (~300). No `wasm/`, no `simd/` directive emission (always-on dispatch in `runtime`), no Pratt templates. | 12,000 |
| `runtime` | 4,000 | `tape/` (~1,500), `document/` (~800), `builder/` (~600), `visitor/` minimal (~400), `support/` (~300), `grammars/json/` skeleton (~400, plus the generated body which is not handwritten). | 8,000 |
| `parse-that-regex` | 4,000 | `regex/hir` subset (~1,000), `regex/nfa` (~1,000), `regex/dfa` (~1,000), `regex/vm` (~700), `literal/` (~300). No `unicode/` algebra, no lazy-DFA cache policy. | 10,000 |
| `bbnf-simd` | 3,500 | Structural scanner and per-target primitive layer used by runtime and bench; carries scalar/SWAR, aarch64, x86_64 AVX2/VBMI2, and ASM-admissible host paths under Lock 16. | 3,500 |
| `bbnf-bench` | 3,300 | Criterion harness, reproducibility schema serializer, parity matrix runner, materialization report, scan report, masking probes, fastest-anchor `S` rendering, subprocess RSS probes, persisted SIMD parity metadata, direct-to-struct workload proof, sidecar comparator reports, and Track 2 handwritten parser/sink probes (substrate-API correspondence per BENCH.md §10.6). The 2026-05-12 full auditability gates plus SK-V3 direct projection proof made the old 2,000/2,400 caps too narrow. SK-V4 moves Track 1 direct into generated runtime/codegen `SinkOnly`; `bbnf-bench` owns measurement and independent oracles only. Under SK-V13 it also owns the common telemetry envelope, CSS comparator/oracle rows, and rolling SOTA delta reporting. Exact internal split owned by BENCH.md. | 4,000 |
| `test-fixtures` | 800 | JSON corpora pointers + checksums (~200), parity matrix manifest (~300), corpus loader (~300). Twitter / citm / canada are not committed as binary; they are downloaded by the loader and checksummed against the manifest. | 1,500 |
| **Skinny total (handwritten)** | **32,000** | | **~83,500** |
| Generated `runtime/src/grammars/json/` | ≤ 4,000 | PASS-2 baseline + 2 percent (`PASS-2.md:432`). Tracked separately; not counted in handwritten LOC. | ≤ 4,000 |
| `xtask` (dev) | ≤ 650 | Minimal subcommand binary for `regen-json`, `check-json`, `check-conformance`, `lint-loc`, `bench-json`, `gate-json`, and `primitive-checkasm`; not counted in skinny crate LOC. | (no V1 equivalent — replaced by `bbnf-cli`) |

### 2.1 The `passes` Contradiction

`passes` carries 6,000 LOC in the skinny because three V1-separate concerns (HM type inference, shape mining, recognizer curation) live there together, and the V1 budget for `passes` is 25,000 LOC. The 6,000-LOC skinny budget is achievable if and only if:

1. **HM-only.** No DK13 algorithmic completeness, no GADT pattern refinements, no OutsideIn implication constraints. Plain Hindley-Milner with let-polymorphism. JSON's grammar is monomorphic; this is sufficient.
2. **One-shot recognizer curation.** No e-graph rewrites; no CSP narrowing; no cost-model selection. A hand-written rule decides "use SIMD scan for JSON structural; use the regex VM for string escapes" and lowers it directly.
3. **Shapes are observational, not optimizing.** `shapes/` produces side tables consumed by `codegen::lower` only; it does not feed back into `passes::layout`.

If any of these three constraints is relaxed, the 6,000 LOC budget cannot hold and the skinny scope itself is wrong. **Flag this as a contradiction signal**: a `passes` budget overrun is evidence that the SOTA-viability test cannot run cheaply and the V1 spec needs partial implementation to validate the SOTA-beat claim.

## 3. Workspace `Cargo.toml`

```toml
[workspace]
resolver = "2"
members = [
  "crates/bbnf",
  "crates/grammar",
  "crates/ir",
  "crates/passes",
  "crates/codegen",
  "crates/runtime",
  "crates/parse-that-regex",
  "crates/bbnf-simd",
  "crates/bbnf-bench",
  "crates/test-fixtures",
  "xtask",
]

[workspace.package]
edition = "2021"
license = "MIT OR Apache-2.0"
repository = "https://github.com/mkbabb/bbnf-lang"
rust-version = "1.78"

[workspace.dependencies]
# Skinny-internal path dependencies. Names mirror the V1 published-name
# convention so the skinny -> V1 graduation does not rename consumers.
bbnf            = { path = "crates/bbnf" }
bbnf-grammar    = { path = "crates/grammar",         package = "grammar" }
bbnf-ir         = { path = "crates/ir",              package = "ir" }
bbnf-passes     = { path = "crates/passes",          package = "passes" }
bbnf-codegen    = { path = "crates/codegen",         package = "codegen" }
bbnf-runtime    = { path = "crates/runtime",         package = "runtime" }
bbnf-regex      = { path = "crates/parse-that-regex", package = "parse-that-regex" }
bbnf-simd       = { path = "crates/bbnf-simd" }
bbnf-bench      = { path = "crates/bbnf-bench" }
bbnf-fixtures   = { path = "crates/test-fixtures",   package = "test-fixtures" }

# Third-party.
criterion       = { version = "0.5", features = ["html_reports"] }
serde           = { version = "1", features = ["derive"] }
# `serde_json` is consumed only by `bbnf-bench` (parity oracle + manifest
# emission) and is therefore a dev-dependency of that crate, not a
# workspace-wide runtime dependency. It is published at the workspace level
# only so the pin (and feature flag set) stays consistent with the BENCH-side
# competitor harness; no non-bench crate imports it.
serde_json      = "1"
sha2            = "0.10"
toml            = "0.8"
anyhow          = "1"
thiserror       = "1"

[workspace.metadata.bbnf]
generated_root = "crates/runtime/src/grammars"
# `fixture_root` is the workspace-relative manifest directory. BENCH.md §3.2
# names the per-grammar corpus directory `tests/fixtures/json/` resolved as
# `<fixture_root>/json/` at load time. The two-stage path (workspace manifest
# dir + per-grammar corpus dir) is intentional: the workspace owns the
# `test-fixtures` crate; BENCH owns the per-grammar layout under it.
fixture_root   = "crates/test-fixtures/corpus"
profile        = "balanced"
# `host_registry` is a symbolic sentinel; the schema validator in `bbnf-grammar`
# accepts it without path lookup (Lock 14 schema-extension surface). The skinny
# value `"skinny-none"` signals "main JSON grammar has no @host fn" and is
# consumed by `xtask::regen` + the bench harness, not by cargo. The V1 schema
# enum will admit this value plus `"host::primitives"` (used by V1 grammars
# carrying @host fn calls); the validator never resolves the string to a code
# path, so the sentinel form is intentional.
host_registry  = "skinny-none"

[workspace.metadata.bbnf.recognizers]
# Recognizer overrides pin skinny choices. ARCH §5's canonical schema names
# `auto` as the default; the skinny extends the enum at the workspace-metadata
# layer (Lock 14 schema-extension surface; matches the host_registry pattern
# above). The V1 schema admits the same extension for grammars that choose to
# pin recognizers rather than auto-select.
pratt           = "off"
simd            = "json-structural-always"
literal_trie    = "off"
regex_prefilter = "json-regex-only"

[workspace.metadata.bbnf.host_fns]
# `default_registry` is a symbol-only reference; the schema validator does NOT
# resolve it to a code path. The V1 value `"host::primitives"` references the
# ARCH §5 canonical primitives registry; the skinny consumes this metadata only
# to thread the symbol through generated code. The on-disk skinny inlines a
# 50-LOC host stub under `bbnf::host_stubs` (no `host::primitives` module is
# present in the prototype) — the main JSON grammar has no @host fn calls, so
# the registry is never invoked. V1 graduation extracts the stub to the `host`
# crate; the metadata symbol survives unchanged.
default_registry    = "host::primitives"
allow_unregistered  = false

[workspace.metadata.bbnf.grammars.json]
source       = "grammars/json.bbnf"
package_name = "json"
features     = []
output_dir   = "crates/runtime/src/grammars/json"

[workspace.metadata.bbnf.grammars.json.runtime]
mode             = "tape-direct"
document_view    = true
owned_document   = false  # borrowed SOTA path; parse_owned is a cold facade wrapper.

[workspace.metadata.bbnf.grammars.json.host]
registry                 = "skinny-none"
allow_declaration_crate  = false
declaration_crate_reason = ""

[workspace.metadata.bbnf.grammars.json.optimization]
profile          = "balanced"
recognizers      = "skinny-json-curated"
pratt            = "off"
simd             = "json-structural-always"
layout           = "hm-only"
regex_prefilter  = "json-regex-only"

[workspace.metadata.bbnf.grammars.json.codegen]
rust                  = true
wasm                  = false
generated_loc_budget  = 1.02   # PASS-2.md:432 (json baseline 3,500; +2%).

[workspace.metadata.bbnf.grammars.json.fixtures]
valid    = ["crates/test-fixtures/corpus/json/valid"]
invalid  = ["crates/test-fixtures/corpus/json/invalid"]
perf     = ["crates/test-fixtures/corpus/json/perf"]

# ─────────────────────────────────────────────────────────────────────────────
# Profiles. The skinny carries three: dev (fast iteration), release (bench
# inheritance baseline), bench (samply-resolvable publish-grade numerics).
# More profiles are V1 territory; the skinny does not need ax-iter / bench-iter
# / profiling-prep splits because there is no LTO-vs-no-LTO trade-off study
# inside the skinny window.
# ─────────────────────────────────────────────────────────────────────────────

[profile.dev]
opt-level     = 0
debug         = "line-tables-only"
incremental   = true
codegen-units = 16

[profile.release]
opt-level         = 3
lto               = "fat"
codegen-units     = 1
debug             = true   # samply-symbol-resolution rule: DWARF survives.
strip             = false
split-debuginfo   = "packed"

[profile.bench]
inherits          = "release"
opt-level         = 3
lto               = "fat"
codegen-units     = 1
debug             = true   # Required for samply / instruments symbol
                           # resolution. Per the user's
                           # samply-symbol-resolution rule, samply is run
                           # interactively (`samply record <bin>`); no
                           # `--save-only` because lazy symbol resolution
                           # depends on debug=true + DWARF that survives
                           # the link step.
strip             = false
split-debuginfo   = "packed"
```

### 3.1 Profile Discipline

The skinny's `release` and `bench` profiles both set `debug = true` and `strip = false` so that `samply record target/bench/deps/json_parity-*` resolves frame symbols correctly. This is non-negotiable per the user's `samply-symbol-resolution` rule: stripped binaries produce address-only stacks that hide whatever pathology the SOTA-beat measurement is supposed to surface (the prior 86.07 percent `Vec<OpenFrame>::clone` pathology cited at Lock 1 was only diagnosable because DWARF survived).

`thin` LTO (not `fat`) is intentional: the skinny budget cannot absorb the multi-minute fat-LTO link cost on every iteration. If the parity rows land within the SOTA-beat envelope under `thin` LTO, fat-LTO is not the determining factor; if they do not, the V1 plan owns the fat-LTO study.

`codegen-units = 16` for `dev` keeps the dev-build incremental fast (no proc-macro hell, no fat link). The user's `build-infra-first` rule applies: dev iteration speed beats marginal release-time gains.

## 4. Directory Layout

Each crate honors Lock 13's 4-10 immediate child-count rule under `src/`. Any directory with fewer than 4 or more than 10 children is a fault.

### 4.1 `crates/bbnf/`

```text
bbnf/src/
  lib.rs
  prelude.rs
  parse/                   # parse, parse_in, cold parse_owned entry points.
  document/                # re-exports of runtime::document::Document.
  diagnostic/              # inlined Diagnostic { span, code, message }.
  metadata/                # workspace.metadata.bbnf reader.
  host_stubs/              # 50-LOC stub for runtime/codegen import compatibility.
```

Six children. Within Lock 13.

### 4.2 `crates/grammar/`

```text
grammar/src/
  lib.rs
  ast/                     # BBNF AST types.
  parse/                   # bootstrap parser.
  validate/                # semantic validation; rejects non-skinny directives.
  metadata/                # workspace.metadata.bbnf schema validator.
  bootstrap/               # bootstrap entrypoints used by xtask.
```

Five children.

### 4.3 `crates/ir/`

```text
ir/src/
  lib.rs
  grammar_ir/              # subset variants for JSON.
  backend_ir/              # subset BIR alphabet.
  side_tables/             # Layout, Shape, Recognizer.
  validate/
  pretty/
```

Five children.

### 4.4 `crates/passes/`

```text
passes/src/
  lib.rs
  normalize/               # alpha-rename, dead-rule elim.
  layout/                  # HM-only inference + layout lowering.
    types/                 # Algorithm W; V1 wraps this as a layout subroutine.
  shapes/                  # observational shape mining.
  recognizers/             # inlined recognizer curation (SIMD-scan + regex).
  bridge/                  # stubs; populated only enough to compile.
  source_stub/             # 150-LOC source-file/span shim.
```

Six children at `src/`. The `bridge/` directory is intentionally vestigial in the skinny; it exists so the V1 `passes::bridge` import path is reserved.

`layout/types/` is a deliberate single-child mount-point: `types/` is the only child of `layout/` in the skinny because DK13 / GADT / OutsideIn / CSP siblings are V1 territory (per §10 row "GADT / DK13 / OutsideIn / CSP type-system"). Lock 13's 4-10 immediate-children rule applies to crates with public reach; the mount-point form is ratified for single-child dirs whose siblings are named in the deviation ledger (§8.1 row "HM-only `passes` constraint"). V1 graduation adds the DK13/GADT/CSP siblings around `layout/types::algorithm_w`, lifting the directory back into Lock 13 compliance.

### 4.5 `crates/codegen/`

```text
codegen/src/
  lib.rs
  lower/                   # BIR -> RustModule.
  rust/                    # Rust source emission.
  templates/               # JSON-runtime template strings.
  verify/                  # content-equality regen check.
```

Four children. Under Lock 13.

### 4.6 `crates/runtime/`

```text
runtime/src/
  lib.rs
  tape/                    # Tape, ValueRef, payload arena (SUBSTRATE.md owns details).
  document/                # DocumentView, generic root projection.
  builder/                 # TapeBuilder + checkpoint primitives.
  visitor/                 # minimal visitor.
  support/                 # span -> bytes, slice loaders.
  grammars/                # generated subdirs only.
    json/                  # generated; not handwritten.
```

Six children at `src/`. `grammars/` is a generated mount-point: the historical
skinny started with only `json/`, while SK-V13 adds CSS L4 generated rows under
the same generated-provider discipline. The mount-point form is ratified by
Lock 14's generation-target shape: every grammar lands as its own generated
subdir under `grammars/`, and a single-child intermediate is acceptable only as
a tranche state, not as a campaign close. Lock 13's 4-10 rule applies to crates
whose immediate children are handwritten; `grammars/` is generated and exempt
from the per-file 500 LOC cap and from the immediate-children minimum.

### 4.7 `crates/parse-that-regex/`

The natural `regex/{hir,nfa,dfa,vm}` nesting gives only 3 children at the `src/` level (`lib.rs`, `regex/`, `literal/`). Lock 13 demands 4-10. Resolution: promote the regex sub-trees to top-level siblings:

```text
parse-that-regex/src/
  lib.rs
  hir/
  nfa/
  dfa/
  vm/
  literal/
```

Six children. Within Lock 13. The V1 `parse-that` carries `unicode/` and `prefilter/` siblings as well; the skinny omits both.

### 4.8 `crates/bbnf-simd/`

```text
bbnf-simd/src/
  lib.rs
  scalar/
  aarch64/
  x86_64/
  dispatch.rs
  classifier.rs
```

Six entries. `bbnf-simd` is the scanner and byte-primitive dependency surface. The JSON skinny exercises scalar/SWAR and aarch64 on the primary host; x86_64 AVX2/VBMI2 remains the secondary host path.

### 4.9 `crates/bbnf-bench/`

```text
bbnf-bench/src/
  lib.rs
  fixtures/                # corpus loader + checksum.
  metadata/                # reproducibility-schema emitter.
  parity/                  # token stream + canonical-output oracle.
  gates/                   # SOTA gate runner.
  report/                  # RESULTS.md renderer.
  track2/                  # handwritten substrate ceiling probe; correspondence-gated.
```

Seven children. Internal layout owned by BENCH.md.

### 4.10 `crates/test-fixtures/`

```text
test-fixtures/src/
  lib.rs
  corpus/                  # corpus manifest + checksum loader.
  matrix/                  # parity matrix.
  generated/               # generated fixture references.
```

Four children. Within Lock 13.

### 4.11 `xtask/`

```text
xtask/src/
  main.rs                  # ≤650 LOC; dispatch + regen + checks + LOC + bench + checkasm.
```

Single-file binary; not subject to Lock 13's directory rule because the dev tool is not a public crate.

## 5. Build And Test Commands

Each command below is copy-pasteable from the workspace root. All commands are sandbox-safe (read or build only); none depend on network access except `xtask download-fixtures`.

```bash
# 1. Workspace check (no link).
cargo check --workspace --all-targets

# 2. Workspace build (release).
cargo build --workspace --release

# 3. Workspace tests (incremental dev; fast feedback loop).
cargo test --workspace

# 4. Regen the JSON generated runtime; fail if content differs after regen.
cargo xtask regen-json

# 5. Regen-equality check (no write; fail if regen would diverge).
cargo xtask check-json

# 6. Run the JSON bench dispatch (criterion; emits HTML reports + JSON manifest).
cargo bench -p bbnf-bench --bench json_parity --bench simd_scan -- --save-baseline skinny

# 7. LOC guard for handwritten + generated skinny budgets.
cargo xtask lint-loc

# 8. ASAN run on JSON corpora (catches tape-arena UB before SOTA-beat noise hides it).
RUSTFLAGS="-Z sanitizer=address" \
  cargo +nightly test --workspace --target x86_64-apple-darwin

# 9. UBSAN run.
RUSTFLAGS="-Z sanitizer=undefined" \
  cargo +nightly test --workspace --target x86_64-apple-darwin

# 10. Samply profiling capture for the JSON parser hot loop.
#    --release inherits the bench profile via --profile=bench.
#    Per the user's samply-symbol-resolution rule:
#    - debug = true must hold in the profile (it does, see §3).
#    - Use interactive `samply record`; do NOT use `--save-only` — lazy
#      symbol resolution depends on debug DWARF surviving the link step
#      and being looked up at view time.
cargo build --profile=bench -p bbnf-bench --bench json_parity
samply record \
  ./target/bench/deps/json_parity-* \
  --bench --profile-time 30 \
  twitter

# 11. Build-time guard: clean release build under 90s on M1 Pro.
cargo clean
time cargo build --workspace --release    # expect <= 90s.
```

The build-time target at command 10 is the iteration discipline floor. A clean release build that takes longer than 90 seconds breaks the 2-4-week skinny window: a developer running the SOTA-beat loop will recompile under `--release` repeatedly to chase samply pathologies, and a 3-minute clean-build penalty multiplied across iterations dominates the calendar. Per the user's `build-infra-first` rule, dev-loop speed is paid up front.

The 90-second target is provenance: it is the iteration ceiling under which an edit-build-bench loop stays under 5 minutes wall-clock on M1 Pro (M1 Pro 10-core, 32 GB; `thin` LTO; `codegen-units = 1` for the release/bench profiles; `debug = true` for samply symbol resolution). The constituent budget: ≤90s clean release + ≤30s samply capture per corpus (twitter / citm / canada) + ≤60s edit-think-revise overhead. Two iterations per loop hour ≈ 14-16 SOTA-beat samples per work-day, sufficient for the 2-4-week skinny window. A clean release exceeding 90s drops the daily sample count below 8 and pushes the window past 4 weeks; the build-time gate is therefore not aesthetic but binding.

If the 90-second target is not met:

| Cause | Surgery |
|---|---|
| `passes` proc-macros over-instantiate. | Push to runtime function calls; no proc-macros in the skinny. |
| `parse-that-regex` builds a full DFA at compile time. | Defer DFA construction to `OnceCell<Lazy>` at runtime. |
| Generic monomorphization explosion in `runtime`. | Erase one or two generic parameters via dyn-dispatch on cold paths. |
| `bbnf-simd` secondary-target modules drag in heavy intrinsics. | Cargo-cfg gate them off for the skinny build host unless the target feature is present and the kernel has scalar parity tests. |

## 6. The `xtask` Runner

The skinny ships one `xtask` binary at `xtask/src/main.rs`. The full `bbnf-cli` is overkill: command discovery, multi-grammar workspace traversal, debug subcommands, output formatting, and the playground/DAP/LSP bridge are unbuildable in the skinny window and irrelevant to the SOTA-beat measurement.

```rust
// xtask/src/main.rs (sketch; full source ≤650 LOC).

fn main() -> anyhow::Result<()> {
    let mut args = std::env::args().skip(1);
    let subcommand = args.next().context("usage: xtask <subcommand>")?;
    match subcommand.as_str() {
        "regen-json"  => regen::run(/*check_only=*/ false),
        "check-json"  => regen::run(/*check_only=*/ true),
        "lint-loc"    => loc::run(),
        "bench-json"  => bench::run(),
        other => anyhow::bail!("unknown xtask subcommand: {other}"),
    }
}

mod regen {
    // Reads workspace.metadata.bbnf, locates grammars/json.bbnf,
    // invokes grammar::parse -> passes::pipeline -> codegen::emit,
    // writes generated runtime under runtime/src/grammars/json/,
    // skipping content-identical writes (mtime preservation).
}

mod bench {
    // Wraps `cargo bench -p bbnf-bench --bench json_parity --bench simd_scan`
    // and then invokes the gate that renders skinny/RESULTS.md.
}

mod loc {
    // Enforces the ≤4,000 generated JSON LOC budget and the ≤3,300 LOC
    // `bbnf-bench` aggregate budget before bench results can authorize
    // dispatch. The Track 2 probe is no longer LOC-capped: it is gated by
    // substrate-API correspondence (must `use` `runtime::tape::*` +
    // `bbnf_simd::*` directly per BENCH.md §10.6). Track 2 measured at
    // ~318 LOC post-iteration; the cap is informational, not binding.
    // Budget-cliff diagnostic: when `bbnf-bench` LOC is in [3250, 3300],
    // emit a yellow warning naming the cliff before pass; when over, emit
    // BBNF-BUDGET-CLIFF naming the post-iteration headroom is exhausted
    // and pointing implementers at the SK-V2 surgery options.
}
```

The xtask runner is dev-only and does not appear in the public crate set. Its ≤650 LOC is not counted in the skinny's 32,000 LOC handwritten budget.

## 7. Stub Policy For Skipped Crates

Each skipped V1 crate is replaced in the skinny by either a deleted dependency, an inlined module, or a no-op shim. The shim discipline is:

| V1 crate | Skinny shim | Location | LOC |
|---|---|---|---|
| `error` | `Diagnostic { span: Span, code: &'static str, message: String }`. | `crates/bbnf/src/diagnostic/` | ~100 |
| `source` | `Source { id: SourceId, bytes: Arc<[u8]>, name: String }`, `Span { start, end, source: SourceId }`. | `crates/passes/src/source_stub/` | ~150 |
| `host` | Empty `HostRegistry` + `HostFnId(u32)` placeholder. Main JSON grammar has no `@host fn`; BENCH.md's one-host-fn probe is the only skinny `CallHost` measurement. | `crates/bbnf/src/host_stubs/` | ~50 |
| `pipeline` | Linear orchestrator function `compile_grammar(metadata) -> Result<RustModule>`. | `xtask/src/main.rs::regen` (regen path) + `bbnf-bench/src/probes.rs` (bench harness path); no `bbnf::compile` public facade until V1 graduation. | ~200 |
| `cost-model` | Pre-selected optimization choices wired into `passes::recognizers::json_curate()` and bounded by BENCH.md alternate-plan probes. | `crates/passes/src/recognizers/` | (counted in passes) |
| `vm` | None. BIR is validated by Rust-lowerer output running against fixtures. | — | 0 |
| `bbnf-cli` | `xtask` subcommands. | `xtask/src/main.rs` | ≤650 |
| `bbnf-language-server` | None. | — | 0 |
| `egraph` / `egraph-derive` / `csp-solver` | None; `passes` does not import. | — | 0 |
| `parse-that` | None; `parse-that-regex` is the only mined sister. | — | 0 |
| `path` / `path-core` / `path-ts` | None; benches use `Document` views directly. | — | 0 |

Total inlined-shim LOC: ~500. These are counted within their host crate's budget (`bbnf` carries 150 LOC of shims; `passes` carries 150 LOC of shims).

The shim discipline rule: when a shim grows past 500 LOC, it has earned its own crate. At that point the skinny is no longer skinny and the V1 graduation is what's needed.

## 8. Migration Parity

The skinny graduates to V1 by mechanical moves, not re-architecture. Every skinny crate has a clear V1 destination:

| Skinny crate | V1 destination | Mining or fresh |
|---|---|---|
| `bbnf` | `bbnf` | Mostly fresh (~600 LOC); migration: extract `diagnostic` into `error`, `host_stubs` into `host`, `metadata` reader stays. |
| `grammar` | `grammar` | Fresh write of bootstrap parser + AST. Mine concept from current `crates/core/src/imports/` and the bootstrap in `crates/bootstrap/`. |
| `ir` | `ir` | KEEP-MODIFY of extant `crates/ir/`: the IR ID arenas and validate scaffolding are mined; the variant set is rewritten to match ARCH §7 (subset for skinny, full for V1). |
| `passes` | `passes` (split into `passes::layout`, `passes::shapes`, `passes::recognizers`, `passes::bridge`) | Fresh HM in `layout/types`. Skinny `passes::source_stub/` migrates to `source` crate. Skinny `passes::recognizers/` consumes `bbnf-simd` integration patterns; the inlined cost-model heuristic moves to `cost-model`. DK13/GADT/CSP add siblings around `layout/types::algorithm_w` rather than rewriting it. |
| `codegen` | `codegen` | Fresh write per Lock 5 (lowerers consume BIR, not Grammar IR). Mine current Rust emission idioms from `crates/core/src/backend/` *as reference only* — the per-grammar walker pattern is a deletion target, not a migration source. |
| `runtime` | `runtime` | Tape internals are fresh per Lock 1. `runtime/src/grammars/json/` migrates verbatim (it is generated). Mine the current `crates/core/src/runtime/` for visitor patterns; reject the OpenFrame clone substrate. |
| `parse-that-regex` | `parse-that` (with `parse-that-regex` as its regex sub-crate per Lock 11 amendment) | Fresh write of regex HIR / NFA / DFA / VM. Mine current `regex` family approach as reference. |
| `bbnf-simd` | `bbnf-simd` | Scanner and byte-primitive dependency surface; V1 keeps the per-target primitive boundary and Lock 16 admissibility rules. |
| `bbnf-bench` | `bbnf-bench` | Skinny is a subset; V1 grows to cover the full SOTA matrix. |
| `test-fixtures` | `test-fixtures` | Skinny is JSON-only; V1 grows to nine grammars. |

Per the user's `kiss-perf-bias` rule, the smallest set of changes that achieves the goal is preferred. The skinny now uses `crates/bbnf-simd/`, mines `crates/csp-solver/` ABI shapes only where relevant for V1 graduation, and keeps `crates/ir/`'s ID-arena scaffolding. Everything else is fresh because the V1 architecture's IR boundary (Lock 5) and tape substrate (Lock 1) are mechanism-incompatible with the current `crates/core/` walker pattern; mining those would import the failure modes.

### 8.1 Mechanical Closure Of Skinny Deviations

| Deviation | Skinny shape | V1 closure | Estimated closure cost |
|---|---|---|---:|
| HM hierarchy inversion | `layout/types::algorithm_w` is called as the skinny top-level type pass. | V1 wraps it as a subroutine under `passes::layout`; DK13/GADT/CSP siblings consume the same facts. | 150-300 LOC wrapper; no Algorithm-W rewrite. |
| Tape `Box<[T]>` -> private-`Vec` sealing inversion | Finished `Tape<'input>` owns a private `Vec<TapeToken>` and exposes immutable token slices; allocated capacity is bench-reported (REDRESS §15; SUBSTRATE.md §1.2). The skinny inverts the SK-V1-spec'd `Box<[T]>` seal because the parse-boundary shrink/copy was a measurable cost. | V1 retains the read-side `Tape`/`ValueRef` contract and moves mutable reuse into an upstream `TapeBuilder` / snapshot path (tranche I). The skinny's private-`Vec` semantic sealing remains canonical at V1 graduation. | 200-400 LOC additive builder/snapshot work; no consumer rewrite. |
| Host-fn-free JSON | Main JSON grammar emits direct string/number span handling. | Add `@host fn` decode-string route and registry dispatch alongside the direct helper; BENCH.md host-call probes bound throughput. V1 graduation must retain lazy string decode — the eager-decode probe (REDRESS §19) MASKs on all three corpora, so a parse-time `decode_json_string_to_arena` route would break the SOTA closure. | 150-250 LOC. |
| `parse-that-regex` directory promotion | `hir/`, `nfa/`, `dfa/`, `vm/`, `literal/` are top-level siblings. | V1 inherits the shape and adds `unicode/` / `prefilter/` siblings. | 0-100 LOC movement. |
| HM-only `passes` constraint | DK13/GADT/CSP absent. | Add passes under `layout/` and keep `layout/types::algorithm_w` intact. | 1,500-3,000 LOC additive. |
| `wasm = false` metadata | Rust backend only. | V2 flips/adds backend metadata when `WasmBackend: Backend` exists. | 50-100 LOC schema extension. |
| Lazy-offset tape route (MEASURED-CANONICAL-FOR-JSON) | Skinny shape: lazy offset tape with sparse flags, direct spare-capacity offset writes, zero JSON payload writes, and typed projection over the same structural storage. REDRESS §16-§18 and §25 reject pair-token fusion, function-pointer dispatch, 12-byte/width churn, structural-index typed parser prepass, NEON no-escape matcher, separator elision, and generic SWAR whitespace. | V1 closure: retain the measured lazy-offset path for JSON-class grammars and keep eager/chunked variants only when a grammar's recovery/layout/host materialization needs them. Lock 1 remains unchanged because the structural projection is still the tape. | 100-300 LOC carry-forward hardening; future alternates require before/after bench proof. |

## 9. Build-Time Targets

Per-profile build-time targets on M1 Pro (clean):

| Profile | Target | Rationale |
|---|---:|---|
| `dev` (incremental, post-touch on one crate) | ≤ 5 s | Iteration speed for `cargo test -p passes`. |
| `dev` (clean, all crates) | ≤ 30 s | First-time onboarding cost. |
| `release` (clean, all crates) | ≤ 90 s | The bench-prep iteration ceiling. |
| `bench` (clean, `bbnf-bench` only) | ≤ 60 s | SOTA-beat iteration ceiling. |
| `bench` + samply capture (one corpus run) | ≤ 60 s wall | Combined: prep + capture. |

The 90-second clean-release target is the binding ceiling. Three discipline rules support it:

1. **No proc-macro codegen** (Lock 6): generated parsers are committed Rust source, not proc-macro expansion. The skinny's `codegen` emits `runtime/src/grammars/json/generated.rs` once per `xtask regen-json`; the rust compiler reads it as ordinary source.
2. **`codegen-units = 16` for dev**: parallel codegen at the cost of marginal release-quality optimization that doesn't matter in dev.
3. **`thin` LTO not `fat` LTO** for both `release` and `bench`: the skinny does not run a fat-LTO comparison study; if the parity rows fail under thin LTO, the V1 plan owns the fat-LTO escalation.

The dev-iteration loop is:

```text
edit -> cargo check -> cargo test -p <crate> -> cargo bench -p bbnf-bench --bench json_parity -- --quick
```

Each step is sub-30s on M1 Pro after the clean build. A loop iteration (edit + check + test + quick-bench) is sub-2-minutes; a SOTA-beat sample (edit + bench + samply) is sub-5-minutes. This is the iteration cadence the 2-4-week skinny window assumes.

## 10. What The Skinny Omits

The skinny explicitly omits the following V1 mechanisms. Each omission's impact on the SOTA-viability test is recorded so the prior-validation update can weigh whether the JSON measurement actually carries to the full V1 SOTA-beat probability.

| Omitted mechanism | Skinny scope reason | Impact on SOTA-viability test |
|---|---|---|
| Per-grammar declaration crates (Lock 14 escape valve) | Main JSON grammar has no `@host fn`; declaration crates are V1's rare-exception form. | Low JSON impact only if BOTH BENCH.md host-call probes pass (per BENCH.md §7.8.1): dispatch overhead ≤50ns AND eager-decode bands within their target envelope. The split adopted in REDRESS §19 ratifies the two-probe shape. |
| LSP / DAP / incremental parse | Editor surfaces don't influence parse throughput. | Zero impact on SOTA-beat. |
| GADT / DK13 / OutsideIn / CSP type-system | JSON's grammar is monomorphic; HM-only suffices. Carried into V1 at tranche D (receiver: tranche D type-system body per INDEX cross-references). | Risk: V1 grammars (CSS L4, Sheets) carry generics + GADTs; the JSON SOTA-beat number does not validate that the type system layer adds zero perf cost. The BENCH agent must mark the JSON number as a *necessary but insufficient* SOTA-viability signal. |
| Cost-model + e-graph + CSP optimization graph | Skinny pre-selects optimization choices for JSON. | Risk: V1 optimization mining might shift parse plans away from skinny's hand-tuned baseline. Mitigation: BENCH.md's alternate-plan probes bound whether the canonical plan is hiding a missing cost-model win; a probe win routes to H.W2/H.W3 instead of being called free. |
| Pratt auto-detection | JSON has no operator precedence. | Zero impact on JSON; risk for CSS / math grammars is V1-territory. |
| SIMD auto-detection | Skinny pre-wires SIMD for JSON structural. | Zero impact: the V1 auto-detector would also choose SIMD for JSON. |
| WASM / TS backends | V2 territory per Lock 5 amendment. | Zero impact on V1 SOTA-beat (V1 is Rust-line only per `restart/MASTER-PLAN.md:140-143`). |
| Path / select macros | Bench harness reads `Document` views directly. | Zero impact on parse-throughput SOTA gate; visitor/access throughput is a different gate. |
| Host fns + chains | Main JSON grammar has none. | JSON-FAITHFUL only after BOTH host-call probes pass: dispatch ≤50ns AND eager-decode bands per BENCH §7.8.1. REDRESS §19: eager-decode currently exceeds expected bands (57.6%/77.2%/81.9% of Track 1 ns for twitter/citm/canada) — the host-fn-free cut is FAITHFUL only for a V1 path that keeps string decode lazy. CSS / Sheets carry host calls and the V1 must measure their cost separately. |
| Direct decoded-string sink delivery | Skinny now passes raw string spans plus decode flags through generated `JsonSink::*_source` hooks. | The source-hook seam is JSON-FAITHFUL and grammar-general; the attempted generic no-allocation decoded visitor, exact decoded-stats sink, and quote-source streaming hasher are not. V1 closure must land a measured field-layout decode+sink materializer for grammars that need decoded direct fields, preserving lazy retained views and avoiding parser-side eager decode or sink-local decoded hash helpers. |
| Recovery / `@error` directives | Skinny tests on valid + minimally malformed corpus only. | Zero impact on twitter / citm / canada SOTA rows; recovery is its own gate (tranche I). |
| Multiple grammars | Historical skinny was JSON-only; SK-V13 makes CSS L4 an active generated row family. | Risk: SIMD-beat for JSON does not imply SIMD-beat for CSS L4. Current mitigation is no longer deferral: CSS L4 rows must admit against lightningcss/cssparser or record architectural-block evidence. |
| CSS prior probe | Historical anti-overfit lever in BENCH.md §9.1 / §11.1. | Historical only. SK-V13 closes non-JSON generality through generated CSS L4 strict parity rows, not an optional substrate-only probe. |
| Lazy-offset tape route | Measured canonical JSON substrate for the historical triad (REDRESS §20-§25). The expanded parse gate still has 5 G rows plus D/E codegen-gap rows, and the full gate is N-direct / NoGo; misses concentrate in event-cursor dispatch, string/Unicode-shaped rows, SinkOnly digest stressor rows, and exact float/string/Unicode materialization inside typed sinks. Canada structural scan is no longer the floor blocker after SK-V5 redress item 56. | Risk: V1 may overgeneralize the triad result to grammars with recovery, layout, eager host materialization, or different token alphabets. Mitigation: keep grammar-specific materialization gates and require before/after bench proof for rejected alternates. |
| `egraph-derive` / proc-macro infrastructure | Not invoked in skinny. | Zero impact. |
| Workspace metadata cross-grammar coherence | One grammar entry only. | Zero impact for JSON. |
| Generated LOC budget enforcement at scale | One generated tree (`json/`); `xtask lint-loc` gates ≤4,000 JSON generated LOC and ≤3,300 `bbnf-bench` aggregate LOC (Track 2 gated by substrate-API correspondence per BENCH.md §10.6, not by an LOC cap; measured at ~318 LOC). | Risk: V1's nine-grammar generated-LOC ceiling (172,125 LOC per `PASS-2.md:435`) is not exercised. The skinny prevents local JSON bloat but still routes nine-grammar scale to F.W3. |

The skinny's SOTA-viability claim is: **if the JSON skinny lands within or beats the sonic-rs / simd-json envelope on twitter, citm, canada with the V1 substrate (tape + direct-to-struct), then the V1 architectural premise is validated for JSON-class grammars.** The claim does not extend to CSS, Sheets, or BBNF-self.

## 11. Closure And Open Contradictions

The skinny is buildable in 2-4 weeks at 32,000 handwritten LOC plus ≤ 4,000 generated LOC if and only if:

1. The `passes` HM-only constraint holds (§2.1).
2. The 90-second clean release-build target holds (§9).
3. The xtask runner stays small while adding `lint-loc` (§6).
4. No new crate is added (§1).
5. The shim discipline holds: each inlined shim stays under 500 LOC and migrates mechanically to its V1 crate (§7).
6. BENCH.md's host-call and alternate-plan probes pass, or RESULTS marks the relevant cut MASKING and blocks a full SK-READY verdict.
7. Conditional bench outcomes are non-green; only an unconditional GO authorizes dispatch.
8. The cross-quadrant deviation ledger (INDEX §"Open contradictions" + WORKSPACE §8.1) stays consistent: every skinny deviation appears in both ledgers with the same row, the same V1 closure cost, and the same MECHANICAL / MASKING-CANDIDATE / DEFERRED-MASKING-CANDIDATE classification.
9. The empirical LOC headroom is read as V1-destination-shape headroom, not as scope-wrong evidence: the on-disk skinny prototype remains far under the 32,000 ceiling. `bbnf-bench` is the binding ceiling and now carries a 3,300 LOC cap because the final auditability gates (`S` anchor rendering, subprocess RSS, persisted SIMD parity metadata, conformance hooks, direct-to-struct workload rows) are BENCH-owned, not optional reporting flourishes. Every other crate carries V1-destination headroom.
10. CSS prior-probe deferral no longer closes anything. SK-V13 discharges the
    non-JSON risk through generated CSS L4 strict parity rows or architectural-
    block evidence; JSON SOTA-beat numbers still do not extend to non-JSON
    grammars without that measurement.

Open contradictions flagged for the synthesis pass:

- **`passes` budget vs scope.** The 6,000 LOC budget assumes HM-only, observational-shapes-only, hand-curated-recognizers. If the SOTA-beat measurement requires e-graph rewrites or CSP narrowing in `passes`, the skinny scope is wrong. **No surgery; flag as the binding constraint.**
- **`parse-that-regex` directory layout.** The natural `regex/{hir,nfa,dfa,vm}` nesting violates Lock 13's 4-10 immediate-children rule at the `src/` level. The skinny resolves by promoting to top-level siblings (§4.7); the V1 inherits the same shape.
- **`workspace.metadata.bbnf.grammars.json.codegen.wasm = false`** is the only metadata field the V1 schema validator might reject because Lock 5 amendment says `wasm = true` is invalid in V1. `wasm = false` is fine; the skinny is consistent with V1 semantics.
- **Bench competitor crates** (`sonic-rs`, `simd-json`) are dev-dependencies of `bbnf-bench`. They are not workspace.dependencies. Owned by BENCH.md.

The skinny exists to validate; if it falsifies, the V1 plan adjusts. If it validates, tranche A.W0 begins with the skinny crates already in place and `cargo build --workspace --release` already passing on the V1 superset.
