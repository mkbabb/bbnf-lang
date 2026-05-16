# SK-V7 Research A1 — Comparator-Plane Repair Gate

Date: 2026-05-16
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Scope: read-only investigation. No tracked file modified. Output written only to `/tmp/skv7-A1-comparator-repair.md`.

## 0. Executive verdict

The current sonic-rs row in every retained, direct-to-struct, and real-typed-struct table of `skinny/RESULTS.md` is built against a **non-default, opt-in `utf8_lossy` configuration** of the upstream crate. With that feature compiled in, `sonic_rs::from_slice` and `sonic_rs::from_slice::<T>` silently fall back to a `String::from_utf8_lossy`-padded parse on any input containing invalid UTF-8 (`sonic-rs-0.5.8/src/serde/de.rs:381-385`, `1280-1283`). Therefore every `sonic-rs` Mbps figure in the live `RESULTS.md` table is measured against a *permissive flaw probe*, not against a strict-vs-strict competitor. The verdict line `Sidecar strictness metadata: sonic-rs/simd-json/serde_json rows are strict / scan-boundary / yes` at `skinny/RESULTS.md:224` is false as written; the same hard-coded prose lives at `skinny/crates/bbnf-bench/src/bin/gate.rs:218`. Until the dependency is rebuilt without `utf8_lossy` and the bench is rerun, the SK-V6 `N-direct / NoGo` close at `skinny/RESULTS.md:221` cannot be interpreted as a strict SOTA-beat failure — half of its anchors are not strict.

The single most critical action item is: **delete the `utf8_lossy` feature from `skinny/crates/bbnf-bench/Cargo.toml:21` and rerun the full 17-corpus matrix before any other SK-V7 wave executes.** No other comparator change can produce honest numbers ahead of this repair.

## 1. Verification of the `utf8_lossy` claim

### 1.1 Cargo entry

`skinny/crates/bbnf-bench/Cargo.toml:21` reads:

```toml
sonic-rs = { version = "=0.5.8", default-features = false, features = ["sort_keys", "utf8_lossy"] }
```

Two facts follow directly: `default-features = false` switches off the empty-by-default feature set (`sonic-rs-0.5.8/Cargo.toml:47` declares `default = []`, so the disable is cosmetic), and `features = ["sort_keys", "utf8_lossy"]` actively turns on the lossy path. The workspace `skinny/Cargo.toml` does not re-declare sonic-rs at the workspace level (lines 23-48 list `serde`, `serde_json`, `criterion`, `mimalloc`, etc., but no sonic-rs), so the bench-crate Cargo.toml is the only declaration in the tree.

### 1.2 Upstream feature semantics

`/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.8/Cargo.toml:44-51` declares the full feature set:

```toml
[features]
arbitrary_precision = []
avx512 = ["sonic-simd/avx512"]
default = []
non_trailing_zero = []
sanitize = []
sort_keys = []
utf8_lossy = []
```

The upstream README directly contradicts the lossy build: "By default, sonic-rs enable the UTF-8 validation, except for `xx_unchecked` APIs" (`https://github.com/cloudwego/sonic-rs`). Strict UTF-8 rejection is the documented default; the skinny harness opts out of it.

### 1.3 Where the feature applies

`sonic-rs-0.5.8/src/serde/de.rs:83-86` exposes the runtime toggle:

```rust
pub fn utf8_lossy(mut self) -> Self {
    self.parser.cfg.utf8_lossy = true;
    self
}
```

The bench does not call this method directly, but `from_trait` — the workhorse that backs both `from_slice` and `from_slice::<T>` — auto-enables it under `cfg(feature = "utf8_lossy")` at `de.rs:1280-1283`:

```rust
#[cfg(feature = "utf8_lossy")]
{
    de = de.utf8_lossy();
}
```

When the flag is set, the Value path in `de.rs:379-389` substitutes `String::from_utf8_lossy(json)` into the parse buffer before invoking `parse_with_padding`, materializing `U+FFFD` for every invalid sequence rather than returning an `InvalidUtf8` error. The terminal validation at `de.rs:1291` (`check_utf8_final`) is also short-circuited because the lossy preprocess replaces the offending bytes before that check ever runs.

### 1.4 Feature-graph resolution

`cargo tree -p bbnf-bench --edges=features` under `skinny/` produces (excerpt):

```
├── sonic-rs feature "sort_keys"
│   └── sonic-rs v0.5.8
│       ├── sonic-number feature "default"
│       │   └── sonic-number v0.1.2
│       ├── sonic-simd feature "default"
│       │   └── sonic-simd v0.1.4
├── sonic-rs feature "utf8_lossy"
│   └── sonic-rs v0.5.8 (*)
```

So `utf8_lossy` is resolved live, not pruned by any conditional. Every `sonic_rs::from_slice` invocation in the bench is the lossy-mode binary.

### 1.5 Call-site audit

The lossy build affects three distinct row families:

1. **Value DOM row** — `benches/json_parity.rs:89` and `:106` both call `sonic_rs::from_slice::<sonic_rs::Value>(black_box(&fixture.bytes)).unwrap()`. These produce the `sonic_rs_anchor` and `sonic_rs_checked` rows in `RESULTS.md` (columns "sonic-rs Mbps" at lines 5-21). Note that `sonic_rs_anchor` and `sonic_rs_checked` currently use *identical* code — the only thing that should have differentiated them is strictness, but with `utf8_lossy` enabled there is no checked vs unchecked distinction at the Value level.
2. **Typed-struct row** — `src/real_typed_struct.rs:151,154` call `sonic_rs::from_slice::<TwitterSearch<'a>>(bytes)` and `sonic_rs::from_slice::<UpdateCenter<'a>>(bytes)`. The target structs are `Cow<'a, str>`-borrowed (`real_typed_struct.rs:26,32,36,40,46-60`), so the lossy substitution either borrows directly from the input slice (when no invalid bytes are present) or silently allocates a `Cow::Owned` U+FFFD-substituted copy.
3. **Direct digest row** — `src/direct_struct.rs:417` calls `sonic_rs::from_slice(bytes)` for the `sonic_rs_direct_to_struct` workload (`RESULTS.md` workload column "sonic-rs Mbps" at lines 27-45).

There are no `sonic_rs::from_slice_unchecked`, `LazyValue`, or `get_unchecked` call sites. Every measured sonic row is therefore equally contaminated by the feature flag.

### 1.6 Default status

`utf8_lossy` is **not** the upstream default. It is explicitly opted in by the bench Cargo.toml. The fix is purely subtractive: remove the feature string and rebuild. No call-site signature change is needed because none of the existing sites depend on a lossy-only API — they all use the standard `from_slice` symbol.

## 2. Repair path

### 2.1 Cargo edit

The exact one-line diff against `skinny/crates/bbnf-bench/Cargo.toml:21`:

```diff
-sonic-rs = { version = "=0.5.8", default-features = false, features = ["sort_keys", "utf8_lossy"] }
+sonic-rs = { version = "=0.5.8", default-features = false, features = ["sort_keys"] }
```

That is the entire mechanical Cargo change. `default-features = false` is retained because the upstream `default = []` set is empty and the explicit form documents intent. `sort_keys` is retained because it is required by the bench parity oracle (deterministic key iteration in `sonic_rs::Value`), and it has no strictness implication.

### 2.2 Call-site survey

A grep across `skinny/crates/bbnf-bench/{src,benches}` for `sonic_rs::` returns nine call sites (Bench 1.5 above plus `src/bin/gate.rs:642` which re-parses for the parity oracle). None of them rely on lossy-only behaviour. None of them use `from_slice_unchecked`, `LazyValue`, `get_unchecked`, `to_array_iter_unchecked`, or `to_object_iter_unchecked`. None of them call `.utf8_lossy()` on a `Deserializer` directly. The strict rebuild is therefore zero-LOC at the call sites — only the dependency declaration changes.

### 2.3 Build and rerun

After the Cargo edit:

1. `cargo clean -p sonic-rs` to force a strict re-resolution of the dependency.
2. `cargo build --profile bench -p bbnf-bench` to confirm strict-build success.
3. `cargo bench -p bbnf-bench --bench json_parity` for the 17-corpus matrix.
4. `cargo run -p bbnf-bench --bin gate --release` to regenerate `RESULTS.md`.

### 2.4 Test surface

No `#[test]` in `skinny/crates/bbnf-bench` exercises sonic-rs directly. The parity oracle in `src/bin/gate.rs:641-642` reads sonic-rs Value as a *reference oracle* for shape equality against Track 1/Track 2; with strict sonic-rs this oracle will now reject any corpus that contains invalid UTF-8. The corpus check at `skinny/crates/test-fixtures/corpus/` should be reviewed for any fixture whose bytes include invalid UTF-8 — none of the standard JSONTestSuite-derived fixtures (twitter, citm_catalog, canada, etc.) contain invalid UTF-8, but the `unicode_mixed`, `unicode_escapes`, `unicode_basic`, and `y_string_unicode` fixtures should be re-validated. If any one of them fails strict sonic-rs parse, the bench either drops that fixture or routes it through an explicit `flaw_probe` lane.

### 2.5 Estimated LOC and complexity

- Cargo diff: **1 line**.
- Call-site edits: **0 lines**.
- Bench-harness wiring: **0 lines** unless the harness wants to distinguish `sonic_rs_anchor` from `sonic_rs_checked` as separate strictness rows (currently they share an identical body — see §1.5(1)).
- Corpus validation: ~30 minutes of fixture re-check; no code change unless a corpus must move to a `flaw_probe` lane.
- Re-run time: one full 17-corpus criterion sweep, ~45 minutes on the M5 Max.

Total: **trivial mechanical change**, dominated by re-run time. The complexity is in the gate logic (§4 below) that consumes the rerun, not in the dependency edit.

## 3. sonic-rs API surface at strict-vs-strict

Once the feature is removed, sonic-rs exposes the following parse APIs that are admissible at strict-vs-strict against bbnf Track 1 (retained tape + typed root) and Track 2 (hand-coded oracle):

| Symbol | Plane | Strictness (without `utf8_lossy`) | Admissible vs Track 1 | Admissible vs Track 2 |
|---|---|---|---|---|
| `sonic_rs::from_slice::<sonic_rs::Value>` | `dom_value` | `strict_bytes` | yes, against `retained_tape_typed_root` if Track 1 row records `strict_after_utf8_view` and the harness charges a `prevalidation_charged=true` flag, or against any Track 1 byte-entry row | yes, same conditions |
| `sonic_rs::from_slice::<T>` | `typed_serde_direct` | `strict_bytes`, target-dependent ownership | yes, against `generated_typed_directbuild` rows in `real_typed_struct` | yes |
| `sonic_rs::from_str::<sonic_rs::Value>` | `dom_value` | `strict_after_utf8_view` (caller pre-validated) | yes, against Track 1 `&str` retained rows; this is the *closest plane match* | yes |
| `sonic_rs::from_str::<T>` | `typed_serde_direct` | `strict_after_utf8_view` | yes, against generated typed direct rows that take `&str` | yes |
| `sonic_rs::LazyValue` + `get` | `ondemand_cursor`-like | `strict_after_utf8_view` if backed by `&str`, `strict_bytes` if backed by `&[u8]`; per-leaf validation only | **not** admissible as a strict S anchor for retained DOM/tape because skipped values are never parsed; admissible only against a matching BBNF lazy-path workload | same caveat |
| `sonic_rs::from_slice_unchecked::<T>` | `typed_serde_direct` | `permissive` (skips UTF-8 validation by API contract per upstream README) | **not** admissible; flaw-probe only | same |
| `sonic_rs::get_unchecked` | path-access lazy | `permissive` | **not** admissible; flaw-probe only | same |

The current bench uses only `from_slice` and `from_slice::<T>`. After the feature removal, both are `strict_bytes`. The harness should consider also benching `from_str::<sonic_rs::Value>` as a parallel `strict_after_utf8_view` row to give Track 1 retained (which is currently `&str`-entry; see §3 of `skv6-A3-comparator-planes.md`) a same-plane competitor on the exact same UTF-8 validation contract.

## 4. Strictness planes across all five comparators

The post-repair plane table for the SK-V7 cohort:

| Comparator | API | Plane | Default strictness | Currently in skinny | Currently used? |
|---|---|---|---|---|---|
| **sonic-rs** | `from_slice::<Value>` | `dom_value` | strict by default; lossy only when `utf8_lossy` feature is opted in | feature `utf8_lossy` *opted in* — currently lossy | yes (the contaminated S anchor) |
| **sonic-rs** | `from_slice_unchecked` | `dom_value` | permissive (skips UTF-8 validation per API contract) | not built | no |
| **sonic-rs** | `LazyValue` / `get` / `get_unchecked` | `ondemand_cursor`-like | per-leaf strict (without `_unchecked`) or permissive (with `_unchecked`) | not built | no |
| **simdjson C++ DOM** | `dom::parser::parse` | `dom_value` | strict by default (full UTF-8 + escape + number validation) | profile-only at `skinny/profile/simdjson-expanded/` | no — stale sidecar |
| **simdjson C++ On-Demand** | `ondemand::parser::iterate` | `ondemand_cursor` | `partial_ondemand` unless workload forces full traversal | not present | no |
| **yyjson** | `yyjson_read` / `yyjson_read_opts` (default flags) | `dom_value` | strict RFC-8259 by default; flags exist for comments, trailing commas, invalid Unicode, in-situ mutation | profile-only at `skinny/profile/yyjson/` | no — stale sidecar |
| **asmjson SWAR** | `parse_to_dom_zmm` / SWAR fallback | `dom_value` or `sax_sink` | **permissive** — accepts `0x00..0x1F` as whitespace, accepts unescaped controls inside strings (`skinny/profile/native-sidecars/asmjson/NOTE.md:7-25`; `restart/skinny/BENCH.md:643-647`) | synthetic AVX-512 profile, no arm64 strict row | no |
| **RapidJSON** | `Document::Parse` (default `kParseDefaultFlags`) | `dom_value` | **permissive by default** — `kParseFullPrecisionFlag` off, no UTF-8 validation flag set by default; strict requires `kParseValidateEncodingFlag` | mentioned only as a future Wave 6 row (`bin/gate.rs:218`) | no |
| **serde_json** | `from_slice::<Value>` / `from_slice::<T>` | `dom_value` floor / `typed_serde_direct` | strict by default | live | yes (floor/control) |
| **simd-json (Rust)** | `to_borrowed_value(&mut bytes)` / `to_owned_value(&mut bytes)` | `dom_value` | `strict_bytes` if probes pass; mutates input in-situ | live | yes |

### 4.1 Row-by-row admissibility for current `RESULTS.md`

Applied to the live `skinny/RESULTS.md:5-21` retained table:

| Row | Current label | True post-strict label | Admissible at strict-vs-strict? |
|---|---|---|---|
| Track 1 retained | `deferred` / `view-boundary` | `strict_after_utf8_view` | yes, against any other `strict_after_utf8_view` or *prevalidation-charged* `strict_bytes` row |
| Track 2 retained | `deferred` / `view-boundary` | `strict_after_utf8_view` | yes, same |
| sonic-rs Mbps | implicitly strict | currently **`lossy_utf8`**; post-repair **`strict_bytes`** | currently **flaw-probe only**; post-repair yes |
| simd-json borrowed | implicitly strict | `strict_bytes` with `input_mutated=true` | yes, after disclosure of in-situ mutation |
| simd-json owned | implicitly strict | `strict_bytes` with `input_mutated=true` (mutates the cloned buffer) | yes, same |
| serde_json | implicit floor | `strict_bytes` | yes, floor/control |

For the workloads table at `RESULTS.md:25-45` (direct_to_struct + real_typed_struct), the same correction propagates: every `sonic-rs Mbps` column entry is currently a lossy-mode measurement. The `Track 1 / sonic` ratios listed there cannot be read as strict-vs-strict.

### 4.2 Native-sidecar staleness

Independent of `utf8_lossy`, the simdjson C++ and yyjson rows are *profile-only* — they live under `skinny/profile/{simdjson-expanded,yyjson}/PROFILE-REPORT.md` and have not been rerun on the exact 17-corpus skinny fixture set on the same machine, same compiler, same commit. `restart/skinny/audit/SK-V6-COHORT/skv6-R5-sidecar-refresh.md` (referenced by C3) records that `/tmp/simdjson-research` and `/tmp/yyjson-research` are absent from the workspace and have not been refreshed. Until those sidecars are rebuilt and rerun, the strict native ceiling can size the SK-V7 SOTA-beat target but cannot classify the live Rust gate. The asmjson SWAR row at `skinny/profile/native-sidecars/asmjson/NOTE.md` is permissive synthetic, not strict 17-corpus, and is not a strict S anchor candidate on Apple Silicon.

## 5. Predicted post-repair gate state

### 5.1 Sonic-rs Mbps delta

The lossy preprocess in `sonic-rs-0.5.8/src/serde/de.rs:379-389` runs `next_invalid_utf8()` only once per parse (a single SIMD UTF-8 scan over the whole input), and only invokes `String::from_utf8_lossy` if that scan reports an invalid offset. For valid-UTF-8 fixtures (twitter, citm_catalog, canada, mesh, marine_ik, gsoc-2018, instruments, numbers, github_events, apache_builds, update_center, random, distinct_values, unicode_basic, unicode_escapes, unicode_mixed, y_string_unicode), the lossy path is just one extra SIMD scan and no allocation. The expected post-strict regression for sonic-rs is therefore in the **3–8% range** for clean inputs, not the 15% suggested by a naive `from_utf8`-of-everything model. The Value parser already runs an end-of-parse `check_utf8_final` call (`de.rs:1291`), so the strict mode mostly adds an early-exit equivalent of that same check rather than introducing a second walk.

For unicode-heavy or actually-invalid corpora the regression is larger because the lossy allocation activates. None of the current 17 corpora is known to contain raw invalid UTF-8; the `y_*`/`i_*` JSONTestSuite invalid-input probes are excluded from the timed parse rows and live only in the conformance lane (`RESULTS.md:5` etc. record `i_string_invalid_utf8 rejected outside hot scan`).

### 5.2 Re-classification table (predicted)

Take a representative line from the current retained table, `twitter` at `RESULTS.md:5`:

- Track 1 = 15597 Mbps
- Track 2 = 12128 Mbps
- sonic-rs = 21184 Mbps (lossy)
- Track 1 / S = 73.6%

Predicted strict sonic-rs at twitter ≈ `21184 × 0.93 ≈ 19700 Mbps`. Track 1 / S then becomes `15597 / 19700 ≈ 79.2%`. Still a NoGo at this corpus, but closer to the slack edge.

Applying the same ~7% mean regression to all 17 retained rows:

| Corpus | Current T1/S | Predicted post-strict T1/S | Predicted post-strict gate |
|---|---:|---:|---|
| twitter | 73.6% | ~79.2% | NoGo |
| citm_catalog | 130.3% | ~140.1% | Go (already Go) |
| canada | 148.3% | ~159.5% | Go (already Go) |
| apache_builds | 78.0% | ~83.9% | NoGo |
| github_events | 68.8% | ~74.0% | NoGo |
| update_center | 59.6% | ~64.1% | NoGo |
| mesh | 121.1% | ~130.2% | Go |
| random | 65.5% | ~70.4% | NoGo |
| gsoc-2018 | 53.6% | ~57.6% | NoGo |
| marine_ik | 136.0% | ~146.2% | Go |
| instruments | 92.0% | ~98.9% | NoGo (within ~10% slack window) |
| numbers | 148.0% | ~159.1% | Go |
| unicode_mixed | 56.1% | ~60.3% | NoGo |
| unicode_escapes | 80.4% | ~86.5% | NoGo |
| unicode_basic | 91.7% | ~98.6% | NoGo (slack edge) |
| distinct_values | 60.2% | ~64.7% | NoGo |
| y_string_unicode | 46.0% | ~49.5% | NoGo |

No row currently below 80% T1/S is expected to cross the 100% threshold from the strict rebuild alone; the 11 NoGo retained rows remain NoGo. The `instruments` and `unicode_basic` rows move close enough to the slack edge that a second small intervention (escape-tail or tiny-string per the SK-V6 candidate shortlist) could push them across. **The strict rebuild does not, by itself, change the overall N-direct / NoGo verdict** — but it makes that verdict defensible, which is the gate condition for Wave 1 onwards.

### 5.3 The honesty payoff

The post-repair value is not throughput motion. It is:

1. The `N-direct / NoGo` label becomes a strict-vs-strict measurement rather than a permissive-flaw-probe artifact.
2. The `Sidecar strictness metadata: ... strict / scan-boundary / yes` line at `RESULTS.md:224` and `gate.rs:218` becomes true.
3. The SK-V6 candidate-intervention shortlist (`IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md:107-129`) becomes addressable: a 15% retained-parse intervention against a strict sonic baseline is meaningful; against a lossy baseline it is not.
4. Wave 1+ of the SK-V7 plan can declare its first close ("comparator-plane is strict-honest") and proceed to substrate work without REDRESS objections about anchor contamination.

## 6. Wave 0 deliverable for SK-V7

### 6.1 Cargo.toml diff (the entire mechanical edit)

```diff
--- a/skinny/crates/bbnf-bench/Cargo.toml
+++ b/skinny/crates/bbnf-bench/Cargo.toml
@@
-sonic-rs = { version = "=0.5.8", default-features = false, features = ["sort_keys", "utf8_lossy"] }
+sonic-rs = { version = "=0.5.8", default-features = false, features = ["sort_keys"] }
```

### 6.2 Bench-harness changes

Two minimal additions to give the harness room to disclose strictness:

1. **Disambiguate `sonic_rs_anchor` from `sonic_rs_checked`.** Currently `benches/json_parity.rs:87-92` and `:104-109` are byte-identical. The strict rebuild already collapses them onto the same `strict_bytes` plane; the harness can either drop one row or rename them to mean different things (e.g., `sonic_rs_value_from_slice` vs `sonic_rs_value_from_str`). Recommendation: drop `sonic_rs_checked` (it adds no information) and add `sonic_rs_value_from_str` so a `strict_after_utf8_view` competitor lands on the same plane as Track 1 retained `&str` entry. LOC: ~12 lines in `benches/json_parity.rs` plus a new `write_competitor_row` invocation.
2. **Replace the hard-coded sidecar strictness prose.** `src/bin/gate.rs:218` and `src/report.rs:141-180` currently render strictness fields from prose constants. These must read from `RowMetadata` (schema v3) so that flipping `utf8_lossy` in the Cargo.toml automatically flips the rendered strictness column. C3 identified this; it remains the open implementation work.

### 6.3 Re-run protocol

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo clean -p sonic-rs
cargo build --profile bench -p bbnf-bench
cargo bench -p bbnf-bench --bench json_parity > /tmp/skv7-w0-rerun.log 2>&1
cargo run -p bbnf-bench --bin gate --release
```

The criterion artifacts at `target/criterion/*/sonic_rs_anchor/metadata.toml` should then be regenerated with the strict feature mask. The `gate` binary regenerates `RESULTS.md` from the criterion JSON.

### 6.4 Schema v3 column inventory for `RESULTS.md`

Per HANDOFF-SK-V6 §2 (`restart/skinny/audit/HANDOFF-SK-V6.md:42-45`) and per BENCH.md §5 metadata schema requirements, every row in `RESULTS.md` must carry these columns. The current `report.rs:141` table has eight content columns plus four ratio columns. The strict-honest v3 minimum is:

- `corpus` — exists
- `outcome` — exists
- `verdict` — exists
- `plane` — new: `retained_tape_typed_root` | `dom_value` | `typed_serde_direct` | `generated_typed_directbuild` | `semantic_full_digest_stressor` | `ondemand_cursor` | `sax_sink` | `structural_scan_only`
- `strictness` — new (replaces the current `Strictness` prose): `strict_bytes` | `strict_after_utf8_view` | `strict_fullwalk_ondemand` | `partial_ondemand` | `lossy_utf8` | `permissive` | `unknown`
- `parse_utf8` — exists (currently `view-boundary` prose; should become `byte_validated_in_parse` | `prevalidated_view` | `lossy_substitution` | `none`)
- `escape_complete` — exists
- `flaw_probe` — exists
- `api_symbol` — new: exact symbol, e.g. `sonic_rs::from_slice::<sonic_rs::Value>`
- `feature_mask` — new: e.g. `sort_keys` (strict sonic) or `sort_keys+utf8_lossy` (lossy sonic)
- `input_ownership` — new: `borrowed_immutable` | `borrowed_mut_in_situ` | `owned_clone_setup` | `native_padded` | `native_doc_owned`
- `output_ownership` — new: `borrows_input` | `owns_doc_arena` | `owns_tape_borrows_input` | `owns_struct` | `cow_mixed` | `sink_only`
- `input_mutated` — new bool
- `clone_charged` — new bool
- `prevalidation_charged` — new bool
- `sidecar_freshness` — new: `current_same_run` | `current_sidecar` | `stale_profile_only` | `published_cross_arch` | `advisory`
- `corpus_hash` — new: blake3 of fixture bytes (already collected at `BenchFacts::bbnf_json_workload` but not emitted)
- `hardware` — new: e.g. `M5 Max macOS arm64 NEON`
- `build_flags` — new: profile name + RUSTFLAGS
- `primitive_status` — new: `checkasm_passing` | `scalar_only` | `n/a`
- `s_anchor_eligible` — new bool
- Mbps / ns/iter / c/B — per row

The "S anchor" and "S Mbps" columns become *derived* — populated only by rows where `s_anchor_eligible=true` and `plane` matches the gated row's plane. The gate logic in `gate.rs:138-201` and `bin/gate.rs:406-416` must filter on these fields before selecting S.

### 6.5 "Comparator-plane is strict-honest" close condition

The Wave 0 close fires when all of the following are true:

1. `sonic-rs` row in `RESULTS.md` reports `strictness=strict_bytes` and `feature_mask=sort_keys` (no `utf8_lossy`).
2. The strict-rebuild bench has been rerun on all 17 corpora and the criterion `metadata.toml` per row shows `schema_version=3`.
3. `gate.rs` rejects any S-anchor candidate row whose `strictness ∈ {lossy_utf8, permissive, unknown}` or whose `s_anchor_eligible=false`, and asserts that no such row was selected.
4. `report.rs` renders strictness fields from `RowMetadata`, not from hard-coded prose. The Notes block in `RESULTS.md` no longer contains the false `sonic-rs/simd-json/serde_json rows are strict / scan-boundary / yes` line.
5. The simdjson C++ DOM and yyjson rows are either rerun on the same machine and admitted as `sidecar_freshness=current_sidecar`, or marked `stale_profile_only` and explicitly excluded from S selection.
6. asmjson SWAR remains `strictness=permissive` / `s_anchor_eligible=false`. RapidJSON, if added in this wave, defaults to `permissive` unless built with `kParseValidateEncodingFlag` and `kParseFullPrecisionFlag`, in which case it is `strict_bytes` and `s_anchor_eligible=true`.

When all six are true, subsequent SK-V7 waves can measure SOTA-beat against the strict-honest comparator plane. Until they are, every retained-parse intervention, every direct typed activation, and every primitive-checkasm wave is operating against an invalid baseline.

## 7. Files cited

- `skinny/Cargo.toml`
- `skinny/crates/bbnf-bench/Cargo.toml`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/audit/HANDOFF-SK-V6.md`
- `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V6-SOTA-RECOVERY.md`
- `restart/skinny/audit/SK-V6-COHORT/skv6-C3-sidecar-planes.md`
- `restart/skinny/audit/SK-V6-COHORT/skv6-A3-comparator-planes.md`
- `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.8/Cargo.toml`
- `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.8/src/serde/de.rs`
- Upstream: `https://github.com/cloudwego/sonic-rs` README ("By default, sonic-rs enable the UTF-8 validation, except for `xx_unchecked` APIs.")

## 8. Single most critical action item

**Delete the literal string `, "utf8_lossy"` from `skinny/crates/bbnf-bench/Cargo.toml:21`, then rerun `cargo bench -p bbnf-bench --bench json_parity` followed by `cargo run -p bbnf-bench --bin gate --release`.** Every other SK-V7 wave decision depends on the resulting `RESULTS.md` being a strict-vs-strict measurement. No call-site edit is required; no API migration is required; the change is one Cargo.toml token and one full bench rerun. Until it lands, the SK-V6 N-direct / NoGo close is a permissive-baseline artifact and SK-V7 cannot honestly claim SOTA-beat motion.
