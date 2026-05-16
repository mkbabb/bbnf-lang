# SK-V6 Wave 1c R6c — i-cache / branch / assembly shape after Candidate 4

Date: 2026-05-14
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Scope: research only; no repository files edited.
Hard cap: 30 min.

## Inputs Read

- `skinny/REDRESS.md` item 63: Candidate 4 (`ContainerNext` / next-byte carry) admitted.
- `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md` §E / Lock 15 cluster and §8 Candidate 4.
- `skinny/crates/runtime/src/grammars/json/generated.rs` and `skinny/crates/codegen/src/json_templates/generated.rs` post-Candidate4 shape.
- `skinny/RESULTS.md` current requested-row retained and direct rows.

## Commands / Artifacts

- Built retained parser profile binary with attribution:
  `CARGO_TARGET_DIR=/tmp/skv6-cargo/R6c cargo build --release -p xtask --bin profile-lazy --features runtime/parse-attribution`
- Built default fused retained parser binary:
  `CARGO_TARGET_DIR=/tmp/skv6-cargo/R6c-default cargo build --release -p xtask --bin profile-lazy`
- Static size and branch proxies:
  `size`, `nm -nm | rustfilt`, `otool -tvV`.
- Stack sampling profiles:
  `/tmp/skv6-R6c-profiles/{update_center,distinct_values,unicode_basic,y_string_unicode}.profile.json.gz`
  plus `.syms.json` sidecars.
- PMU availability check:
  `perf` unavailable; `xctrace list templates` failed because only CommandLineTools are active, not full Xcode. No branch-mispredict / L1i / IPC counters were accessible without privilege/toolchain setup, so this report uses static branch density plus samply stack sampling.

## Current Row State

From `skinny/RESULTS.md` after Candidate 4:

| row | retained Track 1 Mbps | S anchor Mbps | Track 1 / S | outcome |
|---|---:|---:|---:|---|
| update_center | 9259 | 19242 | 48.1% | G |
| distinct_values | 6144 | 17728 | 34.7% | G |
| unicode_basic | 11092 | 15802 | 70.2% | G |
| y_string_unicode | 6272 | 13644 | 46.0% | G |

Local default-binary smoke for the same rows:

| row | profile-lazy Mbps | notes |
|---|---:|---|
| update_center | 8719 | `test_data/update-center.json`, 5000 iters |
| distinct_values | 5747 | `test_data/distinct_values.json`, 5000 iters |
| unicode_basic | 10587 | `test_data/unicode_basic.json`, 5000 iters |
| y_string_unicode | 5933 | `test_data/y_string_unicode.json`, 5000 iters |

The local smoke is lower than the advisory rows because it is a one-shot subprocess run without Criterion scheduling. It is useful only to confirm the current binary path and profile artifacts, not to replace `RESULTS.md`.

## Static Code Size

Default fused retained parser (`/tmp/skv6-cargo/R6c-default/release/profile-lazy`):

| symbol | size | instruction count | static branches | calls | verdict |
|---|---:|---:|---:|---:|---|
| `runtime::generated_json::generated::dispatch_value` | 9412 B | 2353 | 461 | 28 | below Lock 15 |
| `runtime::generated_json::scan::structural_capacity_for` | 3108 B | 777 | 95 | 14 | below Lock 15 |

The hot retained parse body is still under the 20 KiB Lock 15 cap after Candidate 4. Even counting `dispatch_value` plus `structural_capacity_for`, the visible generated JSON hot text is about 12.5 KiB. This reconfirms the earlier SK-V6 §E conclusion: an i-cache-capacity split is not a justified Wave 2 candidate.

Parse-attribution binary (`/tmp/skv6-cargo/R6c/release/profile-lazy`) splits the same path into small leaves:

| symbol | size | instructions | branches | branch / insn | calls |
|---|---:|---:|---:|---:|---:|
| `dispatch_value` | 192 B | 48 | 17 | 35.4% | 1 |
| `parse_array` | 296 B | 74 | 6 | 8.1% | 7 |
| `parse_string` | 268 B | 67 | 6 | 9.0% | 5 |
| `match_tiny_plain_string` | 80 B | 20 | 4 | 20.0% | 0 |
| `match_string_at_quote` | 1976 B | 494 | 66 | 13.4% | 0 |
| `consume_array_next` | 588 B | 147 | 34 | 23.1% | 2 |
| `consume_container_next` | 556 B | 139 | 32 | 23.0% | 1 |
| `parse_key_colon` | 712 B | 178 | 35 | 19.7% | 5 |

The branch density is real, but it is concentrated in compact control leaves and string scanning, not in an oversized fused function.

## Stack-Sampling Attribution On Requested Rows

Samply profiles were recorded with `runtime/parse-attribution` so leaves did not fuse. Self-sample shares:

| row | top self-time symbols |
|---|---|
| update_center | `match_tiny_plain_string` 39.3%, `match_string_at_quote` 24.9%, `emit_plain_offset` 5.9%, `parse_key_colon` 5.3%, `consume_quote_at_cursor` 4.9%, `consume_container_next` 4.6%, `consume_array_next` 1.9% |
| distinct_values | `match_tiny_plain_string` 45.7%, `match_string_at_quote` 20.0%, `consume_container_next` 7.2%, `consume_quote_at_cursor` 6.9%, `parse_key_colon` 6.7%, `emit_plain_offset` 5.7% |
| unicode_basic | `match_tiny_plain_string` 29.8%, `match_string_at_quote` 24.8%, `consume_quote_at_cursor` 7.5%, `consume_container_next` 6.1%, `emit_plain_offset` 6.0%, `parse_key_colon` 4.8%, `consume_array_next` 4.0% |
| y_string_unicode | `match_string_at_quote` 63.3%, `consume_array_next` 7.9%, `match_tiny_plain_string` 7.3%, `consume_quote_at_cursor` 4.6%, `patch_flags` 4.1%, `emit_plain_offset` 3.5% |

Inclusive samples also keep the interpretation stable:

- `update_center`: `parse_string` 51.6%, `parse_pair` / `parse_key_colon` about 30%, `parse_array` 29.7%.
- `distinct_values`: `parse_string` 44.9%, `parse_pair` / `parse_key_colon` about 42%, `parse_array` 99.9% because the root is an array.
- `unicode_basic`: `parse_string` 55.8%, `parse_pair` / `parse_key_colon` about 16-17%, `parse_array` 99.7%.
- `y_string_unicode`: `parse_string` 88.7%, `parse_array` 98.2%.

Candidate 4 successfully moved some array-control cost into `consume_array_next`, but the requested rows are now dominated by the existing scalar string leaves. The result does not support a broad code-layout intervention.

## Candidate Decision

### i-cache split

Verdict: reject as Wave 2 candidate.

Reason: the default fused generated retained parser is 9412 B; with `structural_capacity_for` it is still about 12.5 KiB. The Lock 15 cap is 20 KiB. Samply shows self-time in string leaves, not a scattered whole-function front-end stall signature. Splitting the parser would be generic code layout churn without a same-row falsification gate.

### `#[cold]` error outlining

Verdict: reject as Wave 2 candidate.

Reason: `generated.rs` already has `#[cold] #[inline(never)] fn error(...)`; `nm` places `runtime::generated_json::generated::error` as a 36 B cold symbol far from the hot generated cluster. The row profiles do not show error construction in hot self-time. Additional cold outlining would be speculative and unlikely to move the requested rows.

### monomorphization change

Verdict: reject as Wave 2 candidate.

Reason: there is no evidence of retained parser code bloat from excessive monomorphization. The default binary has a single fused generated retained hot symbol, not many row-specific copies, and the fused size is under budget. Monomorphization policy changes would not target the measured row leaves.

## R6c Recommendation

Do not dispatch a code-size / i-cache / cold-outlining / monomorphization Wave 2 intervention.

The legitimate next retained-parse candidate must come from the string cluster named by R1c/R2c, not from front-end capacity. If a branch-oriented candidate is considered, it must target the measured string leaves (`match_tiny_plain_string` / `match_string_at_quote`) with a row-specific falsification gate on `update_center`, `distinct_values`, `unicode_basic`, and `y_string_unicode`. It must not re-open the blocked SK-V4/SK-V5 routes: NEON `match_tiny_plain_string` wiring, always-wide string scanner, delayed-wide trusted scan, UTF-8 fusion, or any sidecar source pass.

## Bottom Line

R6c reconfirms SK-V6 §E after Candidate 4: Lock 15 holds. The remaining requested-row deficit is string-path execution, not an i-cache-capacity failure. No R6c-owned Wave 2 intervention is admissible.
