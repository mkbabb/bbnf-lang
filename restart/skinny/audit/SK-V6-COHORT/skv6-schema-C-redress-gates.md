# SK-V6 schema-source redress agent C

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Artifact date: 2026-05-15
Source notes inspected are dated 2026-05-14.
Committed context: `10abb7b0 docs(sk-v6-wave3-redress): reject hand-authored real typed struct sink as DirectBuild proof`

## Readback

- `skinny/RESULTS.md:25-43` is the binding current direct row table.
- `skinny/REDRESS.md:1672-1926` rejects items 66-70: source-hook folding, parser-owned decoded scratch, byte-output `unescape_json_string`, semantic string facts for the digest stressor, and the hand-authored JSON typed sink as DirectBuild proof.
- `restart/skinny/BENCH.md:772` splits `semantic_full_digest_stressor` from `real_typed_struct`; `BENCH.md:883-884` defines the required rows; `BENCH.md:1272` says the next admissible row must first name the host/API schema source feeding `DirectBuild` field facts without adding a grammar directive.
- `restart/MASTER-PLAN.md:138` adds the schema-source rule and the scout threshold: Track 1 within `sonic-rs * 1.10` on one generated typed fixture and no worse than `sonic-rs * 1.25` on the other before folding into the full gate.
- Note: `restart/skinny/BENCH.md:772` and `MASTER-PLAN.md:516` still say `mesh` is one of four passing digest stressor rows, but the current `skinny/RESULTS.md:33` row is below the `sonic/1.10` Mbps floor and is marked `NO-GO`. Treat `skinny/RESULTS.md` as binding until a rerun resolves the prose/table mismatch.

## Next Falsifiable Candidate

Candidate name: `SK-V6 Wave 3 Candidate 12: schema-source DirectBuild`.

The candidate is not another string materializer. It first introduces a host/API output schema source for the two real typed fixtures and lowers that schema into the existing `DirectBuild { shape, fields }` payload. The schema source supplies field rosters, optional/null policy, repeated fields, map fields, borrowed/owned representation policy, and exact string/number materializers. It is not a BBNF grammar directive, not a new top-level BIR variant, not a retained side table, and not a benchmark-private parser.

Track 1 must call generated `SinkOnly` / `parse_direct` and consume generated field facts to produce owned Rust output. A hand-authored typed sink may exist only as a profiler-only `before` row; it cannot be cited as DirectBuild proof. Track 2 remains structurally independent and returns the same owned Rust type. Sonic-rs and serde_json parse into the exact same serde-derived types. Checksums are computed only after parse.

## Exact Real Typed Rows

Expected Mbps thresholds below are derived from REDRESS item 70 Candidate 11 measured same-host anchors. The same-run anchor supersedes these numbers if it moves.

| row | output schema | C11 sonic-rs typed Mbps | C11 serde_json typed Mbps | strict floor `sonic/1.10` | scout floor `sonic/1.25` | candidate scout requirement |
|---|---|---:|---:|---:|---:|---|
| `twitter.real_typed_struct` | `TwitterSearch` | 6286 | not recorded in item 70 | 5715 | 5029 | generated schema-source Track 1 must be >= 5715 if this is the strict row, otherwise >= 5029 |
| `update_center.real_typed_struct` | `UpdateCenter` | 7117 | 5327 | 6470 | 5694 | generated schema-source Track 1 must be >= 6470 if this is the strict row, otherwise >= 5694 |

Scout pass: one generated schema-source Track 1 row meets the strict floor and the other meets the scout floor, with typed parity green. Full H.W4 fold: Track 1 and Track 2 both meet the strict floor on both rows, schema metadata is present, and guard rows below do not regress.

Candidate 11 baselines to beat:

| row | mode | median Mbps | implication |
|---|---|---:|---|
| `twitter.real_typed_struct` | Track 1 `parse_direct -> serde_json::Value -> typed` | 3309 | invalid as close; useful only as pre-schema baseline |
| `update_center.real_typed_struct` | Track 1 `parse_direct -> serde_json::Value -> typed` | 2018 | invalid as close; useful only as pre-schema baseline |
| `update_center.real_typed_struct` | Track 1 hand typed builder + post-parse checksum | 4845 | profiler-only baseline; schema-source candidate must beat it and reach at least 5694 |
| `update_center.real_typed_struct` | independent Track 2 typed path | 2661 | full H.W4 remains blocked unless Track 2 also reaches 6470 |
| `update_center.real_typed_struct` | sonic-rs typed struct | 7117 | current S anchor |
| `update_center.real_typed_struct` | serde_json typed struct | 5327 | secondary anchor |

## Keep `semantic_full_digest_stressor` as Guard

Yes. Keep it visible as a guard family. It stresses every semantic key/string/number byte and is not the representative DirectBuild closure row, but passing `real_typed_struct` must not hide digest regressions.

Guard rule for Candidate 12:

- Report all 17 existing `direct_to_struct` / `semantic_full_digest_stressor` rows.
- No row may regress by more than 5% against the current `skinny/RESULTS.md` Mbps baseline.
- Any row currently passing must remain above `sonic/1.10` in time. With the current table, that means `citm_catalog`, `marine_ik`, and `numbers`. If the bench prose is corrected to reclassify `mesh`, add `mesh` to the PASS-preservation set.
- Always report `unicode_escapes`, `unicode_mixed`, `distinct_values`, and `y_string_unicode`; they are the string/Unicode stressor rows most likely to catch a hidden typed-output shortcut.

Current guard thresholds from `skinny/RESULTS.md`:

| corpus | current T1 Mbps | current T2 Mbps | sonic Mbps | strict `sonic/1.10` | T1 guard Mbps | T2 guard Mbps | guard class |
|---|---:|---:|---:|---:|---:|---:|---|
| `twitter` | 11873 | 11015 | 15648 | 14226 | 11280 | 10465 | regression guard |
| `citm_catalog` | 21388 | 20446 | 21428 | 19480 | 20319 | 19480 | PASS guard |
| `canada` | 10563 | 10453 | 12508 | 11371 | 10035 | 9931 | regression guard |
| `apache_builds` | 11330 | 10335 | 11675 | 10614 | 10764 | 9819 | regression guard |
| `github_events` | 12275 | 11259 | 17062 | 15511 | 11662 | 10697 | regression guard |
| `update_center` | 8308 | 7579 | 12581 | 11438 | 7893 | 7201 | regression guard |
| `mesh` | 8273 | 8442 | 9537 | 8670 | 7860 | 8020 | regression guard until table/prose conflict is resolved |
| `random` | 7785 | 7086 | 10190 | 9264 | 7396 | 6732 | regression guard |
| `gsoc-2018` | 15013 | 14458 | 24163 | 21967 | 14263 | 13736 | regression guard |
| `marine_ik` | 9065 | 9280 | 8839 | 8036 | 8612 | 8816 | PASS guard |
| `instruments` | 12071 | 11134 | 13459 | 12236 | 11468 | 10578 | regression guard |
| `numbers` | 12616 | 12012 | 12474 | 11340 | 11986 | 11412 | PASS guard |
| `unicode_mixed` | 3881 | 4137 | 10142 | 9220 | 3687 | 3931 | regression guard |
| `unicode_escapes` | 5143 | 5030 | 14485 | 13169 | 4886 | 4779 | regression guard |
| `unicode_basic` | 9095 | 8316 | 9803 | 8912 | 8641 | 7901 | regression guard |
| `distinct_values` | 6072 | 5563 | 13185 | 11987 | 5769 | 5285 | regression guard |
| `y_string_unicode` | 3674 | 3679 | 8676 | 7888 | 3491 | 3496 | regression guard |

## Measurement Commands

Run from the skinny workspace:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
```

Correctness before throughput:

```sh
CARGO_TARGET_DIR=/tmp/skv6-schema-C-correctness \
  cargo test -p runtime --profile ax-iter
CARGO_TARGET_DIR=/tmp/skv6-schema-C-correctness \
  cargo test -p bbnf-bench --profile ax-iter real_typed_struct -- --nocapture
CARGO_TARGET_DIR=/tmp/skv6-schema-C-correctness \
  cargo run -p xtask --release -- check-json
CARGO_TARGET_DIR=/tmp/skv6-schema-C-correctness \
  cargo run -p xtask --release -- check-conformance
```

Current-HEAD digest guard baseline:

```sh
BASE_FAST_TARGET=/tmp/skv6-schema-C-base-fast
CARGO_TARGET_DIR="$BASE_FAST_TARGET" \
  cargo build --release -p bbnf-bench --bin profile_direct

BASE_BIN="$BASE_FAST_TARGET/release/profile_direct"
"$BASE_BIN" 3000 unicode_escapes track1
"$BASE_BIN" 3000 unicode_escapes track2
"$BASE_BIN" 3000 unicode_mixed track1
"$BASE_BIN" 3000 unicode_mixed track2
"$BASE_BIN" 200000 y_string_unicode track1
"$BASE_BIN" 200000 y_string_unicode track2
"$BASE_BIN" 30000 distinct_values track1
"$BASE_BIN" 30000 distinct_values track2
```

Candidate typed smoke. Candidate 12 must add these explicit profiler modes or an equivalent `profile_real_typed` binary: `typed-track1-hand`, `typed-track1-schema`, `typed-track2`, `typed-sonic`, `typed-serde`.

```sh
CAND_FAST_TARGET=/tmp/skv6-schema-C-cand-fast
CARGO_TARGET_DIR="$CAND_FAST_TARGET" \
  cargo build --release -p bbnf-bench --bin profile_direct

CAND_BIN="$CAND_FAST_TARGET/release/profile_direct"
for mode in typed-track1-hand typed-track1-schema typed-track2 typed-sonic typed-serde; do
  "$CAND_BIN" 2000 twitter "$mode"
  "$CAND_BIN" 500 update_center "$mode"
done
```

Five paired samples are required before making the scout call:

```sh
for sample in 1 2 3 4 5; do
  for mode in typed-track1-hand typed-track1-schema typed-track2 typed-sonic typed-serde; do
    "$CAND_BIN" 2000 twitter "$mode" 2>> /tmp/skv6-schema-C-typed-smoke.raw
    "$CAND_BIN" 500 update_center "$mode" 2>> /tmp/skv6-schema-C-typed-smoke.raw
  done
done
```

Full advisory gate after typed smoke passes:

```sh
CARGO_TARGET_DIR=/tmp/skv6-schema-C-criterion \
  cargo run -p xtask --release -- bench-json --advisory
```

If the candidate adds Criterion rows for `real_typed_struct`, the gate must write them separately from the existing direct digest rows and must not rename the digest rows away.

## Rejection Meaning

- Correctness failure means the schema-source lowering is not a valid host/API contract. Revert and redress the schema mapping, not the string materializer.
- If `typed-track1-schema` is slower than `typed-track1-hand`, the schema-source machinery is adding overhead and has not earned admission; do not cite it as DirectBuild proof.
- If `typed-track1-schema` beats the hand profiler row but misses both scout thresholds, schema-source was necessary for authority but insufficient for throughput. The remaining gap is generated direct parser control, dynamic map insertion, string/number materialization, or event-cursor/fusion quality, not hidden output-schema provenance.
- If Track 1 meets the scout gate but Track 2 stays below the strict floor, generated DirectBuild authority is plausible but the full representative H.W4 gate remains blocked by the independent substrate ceiling.
- If typed rows pass while digest guard rows regress, keep `real_typed_struct` as a supplemental representative row only; `semantic_full_digest_stressor` remains the SOTA guard and must carry its own redress route.
- If the implementation uses a hidden grammar directive, a benchmark-private Track 1 parser, a parse-time checksum-only sink, broad live-field `serde_json::Value`, a new top-level BIR variant, a retained side table, or a parallel source scan, the result is INVALID rather than a throughput rejection.

## Commit Split Plan

1. Research commit: `docs(sk-v6-wave3-research): specify schema-source DirectBuild scout`
   - Docs only. Record the schema-source contract, exact rows, thresholds, and why REDRESS 70 blocks hand-authored sinks as proof.
2. Plan/implementation commit: `feat(sk-v6-schema-source): lower host output schema into DirectBuild field facts`
   - Add the host/API schema carrier, generated field facts, generated typed sink consumption, tests, and profiler modes. Keep digest rows intact. Preserve unrelated staged work.
3. Redress commit if rejected: `docs(sk-v6-wave3-redress): reject schema-source generated typed sink as DirectBuild close`
   - Body required under local commit discipline: why the route was tried, what landed and was reverted or retained, exact correctness and Mbps evidence, and where the remainder routes. Save the rejected patch under `/tmp/`.
4. Redress/report commit if accepted: `docs(sk-v6-wave3-report): admit schema-source real typed struct scout`
   - Update `skinny/RESULTS.md`, `skinny/REDRESS.md`, `restart/skinny/BENCH.md`, and `restart/MASTER-PLAN.md` with the measured rows, guard status, and whether the result is only a scout pass or a full H.W4 fold.
