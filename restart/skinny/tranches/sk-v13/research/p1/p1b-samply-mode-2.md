# SK-V13 P1-B: Samply Mode II Product-Plane Profile

Pass: S-P1 Profile. Cycle: V13 / S-P1 V2 fold.
Date: 2026-05-21.
Scope: samply profiling mode II, cold per-parse `direct_to_struct` plus `real_typed_struct` workloads.
Output: this file.
Baseline: SK-V13-open (`7ee299096be7d7fdaa0e69344a6cd18bbd55524f`; source-equivalent to the V1 profile binary for `skinny/crates/`).
Host triple: aarch64-apple-darwin.
Build flags: release profile, `debug=true`, `RUSTFLAGS="-C target-cpu=native"`, target dir `/tmp/skv13-profile-target-v2`.
Profile tool: samply 0.13.1, `samply record --save-only --unstable-presymbolicate -r 1000`; offline symbol resolution via `.json.syms.json`.
Corpus coverage: direct samply 17/17 Track 1 + Track 2, 0 bad return codes; typed evidence retained from V1 for the 7 generated typed rows.

V3 fold note: direct rows are classified in
`support/evidence-ledger-v3.md`. Generated `parse_*_direct` leaves are
`json-direct-envelope`; `unicode_escapes` / `unescape_string` is a
`resolved-json-unicode-candidate`; `y_string_unicode` Track 2 is
`timer/noise`.

## §1 - Method (commands run; verbatim, reproducible)

Identity:

```bash
cat /tmp/skv13-p1-v2/artifacts/identity.txt
# root=/tmp/skv13-p1-v2
# repo=/Users/mkbabb/Programming/bbnf-lang
# head=7ee299096be7d7fdaa0e69344a6cd18bbd55524f
# date=2026-05-21T06:56:28Z
```

Build:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv13-profile-target-v2 \
RUSTFLAGS='-C target-cpu=native' \
cargo build --release -p bbnf-bench --bin profile_direct --bin xctrace_probe
```

Direct capture loop:

```bash
samply record --save-only --unstable-presymbolicate -r 1000 \
  -o /tmp/skv13-p1-v2/samply/profiles/direct__${corpus}__${mode}.json.gz \
  /tmp/skv13-profile-target-v2/release/profile_direct 3000 "${corpus}" "${mode}"
```

Status/reproduction checks:

```bash
awk -F '\t' 'NR>1{n++; bad+=($4!=0)} END{print n,bad+0}' \
  /tmp/skv13-p1-v2/samply/direct_capture_status.tsv
# 34 0

find /tmp/skv13-p1-v2/samply/profiles -name 'direct__*.json.gz' | wc -l
# 34
find /tmp/skv13-p1-v2/samply/profiles -name 'direct__*.json.syms.json' | wc -l
# 34
```

The V1 blocker was a command quoting bug: `profile_direct` received literal
quoted corpus names and panicked before the timed loop. V2 passes corpus names
as real argv entries; every direct log contains `profile-direct: starting timed
loop` and a `PROBE_RESULT`.

## §2 - Findings (per-corpus table; file:line on every hot-leaf claim)

Top self-time symbols below are the rank-1 leaf after resolving samply frame
RVAs through the matching `.json.syms.json` sidecar. The full top-20 extraction
for every direct profile is `/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv`.

| Corpus | Track 1 Mbps / cB | Track 1 rank-1 self-time symbol | Track 2 Mbps / cB | Track 2 rank-1 self-time symbol |
|---|---:|---|---:|---|
| twitter | 11821.161 / 2.969 | 74.0% `parse_object_value_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:466`) | 10841.589 / 3.224 | 55.0% `HandParser::string` (`skinny/crates/bbnf-bench/src/direct_struct.rs:541`) |
| citm_catalog | 21968.958 / 1.605 | 58.4% `parse_array_element_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:506`) | 20806.401 / 1.694 | 46.9% `HandParser::value` (`skinny/crates/bbnf-bench/src/direct_struct.rs:460`) |
| canada | 10547.205 / 3.262 | 85.3% `parse_array_element_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:506`) | 10148.055 / 3.332 | 87.5% `HandParser::value` (`skinny/crates/bbnf-bench/src/direct_struct.rs:460`) |
| apache_builds | 11071.291 / 3.081 | 38.1% `parse_object_value_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:466`) | 10128.990 / 3.355 | 44.1% `HandParser::value` (`skinny/crates/bbnf-bench/src/direct_struct.rs:460`) |
| github_events | 11885.718 / 2.839 | 67.7% `parse_object_value_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:466`) | 11062.206 / 3.085 | 45.1% `HandParser::tiny_plain_string` (`skinny/crates/bbnf-bench/src/direct_struct.rs:567`) |
| update_center | 8206.081 / 4.140 | 68.3% `parse_object_value_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:466`) | 7334.045 / 4.622 | 56.8% `HandParser::string` (`skinny/crates/bbnf-bench/src/direct_struct.rs:541`) |
| mesh | 8786.959 / 3.865 | 76.7% `parse_array_element_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:506`) | 8063.295 / 4.205 | 95.3% `HandParser::value` (`skinny/crates/bbnf-bench/src/direct_struct.rs:460`) |
| random | 7661.152 / 4.425 | 37.7% `parse_object_value_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:466`) | 6839.907 / 4.957 | 39.8% `HandParser::string` (`skinny/crates/bbnf-bench/src/direct_struct.rs:541`) |
| gsoc-2018 | 14522.580 / 2.337 | 60.2% `parse_object_value_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:466`) | 13954.747 / 2.432 | 52.1% `HandParser::string` (`skinny/crates/bbnf-bench/src/direct_struct.rs:541`) |
| marine_ik | 9241.327 / 3.673 | 72.3% `parse_array_element_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:506`) | 9224.560 / 3.663 | 82.1% `HandParser::value` (`skinny/crates/bbnf-bench/src/direct_struct.rs:460`) |
| instruments | 11738.320 / 2.882 | 58.3% `Option<&u8>::copied` (`core/src/option.rs:2141`) | 10895.383 / 3.112 | 40.2% `HandParser::string` (`skinny/crates/bbnf-bench/src/direct_struct.rs:541`) |
| numbers | 12216.215 / 2.777 | 76.1% `parse_array_element_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:506`) | 11950.227 / 2.832 | 89.8% `HandParser::value` (`skinny/crates/bbnf-bench/src/direct_struct.rs:460`) |
| unicode_mixed | 4422.918 / 7.667 | 55.9% `parse_object_value_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:466`) | 4283.724 / 7.878 | 55.0% `HandParser::string` (`skinny/crates/bbnf-bench/src/direct_struct.rs:541`) |
| unicode_escapes | 4771.925 / 7.074 | 46.7% `parse_that_regex::unescape_string` (`skinny/crates/parse-that-regex/src/lib.rs:718`) | 4259.928 / 7.578 | 46.4% `parse_that_regex::unescape_string` (`skinny/crates/parse-that-regex/src/lib.rs:718`) |
| unicode_basic | 8858.170 / 3.817 | 44.1% `parse_object_value_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:466`) | 8043.084 / 4.209 | 48.2% `HandParser::string` (`skinny/crates/bbnf-bench/src/direct_struct.rs:541`) |
| distinct_values | 6097.397 / 5.559 | 49.5% `parse_array_element_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:542`) | 5458.584 / 6.208 | 34.8% `HandParser::take` (`skinny/crates/bbnf-bench/src/direct_struct.rs:610`) |
| y_string_unicode | 3101.039 / 10.942 | 19.5% `parse_array_element_at_direct::<JsonDigestSink>` (`skinny/crates/runtime/src/grammars/json/generated.rs:506`) | 2975.830 / 11.408 | 31.1% `mach_absolute_time` (`libsystem_kernel.dylib`) |

Typed evidence remains the V1 seven-row generated typed subset. There was no
source change under `skinny/crates/` between the V1 profile baseline and V2;
the V2 fold did not invent typed rows for the ten unsupported corpora.

## §3 - Delta vs SK-V12 (per row; Mbps + c/B + classification)

SK-V12 close does not publish a machine-readable product-plane profile ledger
with direct samply symbols. V2 therefore reports current SK-V13-open direct
symbols and leaves prior-tranche symbol delta as unavailable.

The direct V2 profile materially changes the S-P1 state relative to V1:

| Surface | V1 state | V2 state | S-P2 consequence |
|---|---|---|---|
| direct samply | 34 profile files were panic-path captures | 34 non-panic profile files, 34 sidecars, 0 bad rc | direct rows are now symbol-attributable |
| direct unicode | unresolved at symbol level | `parse_that_regex::unescape_string` is rank-1 for `unicode_escapes` Track 1 and Track 2 | unicode direct research can target a named leaf |
| direct string/digest rows | only PMU/cB evidence | generated `parse_*_direct` envelopes dominate Track 1; Track 2 hand parser string/value leaves dominate | S-P2 must separate generated dispatch envelope from primitive leaf work |

## §4 - Anomalies + masking signals (flagged for S-P2)

- V2 uses `--save-only --unstable-presymbolicate`; file:line resolution is
  through the `.json.syms.json` sidecar and the top-20 TSV, not through a live
  interactive samply UI. CH6 must decide whether this offline resolution is
  sufficient for S-P1.
- Direct Track 1 still often resolves to generated direct envelopes rather than
  a primitive leaf. That is an attribution fact, not a license to scope a broad
  dispatch rewrite without P1-C/P1-E corroboration.
- `unicode_escapes` is now a clean primitive attribution:
  `parse_that_regex::unescape_string` at `lib.rs:718` consumes 46.7% / 46.4%
  self-time in Track 1 / Track 2.
- `y_string_unicode` Track 2 rank-1 resolves to `mach_absolute_time`; its
  parser leaves are in lower ranks in `/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv`.
  S-P2 should treat that row as timer-noisy unless a longer capture confirms it.
- The ten missing typed rows remain missing product surfaces, not profiling
  omissions.
- CH3 direct-row guard: none of these direct profile signals reopens
  REDRESS 119/120 by itself. Every residual direct-row reopen must cite the
  prior fixpoint, name a fresh material differential, and carry same-harness
  strict comparator evidence.
- CH3 pre-pin guard: generated direct envelopes and `unescape_string` do not
  reopen dispatch-table/function-pointer alternates, parser-local cursors,
  event sidecars, decoded-string stats sinks, generic source visitors, or
  source-method digest folds.

## §5 - Sources (every artefact path + run id)

- `/tmp/skv13-p1-v2/artifacts/identity.txt`
- `/tmp/skv13-p1-v2/samply/direct_capture_status.tsv`
- `/tmp/skv13-p1-v2/samply/logs/direct__{corpus}__track{1,2}.log`
- `/tmp/skv13-p1-v2/samply/profiles/direct__{corpus}__track{1,2}.json.gz`
- `/tmp/skv13-p1-v2/samply/profiles/direct__{corpus}__track{1,2}.json.syms.json`
- `/tmp/skv13-p1-v2/summary/direct_summary.tsv`
- `/tmp/skv13-p1-v2/summary/hotleaf_top20.tsv`
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`
- `restart/skinny/tranches/sk-v13/research/p1/support/profile-provenance-v3.md`
- `/tmp/skv13-p1/pmu/pmu_rows.tsv` and V1 typed samply profiles for the seven existing typed rows
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`
