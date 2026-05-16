# SK-V6 Wave 3 R2d Direct Measurement Plan

Date: 2026-05-14
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Scope: measurement design only. Do not edit repository files while executing the plan unless the candidate itself is being implemented.

## Source Inspection Summary

- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs` is the focused CLI. It accepts `iters corpus mode`, warms up 16 runs, then loops `track1`, `track2`, `sonic`, or `serde`. `track1` calls generated direct parsing through `bbnf_bench::direct_struct::track1_digest`.
- `skinny/crates/bbnf-bench/src/direct_struct.rs` has the direct digest receiver and comparators:
  - `track1_digest` calls `runtime::generated_json::parse_direct(input, &mut JsonDigestSink)`.
  - `track2_digest` calls the independent hand parser.
  - `sonic_digest` and `serde_digest` provide direct-to-digest anchors.
  - `JsonDigestSink` and `JsonDirectDigest` fold strings/numbers into the digest, currently with `#[inline(always)]` helpers.
- `skinny/crates/runtime/src/grammars/json/generated.rs` already has `runtime/parse-attribution` no-inline boundaries around generated direct entry/dispatch/container/string/number functions:
  - `parse_direct`
  - `parse_value_direct`
  - `parse_object_value_at_direct`
  - `parse_array_element_at_direct`
  - `parse_object_direct`
  - `parse_array_direct`
  - `parse_string_direct`
  - `parse_number_*_direct`
  - `emit_number_*_direct`
  - `consume_literal_direct`
- `skinny/crates/runtime/src/grammars/json/sink.rs` is the weak attribution spot. The default `*_source` string materialization hooks are `#[inline(always)]`; they call `unescape_json_string` and then the sink fold method. That is fine for production, but it can blur PC attribution for a direct string materializer.
- `skinny/crates/bbnf-bench/benches/json_parity.rs` defines the direct Criterion rows used by the gate:
  - `track1_direct_to_struct`
  - `track2_direct_to_struct`
  - `sonic_rs_direct_to_struct`
  - `serde_json_direct_to_struct`
- `skinny/crates/bbnf-bench/src/gate.rs` marks a direct projection failure when Track 1 or Track 2 is slower than `sonic-rs * 1.10` in time.

## Measurement Target

The next direct candidate should be judged as a direct field-layout or same-loop string materializer, not as another sink-local decoded-stats helper, quote-source streaming hash, or receiver-hook shortcut. The target rows are the Wave 3 direct rows already named by R4c and GRAND-SYNTHESIS:

| row | reason | profile iters |
|---|---|---:|
| `unicode_escapes` | escaped-string decode/materialization dominated | 3000 |
| `unicode_mixed` | string recognition plus decode and copy | 3000 |
| `y_string_unicode` | small escaped Unicode row; noisy but important | 200000 |
| `distinct_values` | non-escaped guard for receiver/fold overhead | 30000 |
| `gsoc-2018` | large string row with receiver/fold cost | 3000 |

Throughput guard rows for the same `profile_direct` smoke:

| row | reason | smoke iters |
|---|---|---:|
| `twitter` | plain/tiny string guard | 10000 |
| `apache_builds` | array/string receiver guard | 60000 |
| `github_events` | small event row guard | 120000 |
| `unicode_basic` | non-escaped Unicode guard | 6000 |
| `canada` | number path must not regress | 3000 |

Only run samply on guard rows if the production smoke shows a guard regression above 5%.

## Build Commands

Run from the skinny workspace.

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny

# Baseline before applying the candidate patch.
export BASE_TARGET=/tmp/skv6-R2d-direct-base-target
CARGO_TARGET_DIR="$BASE_TARGET" \
  cargo build --release -p bbnf-bench --bin profile_direct \
  --features runtime/parse-attribution

# Candidate after applying the candidate patch.
export CAND_TARGET=/tmp/skv6-R2d-direct-cand-target
CARGO_TARGET_DIR="$CAND_TARGET" \
  cargo build --release -p bbnf-bench --bin profile_direct \
  --features runtime/parse-attribution

# Production binaries for throughput smoke. These must not use parse-attribution.
export BASE_FAST_TARGET=/tmp/skv6-R2d-direct-base-fast-target
CARGO_TARGET_DIR="$BASE_FAST_TARGET" \
  cargo build --release -p bbnf-bench --bin profile_direct

export CAND_FAST_TARGET=/tmp/skv6-R2d-direct-cand-fast-target
CARGO_TARGET_DIR="$CAND_FAST_TARGET" \
  cargo build --release -p bbnf-bench --bin profile_direct
```

If the candidate adds attribution-only boundaries in `bbnf-bench` itself, prefer a forwarded package feature named `parse-attribution` and build with:

```sh
CARGO_TARGET_DIR="$CAND_TARGET" \
  cargo build --release -p bbnf-bench --bin profile_direct \
  --features parse-attribution
```

## Correctness Commands

Run these before any throughput claim:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv6-R2d-direct-correctness-target \
  cargo test -p runtime --profile ax-iter
CARGO_TARGET_DIR=/tmp/skv6-R2d-direct-correctness-target \
  cargo test -p bbnf-bench --profile ax-iter
```

Failure criterion: any correctness failure rejects the candidate before profiling.

## Production Throughput Smoke

This is the fast before/after measurement. Use production release binaries, not parse-attribution binaries.

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
mkdir -p /tmp/skv6-R2d-direct-smoke

rows=(unicode_escapes unicode_mixed y_string_unicode distinct_values gsoc-2018 twitter apache_builds github_events unicode_basic canada)

iters_for() {
  case "$1" in
    unicode_escapes) echo 3000 ;;
    unicode_mixed) echo 3000 ;;
    y_string_unicode) echo 200000 ;;
    distinct_values) echo 30000 ;;
    gsoc-2018) echo 3000 ;;
    twitter) echo 10000 ;;
    apache_builds) echo 60000 ;;
    github_events) echo 120000 ;;
    unicode_basic) echo 6000 ;;
    canada) echo 3000 ;;
    *) echo 10000 ;;
  esac
}

run_smoke() {
  label="$1"
  bin="$2"
  out="/tmp/skv6-R2d-direct-smoke/${label}.csv"
  printf 'label,row,rep,iters,mbps\n' > "$out"
  for row in "${rows[@]}"; do
    iters="$(iters_for "$row")"
    for rep in 1 2 3 4 5; do
      mbps="$("$bin" "$iters" "$row" track1 2>&1 \
        | tee "/tmp/skv6-R2d-direct-smoke/${label}.${row}.${rep}.log" \
        | awk '/->/ { for (i = 1; i <= NF; i++) if ($i == "->") print $(i + 1) }')"
      printf '%s,%s,%s,%s,%s\n' "$label" "$row" "$rep" "$iters" "$mbps" >> "$out"
    done
  done
}

run_smoke baseline "$BASE_FAST_TARGET/release/profile_direct"
run_smoke candidate "$CAND_FAST_TARGET/release/profile_direct"
```

Summarize medians:

```sh
python3 - <<'PY'
import csv, statistics
from pathlib import Path
root = Path('/tmp/skv6-R2d-direct-smoke')
rows = {}
for path in [root/'baseline.csv', root/'candidate.csv']:
    with path.open() as f:
        for r in csv.DictReader(f):
            rows.setdefault((r['label'], r['row']), []).append(float(r['mbps']))
print('row,baseline_median,candidate_median,delta_pct')
all_rows = sorted({row for (_, row) in rows})
for row in all_rows:
    b = statistics.median(rows[('baseline', row)])
    c = statistics.median(rows[('candidate', row)])
    print(f'{row},{b:.0f},{c:.0f},{(c/b-1)*100:.2f}')
PY
```

## Samply PC Attribution

Run samply on the five target rows for both baseline and candidate attribution binaries.

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
mkdir -p /tmp/skv6-R2d-direct-profiles

profile_rows=(unicode_escapes unicode_mixed y_string_unicode distinct_values gsoc-2018)

profile_iters_for() {
  case "$1" in
    unicode_escapes) echo 3000 ;;
    unicode_mixed) echo 3000 ;;
    y_string_unicode) echo 200000 ;;
    distinct_values) echo 30000 ;;
    gsoc-2018) echo 3000 ;;
    *) echo 10000 ;;
  esac
}

run_profiles() {
  label="$1"
  bin="$2"
  for row in "${profile_rows[@]}"; do
    iters="$(profile_iters_for "$row")"
    samply record --rate 4000 --main-thread-only --unstable-presymbolicate \
      --save-only --no-open \
      -o "/tmp/skv6-R2d-direct-profiles/${label}.${row}.track1.profile.json.gz" \
      "$bin" "$iters" "$row" track1 \
      > "/tmp/skv6-R2d-direct-profiles/${label}.${row}.record.txt" 2>&1
    grep -E 'parse_direct|parse_string_direct|unescape_json_string|match_tiny_plain_string|JsonSink|JsonDigestSink|JsonDirectDigest|fold_string_scalar|hash_bytes|array_string|object_string|materialize|direct' \
      "/tmp/skv6-R2d-direct-profiles/${label}.${row}.track1.profile.json.syms.json" \
      > "/tmp/skv6-R2d-direct-profiles/${label}.${row}.syms-proof.txt" || true
  done
}

run_profiles baseline "$BASE_TARGET/release/profile_direct"
run_profiles candidate "$CAND_TARGET/release/profile_direct"
```

Expected named proof in each `*.syms-proof.txt`:

- Always present: `runtime::generated_json::generated::parse_direct::<bbnf_bench::direct_struct::JsonDigestSink>`.
- Always present on target rows: `runtime::generated_json::generated::parse_string_direct`.
- Escape rows: `parse_that_regex::unescape_json_string`.
- Plain/receiver rows: one or more of `JsonDigestSink::array_string`, `JsonDigestSink::object_string`, `JsonDirectDigest::fold_string_scalar`, `hash_bytes`, or a candidate-specific direct field-layout helper.
- The candidate must add or expose a named direct materializer boundary. If all candidate samples remain inside only `parse_string_direct`, `unescape_json_string`, or anonymous closure frames, the attribution is not strong enough to accept the candidate.

## Profile Analysis Command

Use this temporary analyzer to compute self and inclusive samples by resolved symbol from samply's profile plus `*.syms.json`.

```sh
cat > /tmp/skv6-R2d-direct-profile-analyze.py <<'PY'
#!/usr/bin/env python3
import bisect, gzip, json, sys
from collections import defaultdict
from pathlib import Path

def load(path):
    with gzip.open(path, 'rb') as f:
        profile = json.load(f)
    with open(str(path).replace('.json.gz', '.json.syms.json')) as f:
        syms = json.load(f)
    return profile, syms

def resolver(profile, syms):
    libs = profile['libs']
    lib_name_by_idx = [lib['name'] for lib in libs]
    strings = syms['string_table']
    tables = {}
    for data in syms['data']:
        table = sorted(data['symbol_table'], key=lambda s: s['rva'])
        rvas = [s['rva'] for s in table]
        ends = [s['rva'] + s['size'] for s in table]
        names = [strings[s['symbol']] if s.get('symbol') is not None else '<no sym>' for s in table]
        tables[data['debug_name']] = (rvas, ends, names)
    thread = profile['threads'][0]
    func_resource = thread['funcTable']['resource']
    resource_lib = thread['resourceTable']['lib']
    addresses = thread['frameTable']['address']
    frame_func = thread['frameTable']['func']
    func_names = thread['funcTable']['name']
    string_array = thread['stringArray']
    def resolve(frame):
        addr = addresses[frame]
        f = frame_func[frame]
        lib_idx = resource_lib[func_resource[f]]
        lib = lib_name_by_idx[lib_idx] if lib_idx >= 0 else '?'
        if lib in tables:
            rvas, ends, names = tables[lib]
            i = bisect.bisect_right(rvas, addr) - 1
            if i >= 0 and addr < ends[i]:
                return names[i]
        name_idx = func_names[f]
        if name_idx is not None and name_idx >= 0:
            return string_array[name_idx]
        return f'{lib}!0x{addr:x}'
    return resolve

def compute(profile, resolve):
    thread = profile['threads'][0]
    stacks = thread['stackTable']
    frames = thread['frameTable']
    samples = thread['samples']
    frame_names = [resolve(i) for i in range(frames['length'])]
    prefix = stacks['prefix']
    stack_frame = stacks['frame']
    leaf_cache = {}
    lineage_cache = {}
    def leaf(stack):
        if stack not in leaf_cache:
            leaf_cache[stack] = frame_names[stack_frame[stack]]
        return leaf_cache[stack]
    def lineage(stack):
        if stack in lineage_cache:
            return lineage_cache[stack]
        out = set()
        cur = stack
        while cur is not None and cur != -1:
            out.add(frame_names[stack_frame[cur]])
            cur = prefix[cur]
        lineage_cache[stack] = out
        return out
    self_time = defaultdict(int)
    incl_time = defaultdict(int)
    total = 0
    for stack in samples['stack']:
        if stack is None or stack == -1:
            continue
        total += 1
        self_time[leaf(stack)] += 1
        for name in lineage(stack):
            incl_time[name] += 1
    return total, self_time, incl_time

interesting = [
    'parse_direct',
    'parse_string_direct',
    'unescape_json_string',
    'match_tiny_plain_string',
    'parse_object_direct',
    'parse_array_direct',
    'JsonDigestSink',
    'JsonDirectDigest',
    'fold_string_scalar',
    'hash_bytes',
    'materialize',
]

for arg in sys.argv[1:]:
    path = Path(arg)
    profile, syms = load(path)
    total, self_time, incl_time = compute(profile, resolver(profile, syms))
    print(f'## {path.name}')
    print(f'samples,{total}')
    print('self_pct,self_samples,incl_pct,incl_samples,symbol')
    seen = set()
    ordered = sorted(self_time.items(), key=lambda kv: -kv[1])
    for sym, n in ordered[:30]:
        seen.add(sym)
        print(f'{100*n/total:.2f},{n},{100*incl_time[sym]/total:.2f},{incl_time[sym]},{sym}')
    for needle in interesting:
        for sym, n in sorted(self_time.items(), key=lambda kv: -kv[1]):
            if sym in seen:
                continue
            if needle in sym:
                seen.add(sym)
                print(f'{100*n/total:.2f},{n},{100*incl_time[sym]/total:.2f},{incl_time[sym]},{sym}')
    print()
PY

python3 /tmp/skv6-R2d-direct-profile-analyze.py \
  /tmp/skv6-R2d-direct-profiles/*.track1.profile.json.gz \
  > /tmp/skv6-R2d-direct-profiles/summary.csv
```

## Expected Baseline Symbols By Row

| row | expected hot symbols before candidate | candidate must move |
|---|---|---|
| `unicode_escapes` | `unescape_json_string`, `parse_string_direct`; little `match_tiny_plain_string` | combined `parse_string_direct + unescape_json_string` self share |
| `unicode_mixed` | `parse_string_direct`, `unescape_json_string`, possible `_platform_memmove` | combined string/materialization self share and copy/allocation residual |
| `y_string_unicode` | `unescape_json_string`, `parse_string_direct`, allocator/timer noise | application string/materialization share; require high sample count |
| `distinct_values` | `parse_object_direct`, `parse_string_direct`, `match_tiny_plain_string`, receiver/fold symbols | receiver/fold closure share and no escaped-path regression |
| `gsoc-2018` | `parse_string_direct`, `fold_string_scalar` or receiver/fold symbols, `unescape_json_string`, `match_tiny_plain_string` | receiver/fold closure share and string/materialization share |

## Pass/Fail Criteria

Correctness:

- PASS only if `cargo test -p runtime --profile ax-iter` and `cargo test -p bbnf-bench --profile ax-iter` pass.
- FAIL immediately on direct digest mismatch, parser panic, or fixture read failure.

Throughput, using fresh same-tree production medians from `/tmp/skv6-R2d-direct-smoke`:

- `unicode_escapes` Track 1 median improves by at least 20%.
- `unicode_mixed` Track 1 median improves by at least 15%.
- At least two of `y_string_unicode`, `distinct_values`, and `gsoc-2018` improve by at least 8%.
- No target row or guard row regresses by more than 5%.
- `y_string_unicode` counts as a positive companion only if the five-run median improves by at least 8% and the samply run has at least 50000 non-null samples. Treat timer/allocator-heavy movement as supporting evidence, not sole evidence.

Current `skinny/RESULTS.md` sanity floors, not substitutes for fresh baseline:

- `unicode_escapes`: 5143 Mbps, so +20% is 6172 Mbps.
- `unicode_mixed`: 3881 Mbps, so +15% is 4463 Mbps.
- `y_string_unicode`: 3674 Mbps, so +8% is 3968 Mbps.
- `distinct_values`: 6072 Mbps, so +8% is 6558 Mbps.
- `gsoc-2018`: 15013 Mbps, so +8% is 16214 Mbps.

PC attribution:

- PASS only if `parse_direct` is in the inclusive stack for effectively all application samples on every target row.
- `unicode_escapes` and `unicode_mixed`: combined self share for `parse_string_direct + unescape_json_string` drops at least 20% relative.
- `distinct_values` and `gsoc-2018`: receiver/fold closure share drops at least 30% relative. Count symbols matching `JsonDigestSink::*string`, `JsonDirectDigest::fold_string_scalar`, `hash_bytes`, and candidate-specific direct string/fold helpers.
- The candidate-specific direct materializer must appear as a named symbol in `*.syms-proof.txt` or the analyzer summary. If the cost only disappears into a broader wrapper, do not accept the candidate until attribution-only instrumentation is added and rerun.

Rejection:

- Revert and record a REDRESS rejection if throughput thresholds fail, even when profile percentages move.
- Revert and record a REDRESS rejection if profile percentages move only on one escaped row and guards do not move.
- Do not reopen REDRESS 54/55 shapes under a new name: sink-local decoded stats and quote-source streaming hash remain rejected.
- Do not accept a candidate that changes Track 2 or competitor behavior unless the change is explicitly part of the candidate. The primary pass/fail surface is Track 1 generated SinkOnly.

## Full Criterion Gate After A Fast Pass

Only run this after the focused smoke and samply gates pass.

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CRITERION_TARGET=/tmp/skv6-R2d-direct-candidate-criterion
CARGO_TARGET_DIR="$CRITERION_TARGET" cargo bench -p bbnf-bench --bench json_parity
CARGO_TARGET_DIR="$CRITERION_TARGET" cargo run -p bbnf-bench --bin gate -- --advisory
```

Hard failure in the advisory gate rejects the candidate. A remaining `N-direct` verdict on unrelated rows is acceptable only if the focused Wave 3 candidate gate above passed and the REDRESS entry states which rows remain.

## Attribution Gap And Minimal Instrumentation

Current `runtime/parse-attribution` is sufficient to prove that Track 1 reaches generated `parse_direct`, and it separates parser control from `parse_string_direct`, container functions, and number emission. It is not sufficient by itself to prove a new direct string materializer when the candidate's effect lives inside default sink source hooks or inline digest helpers.

Minimal non-invasive instrumentation, to add only if the candidate profile cannot name the materialization boundary:

1. In `skinny/crates/runtime/src/grammars/json/sink.rs`, change default source hooks to attribution-only no-inline:

```rust
#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn array_string_source(&mut self, raw: &str, needs_unescape: bool) -> Result<(), RegexError> { ... }
```

Apply the same shape to `key_source`, `string_source`, and `object_string_source`.

2. In `skinny/crates/codegen/src/json_sink_direct.rs`, add generated attribution wrappers around the sink calls:

```rust
#[cfg_attr(feature = "parse-attribution", inline(never))]
#[cfg_attr(not(feature = "parse-attribution"), inline(always))]
fn emit_array_string_source_direct<S: JsonSink>(
    input: &str,
    raw: &str,
    needs_unescape: bool,
    sink: &mut S,
) -> Result<(), ParseError<'_>> {
    sink.array_string_source(raw, needs_unescape)
        .map_err(|err| string_error(input, err))
}
```

Use equivalent wrappers for root string, object string, and key. This keeps production behavior inline while giving samply a named boundary between string recognition and materialization/fold.

3. If the candidate changes bench receiver helpers, add a forwarded attribution feature to `skinny/crates/bbnf-bench/Cargo.toml`:

```toml
[features]
parse-attribution = ["runtime/parse-attribution"]
```

Then gate no-inline attributes in `skinny/crates/bbnf-bench/src/direct_struct.rs` for `JsonDigestSink::{key,string,array_string,object_string}` and `JsonDirectDigest::{fold_string_scalar,fold_key,hash_bytes}`.

4. If the candidate changes internals of `parse-that-regex::unescape_json_string`, add a `parse-attribution` feature to `parse-that-regex` and forward it from `runtime/parse-attribution`. Use it to no-inline only the internal decode/materialization helpers needed for diagnosis, such as Unicode escape decode and escape/control scanning. Do not add counters or logging to the hot path.

Instrumentation acceptability rule: all instrumentation must be `cfg_attr(feature = "parse-attribution", inline(never))` with `inline(always)` or current behavior in normal release builds. No new side tables, counters, source passes, logging, or semantic branches belong in the production candidate.
