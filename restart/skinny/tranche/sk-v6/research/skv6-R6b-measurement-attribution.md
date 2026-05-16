# SK-V6 Wave 1b R6b — measurement / attribution audit

Date: 2026-05-14. Workspace:
`/Users/mkbabb/Programming/bbnf-lang`.

## Inputs Read

- `skinny/RESULTS.md` is the current authority. It shows retained parse Track
  1 is still red on the expanded G rows, with relevant current rows:
  `canada` 17738 Mbps, `gsoc-2018` 21907 Mbps, `instruments` 11887 Mbps,
  `unicode_mixed` 8720 Mbps.
- `skinny/REDRESS.md` item 61 / "SK-V6 Wave 2 Candidate-2 Redress" documents
  the failure pattern: focused `profile-lazy` rows improved
  `unicode_mixed` +16.9%, `gsoc-2018` +15.8%, and `y_string_unicode` +6.0%,
  but the full advisory matrix only gave one >=10% target win and regressed
  `canada` -9.8% and `instruments` -7.5%.
- `skinny/crates/bbnf-bench/src/bin`, `skinny/xtask/src/bin`, and profile
  directories were audited. Existing tools are sufficient; no new repo tool is
  needed for the next candidate's measurement ritual.

## Tool Decision

Use `profile-lazy` for retained-parse c/B triage. It directly drives
`runtime::generated_json::parse`, emits Mbps, and can be built both as the
production shape and with `runtime/parse-attribution`.

Use `samply` with `runtime/parse-attribution` for attribution only, not as the
accept/reject metric by itself. Candidate 2 proved that aggregate wrapper share
under `match_string_at_quote` can stay high even when row Mbps improves.

Use `cargo xtask bench-json --advisory` only as the final gate authority. It
is Criterion-backed and includes the full matrix, but it invokes `gate`, which
writes `skinny/RESULTS.md`; do not use it for quick exploratory loops unless
that repo write is intended.

Do not use `profile_direct` for retained parse candidates. Use it only if the
candidate intentionally affects `parse_direct` / `SinkOnly` or shared
`parse-that-regex` code whose direct effect must be checked.

Do not use existing `skinny/profile/*/PROFILE-REPORT.md` as current evidence.
Those reports are useful methodology references, but they are profile snapshots
from older binaries.

## Row List

The next retained-parse candidate must measure all 17 retained rows in the
fast `profile-lazy` c/B smoke. Candidate 2 failed because the focused set did
not include the regression sentinels.

Full retained smoke rows:

`twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`,
`update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `instruments`,
`numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`,
`distinct_values`, `y_string_unicode`.

Mandatory attribution rows before any Wave 2 synthesis claim:

`unicode_mixed`, `gsoc-2018`, `y_string_unicode`, `canada`, `instruments`,
`twitter`.

Add any other retained row whose median c/B changes by >=2.0% in the smoke
loop. For a string candidate, also add `apache_builds`, `github_events`, and
`update_center` when any of them move by >=2.0%.

## Exact Command Set

Run the baseline commands from the current authority tree before applying the
candidate patch. Then apply the candidate and run the candidate commands from
the patched tree. All outputs go to `/tmp`.

### 1. Build retained-parse binaries

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
OUT=/tmp/skv6-wave2-next
rm -rf "$OUT"
mkdir -p "$OUT"

# Baseline, before candidate patch.
CARGO_TARGET_DIR="$OUT/base-release" \
  cargo build --release -p xtask --bin profile-lazy
CARGO_TARGET_DIR="$OUT/base-attr" \
  cargo build --release -p xtask --bin profile-lazy \
  --features runtime/parse-attribution

# After applying the candidate patch, run the same two builds.
CARGO_TARGET_DIR="$OUT/cand-release" \
  cargo build --release -p xtask --bin profile-lazy
CARGO_TARGET_DIR="$OUT/cand-attr" \
  cargo build --release -p xtask --bin profile-lazy \
  --features runtime/parse-attribution
```

### 2. Production-shape retained parse c/B smoke

This loop uses production release binaries, not parse-attribution binaries.
It repeats each row three times and records median-ready Mbps/c/B data.

```bash
/bin/bash <<'BASH'
set -euo pipefail
ROOT=/Users/mkbabb/Programming/bbnf-lang/skinny
OUT=/tmp/skv6-wave2-next
FREQ_HZ=3500000000

ROWS=(
  "twitter|12000|crates/test-fixtures/corpus/json/twitter.json"
  "citm_catalog|4000|crates/test-fixtures/corpus/json/citm_catalog.json"
  "canada|3000|crates/test-fixtures/corpus/json/canada.json"
  "apache_builds|40000|test_data/apache_builds.json"
  "github_events|80000|test_data/github_events.json"
  "update_center|8000|test_data/update-center.json"
  "mesh|8000|test_data/mesh.json"
  "random|12000|test_data/random.json"
  "gsoc-2018|3000|test_data/gsoc-2018.json"
  "marine_ik|2000|test_data/marine_ik.json"
  "instruments|25000|test_data/instruments.json"
  "numbers|50000|test_data/numbers.json"
  "unicode_mixed|8000|test_data/unicode_mixed.json"
  "unicode_escapes|8000|test_data/unicode_escapes.json"
  "unicode_basic|8000|test_data/unicode_basic.json"
  "distinct_values|20000|test_data/distinct_values.json"
  "y_string_unicode|80000|test_data/y_string_unicode.json"
)

run_set() {
  local tag="$1"
  local bin="$2"
  mkdir -p "$OUT/$tag"
  local csv="$OUT/$tag-profile-lazy.csv"
  printf 'tag,rep,row,iters,bytes,mbps,cpb,stderr\n' > "$csv"
  for rep in 1 2 3; do
    for spec in "${ROWS[@]}"; do
      IFS='|' read -r row iters relpath <<< "$spec"
      stderr="$OUT/$tag/${row}.r${rep}.stderr"
      "$bin" "$iters" "$ROOT/$relpath" > /dev/null 2> "$stderr"
      bytes=$(sed -nE 's/.*fixture size = ([0-9]+) bytes.*/\1/p' "$stderr")
      mbps=$(sed -nE 's/.* -> ([0-9.]+) Mbps.*/\1/p' "$stderr")
      cpb=$(awk -v m="$mbps" -v f="$FREQ_HZ" \
        'BEGIN { printf "%.6f", f / (m * 1000000.0 / 8.0) }')
      printf '%s,%s,%s,%s,%s,%s,%s,%s\n' \
        "$tag" "$rep" "$row" "$iters" "$bytes" "$mbps" "$cpb" "$stderr" >> "$csv"
    done
  done
}

run_set base "$OUT/base-release/release/profile-lazy"
run_set cand "$OUT/cand-release/release/profile-lazy"
BASH
```

Compare medians:

```bash
python3 - <<'PY'
import csv, statistics
from pathlib import Path
OUT = Path("/tmp/skv6-wave2-next")
rows = {}
for tag in ("base", "cand"):
    with open(OUT / f"{tag}-profile-lazy.csv") as f:
        for r in csv.DictReader(f):
            rows.setdefault(r["row"], {}).setdefault(tag, []).append(float(r["cpb"]))
print("row,base_cpb,cand_cpb,delta_cpb_pct")
for row in sorted(rows):
    b = statistics.median(rows[row]["base"])
    c = statistics.median(rows[row]["cand"])
    print(f"{row},{b:.6f},{c:.6f},{((c-b)/b)*100:.2f}")
PY
```

### 3. Parse-attribution samply profiles

Run this only for mandatory attribution rows plus rows with >=2.0% c/B movement
from the smoke step. This profile is for attribution; the production c/B smoke
is the performance signal.

```bash
/bin/bash <<'BASH'
set -euo pipefail
ROOT=/Users/mkbabb/Programming/bbnf-lang/skinny
OUT=/tmp/skv6-wave2-next

ATTR_ROWS=(
  "unicode_mixed|8000|test_data/unicode_mixed.json"
  "gsoc-2018|3000|test_data/gsoc-2018.json"
  "y_string_unicode|80000|test_data/y_string_unicode.json"
  "canada|3000|crates/test-fixtures/corpus/json/canada.json"
  "instruments|25000|test_data/instruments.json"
  "twitter|12000|crates/test-fixtures/corpus/json/twitter.json"
)

run_profiles() {
  local tag="$1"
  local bin="$2"
  mkdir -p "$OUT/$tag-profiles"
  for spec in "${ATTR_ROWS[@]}"; do
    IFS='|' read -r row iters relpath <<< "$spec"
    samply record --rate 4000 --main-thread-only --unstable-presymbolicate \
      --save-only --no-open \
      -o "$OUT/$tag-profiles/${row}.profile.json.gz" \
      "$bin" "$iters" "$ROOT/$relpath" \
      > "$OUT/$tag-profiles/${row}.samply.stdout" \
      2> "$OUT/$tag-profiles/${row}.samply.stderr"
  done
}

run_profiles base "$OUT/base-attr/release/profile-lazy"
run_profiles cand "$OUT/cand-attr/release/profile-lazy"
BASH
```

### 4. PC/cycles-per-byte attribution

Use attributed c/B, not wrapper percentage alone:

`attributed_cpb(symbol_or_pcbin) = row_median_cpb * self_sample_share`.

For every row with >=2.0% movement, compare baseline and candidate attributed
c/B by both symbol and 64-byte PC bins inside hot wrapper symbols. This avoids
declaring "match_string_at_quote is high" as a diagnosis.

```bash
python3 - <<'PY'
import bisect, csv, gzip, json, statistics
from collections import defaultdict
from pathlib import Path

OUT = Path("/tmp/skv6-wave2-next")
WRAPPERS = {
    "runtime::generated_json::generated::match_string_at_quote",
    "runtime::generated_json::generated::match_tiny_plain_string",
    "runtime::generated_json::generated::match_number_at_digit",
    "runtime::generated_json::generated::consume_container_next",
    "runtime::generated_json::generated::parse_key_colon",
}

def med_cpb(tag):
    out = {}
    with open(OUT / f"{tag}-profile-lazy.csv") as f:
        for r in csv.DictReader(f):
            out.setdefault(r["row"], []).append(float(r["cpb"]))
    return {k: statistics.median(v) for k, v in out.items()}

CPB = {"base": med_cpb("base"), "cand": med_cpb("cand")}

def resolver(profile, syms):
    strings = syms["string_table"]
    tables = {}
    for d in syms["data"]:
        entries = sorted(d["symbol_table"], key=lambda s: s["rva"])
        starts = [s["rva"] for s in entries]
        ends = [s["rva"] + s["size"] for s in entries]
        names = [strings[s["symbol"]] for s in entries]
        tables[d["debug_name"]] = (starts, ends, names)
    libs = [lib["name"] for lib in profile["libs"]]
    t = profile["threads"][0]
    resource_lib = t["resourceTable"]["lib"]
    func_resource = t["funcTable"]["resource"]
    frame_func = t["frameTable"]["func"]
    frame_addr = t["frameTable"]["address"]
    def resolve(frame):
        addr = frame_addr[frame]
        func = frame_func[frame]
        lib = libs[resource_lib[func_resource[func]]]
        tab = tables.get(lib)
        if not tab:
            return f"{lib}!0x{addr:x}", addr, None
        starts, ends, names = tab
        i = bisect.bisect_right(starts, addr) - 1
        if i >= 0 and addr < ends[i]:
            return names[i], addr, addr - starts[i]
        return f"{lib}!0x{addr:x}", addr, None
    return resolve

def self_counts(tag, row):
    prof_path = OUT / f"{tag}-profiles" / f"{row}.profile.json.gz"
    syms_path = OUT / f"{tag}-profiles" / f"{row}.profile.json.syms.json"
    with gzip.open(prof_path, "rt") as f:
        profile = json.load(f)
    with open(syms_path) as f:
        syms = json.load(f)
    res = resolver(profile, syms)
    t = profile["threads"][0]
    stacks = t["stackTable"]
    samples = t["samples"]
    weights = samples.get("weight") or [1] * samples["length"]
    sym = defaultdict(float)
    pcbin = defaultdict(float)
    total = 0.0
    for stack, weight in zip(samples["stack"], weights):
        if stack is None:
            continue
        total += weight
        frame = stacks["frame"][stack]
        name, _addr, off = res(frame)
        sym[name] += weight
        if name in WRAPPERS and off is not None:
            pcbin[f"{name}+0x{(off // 64) * 64:x}"] += weight
    return total, sym, pcbin

mandatory = ["unicode_mixed", "gsoc-2018", "y_string_unicode", "canada", "instruments", "twitter"]
print("row,kind,name,base_pct,cand_pct,base_attr_cpb,cand_attr_cpb,delta_attr_cpb")
for row in mandatory:
    for kind in ("sym", "pcbin"):
        bt, bs, bp = self_counts("base", row)
        ct, cs, cp = self_counts("cand", row)
        bmap = bs if kind == "sym" else bp
        cmap = cs if kind == "sym" else cp
        keys = set(bmap) | set(cmap)
        ranked = sorted(
            keys,
            key=lambda k: (bmap.get(k, 0) / bt + cmap.get(k, 0) / ct),
            reverse=True,
        )[:20]
        for k in ranked:
            bpct = bmap.get(k, 0) / bt
            cpct = cmap.get(k, 0) / ct
            ba = CPB["base"][row] * bpct
            ca = CPB["cand"][row] * cpct
            if max(bpct, cpct) >= 0.01:
                print(f"{row},{kind},{k},{bpct*100:.2f},{cpct*100:.2f},{ba:.6f},{ca:.6f},{ca-ba:.6f}")
PY
```

### 5. Final Criterion gate

Only after the smoke and attribution thresholds below pass:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
CARGO_TARGET_DIR=/tmp/skv6-wave2-next/cand-bench \
  cargo xtask bench-json --advisory
```

This command writes `skinny/RESULTS.md`; run it only when updating the working
tree with candidate evidence is intended.

## Accept / Reject Thresholds

### Smoke c/B threshold

Use median c/B across the three production `profile-lazy` repetitions.

Proceed to attribution only if:

- At least two of `unicode_mixed`, `gsoc-2018`, and `y_string_unicode` improve
  by >=5.0% c/B in the smoke.
- No retained row regresses by >2.0% c/B.
- `canada` and `instruments` do not regress by >1.5% c/B. If either is between
  1.0% and 1.5% worse, keep going only if attribution profiles explain the
  loss at PC level.

Reject immediately if any retained row regresses by >2.0% c/B, even if target
rows improve. Candidate 2 would have been stopped here once `canada` and
`instruments` were included.

### Attribution threshold

A profile is usable only if each baseline/candidate row has at least 8000
samples. If not, rerun with higher iters.

For each row whose c/B changes by >=2.0%:

- The claim must be expressed as attributed c/B delta, not as wrapper share.
- At least 50% of the row-level c/B gain or loss must be accounted for by a
  changed symbol or by 64-byte PC bins inside the wrapper.
- If the candidate introduces a primitive and the primitive is meant to be the
  hot improvement, it must either appear as a separate noinline symbol in the
  parse-attribution profile or have a named PC-bin region under the wrapper.
- A report that says only "`match_string_at_quote` is still 60-70%" is
  inconclusive. That was the Candidate 2 wrapper-symbol failure mode.

### Final gate threshold

The final `bench-json --advisory` result is the authority.

Accept a retained-parse Wave 2 candidate only if:

- At least two of `unicode_mixed`, `gsoc-2018`, and `y_string_unicode` improve
  by >=10.0% Track 1 Mbps in the full Criterion matrix.
- No retained parse row regresses by >2.0% Track 1 Mbps.
- `canada` and `instruments` do not regress by >1.5% Track 1 Mbps.
- Correctness/parity rows remain PASS.
- If the candidate touches shared `parse-that-regex` code consumed by direct
  parsing, direct Track 1 rows must also have no >2.0% regression in the full
  matrix, even though existing N-direct rows remain red for older reasons.

Reject or revert if any hard regression threshold fails, even when target rows
improve.

## Summary Recommendation

The next candidate should not be judged by a narrow focused toggle run or by
aggregate parse-attribution wrapper percentages. The ritual should be:

1. Production `profile-lazy`, all 17 retained rows, three repetitions, median
   c/B.
2. Parse-attribution `samply` only on mandatory target/sentinel rows plus any
   row with >=2.0% movement.
3. Attribute deltas by c/B share at symbol and PC-bin granularity.
4. Full `bench-json --advisory` only after the smoke and attribution checks
   pass.

This keeps the fast loop cheap enough for Wave 2 iteration while preventing a
repeat of Candidate 2's missed `canada` / `instruments` regressions and its
wrapper-symbol attribution ambiguity.
