# SK-V5 B2 — Direct-to-Struct Attribution (11 failing rows)

Date: 2026-05-13.
Author: orchestrator (B2 profiling agent).
Profile inputs: `/tmp/skv5-B2-profiles/*.track1.json.gz` (samply 0.13.1, rate
4000, main-thread-only, presymbolicated). Binary:
`/tmp/skv5-cargo/B2/release/profile_direct` built from
`skinny/crates/bbnf-bench/src/bin/profile_direct.rs`.

## 0. Headline

The direct-to-struct workload's eleven failing rows split into two
substrate-level pathologies, both rooted in the **bench-private
`SinkParser`** at `skinny/crates/bbnf-bench/src/direct_struct.rs:204-353`,
not in any generated runtime path. Both `track1_digest` and `track2_digest`
funnel into `sink_only_digest` (`lib.rs:150-156`). The generated runtime
under `runtime::grammars::json` has no `SinkOnly` entry-point yet.

Single largest-impact primitive to land first: **Eisel–Lemire (or any
fast-float) for exact-number materialization**. Four rows (numbers, canada,
mesh, marine_ik) spend 24–64% inclusive time inside `serde_json::parse_number
+ serde_number_digest`. The `bbnf-simd::x86_64::avx_ifma::mantissa`
implementation is still `unimplemented!("Wave 6: vpmadd52luq …")`; a scalar
Eisel–Lemire would already cap the gap on those four rows.

Second largest lever: **NEON SIMD body-scan in
`unescape_json_string`**. Today (`parse-that-regex/src/lib.rs:624-629`) the
non-escape continuation walks one codepoint at a time via `str::chars()`
even when the SIMD body-scan was already used to detect the first escape.
This re-walks the entire string after every escape.

The bench-private direct path must be **demolished now** (SK-V4 already
demotes it). The current Track 1 column measures the bench's hand parser,
not the generated `SinkOnly` lowering of BIR `DirectBuild`.

## 1. Per-row attribution

Self-time (leaf-frame) attribution; inclusive numbers in §1.1. Throughput
column is the in-binary cold-loop throughput measured by `profile_direct`
during the samply capture; this is not the gate Mbps but it ranks the rows.

| Corpus | Mbps (Track 1) | Top leaf 1 | Top leaf 2 | Top leaf 3 | Classification | Projected fix |
|---|---:|---|---|---|---|---|
| twitter | 4094 | SinkParser::string 59.15% | SinkParser::value 21.07% | SinkParser::object 17.10% | STRING (ASCII-heavy body scan) | NEON body-scan + escape-boundary fast path |
| random | 2398 | SinkParser::string 66.25% | SinkParser::value 18.70% | SinkParser::object 15.04% | STRING + UNICODE (Cyrillic high-byte exits SIMD loop) | NEON body-scan w/ in-loop UTF-8 validation (no scalar bail) |
| unicode_mixed | 1356 | SinkParser::string 82.47% | SinkParser::value 12.18% | SinkParser::object 2.45% | UNICODE (mixed Latin-1 / CJK / emoji + \\u escapes) | NEON body-scan + TBL hex decode + surrogate-pair pipeline |
| unicode_basic | 1779 | SinkParser::string 72.67% | SinkParser::value 22.78% | SinkParser::object 4.53% | UNICODE (multibyte UTF-8 body) | NEON body-scan with in-loop UTF-8 width detection |
| numbers | 3890 | SinkParser::value 66.84% | serde_json::parse_number 19.91% | serde_number_digest 10.28% | NUMBER (pure floats) | Eisel–Lemire fast path + raw-digest |
| canada | 4383 | serde_json::parse_number 50.27% | SinkParser::value 36.24% | serde_number_digest 9.74% | NUMBER (long-precision floats) | Eisel–Lemire fast path |
| mesh | 4318 | SinkParser::value 73.65% | serde_json::parse_number 15.48% | serde_number_digest 9.19% | NUMBER + dispatch (float-heavy array) | Eisel–Lemire + flat numeric-array fast path |
| unicode_escapes | 3320 | SinkParser::string 53.50% | decode_json_unicode_escape 21.61% | SinkParser::value 14.35% | UNICODE (escape-decode bound) | NEON \\uXXXX hex decode + escape-boundary pipeline |
| gsoc | 4207 | SinkParser::string 52.23% | SinkParser::value 41.84% | SinkParser::object 3.28% | STRING (small ASCII strings, tight dispatch) | NEON body-scan + dispatch-hub fusion |
| marine_ik | 5186 | SinkParser::value 64.71% | serde_json::parse_number 13.49% | serde_number_digest 9.06% | NUMBER + dispatch (float arrays, short string keys) | Eisel–Lemire + flat numeric-array fast path |
| y_string_unicode | 2696 | SinkParser::string 19.14%* | decode_json_unicode_escape 14.34%* | unescape_uxxxx_neon 2.18% | UNICODE (surrogate-pair-heavy, tiny corpus) | NEON \\uXXXX TBL + surrogate-pair pipeline |

\* `y_string_unicode` is a 35 KiB corpus; the 50 000-iter loop pushes
sampling overhead high — `mach_absolute_time` shows 33.84% self-time. The
relative ordering of the application leaves is unchanged.

### 1.1 Inclusive-time backbone

Every row sits under `bbnf_bench::direct_struct::sink_only_digest` at
99.99% inclusive — confirming that nothing escapes into the generated
runtime. The hot inclusive-time leaves per pathology:

```
STRING / UNICODE rows
  twitter        SinkParser::string  59.59% incl
  random         SinkParser::string  66.25%
  unicode_mixed  SinkParser::string  82.79%
  unicode_basic  SinkParser::string  72.67%
  unicode_escapes SinkParser::string 73.94%  decode_json_unicode_escape 24.52%
  y_string_unicode SinkParser::string 44.46%  decode_json_unicode_escape 16.52%
  gsoc           SinkParser::string  52.60%

NUMBER rows (inclusive in serde_number_digest)
  numbers        serde_number_digest 33.16%
  canada         serde_number_digest 63.71%   parse_number 50.27%
  mesh           serde_number_digest 26.31%
  marine_ik      serde_number_digest 24.80%
```

`canada` is the only row where the **leaf** is serde_json::parse_number
rather than SinkParser::value — its floats are long enough that the
parse_number subroutine outweighs the dispatch hub.

## 2. Classification (7 + 4)

```
STRING / UNICODE  (7 rows)   NUMBER (4 rows)
  twitter                       numbers
  random                        canada
  unicode_mixed                 mesh
  unicode_basic                 marine_ik
  unicode_escapes
  gsoc
  y_string_unicode
```

This matches the A5 reassay split (7 string-bound + 4 number-bound) and
the hypothesis stated in the SK-V5 charter. No row attributes primarily to
DISPATCH, TAPE, SINK-WRITE, or ALLOCATOR. Within STRING, the structure
factor matters:

- **Body-scan-bound** (no escape, just delimiter search + UTF-8 walk):
  twitter, random, gsoc, unicode_basic.
- **Escape-decode-bound** (mostly `\\X` short escapes):
  unicode_escapes.
- **Mixed body + escape**:
  unicode_mixed, y_string_unicode.

## 3. Dominant blocker — leaf code

### STRING rows — SinkParser::string

The hot inner code is `direct_struct.rs:291-305`:

```rust
fn string(&mut self) -> Result<Cow<'a, str>, DirectStructError> {
    let span = match_json_string_at_quote(self.bytes, self.cursor)?;
    let raw = unsafe { std::str::from_utf8_unchecked(...) };
    self.cursor = span.raw_end;
    if span.needs_unescape {
        unescape_json_string(raw)
    } else {
        Ok(Cow::Borrowed(raw))
    }
}
```

For `needs_unescape == false` (twitter, gsoc, much of unicode_basic) the
hot path is *only* `match_json_string_at_quote` →
`match_string_at_quote` (`parse-that-regex/src/lib.rs:294-347`) which on
aarch64 calls `bbnf_simd::aarch64::string_block::scan_string_special_block`
(16-byte NEON) but immediately leaves the SIMD loop on the **first
non-ASCII byte** (`0x80..=0xff` arm, line 331) and falls into
`validate_utf8_codepoint`. Cyrillic, CJK, emoji corpora therefore alternate
SIMD/scalar at every codepoint, neutralizing the SIMD speedup.

For `needs_unescape == true` (unicode_escapes, unicode_mixed) the parser
calls `unescape_json_string` which **rescans the entire string from byte 0**
without using SIMD (`parse-that-regex/src/lib.rs:566-635`), driving each
codepoint through `raw_content[cursor..].chars().next()`.

### UNICODE-escape rows — decode_json_unicode_escape

`parse-that-regex/src/lib.rs:362-418`. Today's scalar
`read_hex_unit_with_error_offset` (4-byte unrolled hex parse) is the leaf.
There is a NEON `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon`
that already exists (visible at 2.18% on y_string_unicode and 2.90% on
unicode_escapes) but it is only used inside `unescape_json_string` after
the slash is found — the body that finds slashes is still scalar between
SIMD batches.

### NUMBER rows — serde_number_digest → serde_json::parse_number

`direct_struct.rs:530-545` calls `serde_json::from_str::<serde_json::Number>(raw)`
**once per parsed number** — re-parsing the same bytes the JSON tokenizer
already walked. Internally serde routes through `lexical-core` for floats.
The cost is:

1. Re-tokenize ASCII digits.
2. Allocate a `serde_json::Number` (small but real).
3. Discriminate i64 / u64 / f64.
4. Dispatch into `lexical-parse-float` for the f64 case.

Both the redundant tokenize and the absence of an in-line Eisel–Lemire are
the blockers. The `bbnf-simd` AVX-IFMA path is still `unimplemented!()`
(see `crates/bbnf-simd/src/x86_64/avx_ifma/mantissa.rs:37`); the aarch64
side has no Eisel–Lemire path at all.

## 4. Projected fix (per primitive)

| Primitive | Affects | Today's cost | Projected after-fix |
|---|---|---|---|
| Eisel–Lemire fast f64 (scalar baseline + NEON or VPMADD52) | numbers, canada, mesh, marine_ik | ~25–50% inclusive | ~5–10% inclusive |
| NEON body-scan with in-loop UTF-8 width | random, unicode_basic, unicode_mixed (body), twitter (ASCII) | 16-byte block aborts on every high byte | continuous SIMD walk; 2–3× string-bound throughput |
| NEON \\uXXXX TBL hex decode + surrogate pipeline | unicode_escapes, unicode_mixed, y_string_unicode | scalar `decode_json_unicode_escape` 14–22% leaf | sub-3% leaf |
| Drop redundant re-tokenize in `serde_number_digest` (use `JsonNumberMatch.is_integer` + raw bytes directly) | numbers, canada, mesh, marine_ik | duplicate ASCII walk | one-pass digest |
| `unescape_json_string` body fast path (SIMD scan to next `\` instead of `.chars()`) | unicode_escapes, unicode_mixed, y_string_unicode | per-codepoint scalar walk | constant + ~16 B/cycle |

## 5. Track 1 vs Track 2 divergence

In the current binary **Track 1 and Track 2 are byte-for-byte the same code
path** — both `track1_digest` and `track2_digest` immediately call
`sink_only_digest` (`direct_struct.rs:150-156`). There is no measurable
divergence because there is no second implementation. The
`track1_view_walk_digest` / `track2_view_walk_digest` functions (which would
exercise the generated runtime + tape view) are not what the bench gate or
this profile binary exercise.

Consequence: **the codegen overhead claim cannot be tested with this
binary as it stands**. Promoting Track 1 to the generated SinkOnly path is
a prerequisite for any Track 1 / Track 2 divergence measurement.

## 6. Bench-private direct parser status

Per SK-V4 (restart/skinny/tranches/SOTA-BEAT-DESIGN, BackendShape lock 1 +
ARCHITECTURE.md:944), `SinkOnly` is one of the five `BackendShape`
variants and is the *intended* lowering for `BIR::DirectBuild` on rules
with a struct-like shape. The codegen crate has **no `SinkOnly` emitter
yet** (`grep -rn "SinkOnly" skinny/crates/codegen/src/` returns nothing).
Until that emitter lands and the bench is rewired to call it through
`runtime::grammars::json::parse_sink` (or equivalent), the bench-private
`SinkParser` is the de-facto Track 1 — and we are measuring the bench
parser, not the language.

| Row | Current Track 1 call path | Bench-private? | Intended generated path |
|---|---|---|---|
| all 11 | `profile_direct → run_once → track1_digest → sink_only_digest → SinkParser::value` | yes | `runtime::grammars::json::parse_sink<JsonDirectDigest>` lowered from BIR `DirectBuild(JsonRoot)` with field slots + `parse-that-regex` primitive calls inlined per shape |

The intended generated path would *still* call `match_json_string_at_quote`
and `match_json_number_from_first` (those are the substrate primitives),
but the structural dispatch hub (the equivalent of `SinkParser::value`
which is at 18–73% leaf time on every row) would be a flat
codegen-generated state machine with the digest writes inlined per rule.

The dispatch hub overhead alone (`SinkParser::value` and
`SinkParser::object` self-time, summed) is:

```
twitter         38.17%   gsoc           45.12%
random          33.74%   marine_ik      71.29%   (object-heavy)
unicode_mixed   14.63%   numbers        66.84%
unicode_basic   27.31%   canada         36.24%
unicode_escapes 14.35%   mesh           73.65%
y_string_unicode 8.08%
```

Numeric rows (mesh, marine_ik, numbers) and dispatch-heavy rows
(marine_ik) are dominated by the hub. Even before any primitive lands, a
codegen-lowered `DirectBuild` that **inlines the digest into the dispatch
table** would erase a big share of that hub time. The bench-private hub is
a generic Rust function with one match per byte; the codegen-lowered hub
would per-rule dispatch directly to the typed digest write.

## 7. Numbers row deep dive

Throughput today: 3890 Mbps (33% of sonic-rs direct per the SK-V4 gate
table). Leaf attribution (self-time, 30 852 samples):

```
SinkParser::value             66.84%   <- dispatch + match_json_number call
serde_json::parse_number      19.91%   <- redundant re-tokenize + classify
serde_number_digest           10.28%   <- alloc + conversion of serde_json::Number
serde_json::parse_integer      2.95%
```

Inclusive: `serde_number_digest` 33.16%, `parse_number` 19.93%,
`parse_integer` 2.95%.

The corpus is 96.7% scalar floats in `[0,1)` with 11–12 digits of
mantissa — exactly the population Eisel–Lemire's 96% fast-path hit-rate
paper targets (per Lemire & Eisel 2021, the slow path triggers for ≈4% of
inputs in real-world JSON; in this corpus the fast path will hit on
≥99% of records since all values are short and bounded).

Sonic-rs uses the `sonic-number 0.1.2` crate (see `Cargo.lock`), which
implements Eisel–Lemire with AVX2 and NEON paths. The skinny equivalent at
`crates/bbnf-simd/src/x86_64/avx_ifma/mantissa.rs:37` is still
`unimplemented!("Wave 6: vpmadd52luq 4-lane Eisel-Lemire mantissa
multiply")`. No aarch64 Eisel–Lemire kernel exists.

Projected gap closure if Eisel–Lemire lands as scalar (no SIMD):

- `parse_number` (19.91% self) + `serde_number_digest` (10.28%) collapse
  into a single Eisel–Lemire mantissa multiply + exponent table lookup,
  estimated ≤5% combined.
- 25–30 percentage-point reduction in inclusive number-cost.
- Throughput projection on the numbers row: from 3890 Mbps → ≈5500–6000
  Mbps, closing ≈half of the 33% → 100% sonic gap on this row alone.
- A SIMD Eisel–Lemire (NEON `vqdmulh` lane multiply or VPMADD52) buys
  another ≈25–35%; expected ≈7500–8000 Mbps if dispatch hub is also
  flattened by codegen.

Caveat: confidence is medium. The dispatch hub (`SinkParser::value` at
66.84% self) hides the actual number-call cost. A no-inline build that
forces `match_json_number_from_first` to be its own symbol would let us
attribute that 66.84% between the dispatch byte-match and the number
primitive itself.

## 8. unicode_mixed deep dive

Throughput today: 1356 Mbps (50% of sonic direct). Leaf attribution
(37 624 samples):

```
SinkParser::string  82.47%
SinkParser::value   12.18%
SinkParser::object   2.45%
mach_absolute_time   1.92%
```

Inclusive within `SinkParser::string`: split between the body scanner
(`match_json_string_at_quote` → `scan_string_special_block` on aarch64) and
`unescape_json_string`. The sample frame for the body-scan helper is
inlined into `SinkParser::string` (size 4848 bytes — large for an inline
function — implies most callees are folded in).

Corpus: per-record entries alternate ASCII, Latin-1, CJK, and emoji values
plus a small share of `\\u` escapes. The body-scan loop in
`skip_json_string_plain` (`parse-that-regex/src/lib.rs:420-446`) processes
16 bytes at a time on aarch64 until any "interesting" byte (terminator,
backslash, control, or non-ASCII high bit) is found. For Latin-1, CJK, and
emoji that fires every 1–4 bytes, defeating the SIMD width. The fall-back
is `validate_utf8_codepoint` (single-codepoint scalar) followed by
re-entering the SIMD loop one codepoint later.

The corpus is **body-scan-heavy** (most bytes are UTF-8 continuation /
high bytes, not escapes). Sample counts agree: `decode_json_unicode_escape`
appears at 0.31% self in this corpus, not the 21.61% it has in
unicode_escapes.

Projected gap closure:

| Primitive | Expected impact on unicode_mixed |
|---|---|
| Class A — NEON body scan with in-loop UTF-8 width | 1356 → ≈2400 Mbps (≈75% improvement, the SIMD loop survives across multibyte spans) |
| Class B — NEON \\uXXXX TBL decode | small, ~5–10% on this corpus (escapes are rare) |
| Combined A + B | 1356 → ≈2500 Mbps → ≈85–95% of sonic on this row |

Confidence: medium-high. The body-scan claim is grounded in the source
(line 331 of `parse-that-regex/src/lib.rs`); the unicode_escapes
attribution is the orthogonal proof — when escapes dominate, B's primitive
shows up at 21.61% leaf time.

## 9. Wave-style recommendation

### Wave 1 — substrate (lands first, simultaneously)

| Primitive | Code path | Rows lifted | Mbps lift estimate |
|---|---|---|---|
| **Eisel–Lemire scalar f64** (replace `serde_json::from_str` in `serde_number_digest`) | `bbnf-bench/src/direct_struct.rs:530-545` + new `parse-that-regex` `match_and_decode_json_number_f64` | numbers, canada, mesh, marine_ik | +30–50% each |
| **NEON in-loop UTF-8 body scan** (extend `scan_string_special_block` so the loop survives 0x80–0xff bytes when UTF-8 width is consistent) | `parse-that-regex/src/lib.rs:294-347` + `bbnf-simd::aarch64::string_block` | random, unicode_basic, unicode_mixed, twitter | +30–80% each |

### Wave 2 — escape pipeline (lands after Wave 1)

| Primitive | Code path | Rows lifted | Mbps lift estimate |
|---|---|---|---|
| **NEON \\uXXXX TBL hex decode + surrogate-pair fast path** | `parse-that-regex/src/lib.rs:362-418` + `bbnf-simd::aarch64::unescape_uxxxx` | unicode_escapes, unicode_mixed, y_string_unicode | +25–50% on unicode_escapes |
| **SIMD body fast-path inside `unescape_json_string`** (find next `\` via existing body-scan kernel; drop `.chars()`) | `parse-that-regex/src/lib.rs:566-635` | unicode_escapes, unicode_mixed, y_string_unicode, any row with escapes | +10–25% on escape-heavy rows |

### Wave 3 — codegen (lands after substrate)

| Primitive | Code path | Rows lifted | Mbps lift estimate |
|---|---|---|---|
| **`SinkOnly` BIR emitter** (codegen produces `runtime::grammars::json::parse_sink<D: SinkDigest>`; bench rewires Track 1 to that path; bench-private `SinkParser` is deleted) | `crates/codegen/src/` + new `runtime/grammars/json/sink.rs` | all 11 | per-row 5–25% by flattening the dispatch hub; eliminates "wrong Track 1" measurement |

### Single biggest unaddressed lever

**Eisel–Lemire fast-float**. Four rows are number-bound; today there is no
fast-float path anywhere in the workspace except an `unimplemented!()`
AVX-IFMA stub. The substrate cost of `serde_json::parse_number` (19.91%
self on numbers, 50.27% on canada) is pure waste — the JSON tokenizer
already established the digit span; we just re-walk the same bytes through
serde's general number deserializer to extract a typed value.

### Nuke the bench-private direct parser — yes, now

`skinny/crates/bbnf-bench/src/direct_struct.rs:204-353` (the `SinkParser`
struct + `sink_only_digest`) is fundamentally the wrong baseline for SK-V5.
Three reasons to delete it before any further measurement:

1. SK-V4 already declares it disqualified — `SinkOnly` is a BackendShape
   the codegen owes, not the bench.
2. With it in place, the dispatch hub (`SinkParser::value` /
   `SinkParser::object` at 14–73% self) is being measured **as the
   language's direct-to-struct cost** when in fact it is a hand-written
   match statement that the language has never seen. Every fix that goes
   into it is a fix to the bench, not the language.
3. Any primitive optimization (Eisel–Lemire, NEON body scan, TBL hex) that
   lands while the bench-private parser is still on the gate will be
   credited to the bench rather than the language, distorting the
   sonic-rs delta on every subsequent capture.

The replacement is a `SinkOnly` emit in `crates/codegen/src/` that
generates a `runtime::grammars::json::parse_sink<D: SinkDigest>(input:
&str) -> Result<D, ParseError>` function from the existing JSON BBNF, and
a `bbnf-bench` rewire so `track1_digest` calls into it (with
`JsonDirectDigest` implementing `SinkDigest`). Track 2 keeps its
hand-written parser as the "best human can do" comparator; Track 1 must
be the generated path.

## 10. Confidence notes

- **High confidence**: classification of all 11 rows (STRING/UNICODE vs
  NUMBER), the identification of `SinkParser::string` and
  `serde_number_digest` as the dominant leaves, the absence of a
  generated `SinkOnly` path, the Eisel–Lemire gap.
- **Medium confidence**: per-primitive Mbps lift estimates. Held tightly
  by the leaf-time percentages but the dispatch hub still hides 10–30%
  of cost behind a single inlined symbol; a no-inline build would
  refine the estimates by ≈±10%.
- **Lower confidence**: y_string_unicode attribution. The 35 KiB corpus
  pushes sampling overhead (`mach_absolute_time`) to 33% leaf-time,
  compressing the dynamic range. The qualitative classification (UNICODE,
  escape-heavy) is sound but the relative rank of escape-decode vs
  surrogate-pair vs body-scan is uncertain.

## 11. Reproduction

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
export CARGO_TARGET_DIR=/tmp/skv5-cargo/B2
export CARGO_MANIFEST_DIR=/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-bench
cargo build --release -p bbnf-bench --bin profile_direct

mkdir -p /tmp/skv5-B2-profiles
BIN=$CARGO_TARGET_DIR/release/profile_direct
SAM="samply record --rate 4000 --main-thread-only --unstable-presymbolicate --save-only --no-open"

for spec in "twitter 5000" "random 6000" "unicode_mixed 1500" "unicode_basic 1500" \
            "numbers 25000" "canada 2000" "mesh 5000" "unicode_escapes 3000" \
            "gsoc-2018 1000" "marine_ik 2000" "y_string_unicode 50000"; do
  set -- $spec
  name=$1; iters=$2
  out=/tmp/skv5-B2-profiles/${name%-2018}.track1.json.gz
  $SAM -o "$out" $BIN $iters $name track1
done

python3 /tmp/skv5-B2-profiles/attribute.py /tmp/skv5-B2-profiles/<corpus>.track1.json.gz
python3 /tmp/skv5-B2-profiles/inclusive.py /tmp/skv5-B2-profiles/<corpus>.track1.json.gz
```

Scripts `attribute.py` and `inclusive.py` are committed alongside the
profile gzips in `/tmp/skv5-B2-profiles/`.

## 12. Artefacts

```
/tmp/skv5-B2-profiles/twitter.track1.json.gz          (24684 samples, 6.17s wall)
/tmp/skv5-B2-profiles/random.track1.json.gz           (40926 samples, 10.22s)
/tmp/skv5-B2-profiles/unicode_mixed.track1.json.gz    (37624 samples, 9.32s)
/tmp/skv5-B2-profiles/unicode_basic.track1.json.gz    (28573 samples, 7.07s)
/tmp/skv5-B2-profiles/numbers.track1.json.gz          (30852 samples, 7.72s)
/tmp/skv5-B2-profiles/canada.track1.json.gz           (33033 samples, 8.22s)
/tmp/skv5-B2-profiles/mesh.track1.json.gz             (26893 samples, 6.70s)
/tmp/skv5-B2-profiles/unicode_escapes.track1.json.gz  (30500 samples, 7.60s)
/tmp/skv5-B2-profiles/gsoc.track1.json.gz             (25676 samples, 6.33s)
/tmp/skv5-B2-profiles/marine_ik.track1.json.gz        (37054 samples, 9.20s)
/tmp/skv5-B2-profiles/y_string_unicode.track1.json.gz (21119 samples, 5.28s)
```

Each gzip is paired with a `.syms.json` presymbolication sidecar so the
profile is reproducibly attributable without re-resolving.
