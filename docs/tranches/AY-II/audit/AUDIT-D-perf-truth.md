# AUDIT-D — Hard Performance Truth at AY Mid-Tranche Pause

**Run**: 2026-04-21 02:00–02:14 EDT. **HEAD**: `62de21c4` (docs on
`b346ebca` AY.W6 close; no runtime diff). **HW**: Apple arm64, 3.2 GHz
P-core (matches prior AY artefacts). **Allocator**: mimalloc.
**Profile**: `bench` (fat-LTO, CGU=1, debug=true) unless noted.
**Cache**: `.bbnf-cache` cleared each run; `CARGO_BUILD_JOBS=4`.

Hard numbers; every claim cites file + timestamp.

## 1. Trajectory table — cycles/byte per checkpoint

Fixtures (verified `wc -c`): data_s 35,491; twitter 631,514; citm
1,727,204; canada 2,251,051; data_xl 21,281,177. `cyc/byte = ns × 3.2
/ bytes`.

### JSON parse (tape-first, `json_monolithic`) — twitter

| Checkpoint | ns/iter | MB/s | cyc/byte | bytes/cyc | vs sonic (measured) |
|---|---:|---:|---:|---:|---:|
| AU (`3b8b757d`) | 320,995 | 1967 | 1.626 | 0.615 | 0.91× |
| post-AX-W1 (`6516086f`) | — | 448 | 7.126 | 0.140 | 0.21× |
| W1-fix (`64ad2b3e`) | 916,958 | 688 | 4.645 | 0.215 | 0.32× |
| **W4** (`38e3e749`) | 846,470 | 746 | 4.288 | 0.233 | 0.35× |
| W5 (`17691852`) | 1,023,994 | 616 | 5.191 | 0.193 | 0.29× |
| W6 (`b1c7d47a`) | 1,152,054 | 548 | 5.838 | 0.171 | 0.25× |
| **CURRENT** (`62de21c4`) | 1,161,706 | 543 | 5.886 | 0.170 | 0.25× |
| sonic (CURRENT) | 293,139 | 2154 | 1.486 | 0.673 | 1.00× |

Sources: `docs/benchmarks/{post-AU.json,post-AY-W1-bytes-cyc.txt,
post-AY-W4-bytes-cyc.txt,post-AY-W5-bench.txt,post-AY-W6-bench.txt}`,
`/tmp/audit-d-fatlto-json.txt` (02:00).

AU's 1.626 cyc/byte was local max. AX-W1 drifted to 7.126. W1-fix + W4
recovered to 4.288. **W5 + W6 re-regressed to 5.886 — CURRENT twitter
is 3.62× worse than AU, 3.96× worse than sonic.** Sonic ratio widened
from W3's 3.63× to 3.995×.

### Full 5-fixture matrix — AU vs W4 vs CURRENT

| Fixture | AU MB/s | W4 MB/s | CUR MB/s | CUR cyc/B | vs AU | vs W4 |
|---|---:|---:|---:|---:|---:|---:|
| data (35,491 B) | 1746 | 733 | 518 | 6.175 | 0.30× | 0.71× |
| twitter (631,514) | 1967 | 746 | 543 | 5.886 | 0.28× | 0.73× |
| citm (1,727,204) | 2438 | 753 | 515 | 6.209 | 0.21× | 0.68× |
| canada (2,251,051) | 1231 | 363 | 254 | 12.570 | 0.21× | 0.70× |
| data_xl (21,281,177) | 1179 | 474 | 342 | 9.343 | 0.29× | 0.72× |

Every JSON fixture regressed 27–32 % from W4 and sits at 0.21–0.30×
AU. Canada has the worst absolute (12.57 cyc/byte) despite W4's
Eisel-Lemire substrate.

## 2. Sonic / simdjson gap — value-matrix at CURRENT

`/tmp/audit-d-fatlto-jsonvalue.txt` (32 entries, wall 135 s, green).
Three bbnf lanes: `bbnf_*` (raw + full tape walk); `bbnf_value_*`
(`Parsed::to_value()` via `materialize_*` — AY near-parity headline);
`bbnf_visitor_*` (`parse_with_visitor::<V>()`, bypasses tape).

**Twitter at CURRENT (631,514 B)**:

| Lane | ns/iter | MB/s | cyc/byte | vs sonic |
|---|---:|---:|---:|---:|
| `sonic_value_twitter` | 293,545 | 2151 | 1.488 | 1.00× |
| `proto_value_twitter` | 291,439 | 2166 | 1.477 | **0.99×** |
| **`bbnf_visitor_twitter`** | **329,258** | **1917** | **1.669** | **0.89×** |
| `bbnf_twitter` (raw+walk) | 1,717,698 | 367 | 8.706 | 0.17× |
| **`bbnf_value_twitter`** (AY gate) | **1,172,666** | **538** | **5.942** | **0.25× / 3.995× ratio** |

**Full eager matrix** (`bbnf_value` vs `sonic_value`):

| Fixture | bbnf MB/s | sonic MB/s | ratio | `bbnf_visitor` MB/s | visitor ratio |
|---|---:|---:|---:|---:|---:|
| data (35,491) | 509 | 2012 | 3.953 | 2044 | **0.98× (beats)** |
| twitter | 538 | 2151 | 3.997 | 1917 | 1.12× |
| citm | 494 | 2521 | 5.102 | 2721 | **0.93× (beats)** |
| canada | 250 | 1292 | 5.166 | 1261 | 1.03× |
| data_xl | 327 | 1165 | 3.563 | 1286 | **0.91× (beats)** |

**Geomean `bbnf_value / sonic_value` = 4.292×**. AY.W8 gate requires
geomean ≤ 1.20× and twitter ≤ 1.15×. Gap is 3.72× on geomean, 3.48× on
twitter.

**Visitor-lane geomean vs sonic = 0.99×** — already inside the AY.W8
gate (at or below 1.15× on every fixture; beats sonic on 3 of 5).

**simdjson (cite-from-published)**: `json_monolithic_value` does not
link simdjson. Published simdjson-rs on Apple M-series arm64 lands in
3.0–4.0 GB/s on twitter — at 3.0 GB/s twitter ~211 µs, **5.55× faster
than bbnf_value**. AY gates on sonic, not simdjson.

**Sonic drift**: `post-AY-W1-bytes-cyc.txt` cited sonic twitter 2587
MB/s (0.808 bytes/cyc); CURRENT measures 2151 MB/s (0.673). Against
historical 2587, bbnf ratio widens to 4.81×.

## 3. Profile normalization — fat-LTO vs thin-LTO

CURRENT-HEAD comparison, same machine, `.bbnf-cache` cleared each run:

| Fixture | `bench` ns/iter | `profiling-prep` ns/iter | thin vs fat Δ |
|---|---:|---:|---:|
| data_s | 68,450 | 69,023 | −0.8 % |
| twitter | 1,161,706 | 1,181,335 | −1.7 % |
| citm | 3,348,616 | 3,488,351 | −3.9 % |
| canada | 8,833,874 | 8,873,700 | −0.5 % |
| data_xl | 62,127,941 | 63,175,016 | −1.7 % |

Artefacts: `/tmp/audit-d-fatlto-json.txt`, `/tmp/audit-d-prep-json.txt`.

**Fat-LTO buys 0.5–3.9 % over thin LTO on JSON** — less than single-run
variance. Cross-crate helpers (`push_structural`, `push_leaf`,
`set_sib_skip_at`) already `#[inline(always)]` so thin LTO inlines them
and fat has little extra to fold. Compile cost (clean `.bbnf-cache`,
warm target): `bench` 44.87 s; `profiling-prep` 137 s.

## 4. The remaining gap — concrete numbers

### twitter (headline)

| Metric | CURRENT | sonic | AY.W8 gate (≤ 1.15×) |
|---|---:|---:|---:|
| ns/iter (`bbnf_value`) | 1,172,666 | 293,545 | ≤ 337,577 |
| MB/s | 538 | 2151 | ≥ 1871 |
| cyc/byte | 5.942 | 1.488 | ≤ 1.711 |
| ratio vs sonic | 3.995× | 1.000× | ≤ 1.15× |

**ns/iter must drop 3.47×; MB/s must rise 3.48×; cyc/byte must fall
3.47×.**

### canada + citm

- canada gate ≤ 1.20× sonic = ≤ 2,090,609 ns. CURRENT = 8,999,908 ns
  → **4.30× reduction**.
- citm gate ≤ 1.20× sonic = ≤ 822,082 ns. CURRENT = 3,491,199 ns →
  **4.25× reduction**.
- 5-fixture geomean gate ≤ 1.20×; current 4.292× → 3.58× reduction;
  canada at 5.17× is the binding constraint.

### The visitor-lane hinge

`bbnf_visitor_*` already lands at **0.91× – 1.12× sonic on all 5
fixtures**, inside the ≤ 1.15× twitter gate on 3 of 5 and marginally
outside on canada (1.03×) and twitter (1.12×). The visitor lane is
emitted from the same grammar by the same codegen, bypassing the tape.

**The gap is therefore not a cycles-per-byte parse-time floor.** The
parser emits at near-sonic byte cost when the tape-then-walk
reconstruction is skipped. The gap is the default `Parsed::to_value()`
consumer still reconstructs from the generic tape. This matches the
AY internal critique at `audit/AY-critique-path-forward-2026-04-20.md`
§1 (tape-first bias).

## 5. Regen anomaly — CSS + Sheets bench panic at CURRENT HEAD

**Two of four AY close-matrix benches PANIC at current master HEAD**
under `cargo bench --profile bench`:

```
css_l4/bootstrap:
  panicked at crates/tape/src/columns.rs:409:22:
  index out of bounds: the len is 2500 but the index is 2500

google_sheets_monolithic/parse_nested:
  panicked at crates/tape/src/columns.rs:409:22:
  index out of bounds: the len is 26 but the index is 27
```

Artefacts: `/tmp/audit-d-fatlto-css.txt:45`,
`/tmp/audit-d-fatlto-sheets.txt:29`.

Location: `columns.rs:409` is inside `set_sib_skip_at`, called from
`builder.rs:240` in `note_push` whenever an `open_compound` frame is
on the stack with `last_child != u32::MAX`.

**Root cause**: AY.W5.b author documented that Shape-2
(`emit_parse_array_list`, used by CSS stylesheet + BBNF) could NOT be
retargeted to `open_compound` because its per-iter body runs under a
retry-IIFE that `truncate`s `columns_mut()`, and `open_stack` is not
frame-aware of rollback (`PROGRESS.md:560–568`). Post-W5.b this held
because JSON Shape-1 has no retry under an open frame. **W6.c Pratt
outer-compound retarget (`bfadba84`)** reopened the class of rollbacks
under an open frame — CSS bootstrap and Sheets parse_nested now hit
it. Index `2500` / `27` are the post-truncate `records.len()`; the
stale `last_child` sits at that index.

**W5/W6 close artefacts never benched CSS or Sheets under `bench`**.
`post-AY-W5-bench.txt` and `post-AY-W6-bench.txt` both only list 5
JSON fixtures. The W6 close ledger row 6 rationale ("not re-bench'd")
masked this panic.

**bbnf_monolithic works** — BBNF grammars emit Seq/Alt/Rule compounds,
not Shape-1 object/array; retargeted code paths never fire:

bbnf_self 52,208 (98); css_l4_grammar 412,228 (135); css_pretty
15,156 (168); ebnf 27,915 (52); google_sheets 33,512 (223); json
6,905 (77) — all green.

The **separate "W6 regen broke @pretty for BBNF grammar"** incident
is orthogonal: W6's generated.rs delta was reverted to W3b-era state;
that state compiles. This `columns.rs` panic lives in the cross-crate
open/close substrate, not emitted code.

## 6. Samply top-hotspot — CURRENT

`.profiles/samply/audit-d/json_twitter/profile.json.gz` (6,050 samples,
profiling-prep binary, 2026-04-21 02:12). Addresses resolved via `nm`
+ bisect since `--save-only` leaves `frameTable.nativeSymbol=None`.

| % self | Samples | Symbol |
|---:|---:|---|
| **41.21** | 2,493 | `parse_object_JsonParser_object` |
| **37.88** | 2,292 | `<JsonParser>::parse` (inlined body) |
| 11.88 | 719 | `parse_wrap_JsonParser_value` |
| 3.42 | 207 | `parse_array_JsonParser_array` |
| 2.76 | 167 | `std::sys::Once::state` (LazyLock on GRAMMAR_PROFILE) |
| 0.61 | 37 | `core::str::from_utf8` |
| 0.56 | 34 | `parse_that::scan::decode::owned_decode` |
| 0.33 | 20 | `parse_string_escaped` |

### Comparison vs W1-fix samply (688 MB/s, `AYW1-twitter-regression-diag.md` §Post-fix)

| Symbol | W1-fix % | CURRENT % | Δ pp |
|---|---:|---:|---:|
| `<JsonParser>::parse` (inlined) | 55.19 | 37.88 | −17.31 |
| `parse_object_JsonParser_object` | 24.12 | **41.21** | **+17.09** |
| `parse_wrap_JsonParser_value` | 11.81 | 11.88 | +0.07 |
| `parse_array_JsonParser_array` | 1.95 | 3.42 | +1.47 |
| `parse_string_escaped` | 3.28 | 0.33 | −2.95 |

**The +17 pp migration into `parse_object` is the W5+W6 regression
signature.** Shape-1 object emission retargeted at W5-B `09ca39d6` (10
`push_compound`→`open_compound`, 9 `mark_children` → implicit
`note_push`). Each key/value push now pays `set_sib_skip_at` +
`or_extra_at` for `SIB_SKIP_STAMPED_BIT` + a frame-stack test in
`note_push`. Twitter's ~80K pushes × per-push cost = observed shift.

`parse_string_escaped` shrunk 3.28 % → 0.33 %: W4.1 SIMD unescape is
firing. This is the sole clear W4 win still visible.

**W7 implication**: the shared-fact optimizer must audit `note_push`
before adding decisions on top of it. The `open_stack.last_mut()` test
is runtime-only — LTO cannot eliminate it. Either fold the frame check
into per-shape specialised emission (grammars with no `open_compound`
consumer skip the test) or inline the `open_frame_active` branch at
the specific push site.

## 7. Verdicts

### Concrete remaining gap (twitter, CURRENT `62de21c4`)

- `bbnf_value_twitter`: **1,172,666 ns, 538 MB/s, 5.942 cyc/byte**.
- `sonic_value_twitter`: **293,545 ns, 2,151 MB/s, 1.488 cyc/byte**.
- **Ratio bbnf/sonic = 3.995×**.
- **AY.W8 gate ≤ 1.15×: gap = 3.47× reduction** (1,172,666 →
  337,577 ns).

### Profile normalization verdict

**Gate AY close on `bench` (fat LTO); iterate on `profiling-prep`
(thin LTO + DWARF).** Fat-LTO buys 0.5–3.9 % on JSON over thin LTO at
current HEAD — within variance. Thin LTO carries samply-usable DWARF.
Fat LTO is the canonical product-binary shape and is what the W8
close numbers should publish as; mid-wave re-benches use
profiling-prep without noise-level throughput loss.

### Is the AY.W8 gate attainable in current substrate?

**CONDITIONALLY YES**, with three conditions, all inside the AY plan
scope:

1. **Default `to_value()` moves onto the visitor-lane emission
   discipline.** `bbnf_visitor_*` already averages 0.99× sonic geomean;
   the codegen substrate exists (W3b emitted 48 `materialize_*` fns).
   W5's open/close indirection undercuts this because the tape still
   carries an intermediate — W7/W8 must route the default consumer to
   the same direct construction the visitor lane uses.
2. **The CSS/Sheets Shape-1-under-retry panic is fixed before any
   close-matrix re-bench.** W5 documented Shape-2 avoidance; W6's
   Pratt outer retarget reopened the class on Shape-1 paths nested
   under retry-IIFEs. Either `open_stack` becomes frame-aware of
   retry truncation, or `close_compound` degrades to `push_compound`
   when `last_child` is invalid.
3. **Samply attribution on CURRENT confirms the note_push hotspot
   location before W7 lands optimizer levers.** This audit ran the
   samply; the +17 pp migration into `parse_object` is the confirmed
   target.

If those three land in W7–W8, close gates are attainable in current
substrate without reopening AY as a rewrite. If the CSS/Sheets panic
is left unaddressed and the tape-first default path persists, the W8
gate is **NOT attainable as declared** and AY must re-plan.

## Verification artefacts

- `/tmp/audit-d-fatlto-{json,css,sheets,bbnf,jsonvalue}.txt` — fat-LTO
  bench captures (2026-04-21 02:00–02:07).
- `/tmp/audit-d-prep-json.txt` — profiling-prep comparison (02:12).
- `.profiles/samply/audit-d/json_twitter/profile.json.gz` —
  CURRENT samply (6,050 samples, 02:12).
- `docs/benchmarks/post-AU.json`, `post-AY-W1-close.json`,
  `post-AY-W1-bytes-cyc.txt`, `post-AY-W3-value.json`,
  `post-AY-W4-close.json`, `post-AY-W4-bytes-cyc.txt`,
  `post-AY-W5-bench.txt`, `post-AY-W6-bench.txt` — trajectory sources.
- `crates/tape/src/builder.rs:236-248` (note_push), `columns.rs:405-411`
  (set_sib_skip_at) — panic-site source for §5.
