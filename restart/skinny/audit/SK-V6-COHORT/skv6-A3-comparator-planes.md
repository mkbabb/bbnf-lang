# SK-V6 Research Cohort A3 - Comparator Planes

Date: 2026-05-15
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Scope: read-only research. No repository files edited.

## Executive Finding

The current bench/report stack is close to honest plane disclosure, but it is
not yet strong enough for a strict SOTA-beat claim. `skinny/RESULTS.md` exposes
strictness/output-plane columns, `restart/skinny/BENCH.md` names Rust and
native comparator planes, and `restart/MASTER-PLAN.md` splits retained parse,
direct digest, and representative typed output rows. The gap is that these
planes are still partly prose and partly hard-coded Markdown, not row metadata.

Two corrections are binding:

1. The current local `sonic-rs` dependency is `=0.5.8` with
   `features = ["sort_keys", "utf8_lossy"]`, while `BENCH.md` still documents
   `=0.5` and only `sort_keys`. Local `sonic-rs` source shows the
   `utf8_lossy` feature can cause `from_slice` to run a lossy deserializer.
   Therefore the current report line saying `sonic-rs` rows are strict is not
   defensible unless the feature is removed or a row-local flaw probe proves
   that the measured API rejects invalid UTF-8 and invalid surrogate escapes.
2. BBNF retained rows are currently `&str` rows. Invalid UTF-8 is rejected
   outside the hot parser/view boundary, so these rows should stay classified
   as `strict_after_utf8_view` or `deferred_utf8`, not `strict_bytes`, until a
   timed byte-entry row includes UTF-8 validation in the measured scope.

## Local Authority Read

`skinny/RESULTS.md` currently reports:

- Overall outcome: `N-direct / NoGo`.
- Retained parse/tape: 17 corpora, with strictness rendered as `deferred`,
  parse UTF-8 rendered as `view-boundary`, output plane rendered as
  `typed_root_over_offset_tape vs competitor DOM`.
- Workloads: `direct_to_struct` digest stressor and representative
  `real_typed_struct` rows. The direct digest rows compare against
  `sonic-rs` typed serde and `serde_json`; the representative typed rows
  compare generated typed DirectBuild output against sonic-rs/serde_json over
  matching Rust structs.
- Notes: Track 1 is `runtime::generated_json::parse`; Track 2 is independent
  hand-coded runtime/tape. The sidecar note says `sonic-rs`,
  `simd-json`, and `serde_json` are strict / scan-boundary / escape-complete.

`restart/skinny/BENCH.md` currently:

- Defines the two broad comparator groups: Rust in-process rows and native
  SOTA reference rows.
- Documents `sonic-rs = "=0.5"` and `simd-json = "=0.13"`, which no longer
  matches `skinny/crates/bbnf-bench/Cargo.toml` (`sonic-rs = "=0.5.8"`,
  `simd-json = "=0.13.11"`).
- Says `simdjson C++` is both part of the native SOTA reference plane and,
  later, "not in the competitor set for skinny." That needs a split between
  "not a Rust gate row" and "still a native strict DOM reference row."

`restart/MASTER-PLAN.md` currently:

- Keeps SOTA rows in §4 and H tranche rows in §13.
- Correctly separates retained parse rows, the
  `semantic_full_digest_stressor`, and the `real_typed_struct` representative
  DirectBuild row.
- Still uses "SOTA-beat" in ways that mix current Rust in-process anchors,
  stale native sidecar ceilings, and x86-only asmjson. The amendment below
  makes SOTA-beat same-plane and same-strictness only.

## Primary Comparator Facts

### sonic-rs

Primary docs/repos:

- Repository: `cloudwego/sonic-rs`
- Docs/readme: `https://github.com/cloudwego/sonic-rs`
- Local pinned source: Cargo registry `sonic-rs-0.5.8`

Relevant planes:

- `sonic_rs::from_slice::<sonic_rs::Value>(bytes)` is the Rust in-process
  Value/DOM-like row used by the local retained parse gate.
- `sonic_rs::from_slice::<T>(bytes)` is the typed serde direct row used by
  `real_typed_struct` and by the digest stressor sidecar.
- `LazyValue` / `get` / `get_unchecked` are lazy raw-slice and path-access
  planes. They are useful architecture references but are not the retained
  S anchor unless BBNF also measures a lazy path-only workload.

Strictness and caveats:

- Checked `from_slice` without lossy mode is the strict row.
- `_unchecked`, `get_unchecked`, and `LazyValue` shortcuts are not strict
  SOTA anchors.
- The local bench currently enables `utf8_lossy`; local source documents that
  lossy mode replaces invalid UTF-8/UTF-16 with replacement characters. This
  must either be removed or recorded as `lossy_utf8` and excluded from strict
  S anchor selection.
- sonic-rs typed serde can borrow into target structs when the target type
  uses borrowed/Cow fields. Ownership must be recorded per output type, not
  assumed from the API name.

### Rust simd-json

Primary docs/repos:

- Repository: `simd-lite/simd-json`
- Docs: `https://docs.rs/simd-json`
- Local pinned dependency: `simd-json = "=0.13.11"`

Relevant planes:

- `simd_json::to_borrowed_value(&mut bytes)`: borrowed value/DOM row.
- `simd_json::to_owned_value(&mut bytes)`: owned value/DOM row.

Strictness and caveats:

- Treat as strict if invalid UTF-8/control/escape probes pass for the measured
  API and feature set.
- The borrowed API mutates the input buffer and returns values borrowing from
  that mutable buffer. The local Criterion setup clones bytes per iteration in
  `iter_batched`; that clone is outside the timed parse function but the row
  must still be labeled `input_mutability=in_situ_mut`.
- Borrowed and owned rows must never be collapsed into a single "simd-json"
  number in reports.

### simdjson C++

Primary docs/repos:

- Repository/docs: `https://github.com/simdjson/simdjson`
- DOM and On-Demand docs: `doc/basics.md`, `doc/ondemand.md`,
  `doc/ondemand_design.md`

Relevant planes:

- DOM: `dom::parser::parse` builds a materialized DOM/tape after stage 1
  structural indexing. This is a strict native DOM reference plane.
- On-Demand: `ondemand::parser::iterate` exposes an iterator over the document
  backed by the structural index. It is not a full-document materialization
  row unless the workload explicitly traverses/validates the full document.

Strictness and caveats:

- DOM can be used as a strict native DOM reference row when run on exact
  skinny corpora, same machine, same compiler metadata, and current sources.
- On-Demand is a separate advisory plane. It can teach structural-index
  consumption, ownership, and cursor constraints, but it must not become the
  S anchor for BBNF retained DOM/tape unless the measured workload forces the
  same full traversal and validation.
- On-Demand document/string views are lifetime-bound to the parser/input
  buffer. This is not the same ownership contract as an eager `Value` or BBNF
  sealed offset tape.

### yyjson

Primary docs/repos:

- Repository/docs: `https://github.com/ibireme/yyjson`

Relevant plane:

- `yyjson_read_opts` / immutable document DOM is a native C strict DOM row by
  default. The local profiles use it as the no-SIMD i-cache-resident DOM
  ceiling.

Strictness and caveats:

- yyjson default parse is strict RFC 8259-style JSON. It also exposes flags for
  non-standard input such as comments, trailing commas, invalid Unicode, and
  number variants. Any row using those flags must be labeled permissive and
  excluded from strict S anchor selection.
- yyjson values are document-owned. The `yyjson_doc` lifetime and allocator
  behavior must be recorded separately from source-buffer ownership. In-situ
  modes, if used, must be a distinct row.
- Existing yyjson rows are profile-only/stale unless the source checkout and
  exact 17-corpus sidecar are restored and rerun.

### asmjson

Primary docs/repos:

- Docs: `https://docs.rs/asmjson`
- Source: `https://github.com/atomicincrement/asmjson`
- Local note: `skinny/profile/native-sidecars/asmjson/NOTE.md`

What asmjson can be used for:

- x86_64 AVX-512 architectural ceiling and primitive-shape research:
  direct-threaded FSM, mask-held parser state, `tzcnt` event seeking,
  flat tape/SAX emission, and SWAR fallback.
- A permissive flaw probe showing how far a highly specialized byte-classifier
  can go when strict JSON validation is not the same contract.
- Future `CollapsedStage` design input, but only after BBNF adds strict
  validation on top of any asmjson-shaped architecture.

What asmjson cannot be used for:

- It cannot be mixed into strict BBNF/sonic/simdjson/yyjson S anchor selection.
- It cannot prove an Apple Silicon/M5 Max SOTA-beat claim; the headline path is
  AVX-512BW and does not run on arm64.
- Its synthetic `string_array`, `string_object`, and `mixed` benches are not
  the 17 skinny corpora.
- Its SAX/sink plane cannot be compared to DOM/tape/typed output unless the
  row produces the same semantic output and strictness.
- It cannot authorize JSON-specific assembly or generic-crate JSON branches.
  It is a primitive/backend-shape reference, not a shortcut around Lock 14.

## Plane Taxonomy

Use these row-level values. Do not infer them from the library name.

| Plane | Meaning | Examples | Gate use |
|---|---|---|---|
| `retained_tape_typed_root` | BBNF retained offset tape plus typed projections, scalar materialization lazy | Track 1 / Track 2 retained parse | Rust retained gate |
| `dom_value` | Fully materialized generic DOM/value | sonic-rs `Value`, simd-json borrowed/owned value, simdjson C++ DOM, yyjson doc | Same-plane retained DOM/tape comparator if strictness matches |
| `ondemand_cursor` | Lazy cursor over structural index; skipped values may not be parsed/materialized | simdjson On-Demand | Advisory unless full traversal workload is explicit |
| `typed_serde_direct` | Serde-shaped direct output into user struct | sonic-rs `from_slice::<T>`, serde_json `from_slice::<T>` | Representative DirectBuild comparator |
| `generated_typed_directbuild` | Generated BBNF typed output from host/API schema facts | BBNF `real_typed_struct` Track 1 | Representative DirectBuild gate |
| `semantic_full_digest_stressor` | Full semantic digest over every key/string/number/literal | current `direct_to_struct` rows | Strict guard/stressor, not representative typed-output close |
| `sax_sink` | Event sink with no retained document | asmjson SAX | Separate plane only |
| `structural_scan_only` | Structural offsets/masks, no parse tree | BBNF SIMD scan, simdjson stage 1 microbench | Microbenchmark, not parse S anchor |

## Strictness Taxonomy

Use these row-level values.

| Strictness value | Definition | S anchor eligibility |
|---|---|---|
| `strict_bytes` | Timed row accepts bytes and rejects invalid UTF-8, invalid controls, invalid escapes, invalid numbers, trailing junk, and delimiter errors inside the measured parse scope | Eligible |
| `strict_after_utf8_view` | Timed row accepts `&str`; UTF-8 was validated before the measured parser, but JSON syntax/escape strictness is measured | Eligible only against similarly prevalidated comparator rows, or reported as deferred against raw-byte competitors |
| `strict_fullwalk_ondemand` | On-Demand row traverses every value and validates trailing content under the measured workload | Eligible only for an explicit fullwalk On-Demand plane |
| `partial_ondemand` | On-Demand row accesses only selected values; skipped values are not equivalent to full DOM validation | Not eligible for DOM/tape S anchor |
| `lossy_utf8` | Invalid UTF-8/UTF-16 may be replaced or tolerated | Not eligible |
| `permissive` | Non-standard JSON accepted, or known validation gaps | Not eligible |
| `unknown` | Missing feature/API/flaw-probe evidence | Not eligible |

BBNF retained rows are currently `strict_after_utf8_view`, not
`strict_bytes`. Current sonic-rs rows are `unknown` or `lossy_utf8` until the
`utf8_lossy` feature is removed or row probes prove otherwise.

## Ownership / Borrowed Caveats

The report schema should record at least these fields:

- `input_ownership`: `borrowed_immutable`, `borrowed_mut_in_situ`,
  `owned_clone_setup`, `native_padded_string`, `native_doc_owned`.
- `output_ownership`: `borrows_input`, `borrows_mut_input`, `owns_doc_arena`,
  `owns_tape_borrows_input`, `owns_struct`, `cow_mixed`, `sink_only`.
- `input_mutated`: boolean.
- `clone_charged`: boolean.
- `prevalidation_charged`: boolean.
- `api_symbol`: exact function symbol, including `_unchecked`, On-Demand,
  DOM, Value, borrowed, owned, or typed target.

Current caveats:

- BBNF retained output owns a sealed tape but borrows the input `&str`.
- BBNF `real_typed_struct` currently uses Rust structs with `Cow<'a, str>`
  fields. Do not call that "owned typed struct" unless the schema forces owned
  `String` output or the row records `output_ownership=cow_mixed`.
- simd-json borrowed mutates a per-iteration cloned `Vec<u8>` and returns a
  value borrowing from that mutated buffer.
- sonic-rs typed rows may borrow into the target type when the struct permits
  borrowing; the plane is typed direct, but ownership is target-dependent.
- yyjson values are owned by `yyjson_doc`; source buffer lifetime is not the
  same as document value lifetime unless an in-situ mode is used.
- simdjson On-Demand references the parser/input lifetime and advances a
  cursor; it is not random-access DOM ownership.

## What Counts As SOTA-Beat

SOTA-beat must be same strictness, same output/workload plane, same corpus,
same machine class, same benchmark freshness, and same measured scope.

For current SK-V6:

- Rust retained parse S anchor: fastest eligible strict in-process
  `dom_value` row among sonic-rs Value and simd-json borrowed/owned, after the
  sonic-rs strictness issue is fixed or excluded.
- Native strict reference ceiling: yyjson DOM and simdjson C++ DOM rerun on the
  exact 17 corpora. These can size the J/H SOTA-beat target, but stale
  profile-only rows cannot classify the current Rust gate.
- Representative typed DirectBuild S anchor: sonic-rs typed serde over the
  same declared Rust output schema and ownership policy. serde_json typed is a
  floor/control.
- Digest stressor S anchor: sonic-rs/serde_json digest workload only. This is
  a guard/stressor plane; passing or failing it is not the representative
  typed DirectBuild close.
- On-Demand S anchor: none by default. It needs a separately named full-walk
  On-Demand workload before eligibility.
- asmjson S anchor: none for strict Apple Silicon SK-V6. It is x86 AVX-512
  aspirational and permissive/advisory unless a strict BBNF-compatible
  workload is implemented and measured on comparable hardware.

Threshold language should be explicit:

- `parity`: Track 1 time <= `S * 1.10` on the same eligible plane.
- `sota_beat`: Track 1 throughput >= `S * 1.10` (equivalently Track 1 time
  <= `S_time / 1.10`) on the same eligible plane.
- `substrate_beat`: Track 2 time <= `S * 0.95`; this is useful for diagnosing
  substrate headroom but is not a final user-facing SOTA-beat unless Track 1
  also meets the same-plane Track 1 beat threshold.

The current `Outcome::ABeatAndParity` in `gate.rs` is really
`substrate_beat_with_codegen_parity`. It should not be reported as final
SOTA-beat unless Track 1 independently beats the eligible S anchor.

## Exact BENCH.md Amendments

Apply these amendments to `restart/skinny/BENCH.md` when the repo is edited.

### 1. Replace §2 opening with this plane split

```markdown
## §2 Comparator baselines and workload planes

The competitor set is fixed by Lock 8 and extended by the SK-V6 comparator
plane ledger. Rows are comparable only when strictness, output plane,
ownership, corpus, hardware, and measured scope match.

The harness reports five planes:

- **Rust retained DOM/tape plane**: Track 1 retained
  `retained_tape_typed_root`, Track 2 retained, sonic-rs `Value`, Rust
  simd-json borrowed/owned, and serde_json `Value` floor. This is the current
  in-process retained parse gate after strictness filtering.
- **Rust typed direct plane**: generated BBNF typed DirectBuild output,
  independent Track 2 typed oracle, sonic-rs typed serde, and serde_json typed
  serde over the same declared host/API output schema.
- **Semantic full digest stressor plane**: generated and hand-coded BBNF
  digest rows, sonic-rs digest, and serde_json digest. This remains a strict
  guard workload and is not the representative DirectBuild closure plane.
- **Native strict DOM reference plane**: yyjson default DOM and simdjson C++
  DOM, rerun on exact skinny corpora with compiler/commit metadata. These rows
  are reference ceilings and final SOTA-beat targets, not mixed into the Rust
  in-process gate unless a same-run native sidecar is explicitly selected.
- **Advisory / flaw-probe plane**: simdjson On-Demand partial traversal,
  asmjson SWAR/AVX-512 permissive rows, and structural-scan-only rows. These
  guide architecture and primitive design but cannot serve as strict DOM/tape
  S anchors.
```

### 2. Replace §2.1 sonic-rs block

````markdown
### 2.1 sonic-rs

Repository: `cloudwego/sonic-rs`.

Cargo entry in the current skinny harness:

```toml
sonic-rs = { version = "=0.5.8", default-features = false, features = ["sort_keys"] }
```

Rows using `utf8_lossy`, `_unchecked`, `get_unchecked`, or other invalid-input
tolerant paths are `lossy_utf8` / `permissive` advisory rows and are excluded
from strict S anchor selection. If the crate feature `utf8_lossy` is enabled,
the row is ineligible for `strict_bytes` until flaw probes prove the exact API
rejects invalid UTF-8 and invalid surrogate escapes.

APIs:

- `sonic_rs_value_dom`: `sonic_rs::from_slice::<sonic_rs::Value>(bytes)`,
  `plane=dom_value`.
- `sonic_rs_typed_direct`: `sonic_rs::from_slice::<T>(bytes)`,
  `plane=typed_serde_direct`, with `T` and field ownership recorded.
- `sonic_rs_lazy_path`: `LazyValue` / `get` path rows, advisory unless the BBNF
  workload is also lazy path access.
````

### 3. Replace §2.2 simd-json block

````markdown
### 2.2 simd-json (Rust port)

Cargo entry:

```toml
simd-json = { version = "=0.13.11", default-features = false, features = ["serde_impl"] }
```

Rows:

- `simd_json_borrowed`: `simd_json::to_borrowed_value(&mut bytes)`,
  `plane=dom_value`, `input_ownership=borrowed_mut_in_situ`,
  `input_mutated=true`.
- `simd_json_owned`: `simd_json::to_owned_value(&mut bytes)`,
  `plane=dom_value`, `input_ownership=borrowed_mut_in_situ`,
  `input_mutated=true`.

The Criterion clone in `iter_batched` is setup, not timed parse, but the row
must still disclose that the parser receives and mutates a fresh owned buffer.
````

### 4. Add simdjson C++ before yyjson

```markdown
### 2.4 simdjson C++ (native strict DOM and On-Demand advisory)

simdjson C++ is not a Rust in-process gate row, but it is part of the native
reference plane. DOM rows (`dom::parser::parse`) are strict native DOM
reference rows when rerun on exact skinny corpora. On-Demand rows are reported
as `partial_ondemand` unless the workload explicitly traverses every value and
validates trailing content; partial On-Demand rows cannot be S anchors for
BBNF retained DOM/tape.
```

### 5. Replace yyjson block

```markdown
### 2.5 yyjson (native strict DOM, no-SIMD i-cache reference)

yyjson default `yyjson_read_opts` immutable-doc parsing is a strict native DOM
reference row. Rows using permissive flags (comments, trailing commas, invalid
Unicode acceptance, non-standard numbers, or in-situ mutation) must be separate
rows with strictness/ownership recorded. Existing yyjson profile rows are
profile-only until the source checkout is restored and rerun on exact 17
corpora.
```

### 6. Replace asmjson block

```markdown
### 2.6 asmjson (x86 AVX-512 architecture reference / permissive flaw probe)

asmjson is not a strict Apple Silicon S anchor. Its published AVX-512 path is
x86_64-only, its local M5 Max path is SWAR-only, and its current rows are
synthetic and permissive. Use it for `CollapsedStage` architecture research
and primitive-shape ceilings only. Do not mix asmjson rows with strict BBNF,
sonic-rs, simd-json, simdjson C++ DOM, or yyjson DOM S anchor selection unless
a strict, same-corpus, same-plane asmjson-compatible row is implemented and
measured on comparable x86_64 hardware.
```

### 7. Replace the "not in competitor set" subsection

```markdown
### 2.7 What is not a Rust in-process gate row

- `simdjson C++` and `yyjson`: native strict reference rows, not Rust harness
  gate rows.
- `simdjson On-Demand`: advisory unless full-walk workload is explicit.
- `asmjson`: permissive/x86 architecture reference unless strict same-plane
  row is added.
- `lightning-css`: CSS, not JSON.
- `tree-sitter`, `jq`, `oj`, `json-parser-c`: different output or ecosystem
  planes; diagnostic references only.
```

### 8. Replace the competitor configuration table

```markdown
| Row | API symbol | Plane | Strictness | Input ownership | Output ownership | S-anchor eligible |
|---|---|---|---|---|---|---|
| bbnf Track 1 retained | `runtime::generated_json::parse(&str)` | `retained_tape_typed_root` | `strict_after_utf8_view` | borrowed immutable `&str`; prevalidation not timed | owns tape, borrows input | only against prevalidated rows |
| bbnf Track 2 retained | `bbnf_bench::track2::json::parse(&str)` | `retained_tape_typed_root` | `strict_after_utf8_view` | borrowed immutable `&str`; prevalidation not timed | owns tape, borrows input | only against prevalidated rows |
| sonic-rs Value | `sonic_rs::from_slice::<Value>` | `dom_value` | `strict_bytes` only without `utf8_lossy`/unchecked | borrowed immutable bytes | owns Value/arena | yes if strict probes pass |
| simd-json borrowed | `simd_json::to_borrowed_value(&mut bytes)` | `dom_value` | `strict_bytes` | cloned owned buffer, mutable in-situ | borrows mutated buffer | yes |
| simd-json owned | `simd_json::to_owned_value(&mut bytes)` | `dom_value` | `strict_bytes` | cloned owned buffer, mutable in-situ | owned value | yes |
| serde_json Value | `serde_json::from_slice::<Value>` | `dom_value_floor` | `strict_bytes` | borrowed immutable bytes | owned Value | floor only |
| sonic-rs typed | `sonic_rs::from_slice::<T>` | `typed_serde_direct` | `strict_bytes` only without lossy/unchecked | borrowed immutable bytes | target-dependent (`Cow`/owned) | direct plane only |
| bbnf real typed | generated typed DirectBuild entry | `generated_typed_directbuild` | `strict_after_utf8_view` | borrowed immutable `&str` | schema-dependent; record `Cow` vs owned | direct plane only |
| yyjson DOM | `yyjson_read_opts` default | `dom_value` | `strict_bytes` | native row metadata | owns `yyjson_doc` | native reference |
| simdjson C++ DOM | `dom::parser::parse` | `dom_value` | `strict_bytes` | padded native input | owns DOM/tape | native reference |
| simdjson On-Demand | `ondemand::parser::iterate` | `ondemand_cursor` | `partial_ondemand` unless fullwalk | padded native input | cursor borrows parser/input | no by default |
| asmjson DOM/SAX | `parse_to_dom_zmm` / `parse_with_zmm` | `dom_value` or `sax_sink` | `permissive` unless proven strict | x86-only/synthetic unless row-aligned | flat tape or sink | no by default |
```

### 9. Add to §5 metadata schema

```markdown
Row metadata schema v3 adds:

- `api_symbol`
- `plane`
- `strictness`
- `validation_boundary`
- `escape_completeness`
- `flaw_probe`
- `input_ownership`
- `output_ownership`
- `input_mutated`
- `clone_charged`
- `prevalidation_charged`
- `sidecar_freshness` (`current_same_run | current_sidecar | stale_profile_only | published_cross_arch | advisory`)
- `s_anchor_eligible`

Rows missing these fields fail the gate. Markdown rendering must read these
fields from metadata rather than hard-coding `deferred`, `view-boundary`, or
competitor strictness in `report.rs`.
```

## Exact MASTER-PLAN.md Amendments

Apply these amendments to `restart/MASTER-PLAN.md` when the repo is edited.

### 1. Add after the SOTA close-row table in §4

```markdown
SOTA-beat classification is same-plane only. A row may claim SOTA-beat only if
the competitor and BBNF row share strictness, output/workload plane, corpus,
hardware class, benchmark freshness, ownership disclosure, and measured scope.
For retained JSON this means Track 1 throughput must be at least 1.10x the
fastest eligible strict retained DOM/tape S anchor on the same corpus. Track 2
beating the anchor is a substrate-headroom signal, not final SOTA-beat unless
Track 1 also beats the same eligible anchor.

Native `yyjson` and `simdjson C++` DOM rows are final native reference ceilings
only when rerun on exact skinny corpora with current source/commit/compiler
metadata. Stale profile-only rows size the target but do not classify the
current Rust gate. `simdjson` On-Demand and `asmjson` rows are advisory unless
they are made strict, same-plane, full-workload rows.
```

### 2. Replace the `json/direct_to_struct` row text

```markdown
| `json/semantic_full_digest_stressor` | sonic-rs and serde_json digest rows over the same full semantic digest workload, strictness and ownership recorded per row. | The stressor remains correctness-green and visible. It may fail throughput while representative typed DirectBuild passes; remaining misses require falsified REDRESS routes or an explicit decision that no real consumer needs this maximal digest plane. | M5 Max macOS arm64 NEON. | H.W4, J.W1 guard. |
```

### 3. Replace the `json/real_typed_struct` row text

```markdown
| `json/real_typed_struct` (representative DirectBuild gate) | sonic-rs typed serde and serde_json typed serde over the same declared host/API output schema, same field ownership policy (`owned`, `borrowed`, or `Cow`), and same post-parse checksum. | Generated Track 1 must produce typed output from schema-fed DirectBuild facts before checksum. Track 2 is a structurally independent oracle and is reported separately. Broad `serde_json::Value` output is allowed only for fields proven null-only in the checked fixture. Representative close is Track 1 within `sonic-rs * 1.10` time for parity and Track 1 >= `1.10 * sonic-rs` throughput for SOTA-beat on the same typed-output plane. | M5 Max macOS arm64 NEON. | SK-V6 Wave 3 / H.W4. |
```

### 4. Replace H tranche goal paragraph at §13

```markdown
Goal: activate performance recognizers plus typed/direct codegen templates and
per-target SIMD/ASM primitive layers on the Rust line; close the expanded
skinny SOTA gate against same-plane strict rows. The primary close is arm64
Apple Silicon. x86_64 AVX-512 work is a secondary hardware gate. asmjson is an
architecture/flaw-probe reference until a strict same-plane row exists; it is
not an Apple Silicon S anchor.
```

### 5. Replace H.W6 row

```markdown
| H.W6 | **SK-V6 strict matrix target before CSS gates.** Full 17-corpus matrix with row-level `plane`, `strictness`, `api_symbol`, `input_ownership`, `output_ownership`, `prevalidation_charged`, `input_mutated`, `sidecar_freshness`, and `s_anchor_eligible`. Retained parse, representative typed DirectBuild, semantic digest stressor, native strict DOM references, On-Demand advisory rows, and asmjson flaw probes are rendered in separate tables. | SK-V6 close condition fires only from eligible same-plane strict rows. Advisory/permissive/stale rows can size target deltas but cannot convert a miss into a pass. |
```

### 6. Add to risk table around SOTA metadata

```markdown
| Comparator-plane drift | Benchmark report mixes DOM, On-Demand, typed direct, digest, SAX, or permissive ASM rows under one S anchor. | Row metadata v3 requires `plane`, `strictness`, `api_symbol`, ownership, freshness, and S-anchor eligibility; gate selection rejects mixed-plane anchors. | H/J benchmark schema tests plus rendered report audit. |
```

## Bench/Report Code Implications

When implementation is allowed, the local code changes should be mechanical:

- `metadata.rs`: bump `SCHEMA_VERSION` to `3`; add the fields listed above;
  make `required_fields_present` reject missing plane/strictness/ownership.
- `BenchFacts::competitor`: accept `api_symbol`, `plane`, `strictness`,
  `input_ownership`, `output_ownership`, `input_mutated`,
  `prevalidation_charged`, and `s_anchor_eligible`.
- `json_parity.rs`: remove `utf8_lossy` from sonic-rs features before strict
  rerun, or label the row `lossy_utf8` and `s_anchor_eligible=false`.
- `report.rs`: render strictness/output/parse UTF-8/escape/flaw probe from row
  metadata, not hard-coded strings.
- `gate.rs`: compute S only from `s_anchor_eligible=true` rows with matching
  `plane` and compatible `strictness`. Rename or augment
  `Outcome::ABeatAndParity` so final report distinguishes
  `substrate_beat_with_codegen_parity` from `track1_sota_beat`.

## Sources

- Local: `skinny/RESULTS.md`
- Local: `restart/skinny/BENCH.md`
- Local: `restart/corpora/SOTA.md`
- Local: `restart/MASTER-PLAN.md`
- Local: `skinny/crates/bbnf-bench/src/{metadata.rs,report.rs,gate.rs}`
- Local: `skinny/crates/bbnf-bench/benches/json_parity.rs`
- Local: `skinny/crates/bbnf-bench/Cargo.toml`
- Local: `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- Local: `skinny/profile/native-sidecars/PROFILE-REPORT.md`
- Local: `skinny/profile/native-sidecars/asmjson/NOTE.md`
- Local: `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md`
- sonic-rs: https://github.com/cloudwego/sonic-rs
- simd-json: https://github.com/simd-lite/simd-json and https://docs.rs/simd-json
- simdjson: https://github.com/simdjson/simdjson
- yyjson: https://github.com/ibireme/yyjson
- asmjson: https://docs.rs/asmjson and https://github.com/atomicincrement/asmjson
