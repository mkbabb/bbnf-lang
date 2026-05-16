# SK-V7 / A4 — parse-that-regex primitive gap audit after SK-V5 + SK-V6 admits

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Date: 2026-05-16
Scope: read-only inventory of `skinny/crates/parse-that-regex/`; cross-reads of
`skinny/crates/bbnf-simd/`, `skinny/crates/runtime/src/grammars/json/`,
`skinny/RESULTS.md`, `skinny/REDRESS.md`, and the `SK-V6-COHORT/` digest.
Repository was not modified.

## Authorities read

- Current source: `skinny/crates/parse-that-regex/src/{lib.rs,number/mod.rs,number/integer.rs,number/eisel_lemire/{mod,algorithm,table}.rs,unicode/{mod,utf8_block,utf8_hoehrmann}.rs,integration/{mod,simd_scan_hook}.rs}`.
- Cohort C5 (latest parse-that audit): `restart/skinny/tranches/sk-v6/research/skv6-C5-parse-that-gaps.md`.
- Cohort C1 (retained profile + per-`\uXXXX` candidate): `skv6-C1-retained-profile.md`.
- Cohort C2 (direct profile + mesh DirectBuild candidate): `skv6-C2-direct-profile.md`.
- Cohort C6 (Lock 14 generality leaks): `skv6-C6-generality-costfacts.md`.
- Cohort B5 (post-V6 primitive vocabulary): `skv6-B5-primitive-gap-inventory.md`.
- V5 baseline (what was empty at SK-V5 start): `SK-V5-COHORT/skv5-A3-parse-that-gaps.md`.
- HANDOFF: `restart/skinny/tranches/sk-v6/HANDOFF.md`.
- Row state: `skinny/RESULTS.md:5-21`, `:27-45`.
- Rejected route record: `skinny/REDRESS.md` items 50-72.

## Bottom line

Between SK-V5 start and the SK-V7 cohort dispatch, parse-that-regex gained a real
materializer layer: a `number/` submodule with span scanning, SWAR digit
accumulation, integer materializers, and an Eisel-Lemire f64 path with a Clinger
fast subpath; a `unicode/` submodule with a Hoehrmann scalar reference and a
NEON UTF-8 block validator dispatch hook; and an AArch64-fused string scanner
that already calls `bbnf_simd::aarch64::string_block::scan_string_special_block`
and `bbnf_simd::aarch64::utf8::validate_block` inline. None of the open SK-V7
parse rows nominate a missing semantic primitive; every named close is a shape
or codegen contract, not a missing oracle. The two remaining admissible parse-G
primitive routes are per-`\uXXXX` TBL classifier specialization inside the
existing escape path and Lock 14 grammar-neutrality cleanup. UTF-8 block fusion
into the trusted string scan is admitted and already in place — the SK-V5 broad
UTF-8 fusion route remains refused at the generated baseline by REDRESS 50-55
and the SK-V6 grand synthesis (`GRAND-SYNTHESIS-SK-V6.md:46-58`,
`HANDOFF-SK-V6.md:9-11`).

## 1. Current state inventory

### 1.1 Module layout

```
skinny/crates/parse-that-regex/
├── Cargo.toml
└── src/
    ├── lib.rs                                       (1353 LOC; tests included)
    ├── integration/
    │   ├── mod.rs                                   (1 LOC)
    │   └── simd_scan_hook.rs                        (19 LOC)
    ├── number/
    │   ├── mod.rs                                   (449 LOC; tests included)
    │   ├── integer.rs                               (77 LOC)
    │   └── eisel_lemire/
    │       ├── mod.rs                               (177 LOC)
    │       ├── algorithm.rs                         (93 LOC)
    │       └── table.rs                             (660 LOC; POWER_OF_FIVE_128)
    └── unicode/
        ├── mod.rs                                   (4 LOC)
        ├── utf8_block.rs                            (36 LOC)
        └── utf8_hoehrmann.rs                        (87 LOC)
```

Crate now exposes three submodules — `number`, `unicode`, `integration` — plus a
non-submoduled string surface that still lives directly in `lib.rs`. Compared
with the SK-V5 inventory (one ~780 LOC `lib.rs` plus integration shim,
`skv5-A3:13-30`), the executable surface is approximately **2950 LOC** of
non-test code split across nine files.

No `string/` submodule exists yet. Every string primitive (`match_string`,
`match_string_at_quote`, `match_json_string*`, `skip_json_string_plain`,
`skip_json_string_plain_trusted`, `unescape_json_string`, `decode_json_unicode_escape`,
`classify_json_string_content`, AArch64 `neon_classify_json_string_content`,
`movemask_u8x16`, `validate_utf8_codepoint`, `read_hex_unit_with_error_offset`,
hex/scalar oracles) still lives at `lib.rs:264-1112`. The SK-V5 plan to lift
these into `string/` (`skv5-A3:212-220`) has not landed — and is no longer
binding because the closures named for SK-V7 do not require it.

### 1.2 Eisel-Lemire status (SK-V5 Wave 2)

| Item | Location | Status |
|---|---|---|
| Algorithm body (compute_float, 128-bit multiply, halfway detect) | `src/number/eisel_lemire/algorithm.rs:1-93` | Vendored from fast_float2 v0.2.3; uses `POWER_OF_FIVE_128` |
| Table (10^k high+low u64 pairs) | `src/number/eisel_lemire/table.rs:1-660` | 651 rows covering `[-342, 308]` exponent range |
| f64 constants and Clinger fast path | `src/number/eisel_lemire/mod.rs:17-128` | `MAX_MANTISSA_FAST_PATH=2^53`, `FAST_PATH_POW10[0..=22]`, `INT_POW10[0..=15]`; disguised path up to e10=37 |
| Public `compute_f64` (exponent,mantissa,negative) → `Option<f64>` | `mod.rs:131-177` | Returns `None` on the ambiguous halfway band (~0.01%); caller falls back to `str::parse` |
| Consumer (`materialize_f64`) | `src/number/mod.rs:260-272` | Calls `compute_f64`; falls through to `std::str::from_utf8` + `str::parse::<f64>` on `mantissa_overflow` or ambiguous return |
| Test coverage | `src/number/mod.rs:430-448` | Bit-exact parity across `0.0`, `-0.0`, `1.0`, subnormals `5e-324`, `2.2250738585072014e-308`, `1.7976931348623157e308`, `43.474709000000125`, `6.02214076e23` |

The `numbers` direct row now sits at 12625 Mbps vs sonic 12974 Mbps (97.3%
sonic, `RESULTS.md:40`), and C2 records `number::materialize_f64` at 11.3% of
`numbers` and 4.8% of `mesh` direct hot symbols (`skv6-C2:54`, `:57`). The old
SK-V5 33% sonic gap on numbers (`skv5-A3:8`) is closed.

### 1.3 Integer materializer status

V5 prescribed lifting `parse_integer_digest` from `bbnf-bench/src/direct_struct.rs`
(`skv5-A3:107-110`, `:113-121`). Lift completed at `src/number/integer.rs:1-77`:

| Symbol | Location | Notes |
|---|---|---|
| `parse_integer` (alias for `parse_i64`) | `integer.rs:3-6` | Public entry |
| `parse_i64` | `integer.rs:8-30` | Handles negative sign; preserves `i64::MIN` via the `i64::MAX + 1` branch; `Overflow` errors |
| `parse_u64` | `integer.rs:32-38` | Rejects negative |
| `parse_u64_digits` | `integer.rs:40-54` | Inner SWAR-free `checked_mul`/`checked_add` loop |
| Test coverage | `integer.rs:56-77` | `preserves_i64_min`, `preserves_u64_max`, plus the overflow edges |

`number::mod` consumes these from the slow path of `materialize_i64` /
`materialize_u64` when `digit_count > 19 || mantissa_overflow`
(`number/mod.rs:225-258`).

### 1.4 String primitive status

Span and scan surface is SK-V5-grade and has been extended in V6:

| Symbol | Location | Status |
|---|---|---|
| `match_string`, `match_string_at_quote` (3-mode: StrictJson / GrammarString / ByteString / StrictJsonTrustedUtf8) | `lib.rs:343-413` | Mode-parameterized; reports `StringFlags { HAS_ESC, HAS_CONTROL, HAS_NON_ASCII, NEEDS_DECODE, UTF8_VALIDATED }` |
| `skip_json_string_plain` (NEON `scan_string_special_block` + 8-byte SWAR + scalar tail) | `lib.rs:594-676` | Already calls `bbnf_simd::aarch64::utf8::validate_block` when `non_ascii_mask != 0 && mode.validates_utf8()` (lib.rs:621-637). This is SK-V5 Wave 3 partially landed for the non-trusted mode |
| `skip_json_string_plain_trusted` (no UTF-8 validation, no scalar tail) | `lib.rs:678-706` | The SK-V6 R1b finding: 16-byte NEON loop, then 8-byte SWAR, then **returns without scalar tail completion** at `lib.rs:705`. Outer matcher (`match_json_string_at_quote_trusted_utf8`, `lib.rs:298-341`) byte-steps after this returns |
| `unescape_json_string` (Cow<str>) | `lib.rs:854-946` | Borrowed fast path when `!contains(\\)` (calls `classify_json_string_content` for control-byte validation); otherwise owned `String` build using `find_next_escape_or_control` + per-escape match arms + AArch64 `unescape_four_unicode_escapes` x4 batch |
| `decode_json_unicode_escape` (scalar surrogate-aware) | `lib.rs:434-476` | Calls `read_hex_unit_with_error_offset` → AArch64 `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon` per 4-byte unit |
| `unescape_four_unicode_escapes` (x4 batch, AArch64) | `lib.rs:516-591` | Calls `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon` + `join_surrogate_pair_neon`; pushes resulting chars one by one into `&mut String` |
| `classify_json_string_content` (escape-or-control prefilter) | `lib.rs:766-779` | Dispatches to AArch64 `neon_classify_json_string_content` (`lib.rs:801-830`) using `vcltq_u8` for control mask and `vceqq_u8` for slash mask |
| `find_next_escape_or_control` (SWAR slash-or-control mask) | `lib.rs:949-965` | 8-byte block scanner inside the borrowed unescape path; the V5 audit found this missing (`skv5-A3:204-208`); now landed |
| Scalar oracle hex unit | `lib.rs:1080-1102` (`read_hex_unit_scalar`, `hex_nibble`) | Used when the AArch64 path is unavailable |

The SK-V5-named `unescape_into_arena` writer-based path has not landed and is
not currently admissible: V6 REDRESS 67 (parser-owned decoded scratch
materializer) and REDRESS 68 (byte-output `unescape_json_string`) both regressed
the row set (`REDRESS.md:1736-1835`).

### 1.5 Unicode primitive status

| Item | Location | Status |
|---|---|---|
| `unicode::utf8_block::validate_block(&[u8; 16]) → ValidateStatus` | `src/unicode/utf8_block.rs:21-35` | Dispatches to `bbnf_simd::aarch64::utf8::validate_block` on aarch64, falls through to Hoehrmann scalar |
| `ValidateStatus` (is_valid, complete_bytes, bad_byte_offset, continues) | `utf8_block.rs:1-19` | Carries the 16-byte verdict facts |
| Hoehrmann-shaped scalar reference | `src/unicode/utf8_hoehrmann.rs:1-87` | Codepoint-walker DFA; handles 2/3/4-byte width, overlong `0xe0`/`0xed`/`0xf0`/`0xf4` ranges, and incomplete tail |
| `validate_utf8_codepoint` (per-codepoint, scalar) | `lib.rs:979-1047` | Still present in lib.rs; called from the non-trusted scan as the post-16-byte completion path |
| `decode_json_unicode_escape`, `validate_json_unicode_escape_run` | `lib.rs:434-514` | Scalar oracle for the JSON-specific `\uXXXX` shape; AArch64 batches via `unescape_four_unicode_escapes` |

The block validator landed but **only the non-trusted scan consumes it**
(`lib.rs:621-637`); the trusted scan deliberately skips raw UTF-8 validation
(`lib.rs:293-297`, `:679-706`). V6 keeps that boundary because the generated
parser receives `&str`, so input UTF-8 is already validated at `parser.rs:54-67`.

### 1.6 SIMD ergonomic helpers status

`SimdScannerHook` (`integration/simd_scan_hook.rs:3`) remains the single-method
ladder it was at SK-V5 — `classify_chunk` plus `alphabet()`. The six-rung
extension proposed by SK-V5 (block-scan, mask-iter, whitespace, digit-block,
hex-block, utf8-validate) has not landed and is **no longer prescribed**:
parse-that-regex now calls `bbnf_simd` primitives directly at the named sites
(`scan_string_special_block`, `unescape_uxxxx_neon`, `unescape_uxxxx_x4_neon`,
`join_surrogate_pair_neon`, `utf8::validate_block`), and the V6 cohort
consistently treats direct dispatch as the canonical pattern (B5 admitted
primitive vocabulary, `skv6-B5:54-82`).

## 2. Per-`\uXXXX` TBL classifier candidate

### 2.1 What C1 / C5 prescribe

C1 (`skv6-C1-retained-profile.md:99-130`) names the candidate explicitly:
**replace the per-`\uXXXX` scalar nibble classification inside the existing
retained trusted string/escape path with a per-unit table/TBL classifier**. C1
demands the route remain inside `match_string_at_quote` / parse-that string
validation boundaries with **no retained sidecar, no second source pass, no BIR
variant, no grammar directive**.

C5 (`skv6-C5-parse-that-gaps.md:30-31`, gap matrix row 2) records the same
candidate as `unicode_escape_run_decode_utf8` / `simple_escape_run_decode_utf8`
inside the existing `string_block`, `unescape_uxxxx_neon`, `unescape_uxxxx_x4_neon`,
and `surrogate_pair_join` primitives. R3e (`skv6-R3e:5-16`, `:19-34`) confirms
the close is **not** a new bbnf-simd vector primitive — every NEON semantic is
already in place. The work is a **parse-that-regex scalar/reference materializer
rewrite** that reuses existing SIMD calls.

### 2.2 Difference from REDRESS 64 (rejected four-unit contiguous-run validator)

REDRESS 64 batched four contiguous Unicode escape units and failed on
`y_string_unicode` because that row's strings are short and boundary-heavy
(`skv6-C1:107-113`). The four-unit validator stalls when there are fewer than
four consecutive `\uXXXX` units, which is the dominant `y_string_unicode`
shape.

The per-unit TBL candidate targets **every individual `\uXXXX` unit**, not a
batched four-unit run. The hex classification of the four nibbles inside a
single unit happens via a single NEON `vqtbl1q_u8` over a 16-entry nibble LUT
(the shape already used in `bbnf-simd/src/aarch64/unescape_uxxxx.rs:74-95`),
without requiring contiguous neighbors. So both `unicode_escapes` (dense long
runs) and `y_string_unicode` (short runs) get the same per-unit speedup.

### 2.3 Implementation sketch

NEON form already lives at `bbnf-simd/src/aarch64/unescape_uxxxx.rs`:
`unescape_uxxxx_neon` (`:74-95`) decodes one quartet with one `vqtbl1q_u8` plus
shift+OR; `unescape_uxxxx_x4_neon` (`:125-160`) decodes four quartets in one op
with the same TBL lane LUT. R2e records that of `unicode_escapes` 136682 `\u`
units, x4 already handles 135148 (98.88%) (`skv6-R2e:50-62`).

The remaining gap is the **slow per-unit fallback path** in
`parse-that-regex/src/lib.rs:919-922`:
```
let (ch, next) = decode_json_unicode_escape(bytes, slash)?;
out.push(ch);
cursor = next;
```
This routes through `read_hex_unit_with_error_offset` → `unescape_uxxxx_neon`
(per unit), then through `char::from_u32` + `String::push`. R1e attributes the
cost not to the TBL decode but to the **per-char push and per-byte escape match
arms** (`skv6-R1e:29-59`).

The actual admissible work is a **scalar reference rewrite** of the escape-run
materializer that:

1. Classifies each `\uXXXX` unit via the existing `unescape_uxxxx_neon`
   primitive (already TBL-based on aarch64; scalar reference at
   `bbnf-simd/src/aarch64/unescape_uxxxx.rs:40-58`).
2. Surrogate-pair joins via the existing `join_surrogate_pair_neon`
   (already checked at `bbnf-simd/src/aarch64/unescape_uxxxx.rs:168-175`).
3. Writes the decoded UTF-8 bytes **into the output buffer directly**, not via
   `char::from_u32` + `String::push` per unit.
4. Stays inside the `Cow<'_, str>` public API; does not change
   `unescape_json_string`'s signature (REDRESS 68 blocks byte-output return,
   `REDRESS.md:1789-1835`).

### 2.4 Where it lives

Per C5 (`skv6-C5:42-43`, `:55-63`) and B5 (`skv6-B5:71-72`), the candidate
primitive name is `unicode_escape_run_decode_utf8` (alongside
`simple_escape_run_decode_utf8`). It lives **inside parse-that-regex**, not in
bbnf-simd, because no new vector semantics are needed — only a scalar-reference
materializer that fuses the existing NEON `unescape_uxxxx_*` calls with the
output writer. Likely placement: a new `src/unicode/escape_run.rs` (the existing
`unicode/` submodule already exists) or, more conservatively, an extension to
`unescape_json_string`'s owned-path inner loop at `lib.rs:911-921`.

If placed under `src/unicode/`, the existing `unicode/mod.rs:1-4` adds one
`pub mod escape_run;` line and one re-export. Cross-crate isomorphism would
not be violated; bbnf-simd already owns the NEON primitives, and this addition
is a scalar reference + caller fusion.

### 2.5 Scalar reference

Hoehrmann-style 9-state DFA is overkill for nibble classification: the existing
4-byte oracle is `bbnf-simd/src/aarch64/unescape_uxxxx.rs:40-58`
(`unescape_uxxxx_scalar` + `hex_nibble`). A nibble validator using the
`hex_nibble` table is the correct reference; the SK-V5 prescription to add a
Hoehrmann scalar reference (`skv5-A3:236`) was for *UTF-8 block validation*,
which already landed at `src/unicode/utf8_hoehrmann.rs`.

### 2.6 Same-wave consumer

The same-wave consumer must be `unescape_json_string`'s owned path
(`lib.rs:854-946`) or — if `escape_run.rs` exposes a writer-based primitive — a
new `materialize_escape_run_into` that the existing `unescape_json_string`
calls. The owned path currently splits between SWAR `find_next_escape_or_control`
(landed at `lib.rs:949-965`) and the per-escape match arms (`lib.rs:872-929`).
The match arms are the hot site C1 names; the SWAR escape-mask scanner is
already wide.

R3e demands the same-wave consumer be present **before** any new SIMD primitive
admission (`skv6-R3e:55-68`). Since no new SIMD primitive is required, the
demand reduces to: the consumer must land in the same wave as the scalar
reference rewrite.

### 2.7 Falsifiability gate

C1 (`skv6-C1:115-126`):

- `unicode_escapes` retained Track 1 must improve by ≥ 15%
  (current 12905 vs sonic 16048 = 80.4%, `RESULTS.md:18`).
- `y_string_unicode` retained Track 1 must improve by ≥ 8%
  (current 6290 vs sonic 13673 = 46.0%, `RESULTS.md:21`).
- `twitter`, `github_events`, `random`, `distinct_values`, `numbers`, `canada`
  must each regress by no more than 2% under native `profile-lazy` smoke.
- Native Criterion guard rows: no >5% regression.

C5 (`skv6-C5:31`) raises the secondary direct-row gates:

- `unicode_escapes` direct: 5303 vs sonic 9072 = 58.5% (`RESULTS.md:42`).
- `unicode_mixed` direct: 4782 vs sonic 6406 = 74.6% (`RESULTS.md:41`).
- `y_string_unicode` direct: 5070 vs sonic 8547 = 59.3% (`RESULTS.md:45`).

The same-wave direct consumer is `unescape_json_string` called from
`runtime/src/grammars/json/view.rs:206-216` and the sink source hooks at
`runtime/src/grammars/json/sink.rs:16-35`, `:44-92`.

## 3. What's still missing in parse-that-regex post-V6

### 3.1 UTF-8 NEON block validator

V5 prescribed it (`skv5-A3:233-235`, `:246-254`). It has **landed for the
non-trusted scan** at `lib.rs:621-637` (calls
`bbnf_simd::aarch64::utf8::validate_block` whenever `non_ascii_mask != 0 &&
mode.validates_utf8()`). The `src/unicode/utf8_block.rs` wrapper plus
Hoehrmann scalar reference at `src/unicode/utf8_hoehrmann.rs` exist as scalar
oracle and dispatch.

The V5 Wave 3 prescription of a **broad UTF-8 fusion into the trusted body
scan** is the rejected route. V6 grand synthesis
(`GRAND-SYNTHESIS-SK-V6.md:46-58`, `:143-145`, `:272-278`) explicitly
invalidates it: no current retained profile names `validate_utf8_codepoint` as
hot. The HANDOFF lists "broad UTF-8 fusion as generated-baseline close" as a
do-not-reopen item (`HANDOFF-SK-V6.md:76-77`). REDRESS 50-55 catalogues five
prior UTF-8 routes that regressed the row set.

**Verdict**: Lemire-style 64-byte UTF-8 validator is *not* admissible at the
generated baseline. The 16-byte NEON block validator is admitted and consumed.
Further UTF-8 work is gated on new row evidence.

### 3.2 String body scan beyond the 0x80 early-exit

SK-V5-B1 finding (`skv5-A3:418` referenced in lib.rs:331). The trusted scanner
`skip_json_string_plain_trusted` (`lib.rs:678-706`) terminates its 16-byte NEON
loop and 8-byte SWAR loop without a scalar tail; the outer
`match_json_string_at_quote_trusted_utf8` then byte-steps from `lib.rs:330-333`.

R1b (`skv6-R1b:25-29`, `:84-112`) records this and **C5 rules out widening**
(`skv6-C5:30`). The R2c finding that no defensible retained-string threshold
remains (`skv6-R2c:95-106`) blocks Candidate 1 (delete the tiny probe) and
Candidate 2 (always-wide scanner). The admissible name is
`trusted_string_special_tail_scan` (B5 vocabulary, `skv6-B5:67`); the route is
a tail-completion helper, not a wider block.

This **is** admissible parse-that-regex work, but lower priority than the
per-`\uXXXX` TBL route (C1 names the TBL route first).

### 3.3 Number scan SIMD

Currently scalar SWAR 8/4/2 digit blocks (`number/mod.rs:106-223`). Eisel-Lemire
is a post-scan materializer; the digit-block accumulator runs in scalar SWAR.

C2 evidence (`skv6-C2:54`, `:57`): `numbers` and `mesh` direct are dominated by
the generated array/value loop and `number::materialize_f64`, not by the digit
scan. SK-V6 grand synthesis (`GRAND-SYNTHESIS-SK-V6.md:96-109`) says no Wave 2
retained parse work should route through number parsing. **Verdict**: SIMD
digit-block classifier is not currently admissible.

### 3.4 Escape boundary scan

Landed at `lib.rs:949-965` (`find_next_escape_or_control` with
`json_string_escape_control_mask` SWAR). This was an SK-V5 gap (`skv5-A3:208`)
that has been closed.

### 3.5 ASCII-only/no-escape fast path

Landed at `lib.rs:855-858`:
```
if !raw_content.as_bytes().contains(&b'\\') {
    classify_json_string_content(raw_content.as_bytes(), 0, raw_content.len())?;
    return Ok(Cow::Borrowed(raw_content));
}
```
Plus the NEON content classifier at `lib.rs:801-830`. SK-V5 closure.

### 3.6 What parse-G actually bottlenecks on now

Per C1 (`skv6-C1:62-71`):

| Row | Hot symbols (current) |
|---|---|
| twitter | `match_tiny_plain_string` 42.26%, `match_string_at_quote` 18.13%, `consume_container_next` 9.82%, `parse_key_colon` 5.48% |
| citm_catalog | container re-entry (already 24.97→14.51% self after candidate-4) |
| github_events | `parse_value_at` 85.5% inclusive; `match_tiny_plain_string` key 32.8%, value 17.3% |
| update_center | short-string SWAR/tiny-string scalar loop dominates |
| random | `match_tiny_plain_string` 31.0% key / 20.5% value; high mixed dispatch entropy |
| unicode_escapes | `match_string_at_quote` 90.44% self (`\uXXXX` decode region) |
| distinct_values | gate plus REDRESS 72 cap-16 (+57.5% Track 1) |
| y_string_unicode | escape-decode scalar; `\uXXXX` hex decode 13.9% source-band, escape recovery 35.1% |

`match_tiny_plain_string` cap-16 widening already landed (REDRESS 72,
`REDRESS.md:1996-2059`). Remaining parse-G work is escape-tail
(`unicode_escapes`, `y_string_unicode`) and generated cadence (container/key),
not a missing parse-that-regex primitive.

## 4. Mesh DirectBuild candidate (C2)

### 4.1 What mesh DirectBuild needs

Per C2 (`skv6-C2:120-144`): a generated `real_typed_struct` schema for `mesh`,
following the existing `twitter` / `update_center` pattern at
`runtime/src/grammars/json/generated.rs` `parse_twitter_search` /
`parse_update_center`. Shape: host/API `DirectSchemaSet` for a `MeshDirect`
product type with **typed numeric vectors** for the large numeric fields
(`positions`, normal/index/vector fields where admitted), typed `Batch`
entries, explicit ignored-field skip facts, and capacity hints for the large
arrays. Generated through the existing
`DirectSchemaSet → schema_direct → json_typed_direct` codegen path.

The candidate is **codegen plus host/API schema declaration** — there is no
new parse-that-regex primitive involved. The existing Eisel-Lemire path scales
because mesh's numeric distribution is integer-heavy `positions` arrays (C2:
`number::materialize_f64` 4.8% and `materialize_u64` 2.6% on mesh digest,
`skv6-C2:57`).

### 4.2 mesh schema vs twitter schema

twitter typed direct currently passes at 151.5% sonic (18129 / 11969 Mbps,
`RESULTS.md:28`); update_center typed at 99.2% sonic (12044 / 12144,
`RESULTS.md:34`). Both are **flat object** schemas with mostly string fields
plus a few scalars.

mesh is **numeric-array-heavy**. The schema needs `Vec<f64>` (or
`Vec<u64>`/`Vec<i64>`) typed-vector materializers, not just scalar field
emitters. R3 attributes canada direct to `parse_number_array_direct` 49.1%,
`materialize_f64` 12.3%, `emit_number_array_direct` 11.2% (`skv6-R3:35-56`).
mesh would consume the same generated number-array materializer.

### 4.3 Falsifiability gate (C2)

- Correctness parity across generated Track 1 typed output, structurally
  independent Track 2 typed oracle, sonic-rs typed serde, serde_json typed
  serde.
- Same-plane `mesh real_typed_struct` generated Track 1 within `1.10x`
  sonic-rs time.
- Profile shows typed generated symbols (`parse_type_mesh`, `parse_vec_f64`,
  skip helpers) replacing `JsonDigestSink` symbols; `JsonDigestSink::array_string`
  absent on the typed row.
- Existing `semantic_full_digest_stressor` rows remain correctness-green and
  are reported separately.

mesh direct_to_struct currently 91.8% sonic (8818 vs 9606,
`RESULTS.md:35`). The typed close should beat that comfortably given the
twitter precedent.

### 4.4 LOC budget

Schema declaration (1 typed product type with ~4-8 fields including a
numeric-vector field): ~50-80 LOC in the host fixture. Codegen plumbing reuses
the `twitter`/`update_center` pattern; if the generated path already covers
typed vector emission, the codegen delta is 0 LOC; if not, generic numeric-vector
codegen extension is ~80-120 LOC in `codegen/src/json_typed_direct.rs`.

Total: ~100-200 LOC, all in codegen + fixtures, **none in parse-that-regex**.

## 5. Generalization audit — Lock 14 leaks in parse-that-regex

Per C6 (`skv6-C6-generality-costfacts.md:51-83`), parse-that-regex has blocking
Lock 14 leaks. The crate exposes JSON-prefixed public symbols even though the
SK-V5 contract names parse-that-regex as grammar-neutral. C6 (`skv6-C6:76-82`)
lists six leak families. Updated enumeration below with current file:line.

### 5.1 Public JSON-leaked symbol surface

| Symbol | file:line | Grammar-neutral replacement |
|---|---|---|
| `JsonStringMatch` (pub struct) | `lib.rs:34-40` | `StringMatch` already exists at `lib.rs:96-100`; collapse |
| `StringMode::StrictJson`, `StrictJsonTrustedUtf8` | `lib.rs:44-45` | `PrimitiveFacts`-supplied delimiter/control/escape policy |
| `JsonNumberMatch` | `lib.rs:120-124` | `NumberSpan` already exists at `number/mod.rs:5-14`; collapse |
| `skip_json_whitespace` | `lib.rs:127-139` | `StructuralClassTable::skip_class_run` over a `StringMode::Trivia` byte set |
| `match_json_number`, `match_json_number_from_first` | `lib.rs:164, 174` | `match_number_span`, `match_number_span_from_first` already exist at `number/mod.rs:31-103`; the JSON wrappers are thin shims |
| `validate_json_number` | `lib.rs:260` | `validate_number` |
| `match_json_string`, `match_json_string_at_quote`, `match_json_string_at_quote_trusted_utf8` | `lib.rs:268, 280, 298` | `match_string`, `match_string_at_quote` already exist; the JSON wrappers reduce to fixed-mode aliases |
| `decode_json_unicode_escape`, `validate_json_unicode_escape_run` | `lib.rs:434, 479` | `decode_unicode_escape`, `validate_unicode_escape_run` under `unicode/` |
| `skip_json_string_plain`, `skip_json_string_plain_trusted`, `json_string_special_mask` | `lib.rs:594, 678, 709` | `skip_string_plain`, `string_special_mask_64` with `SpecialByteSet` parameter |
| `classify_json_string_content`, `scalar_classify_json_string_content`, `neon_classify_json_string_content` | `lib.rs:766, 781, 801` | `classify_string_content` with `SpecialByteSet`/`ControlPolicy` parameters |
| `validate_json_string` | `lib.rs:847` | `validate_string` |
| `unescape_json_string` | `lib.rs:854` | `unescape_string` with mode-supplied escape table |
| `validate_json_string_escape` | `lib.rs:416` (private) | `validate_string_escape` |
| `json_string_escape_control_mask` | `lib.rs:968` (private) | `string_escape_control_mask_64` |

C6 estimate (`skv6-C6:140-144`): Lock 14 cleanup in parse-that-regex routes
through "Wave 4 cleanup" with optional "Wave 5 if a new primitive body is
required". Per the audit, all bodies already exist; only the **public-symbol
rename** is required.

### 5.2 Test surface JSON-leak

Tests at `lib.rs:1118-1352` also use JSON names. They are valid corpus tests
but should move to a JSON-fixture file under `tests/` once the public API
renames; until then they remain inside the crate as scalar oracles.

### 5.3 Generalization path

The crate should expose grammar-neutral primitives (`number/`, `string/`,
`unicode/`) that the JSON wrapper consumes from a separate per-grammar layer
(or from generated code). Concretely:

- `number/mod.rs` is already grammar-neutral (`match_number_span`, `materialize_*`).
  No work needed there.
- `unicode/` is already neutral.
- String primitives must lift into a `string/` submodule with `SpecialByteSet`
  parameterization. The bodies do not change; only the dispatch parameter does.

C5 (`skv6-C5:65-69`) blocks this from being treated as a SOTA close — the
rename is **separate from row recovery**. It's structural cleanup.

## 6. LOC budgets

| Item | LOC budget | Files affected | Notes |
|---|---:|---|---|
| Per-`\uXXXX` TBL classifier (scalar ref + per-unit fusion) | 150-300 | new `src/unicode/escape_run.rs` (~200 LOC) or extension to `lib.rs:854-946` (~100 LOC delta); test parity in same file (~100 LOC) | No bbnf-simd delta; consumer is existing `unescape_json_string` |
| mesh DirectBuild lowering | 100-200 | `bbnf-bench/src/direct_struct.rs` schema fixture (~50 LOC); `codegen/src/json_typed_direct.rs` numeric-vector extension (~80 LOC if needed); host/API schema declaration (~50 LOC) | All outside parse-that-regex |
| Lock 14 cleanup in parse-that-regex | ~30 file:line rename sites in `lib.rs:34-1112`; mechanical | `lib.rs` (rename + thin JSON aliases), `unicode/mod.rs` (re-export), small test edits | Public symbols rename; no body change |
| UTF-8 NEON validator (if admitted) | 0 — already landed for non-trusted scan; refused for trusted scan | n/a | Lemire 64-byte form not admissible per HANDOFF |
| Number scan SIMD (digit-block classifier) | 0 — not currently admissible | n/a | C2 evidence shows non-issue |
| String body scan SIMD beyond 0x80 early-exit | 50-150 (tail-completion helper only) | `lib.rs:678-706` (`skip_json_string_plain_trusted` tail) | `trusted_string_special_tail_scan` per B5 vocabulary; admissible but lower priority than TBL |
| `trusted_string_special_tail_scan` | included above | `lib.rs:678-706` | Adds the missing scalar tail completion R1b named |

Total parse-that-regex code growth for the SK-V7 admissible queue: ~180-450
LOC. Lock 14 cleanup is mechanical and does not grow the body.

## 7. Order of admission

C1 names the per-`\uXXXX` TBL route first; C2 names mesh DirectBuild second.
Beyond those:

### Priority 1 — per-`\uXXXX` TBL classifier in escape-run materializer

Closes the largest sonic gaps in `RESULTS.md`:

- `y_string_unicode` retained: 46.0% sonic (`RESULTS.md:21`).
- `unicode_escapes` direct: 58.5% sonic (`RESULTS.md:42`).
- `distinct_values` direct: 53.7% sonic (`RESULTS.md:44`).
- `unicode_mixed` direct: 74.6% sonic (`RESULTS.md:41`).
- `y_string_unicode` direct: 59.3% sonic (`RESULTS.md:45`).
- `unicode_escapes` retained: 80.4% sonic (`RESULTS.md:18`).

Single intervention with same-wave consumer in `unescape_json_string`
(view + sink). Cost: ~200-300 LOC, no SIMD delta.

### Priority 2 — mesh DirectBuild lowering

Closes a single direct row but extends the typed-product proof beyond twitter /
update_center. Cost: ~100-200 LOC in codegen + fixtures; no parse-that-regex
change.

### Priority 3 — Lock 14 cleanup in parse-that-regex

Structural debt. Required before any cross-grammar test (the V6 packet routes
this to Wave 4). Cost: mechanical rename across ~30 sites. Does not move rows;
unblocks future grammar fixtures.

### Priority 4 — `trusted_string_special_tail_scan`

R1b finding: the trusted scanner's loop exits without scalar tail. Helps
short-string row cluster (`twitter`, `random`, `update_center`, `gsoc-2018`,
`distinct_values`) but R2c blocks the obvious "always-wide" / "delete tiny
probe" routes (`skv6-R2c:95-106`). Cost: ~50-150 LOC tail helper plus
falsifiability against guard rows.

### Order argument from RESULTS.md

The largest single-row sonic gap is `y_string_unicode` at 46% retained / 59%
direct. Both rows are escape-decode-dominated (C2: `unescape_json_string`
21.0% direct, `HandParser::string` 17.1%). The per-`\uXXXX` TBL route directly
addresses both. The mesh row is at 91.8% direct (smaller gap) but the typed
proof is a separate generality dividend. Lock 14 is structural and does not
move rows. The `trusted_string_special_tail_scan` route targets short-string
rows where REDRESS 72 cap-16 widening already captured most of the gain.

**Recommended sequence**: TBL escape-run materializer first (closes largest
gap, lowest scope risk); mesh DirectBuild second (extends typed proof);
trusted-string tail third (smaller marginal gain, needs careful guard); Lock 14
rename last or in parallel as Wave 4 work since it has zero perf risk.

## 8. Summary

Top three missing primitives by impact:

1. **`unicode_escape_run_decode_utf8`** — per-`\uXXXX` TBL scalar reference plus
   fused materializer inside `unescape_json_string`. Closes `unicode_escapes`,
   `unicode_mixed`, `y_string_unicode` rows on both retained and direct planes.
   No new SIMD; reuses `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_neon`
   and the per-unit oracle. Budget 150-300 LOC. Falsifiability gate per C1.
2. **mesh `real_typed_struct` DirectBuild schema** — extends the
   twitter/update_center typed-output proof to numeric-array-heavy corpus. No
   parse-that-regex change. Budget 100-200 LOC in codegen + fixture. Cost: C2
   gate at 1.10x sonic.
3. **Lock 14 grammar-neutrality cleanup** — rename ~30 JSON-prefixed
   public/private symbols in `parse-that-regex/src/lib.rs` to grammar-neutral
   names; collapse `JsonStringMatch`/`JsonNumberMatch` into `StringMatch`/`NumberSpan`;
   parameterize string mode by `SpecialByteSet`/`ControlPolicy`. Structural
   debt; mechanical work; required before cross-grammar fixtures land.

Total LOC budget for the SK-V7 admissible queue: ~280-650 LOC across
parse-that-regex, codegen, and fixtures. The crate's executable surface is
~2950 LOC today (vs ~780 LOC at SK-V5 start, ~38% of the original SK-V5
"all primitives shipped" target of ~3380 LOC). Eisel-Lemire, integer
materializers, the UTF-8 block validator, and the AArch64 string-scan dispatch
have already landed; what remains is materializer-shape work, not new
primitive bodies.

Report file: `/tmp/skv7-A4-parse-that-gaps.md`
