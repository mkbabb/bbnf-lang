# SK-V5 Nuke Plan

Date: 2026-05-13.

Authority: `restart/skinny/audit/GRAND-SYNTHESIS-SK-V5.md` + cohort
reports A4 + B2 + D4 + D5 + D6 at `restart/skinny/audit/SK-V5-COHORT/`.

Each item below names: file path, why it must die, who depended on it,
the verification command after removal, and the wave that executes the
deletion. Execution batches in Wave 4 (per IMPLEMENTATION-PACKET-SK-V5)
so all related changes land in one commit.

## Wave 0 Decision Ledger

Wave 0 records disposition only; it does not delete files whose consumers
have not landed yet.

| Item | Decision | Execution wave | Verification command |
|---|---|---:|---|
| `generated_eventcursor.rs` | NUKE; measured parallel prepass regression | 4 | `rg "eventcursor\|generated_eventcursor\|EventCursor\|ParseIndexCursor" skinny/` |
| Bench-private `SinkParser` | REWIRE then NUKE after generated `SinkOnly` exists | 2 | `samply ... profile_direct ... track1` shows generated runtime |
| `crates/simd-scan/` | NUKE fossil crate | 4 | `rg -l "simd-scan\|simd_scan" skinny/` |
| `eventcursor` feature | NUKE with generated EventCursor | 4 | `cargo build --workspace --all-features && rg "eventcursor" skinny/` |
| `ParseIndexCursor` / `scan_parse_index` | NUKE if unused after eventcursor removal | 4 | `rg "ParseIndexCursor|scan_parse_index" skinny/` |
| `wave2_bench.rs` example | NUKE if present | 4 | `cargo build --workspace --examples` |
| `bbnf-simd` JSON god-module | SPLIT/MOVE, not raw delete | 4 | Lock 14 grep in §14 |
| JSON hardcoded scalar references | PARAMETERIZE alphabet | 4 | `rg "b'\\{' \| b'\\}' \| b'\\['" skinny/crates/bbnf-simd/` |
| `JsonStringMatch` / `JsonNumberMatch` names | ALIAS/MOVE during string-number work | 3/4 | `rg "JsonStringMatch|JsonNumberMatch" skinny/crates/parse-that-regex/` |
| `include_str!` decorative codegen | REPLACE with real lowerer | 1 | `cargo run -p xtask --release -- gen --check` |
| hardcoded JSON in `passes` | GENERALIZE over grammar facts | 1 | `rg '"object"\|"array"\|"pair"\|"string"\|"number"\|"bool"\|"null"' skinny/crates/passes/src/` |
| `parse_integer_digest` | MOVE before SinkParser nuke | 2 | `rg "parse_integer_digest|parse_integer" skinny/crates/` |
| AVX-IFMA "Wave 6" message | FIX documentation drift now | 0 | `rg 'Wave 6: vpmadd52' skinny/crates/bbnf-simd/src/x86_64/avx_ifma/mantissa.rs` returns zero |

## 1. Refuted Parallel Prepass — `generated_eventcursor.rs`

**Path**: `skinny/crates/runtime/src/grammars/json/generated_eventcursor.rs`

**Reason**: Wave 2 Agent 4 measured 11-37% regression on every parse-G
corpus; the EventCursor-as-prepass shape is structurally
substrate-without-consumer (Lock 1). REDRESS.md records the refutation.
The file is committed (`M` in git status, not `??`) — it sits in the
canonical history.

**Dependents** (per A4 audit):
- `runtime/src/grammars/json/mod.rs` — `#[cfg(feature = "eventcursor")]
  pub mod generated_eventcursor;` branch
- `runtime/src/grammars/json/parser.rs` — `#[cfg(feature = "eventcursor")]`
  alternate parser path
- `runtime/Cargo.toml` — `eventcursor` feature declaration
- `codegen/src/json_templates/` — corresponding template file (if it
  exists; verify and delete)
- `skinny/crates/bbnf-simd/` — `ParseIndexCursor` + `scan_parse_index`
  exports gated under the same feature
- Any `examples/wave2_bench.rs` or similar reference

**Wave**: Wave 4.

**Action**:
```bash
git rm skinny/crates/runtime/src/grammars/json/generated_eventcursor.rs
# Remove cfg branches from mod.rs and parser.rs
# Remove eventcursor from Cargo.toml features section
# Remove ParseIndexCursor + scan_parse_index from bbnf-simd if D5 verifies unused
# Delete examples/wave2_bench.rs if present
```

**Post-nuke verification**:
```bash
cargo build --workspace --all-features
cargo test --workspace
rg "eventcursor|generated_eventcursor|EventCursor|ParseIndexCursor" skinny/
# Expect zero hits except in documentation referencing the refutation.
```

## 2. Bench-Private SinkParser — `direct_struct.rs`

**Path**: `skinny/crates/bbnf-bench/src/direct_struct.rs`

**Reason**: D5 + B2 verified: `track1_digest` and `track2_digest` both
call the same `sink_only_digest` hand-rolled recursive-descent parser
that never touches `Tape` or generated code. The bench measures a
private parser twice, calling it "Track 1" vs "Track 2". This makes
the gate dishonest: every primitive landed while the bench-private
parser is on the gate is credited to the bench, not the language.

**Dependents**:
- `bbnf-bench/src/lib.rs` — `pub mod direct_struct;` (already wired)
- `xtask` bin targets that call `track1_digest` / `track2_digest`
- `gate-json` xtask command

**Wave**: Wave 2 — must die in the same wave that lands generated
`SinkOnly` lowering. Replacing it before generated `SinkOnly` exists
breaks the gate; nuking it after the gate consumes bench-private is
dishonest.

**Action**:
- Wave 2 stage 1: implement `codegen/src/lower/sink_only.rs` + runtime
  `parse_direct<S: JsonSink>`.
- Wave 2 stage 2: rewrite `direct_struct.rs` to call generated runtime
  for `track1_digest` and to call a STRUCTURALLY-DIFFERENT hand-coded
  path for `track2_digest` (iterator-based vs callback-based).
- Wave 2 stage 3: delete `SinkParser` struct and all private parser
  bodies; keep only the `JsonSinkDigest` struct and the thin wiring
  to runtime / hand-coded modules.

**Migration**: the integer materializer at `direct_struct.rs:501-528`
(`parse_integer_digest`) moves to
`parse-that-regex/src/number/integer.rs` BEFORE the nuke (Wave 2 step 1
already does this).

**Post-nuke verification**:
```bash
cargo run -p xtask --release -- bench-json
samply record --rate 4000 --main-thread-only \
  ./target/release/profile_direct 10000 twitter track1
# Expect samply hot leaf to show
# `runtime::generated_json::parse_direct`,
# NOT `bbnf_bench::direct_struct::SinkParser::*`.
```

## 3. simd-scan Fossil Crate

**Path**: `skinny/crates/simd-scan/` (entire directory, 584 LOC at
`src/lib.rs`)

**Reason**: D4 verified: NOT in `skinny/Cargo.toml` `workspace.members`,
NOT in `workspace.dependencies`, zero crates depend on it. The
`simd_scan` hit in `bbnf-bench/Cargo.toml:34` is a criterion `[[bench]]`
target name, not a crate dependency. The MIGRATION renamed
`simd-scan → bbnf-simd` (MIGRATION.md:49, 75, 104, 158-159, 259-269,
489-493) but never deleted the source directory. The crate is a
near-verbatim duplicate of `bbnf-simd/lib.rs:13-100`.

**Dependents**: none.

**Wave**: Wave 4.

**Action**:
```bash
git rm -rf skinny/crates/simd-scan/
```

**Migration**: none required (no callers).

**Post-nuke verification**:
```bash
cargo build --workspace
rg -l "simd-scan|simd_scan" skinny/
# Expect zero hits except the criterion bench target name (renameable
# in a separate cosmetic pass if desired).
```

## 4. eventcursor Feature Flag

**Path**: `skinny/crates/runtime/Cargo.toml` (`[features]` section,
`eventcursor` line) + any `#[cfg(feature = "eventcursor")]` references
across `runtime/`, `codegen/`, `bbnf-bench/`, `xtask/`.

**Reason**: All branches under this flag are the refuted prepass
shape. The flag itself is dead.

**Dependents**: nothing else uses the feature flag itself; it gates the
refuted code only.

**Wave**: Wave 4 (alongside #1).

**Action**:
```bash
# Remove from Cargo.toml [features] block.
# Remove every #[cfg(feature = "eventcursor")] from runtime/, codegen/, xtask/.
# Remove from xtask/src/main.rs feature passthroughs if present.
```

**Post-nuke verification**:
```bash
cargo build --workspace --all-features
rg "eventcursor" skinny/
# Expect zero hits.
```

## 5. ParseIndexCursor + scan_parse_index Substrate Exports

**Path**: `skinny/crates/bbnf-simd/src/parse_index/` or wherever
`ParseIndexCursor` is declared (verify with `rg`).

**Reason**: D5 confirmed these are feature-gated under `eventcursor`
only; once the feature is gone, they have no callers. Per A4 audit
they are parallel-substrate residue.

**Dependents**: `generated_eventcursor.rs` (also being nuked).

**Wave**: Wave 4 (alongside #1 and #4).

**Action**:
```bash
# Delete the parse_index/ subdirectory or strike the relevant
# pub items from bbnf-simd/src/lib.rs after grepping for cross-crate use.
```

**Post-nuke verification**:
```bash
cargo build --workspace
rg "ParseIndexCursor|scan_parse_index" skinny/
# Expect zero hits.
```

## 6. wave2_bench.rs Example (if present)

**Path**: `skinny/crates/bbnf-bench/examples/wave2_bench.rs` (verify
existence).

**Reason**: Per A4 audit candidate; references the refuted EventCursor
prepass.

**Wave**: Wave 4.

**Action**:
```bash
find skinny -name "wave2_bench.rs" -exec git rm {} \;
```

**Post-nuke verification**:
```bash
cargo build --workspace --examples
```

## 7. bbnf-simd JSON God-Module Split

**Path**: `skinny/crates/bbnf-simd/src/lib.rs` (716 LOC, JSON-isms
throughout)

**Reason**: Lock 14 violation. D4 verified the split-by-primitive
intent at MIGRATION.md:259-269; only 1 of 9 primitives (BYTE_CLASS_FROM_EQ_SET_64)
was lifted in commit 9eef728c.

This is not strictly a NUKE; it's a SPLIT + MOVE. The split keeps the
generic helpers, moves the JSON-specific pieces to
`runtime/grammars/json/`.

**KEEP** in `bbnf-simd/src/lib.rs`:
- `StructuralAlphabet` (already grammar-neutral)
- `ScanBackend` enum
- `StructuralIndex`
- `select_classifier`
- A new `classify_chunk_from_alphabet(chunk: uint8x16_t, alphabet:
  uint8x16_t) -> u16` generic primitive that replaces the JSON-hardcoded
  `mod neon` body at lines 463-693.

**MOVE OUT** to `skinny/crates/runtime/src/grammars/json/`:
- `JSON_STRUCTURAL` constant
- `is_json_structural_alphabet` helper
- `is_json_punctuation`
- `scan_json_tail`
- `JsonParseIndex` (alias for `StructuralIndex` with the JSON alphabet)
- `resolve_json_string_masks_64`
- The JSON-specific caller that builds the 7-char alphabet and calls
  `classify_chunk_from_alphabet`

**Wave**: Wave 4.

**Action**: substantial refactor across `bbnf-simd/src/lib.rs` + new
`runtime/grammars/json/scan.rs` + `runtime/grammars/json/structural.rs`.

**Post-nuke verification**:
```bash
cargo build --workspace
cargo test --workspace
rg "JSON|json" skinny/crates/bbnf-simd/src/ | grep -v "// "
# Expect hits only in:
# (a) the generic dispatch dispatching ON an alphabet parameter,
# (b) documentation comments,
# (c) the bench-private fixtures (which can stay or move).
```

## 8. JSON-Hardcoded Scalar References in bbnf-simd

**Paths**:
- `skinny/crates/bbnf-simd/src/x86_64/avx2/classify.rs:31`
- `skinny/crates/bbnf-simd/src/x86_64/avx512_vbmi2/classify.rs:28`
- `skinny/crates/bbnf-simd/src/x86_64/avx512_gfni/classify_affine.rs:31`
- `skinny/crates/bbnf-simd/src/x86_64/avx512_bitalg/multiclass.rs:30`
- `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:65-71` (`json_ascii_table`)

**Reason**: Lock 14 violation. 4 separate `classify_block_scalar`
functions all hardcode `b'{' | b'}' | b'[' | b']' | b',' | b':' | b'"'`.
The TBL4 LUT bakes the same set.

**Wave**: Wave 4.

**Action**: Replace each `classify_block_scalar(input: &[u8; 64]) -> u64`
that hardcodes JSON punctuation with a generic
`classify_block_scalar(input: &[u8; 64], alphabet: &[u8; 8],
alphabet_len: usize) -> u64`. Move the `json_ascii_table` LUT to
codegen-emitted `.data` at `codegen/src/grammars/json/tables.rs`.

**Post-nuke verification**:
```bash
cargo build --workspace
cargo test -p bbnf-simd
rg "b'\\{' \\| b'\\}' \\| b'\\['" skinny/crates/bbnf-simd/
# Expect zero hits.
```

## 9. JsonStringMatch / JsonNumberMatch in parse-that-regex

**Path**: `skinny/crates/parse-that-regex/src/lib.rs:80, :104` (the
JSON-prefixed types in a supposedly generic crate)

**Reason**: A3 finding. The crate is supposed to be grammar-neutral;
`JsonStringMatch` / `JsonNumberMatch` are public JSON types in a
generic crate. The crate also holds direct NEON intrinsics at
`lib.rs:421-545` that duplicate `bbnf-simd::aarch64::movemask`.

**Wave**: Wave 3 (string/Unicode work) + Wave 4 (final Lock 14 audit).

**Action**:
- Wave 3 introduces `parse-that-regex/src/string/` and
  `parse-that-regex/src/unicode/` submodules.
- The Json-prefixed types become aliases for generic `StringMatch` /
  `NumberMatch` types that take an alphabet/strictness parameter.
- The misplaced NEON intrinsics at `lib.rs:421-545` move to
  `bbnf-simd/src/aarch64/movemask.rs` (where the canonical
  implementation already lives).

**Post-nuke verification**:
```bash
cargo build --workspace
rg "JsonStringMatch|JsonNumberMatch" skinny/crates/parse-that-regex/
# Expect hits only in compatibility-alias declarations, not in core
# logic.
```

## 10. Codegen include_str! Decorative Templates

**Path**: `skinny/crates/codegen/src/lib.rs:111-117`

**Reason**: D3 + D5 verified: `let _ = backend;` discards the BIR
explicitly, then `include_str!`s `templates/json/parser.rs` and
`templates/json/generated.rs` verbatim. The "BIR → Rust text" step is a
no-op pass-through; the BIR build itself is honest.

**This is not strictly a nuke; it's a replacement.**

**Wave**: Wave 1 (substrate authoring).

**Action**: replace the `let _ = backend;` + `include_str!` lines with
proper consumption of `&BackendIr` via `lower::lower_to_rust(&grammar,
backend, &shapes, &ctx)`. The existing template files at
`codegen/src/json_templates/` stay as REFERENCE / FALLBACK during the
transition; once `lower_to_rust` is regression-free against current
templates, the static templates can be deleted.

**Post-nuke verification**:
```bash
cargo run -p xtask --release -- gen --check
# Compare generated output to checked-in templates byte-for-byte for
# the JSON grammar; expect equality during transition, then drift as
# new cost-model shapes emerge.
```

## 11. nominate_json / hardcoded JSON in passes

**Path**: `skinny/crates/passes/src/lib.rs:28-29` (`compile()` hardcodes
`shapes_for_json()` regardless of input grammar) +
`materialization_for_rule` name-matching the 7 JSON rule names

**Reason**: Lock 14 violation. A `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE`
diagnostic case.

**Wave**: Wave 1 (substrate authoring).

**Action**:
- `shapes_for_json()` becomes `shapes_for_grammar(&Grammar)` — same
  algorithm but parameterised on grammar facts (rule kinds, host fn
  decls, error directives).
- `materialization_for_rule(rule_name: &str)` becomes
  `materialization_for_rule(rule: &Rule)` — consults `rule.kind` and
  `rule.metadata`, not literal string matching.
- `nominate_json` is split into a generic `nominate_recognizers(&Grammar)`
  that uses grammar facts.

**Post-nuke verification**:
```bash
cargo build --workspace
rg '"object"|"array"|"pair"|"string"|"number"|"bool"|"null"' skinny/crates/passes/src/
# Expect hits only in test fixtures or grammar-definition consumers,
# not in core logic.
```

## 12. Misplaced parse_integer_digest

**Path**: `skinny/crates/bbnf-bench/src/direct_struct.rs:501-528`

**Reason**: D1 verified. The integer materializer with proper
`i64::MIN` handling lives in the BENCH crate. It is grammar-neutral and
belongs in `parse-that-regex/src/number/integer.rs`.

**Wave**: Wave 2.

**Action**:
- Move the function to `parse-that-regex/src/number/integer.rs`.
- Update all callers to use the new path.
- Once the bench-private SinkParser is nuked (item #2), the original
  caller goes away too — the function lives only in its new home.

**Post-nuke verification**:
```bash
cargo build --workspace
rg "parse_integer_digest|parse_integer" skinny/crates/
# Expect hits in parse-that-regex/src/number/ and runtime/grammars/json/
# only; not in bbnf-bench/src/direct_struct.rs.
```

## 13. Documentation Drift — Wave 6 vs Wave 5

**Path**: `skinny/crates/bbnf-simd/src/x86_64/avx_ifma/mantissa.rs:37`

**Reason**: D1 found the `unimplemented!("Wave 6: vpmadd52luq …")`
references "Wave 6" but H.W6 in `MASTER-PLAN.md:510` is CSS SOTA gates,
not AVX-IFMA mantissa work. AVX-IFMA belongs to H.W5 (`MASTER-PLAN.md:509`).

**This is a documentation fix, not a code nuke.**

**Wave**: Wave 0 (alongside the strictness column update).

**Action**: Edit the `unimplemented!` message to reference H.W5 (or
delete the message; the kernel body lands in Wave 5).

## 14. Final Lock 14 Sweep

After items #7-#12 land, sweep for any remaining JSON leaks across all
generic crates:

```bash
rg -in "json|\"object\"|\"array\"|\"pair\"|\\{}\\[\\]" \
  skinny/crates/bbnf-simd/src/ \
  skinny/crates/parse-that-regex/src/ \
  skinny/crates/codegen/src/lower/ \
  skinny/crates/passes/src/ \
  skinny/crates/ir/src/
```

**Acceptance**: every hit is either:
- a generic dispatcher parameterised on grammar facts (e.g. taking an
  alphabet argument),
- a documentation comment,
- a test fixture for the JSON grammar specifically.

No hot kernel body, no hardcoded structural-character list, no
literal-rule-name match in generic crates.

**Wave**: Wave 4 final exit gate.

## 15. Verification Sequence

After all nukes execute:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo build --workspace --all-features
cargo test --workspace
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- primitive-checkasm
cargo run -p xtask --release -- gate-json
```

All must pass. The hard gate authority remains `skinny/RESULTS.md`; if
any nuke causes a regression, REDRESS.md records the regression and the
nuke is rolled back pending corrected wiring.

## 16. Anti-Patterns the Plan Refuses

The nuke list does NOT include:

- The Lock 1 storage substrate (`Tape<'input>`, `TapeBuilder`,
  `ValueRef`): A4 verified this holds cleanly with zero
  type-ambivalence. Leave alone.
- The Layer 0 vendored x86inc.asm (1,978 LOC, BSD-2 dav1d): grammar-neutral
  by definition. Leave alone.
- The Layer 1 bbnf.asm macro contracts (485 LOC, 9 macros):
  grammar-neutral by construction. Extend, do not nuke.
- The BYTE_CLASS_FROM_EQ_SET_64 primitive (commit 9eef728c): scalar
  reference + NEON + AVX-512 + checkasm gate all green. Leave alone.
- The `match_tiny_plain_string.rs` NEON kernel: parity-green; targets
  the wrong layer per D6 but the kernel itself is salvageable for a
  future grammar that does target that layer. Move to
  `bbnf-simd/src/aarch64/scan/` (with a different name reflecting its
  actual layer) rather than deleting outright.
- The `unescape_uxxxx.rs` NEON kernel: D6 confirmed wired and
  working at single-quartet; Wave 3 extends with `_x4` batched form.
  Leave the existing body; extend.
- The existing JSON grammar definition file: this is the grammar
  source of truth; not generic-crate code.

The nukes are surgical. The architecture is sound. The corrected
diagnosis from B1 (UTF-8 fusion) + D6 (Class A wrong layer) + D1
(Eisel-Lemire vendorable) is what changes; the substrate stays.
