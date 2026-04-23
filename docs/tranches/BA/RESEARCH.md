# Tranche BA — Supplementary Research

Concrete external references that inform BA's substrate choices.
This document is consumed alongside `BA.md`; it holds the technique
detail that would otherwise bloat the plan.

## simdjson tape record layout

simdjson's tape is a flat `uint64_t[]` where each slot is a tagged
element:

- High 8 bits: element tag (`'{'`, `'}'`, `'['`, `']'`, `'"'`,
  `'d'`, `'l'`, `'u'`, `'t'`, `'f'`, `'n'`, `'r'`).
- Low 56 bits: payload (offset into input, offset to paired
  container element, or interpreted value).

Compound containers appear twice: an opening slot at the start and
a closing slot at the end. Each points at the other via the 56-bit
payload. Numbers, strings, and other leaves occupy a single slot;
strings carry an offset into a separate byte tape that holds the
length-prefixed decoded UTF-8.

Source: [simdjson tape docs](https://simdjson.github.io/simdjson/md_doc_tape.html).
BA adopts the paired-slot discipline with bbnf's 16-byte `TapeRec`
as the per-slot record instead of a 64-bit tagged integer. The bbnf
record already holds `kind`, `child_off`, `payload_idx`, and a kind-
specific discriminator; the backward pointer fits into the close
record's existing `child_off` slot (which is zero on close records
today).

## Backward container pointer indexing

The addressing scheme BA lands is:

- Open record: `child_off` = index of matching close record (already
  present).
- Close record: `child_off` = index of matching open record (new).

Both pointers are `u32` offsets into the same `TapeRec` vector, not
byte offsets into the input. This gives O(1) navigation in either
direction without requiring the input bytes to be in cache.

The alternative — a sidecar `parent_idx: Vec<u32>` column — costs
4 bytes per close record but keeps the 16-byte record intact. The
choice depends on W3's measurement: if the in-record form thrashes
L1 for large documents (twitter ~600 KB), the sidecar wins; if the
in-record form keeps hot lookups in a single cache line, the
in-record form wins.

Era V's `Columns` split made sidecar columns first-class (AY-I.W1
later reverted the specific split); BA's W3 is not a return to
seven columns but a targeted addition of a single backward-pointer
column if the profile demands it. The single-column form is
compatible with `Columns::rollback_to`, which AY-II already canonicalised
as the rollback primitive.

## sonic-rs pointer! + StructRegistry analogue

sonic-rs exposes two APIs relevant to BA / BB:

1. **`pointer!["a", "b", 1]`** — a macro that builds a
   `PointerTree`, which is then traversed over a `LazyValue`
   without fully parsing the document. Sibling keys and
   off-path array elements are skipped via simdjson-shape
   structural navigation.
2. **serde `Deserialize` direct-to-struct** — via the
   `serde_json_fast` hot path, sonic-rs populates user structs
   directly, avoiding an intermediate `serde_json::Value`.

Sources:
- [sonic-rs repo](https://github.com/cloudwego/sonic-rs)
- [sonic-rs LazyValue docs](https://docs.rs/sonic-rs/latest/sonic_rs/lazyvalue/struct.LazyValue.html)
- [sonic-cpp](https://github.com/bytedance/sonic-cpp) — the C++
  antecedent that informed sonic-rs's design.

bbnf's analogue is structurally stronger: the grammar declares the
struct shape, so a `StructRegistry` populated from the
`project_types` IR pass produces typed accessors without a user
deriving `Deserialize`. The registry maps `(GrammarId, NamedRuleId)
→ StructLayout`, where `StructLayout` records field names, typed
offsets into a compound's child sequence, and the payload kind of
each field. Every field projection in a typed view consults the
registry, not a runtime match on rule kind.

BA lands the registry population; BB consumes it for pointer-path
queries. The split is intentional: BA proves every `->` reaches the
tape in a fully-eager traversal; BB introduces laziness on top of
the activated substrate.

## lightningcss CSS property type-generation pipeline

lightningcss generates one Rust type per CSS property via
`lightningcss-derive`, a proc-macro that reads property declarations
from a macro invocation and emits `Deserialize` + `ToCss`
implementations. Typed values (`Length`, `Percentage`, `Angle`,
`Time`, `Resolution`, `Color`) are modelled as enums covering every
spec-legal representation; round-tripping through parse / serialize
preserves typed structure.

Sources:
- [Lightning CSS main site](https://lightningcss.dev/)
- [lightningcss repo](https://github.com/parcel-bundler/lightningcss)
- [lightningcss-derive](https://lib.rs/crates/lightningcss-derive)

bbnf's position inverts the declaration direction. The CSS L4
grammar at `grammars/css/css-l4.bbnf` declares property rules with
typed `->` returns; the bbnf IR type-inference pass deduces the
typed accessor shape per rule; the emitter produces
`NodeView<CssL4, DeclarationId>` with field accessors whose return
types are the inferred types. No proc-macro, no hand-maintained
enum list, no out-of-band type registry — the grammar IS the type
registry. `feedback_direct-to-struct-approach` names this: a
generalised regex-to-value conversion with no hard-coded pattern
lists.

BA's CSS L4 parity gate is that every `<length>` production in the
grammar returns a `Length` that is convertible to lightningcss's
`lightningcss::values::length::Length` without loss and that
round-trips through the parity harness. The gate applies per-rule,
not as a whole-file comparison, so partial coverage is reportable
as partial coverage rather than a single pass / fail bit.

## yyjson — dispatch and allocation over SIMD

yyjson (pure C89, no mandatory SIMD) outperforms simdjson on modern
EPYC by exploiting ILP, branch prediction, and low misaligned-access
penalty. The benchmark number reported is 1.72 GB/s vs simdjson's
1.52 GB/s on canada.

Source: [yyjson introduction](https://ibireme.github.io/yyjson/),
[yyjson repo](https://github.com/ibireme/yyjson).

The lesson for BA: SIMD is not where the next ~10% lives. bbnf is
already SIMD-heavy in scanners (delim-scan, NibbleLut, Pratt LUT
propagation). The frontier is dispatch (key dispatch, structural
dispatch) and allocation (in-place payload projection). AP.4 key
dispatch and AP.5 NibbleLut are examples of the dispatch frontier;
BA's payload activation is the allocation partner. The backward
container pointer belongs to the dispatch frontier too — it removes
a class of re-scans from accessor code paths.

## Era V reversal archaeology

The reversals that land AU-baseline matter to BA because BA is
the tranche that recovers it. Key Era V reversal points:

| Reversal | Commit | Cost |
|---|---|---|
| Structural pre-scan (simdjson-style bitmap) | `2f7c1bd` (AQ.5) | ~32 commits sunk before delete |
| EmissionTier lattice | `7608530` (AM.1) | ~2000 LOC dead |
| DTA interpreter + walker | AX.W0b cluster | ~572 tranche-tagged commits sunk |
| AW-V shape-emitter thesis | AX.W0c (`AW-V-rewrite`) | 80 commits rewritten |
| Hand-coded `bbnf::json::Value` + `bbnf::css::StyleSheet` | `3429aaba` (AX.W1r.0) | 6,128 LOC deleted |
| Column split (seven-Vec substrate) | AY-I.W1 | ~400 commits sunk |

Sources: `docs/tranches/AQ/AQ.md`, `docs/tranches/AM/AM.md`,
`docs/tranches/AX/FINAL.md`, `docs/tranches/AY-I/FINAL.md`.

The pattern: every reversal's cost scales with how long the
substrate sat without an activated consumer. AQ.5 was cheap
(~32 commits) because the pre-scan was recently landed; AW-I
through AW-V cost ~572 commits because seven substrates stacked
without any one fully activating. BA's reversal discipline (one
wave = one decision surface = one runtime call site = wave-local
20% revert) is the inverse of Era V's failure mode.

## IR audit pass — concrete form

The audit pass at `crates/ir/src/passes/audit/payload_coverage.rs`
enumerates:

```
for each Grammar in workspace:
  for each Rule with typed `->`:
    let T = project_types(rule).return_type
    assert emitter_call_site(rule) in {
      push_leaf_with_f64,
      push_leaf_with_i64,
      push_leaf_with_u64,
      push_leaf_with_bool,
      push_leaf_with_span,
      push_leaf_with_named(T),
      begin_compound(T) && end_compound(T),
    }
    else fail(rule, T, "no emitter call site for typed ->")
```

The pass runs on every `cargo check` via build.rs and emits a
JSON report to `docs/benchmarks/BA/audit/coverage.json` with
per-grammar / per-rule resolution status. `feedback_no-silent-epsilon`
applies: the pass panics on unknown rule kinds rather than silently
succeeding.

## Measurement fleet layout

BA's `docs/benchmarks/profiles/BA/` directory follows the Era V /
Era VI convention:

```
docs/benchmarks/profiles/BA/
  W0/  cold-parse baseline over 17-entry matrix
  W1/  post-scalar-payload (JSON + Sheets)
  W2/  post-struct-registry (CSS L4)
  W3/  post-backward-pointer
  W4/  final + samply fleet per grammar
```

Each wave directory holds:
- `bench-matrix.json` — 17-entry cold-parse / MB/s table.
- `samply-<grammar>.json` — samply profile per grammar family.
- `audit-coverage.json` — IR audit pass output.
- `parity-harness.json` — sonic-rs / lightningcss / simdjson /
  serde_json / cssparser parity summary.

`feedback_no-warm-benches` applies: cold-parse numbers only.
`feedback_samply-symbol-resolution` applies: `samply record`
interactive, not `--save-only`.

## Derive-cache lift

The lift target is `$XDG_CACHE_HOME/bbnf-derive/` with a
fingerprint keyed on:

- Grammar source file content hash.
- `bbnf-derive` crate version.
- Host `rustc` version.
- Relevant codegen feature flags.

Expected hit rate on iteration (derive cache active, grammar
unchanged, small Rust edit) is >95% based on AY-II.W0' observed
derive-rebuild share. The lift is a W0 landing; later waves depend
on the fast-iteration path it establishes.

## Cross-reference: where BA connects to BB / BC

- BB consumes BA's `StructRegistry` + backward pointer for
  pointer-path typechecking and ondemand-style lazy skip.
- BC consumes BA's `project_types` output + the IR audit coverage
  surface as the "settled semantic surface" over which rewrite
  inference operates.
- Neither downstream tranche opens until BA's handoff contract is
  met.

## Anti-precedents to avoid

Concrete patterns BA does not repeat:

1. **AW-V's shape-emitter-for-JSON-only.** The thesis must work
   for every grammar at every wave, not one grammar at one wave.
2. **AO phase-0 activation failure.** Ship the runtime consumer
   with the substrate, never before.
3. **AI-era EmissionTier lattice.** One decision surface, not
   two.
4. **AX.W1.A / AX.W1.B hand-coded values.** Grammar-derived or
   not at all.
5. **AW-IV "every entry exceeds post-AU" with zero entries
   exceeding.** Gate at the 20% floor; revert on miss.
