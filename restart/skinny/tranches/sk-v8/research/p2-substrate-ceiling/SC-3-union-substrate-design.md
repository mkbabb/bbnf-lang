---
title: SC-3 — Tape ⊕ Structural-Projection Union Substrate Design
tranche: SK-V8
phase: P2 substrate-ceiling
scope: SC-3
date: 2026-05-17
status: design-proposal
kind: research
authority:
  - restart/ARCHITECTURE.md §1, §7.2, §7.3, §9
  - restart/locks/LOCKS.md Lock 1, Lock 14
  - restart/skinny/tranches/sk-v8/SPEC.md §1, §10
  - skinny/RESULTS.md (lazy-tape-materialization Notes)
constraints:
  - no new BBNF directive
  - no new BIR variant
  - no parallel/sidecar substrate — the union REPLACES the offset-tape
  - grammar-neutral (Lock 14)
  - all 5 BackendShapes remain expressible
---

# SC-3 — Tape ⊕ Structural-Projection Union Substrate

## §1 Current Substrate

### §1.1 What exists

The skinny offset-tape substrate is three small files plus one generated
consumer:

- `skinny/crates/runtime/src/tape/mod.rs:90-97` — `Tape<'input>` holds
  `source: &'input [u8]`, `offsets: Vec<u32>`, `flag_cursors: Vec<u32>`,
  `flag_values: Vec<u8>`, `payloads: PayloadArena`.
- `skinny/crates/runtime/src/tape/assembler.rs:42-124` — `TapeBuilder`
  appends offsets one at a time via `push_plain_offset`
  (`assembler.rs:71-85`) with a `#[cold]` geometric grow
  (`assembler.rs:87-91`). Flags are a sparse parallel pair patched in cursor
  order (`assembler.rs:94-113`).
- `skinny/crates/runtime/src/tape/offsets.rs:1-6` — `OffsetTapeStats`.

The structural producer is separate:

- `skinny/crates/bbnf-simd/src/lib.rs:72-97` — `StructuralIndex` is a
  `Vec<u32>` of structural byte positions plus a `ScanBackend` tag.
- `skinny/crates/runtime/src/grammars/json/scan.rs:21-29` —
  `scan_structurals` runs a branch-free NEON `vqtbl`-class block scan and
  produces a `StructuralIndex`.

### §1.2 The two-stage redundancy — the substrate ceiling

The hypothesis names the right defect. The SIMD structural scanner and the
offset-tape are **two separate artefacts produced by two separate passes that
never meet**:

1. `scan.rs:46-55` — `structural_capacity_for` runs `scan_structurals`
   (under `CapacityPlan::OneShotSimd`) **only to count** how many positions
   exist, then **throws the `Vec<u32>` away**, keeping only `.len()`.
2. `skinny/crates/runtime/src/grammars/json/generated.rs:14-17` —
   `attach_structural_index` is a literal **no-op** (`let _ = state;`).
   The SIMD index never reaches the parser.
3. `generated.rs:292-307` — `consume_structural` then **re-discovers every
   structural byte scalar-by-scalar** at `state.cursor`, and
   `parser.rs:34-37` `emit_plain_offset` appends each rediscovered position
   into `tape.offsets` one `u32` at a time.

So the offset-tape is rebuilt, position by position, by a scalar
recursive-descent walk — duplicating work the SIMD scanner already did and
discarded. simdjson/asmjson are two-stage *to amortise* (one SIMD index → one
structural-only tape build). bbnf is two-stage *to discard*: it pays the SIMD
scan and then pays the scalar rediscovery on top. This is the ceiling. The
`skinny/RESULTS.md` "lazy tape materialization" Notes lines confirm the tape
is already ~0.2–0.5× input in offset bytes — a fully-materialised offset
column that no consumer reads until view-walk time.

The throughput evidence: bbnf wins number-heavy corpora (`canada` +54.6%,
`mesh` +51.5%, `marine_ik`, `numbers` — corpora where structural density is
low and the scalar number scanner dominates) and loses string/structural-
heavy corpora badly (`gsoc-2018` −53.3%, `distinct_values` −61.2%,
`y_string_unicode` −54.1%, `apache_builds` −65.3% — corpora dense in `"` and
`{}[],:` where the per-byte structural rediscovery dominates). The split is
exactly the structural-byte-density axis. That is the substrate ceiling
expressing itself per corpus.

### §1.3 The 5-shape taxonomy today

`skinny/crates/codegen/src/lower/{offset_tape,event_tape,eager_tape,sink_only,
collapsed_stage}.rs` are all 17-line placeholder lowerers emitting
`format!("rule {} -> offset_tape", ...)` strings. `lower/mod.rs:17-25`
selects between them by `CostFacts.chosen`. The taxonomy is a contract, not
yet a substrate. This is the moment to land the union, before the lowerers
are filled with bodies that bake in the offset-tape shape.

---

## §2 The Union Design

### §2.1 Thesis

The structural projection **is** the tape. Lock 1 already says so verbatim:
"if structural offsets are retained, the structural projection IS the tape."
The defect is that today the structural offsets are *not* retained — they are
counted and discarded, then re-derived. The union is therefore not a new
artefact: it is the deletion of the rediscovery pass by **retaining the SIMD
scan output and making it the tape's offset column directly**.

One substrate, one producer, two columns that are co-resident and
co-allocated:

- the **offset column** — `Vec<u32>` of structural positions, written *once*
  by the SIMD compaction step;
- the **class column** — one byte per structural position naming which member
  of the `StructuralAlphabet` sits there (`{` `}` `[` `]` `:` `,` `"` for
  JSON), written *in the same SIMD pass* from the same classify mask.

The parser stops *producing* offsets. It becomes a **consumer** that walks the
class column, validates grammar well-formedness, and lazily materialises
scalar spans (numbers, string bodies) only when a consumer demands the value.
This is the two-stage win simdjson has: SIMD stage builds the index+classes;
the structural-validation stage walks it branch-predictably.

### §2.2 Data layout

The union is a single struct, `Tape<'input>`, replacing the current one. It
is **structure-of-arrays** keyed by one cursor index — the structural
ordinal. Every column is `index`-aligned: column `c[i]` describes the i-th
structural character.

```
                       Tape<'input>  — the union substrate
  ┌──────────────────────────────────────────────────────────────────┐
  │ source : &'input [u8]      borrowed input, zero-copy               │
  │ alphabet_id : u16          which StructuralAlphabet (data table)   │
  └──────────────────────────────────────────────────────────────────┘
                                  │  one SIMD pass writes ▼
  ┌──────────────────────────────────────────────────────────────────┐
  │ STRUCTURAL COLUMNS  (SoA, length = N structural chars, i-aligned)  │
  │                                                                    │
  │   offset : Vec<u32>     ── byte position of i-th structural char   │
  │   class  : Vec<u8>      ── StructuralClass id of i-th char         │
  │                            (alphabet ordinal: 0..K, K = |alphabet|)│
  │                                                                    │
  │   index i:   0    1    2    3    4    5    6   ...                  │
  │   offset  [  0,   1,   8,   9,  10,  17,  18, ...]                  │
  │   class   [ '{', '"', '"', ':', '{', '"', '"',...]   ← JSON LUT    │
  └──────────────────────────────────────────────────────────────────┘
  ┌──────────────────────────────────────────────────────────────────┐
  │ SPAN-FACT COLUMN  (sparse, populated only by EventTape shape)      │
  │   facts : Vec<(u32 cursor, u8 flags)>   ── escape/control bits,    │
  │                                            recovery, layout side  │
  │                                            facts; cursor-ordered   │
  │                                            binary-searched.        │
  │   (this is today's flag_cursors/flag_values pair, generalised and  │
  │    kept SPARSE — most grammars touch it zero times.)               │
  └──────────────────────────────────────────────────────────────────┘
  ┌──────────────────────────────────────────────────────────────────┐
  │ PAYLOAD ARENA  (lazy, opt-in; empty for borrowed-view shapes)      │
  │   payloads : PayloadArena   ── decoded/normalised scalar bytes,    │
  │                                written ONLY when a shape needs an  │
  │                                owned value (unescaped string,      │
  │                                widened number). Borrowed views     │
  │                                never touch it; counters confirm    │
  │                                0/0 today.                          │
  └──────────────────────────────────────────────────────────────────┘
```

The `offset` and `class` columns are **the same length** and **written
together** by the SIMD compaction step. There is no second `offsets`
producer. `class` costs one extra byte per structural char — for `twitter`
that is +29.5 KiB against a 118 KiB offset column (~+25%), and it *deletes*
the entire scalar `consume_structural` rediscovery loop, which is the
dominant cost on string-dense corpora. The classify mask that SIMD already
computes to *find* a structural byte is the same mask that *names* it; the
class column is free at the mask level and costs only the compressed store.

### §2.3 The one-pass branch-free producer

`scan_structurals` already runs the branch-free `byte_class_from_table_64`
NEON kernel (`scan.rs:24`, `lib.rs:110-117`). Today `compact_mask`
(`lib.rs:115`) compresses the mask to **positions only**. The union changes
`compact_mask` to compress to **positions + classes in lockstep**: for each
set bit, emit `(cursor+bit_index)` into `offset` and emit the LUT-mapped
class byte (the `STRUCTURAL_CLASS_TABLE_LO6`-style table already in
`scan.rs:11-19`) into `class`. One `vqtbl` classify, one bit-compaction,
two co-indexed stores. No second scan, no branch per byte.

The escape/quote-pairing carry (`scan_tail`'s `in_string`/`escaped` state,
`scan.rs:111-119`) stays exactly where it is — it is the transient mask
refinement, not a retained column. It only decides *whether* a `"`-class
position is a string boundary; it writes nothing extra.

### §2.4 How the second materialization pass is eliminated

Today: SIMD pass (discarded) → scalar rediscovery pass → offset column.
Union: SIMD pass → offset+class columns (**retained, this IS the tape**) →
parser is a *validator/walker*, not a *producer*.

`attach_structural_index` (`generated.rs:14`) stops being a no-op: it moves
the retained `StructuralIndex` (now carrying classes) into the
`TapeBuilder`. `consume_structural` (`generated.rs:292-307`) — the scalar
rediscovery hot leaf — is **deleted**. In its place the parser advances a
`cursor: u32` over the `class` column: to consume a `{` it asserts
`tape.class[cursor] == CLASS_LBRACE` and `cursor += 1`. That is a
branch-predictable array read, not a byte scan with whitespace-skip fallback.

Zero-copy borrowed-view output is *preserved by construction*: the `offset`
column already holds byte positions into `source`; `ValueRef`
(`mod.rs:171-217`) keeps its `(tape, cursor)` shape unchanged; `offset()`
(`mod.rs:212-216`) still resolves `offsets[cursor]`. Scalar spans
(number text, string body) are *not* materialised at parse time — they are
recovered lazily from `source[offset[i]..offset[i+next]]` when the view
demands them, exactly as the `RESULTS.md` "lazy tape materialization" /
"0/0 writes/allocations" Notes already report. The union does not weaken
laziness; it removes the eager *structural* rediscovery while keeping the
*scalar* laziness already in place.

### §2.5 Why this is not a new substrate (Lock 1)

The union has **one producer** (the SIMD compaction step), **one retained
artefact** (`Tape`), **one cursor identity** (`(TapeId, cursor, class)`).
There is no sidecar: the `StructuralIndex` ceases to be a free-standing
discarded `Vec` and becomes the tape's own column. The mask stream remains a
transient producer per ARCHITECTURE.md §9.1. Nothing parallel is added; one
scalar pass (`consume_structural`) is *removed*. This is strictly fewer
substrates than today. Lock 1's no-parallel-substrate clause is honoured by
subtraction.

---

## §3 BackendShape Projection

All five shapes project onto the *same* union columns. The shape selects
*which columns are populated* and *how the cursor walks them* — never a
different substrate. The codegen lowerers under `lower/*.rs` emit the walk;
the runtime owns the columns.

| Shape | offset col | class col | facts col | payload arena | Cursor discipline |
|---|---|---|---|---|---|
| `OffsetTape` | yes | yes | empty | empty (lazy) | Retained doc. Parser validates by walking `class`; `ValueRef` borrows `source` via `offset[cursor]`. The canonical union shape. |
| `EventTape` | yes | yes | **populated** | lazy | Retained doc carrying per-cursor side facts (escape/control flags, recovery marks, layout depth) in the sparse `facts` column — today's `flag_cursors`/`flag_values`, generalised. |
| `EagerTape` | yes | yes | optional | optional | Retained doc, but the cursor reads `source[pos]` *eagerly* for rules with first-set overlap / `@error` / `@host` / `@layout` (per `derive_backend_shape` steps 1–4). The class column still backs the structural skeleton; eager byte reads handle only the ambiguous sub-rules. The union is a superset — eager rules simply bypass the class fast-path. |
| `SinkOnly` | yes (transient) | yes (transient) | empty | n/a | Parser walks the union columns to drive typed-field writes, then **drops the `Tape`**: no retained document identity. The union is the parse-time scaffold; the SoA columns are freed at `finish()`. SOTA direct shape. |
| `CollapsedStage` | fused | fused | n/a | n/a | The AVX-512/asmjson FSM does not separate "find structural" from "build column" at all — the mask-held state walk *is* the union, collapsed into one pass with no retained `Vec`. The union's data-layout is the *uncollapsed* form of exactly this; `CollapsedStage` is the union with `offset` and `class` never spilling to memory. The taxonomy's extreme point, still the same artefact conceptually. |

The key property: `OffsetTape`, `EventTape`, `EagerTape`, `SinkOnly` differ
only in `facts`-column population and in whether the `Tape` survives
`finish()`. `CollapsedStage` is the same columns held in registers. No shape
needs a substrate the others lack. The 5-shape taxonomy is **fully
expressible** and, in fact, *clarified* — today the shapes are five empty
lowerers; under the union they are five well-defined column/cursor disciplines
over one substrate.

---

## §4 Grammar-Neutral Generalisation

### §4.1 StructuralAlphabet is the generic seam

`StructuralAlphabet` already exists (`bbnf-simd/src/lib.rs:20-50`) as a
grammar-neutral 256-entry membership table built `from_bytes`. The JSON
alphabet `b"{}[],:\""` is **one instance** constructed by codegen
(`scan.rs:6-7`). The union generalises by making the **class LUT** —
`StructuralClass id ← byte` — a codegen-emitted data table per grammar, not
hard-coded runtime logic.

The generic substrate (`runtime/src/tape/`) carries:

- `StructuralAlphabet` — already neutral.
- a `StructuralClassTable` — a `[u8; 256]` mapping byte → class ordinal
  (`0` = non-structural, `1..K` = alphabet members in declaration order).
  This is the generalisation of `STRUCTURAL_CLASS_TABLE_LO6`
  (`scan.rs:11-19`). It is **data**, emitted by codegen into the per-grammar
  `scan.rs`, consumed by the generic `compact_mask`.
- `Tape` with `offset`/`class`/`facts`/`payloads` columns — fully neutral;
  `class` values are opaque ordinals the generic code never interprets.

Per-grammar variation lives **only** in the codegen-emitted data table
(`StructuralClassTable`) and the per-grammar wrapper dir
(`runtime/src/grammars/<name>/`). The generic substrate never branches on
grammar (Lock 14). `compact_mask` reads class ordinals it does not understand;
the generated parser walks ordinals it *does* understand because codegen
emitted both the table and the walk.

### §4.2 Instances

**JSON** (current): alphabet `{ } [ ] : , "`, K = 7.

```
StructuralClassTable (JSON):  '{'→1  '}'→2  '['→3  ']'→4  ':'→5  ','→6  '"'→7
                              all others → 0
```

**CSS L4**: the structural alphabet is the token-boundary set
`{ } ( ) [ ] : ; , @`. A SIMD block-classify finds rule-block braces,
declaration semicolons, selector-list commas, at-rule sigils, and
function-call parens. K = 10.

```
StructuralClassTable (CSS L4): '{'→1 '}'→2 '('→3 ')'→4 '['→5 ']'→6
                               ':'→7 ';'→8 ','→9 '@'→10   others → 0
```

The CSS parser then walks the `class` column: a `;`-class position closes a
declaration, a `{`-class opens a rule block, a `@`-class opens an at-rule.
Property values and selector text are lazy scalar spans recovered between
structural positions — exactly the JSON number/string laziness.

**Google Sheets** (formula/cell grammar): the structural alphabet is
`( ) , ; { } : & = + - * / "`  — formula parens and arg separators, array
literal braces, range colons, the cell-ref/string quote. K ≈ 14. The same
`compact_mask` produces the same SoA columns; the Sheets parser walks them.

**BBNF-self**: alphabet `= ; | ( ) [ ] { } < > , " /` — rule terminators,
alternation bars, grouping. The self-hosting parser becomes a class-column
walker over its own grammar.

In every case the *only* per-grammar artefact is the `StructuralClassTable`
data table plus the generated walk in `grammars/<name>/`. The substrate
(`tape/`), the SIMD compaction, and `StructuralAlphabet`/`StructuralIndex`
are byte-for-byte identical. No new directive expresses the alphabet — it is
already mined from grammar literals into the existing `SimdScan` recognizer
(ARCHITECTURE.md §7.2 `SimdScan` row; `passes::recognizers` first-set/literal
mining). The class table is a codegen *projection* of the same mined fact.

---

## §5 Migration Sketch — Offset-Tape → Union

### §5.1 What changes

**`skinny/crates/bbnf-simd/src/lib.rs`** (~+40 LOC)
- `StructuralIndex` gains a `classes: Vec<u8>` column alongside `positions`.
  `from_positions` → `from_positions_and_classes`; `positions()` keeps its
  signature; add `classes()`.
- `compact_mask` (`lib.rs:115`) emits class ordinals in lockstep with
  positions, reading a `&[u8; 256]` class table argument.
- `scan_dispatch` / NEON `neon::scan` thread the class table through.

**`skinny/crates/runtime/src/tape/`** (~+90 LOC, ~−10 LOC)
- `mod.rs` — `Tape` gains `classes: Vec<u8>`; `class_at(cursor) -> u8`;
  `from_offsets` → `from_columns`. `flag_cursors`/`flag_values` keep their
  shape but are renamed to the `facts` column for clarity (mechanical).
- `assembler.rs` — `TapeBuilder::new` takes the retained
  `StructuralIndex`; `push_plain_offset`/`reserve_offsets_cold` are
  **deleted** (the offset column arrives whole from SIMD, not appended).
  `TapeBuilder` becomes a thin column-mover + sparse-`facts` patcher.
- `offsets.rs` — `OffsetTapeStats` gains `class_bytes`.

**`skinny/crates/runtime/src/grammars/json/`** (generated; regenerated, not
hand-patched per `feedback_generated_files_clean_regen`)
- `scan.rs` — `structural_capacity_for` and the discard-the-`Vec` path
  collapse; `scan_structurals` is called once, its full output retained.
  `StructuralClassTable` constant emitted here.
- `generated.rs` — `attach_structural_index` moves the retained index into
  the builder; `consume_structural` (~16 LOC) **deleted**; consume sites
  become `class`-column cursor reads. `parser.rs` `emit_plain_offset`
  **deleted**.

**`skinny/crates/codegen/src/lower/`** (~+120 LOC)
- `offset_tape.rs` / `event_tape.rs` / `eager_tape.rs` / `sink_only.rs` /
  `collapsed_stage.rs` — the five 17-line placeholders gain real bodies that
  emit the column/cursor disciplines of §3. This is net-new lowering code,
  but it is *required regardless* — the union just defines what they emit.
- codegen emits the `StructuralClassTable` from the mined `SimdScan`
  alphabet fact.

### §5.2 LOC estimate

| Area | Delta |
|---|---:|
| `bbnf-simd` (class column) | +40 |
| `runtime/src/tape/` | +90 / −10 |
| generated `grammars/json/` | −30 (rediscovery deleted) regenerated |
| `codegen/src/lower/` | +120 (placeholder → real, required anyway) |
| **Net source** | **≈ +210 LOC** |

This sits inside a W2/W3-class source LOC budget (SPEC.md §2: W3 450). It is
*not* a W0/W1 change — W0 is telemetry-only, W1 is CostFacts-gate-only
(SPEC.md §1: "No SK-V8 implementation wave dispatches before G-Alpha"; W0/W1
forbid parser/codegen behaviour change). The union is a **W3 candidate**
(profile-selected parse candidate, SPEC.md §6) and must be challenge-proven
not a renamed REDRESS 60–72 retained-parse route — it is not: REDRESS 60–72
rejected *sidecar producers and retained side-table columns added during
parse*; the union *removes* a pass and retains *fewer* artefacts. It must
carry the SPEC §6 entry gate: scalar reference + checkasm for the modified
`compact_mask` primitive.

### §5.3 Risk class

**Medium.** It is a substrate replacement, but a *contracting* one — it
deletes a producer and a hot leaf rather than adding. The blast radius is
bounded: `Tape`, `TapeBuilder`, one SIMD function, and regenerated JSON
output. No new BIR variant, no new directive, no new public API name. The
five-shape lowerer fill is the larger half of the LOC and is mandatory work
the union merely *directs*.

---

## §6 Risks — REDRESS to Pre-Block

| # | Risk | Pre-block / mitigation |
|---|---|---|
| R1 | The union reads as a "parser-owned structural projection" or "retained aux column" — SPEC.md §6 fails any such on Lock 1. | Frame the W3 challenge precisely: the union *deletes* the scalar rediscovery pass and retains *one* artefact where today two exist (discarded `StructuralIndex` + appended offset `Vec`). It is fewer substrates, not more. The `class` column is not an *aux* column — it is the *primary* structural identity, replacing the implicit class that `consume_structural`'s byte-match recovered per call. |
| R2 | REDRESS 50 rejected "parse-time retained projection side tables" — dense/sparse aux columns that regressed the parse plane. The `class` column could be read as exactly that. | REDRESS 50 rejected columns *written during the scalar parse pass*. The `class` column is written by the *SIMD pass*, before parse, and the parse pass becomes a pure *reader*. The regression mode REDRESS 50 found (parse plane pays to write a column the view plane reads) is inverted here: the parse plane pays *less* (no rediscovery) and reads more. Must be proven with a before/after parse-plane bench row. |
| R3 | The class column adds ~+25% to retained tape bytes; could regress RSS-sensitive or cache-bound corpora. | `SinkOnly` shape frees the columns at `finish()` — zero retained cost for direct workloads. For retained shapes, the +1 byte/structural is offset by deleting the per-call whitespace-skip fallback in `consume_structural`. Bench the structural-dense corpora (`gsoc-2018`, `distinct_values`, `apache_builds`) where the win is predicted; if RSS regresses without throughput gain, the column packs to 4-bit nibbles (K ≤ 15 for every named grammar). |
| R4 | `CollapsedStage` "fused" projection may not obviously reconcile with a memory-resident `offset`/`class` SoA. | The design states it explicitly: `CollapsedStage` is the union with the columns held in registers and never spilled. The taxonomy point is preserved by *definition*, not by a separate substrate. No code lands for `CollapsedStage` until a per-grammar kernel author is in flight (ARCHITECTURE.md §7.3 same-wave-consumer rule). |
| R5 | Grammar-neutrality slip: the JSON `StructuralClassTable` could leak into the generic `tape/` crate. | The class table is emitted by codegen into per-grammar `scan.rs`; the generic `compact_mask` takes it as a `&[u8; 256]` argument and never names a grammar. Lock 14 audit: `rg 'Json|CssL4' crates/runtime/src/tape crates/bbnf-simd/src` must stay zero. The W5 grammar-neutral audit (SPEC.md §8) covers this. |
| R6 | The number-heavy corpora (`canada`, `mesh`, `numbers`) currently *win*; the union must not regress them. | Those corpora are structural-sparse — the `class` walk replaces a short `consume_structural` path and the scalar number scanner is untouched. Predicted neutral-to-slight-positive. Guard rows per SPEC.md §0.5 ("Guard row for numeric/bitmap changes"); W3 maintain budget −2.0% applies. |
| R7 | Capacity planning (`CapacityPlan` A–D, `assembler.rs:13-40`) becomes vestigial — the offset column arrives exact-sized from SIMD. | This is a *simplification*, not a risk: `CapacityPlan::OneShotSimd` becomes the only plan and the env-var selector retires. Fold the deletion into the same W3 slice; it removes ~50 LOC and the `BBNF_CAPACITY_PLAN` surface. |

---

## §7 Sources

- `skinny/crates/runtime/src/tape/mod.rs:90-97,171-217` — current `Tape`,
  `ValueRef`.
- `skinny/crates/runtime/src/tape/assembler.rs:42-124` — `TapeBuilder`,
  `push_plain_offset`, `CapacityPlan`.
- `skinny/crates/runtime/src/tape/offsets.rs:1-6` — `OffsetTapeStats`.
- `skinny/crates/runtime/src/grammars/json/scan.rs:6-55,111-119` —
  `STRUCTURAL_BYTES`, `STRUCTURAL_CLASS_TABLE_LO6`, `scan_structurals`,
  `structural_capacity_for`, `scan_tail`.
- `skinny/crates/runtime/src/grammars/json/generated.rs:14-17,292-307` —
  `attach_structural_index` no-op, `consume_structural` rediscovery.
- `skinny/crates/runtime/src/grammars/json/parser.rs:6-43` — `ParserState`,
  `emit_plain_offset`.
- `skinny/crates/bbnf-simd/src/lib.rs:20-127` — `StructuralAlphabet`,
  `StructuralIndex`, `scan_dispatch`, `compact_mask`.
- `skinny/crates/codegen/src/lower/{mod,offset_tape,event_tape,eager_tape,
  sink_only,collapsed_stage}.rs` — placeholder lowerers.
- `restart/ARCHITECTURE.md` §7.2 (BIR `SimdScan`/`TapeEmit`), §7.3
  (`BackendShape` enum + `derive_backend_shape`), §9.1/§9.2 (tape +
  direct-to-struct union, runtime materialization model).
- `restart/locks/LOCKS.md` Lock 1 (substrate union; "structural projection
  IS the tape"), Lock 14 (grammar-neutrality).
- `restart/skinny/tranches/sk-v8/SPEC.md` §1 (non-negotiables), §6 (W3
  parse candidate gate), §10 (pre-blocked routes).
- `skinny/RESULTS.md` — per-corpus throughput split; "lazy tape
  materialization" and "0/0 writes/allocations" Notes lines.
