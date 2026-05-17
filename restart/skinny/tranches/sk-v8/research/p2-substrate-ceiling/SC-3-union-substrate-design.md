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

One retained substrate, one producer, two mandatory columns that are
co-resident and co-allocated:

- the **offset column** — `Vec<u32>` of structural positions, written *once*
  by the SIMD compaction step;
- the **class column** — one byte per structural position naming which member
  of the `StructuralAlphabet` sits there (`{` `}` `[` `]` `:` `,` `"` for
  JSON), written *in the same SIMD pass* from the same classify mask.

Cardinality invariant: after construction there is exactly one retained
`Tape`. `StructuralIndex` is consumed by move into `Tape`; it has no query
surface, clone path, cache path, attach-after-build path, parser-owned cursor,
or post-build API. The builder may receive the scan product only to move its
columns into the tape; any surviving independent `StructuralIndex` falsifies
the design.

The parser stops *producing* offsets. It becomes a **consumer** that walks the
class column, validates grammar well-formedness, and lazily materialises
scalar spans (numbers, string bodies) only when a consumer demands the value.
This is the two-stage win simdjson has: SIMD stage builds the index+classes;
the structural-validation stage walks it branch-predictably.

### §2.2 Data layout

The union is a single struct, `Tape<'input>`, replacing the current one. It
uses **co-indexed internal tape columns** keyed by one cursor index — the
structural ordinal. Every column is `index`-aligned: column `c[i]` describes
the i-th structural character. This is not Lock 1's rejected dead columnar
SoA substrate: there is no separate columnar artefact beside the tape, and no
consumer can retain or query a second projection.

```
                       Tape<'input>  — the union substrate
  ┌──────────────────────────────────────────────────────────────────┐
  │ source : &'input [u8]      borrowed input, zero-copy               │
  │ alphabet_id : u16          which StructuralAlphabet (data table)   │
  └──────────────────────────────────────────────────────────────────┘
                                  │  one SIMD pass writes ▼
  ┌──────────────────────────────────────────────────────────────────┐
  │ STRUCTURAL COLUMNS  (length = N structural chars, i-aligned)       │
  │                                                                    │
  │   offset : Vec<u32>     ── byte position of i-th structural char   │
  │   class  : Vec<u8>      ── mandatory StructuralClass id of i-th    │
  │                            char; primary structural identity       │
  │                            (alphabet ordinal: 0..K, K = |alphabet|)│
  │                                                                    │
  │   index i:   0    1    2    3    4    5    6   ...                  │
  │   offset  [  0,   1,   8,   9,  10,  17,  18, ...]                  │
  │   class   [ '{', '"', '"', ':', '{', '"', '"',...]   ← JSON LUT    │
  └──────────────────────────────────────────────────────────────────┘
  ┌──────────────────────────────────────────────────────────────────┐
  │ SPAN-FACT COLUMN  (sparse, populated only by admitted shapes)      │
  │   facts : Vec<FactRecord>       ── generated opaque fact ids keyed  │
  │                                    by structural cursor; generic    │
  │                                    code stores/searches records but │
  │                                    never interprets the ids.        │
  │   FactRecord { cursor: u32, fact_id: u16, payload: u32 }           │
  │   (this is today's flag_cursors/flag_values pair re-specified as   │
  │    sparse generated facts. Most grammars touch it zero times.)      │
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
producer. `class` is mandatory primary structural identity: scan-written
only, never parser-patched, never optional for retained shapes, and never
backfilled from byte rediscovery. `class` costs one extra byte per structural
char — for `twitter` that is +29.5 KiB against a 118 KiB offset column
(~+25%), and it *deletes* the entire scalar `consume_structural` rediscovery
loop, which is the dominant cost on string-dense corpora. The classify mask
that SIMD already computes to *find* a structural byte is the same mask that
*names* it; the class column is free at the mask level and costs only the
compressed store.

The `facts` column is deliberately narrow and uses **opaque generated fact
ids**, not a generic recovery/layout enum. `runtime/src/tape/` may store facts,
sort by cursor, and binary-search by `(cursor, fact_id)`, but it must not
`match` on a fact id, name JSON/JSONL/CSS/indentation policy, or branch on a
grammar. The generated grammar module owns both the fact-id table and the
meaning. This preserves Lock 14 better than a closed neutral vocabulary because
reused punctuation or layout bytes can have parser-state-specific meaning
without forcing that meaning into generic runtime code.

The facts lane does **not** admit density tables, quote caches, skip caches,
profile counters, parser-owned slots, per-consumer caches, or any fact with an
independent lifetime from the tape. Unlisted recovery or layout fact ids are
out of scope for Tier A.

### §2.3 Fact admission matrix

Tier A admits only the current JSON retained-parse fact slice below. JSONL and
indentation-sensitive rows are Lock 14 examples that prove the storage model is
grammar-neutral; they are not Tier A implementation scope until a later grammar
owner file exists and the same challenge gate accepts it.

| Fact name/id | Producer | Consumer | Owner file | Cursor domain | Lifetime | Challenge gate |
|---|---|---|---|---|---|---|
| `json.fact.0` (`string_escape_or_control`, opaque generated id) | Generated JSON string scanner when the opening-quote structural cursor owns a span requiring escape/control handling. | Generated JSON retained view/EventTape code that decides whether to borrow the span or consult payload materialization. | `skinny/crates/runtime/src/grammars/json/generated.rs`; generated from `skinny/crates/codegen/src/json_templates/generated.rs` and consumed by `skinny/crates/runtime/src/grammars/json/view.rs`. | Structural ordinal of the opening `"` class for the scalar span. | Stored only in the retained `Tape`; dropped with the `Tape`; never copied into a parser-owned table. | Tier A admitted only if current `OffsetFlags::HAS_ESC` semantics are preserved, `rg 'match .*fact_id|json.fact|JSONL|indent' skinny/crates/runtime/src/tape skinny/crates/bbnf-simd/src` is zero, and generated JSON parity/conformance tests pass. |
| `jsonl.fact.0` (`record_boundary`, opaque generated id) | A future generated JSONL scanner/parser whose generated class table includes its record terminator byte(s). | Future generated JSONL parser/recovery code that treats a cursor as a record boundary in parser state. | Owner-path family: `skinny/crates/runtime/src/grammars/jsonl/{scan.rs,generated.rs,view.rs}` generated from a JSONL grammar module. | Structural ordinal of the generated record-terminator class, for example the newline cursor between two JSON values. | Stored only in that grammar's `Tape`; no global "record" lifetime or sidecar event vector. | Lock 14 example only in Tier A. Admission requires a generated owner, no generic newline/record branch, and CSS/Sheets/BBNF-self no-op proofs if generic crates changed. |
| `layout.fact.0` (`indent_delta`, opaque generated id) | A future generated indentation-sensitive scanner/parser at logical-line start. | Future generated indentation-sensitive parser state that interprets payload as indent/dedent/equal only inside that grammar. | Owner-path family: `skinny/crates/runtime/src/grammars/<indent-grammar>/{scan.rs,generated.rs,view.rs}`. | Structural ordinal of the first generated line-start/layout class for a logical line; payload is grammar-owned. | Stored only in the retained `Tape`; no generic indent stack, cache, or parser-owned cursor. | Lock 14 example only in Tier A. Admission requires no `indent`, `dedent`, or newline policy in `runtime/src/tape` or `bbnf-simd`, plus generated grammar tests named by S-P3. |

JSONL example:

```text
input bytes:  {"a":1}\n{"a":2}\n
generated JSONL class ids:  {→1 "→2 "→2 :→3 }→4 \n→5 {→1 ...
generated facts:            (cursor=5, fact_id=jsonl.fact.0, payload=0)
generic runtime action:     store `(5, opaque-id, 0)` beside the tape and
                             return it only when generated JSONL code asks.
forbidden generic action:   `if byte == b'\n' { end_json_record(); }`
```

Indentation-sensitive example:

```text
input bytes:  parent\n  child\nsibling\n
generated layout classes:   line-start/indent bytes are grammar class ids,
                             not generic whitespace policy.
generated facts:            (cursor=1, fact_id=layout.fact.0, payload=+2)
                             (cursor=2, fact_id=layout.fact.0, payload=-2)
generic runtime action:     store opaque payloads sorted by cursor.
forbidden generic action:   maintain an indent stack or interpret payload sign
                             outside the generated grammar module.
```

### §2.4 The one-pass branch-free producer

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
refinement, not a retained column. Tier A does not retain string-boundary,
quote, backslash, or parity masks; it only emits structural cursor ordinals
and opaque class ids. String-boundary closure belongs to Tier B.

### §2.5 How the second materialization pass is eliminated

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
(number text, string body) are *not* materialised at parse time. Tier A still
allows existing string-body consumers to find the closing boundary with their
current generated string scanner; it does **not** claim string-boundary closure
or deletion of quote/backslash/parity work. The Tier A win is narrower: it
removes eager *structural* rediscovery while keeping the *scalar* laziness
already in place. Tier B is the only scope that may claim strings consume
bounds from the union cursor.

### §2.6 Why this is not a new substrate (Lock 1)

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
| `EventTape` | yes | yes | **populated** | lazy | Retained doc carrying admitted per-cursor facts only: opaque generated fact ids from §2.3 in the sparse `facts` column. Generic code stores and searches records; generated grammar modules interpret them. Unlisted recovery or layout fact ids are out of scope for Tier A. |
| `EagerTape` | yes | yes | optional | optional | Retained doc, but the cursor reads `source[pos]` *eagerly* for rules with first-set overlap / `@error` / `@host` / `@layout` (per `derive_backend_shape` steps 1–4). The class column still backs the structural skeleton; eager byte reads handle only the ambiguous sub-rules. The union is a superset — eager rules simply bypass the class fast-path. |
| `SinkOnly` | yes (transient) | yes (transient) | empty | n/a | Parser walks the union columns to drive typed-field writes, then **drops the `Tape`**: no retained document identity. The union is the parse-time scaffold; the co-indexed internal tape columns are freed at `finish()`. SOTA direct shape. |
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
  `class` values and `fact_id` values are opaque generated ordinals the
  generic code never interprets.

Per-grammar variation lives **only** in the codegen-emitted data tables
(`StructuralClassTable`, optional generated fact-id table) and the per-grammar
wrapper dir (`runtime/src/grammars/<name>/`). The generic substrate never
branches on grammar (Lock 14). `compact_mask` reads class ordinals it does not
understand; the generated parser walks ordinals and fact ids it *does*
understand because codegen emitted the tables and the walk. Event-role meaning
is interpreted only inside generated grammar modules, keyed by parser state plus
class/byte, so a reused punctuation byte is not forced into one global generic
role.

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
`compact_mask` produces the same co-indexed internal tape columns; the Sheets
parser walks them.

**BBNF-self**: alphabet `= ; | ( ) [ ] { } < > , " /` — rule terminators,
alternation bars, grouping. The self-hosting parser becomes a class-column
walker over its own grammar.

In every case the *only* per-grammar artefacts are generated data tables
(`StructuralClassTable`, optional fact-id table) plus the generated walk in
`grammars/<name>/`. The substrate (`tape/`), the SIMD compaction, and
`StructuralAlphabet`/`StructuralIndex` are byte-for-byte identical. No new
directive expresses the alphabet or facts — they are mined from grammar
literals/layout declarations already flowing into the existing `SimdScan`
recognizer (ARCHITECTURE.md §7.2 `SimdScan` row; `passes::recognizers`
first-set/literal mining). The class and fact tables are codegen projections
of those existing mined facts.

---

## §5 Migration Sketch — Offset-Tape → Union

### §5.1 What changes

Cardinality invariant for every migration slice: exactly one retained `Tape`
may survive. `StructuralIndex` is a move-only scan product consumed into that
`Tape`; no `positions()`/`classes()` query API may survive past build except
through `Tape`, no clone or cache is allowed, `attach_structural_index` cannot
be a post-build attachment hook, and generated parsers must not own an
independent structural cursor.

**Tier A: structural-class cursor migration requiring W3 challenge proof**

Tier A has one production scope: migrate the retained JSON Track 1
structural walk from scalar byte rediscovery to scan-written
`offset`+`class` cursor reads, and keep the retained view/`ValueRef` consumer
working in the same wave. Existing JSON EventTape-style escape/control
patching is admitted only as `json.fact.0` from §2.3. `tape_vs_tape`
telemetry, direct/SinkOnly rows, `path!`, and Track 2 do **not** count as the
same-wave production consumer for Tier A; they are explicit
touched/proven-untouched audit rows below.

Out of scope for Tier A: string-boundary closure, quote/backslash/parity
masks, CostFacts-template parity, density policy, production migration of
non-JSON grammars, `CollapsedStage`, and the five placeholder lowerer bodies
beyond the generated JSON retained walk needed for the same-wave consumer. If
Tier A edits generic crates (`bbnf-simd`, `runtime/src/tape/`, or generic
codegen tables), the Lock 14 proof for CSS L4, Sheets, and BBNF-self is
inside Tier A as no-op/diff evidence, not as production parser migration.

**`skinny/crates/bbnf-simd/src/lib.rs`** (~+40 LOC)
- `StructuralIndex` gains a `classes: Vec<u8>` column alongside `positions`.
  `from_positions` → `from_positions_and_classes` internally, then the
  index is move-consumed into `Tape`; no post-build `positions()` or
  `classes()` query surface survives.
- `compact_mask` (`lib.rs:115`) emits class ordinals in lockstep with
  positions, reading a `&[u8; 256]` class table argument.
- `scan_dispatch` / NEON `neon::scan` thread the class table through.
- Scalar oracle placeholder:
  `skinny/crates/bbnf-simd/src/scalar.rs::compact_mask_positions_classes_oracle`.
- checkasm placeholder:
  `checkasm_bbnf_simd_compact_mask_positions_classes`.

**`skinny/crates/runtime/src/tape/`** (~+115 LOC, ~−25 LOC)
- `mod.rs` — `Tape` gains `classes: Vec<u8>`; `class_at(cursor) -> u8`;
  `from_offsets` → `from_columns`. `class` is mandatory scan-written primary
  identity; parser patching is forbidden and is row-falsified against REDRESS
  50 aux-side-table regressions. `flag_cursors`/`flag_values` are replaced or
  re-specified as sparse `FactRecord` storage for admitted opaque generated
  fact ids only.
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

Tier A S-P3 owner/cost table:

| Slice / audit row | Owner files or owner-path families | Source LOC | Generated-output audit | Row / plane target | Same-wave production consumer | Named tests and commands | Revert slice |
|---|---|---:|---|---|---|---|---|
| SIMD `offset`+`class` producer | `skinny/crates/bbnf-simd/src/lib.rs`; new/updated `skinny/crates/bbnf-simd/src/scalar/compact_mask_positions_classes.rs`; `skinny/crates/bbnf-simd/tests/{classifier_parity.rs,corpus_parity.rs,checkasm_byte_class_from_table_64.rs,checkasm_compact_mask_positions_classes.rs}` | +55 | n/a | Structural-scan plane: all 17 JSON fixtures keep scalar/SIMD parity hash; `simd_structural_scan/{fixture}_simd` rows present after bench refresh. | Generated JSON retained parser consumes the moved `StructuralIndex` in the same wave. | `compact_mask_positions_classes_oracle`, `checkasm_compact_mask_positions_classes`, `cargo test -p bbnf-simd --test classifier_parity --test corpus_parity`, `BBNF_SIMD_STRICT=1 cargo xtask primitive-checkasm`. | Revert `bbnf-simd` producer/scalar/test files together; no parser changes may remain with a positions-only producer. |
| Runtime `Tape` / `TapeBuilder` / facts | `skinny/crates/runtime/src/tape/{mod.rs,assembler.rs,offsets.rs}` | +115 / −25 | n/a | Retained-parse plane: cursor and `offset_at`/`class_at` share one structural ordinal; no old offset append path; opaque fact lookup only. | `ValueRef::offset()` and retained `JsonDocument` view read the new tape shape. | Add `runtime::tape_union::{from_columns_rejects_len_mismatch,class_at_and_offset_at_share_cursor,opaque_fact_lookup_has_no_semantics}`; `cargo test -p runtime`. | Revert `runtime/src/tape/` as a unit; if reverted, generated JSON must also revert to `TapeBuilder::push_offset`. |
| Generated Track 1 retained JSON parser (touched) | Templates: `skinny/crates/codegen/src/json_templates/{scan.rs,generated.rs,parser.rs,view.rs}`. Generated output: `skinny/crates/runtime/src/grammars/json/{scan.rs,generated.rs,parser.rs,view.rs}`. | +60 templates / −30 regenerated | `cargo xtask check-json`; generated diff must contain only class-table emission, structural-index move, `consume_structural` deletion, and `json.fact.0` rewrite. | Retained JSON parse plane: all `json/<fixture>/track1_generated` strict rows remain valid; number-heavy guard rows `canada`, `mesh`, `numbers` no worse than −2.0%; structural-heavy rows `twitter`, `apache_builds`, `gsoc-2018`, `distinct_values`, `y_string_unicode` report the scalar rediscovery leaf removed. | This is the Tier A production consumer. It must validate UTF-8/control/escape work in the measured row; no `tape_vs_tape` substitute. | `cargo xtask check-json`, `cargo xtask check-conformance`, `cargo bench -p bbnf-bench --bench json_parity`, `cargo xtask gate-json --advisory`. | Revert codegen templates and regenerated JSON output together; never hand-patch generated files as the revert boundary. |
| Retained view / `ValueRef` (touched) | `skinny/crates/runtime/src/grammars/json/{view.rs,value.rs}` generated from `skinny/crates/codegen/src/json_templates/{view.rs,value.rs}`; `skinny/crates/runtime/src/tape/mod.rs` | +20 templates/runtime | Covered by `cargo xtask check-json`; generated output must keep public view API stable. | Retained-view plane: `bbnf_bench::parity::assert_parity` token stream stays equal to Track 2 for all fixtures; payload counters remain reported. | Same retained `Tape` as parser; no view-owned cursor/fact cache. | `cargo xtask check-conformance`; `cargo bench -p bbnf-bench --bench json_parity -- json/twitter/track1_generated`. | Revert with Track 1 parser slice; a view-only fallback cursor is forbidden. |
| `path!` consumer (proven untouched unless S-P3 finds one) | Current audit command owns this row: `rg 'path!' skinny/crates`. If a `path!` macro exists by S-P3, owner family is the generated retained-view path for that grammar. | 0 | n/a | Must be proven untouched: no `path!` source diff and no cursor-specific shim. | Not a Tier A consumer unless S-P3 explicitly expands scope with owner paths/tests. | `rg 'path!' skinny/crates` before and after; if nonzero, S-P3 must name a test such as `json_path_cursor_uses_tape_class_cursor`. | No revert slice unless touched; any touch promotes this row from "proven untouched" to same-wave consumer scope. |
| Direct/SinkOnly rows (proven untouched) | `skinny/crates/codegen/src/lower/{sink_only.rs,schema_direct.rs}`; `skinny/crates/runtime/src/grammars/json/sink.rs`; `skinny/crates/bbnf-bench/src/{direct_struct.rs,generated_real_typed.rs,real_typed_struct.rs}` | 0 | `cargo xtask check-real-typed` must be clean; no generated real-typed diff. | Direct plane rows `track1_direct_to_struct`, `track2_direct_to_struct`, and real-typed rows are guard rows only; no regression worse than −2.0% if touched by accident. | Not a Tier A production consumer. | `cargo xtask check-real-typed`; `cargo bench -p bbnf-bench --bench json_parity -- json/twitter/track1_direct_to_struct`. | Any direct/SinkOnly diff is either reverted before Tier A lands or S-P3 expands the plan with same-wave direct consumers. |
| Track 2 independent oracle (proven untouched) | `skinny/crates/bbnf-bench/src/track2/json.rs`; `skinny/crates/bbnf-bench/src/parity.rs` | 0 | n/a | Track 2 remains structurally independent and does not call generated Track 1, generated SinkOnly, or new tape internals. | Oracle only, not production consumer. | `git diff --exit-code -- skinny/crates/bbnf-bench/src/track2/json.rs skinny/crates/bbnf-bench/src/parity.rs`; `bbnf_bench::parity::assert_parity`. | Any Track 2 source diff must be routed as a separate W2/W4 proof, not hidden inside Tier A. |
| Lock 14 / no-new-substrate proof if generic crates change | `skinny/crates/codegen/src/{lib.rs,lower/,json_templates/}`; `skinny/crates/runtime/src/tape/`; `skinny/crates/bbnf-simd/src/`; no new `skinny/crates/runtime/src/grammars/<non-json>/` production files in Tier A | +60 tests/audits | Add no-op/diff tests named `lock14_css_l4_structural_table_noop_diff`, `lock14_sheets_structural_table_noop_diff`, and `lock14_bbnf_self_structural_table_noop_diff`; generated JSON diff remains the only production generated-output diff. | Grammar-neutrality plane: CSS L4, Sheets, and BBNF-self examples compile/lower/cost without generic JSON roles; no public generic grammar API. | The generic producer and runtime remain grammar-blind while JSON Track 1 consumes the data. | `rg 'Json|JSON|jsonl|record|indent|dedent|newline' skinny/crates/runtime/src/tape skinny/crates/bbnf-simd/src`; `rg 'UnionTape|BackendShape::Union' skinny/crates`; `git diff -- skinny/crates/grammar skinny/crates/ir skinny/crates/passes skinny/crates/codegen | rg '^\\+.*(directive|BIR|BackendShape)'`; `cargo xtask gate-json --with-cost-facts --advisory`. | Revert all generic-crate changes if the grep/API scan finds a generic grammar branch, new directive, new BIR variant, public `UnionTape`, or second substrate. |

Full-gate rerun budget if S-P3 promotes Tier A: one focused
scalar/checkasm run, one generated-output audit, and one full SK-V8 gate
refresh:

```sh
cd skinny
cargo xtask check-json
cargo xtask check-real-typed
cargo xtask check-conformance
cargo xtask lint-loc
BBNF_SIMD_STRICT=1 cargo xtask primitive-checkasm
cargo bench -p bbnf-bench --bench simd_scan
cargo xtask bench-json --advisory
cargo xtask gate-json --with-cost-facts --advisory
```

Risk: medium. It replaces a parser hot leaf and modifies the scan primitive,
but the same-wave production consumer list is now bounded to generated JSON
Track 1 retained parse plus retained view/`ValueRef`. Direct/SinkOnly, `path!`,
Track 2, string-boundary closure, and CostFacts-template parity are either
proven untouched or explicitly Tier B. A second full-gate rerun requires a
REDRESS cost note rather than becoming hidden Tier A scope.

### §5.2 Tier B — string-boundary / quote-backslash-parity / CostFacts-template union

Tier B is follow-on work, not part of the narrow Tier A challenge candidate.
It routes the larger string-boundary union: quote/backslash/parity masks,
string-boundary cursor facts, CostFacts/template parity, density admission,
non-JSON grammar table generation, and any additional recovery/layout fact
admissions. These items must prove they remain transient masks or admitted
opaque tape facts under the §2.3 matrix; retained quote caches, skip caches,
density tables, parser-owned slots, profile counters, or per-consumer caches
remain banned.

Tier B owns the larger `skinny/crates/codegen/src/lower/` fill:
`offset_tape.rs`, `event_tape.rs`, `eager_tape.rs`, `sink_only.rs`, and
`collapsed_stage.rs` growing from placeholders into complete lowerers, plus
template parity for CostFacts. SC-3 does not price that as a single W3 slice;
it is multi-wave unless S-P3 supplies an explicit 650-LOC template-parity plan
with same-wave consumers and accepted challenge proof.

### §5.3 Admission posture

Tier A sits inside a W3-class source LOC budget only as a **candidate
requiring W3 challenge proof**. It is *not* a W0/W1 change — W0 is
telemetry-only, W1 is CostFacts-gate-only (SPEC.md §1: "No SK-V8
implementation wave dispatches before G-Alpha"; W0/W1 forbid parser/codegen
behaviour change). S-P2 does not select or prescribe it. The candidate must
prove it is not a renamed REDRESS 50 or REDRESS 60–72 retained-parse route:
REDRESS 50 rejected parser-written aux side tables, and REDRESS 60–72
rejected sidecar producers and retained side-table columns added during
parse. SC-3 can satisfy that REDRESS challenge only with W3 evidence showing
one retained tape, scan-written mandatory class identity, no surviving
`StructuralIndex`, no parser-owned aux slots or fact cursors, no generic
interpretation of generated fact ids, scalar/checkasm parity for the modified
`compact_mask`, same-run no-regression rows, and no claim that Tier A closes
the string-boundary plane.

**Medium.** It is a substrate replacement, but a *contracting* one — it
deletes a producer and a hot leaf rather than adding. The blast radius is
bounded for Tier A: `Tape`, `TapeBuilder`, one SIMD function, and regenerated
JSON output. No new BIR variant, no new directive, no new public API name or
generic grammar API. The five-shape lowerer fill and full string-boundary /
CostFacts-template union are Tier B work.

---

## §6 Risks — REDRESS to Pre-Block

| # | Risk | Pre-block / mitigation |
|---|---|---|
| R1 | The union reads as a "parser-owned structural projection" or "retained aux column" — SPEC.md §6 fails any such on Lock 1. | Frame the W3 challenge precisely: the union *deletes* the scalar rediscovery pass and retains *one* artefact where today two exist (discarded `StructuralIndex` + appended offset `Vec`). It is fewer substrates, not more. The `class` column is not an *aux* column — it is the *primary* structural identity, replacing the implicit class that `consume_structural`'s byte-match recovered per call. |
| R2 | REDRESS 50 rejected "parse-time retained projection side tables" — dense/sparse aux columns that regressed the parse plane. The `class` column could be read as exactly that. | REDRESS 50 row-falsifier: `class` must be mandatory primary structural identity, scan-written only, and never parser-patched. Facts must be admitted only through the §2.3 matrix and stored by the tape builder with tape lifetime. If any parser pass owns an independent class/fact slot or cursor, the candidate fails as a REDRESS 50 aux-side-table regression. Must be proven with a before/after parse-plane bench row. |
| R3 | The class column adds ~+25% to retained tape bytes; could regress RSS-sensitive or cache-bound corpora. | `SinkOnly` shape frees the columns at `finish()` — zero retained cost for direct workloads. For retained shapes, the +1 byte/structural is offset by deleting the per-call whitespace-skip fallback in `consume_structural`. Bench the structural-dense corpora (`gsoc-2018`, `distinct_values`, `apache_builds`) where the win is predicted; if RSS regresses without throughput gain, the column packs to 4-bit nibbles (K ≤ 15 for every named grammar). |
| R4 | `CollapsedStage` "fused" projection may not obviously reconcile with memory-resident `offset`/`class` columns. | The design states it explicitly: `CollapsedStage` is the union with the co-indexed internal tape columns held in registers and never spilled. The taxonomy point is preserved by *definition*, not by a separate substrate. No code lands for `CollapsedStage` until a per-grammar kernel author is in flight (ARCHITECTURE.md §7.3 same-wave-consumer rule). |
| R5 | Grammar-neutrality slip: the JSON `StructuralClassTable` or fact ids could leak into the generic `tape/` crate. | The class and fact tables are emitted by codegen into per-grammar files; generic `compact_mask` takes a `&[u8; 256]` class table and never names a grammar. Lock 14 audit: `rg 'Json|JSON|jsonl|record|indent|dedent|newline' crates/runtime/src/tape crates/bbnf-simd/src` and `rg 'UnionTape|BackendShape::Union' crates` must stay zero; `git diff -- crates/grammar crates/ir crates/passes crates/codegen | rg '^\\+.*(directive|BIR|BackendShape)'` must show no new directive, BIR variant, or substrate/API addition. The W5 grammar-neutral audit (SPEC.md §8) covers this. |
| R6 | The number-heavy corpora (`canada`, `mesh`, `numbers`) currently *win*; the union must not regress them. | Those corpora are structural-sparse — the `class` walk replaces a short `consume_structural` path and the scalar number scanner is untouched. Predicted neutral-to-slight-positive. Guard rows per SPEC.md §0.5 ("Guard row for numeric/bitmap changes"); W3 maintain budget −2.0% applies. |
| R7 | Capacity planning (`CapacityPlan` A–D, `assembler.rs:13-40`) becomes vestigial — the offset column arrives exact-sized from SIMD. | This is a *simplification*, not a risk: `CapacityPlan::OneShotSimd` becomes the only plan and the env-var selector retires. Fold the deletion into the same W3 slice; it removes ~50 LOC and the `BBNF_CAPACITY_PLAN` surface. |
| R8 | Tier A is over-sold as a string-plane close. | Tier A is only structural-class cursor migration. It may report structural rediscovery deletion and retained-view parity, but it must not claim quote/backslash/parity deletion, string-boundary closure, CostFacts-template parity, or a moved JSON string-fraction knee. Those are Tier B and require their own owner table and challenge proof. |

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
