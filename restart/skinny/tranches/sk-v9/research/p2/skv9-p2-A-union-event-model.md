# SK-V9 P2-A: Union Event-Model — W3 Fit-Gate Diagnosis + Alternate Design

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-18.
Scope: Diagnose SK-V8 W3 fit-gate rejection (REDRESS 92) and design an
alternate event-model that admits the union substrate (the retained SIMD
structural index IS the tape) without re-opening pre-blocked routes.
Output: this file.
P1 hot-leaf antecedents: `scan_structurals` 0.00% self-time on every
(corpus, track) row (dead SIMD index); `consume_structural` /
`match_tiny_plain_string` / `match_string_at_quote` 47–67% self-time on
string-dense losses; `JsonNodeKind::at_cursor` per-cursor source-byte
rediscovery; `read_hex_unit_scalar` 38–44% on `y_string_unicode`.
Lock surface: Lock 1 (substrate union + cardinality); Lock 14 (grammar
neutrality); Lock 16 (Layer-1 primitive vocabulary).

## §1 — W3 fit-gate diagnosis: what failed, and why

### §1.1 — The headline finding

W3 Tier A's hypothesis — "retain the SIMD structural index and make it
the tape" — is architecturally correct and is **not** what the fit gate
rejected. The fit gate rejected the **storage-only swap** framing of
that hypothesis: substituting `Vec<u32>` producers and treating the move
as a representation change. The two artefacts are not isomorphic. They
encode different events keyed at different cursor positions, and the
retained-view contract (`ValueRef` → `JsonNodeKind::at_cursor`) is
written against the parser-event tape, not the structural-byte index.

Citation: `restart/skinny/tranches/sk-v8/research/skv8-W3-tape-structural-research.md`
§Finding lines 7–49; the executable example at lines 38–43:

```text
input: {"a":[1,true]}
current retained tape offsets: [0, 1, 5, 6, 8, 12, 13]
scanner structural positions:  [0, 1, 3, 4, 5, 7, 12, 13]
```

The two cursor streams differ at three classes of positions:

1. **Scanner has, tape omits**: the *closing* quote of every string (key
   quotes especially: position 3 in the example); the colon (`:`,
   position 5); the comma (`,`).
2. **Tape has, scanner omits**: the *opening byte* of every scalar — the
   first digit of a number, the first letter of `true`/`false`/`null`
   (position 8 in the example: the `1` of `[1,true]`). The scanner only
   retains structural punctuation plus quotes; the tape carries scalar
   *anchors* the structural alphabet does not name.
3. **Both have, but for different reasons**: container opens/closes, the
   *opening* quote of every string. Coincidentally equal positions, but
   the *meaning* (and the row's downstream consumer) diverges.

### §1.2 — Why the retained-view contract breaks under a storage-only swap

The consumer side is `skinny/crates/runtime/src/grammars/json/value.rs`
lines 29–47 — `JsonNodeKind::at_cursor`:

```rust
match tape.source()[offset] {
    b'{' => Self::ObjectOpen,
    ...
    b'-' | b'0'..=b'9' => Self::Number,
    b't' => Self::True,  // etc.
}
```

The view recovers the event-class by **reading the source byte at the
cursor's offset** and matching its byte value. This works only because
the parser-event tape's cursor scheme guarantees that `tape.source()
[offset_at(cursor)]` *is* a class-determining byte: `{`, `[`, `,`, `:`,
`"`, a digit, `t`, `f`, or `n`. The current generated parser
(`skinny/crates/runtime/src/grammars/json/generated.rs` line 280–306
`consume_structural` + line 35 `parser.rs::emit_plain_offset`) preserves
that invariant by writing one offset per *event* — opens/closes, opening
quotes only (closing quote elided), number start, literal start. The
scanner index, by contrast, writes one position per *structural byte*
under the JSON alphabet `b"{}[],:\""` (`generated.rs:10`). It has no
position at digit/`t`/`f`/`n`; it has positions at `:` and `,` and
closing quotes that the tape and the view's match arms never see.

So a storage-only swap — assigning `tape.offsets = scanner.positions` —
would (i) lose the number/literal anchor cursors the parser needs to
emit and the view needs to interpret, and (ii) introduce
colon/comma/close-quote cursors the view's match expects to be
unreachable (the `unreachable!()` arm at `value.rs:45` would fire). The
view's class-recovery itself is also a hot-path defect — it pays a
**per-cursor random-access source-byte read** to recover an event the
SIMD pass already knew at classify time — but it cannot simply be
deleted, because today the tape carries no class column.

### §1.3 — Why the W3 plan named the gap as "event-model mismatch"

`restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md` §Fit Gate lines
32–66 enumerates the required owner surface for a same-wave fit:
`bbnf-simd/src/lib.rs`, `runtime/src/grammars/json/scan.rs`,
`runtime/src/tape/{mod,assembler}.rs`,
`runtime/src/grammars/json/{generated,parser,view,value}.rs`, the four
matching codegen templates, and the bench `parity.rs` /
`materialization.rs` / `gate.rs`. Estimated > 450 LOC default +
> 650 LOC exceptional budget + > 90-minute cap. The plan named this a
**split event-model redesign**, because no representation-only edit can
simultaneously (a) feed the parser its scalar-anchor cursors, (b) feed
the view its event-class recovery, and (c) reduce to one retained
artefact — without redesigning the *event vocabulary* the tape carries.

REDRESS 92 verbatim (`skinny/REDRESS.md:2661-2690`) ratifies that
verdict: "The scanner/tape event model is not isomorphic: the scanner
retains structural punctuation plus real quotes, while the current
retained tape is a generated parser event stream containing container
opens/closes, opening quotes, number starts, and literal starts. … No
source patch or rejected patch artifact exists for W3 because the
accepted plan failed the pre-redress fit gate."

The routing line: SK-V9 must "define the retained class/event grammar
including numbers/literals and string quote ownership, prove the
retained `ValueRef` cursor contract over that grammar, and only then
reopen a measured structural-heavy parse row wave."

### §1.4 — What W3 *proposed* as the event-model

W3 Tier A's working model was: extend the structural alphabet's
projection with an **opaque class column** co-indexed with
`scanner.positions`, and move it whole into `Tape`. Each `class[i]` was
to be a generated ordinal naming which alphabet member (`{ } [ ] : , "`
for JSON) sat at `offset[i]`. See SC-3 §2.1–§2.4 (`restart/skinny/
tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md
:140-178`).

That model handles three of the seven JSON event classes
(`ObjectOpen/Close`, `ArrayOpen/Close`, `MemberSeparator`,
`ElementSeparator`, and the opening *and* closing of strings — six if
you separate open/close, seven counting both colon and comma). It does
**not** handle the other four (`Number`, `True`, `False`, `Null`) which
have no byte in the structural alphabet. SC-3 deferred those to
"existing string-body consumers" and "scalar laziness" (§2.5) — a hand-
wave that papered over the fit gap, because the *cursor* for "this is a
Number" must still be emitted somewhere and consumed by the view.

That is the structural defect W3 surfaced and the plan rejected: the
class column alone cannot carry the four scalar-anchor events.
Adding number/literal anchors as additional class ordinals would force
the SIMD scanner to recognise digits and the four ASCII literal-leads
in its alphabet — which is feasible (`BYTE_CLASS_FROM_EQ_SET_64` admits
arbitrary 64-byte equality sets, Lock 16 line 70) but inflates the
alphabet from 7 entries to roughly 18 (`{ } [ ] : , "` plus `- 0..9
t f n`) and conflates *structural skeleton* with *scalar entry*. The
SC-3 design did not propose that fold; it stopped at the seven-byte
alphabet and elided the scalar-anchor question.

## §2 — Alternate event-model design

### §2.1 — Thesis: separate the *cursor stream* from the *class stream*

The W3 design conflated two questions: (a) what *positions* does the
tape carry, and (b) what *events* does each position name? SC-3 made
both come from the SIMD pass, and that forced the alphabet to carry the
union of both questions. The alternate model splits them:

- **Position stream** is the existing parser-event tape's cursor scheme,
  unchanged: open-of-container, open-of-string, start-of-scalar, close-
  of-container, separators only when they are events the view consumes
  (today: close-of-container; comma/colon are walked but not emitted as
  cursors — see `generated.rs:310-339` `consume_container_next` which
  walks `,` without `emit_plain_offset`). The cursor *count* is
  preserved row-for-row across all 17 corpora.
- **Class stream** is a *co-indexed* byte column on the tape, written
  by the parser at the same call site that produces the offset
  (`emit_plain_offset` becomes `emit_event_offset(offset, class)`). The
  class enumerates the seven `JsonNodeKind`-equivalent ordinals
  (`ObjectOpen`, `ObjectClose`, `ArrayOpen`, `ArrayClose`, `String`,
  `Number`, `Literal` — `True`/`False`/`Null` collapse to one class
  with a one-byte payload; or three classes if codegen prefers; both
  are admissible).
- **SIMD structural index** drives the cursor walk **inside the
  parser**, not the tape's storage. The parser reads
  `index.positions[i]` to know where the next structural byte lives,
  uses `index.classes[i]` (the structural-alphabet ordinal — `{ } [ ]
  : , "`, the seven-byte set) to decide whether to dispatch to
  `parse_object`/`parse_array`/`parse_string`/`parse_member_separator`/
  `parse_element_separator`/`close`, and emits the *parser-event*
  cursor + class into the tape. The structural index is **consumed by
  move** during parse and is not retained: it never reaches the view.

The cardinality invariant (Lock 1) is preserved: there is exactly one
retained substrate, the tape. The structural index is a *transient
producer*, exactly as Lock 1 already permits ("A SIMD mask stream is a
transient producer, not a retained sidecar"). The tape gains a class
column; the offset column's cardinality is unchanged.

### §2.2 — Data layout

```
                        Tape<'input>  — alternate union
  ┌──────────────────────────────────────────────────────────────────┐
  │ source : &'input [u8]            zero-copy borrowed input         │
  │ offsets : Vec<u32>               parser-event cursor stream       │
  │                                  (UNCHANGED count + ordering)     │
  │ classes : Vec<u8>                co-indexed event-class ordinals  │
  │                                  (NEW; one byte per cursor)       │
  │ flag_cursors : Vec<u32>     )                                     │
  │ flag_values  : Vec<u8>      )    extant sparse fact column        │
  │ payloads     : PayloadArena      lazy scalar payloads (unchanged) │
  │ id           : TapeId                                             │
  └──────────────────────────────────────────────────────────────────┘
```

The class ordinals are **opaque generated ids** per Lock 14: the
generic substrate (`runtime/src/tape/`) stores `Vec<u8>` and exposes
`class_at(cursor) -> u8`; it does not match on the byte. Generated
grammar modules carry the meaning. For JSON the ordinal set is the
existing `JsonNodeKind` minus the byte-rediscovered `*Separator` arms
(those become parser-internal walk state, not view cursors).

LOC: tape grows by `~+1 byte / cursor`. For `twitter` (~30 KiB
offsets at 4 bytes/cursor) that is `~+8 KiB`, recovering all of it (and
more) by deleting `at_cursor`'s per-cursor random-access source-byte
read on traversal — the hot leaf hidden from S-P1's static-byte hot-
leaf taxonomy because it is amortised across view operations rather
than the parse loop.

### §2.3 — Primitive interface

The structural-index *producer* is unchanged at the Layer-1 vocabulary
level: `EOB_PAD_CLAMP → BYTE_CLASS_FROM_EQ_SET_64(json_alphabet) →
BITMAP_PREFIX_XOR_64 → BITMAP_NEXT_SET_BIT → BULK_EMIT_COMPRESSED`. The
producer emits `(positions, classes)` co-indexed under the **structural
alphabet** (seven bytes for JSON), not the parser-event class set
(seven ordinals: `ObjectOpen`/etc.). These are *different* opaque ids
in different domains, and the mapping is generated:

| Structural class (scanner) | Parser-event class (tape) | Mapping site |
|---|---|---|
| `{` → s1 | `ObjectOpen` → e1 | `parse_object` entry: emit e1 at the s1 position |
| `}` → s2 | `ObjectClose` → e2 | `parse_object` close: emit e2 at the s2 position |
| `[` → s3 | `ArrayOpen` → e3 | analogous |
| `]` → s4 | `ArrayClose` → e4 | analogous |
| `:` → s5 | (none — walk only) | `parse_member` advances index cursor, no tape emit |
| `,` → s6 | (none — walk only) | `parse_container_next` advances index cursor, no tape emit |
| `"` → s7 | `String` (on the *opening* `"` only) → e5 | `parse_string` entry: emit e5 at the first s7; second s7 advances index cursor only |
| (no structural class — digits, `t`/`f`/`n`) | `Number` → e6 / `Literal` → e7 | `parse_value_at` reads `index.positions[i+1] - 1`-ish gap; the first non-whitespace byte between two structural positions is the scalar anchor; emit e6/e7 at that byte's offset |

The crucial admissibility move: the SIMD scanner does *not* need
digits/`t`/`f`/`n` in its alphabet. The parser walks the structural
index, and scalars live in the *gaps between structural positions*.
The parser locates the scalar anchor by reading
`input[skip_ws(positions[i-1]+1)]` once per scalar — exactly today's
cost — but it does so **without re-doing the structural rediscovery
that `consume_structural` performs per-byte**, because the structural-
position cursor is monotone and known.

This is the alternate event-model's load-bearing claim: the four
scalar-anchor classes (`Number`, `Literal`, plus the not-emitted
separators) are not held in the SIMD alphabet — they are derived by
the parser from the *gaps* in the structural index. The class column
on the tape names the parser-event class (not the structural class),
and the parser writes it at the offset where the gap-derived scalar
begins.

### §2.4 — How this satisfies the W3 plan's three load-bearing
constraints

1. **One retained substrate.** The structural index is move-consumed
   by the parser; only the tape survives. The tape carries `offsets`,
   `classes`, `flag_cursors`/`flag_values` (existing sparse facts),
   `payloads`. No second `Vec<u32>`, no public `StructuralIndex` query
   surface after parse.
2. **Same-wave production consumer.** The retained `JsonRoot` view
   reads `tape.class_at(cursor)` instead of
   `tape.source()[offset_at(cursor)]` in `JsonNodeKind::at_cursor`.
   That is **the** production consumer for the new column, and it
   lands in the same wave as the column. The byte-rediscovery line in
   `value.rs:33-46` is deleted; the `at_cursor` match becomes a
   `class`-byte match.
3. **`ValueRef` cursor contract preserved.** `ValueRef::offset()`
   continues to read `tape.offset_at(cursor)`; the cursor scheme — one
   cursor per parser event — is byte-for-byte the same as today's. No
   view re-traversal. No `path!` semantics change. Track 2 untouched
   (it consumes the same `JsonRoot` API).

### §2.5 — How `consume_structural` is removed (or shrunk)

Today `consume_structural` (`generated.rs:292-306`) is the per-byte
scalar rediscovery. Under the alternate model, the parser consults the
structural index instead: `parse_object` reads
`index.positions[idx]`, asserts `index.classes[idx] == s1` (the `{`
structural class), advances `idx`, and emits `(positions[idx-1], e1)`
into the tape. Whitespace between structural bytes is *implicit* — it
is the gap between `positions[i]` and `positions[i+1]`. No
`skip_ascii_whitespace` walk; no per-byte structural fallback.

The exception is the scalar-anchor case (`Number`, `Literal`). There
the parser still reads bytes from `input[positions[i-1]+1 ..
positions[i]]` to locate the first non-whitespace byte. The cost is
*linear in scalar count*, not linear in input size as today. On
number-heavy corpora (`canada`, `mesh`, `numbers`) the per-scalar cost
is unchanged (today's scalar number scanner already pays it); on
string-dense corpora (`twitter`, `apache_builds`, `gsoc-2018`) the
*structural-fallback* path inside `consume_structural` is eliminated,
which is where the 47–67% string-scanner self-time
(`match_tiny_plain_string` + `match_string_at_quote`) reaches its
shelf.

## §3 — Cross-grammar admission

### §3.1 — CSS L4

The CSS L4 structural alphabet (per SC-6 §4.3:
`restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/
SC-6-lock1-amendment-generalisation.md:418-451`) is
`{ } ; : ( ) [ ] , " '`. The structural index walks rule-block braces,
declaration semicolons, selector commas, function parens, attribute
brackets, two string delimiters. The CSS parser-event class set is
(roughly): `RuleBlockOpen/Close`, `AtRuleStart`, `DeclarationTerminator`,
`SelectorListSeparator`, `FunctionCallOpen/Close`,
`AttributeSelectorOpen/Close`, `String`, `Ident`, `Number`,
`Dimension`. The *Ident*, *Number*, and *Dimension* classes are
scalar-anchors derived from the gaps — identical mechanism to JSON's
`Number` and `Literal`.

The CSS-specific moves the alternate model **does not** require:

- No per-grammar branch in `bbnf-simd`. The producer takes the
  structural-alphabet byte set as a 64-byte argument.
- No per-grammar branch in `runtime/src/tape/`. The class column is
  `Vec<u8>` of opaque ordinals.
- No new directive: the structural-alphabet derivation is the same
  recogniser pass that today derives `RecognizerFacts` and
  `backend_shape` (per SC-6 §4.1:381).

### §3.2 — Google Sheets

Sheets structural alphabet: `( ) , : { } "`. Sheets escapes a quote by
doubling it (`""`), a fact orthogonal to the event-model: the parser
walks `positions` and treats two adjacent s7-class positions inside an
open string as a literal-quote payload, not a string-close. The class
column on the tape names the parser-event class (`FormulaCallOpen`,
`RangeOperator`, `ArrayLiteralOpen`, etc.); the doubled-delimiter rule
is parser state, not generic substrate logic. Lock 14 holds.

### §3.3 — BBNF-self

BBNF-self alphabet: `= ; | ( ) [ ] { } < > , " /` (per SC-3 §4.2 last
paragraph). The self-hosting parser walks the structural index, emits
parser-event cursors for `RuleStart`, `Alternation`, `GroupOpen/Close`,
`OptionalOpen/Close`, `RepetitionOpen/Close`, `RuleEnd`, etc. The
fourteen-byte alphabet exercises `BYTE_CLASS_FROM_EQ_SET_64`'s
arbitrary 64-byte set capacity (Lock 16 line 70); no new primitive.

### §3.4 — Empty-alphabet grammars (whitespace-significant, regex)

Per SC-6 §4.5, an empty structural alphabet routes to `EagerTape`
through `derive_backend_shape` step 4. The alternate event-model is
inert in that case: no structural index is produced, no class column
is written; the tape's class column is empty and `at_cursor` resolves
via the `EagerTape`'s own first-set dispatch. No code branch on grammar
identity is needed because the alphabet's size is the discriminant.

## §4 — Falsifiability gate

The alternate model must reduce S-P1's load-bearing hot leaves on the
named loss rows without regressing the won rows. Concrete thresholds,
keyed to the per-corpus parse_only rows from
`skinny/RESULTS.md`:

### §4.1 — Must-improve (string-dense + structural-dense losses)

| Corpus | Track 1 today | Floor (sonic-strict / 1.10) | Required Track 1 | Hot-leaf delta required |
|---|---:|---:|---:|---|
| twitter | 13188 | 17685 | ≥ 17685 | `consume_structural` ≤ 5% self-time (today: structural rediscovery dominates); `JsonNodeKind::at_cursor` ≤ 1% self-time (today: per-cursor random byte read). |
| apache_builds | 11917 | 14124 | ≥ 14124 | same as twitter. |
| gsoc-2018 | 22184 | 41198 | ≥ 41198 | structural rediscovery deletion + string-scanner sustained; this is one of the four P1-named uncloseable-by-delimiter rows — if the alternate model cannot close it, the gate falsifies even with structural deletion (the string-scanner regression budget is named separately). |
| distinct_values | 8972 | 15731 | ≥ 15731 | as gsoc-2018; dense-key structural-heavy. |
| update_center | 9857 | 14369 | ≥ 14369 | structural rediscovery deletion. |

### §4.2 — Must-not-regress (number-heavy wins + neutral guards)

| Corpus | Track 1 today | Floor (max(today, sonic-strict)) | Notes |
|---|---:|---:|---|
| canada | 16190 | ≥ 15871 (-2.0% maintain) | scalar number scanner untouched; class column +1 byte/cursor RSS only. |
| mesh | 12435 | ≥ 12186 (-2.0%) | as canada. |
| marine_ik | 12073 | ≥ 11831 (-2.0%) | as canada. |
| numbers | 17956 | ≥ 17597 (-2.0%) | as canada. |
| citm_catalog | 29215 | ≥ 28631 (-2.0%) | structural-dense object skeleton; class column write is the only added work in the parse loop. |

### §4.3 — Out of scope (string-scanner + escape codec)

The alternate model is **not** claimed to close the four P1-named
uncloseable-by-delimiter rows: `unicode_mixed` (6803 Mbps vs sonic
14515), `unicode_escapes` (12047 vs 18132), `y_string_unicode` (5428
vs 11814), `gsoc-2018` (22184 vs 45318 — only partially closed by
structural deletion). Those rows' load-bearing leaves are the
string-scanner pair and the unicode-escape codec — different P2 agents
own those. The falsifiability gate above explicitly *separates* the
structural-rediscovery delta from the string-scanner delta. If the
alternate model lands and gsoc-2018 closes only to ~35000 Mbps (still
below 41198 floor), that is **not** falsification of the event-model;
it is the residual handed to P2-D's unicode-escape codec design and
P2-C's string-scanner work. The event-model gate is falsified only if
the structural-rediscovery hot leaf does **not** drop to ≤ 5%
self-time on the structural-dense corpora.

### §4.4 — Falsifying observations

The model is falsified if any of:

1. `consume_structural` self-time > 5% on twitter or apache_builds
   post-implementation. (Means the structural index isn't actually
   driving the parser walk — a sidecar drift, Lock 1 R1 in SC-6.)
2. Class column read in `at_cursor` not present, i.e. the per-cursor
   source-byte rediscovery survives in `value.rs`. (Means the same-
   wave consumer wasn't wired.)
3. `canada` / `mesh` / `numbers` / `marine_ik` regress > -2.0%.
   (Means the +1 byte/cursor write is paying more than the
   structural-deletion saves — implausible per the column-write cost
   model but must be measured.)
4. Track 2 / `path!` / direct-to-struct / SinkOnly rows show any
   delta beyond noise. (Means a cross-substrate leak; the alternate
   model touches only retained-view consumers.)
5. Any non-JSON grammar generic-crate file gains a JSON-named symbol
   in the diff. (Lock 14 falsification.)
6. The class column adds an entry the SIMD producer can't fill (e.g.
   `Number`/`Literal` ordinals leak into the structural alphabet).
   (Means the cursor/class split wasn't preserved — the W3 conflation
   re-emerged.)

## §5 — LOC + risk envelope (S-P3 owns final cost set)

Order-of-magnitude only.

| Slice | Hand LOC | Generated-regen LOC | Risk |
|---|---:|---:|---|
| `runtime/src/tape/{mod,assembler}.rs`: add `classes: Vec<u8>`, `class_at`, `push_offset_with_class`, drop `push_plain_offset` once codegen migrated. | +60 / -20 | n/a | LOW — additive column; existing API survives. |
| `runtime/src/grammars/json/parser.rs`: `emit_plain_offset` → `emit_event_offset(offset, class)`; structural-index field. | +15 (template) | +0 regen | LOW — one site. |
| `runtime/src/grammars/json/generated.rs`: each `emit_plain_offset` callsite passes a class ordinal; `consume_structural` deleted; `parse_object`/`parse_array`/etc. walk the structural index instead. | n/a | +80 / -50 regen | MEDIUM — regenerated; codegen template carries the structural-walk lowering. |
| `runtime/src/grammars/json/value.rs::JsonNodeKind::at_cursor`: byte-rediscovery → class-column read. | n/a | +5 / -15 regen | LOW — pure consumer swap. |
| `codegen/src/json_templates/{generated,parser,view,value}.rs`: emit the class column write, the structural-walk lowering, the `class_at` read. | +120 templates | n/a | MEDIUM — the structural-walk lowering is the new mechanism; checkasm-style parity tests required. |
| `bbnf-simd/src/lib.rs`: producer already emits positions; class is a parallel `Vec<u8>` co-written under the structural-alphabet's class table (already exists at `StructuralAlphabet::class_table`, `lib.rs:41`). | +20 | n/a | LOW — additive. |
| `runtime/src/grammars/json/scan.rs`: stop discarding the index; surface a move-consume API to the parser. | n/a | +10 / -5 regen | LOW. |
| `bbnf-bench/src/parity.rs`: class-column parity assert; structural-index move-consumed assert. | +30 | n/a | LOW — telemetry only. |

Hand source LOC: **~265** (templates + tape + tests). Regenerated
output: **~120 LOC net**, ~120 LOC added, ~70 LOC deleted (mainly
`consume_structural` and `at_cursor`'s byte match). Total well inside
the W3 default 450 LOC budget; the exceptional 650 LOC budget covers
any expansion in the same wave.

**Risk: medium-low.** The mechanism is a *contracting* one (it
*deletes* `consume_structural` and shrinks `at_cursor`) and the cursor
scheme is preserved row-for-row. The novel surface is the codegen
template emitting the structural-walk lowering, which is bounded.

## §6 — REDRESS pre-block citations: routes the alternate design must
NOT re-open

The alternate event-model is admissible only if it is **distinct** from
every previously-rejected route. The cited entries:

- **REDRESS 50** (`skinny/REDRESS.md`, SK-V5 cohort): rejected
  parser-written aux side tables — dense/sparse columns appended by
  the parser to memoise structural facts. The class column is *not*
  parser-written as a side-table: it is co-emitted at the *same*
  `emit_plain_offset` call site as today's offset, replacing
  `push_plain_offset` with `push_offset_with_class`. There is no
  separate "aux" producer pass and no second cursor. Falsifier: if a
  pass other than the parser writes `classes`, this fails REDRESS 50.

- **REDRESS 51** (`skinny/REDRESS.md`, byte-class whitespace/event
  cursor): rejected a parser-local byte-class cursor synthesised
  alongside the parser walk. The alternate model has no such cursor;
  the parser walks the SIMD-produced structural index, then writes
  parser-event class to the tape. There is no parser-owned cursor
  beside the tape's cursor. Falsifier: if `ParserState` gains a
  cursor field other than `state.cursor: usize` (the byte cursor) +
  the structural-index walker idx, this fails REDRESS 51.

- **REDRESS 53** (`skinny/REDRESS.md`, parser-local structural-mask
  cursor / second scanner): rejected a parser-local mask cursor that
  consumed the emit mask and carried quote/backslash state. The
  alternate model uses *the* structural index (the existing
  `scan_structurals` product) and consumes it by move; no second mask
  is built inside the parser. Falsifier: if a second
  `compact_mask`-class call site appears inside the parser, this fails
  REDRESS 53.

- **REDRESS 60–72** (`skinny/REDRESS.md`, SK-V6 cohort): rejected
  retained-parse sidecar producers, retained side-table columns, and
  digest cap-16/source-hook routes. Common form: a second producer
  that runs alongside parse, writing into a column that survives. The
  class column survives, but it is not a *sidecar* producer — it is
  the tape's own column, written by the *primary* (and only) producer
  (the parser). No new producer pass. Falsifier: if any wave-level
  pass other than the parser writes into `tape.classes`, this fails
  REDRESS 60–72.

- **REDRESS 82** (single-quartet Unicode escape classifier): the
  alternate model does not address Unicode escape codecs. P2-D / P2's
  unicode-escape design owns that row. The event-model is silent on
  the four-uncloseable rows' string content.

- **REDRESS 83** (StringBlock16 tiny probe): orthogonal — the event-
  model does not change the string-scanner pair. Same-wave: the
  string-scanner sees a class-tagged opening-quote cursor, not a
  byte rediscovery; that is a removal of work, not a new route into
  string-boundary closure.

- **REDRESS 84** (object-pair value-byte compaction): orthogonal —
  the alternate model adds no compaction; class column is one byte
  per cursor without packing.

- **REDRESS 88** (PMULL prefix-XOR as hot body) and **REDRESS 89**
  (CSSC CTZ next-bit bulk consumer): orthogonal — the SIMD producer
  is unchanged at the Layer-1 vocabulary level.

- **REDRESS 92** (THIS rejection): the alternate model addresses
  the *event-model fit gate* REDRESS 92 cited. It satisfies the
  three load-bearing constraints (one substrate, same-wave consumer,
  preserved `ValueRef` cursor contract) by splitting the conflated
  cursor/class question rather than merging them. It does not re-open
  REDRESS 92; it implements the routed precursor.

The blanket pre-blocks REDRESS 92 enumerated (`skinny/REDRESS.md:
2673-2676`) must be checked: the alternate model introduces
**no new `BackendShape` variant** (Lock 1 R2 in SC-6: the union is
representation of `OffsetTape`, not a sixth shape), **no new BIR
variant**, **no new directive**, **no public substrate API**, **no
parser-owned structural cursor/facts**, **no `tape_vs_tape` as
production consumer** (the production consumer is `JsonRoot` view's
`at_cursor`), **no `UnionTape` public type**, and **no Tier B
string-boundary/quote-backslash/parity work** (those route to the
separate string-scanner and unicode-escape designs).

## §7 — Sources

Internal:
- `restart/skinny/tranches/sk-v9/research/p1/hardening/
  HARDENING-S-P1-CONVERGED.md` §Load-bearing diagnoses (lines 76–94).
- `restart/skinny/tranches/sk-v8/research/skv8-W3-tape-structural-research.md`
  §Finding (lines 7–49); §Falsifiability Against W3 Entry (52–88);
  §Historical Route Checks (91–98).
- `restart/skinny/tranches/sk-v8/research/skv8-W3-plan.md`
  §Fit Gate (32–66); §Same-Wave Consumer (70–76); §Pre-Blocked Routes
  (79–93); §Redress Plan (96–105).
- `skinny/REDRESS.md` §SK-V8 Wave 3 Tape Plus Structural-Projection
  Redress, Item 92 (lines 2661–2690).
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/
  SC-3-union-substrate-design.md` §1 Current Substrate (lines 24–95);
  §2 The Union Design (98–296); §3 BackendShape Projection (299–321);
  §4 Grammar-Neutral Generalisation (323–401).
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/
  SC-6-lock1-amendment-generalisation.md` §3 SC-6-L1-R1 amendment
  (lines 247–335); §4 StructuralAlphabet generalisation (337–536); §6
  R1–R6 risk register (640–704).
- `restart/locks/LOCKS.md` Lock 1 (line 34); Lock 14 (line 60); Lock
  16 admissibility allowlist + Layer-0/Layer-1 (lines 69–94).
- `restart/prompts/skinny/PASS-2-RESEARCH.md` §2 scope (lines 40–60);
  §3 CHALLENGE pre-block surface (CH3, lines 108–118); §8 bbnf-lang
  axes 1, 5, 6 (lines 212–241).
- `skinny/RESULTS.md` parse_only rows (current Track 1/2 Mbps and
  sonic-strict/simdjson/yyjson per corpus).

Code:
- `skinny/crates/runtime/src/grammars/json/generated.rs:1-17`
  (`STRUCTURAL_ALPHABET_JSON`, `attach_structural_index` no-op);
  `:280-306` (`consume_structural` per-byte rediscovery);
  `:310-339` (`consume_container_next` `,`-walk without emit).
- `skinny/crates/runtime/src/grammars/json/parser.rs:1-67`
  (`ParserState`, `emit_plain_offset`, `parse` entrypoint).
- `skinny/crates/runtime/src/grammars/json/value.rs:29-47`
  (`JsonNodeKind::at_cursor` byte-rediscovery — the second redundancy
  hidden from S-P1's static taxonomy because it amortises across view
  ops).
- `skinny/crates/runtime/src/tape/mod.rs:87-169` (`Tape` shape,
  `from_offsets`, `offset_at`, `flags_at`).
- `skinny/crates/runtime/src/tape/assembler.rs:42-124` (`TapeBuilder`,
  `push_plain_offset`, `push_offset`).
- `skinny/crates/bbnf-simd/src/lib.rs:20-127` (`StructuralAlphabet`,
  `StructuralIndex`, `scan_dispatch`, `compact_mask`, `class_table`
  at `:41` — already produces the structural-class table; the
  alternate model retains the index instead of discarding it).
