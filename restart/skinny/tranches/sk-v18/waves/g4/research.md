# SK-V18 G4 — RESEARCH + PLAN: shared value-API `Cursor` trait + phantom `<G>` resolution

Wave G4 (SPEC §7, R-D candidate A). Research only — no code, no cargo, no regen. Every claim is
grounded on disk at HEAD `b5ab2e17c` (G3 closed: "un-fork the emitter — DELETE RuntimeEmitterKind").
Working tree: `skinny/`. Host: aarch64 / Apple M5 Max ONLY.

Entry-gate status: **G1 ∧ G2 ∧ G3 closed, GREEN.** G3 landed at HEAD `b5ab2e17c`; G1+G2 are its
transitive predecessors (the un-fork could not close without both DERIVED value-API surfaces existing
through one path). The 3-conjunct entry predicate (SPEC §7 G4.1) holds.

G4 mandate (SPEC §7): (1) **DELETE the phantom `<G: EventGrammar = AnyGrammar>`** on `ValueRef`
(`tape/mod.rs:175`, the `_grammar: PhantomData<fn() -> G>` the S-P0 audit flagged — zero non-test
production animator → DELETE); (2) introduce a thin shared **`Cursor`** micro-trait that BOTH JSON
(rich tree nav) and CSS (flat tape sweep) implement OVER the single existing `Tape`, staying LAZY,
with ≥2 real impls that CANNOT LCD-collapse to the lesser.

---

## 1. CURRENT-STATE MAP

### 1.1 The phantom `<G>` definition + every use site (production vs `#[cfg(test)]`)

**The definition** — `crates/runtime/src/tape/mod.rs:175-181`:

```rust
pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind, G: EventGrammar = AnyGrammar> {
    tape: &'doc Tape<'input>,
    cursor: u32,
    _kind: PhantomData<fn() -> K>,      // <-- the REAL Kind axis (PRESERVE)
    _grammar: PhantomData<fn() -> G>,   // <-- the PHANTOM G axis (DELETE)
    _input: PhantomData<&'input [u8]>,
}
```

The `EventGrammar` trait + `AnyGrammar` default live in `tape/event_grammar.rs` (31 LOC, whole file):
`trait EventGrammar { const STRUCTURAL_CLASS_COUNT: u8; type FactId; fn admits_fact; fn admits_class }`
+ `enum AnyGrammar {}` (the inert default, `STRUCTURAL_CLASS_COUNT == 0`, `admits_class → false`) +
`enum NoFactId {}`. Re-exported at `tape/mod.rs:11` (`pub use event_grammar::{AnyGrammar, EventGrammar}`).

**Every use site of the `G` (EventGrammar) axis** (exhaustive grep over `runtime/src/`):

| Site | File:line | Production or test? | Disposition |
|---|---|---|---|
| `ValueRef<…, K, G: EventGrammar = AnyGrammar>` struct | `tape/mod.rs:175` | substrate def | DELETE `G` param + `_grammar` field |
| `impl … Copy/Clone/inherent for ValueRef<…,K,G>` | `tape/mod.rs:183,185,191` | substrate impls | DROP `G` from all three impl headers |
| `erase(self) -> ValueRef<…,AnyKind,G>` | `tape/mod.rs:202` | substrate fn | `-> ValueRef<…,AnyKind>` (loses `G`) |
| `pub use …{AnyGrammar, EventGrammar}` | `tape/mod.rs:11` | re-export | DELETE |
| `pub mod event_grammar` + `mod event_grammar_tests` | `tape/mod.rs:2-4` | module decls | DELETE both |
| whole `tape/event_grammar.rs` | file | substrate def (the trait+enum) | DELETE file |
| `JsonEventGrammar` witness | `grammars/json/event_grammar_witness.rs` | **`#[cfg(any(test, feature="proof"))]`** (lib.rs:24-25) | DELETE file |
| `SheetsEventGrammar` witness | `grammars/sheets_witness/event_grammar_witness.rs` + `mod.rs` | **`#[cfg(any(test, feature="proof"))]`** (lib.rs:28-29) | DELETE files |
| `_proof_compiles::<…>` + 4 tests | `tape/event_grammar_tests.rs` (135 LOC) | `#[cfg(test)] mod` (gated `tape/mod.rs:3`) | DELETE file |
| `json_event_grammar_witness` / `sheets_witness` mod decls | `lib.rs:24-29` | `#[cfg(any(test, feature="proof"))]` | DELETE both decls |

**The S-P0 finding is CONFIRMED on disk: ZERO non-test production animator of the `G` axis.** Every
`ValueRef` instantiation on the PRODUCTION path uses the **3-arg form** `ValueRef<'doc,'input, K>` (or
the bare `ValueRef<'doc,'input>`), letting `G` default to `AnyGrammar` and never naming it:

- JSON: `ValueRef::new(...)` (3-arg `K`-typed) at `json/view.rs:27,72,86,143,...`,
  `json/value.rs:147-169`; `DocumentView::Root = ValueRef<'input,'input, RootKind>` (`view.rs:69`).
- CSS: `CssNode { node: ValueRef<'doc,'input> }` (bare, `generated.rs:74`), `ValueRef::new(&tape, cursor)`
  (`generated.rs:288`).

The ONLY sites that name `G` / `AnyGrammar` / `JsonEventGrammar` / `SheetsEventGrammar` are:
`event_grammar_tests.rs` (`#[cfg(test)]`), the two witness files (`#[cfg(any(test, feature="proof"))]`,
lib.rs:24-29 — **excluded from production builds**), and the substrate def itself. The
`_proof_compiles::<JsonEventGrammar/SheetsEventGrammar>` const-fn proofs at `event_grammar_tests.rs:18-20`
are `#[cfg(feature = "proof")]`; the bare `_proof_compiles::<AnyGrammar>` at `:21` is inside the
`#[cfg(test)] mod event_grammar_tests`. **The non-test instantiation census is EMPTY.**

### 1.2 The JSON rich-nav surface (what makes it a TREE)

`grammars/json/` — a recursive nested tree, `K`-typed:

- **`ValueRef<K>` carries a REAL `K` axis**: `RootKind`/`ObjectKind`/`ArrayKind`/`StringKind`/
  `NumberKind`/`BoolKind`/`NullKind` (`view.rs:453-460`), used as field types
  (`JsonObject.inner: ValueRef<…,ObjectKind>` `:86`, `JsonString.inner: ValueRef<…,StringKind>` `:197`, …).
- **Keyed + iterated navigation**: `JsonObject::get(key)` (`view.rs:106`), `pairs()` (`:90`),
  `JsonArray::values()` (`:147`), `len`/`is_empty`.
- **Typed 6-variant enum** `JsonValue::{Object,Array,String,Number,Bool,Null}` (`value.rs:69`),
  byte-dispatched by `value_from_ref` (`value.rs:143`) / `JsonNodeKind::at_cursor` (`value.rs:29`).
- **Recursive visitor** `walk_value` (`visitor.rs:16-39`) recurses into containers via `object.pairs()` /
  `array.values()`; the `JsonVisitor` trait (8 `visit_*` hooks).
- **Container nesting + depth-walk**: `next_sibling_cursor` (`view.rs:355`) does the nested depth scan;
  `JsonObjectPairs`/`JsonArrayValues` iterators (`view.rs:259-331`).
- **`DocumentView for JsonDocument`** (`view.rs:68`, `type Root = ValueRef<'input,'input,RootKind>`).

### 1.3 The CSS flat-tape projection surface (what makes it FLAT)

`grammars/css_l4_declaration_values/generated.rs` — a LINEAR node sweep, `K`-less:

- **`CssNode` wraps a BARE `ValueRef`** (no `K`, `:73-74`); `kind()` decodes lazily via
  `CssNodeKind::at_cursor` (`:53`, the analogue of `JsonNodeKind::at_cursor`).
- **Linear sweep, NOT recursive**: `CssDocument::nodes()` = `(0..offsets.len()).map(CssNode)` (`:286-290`).
  There is NO `get(key)`, NO container nesting, NO recursion.
- **Typed 2-variant enum** `CssTypedNode::{Rule, Declaration}` (`:139`), projected by `CssNode::value()`
  (`:119`, the analogue of `value_from_ref`).
- **The 9-field rich summary** `CssRichSummary` (`:356-366`: rules, at_rules, qualified_rules,
  declarations, selectors, dimensions, numbers, colors, functions), materialized lazily by
  `rich_summary()` (`:316-342`) — re-derives every field from `(source, offset)` spans, **writing nothing
  to the arena** (`:316` doc). The hot leaves: `CssRule::selector_count` (`:171`),
  `CssDeclaration::value_span` (`:209`, the `scan_value_end` balanced scan), `CssTypedValue::classify`
  (`:246`).
- **`CssDocument` does NOT implement `DocumentView`** (today only `JsonDocument` does).

### 1.4 The latent `DocumentView` trait — the seam to extend

`tape/mod.rs:227-232`:

```rust
pub trait DocumentView<'a> {
    type Root: 'a;
    fn root_value(&'a self) -> Self::Root;
    fn tape_id(&self) -> TapeId;
    fn source(&'a self) -> &'a [u8];
}
```

**Status: latent with ONE impl** (`JsonDocument`, `view.rs:68`) and **zero generic consumers** —
`root_value`/`tape_id`/`source` are never called through the trait bound; `DocumentView` is dead
generic surface today. `type Root` is the document-level associated-type seam. The codegen
template `json_templates/view.rs:68` emits the JSON impl verbatim.

**VERDICT — TWO seams, two roles (NOT one):**
- **`DocumentView` (extend) = the DOCUMENT-level seam.** Implement it for `CssDocument` (and Sheets'
  doc) to give it its earned second/third impl; `type Root = CssNode` (the CSS root cursor). This is a
  document-handle abstraction (root + tape-id + source bytes) — naturally grammar-agnostic, no
  navigation shape, zero flatten risk.
- **`Cursor` (NEW, `tape/cursor.rs`) = the NODE-level seam.** A fresh micro-trait over the now-clean
  `ValueRef<K>` capturing ONLY the cursor/laziness primitive (`tape`, `cursor`, `offset`, `kind()`). It
  does NOT subsume `DocumentView` — they sit at different altitudes (node vs document). Both are part of
  the G4 deliverable; neither carries navigation.

---

## 2. THE PHANTOM DELETE — `ValueRef<K, G>` → `ValueRef<K>`

### 2.1 The mechanical edit set (the full signature/impl census)

Drop the `G` axis everywhere, **preserving the `K` axis untouched** (`_kind: PhantomData<fn() -> K>`
survives — preserve-rich-ast):

1. **`tape/mod.rs:175`** — `pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind>` (drop
   `, G: EventGrammar = AnyGrammar`); delete the `_grammar: PhantomData<fn() -> G>` field (`:179`).
2. **`tape/mod.rs:183,185,191`** — three `impl` headers: drop `, G: EventGrammar` from the generics and
   `, G>` from the `ValueRef<…>` type.
3. **`tape/mod.rs:197`** — delete `_grammar: PhantomData,` from `new()`.
4. **`tape/mod.rs:202`** — `erase(self) -> ValueRef<'doc,'input, AnyKind>` (drop `, G`).
5. **`tape/mod.rs:11`** — delete `pub use event_grammar::{AnyGrammar, EventGrammar};`.
6. **`tape/mod.rs:2-4`** — delete `pub mod event_grammar;` + `#[cfg(test)] mod event_grammar_tests;`.
7. **DELETE files**: `tape/event_grammar.rs`, `tape/event_grammar_tests.rs`,
   `grammars/json/event_grammar_witness.rs`, `grammars/sheets_witness/event_grammar_witness.rs`,
   `grammars/sheets_witness/mod.rs`.
8. **`lib.rs:23-29`** — delete the `json_event_grammar_witness` + `sheets_witness` cfg-gated mod decls.

### 2.2 Is the delete CLEAN? — YES, fully clean. Nothing production depends on `G`.

The `G` axis is layout-invariant (`ValueRef` size == `(&Tape, u32)`, asserted at
`event_grammar_tests.rs:52-61` — and that test itself is deleted). No production code reads
`STRUCTURAL_CLASS_COUNT`, `admits_fact`, or `admits_class`. Deleting `G` does not change any production
`ValueRef` type (they all already use the 3-arg form, so the post-delete `ValueRef<'doc,'input,K>` is
the SAME type they spell today). **No production signature changes; the generated JSON `value.rs`/
`view.rs` and CSS `generated.rs` are byte-IDENTICAL before and after the delete** (they never named `G`).

### 2.3 The ONE honest caveat — the `EventGrammar` STRING in codegen (NOT the phantom)

`crates/codegen/src/lower/tape_plan.rs:25` emits the lowering capability descriptor string
`"ParserState+TapeBuilder+EventGrammar"` for the `Event` tape flavor (asserted at
`codegen/src/lib.rs:1347`). This is a **profile-IR tape-flavor capability LABEL, NOT the `ValueRef<G>`
phantom** — it lives in `codegen/`, not `runtime/src/`, so the G4 phantom-resolved grep (scoped to
`runtime/src/`, SPEC §7 G4.2-conjunct-1) does NOT see it and is not false-greened by it. The
`lock14_baseline.rs:2387` comment already documents this descriptor as exempt from the
`runtime_generator.rs`-scoped `FORBIDDEN_GENERIC_TOKENS` (it is a lowering descriptor, not an emitted
runtime literal). **Disposition: out of G4 scope** — G4 deletes the `EventGrammar` *trait/witness/type*;
this string is a tape-flavor label that survives. Flagged HONESTLY: if a future wave wants the
`EventGrammar` token fully gone campaign-wide, the `Event` flavor label is a separate rename (a P4/G3
descriptor concern, not the G4 phantom-delete). It does NOT block G4's `phantom_generic_resolved ==
deleted` (the grep is `runtime/src/`-scoped and test-excluded).

### 2.4 P4 interaction

P4's `FORBIDDEN_GENERIC_TOKENS ⊇ {EventGrammar, *EventGrammar}` (SPEC close-cond 8) is the enforcement
that no `*EventGrammar` witness literal re-emits into generated runtime after the delete. Post-G4 the
trait is gone, so no literal CAN be emitted — the P4 gate is satisfied trivially and stays the standing
guard against re-introduction.

---

## 3. THE SHARED `Cursor` TRAIT DESIGN (R-D candidate A — the anti-LCD design)

### 3.1 The minimal trait — cursor/laziness ONLY, NEVER navigation

`crates/runtime/src/tape/cursor.rs` (NEW; directory-module isomorphic — under `tape/`, the substrate
home; `[directory-module-structure]`):

```rust
/// The lazy cursor primitive both grammars' node views share: a position into
/// the single Tape plus the lazily-decoded structural kind. Shares ONLY the
/// laziness/cursor contract — NEVER navigation (no get/pairs/children/nodes).
pub trait Cursor<'doc, 'input: 'doc> {
    /// The grammar's own kind enum (JSON: JsonNodeKind; CSS: CssNodeKind; Sheets: SheetsNodeKind).
    type Kind: Copy;

    fn tape(&self) -> &'doc Tape<'input>;
    fn cursor(&self) -> u32;
    fn offset(&self) -> usize;        // re-derived from the tape; nothing eager
    fn kind(&self) -> Self::Kind;     // the lazy at_cursor decode
}
```

What it shares: the `(tape, cursor, offset, kind)` plumbing that JSON's `value_from_ref` /
`JsonNodeKind::at_cursor` and CSS's `CssNode::value` / `CssNodeKind::at_cursor` BOTH duplicate today —
the de-dup of the cursor-over-offset primitive, nothing more. The associated `type Kind` keeps each
grammar's OWN kind enum native (no shared kind enum → no shared structural vocabulary → no flatten).

What it does NOT share (stays grammar-OWNED, native, full-fidelity):
- JSON: `get(key)`, `pairs()`, `values()`, `JsonValue` (6 variants), `walk_value` recursive visitor,
  `next_sibling_cursor` depth-walk, `JsonObject/Array/String/Number/Bool/Null`.
- CSS: `nodes()` linear sweep, `value() -> CssTypedNode` (2 variants), `rich_summary()` (9 fields),
  `selector_count`, `value_span`, `CssTypedValue::classify`.

### 3.2 The ≥2 real impls — and WHY they cannot LCD-collapse

**Impl 1 — JSON `ValueRef<K>`** (in `grammars/json/`, the K-typed cursors backing
`JsonObject`/`JsonArray`/`JsonString`/…): `type Kind = JsonNodeKind`; `kind()` = `JsonNodeKind::at_cursor`.
The rich tree (`get`/`pairs`/`values`/recursive visitor) is built ATOP the cursor primitive but lives
ENTIRELY outside the trait.

**Impl 2 — CSS `CssNode`** (in `grammars/css_l4_declaration_values/`): `type Kind = CssNodeKind`;
`kind()` = `CssNodeKind::at_cursor`. The flat sweep (`nodes()`/`value()`/`rich_summary`) lives ENTIRELY
outside the trait.

**Impl 3 (PROVE, downstream) — Sheets node** with `type Kind = SheetsNodeKind`; the precedence-tower
navigation stays native. (G4 supplies the trait; PROVE instantiates the third impl — SPEC §7 downstream:
"the Sheets value type instantiates the G4 trait".)

**The non-collapsibility is STRUCTURAL, not asserted (the conjunct-3 falsifier):** JSON's
navigation surface (`get`/`pairs`/recursive containers) has NO CSS analogue; CSS's flat-sweep surface
(`nodes()`/9-field summary) has NO JSON analogue. Because those surfaces live OUTSIDE the trait,
**substituting JSON's full node type for CSS's `CssNode` (or vice-versa) FAILS to compile** — the
`CssDocument::rich_summary` call sites need `CssNode::value()→CssTypedNode`, which a JSON `ValueRef<K>`
does not provide, and JSON's `JsonObject::get` needs `pairs()`, which `CssNode` does not provide. The
two navigation surfaces are structurally distinct; neither is the other's degenerate.
`shared_trait_non_collapsible == true` is PROVEN by this substitution falsifier (SPEC §7 G4.2-conjunct-3),
NOT by the bare `shared_trait_impl_count >= 2`.

### 3.3 Why the trait is too NARROW to force a common value shape (anti-LCD)

The LCD-flatten trap (R-D §0): a trait whose navigation verb is `nodes() -> impl Iterator<Node>` +
`Node::kind()` lets CSS implement trivially but ERASES JSON's `get(key)` + recursive container model.
**The `Cursor` trait carries NO navigation verb at all** — only `(tape, cursor, offset, kind)`. It is
structurally incapable of seeing `get`/`pairs`/`children`/`nodes`, so it CANNOT erase them.
`json_rich_navigation_preserved == true` follows because the JSON `value.rs`/`view.rs`/`visitor.rs`
navigation surface is UNTOUCHED by adding the impl (a `Cursor` impl is additive — it does not rewrite
`get`/`pairs`).

### 3.4 `DocumentView` extension (the document-level seam, part of G4b)

Extend `DocumentView` to `CssDocument`: `impl<'input> DocumentView<'input> for CssDocument<'input>`
with `type Root = CssNode<'input,'input>`, `root_value` = `CssNode { node: ValueRef::new(&tape, 0) }`,
`tape_id`/`source` from the tape. This gives the latent `DocumentView` its earned 2nd impl. (Sheets =
3rd, at PROVE.) `DocumentView` carries NO navigation either (root + tape-id + source bytes), so it is
also flatten-free.

### 3.5 Where the trait + impls are AUTHORED vs EMITTED (clean-regen discipline)

- **`Cursor` trait def** = hand-written substrate, `tape/cursor.rs`, wired `pub mod cursor;` in
  `tape/mod.rs` + re-exported. Substrate, not generated.
- **The JSON impl** = emitted by the codegen JSON view template (`codegen/src/json_templates/view.rs`,
  byte-1:1 with `grammars/json/view.rs`). Add the `impl Cursor for ValueRef<…>` (or for the JSON node
  wrappers) to the template so the generated file carries it after regen.
- **The CSS impl + `DocumentView for CssDocument`** = emitted by the CSS projection template
  (`codegen/src/css_scan_direct/projection.rs`, 539 LOC, → `css_l4_*/generated.rs`).
- **Both regenerate 1:1** (`cargo xtask regen --check` exit 0). No hand-patched `// @generated` file
  (`[clean-regen-discipline]`). The JSON rich-nav surface stays byte-equal under regen.

---

## 4. FEASIBILITY VERDICT

**G4 is ACHIEVABLE AS-SPECIFIED. No amendment required.** Both sub-tasks are clean and falsifiable.

### 4.1 Phantom-delete cleanliness — CLEAN (high confidence)

Disk evidence is unambiguous (§1.1, §2.2): the `G` axis has ZERO non-test production animator; every
production `ValueRef` already uses the 3-arg form; `G` is layout-invariant. DELETE is the
abrogate-before-patch default (`[abrogate-before-patch]`); "instantiate" would manufacture the
grammar-named surface addenda 2/3 forbid, for a parameter production code never animates. The delete
changes no production type and leaves generated output byte-identical. `phantom_generic_resolved ==
deleted` (test-excluded, `runtime/src/`-scoped grep == 0).

### 4.2 The shared trait — a GENUINELY NARROW cursor contract, not an LCD-flatten, not decorative

The `Cursor` trait is the unique altitude that (i) gives ≥2 structurally-non-collapsible impls,
(ii) shares the REAL duplication (cursor-over-offset plumbing both grammars hand-roll), (iii) carries
NO navigation verb so it CANNOT flatten JSON's tree, (iv) adds no second substrate (one `Tape`),
(v) preserves laziness (cursor over offsets; spans re-derived; arena untouched).

### 4.3 The §5-risk-4 critique ("trait too thin under-delivers") — REBUTTED by falsifiable telemetry

The critique: a `Cursor` micro-trait that shares only the cursor primitive is "too small to count as a
generalization." The binding rebuttal (SPEC §7 G4.3) is STRUCTURAL and ties the verdict to two
falsifiable columns, NOT a subjective width judgement:

> ANY trait wide enough to satisfy the "too thin" critic is wide enough to LCD-flatten JSON (the
> addendum-4 / preserve-rich-ast REJECT). The honest generalization IS the cursor/laziness contract.

A wider trait must add a navigation verb (`nodes()`/`children()`/`as_object()`); the moment it does,
either CSS gets a degenerate/dead impl (an inverse phantom — Candidate B's failure) OR the verb is
weakened to CSS's flat shape, erasing JSON's keyed nesting (`json_rich_navigation_preserved == false`,
REJECT). The critic cannot reopen the candidate choice without producing a wider trait that passes
conjunct 2 (json-nav-preserved) — which §5-risk-4 shows is impossible. So thin-but-honest is the correct
altitude; the ≥2-non-collapsible-impl gate (conjunct 3) proves it is a REAL generalization, not
decorative. The bare count is necessary-NOT-sufficient; the substitution falsifier
(`shared_trait_non_collapsible == true`) is the binding proof.

### 4.4 §6 / amendment honesty (surfaced as required, like G1-G3)

- **The `tape_plan.rs:25` `EventGrammar` string** (§2.3) is a HONEST residual: a lowering tape-flavor
  capability LABEL in codegen, NOT the `ValueRef<G>` phantom. It is out of G4's `runtime/src/`-scoped
  phantom grep and already documented exempt in `lock14_baseline.rs:2387`. **Non-blocking for G4.** A
  campaign-wide `EventGrammar`-token purge (if desired) is a separate descriptor rename, not G4's job.
  Recorded, not silent.
- **No other §6 finding.** The candidate (A) is the SPEC-mandated one; B (tree-shaped `Value` stack) and
  C (`DocumentView`+stream-only) are pre-blocked REJECTs (B = LCD/dead surface, C = under-delivers,
  shares the stream not the value navigation). No grammar-derived parser CANNOT-preserve-without-shaping
  finding arises — G4 touches the value-API substrate, not the hot scan.

**Verdict: G4 achievable as-specified; the phantom-delete is clean; the `Cursor` trait is a
genuinely-narrow, non-LCD, non-decorative cursor contract proven by the two falsifiable telemetry
columns + the non-collapsibility substitution falsifier.**

---

## 5. IMPLEMENTATION STEP PLAN (the concrete edit sequence + verification)

LOC budget: ≤450 hand source/test/gate LOC (well within — the net is a DELETION + a ~15-line trait +
two small impls + two template edits). Cap: ≤90 min wave wall; 30 min/redress. The `<G>` DELETE
(G4a) is INDEPENDENT of the trait (G4b) and lands FIRST to expose the clean `ValueRef<K>`.

### G4a — the phantom `<G>` DELETE (lands first)

1. `tape/mod.rs`: drop the `G` param + `_grammar` field from `ValueRef` (`:175,179`); drop `G` from the
   three impl headers (`:183,185,191`); drop `_grammar: PhantomData` from `new()` (`:197`); fix
   `erase` return (`:202`). Delete the `pub use …{AnyGrammar, EventGrammar}` (`:11`) + the
   `event_grammar` / `event_grammar_tests` mod decls (`:2-4`).
2. DELETE `tape/event_grammar.rs`, `tape/event_grammar_tests.rs`,
   `grammars/json/event_grammar_witness.rs`, `grammars/sheets_witness/event_grammar_witness.rs`,
   `grammars/sheets_witness/mod.rs`.
3. `lib.rs`: delete the `json_event_grammar_witness` + `sheets_witness` cfg-gated mod decls (`:23-29`).
4. Mirror any of the above in the codegen templates IF a template emits a witness/`G` reference
   (none do — confirmed: templates emit only the 3-arg `ValueRef`).

### G4b — the `Cursor` micro-trait + `DocumentView` extension

5. NEW `tape/cursor.rs`: the `Cursor<'doc,'input>` trait (§3.1); `pub mod cursor;` + re-export in
   `tape/mod.rs`.
6. CSS template `codegen/src/css_scan_direct/projection.rs`: emit `impl Cursor for CssNode` +
   `impl DocumentView for CssDocument` (`type Root = CssNode`).
7. JSON template `codegen/src/json_templates/view.rs`: emit `impl Cursor` for the JSON node cursor
   (additive — does not touch `get`/`pairs`/`JsonValue`).
8. Regen JSON + CSS `generated.rs` (the impls land via the templates; JSON rich-nav surface byte-equal).

### Verification (the SPEC §7 G4.2 exit-gate telemetry + the standing guards)

- **conjunct 1** `phantom_generic_resolved == deleted`: `grep -E "EventGrammar|AnyGrammar|G: EventGrammar|_grammar: PhantomData"`
  over `runtime/src/`, test-excluded, == 0. (`_kind: PhantomData<fn() -> K>` survives — preserve-rich-ast.)
- **conjunct 2** `json_rich_navigation_preserved == true`: byte-equal diff of JSON `value.rs`/`view.rs`/
  `visitor.rs` navigation surface vs pre-G4 form (diffs EMPTY for the nav methods; the only addition is
  the `impl Cursor`) ∧ JSON 51/51 strict-vs-sonic-rs held ∧ value-plane population parity intact.
- **conjunct 3** `shared_trait_impl_count >= 2` (JSON + CSS) ∧ `shared_trait_non_collapsible == true`:
  the substitution falsifier — swap JSON's node impl for CSS's `CssNode` ⇒ crate FAILS to compile
  (rich-tree `get`/`pairs` has no CSS analogue; flat-sweep `value()→CssTypedNode` has no JSON analogue).
  A degenerate-equal CSS impl that COMPILES under substitution ⇒ false ⇒ REJECT.
- **conjunct 4 (Lock 1, cross-cutting)** no second substrate: grep for a new `*Tape`/`*Cursor` SUBSTRATE
  type == 0 (the `Cursor` TRAIT is a view, not a substrate); laziness intact (no per-leaf `Box::new`,
  no eager value tree).
- **Standing guards**: CSS parity (the 9-field oracle) holds; CSS >SOTA ratios unchanged (G4 is a
  value-API change, not a hot-path change); `cargo xtask regen --check` exit 0; `cargo build` +
  `cargo test --no-run` clean.

### Revert slice (if rejected)

Revert `tape/mod.rs` + `tape/event_grammar.rs`(+tests) deletion + `tape/cursor.rs` (new) + the two
template/impl edits + the regenerated output as ONE slice; restore RESULTS; add a REDRESS naming WHICH
conjunct failed (phantom-resolved / json-nav-preserved / impl-count / non-collapsible) and whether the
trait LCD-flattened JSON.

### Downstream

**G4 REJECTION BLOCKS PROVE** (the Sheets value type instantiates the G4 `Cursor`/`DocumentView` trait
— the phantom-`<G>` resolution made concrete by a third impl; G4 is a DIRECT conjunct of PROVE's entry
gate). G4 does NOT block G6 (G5/G6 hangs off G3, parallel to G4 — NEON wiring is independent of the
value-API trait). Outcome `G`/`A`/`C` on close, `R` on miss.
