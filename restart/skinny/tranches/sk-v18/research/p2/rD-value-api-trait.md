# SK-V18 S-P2 / R-D — Shared Lazy Value-API Trait + Phantom `<G>` (research digest)

Class R-D. Addendum 4 (phantom-generic, `a1-six-addenda-lens-registry.md` §L4). Wave G4.
This pass is RESEARCH only — no code, no cargo. Every claim is grounded in the live tree
(paths/lines cited at HEAD-of-worktree). Host: aarch64 / Apple M5 Max ONLY.

## 0. The grounded problem (two REAL impls over ONE tape, structurally divergent)

The single substrate is real and CLEAN (Lock 1, `SYNTHESIS-AUDIT-OVERFIT.md` §2 KEEP): one
`Tape<'input>` = `{ source, offsets: Vec<u32>, flag_cursors/flag_values sparse sidecar,
payloads: PayloadArena }` (`tape/mod.rs:94-101`), and one cursor `ValueRef<'doc,'input, K =
AnyKind, G: EventGrammar = AnyGrammar>` = `&Tape` + `u32` cursor + 3 zero-size `PhantomData`
(`:175-181`; size == `(&Tape, u32)`, asserted `event_grammar_tests.rs:52-61`). Both value
APIs are cursors-over-offsets — laziness is already the substrate invariant: nothing is
materialized into the arena by either navigation (`json/view.rs` re-derives every span via
`string_body_range`/`scalar_span`/`next_sibling_cursor`; `css_l4_*/generated.rs:269-332`
re-derives every field from `(source, offset)`, "writing nothing to the payload arena", :303).

The two impls DIVERGE in navigation shape — this is the crux:

- **JSON = a recursive nested TREE.** `json/value.rs` + `json/view.rs`: `ValueRef<K>` carries
  a REAL `K` axis (`RootKind`/`ObjectKind`/`ArrayKind`/`StringKind`/`NumberKind`/`BoolKind`/
  `NullKind`, `view.rs:453-460`, used as field types `:86`/`:143`/`:197`…). Rich navigation:
  `JsonObject::get(key)` (`:106`), `pairs()` (`:90`), `JsonArray::values()` (`:147`), the
  6-variant typed enum `JsonValue` (`value.rs:69`), `value_from_ref` byte-dispatch
  (`value.rs:143`), recursive `walk_value` visitor (`visitor.rs:16-39`, recurses into
  containers), `DocumentView for JsonDocument` (`view.rs:68`). Containers NEST: object → pairs
  → values → (object | array | scalar), `next_sibling_cursor` (`view.rs:355`) does the depth-walk.
- **CSS = a FLAT node sweep.** `css_l4_*/generated.rs`: `CssNode` wraps a bare `ValueRef`
  (NO `K`, `:46`). `CssDocument::nodes()` is `(0..offsets.len()).map(CssNode)` — a linear sweep
  (`:269-273`), NOT recursive. 2-variant typed enum `CssTypedNode` (`:115`), `value()` →
  `CssTypedNode` (`:95`, the analogue of `value_from_ref`), declaration → `typed_value()` →
  `CssTypedValue::classify` (`:201`/`:229`, the rich-plane hot candidate per profile §4).
  There is NO `get(key)`, NO container nesting, NO recursion. `CssDocument` does NOT implement
  `DocumentView`.

**The LCD-flatten trap (R6, the load-bearing risk).** A trait whose only navigation verb is
`nodes() -> impl Iterator<Node>` + `Node::kind()` lets CSS implement trivially but FLATTENS
JSON — erasing `get(key)`, the recursive container model, and the typed `JsonValue` variants.
That is exactly the `json_rich_navigation_preserved == false` REJECT addendum 4 forbids
(`a1` §L4 "critical preserve-rich-ast guard"). The trait must be the GREATER common shape, not
the lesser. **The phantom `<G>` is a SEPARATE, orthogonal question** (`a1` §L4: G4 targets the
`G` axis ONLY; `K` is the already-real axis, do NOT conflate): `G: EventGrammar = AnyGrammar`
has ZERO non-test instantiations — the only animators are `_proof_compiles::<JsonEventGrammar/
SheetsEventGrammar>` in `event_grammar_tests.rs:18-21` (cfg `proof`/test); `AnyGrammar::
STRUCTURAL_CLASS_COUNT == 0` (production default is inert, `event_grammar.rs:20`);
`admits_fact`/`admits_class`/`STRUCTURAL_CLASS_COUNT` have zero production call sites; the
witnesses (`json/event_grammar_witness.rs`, `sheets_witness/event_grammar_witness.rs`) are
inert types. Deleting `<G>` and defining the shared trait are SEPARABLE (`a1` §L4).

## 1. The phantom `<G>` — instantiate-or-DELETE (decided first; it gates the trait surface)

DELETE is the abrogate-before-patch default (`SYNTHESIS.md:334`, `a1` §L4 REVISE). Disk
evidence is unambiguous: no `CssEventGrammar` exists; the `G`-axis is never reached at a
non-test call site; `ValueRef`'s layout is `G`-invariant (PhantomData, asserted). "Instantiate"
would entail AUTHORING a new grammar-named `*EventGrammar` type AND threading it onto every
production `ValueRef` — manufacturing the very grammar-named surface addenda 2/3 forbid, for a
parameter production code never animates. **RECOMMENDATION: DELETE `<G>` from `ValueRef`** (drop
`G`, `_grammar`, the `EventGrammar` bound; keep `K`). The `EventGrammar` trait + the two
witnesses are then dead — delete them too (P4 `FORBIDDEN_GENERIC_TOKENS` adds `EventGrammar`/
`*EventGrammar` per `SYNTHESIS-AUDIT-OVERFIT.md` §2.1.2, catching any re-emission). Telemetry:
`phantom_generic_resolved == deleted`. The shared trait is defined over the surviving
`ValueRef<K>`, NOT over `<G>` — the two are independent, so DELETE does not weaken the trait.

## 2. Candidate trait architectures (≥2 real impls so the trait cannot collapse to the lesser)

### Candidate A — `Cursor` micro-trait + grammar-owned navigation (RECOMMENDED)

The ONLY shared abstraction is the laziness primitive both already share: a cursor over the
tape. A minimal `Cursor` trait captures `{ tape() -> &Tape, cursor() -> u32, offset() -> usize,
kind() -> Self::Kind }` with an associated `type Kind` (JSON: `JsonNodeKind`; CSS: `CssNodeKind`)
— this is the de-duplication of the `at_cursor`/`offset`/`ValueRef` plumbing, nothing more.
Container navigation is NOT in the shared trait: `get(key)`/`pairs()`/`values()`/`walk_value`
stay JSON-owned; `nodes()`/`value()`/`typed_value()` stay CSS-owned. Each grammar's typed enum
(`JsonValue`, `CssTypedNode`) and rich verbs remain native, full-fidelity. `DocumentView`
(already `tape/mod.rs:227`, with `type Root` associated) is the document-level seam: implement
it for `CssDocument` too (today only `JsonDocument`, `view.rs:68`), `Root = the grammar's root
cursor`. Two real impls: JSON `ValueRef<K>`-backed cursors, CSS `CssNode`-backed cursors.

- **Pros:** ZERO LCD-flatten risk — JSON rich nav is structurally untouched (the trait never
  sees `get`/containers, so it cannot erase them); `json_rich_navigation_preserved == true` by
  construction. Smallest surface; no phantom re-introduction; preserves the divergence as
  divergence (tree vs flat) instead of forcing a false common shape. `DocumentView` finally
  earns a second impl (today it has zero generic consumers — `root_value` is never called
  generically, `DocumentView` is latent). Sheets (PROVE) implements the same `Cursor`+
  `DocumentView` with its own `SheetsNodeKind`.
- **Cons:** the shared trait is THIN — it unifies the cursor primitive, not navigation; a critic
  may call it "too small to be a generalization." Rebuttal: the navigation divergence is REAL
  (tree vs flat); a wider trait can only unify by flattening (LCD REJECT) — so thin-but-honest
  is the correct altitude, and the laziness/cursor contract is exactly the addendum-4 target.

### Candidate B — `Document`/`Value`/`Cursor` three-trait stack with a TREE-shaped `Value`

A richer hierarchy: `Document::root() -> Self::Value`; `Value` exposes a `classify() ->
Self::Typed` (returns the grammar's typed enum as an associated type) PLUS structural navigation
verbs `as_object()`/`as_array()`/`children()` that JSON implements fully and CSS implements as
"flat sequence of nodes, no keyed object." The trait keeps JSON's `get(key)` reachable by making
the object/array views associated types with their own keyed/indexed methods (trait carries the
SHAPE, JSON fills it, CSS's object-view is empty/`None`).

- **Pros:** a genuinely richer shared vocabulary; one generic consumer (e.g. a canonical-string
  or visitor driver) could in principle run over either grammar; reads as a "real" value-API.
- **Cons:** HIGH LCD-flatten hazard — to make CSS satisfy `as_object()`/`children()` you either
  (a) give CSS empty/degenerate impls (the trait then advertises tree-nav that CSS doesn't have
  — dead surface, an inverse phantom), or (b) weaken the verbs to the CSS flat shape (erasing
  JSON's keyed nesting — the REJECT). The associated-type escape works on paper but balloons the
  trait (object-view + array-view + scalar-view associated types, each with methods) for ONE real
  generic consumer that does not exist today. Risks re-introducing complexity the substrate was
  praised for avoiding. Over-fits a "value-API" aesthetic over two grammars that genuinely don't
  share value SHAPE.

### Candidate C — Unify only at `DocumentView` + a shared lazy `nodes()`/`tokens()` stream

Keep the existing `DocumentView` trait as the sole shared abstraction; add ONE shared method —
a lazy `tokens()`/`nodes()` offset-stream iterator (JSON already has `token_stream()`
`view.rs:46`; CSS already has `nodes()` `:269`) — and stop there. No `Value`/`Cursor` trait; the
typed navigation stays 100% grammar-native.

- **Pros:** truly minimal; both already expose an offset/token stream, so the shared method is a
  rename, not new code; zero flatten risk (the stream is the structural plane, not the value
  plane).
- **Cons:** this barely qualifies as a "Value/Document/Cursor trait" — it shares the STREAM, not
  the value navigation, so it does not discharge the R-D mandate ("a shared Value/Document/Cursor
  trait both implement") in spirit; it leaves the `at_cursor`/`offset`/cursor plumbing duplicated
  per grammar (the actual de-dup opportunity); `DocumentView::Root` stays a zero-consumer latent
  trait. Honest but under-delivers the generalization.

## 3. Recommendation, risk, prune/sequencing

**RECOMMEND Candidate A** (`Cursor` micro-trait over the surviving `ValueRef<K>` + extend the
existing `DocumentView` to a second/third impl; DELETE `<G>`). It is the unique candidate that
(i) gives ≥2 real impls that CANNOT collapse to the lesser — the trait shares the laziness/cursor
contract, never the navigation, so JSON's rich tree (`get`/`pairs`/`values`/typed `JsonValue`/
recursive visitor) is preserved by construction (`json_rich_navigation_preserved == true`); (ii)
resolves the phantom by DELETE without manufacturing a grammar-named type; (iii) adds no second
substrate (one `Tape`); (iv) preserves laziness (cursor over offsets, span re-derived on demand,
arena untouched) — the preserve-rich-ast invariant holds. Candidate B is the seductive
over-build (LCD hazard + dead surface for an absent consumer); Candidate C under-delivers.

**KEY RISK:** the "shared trait too thin to count as a generalization" critique at CHALLENGE —
mitigated by binding the trait to its TWO falsifiable telemetry columns (`phantom_generic_resolved
== deleted` AND `json_rich_navigation_preserved == true`), and by noting the wider trait is the
REJECT, not the win: any trait broad enough to satisfy the critic is broad enough to LCD-flatten
JSON. The honest generalization is the cursor/laziness contract, not a forced common value shape.

**PRUNE/SEQUENCING:** G4 entry-gates on G1+G2+G3 CLOSED (`SYNTHESIS-AUDIT-OVERFIT.md` §5 graph) —
the un-forked emitter (G3) must emit the JSON+CSS `generated.rs` value-API surfaces THROUGH one
path before G4 can define the trait both emit. P3 (collapse the 7 byte-identical CSS replicas,
md5 `b654562c…`) must precede so the CSS impl is singular, not 7×. `<G>` DELETE is independent of
the trait and may land within G4 first (it unblocks the clean `ValueRef<K>` the trait targets);
P4's `FORBIDDEN_GENERIC_TOKENS` extension (`EventGrammar`) must be live so deletion stays enforced.
