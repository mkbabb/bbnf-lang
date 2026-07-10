# PROTO-PB-3 — The greenfield value-plane API sketch

Analysis instrument, never landed. Repo: bbnf-lang (scratch crate). Date: 2026-07-10.
Scratch crate: `/private/tmp/pb3-scratch` (602 LOC total; the neutral surface is
`src/main.rs:77-360`, ~283 LOC — within the charter's ~300-500 band). Dependency-free;
`cargo run` exits 0, all four lane assertions pass, zero errors, two dead-code warnings
(unused `ScalarClass::Int`, `View::is_null` — surface authored ahead of the worked walk).

## Verdict

**YES.** One grammar-neutral surface — eager fused `Sink` + lazy `View` cursor + `Path`
alphabet + direct-to-struct — compiles as Rust signatures the existing AoS tape satisfies,
with **no per-grammar enum vocabulary** on the neutral plane. The whole surface names
exactly one structural enum (`NodeClass`); every grammar bridges to it through a two-method
`Classify` trait. That single bridge is the constructive answer to the sheets disclaimer.

## The tape contract carried (not rebuilt)

The scratch `Tape` mirrors skinny `crates/runtime/src/tape/mod.rs:92-171` faithfully:
`source: &[u8]` + `offsets: Vec<u32>` + a sparse `(flag_cursors, flag_values)` sidecar,
with the same four accessors the real grammars read through — `source()`, `offsets()`,
`offset_at(cursor)` (mod.rs:136), `flags_at(cursor)` (binary-search sidecar, mod.rs:142).
No second substrate; the AoS one-tape is the only backing store, per the sk-v21 AoS
adjudication. The JSON offset shape was verified against the real scanner: only
`{ } [ ] "` and number/`t`/`f`/`n` starts are sealed; commas and colons are consumed but
never recorded (`generated.rs:318,332,352,371` — `consume_container_next` /
`consume_array_next` emit the close, skip the comma). The scratch recorder `record_json`
reproduces exactly that offset sequence, so the neutral walk reads a byte-faithful tape.

## The neutral vocabulary — the crux

```rust
pub enum Shape       { Map, Seq }
pub enum ScalarClass { Str, Int, Float, Bool, Null, Raw }
pub enum NodeClass   { Enter(Shape), Leave(Shape), Scalar(ScalarClass) }

pub trait Classify {
    fn classify(tape: &Tape, cursor: u32) -> NodeClass;
    fn scalar_span(tape: &Tape, cursor: u32) -> Range<usize>;
}
```

`NodeClass` is the ONLY structural enum on the neutral plane. A grammar's rich native
vocabulary — `JsonNodeKind`'s 13 variants (`json/value.rs:12-26`), `GoogleSheetsKind`'s
39 (`sheets/view.rs:8-48`) — never crosses the seam; it is *projected* into `NodeClass`
by `classify`, never *flattened* into a shared kind type. This is precisely what the
`Cursor` trait doc calls impossible for its own `type Kind`
(`tape/cursor.rs:11-13`, "there is no shared structural vocabulary to flatten into") — and
it is answered by moving one level down the abstraction ladder: grammars share no *kind
names*, but they do share a *structural alphabet* (open / close / leaf). The bridge is two
functions; the grammar keeps everything else private.

Decisive proof of neutrality: the identical `Sink`/`drive`/`View` code drives **two disjoint
grammars** in the same binary. `JsonClass` sources `NodeClass` from the source byte
(`'{'`→`Enter(Map)`, `'"'`→`Scalar(Str)`, …); `FlatClass` sources it from the sparse **flag
plane** (`flags_at` tag `1`→`Scalar(Float)`) — a `GoogleSheetsKind`-shaped classifier with a
vocabulary that shares nothing with JSON's. Both satisfy `Classify`; neither leaks an enum.

## The four lanes (all exercised on one twitter-fragment tape)

Fixture source (twitter-shaped):
`{"id":12345,"text":"hello world","user":{"name":"jack","verified":true},"retweets":100,"score":3.5,"deleted":null,"tags":["news","tech"]}`

- **Lane 1 — eager fused `Sink`.** `trait Sink { enter/leave/key/scalar }` +
  `fn drive<C: Classify, S: Sink>(tape, &mut S)` — one linear pass, key/value alternation
  tracked by a `(Shape, expecting_key)` stack (Map position assigns the key role, exactly
  as `json/view.rs`'s `JsonObjectPairs` treats the first of each pair as key). A
  `MinifySink` reconstruction round-trips the source string byte-for-byte
  (`assert_eq!(minified, src)` passes).

- **Lane 2 — lazy `View` cursor.** 8-byte `View<'d,'i,C>` (tape + cursor, `PhantomData<C>`),
  isomorphic to `ValueRef` (`tape/mod.rs:173`). Navigation is **grammar-neutral**:
  `next_sibling` is balanced `Enter`/`Leave` depth counting over the alphabet — the generic
  form of `json/view.rs:355 next_sibling_cursor`, but with zero JSON knowledge. `entries()`
  / `items()` mirror `JsonObjectPairs` / `JsonArrayValues`. The **byte-identity oracle**:
  `to_canonical(root)` (sorted keys, minified, serde-shaped string escaping, raw numbers)
  equals the `json/view.rs:116 to_canonical_string` output character-for-character:
  `{"deleted":null,"id":12345,"retweets":100,"score":3.5,"tags":["news","tech"],"text":"hello world","user":{"name":"jack","verified":true}}` — `assert_eq!` against that literal passes.

- **Lane 3 — `Path` alphabet.** `enum Seg { Key(&str), Index(usize) }`; `View::path(&[Seg])`
  folds `get`/`at`. `path([Key("user"),Key("name")]) == "jack"`,
  `path([Key("tags"),Index(1)]) == "tech"`, `path([Key("score")]).as_f64() == 3.5` — all
  assert-pass. This is the `bbnf-path` cursor alphabet rebuilt natively (sk-v22, not
  ported), reduced to two neutral segment kinds over the same `View`.

- **Lane 4 — direct-to-struct.** `trait FromView<'i> { fn from_view<C: Classify>(View) -> Option<Self> }`.
  A `Tweet { id: i64, text: String, user_name: String, verified: bool, first_tag: String }`
  materializes directly off the lazy `View` (fields pulled via `get`/`path`, no intermediate
  value tree), `assert_eq!` against the expected struct passes. The serde `Deserialize` path
  is the mechanical wrapper of this same access — sketched below, not compiled here to keep
  the instrument dependency-free.

## The serde bridge (signature-only; mechanical over the same View methods)

```rust
impl<'de, 'd, 'i: 'd, C: Classify> serde::Deserializer<'de> for View<'d, 'i, C> {
    type Error = ViewError;
    fn deserialize_any<V: Visitor<'de>>(self, v: V) -> Result<V::Value, Self::Error> {
        match self.class() {
            NodeClass::Enter(Shape::Map) => v.visit_map(EntriesAccess(self.entries())),
            NodeClass::Enter(Shape::Seq) => v.visit_seq(ItemsAccess(self.items())),
            NodeClass::Scalar(ScalarClass::Str)  => v.visit_borrowed_str(self.as_str().unwrap()),
            NodeClass::Scalar(ScalarClass::Bool) => v.visit_bool(self.as_bool().unwrap()),
            NodeClass::Scalar(ScalarClass::Null) => v.visit_unit(),
            NodeClass::Scalar(_)                 => v.visit_f64(self.as_f64().unwrap()),
            NodeClass::Leave(_) => unreachable!(),
        }
    }
    // forward_to_deserialize_any! for the rest
}
```

`EntriesAccess`/`ItemsAccess` are one-line `MapAccess`/`SeqAccess` adapters over the Lane-2
iterators. `#[derive(Deserialize)]` then materializes any struct — this is the Q1
derive-macro direct-to-struct fold (SPEC-DRAFT:642) landing for free once the neutral `View`
exists. The compiled `FromView` in the scratch crate is this shape with the proc-macro elided.

## Caveats surfaced (honest delineation)

1. **String-escape parity.** `to_canonical`'s `push_json_string` covers `" \ \n \r \t` +
   control-`\uXXXX`; full serde-json unicode-escape parity (and the `STRING_NEEDS_DECODE`
   flag-driven unescape at `json/view.rs:206 as_str` / `config.rs:5`) must be carried into
   the real `View::as_str`. The fixture is ASCII-clean, so byte-identity holds exactly here;
   escaped corpora need the flag-plane unescape wired through `Classify` (a third optional
   method, `decode_scalar`).
2. **`scalar_span` re-scan.** Numbers/strings re-scan for their end on demand, matching
   `json/view.rs:392 scalar_span` / `string_body_range`. This is carried as-is; it is the
   lazy tax the eager `Sink` avoids (the fused-sink-default adjudication stands).
3. **Container `Leave` shape.** Depth counting needs only `Enter`/`Leave`; `Leave(Shape)`
   carries the shape purely for symmetric `Sink` emission. A grammar whose closes are
   unshaped can return `Leave(Shape::Map)` uniformly with no navigation cost.

## Bearing on the tranche plan (TB4.W2)

The value-plane greenfield is de-risked: the neutral surface is small (~283 LOC for all four
lanes vs the −1,059 LOC of per-grammar `json/sheets/css` view+sink+value it retires,
SPEC-DRAFT:93,312), it compiles over the carried AoS tape unchanged, and the per-grammar cost
is a two-method `Classify` — the whole obligation a grammar owes the plane. The
projection-totality invariant (SPEC-DRAFT:308) has a concrete shape: every sealed cursor must
`classify` into `NodeClass` and every leaf `scalar_span` into valid source bytes; the
differential oracle is exactly `to_canonical` vs the frozen `json/view.rs` — demonstrated
byte-identical here. Recommend TB4.W2 adopt `NodeClass`/`Classify` as the seam and fold the
`Diagnostic` error surface (SPEC-DRAFT:317) into `ViewError`.
