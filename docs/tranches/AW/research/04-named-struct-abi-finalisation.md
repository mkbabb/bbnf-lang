# 04 — Named-struct ABI finalisation: W0.5 layout contract and the `Color` view

## Framing

Not a revival of `StructRegistry`. That scaffold was deleted in
AU.4.2 (commit `ab8588a`) after two tranches of zero population;
the forward path: *"codegen handles struct projections via
per-backend type tables, not a central registry."* AV.0.5 took
that path — `PayloadData::LargeAggregate` in bbnf-tape
(`e7add15`), tuple-shaped colour grammar in
`grammar/css/l4/color.bbnf` (`ec20e99`:
`colorFunction -> (u8 space, f64 c1, f64 c2, f64 c3, f64 alpha)`),
and `aggregate_payload_ctor(total_bytes)` routing in
`tape_prelude.rs`. The substrate is live. What never landed is
the consumer: the IR layout pass rejects
`TypeDesc::Named("Color")` at admission, and no Rust-side view
accessor decodes the blob into a typed struct. W0.5 writes the
consumer.

## 1. Layout-pass admission contract

Today's rejection arm in
`crates/ir/src/passes/payload/layout.rs::compute_payload_layouts`
(lines 129–133) reads:

```rust
TypeDesc::Named(_) => continue,
```

The AU.4.2 rewrite replaced a `struct_registry` lookup with a
bare `continue` — correct *as long as* the per-backend type
table is invoked at the right seam. W0.5 threads that seam into
the pass without touching `TypeDesc`:

```rust
TypeDesc::Named(sid) => match ctx.backend_types.resolve_named(*sid) {
    // Backend resolved "Color" to a concrete scalar tuple.
    // Plan the layout exactly as we do for TypeDesc::Tuple —
    // no new variant, no Struct codepath, no central registry.
    Some(TypeDesc::Tuple(fields)) => plan_layout(&fields),
    None | Some(_) => continue,
},
```

Key points:

- **No new `TypeDesc::Struct` variant.** Admission runs through
  the same `plan_layout` the scalar-tuple arm uses.
- **`ctx.backend_types` is backend-supplied.** The IR crate owns
  the trait; the implementation lives in the backend crate (§2).
- **KV-pair recognition unchanged.** `(U8, F64, F64, F64, F64)`
  has a non-Span first field; `is_kv_pair_shape` rejects it.
- **`MAX_PAYLOAD_BYTES = 16` is the inline cap.** The
  colour-function tuple plans to `total_bytes = 40` (u8 at 0 →
  align-bump to 8 → four f64s at 8/16/24/32). AV.0.5's
  `aggregate_payload_ctor` already dispatches `> 16 →
  LargeAggregate`; W0.5 raises the `plan_layout` cap to a
  `LARGE_PAYLOAD_MAX` (64, leaves slack for `colorMix`) and lets
  the emitter's ctor pick inline-vs-arena.

## 2. Per-backend resolver placement

New file: `crates/core/src/backend/rust/view/named_types.rs`:

```rust
pub trait BackendNamedTypes {
    fn resolve_named(&self, sid: StringId) -> Option<TypeDesc>;
}

pub struct RustNamedTypes<'ir> { strings: &'ir StringArena }

impl<'ir> BackendNamedTypes for RustNamedTypes<'ir> {
    fn resolve_named(&self, sid: StringId) -> Option<TypeDesc> {
        match self.strings.resolve(sid) {
            "Color" | "ColorMix" => Some(TypeDesc::Tuple(vec![
                TypeDesc::U8, TypeDesc::F64, TypeDesc::F64,
                TypeDesc::F64, TypeDesc::F64,
            ])),
            _ => None,
        }
    }
}
```

TS/WASM backends carry sibling tables (`ts/view/named_types.rs`,
`wasm/view/named_types.rs`). IR sees none; the trait object
threads in at pass-dispatch time.

## 3. `LargeAggregate` decode contract

### Byte layout (40 B payload, 40 B arena slot)

```
offset  field    width  dtype     source
──────  ─────    ─────  ─────     ──────────────────────────
0       space    1 B    u8        colorType (0=rgb, 1=rgba, 2=hsl, 3=hsla,
                                   4=hwb, 5=lab, 6=lch, 7=oklab, 8=oklch)
1..8    pad      7 B    [u8;7]    zero-init (plan_layout align 1→8)
8       c1       8 B    f64       colorValue #1 LE bytes
16      c2       8 B    f64       colorValue #2 LE bytes
24      c3       8 B    f64       colorValue #3 LE bytes
32      alpha    8 B    f64       colorValue #4 LE bytes (NaN = absent)
──────                  40 B payload, 40 B arena slot
```

### Alignment: 8-byte, 40 B slot

Rejecting 1-byte packing (33 B) because:

1. `plan_layout`'s `aligned = (offset + align - 1) & !(align - 1)`
   already bumps each field to natural alignment; hand-packing
   requires a second arm — a conditional codepath the
   no-orthogonal-codepaths invariant forbids.
2. `alloc_large_aggregate_slot` rounds to an 8-byte boundary via
   `div_ceil(8) * 8`. The pad is free.
3. D-cache pressure follows record count, not width past 32 B.

`leaves::aggregate_field_read` already decodes each scalar via
`f64::from_le_bytes` at its field offset; `Tape::payload_bytes(rec,
40)` already exists.

## 4. `Color` Rust struct + lightningcss mapping

```rust
// crates/core/src/backend/rust/view/color.rs (new)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Color {
    pub space: ColorSpace,
    pub c1: f64, pub c2: f64, pub c3: f64, pub alpha: f64,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ColorSpace {
    Rgb = 0, Rgba = 1, Hsl = 2, Hsla = 3, Hwb = 4,
    Lab = 5, Lch = 6, Oklab = 7, Oklch = 8,
    // colorFn predefined spaces shifted +10 to avoid collision:
    Srgb = 10, SrgbLinear = 11, DisplayP3 = 12, A98Rgb = 13,
    ProphotoRgb = 14, Rec2020 = 15, XyzD50 = 16, XyzD65 = 17, Xyz = 18,
}
```

The accessor shifts `colorFn`'s space +10 so `ColorSpace` is a
single enum across both grammar rules.

### Field-by-field mapping

`CssColor` is a tagged enum. `RGBA` holds all-u8 (`red/green/blue/
alpha: u8`, lines 1591–1601). `Predefined(Box<...>)` and
`LAB(Box<...>)` hold f32 channels + f32 alpha via
`define_colorspace!`. bbnf keeps f64 — CSS Color Level 4 accepts
`<number>`/`<percentage>` in every channel (`hsl(180deg 50% 50%)`'s
hue doesn't fit u8), and one shape covers all nine families
without boxing.

Equivalence is value-after-canonicalisation. For `RGBA`:

```
bbnf.space == Rgb | Rgba
bbnf.c1/c2/c3 == red/green/blue as f64
bbnf.alpha == (alpha as f64) / 255.0   // lightningcss stores u8
```

For `Predefined(SRGB { r, g, b, alpha })`: space=Srgb; c1/c2/c3
= r/g/b; alpha=alpha. For `LAB(LABColor::LAB(LAB{l,a,b,alpha}))`:
space=Lab; c1/c2/c3=l/a/b; alpha=alpha.

## 5. `.view().as_color()` codegen path

**Rule-local, not a generic trait impl.** Three rules project
into `Color` (`colorFunction`, `colorFn`, `colorMix`), each
with distinct layouts (`colorMix` uses nested `ColorRef`). A
generic trait still needs `rule_kind()` dispatch; per-rule
emission keeps the hot path monomorphic.

`emit_aggregate_accessors` lands on colour-function rules
unchanged via the `Named → Tuple` resolution. The Rust backend
additionally emits a thin `.as_color()` shim alongside
`.value()` — gated on
`ir.get_string(type_annotation) == "Color"`:

```rust
impl<'p> ColorFunctionView<'p> {
    #[inline]
    pub fn as_color(&self) -> crate::view::color::Color {
        let (space, c1, c2, c3, alpha) = self.value();
        crate::view::color::Color {
            space: crate::view::color::ColorSpace::from_u8(space),
            c1, c2, c3, alpha,
        }
    }
}
```

The top-level `color` Alt dispatches on the chosen branch:

```rust
impl<'p> ColorView<'p> {
    pub fn as_color(&self) -> Color {
        match self.chosen() {
            ColorVariant::namedColor(n)    => Color::from_named_u32(n.value()),
            ColorVariant::hex(h)           => Color::from_named_u32(h.value()),
            ColorVariant::colorFunction(f) => f.as_color(),
            ColorVariant::colorFn(f)       => f.as_color(),
            ColorVariant::colorMix(m)      => m.as_color(),
        }
    }
}
```

## 6. lightningcss equivalence — W5 parity harness

New test `css_l4_named_color_parity::white_materialises` in
`crates/core/tests/lightningcss_parity.rs`.

```rust
#[test]
fn white_materialises() {
    let cases = &[
        ("white",                 ColorSpace::Rgb, 255.0, 255.0, 255.0, 1.0),
        ("rgb(255 128 0 / 0.5)",  ColorSpace::Rgb, 255.0, 128.0,   0.0, 0.5),
        ("transparent",           ColorSpace::Rgb,   0.0,   0.0,   0.0, 0.0),
        ("hsl(180 50% 50%)",      ColorSpace::Hsl, 180.0,  50.0,  50.0, f64::NAN),
    ];
    for (src, space, c1, c2, c3, alpha) in cases {
        let bbnf = CssL4Parser::parse(src).unwrap().view().as_color();
        assert_eq!(bbnf.space, *space);
        assert_f64_eq_or_nan(bbnf.c1, *c1);
        assert_f64_eq_or_nan(bbnf.c2, *c2);
        assert_f64_eq_or_nan(bbnf.c3, *c3);
        assert_f64_eq_or_nan(bbnf.alpha, *alpha);
    }
    // Cross-check against lightningcss (rgb family):
    if let CssColor::RGBA(rgba) = lightningcss::values::color::CssColor::parse_string("white").unwrap() {
        let bbnf = CssL4Parser::parse("white").unwrap().view().as_color();
        assert_eq!(bbnf.c1, rgba.red as f64);
        assert_eq!(bbnf.alpha, (rgba.alpha as f64) / 255.0);
    }
}
```

### Edge cases

- **`white` (AW.0.8).** `0xFFFFFFFFu32 == TapeOffset::NONE`;
  AW.0.8 routes all `namedColor` through `WideScalar` preserving
  the value past the sentinel. `from_named_u32(0xFFFFFFFF)` →
  `Color { Rgb, 255.0, 255.0, 255.0, 1.0 }`.
- **`currentColor`, `transparent`.** lightningcss preserves
  `CurrentColor` as a non-channel variant; bbnf folds both into
  the u32 encoding (grammar lines 56, 178). Known lossy;
  `.is_current_color_keyword()` distinguishes source form.
- **Alpha-less.** `rgb(255 128 0)` — optional alpha doesn't fire;
  pad at offset 32 is zero, decoding to 0.0. The emitter writes
  `f64::NAN.to_le_bytes()` when skipped (one-line epilogue
  addition). W5 checks `bbnf.alpha.is_nan()`.

### Risks

1. **Discriminant drift.** bbnf's `colorType` (0–8) vs.
   lightningcss's enum ordering is not stable across lightningcss
   versions. `ColorSpace` pins bbnf's contract; the lightningcss
   side is a projection function.
2. **Named-color u32 collision.** `currentColor → 0x000000FFu32`
   collides with `black`. Span-text distinguishes them; the u32
   alone does not.
3. **f32↔f64.** Predefined/LAB store f32; parity compares
   `(f32 as f64)`.

## 7. Interaction with W0.10 `fuse_single_use`

AW.0.10 drops the always-true `scc_id.is_none()` guard so fusion
runs. Colour-function rules are called from `color` via the
single top-level Alt — ref-count = 1. Are they fused?

The `is_composite_seq` filter (`fuse.rs:57`) rejects multi-child
Seq bodies. Colour-function bodies are
`Seq(colorType, "(", colorValue, colorValue, colorValue,
(alphaSep >> colorValue)?, ")")` — multi-child, so filtered out
and **not fused**. Post-W0.10 layout-pass view is unchanged: the
rule remains callable, its aggregate layout fires, the record is
a leaf with the 40 B payload. The `color` Alt (ref-count > 1 at
every CSS value call site) is also unaffected. Fusion's net
effect on the colour substrate is zero.

AW.0.10's DTA state-count reduction (hard gate: CSS L4 < 2000
states, down from 2473) comes from fusing non-colour glue
(commas, whitespace wrappers, separator compositions). The
colour-function substrate is insulated by `is_composite_seq`.

## Citations

- AU.4.2 commit `ab8588a` — StructRegistry deletion; per-backend
  type-tables forward path.
- AV.0.5 commit `e7add15` — `PayloadData::LargeAggregate`.
- AV.0.5 commit `ec20e99` — colour-function grammar projections.
- AV.0.5 CO-E1 — `aggregate_payload_ctor(total_bytes)`.
- AW.md §§Phase 0.5, invariant 5, hard gate 7, §AW.0.8, §AW.0.10.
- `lightningcss 1.0.0-alpha.71::values::color::{CssColor, RGBA}`.
