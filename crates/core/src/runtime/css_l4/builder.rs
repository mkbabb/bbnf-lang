//! AZ-I.W2-act.B3 — `CssStructBuilder` — the concrete `StructBuilder`
//! impl that the generated CSS L4 parse function targets.
//!
//! The builder maintains an in-flight typed stack of partially-built
//! compounds. Each frame on the stack collects per-shape state matching
//! one of the grammar's compound rules — stylesheet / rule / style
//! rule / media rule / keyframe block / declaration / selector list /
//! value list — finalising onto the parent frame when the matching
//! `end_compound` arrives.
//!
//! Scalar pushes (`push_leaf_with_*`) land on the topmost frame's
//! pending slot per the rule's typed projection:
//!
//! - `length` / `angle` / `time` / `frequency` / `resolution` / `flex`
//!   / `percentage` rules combine `(f64, u8)` into the matching typed
//!   variant.
//! - `colorType` / `colorSpace` / `mathOperator` / `globalKeyword` u8
//!   discriminants land on the open frame's tag slot.
//! - `hex` / `namedColor` u32 packed colours land as scalar leaves.
//! - String / span pushes land as borrowed slices on the current open
//!   frame's slot.
//!
//! `begin_compound` consults the [`StructLayout`]'s
//! [`bbnf_ir::registry::LayoutKind`] + rule name to pick the matching
//! frame variant. `end_compound` finalises the frame onto its parent.
//!
//! # Wire contract
//!
//! Every typed `->` annotation in the CSS L4 grammar reaches one
//! [`StructBuilder`] method. The grammar's modular structure
//! (`grammar/css/l4/*.bbnf`) declares 187 layouts; this builder routes
//! them through eight coarse frame variants whose composition covers
//! the full alternation.

use bbnf_ir::registry::StructLayout;

use crate::runtime::builder::StructBuilder;
use crate::runtime::css_l4::arena::CssArena;
use crate::runtime::css_l4::document::CssDocument;
use crate::runtime::css_l4::value::{
    CssAngle, CssAngleUnit, CssColor, CssColorFunction, CssColorMix, CssColorPredefined,
    CssColorSpace, CssColorType, CssDimension, CssFlex, CssFrequency, CssFrequencyUnit,
    CssFunction, CssGlobalKeyword, CssHueMethod, CssLength, CssLengthUnit, CssMathOperator,
    CssPercentage, CssResolution, CssResolutionUnit, CssRule, CssTime, CssTimeUnit,
    CssTypedValue, Declaration, GenericAtRule, KeyframeBlock, KeyframesRule, MediaRule,
    Selector, StyleRule, StyleSheet,
};
use crate::runtime::handle::CompoundHandle;

/// One open compound frame on the builder's stack.
///
/// Each frame collects per-shape state and finalises by
/// [`CssStructBuilder::end_compound`] into the matching typed value
/// (or directly onto the enclosing aggregate's slot).
#[derive(Debug, Clone)]
enum OpenFrame<'p> {
    /// `stylesheet` — collecting [`CssRule`] entries.
    StyleSheet { rules: Vec<CssRule<'p>> },
    /// `qualifiedRule` — selector list + declaration list.
    StyleRule {
        selectors: Vec<Selector<'p>>,
        declarations: Vec<Declaration<'p>>,
        span: &'p str,
    },
    /// `mediaRule` — query span + nested rules.
    MediaRule {
        query: &'p str,
        rules: Vec<CssRule<'p>>,
    },
    /// `keyframesRule` — name + keyframe blocks.
    KeyframesRule {
        name: &'p str,
        blocks: Vec<KeyframeBlock<'p>>,
    },
    /// `keyframeBlock` — selector + declarations.
    KeyframeBlock {
        selector: &'p str,
        declarations: Vec<Declaration<'p>>,
    },
    /// `genericAtRule` — name + prelude + body.
    GenericAtRule {
        name: &'p str,
        prelude: &'p str,
        body: &'p str,
    },
    /// `<typedDecl>` — declaration with a property name and a value
    /// list. The value list collects [`CssTypedValue`] entries until
    /// the closing `;` / block end.
    Declaration {
        property: Option<&'p str>,
        values: Vec<CssTypedValue<'p>>,
        important: bool,
    },
    /// `selectorList` / `compoundSelector` / `complexSelector` —
    /// collecting [`Selector`] entries.
    SelectorList { selectors: Vec<Selector<'p>> },
    /// `value` Alt-of-Refs — forwards the single classified child to
    /// the enclosing frame's pending slot. Mirrors JSON's `Wrap`
    /// frame.
    Wrap { value: Option<CssTypedValue<'p>> },
    /// Length / angle / time / frequency / resolution / flex /
    /// percentage typed-numeric rules — collects `(f64, u8)` and
    /// finalises into the matching [`CssDimension`] variant.
    Numeric {
        rule_name: &'p str,
        magnitude: Option<f64>,
        unit: Option<u8>,
    },
    /// `colorFunction` / `colorFn` — collects type + components +
    /// optional alpha.
    ColorFunction {
        kind_tag: Option<u8>,
        space_tag: Option<u8>,
        components: Vec<f64>,
    },
    /// `colorMix` — collects mix space + hue method + nested colours
    /// and percentages.
    ColorMix {
        mix_space: Option<u8>,
        hue_method: Option<u8>,
        left: Option<&'p CssColor<'p>>,
        left_pct: Option<f64>,
        right: Option<&'p CssColor<'p>>,
        right_pct: Option<f64>,
    },
    /// Function call — name + argument value list.
    Function {
        name: &'p str,
        args: Vec<CssTypedValue<'p>>,
    },
}

/// Concrete `StructBuilder` for the CSS L4 grammar.
///
/// Owns a [`CssArena`] and an in-flight stack of open frames. The
/// generated parse function constructs a builder, threads it through
/// every per-shape parse fn, and calls [`CssStructBuilder::finalise`]
/// at EOF to recover the [`CssDocument`].
#[derive(Debug)]
pub struct CssStructBuilder<'p> {
    /// Owning arena.
    arena: CssArena<'p>,
    /// In-flight open compound stack.
    stack: Vec<OpenFrame<'p>>,
    /// The root stylesheet, set when the outermost compound finalises.
    root: Option<StyleSheet>,
    /// Monotonic compound handle counter.
    next_handle: u64,
    /// Pending typed value — set by scalar pushes that don't have an
    /// open frame to land on (e.g. a number literal pushed during a
    /// dimension's intermediate state). Drained by the next compound
    /// finalisation.
    pending_value: Option<CssTypedValue<'p>>,
}

/// Rollback snapshot for [`CssStructBuilder`].
#[derive(Debug, Clone)]
pub struct CssStructCheckpoint<'p> {
    rules: usize,
    decls: usize,
    selectors: usize,
    values: usize,
    keyframes: usize,
    colors: usize,
    stack: Vec<OpenFrame<'p>>,
    root: Option<StyleSheet>,
    next_handle: u64,
    pending_value: Option<CssTypedValue<'p>>,
}

impl<'p> Default for CssStructBuilder<'p> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'p> CssStructBuilder<'p> {
    /// Construct a fresh builder.
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: CssArena::new(),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
            pending_value: None,
        }
    }

    /// Construct a builder with arena slab capacity hints.
    #[inline]
    pub fn with_capacity(
        rules: usize,
        decls: usize,
        selectors: usize,
        values: usize,
        keyframes: usize,
    ) -> Self {
        Self {
            arena: CssArena::with_capacity(rules, decls, selectors, values, keyframes),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
            pending_value: None,
        }
    }

    /// Finalise the builder into a [`CssDocument`]. Panics if no
    /// stylesheet root was emitted or if any open frame remains.
    ///
    /// `input` is the slice the parse consumed, threaded through to
    /// the produced [`CssDocument::input`] so view callers can
    /// retrieve the source without re-acquiring it.
    #[inline]
    pub fn finalise(mut self, input: &'p str) -> CssDocument<'p> {
        debug_assert!(
            self.stack.is_empty(),
            "CssStructBuilder::finalise called with {} open frame(s)",
            self.stack.len()
        );
        let root = self.root.take().unwrap_or(StyleSheet {
            rules: crate::runtime::css_l4::arena::CssRuleListId::EMPTY,
        });
        CssDocument::new(self.arena, root, input)
    }

    /// Land a finalised typed value on the topmost open frame, or
    /// store it as the pending value when no frame is open.
    fn deposit_value(&mut self, value: CssTypedValue<'p>) {
        match self.stack.last_mut() {
            None => {
                self.pending_value = Some(value);
            }
            Some(OpenFrame::Declaration { values, .. }) => {
                values.push(value);
            }
            Some(OpenFrame::Wrap { value: slot }) => {
                *slot = Some(value);
            }
            Some(OpenFrame::Function { args, .. }) => {
                args.push(value);
            }
            // Other frames don't admit a typed value scalar push
            // directly — the per-shape parse fn calls don't reach this
            // branch in well-formed generation. Defensive store on the
            // pending_value slot keeps generation total per
            // `feedback_no-workarounds`.
            _ => {
                self.pending_value = Some(value);
            }
        }
    }

    /// Land a [`Declaration`] on the topmost frame admitting one
    /// (StyleRule / MediaRule's nested style rules / KeyframeBlock).
    fn deposit_declaration(&mut self, decl: Declaration<'p>) {
        if let Some(frame) = self.stack.iter_mut().rev().find(|f| {
            matches!(
                f,
                OpenFrame::StyleRule { .. } | OpenFrame::KeyframeBlock { .. }
            )
        }) {
            match frame {
                OpenFrame::StyleRule { declarations, .. } => declarations.push(decl),
                OpenFrame::KeyframeBlock { declarations, .. } => {
                    declarations.push(decl)
                }
                _ => {}
            }
        }
    }

    /// Land a [`CssRule`] on the topmost frame admitting one
    /// (StyleSheet / MediaRule).
    fn deposit_rule(&mut self, rule: CssRule<'p>) {
        if let Some(frame) = self.stack.iter_mut().rev().find(|f| {
            matches!(
                f,
                OpenFrame::StyleSheet { .. } | OpenFrame::MediaRule { .. }
            )
        }) {
            match frame {
                OpenFrame::StyleSheet { rules } => rules.push(rule),
                OpenFrame::MediaRule { rules, .. } => rules.push(rule),
                _ => {}
            }
        }
    }
}

impl<'p> StructBuilder for CssStructBuilder<'p> {
    type Checkpoint = CssStructCheckpoint<'p>;

    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        CssStructCheckpoint {
            rules: self.arena.rule_slab_count(),
            decls: self.arena.decl_slab_count(),
            selectors: self.arena.selector_slab_count(),
            values: self.arena.value_slab_count(),
            keyframes: self.arena.keyframe_slab_count(),
            colors: self.arena.color_count(),
            stack: self.stack.clone(),
            root: self.root,
            next_handle: self.next_handle,
            pending_value: self.pending_value,
        }
    }

    #[inline]
    fn rollback(&mut self, checkpoint: Self::Checkpoint) {
        self.arena.truncate(
            checkpoint.rules,
            checkpoint.decls,
            checkpoint.selectors,
            checkpoint.values,
            checkpoint.keyframes,
            checkpoint.colors,
        );
        self.stack = checkpoint.stack;
        self.root = checkpoint.root;
        self.next_handle = checkpoint.next_handle;
        self.pending_value = checkpoint.pending_value;
    }

    fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle {
        let frame = match (layout.kind, layout.rule_name.as_str()) {
            // Aggregate top-level rules.
            (_, "stylesheet") | (_, "ruleList") => OpenFrame::StyleSheet {
                rules: Vec::new(),
            },
            (_, "qualifiedRule") | (_, "ruleBlock") => OpenFrame::StyleRule {
                selectors: Vec::new(),
                declarations: Vec::new(),
                span: "",
            },
            (_, "mediaRule") => OpenFrame::MediaRule {
                query: "",
                rules: Vec::new(),
            },
            (_, "keyframesRule") => OpenFrame::KeyframesRule {
                name: "",
                blocks: Vec::new(),
            },
            (_, "keyframeBlock") => OpenFrame::KeyframeBlock {
                selector: "",
                declarations: Vec::new(),
            },
            (_, "genericAtRule") => OpenFrame::GenericAtRule {
                name: "",
                prelude: "",
                body: "",
            },
            // Declaration family — every typed `*Decl` rule.
            (_, name)
                if name.ends_with("Decl") || name == "declaration" || name == "customPropertyDecl"
                    || name == "genericDecl" =>
            {
                OpenFrame::Declaration {
                    property: None,
                    values: Vec::new(),
                    important: false,
                }
            }
            (_, "selectorList") | (_, "compoundSelector") | (_, "complexSelector") => {
                OpenFrame::SelectorList {
                    selectors: Vec::new(),
                }
            }
            // Numeric typed rules — length / angle / time / etc.
            (_, name)
                if matches!(
                    name,
                    "length"
                        | "angle"
                        | "time"
                        | "frequency"
                        | "resolution"
                        | "flex"
                        | "percentage"
                        | "unitless"
                ) =>
            {
                OpenFrame::Numeric {
                    rule_name: leak_static_str(name),
                    magnitude: None,
                    unit: None,
                }
            }
            // Color function family.
            (_, "colorFunction") | (_, "colorFn") => OpenFrame::ColorFunction {
                kind_tag: None,
                space_tag: None,
                components: Vec::new(),
            },
            (_, "colorMix") => OpenFrame::ColorMix {
                mix_space: None,
                hue_method: None,
                left: None,
                left_pct: None,
                right: None,
                right_pct: None,
            },
            // Function family — calc / min / max / clamp / var / env /
            // url / gradient / transformFunction / filterFunction /
            // easingFunction / genericFunction.
            (_, name) if name.ends_with("Function") || name == "url" => OpenFrame::Function {
                name: leak_static_str(name),
                args: Vec::new(),
            },
            // Catch-all: transparent wrap shape.
            _ => OpenFrame::Wrap { value: None },
        };
        self.stack.push(frame);
        self.next_handle = self.next_handle.wrapping_add(1);
        CompoundHandle::new(self.next_handle, 0)
    }

    fn end_compound(&mut self, _handle: CompoundHandle) {
        let frame = self
            .stack
            .pop()
            .expect("CssStructBuilder::end_compound on empty stack");
        match frame {
            OpenFrame::StyleSheet { rules } => {
                let id = self.arena.push_rules(rules);
                let sheet = StyleSheet { rules: id };
                if self.stack.is_empty() {
                    self.root = Some(sheet);
                }
            }
            OpenFrame::StyleRule {
                selectors,
                declarations,
                span,
            } => {
                let sel_id = self.arena.push_selectors(selectors);
                let decl_id = self.arena.push_decls(declarations);
                let style = StyleRule {
                    selectors: sel_id,
                    declarations: decl_id,
                    span,
                };
                self.deposit_rule(CssRule::Style(style));
            }
            OpenFrame::MediaRule { query, rules } => {
                let id = self.arena.push_rules(rules);
                let media = MediaRule { query, rules: id };
                self.deposit_rule(CssRule::Media(media));
            }
            OpenFrame::KeyframesRule { name, blocks } => {
                let id = self.arena.push_keyframes(blocks);
                let kf = KeyframesRule { name, blocks: id };
                self.deposit_rule(CssRule::Keyframes(kf));
            }
            OpenFrame::KeyframeBlock {
                selector,
                declarations,
            } => {
                let id = self.arena.push_decls(declarations);
                let block = KeyframeBlock {
                    selector,
                    declarations: id,
                };
                // Land on the enclosing KeyframesRule's blocks Vec.
                if let Some(OpenFrame::KeyframesRule { blocks, .. }) =
                    self.stack.last_mut()
                {
                    blocks.push(block);
                }
            }
            OpenFrame::GenericAtRule {
                name,
                prelude,
                body,
            } => {
                let rule = GenericAtRule {
                    name,
                    prelude,
                    body,
                };
                self.deposit_rule(CssRule::GenericAt(rule));
            }
            OpenFrame::Declaration {
                property,
                values,
                important,
            } => {
                let property = property.unwrap_or("");
                // Project the value list into a single typed value:
                // - empty → Span("")
                // - one entry → that entry directly
                // - many entries → arena-backed CssValueListId
                let value = match values.len() {
                    0 => CssTypedValue::Span(""),
                    1 => values.into_iter().next().unwrap_or(CssTypedValue::Span("")),
                    _ => {
                        let id = self.arena.push_values(values);
                        CssTypedValue::List(id)
                    }
                };
                let decl = Declaration {
                    property,
                    value,
                    important,
                };
                self.deposit_declaration(decl);
            }
            OpenFrame::SelectorList { selectors } => {
                if let Some(OpenFrame::StyleRule { selectors: dst, .. }) =
                    self.stack.last_mut()
                {
                    dst.extend(selectors);
                }
            }
            OpenFrame::Wrap { value } => {
                if let Some(v) = value {
                    self.deposit_value(v);
                }
            }
            OpenFrame::Numeric {
                rule_name,
                magnitude,
                unit,
            } => {
                let value = magnitude.unwrap_or(0.0);
                let dim = match (rule_name, unit) {
                    ("length", Some(u)) => CssDimension::Length(CssLength {
                        value,
                        unit: CssLengthUnit::from_discriminant(u),
                    }),
                    ("angle", Some(u)) => CssDimension::Angle(CssAngle {
                        value,
                        unit: CssAngleUnit::from_discriminant(u).unwrap_or(CssAngleUnit::Deg),
                    }),
                    ("time", Some(u)) => CssDimension::Time(CssTime {
                        value,
                        unit: CssTimeUnit::from_discriminant(u).unwrap_or(CssTimeUnit::S),
                    }),
                    ("frequency", Some(u)) => CssDimension::Frequency(CssFrequency {
                        value,
                        unit: CssFrequencyUnit::from_discriminant(u)
                            .unwrap_or(CssFrequencyUnit::Hz),
                    }),
                    ("resolution", Some(u)) => CssDimension::Resolution(CssResolution {
                        value,
                        unit: CssResolutionUnit::from_discriminant(u)
                            .unwrap_or(CssResolutionUnit::Dppx),
                    }),
                    ("flex", _) => CssDimension::Flex(CssFlex { value }),
                    ("percentage", _) => CssDimension::Percentage(CssPercentage { value }),
                    ("unitless", _) => CssDimension::Unitless(value),
                    _ => CssDimension::Unitless(value),
                };
                self.deposit_value(CssTypedValue::Dimension(dim));
            }
            OpenFrame::ColorFunction {
                kind_tag,
                space_tag,
                components,
            } => {
                let c1 = components.first().copied().unwrap_or(0.0);
                let c2 = components.get(1).copied().unwrap_or(0.0);
                let c3 = components.get(2).copied().unwrap_or(0.0);
                let alpha = components.get(3).copied();
                let color = if let Some(kind) = kind_tag {
                    let kind = CssColorType::from_discriminant(kind).unwrap_or(CssColorType::Rgb);
                    CssColor::Function(CssColorFunction {
                        kind,
                        c1,
                        c2,
                        c3,
                        alpha,
                    })
                } else if let Some(space) = space_tag {
                    let space = CssColorSpace::from_discriminant(space)
                        .unwrap_or(CssColorSpace::Srgb);
                    CssColor::Predefined(CssColorPredefined {
                        space,
                        c1,
                        c2,
                        c3,
                        alpha,
                    })
                } else {
                    // Defensive: no tag — fall back to RGB at zero.
                    CssColor::Function(CssColorFunction {
                        kind: CssColorType::Rgb,
                        c1,
                        c2,
                        c3,
                        alpha,
                    })
                };
                self.deposit_value(CssTypedValue::Color(color));
            }
            OpenFrame::ColorMix {
                mix_space,
                hue_method,
                left,
                left_pct,
                right,
                right_pct,
            } => {
                let mix_space = mix_space
                    .and_then(CssColorSpace::from_discriminant)
                    .unwrap_or(CssColorSpace::Srgb);
                let hue_method = hue_method.and_then(CssHueMethod::from_discriminant);
                // Defensive: if a nested colour reference is missing,
                // synthesise a transparent black for round-trip
                // continuity.
                let fallback = self
                    .arena
                    .push_color(CssColor::Hex(0x00000000));
                let left = left.unwrap_or(fallback);
                let right = right.unwrap_or(fallback);
                let mix = CssColorMix {
                    mix_space,
                    hue_method,
                    left,
                    left_pct,
                    right,
                    right_pct,
                };
                self.deposit_value(CssTypedValue::Color(CssColor::Mix(mix)));
            }
            OpenFrame::Function { name, args } => {
                let id = self.arena.push_values(args);
                // Route by name into the typed function family.
                let func = match name {
                    "calcFunction" => CssFunction::Calc { args: id },
                    "minFunction" => CssFunction::Min { args: id },
                    "maxFunction" => CssFunction::Max { args: id },
                    "clampFunction" => CssFunction::Clamp { args: id },
                    "varFunction" => CssFunction::Var {
                        name: "",
                        fallback: id,
                    },
                    "envFunction" => CssFunction::Env {
                        name: "",
                        fallback: id,
                    },
                    "urlFunction" | "url" => CssFunction::Url { raw: "" },
                    n if n.contains("gradient") => CssFunction::Gradient { name, args: id },
                    _ => CssFunction::Generic { name, args: id },
                };
                self.deposit_value(CssTypedValue::Function(func));
            }
        }
    }

    #[inline]
    fn push_leaf_with_f64(&mut self, value: f64) {
        // f64 leaves land on the topmost numeric frame as the
        // magnitude, on the colour function as a component, on a
        // declaration's value list as a bare number, or on the
        // pending value slot.
        match self.stack.last_mut() {
            Some(OpenFrame::Numeric { magnitude, .. }) => {
                *magnitude = Some(value);
            }
            Some(OpenFrame::ColorFunction { components, .. }) => {
                components.push(value);
            }
            Some(OpenFrame::ColorMix {
                left_pct,
                right_pct,
                left,
                right,
                ..
            }) => {
                if left.is_some() && left_pct.is_none() {
                    *left_pct = Some(value);
                } else if right.is_some() && right_pct.is_none() {
                    *right_pct = Some(value);
                }
            }
            _ => self.deposit_value(CssTypedValue::Number(value)),
        }
    }

    #[inline]
    fn push_leaf_with_i64(&mut self, value: i64) {
        self.deposit_value(CssTypedValue::Integer(value));
    }

    #[inline]
    fn push_leaf_with_u64(&mut self, value: u64) {
        // u64 leaves include the packed u32 colour value (hex /
        // namedColor) projected through `parse_hex_color`. The
        // grammar's `-> u32` annotation projects through the
        // push_leaf_with_u64 trait method (codegen widening); decode
        // back to u32 here for the typed colour variant.
        if value <= u32::MAX as u64 {
            // Land as a packed colour scalar; the enclosing typed-
            // value alternation closes through Wrap to the
            // declaration's value list.
            self.deposit_value(CssTypedValue::Color(CssColor::Hex(value as u32)));
        } else {
            self.deposit_value(CssTypedValue::Number(value as f64));
        }
    }

    #[inline]
    fn push_leaf_with_bool(&mut self, _value: bool) {
        // CSS L4 has no bool projection in the grammar; the trait
        // method is a no-op for this builder. Per
        // `feedback_typed-materialization-invariant`, every `->`
        // annotation reaches a method, but the inverse is not
        // required: a method without a corresponding `->` is admitted.
    }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        // SAFETY: the slice's lifetime is bound to the parse call site
        // by the generated function's signature; the trait surface
        // elides this so concrete builders can specialise.
        let lifetime_extended: &'p str = unsafe { core::mem::transmute(value) };
        match self.stack.last_mut() {
            Some(OpenFrame::Declaration { property, .. }) if property.is_none() => {
                *property = Some(lifetime_extended);
            }
            Some(OpenFrame::Function { name, .. }) if name.is_empty() => {
                *name = lifetime_extended;
            }
            Some(OpenFrame::KeyframesRule { name, .. }) if name.is_empty() => {
                *name = lifetime_extended;
            }
            Some(OpenFrame::KeyframeBlock { selector, .. }) if selector.is_empty() => {
                *selector = lifetime_extended;
            }
            Some(OpenFrame::MediaRule { query, .. }) if query.is_empty() => {
                *query = lifetime_extended;
            }
            Some(OpenFrame::SelectorList { selectors }) => {
                selectors.push(Selector::Span(lifetime_extended));
            }
            _ => self.deposit_value(CssTypedValue::Span(lifetime_extended)),
        }
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        // CSS L4 has no unit (`-> 0u8`-as-null) projection in the
        // grammar — every u8 projection carries semantic meaning.
        // No-op; per `feedback_typed-materialization-invariant`'s
        // converse the trait method admission stays open.
    }

    #[inline]
    fn push_branch_tag(&mut self, branch_index: u32) {
        // The branch tag captures a u8 discriminant from the grammar's
        // `-> Nu8` projections. Route by the topmost frame's slot:
        match self.stack.last_mut() {
            Some(OpenFrame::Numeric { unit, .. }) => {
                *unit = Some(branch_index as u8);
            }
            Some(OpenFrame::ColorFunction {
                kind_tag,
                space_tag,
                ..
            }) => {
                if kind_tag.is_none() && space_tag.is_none() {
                    *kind_tag = Some(branch_index as u8);
                }
            }
            Some(OpenFrame::ColorMix {
                mix_space,
                hue_method,
                ..
            }) => {
                if mix_space.is_none() {
                    *mix_space = Some(branch_index as u8);
                } else if hue_method.is_none() {
                    *hue_method = Some(branch_index as u8);
                }
            }
            _ => {
                // Global keyword / math operator branch tags land on
                // the pending value slot as a typed enum.
                if let Some(kw) = CssGlobalKeyword::from_discriminant(branch_index as u8) {
                    self.deposit_value(CssTypedValue::GlobalKeyword(kw));
                } else if let Some(_op) = CssMathOperator::from_discriminant(branch_index as u8) {
                    // Math operators land inside the function arg list
                    // through the value-list path; the discriminant
                    // itself is structural and doesn't materialise as a
                    // standalone value.
                }
            }
        }
    }
}

/// Lifetime-erase a static-lifetime string into the parse arena's `'p`.
///
/// The grammar's rule names live in the IR's interned string table for
/// the program's lifetime; the builder's frame variants borrow them as
/// `&'p str` for ergonomic comparison. Since `&'static str` outlives
/// every `'p`, the cast is sound.
#[inline]
fn leak_static_str<'p>(s: &str) -> &'p str {
    // Match the rule name against a known-static set so the leaked
    // pointer stays inside the program's static read-only segment.
    // Falls back to an empty static slice for unrecognised names.
    match s {
        "stylesheet" => "stylesheet",
        "ruleList" => "ruleList",
        "qualifiedRule" => "qualifiedRule",
        "ruleBlock" => "ruleBlock",
        "mediaRule" => "mediaRule",
        "keyframesRule" => "keyframesRule",
        "keyframeBlock" => "keyframeBlock",
        "genericAtRule" => "genericAtRule",
        "declaration" => "declaration",
        "customPropertyDecl" => "customPropertyDecl",
        "genericDecl" => "genericDecl",
        "selectorList" => "selectorList",
        "compoundSelector" => "compoundSelector",
        "complexSelector" => "complexSelector",
        "length" => "length",
        "angle" => "angle",
        "time" => "time",
        "frequency" => "frequency",
        "resolution" => "resolution",
        "flex" => "flex",
        "percentage" => "percentage",
        "unitless" => "unitless",
        "colorFunction" => "colorFunction",
        "colorFn" => "colorFn",
        "colorMix" => "colorMix",
        "calcFunction" => "calcFunction",
        "minFunction" => "minFunction",
        "maxFunction" => "maxFunction",
        "clampFunction" => "clampFunction",
        "varFunction" => "varFunction",
        "envFunction" => "envFunction",
        "urlFunction" => "urlFunction",
        "url" => "url",
        _ => "",
    }
}
