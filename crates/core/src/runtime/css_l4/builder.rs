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
    CssFunction, CssHueMethod, CssLength, CssLengthUnit, CssPercentage, CssResolution,
    CssResolutionUnit, CssRule, CssTime, CssTimeUnit, CssTypedValue, Declaration, GenericAtRule,
    KeyframeBlock, KeyframesRule, MediaRule, Selector, StyleRule, StyleSheet,
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
    ///
    /// `kind` is the registry-projected discriminator selected from
    /// the layout's `rule_id` at `begin_compound` time, eliminating
    /// rule-name string matches from the runtime hot path.
    Numeric {
        kind: NumericKind,
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
    /// Function call — registry-projected kind + parsed identifier
    /// span (filled lazily by `push_leaf_with_str` for the
    /// `genericFunction` family) + argument value list.
    Function {
        kind: FunctionKind,
        name: &'p str,
        args: Vec<CssTypedValue<'p>>,
    },
    /// `hex = "#" , /[0-9a-fA-F]{3,8}/ -> parse_hex_color(input) : u32`.
    /// The span between `#` and the closing trivia decodes to a packed
    /// 0xRRGGBBAA u32 via `crate::css_types::parse_hex_color`. The
    /// emitter currently lands the matched span via
    /// `push_leaf_with_str`; this frame catches the span on
    /// `end_compound` and produces the typed [`CssColor::Hex`] payload.
    HexColor { hex_span: Option<&'p str> },
    /// `dirPseudo = ":dir" , "(" >> dirKeyword << ")"` — captures the
    /// inner `dirKeyword`'s u8 discriminant (0 = ltr, 1 = rtl) so
    /// `end_compound` can deposit the matching
    /// `Selector::PseudoClass(":dir(<kind>)")` on the enclosing
    /// SelectorList. Without this dedicated frame the inner
    /// `push_branch_tag` from `parse_keyword_dirKeyword` would fall
    /// through the catch-all and never reach the typed selector graph.
    DirPseudo { kind_tag: Option<u8> },
}

/// Numeric-rule discriminator projected from `StructLayout::rule_id`.
///
/// One variant per typed-numeric CSS L4 rule the builder routes into
/// the matching [`CssDimension`] variant at `end_compound` time. The
/// projection is total — every rule_id mapping to a Numeric arm
/// resolves to a known kind; unrecognised rule ids stay outside this
/// alphabet (the `(layout.kind, layout.rule_id)` builder dispatch
/// chooses a different `OpenFrame` variant).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum NumericKind {
    Length,
    Angle,
    Time,
    Frequency,
    Resolution,
    Flex,
    Percentage,
}

/// Function-rule discriminator projected from `StructLayout::rule_id`.
///
/// One variant per typed-function CSS L4 rule the builder routes
/// through `OpenFrame::Function`. `Generic` covers `genericFunction`
/// (the open-ended /[a-zA-Z][\w-]*/-prefixed rule) where the actual
/// identifier flows through `push_leaf_with_str` into the frame's
/// `name` slot. `Url` covers `urlFunction`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum FunctionKind {
    Calc,
    Min,
    Max,
    Clamp,
    Var,
    Env,
    Url,
    Generic,
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
                OpenFrame::KeyframeBlock { declarations, .. } => declarations.push(decl),
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
        // Rule-id literals match the CSS L4 grammar's allocation in
        // `crates/core/src/grammar/generated/css_l4.rs`. Each arm
        // routes a layout to a typed `OpenFrame`; unrecognised ids
        // collapse to the transparent Wrap shape. The projection is
        // a registry-projected discriminator, not a rule-name string
        // match (Fermat F2 / F7 redress).
        let frame = match layout.rule_id {
            // Aggregate top-level rules.
            // 148 = ruleList (the structural body of `stylesheet`).
            148 => OpenFrame::StyleSheet { rules: Vec::new() },
            // 3 = hex — colour-specific frame: captures the digit
            // span; `end_compound` decodes via `parse_hex_color`.
            // 2 = namedColor stays a Wrap forwarder — the per-branch
            // `push_leaf_with_u64(packed)` lands the `CssColor::Hex`
            // payload directly in `push_leaf_with_u64`.
            3 => OpenFrame::HexColor { hex_span: None },
            // 143 = qualifiedRule. The qualifiedRule is the
            // declaration host; its StyleRule frame collects both
            // selectors (from the inner selectorList) and declarations
            // (from the inner ruleBlock).
            143 => OpenFrame::StyleRule {
                selectors: Vec::new(),
                declarations: Vec::new(),
                span: "",
            },
            // 142 = ruleBlock — `"{" >> blockContent ?w << "}"`. This
            // is a structural body container shared by qualifiedRule,
            // mediaRule, and atRule; it does NOT itself produce a
            // StyleRule. Declarations parsed inside ruleBlock land on
            // the enclosing host (StyleRule / KeyframeBlock) via
            // `deposit_declaration`'s stack walk; selectors reach the
            // host via the surrounding qualifiedRule's StyleRule
            // frame. Pre-AZ-IV.W1-CLOSE.B both 142 and 143 opened
            // StyleRule frames, causing each `qualifiedRule` to
            // surface as TWO StyleRule entries (one with selectors but
            // no declarations from qualifiedRule's end_compound, one
            // with declarations but no selectors from ruleBlock's
            // end_compound). The Wrap forwarder restores 1:1 parity.
            142 => OpenFrame::Wrap { value: None },
            // 144 = mediaRule.
            144 => OpenFrame::MediaRule {
                query: "",
                rules: Vec::new(),
            },
            // 139 = keyframesRule.
            139 => OpenFrame::KeyframesRule {
                name: "",
                blocks: Vec::new(),
            },
            // 141 = genericAtRule.
            141 => OpenFrame::GenericAtRule {
                name: "",
                prelude: "",
                body: "",
            },
            // Declaration family — every typed `*Decl` rule plus
            // `declaration` / `customPropertyDecl` / `genericDecl`.
            // 62 = customPropertyDecl, 63 = genericDecl, 138 =
            // declaration; 112..=137 are the 26 typed `*Decl` rules
            // (colorDecl ... cursorDecl) in declaration order. The
            // `__*Decl_cont_*` continuation rules are structural-only
            // and route through Wrap.
            62 | 63 | 112..=137 | 138 => OpenFrame::Declaration {
                property: None,
                values: Vec::new(),
                important: false,
            },
            // 96 = selectorList, 99 = complexSelector, 101 =
            // compoundSelector.
            96 | 99 | 101 => OpenFrame::SelectorList {
                selectors: Vec::new(),
            },
            // Numeric typed rules — length / angle / time / etc.
            79 => OpenFrame::Numeric {
                kind: NumericKind::Length,
                magnitude: None,
                unit: None,
            },
            57 => OpenFrame::Numeric {
                kind: NumericKind::Angle,
                magnitude: None,
                unit: None,
            },
            58 => OpenFrame::Numeric {
                kind: NumericKind::Time,
                magnitude: None,
                unit: None,
            },
            59 => OpenFrame::Numeric {
                kind: NumericKind::Frequency,
                magnitude: None,
                unit: None,
            },
            60 => OpenFrame::Numeric {
                kind: NumericKind::Resolution,
                magnitude: None,
                unit: None,
            },
            61 => OpenFrame::Numeric {
                kind: NumericKind::Flex,
                magnitude: None,
                unit: None,
            },
            53 => OpenFrame::Numeric {
                kind: NumericKind::Percentage,
                magnitude: None,
                unit: None,
            },
            // Color function family. 86 = colorFn (the only typed
            // colour-function rule in the current grammar; the
            // `colorFunction` and `colorMix` allowlist arms
            // referenced rules absent from the IR and routed dead).
            86 => OpenFrame::ColorFunction {
                kind_tag: None,
                space_tag: None,
                components: Vec::new(),
            },
            // Function family — calc / min / max / clamp / var / env /
            // url / generic. The discriminator is rule-id keyed so the
            // OpenFrame finalisation knows which `CssFunction` variant
            // to emit. The `name` slot stays empty until
            // `push_leaf_with_str` lands the parsed identifier
            // (genericFunction's regex match flows through there).
            105 => OpenFrame::Function {
                kind: FunctionKind::Calc,
                name: "",
                args: Vec::new(),
            },
            106 => OpenFrame::Function {
                kind: FunctionKind::Min,
                name: "",
                args: Vec::new(),
            },
            107 => OpenFrame::Function {
                kind: FunctionKind::Max,
                name: "",
                args: Vec::new(),
            },
            108 => OpenFrame::Function {
                kind: FunctionKind::Clamp,
                name: "",
                args: Vec::new(),
            },
            77 => OpenFrame::Function {
                kind: FunctionKind::Var,
                name: "",
                args: Vec::new(),
            },
            78 => OpenFrame::Function {
                kind: FunctionKind::Env,
                name: "",
                args: Vec::new(),
            },
            55 => OpenFrame::Function {
                kind: FunctionKind::Url,
                name: "",
                args: Vec::new(),
            },
            73 => OpenFrame::Function {
                kind: FunctionKind::Generic,
                name: "",
                args: Vec::new(),
            },
            // 71 = dirPseudo — `:dir(ltr|rtl)`. The inner dirKeyword's
            // u8 branch tag (0 = ltr, 1 = rtl) lands here via
            // `push_branch_tag`; `end_compound` deposits the matching
            // `Selector::PseudoClass` text on the enclosing
            // SelectorList (AZ-IV.W1-CLOSE.B).
            71 => OpenFrame::DirPseudo { kind_tag: None },
            // Catch-all: transparent wrap shape. The `LayoutKind`
            // co-discriminator stays available for future structural
            // dispatch but the Wrap fallback is uniform.
            _ => {
                let _ = layout.kind;
                OpenFrame::Wrap { value: None }
            }
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
                if let Some(OpenFrame::KeyframesRule { blocks, .. }) = self.stack.last_mut() {
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
                // AZ-IV.W1-CLOSE.B — selectorList / complexSelector /
                // compoundSelector all open OpenFrame::SelectorList.
                // The parent of a compoundSelector is a complexSelector
                // (also SelectorList); the parent of a complexSelector
                // is the outer selectorList (also SelectorList); only
                // the OUTERMOST selectorList finds StyleRule directly.
                // Extend along the chain so deeply nested selectors
                // (`:dir(ltr)` opened inside a compoundSelector) reach
                // the StyleRule's selector list.
                match self.stack.last_mut() {
                    Some(OpenFrame::StyleRule { selectors: dst, .. }) => {
                        dst.extend(selectors);
                    }
                    Some(OpenFrame::SelectorList { selectors: dst }) => {
                        dst.extend(selectors);
                    }
                    _ => {}
                }
            }
            OpenFrame::Wrap { value } => {
                if let Some(v) = value {
                    self.deposit_value(v);
                }
            }
            OpenFrame::Numeric {
                kind,
                magnitude,
                unit,
            } => {
                let value = magnitude.unwrap_or(0.0);
                let dim = match (kind, unit) {
                    (NumericKind::Length, Some(u)) => CssDimension::Length(CssLength {
                        value,
                        unit: CssLengthUnit::from_discriminant(u),
                    }),
                    (NumericKind::Angle, Some(u)) => CssDimension::Angle(CssAngle {
                        value,
                        unit: CssAngleUnit::from_discriminant(u).unwrap_or(CssAngleUnit::Deg),
                    }),
                    (NumericKind::Time, Some(u)) => CssDimension::Time(CssTime {
                        value,
                        unit: CssTimeUnit::from_discriminant(u).unwrap_or(CssTimeUnit::S),
                    }),
                    (NumericKind::Frequency, Some(u)) => CssDimension::Frequency(CssFrequency {
                        value,
                        unit: CssFrequencyUnit::from_discriminant(u)
                            .unwrap_or(CssFrequencyUnit::Hz),
                    }),
                    (NumericKind::Resolution, Some(u)) => CssDimension::Resolution(CssResolution {
                        value,
                        unit: CssResolutionUnit::from_discriminant(u)
                            .unwrap_or(CssResolutionUnit::Dppx),
                    }),
                    (NumericKind::Flex, _) => CssDimension::Flex(CssFlex { value }),
                    (NumericKind::Percentage, _) => {
                        CssDimension::Percentage(CssPercentage { value })
                    }
                    // Length / Angle / Time / Frequency / Resolution
                    // without a parsed unit fall through to unitless.
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
                    let space =
                        CssColorSpace::from_discriminant(space).unwrap_or(CssColorSpace::Srgb);
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
                let fallback = self.arena.push_color(CssColor::Hex(0x00000000));
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
            OpenFrame::Function { kind, name, args } => {
                let id = self.arena.push_values(args);
                // Route by registry-projected kind into the typed
                // function family. The `name` slot carries the
                // parsed function identifier for the Generic family;
                // typed functions ignore it.
                let func = match kind {
                    FunctionKind::Calc => CssFunction::Calc { args: id },
                    FunctionKind::Min => CssFunction::Min { args: id },
                    FunctionKind::Max => CssFunction::Max { args: id },
                    FunctionKind::Clamp => CssFunction::Clamp { args: id },
                    FunctionKind::Var => CssFunction::Var {
                        name: "",
                        fallback: id,
                    },
                    FunctionKind::Env => CssFunction::Env {
                        name: "",
                        fallback: id,
                    },
                    FunctionKind::Url => CssFunction::Url { raw: "" },
                    FunctionKind::Generic => CssFunction::Generic { name, args: id },
                };
                self.deposit_value(CssTypedValue::Function(func));
            }
            OpenFrame::DirPseudo { kind_tag } => {
                // AZ-IV.W1-CLOSE.B — deposit the matching
                // `:dir(ltr)` / `:dir(rtl)` Selector::PseudoClass on
                // the enclosing SelectorList. The dirKeyword grammar
                // declares `"ltr" -> 0u8 | "rtl" -> 1u8`; map back to
                // the canonical text form for selector-list parity.
                let text: &'p str = match kind_tag {
                    Some(1) => ":dir(rtl)",
                    Some(0) => ":dir(ltr)",
                    // Defensive: no tag — emit a neutral Span so the
                    // selector list still records the structural reach
                    // without fabricating a discriminant.
                    _ => ":dir()",
                };
                if let Some(OpenFrame::SelectorList { selectors }) = self.stack.last_mut() {
                    selectors.push(Selector::PseudoClass(text));
                } else if let Some(OpenFrame::StyleRule { selectors, .. }) = self.stack.last_mut() {
                    selectors.push(Selector::PseudoClass(text));
                } else {
                    self.deposit_value(CssTypedValue::Span(text));
                }
            }
            OpenFrame::HexColor { hex_span } => {
                // Decode the captured hex digit span via the host
                // `parse_hex_color` shim — the codegen's typed `-> u32`
                // host-fn projection currently lands the raw matched
                // span at this frame; the runtime materialises the
                // packed 0xRRGGBBAA u32 here.
                //
                // The flat-shape codegen captures the entire compound
                // body span (`#` + digits), so strip the leading `#`
                // before passing the digit slice to `parse_hex_color`.
                let packed = hex_span
                    .map(|s| {
                        let digits = s.strip_prefix('#').unwrap_or(s);
                        crate::css_types::parse_hex_color(digits)
                    })
                    .unwrap_or(0);
                self.deposit_value(CssTypedValue::Color(CssColor::Hex(packed)));
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
            // hex captures the matched digit span; the host
            // `parse_hex_color` shim decodes on `end_compound`.
            Some(OpenFrame::HexColor { hex_span }) if hex_span.is_none() => {
                *hex_span = Some(lifetime_extended);
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
            // AZ-IV.W1-CLOSE.B — dirPseudo's inner dirKeyword tag.
            Some(OpenFrame::DirPseudo { kind_tag }) => {
                if kind_tag.is_none() {
                    *kind_tag = Some(branch_index as u8);
                }
            }
            _ => {
                // AZ-IV.W1-CLOSE.B — `push_branch_tag` from a catch-all
                // Wrap-frame fall-through is structural: the tag is the
                // discriminator of the rule whose own `push_leaf_with_*`
                // call already deposited the typed payload on the
                // enclosing slot. The previous trial-cast through
                // `CssGlobalKeyword::from_discriminant` /
                // `CssMathOperator::from_discriminant` mis-routed every
                // Wrap-frame branch tag (namedColor's outer prefix-group
                // index 0..=19, dirPseudo's 0/1) into a fabricated
                // `GlobalKeyword` value, overwriting the typed payload
                // that `push_leaf_with_u64` / `push_leaf_with_str` had
                // already deposited.
                //
                // GlobalKeyword and MathOperator must reach the typed
                // graph from their owning rules' frames (via the
                // `push_leaf_with_*` path on a dedicated frame), not
                // from a catch-all trial-cast. Until those frames
                // exist, the catch-all is a deliberate no-op — the
                // structural tag is consumed for shape recognition
                // but the typed payload wins on the value slot.
            }
        }
    }
}
