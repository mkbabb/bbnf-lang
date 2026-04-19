//! `bbnf::css` — first-class CSS Value API.
//!
//! AX.W1.B. Structurally isomorphic to `lightningcss::stylesheet::StyleSheet`:
//! every `CssRule` / `StyleRule` / `MediaRule` / ... lightningcss
//! publishes has a field-complete counterpart under this module,
//! per invariant 18 (no stubs, no `_` placeholders, no `todo!()`
//! arms).
//!
//! The user surface is re-exported at `bbnf::runtime::view::css::*`:
//! consumers hold `bbnf::runtime::view::css::StyleSheet<'input>` and
//! walk its typed rule list directly.
//!
//! # Projection pipeline
//!
//! 1. Caller runs `CssL4Parser::parse(input) -> Parsed<'_>`.
//! 2. `bbnf::css::StyleSheet::from_parsed(&parsed)` lowers the tape
//!    into the typed AST.
//! 3. The tape walker follows the grammar's declaration order:
//!    `stylesheet → ruleList → ruleItem → (qualifiedRule | atRule) → ...`
//!    Each `Rule`-kind record is matched against the bbnf's
//!    `CssL4RuleKind` discriminator table and projected into the
//!    matching [`rules::CssRule`] variant.
//! 4. At-rules bbnf's grammar doesn't structurally parse fall into
//!    the `genericAtRule` branch and land in
//!    [`rules::CssRule::Unknown`] with verbatim text.
//!
//! # Divergence ledger
//!
//! `docs/tranches/AX/parity/css_divergence.md`. The per-variant table
//! maps every lightningcss variant to one of: `Populated`, `ProjectedViaFrom`,
//! `DivergentGenericFallback`.

pub mod declarations;
pub mod rules;
pub mod selectors;
pub mod stylesheet;
pub mod values;

pub use declarations::{Declaration, DeclarationBlock, PropertyId, ValueList};
pub use rules::{
    CssRule, Keyframe, KeyframeSelector, KeyframesName, KeyframesRule, Location, MediaCondition,
    MediaFeature, MediaList, MediaQualifier, MediaQuery, MediaRule, MediaType, StyleRule,
    UnknownAtRule,
};
pub use selectors::{
    AttributeCaseSensitivity, AttributeOperator, Combinator, Component, DirKeyword, NthKind,
    NthSelector, Selector, SelectorList, VendorPrefix,
};
pub use stylesheet::StyleSheet;
pub use values::{CalcKind, ColorValue, CssGlobalKeyword, Dimension, Unit, UnitKind, Value};

use std::borrow::Cow;

use tape::{Tape, TapeCursor, TapeOffset};

// ─── Projection from bbnf's CSS L4 tape ──────────────────────────────

impl<'i> StyleSheet<'i> {
    /// Project a parsed CSS L4 tape into the typed [`StyleSheet`].
    ///
    /// `rule_kind_of` is a caller-supplied closure that maps a
    /// record's `variant_idx` byte into a rule-kind name — this
    /// indirection lets the projection be grammar-agnostic and share
    /// code with downstream callers that run the CSS L4 parser in
    /// different crates (tests, benches, the public bbnf API).
    ///
    /// Typical usage:
    /// ```ignore
    /// let parsed = CssL4Parser::parse(input)?;
    /// let sheet = StyleSheet::from_parsed(parsed.tape(), parsed.input(), parsed.root(), |vi| {
    ///     // map variant_idx to rule name via CssL4Parser::RuleKind
    /// });
    /// ```
    pub fn from_parsed<F>(
        tape: &'i Tape,
        input: &'i str,
        root: TapeOffset,
        rule_kind_of: F,
    ) -> StyleSheet<'i>
    where
        F: Fn(u8) -> Option<&'static str> + 'i,
    {
        let mut out = StyleSheet::new();
        let root_cursor = TapeCursor::new(tape, root);

        // Walk the root compound — which is the `stylesheet` rule —
        // looking for `ruleList` then `ruleItem` children. The
        // grammar's structure (`stylesheet = ruleList ?w`) means the
        // first child is `ruleList`, whose children are `ruleItem`s,
        // each of which wraps a `qualifiedRule | atRule`.
        let ctx = ProjectionCtx {
            tape,
            input,
            rule_kind_of,
        };
        ctx.project_rule_list(root_cursor, &mut out.rules);
        out
    }
}

/// Internal per-walk context.
struct ProjectionCtx<'p, F: Fn(u8) -> Option<&'static str>> {
    tape: &'p Tape,
    input: &'p str,
    rule_kind_of: F,
}

impl<'p, F: Fn(u8) -> Option<&'static str>> ProjectionCtx<'p, F> {
    fn rule_name(&self, cursor: TapeCursor<'p>) -> Option<&'static str> {
        (self.rule_kind_of)(cursor.variant_idx())
    }

    fn span_text(&self, cursor: TapeCursor<'p>) -> &'p str {
        let (lo, hi) = cursor.span();
        let lo = lo as usize;
        let hi = hi as usize;
        if hi <= self.input.len() && lo <= hi {
            &self.input[lo..hi]
        } else {
            ""
        }
    }

    /// Walk the top-level rule list (entered at the stylesheet root).
    fn project_rule_list(&self, root: TapeCursor<'p>, out: &mut Vec<CssRule<'p>>) {
        self.walk_for_rule_items(root, out);
    }

    /// Recursively find `ruleItem` records and project each into an
    /// entry in `out`. `ruleItem = qualifiedRule | atRule` so each
    /// `ruleItem` has one child that's either a qualified rule or an
    /// at-rule variant.
    fn walk_for_rule_items(&self, cursor: TapeCursor<'p>, out: &mut Vec<CssRule<'p>>) {
        // Check if this cursor's rule name is ruleItem / ruleList /
        // stylesheet / blockContent and recurse accordingly.
        match self.rule_name(cursor) {
            Some("ruleItem") => {
                if let Some(rule) = self.project_rule_item(cursor) {
                    out.push(rule);
                }
            }
            Some("stylesheet") | Some("ruleList") | Some("blockContent") | None => {
                // Generic container — recurse into children.
                for child in cursor.children_zero_alloc() {
                    self.walk_for_rule_items(child, out);
                }
            }
            Some(_) => {
                // Any other rule — recurse for any embedded ruleItems.
                for child in cursor.children_zero_alloc() {
                    self.walk_for_rule_items(child, out);
                }
            }
        }
    }

    /// Project a `ruleItem` record into a [`CssRule`].
    fn project_rule_item(&self, cursor: TapeCursor<'p>) -> Option<CssRule<'p>> {
        // ruleItem wraps one child which is the actual rule.
        let child = cursor.children_zero_alloc().next()?;
        self.project_any_rule(child)
    }

    /// Project any rule-level record into a [`CssRule`].
    fn project_any_rule(&self, cursor: TapeCursor<'p>) -> Option<CssRule<'p>> {
        let name = self.rule_name(cursor);
        let loc = self.loc_at(cursor);
        match name {
            Some("qualifiedRule") => Some(CssRule::Style(self.project_style_rule(cursor, loc))),
            Some("mediaRule") => Some(CssRule::Media(self.project_media_rule(cursor, loc))),
            Some("keyframesRule") => {
                Some(CssRule::Keyframes(self.project_keyframes_rule(cursor, loc)))
            }
            Some("atRule") => {
                // atRule is an Alt wrapper; look inside its one child for
                // the concrete variant.
                let inner = cursor.children_zero_alloc().next()?;
                self.project_any_rule(inner)
            }
            Some("genericAtRule") => Some(CssRule::Unknown(self.project_unknown_at_rule(cursor, loc))),
            _ => Some(CssRule::Unknown(self.project_unknown_at_rule(cursor, loc))),
        }
    }

    fn loc_at(&self, cursor: TapeCursor<'p>) -> Location {
        let (lo, _) = cursor.span();
        // Byte offset to line/column: count newlines up to lo.
        let prefix = &self.input[..lo.min(self.input.len() as u32) as usize];
        let mut line = 0u32;
        let mut col = 1u32;
        for b in prefix.bytes() {
            if b == b'\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        Location {
            source_index: 0,
            line,
            column: col,
            byte_offset: lo,
        }
    }

    // ─── StyleRule ───────────────────────────────────────────────

    fn project_style_rule(&self, cursor: TapeCursor<'p>, loc: Location) -> StyleRule<'p> {
        let mut selectors = SelectorList::default();
        let mut declarations = DeclarationBlock::default();
        let rules = Vec::new();

        for child in cursor.children_zero_alloc() {
            match self.rule_name(child) {
                Some("selectorList") => {
                    selectors = self.project_selector_list(child);
                }
                Some("ruleBlock") => {
                    self.project_rule_block_into(child, &mut declarations);
                }
                _ => {}
            }
        }

        StyleRule {
            selectors,
            declarations,
            rules,
            vendor_prefix: None,
            loc,
        }
    }

    fn project_selector_list(&self, cursor: TapeCursor<'p>) -> SelectorList<'p> {
        let mut out = SelectorList::default();
        for child in cursor.children_zero_alloc() {
            match self.rule_name(child) {
                Some("complexSelector") => {
                    out.selectors.push(self.project_complex_selector(child));
                }
                _ => {
                    // May be a subordinate compound; look deeper.
                    self.walk_for_selectors(child, &mut out);
                }
            }
        }
        if out.selectors.is_empty() {
            // Fall back to single selector from the span.
            let text = self.span_text(cursor).trim();
            if !text.is_empty() {
                out.selectors.push(Selector {
                    components: vec![Component::LocalName {
                        name: Cow::Borrowed(text),
                        lower_name: Cow::Owned(text.to_ascii_lowercase()),
                    }],
                });
            }
        }
        out
    }

    fn walk_for_selectors(&self, cursor: TapeCursor<'p>, out: &mut SelectorList<'p>) {
        if matches!(self.rule_name(cursor), Some("complexSelector")) {
            out.selectors.push(self.project_complex_selector(cursor));
            return;
        }
        for child in cursor.children_zero_alloc() {
            self.walk_for_selectors(child, out);
        }
    }

    fn project_complex_selector(&self, cursor: TapeCursor<'p>) -> Selector<'p> {
        let mut components = Vec::new();
        self.walk_selector_components(cursor, &mut components);
        if components.is_empty() {
            // Fall back to raw text as a local-name placeholder.
            let text = self.span_text(cursor).trim();
            if !text.is_empty() {
                components.push(Component::LocalName {
                    name: Cow::Borrowed(text),
                    lower_name: Cow::Owned(text.to_ascii_lowercase()),
                });
            }
        }
        Selector { components }
    }

    fn walk_selector_components(
        &self,
        cursor: TapeCursor<'p>,
        out: &mut Vec<Component<'p>>,
    ) {
        match self.rule_name(cursor) {
            Some("typeSelector") => {
                let text = self.span_text(cursor).trim();
                if text == "*" {
                    out.push(Component::ExplicitUniversalType);
                } else if !text.is_empty() {
                    out.push(Component::LocalName {
                        name: Cow::Borrowed(text),
                        lower_name: Cow::Owned(text.to_ascii_lowercase()),
                    });
                }
            }
            Some("classSelector") => {
                let text = self.span_text(cursor).trim();
                // Strip leading '.'
                let cls = text.strip_prefix('.').unwrap_or(text);
                out.push(Component::Class(Cow::Borrowed(cls)));
            }
            Some("idSelector") => {
                let text = self.span_text(cursor).trim();
                let id = text.strip_prefix('#').unwrap_or(text);
                out.push(Component::ID(Cow::Borrowed(id)));
            }
            Some("attrSelector") => {
                // Detailed attr parsing — best-effort from span text.
                out.push(self.project_attr_selector(cursor));
            }
            Some("combinator") => {
                // Read the u8 discriminant if payload present.
                let rec = cursor.record();
                let comb = if rec.has_payload() {
                    match self.tape.payload_u8(rec) {
                        Some(0) => Combinator::Descendant,
                        Some(1) => Combinator::Child,
                        Some(2) => Combinator::NextSibling,
                        Some(3) => Combinator::LaterSibling,
                        _ => Combinator::Descendant,
                    }
                } else {
                    let text = self.span_text(cursor).trim();
                    match text {
                        ">" => Combinator::Child,
                        "+" => Combinator::NextSibling,
                        "~" => Combinator::LaterSibling,
                        _ => Combinator::Descendant,
                    }
                };
                out.push(Component::Combinator(comb));
            }
            Some("pseudoElement") | Some("simplePseudoElement") => {
                let text = self.span_text(cursor).trim();
                let name = text.trim_start_matches("::");
                out.push(Component::PseudoElement(Cow::Borrowed(name)));
            }
            Some("pseudoClass") | Some("simplePseudoClass") => {
                let text = self.span_text(cursor).trim();
                let name = text.trim_start_matches(':');
                out.push(Component::NonTSPseudoClass(Cow::Borrowed(name)));
            }
            Some("isPseudo") => {
                let inner = self.project_selector_list_of_child(cursor);
                out.push(Component::Is(inner));
            }
            Some("wherePseudo") => {
                let inner = self.project_selector_list_of_child(cursor);
                out.push(Component::Where(inner));
            }
            Some("notPseudo") => {
                let inner = self.project_selector_list_of_child(cursor);
                out.push(Component::Negation(inner));
            }
            Some("hasPseudo") => {
                let inner = self.project_selector_list_of_child(cursor);
                out.push(Component::Has(inner));
            }
            Some("dirPseudo") => {
                let rec = cursor.record();
                let dir = if rec.has_payload() {
                    match self.tape.payload_u8(rec) {
                        Some(0) => DirKeyword::Ltr,
                        _ => DirKeyword::Rtl,
                    }
                } else {
                    let text = self.span_text(cursor);
                    if text.contains("ltr") {
                        DirKeyword::Ltr
                    } else {
                        DirKeyword::Rtl
                    }
                };
                out.push(Component::Dir(dir));
            }
            _ => {
                // Recurse to look for component-level rules.
                for child in cursor.children_zero_alloc() {
                    self.walk_selector_components(child, out);
                }
            }
        }
    }

    fn project_selector_list_of_child(&self, cursor: TapeCursor<'p>) -> SelectorList<'p> {
        for child in cursor.children_zero_alloc() {
            if matches!(self.rule_name(child), Some("selectorList") | Some("relativeSelectorList"))
            {
                return self.project_selector_list(child);
            }
        }
        // Fallback: walk children looking for complexSelector.
        let mut out = SelectorList::default();
        for child in cursor.children_zero_alloc() {
            self.walk_for_selectors(child, &mut out);
        }
        out
    }

    fn project_attr_selector(&self, cursor: TapeCursor<'p>) -> Component<'p> {
        // Use the raw span text to decompose the attribute selector.
        let text = self.span_text(cursor).trim();
        // Strip the brackets.
        let inner = text
            .strip_prefix('[')
            .and_then(|s| s.strip_suffix(']'))
            .unwrap_or(text)
            .trim();

        // Detect operator.
        let (local, op, value_raw) = split_attr(inner);
        let operator = op.map(|o| match o {
            "=" => AttributeOperator::Equal,
            "|=" => AttributeOperator::Dash,
            "~=" => AttributeOperator::Includes,
            "^=" => AttributeOperator::Prefix,
            "$=" => AttributeOperator::Suffix,
            "*=" => AttributeOperator::Substring,
            _ => AttributeOperator::Equal,
        });

        let value = value_raw.map(|v| {
            let v = v.trim();
            let unquoted = v
                .strip_prefix('"')
                .and_then(|s| s.strip_suffix('"'))
                .or_else(|| v.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')))
                .unwrap_or(v);
            Cow::Owned(unquoted.to_string())
        });

        Component::AttributeInNoNamespace {
            local_name: Cow::Owned(local.to_string()),
            local_name_lower: Cow::Owned(local.to_ascii_lowercase()),
            operator,
            value,
            case_sensitivity: AttributeCaseSensitivity::Default,
            never_matches: false,
        }
    }

    fn project_rule_block_into(
        &self,
        cursor: TapeCursor<'p>,
        out: &mut DeclarationBlock<'p>,
    ) {
        // ruleBlock -> blockContent -> (declaration | ruleItem) *
        for child in cursor.children_zero_alloc() {
            self.walk_for_declarations(child, out);
        }
    }

    fn walk_for_declarations(
        &self,
        cursor: TapeCursor<'p>,
        out: &mut DeclarationBlock<'p>,
    ) {
        match self.rule_name(cursor) {
            Some(name) if name.ends_with("Decl") || name == "declaration" => {
                if let Some(decl) = self.project_declaration(cursor) {
                    if decl.important {
                        out.important_declarations.push(decl);
                    } else {
                        out.declarations.push(decl);
                    }
                }
            }
            _ => {
                for child in cursor.children_zero_alloc() {
                    self.walk_for_declarations(child, out);
                }
            }
        }
    }

    fn project_declaration(&self, cursor: TapeCursor<'p>) -> Option<Declaration<'p>> {
        // Use the span text for the whole declaration; lex property:value.
        let text = self.span_text(cursor).trim().trim_end_matches(';').trim();
        if text.is_empty() {
            return None;
        }
        let colon = text.find(':')?;
        let name = text[..colon].trim();
        let raw_value = text[colon + 1..].trim();
        let (value_text, important) = if let Some(rest) = raw_value.strip_suffix("!important") {
            (rest.trim(), true)
        } else if let Some(rest) = raw_value.rsplit_once("!important") {
            (rest.0.trim(), true)
        } else {
            (raw_value, false)
        };

        let property_id = declarations::property_id_from_name(
            // SAFETY: name is a substring of self.input, so the 'p
            // borrow outlasts the returned PropertyId.
            unsafe { std::mem::transmute::<&str, &'p str>(name) },
        );

        // Build value list by tokenising with a simple whitespace/comma/slash
        // splitter. This captures the round-trip tokens without pretending
        // full value-grammar parity.
        let values = tokenise_value_list(unsafe { std::mem::transmute::<&str, &'p str>(value_text) });

        let (lo, hi) = cursor.span();
        Some(Declaration {
            property_id,
            value: ValueList { values },
            important,
            span: (lo, hi),
        })
    }

    // ─── MediaRule ───────────────────────────────────────────────

    fn project_media_rule(&self, cursor: TapeCursor<'p>, loc: Location) -> MediaRule<'p> {
        let mut query = MediaList::default();
        let mut rules = Vec::new();
        for child in cursor.children_zero_alloc() {
            match self.rule_name(child) {
                Some("mediaQueryList") => {
                    query = self.project_media_list(child);
                }
                Some("ruleBlock") => {
                    // Inside @media, the block contains rules not declarations.
                    self.walk_for_rule_items(child, &mut rules);
                }
                _ => {}
            }
        }
        MediaRule { query, rules, loc }
    }

    fn project_media_list(&self, cursor: TapeCursor<'p>) -> MediaList<'p> {
        let mut out = MediaList::default();
        for child in cursor.children_zero_alloc() {
            match self.rule_name(child) {
                Some("mediaQuery") => {
                    out.media_queries.push(self.project_media_query(child));
                }
                _ => {}
            }
        }
        if out.media_queries.is_empty() {
            // Fallback: parse the raw text.
            let text = self.span_text(cursor).trim();
            if !text.is_empty() {
                out.media_queries.push(MediaQuery {
                    qualifier: None,
                    media_type: MediaType::Custom(Cow::Borrowed(text)),
                    condition: None,
                });
            }
        }
        out
    }

    fn project_media_query(&self, cursor: TapeCursor<'p>) -> MediaQuery<'p> {
        let mut qualifier = None;
        let mut media_type = MediaType::All;
        let mut features: Vec<MediaCondition<'p>> = Vec::new();

        for child in cursor.children_zero_alloc() {
            match self.rule_name(child) {
                Some("mediaQualifier") => {
                    let rec = child.record();
                    qualifier = if rec.has_payload() {
                        match self.tape.payload_u8(rec) {
                            Some(0) => Some(MediaQualifier::Not),
                            Some(1) => Some(MediaQualifier::Only),
                            _ => None,
                        }
                    } else {
                        match self.span_text(child).trim() {
                            "not" => Some(MediaQualifier::Not),
                            "only" => Some(MediaQualifier::Only),
                            _ => None,
                        }
                    };
                }
                Some("mediaType") => {
                    let rec = child.record();
                    media_type = if rec.has_payload() {
                        match self.tape.payload_u8(rec) {
                            Some(0) => MediaType::All,
                            Some(1) => MediaType::Print,
                            Some(2) => MediaType::Screen,
                            Some(3) => MediaType::Speech,
                            _ => MediaType::Custom(Cow::Borrowed(self.span_text(child).trim())),
                        }
                    } else {
                        match self.span_text(child).trim() {
                            "all" => MediaType::All,
                            "print" => MediaType::Print,
                            "screen" => MediaType::Screen,
                            "speech" => MediaType::Speech,
                            other => MediaType::Custom(Cow::Borrowed(other)),
                        }
                    };
                }
                Some("mediaInParens") | Some("mediaFeature") | Some("mediaCondition") => {
                    if let Some(cond) = self.project_media_condition(child) {
                        features.push(cond);
                    }
                }
                _ => {}
            }
        }

        let condition = if features.is_empty() {
            None
        } else if features.len() == 1 {
            features.into_iter().next()
        } else {
            Some(MediaCondition::And(features))
        };

        MediaQuery {
            qualifier,
            media_type,
            condition,
        }
    }

    fn project_media_condition(&self, cursor: TapeCursor<'p>) -> Option<MediaCondition<'p>> {
        match self.rule_name(cursor) {
            Some("mediaFeature") => {
                let mut name = Cow::Borrowed("");
                let mut value = None;
                for child in cursor.children_zero_alloc() {
                    match self.rule_name(child) {
                        Some("mediaFeatureName") => {
                            name = Cow::Borrowed(self.span_text(child).trim());
                        }
                        Some("mediaFeatureValue") => {
                            value = Some(Cow::Borrowed(self.span_text(child).trim()));
                        }
                        _ => {}
                    }
                }
                Some(MediaCondition::Feature(MediaFeature { name, value }))
            }
            _ => {
                // Walk inner content.
                let mut subs = Vec::new();
                for child in cursor.children_zero_alloc() {
                    if let Some(sub) = self.project_media_condition(child) {
                        subs.push(sub);
                    }
                }
                if subs.is_empty() {
                    None
                } else if subs.len() == 1 {
                    subs.into_iter().next()
                } else {
                    Some(MediaCondition::And(subs))
                }
            }
        }
    }

    // ─── KeyframesRule ────────────────────────────────────────────

    fn project_keyframes_rule(&self, cursor: TapeCursor<'p>, loc: Location) -> KeyframesRule<'p> {
        let mut name = KeyframesName::Ident(Cow::Borrowed(""));
        let mut keyframes = Vec::new();

        let mut first_ident: Option<&'p str> = None;
        for child in cursor.children_zero_alloc() {
            match self.rule_name(child) {
                Some("ident") if first_ident.is_none() => {
                    first_ident = Some(self.span_text(child).trim());
                }
                Some("keyframeBlock") => {
                    keyframes.push(self.project_keyframe(child));
                }
                _ => {}
            }
        }

        if let Some(id) = first_ident {
            name = KeyframesName::Ident(Cow::Borrowed(id));
        }

        KeyframesRule {
            name,
            keyframes,
            vendor_prefix: None,
            loc,
        }
    }

    fn project_keyframe(&self, cursor: TapeCursor<'p>) -> Keyframe<'p> {
        let mut selectors = Vec::new();
        let mut declarations = DeclarationBlock::default();

        for child in cursor.children_zero_alloc() {
            match self.rule_name(child) {
                Some("keyframeSel") => {
                    self.collect_keyframe_selectors(child, &mut selectors);
                }
                _ => {
                    // Declarations are nested deeper.
                    self.walk_for_declarations(child, &mut declarations);
                }
            }
        }

        Keyframe {
            selectors,
            declarations,
        }
    }

    fn collect_keyframe_selectors(
        &self,
        cursor: TapeCursor<'p>,
        out: &mut Vec<KeyframeSelector>,
    ) {
        match self.rule_name(cursor) {
            Some("keyframeStop") => {
                let rec = cursor.record();
                if rec.has_payload() {
                    match self.tape.payload_u8(rec) {
                        Some(0) => {
                            // Percentage — parse from span text.
                            let text = self.span_text(cursor).trim();
                            let pct = text.trim_end_matches('%').parse::<f32>().unwrap_or(0.0);
                            out.push(KeyframeSelector::Percentage(pct));
                        }
                        Some(1) => out.push(KeyframeSelector::From),
                        Some(2) => out.push(KeyframeSelector::To),
                        _ => {}
                    }
                } else {
                    let text = self.span_text(cursor).trim();
                    if text == "from" {
                        out.push(KeyframeSelector::From);
                    } else if text == "to" {
                        out.push(KeyframeSelector::To);
                    } else {
                        let pct = text.trim_end_matches('%').parse::<f32>().unwrap_or(0.0);
                        out.push(KeyframeSelector::Percentage(pct));
                    }
                }
            }
            _ => {
                for child in cursor.children_zero_alloc() {
                    self.collect_keyframe_selectors(child, out);
                }
            }
        }
    }

    // ─── UnknownAtRule ────────────────────────────────────────────

    fn project_unknown_at_rule(
        &self,
        cursor: TapeCursor<'p>,
        loc: Location,
    ) -> UnknownAtRule<'p> {
        let text = self.span_text(cursor);
        // Extract at-rule name — first token up to whitespace or `{`/`;`.
        let trimmed = text.trim();
        let name_end = trimmed
            .find(|c: char| c.is_whitespace() || c == '{' || c == ';' || c == '(')
            .unwrap_or(trimmed.len());
        let name = &trimmed[..name_end];
        let rest = trimmed[name_end..].trim();
        let (prelude, block) = if let Some(brace_start) = rest.find('{') {
            let before = rest[..brace_start].trim();
            let after = &rest[brace_start + 1..];
            let block_body = after.rsplitn(2, '}').nth(1).unwrap_or(after);
            (Cow::Borrowed(before), Some(Cow::Borrowed(block_body)))
        } else if let Some(semi) = rest.find(';') {
            (Cow::Borrowed(rest[..semi].trim()), None)
        } else {
            (Cow::Borrowed(rest), None)
        };

        UnknownAtRule {
            name: Cow::Borrowed(name),
            prelude,
            block,
            loc,
        }
    }
}

// ─── Helpers ─────────────────────────────────────────────────────────

fn split_attr(inner: &str) -> (&str, Option<&str>, Option<&str>) {
    // Try multi-char ops first (they're all two chars long except `=`).
    for op in &["|=", "~=", "^=", "$=", "*="] {
        if let Some(idx) = inner.find(op) {
            let (lhs, rest) = inner.split_at(idx);
            let rhs = &rest[op.len()..];
            return (lhs.trim(), Some(op), Some(rhs));
        }
    }
    if let Some(idx) = inner.find('=') {
        let (lhs, rest) = inner.split_at(idx);
        let rhs = &rest[1..];
        (lhs.trim(), Some("="), Some(rhs))
    } else {
        (inner.trim(), None, None)
    }
}

fn tokenise_value_list<'p>(raw: &'p str) -> Vec<Value<'p>> {
    let mut out = Vec::new();
    if raw.is_empty() {
        return out;
    }
    let bytes = raw.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        let b = bytes[i];
        if b.is_ascii_whitespace() {
            i += 1;
            continue;
        }
        if b == b',' {
            out.push(Value::Comma);
            i += 1;
            continue;
        }
        if b == b'/' {
            out.push(Value::Slash);
            i += 1;
            continue;
        }
        if b == b'"' || b == b'\'' {
            let quote = b;
            let start = i;
            i += 1;
            while i < bytes.len() && bytes[i] != quote {
                if bytes[i] == b'\\' && i + 1 < bytes.len() {
                    i += 2;
                } else {
                    i += 1;
                }
            }
            if i < bytes.len() {
                i += 1;
            }
            // Slice between quotes.
            out.push(Value::String(Cow::Borrowed(&raw[start + 1..i.saturating_sub(1).max(start + 1)])));
            continue;
        }
        if b == b'#' {
            let start = i;
            i += 1;
            while i < bytes.len()
                && (bytes[i].is_ascii_hexdigit() || bytes[i] == b'_')
            {
                i += 1;
            }
            let hex_str = &raw[start + 1..i];
            if let Ok(rgba) = parse_hex_color(hex_str) {
                out.push(Value::Hex(rgba));
            } else {
                out.push(Value::Raw(Cow::Borrowed(&raw[start..i])));
            }
            continue;
        }
        if b == b'-' || b == b'+' || b.is_ascii_digit() || b == b'.' {
            let start = i;
            if b == b'-' || b == b'+' {
                i += 1;
            }
            while i < bytes.len() && (bytes[i].is_ascii_digit() || bytes[i] == b'.') {
                i += 1;
            }
            // Optional unit suffix.
            let num_end = i;
            while i < bytes.len()
                && (bytes[i].is_ascii_alphabetic() || bytes[i] == b'%')
            {
                i += 1;
            }
            let num_str = &raw[start..num_end];
            let unit_str = &raw[num_end..i];
            let num: f64 = num_str.parse().unwrap_or(0.0);
            if unit_str.is_empty() {
                if num_str.contains('.') {
                    out.push(Value::Number(num));
                } else if let Ok(iv) = num_str.parse::<i64>() {
                    out.push(Value::Integer(iv));
                } else {
                    out.push(Value::Number(num));
                }
            } else if unit_str == "%" {
                out.push(Value::Percentage(num));
            } else if let Some(unit) = values::parse_unit(unit_str) {
                out.push(Value::Dimension(Dimension { value: num, unit }));
            } else {
                out.push(Value::Raw(Cow::Borrowed(&raw[start..i])));
            }
            continue;
        }
        if b.is_ascii_alphabetic() || b == b'_' || b == b'-' {
            let start = i;
            while i < bytes.len()
                && (bytes[i].is_ascii_alphanumeric()
                    || bytes[i] == b'_'
                    || bytes[i] == b'-')
            {
                i += 1;
            }
            let ident = &raw[start..i];
            // Check for function call.
            if i < bytes.len() && bytes[i] == b'(' {
                let paren_start = i;
                let mut depth = 1i32;
                i += 1;
                while i < bytes.len() && depth > 0 {
                    match bytes[i] {
                        b'(' => depth += 1,
                        b')' => depth -= 1,
                        _ => {}
                    }
                    i += 1;
                }
                let args = &raw[paren_start + 1..i.saturating_sub(1).max(paren_start + 1)];
                if ident == "var" {
                    // Split on first comma.
                    let (name, fb) = match args.find(',') {
                        Some(c) => (
                            args[..c].trim(),
                            Some(Cow::Borrowed(args[c + 1..].trim())),
                        ),
                        None => (args.trim(), None),
                    };
                    out.push(Value::Var {
                        name: Cow::Borrowed(name),
                        fallback: fb,
                    });
                } else if ident == "url" {
                    let u = args.trim();
                    let u = u
                        .strip_prefix('"')
                        .and_then(|s| s.strip_suffix('"'))
                        .or_else(|| u.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')))
                        .unwrap_or(u);
                    out.push(Value::Url {
                        url: Cow::Borrowed(u),
                    });
                } else if matches!(
                    ident,
                    "calc" | "min" | "max" | "clamp" | "mod" | "rem" | "round" | "sign" | "abs"
                ) {
                    let kind = match ident {
                        "calc" => CalcKind::Calc,
                        "min" => CalcKind::Min,
                        "max" => CalcKind::Max,
                        "clamp" => CalcKind::Clamp,
                        "mod" => CalcKind::Mod,
                        "rem" => CalcKind::Rem,
                        "round" => CalcKind::Round,
                        "sign" => CalcKind::Sign,
                        "abs" => CalcKind::Abs,
                        _ => CalcKind::Calc,
                    };
                    out.push(Value::Calc {
                        kind,
                        body: Cow::Borrowed(args),
                    });
                } else {
                    out.push(Value::Function {
                        name: Cow::Borrowed(ident),
                        args: Cow::Borrowed(args),
                    });
                }
            } else {
                // Ident — check global keywords.
                let kw = match ident {
                    "inherit" => Some(CssGlobalKeyword::Inherit),
                    "initial" => Some(CssGlobalKeyword::Initial),
                    "unset" => Some(CssGlobalKeyword::Unset),
                    "revert" => Some(CssGlobalKeyword::Revert),
                    "revert-layer" => Some(CssGlobalKeyword::RevertLayer),
                    _ => None,
                };
                if let Some(k) = kw {
                    out.push(Value::Global(k));
                } else if ident.starts_with('-') {
                    out.push(Value::DashedIdent(Cow::Borrowed(ident)));
                } else {
                    out.push(Value::Ident(Cow::Borrowed(ident)));
                }
            }
            continue;
        }
        // Unknown byte — skip.
        i += 1;
    }
    out
}

fn parse_hex_color(hex: &str) -> Result<u32, ()> {
    let hex = hex.trim();
    let bytes = hex.as_bytes();
    let hv = |b: u8| -> Option<u32> {
        match b {
            b'0'..=b'9' => Some((b - b'0') as u32),
            b'a'..=b'f' => Some((b - b'a' + 10) as u32),
            b'A'..=b'F' => Some((b - b'A' + 10) as u32),
            _ => None,
        }
    };
    match bytes.len() {
        3 => {
            let r = hv(bytes[0]).ok_or(())?;
            let g = hv(bytes[1]).ok_or(())?;
            let b = hv(bytes[2]).ok_or(())?;
            Ok(((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | 0xFF)
        }
        4 => {
            let r = hv(bytes[0]).ok_or(())?;
            let g = hv(bytes[1]).ok_or(())?;
            let b = hv(bytes[2]).ok_or(())?;
            let a = hv(bytes[3]).ok_or(())?;
            Ok(((r << 4 | r) << 24)
                | ((g << 4 | g) << 16)
                | ((b << 4 | b) << 8)
                | (a << 4 | a))
        }
        6 => {
            let r = (hv(bytes[0]).ok_or(())? << 4) | hv(bytes[1]).ok_or(())?;
            let g = (hv(bytes[2]).ok_or(())? << 4) | hv(bytes[3]).ok_or(())?;
            let b = (hv(bytes[4]).ok_or(())? << 4) | hv(bytes[5]).ok_or(())?;
            Ok((r << 24) | (g << 16) | (b << 8) | 0xFF)
        }
        8 => {
            let r = (hv(bytes[0]).ok_or(())? << 4) | hv(bytes[1]).ok_or(())?;
            let g = (hv(bytes[2]).ok_or(())? << 4) | hv(bytes[3]).ok_or(())?;
            let b = (hv(bytes[4]).ok_or(())? << 4) | hv(bytes[5]).ok_or(())?;
            let a = (hv(bytes[6]).ok_or(())? << 4) | hv(bytes[7]).ok_or(())?;
            Ok((r << 24) | (g << 16) | (b << 8) | a)
        }
        _ => Err(()),
    }
}

