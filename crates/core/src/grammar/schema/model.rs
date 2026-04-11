//! `CstSchema` model — target-agnostic types describing the CST shape.
//!
//! These types are produced by `build::from_ir()` and consumed by every
//! per-target emitter under `emit/`. Nothing here references syn, quote,
//! TokenStream, or any backend-specific types.

use std::collections::HashSet;

use bbnf_ir::{RuleId, TypeDesc};

/// First-class description of the CST shape, with explicit field roles.
///
/// Built from `GrammarIR` by `build::from_ir()`. After construction the
/// schema is self-contained — downstream consumers read it directly and
/// never re-walk raw `IrNode` / `TypeDesc` to rediscover CST structure.
#[derive(Debug, Clone)]
pub struct CstSchema {
    /// Target name — under the eager-AST emitter this held the
    /// concrete enum identifier (e.g. `BbnfBootstrapEnum`); under
    /// the tape-first AC.2 emitter it's the root grammar marker
    /// struct (e.g. `BbnfBootstrap`), with per-rule views resolved
    /// via codegen from the schema variant list.
    pub enum_name: String,
    /// All variants in stable emission order:
    /// rule variants → sub-variants → Recovered (if any) → __Phantom.
    pub variants: Vec<VariantDescriptor>,
    /// Set of rule IDs reachable from the `directive` rule (if it exists).
    pub directive_rules: HashSet<RuleId>,
    /// Set of rule IDs whose body is a single transparent wrapper.
    pub transparent_wrappers: HashSet<RuleId>,
    /// Whether a `Recovered` variant is emitted (any `@recover` directive
    /// + `!skip_recover`).
    pub has_recovered: bool,
}

/// Per-variant description: name, category, payload type, field roles.
#[derive(Debug, Clone)]
pub struct VariantDescriptor {
    /// Variant name as it appears in the enum (e.g. `factor`, `factor_0`).
    pub name: String,
    /// `Some(rule_id)` for rule variants, `None` for sub-variants /
    /// Phantom / Recovered.
    pub rule_id: Option<RuleId>,
    /// Semantic category.
    pub category: VariantCategory,
    /// The variant's payload type. `None` for `Recovered` / `__Phantom`.
    pub type_desc: Option<TypeDesc>,
    /// Per-field role assignments. One entry per top-level position of
    /// the payload (e.g., 4 entries for a 4-tuple). Empty for variants
    /// with no payload (`Recovered`).
    pub fields: Vec<FieldDescriptor>,
}

/// A single field within a variant's payload (top-level tuple position).
#[derive(Debug, Clone)]
pub struct FieldDescriptor {
    /// Position in the payload tuple (0-based). For non-tuple payloads,
    /// always 0.
    pub index: usize,
    /// The field's resolved type.
    pub type_desc: TypeDesc,
    /// Semantic role.
    pub role: FieldRole,
}

/// Semantic category of a variant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariantCategory {
    /// Span carrier only — leaf node holding text (e.g. `identifier`,
    /// `literal`, `regex`).
    Terminal,
    /// Single-payload pass-through (rule body is a bare `Ref`).
    Transparent,
    /// Structural node with multiple semantic fields.
    Composite,
    /// Directive rule (`import`, `recover`, `pretty`, …) — discovered by
    /// walking from the `directive` rule.
    Directive(DirectiveKind),
    /// Auto-generated `__Phantom` variant.
    Phantom,
    /// Auto-generated `Recovered` variant.
    Recovered,
}

/// Directive kind, derived from the rule name (e.g. `import_directive`
/// → `Import`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirectiveKind {
    Import,
    Recover,
    Pretty,
    Ws,
    Token,
    Debug,
    Host,
    /// Unknown directive — categorized by the rule's name.
    Other(String),
}

/// Semantic role of a field within a variant's payload.
///
/// Roles drive walker behavior:
/// - `Keyword` / `Punctuation` / `Annotation` are skipped by traversal walkers.
/// - `PrimaryChild` / `OptionalChild` / `RepeatedChildren` are visited.
/// - `IdentifierCarrier` participates in name resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldRole {
    /// Single child that IS this node's semantic value (used for
    /// `Transparent` variants).
    Transparent,
    /// Literal keyword (e.g. `@token`, `import`, `=`) — skipped by walkers.
    Keyword,
    /// Structural punctuation (`,`, `;`, `(`, `)`) — skipped by walkers.
    Punctuation,
    /// The "meaningful" primary child (rule body, directive target, …).
    PrimaryChild,
    /// Carries an identifier name (used for name lookups).
    IdentifierCarrier,
    /// `Vec<Child>` — list of homogeneous children.
    RepeatedChildren,
    /// `Option<Child>` — optional sub-expression.
    OptionalChild,
    /// Non-semantic annotation (leading/trailing comment, modifier marker, …).
    Annotation,
}

impl CstSchema {
    /// Iterate variants by category, in declaration order.
    pub fn variants_by_category<'a>(
        &'a self,
        category: &'a VariantCategory,
    ) -> impl Iterator<Item = &'a VariantDescriptor> + 'a {
        self.variants.iter().filter(move |v| &v.category == category)
    }

    /// Look up a variant by name. O(n); intended for one-off queries.
    pub fn variant_by_name(&self, name: &str) -> Option<&VariantDescriptor> {
        self.variants.iter().find(|v| v.name == name)
    }

    /// Whether the schema describes any directive rules.
    pub fn has_directives(&self) -> bool {
        !self.directive_rules.is_empty()
    }
}
