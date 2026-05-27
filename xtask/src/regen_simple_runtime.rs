//! Root simple-runtime projection emitter.
//!
//! `cargo xtask regen-math` regenerates the root math runtime from the
//! declarative projection source at `xtask/runtime-projections/math.toml`
//! plus the current grammar registry sidecar. The sidecar supplies
//! current rule ids; the projection supplies the runtime surface.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use anyhow::{anyhow, bail, Context, Result};
use bbnf_ir::registry::StructRegistry;

#[derive(Debug, serde::Deserialize)]
struct RuntimeProjection {
    schema_version: u32,
    grammar: String,
    grammar_source: String,
    entry_rule: String,
    runtime_module: String,
    output_dir: String,
    parser_type: String,
    type_prefix: String,
    module_name: String,
    #[serde(default)]
    runtime_style: RuntimeStyle,
    kind: KindProjection,
}

#[derive(Debug, Default, serde::Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
enum RuntimeStyle {
    #[default]
    Simple,
    TypedFormula,
    TypedBbnf,
}

#[derive(Debug, serde::Deserialize)]
struct KindProjection {
    default: String,
    layout_mode: LayoutMode,
    variants: Vec<String>,
    #[serde(default)]
    routes: Vec<KindRoute>,
    #[serde(default)]
    transparent: Vec<String>,
}

#[derive(Debug, serde::Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
enum LayoutMode {
    AlwaysDefault,
    RuleRoutes,
}

#[derive(Debug, serde::Deserialize)]
struct KindRoute {
    rule: String,
    kind: String,
}

#[derive(Debug)]
struct ResolvedKindRoute {
    rule_name: String,
    rule_id: u32,
    kind: String,
}

/// Regenerate one simple root runtime tree.
pub fn run(grammar: &str) -> Result<()> {
    let workspace_root = workspace_root()?;
    let projection = load_projection(&workspace_root, grammar)?;
    validate_workspace_grammar(&workspace_root, &projection)?;
    validate_projection(&projection)?;
    let registry = load_registry(&workspace_root, &projection)?;
    let kind_routes = resolve_kind_routes(&projection, &registry)?;

    let files = match projection.runtime_style {
        RuntimeStyle::Simple => emit_simple_runtime(&projection, &kind_routes),
        RuntimeStyle::TypedFormula => emit_typed_formula_runtime(&projection, &kind_routes),
        RuntimeStyle::TypedBbnf => emit_typed_bbnf_runtime(&projection, &kind_routes),
    };

    for (relative, source) in files {
        let path = workspace_root.join(&projection.output_dir).join(relative);
        write_if_changed(&path, &format_rust(&source, &path)?)?;
    }

    Ok(())
}

fn emit_simple_runtime(
    projection: &RuntimeProjection,
    kind_routes: &[ResolvedKindRoute],
) -> Vec<(String, String)> {
    vec![
        ("arena.rs".into(), emit_arena(projection)),
        ("builder.rs".into(), emit_builder(projection)),
        ("document.rs".into(), emit_document(projection)),
        ("kind.rs".into(), emit_kind(projection, kind_routes)),
        ("mod.rs".into(), emit_mod(projection)),
        ("value.rs".into(), emit_value(projection)),
        ("view.rs".into(), emit_view(projection)),
    ]
}

fn workspace_root() -> Result<PathBuf> {
    let metadata = cargo_metadata::MetadataCommand::new()
        .no_deps()
        .exec()
        .context("cargo_metadata: failed to read workspace manifest")?;
    Ok(metadata.workspace_root.into_std_path_buf())
}

fn load_projection(root: &Path, grammar: &str) -> Result<RuntimeProjection> {
    let path = root
        .join("xtask/runtime-projections")
        .join(format!("{grammar}.toml"));
    let bytes = std::fs::read(&path).with_context(|| format!("read `{}`", path.display()))?;
    let source =
        std::str::from_utf8(&bytes).with_context(|| format!("utf8 `{}`", path.display()))?;
    let projection: RuntimeProjection =
        toml::from_str(source).with_context(|| format!("parse `{}`", path.display()))?;
    if projection.schema_version != 1 {
        bail!(
            "unsupported simple runtime projection schema_version {}",
            projection.schema_version
        );
    }
    if projection.grammar != grammar {
        bail!(
            "projection grammar `{}` does not match requested grammar `{grammar}`",
            projection.grammar
        );
    }
    Ok(projection)
}

fn validate_workspace_grammar(root: &Path, projection: &RuntimeProjection) -> Result<()> {
    let manifest = root.join("Cargo.toml");
    let bytes =
        std::fs::read(&manifest).with_context(|| format!("read `{}`", manifest.display()))?;
    let source =
        std::str::from_utf8(&bytes).with_context(|| format!("utf8 `{}`", manifest.display()))?;
    let value: toml::Value =
        toml::from_str(source).with_context(|| format!("parse `{}`", manifest.display()))?;
    let grammars = value
        .get("workspace")
        .and_then(|v| v.get("metadata"))
        .and_then(|v| v.get("bbnf"))
        .and_then(|v| v.get("grammars"))
        .and_then(toml::Value::as_array)
        .ok_or_else(|| anyhow!("workspace manifest missing [workspace.metadata.bbnf].grammars"))?;
    let row = grammars
        .iter()
        .find(|entry| {
            entry
                .get("ident")
                .and_then(toml::Value::as_str)
                .is_some_and(|ident| ident == projection.grammar)
        })
        .ok_or_else(|| {
            anyhow!(
                "workspace manifest has no grammar metadata row for `{}`",
                projection.grammar
            )
        })?;
    let Some(path) = row.get("path").and_then(toml::Value::as_str) else {
        bail!("workspace grammar row `{}` has no path", projection.grammar);
    };
    if path != projection.grammar_source {
        bail!(
            "workspace grammar `{}` path `{path}` does not match projection `{}`",
            projection.grammar,
            projection.grammar_source
        );
    }
    Ok(())
}

fn validate_projection(projection: &RuntimeProjection) -> Result<()> {
    if projection.runtime_module != format!("crate::runtime::{}", projection.module_name) {
        bail!(
            "simple runtime projection `{}` has runtime_module `{}` outside module `{}`",
            projection.grammar,
            projection.runtime_module,
            projection.module_name
        );
    }
    validate_ident(&projection.type_prefix, "type_prefix")?;
    validate_ident(&projection.module_name, "module_name")?;
    validate_ident(&projection.parser_type, "parser_type")?;
    if projection.kind.variants.is_empty() {
        bail!("simple runtime projection has no compound-kind variants");
    }
    let mut seen = BTreeSet::new();
    for variant in &projection.kind.variants {
        validate_ident(variant, "compound-kind variant")?;
        if !seen.insert(variant) {
            bail!("duplicate compound-kind variant `{variant}`");
        }
    }
    if !seen.contains(&projection.kind.default) {
        bail!(
            "default compound-kind variant `{}` is not declared",
            projection.kind.default
        );
    }
    for variant in &projection.kind.transparent {
        validate_ident(variant, "transparent compound-kind variant")?;
        if !seen.contains(variant) {
            bail!("transparent compound-kind variant `{variant}` is not declared");
        }
    }
    match projection.kind.layout_mode {
        LayoutMode::AlwaysDefault if !projection.kind.routes.is_empty() => {
            bail!("always_default compound-kind mode must not declare routes")
        }
        LayoutMode::RuleRoutes if projection.kind.routes.is_empty() => {
            bail!("rule_routes compound-kind mode requires at least one route")
        }
        _ => {}
    }
    Ok(())
}

fn validate_ident(value: &str, field: &str) -> Result<()> {
    let mut chars = value.chars();
    let Some(first) = chars.next() else {
        bail!("{field} must not be empty");
    };
    if !(first == '_' || first.is_ascii_alphabetic()) {
        bail!("{field} `{value}` is not a Rust identifier");
    }
    if !chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric()) {
        bail!("{field} `{value}` is not a Rust identifier");
    }
    Ok(())
}

fn load_registry(root: &Path, projection: &RuntimeProjection) -> Result<StructRegistry> {
    let path = root
        .join("crates/core/src/grammar/generated")
        .join(format!("{}.registry.json", projection.grammar));
    let bytes = std::fs::read(&path).with_context(|| format!("read `{}`", path.display()))?;
    let sidecar: crate::regen::GrammarRegistrySidecar =
        serde_json::from_slice(&bytes).with_context(|| format!("parse `{}`", path.display()))?;
    if sidecar.entry_rule != projection.entry_rule {
        bail!(
            "registry entry rule `{}` does not match projection `{}`",
            sidecar.entry_rule,
            projection.entry_rule
        );
    }
    Ok(sidecar.registry)
}

fn resolve_kind_routes(
    projection: &RuntimeProjection,
    registry: &StructRegistry,
) -> Result<Vec<ResolvedKindRoute>> {
    let mut seen_rules = BTreeSet::new();
    projection
        .kind
        .routes
        .iter()
        .map(|route| {
            if !projection
                .kind
                .variants
                .iter()
                .any(|kind| kind == &route.kind)
            {
                bail!(
                    "compound-kind route references unknown kind `{}`",
                    route.kind
                );
            }
            let layout = registry.layout_by_name(&route.rule).ok_or_else(|| {
                anyhow!(
                    "simple runtime projection references unknown rule `{}`",
                    route.rule
                )
            })?;
            if !seen_rules.insert(layout.rule_id) {
                bail!("duplicate compound-kind route for rule `{}`", route.rule);
            }
            Ok(ResolvedKindRoute {
                rule_name: route.rule.clone(),
                rule_id: layout.rule_id,
                kind: route.kind.clone(),
            })
        })
        .collect()
}

fn format_rust(source: &str, path: &Path) -> Result<String> {
    let parsed: syn::File =
        syn::parse_file(source).with_context(|| format!("parse generated `{}`", path.display()))?;
    Ok(prettyplease::unparse(&parsed))
}

fn write_if_changed(path: &Path, source: &str) -> Result<()> {
    let existing = std::fs::read(path).ok();
    if existing.as_deref() == Some(source.as_bytes()) {
        return Ok(());
    }
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("create parent dir for `{}`", path.display()))?;
    }
    std::fs::write(path, source).with_context(|| format!("write `{}`", path.display()))
}

fn emit_typed_formula_runtime(
    projection: &RuntimeProjection,
    routes: &[ResolvedKindRoute],
) -> Vec<(String, String)> {
    vec![
        (
            "arena.rs".into(),
            emit_typed_formula_arena(projection, routes),
        ),
        ("builder.rs".into(), emit_typed_formula_builder(projection)),
        (
            "document/canonical.rs".into(),
            emit_typed_formula_document_canonical(projection),
        ),
        (
            "document/mod.rs".into(),
            emit_typed_formula_document_mod(projection),
        ),
        (
            "document/path_query.rs".into(),
            emit_typed_formula_document_path_query(projection),
        ),
        (
            "document/view.rs".into(),
            emit_typed_formula_document_view(projection),
        ),
        ("mod.rs".into(), emit_typed_formula_mod(projection)),
        (
            "parse_with.rs".into(),
            emit_typed_formula_parse_with(projection),
        ),
        ("value.rs".into(), emit_typed_formula_value(projection)),
        ("view.rs".into(), emit_typed_formula_view(projection)),
    ]
}

fn runtime_template(source: &str, projection: &RuntimeProjection) -> String {
    source
        .replace("__P__", &projection.type_prefix)
        .replace("__MODULE__", &projection.runtime_module)
        .replace("__PARSER__", &projection.parser_type)
        .replace("__ENTRY__", &projection.entry_rule)
}

fn emit_typed_formula_arena(
    projection: &RuntimeProjection,
    routes: &[ResolvedKindRoute],
) -> String {
    let variants = projection
        .kind
        .variants
        .iter()
        .map(|variant| format!("    {variant},"))
        .collect::<Vec<_>>()
        .join("\n");
    let arms = routes
        .iter()
        .map(|route| format!("            {} => Self::{},", route.rule_id, route.kind))
        .collect::<Vec<_>>()
        .join("\n");
    let transparent = if projection.kind.transparent.is_empty() {
        "false".to_string()
    } else {
        format!(
            "matches!(self, {})",
            projection
                .kind
                .transparent
                .iter()
                .map(|variant| format!("Self::{variant}"))
                .collect::<Vec<_>>()
                .join(" | ")
        )
    };
    runtime_template(
        &format!(
            r#"
use bbnf_ir::registry::StructLayout;

use __MODULE__::value::__P__Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum __P__CompoundKind {{
__VARIANTS__
}}

impl __P__CompoundKind {{
    #[inline]
    pub fn is_transparent_wrap(self) -> bool {{
        __TRANSPARENT__
    }}

    #[inline]
    pub fn from_layout(layout: &StructLayout) -> Self {{
        match layout.rule_id {{
__ARMS__
            _ => Self::__DEFAULT__,
        }}
    }}
}}

#[derive(Debug, Clone)]
pub struct __P__Compound<'p> {{
    pub kind: __P__CompoundKind,
    pub children: Vec<__P__Value<'p>>,
}}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct __P__CompoundId(u32);

impl __P__CompoundId {{
    pub const EMPTY: Self = Self(0);

    #[inline]
    pub const fn is_empty(self) -> bool {{
        self.0 == 0
    }}

    #[inline]
    fn slab_index(self) -> Option<usize> {{
        if self.0 == 0 {{
            None
        }} else {{
            Some((self.0 - 1) as usize)
        }}
    }}
}}

#[derive(Debug, Default)]
pub struct __P__Arena<'p> {{
    compounds: Vec<__P__Compound<'p>>,
}}

impl<'p> __P__Arena<'p> {{
    #[inline]
    pub fn new() -> Self {{
        Self::default()
    }}

    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {{
        Self {{
            compounds: Vec::with_capacity(compounds),
        }}
    }}

    #[inline]
    pub fn push_compound(
        &mut self,
        kind: __P__CompoundKind,
        children: Vec<__P__Value<'p>>,
    ) -> __P__CompoundId {{
        self.compounds.push(__P__Compound {{ kind, children }});
        __P__CompoundId(self.compounds.len() as u32)
    }}

    #[inline]
    pub fn compound(&self, id: __P__CompoundId) -> __P__CompoundView<'_, 'p> {{
        match id.slab_index() {{
            None => __P__CompoundView {{
                kind: __P__CompoundKind::Wrap,
                children: &[],
            }},
            Some(i) => {{
                let entry = &self.compounds[i];
                __P__CompoundView {{
                    kind: entry.kind,
                    children: entry.children.as_slice(),
                }}
            }}
        }}
    }}

    #[inline]
    pub fn compound_count(&self) -> usize {{
        self.compounds.len()
    }}

    #[inline]
    pub fn truncate(&mut self, compounds: usize) {{
        self.compounds.truncate(compounds);
    }}
}}

#[derive(Debug, Clone, Copy)]
pub struct __P__CompoundView<'a, 'p: 'a> {{
    pub kind: __P__CompoundKind,
    pub children: &'a [__P__Value<'p>],
}}
"#,
        )
        .replace("__VARIANTS__", &variants)
        .replace("__ARMS__", &arms)
        .replace("__DEFAULT__", &projection.kind.default)
        .replace("__TRANSPARENT__", &transparent),
        projection,
    )
}

fn emit_typed_formula_builder(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
use bbnf_ir::registry::StructLayout;

use crate::runtime::builder::StructBuilder;
use crate::runtime::handle::CompoundHandle;
use __MODULE__::arena::{__P__Arena, __P__CompoundKind};
use __MODULE__::document::__P__Document;
use __MODULE__::value::__P__Value;

#[derive(Debug, Clone)]
struct Frame<'p> {
    kind: __P__CompoundKind,
    children: Vec<__P__Value<'p>>,
    #[allow(dead_code)]
    handle_token: u64,
}

#[derive(Debug)]
pub struct __P__StructBuilder<'p> {
    arena: __P__Arena<'p>,
    stack: Vec<Frame<'p>>,
    root: Option<__P__Value<'p>>,
    next_handle: u64,
}

#[derive(Debug, Clone)]
pub struct __P__StructCheckpoint<'p> {
    compounds: usize,
    stack: Vec<Frame<'p>>,
    root: Option<__P__Value<'p>>,
    next_handle: u64,
}

impl<'p> Default for __P__StructBuilder<'p> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'p> __P__StructBuilder<'p> {
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: __P__Arena::new(),
            stack: Vec::with_capacity(8),
            root: None,
            next_handle: 0,
        }
    }

    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            arena: __P__Arena::with_capacity(compounds),
            stack: Vec::with_capacity(8),
            root: None,
            next_handle: 0,
        }
    }

    #[inline]
    pub fn finalise(mut self, input: &'p str) -> __P__Document<'p> {
        debug_assert!(
            self.stack.is_empty(),
            "__P__StructBuilder::finalise called with open frames"
        );
        let root = self
            .root
            .take()
            .expect("__P__StructBuilder::finalise called before value emission");
        __P__Document::new(self.arena, root, input)
    }

    #[inline]
    fn deposit(&mut self, value: __P__Value<'p>) {
        match self.stack.last_mut() {
            None => self.root = Some(value),
            Some(frame) => frame.children.push(value),
        }
    }
}

impl<'p> StructBuilder for __P__StructBuilder<'p> {
    type Checkpoint = __P__StructCheckpoint<'p>;

    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        __P__StructCheckpoint {
            compounds: self.arena.compound_count(),
            stack: self.stack.clone(),
            root: self.root,
            next_handle: self.next_handle,
        }
    }

    #[inline]
    fn rollback(&mut self, checkpoint: Self::Checkpoint) {
        self.arena.truncate(checkpoint.compounds);
        self.stack = checkpoint.stack;
        self.root = checkpoint.root;
        self.next_handle = checkpoint.next_handle;
    }

    fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle {
        let kind = __P__CompoundKind::from_layout(layout);
        self.next_handle = self.next_handle.wrapping_add(1);
        let handle_token = self.next_handle;
        self.stack.push(Frame {
            kind,
            children: Vec::new(),
            handle_token,
        });
        CompoundHandle::new(handle_token, 0)
    }

    fn end_compound(&mut self, _handle: CompoundHandle) {
        let frame = self
            .stack
            .pop()
            .expect("__P__StructBuilder::end_compound on empty stack");
        let value = if frame.kind.is_transparent_wrap() && frame.children.len() == 1 {
            frame.children[0]
        } else {
            let id = self.arena.push_compound(frame.kind, frame.children);
            __P__Value::Compound(id)
        };
        self.deposit(value);
    }

    #[inline]
    fn push_leaf_with_f64(&mut self, value: f64) {
        self.deposit(__P__Value::Number(value));
    }

    #[inline]
    fn push_leaf_with_i64(&mut self, value: i64) {
        self.deposit(__P__Value::Number(value as f64));
    }

    #[inline]
    fn push_leaf_with_u64(&mut self, value: u64) {
        self.deposit(__P__Value::Number(value as f64));
    }

    #[inline]
    fn push_leaf_with_bool(&mut self, value: bool) {
        self.deposit(__P__Value::Bool(value));
    }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        let extended: &'p str = unsafe { core::mem::transmute(value) };
        self.deposit(__P__Value::String(extended));
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.deposit(__P__Value::Tag(0));
    }

    #[inline]
    fn push_branch_tag(&mut self, branch_index: u32) {
        debug_assert!(branch_index < 256);
        self.deposit(__P__Value::Tag(branch_index as u8));
    }
}

impl<'p> __P__StructBuilder<'p> {
    #[inline]
    pub fn push_leaf_cell_ref(&mut self, value: &str) {
        let extended: &'p str = unsafe { core::mem::transmute(value) };
        self.deposit(__P__Value::CellRef(extended));
    }

    #[inline]
    pub fn push_leaf_identifier(&mut self, value: &str) {
        let extended: &'p str = unsafe { core::mem::transmute(value) };
        self.deposit(__P__Value::Identifier(extended));
    }

    #[inline]
    pub fn push_leaf_sheet_prefix(&mut self, tag: u8, value: &str) {
        let extended: &'p str = unsafe { core::mem::transmute(value) };
        self.deposit(__P__Value::SheetPrefix {
            tag,
            text: extended,
        });
    }

    #[inline]
    pub fn push_leaf_error(&mut self, value: u8) {
        self.deposit(__P__Value::Error(value));
    }
}
"#,
        projection,
    )
}

fn emit_typed_formula_document_mod(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
pub mod canonical;
pub mod path_query;
pub mod view;

use __MODULE__::arena::{__P__Arena, __P__CompoundId, __P__CompoundView};
use __MODULE__::value::__P__Value;
use crate::runtime::path::Path;

pub use self::path_query::__P__PathQuery;
pub use self::view::{__P__Kind, __P__View};

#[derive(Debug)]
pub struct __P__Document<'p> {
    pub arena: __P__Arena<'p>,
    pub root: __P__Value<'p>,
    pub input: &'p str,
}

impl<'p> __P__Document<'p> {
    #[inline]
    pub fn new(arena: __P__Arena<'p>, root: __P__Value<'p>, input: &'p str) -> Self {
        Self { arena, root, input }
    }

    #[inline]
    pub fn root(&self) -> &__P__Value<'p> {
        &self.root
    }

    #[inline]
    pub fn arena(&self) -> &__P__Arena<'p> {
        &self.arena
    }

    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }

    #[inline]
    pub fn compound(&self, id: __P__CompoundId) -> __P__CompoundView<'_, 'p> {
        self.arena.compound(id)
    }

    #[inline]
    pub fn view<'a>(&'a self) -> __P__View<'a, 'p> {
        __P__View::focused(self, self.root)
    }

    #[inline]
    pub fn to_value(&self) -> &__P__Value<'p> {
        &self.root
    }

    #[inline]
    pub fn get<T: __P__PathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }

    pub fn serialize_compact(&self) -> String {
        canonical::serialize_compact(self)
    }
}
"#,
        projection,
    )
}

fn emit_typed_formula_document_path_query(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
use __MODULE__::value::__P__Value;
use crate::runtime::path::{Path, PathSegment};

use super::__P__Document;

pub trait __P__PathQuery: Sized {
    fn query<'p>(doc: &__P__Document<'p>, path: Path<'_>) -> Option<Self>;
}

#[inline]
fn walk_path<'a, 'p>(doc: &'a __P__Document<'p>, path: Path<'_>) -> Option<&'a __P__Value<'p>> {
    let mut current: &'a __P__Value<'p> = &doc.root;
    for segment in path.iter() {
        current = match (current, segment) {
            (__P__Value::Compound(id), PathSegment::Index(idx)) => {
                let entry = doc.compound(*id);
                entry.children.get(*idx)?
            }
            _ => return None,
        };
    }
    Some(current)
}

impl __P__PathQuery for f64 {
    #[inline]
    fn query<'p>(doc: &__P__Document<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            __P__Value::Number(n) => Some(*n),
            _ => None,
        }
    }
}

impl __P__PathQuery for bool {
    #[inline]
    fn query<'p>(doc: &__P__Document<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            __P__Value::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

impl __P__PathQuery for u8 {
    #[inline]
    fn query<'p>(doc: &__P__Document<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            __P__Value::Tag(t) | __P__Value::Error(t) => Some(*t),
            __P__Value::SheetPrefix { tag, .. } => Some(*tag),
            _ => None,
        }
    }
}

impl __P__PathQuery for &str {
    #[inline]
    fn query<'p>(doc: &__P__Document<'p>, path: Path<'_>) -> Option<Self> {
        let value = walk_path(doc, path)?;
        match value {
            __P__Value::String(s)
            | __P__Value::CellRef(s)
            | __P__Value::Identifier(s)
            | __P__Value::SheetPrefix { text: s, .. } => {
                let extended: &'p str = *s;
                Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
            }
            _ => None,
        }
    }
}

impl __P__PathQuery for __P__Value<'_> {
    #[inline]
    fn query<'p>(doc: &__P__Document<'p>, path: Path<'_>) -> Option<Self> {
        let value = walk_path(doc, path)?;
        let copied: __P__Value<'p> = *value;
        Some(unsafe { core::mem::transmute::<__P__Value<'p>, __P__Value<'_>>(copied) })
    }
}
"#,
        projection,
    )
}

fn emit_typed_formula_document_view(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
use __MODULE__::arena::{__P__Arena, __P__CompoundId, __P__CompoundView};
use __MODULE__::value::__P__Value;

use super::__P__Document;

#[derive(Debug, Clone, Copy)]
pub struct __P__View<'a, 'p: 'a> {
    pub(crate) doc: &'a __P__Document<'p>,
    pub(crate) focus: __P__Value<'p>,
}

impl<'a, 'p: 'a> __P__View<'a, 'p> {
    #[inline]
    pub fn focused(doc: &'a __P__Document<'p>, focus: __P__Value<'p>) -> Self {
        Self { doc, focus }
    }

    #[inline]
    pub fn document(&self) -> &'a __P__Document<'p> {
        self.doc
    }

    #[inline]
    pub fn focus(&self) -> __P__Value<'p> {
        self.focus
    }

    #[inline]
    pub fn root(&self) -> &'a __P__Value<'p> {
        &self.doc.root
    }

    #[inline]
    pub fn arena(&self) -> &'a __P__Arena<'p> {
        &self.doc.arena
    }

    #[inline]
    pub fn compound(&self, id: __P__CompoundId) -> __P__CompoundView<'a, 'p> {
        self.doc.compound(id)
    }

    #[inline]
    pub fn kind(&self) -> __P__Kind {
        match &self.focus {
            __P__Value::Number(_) => __P__Kind::Number,
            __P__Value::String(_) => __P__Kind::String,
            __P__Value::Bool(_) => __P__Kind::Bool,
            __P__Value::Error(_) => __P__Kind::Error,
            __P__Value::CellRef(_) => __P__Kind::CellRef,
            __P__Value::Identifier(_) => __P__Kind::Identifier,
            __P__Value::SheetPrefix { .. } => __P__Kind::SheetPrefix,
            __P__Value::Tag(_) => __P__Kind::Tag,
            __P__Value::Compound(_) => __P__Kind::Compound,
        }
    }

    #[inline]
    pub fn is_compound(&self) -> bool {
        matches!(self.focus, __P__Value::Compound(_))
    }

    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self.focus, __P__Value::Number(_))
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        matches!(
            self.focus,
            __P__Value::String(_)
                | __P__Value::CellRef(_)
                | __P__Value::Identifier(_)
                | __P__Value::SheetPrefix { .. }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum __P__Kind {
    Number,
    String,
    Bool,
    Error,
    CellRef,
    Identifier,
    SheetPrefix,
    Tag,
    Compound,
}
"#,
        projection,
    )
}

fn emit_typed_formula_value(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
use __MODULE__::arena::__P__CompoundId;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum __P__Value<'p> {
    Number(f64),
    String(&'p str),
    Bool(bool),
    Error(u8),
    CellRef(&'p str),
    Identifier(&'p str),
    SheetPrefix { tag: u8, text: &'p str },
    Tag(u8),
    Compound(__P__CompoundId),
}

impl<'p> __P__Value<'p> {
    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self, __P__Value::Number(_))
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        matches!(self, __P__Value::String(_))
    }

    #[inline]
    pub fn is_compound(&self) -> bool {
        matches!(self, __P__Value::Compound(_))
    }

    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        match *self {
            __P__Value::Number(n) => Some(n),
            _ => None,
        }
    }

    #[inline]
    pub fn as_str(&self) -> Option<&'p str> {
        match *self {
            __P__Value::String(s)
            | __P__Value::CellRef(s)
            | __P__Value::Identifier(s)
            | __P__Value::SheetPrefix { text: s, .. } => Some(s),
            _ => None,
        }
    }

    #[inline]
    pub fn as_bool(&self) -> Option<bool> {
        match *self {
            __P__Value::Bool(b) => Some(b),
            _ => None,
        }
    }

    #[inline]
    pub fn as_u8(&self) -> Option<u8> {
        match *self {
            __P__Value::Tag(t) | __P__Value::Error(t) => Some(t),
            __P__Value::SheetPrefix { tag, .. } => Some(tag),
            _ => None,
        }
    }
}
"#,
        projection,
    )
}

fn emit_typed_formula_document_canonical(projection: &RuntimeProjection) -> String {
    runtime_template(
        r##"
use __MODULE__::arena::{__P__CompoundId, __P__CompoundKind, __P__CompoundView};
use __MODULE__::value::__P__Value;

use super::__P__Document;

pub(super) fn serialize_compact(doc: &__P__Document<'_>) -> String {
    let mut out = String::with_capacity(doc.input.len());
    let __P__Value::Compound(_) = doc.root else {
        write_value(doc, &doc.root, __P__CompoundKind::Wrap, &mut out);
        return out;
    };
    out.push('=');
    if let __P__Value::Compound(id) = doc.root {
        write_compound(doc, id, &mut out);
    }
    out
}

fn write_value<'p>(
    doc: &__P__Document<'p>,
    value: &__P__Value<'p>,
    parent_kind: __P__CompoundKind,
    out: &mut String,
) {
    use core::fmt::Write;
    match *value {
        __P__Value::Number(n) => {
            if n.fract() == 0.0 && n.is_finite() && n.abs() < 1e16 {
                write!(out, "{}", n as i64).unwrap();
            } else {
                write!(out, "{}", n).unwrap();
            }
        }
        __P__Value::String(s)
        | __P__Value::CellRef(s)
        | __P__Value::Identifier(s)
        | __P__Value::SheetPrefix { text: s, .. } => out.push_str(s),
        __P__Value::Bool(b) => out.push_str(if b { "TRUE" } else { "FALSE" }),
        __P__Value::Error(n) => out.push_str(error_lexeme(n)),
        __P__Value::Tag(n) => out.push_str(tag_lexeme(parent_kind, n)),
        __P__Value::Compound(id) => write_compound(doc, id, out),
    }
}

fn write_compound<'p>(doc: &__P__Document<'p>, id: __P__CompoundId, out: &mut String) {
    let entry = doc.compound(id);
    let kind = entry.kind;
    match kind {
        __P__CompoundKind::ParenExpr => {
            out.push('(');
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
            out.push(')');
        }
        __P__CompoundKind::FuncCall => write_func_call(doc, &entry, out),
        __P__CompoundKind::FuncOpen => {
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
            out.push('(');
        }
        __P__CompoundKind::FuncArgs | __P__CompoundKind::LetArgs => {
            for (i, child) in entry.children.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
            }
        }
        __P__CompoundKind::Arg => {
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
        }
        __P__CompoundKind::LetCall => {
            out.push_str("LET(");
            for (i, child) in entry.children.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
            }
            out.push(')');
        }
        __P__CompoundKind::LetBinding => {
            for (i, child) in entry.children.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
            }
        }
        __P__CompoundKind::LambdaCall => {
            out.push_str("LAMBDA(");
            for (i, child) in entry.children.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
            }
            out.push(')');
        }
        __P__CompoundKind::LambdaParams => {
            for (i, child) in entry.children.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
            }
        }
        __P__CompoundKind::ArrayLiteral => {
            out.push('{');
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
            out.push('}');
        }
        __P__CompoundKind::ArrayRows => {
            let mut emitted = 0usize;
            for child in entry.children {
                if matches!(child, __P__Value::Tag(_)) {
                    continue;
                }
                if emitted > 0 {
                    out.push(';');
                }
                write_value(doc, child, kind, out);
                emitted += 1;
            }
        }
        __P__CompoundKind::ArrayRow => {
            let mut emitted = 0usize;
            for child in entry.children {
                if matches!(child, __P__Value::Tag(_)) {
                    continue;
                }
                if emitted > 0 {
                    out.push(',');
                }
                write_value(doc, child, kind, out);
                emitted += 1;
            }
        }
        __P__CompoundKind::RangeRef => {
            let n = entry.children.len();
            for (i, child) in entry.children.iter().enumerate() {
                if i == n.saturating_sub(1) && n >= 2 {
                    out.push(':');
                }
                write_value(doc, child, kind, out);
            }
        }
        __P__CompoundKind::Cell
        | __P__CompoundKind::PostfixExpr
        | __P__CompoundKind::UnaryExpr
        | __P__CompoundKind::AddExpr
        | __P__CompoundKind::MulExpr
        | __P__CompoundKind::ExpExpr
        | __P__CompoundKind::ConcatExpr
        | __P__CompoundKind::ComparisonExpr
        | __P__CompoundKind::CompareOp
        | __P__CompoundKind::AddOp
        | __P__CompoundKind::MulOp
        | __P__CompoundKind::UnaryPrefix
        | __P__CompoundKind::SheetPrefix
        | __P__CompoundKind::Formula
        | __P__CompoundKind::Expression
        | __P__CompoundKind::Primary
        | __P__CompoundKind::Wrap
        | __P__CompoundKind::RangeEnd
        | __P__CompoundKind::CellOrRange
        | __P__CompoundKind::Unknown => {
            for child in entry.children {
                write_value(doc, child, kind, out);
            }
        }
        __P__CompoundKind::ErrorLiteral => {
            for child in entry.children {
                match *child {
                    __P__Value::Tag(n) | __P__Value::Error(n) => out.push_str(error_lexeme(n)),
                    _ => write_value(doc, child, kind, out),
                }
            }
        }
    }
}

fn write_func_call<'p>(
    doc: &__P__Document<'p>,
    entry: &__P__CompoundView<'_, 'p>,
    out: &mut String,
) {
    let mut iter = entry.children.iter();
    if let Some(head) = iter.next() {
        write_value(doc, head, __P__CompoundKind::FuncCall, out);
    }
    if !out.ends_with('(') {
        out.push('(');
    }
    let mut first_arg = true;
    for arg in iter {
        if !first_arg {
            out.push(',');
        }
        first_arg = false;
        write_value(doc, arg, __P__CompoundKind::FuncCall, out);
    }
    out.push(')');
}

fn error_lexeme(n: u8) -> &'static str {
    match n {
        0 => "#N/A",
        1 => "#VALUE!",
        2 => "#REF!",
        3 => "#DIV/0!",
        4 => "#NULL!",
        5 => "#NAME?",
        6 => "#NUM!",
        7 => "#ERROR!",
        8 => "#SPILL!",
        _ => "",
    }
}

fn tag_lexeme(parent: __P__CompoundKind, n: u8) -> &'static str {
    match (parent, n) {
        (__P__CompoundKind::AddExpr, 0) | (__P__CompoundKind::UnaryExpr, 0) => "+",
        (__P__CompoundKind::AddExpr, 1) | (__P__CompoundKind::UnaryExpr, 1) => "-",
        (__P__CompoundKind::MulExpr, 0) => "*",
        (__P__CompoundKind::MulExpr, 1) => "/",
        (__P__CompoundKind::ExpExpr, _) => "^",
        (__P__CompoundKind::ConcatExpr, _) => "&",
        (__P__CompoundKind::ComparisonExpr, 0) => "<>",
        (__P__CompoundKind::ComparisonExpr, 1) => "<=",
        (__P__CompoundKind::ComparisonExpr, 2) => ">=",
        (__P__CompoundKind::ComparisonExpr, 3) => "<",
        (__P__CompoundKind::ComparisonExpr, 4) => ">",
        (__P__CompoundKind::ComparisonExpr, 5) => "=",
        (__P__CompoundKind::AddOp, 0) | (__P__CompoundKind::UnaryPrefix, 0) => "+",
        (__P__CompoundKind::AddOp, 1) | (__P__CompoundKind::UnaryPrefix, 1) => "-",
        (__P__CompoundKind::MulOp, 0) => "*",
        (__P__CompoundKind::MulOp, 1) => "/",
        (__P__CompoundKind::CompareOp, 0) => "<>",
        (__P__CompoundKind::CompareOp, 1) => "<=",
        (__P__CompoundKind::CompareOp, 2) => ">=",
        (__P__CompoundKind::CompareOp, 3) => "=",
        (__P__CompoundKind::CompareOp, 4) => "<",
        (__P__CompoundKind::CompareOp, 5) => ">",
        _ => "",
    }
}
"##,
        projection,
    )
}

fn emit_typed_formula_mod(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
pub mod arena;
pub mod builder;
pub mod document;
pub mod parse_with;
pub mod value;
pub mod view;

pub use arena::{
    __P__Arena, __P__Compound, __P__CompoundId, __P__CompoundKind, __P__CompoundView,
};
pub use builder::__P__StructBuilder;
pub use document::{__P__Document, __P__Kind, __P__PathQuery, __P__View};
pub use parse_with::parse_with;
pub use value::__P__Value;
"#,
        projection,
    )
}

fn emit_typed_formula_parse_with(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
use super::document::{__P__Document, __P__PathQuery};
use crate::grammar::generated::google_sheets::{
    __path_plan, __shape_support___PARSER__, parse___PARSER_____ENTRY__,
};
use crate::path::cursor::Decision;
use crate::path::executor::PathExecutor;
use crate::path::ir::{PathSegment as TypedSegment, TypedPath};
use crate::path::markers::Sheets;
use crate::runtime::google_sheets::__P__StructBuilder;
use crate::runtime::path::{Path, PathSegment};

fn lower<'a>(seg: &TypedSegment<'a>) -> Option<PathSegment<'a>> {
    match seg {
        TypedSegment::Field(s) => Some(PathSegment::Field(s)),
        TypedSegment::Index(i) => Some(PathSegment::Index(*i)),
        TypedSegment::VariantName(s) => Some(PathSegment::Field(s)),
        TypedSegment::Wildcard => None,
    }
}

pub fn parse_with<T>(input: &str, path: &TypedPath<Sheets, T>) -> Option<T>
where
    T: __P__PathQuery,
{
    PathExecutor::execute(
        input,
        path,
        |rule_id, kind, _idx| {
            __path_plan::lookup(rule_id, kind)
                .map(|e| e.decision)
                .unwrap_or(Decision::ParseFully)
        },
        |src, cursor| {
            let mut state = __shape_support___PARSER__::ScanState::new();
            let mut builder = __P__StructBuilder::new();
            let mut pos: usize = 0;
            parse___PARSER_____ENTRY__(
                src.as_bytes(),
                &mut pos,
                &mut state,
                &mut builder,
                cursor,
            )
            .ok()?;
            let doc: __P__Document<'_> = builder.finalise(src);
            let mut segments: Vec<PathSegment<'_>> = Vec::with_capacity(path.len());
            for owned in path.owned_segments() {
                segments.push(lower(&owned.as_borrowed())?);
            }
            doc.get::<T>(Path::new(&segments))
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::path::ir::OwnedPathSegment;

    #[test]
    #[ignore = "Flat-shape lazy honoring: formula is a flat compound."]
    fn parse_with_resolves_number_leaf() {
        let src = "=42";
        let path: TypedPath<Sheets, f64> =
            TypedPath::from_owned(vec![OwnedPathSegment::Index(0), OwnedPathSegment::Index(0)]);
        let lazy = parse_with::<f64>(src, &path);
        assert_eq!(lazy, Some(42.0));
    }

    #[test]
    fn parse_with_returns_none_on_invalid_input() {
        let path: TypedPath<Sheets, f64> = TypedPath::from_owned(Vec::new());
        let out = parse_with::<f64>("not a formula @@@", &path);
        assert!(out.is_none());
    }
}
"#,
        projection,
    )
}

fn emit_typed_formula_view(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
use crate::runtime::RuntimeView;
use __MODULE__::document::{__P__Kind, __P__View};
use __MODULE__::value::__P__Value;

impl<'a, 'p: 'a> RuntimeView<'p> for __P__View<'a, 'p> {
    type Kind = __P__Kind;

    #[inline]
    fn kind(&self) -> Self::Kind {
        match self.focus {
            __P__Value::Number(_) => __P__Kind::Number,
            __P__Value::String(_) => __P__Kind::String,
            __P__Value::Bool(_) => __P__Kind::Bool,
            __P__Value::Error(_) => __P__Kind::Error,
            __P__Value::CellRef(_) => __P__Kind::CellRef,
            __P__Value::Identifier(_) => __P__Kind::Identifier,
            __P__Value::SheetPrefix { .. } => __P__Kind::SheetPrefix,
            __P__Value::Tag(_) => __P__Kind::Tag,
            __P__Value::Compound(_) => __P__Kind::Compound,
        }
    }

    #[inline]
    fn span(&self) -> Option<&'p str> {
        match self.focus {
            __P__Value::String(s)
            | __P__Value::CellRef(s)
            | __P__Value::Identifier(s)
            | __P__Value::SheetPrefix { text: s, .. } => Some(s),
            _ => None,
        }
    }

    #[inline]
    fn input(&self) -> &'p str {
        self.doc.input
    }

    fn children(&self) -> impl Iterator<Item = Self> + '_ {
        let doc = self.doc;
        let focus = self.focus;
        __P__ChildrenIter {
            doc,
            focus,
            index: 0,
        }
    }
}

pub struct __P__ChildrenIter<'a, 'p: 'a> {
    doc: &'a __MODULE__::__P__Document<'p>,
    focus: __P__Value<'p>,
    index: usize,
}

impl<'a, 'p: 'a> Iterator for __P__ChildrenIter<'a, 'p> {
    type Item = __P__View<'a, 'p>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.focus {
            __P__Value::Compound(id) => {
                let entry = self.doc.compound(id);
                let item = entry.children.get(self.index)?;
                self.index += 1;
                Some(__P__View::focused(self.doc, *item))
            }
            _ => None,
        }
    }
}
"#,
        projection,
    )
}

fn emit_typed_bbnf_runtime(
    projection: &RuntimeProjection,
    routes: &[ResolvedKindRoute],
) -> Vec<(String, String)> {
    vec![
        ("arena.rs".into(), emit_typed_bbnf_arena(projection, routes)),
        ("builder.rs".into(), emit_typed_bbnf_builder(projection)),
        ("document.rs".into(), emit_typed_bbnf_document(projection)),
        ("mod.rs".into(), emit_typed_bbnf_mod(projection)),
        (
            "parse_with.rs".into(),
            emit_typed_bbnf_parse_with(projection),
        ),
        ("serialize.rs".into(), emit_typed_bbnf_serialize(projection)),
        ("value.rs".into(), emit_typed_bbnf_value(projection)),
        ("view.rs".into(), emit_typed_bbnf_view(projection)),
    ]
}

fn typed_bbnf_kind_variants(projection: &RuntimeProjection) -> String {
    projection
        .kind
        .variants
        .iter()
        .map(|variant| format!("    {variant},"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn emit_typed_bbnf_arena(projection: &RuntimeProjection, routes: &[ResolvedKindRoute]) -> String {
    let variants = typed_bbnf_kind_variants(projection);
    let arms = routes
        .iter()
        .map(|route| {
            format!(
                "            \"{}\" => Self::{},",
                route.rule_name, route.kind
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    runtime_template(
        &r#"
use bbnf_ir::registry::{StructLayout, StructRegistry};

use __MODULE__::value::__P__Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum __P__CompoundKind {
__VARIANTS__
}

impl __P__CompoundKind {
    pub fn from_layout(layout: &StructLayout) -> Self {
        match StructRegistry::compound_kind_for_layout(layout) {
__ARMS__
            _ => Self::__DEFAULT__,
        }
    }
}

#[derive(Debug, Clone)]
pub struct __P__Compound<'p> {
    pub kind: __P__CompoundKind,
    pub branch_tag: Option<u32>,
    pub bounds: Option<(u32, u32)>,
    pub children: Vec<__P__Value<'p>>,
}

impl<'p> Default for __P__Compound<'p> {
    fn default() -> Self {
        Self {
            kind: __P__CompoundKind::__DEFAULT__,
            branch_tag: None,
            bounds: None,
            children: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct __P__CompoundId(u32);

impl __P__CompoundId {
    pub const EMPTY: Self = Self(0);

    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    #[inline]
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 {
            None
        } else {
            Some((self.0 - 1) as usize)
        }
    }
}

#[derive(Debug, Default)]
pub struct __P__Arena<'p> {
    compounds: Vec<__P__Compound<'p>>,
    empty: __P__Compound<'p>,
}

impl<'p> __P__Arena<'p> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            compounds: Vec::with_capacity(compounds),
            empty: __P__Compound::default(),
        }
    }

    #[inline]
    pub fn push_compound(&mut self, compound: __P__Compound<'p>) -> __P__CompoundId {
        self.compounds.push(compound);
        __P__CompoundId(self.compounds.len() as u32)
    }

    #[inline]
    pub fn compound(&self, id: __P__CompoundId) -> &__P__Compound<'p> {
        match id.slab_index() {
            None => &self.empty,
            Some(i) => &self.compounds[i],
        }
    }

    #[inline]
    pub fn compound_count(&self) -> usize {
        self.compounds.len()
    }

    #[inline]
    pub fn truncate(&mut self, compounds: usize) {
        self.compounds.truncate(compounds);
    }
}
"#
        .replace("__VARIANTS__", &variants)
        .replace("__ARMS__", &arms)
        .replace("__DEFAULT__", &projection.kind.default),
        projection,
    )
}

fn emit_typed_bbnf_builder(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
use bbnf_ir::registry::StructLayout;

use __MODULE__::arena::{__P__Arena, __P__Compound, __P__CompoundKind};
use __MODULE__::document::__P__Document;
use __MODULE__::value::__P__Value;
use crate::runtime::builder::StructBuilder;
use crate::runtime::handle::CompoundHandle;

#[derive(Debug, Clone)]
struct OpenFrame<'p> {
    kind: __P__CompoundKind,
    branch_tag: Option<u32>,
    start_offset: Option<u32>,
    end_offset: Option<u32>,
    children: Vec<__P__Value<'p>>,
}

#[derive(Debug)]
pub struct __P__StructBuilder<'p> {
    arena: __P__Arena<'p>,
    stack: Vec<OpenFrame<'p>>,
    root: Option<__P__Value<'p>>,
    next_handle: u64,
}

#[derive(Debug, Clone)]
pub struct __P__StructCheckpoint<'p> {
    compounds: usize,
    stack: Vec<OpenFrame<'p>>,
    root: Option<__P__Value<'p>>,
    next_handle: u64,
}

impl<'p> Default for __P__StructBuilder<'p> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'p> __P__StructBuilder<'p> {
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: __P__Arena::new(),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }

    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            arena: __P__Arena::with_capacity(compounds),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }

    #[inline]
    pub fn finalise(mut self, input: &'p str) -> __P__Document<'p> {
        debug_assert!(
            self.stack.is_empty(),
            "__P__StructBuilder::finalise called with {} open frame(s)",
            self.stack.len()
        );
        let root = self
            .root
            .take()
            .expect("__P__StructBuilder::finalise called before any value emission");
        __P__Document::new(self.arena, root, input)
    }

    #[inline]
    fn deposit(&mut self, value: __P__Value<'p>) {
        match self.stack.last_mut() {
            None => self.root = Some(value),
            Some(frame) => frame.children.push(value),
        }
    }
}

impl<'p> StructBuilder for __P__StructBuilder<'p> {
    type Checkpoint = __P__StructCheckpoint<'p>;

    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        __P__StructCheckpoint {
            compounds: self.arena.compound_count(),
            stack: self.stack.clone(),
            root: self.root,
            next_handle: self.next_handle,
        }
    }

    #[inline]
    fn rollback(&mut self, checkpoint: Self::Checkpoint) {
        self.arena.truncate(checkpoint.compounds);
        self.stack = checkpoint.stack;
        self.root = checkpoint.root;
        self.next_handle = checkpoint.next_handle;
    }

    fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle {
        self.stack.push(OpenFrame {
            kind: __P__CompoundKind::from_layout(layout),
            branch_tag: None,
            start_offset: None,
            end_offset: None,
            children: Vec::new(),
        });
        self.next_handle = self.next_handle.wrapping_add(1);
        CompoundHandle::new(self.next_handle, 0)
    }

    fn end_compound(&mut self, _handle: CompoundHandle) {
        let frame = self
            .stack
            .pop()
            .expect("__P__StructBuilder::end_compound on empty stack");
        let bounds = match (frame.start_offset, frame.end_offset) {
            (Some(start), Some(end)) => Some((start, end)),
            _ => None,
        };
        let id = self.arena.push_compound(__P__Compound {
            kind: frame.kind,
            branch_tag: frame.branch_tag,
            bounds,
            children: frame.children,
        });
        self.deposit(__P__Value::Compound(id));
    }

    #[inline]
    fn push_leaf_with_f64(&mut self, value: f64) {
        self.deposit(__P__Value::Float(value));
    }

    #[inline]
    fn push_leaf_with_i64(&mut self, value: i64) {
        self.deposit(__P__Value::Int(value));
    }

    #[inline]
    fn push_leaf_with_u64(&mut self, value: u64) {
        self.deposit(__P__Value::Int(value as i64));
    }

    #[inline]
    fn push_leaf_with_bool(&mut self, value: bool) {
        self.deposit(__P__Value::Bool(value));
    }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        let extended: &'p str = unsafe { std::mem::transmute(value) };
        self.deposit(__P__Value::Span(extended));
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.deposit(__P__Value::Unit);
    }

    #[inline]
    fn push_branch_tag(&mut self, branch_index: u32) {
        if let Some(frame) = self.stack.last_mut() {
            frame.branch_tag = Some(branch_index);
        }
    }

    #[inline]
    fn record_compound_bounds_start(&mut self, offset: u32) {
        if let Some(frame) = self.stack.last_mut() {
            frame.start_offset = Some(offset);
        }
    }

    #[inline]
    fn record_compound_bounds_end(&mut self, offset: u32) {
        if let Some(frame) = self.stack.last_mut() {
            frame.end_offset = Some(offset);
        }
    }
}
"#,
        projection,
    )
}

fn emit_typed_bbnf_value(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
use __MODULE__::arena::__P__CompoundId;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum __P__Value<'p> {
    Int(i64),
    Float(f64),
    Bool(bool),
    Span(&'p str),
    Tag(u8),
    Unit,
    Compound(__P__CompoundId),
}

impl<'p> Default for __P__Value<'p> {
    fn default() -> Self {
        __P__Value::Unit
    }
}
"#,
        projection,
    )
}

fn emit_typed_bbnf_document(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
use __MODULE__::arena::{__P__Arena, __P__Compound, __P__CompoundId, __P__CompoundKind};
use __MODULE__::value::__P__Value;
use crate::runtime::path::{Path, PathSegment};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum __P__Kind {
    Int,
    Float,
    Bool,
    Span,
    Tag,
    Unit,
    Compound,
}

#[derive(Debug)]
pub struct __P__Document<'p> {
    pub arena: __P__Arena<'p>,
    pub root: __P__Value<'p>,
    pub input: &'p str,
}

impl<'p> __P__Document<'p> {
    #[inline]
    pub fn new(arena: __P__Arena<'p>, root: __P__Value<'p>, input: &'p str) -> Self {
        Self { arena, root, input }
    }

    #[inline]
    pub fn root(&self) -> &__P__Value<'p> {
        &self.root
    }

    #[inline]
    pub fn arena(&self) -> &__P__Arena<'p> {
        &self.arena
    }

    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }

    #[inline]
    pub fn compound(&self, id: __P__CompoundId) -> &__P__Compound<'p> {
        self.arena.compound(id)
    }

    #[inline]
    pub fn view<'a>(&'a self) -> __P__View<'a, 'p> {
        __P__View {
            doc: self,
            focus: self.root,
        }
    }

    #[inline]
    pub fn to_value(&self) -> &__P__Value<'p> {
        &self.root
    }

    #[inline]
    pub fn get<T: __P__PathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct __P__View<'a, 'p: 'a> {
    pub(crate) doc: &'a __P__Document<'p>,
    pub(crate) focus: __P__Value<'p>,
}

impl<'a, 'p: 'a> __P__View<'a, 'p> {
    #[inline]
    pub fn focused(doc: &'a __P__Document<'p>, focus: __P__Value<'p>) -> Self {
        Self { doc, focus }
    }

    #[inline]
    pub fn document(&self) -> &'a __P__Document<'p> {
        self.doc
    }

    #[inline]
    pub fn focus(&self) -> __P__Value<'p> {
        self.focus
    }

    #[inline]
    pub fn root(&self) -> &'a __P__Value<'p> {
        &self.doc.root
    }

    #[inline]
    pub fn arena(&self) -> &'a __P__Arena<'p> {
        &self.doc.arena
    }

    #[inline]
    pub fn compound(&self, id: __P__CompoundId) -> &'a __P__Compound<'p> {
        self.doc.compound(id)
    }

    #[inline]
    pub fn kind(&self) -> __P__Kind {
        match self.focus {
            __P__Value::Int(_) => __P__Kind::Int,
            __P__Value::Float(_) => __P__Kind::Float,
            __P__Value::Bool(_) => __P__Kind::Bool,
            __P__Value::Span(_) => __P__Kind::Span,
            __P__Value::Tag(_) => __P__Kind::Tag,
            __P__Value::Unit => __P__Kind::Unit,
            __P__Value::Compound(_) => __P__Kind::Compound,
        }
    }

    #[inline]
    pub fn is_compound(&self) -> bool {
        matches!(self.focus, __P__Value::Compound(_))
    }

    #[inline]
    pub fn is_span(&self) -> bool {
        matches!(self.focus, __P__Value::Span(_))
    }

    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self.focus, __P__Value::Int(_) | __P__Value::Float(_))
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
        matches!(self.focus, __P__Value::Bool(_))
    }

    #[inline]
    pub fn is_tag(&self) -> bool {
        matches!(self.focus, __P__Value::Tag(_))
    }

    #[inline]
    pub fn is_unit(&self) -> bool {
        matches!(self.focus, __P__Value::Unit)
    }

    #[inline]
    pub fn input(&self) -> &'p str {
        self.doc.input
    }

    #[inline]
    pub fn num_children(&self) -> usize {
        match self.focus {
            __P__Value::Compound(id) => self.doc.compound(id).children.len(),
            _ => 0,
        }
    }

    pub fn span_range(&self) -> Option<(usize, usize)> {
        let input = self.doc.input;
        let input_start = input.as_ptr() as usize;
        let input_end = input_start + input.len();
        let mut acc: Option<(usize, usize)> = None;
        self.fold_span_range(input_start, input_end, &mut acc);
        acc
    }

    fn fold_span_range(
        &self,
        input_start: usize,
        input_end: usize,
        acc: &mut Option<(usize, usize)>,
    ) {
        match self.focus {
            __P__Value::Span(s) => {
                let s_start = s.as_ptr() as usize;
                let s_end = s_start + s.len();
                if s_start < input_start || s_end > input_end {
                    return;
                }
                let lo = s_start - input_start;
                let hi = s_end - input_start;
                *acc = Some(match *acc {
                    None => (lo, hi),
                    Some((a, b)) => (a.min(lo), b.max(hi)),
                });
            }
            __P__Value::Compound(_) => {
                for child in self.children_iter() {
                    child.fold_span_range(input_start, input_end, acc);
                }
            }
            _ => {}
        }
    }

    #[inline]
    pub fn children_iter(&self) -> __P__ChildrenSlice<'a, 'p> {
        match self.focus {
            __P__Value::Compound(id) => __P__ChildrenSlice {
                doc: self.doc,
                children: &self.doc.compound(id).children,
                index: 0,
            },
            _ => __P__ChildrenSlice {
                doc: self.doc,
                children: &[],
                index: 0,
            },
        }
    }

    pub fn find_descendant_by_kind(
        &self,
        target: __P__CompoundKind,
    ) -> Option<__P__View<'a, 'p>> {
        if self.compound_kind() == Some(target) {
            return Some(*self);
        }
        for child in self.children_iter() {
            if let Some(found) = child.find_descendant_by_kind(target) {
                return Some(found);
            }
        }
        None
    }

    #[inline]
    pub fn iter_children(&self) -> __P__ChildrenSlice<'a, 'p> {
        self.children_iter()
    }
}

#[derive(Clone)]
pub struct __P__ChildrenSlice<'a, 'p: 'a> {
    doc: &'a __P__Document<'p>,
    children: &'a [__P__Value<'p>],
    index: usize,
}

impl<'a, 'p: 'a> Iterator for __P__ChildrenSlice<'a, 'p> {
    type Item = __P__View<'a, 'p>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let value = self.children.get(self.index)?;
        self.index += 1;
        Some(__P__View::focused(self.doc, *value))
    }
}

pub trait __P__PathQuery: Sized {
    fn query<'p>(doc: &__P__Document<'p>, path: Path<'_>) -> Option<Self>;
}

#[inline]
fn walk_path<'a, 'p>(doc: &'a __P__Document<'p>, path: Path<'_>) -> Option<&'a __P__Value<'p>> {
    let mut current: &'a __P__Value<'p> = &doc.root;
    for segment in path.iter() {
        current = match (current, segment) {
            (__P__Value::Compound(id), PathSegment::Index(idx)) => {
                let entry = doc.compound(*id);
                entry.children.get(*idx)?
            }
            (__P__Value::Compound(_), PathSegment::Field(_)) => return None,
            _ => return None,
        };
    }
    Some(current)
}

impl __P__PathQuery for &str {
    #[inline]
    fn query<'p>(doc: &__P__Document<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            __P__Value::Span(s) => {
                let extended: &'p str = *s;
                Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
            }
            _ => None,
        }
    }
}

impl __P__PathQuery for i64 {
    #[inline]
    fn query<'p>(doc: &__P__Document<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            __P__Value::Int(v) => Some(*v),
            _ => None,
        }
    }
}

impl __P__PathQuery for f64 {
    #[inline]
    fn query<'p>(doc: &__P__Document<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            __P__Value::Float(v) => Some(*v),
            __P__Value::Int(v) => Some(*v as f64),
            _ => None,
        }
    }
}

impl __P__PathQuery for bool {
    #[inline]
    fn query<'p>(doc: &__P__Document<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            __P__Value::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

impl __P__PathQuery for u8 {
    #[inline]
    fn query<'p>(doc: &__P__Document<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            __P__Value::Tag(t) => Some(*t),
            _ => None,
        }
    }
}

impl __P__PathQuery for __P__Value<'_> {
    #[inline]
    fn query<'p>(doc: &__P__Document<'p>, path: Path<'_>) -> Option<Self> {
        let value = walk_path(doc, path)?;
        let copied: __P__Value<'p> = *value;
        Some(unsafe { core::mem::transmute::<__P__Value<'p>, __P__Value<'_>>(copied) })
    }
}
"#,
        projection,
    )
}

fn emit_typed_bbnf_view(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
use crate::runtime::RuntimeView;
use __MODULE__::arena::{__P__Compound, __P__CompoundKind};
use __MODULE__::document::{__P__Document, __P__Kind, __P__View};
use __MODULE__::value::__P__Value;

impl<'a, 'p: 'a> RuntimeView<'p> for __P__View<'a, 'p> {
    type Kind = __P__Kind;

    #[inline]
    fn kind(&self) -> Self::Kind {
        match self.focus {
            __P__Value::Int(_) => __P__Kind::Int,
            __P__Value::Float(_) => __P__Kind::Float,
            __P__Value::Bool(_) => __P__Kind::Bool,
            __P__Value::Span(_) => __P__Kind::Span,
            __P__Value::Tag(_) => __P__Kind::Tag,
            __P__Value::Unit => __P__Kind::Unit,
            __P__Value::Compound(_) => __P__Kind::Compound,
        }
    }

    #[inline]
    fn span(&self) -> Option<&'p str> {
        match self.focus {
            __P__Value::Span(s) => Some(s),
            _ => None,
        }
    }

    #[inline]
    fn input(&self) -> &'p str {
        self.doc.input
    }

    fn children(&self) -> impl Iterator<Item = Self> + '_ {
        let doc = self.doc;
        let focus = self.focus;
        __P__ChildrenIter {
            doc,
            focus,
            index: 0,
        }
    }
}

impl<'a, 'p: 'a> __P__View<'a, 'p> {
    pub fn byte_span(&self) -> Option<(u32, u32)> {
        compute_byte_span(self.doc, self.focus)
    }

    #[inline]
    pub fn span_text(&self) -> &'p str {
        match self.byte_span() {
            Some((lo, hi)) if hi >= lo => &self.doc.input[lo as usize..hi as usize],
            _ => "",
        }
    }

    #[inline]
    pub fn span_text_opt(&self) -> Option<&'p str> {
        match self.byte_span() {
            Some((lo, hi)) if hi >= lo => Some(&self.doc.input[lo as usize..hi as usize]),
            _ => None,
        }
    }

    #[inline]
    pub fn span_bounds(&self) -> Option<(u32, u32)> {
        self.byte_span()
    }

    pub fn child(&self, i: usize) -> Option<__P__View<'a, 'p>> {
        match self.focus {
            __P__Value::Compound(id) => self
                .doc
                .compound(id)
                .children
                .get(i)
                .map(|v| __P__View::focused(self.doc, *v)),
            _ => None,
        }
    }

    #[inline]
    pub fn compound_kind(&self) -> Option<__P__CompoundKind> {
        match self.focus {
            __P__Value::Compound(id) => Some(self.doc.compound(id).kind),
            _ => None,
        }
    }

    #[inline]
    pub fn branch_tag(&self) -> Option<u32> {
        match self.focus {
            __P__Value::Compound(id) => self.doc.compound(id).branch_tag,
            _ => None,
        }
    }

    #[inline]
    pub fn is_compound_kind(&self, kind: __P__CompoundKind) -> bool {
        self.compound_kind() == Some(kind)
    }

    #[inline]
    pub fn compound_identity(&self) -> Option<usize> {
        match self.focus {
            __P__Value::Compound(id) => Some(self.doc.compound(id) as *const _ as usize),
            _ => None,
        }
    }

    #[inline]
    pub fn compound_entry(&self) -> Option<&'a __P__Compound<'p>> {
        match self.focus {
            __P__Value::Compound(id) => Some(self.doc.compound(id)),
            _ => None,
        }
    }
}

fn compute_byte_span<'p>(doc: &__P__Document<'p>, focus: __P__Value<'p>) -> Option<(u32, u32)> {
    match focus {
        __P__Value::Span(s) => {
            let input_ptr = doc.input.as_ptr() as usize;
            let s_ptr = s.as_ptr() as usize;
            if s_ptr < input_ptr {
                return None;
            }
            let lo = (s_ptr - input_ptr) as u32;
            let hi = lo + s.len() as u32;
            Some((lo, hi))
        }
        __P__Value::Compound(id) => {
            let entry = doc.compound(id);
            if let Some(bounds) = entry.bounds {
                return Some(bounds);
            }
            let mut lo: Option<u32> = None;
            let mut hi: Option<u32> = None;
            for child in &entry.children {
                if let Some((clo, chi)) = compute_byte_span(doc, *child) {
                    lo = Some(lo.map_or(clo, |existing| existing.min(clo)));
                    hi = Some(hi.map_or(chi, |existing| existing.max(chi)));
                }
            }
            match (lo, hi) {
                (Some(l), Some(h)) => Some((l, h)),
                _ => None,
            }
        }
        _ => None,
    }
}

pub struct __P__ChildrenIter<'a, 'p: 'a> {
    doc: &'a __P__Document<'p>,
    focus: __P__Value<'p>,
    index: usize,
}

impl<'a, 'p: 'a> Iterator for __P__ChildrenIter<'a, 'p> {
    type Item = __P__View<'a, 'p>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.focus {
            __P__Value::Compound(id) => {
                let entry = self.doc.compound(id);
                let item = entry.children.get(self.index)?;
                self.index += 1;
                Some(__P__View::focused(self.doc, *item))
            }
            _ => None,
        }
    }
}
"#,
        projection,
    )
}

fn emit_typed_bbnf_parse_with(projection: &RuntimeProjection) -> String {
    runtime_template(
        &r#"
use super::document::{__P__Document, __P__PathQuery};
use crate::grammar::generated::__MODULE__::__path_plan;
use crate::grammar::generated::__MODULE__::{
    __shape_support___PARSER__, parse___PARSER_____ENTRY__,
};
use crate::path::cursor::Decision;
use crate::path::executor::PathExecutor;
use crate::path::ir::{PathSegment as TypedSegment, TypedPath};
use crate::path::markers::__P__;
use crate::runtime::__MODULE__::__P__StructBuilder;
use crate::runtime::path::{Path, PathSegment};

fn lower<'a>(seg: &TypedSegment<'a>) -> Option<PathSegment<'a>> {
    match seg {
        TypedSegment::Field(s) => Some(PathSegment::Field(s)),
        TypedSegment::Index(i) => Some(PathSegment::Index(*i)),
        TypedSegment::VariantName(s) => Some(PathSegment::Field(s)),
        TypedSegment::Wildcard => None,
    }
}

pub fn parse_with<T>(input: &str, path: &TypedPath<__P__, T>) -> Option<T>
where
    T: __P__PathQuery,
{
    PathExecutor::execute(
        input,
        path,
        |rule_id, kind, _idx| {
            __path_plan::lookup(rule_id, kind)
                .map(|e| e.decision)
                .unwrap_or(Decision::ParseFully)
        },
        |src, cursor| {
            let mut state = __shape_support___PARSER__::ScanState::new();
            let mut builder = __P__StructBuilder::new();
            let mut pos: usize = 0;
            parse___PARSER_____ENTRY__(
                src.as_bytes(),
                &mut pos,
                &mut state,
                &mut builder,
                cursor,
            )
            .ok()?;
            if path.is_empty() {
                let bytes = src.as_bytes();
                let mut leading = 0usize;
                while let Some(&b) = bytes.get(leading) {
                    if matches!(b, b' ' | b'\t' | b'\n' | b'\r') {
                        leading += 1;
                    } else {
                        break;
                    }
                }
                if pos <= leading && leading < bytes.len() {
                    return None;
                }
            }
            let doc: __P__Document<'_> = builder.finalise(src);
            let mut segments: Vec<PathSegment<'_>> = Vec::with_capacity(path.len());
            for owned in path.owned_segments() {
                segments.push(lower(&owned.as_borrowed())?);
            }
            doc.get::<T>(Path::new(&segments))
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::path::ir::TypedPath;
    use crate::runtime::__MODULE__::value::__P__Value;

    #[test]
    fn parse_with_resolves_root_value() {
        let path: TypedPath<__P__, __P__Value<'_>> = TypedPath::from_owned(Vec::new());
        let out = parse_with::<__P__Value<'_>>("a = b ;\n", &path);
        assert!(out.is_some(), "__P__ root should resolve as identity");
    }

    #[test]
    fn parse_with_returns_none_on_invalid_input() {
        let path: TypedPath<__P__, __P__Value<'_>> = TypedPath::from_owned(Vec::new());
        let out = parse_with::<__P__Value<'_>>("@@@ not bbnf @@@", &path);
        assert!(out.is_none());
    }
}
"#
        .replace("generated::__MODULE__", "generated::bbnf")
        .replace("runtime::__MODULE__", "runtime::bbnf"),
        projection,
    )
}

fn emit_typed_bbnf_mod(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
pub mod arena;
pub mod builder;
pub mod document;
pub mod parse_with;
pub mod serialize;
pub mod value;
pub mod view;

pub use arena::{__P__Arena, __P__Compound, __P__CompoundId, __P__CompoundKind};
pub use builder::__P__StructBuilder;
pub use document::{__P__Document, __P__Kind, __P__PathQuery, __P__View};
pub use parse_with::parse_with;
pub use serialize::serialize_compact_doc;
pub use value::__P__Value;
"#,
        projection,
    )
}

fn emit_typed_bbnf_serialize(projection: &RuntimeProjection) -> String {
    runtime_template(
        r#"
use std::fmt::Write;

use __MODULE__::arena::{__P__CompoundId, __P__CompoundKind};
use __MODULE__::document::__P__Document;
use __MODULE__::value::__P__Value;

pub fn serialize_compact_doc<'p>(doc: &__P__Document<'p>) -> String {
    let mut out = String::new();
    emit_value(doc, &doc.root, &mut out);
    out
}

fn emit_value<'p>(doc: &__P__Document<'p>, value: &__P__Value<'p>, out: &mut String) {
    match value {
        __P__Value::Span(s) => out.push_str(s),
        __P__Value::Int(i) => write!(out, "{i}").unwrap(),
        __P__Value::Float(f) => write!(out, "{f}").unwrap(),
        __P__Value::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        __P__Value::Tag(t) => write!(out, "{t}").unwrap(),
        __P__Value::Unit => {}
        __P__Value::Compound(id) => emit_compound(doc, *id, out),
    }
}

fn emit_compound<'p>(doc: &__P__Document<'p>, id: __P__CompoundId, out: &mut String) {
    let compound = doc.compound(id);
    match compound.kind {
        __P__CompoundKind::Grammar => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push('\n');
                }
                emit_value(doc, child, out);
            }
        }
        __P__CompoundKind::GrammarItem
        | __P__CompoundKind::Lhs
        | __P__CompoundKind::Rhs
        | __P__CompoundKind::Directive
        | __P__CompoundKind::Other => {
            for child in &compound.children {
                emit_value(doc, child, out);
            }
        }
        __P__CompoundKind::Rule => {
            let mut iter = compound.children.iter();
            if let Some(lhs) = iter.next() {
                emit_value(doc, lhs, out);
            }
            out.push_str(" = ");
            if let Some(rhs) = iter.next() {
                emit_value(doc, rhs, out);
            }
            out.push_str(" ;");
        }
        __P__CompoundKind::Alternation => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(" | ");
                }
                emit_value(doc, child, out);
            }
        }
        __P__CompoundKind::Concatenation => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(" , ");
                }
                emit_value(doc, child, out);
            }
        }
        __P__CompoundKind::BinaryFactor => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(" - ");
                }
                emit_value(doc, child, out);
            }
        }
        __P__CompoundKind::MappedFactor => {
            for (i, child) in compound.children.iter().enumerate() {
                if i == 1 {
                    out.push_str(" -> ");
                }
                emit_value(doc, child, out);
            }
        }
        __P__CompoundKind::Factor => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
        }
        __P__CompoundKind::Term => {
            let children = &compound.children;
            if let Some(first) = children.first() {
                emit_value(doc, first, out);
                if children.len() > 1 {
                    let starts_bracket = match first {
                        __P__Value::Span(s) => {
                            s.starts_with('(')
                                || s.starts_with('[')
                                || s.starts_with('{')
                                || s.starts_with("@{")
                        }
                        _ => false,
                    };
                    if starts_bracket {
                        for child in children.iter().skip(1) {
                            emit_value(doc, child, out);
                        }
                    } else {
                        out.push('(');
                        for (i, child) in children.iter().skip(1).enumerate() {
                            if i > 0 {
                                out.push_str(", ");
                            }
                            emit_value(doc, child, out);
                        }
                        out.push(')');
                    }
                }
            }
        }
        __P__CompoundKind::Closure => {
            let n = compound.children.len();
            if n == 0 {
                return;
            }
            out.push('|');
            for (i, child) in compound.children.iter().take(n - 1).enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_value(doc, child, out);
            }
            out.push_str("| ");
            emit_value(doc, &compound.children[n - 1], out);
        }
        __P__CompoundKind::CallArg => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(" | ");
                }
                emit_value(doc, child, out);
            }
        }
        __P__CompoundKind::ImportPath => {
            for child in &compound.children {
                if let __P__Value::Span(s) = child {
                    if s.starts_with('"') && s.ends_with('"') {
                        out.push_str(s);
                    } else {
                        out.push('"');
                        out.push_str(s);
                        out.push('"');
                    }
                } else {
                    emit_value(doc, child, out);
                }
            }
        }
        __P__CompoundKind::ImportItems => {
            out.push_str("{ ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_value(doc, child, out);
            }
            out.push_str(" }");
        }
        __P__CompoundKind::ImportDirective => {
            out.push_str("@import ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    let prev_was_items = matches!(
                        &compound.children[i - 1],
                        __P__Value::Compound(cid)
                            if doc.compound(*cid).kind == __P__CompoundKind::ImportItems
                    );
                    let this_is_path = matches!(
                        child,
                        __P__Value::Compound(cid)
                            if doc.compound(*cid).kind == __P__CompoundKind::ImportPath
                    );
                    if prev_was_items && this_is_path {
                        out.push_str(" from ");
                    } else {
                        out.push(' ');
                    }
                }
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        __P__CompoundKind::PrettyHint => {
            let mut iter = compound.children.iter();
            if let Some(first) = iter.next() {
                emit_value(doc, first, out);
            }
            if let Some(arg) = iter.next() {
                out.push('(');
                emit_value(doc, arg, out);
                out.push(')');
            }
        }
        __P__CompoundKind::PrettyDirective => {
            out.push_str("@pretty ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        __P__CompoundKind::WsDirective => {
            out.push_str("@ws ");
            for child in &compound.children {
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        __P__CompoundKind::TokenDirective => {
            out.push_str("@token ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        __P__CompoundKind::DebugDirective => {
            out.push_str("@debug ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        __P__CompoundKind::HostDirective => {
            out.push_str("@host ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        __P__CompoundKind::RecoverDirective => {
            out.push_str("@recover ");
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
            out.push_str(" ;");
        }
        __P__CompoundKind::ValueExpr
        | __P__CompoundKind::ValueClosure
        | __P__CompoundKind::ValueOr
        | __P__CompoundKind::ValueAnd
        | __P__CompoundKind::ValueCmp
        | __P__CompoundKind::ValueAdd
        | __P__CompoundKind::ValueMul
        | __P__CompoundKind::ValueUnary
        | __P__CompoundKind::ValueAtom
        | __P__CompoundKind::ValuePath
        | __P__CompoundKind::ValueInput
        | __P__CompoundKind::ValueFnCall => {
            for (i, child) in compound.children.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                emit_value(doc, child, out);
            }
        }
        __P__CompoundKind::TypeAnnotation => {
            out.push(':');
            out.push(' ');
            for child in &compound.children {
                emit_value(doc, child, out);
            }
        }
    }
}
"#,
        projection,
    )
}

fn emit_arena(projection: &RuntimeProjection) -> String {
    let p = &projection.type_prefix;
    let module = &projection.runtime_module;
    format!(
        r#"
use crate::runtime::arena_template::CompoundSlabArena;
use {module}::kind::{p}Compound;

#[derive(Debug, Default)]
pub struct {p}Arena<'p>(CompoundSlabArena<{p}Compound<'p>>);

impl<'p> {p}Arena<'p> {{
    #[inline]
    pub fn new() -> Self {{
        Self(CompoundSlabArena::new())
    }}

    #[inline]
    pub fn with_capacity(n: usize) -> Self {{
        Self(CompoundSlabArena::with_capacity(n))
    }}

    #[inline]
    pub(crate) fn from_template(t: CompoundSlabArena<{p}Compound<'p>>) -> Self {{
        Self(t)
    }}

    #[inline]
    pub fn push_compound(&mut self, c: {p}Compound<'p>) -> {p}CompoundId {{
        {p}CompoundId(self.0.push_compound(c))
    }}

    #[inline]
    pub fn compound(&self, id: {p}CompoundId) -> &{p}Compound<'p> {{
        self.0.compound(id.0)
    }}

    #[inline]
    pub fn compound_count(&self) -> usize {{
        self.0.compound_count()
    }}

    #[inline]
    pub fn truncate(&mut self, n: usize) {{
        self.0.truncate(n);
    }}
}}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct {p}CompoundId(u32);

impl {p}CompoundId {{
    pub const EMPTY: Self = Self(0);

    #[inline]
    pub const fn is_empty(self) -> bool {{
        self.0 == 0
    }}

    #[inline]
    pub(crate) const fn from_raw(id: u32) -> Self {{
        Self(id)
    }}
}}
"#
    )
}

fn emit_builder(projection: &RuntimeProjection) -> String {
    let p = &projection.type_prefix;
    let module = &projection.runtime_module;
    format!(
        r#"
use bbnf_ir::registry::StructLayout;

use crate::runtime::builder_template::{{SimpleCompound, SimpleStructBuilder, SimpleValue}};
use {module}::arena::{{{p}Arena, {p}CompoundId}};
use {module}::document::{p}Document;
use {module}::kind::{{{p}Compound, {p}CompoundKind}};
use {module}::value::{p}Value;

impl<'p> SimpleValue<'p> for {p}Value<'p> {{
    #[inline]
    fn from_span(s: &'p str) -> Self {{
        Self::Span(s)
    }}

    #[inline]
    fn unit() -> Self {{
        Self::Unit
    }}

    #[inline]
    fn from_compound_index(id_plus_one: u32) -> Self {{
        Self::Compound({p}CompoundId::from_raw(id_plus_one))
    }}
}}

impl<'p> SimpleCompound<'p, {p}Value<'p>> for {p}Compound<'p> {{
    #[inline]
    fn new_entry(
        layout: &StructLayout,
        branch_tag: Option<u32>,
        children: Vec<{p}Value<'p>>,
    ) -> Self {{
        Self {{
            kind: {p}CompoundKind::from_layout(layout),
            branch_tag,
            children,
        }}
    }}
}}

pub type {p}StructBuilder<'p> = SimpleStructBuilder<'p, {p}Value<'p>, {p}Compound<'p>>;
pub type {p}StructCheckpoint<'p> =
    crate::runtime::builder_template::SimpleCheckpoint<'p, {p}Value<'p>>;

impl<'p> {p}StructBuilder<'p> {{
    #[inline]
    pub fn finalise(self, input: &'p str) -> {p}Document<'p> {{
        let (template_arena, root) = self.into_finalised();
        {p}Document::new({p}Arena::from_template(template_arena), root, input)
    }}
}}
"#
    )
}

fn emit_document(projection: &RuntimeProjection) -> String {
    let p = &projection.type_prefix;
    let module = &projection.runtime_module;
    format!(
        r#"
use {module}::arena::{{{p}Arena, {p}CompoundId}};
use {module}::kind::{{{p}Compound, {p}CompoundKind}};
use {module}::value::{p}Value;
use crate::runtime::path::{{Path, PathSegment}};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum {p}Kind {{
    Span,
    Unit,
    Compound,
}}

#[derive(Debug)]
pub struct {p}Document<'p> {{
    pub arena: {p}Arena<'p>,
    pub root: {p}Value<'p>,
    pub input: &'p str,
}}

impl<'p> {p}Document<'p> {{
    #[inline]
    pub fn new(arena: {p}Arena<'p>, root: {p}Value<'p>, input: &'p str) -> Self {{
        Self {{ arena, root, input }}
    }}

    #[inline]
    pub fn root(&self) -> &{p}Value<'p> {{
        &self.root
    }}

    #[inline]
    pub fn arena(&self) -> &{p}Arena<'p> {{
        &self.arena
    }}

    #[inline]
    pub fn input(&self) -> &'p str {{
        self.input
    }}

    #[inline]
    pub fn compound(&self, id: {p}CompoundId) -> &{p}Compound<'p> {{
        self.arena.compound(id)
    }}

    #[inline]
    pub fn view<'a>(&'a self) -> {p}View<'a, 'p> {{
        {p}View {{
            doc: self,
            focus: self.root,
        }}
    }}

    #[inline]
    pub fn to_value(&self) -> &{p}Value<'p> {{
        &self.root
    }}

    #[inline]
    pub fn get<T: {p}PathQuery>(&self, path: Path<'_>) -> Option<T> {{
        T::query(self, path)
    }}
}}

#[derive(Debug, Clone, Copy)]
pub struct {p}View<'a, 'p: 'a> {{
    pub(crate) doc: &'a {p}Document<'p>,
    pub(crate) focus: {p}Value<'p>,
}}

impl<'a, 'p: 'a> {p}View<'a, 'p> {{
    #[inline]
    pub fn focused(doc: &'a {p}Document<'p>, focus: {p}Value<'p>) -> Self {{
        Self {{ doc, focus }}
    }}

    #[inline]
    pub fn document(&self) -> &'a {p}Document<'p> {{
        self.doc
    }}

    #[inline]
    pub fn focus(&self) -> {p}Value<'p> {{
        self.focus
    }}

    #[inline]
    pub fn root(&self) -> &'a {p}Value<'p> {{
        &self.doc.root
    }}

    #[inline]
    pub fn arena(&self) -> &'a {p}Arena<'p> {{
        &self.doc.arena
    }}

    #[inline]
    pub fn compound(&self, id: {p}CompoundId) -> &'a {p}Compound<'p> {{
        self.doc.compound(id)
    }}

    #[inline]
    pub fn kind(&self) -> {p}Kind {{
        match &self.focus {{
            {p}Value::Span(_) => {p}Kind::Span,
            {p}Value::Unit => {p}Kind::Unit,
            {p}Value::Compound(_) => {p}Kind::Compound,
        }}
    }}

    #[inline]
    pub fn is_compound(&self) -> bool {{
        matches!(self.focus, {p}Value::Compound(_))
    }}

    #[inline]
    pub fn is_span(&self) -> bool {{
        matches!(self.focus, {p}Value::Span(_))
    }}

    #[inline]
    pub fn input(&self) -> &'p str {{
        self.doc.input
    }}

    #[inline]
    pub fn compound_kind(&self) -> Option<{p}CompoundKind> {{
        match self.focus {{
            {p}Value::Compound(id) => Some(self.doc.compound(id).kind),
            _ => None,
        }}
    }}
}}

pub trait {p}PathQuery: Sized {{
    fn query<'p>(doc: &{p}Document<'p>, path: Path<'_>) -> Option<Self>;
}}

#[inline]
fn walk_path<'a, 'p>(doc: &'a {p}Document<'p>, path: Path<'_>) -> Option<&'a {p}Value<'p>> {{
    let mut current: &'a {p}Value<'p> = &doc.root;
    for segment in path.iter() {{
        current = match (current, segment) {{
            ({p}Value::Compound(id), PathSegment::Index(idx)) => {{
                let entry = doc.compound(*id);
                entry.children.get(*idx)?
            }}
            ({p}Value::Compound(_), PathSegment::Field(_)) => return None,
            _ => return None,
        }};
    }}
    Some(current)
}}

impl {p}PathQuery for &str {{
    #[inline]
    fn query<'p>(doc: &{p}Document<'p>, path: Path<'_>) -> Option<Self> {{
        match walk_path(doc, path)? {{
            {p}Value::Span(s) => {{
                let extended: &'p str = *s;
                Some(unsafe {{ core::mem::transmute::<&'p str, &str>(extended) }})
            }}
            _ => None,
        }}
    }}
}}

impl {p}PathQuery for {p}Value<'_> {{
    #[inline]
    fn query<'p>(doc: &{p}Document<'p>, path: Path<'_>) -> Option<Self> {{
        let value = walk_path(doc, path)?;
        let copied: {p}Value<'p> = *value;
        Some(unsafe {{ core::mem::transmute::<{p}Value<'p>, {p}Value<'_>>(copied) }})
    }}
}}
"#
    )
}

fn emit_kind(projection: &RuntimeProjection, routes: &[ResolvedKindRoute]) -> String {
    let p = &projection.type_prefix;
    let module = &projection.runtime_module;
    let variants = projection
        .kind
        .variants
        .iter()
        .map(|variant| format!("    {variant},"))
        .collect::<Vec<_>>()
        .join("\n");
    let from_layout = if routes.is_empty() {
        format!(
            r#"
    pub fn from_layout(_layout: &StructLayout) -> Self {{
        Self::{}
    }}
"#,
            projection.kind.default
        )
    } else {
        let arms = routes
            .iter()
            .map(|route| format!("            {} => Self::{},", route.rule_id, route.kind))
            .collect::<Vec<_>>()
            .join("\n");
        format!(
            r#"
    pub fn from_layout(layout: &StructLayout) -> Self {{
        match layout.rule_id {{
{arms}
            _ => Self::{},
        }}
    }}
"#,
            projection.kind.default
        )
    };
    format!(
        r#"
use bbnf_ir::registry::StructLayout;

use {module}::value::{p}Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum {p}CompoundKind {{
{variants}
}}

impl {p}CompoundKind {{
{from_layout}
}}

#[derive(Debug, Clone)]
pub struct {p}Compound<'p> {{
    pub kind: {p}CompoundKind,
    pub branch_tag: Option<u32>,
    pub children: Vec<{p}Value<'p>>,
}}

impl<'p> Default for {p}Compound<'p> {{
    fn default() -> Self {{
        Self {{
            kind: {p}CompoundKind::{},
            branch_tag: None,
            children: Vec::new(),
        }}
    }}
}}
"#,
        projection.kind.default
    )
}

fn emit_mod(projection: &RuntimeProjection) -> String {
    let p = &projection.type_prefix;
    format!(
        r#"
pub mod arena;
pub mod builder;
pub mod document;
pub mod kind;
pub mod value;
pub mod view;

pub use arena::{{{p}Arena, {p}CompoundId}};
pub use builder::{p}StructBuilder;
pub use document::{{{p}Document, {p}Kind, {p}PathQuery, {p}View}};
pub use kind::{{{p}Compound, {p}CompoundKind}};
pub use value::{p}Value;
"#
    )
}

fn emit_value(projection: &RuntimeProjection) -> String {
    let p = &projection.type_prefix;
    let module = &projection.runtime_module;
    format!(
        r#"
use {module}::arena::{p}CompoundId;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum {p}Value<'p> {{
    Span(&'p str),
    Unit,
    Compound({p}CompoundId),
}}

impl<'p> Default for {p}Value<'p> {{
    fn default() -> Self {{
        Self::Unit
    }}
}}
"#
    )
}

fn emit_view(projection: &RuntimeProjection) -> String {
    let p = &projection.type_prefix;
    let module = &projection.runtime_module;
    format!(
        r#"
use crate::runtime::RuntimeView;
use {module}::document::{{{p}Document, {p}Kind, {p}View}};
use {module}::value::{p}Value;

impl<'a, 'p: 'a> RuntimeView<'p> for {p}View<'a, 'p> {{
    type Kind = {p}Kind;

    #[inline]
    fn kind(&self) -> Self::Kind {{
        match self.focus {{
            {p}Value::Span(_) => {p}Kind::Span,
            {p}Value::Unit => {p}Kind::Unit,
            {p}Value::Compound(_) => {p}Kind::Compound,
        }}
    }}

    #[inline]
    fn span(&self) -> Option<&'p str> {{
        match self.focus {{
            {p}Value::Span(s) => Some(s),
            _ => None,
        }}
    }}

    #[inline]
    fn input(&self) -> &'p str {{
        self.doc.input
    }}

    fn children(&self) -> impl Iterator<Item = Self> + '_ {{
        let doc = self.doc;
        let focus = self.focus;
        {p}ChildrenIter {{
            doc,
            focus,
            index: 0,
        }}
    }}
}}

pub struct {p}ChildrenIter<'a, 'p: 'a> {{
    doc: &'a {p}Document<'p>,
    focus: {p}Value<'p>,
    index: usize,
}}

impl<'a, 'p: 'a> Iterator for {p}ChildrenIter<'a, 'p> {{
    type Item = {p}View<'a, 'p>;

    fn next(&mut self) -> Option<Self::Item> {{
        match self.focus {{
            {p}Value::Compound(id) => {{
                let entry = self.doc.compound(id);
                let item = entry.children.get(self.index)?;
                self.index += 1;
                Some({p}View::focused(self.doc, *item))
            }}
            _ => None,
        }}
    }}
}}
"#
    )
}
