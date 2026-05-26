//! Root simple-runtime projection emitter.
//!
//! `cargo xtask regen-math` regenerates the root math runtime from the
//! declarative projection source at `xtask/runtime-projections/math.toml`
//! plus the current grammar registry sidecar. The sidecar supplies
//! current rule ids; the projection supplies the runtime surface.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, anyhow, bail};
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
    kind: KindProjection,
}

#[derive(Debug, serde::Deserialize)]
struct KindProjection {
    default: String,
    layout_mode: LayoutMode,
    variants: Vec<String>,
    #[serde(default)]
    routes: Vec<KindRoute>,
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

    let output_dir = workspace_root.join(&projection.output_dir);
    std::fs::create_dir_all(&output_dir)
        .with_context(|| format!("create `{}`", output_dir.display()))?;

    let files = [
        ("arena.rs", emit_arena(&projection)),
        ("builder.rs", emit_builder(&projection)),
        ("document.rs", emit_document(&projection)),
        ("kind.rs", emit_kind(&projection, &kind_routes)),
        ("mod.rs", emit_mod(&projection)),
        ("value.rs", emit_value(&projection)),
        ("view.rs", emit_view(&projection)),
    ];

    for (name, source) in files {
        let path = output_dir.join(name);
        write_if_changed(&path, &format_rust(&source, &path)?)?;
    }

    Ok(())
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
    let expected_parser = format!("{}Parser", projection.type_prefix);
    if projection.parser_type != expected_parser {
        bail!(
            "simple runtime projection `{}` has parser_type `{}`, expected `{expected_parser}`",
            projection.grammar,
            projection.parser_type
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
