//! Root CSS L4 runtime projection emitter.
//!
//! `cargo xtask regen-css` regenerates the root
//! `crates/core/src/runtime/css_l4/` runtime from the declarative
//! projection source at `xtask/runtime-projections/css_l4.toml` plus
//! the current CSS L4 registry sidecar. The sidecar supplies current
//! rule ids; the projection supplies CSS runtime semantics.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, anyhow, bail};
use bbnf_ir::registry::StructRegistry;

#[derive(Debug, serde::Deserialize)]
struct RuntimeProjection {
    schema_version: u32,
    grammar: String,
    entry_rule: String,
    runtime_module: String,
    output_dir: String,
    parser_type: String,
    parser_entry: String,
    shape_support: String,
    marker: String,
    builder_type: String,
    document: String,
    module: ModuleProjection,
    #[serde(default)]
    repr_enums: Vec<ReprEnumSpec>,
    #[serde(default)]
    records: Vec<RecordSpec>,
    #[serde(default)]
    sum_enums: Vec<SumEnumSpec>,
    arena: ArenaSpec,
    builder: BuilderSpec,
}

#[derive(Debug, serde::Deserialize)]
struct ModuleProjection {
    modules: Vec<String>,
    arena_exports: Vec<String>,
    document_exports: Vec<String>,
    value_exports: Vec<String>,
}

#[derive(Debug, serde::Deserialize)]
struct ReprEnumSpec {
    name: String,
    decode: DecodeMode,
    variants: Vec<DiscriminantVariant>,
    unknown: Option<UnknownVariant>,
}

#[derive(Debug, serde::Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
enum DecodeMode {
    #[serde(rename = "self", alias = "self_")]
    Self_,
    Option,
}

impl DecodeMode {
    fn is_self(&self) -> bool {
        matches!(self, Self::Self_)
    }
}

#[derive(Debug, serde::Deserialize)]
struct DiscriminantVariant {
    name: String,
    value: u8,
}

#[derive(Debug, serde::Deserialize)]
struct UnknownVariant {
    name: String,
    payload: String,
}

#[derive(Debug, serde::Deserialize)]
struct RecordSpec {
    name: String,
    #[serde(default)]
    lifetime: bool,
    fields: Vec<FieldSpec>,
}

#[derive(Debug, serde::Deserialize)]
struct FieldSpec {
    name: String,
    ty: String,
}

#[derive(Debug, serde::Deserialize)]
struct SumEnumSpec {
    name: String,
    #[serde(default)]
    lifetime: bool,
    variants: Vec<SumVariantSpec>,
}

#[derive(Debug, serde::Deserialize)]
struct SumVariantSpec {
    name: String,
    payload: Option<String>,
    #[serde(default)]
    fields: Vec<FieldSpec>,
}

#[derive(Debug, serde::Deserialize)]
struct ArenaSpec {
    slabs: Vec<SlabSpec>,
    colors: ColorPoolSpec,
}

#[derive(Debug, serde::Deserialize)]
struct SlabSpec {
    field: String,
    handle: String,
    item: String,
    push: String,
    resolve: String,
    count: String,
    capacity_arg: String,
}

#[derive(Debug, serde::Deserialize)]
struct ColorPoolSpec {
    field: String,
    item: String,
    push: String,
    count: String,
}

#[derive(Debug, serde::Deserialize)]
struct BuilderSpec {
    declaration_rules: Vec<String>,
    selector_rules: Vec<String>,
    wrap_rules: Vec<String>,
    #[serde(default)]
    aggregate_routes: Vec<FrameRoute>,
    #[serde(default)]
    numeric_routes: Vec<KindRoute>,
    #[serde(default)]
    function_routes: Vec<KindRoute>,
    #[serde(default)]
    color_routes: Vec<FrameRoute>,
}

#[derive(Debug, serde::Deserialize)]
struct FrameRoute {
    rule: String,
    frame: String,
}

#[derive(Debug, serde::Deserialize)]
struct KindRoute {
    rule: String,
    kind: String,
}

struct ResolvedBuilderRoutes {
    aggregate: Vec<(u32, String)>,
    numeric: Vec<(u32, String)>,
    functions: Vec<(u32, String)>,
    colors: Vec<(u32, String)>,
    declarations: Vec<u32>,
    selectors: Vec<u32>,
    wraps: Vec<u32>,
}

struct RenderedRuntimeFile {
    path: PathBuf,
    source: String,
}

/// Regenerate the root CSS L4 runtime tree.
pub fn run() -> Result<()> {
    for file in render_runtime_files()? {
        write_if_changed(&file.path, &file.source)?;
    }

    Ok(())
}

/// Check the root CSS L4 runtime tree without writing generated files.
pub fn check() -> Result<Vec<PathBuf>> {
    let files = render_runtime_files()?;
    let mut drift = Vec::new();
    for file in &files {
        let existing =
            std::fs::read(&file.path).with_context(|| format!("read `{}`", file.path.display()))?;
        if existing != file.source.as_bytes() {
            drift.push(file.path.display().to_string());
        }
    }
    if !drift.is_empty() {
        bail!(
            "root runtime projection `css_l4` has generated drift; rerun `cargo xtask regen-css`:\n{}",
            drift
                .iter()
                .map(|path| format!("  {path}"))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
    Ok(files.into_iter().map(|file| file.path).collect())
}

fn render_runtime_files() -> Result<Vec<RenderedRuntimeFile>> {
    let workspace_root = workspace_root()?;
    let projection = load_projection(&workspace_root)?;
    validate_workspace_grammar(&workspace_root, &projection)?;
    let registry = load_registry(&workspace_root, &projection)?;
    let routes = resolve_builder_routes(&projection, &registry)?;

    let output_dir = workspace_root.join(&projection.output_dir);
    let files = [
        ("value.rs", emit_value(&projection)?),
        ("arena.rs", emit_arena(&projection)?),
        ("builder.rs", emit_builder(&projection, &routes)?),
        ("document.rs", emit_document()?),
        ("view.rs", emit_view()?),
        ("parse_with.rs", emit_parse_with(&projection)?),
        ("mod.rs", emit_mod(&projection)?),
    ];

    files
        .into_iter()
        .map(|(name, source)| {
            let path = output_dir.join(name);
            let formatted = format_rust(&source, &path)?;
            Ok(RenderedRuntimeFile {
                path,
                source: format!("{CSS_GENERATED_HEADER}{formatted}"),
            })
        })
        .collect()
}

const CSS_GENERATED_HEADER: &str = "// @generated by xtask regen-css; do not edit by hand.\n";

fn workspace_root() -> Result<PathBuf> {
    let metadata = cargo_metadata::MetadataCommand::new()
        .no_deps()
        .exec()
        .context("cargo_metadata: failed to read workspace manifest")?;
    Ok(metadata.workspace_root.into_std_path_buf())
}

fn load_projection(root: &Path) -> Result<RuntimeProjection> {
    let path = root.join("xtask/runtime-projections/css_l4.toml");
    let bytes = std::fs::read(&path).with_context(|| format!("read `{}`", path.display()))?;
    let source =
        std::str::from_utf8(&bytes).with_context(|| format!("utf8 `{}`", path.display()))?;
    let projection: RuntimeProjection =
        toml::from_str(source).with_context(|| format!("parse `{}`", path.display()))?;
    if projection.schema_version != 1 {
        bail!(
            "unsupported CSS runtime projection schema_version {}",
            projection.schema_version
        );
    }
    if projection.grammar != "css_l4" {
        bail!("regen-css only accepts grammar = `css_l4`");
    }
    if projection.runtime_module != "crate::runtime::css_l4" {
        bail!(
            "regen-css expects runtime_module = `crate::runtime::css_l4`, got `{}`",
            projection.runtime_module
        );
    }
    if projection.parser_type != "CssL4Parser" {
        bail!(
            "regen-css expects parser_type = `CssL4Parser`, got `{}`",
            projection.parser_type
        );
    }
    if projection.output_dir != "crates/core/src/runtime/css_l4" {
        bail!(
            "regen-css expects output_dir = `crates/core/src/runtime/css_l4`, got `{}`",
            projection.output_dir
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
    let found = grammars.iter().any(|entry| {
        entry
            .get("ident")
            .and_then(toml::Value::as_str)
            .is_some_and(|ident| ident == projection.grammar)
    });
    if !found {
        bail!(
            "workspace manifest has no grammar metadata row for `{}`",
            projection.grammar
        );
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

fn resolve_builder_routes(
    projection: &RuntimeProjection,
    registry: &StructRegistry,
) -> Result<ResolvedBuilderRoutes> {
    let mut seen = BTreeSet::new();
    let mut resolve = |rule: &str| -> Result<u32> {
        let layout = registry
            .layout_by_name(rule)
            .ok_or_else(|| anyhow!("CSS projection references unknown rule `{rule}`"))?;
        if !seen.insert((layout.rule_id, rule.to_string())) {
            bail!("duplicate route for rule `{rule}`");
        }
        Ok(layout.rule_id)
    };

    let aggregate = projection
        .builder
        .aggregate_routes
        .iter()
        .map(|route| Ok((resolve(&route.rule)?, route.frame.clone())))
        .collect::<Result<Vec<_>>>()?;
    let numeric = projection
        .builder
        .numeric_routes
        .iter()
        .map(|route| Ok((resolve(&route.rule)?, route.kind.clone())))
        .collect::<Result<Vec<_>>>()?;
    let functions = projection
        .builder
        .function_routes
        .iter()
        .map(|route| Ok((resolve(&route.rule)?, route.kind.clone())))
        .collect::<Result<Vec<_>>>()?;
    let colors = projection
        .builder
        .color_routes
        .iter()
        .map(|route| Ok((resolve(&route.rule)?, route.frame.clone())))
        .collect::<Result<Vec<_>>>()?;
    let declarations = projection
        .builder
        .declaration_rules
        .iter()
        .map(|rule| resolve(rule))
        .collect::<Result<Vec<_>>>()?;
    let selectors = projection
        .builder
        .selector_rules
        .iter()
        .map(|rule| resolve(rule))
        .collect::<Result<Vec<_>>>()?;
    let wraps = projection
        .builder
        .wrap_rules
        .iter()
        .map(|rule| resolve(rule))
        .collect::<Result<Vec<_>>>()?;

    Ok(ResolvedBuilderRoutes {
        aggregate,
        numeric,
        functions,
        colors,
        declarations,
        selectors,
        wraps,
    })
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

fn emit_value(projection: &RuntimeProjection) -> Result<String> {
    validate_unique(
        projection.repr_enums.iter().map(|spec| spec.name.as_str()),
        "repr enum",
    )?;
    validate_unique(
        projection.records.iter().map(|spec| spec.name.as_str()),
        "record",
    )?;
    validate_unique(
        projection.sum_enums.iter().map(|spec| spec.name.as_str()),
        "sum enum",
    )?;

    let mut out = String::new();
    out.push_str(
        "use crate::runtime::css_l4::arena::{CssDeclListId, CssRuleListId, CssSelectorListId, CssValueListId};\n",
    );

    for spec in &projection.repr_enums {
        out.push_str("#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]\n#[repr(u8)]\n");
        out.push_str(&format!("pub enum {} {{\n", spec.name));
        for variant in &spec.variants {
            out.push_str(&format!("{} = {},\n", variant.name, variant.value));
        }
        if let Some(unknown) = &spec.unknown {
            out.push_str(&format!("{}({}) = 255,\n", unknown.name, unknown.payload));
        }
        out.push_str("}\n");
        out.push_str(&format!("impl {} {{\n", spec.name));
        if spec.decode.is_self() {
            out.push_str("pub fn from_discriminant(d: u8) -> Self {\nmatch d {\n");
            for variant in &spec.variants {
                out.push_str(&format!("{} => Self::{},\n", variant.value, variant.name));
            }
            let unknown = spec
                .unknown
                .as_ref()
                .ok_or_else(|| anyhow!("{} decode=self requires unknown variant", spec.name))?;
            out.push_str(&format!("other => Self::{}(other),\n", unknown.name));
            out.push_str("}\n}\n");
        } else {
            out.push_str("pub fn from_discriminant(d: u8) -> Option<Self> {\nmatch d {\n");
            for variant in &spec.variants {
                out.push_str(&format!(
                    "{} => Some(Self::{}),\n",
                    variant.value, variant.name
                ));
            }
            out.push_str("_ => None,\n}\n}\n");
        }
        out.push_str("}\n");
    }

    for spec in &projection.records {
        out.push_str("#[derive(Debug, Clone, Copy, PartialEq)]\n");
        out.push_str(&format!(
            "pub struct {}{} {{\n",
            spec.name,
            lifetime(spec.lifetime)
        ));
        for field in &spec.fields {
            out.push_str(&format!("pub {}: {},\n", field.name, field.ty));
        }
        out.push_str("}\n");
    }

    for spec in &projection.sum_enums {
        out.push_str("#[derive(Debug, Clone, Copy, PartialEq)]\n");
        out.push_str(&format!(
            "pub enum {}{} {{\n",
            spec.name,
            lifetime(spec.lifetime)
        ));
        for variant in &spec.variants {
            if let Some(payload) = &variant.payload {
                out.push_str(&format!("{}({}),\n", variant.name, payload));
            } else if !variant.fields.is_empty() {
                out.push_str(&format!("{} {{\n", variant.name));
                for field in &variant.fields {
                    out.push_str(&format!("{}: {},\n", field.name, field.ty));
                }
                out.push_str("},\n");
            } else {
                out.push_str(&format!("{},\n", variant.name));
            }
        }
        out.push_str("}\n");
    }

    out.push_str("pub use crate::runtime::css_l4::arena::CssKeyframeListId;\n");
    Ok(out)
}

fn lifetime(enabled: bool) -> &'static str {
    if enabled { "<'p>" } else { "" }
}

fn emit_arena(projection: &RuntimeProjection) -> Result<String> {
    validate_unique(
        projection
            .arena
            .slabs
            .iter()
            .map(|spec| spec.handle.as_str()),
        "arena handle",
    )?;
    validate_unique(
        projection
            .arena
            .slabs
            .iter()
            .map(|spec| spec.field.as_str()),
        "arena slab",
    )?;

    let mut out = String::new();
    out.push_str(
        "use crate::runtime::css_l4::value::{CssColor, CssRule, CssTypedValue, Declaration, KeyframeBlock, Selector};\n",
    );
    for slab in &projection.arena.slabs {
        out.push_str("#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]\n");
        out.push_str(&format!("pub struct {}(u32);\n", slab.handle));
        out.push_str(&format!(
            "impl {} {{\npub const EMPTY: Self = Self(0);\npub const fn is_empty(self) -> bool {{ self.0 == 0 }}\nfn slab_index(self) -> Option<usize> {{ if self.0 == 0 {{ None }} else {{ Some((self.0 - 1) as usize) }} }}\n}}\n",
            slab.handle
        ));
    }

    out.push_str("#[derive(Debug, Default)]\npub struct CssArena<'p> {\n");
    for slab in &projection.arena.slabs {
        out.push_str(&format!("{}: Vec<Vec<{}>>,\n", slab.field, slab.item));
    }
    out.push_str(&format!(
        "{}: Vec<Box<{}>>,\n",
        projection.arena.colors.field, projection.arena.colors.item
    ));
    out.push_str("}\n");

    out.push_str("impl<'p> CssArena<'p> {\n");
    out.push_str("pub fn new() -> Self { Self::default() }\n");
    out.push_str("pub fn with_capacity(");
    for (idx, slab) in projection.arena.slabs.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        out.push_str(&format!("{}: usize", slab.capacity_arg));
    }
    out.push_str(") -> Self { Self {\n");
    for slab in &projection.arena.slabs {
        out.push_str(&format!(
            "{}: Vec::with_capacity({}),\n",
            slab.field, slab.capacity_arg
        ));
    }
    out.push_str(&format!("{}: Vec::new(),\n", projection.arena.colors.field));
    out.push_str("}}\n");

    for slab in &projection.arena.slabs {
        out.push_str(&format!(
            "pub fn {}(&mut self, items: Vec<{}>) -> {} {{ if items.is_empty() {{ return {}::EMPTY; }} self.{}.push(items); {}(self.{}.len() as u32) }}\n",
            slab.push,
            slab.item,
            slab.handle,
            slab.handle,
            slab.field,
            slab.handle,
            slab.field
        ));
    }
    out.push_str(&format!(
        "pub fn {}(&mut self, color: {}) -> &'p {} {{ let boxed = Box::new(color); self.{}.push(boxed); let last = self.{}.last().expect(\"just pushed\"); let raw: &{} = last.as_ref(); unsafe {{ core::mem::transmute::<&{}, &'p {}>(raw) }} }}\n",
        projection.arena.colors.push,
        projection.arena.colors.item,
        projection.arena.colors.item,
        projection.arena.colors.field,
        projection.arena.colors.field,
        projection.arena.colors.item.replace("'p", "'_"),
        projection.arena.colors.item.replace("'p", "'_"),
        projection.arena.colors.item
    ));
    for slab in &projection.arena.slabs {
        out.push_str(&format!(
            "pub fn {}(&self, id: {}) -> &[{}] {{ match id.slab_index() {{ None => &[], Some(i) => self.{}[i].as_slice() }} }}\n",
            slab.resolve, slab.handle, slab.item, slab.field
        ));
    }
    for slab in &projection.arena.slabs {
        out.push_str(&format!(
            "pub fn {}(&self) -> usize {{ self.{}.len() }}\n",
            slab.count, slab.field
        ));
    }
    out.push_str(&format!(
        "pub fn {}(&self) -> usize {{ self.{}.len() }}\n",
        projection.arena.colors.count, projection.arena.colors.field
    ));
    out.push_str("pub fn truncate(&mut self, ");
    for slab in &projection.arena.slabs {
        out.push_str(&format!("{}: usize, ", slab.field));
    }
    out.push_str("colors: usize) {\n");
    for slab in &projection.arena.slabs {
        out.push_str(&format!("self.{}.truncate({});\n", slab.field, slab.field));
    }
    out.push_str(&format!(
        "self.{}.truncate(colors);\n",
        projection.arena.colors.field
    ));
    out.push_str("}\n}\n");

    Ok(out)
}

fn emit_mod(projection: &RuntimeProjection) -> Result<String> {
    let mut out = String::new();
    for module in &projection.module.modules {
        out.push_str(&format!("pub mod {module};\n"));
    }
    out.push_str("pub use arena::{");
    out.push_str(&projection.module.arena_exports.join(", "));
    out.push_str("};\n");
    out.push_str("pub use builder::CssStructBuilder;\n");
    out.push_str("pub use document::{");
    out.push_str(&projection.module.document_exports.join(", "));
    out.push_str("};\n");
    out.push_str("pub use parse_with::parse_with;\n");
    out.push_str("pub use value::{");
    out.push_str(&projection.module.value_exports.join(", "));
    out.push_str("};\n");
    Ok(out)
}

fn validate_unique<'a>(items: impl Iterator<Item = &'a str>, label: &str) -> Result<()> {
    let mut seen = BTreeSet::new();
    for item in items {
        if !seen.insert(item.to_string()) {
            bail!("duplicate {label} `{item}` in CSS projection");
        }
    }
    Ok(())
}

fn match_pattern(ids: &[u32]) -> String {
    let mut ids = ids.to_vec();
    ids.sort_unstable();
    ids.into_iter()
        .map(|id| id.to_string())
        .collect::<Vec<_>>()
        .join(" | ")
}

fn emit_builder(_projection: &RuntimeProjection, routes: &ResolvedBuilderRoutes) -> Result<String> {
    let mut arms = BTreeMap::<u32, String>::new();
    for (id, frame) in &routes.aggregate {
        let source = match frame.as_str() {
            "StyleSheet" => {
                "OpenFrame::StyleSheet { rules_base: self.pending_rules.len() }".to_string()
            }
            "HexColor" => "OpenFrame::HexColor { hex_span: None }".to_string(),
            "StyleRule" => {
                "OpenFrame::StyleRule { selectors_base: self.pending_selectors.len(), decls_base: self.pending_decls.len(), span: \"\" }"
                    .to_string()
            }
            "MediaRule" => {
                "OpenFrame::MediaRule { query: \"\", rules_base: self.pending_rules.len() }"
                    .to_string()
            }
            "KeyframesRule" => {
                "OpenFrame::KeyframesRule { name: \"\", blocks_base: self.pending_blocks.len() }"
                    .to_string()
            }
            "KeyframeBlock" => {
                "OpenFrame::KeyframeBlock { selector: \"\", decls_base: self.pending_decls.len() }"
                    .to_string()
            }
            "GenericAtRule" => {
                "OpenFrame::GenericAtRule { name: \"\", prelude: \"\", body: \"\" }".to_string()
            }
            "DirPseudo" => "OpenFrame::DirPseudo { kind_tag: None }".to_string(),
            other => bail!("unknown aggregate frame `{other}`"),
        };
        arms.insert(*id, source);
    }
    for (id, kind) in &routes.numeric {
        arms.insert(
            *id,
            format!(
                "OpenFrame::Numeric {{ kind: NumericKind::{kind}, magnitude: None, unit: None }}"
            ),
        );
    }
    for (id, kind) in &routes.functions {
        arms.insert(
            *id,
            format!(
                "OpenFrame::Function {{ kind: FunctionKind::{kind}, name: \"\", args_base: self.pending_values.len() }}"
            ),
        );
    }
    for (id, frame) in &routes.colors {
        let source = match frame.as_str() {
            "ColorFunction" => {
                "OpenFrame::ColorFunction { kind_tag: None, space_tag: None, components_base: self.pending_components.len() }"
                    .to_string()
            }
            other => bail!("unknown color frame `{other}`"),
        };
        arms.insert(*id, source);
    }

    let mut route_arms = String::new();
    for (id, source) in arms {
        route_arms.push_str(&format!("{id} => {source},\n"));
    }
    if !routes.wraps.is_empty() {
        route_arms.push_str(&format!(
            "{} => OpenFrame::Wrap {{ value: None }},\n",
            match_pattern(&routes.wraps)
        ));
    }
    if !routes.declarations.is_empty() {
        route_arms.push_str(&format!(
            "{} => OpenFrame::Declaration {{ property: None, values_base: self.pending_values.len(), important: false }},\n",
            match_pattern(&routes.declarations)
        ));
    }
    if !routes.selectors.is_empty() {
        route_arms.push_str(&format!(
            "{} => OpenFrame::SelectorList {{ selectors_base: self.pending_selectors.len(), span: \"\" }},\n",
            match_pattern(&routes.selectors)
        ));
    }

    Ok(format!(
        r#"
use bbnf_ir::registry::StructLayout;

use crate::runtime::builder::StructBuilder;
use crate::runtime::css_l4::arena::CssArena;
use crate::runtime::css_l4::document::CssDocument;
use crate::runtime::css_l4::value::{{
    CssAngle, CssAngleUnit, CssColor, CssColorFunction, CssColorMix, CssColorPredefined,
    CssColorSpace, CssColorType, CssDimension, CssFlex, CssFrequency, CssFrequencyUnit,
    CssFunction, CssHueMethod, CssLength, CssLengthUnit, CssPercentage, CssResolution,
    CssResolutionUnit, CssRule, CssTime, CssTimeUnit, CssTypedValue, Declaration, GenericAtRule,
    KeyframeBlock, KeyframesRule, MediaRule, Selector, StyleRule, StyleSheet,
}};
use crate::runtime::handle::CompoundHandle;

// O(1) checkpoint design: every growing per-frame container is hoisted
// out of the `OpenFrame` into a builder-owned append-only scratch stack
// (`pending_rules` / `pending_decls` / `pending_selectors` /
// `pending_values` / `pending_blocks` / `pending_components`). Each
// container-owning frame holds only a `*_base: usize` cursor into the
// matching scratch; interior pushes append to the scratch top, and
// `end_compound` drains the `[base..]` suffix as one contiguous list.
// Because frames now carry only `Copy` scalars + cursor indices, the
// `OpenFrame` is `Copy`, so `stack.clone()` at checkpoint time is an
// O(stack-depth) memcpy of small frames rather than an O(document) deep
// clone of the growing `StyleSheet.rules` Vec — the prior O(N^2) cause.
// Scratch rollback is the count-marker truncate the arena slabs already
// use, so the speculative below-marker deposits (rules/decls/values
// pushed by a committed inline-Repeat iteration or recursive Ref inside a
// structural Alt branch that later fails) are undone exactly: the scratch
// is append-only, the checkpoint records its length, and rollback
// truncates it. Frames below the checkpoint marker keep their `*_base`
// cursor (restored by the cheap `stack.clone()`), so their drain range is
// unchanged. Truncate is the precise inverse of append; soundness is
// parametric in the deposit alphabet.
#[derive(Debug, Clone, Copy)]
enum OpenFrame<'p> {{
    StyleSheet {{ rules_base: usize }},
    StyleRule {{ selectors_base: usize, decls_base: usize, span: &'p str }},
    MediaRule {{ query: &'p str, rules_base: usize }},
    KeyframesRule {{ name: &'p str, blocks_base: usize }},
    KeyframeBlock {{ selector: &'p str, decls_base: usize }},
    GenericAtRule {{ name: &'p str, prelude: &'p str, body: &'p str }},
    Declaration {{ property: Option<&'p str>, values_base: usize, important: bool }},
    SelectorList {{ selectors_base: usize, span: &'p str }},
    Wrap {{ value: Option<CssTypedValue<'p>> }},
    Numeric {{ kind: NumericKind, magnitude: Option<f64>, unit: Option<u8> }},
    ColorFunction {{ kind_tag: Option<u8>, space_tag: Option<u8>, components_base: usize }},
    ColorMix {{
        mix_space: Option<u8>,
        hue_method: Option<u8>,
        left: Option<&'p CssColor<'p>>,
        left_pct: Option<f64>,
        right: Option<&'p CssColor<'p>>,
        right_pct: Option<f64>,
    }},
    Function {{ kind: FunctionKind, name: &'p str, args_base: usize }},
    HexColor {{ hex_span: Option<&'p str> }},
    DirPseudo {{ kind_tag: Option<u8> }},
}}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum NumericKind {{
    Length,
    Angle,
    Time,
    Frequency,
    Resolution,
    Flex,
    Percentage,
}}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum FunctionKind {{
    Calc,
    Min,
    Max,
    Clamp,
    Var,
    Env,
    Url,
    Generic,
}}

#[derive(Debug)]
pub struct CssStructBuilder<'p> {{
    arena: CssArena<'p>,
    stack: Vec<OpenFrame<'p>>,
    root: Option<StyleSheet>,
    next_handle: u64,
    pending_value: Option<CssTypedValue<'p>>,
    input: &'p [u8],
    // Byte-range start of the topmost compound for which a span is
    // being captured (selector preludes). Parallel to the open-frame
    // stack; pushed at `record_compound_bounds_start`, consumed at
    // `record_compound_bounds_end`.
    span_starts: Vec<u32>,
    // Append-only scratch stacks for the per-frame growing containers.
    // Each container-owning `OpenFrame` records a `*_base` cursor into
    // the matching scratch at `begin_compound`; interior deposits append
    // to the scratch top; `end_compound` drains `[base..]` into the arena
    // as one contiguous list. Being append-only, each scratch is
    // rolled back by a count-marker truncate (see `checkpoint` /
    // `rollback`), so speculative below-marker deposits are undone
    // exactly without a per-frame deep clone.
    pending_rules: Vec<CssRule<'p>>,
    pending_decls: Vec<Declaration<'p>>,
    pending_selectors: Vec<Selector<'p>>,
    pending_values: Vec<CssTypedValue<'p>>,
    pending_blocks: Vec<KeyframeBlock<'p>>,
    pending_components: Vec<f64>,
}}

// O(1) checkpoint: pure `Copy`/length markers. `stack` is cloned, but the
// frames are now `Copy` scalars + cursor indices (the growing containers
// live in the builder's append-only scratch stacks), so the clone is an
// O(stack-depth) memcpy rather than the prior O(document) deep clone of
// `StyleSheet.rules`. Every other field is a `usize` length marker or a
// `Copy` snapshot; rollback truncates the scratch stacks to these markers.
#[derive(Debug, Clone)]
pub struct CssStructCheckpoint<'p> {{
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
    span_starts_len: usize,
    pending_rules_len: usize,
    pending_decls_len: usize,
    pending_selectors_len: usize,
    pending_values_len: usize,
    pending_blocks_len: usize,
    pending_components_len: usize,
}}

impl<'p> Default for CssStructBuilder<'p> {{
    fn default() -> Self {{
        Self::new()
    }}
}}

impl<'p> CssStructBuilder<'p> {{
    pub fn new() -> Self {{
        Self {{
            arena: CssArena::new(),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
            pending_value: None,
            input: &[],
            span_starts: Vec::with_capacity(16),
            pending_rules: Vec::with_capacity(64),
            pending_decls: Vec::with_capacity(64),
            pending_selectors: Vec::with_capacity(32),
            pending_values: Vec::with_capacity(32),
            pending_blocks: Vec::with_capacity(16),
            pending_components: Vec::with_capacity(8),
        }}
    }}

    pub fn with_capacity(
        rules: usize,
        decls: usize,
        selectors: usize,
        values: usize,
        keyframes: usize,
    ) -> Self {{
        Self {{
            arena: CssArena::with_capacity(rules, decls, selectors, values, keyframes),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
            pending_value: None,
            input: &[],
            span_starts: Vec::with_capacity(16),
            pending_rules: Vec::with_capacity(64),
            pending_decls: Vec::with_capacity(64),
            pending_selectors: Vec::with_capacity(32),
            pending_values: Vec::with_capacity(32),
            pending_blocks: Vec::with_capacity(16),
            pending_components: Vec::with_capacity(8),
        }}
    }}

    pub fn finalise(mut self, input: &'p str) -> CssDocument<'p> {{
        debug_assert!(
            self.stack.is_empty(),
            "CssStructBuilder::finalise called with {{}} open frame(s)",
            self.stack.len()
        );
        let root = self.root.take().unwrap_or(StyleSheet {{
            rules: crate::runtime::css_l4::arena::CssRuleListId::EMPTY,
        }});
        CssDocument::new(self.arena, root, input)
    }}

    fn deposit_value(&mut self, value: CssTypedValue<'p>) {{
        // Values land on the nearest value-collecting frame's scratch
        // suffix. `Declaration` / `Function` both accumulate into the
        // shared `pending_values` stack (each holds its own `*_base`
        // cursor; LIFO drain keeps the ranges disjoint). `Wrap` stores
        // the value inline in the frame scalar; the root deposits to
        // `pending_value`. The frame-kind probe runs before the scratch
        // push so the `self.stack` borrow does not overlap it.
        if matches!(
            self.stack.last(),
            Some(OpenFrame::Declaration {{ .. }} | OpenFrame::Function {{ .. }})
        ) {{
            self.pending_values.push(value);
            return;
        }}
        match self.stack.last_mut() {{
            Some(OpenFrame::Wrap {{ value: slot }}) => *slot = Some(value),
            _ => self.pending_value = Some(value),
        }}
    }}

    fn deposit_declaration(&mut self, decl: Declaration<'p>) {{
        // The nearest open `StyleRule` / `KeyframeBlock` owns the topmost
        // `pending_decls` suffix, so appending to the scratch top routes
        // the declaration to it (its `decls_base` cursor is unchanged).
        if self.stack.iter().rev().any(|f| {{
            matches!(f, OpenFrame::StyleRule {{ .. }} | OpenFrame::KeyframeBlock {{ .. }})
        }}) {{
            self.pending_decls.push(decl);
        }}
    }}

    fn deposit_rule(&mut self, rule: CssRule<'p>) {{
        // The nearest open `StyleSheet` / `MediaRule` owns the topmost
        // `pending_rules` suffix.
        if self.stack.iter().rev().any(|f| {{
            matches!(f, OpenFrame::StyleSheet {{ .. }} | OpenFrame::MediaRule {{ .. }})
        }}) {{
            self.pending_rules.push(rule);
        }}
    }}
}}

impl<'p> StructBuilder for CssStructBuilder<'p> {{
    type Checkpoint = CssStructCheckpoint<'p>;

    fn checkpoint(&self) -> Self::Checkpoint {{
        CssStructCheckpoint {{
            rules: self.arena.rule_slab_count(),
            decls: self.arena.decl_slab_count(),
            selectors: self.arena.selector_slab_count(),
            values: self.arena.value_slab_count(),
            keyframes: self.arena.keyframe_slab_count(),
            colors: self.arena.color_count(),
            // O(stack-depth) memcpy of `Copy` frames (growing containers
            // hoisted to the scratch stacks below). This restores the
            // exact below-marker frame scalars + `*_base` cursors on
            // rollback.
            stack: self.stack.clone(),
            root: self.root,
            next_handle: self.next_handle,
            pending_value: self.pending_value,
            span_starts_len: self.span_starts.len(),
            pending_rules_len: self.pending_rules.len(),
            pending_decls_len: self.pending_decls.len(),
            pending_selectors_len: self.pending_selectors.len(),
            pending_values_len: self.pending_values.len(),
            pending_blocks_len: self.pending_blocks.len(),
            pending_components_len: self.pending_components.len(),
        }}
    }}

    fn rollback(&mut self, checkpoint: Self::Checkpoint) {{
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
        // Truncate every append-only scratch stack to its checkpoint
        // length. This is the precise inverse of the speculative appends
        // (rules / decls / selectors / values / blocks / components and
        // the span-start cursor); below-marker frames keep their `*_base`
        // cursors via the restored `stack`, so their drain ranges are
        // unchanged.
        self.span_starts.truncate(checkpoint.span_starts_len);
        self.pending_rules.truncate(checkpoint.pending_rules_len);
        self.pending_decls.truncate(checkpoint.pending_decls_len);
        self.pending_selectors.truncate(checkpoint.pending_selectors_len);
        self.pending_values.truncate(checkpoint.pending_values_len);
        self.pending_blocks.truncate(checkpoint.pending_blocks_len);
        self.pending_components.truncate(checkpoint.pending_components_len);
    }}

    fn bind_input(&mut self, input: &[u8]) {{
        // The input outlives the parse; extend to the builder lifetime.
        // Same lifetime-extension pattern as `push_leaf_with_str`.
        self.input = unsafe {{ core::mem::transmute::<&[u8], &'p [u8]>(input) }};
    }}

    fn record_compound_bounds_start(&mut self, offset: u32) {{
        self.span_starts.push(offset);
    }}

    fn record_compound_bounds_end(&mut self, offset: u32) {{
        let Some(start) = self.span_starts.pop() else {{ return }};
        // Only the selector-prelude frame captures its byte extent; the
        // span is the qualified rule's full selector text (one unit per
        // rule, mirroring cssparser's single per-prelude selector event).
        let s = start as usize;
        let e = offset as usize;
        let captured: Option<&'p str> = if s <= e && e <= self.input.len() {{
            core::str::from_utf8(&self.input[s..e]).ok()
        }} else {{
            None
        }};
        match (captured, self.stack.last_mut()) {{
            (Some(text), Some(OpenFrame::SelectorList {{ span, .. }})) => {{
                if span.is_empty() {{
                    *span = text;
                }}
            }}
            // Capture the generic at-rule's full source span as its
            // `name` so the typed summary can recognise the at-rules
            // cssparser's `StyleSheetParser` consumes specially and does
            // not surface as generic at-rules (`@charset` foremost).
            (Some(text), Some(OpenFrame::GenericAtRule {{ name, .. }})) => {{
                if name.is_empty() || *name == ";" {{
                    *name = text;
                }}
            }}
            _ => {{}}
        }}
    }}

    fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle {{
        let frame = match layout.rule_id {{
            {route_arms}
            _ => {{
                let _ = layout.kind;
                OpenFrame::Wrap {{ value: None }}
            }}
        }};
        self.stack.push(frame);
        self.next_handle = self.next_handle.wrapping_add(1);
        CompoundHandle::new(self.next_handle, 0)
    }}

    fn end_compound(&mut self, _handle: CompoundHandle) {{
        let frame = self.stack.pop().expect("CssStructBuilder::end_compound on empty stack");
        match frame {{
            OpenFrame::StyleSheet {{ rules_base }} => {{
                let rules: Vec<CssRule<'p>> = self.pending_rules.split_off(rules_base);
                let id = self.arena.push_rules(rules);
                let sheet = StyleSheet {{ rules: id }};
                if self.stack.is_empty() {{
                    self.root = Some(sheet);
                }}
            }}
            OpenFrame::StyleRule {{ selectors_base, decls_base, span }} => {{
                let declarations: Vec<Declaration<'p>> = self.pending_decls.split_off(decls_base);
                let selectors: Vec<Selector<'p>> = self.pending_selectors.split_off(selectors_base);
                let sel_id = self.arena.push_selectors(selectors);
                let decl_id = self.arena.push_decls(declarations);
                self.deposit_rule(CssRule::Style(StyleRule {{
                    selectors: sel_id,
                    declarations: decl_id,
                    span,
                }}));
            }}
            OpenFrame::MediaRule {{ query, rules_base }} => {{
                let rules: Vec<CssRule<'p>> = self.pending_rules.split_off(rules_base);
                let id = self.arena.push_rules(rules);
                self.deposit_rule(CssRule::Media(MediaRule {{ query, rules: id }}));
            }}
            OpenFrame::KeyframesRule {{ name, blocks_base }} => {{
                let blocks: Vec<KeyframeBlock<'p>> = self.pending_blocks.split_off(blocks_base);
                let id = self.arena.push_keyframes(blocks);
                self.deposit_rule(CssRule::Keyframes(KeyframesRule {{ name, blocks: id }}));
            }}
            OpenFrame::KeyframeBlock {{ selector, decls_base }} => {{
                let declarations: Vec<Declaration<'p>> = self.pending_decls.split_off(decls_base);
                let id = self.arena.push_decls(declarations);
                let block = KeyframeBlock {{ selector, declarations: id }};
                // Land on the nearest open `KeyframesRule`'s `pending_blocks`
                // suffix (it owns the top region; LIFO drain keeps it
                // disjoint). Guarded to match the prior parent-context check.
                if matches!(self.stack.last(), Some(OpenFrame::KeyframesRule {{ .. }})) {{
                    self.pending_blocks.push(block);
                }}
            }}
            OpenFrame::GenericAtRule {{ name, prelude, body }} => {{
                self.deposit_rule(CssRule::GenericAt(GenericAtRule {{ name, prelude, body }}));
            }}
            OpenFrame::Declaration {{ property, values_base, important }} => {{
                let values: Vec<CssTypedValue<'p>> = self.pending_values.split_off(values_base);
                let property = property.unwrap_or("");
                let value = match values.len() {{
                    0 => CssTypedValue::Span(""),
                    1 => values.into_iter().next().unwrap_or(CssTypedValue::Span("")),
                    _ => CssTypedValue::List(self.arena.push_values(values)),
                }};
                self.deposit_declaration(Declaration {{ property, value, important }});
            }}
            OpenFrame::SelectorList {{ selectors_base, span }} => {{
                // A `selectorList` is the qualified rule's full prelude.
                // cssparser raises exactly one selector event per
                // qualified rule, so the typed summary deposits exactly
                // one `Selector` per top-level prelude carrying the
                // prelude's source span. Nested `selectorList`s (inside
                // `:is()` / `:not()` / `:where()`) are part of the
                // enclosing prelude, not separate selector events, so
                // they fold into the parent prelude without adding to
                // the per-rule count.
                let selectors: Vec<Selector<'p>> = self.pending_selectors.split_off(selectors_base);
                let unit = if span.is_empty() {{
                    selectors.into_iter().next().unwrap_or(Selector::Universal)
                }} else {{
                    Selector::Span(span)
                }};
                // After draining this prelude's own region, the
                // `pending_selectors` top is the parent `StyleRule`'s
                // region; deposit the single per-prelude unit there.
                match self.stack.last() {{
                    Some(OpenFrame::StyleRule {{ .. }}) => self.pending_selectors.push(unit),
                    Some(OpenFrame::KeyframeBlock {{ .. }}) => {{}}
                    Some(OpenFrame::SelectorList {{ .. }}) => {{}}
                    _ => {{}}
                }}
            }}
            OpenFrame::Wrap {{ value }} => {{
                if let Some(value) = value {{
                    self.deposit_value(value);
                }}
            }}
            OpenFrame::Numeric {{ kind, magnitude, unit }} => {{
                let value = magnitude.unwrap_or(0.0);
                let dim = match (kind, unit) {{
                    (NumericKind::Length, Some(u)) => CssDimension::Length(CssLength {{
                        value,
                        unit: CssLengthUnit::from_discriminant(u),
                    }}),
                    (NumericKind::Angle, Some(u)) => CssDimension::Angle(CssAngle {{
                        value,
                        unit: CssAngleUnit::from_discriminant(u).unwrap_or(CssAngleUnit::Deg),
                    }}),
                    (NumericKind::Time, Some(u)) => CssDimension::Time(CssTime {{
                        value,
                        unit: CssTimeUnit::from_discriminant(u).unwrap_or(CssTimeUnit::S),
                    }}),
                    (NumericKind::Frequency, Some(u)) => CssDimension::Frequency(CssFrequency {{
                        value,
                        unit: CssFrequencyUnit::from_discriminant(u).unwrap_or(CssFrequencyUnit::Hz),
                    }}),
                    (NumericKind::Resolution, Some(u)) => CssDimension::Resolution(CssResolution {{
                        value,
                        unit: CssResolutionUnit::from_discriminant(u).unwrap_or(CssResolutionUnit::Dppx),
                    }}),
                    (NumericKind::Flex, _) => CssDimension::Flex(CssFlex {{ value }}),
                    (NumericKind::Percentage, _) => CssDimension::Percentage(CssPercentage {{ value }}),
                    _ => CssDimension::Unitless(value),
                }};
                self.deposit_value(CssTypedValue::Dimension(dim));
            }}
            OpenFrame::ColorFunction {{ kind_tag, space_tag, components_base }} => {{
                let components: Vec<f64> = self.pending_components.split_off(components_base);
                let c1 = components.first().copied().unwrap_or(0.0);
                let c2 = components.get(1).copied().unwrap_or(0.0);
                let c3 = components.get(2).copied().unwrap_or(0.0);
                let alpha = components.get(3).copied();
                let color = if let Some(kind) = kind_tag {{
                    CssColor::Function(CssColorFunction {{
                        kind: CssColorType::from_discriminant(kind).unwrap_or(CssColorType::Rgb),
                        c1,
                        c2,
                        c3,
                        alpha,
                    }})
                }} else if let Some(space) = space_tag {{
                    CssColor::Predefined(CssColorPredefined {{
                        space: CssColorSpace::from_discriminant(space).unwrap_or(CssColorSpace::Srgb),
                        c1,
                        c2,
                        c3,
                        alpha,
                    }})
                }} else {{
                    CssColor::Function(CssColorFunction {{
                        kind: CssColorType::Rgb,
                        c1,
                        c2,
                        c3,
                        alpha,
                    }})
                }};
                self.deposit_value(CssTypedValue::Color(color));
            }}
            OpenFrame::ColorMix {{ mix_space, hue_method, left, left_pct, right, right_pct }} => {{
                let mix_space = mix_space
                    .and_then(CssColorSpace::from_discriminant)
                    .unwrap_or(CssColorSpace::Srgb);
                let hue_method = hue_method.and_then(CssHueMethod::from_discriminant);
                let fallback = self.arena.push_color(CssColor::Hex(0x00000000));
                let left = left.unwrap_or(fallback);
                let right = right.unwrap_or(fallback);
                self.deposit_value(CssTypedValue::Color(CssColor::Mix(CssColorMix {{
                    mix_space,
                    hue_method,
                    left,
                    left_pct,
                    right,
                    right_pct,
                }})));
            }}
            OpenFrame::Function {{ kind, name, args_base }} => {{
                let args: Vec<CssTypedValue<'p>> = self.pending_values.split_off(args_base);
                let id = self.arena.push_values(args);
                let func = match kind {{
                    FunctionKind::Calc => CssFunction::Calc {{ args: id }},
                    FunctionKind::Min => CssFunction::Min {{ args: id }},
                    FunctionKind::Max => CssFunction::Max {{ args: id }},
                    FunctionKind::Clamp => CssFunction::Clamp {{ args: id }},
                    FunctionKind::Var => CssFunction::Var {{ name: "", fallback: id }},
                    FunctionKind::Env => CssFunction::Env {{ name: "", fallback: id }},
                    FunctionKind::Url => CssFunction::Url {{ raw: "" }},
                    FunctionKind::Generic => CssFunction::Generic {{ name, args: id }},
                }};
                self.deposit_value(CssTypedValue::Function(func));
            }}
            OpenFrame::DirPseudo {{ kind_tag }} => {{
                let text: &'p str = match kind_tag {{
                    Some(1) => ":dir(rtl)",
                    Some(0) => ":dir(ltr)",
                    _ => ":dir()",
                }};
                // `:dir(...)` lands as one pseudo-class selector on the
                // nearest open `SelectorList` / `StyleRule` (both collect
                // into the `pending_selectors` scratch top); otherwise it
                // degrades to a span value.
                if matches!(
                    self.stack.last(),
                    Some(OpenFrame::SelectorList {{ .. }} | OpenFrame::StyleRule {{ .. }})
                ) {{
                    self.pending_selectors.push(Selector::PseudoClass(text));
                }} else {{
                    self.deposit_value(CssTypedValue::Span(text));
                }}
            }}
            OpenFrame::HexColor {{ hex_span }} => {{
                let packed = hex_span
                    .map(|s| {{
                        let digits = s.strip_prefix('#').unwrap_or(s);
                        crate::css_types::parse_hex_color(digits)
                    }})
                    .unwrap_or(0);
                self.deposit_value(CssTypedValue::Color(CssColor::Hex(packed)));
            }}
        }}
    }}

    fn push_leaf_with_f64(&mut self, value: f64) {{
        // `ColorFunction` collects components into the `pending_components`
        // scratch; the borrow of `self.stack` is released before the
        // scratch push so the two `&mut self` paths do not overlap.
        if matches!(self.stack.last(), Some(OpenFrame::ColorFunction {{ .. }})) {{
            self.pending_components.push(value);
            return;
        }}
        match self.stack.last_mut() {{
            Some(OpenFrame::Numeric {{ magnitude, .. }}) => *magnitude = Some(value),
            Some(OpenFrame::ColorMix {{ left_pct, right_pct, left, right, .. }}) => {{
                if left.is_some() && left_pct.is_none() {{
                    *left_pct = Some(value);
                }} else if right.is_some() && right_pct.is_none() {{
                    *right_pct = Some(value);
                }}
            }}
            _ => self.deposit_value(CssTypedValue::Number(value)),
        }}
    }}

    fn push_leaf_with_i64(&mut self, value: i64) {{
        self.deposit_value(CssTypedValue::Integer(value));
    }}

    fn push_leaf_with_u64(&mut self, value: u64) {{
        if value <= u32::MAX as u64 {{
            self.deposit_value(CssTypedValue::Color(CssColor::Hex(value as u32)));
        }} else {{
            self.deposit_value(CssTypedValue::Number(value as f64));
        }}
    }}

    fn push_leaf_with_bool(&mut self, _value: bool) {{}}

    fn push_leaf_with_str(&mut self, value: &str) {{
        let lifetime_extended: &'p str = unsafe {{ core::mem::transmute(value) }};
        // A bare selector token lands on the `SelectorList`'s
        // `pending_selectors` scratch top; the immutable frame-kind probe
        // releases the `self.stack` borrow before the scratch push.
        if matches!(self.stack.last(), Some(OpenFrame::SelectorList {{ .. }})) {{
            self.pending_selectors.push(Selector::Span(lifetime_extended));
            return;
        }}
        match self.stack.last_mut() {{
            Some(OpenFrame::Declaration {{ property, .. }}) if property.is_none() => {{
                *property = Some(lifetime_extended);
            }}
            Some(OpenFrame::Function {{ name, .. }}) if name.is_empty() => {{
                *name = lifetime_extended;
            }}
            Some(OpenFrame::KeyframesRule {{ name, .. }}) if name.is_empty() => {{
                *name = lifetime_extended;
            }}
            Some(OpenFrame::KeyframeBlock {{ selector, .. }}) if selector.is_empty() => {{
                *selector = lifetime_extended;
            }}
            Some(OpenFrame::MediaRule {{ query, .. }}) if query.is_empty() => {{
                *query = lifetime_extended;
            }}
            // Generic at-rule name (`@charset`, `@font-face`, `@supports`,
            // …). The leading regex leaf carries the `@name`; capture it so
            // the typed summary can classify the at-rule the way cssparser
            // does (it skips `@charset`/`@import`/`@namespace` rather than
            // counting them as generic at-rules).
            Some(OpenFrame::GenericAtRule {{ name, .. }}) if name.is_empty() => {{
                *name = lifetime_extended;
            }}
            Some(OpenFrame::HexColor {{ hex_span }}) if hex_span.is_none() => {{
                *hex_span = Some(lifetime_extended);
            }}
            _ => self.deposit_value(CssTypedValue::Span(lifetime_extended)),
        }}
    }}

    fn push_leaf_with_unit(&mut self) {{}}

    fn push_branch_tag(&mut self, branch_index: u32) {{
        match self.stack.last_mut() {{
            Some(OpenFrame::Numeric {{ unit, .. }}) => *unit = Some(branch_index as u8),
            Some(OpenFrame::ColorFunction {{ kind_tag, space_tag, .. }}) => {{
                if kind_tag.is_none() && space_tag.is_none() {{
                    *kind_tag = Some(branch_index as u8);
                }}
            }}
            Some(OpenFrame::ColorMix {{ mix_space, hue_method, .. }}) => {{
                if mix_space.is_none() {{
                    *mix_space = Some(branch_index as u8);
                }} else if hue_method.is_none() {{
                    *hue_method = Some(branch_index as u8);
                }}
            }}
            Some(OpenFrame::DirPseudo {{ kind_tag }}) => {{
                if kind_tag.is_none() {{
                    *kind_tag = Some(branch_index as u8);
                }}
            }}
            _ => {{}}
        }}
    }}
}}
"#,
    ))
}

fn emit_document() -> Result<String> {
    Ok(
        r#"
use crate::runtime::css_l4::arena::{
    CssArena, CssDeclListId, CssKeyframeListId, CssRuleListId, CssSelectorListId, CssValueListId,
};
use crate::runtime::css_l4::value::{
    CssColor, CssFunction, CssRule, CssTypedValue, Declaration, GenericAtRule, KeyframeBlock,
    KeyframesRule, MediaRule, Selector, StyleRule, StyleSheet,
};
use crate::runtime::path::{Path, PathSegment};

#[derive(Debug)]
pub struct CssDocument<'p> {
    pub arena: CssArena<'p>,
    pub root: StyleSheet,
    pub input: &'p str,
}

impl<'p> CssDocument<'p> {
    pub fn new(arena: CssArena<'p>, root: StyleSheet, input: &'p str) -> Self {
        Self { arena, root, input }
    }
    pub fn root(&self) -> &StyleSheet { &self.root }
    pub fn arena(&self) -> &CssArena<'p> { &self.arena }
    pub fn input(&self) -> &'p str { self.input }
    pub fn rules(&self, id: CssRuleListId) -> &[CssRule<'p>] { self.arena.rules(id) }
    pub fn decls(&self, id: CssDeclListId) -> &[Declaration<'p>] { self.arena.decls(id) }
    pub fn selectors(&self, id: CssSelectorListId) -> &[Selector<'p>] { self.arena.selectors(id) }
    pub fn values(&self, id: CssValueListId) -> &[CssTypedValue<'p>] { self.arena.values(id) }
    pub fn keyframes(&self, id: CssKeyframeListId) -> &[KeyframeBlock<'p>] { self.arena.keyframes(id) }
    pub fn view<'a>(&'a self) -> CssView<'a, 'p> {
        CssView { doc: self, focus: CssFocus::Stylesheet(&self.root) }
    }
    pub fn to_value(&self) -> &StyleSheet { &self.root }
    pub fn get<T: CssPathQuery>(&self, path: Path<'_>) -> Option<T> { T::query(self, path) }
    pub fn walk_declarations(&self) -> CssDeclWalk<'_, 'p> {
        CssDeclWalk { doc: self, stack: vec![CssWalkItem::RuleList(self.root.rules, 0)] }
    }
    pub fn walk_values<'a>(&'a self) -> impl Iterator<Item = (&'p str, &'a CssTypedValue<'p>)> + 'a {
        self.walk_declarations().flat_map(|decl| {
            let property = decl.property;
            let primary = std::iter::once((property, &decl.value));
            let list_extra: Box<dyn Iterator<Item = (&'p str, &'a CssTypedValue<'p>)> + 'a> =
                match &decl.value {
                    CssTypedValue::List(id) => Box::new(self.values(*id).iter().map(move |v| (property, v))),
                    _ => Box::new(std::iter::empty()),
                };
            primary.chain(list_extra)
        })
    }
}

#[derive(Debug)]
enum CssWalkItem {
    RuleList(CssRuleListId, usize),
    DeclList(CssDeclListId, usize),
    KeyframeList(CssKeyframeListId, usize),
}

pub struct CssDeclWalk<'a, 'p: 'a> {
    doc: &'a CssDocument<'p>,
    stack: Vec<CssWalkItem>,
}

impl<'a, 'p: 'a> Iterator for CssDeclWalk<'a, 'p> {
    type Item = &'a Declaration<'p>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let top = self.stack.last_mut()?;
            match top {
                CssWalkItem::RuleList(id, idx) => {
                    let rules = self.doc.rules(*id);
                    if let Some(rule) = rules.get(*idx) {
                        *idx += 1;
                        match rule {
                            CssRule::Style(style) => self.stack.push(CssWalkItem::DeclList(style.declarations, 0)),
                            CssRule::Media(media) => self.stack.push(CssWalkItem::RuleList(media.rules, 0)),
                            CssRule::Keyframes(kf) => self.stack.push(CssWalkItem::KeyframeList(kf.blocks, 0)),
                            CssRule::GenericAt(_) => {}
                        }
                    } else {
                        self.stack.pop();
                    }
                }
                CssWalkItem::DeclList(id, idx) => {
                    let decls = self.doc.decls(*id);
                    if let Some(decl) = decls.get(*idx) {
                        *idx += 1;
                        return Some(decl);
                    }
                    self.stack.pop();
                }
                CssWalkItem::KeyframeList(id, idx) => {
                    let blocks = self.doc.keyframes(*id);
                    if let Some(block) = blocks.get(*idx) {
                        *idx += 1;
                        self.stack.push(CssWalkItem::DeclList(block.declarations, 0));
                    } else {
                        self.stack.pop();
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CssView<'a, 'p: 'a> {
    pub(crate) doc: &'a CssDocument<'p>,
    pub(crate) focus: CssFocus<'a, 'p>,
}

#[derive(Debug, Clone, Copy)]
pub enum CssFocus<'a, 'p: 'a> {
    Stylesheet(&'a StyleSheet),
    Rule(&'a CssRule<'p>),
    Decl(&'a Declaration<'p>),
    Value(&'a CssTypedValue<'p>),
    KeyframeBlock(&'a KeyframeBlock<'p>),
}

impl<'a, 'p: 'a> CssView<'a, 'p> {
    pub fn focused(doc: &'a CssDocument<'p>, focus: CssFocus<'a, 'p>) -> Self { Self { doc, focus } }
    pub fn document(&self) -> &'a CssDocument<'p> { self.doc }
    pub fn focus(&self) -> CssFocus<'a, 'p> { self.focus }
    pub fn root(&self) -> &'a StyleSheet { &self.doc.root }
    pub fn arena(&self) -> &'a CssArena<'p> { &self.doc.arena }
    pub fn rules(&self, id: CssRuleListId) -> &'a [CssRule<'p>] { self.doc.rules(id) }
    pub fn decls(&self, id: CssDeclListId) -> &'a [Declaration<'p>] { self.doc.decls(id) }
    pub fn selectors(&self, id: CssSelectorListId) -> &'a [Selector<'p>] { self.doc.selectors(id) }
    pub fn values(&self, id: CssValueListId) -> &'a [CssTypedValue<'p>] { self.doc.values(id) }
    pub fn keyframes(&self, id: CssKeyframeListId) -> &'a [KeyframeBlock<'p>] { self.doc.keyframes(id) }
    pub fn kind(&self) -> CssDocumentKind {
        match self.focus {
            CssFocus::Stylesheet(sheet) => {
                if sheet.rules.is_empty() { CssDocumentKind::Empty } else { CssDocumentKind::StyleSheet }
            }
            CssFocus::Rule(_) => CssDocumentKind::Rule,
            CssFocus::Decl(_) => CssDocumentKind::Declaration,
            CssFocus::Value(_) => CssDocumentKind::Value,
            CssFocus::KeyframeBlock(_) => CssDocumentKind::KeyframeBlock,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CssDocumentKind {
    Empty,
    StyleSheet,
    Rule,
    Declaration,
    Value,
    KeyframeBlock,
}

pub trait CssPathQuery: Sized {
    fn query<'p>(doc: &CssDocument<'p>, path: Path<'_>) -> Option<Self>;
}

pub trait CssVisitor<'p> {
    fn visit_stylesheet(&mut self, _sheet: &StyleSheet) {}
    fn visit_rule(&mut self, _rule: &CssRule<'p>) {}
    fn visit_style_rule(&mut self, _rule: &StyleRule<'p>) {}
    fn visit_media_rule(&mut self, _rule: &MediaRule<'p>) {}
    fn visit_keyframes_rule(&mut self, _rule: &KeyframesRule<'p>) {}
    fn visit_generic_at_rule(&mut self, _rule: &GenericAtRule<'p>) {}
    fn visit_keyframe_block(&mut self, _block: &KeyframeBlock<'p>) {}
    fn visit_selector(&mut self, _selector: &Selector<'p>) {}
    fn visit_declaration(&mut self, _declaration: &Declaration<'p>) {}
    fn visit_value(&mut self, _value: &CssTypedValue<'p>) {}
    fn visit_color(&mut self, _color: &CssColor<'p>) {}
    fn visit_function(&mut self, _function: &CssFunction<'p>) {}
}

pub fn visit_document<'p, V>(doc: &CssDocument<'p>, visitor: &mut V)
where
    V: CssVisitor<'p> + ?Sized,
{
    walk_stylesheet(doc, doc.root(), visitor);
}

fn walk_stylesheet<'p, V>(doc: &CssDocument<'p>, sheet: &StyleSheet, visitor: &mut V)
where
    V: CssVisitor<'p> + ?Sized,
{
    visitor.visit_stylesheet(sheet);
    walk_rule_list(doc, sheet.rules, visitor);
}

fn walk_rule_list<'p, V>(doc: &CssDocument<'p>, id: CssRuleListId, visitor: &mut V)
where
    V: CssVisitor<'p> + ?Sized,
{
    for rule in doc.rules(id) {
        walk_rule(doc, rule, visitor);
    }
}

fn walk_rule<'p, V>(doc: &CssDocument<'p>, rule: &CssRule<'p>, visitor: &mut V)
where
    V: CssVisitor<'p> + ?Sized,
{
    visitor.visit_rule(rule);
    match rule {
        CssRule::Style(style) => {
            visitor.visit_style_rule(style);
            for selector in doc.selectors(style.selectors) {
                visitor.visit_selector(selector);
            }
            walk_decl_list(doc, style.declarations, visitor);
        }
        CssRule::Media(media) => {
            visitor.visit_media_rule(media);
            walk_rule_list(doc, media.rules, visitor);
        }
        CssRule::Keyframes(keyframes) => {
            visitor.visit_keyframes_rule(keyframes);
            for block in doc.keyframes(keyframes.blocks) {
                visitor.visit_keyframe_block(block);
                walk_decl_list(doc, block.declarations, visitor);
            }
        }
        CssRule::GenericAt(generic) => visitor.visit_generic_at_rule(generic),
    }
}

fn walk_decl_list<'p, V>(doc: &CssDocument<'p>, id: CssDeclListId, visitor: &mut V)
where
    V: CssVisitor<'p> + ?Sized,
{
    for declaration in doc.decls(id) {
        visitor.visit_declaration(declaration);
        walk_value(doc, &declaration.value, visitor);
    }
}

fn walk_value_list<'p, V>(doc: &CssDocument<'p>, id: CssValueListId, visitor: &mut V)
where
    V: CssVisitor<'p> + ?Sized,
{
    for value in doc.values(id) {
        walk_value(doc, value, visitor);
    }
}

fn walk_value<'p, V>(doc: &CssDocument<'p>, value: &CssTypedValue<'p>, visitor: &mut V)
where
    V: CssVisitor<'p> + ?Sized,
{
    visitor.visit_value(value);
    match value {
        CssTypedValue::Color(color) => walk_color(color, visitor),
        CssTypedValue::Function(function) => walk_function(doc, function, visitor),
        CssTypedValue::List(id) => walk_value_list(doc, *id, visitor),
        CssTypedValue::Dimension(_)
        | CssTypedValue::Number(_)
        | CssTypedValue::Integer(_)
        | CssTypedValue::String(_)
        | CssTypedValue::Ident(_)
        | CssTypedValue::GlobalKeyword(_)
        | CssTypedValue::Span(_) => {}
    }
}

fn walk_color<'p, V>(color: &CssColor<'p>, visitor: &mut V)
where
    V: CssVisitor<'p> + ?Sized,
{
    visitor.visit_color(color);
    match color {
        CssColor::Mix(mix) => {
            walk_color(mix.left, visitor);
            walk_color(mix.right, visitor);
        }
        CssColor::Hex(_)
        | CssColor::Named { .. }
        | CssColor::Function(_)
        | CssColor::Predefined(_) => {}
    }
}

fn walk_function<'p, V>(doc: &CssDocument<'p>, function: &CssFunction<'p>, visitor: &mut V)
where
    V: CssVisitor<'p> + ?Sized,
{
    visitor.visit_function(function);
    match function {
        CssFunction::Calc { args }
        | CssFunction::Min { args }
        | CssFunction::Max { args }
        | CssFunction::Clamp { args }
        | CssFunction::Gradient { args, .. }
        | CssFunction::Generic { args, .. } => walk_value_list(doc, *args, visitor),
        CssFunction::Var { fallback, .. } | CssFunction::Env { fallback, .. } => {
            walk_value_list(doc, *fallback, visitor);
        }
        CssFunction::Url { .. } => {}
    }
}

enum CssWalkCursor<'a, 'p> {
    Sheet(&'a StyleSheet, &'a CssArena<'p>),
    Rule(&'a CssRule<'p>, &'a CssArena<'p>),
    Decl(&'a Declaration<'p>, &'a CssArena<'p>),
    Value(&'a CssTypedValue<'p>, #[allow(dead_code)] &'a CssArena<'p>),
}

fn walk_path<'a, 'p>(doc: &'a CssDocument<'p>, path: Path<'_>) -> Option<CssWalkCursor<'a, 'p>> {
    let mut current = CssWalkCursor::Sheet(&doc.root, &doc.arena);
    for segment in path.iter() {
        current = match (current, segment) {
            (CssWalkCursor::Sheet(sheet, arena), PathSegment::Index(idx)) => {
                CssWalkCursor::Rule(arena.rules(sheet.rules).get(*idx)?, arena)
            }
            (CssWalkCursor::Rule(rule, arena), PathSegment::Index(idx)) => match rule {
                CssRule::Style(style) => CssWalkCursor::Decl(arena.decls(style.declarations).get(*idx)?, arena),
                CssRule::Media(media) => CssWalkCursor::Rule(arena.rules(media.rules).get(*idx)?, arena),
                _ => return None,
            },
            (CssWalkCursor::Decl(decl, arena), PathSegment::Field(name)) if *name == "value" => {
                CssWalkCursor::Value(&decl.value, arena)
            }
            _ => return None,
        };
    }
    Some(current)
}

impl CssPathQuery for &str {
    fn query<'p>(doc: &CssDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            CssWalkCursor::Decl(decl, _) => {
                let extended: &'p str = decl.property;
                Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
            }
            CssWalkCursor::Value(value, _) => match value {
                CssTypedValue::String(s) | CssTypedValue::Ident(s) | CssTypedValue::Span(s) => {
                    let extended: &'p str = *s;
                    Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
                }
                _ => None,
            },
            _ => None,
        }
    }
}

impl CssPathQuery for f64 {
    fn query<'p>(doc: &CssDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            CssWalkCursor::Value(value, _) => match value {
                CssTypedValue::Number(n) => Some(*n),
                CssTypedValue::Dimension(d) => Some(match d {
                    crate::runtime::css_l4::value::CssDimension::Length(v) => v.value,
                    crate::runtime::css_l4::value::CssDimension::Angle(v) => v.value,
                    crate::runtime::css_l4::value::CssDimension::Time(v) => v.value,
                    crate::runtime::css_l4::value::CssDimension::Frequency(v) => v.value,
                    crate::runtime::css_l4::value::CssDimension::Resolution(v) => v.value,
                    crate::runtime::css_l4::value::CssDimension::Flex(v) => v.value,
                    crate::runtime::css_l4::value::CssDimension::Percentage(v) => v.value,
                    crate::runtime::css_l4::value::CssDimension::Unitless(v) => *v,
                }),
                _ => None,
            },
            _ => None,
        }
    }
}
"#
        .to_string(),
    )
}

fn emit_view() -> Result<String> {
    Ok(
        r#"
use crate::runtime::RuntimeView;
use crate::runtime::css_l4::document::{CssDocumentKind, CssFocus, CssView};
use crate::runtime::css_l4::value::{CssRule, CssTypedValue, KeyframeBlock};

impl<'a, 'p: 'a> RuntimeView<'p> for CssView<'a, 'p> {
    type Kind = CssDocumentKind;
    fn kind(&self) -> Self::Kind { CssView::kind(self) }
    fn span(&self) -> Option<&'p str> {
        match self.focus {
            CssFocus::Stylesheet(_) => None,
            CssFocus::Rule(rule) => match rule {
                CssRule::Style(s) => Some(s.span),
                CssRule::Media(_) | CssRule::Keyframes(_) | CssRule::GenericAt(_) => None,
            },
            CssFocus::Decl(_) => None,
            CssFocus::Value(value) => match value {
                CssTypedValue::String(s) | CssTypedValue::Ident(s) | CssTypedValue::Span(s) => Some(*s),
                _ => None,
            },
            CssFocus::KeyframeBlock(block) => Some(block.selector),
        }
    }
    fn input(&self) -> &'p str { self.doc.input }
    fn children(&self) -> impl Iterator<Item = Self> + '_ {
        CssChildrenIter { doc: self.doc, focus: self.focus, index: 0 }
    }
}

pub struct CssChildrenIter<'a, 'p: 'a> {
    doc: &'a crate::runtime::css_l4::CssDocument<'p>,
    focus: CssFocus<'a, 'p>,
    index: usize,
}

impl<'a, 'p: 'a> Iterator for CssChildrenIter<'a, 'p> {
    type Item = CssView<'a, 'p>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.focus {
            CssFocus::Stylesheet(sheet) => {
                let rule = self.doc.rules(sheet.rules).get(self.index)?;
                self.index += 1;
                Some(CssView::focused(self.doc, CssFocus::Rule(rule)))
            }
            CssFocus::Rule(rule) => match rule {
                CssRule::Style(style) => {
                    let decl = self.doc.decls(style.declarations).get(self.index)?;
                    self.index += 1;
                    Some(CssView::focused(self.doc, CssFocus::Decl(decl)))
                }
                CssRule::Media(media) => {
                    let nested = self.doc.rules(media.rules).get(self.index)?;
                    self.index += 1;
                    Some(CssView::focused(self.doc, CssFocus::Rule(nested)))
                }
                CssRule::Keyframes(kf) => {
                    let blocks: &'a [KeyframeBlock<'p>] = self.doc.keyframes(kf.blocks);
                    let block = blocks.get(self.index)?;
                    self.index += 1;
                    Some(CssView::focused(self.doc, CssFocus::KeyframeBlock(block)))
                }
                CssRule::GenericAt(_) => None,
            },
            CssFocus::Decl(decl) => {
                if self.index == 0 {
                    self.index += 1;
                    Some(CssView::focused(self.doc, CssFocus::Value(&decl.value)))
                } else {
                    None
                }
            }
            CssFocus::Value(_) => None,
            CssFocus::KeyframeBlock(block) => {
                let decl = self.doc.decls(block.declarations).get(self.index)?;
                self.index += 1;
                Some(CssView::focused(self.doc, CssFocus::Decl(decl)))
            }
        }
    }
}
"#
        .to_string(),
    )
}

fn emit_parse_with(projection: &RuntimeProjection) -> Result<String> {
    Ok(format!(
        r#"
use super::document::{{{document}, CssPathQuery}};
use crate::grammar::generated::{grammar}::{{
    __path_plan, {shape_support}, {parser_entry},
}};
use crate::path::cursor::Decision;
use crate::path::executor::PathExecutor;
use crate::path::ir::{{PathSegment as TypedSegment, TypedPath}};
use crate::path::markers::{marker};
use crate::runtime::css_l4::{builder};
use crate::runtime::path::{{Path as LegacyPath, PathSegment as LegacySegment}};

fn lower<'a>(seg: &TypedSegment<'a>) -> Option<LegacySegment<'a>> {{
    match seg {{
        TypedSegment::Field(s) => Some(LegacySegment::Field(s)),
        TypedSegment::Index(i) => Some(LegacySegment::Index(*i)),
        TypedSegment::VariantName(s) => Some(LegacySegment::Field(s)),
        TypedSegment::Wildcard => None,
    }}
}}

pub fn parse_with<T>(input: &str, path: &TypedPath<{marker}, T>) -> Option<T>
where
    T: CssPathQuery,
{{
    PathExecutor::execute(
        input,
        path,
        |rule_id, kind, _idx| {{
            __path_plan::lookup(rule_id, kind)
                .map(|e| e.decision)
                .unwrap_or(Decision::ParseFully)
        }},
        |src, cursor| {{
            let mut state = {shape_support}::ScanState::new();
            let mut builder = {builder}::new();
            let mut pos: usize = 0;
            {parser_entry}(src.as_bytes(), &mut pos, &mut state, &mut builder, cursor).ok()?;
            let doc: {document}<'_> = builder.finalise(src);
            let mut legacy: Vec<LegacySegment<'_>> = Vec::with_capacity(path.len());
            for owned in path.owned_segments() {{
                legacy.push(lower(&owned.as_borrowed())?);
            }}
            doc.get::<T>(LegacyPath::new(&legacy))
        }},
    )
}}
"#,
        grammar = projection.grammar,
        shape_support = projection.shape_support,
        parser_entry = projection.parser_entry,
        marker = projection.marker,
        builder = projection.builder_type,
        document = projection.document,
    ))
}
