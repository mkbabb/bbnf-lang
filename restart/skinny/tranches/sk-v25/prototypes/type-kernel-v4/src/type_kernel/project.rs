//! Independent flat Rust and TypeScript projection with semantic symbol interning.

use std::collections::{BTreeMap, BTreeSet};

use super::layout::{LoweringPlan, ToolchainIdentity};
use super::solve::{SolvedNode, SolvedRow, SolvedTypeId};
use super::{OperationContext, Origin, Primitive, TypeError, TypeErrorKind, fnv1a64};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TargetLanguage {
    Rust,
    TypeScript,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArtifactProvenance {
    semantic_digest: String,
    input_digest: Option<String>,
    source_digest: String,
    generator: String,
    toolchain: ToolchainIdentity,
    reproduction_command: String,
}

impl ArtifactProvenance {
    #[must_use]
    pub fn semantic_digest(&self) -> &str {
        &self.semantic_digest
    }
    #[must_use]
    pub fn input_digest(&self) -> Option<&str> {
        self.input_digest.as_deref()
    }
    #[must_use]
    pub fn source_digest(&self) -> &str {
        &self.source_digest
    }
    #[must_use]
    pub fn toolchain(&self) -> &ToolchainIdentity {
        &self.toolchain
    }
    #[must_use]
    pub fn reproduction_command(&self) -> &str {
        &self.reproduction_command
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GeneratedArtifact {
    source: String,
    manifest: String,
    provenance: ArtifactProvenance,
}

impl GeneratedArtifact {
    #[must_use]
    pub fn source(&self) -> &str {
        &self.source
    }
    #[must_use]
    pub fn manifest(&self) -> &str {
        &self.manifest
    }
    #[must_use]
    pub fn provenance(&self) -> &ArtifactProvenance {
        &self.provenance
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Precedence {
    Union,
    Function,
    Atomic,
}

#[derive(Clone, Debug)]
struct Printed {
    text: String,
    precedence: Precedence,
}

impl Printed {
    fn grouped_below(&self, required: Precedence) -> String {
        if self.precedence < required {
            format!("({})", self.text)
        } else {
            self.text.clone()
        }
    }
}

#[derive(Debug)]
struct Symbols {
    language: TargetLanguage,
    semantic: BTreeMap<String, String>,
    used: BTreeSet<String>,
}

impl Symbols {
    fn new(language: TargetLanguage) -> Self {
        Self {
            language,
            semantic: BTreeMap::new(),
            used: BTreeSet::new(),
        }
    }

    fn intern(
        &mut self,
        identity: String,
        requested: &str,
        operations: &mut OperationContext,
    ) -> Result<String, TypeError> {
        if let Some(existing) = self.semantic.get(&identity) {
            operations.charge(
                1,
                "project.symbol.memo",
                &[],
                [Origin::new("project.symbol", requested)],
            )?;
            return Ok(existing.clone());
        }
        operations.charge(
            requested
                .len()
                .saturating_add(requested.chars().count())
                .max(1),
            "project.symbol.normalize",
            &[],
            [Origin::new("project.symbol", requested)],
        )?;
        let base = match self.language {
            TargetLanguage::Rust => rust_type_identifier(requested),
            TargetLanguage::TypeScript => normalize_identifier(requested, self.language),
        };
        let mut candidate = base.clone();
        let mut suffix = 2_u32;
        while !self.used.insert(candidate.clone()) {
            candidate = match self.language {
                TargetLanguage::Rust => format!("{base}{suffix}"),
                TargetLanguage::TypeScript => format!("{base}_{suffix}"),
            };
            suffix = suffix.checked_add(1).ok_or_else(|| {
                operations.fail(TypeErrorKind::ArithmeticOverflow, &[], "project.symbol")
            })?;
            operations.charge(
                candidate.len().max(1),
                "project.symbol.collision",
                &[],
                [Origin::new("project.symbol", requested)],
            )?;
        }
        self.semantic.insert(identity, candidate.clone());
        Ok(candidate)
    }
}

pub fn project(
    plan: &LoweringPlan,
    language: TargetLanguage,
    toolchain: ToolchainIdentity,
    operations: &mut OperationContext,
) -> Result<GeneratedArtifact, TypeError> {
    validate_projectable(plan, operations)?;
    for value in [&toolchain.target, &toolchain.rustc, &toolchain.typescript] {
        operations.charge(
            value.len().saturating_add(value.chars().count()).max(1),
            "project.toolchain.bytes",
            &[],
            [Origin::boundary("project.toolchain")],
        )?;
    }
    let source = match language {
        TargetLanguage::Rust => emit_rust(plan, operations)?,
        TargetLanguage::TypeScript => emit_typescript(plan, operations)?,
    };
    operations.charge(
        source.len().max(1),
        "project.source.bytes",
        &[],
        [Origin::boundary("project.source")],
    )?;
    let source_digest = format!("fnv1a64:{:016x}", fnv1a64(source.as_bytes()));
    let reproduction_command = match language {
        TargetLanguage::Rust => "rustc --edition=2024 -D warnings --crate-type=lib artifact.rs",
        TargetLanguage::TypeScript => {
            "npx --yes tsc --strict --noEmit --lib es2022,dom artifact.ts"
        }
    }
    .to_owned();
    let provenance = ArtifactProvenance {
        semantic_digest: plan.semantic_digest().as_str().to_owned(),
        input_digest: plan
            .solution
            .input_digest()
            .map(|value| value.as_str().to_owned()),
        source_digest,
        generator: "bbnf-type-kernel-v4/0.0.0".to_owned(),
        toolchain,
        reproduction_command,
    };
    let manifest = format!(
        "generator={}\nlanguage={language:?}\nsemantic_digest={}\ninput_digest={}\nsource_digest={}\ntarget={}\nrustc={}\ntypescript={}\ncommand={}\n",
        provenance.generator,
        provenance.semantic_digest,
        provenance.input_digest.as_deref().unwrap_or("none"),
        provenance.source_digest,
        provenance.toolchain.target,
        provenance.toolchain.rustc,
        provenance.toolchain.typescript,
        provenance.reproduction_command
    );
    operations.charge(
        manifest.len().max(1),
        "project.manifest.bytes",
        &[],
        [Origin::boundary("project.manifest")],
    )?;
    Ok(GeneratedArtifact {
        source,
        manifest,
        provenance,
    })
}

fn validate_projectable(
    plan: &LoweringPlan,
    operations: &mut OperationContext,
) -> Result<(), TypeError> {
    for node in &plan.solution.nodes {
        operations.charge(
            1,
            "project.validate.node",
            &[],
            [Origin::boundary("project.validate")],
        )?;
        match node {
            SolvedNode::Free { .. } => {
                return Err(operations.fail(
                    TypeErrorKind::OpenTypeVariable,
                    &[],
                    "project.validate",
                ));
            }
            SolvedNode::Rec(_) | SolvedNode::Mu { .. } => {
                return Err(operations.fail(
                    TypeErrorKind::RecursiveTypeRequiresCarrier,
                    &[],
                    "project.validate",
                ));
            }
            SolvedNode::Primitive(_)
            | SolvedNode::Named(_)
            | SolvedNode::Tuple(_)
            | SolvedNode::List(_)
            | SolvedNode::Option(_)
            | SolvedNode::Function { .. }
            | SolvedNode::Record(_)
            | SolvedNode::Variant(_) => {}
        }
    }
    if plan.solution.rows.iter().any(|row| row.tail.is_some()) {
        return Err(operations.fail(TypeErrorKind::OpenRow, &[], "project.validate"));
    }
    Ok(())
}

fn emit_rust(plan: &LoweringPlan, operations: &mut OperationContext) -> Result<String, TypeError> {
    let mut symbols = Symbols::new(TargetLanguage::Rust);
    let row_names = (0..plan.solution.rows.len())
        .map(|index| {
            symbols.intern(
                format!("row:{index}"),
                &format!("KernelRow{}", index + 1),
                operations,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut rendered = Vec::<Printed>::with_capacity(plan.solution.nodes.len());
    let mut declarations = String::new();
    for node in &plan.solution.nodes {
        let printed = match node {
            SolvedNode::Primitive(value) => atomic(rust_primitive(*value)),
            SolvedNode::Named(name) => Printed {
                text: symbols.intern(format!("named:{name}"), name, operations)?,
                precedence: Precedence::Atomic,
            },
            SolvedNode::Tuple(items) => atomic(&rust_tuple(items, &rendered)),
            SolvedNode::List(item) => atomic(&format!("Vec<{}>", rendered[item.0 as usize].text)),
            SolvedNode::Option(item) => {
                atomic(&format!("Option<{}>", rendered[item.0 as usize].text))
            }
            SolvedNode::Function { parameters, result } => atomic(&format!(
                "fn({}) -> {}",
                parameters
                    .iter()
                    .map(|id| rendered[id.0 as usize].text.as_str())
                    .collect::<Vec<_>>()
                    .join(", "),
                rendered[result.0 as usize].text
            )),
            SolvedNode::Record(row) => {
                declarations.push_str(&rust_row(
                    &row_names[row.0 as usize],
                    &plan.solution.rows[row.0 as usize],
                    &rendered,
                    false,
                    operations,
                )?);
                atomic(&row_names[row.0 as usize])
            }
            SolvedNode::Variant(row) => {
                declarations.push_str(&rust_row(
                    &row_names[row.0 as usize],
                    &plan.solution.rows[row.0 as usize],
                    &rendered,
                    true,
                    operations,
                )?);
                atomic(&row_names[row.0 as usize])
            }
            SolvedNode::Free { .. } | SolvedNode::Rec(_) | SolvedNode::Mu { .. } => {
                return Err(operations.fail(
                    TypeErrorKind::InternalInvariant {
                        detail: "unprojectable node passed validation".to_owned(),
                    },
                    &[],
                    "project.rust",
                ));
            }
        };
        rendered.push(printed);
    }
    let mut source = "#[derive(Clone, Copy, Debug, PartialEq, Eq)]\npub struct KernelSpan { pub start: u32, pub end: u32 }\n".to_owned();
    source.push_str(&declarations);
    for key in plan.solution.export_keys() {
        let root = plan
            .solution
            .export_root(key)
            .and_then(|root| plan.solution.root_type(root))
            .ok_or_else(|| {
                operations.fail(
                    TypeErrorKind::InternalInvariant {
                        detail: "missing export".to_owned(),
                    },
                    &[],
                    "project.rust",
                )
            })?;
        let name = symbols.intern(format!("export:{}", key.as_str()), key.as_str(), operations)?;
        source.push_str(&format!(
            "pub type {name} = {};\n",
            rendered[root.0 as usize].text
        ));
    }
    Ok(source)
}

fn emit_typescript(
    plan: &LoweringPlan,
    operations: &mut OperationContext,
) -> Result<String, TypeError> {
    let mut symbols = Symbols::new(TargetLanguage::TypeScript);
    let mut rendered = Vec::<Printed>::with_capacity(plan.solution.nodes.len());
    for node in &plan.solution.nodes {
        let printed = match node {
            SolvedNode::Primitive(value) => atomic(ts_primitive(*value)),
            SolvedNode::Named(name) => Printed {
                text: symbols.intern(format!("named:{name}"), name, operations)?,
                precedence: Precedence::Atomic,
            },
            SolvedNode::Tuple(items) => atomic(&format!(
                "readonly [{}]",
                items
                    .iter()
                    .map(|id| rendered[id.0 as usize].text.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
            SolvedNode::List(item) => atomic(&format!(
                "ReadonlyArray<{}>",
                rendered[item.0 as usize].text
            )),
            SolvedNode::Option(item) => Printed {
                text: format!(
                    "{} | null",
                    rendered[item.0 as usize].grouped_below(Precedence::Atomic)
                ),
                precedence: Precedence::Union,
            },
            SolvedNode::Function { parameters, result } => Printed {
                text: format!(
                    "({}) => {}",
                    parameters
                        .iter()
                        .enumerate()
                        .map(|(index, id)| format!("arg{index}: {}", rendered[id.0 as usize].text))
                        .collect::<Vec<_>>()
                        .join(", "),
                    rendered[result.0 as usize].text
                ),
                precedence: Precedence::Function,
            },
            SolvedNode::Record(row) => atomic(&ts_record(
                &plan.solution.rows[row.0 as usize],
                &rendered,
                operations,
            )?),
            SolvedNode::Variant(row) => {
                ts_variant(&plan.solution.rows[row.0 as usize], &rendered, operations)?
            }
            SolvedNode::Free { .. } | SolvedNode::Rec(_) | SolvedNode::Mu { .. } => {
                return Err(operations.fail(
                    TypeErrorKind::InternalInvariant {
                        detail: "unprojectable node passed validation".to_owned(),
                    },
                    &[],
                    "project.typescript",
                ));
            }
        };
        rendered.push(printed);
    }
    let mut source = String::new();
    for key in plan.solution.export_keys() {
        let root = plan
            .solution
            .export_root(key)
            .and_then(|root| plan.solution.root_type(root))
            .ok_or_else(|| {
                operations.fail(
                    TypeErrorKind::InternalInvariant {
                        detail: "missing export".to_owned(),
                    },
                    &[],
                    "project.typescript",
                )
            })?;
        let name = symbols.intern(format!("export:{}", key.as_str()), key.as_str(), operations)?;
        source.push_str(&format!(
            "export type {name} = {};\n",
            rendered[root.0 as usize].text
        ));
    }
    Ok(source)
}

fn rust_row(
    name: &str,
    row: &SolvedRow,
    rendered: &[Printed],
    variant: bool,
    operations: &mut OperationContext,
) -> Result<String, TypeError> {
    let labels = local_labels(
        row.fields.iter().map(|(label, _)| label),
        variant,
        operations,
    )?;
    let mut source = if variant {
        format!("#[derive(Clone, Debug, PartialEq)]\npub enum {name} {{\n")
    } else {
        format!("#[derive(Clone, Debug, PartialEq)]\npub struct {name} {{\n")
    };
    for ((_, ty), label) in row.fields.iter().zip(labels) {
        if variant {
            source.push_str(&format!("    {label}({}),\n", rendered[ty.0 as usize].text));
        } else {
            source.push_str(&format!(
                "    pub {label}: {},\n",
                rendered[ty.0 as usize].text
            ));
        }
    }
    source.push_str("}\n");
    Ok(source)
}

fn ts_record(
    row: &SolvedRow,
    rendered: &[Printed],
    operations: &mut OperationContext,
) -> Result<String, TypeError> {
    let mut fields = Vec::new();
    for (label, ty) in &row.fields {
        charge_label(label, "project.ts.label", operations)?;
        fields.push(format!(
            "readonly {}: {}",
            ts_property(label),
            rendered[ty.0 as usize].text
        ));
    }
    Ok(format!("{{ {} }}", fields.join("; ")))
}

fn ts_variant(
    row: &SolvedRow,
    rendered: &[Printed],
    operations: &mut OperationContext,
) -> Result<Printed, TypeError> {
    if row.fields.is_empty() {
        return Ok(atomic("never"));
    }
    let mut variants = Vec::new();
    for (label, ty) in &row.fields {
        charge_label(label, "project.ts.variant", operations)?;
        variants.push(format!(
            "{{ readonly tag: {}; readonly value: {} }}",
            ts_string(label),
            rendered[ty.0 as usize].text
        ));
    }
    Ok(Printed {
        text: variants.join(" | "),
        precedence: Precedence::Union,
    })
}

fn local_labels<'a>(
    labels: impl IntoIterator<Item = &'a String>,
    variant: bool,
    operations: &mut OperationContext,
) -> Result<Vec<String>, TypeError> {
    let mut used = BTreeSet::new();
    let mut output = Vec::new();
    for label in labels {
        charge_label(label, "project.rust.label", operations)?;
        let base = if variant {
            rust_type_identifier(label)
        } else {
            rust_field_identifier(label)
        };
        let mut candidate = base.clone();
        let mut suffix = 2;
        while !used.insert(candidate.clone()) {
            candidate = format!("{base}_{suffix}");
            suffix += 1;
        }
        output.push(candidate);
    }
    Ok(output)
}

fn charge_label(
    label: &str,
    operation: &str,
    operations: &mut OperationContext,
) -> Result<(), TypeError> {
    operations.charge(
        label.len().saturating_add(label.chars().count()).max(1),
        operation,
        &[],
        [Origin::new(operation, label)],
    )?;
    Ok(())
}

fn atomic(text: &str) -> Printed {
    Printed {
        text: text.to_owned(),
        precedence: Precedence::Atomic,
    }
}

fn rust_tuple(items: &[SolvedTypeId], rendered: &[Printed]) -> String {
    match items {
        [] => "()".to_owned(),
        [item] => format!("({},)", rendered[item.0 as usize].text),
        _ => format!(
            "({})",
            items
                .iter()
                .map(|id| rendered[id.0 as usize].text.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

fn normalize_identifier(requested: &str, language: TargetLanguage) -> String {
    let mut output = String::new();
    for character in requested.chars() {
        if character.is_ascii_alphanumeric() || character == '_' {
            output.push(character);
        } else if character.is_alphanumeric() {
            output.push_str(&format!("u{:x}", character as u32));
        } else {
            output.push('_');
        }
    }
    while output.contains("__") {
        output = output.replace("__", "_");
    }
    output = output.trim_matches('_').to_owned();
    if output.is_empty() {
        output.push_str("Generated");
    }
    if output.starts_with(|character: char| character.is_ascii_digit()) {
        output.insert(0, '_');
    }
    let keywords = if language == TargetLanguage::Rust {
        RUST_KEYWORDS
    } else {
        TS_KEYWORDS
    };
    if keywords.contains(&output.as_str()) {
        output.push_str("_type");
    }
    output
}

fn rust_type_identifier(requested: &str) -> String {
    let mut output = String::new();
    let mut uppercase = true;
    for character in requested.chars() {
        if character.is_ascii_alphanumeric() {
            if uppercase {
                output.extend(character.to_uppercase());
                uppercase = false;
            } else {
                output.push(character);
            }
        } else if character.is_alphanumeric() {
            output.push('U');
            output.push_str(&format!("{:x}", character as u32));
            uppercase = true;
        } else {
            uppercase = true;
        }
    }
    if output.is_empty() {
        output.push_str("Generated");
    }
    if output.starts_with(|character: char| character.is_ascii_digit()) {
        output.insert_str(0, "Type");
    }
    if RUST_KEYWORDS.contains(&output.as_str()) {
        output.push_str("Type");
    }
    output
}

fn rust_field_identifier(requested: &str) -> String {
    let mut output = String::new();
    let mut separator = false;
    for character in requested.chars() {
        if character.is_ascii_alphanumeric() || character == '_' {
            if separator && !output.is_empty() && !output.ends_with('_') {
                output.push('_');
            }
            output.extend(character.to_lowercase());
            separator = false;
        } else {
            separator = true;
        }
    }
    output = output.trim_matches('_').to_owned();
    if output.is_empty() {
        output.push_str("field");
    }
    if output.starts_with(|character: char| character.is_ascii_digit()) {
        output.insert_str(0, "field_");
    }
    if RUST_KEYWORDS.contains(&output.as_str()) {
        output.push('_');
    }
    output
}

fn ts_property(label: &str) -> String {
    let normalized = normalize_identifier(label, TargetLanguage::TypeScript);
    if normalized == label && !TS_KEYWORDS.contains(&label) {
        normalized
    } else {
        ts_string(label)
    }
}

fn ts_string(value: &str) -> String {
    let mut output = String::from("\"");
    for character in value.chars() {
        match character {
            '"' => output.push_str("\\\""),
            '\\' => output.push_str("\\\\"),
            '\n' => output.push_str("\\n"),
            '\r' => output.push_str("\\r"),
            '\t' => output.push_str("\\t"),
            character if character.is_control() => {
                output.push_str(&format!("\\u{{{:x}}}", character as u32))
            }
            character => output.push(character),
        }
    }
    output.push('"');
    output
}

const fn rust_primitive(value: Primitive) -> &'static str {
    match value {
        Primitive::Bool => "bool",
        Primitive::Bytes => "Vec<u8>",
        Primitive::Diagnostic | Primitive::Str => "String",
        Primitive::Enum | Primitive::U32 => "u32",
        Primitive::BoxedEnum => "Box<u32>",
        Primitive::F64 => "f64",
        Primitive::I8 => "i8",
        Primitive::I16 => "i16",
        Primitive::I32 => "i32",
        Primitive::I64 => "i64",
        Primitive::ParseResult => "Result<(), String>",
        Primitive::Span => "KernelSpan",
        Primitive::U8 => "u8",
        Primitive::U16 => "u16",
        Primitive::U64 => "u64",
        Primitive::Unit => "()",
    }
}

const fn ts_primitive(value: Primitive) -> &'static str {
    match value {
        Primitive::Bool => "boolean",
        Primitive::Bytes => "Uint8Array",
        Primitive::Diagnostic | Primitive::Str => "string",
        Primitive::Enum
        | Primitive::F64
        | Primitive::I8
        | Primitive::I16
        | Primitive::I32
        | Primitive::I64
        | Primitive::U8
        | Primitive::U16
        | Primitive::U32
        | Primitive::U64 => "number",
        Primitive::BoxedEnum => "{ readonly boxed: number }",
        Primitive::ParseResult => "{ readonly ok: boolean }",
        Primitive::Span => "{ readonly start: number; readonly end: number }",
        Primitive::Unit => "undefined",
    }
}

const RUST_KEYWORDS: &[&str] = &[
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
    "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
    "while", "async", "await", "dyn", "try",
];
const TS_KEYWORDS: &[&str] = &[
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "import",
    "in",
    "instanceof",
    "new",
    "null",
    "return",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    "as",
    "implements",
    "interface",
    "let",
    "package",
    "private",
    "protected",
    "public",
    "static",
    "yield",
    "any",
    "boolean",
    "constructor",
    "declare",
    "get",
    "module",
    "require",
    "number",
    "set",
    "string",
    "symbol",
    "type",
    "from",
    "of",
];
