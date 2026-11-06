//! Target-safe Rust and TypeScript projection with artifact provenance.

use std::collections::BTreeMap;
use std::fmt;

use super::solve::{SolvedRow, SolvedType};
use super::store::Primitive;
use super::{OperationContext, Origin, TypeError};

pub const GENERATOR_VERSION: &str = "bbnf-type-kernel-v3/0.3.0";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TargetLanguage {
    Rust,
    TypeScript,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ToolchainIdentity {
    name: String,
    version: String,
}

impl ToolchainIdentity {
    #[must_use]
    pub fn new(name: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            version: version.into(),
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn version(&self) -> &str {
        &self.version
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArtifactProvenance {
    source_digest: String,
    semantic_manifest_digest: String,
    generator_version: String,
    target_toolchain: ToolchainIdentity,
    reproduction_command: String,
}

impl ArtifactProvenance {
    #[must_use]
    pub fn source_digest(&self) -> &str {
        &self.source_digest
    }

    #[must_use]
    pub fn semantic_manifest_digest(&self) -> &str {
        &self.semantic_manifest_digest
    }

    #[must_use]
    pub fn generator_version(&self) -> &str {
        &self.generator_version
    }

    #[must_use]
    pub fn target_toolchain(&self) -> &ToolchainIdentity {
        &self.target_toolchain
    }

    #[must_use]
    pub fn reproduction_command(&self) -> &str {
        &self.reproduction_command
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GeneratedArtifact {
    target: TargetLanguage,
    source: String,
    exported_name: String,
    provenance: ArtifactProvenance,
}

impl GeneratedArtifact {
    #[must_use]
    pub fn target(&self) -> TargetLanguage {
        self.target
    }

    #[must_use]
    pub fn source(&self) -> &str {
        &self.source
    }

    #[must_use]
    pub fn exported_name(&self) -> &str {
        &self.exported_name
    }

    #[must_use]
    pub fn provenance(&self) -> &ArtifactProvenance {
        &self.provenance
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ProjectErrorKind {
    OpenTypeVariable,
    OpenRow,
    RecursiveTypeRequiresCarrier,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ProjectError {
    kind: ProjectErrorKind,
    site: String,
}

impl ProjectError {
    fn new(kind: ProjectErrorKind, site: impl Into<String>) -> Self {
        Self {
            kind,
            site: site.into(),
        }
    }

    #[must_use]
    pub fn kind(&self) -> &ProjectErrorKind {
        &self.kind
    }

    #[must_use]
    pub fn site(&self) -> &str {
        &self.site
    }
}

impl fmt::Display for ProjectError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{:?} at {}", self.kind, self.site)
    }
}

impl std::error::Error for ProjectError {}

#[derive(Debug)]
pub enum ProjectErrorOrBudget {
    Project(ProjectError),
    Budget(TypeError),
}

impl fmt::Display for ProjectErrorOrBudget {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Project(error) => error.fmt(formatter),
            Self::Budget(error) => error.fmt(formatter),
        }
    }
}

impl std::error::Error for ProjectErrorOrBudget {}

#[derive(Clone, Debug)]
pub struct IdentifierAllocator {
    target: TargetLanguage,
    style: IdentifierStyle,
    used: BTreeMap<String, u32>,
}

#[derive(Clone, Copy, Debug)]
pub enum IdentifierStyle {
    Type,
    Field,
}

impl IdentifierAllocator {
    #[must_use]
    pub fn new(target: TargetLanguage, style: IdentifierStyle) -> Self {
        Self {
            target,
            style,
            used: BTreeMap::new(),
        }
    }

    pub fn allocate(&mut self, source: &str) -> String {
        let normalized = normalize_identifier(source, self.style);
        let escaped = escape_keyword(self.target, normalized, self.style);
        let count = self.used.entry(escaped.clone()).or_insert(0);
        *count += 1;
        if *count == 1 {
            escaped
        } else {
            format!("{escaped}_{}", *count)
        }
    }
}

pub fn generate(
    target: TargetLanguage,
    requested_name: &str,
    ty: &SolvedType,
    toolchain: ToolchainIdentity,
    reproduction_command: impl Into<String>,
    operations: &mut OperationContext,
) -> Result<GeneratedArtifact, ProjectErrorOrBudget> {
    operations
        .step(
            "project.generate",
            &[],
            [Origin::new("project.root", requested_name)],
        )
        .map_err(ProjectErrorOrBudget::Budget)?;
    let mut names = IdentifierAllocator::new(target, IdentifierStyle::Type);
    let exported_name = names.allocate(requested_name);
    let body = match target {
        TargetLanguage::Rust => {
            let mut emitter = RustEmitter {
                names,
                fields: IdentifierAllocator::new(target, IdentifierStyle::Field),
                definitions: Vec::new(),
                operations,
            };
            emitter.named(&exported_name, ty)?;
            emitter.definitions.join("\n\n") + "\n"
        }
        TargetLanguage::TypeScript => {
            let mut emitter = TypeScriptEmitter {
                names,
                definitions: Vec::new(),
                operations,
            };
            emitter.named(&exported_name, ty)?;
            emitter.definitions.join("\n\n") + "\n"
        }
    };
    let source_digest = digest(body.as_bytes());
    let semantic_manifest_digest = digest(format!("{requested_name}:{ty:?}").as_bytes());
    let reproduction_command = reproduction_command.into();
    let comment = match target {
        TargetLanguage::Rust => format!(
            "// generator: {GENERATOR_VERSION}\n// source-digest: {source_digest}\n// semantic-digest: {semantic_manifest_digest}\n// toolchain: {} {}\n// reproduce: {}\n",
            toolchain.name, toolchain.version, reproduction_command
        ),
        TargetLanguage::TypeScript => format!(
            "// generator: {GENERATOR_VERSION}\n// source-digest: {source_digest}\n// semantic-digest: {semantic_manifest_digest}\n// toolchain: {} {}\n// reproduce: {}\n",
            toolchain.name, toolchain.version, reproduction_command
        ),
    };
    let source = comment + &body;
    Ok(GeneratedArtifact {
        target,
        source,
        exported_name,
        provenance: ArtifactProvenance {
            source_digest,
            semantic_manifest_digest,
            generator_version: GENERATOR_VERSION.to_owned(),
            target_toolchain: toolchain,
            reproduction_command,
        },
    })
}

struct RustEmitter<'operations> {
    names: IdentifierAllocator,
    fields: IdentifierAllocator,
    definitions: Vec<String>,
    operations: &'operations mut OperationContext,
}

impl RustEmitter<'_> {
    fn named(&mut self, name: &str, ty: &SolvedType) -> Result<(), ProjectErrorOrBudget> {
        self.operations
            .step(
                "project.rust.named",
                &[],
                [Origin::new("project.rust", name)],
            )
            .map_err(ProjectErrorOrBudget::Budget)?;
        match ty {
            SolvedType::Record(row) => {
                ensure_closed(row)?;
                let mut fields = Vec::with_capacity(row.fields().len());
                for (label, ty) in row.fields() {
                    let field = self.fields.allocate(label);
                    let field_ty = self.ty(ty, &format!("{name}_{label}"))?;
                    fields.push(format!("    pub {field}: {field_ty},"));
                }
                self.definitions.push(format!(
                    "#[derive(Clone, Debug, PartialEq)]\npub struct {name} {{\n{}\n}}",
                    fields.join("\n")
                ));
            }
            SolvedType::Variant(row) => {
                ensure_closed(row)?;
                let mut variants = Vec::with_capacity(row.fields().len());
                for (label, ty) in row.fields() {
                    let variant = self.names.allocate(label);
                    let payload = self.ty(ty, &format!("{name}_{label}"))?;
                    variants.push(format!("    {variant}({payload}),"));
                }
                self.definitions.push(format!(
                    "#[derive(Clone, Debug, PartialEq)]\npub enum {name} {{\n{}\n}}",
                    variants.join("\n")
                ));
            }
            _ => {
                let ty = self.ty(ty, &format!("{name}_Value"))?;
                self.definitions.push(format!("pub type {name} = {ty};"));
            }
        }
        Ok(())
    }

    fn ty(&mut self, ty: &SolvedType, hint: &str) -> Result<String, ProjectErrorOrBudget> {
        self.operations
            .step(
                "project.rust.type",
                &[],
                [Origin::new("project.rust.type", hint)],
            )
            .map_err(ProjectErrorOrBudget::Budget)?;
        match ty {
            SolvedType::Free(_) => Err(open_type()),
            SolvedType::Rec(_) | SolvedType::Mu(_) => Err(recursive_type()),
            SolvedType::Primitive(primitive) => Ok(rust_primitive(*primitive).to_owned()),
            SolvedType::Named(name) => Ok(self.names.allocate(name)),
            SolvedType::Tuple(items) => {
                let mut types = items
                    .iter()
                    .enumerate()
                    .map(|(index, item)| self.ty(item, &format!("{hint}_{index}")))
                    .collect::<Result<Vec<_>, _>>()?;
                if types.len() == 1 {
                    Ok(format!("({},)", types.remove(0)))
                } else {
                    Ok(format!("({})", types.join(", ")))
                }
            }
            SolvedType::List(item) => Ok(format!("Vec<{}>", self.ty(item, hint)?)),
            SolvedType::Option(item) => Ok(format!("Option<{}>", self.ty(item, hint)?)),
            SolvedType::Function { parameters, result } => {
                let parameters = parameters
                    .iter()
                    .enumerate()
                    .map(|(index, parameter)| self.ty(parameter, &format!("{hint}_{index}")))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(format!(
                    "fn({}) -> {}",
                    parameters.join(", "),
                    self.ty(result, &format!("{hint}_result"))?
                ))
            }
            SolvedType::Record(_) | SolvedType::Variant(_) => {
                let nested = self.names.allocate(hint);
                self.named(&nested, ty)?;
                Ok(nested)
            }
        }
    }
}

struct TypeScriptEmitter<'operations> {
    names: IdentifierAllocator,
    definitions: Vec<String>,
    operations: &'operations mut OperationContext,
}

impl TypeScriptEmitter<'_> {
    fn named(&mut self, name: &str, ty: &SolvedType) -> Result<(), ProjectErrorOrBudget> {
        self.operations
            .step(
                "project.typescript.named",
                &[],
                [Origin::new("project.typescript", name)],
            )
            .map_err(ProjectErrorOrBudget::Budget)?;
        match ty {
            SolvedType::Record(row) => {
                ensure_closed(row)?;
                let mut fields = Vec::with_capacity(row.fields().len());
                for (label, ty) in row.fields() {
                    let field_ty = self.ty(ty, &format!("{name}_{label}"))?;
                    fields.push(format!("  readonly {}: {field_ty};", string_literal(label)));
                }
                self.definitions.push(format!(
                    "export interface {name} {{\n{}\n}}",
                    fields.join("\n")
                ));
            }
            SolvedType::Variant(row) => {
                ensure_closed(row)?;
                let mut variants = Vec::with_capacity(row.fields().len());
                for (label, ty) in row.fields() {
                    let payload = self.ty(ty, &format!("{name}_{label}"))?;
                    variants.push(format!(
                        "{{ readonly kind: {}; readonly value: {payload} }}",
                        string_literal(label)
                    ));
                }
                let union = if variants.is_empty() {
                    "never".to_owned()
                } else {
                    variants.join(" | ")
                };
                self.definitions
                    .push(format!("export type {name} = {union};"));
            }
            _ => {
                let ty = self.ty(ty, &format!("{name}_Value"))?;
                self.definitions.push(format!("export type {name} = {ty};"));
            }
        }
        Ok(())
    }

    fn ty(&mut self, ty: &SolvedType, hint: &str) -> Result<String, ProjectErrorOrBudget> {
        self.operations
            .step(
                "project.typescript.type",
                &[],
                [Origin::new("project.typescript.type", hint)],
            )
            .map_err(ProjectErrorOrBudget::Budget)?;
        match ty {
            SolvedType::Free(_) => Err(open_type()),
            SolvedType::Rec(_) | SolvedType::Mu(_) => Err(recursive_type()),
            SolvedType::Primitive(primitive) => Ok(typescript_primitive(*primitive).to_owned()),
            SolvedType::Named(name) => Ok(self.names.allocate(name)),
            SolvedType::Tuple(items) => Ok(format!(
                "readonly [{}]",
                items
                    .iter()
                    .enumerate()
                    .map(|(index, item)| self.ty(item, &format!("{hint}_{index}")))
                    .collect::<Result<Vec<_>, _>>()?
                    .join(", ")
            )),
            SolvedType::List(item) => Ok(format!("ReadonlyArray<{}>", self.ty(item, hint)?)),
            SolvedType::Option(item) => Ok(format!("{} | null", self.ty(item, hint)?)),
            SolvedType::Function { parameters, result } => {
                let parameters = parameters
                    .iter()
                    .enumerate()
                    .map(|(index, parameter)| {
                        self.ty(parameter, &format!("{hint}_{index}"))
                            .map(|ty| format!("arg{index}: {ty}"))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(format!(
                    "({}) => {}",
                    parameters.join(", "),
                    self.ty(result, &format!("{hint}_result"))?
                ))
            }
            SolvedType::Record(_) | SolvedType::Variant(_) => {
                let nested = self.names.allocate(hint);
                self.named(&nested, ty)?;
                Ok(nested)
            }
        }
    }
}

fn ensure_closed(row: &SolvedRow) -> Result<(), ProjectErrorOrBudget> {
    if row.tail().is_some() {
        Err(ProjectErrorOrBudget::Project(ProjectError::new(
            ProjectErrorKind::OpenRow,
            "project.row",
        )))
    } else {
        Ok(())
    }
}

fn open_type() -> ProjectErrorOrBudget {
    ProjectErrorOrBudget::Project(ProjectError::new(
        ProjectErrorKind::OpenTypeVariable,
        "project.type",
    ))
}

fn recursive_type() -> ProjectErrorOrBudget {
    ProjectErrorOrBudget::Project(ProjectError::new(
        ProjectErrorKind::RecursiveTypeRequiresCarrier,
        "project.recursion",
    ))
}

fn rust_primitive(primitive: Primitive) -> &'static str {
    match primitive {
        Primitive::Bool => "bool",
        Primitive::Bytes => "Vec<u8>",
        Primitive::Diagnostic => "String",
        Primitive::F64 => "f64",
        Primitive::I8 => "i8",
        Primitive::I16 => "i16",
        Primitive::I32 => "i32",
        Primitive::I64 => "i64",
        Primitive::ParseResult => "String",
        Primitive::Span => "(u32, u32)",
        Primitive::Str => "String",
        Primitive::U8 => "u8",
        Primitive::U16 => "u16",
        Primitive::U32 => "u32",
        Primitive::U64 => "u64",
        Primitive::Unit => "()",
    }
}

fn typescript_primitive(primitive: Primitive) -> &'static str {
    match primitive {
        Primitive::Bool => "boolean",
        Primitive::Bytes => "Uint8Array",
        Primitive::Diagnostic | Primitive::ParseResult | Primitive::Str => "string",
        Primitive::F64
        | Primitive::I8
        | Primitive::I16
        | Primitive::I32
        | Primitive::I64
        | Primitive::U8
        | Primitive::U16
        | Primitive::U32
        | Primitive::U64 => "number",
        Primitive::Span => "readonly [number, number]",
        Primitive::Unit => "null",
    }
}

fn normalize_identifier(source: &str, style: IdentifierStyle) -> String {
    let mut words = Vec::new();
    let mut word = String::new();
    for character in source.chars() {
        if character.is_ascii_alphanumeric() {
            word.push(character);
        } else if character == '_' || character == '-' || character.is_ascii_whitespace() {
            if !word.is_empty() {
                words.push(std::mem::take(&mut word));
            }
        } else {
            if !word.is_empty() {
                words.push(std::mem::take(&mut word));
            }
            words.push(format!("u{:04x}", u32::from(character)));
        }
    }
    if !word.is_empty() {
        words.push(word);
    }
    let mut normalized = match style {
        IdentifierStyle::Type => words
            .iter()
            .map(|word| {
                let mut characters = word.chars();
                characters.next().map_or_else(String::new, |first| {
                    first.to_ascii_uppercase().to_string() + characters.as_str()
                })
            })
            .collect::<String>(),
        IdentifierStyle::Field => words
            .iter()
            .map(|word| word.to_ascii_lowercase())
            .collect::<Vec<_>>()
            .join("_"),
    };
    if normalized.is_empty() {
        normalized = match style {
            IdentifierStyle::Type => "BbnfEmpty".to_owned(),
            IdentifierStyle::Field => "bbnf_empty".to_owned(),
        };
    }
    if normalized.starts_with(|character: char| character.is_ascii_digit()) {
        normalized = match style {
            IdentifierStyle::Type => format!("Bbnf{normalized}"),
            IdentifierStyle::Field => format!("bbnf_{normalized}"),
        };
    }
    normalized
}

fn escape_keyword(target: TargetLanguage, normalized: String, style: IdentifierStyle) -> String {
    let reserved = match target {
        TargetLanguage::Rust => RUST_KEYWORDS.contains(&normalized.as_str()),
        TargetLanguage::TypeScript => TYPESCRIPT_KEYWORDS.contains(&normalized.as_str()),
    };
    if reserved {
        match style {
            IdentifierStyle::Type => format!("Bbnf{normalized}"),
            IdentifierStyle::Field => format!("bbnf_{normalized}"),
        }
    } else {
        normalized
    }
}

fn string_literal(value: &str) -> String {
    let mut escaped = String::from("\"");
    for character in value.chars() {
        match character {
            '\\' => escaped.push_str("\\\\"),
            '\"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            character if character.is_control() => {
                escaped.push_str(&format!("\\u{{{:x}}}", u32::from(character)));
            }
            character => escaped.push(character),
        }
    }
    escaped.push('\"');
    escaped
}

fn digest(bytes: &[u8]) -> String {
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    format!("fnv1a64:{hash:016x}")
}

const RUST_KEYWORDS: &[&str] = &[
    "Self", "abstract", "as", "async", "await", "become", "box", "break", "const", "continue",
    "crate", "do", "dyn", "else", "enum", "extern", "false", "final", "fn", "for", "gen", "if",
    "impl", "in", "let", "loop", "macro", "match", "mod", "move", "mut", "override", "priv", "pub",
    "ref", "return", "self", "static", "struct", "super", "trait", "true", "try", "type", "typeof",
    "union", "unsafe", "unsized", "use", "virtual", "where", "while", "yield",
];

const TYPESCRIPT_KEYWORDS: &[&str] = &[
    "any",
    "as",
    "async",
    "await",
    "boolean",
    "break",
    "case",
    "catch",
    "class",
    "const",
    "constructor",
    "continue",
    "debugger",
    "declare",
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
    "from",
    "function",
    "get",
    "if",
    "implements",
    "import",
    "in",
    "infer",
    "instanceof",
    "interface",
    "keyof",
    "let",
    "module",
    "namespace",
    "never",
    "new",
    "null",
    "number",
    "object",
    "of",
    "package",
    "private",
    "protected",
    "public",
    "readonly",
    "require",
    "return",
    "set",
    "static",
    "string",
    "super",
    "switch",
    "symbol",
    "this",
    "throw",
    "true",
    "try",
    "type",
    "typeof",
    "undefined",
    "unique",
    "unknown",
    "var",
    "void",
    "while",
    "with",
    "yield",
];
