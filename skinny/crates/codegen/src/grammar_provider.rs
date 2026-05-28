use crate::{runtime_generator, CodegenError, EmittedSource};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeGenerationRequest {
    pub grammar_name: String,
    pub profile_id: String,
    pub entry_rule: String,
    pub source_roots: Vec<String>,
    pub sources: Vec<RuntimeGrammarSource>,
    pub workspace_metadata: RuntimeWorkspaceMetadata,
    pub output_dir: String,
    pub profile_contract: RuntimeProfileContract,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeGrammarSource {
    pub rel_path: String,
    pub source: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeWorkspaceMetadata {
    pub repo_manifest_path: String,
    pub skinny_manifest_path: String,
    pub grammar_manifest_path: String,
    pub generated_root: String,
    pub host_registry: String,
    pub profile: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeProfileContract {
    pub emitter: RuntimeEmitterKind,
    pub expected_files: &'static [&'static str],
    pub frontend_requirements: RuntimeFrontendRequirements,
    pub output_labels: Option<RuntimeOutputLabels>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RuntimeEmitterKind {
    CompiledLowering,
    RequestFacts,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RuntimeFrontendRequirements {
    pub import_closure: bool,
    pub whitespace_directive: bool,
    pub whitespace_modifier: bool,
    pub discard_operator: bool,
    pub pretty_directive: bool,
    pub host_capture: bool,
    pub projection: bool,
    pub typed_projection: bool,
    pub token_directive: bool,
    pub comma: bool,
}

impl RuntimeFrontendRequirements {
    pub const fn none() -> Self {
        Self {
            import_closure: false,
            whitespace_directive: false,
            whitespace_modifier: false,
            discard_operator: false,
            pretty_directive: false,
            host_capture: false,
            projection: false,
            typed_projection: false,
            token_directive: false,
            comma: false,
        }
    }

    pub const fn full_request_facts() -> Self {
        Self {
            import_closure: true,
            whitespace_directive: true,
            whitespace_modifier: true,
            discard_operator: true,
            pretty_directive: true,
            host_capture: true,
            projection: true,
            typed_projection: true,
            token_directive: true,
            comma: true,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RuntimeOutputLabels {
    pub fact_schema: &'static str,
    pub row_id: &'static str,
    pub output_plane: &'static str,
}

pub fn emit_runtime_from_request(
    request: RuntimeGenerationRequest,
) -> Result<EmittedSource, CodegenError> {
    validate_metadata(&request.workspace_metadata)?;
    validate_request_shape(&request)?;
    let source_refs = request
        .sources
        .iter()
        .map(|source| grammar::RuntimeSource::new(&source.rel_path, &source.source))
        .collect::<Vec<_>>();
    let facts = grammar::parse_runtime_source_facts(&source_refs)?;
    validate_frontend_closure(&request, &facts)?;
    if request.profile_contract.emitter != RuntimeEmitterKind::RequestFacts {
        if let Some(unsupported) = facts.first_unsupported() {
            return Err(CodegenError::UnsupportedRuntimeConstruct {
                code: unsupported.code,
                path: unsupported.path,
                offset: unsupported.offset,
                source_hash: unsupported.source_hash,
            });
        }
    }
    validate_frontend_requirements(&request.profile_contract.frontend_requirements, &facts)?;
    runtime_generator::emit_from_request(&request, &facts)
}

fn validate_metadata(metadata: &RuntimeWorkspaceMetadata) -> Result<(), CodegenError> {
    for (field, value) in [
        ("repo_manifest_path", &metadata.repo_manifest_path),
        ("skinny_manifest_path", &metadata.skinny_manifest_path),
        ("grammar_manifest_path", &metadata.grammar_manifest_path),
        ("generated_root", &metadata.generated_root),
        ("host_registry", &metadata.host_registry),
        ("profile", &metadata.profile),
    ] {
        if value.trim().is_empty() {
            return Err(CodegenError::Lowering(format!(
                "runtime request missing workspace metadata `{field}`"
            )));
        }
    }
    Ok(())
}

fn validate_request_shape(request: &RuntimeGenerationRequest) -> Result<(), CodegenError> {
    if request.grammar_name.trim().is_empty()
        || request.profile_id.trim().is_empty()
        || request.entry_rule.trim().is_empty()
        || request.output_dir.trim().is_empty()
        || request.source_roots.is_empty()
        || request.sources.is_empty()
    {
        return Err(CodegenError::Lowering(
            "runtime request missing grammar/profile/source/output fields".to_string(),
        ));
    }
    for root in &request.source_roots {
        if !request
            .sources
            .iter()
            .any(|source| &source.rel_path == root)
        {
            return Err(CodegenError::Lowering(format!(
                "runtime request source root `{root}` missing from source map"
            )));
        }
    }
    Ok(())
}

fn validate_frontend_closure(
    request: &RuntimeGenerationRequest,
    facts: &grammar::RuntimeSourceFacts,
) -> Result<(), CodegenError> {
    let frontend = &facts.frontend;
    if frontend.source_hash.trim().is_empty() || frontend.source_hash != facts.source_hash {
        return Err(CodegenError::Lowering(
            "runtime request frontend closure hash mismatch".to_string(),
        ));
    }
    if frontend.sources.len() != request.sources.len() {
        return Err(CodegenError::Lowering(format!(
            "runtime request frontend closure saw {} sources but request supplied {}",
            frontend.sources.len(),
            request.sources.len()
        )));
    }
    let mut source_hashes = std::collections::BTreeMap::new();
    for source in &frontend.sources {
        if source.path.trim().is_empty() || source.source_hash.trim().is_empty() {
            return Err(CodegenError::Lowering(
                "runtime request frontend closure contains an empty source identity".to_string(),
            ));
        }
        if source_hashes
            .insert(source.path.as_str(), source.source_hash.as_str())
            .is_some()
        {
            return Err(CodegenError::Lowering(format!(
                "runtime request frontend closure duplicated source `{}`",
                source.path
            )));
        }
    }
    for source in &request.sources {
        if !source_hashes.contains_key(source.rel_path.as_str()) {
            return Err(CodegenError::Lowering(format!(
                "runtime request frontend closure missing request source `{}`",
                source.rel_path
            )));
        }
    }
    for root in &request.source_roots {
        if !source_hashes.contains_key(root.as_str()) {
            return Err(CodegenError::Lowering(format!(
                "runtime request frontend closure missing source root `{root}`"
            )));
        }
    }
    for import in &frontend.imports {
        if import.specifier.trim().is_empty()
            || import.importer_source_hash.trim().is_empty()
            || import.resolved_source_hash.trim().is_empty()
        {
            return Err(CodegenError::Lowering(
                "runtime request frontend closure contains an empty import identity".to_string(),
            ));
        }
        let importer_hash = source_hashes
            .get(import.importer_path.as_str())
            .ok_or_else(|| {
                CodegenError::Lowering(format!(
                    "runtime request frontend closure import source `{}` is outside the request",
                    import.importer_path
                ))
            })?;
        if *importer_hash != import.importer_source_hash {
            return Err(CodegenError::Lowering(format!(
                "runtime request frontend closure import source hash mismatch for `{}`",
                import.importer_path
            )));
        }
        let resolved_hash = source_hashes
            .get(import.resolved_path.as_str())
            .ok_or_else(|| {
                CodegenError::Lowering(format!(
                    "runtime request frontend closure import target `{}` is outside the request",
                    import.resolved_path
                ))
            })?;
        if *resolved_hash != import.resolved_source_hash {
            return Err(CodegenError::Lowering(format!(
                "runtime request frontend closure import target hash mismatch for `{}`",
                import.resolved_path
            )));
        }
    }
    Ok(())
}

fn validate_frontend_requirements(
    requirements: &RuntimeFrontendRequirements,
    facts: &grammar::RuntimeSourceFacts,
) -> Result<(), CodegenError> {
    let frontend = &facts.frontend;
    if requirements.import_closure && frontend.imports.is_empty() {
        return Err(frontend_missing("import closure"));
    }
    if requirements.whitespace_directive && frontend.layout.whitespace_directives.is_empty() {
        return Err(frontend_missing("whitespace directive"));
    }
    if requirements.whitespace_modifier && frontend.layout.whitespace_modifiers.is_empty() {
        return Err(frontend_missing("whitespace modifier"));
    }
    if requirements.discard_operator && frontend.layout.discard_operators.is_empty() {
        return Err(frontend_missing("discard operator"));
    }
    if requirements.pretty_directive && frontend.pretty_directives.is_empty() {
        return Err(frontend_missing("pretty directive"));
    }
    if requirements.host_capture && frontend.host_captures.is_empty() {
        return Err(frontend_missing("host capture"));
    }
    if requirements.projection && frontend.projections.is_empty() {
        return Err(frontend_missing("projection"));
    }
    if requirements.typed_projection && frontend.typed_projections.is_empty() {
        return Err(frontend_missing("typed projection"));
    }
    for (required, kind) in [
        (
            requirements.token_directive,
            grammar::RuntimeConstructKind::TokenDirective,
        ),
        (requirements.comma, grammar::RuntimeConstructKind::Comma),
    ] {
        if !required {
            continue;
        }
        if facts.count(kind) == 0 {
            return Err(CodegenError::Lowering(format!(
                "runtime request source facts missing {kind:?}"
            )));
        }
    }
    Ok(())
}

fn frontend_missing(fact: &str) -> CodegenError {
    CodegenError::Lowering(format!("runtime request frontend closure missing {fact}"))
}
