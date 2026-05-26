use crate::{grammar_profile, runtime_generator, CodegenError, EmittedSource};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeGenerationRequest {
    pub grammar_name: String,
    pub profile_id: String,
    pub entry_rule: String,
    pub source_roots: Vec<String>,
    pub sources: Vec<RuntimeGrammarSource>,
    pub workspace_metadata: RuntimeWorkspaceMetadata,
    pub output_dir: String,
    pub expected_files: Vec<String>,
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
    let profile = match grammar_profile::select_runtime_profile_for_name(&request.profile_id) {
        Ok(profile) => profile,
        Err(error) => {
            if let Some(unsupported) = facts.first_unsupported() {
                return Err(CodegenError::UnsupportedRuntimeConstruct {
                    code: unsupported.code,
                    path: unsupported.path,
                    offset: unsupported.offset,
                    source_hash: unsupported.source_hash,
                });
            }
            return Err(error);
        }
    };
    let expected = profile
        .generated_runtime_files()
        .iter()
        .map(|path| (*path).to_string())
        .collect::<Vec<_>>();
    if request.expected_files != expected {
        return Err(CodegenError::Lowering(format!(
            "runtime request expected files [{}], profile `{}` requires [{}]",
            request.expected_files.join(", "),
            profile.id(),
            expected.join(", ")
        )));
    }
    if profile.mode() == grammar_profile::RuntimeGenerationMode::FrontendFacts {
        validate_non_json_frontend_materiality(&facts)?;
    }
    runtime_generator::emit_from_request(profile, &request, &facts)
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

fn validate_non_json_frontend_materiality(
    facts: &grammar::RuntimeSourceFacts,
) -> Result<(), CodegenError> {
    let frontend = &facts.frontend;
    if frontend.imports.is_empty() {
        return Err(frontend_missing("import closure"));
    }
    if frontend.layout.whitespace_directives.is_empty() {
        return Err(frontend_missing("whitespace directive"));
    }
    if frontend.layout.whitespace_modifiers.is_empty() {
        return Err(frontend_missing("whitespace modifier"));
    }
    if frontend.layout.discard_operators.is_empty() {
        return Err(frontend_missing("discard operator"));
    }
    if frontend.pretty_directives.is_empty() {
        return Err(frontend_missing("pretty directive"));
    }
    if frontend.host_captures.is_empty() {
        return Err(frontend_missing("host capture"));
    }
    if frontend.projections.is_empty() {
        return Err(frontend_missing("projection"));
    }
    if frontend.typed_projections.is_empty() {
        return Err(frontend_missing("typed projection"));
    }
    for kind in [
        grammar::RuntimeConstructKind::TokenDirective,
        grammar::RuntimeConstructKind::Comma,
    ] {
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
