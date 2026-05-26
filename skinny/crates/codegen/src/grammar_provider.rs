use crate::{grammar_profile, render_runtime_profile, CodegenError, EmittedSource};

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
    if profile.provider() == grammar_profile::RuntimeProvider::Json {
        let Some(source) = request.sources.first() else {
            return Err(CodegenError::Lowering(
                "JSON runtime request requires a source".to_string(),
            ));
        };
        return crate::emit_from_source(&request.grammar_name, &source.source);
    }
    validate_non_json_materiality(&facts)?;
    render_runtime_profile(profile, None)
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

fn validate_non_json_materiality(facts: &grammar::RuntimeSourceFacts) -> Result<(), CodegenError> {
    for kind in [
        grammar::RuntimeConstructKind::Import,
        grammar::RuntimeConstructKind::TokenDirective,
        grammar::RuntimeConstructKind::WhitespaceDirective,
        grammar::RuntimeConstructKind::PrettyDirective,
        grammar::RuntimeConstructKind::Comma,
        grammar::RuntimeConstructKind::WhitespaceModifier,
        grammar::RuntimeConstructKind::ShiftRight,
        grammar::RuntimeConstructKind::ShiftLeft,
        grammar::RuntimeConstructKind::HostCapture,
    ] {
        if facts.count(kind) == 0 {
            return Err(CodegenError::Lowering(format!(
                "runtime request source facts missing {kind:?}"
            )));
        }
    }
    if facts.projection_count() == 0 {
        return Err(CodegenError::Lowering(
            "runtime request source facts missing projection metadata".to_string(),
        ));
    }
    Ok(())
}
