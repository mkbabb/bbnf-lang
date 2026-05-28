use anyhow::{bail, Context, Result};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

#[derive(Clone, Copy, Debug)]
pub(crate) struct RuntimeTarget {
    pub(crate) grammar_name: &'static str,
    pub(crate) profile: &'static str,
    pub(crate) entry_rule: &'static str,
    pub(crate) source_roots: &'static [&'static str],
    pub(crate) output_dir: &'static str,
    pub(crate) check_command: &'static str,
    pub(crate) source_inputs: &'static [&'static str],
    pub(crate) metadata_inputs: &'static [&'static str],
    pub(crate) emitter: codegen::RuntimeEmitterKind,
    pub(crate) expected_files: &'static [&'static str],
    pub(crate) frontend_requirements: codegen::RuntimeFrontendRequirements,
    pub(crate) output_labels: Option<codegen::RuntimeOutputLabels>,
}

pub(crate) fn write_targets(root: &Path, targets: &[RuntimeTarget]) -> Result<()> {
    validate_unique_targets(targets)?;
    for target in targets {
        validate_inputs(root, target)?;
        let request = runtime_request(root, target)?;
        let emitted = codegen::emit_runtime_from_request(request)?;
        emitted
            .write_to_dir(root.join(target.output_dir))
            .with_context(|| format!("failed to write {}", target.output_dir))?;
        println!(
            "regen: {} -> {} ({})",
            target.profile, target.output_dir, target.check_command
        );
    }
    Ok(())
}

pub(crate) fn check_target(root: &Path, target: RuntimeTarget) -> Result<()> {
    validate_inputs(root, &target)?;
    let request = runtime_request(root, &target)?;
    let emitted = codegen::emit_runtime_from_request(request)?;
    emitted
        .check_dir(root.join(target.output_dir))
        .with_context(|| {
            format!(
                "generated runtime profile `{}` is stale; run `cargo xtask regen-css`",
                target.profile
            )
        })
}

pub(crate) fn runtime_request(
    root: &Path,
    target: &RuntimeTarget,
) -> Result<codegen::RuntimeGenerationRequest> {
    let repo_root = repo_root(root)?;
    let sources = target
        .source_inputs
        .iter()
        .map(|rel| {
            let source = std::fs::read_to_string(repo_root.join(rel))
                .with_context(|| format!("failed to read grammar source `{rel}`"))?;
            Ok(codegen::RuntimeGrammarSource {
                rel_path: (*rel).to_string(),
                source,
            })
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(codegen::RuntimeGenerationRequest {
        grammar_name: target.grammar_name.to_string(),
        profile_id: target.profile.to_string(),
        entry_rule: target.entry_rule.to_string(),
        source_roots: target
            .source_roots
            .iter()
            .map(|root| (*root).to_string())
            .collect(),
        sources,
        workspace_metadata: workspace_metadata(root, target.grammar_name)?,
        output_dir: target.output_dir.to_string(),
        profile_contract: codegen::RuntimeProfileContract {
            emitter: target.emitter,
            expected_files: target.expected_files,
            frontend_requirements: target.frontend_requirements,
            output_labels: target.output_labels,
        },
    })
}

fn validate_unique_targets(targets: &[RuntimeTarget]) -> Result<()> {
    let mut profiles = BTreeSet::new();
    let mut dirs = BTreeSet::new();
    let mut commands = BTreeSet::new();
    for target in targets {
        if !profiles.insert(target.profile) {
            bail!("duplicate runtime profile `{}`", target.profile);
        }
        if !dirs.insert(target.output_dir) {
            bail!("duplicate runtime output dir `{}`", target.output_dir);
        }
        if !commands.insert(target.check_command) {
            bail!("duplicate check command `{}`", target.check_command);
        }
    }
    Ok(())
}

fn workspace_metadata(
    root: &Path,
    grammar_name: &str,
) -> Result<codegen::RuntimeWorkspaceMetadata> {
    let repo_root = repo_root(root)?;
    let repo_manifest_path = "Cargo.toml";
    let skinny_manifest_path = "skinny/Cargo.toml";
    let repo_manifest = std::fs::read_to_string(repo_root.join(repo_manifest_path))?;
    let skinny_manifest = std::fs::read_to_string(repo_root.join(skinny_manifest_path))?;
    let repo_toml = repo_manifest.parse::<toml::Value>()?;
    let skinny_toml = skinny_manifest.parse::<toml::Value>()?;
    Ok(codegen::RuntimeWorkspaceMetadata {
        repo_manifest_path: repo_manifest_path.to_string(),
        skinny_manifest_path: skinny_manifest_path.to_string(),
        grammar_manifest_path: root_grammar_path(&repo_toml, grammar_name)
            .unwrap_or_else(|| grammar_name.to_string()),
        generated_root: skinny_metadata_string(&skinny_toml, "generated_root")?,
        host_registry: skinny_metadata_string(&skinny_toml, "host_registry")?,
        profile: skinny_metadata_string(&skinny_toml, "profile")?,
    })
}

fn skinny_metadata_string(manifest: &toml::Value, key: &str) -> Result<String> {
    manifest
        .get("workspace")
        .and_then(|workspace| workspace.get("metadata"))
        .and_then(|metadata| metadata.get("bbnf"))
        .and_then(|bbnf| bbnf.get(key))
        .and_then(toml::Value::as_str)
        .map(str::to_string)
        .with_context(|| format!("skinny manifest missing workspace.metadata.bbnf.{key}"))
}

fn root_grammar_path(manifest: &toml::Value, grammar_name: &str) -> Option<String> {
    manifest
        .get("workspace")?
        .get("metadata")?
        .get("bbnf")?
        .get("grammars")?
        .as_array()?
        .iter()
        .find(|entry| entry.get("ident").and_then(toml::Value::as_str) == Some(grammar_name))?
        .get("path")?
        .as_str()
        .map(str::to_string)
}

fn validate_inputs(root: &Path, target: &RuntimeTarget) -> Result<()> {
    let repo_root = repo_root(root)?;
    let mut hasher = blake3::Hasher::new();
    for rel in target.source_inputs {
        hash_input(&repo_root, rel, &mut hasher)
            .with_context(|| format!("{} source input `{rel}`", target.profile))?;
    }
    for rel in target.metadata_inputs {
        hash_input(&repo_root, rel, &mut hasher)
            .with_context(|| format!("{} metadata input `{rel}`", target.profile))?;
    }
    let digest = hasher.finalize();
    println!("inputs: {} {}", target.profile, digest.to_hex());
    Ok(())
}

fn repo_root(root: &Path) -> Result<PathBuf> {
    root.parent()
        .map(Path::to_path_buf)
        .context("skinny workspace root has no repository parent")
}

fn hash_input(repo_root: &Path, rel: &str, hasher: &mut blake3::Hasher) -> Result<()> {
    let path = repo_root.join(rel);
    let bytes =
        std::fs::read(&path).with_context(|| format!("failed to read input {}", path.display()))?;
    if bytes.is_empty() {
        bail!("input {} is empty", path.display());
    }
    hasher.update(rel.as_bytes());
    hasher.update(&[0]);
    hasher.update(&bytes);
    hasher.update(&[0]);
    Ok(())
}
