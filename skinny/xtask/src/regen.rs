use anyhow::{bail, Context, Result};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

#[derive(Clone, Copy, Debug)]
pub(crate) struct RuntimeTarget {
    pub(crate) profile: &'static str,
    pub(crate) output_dir: &'static str,
    pub(crate) check_command: &'static str,
    pub(crate) source_inputs: &'static [&'static str],
    pub(crate) metadata_inputs: &'static [&'static str],
}

pub(crate) fn write_targets(root: &Path, targets: &[RuntimeTarget]) -> Result<()> {
    validate_unique_targets(targets)?;
    for target in targets {
        validate_inputs(root, target)?;
        let emitted = codegen::emit_runtime_profile(target.profile)?;
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
    let emitted = codegen::emit_runtime_profile(target.profile)?;
    emitted
        .check_dir(root.join(target.output_dir))
        .with_context(|| {
            format!(
                "generated runtime profile `{}` is stale; run `cargo xtask regen-css`",
                target.profile
            )
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
