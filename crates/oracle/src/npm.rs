//! Pinned npm packages for the JavaScript oracles (`crates/oracle/ts/pin.json`).
//!
//! A set is materialised once into `<cache>/<set>/node_modules/<name>`: each
//! tarball is fetched over public HTTPS (no token), its sha256 is checked
//! against the pin before anything is unpacked, and a stamp records the
//! hashes, so a changed pin re-fetches. The cache is `$BBNF_ORACLE_CACHE`, or
//! `oracle-cache/` beside the corpus directory (never `/tmp`).

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context, bail, ensure};
use serde::Deserialize;
use sha2::{Digest, Sha256};

#[derive(Deserialize, Clone, Debug)]
pub struct Package {
    pub name: String,
    pub version: String,
    pub url: String,
    pub sha256: String,
    pub integrity: String,
}

#[derive(Deserialize)]
struct PinFile {
    sets: std::collections::BTreeMap<String, Vec<Package>>,
}

pub fn pin_path() -> PathBuf {
    crate::registry::repo_root().join("crates/oracle/ts/pin.json")
}

pub fn set(name: &str) -> anyhow::Result<Vec<Package>> {
    let text = fs::read_to_string(pin_path())?;
    let pins: PinFile = serde_json::from_str(&text)?;
    pins.sets
        .get(name)
        .cloned()
        .with_context(|| format!("no npm set {name} in {}", pin_path().display()))
}

pub fn cache_root() -> PathBuf {
    match std::env::var_os("BBNF_ORACLE_CACHE") {
        Some(d) if !d.is_empty() => PathBuf::from(d),
        _ => {
            let corpus = bbnf_corpus::dir();
            corpus
                .parent()
                .map(|p| p.join("oracle-cache"))
                .unwrap_or_else(|| corpus.join("oracle-cache"))
        }
    }
}

pub fn sha256_file(p: &Path) -> anyhow::Result<String> {
    let bytes = fs::read(p).with_context(|| format!("read {}", p.display()))?;
    Ok(format!("{:x}", Sha256::digest(&bytes)))
}

/// The directory whose `node_modules` holds the set, fetched and checked.
pub fn ensure(set_name: &str) -> anyhow::Result<PathBuf> {
    let pkgs = set(set_name)?;
    let stamp_want: String = pkgs
        .iter()
        .map(|p| format!("{}@{} {}\n", p.name, p.version, p.sha256))
        .collect();
    // One directory per pin set content, so a changed pin never unpacks over a stale tree.
    let key = format!("{:x}", Sha256::digest(stamp_want.as_bytes()));
    let dir = cache_root().join(set_name).join(&key[..16]);
    let stamp = dir.join("STAMP");
    if fs::read_to_string(&stamp).ok().as_deref() == Some(stamp_want.as_str()) {
        return Ok(dir);
    }
    let dl = cache_root().join("tarballs");
    fs::create_dir_all(&dl)?;
    for p in &pkgs {
        let file = dl.join(format!("{}-{}.tgz", p.name.replace('/', "__"), p.version));
        if sha256_file(&file).ok().as_deref() != Some(p.sha256.as_str()) {
            let st = Command::new("curl")
                .args(["-fsSL", "--retry", "3", "-o"])
                .arg(&file)
                .arg(&p.url)
                .status()?;
            ensure!(st.success(), "curl {} failed ({st})", p.url);
        }
        let got = sha256_file(&file)?;
        if got != p.sha256 {
            bail!(
                "{}@{}: tarball sha256 {got} != pin {}; refused, nothing unpacked",
                p.name,
                p.version,
                p.sha256
            );
        }
        let dest = dir.join("node_modules").join(&p.name);
        fs::create_dir_all(&dest)?;
        let st = Command::new("tar")
            .arg("-xzf")
            .arg(&file)
            .arg("-C")
            .arg(&dest)
            .args(["--strip-components", "1"])
            .status()?;
        ensure!(st.success(), "tar -xzf {} failed ({st})", file.display());
    }
    fs::write(
        dir.join("package.json"),
        "{\"name\":\"bbnf-oracle-arm\",\"private\":true}\n",
    )?;
    fs::write(&stamp, stamp_want)?;
    Ok(dir)
}
