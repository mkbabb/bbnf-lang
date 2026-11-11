//! `cargo xtask corpus fetch|verify|pack` — corpus custody (sk-v27 LW2.TOPO-6, ruling R-c).
//!
//! The corpus that tests, benches and examples read is not tracked. Its
//! transport is the versioned GitHub release asset named in
//! `corpus/MANIFEST.sha256`, fetched over public HTTPS with no token; its
//! custody is that in-tree manifest. Every reader resolves the corpus through
//! `bbnf_corpus::path`, and this module writes and checks the same directory
//! (`bbnf_corpus::dir()`).
//!
//! Manifest format (`shasum -a 256` lines; `#` starts a comment):
//! - exactly one asset line, whose name has no `/`: `<sha256>  corpus-vN.tar.gz`;
//!   the release tag is the name without `.tar.gz`;
//! - one line per corpus file, relative to the corpus directory:
//!   `<sha256>  json/twitter.json`.

use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context, bail, ensure};
use sha2::{Digest, Sha256};

/// The GitHub repository whose releases carry the asset.
const REPO: &str = "mkbabb/bbnf-lang";

/// The parsed in-tree manifest.
pub struct Manifest {
    /// `(sha256, name)` of the release asset.
    pub asset: (String, String),
    /// `(sha256, path relative to the corpus directory)`, in manifest order.
    pub files: Vec<(String, String)>,
}

impl Manifest {
    pub fn path(root: &Path) -> PathBuf {
        root.join("corpus/MANIFEST.sha256")
    }

    pub fn load(root: &Path) -> anyhow::Result<Self> {
        let p = Self::path(root);
        let text = fs::read_to_string(&p).with_context(|| format!("read {}", p.display()))?;
        Self::parse(&text)
    }

    pub fn parse(text: &str) -> anyhow::Result<Self> {
        let mut asset = None;
        let mut files = Vec::new();
        for (n, line) in text.lines().enumerate() {
            let line = line.trim_end();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            let (hash, name) = line
                .split_once("  ")
                .with_context(|| format!("manifest line {}: expected `<sha256>  <path>`", n + 1))?;
            ensure!(
                hash.len() == 64 && hash.bytes().all(|b| b.is_ascii_hexdigit()),
                "manifest line {}: `{hash}` is not a sha256",
                n + 1
            );
            ensure!(
                !name.starts_with('/') && !name.split('/').any(|c| c == ".." || c.is_empty()),
                "manifest line {}: `{name}` must be a plain relative path",
                n + 1
            );
            if name.contains('/') {
                files.push((hash.to_string(), name.to_string()));
            } else {
                ensure!(asset.is_none(), "manifest line {}: a second asset line `{name}`", n + 1);
                asset = Some((hash.to_string(), name.to_string()));
            }
        }
        let asset = asset.context("manifest names no release asset")?;
        ensure!(asset.1.ends_with(".tar.gz"), "asset `{}` is not a .tar.gz", asset.1);
        ensure!(!files.is_empty(), "manifest names no corpus file");
        Ok(Self { asset, files })
    }

    pub fn tag(&self) -> &str {
        self.asset.1.trim_end_matches(".tar.gz")
    }

    pub fn url(&self) -> String {
        format!("https://github.com/{REPO}/releases/download/{}/{}", self.tag(), self.asset.1)
    }
}

pub fn sha256_file(p: &Path) -> std::io::Result<String> {
    let mut f = fs::File::open(p)?;
    let mut h = Sha256::new();
    let mut buf = vec![0u8; 1 << 20];
    loop {
        let n = f.read(&mut buf)?;
        if n == 0 {
            break;
        }
        h.update(&buf[..n]);
    }
    Ok(format!("{:x}", h.finalize()))
}

/// Checks every manifest file in `dir`. Returns the list of failures.
pub fn check(m: &Manifest, dir: &Path) -> Vec<String> {
    let mut bad = Vec::new();
    for (want, rel) in &m.files {
        let p = dir.join(rel);
        match sha256_file(&p) {
            Ok(got) if &got == want => {}
            Ok(got) => bad.push(format!("{rel}: sha256 {got} != manifest {want}")),
            Err(e) => bad.push(format!("{rel}: absent ({e})")),
        }
    }
    bad
}

/// `cargo xtask corpus verify`.
pub fn verify(root: &Path) -> anyhow::Result<()> {
    let m = Manifest::load(root)?;
    let dir = bbnf_corpus::dir();
    let bad = check(&m, &dir);
    if !bad.is_empty() {
        for b in &bad {
            eprintln!("  {b}");
        }
        bail!(
            "corpus verify: {} of {} files fail against {} in {}; run `cargo xtask corpus fetch`",
            bad.len(),
            m.files.len(),
            Manifest::path(root).display(),
            dir.display()
        );
    }
    println!("corpus verify: {}/{} files match ({})", m.files.len(), m.files.len(), m.tag());
    Ok(())
}

/// `cargo xtask corpus fetch [--asset <file>]`. With `--asset`, the asset is
/// read from a local file instead of the release URL (offline use, and the
/// tamper falsifier). The asset's sha256 is checked before anything is
/// unpacked, and every file is checked after.
pub fn fetch(root: &Path, local: Option<&Path>) -> anyhow::Result<()> {
    let m = Manifest::load(root)?;
    let dir = bbnf_corpus::dir();
    if local.is_none() && check(&m, &dir).is_empty() {
        println!("corpus fetch: {} already verified in {}", m.tag(), dir.display());
        return Ok(());
    }
    let tmp = tempfile::tempdir()?;
    let asset = match local {
        Some(p) => p.to_path_buf(),
        None => {
            let out = tmp.path().join(&m.asset.1);
            let url = m.url();
            println!("corpus fetch: {url}");
            let st = Command::new("curl")
                .args(["-fsSL", "--retry", "3", "-o"])
                .arg(&out)
                .arg(&url)
                .status()
                .context("spawn curl")?;
            ensure!(st.success(), "curl {url} failed ({st})");
            out
        }
    };
    let got = sha256_file(&asset).with_context(|| format!("read {}", asset.display()))?;
    ensure!(
        got == m.asset.0,
        "corpus fetch: asset {} sha256 {got} != manifest {}; refused, nothing unpacked",
        asset.display(),
        m.asset.0
    );
    fs::create_dir_all(&dir)?;
    let st = Command::new("tar").arg("-xzf").arg(&asset).arg("-C").arg(&dir).status().context("spawn tar")?;
    ensure!(st.success(), "tar -xzf {} failed ({st})", asset.display());
    let bad = check(&m, &dir);
    if !bad.is_empty() {
        for b in &bad {
            eprintln!("  {b}");
        }
        bail!("corpus fetch: {} files fail after unpacking {}", bad.len(), m.asset.1);
    }
    println!("corpus fetch: {} files verified in {}", m.files.len(), dir.display());
    Ok(())
}

/// `cargo xtask corpus pack --out <file>`: the maintainer's verb for cutting a
/// new asset (bsdtar, as on macOS). Every manifest file must already verify;
/// the archive holds exactly those files, in manifest order, with a neutral
/// owner. The asset's identity is the sha256 this prints, which goes on the
/// manifest's asset line.
pub fn pack(root: &Path, out: &Path) -> anyhow::Result<()> {
    let m = Manifest::load(root)?;
    let dir = bbnf_corpus::dir();
    let bad = check(&m, &dir);
    ensure!(bad.is_empty(), "corpus pack: the corpus does not verify: {bad:?}");
    let st = Command::new("tar")
        .env("COPYFILE_DISABLE", "1")
        .env("GZIP", "-n")
        .args(["--format", "ustar", "--uid", "0", "--gid", "0", "--uname", "", "--gname", ""])
        .args(["--options", "gzip:!timestamp"])
        .arg("-czf")
        .arg(out)
        .arg("-C")
        .arg(&dir)
        .args(m.files.iter().map(|(_, rel)| rel.as_str()))
        .status()
        .context("spawn tar")?;
    ensure!(st.success(), "tar -czf failed ({st})");
    println!("{}  {}", sha256_file(out)?, out.file_name().unwrap().to_string_lossy());
    Ok(())
}
