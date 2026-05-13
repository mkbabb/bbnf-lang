use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

pub const CANONICAL_JSON_FIXTURES: [&str; 17] = [
    "twitter",
    "citm_catalog",
    "canada",
    "apache_builds",
    "github_events",
    "update_center",
    "mesh",
    "random",
    "gsoc-2018",
    "marine_ik",
    "instruments",
    "numbers",
    "unicode_mixed",
    "unicode_escapes",
    "unicode_basic",
    "distinct_values",
    "y_string_unicode",
];

const EMBEDDED_VALID: &[(&str, &str)] = &[
    ("mini_object", r#"{"b":2,"a":[true,null,"x"]}"#),
    ("mini_numbers", r#"[0,-1,3.25,6.02e23]"#),
    ("mini_nested", r#"{"outer":{"inner":[{"ok":false}]}}"#),
    ("noncharacter_escape", r#""\uDBFF\uDFFE""#),
];

const EMBEDDED_INVALID: &[(&str, &[u8])] = &[
    ("bad_trailing_comma", br#"{"a":1,}"#),
    ("bad_unclosed_array", br#"[1,2,3"#),
    ("bad_invalid_utf8", b"[\"\xC3\x28\"]"),
    ("bad_latin1", b"[\"\xE9\"]"),
    ("bad_truncated_utf8", b"[\"\xE2\x82\"]"),
    ("bad_lone_high_surrogate", br#"["\uD800"]"#),
    ("bad_lone_low_surrogate", br#"["\uDD1E"]"#),
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FixtureKind {
    Valid,
    Invalid,
    Perf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FixtureProvenance {
    Embedded,
    CorpusFile,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JsonFixture {
    pub name: String,
    pub kind: FixtureKind,
    pub bytes: Vec<u8>,
    pub sha256: String,
    pub path: Option<PathBuf>,
    pub source_url: Option<String>,
    pub provenance: FixtureProvenance,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnavailableFixture {
    pub name: String,
    pub path: PathBuf,
    pub source_url: Option<String>,
    pub reason: UnavailableReason,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnavailableReason {
    MissingFile,
    PendingSha256,
    SizeMismatch { expected: u64, actual: u64 },
    Sha256Mismatch { expected: String, actual: String },
    ReadFailed(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FixtureStatus {
    Available(JsonFixture),
    Unavailable(UnavailableFixture),
}

#[derive(Debug, Clone)]
pub struct JsonFixtureSuite {
    pub embedded_valid: Vec<JsonFixture>,
    pub embedded_invalid: Vec<JsonFixture>,
    pub corpus: Vec<FixtureStatus>,
}

#[derive(Debug, thiserror::Error)]
pub enum FixtureError {
    #[error("failed to read fixture manifest {path}: {source}")]
    ReadManifest {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
    #[error("failed to parse fixture manifest {path}: {source}")]
    ParseManifest {
        path: PathBuf,
        #[source]
        source: toml::de::Error,
    },
}

#[derive(Debug, Deserialize)]
struct Manifest {
    fixtures: BTreeMap<String, ManifestEntry>,
}

#[derive(Debug, Deserialize)]
struct ManifestEntry {
    path: PathBuf,
    size_bytes: Option<u64>,
    sha256: String,
    source_url: Option<String>,
}

pub fn default_json_corpus_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("corpus/json")
}

pub fn default_json_manifest_path() -> PathBuf {
    default_json_corpus_dir().join("manifest.toml")
}

pub fn embedded_valid_json() -> Vec<JsonFixture> {
    EMBEDDED_VALID
        .iter()
        .map(|(name, text)| embedded_fixture(name, FixtureKind::Valid, text))
        .collect()
}

pub fn embedded_invalid_json() -> Vec<JsonFixture> {
    EMBEDDED_INVALID
        .iter()
        .map(|(name, bytes)| embedded_bytes_fixture(name, FixtureKind::Invalid, bytes))
        .collect()
}

pub fn load_json_suite() -> Result<JsonFixtureSuite, FixtureError> {
    load_json_suite_from(&default_json_manifest_path())
}

pub fn load_json_suite_from(manifest_path: &Path) -> Result<JsonFixtureSuite, FixtureError> {
    Ok(JsonFixtureSuite {
        embedded_valid: embedded_valid_json(),
        embedded_invalid: embedded_invalid_json(),
        corpus: load_manifest_statuses(manifest_path)?,
    })
}

pub fn load_available_bench_fixtures() -> Result<Vec<JsonFixture>, FixtureError> {
    let suite = load_json_suite()?;
    let mut fixtures: Vec<JsonFixture> = suite
        .corpus
        .into_iter()
        .filter_map(|status| match status {
            FixtureStatus::Available(fixture) => Some(fixture),
            FixtureStatus::Unavailable(_) => None,
        })
        .collect();

    if fixtures.is_empty() {
        fixtures = suite.embedded_valid;
    }

    Ok(fixtures)
}

pub fn load_manifest_statuses(manifest_path: &Path) -> Result<Vec<FixtureStatus>, FixtureError> {
    let text = fs::read_to_string(manifest_path).map_err(|source| FixtureError::ReadManifest {
        path: manifest_path.to_path_buf(),
        source,
    })?;
    let manifest: Manifest =
        toml::from_str(&text).map_err(|source| FixtureError::ParseManifest {
            path: manifest_path.to_path_buf(),
            source,
        })?;
    let root = manifest_path.parent().unwrap_or_else(|| Path::new("."));

    let mut entries = manifest.fixtures;
    let mut statuses = Vec::with_capacity(entries.len());

    for name in CANONICAL_JSON_FIXTURES {
        if let Some(entry) = entries.remove(name) {
            statuses.push(load_manifest_entry(root, name.to_string(), entry));
        }
    }

    statuses.extend(
        entries
            .into_iter()
            .map(|(name, entry)| load_manifest_entry(root, name, entry)),
    );

    Ok(statuses)
}

pub fn sha256_hex(bytes: &[u8]) -> String {
    let digest = Sha256::digest(bytes);
    let mut out = String::with_capacity(64);
    for byte in digest {
        use std::fmt::Write as _;
        let _ = write!(&mut out, "{byte:02x}");
    }
    out
}

fn embedded_fixture(name: &str, kind: FixtureKind, text: &str) -> JsonFixture {
    embedded_bytes_fixture(name, kind, text.as_bytes())
}

fn embedded_bytes_fixture(name: &str, kind: FixtureKind, bytes: &[u8]) -> JsonFixture {
    JsonFixture {
        name: name.to_string(),
        kind,
        bytes: bytes.to_vec(),
        sha256: sha256_hex(bytes),
        path: None,
        source_url: None,
        provenance: FixtureProvenance::Embedded,
    }
}

fn load_manifest_entry(root: &Path, name: String, entry: ManifestEntry) -> FixtureStatus {
    let path = root.join(&entry.path);
    if entry.sha256.starts_with("<sha256-pending") {
        return unavailable(
            name,
            path,
            entry.source_url,
            UnavailableReason::PendingSha256,
        );
    }

    let bytes = match fs::read(&path) {
        Ok(bytes) => bytes,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            return unavailable(name, path, entry.source_url, UnavailableReason::MissingFile);
        }
        Err(error) => {
            return unavailable(
                name,
                path,
                entry.source_url,
                UnavailableReason::ReadFailed(error.to_string()),
            );
        }
    };

    if let Some(expected) = entry.size_bytes {
        let actual = bytes.len() as u64;
        if actual != expected {
            return unavailable(
                name,
                path,
                entry.source_url,
                UnavailableReason::SizeMismatch { expected, actual },
            );
        }
    }

    let actual = sha256_hex(&bytes);
    if actual != entry.sha256 {
        return unavailable(
            name,
            path,
            entry.source_url,
            UnavailableReason::Sha256Mismatch {
                expected: entry.sha256,
                actual,
            },
        );
    }

    FixtureStatus::Available(JsonFixture {
        name,
        kind: FixtureKind::Perf,
        bytes,
        sha256: actual,
        path: Some(path),
        source_url: entry.source_url,
        provenance: FixtureProvenance::CorpusFile,
    })
}

fn unavailable(
    name: String,
    path: PathBuf,
    source_url: Option<String>,
    reason: UnavailableReason,
) -> FixtureStatus {
    FixtureStatus::Unavailable(UnavailableFixture {
        name,
        path,
        source_url,
        reason,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn embedded_fixtures_have_hashes_and_parse_expectations() {
        for fixture in embedded_valid_json() {
            assert_eq!(fixture.kind, FixtureKind::Valid);
            assert_eq!(fixture.sha256, sha256_hex(&fixture.bytes));
            serde_json::from_slice::<serde_json::Value>(&fixture.bytes).unwrap();
        }

        for fixture in embedded_invalid_json() {
            assert_eq!(fixture.kind, FixtureKind::Invalid);
            assert!(serde_json::from_slice::<serde_json::Value>(&fixture.bytes).is_err());
        }
    }

    #[test]
    fn manifest_loads_canonical_corpora() {
        let statuses = load_manifest_statuses(&default_json_manifest_path()).unwrap();
        assert_eq!(statuses.len(), CANONICAL_JSON_FIXTURES.len());

        for (status, expected_name) in statuses.iter().zip(CANONICAL_JSON_FIXTURES) {
            match status {
                FixtureStatus::Available(fixture) => {
                    assert_eq!(fixture.name, expected_name);
                    assert_eq!(fixture.kind, FixtureKind::Perf);
                    assert_eq!(fixture.sha256, sha256_hex(&fixture.bytes));
                    assert_eq!(fixture.provenance, FixtureProvenance::CorpusFile);
                    serde_json::from_slice::<serde_json::Value>(&fixture.bytes).unwrap();
                }
                FixtureStatus::Unavailable(unavailable) => {
                    panic!("canonical fixture unexpectedly unavailable: {unavailable:?}");
                }
            }
        }
    }

    #[test]
    fn manifest_validates_sha256_and_size() {
        let temp = TempDir::new("bbnf-fixture-test");
        let fixture_bytes = br#"{"ok":true}"#;
        fs::write(temp.path.join("sample.json"), fixture_bytes).unwrap();
        let manifest = format!(
            r#"
                [fixtures.sample]
                path = "sample.json"
                size_bytes = {}
                sha256 = "{}"
                source_url = "file://sample"
            "#,
            fixture_bytes.len(),
            sha256_hex(fixture_bytes)
        );
        fs::write(temp.path.join("manifest.toml"), manifest).unwrap();

        let statuses = load_manifest_statuses(&temp.path.join("manifest.toml")).unwrap();
        assert_eq!(statuses.len(), 1);
        match &statuses[0] {
            FixtureStatus::Available(fixture) => {
                assert_eq!(fixture.name, "sample");
                assert_eq!(fixture.bytes, fixture_bytes);
                assert_eq!(fixture.provenance, FixtureProvenance::CorpusFile);
            }
            FixtureStatus::Unavailable(unavailable) => {
                panic!("fixture unexpectedly unavailable: {unavailable:?}");
            }
        }
    }

    struct TempDir {
        path: PathBuf,
    }

    impl TempDir {
        fn new(prefix: &str) -> Self {
            let nonce = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos();
            let path = std::env::temp_dir().join(format!("{prefix}-{nonce}"));
            fs::create_dir_all(&path).unwrap();
            Self { path }
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }
}
