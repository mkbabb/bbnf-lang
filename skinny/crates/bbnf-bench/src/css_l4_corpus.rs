use std::path::{Path, PathBuf};

pub const CSS_L4_SK_V14_MIN_BYTES: usize = 800 * 1024;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CssL4CorpusSpec {
    pub id: &'static str,
    pub file_name: &'static str,
    pub source_url: &'static str,
    pub version_pin: &'static str,
    pub bytes: usize,
    pub sha256: &'static str,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CssL4Corpus {
    pub spec: &'static CssL4CorpusSpec,
    pub bytes: Vec<u8>,
}

pub const CSS_L4_SK_V14_CORPORA: &[CssL4CorpusSpec] = &[
    CssL4CorpusSpec {
        id: "bootstrap",
        file_name: "bootstrap-5.3.3.min.css",
        source_url: "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css",
        version_pin: "bootstrap@5.3.3",
        bytes: 232_803,
        sha256: "3c8f27e6009ccfd710a905e6dcf12d0ee3c6f2ac7da05b0572d3e0d12e736fc8",
    },
    CssL4CorpusSpec {
        id: "tailwindcss",
        file_name: "tailwindcss-0.2.0.min.css",
        source_url: "https://cdn.jsdelivr.net/npm/tailwindcss@0.2.0/dist/tailwind.min.css",
        version_pin: "tailwindcss@0.2.0",
        bytes: 179_631,
        sha256: "e463dd783548584666e5e50c47c305def32607a9a2dd64e7593908fc1839ee73",
    },
    CssL4CorpusSpec {
        id: "material-components-web",
        file_name: "material-components-web-14.0.0.min.css",
        source_url: "https://cdn.jsdelivr.net/npm/material-components-web@14.0.0/dist/material-components-web.min.css",
        version_pin: "material-components-web@14.0.0",
        bytes: 495_454,
        sha256: "60f82e183aa0e791c1f3eb5bac905b5ae885f49f9708aeec8ec71a8b014c4f12",
    },
    CssL4CorpusSpec {
        id: "animate",
        file_name: "animate-4.1.1.min.css",
        source_url: "https://cdn.jsdelivr.net/npm/animate.css@4.1.1/animate.min.css",
        version_pin: "animate.css@4.1.1",
        bytes: 71_750,
        sha256: "5fbaeb9f8e25d7e0143bae61d4b1802c16ce7390b96ceb2d498b0d96ff4c853f",
    },
];

pub fn corpus_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join("corpora/css-l4-sk-v14")
}

pub fn load_all() -> std::io::Result<Vec<CssL4Corpus>> {
    CSS_L4_SK_V14_CORPORA
        .iter()
        .map(load_one)
        .collect::<std::io::Result<Vec<_>>>()
}

pub fn total_bytes(corpora: &[CssL4Corpus]) -> usize {
    corpora.iter().map(|corpus| corpus.bytes.len()).sum()
}

fn load_one(spec: &'static CssL4CorpusSpec) -> std::io::Result<CssL4Corpus> {
    let bytes = std::fs::read(corpus_dir().join(spec.file_name))?;
    Ok(CssL4Corpus { spec, bytes })
}

#[cfg(test)]
mod tests {
    use super::*;
    use lightningcss::stylesheet::{ParserOptions, StyleSheet};
    use sha2::{Digest, Sha256};
    use std::collections::BTreeSet;

    #[test]
    fn css_l4_sk_v14_corpora_resolve_and_match_manifest() {
        let corpora = load_all().expect("load css l4 sk-v14 corpora");

        assert_eq!(corpora.len(), 4);
        assert!(total_bytes(&corpora) >= CSS_L4_SK_V14_MIN_BYTES);

        let mut ids = BTreeSet::new();
        for corpus in corpora {
            assert!(ids.insert(corpus.spec.id));
            assert_eq!(corpus.bytes.len(), corpus.spec.bytes, "{}", corpus.spec.id);
            assert_eq!(
                sha256_hex(&corpus.bytes),
                corpus.spec.sha256,
                "{}",
                corpus.spec.id
            );
            std::str::from_utf8(&corpus.bytes).expect(corpus.spec.id);
        }
    }

    #[test]
    fn css_l4_sk_v14_corpora_parse_with_lightningcss() {
        for corpus in load_all().expect("load css l4 sk-v14 corpora") {
            let source = std::str::from_utf8(&corpus.bytes).expect(corpus.spec.id);
            StyleSheet::parse(source, ParserOptions::default()).unwrap_or_else(|error| {
                panic!("{} rejected by lightningcss: {error}", corpus.spec.id)
            });
        }
    }

    fn sha256_hex(bytes: &[u8]) -> String {
        let digest = Sha256::digest(bytes);
        let mut out = String::with_capacity(digest.len() * 2);
        for byte in digest {
            out.push_str(&format!("{byte:02x}"));
        }
        out
    }
}
