//! The grammars G1 reads (the 9 of `[workspace.metadata.bbnf]` plus the
//! held-out set of ruling H-1) and where each one's corpus comes from.
//!
//! A corpus is read only through custody: the files the R-c release asset
//! carries are enumerated from `corpus/MANIFEST.sha256` and resolved through
//! `bbnf_corpus`, and in-tree inputs are read from tracked files. A grammar
//! with no corpus is listed with `inputs=0`; it is a gap, never a pass.

use std::path::{Path, PathBuf};

use crate::arm::Case;

/// The repository root (crates/oracle → ../..).
pub fn repo_root() -> PathBuf {
    let here = Path::new(env!("CARGO_MANIFEST_DIR"));
    here.parent()
        .and_then(Path::parent)
        .unwrap_or(here)
        .to_path_buf()
}

/// Where a grammar's inputs come from.
#[derive(Clone, Copy, Debug)]
pub enum Source {
    /// Custody files whose manifest path starts with `prefix` and ends with `suffix`.
    Custody {
        prefix: &'static str,
        suffix: &'static str,
    },
    /// The `.toml` files toml-test lists for TOML 1.0.0.
    TomlTest,
    /// Every grammar file `grammar/MANIFEST` lists (in-tree, hashed there).
    GrammarManifest,
    /// Tracked files under a repository directory with a suffix.
    InTree {
        dir: &'static str,
        suffix: &'static str,
    },
}

#[derive(Clone, Debug)]
pub struct Grammar {
    /// The id G1 prints (the regen ident for the 9).
    pub id: &'static str,
    /// The grammar file, repository-relative.
    pub path: &'static str,
    /// The start rule. For the 9 it is root's `REGISTRY_ENTRY_RULE` for that
    /// grammar (the rule root's `parse` starts at), so every arm reads the
    /// same language; `None` (the first rule) only for grammars root lacks.
    pub entry: Option<&'static str>,
    pub heldout: bool,
    pub sources: &'static [Source],
}

pub const NINE: [&str; 9] = [
    "bbnf",
    "json",
    "css_l4",
    "css_pretty",
    "google_sheets",
    "ebnf",
    "bnf",
    "csv",
    "math",
];
pub const HELDOUT: [&str; 3] = ["toml", "graphql", "r7rs_datum"];

pub fn grammars() -> Vec<Grammar> {
    use Source::*;
    vec![
        Grammar {
            id: "bbnf",
            path: "grammar/bbnf/bbnf.bbnf",
            entry: Some("grammar"),
            heldout: false,
            sources: &[GrammarManifest],
        },
        Grammar {
            id: "json",
            path: "grammar/json/json.bbnf",
            entry: Some("json"),
            heldout: false,
            sources: &[
                Custody {
                    prefix: "jsontestsuite/test_parsing/",
                    suffix: ".json",
                },
                Custody {
                    prefix: "json/",
                    suffix: ".json",
                },
            ],
        },
        Grammar {
            id: "css_l4",
            path: "grammar/css/l4/stylesheet.bbnf",
            entry: Some("stylesheet"),
            heldout: false,
            sources: &[Custody {
                prefix: "css/",
                suffix: ".css",
            }],
        },
        Grammar {
            id: "css_pretty",
            path: "grammar/css/pretty.bbnf",
            entry: Some("stylesheet"),
            heldout: false,
            sources: &[Custody {
                prefix: "css/",
                suffix: ".css",
            }],
        },
        Grammar {
            id: "google_sheets",
            path: "grammar/google-sheets/google-sheets.bbnf",
            entry: Some("formula"),
            heldout: false,
            sources: &[InTree {
                dir: "crates/core/tests/fixtures/sheets",
                suffix: ".txt",
            }],
        },
        Grammar {
            id: "ebnf",
            path: "grammar/ebnf/ebnf.bbnf",
            entry: Some("grammar"),
            heldout: false,
            sources: &[Custody {
                prefix: "ebnf/cases/",
                suffix: ".txt",
            }],
        },
        Grammar {
            id: "bnf",
            path: "grammar/bnf/bnf.bbnf",
            entry: Some("grammar"),
            heldout: false,
            sources: &[Custody {
                prefix: "bnf/cases/",
                suffix: ".txt",
            }],
        },
        Grammar {
            id: "csv",
            path: "grammar/misc/csv.bbnf",
            entry: Some("csv"),
            heldout: false,
            sources: &[InTree {
                dir: "skinny/profile/wave2-prototype",
                suffix: ".csv",
            }],
        },
        Grammar {
            id: "math",
            path: "grammar/misc/math.bbnf",
            entry: Some("number"),
            heldout: false,
            sources: &[Custody {
                prefix: "math/cases/",
                suffix: ".txt",
            }],
        },
        Grammar {
            id: "toml",
            path: "grammar/heldout/toml.bbnf",
            entry: Some("toml"),
            heldout: true,
            sources: &[TomlTest],
        },
        Grammar {
            id: "graphql",
            path: "grammar/heldout/graphql.bbnf",
            entry: Some("document"),
            heldout: true,
            sources: &[Custody {
                prefix: "graphql/cases/",
                suffix: ".graphql",
            }],
        },
        Grammar {
            id: "r7rs_datum",
            path: "grammar/heldout/r7rs-datum.bbnf",
            entry: Some("data"),
            heldout: true,
            sources: &[Custody {
                prefix: "r7rs/cases/",
                suffix: ".scm",
            }],
        },
    ]
}

pub fn find(id: &str) -> Option<Grammar> {
    grammars().into_iter().find(|g| g.id == id)
}

/// The custody manifest's file entries `(sha256, relative path)`.
pub fn custody_entries() -> anyhow::Result<Vec<(String, String)>> {
    let p = repo_root().join("corpus/MANIFEST.sha256");
    let text = std::fs::read_to_string(&p)?;
    Ok(text
        .lines()
        .filter(|l| !l.trim().is_empty() && !l.starts_with('#'))
        .filter_map(|l| l.split_once("  "))
        .filter(|(_, name)| name.contains('/'))
        .map(|(h, n)| (h.to_string(), n.to_string()))
        .collect())
}

/// The cases of one grammar, in a stable order.
pub fn cases(g: &Grammar) -> anyhow::Result<Vec<Case>> {
    let root = repo_root();
    let mut out = Vec::new();
    for src in g.sources {
        match *src {
            Source::Custody { prefix, suffix } => {
                for (_, rel) in custody_entries()? {
                    if rel.starts_with(prefix) && rel.ends_with(suffix) {
                        out.push(Case {
                            path: bbnf_corpus::path(&rel),
                            id: rel,
                        });
                    }
                }
            }
            Source::TomlTest => {
                let entries = custody_entries()?;
                for (_, rel) in &entries {
                    if rel.starts_with("toml-test/tests/") && rel.ends_with(".toml") {
                        out.push(Case {
                            path: bbnf_corpus::path(rel),
                            id: rel.clone(),
                        });
                    }
                }
            }
            Source::GrammarManifest => {
                let text = std::fs::read_to_string(root.join("grammar/MANIFEST"))?;
                for line in text.lines().filter(|l| !l.trim().is_empty()) {
                    if let Some((_, f)) = line.split_once(char::is_whitespace) {
                        let f = f.trim();
                        out.push(Case {
                            path: root.join(f),
                            id: format!("in-tree:{f}"),
                        });
                    }
                }
            }
            Source::InTree { dir, suffix } => {
                let mut v: Vec<PathBuf> = std::fs::read_dir(root.join(dir))?
                    .filter_map(|e| e.ok().map(|e| e.path()))
                    .filter(|p| p.to_string_lossy().ends_with(suffix))
                    .collect();
                v.sort();
                for p in v {
                    let rel = p
                        .strip_prefix(&root)
                        .unwrap_or(&p)
                        .to_string_lossy()
                        .into_owned();
                    out.push(Case {
                        path: p,
                        id: format!("in-tree:{rel}"),
                    });
                }
            }
        }
    }
    out.sort_by(|a, b| a.id.cmp(&b.id));
    out.dedup_by(|a, b| a.id == b.id);
    Ok(out)
}
