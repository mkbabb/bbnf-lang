//! Error types for import resolution.

use std::fmt;
use std::path::PathBuf;

/// Errors that can occur during import resolution.
#[derive(Debug)]
pub enum ImportError {
    /// File could not be read.
    FileNotFound {
        path: PathBuf,
        imported_from: PathBuf,
    },
    /// Circular import chain detected.
    CircularImport {
        /// The path that closes the cycle.
        path: PathBuf,
        /// The chain of paths leading to the cycle (first = entry).
        chain: Vec<PathBuf>,
    },
    /// A selective import names a rule that doesn't exist in the target file.
    MissingRule {
        rule_name: String,
        path: PathBuf,
        imported_from: PathBuf,
    },
    /// Two imports define the same rule name.
    NameConflict {
        rule_name: String,
        source_a: PathBuf,
        source_b: PathBuf,
        imported_from: PathBuf,
    },
    /// Parse error in a dependent file.
    ParseError { path: PathBuf, message: String },
}

impl fmt::Display for ImportError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ImportError::FileNotFound {
                path,
                imported_from,
            } => {
                write!(
                    f,
                    "File not found: `{}` (imported from `{}`)",
                    path.display(),
                    imported_from.display()
                )
            }
            ImportError::CircularImport { path, chain } => {
                let chain_str: Vec<String> =
                    chain.iter().map(|p| p.display().to_string()).collect();
                write!(
                    f,
                    "Circular import: `{}` (chain: {} \u{2192} {})",
                    path.display(),
                    chain_str.join(" \u{2192} "),
                    path.display()
                )
            }
            ImportError::MissingRule {
                rule_name,
                path,
                imported_from,
            } => {
                write!(
                    f,
                    "Rule `{}` not found in `{}` (imported from `{}`)",
                    rule_name,
                    path.display(),
                    imported_from.display()
                )
            }
            ImportError::NameConflict {
                rule_name,
                source_a,
                source_b,
                imported_from,
            } => {
                write!(
                    f,
                    "Name conflict: rule `{}` is imported from both `{}` and `{}` in `{}`",
                    rule_name,
                    source_a.display(),
                    source_b.display(),
                    imported_from.display()
                )
            }
            ImportError::ParseError { path, message } => {
                write!(f, "Parse error in `{}`: {}", path.display(), message)
            }
        }
    }
}

impl std::error::Error for ImportError {}
