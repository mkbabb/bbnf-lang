//! Grammar-agnostic key-dispatch detection for Alt patterns.
//!
//! Detects Alt nodes where branches start with literal keys followed by a
//! common separator (e.g., `"color" ":" value | "display" ":" value`).
//! Instead of checkpoint/restore per branch, the driver can scan the key
//! token once and dispatch on the consumed bytes.

use bbnf_ir::{AltBranch, GrammarIR, IrNode};

use parse_that::regex::classify::{classify_regex, RegexClass};

use crate::backend::unescape_literal;

// ─── Public Types ──────────────────────────────────────────────────────────

/// Key class for key-dispatch optimization.
#[derive(Debug, Clone)]
pub enum KeyClass {
    /// Identifiers: `[a-zA-Z_][\w-]*`
    Identifier,
    /// Quoted strings: `"..."` or `'...'`
    QuotedString { quote_char: u8 },
}

/// Configuration for key-dispatch alternation.
#[derive(Debug, Clone)]
pub struct KeyDispatchConfig {
    pub key_class: KeyClass,
    pub separator: Option<String>,
    /// Regex ID for the key scanner, registered by the driver.
    pub key_scanner_regex_id: Option<usize>,
}

/// The regex pattern string for a given key class.
pub fn key_class_regex_pattern(class: &KeyClass) -> &'static str {
    match class {
        KeyClass::Identifier => r"[a-zA-Z_][\w-]*",
        KeyClass::QuotedString { quote_char } => match quote_char {
            b'\'' => r"'[^']*'",
            _ => r#""[^"]*""#,
        },
    }
}

/// Result of key dispatch detection for a single branch.
#[derive(Debug, Clone)]
pub struct DetectedBranch {
    pub key_literals: Vec<String>,
    pub branch_idx: usize,
}

// ─── Public Entry Point ────────────────────────────────────────────────────

/// Try to detect key-dispatch pattern in an alternation.
///
/// Returns `(config, detected_branches, fallback_branch_idx)`.
pub fn try_detect(
    branches: &[AltBranch],
    ir: &GrammarIR,
) -> Option<(KeyDispatchConfig, Vec<DetectedBranch>, Option<usize>)> {
    if branches.len() < 3 {
        return None;
    }

    // Check if last branch is a regex fallback.
    let fallback_idx = if is_leading_regex(&branches[branches.len() - 1].node, ir) {
        Some(branches.len() - 1)
    } else {
        None
    };

    // Extract leading literals from all non-fallback branches.
    let mut all_literals: Vec<(usize, Vec<String>)> = Vec::new();
    for (i, branch) in branches.iter().enumerate() {
        if Some(i) == fallback_idx {
            continue;
        }
        let lits = extract_leading_literals(&branch.node, ir)?;
        if lits.is_empty() {
            return None;
        }
        all_literals.push((i, lits));
    }

    if all_literals.len() < 2 {
        return None;
    }

    // Detect common separator.
    let separator = detect_separator(&all_literals, branches, ir);

    // Classify key type from fallback regex (if present).
    let key_class = if let Some(fb_idx) = fallback_idx {
        classify_fallback_key(&branches[fb_idx].node, ir)?
    } else {
        // Default to Identifier if no fallback regex.
        KeyClass::Identifier
    };

    // Validate all keys against key class.
    for (_, lits) in &all_literals {
        for lit in lits {
            let key = if let Some(ref sep) = separator {
                lit.strip_suffix(sep.as_str()).unwrap_or(lit)
            } else {
                lit
            };
            if !validate_key_for_class(key, &key_class) {
                return None;
            }
        }
    }

    let detected: Vec<DetectedBranch> = all_literals
        .into_iter()
        .map(|(idx, lits)| {
            let keys = lits
                .into_iter()
                .map(|lit| {
                    if let Some(ref sep) = separator {
                        lit.strip_suffix(sep.as_str())
                            .unwrap_or(&lit)
                            .to_string()
                    } else {
                        lit
                    }
                })
                .collect();
            DetectedBranch {
                key_literals: keys,
                branch_idx: idx,
            }
        })
        .collect();

    Some((
        KeyDispatchConfig {
            key_class,
            separator,
            key_scanner_regex_id: None, // Set by driver after detection.
        },
        detected,
        fallback_idx,
    ))
}

// ─── Detection Helpers ─────────────────────────────────────────────────────

/// Extract leading literal(s) from a branch node.
fn extract_leading_literals(node: &IrNode, ir: &GrammarIR) -> Option<Vec<String>> {
    match node {
        IrNode::Literal(sid) => Some(vec![ir.get_string(*sid).to_string()]),
        IrNode::Seq(children) if !children.is_empty() => {
            extract_leading_literals(&children[0], ir)
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            extract_leading_literals(inner, ir)
        }
        IrNode::Ref(rule_id) => {
            let rule = &ir.rules[*rule_id as usize];
            extract_leading_literals(&rule.body, ir)
        }
        IrNode::Alt(branches, _) => {
            // Inner Alt: collect literals from all branches.
            let mut all = Vec::new();
            for branch in branches {
                let lits = extract_leading_literals(&branch.node, ir)?;
                all.extend(lits);
            }
            Some(all)
        }
        _ => None,
    }
}

/// Check if a node has a regex in leading position.
fn is_leading_regex(node: &IrNode, ir: &GrammarIR) -> bool {
    match node {
        IrNode::Regex(_) => true,
        IrNode::Seq(children) if !children.is_empty() => is_leading_regex(&children[0], ir),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            is_leading_regex(inner, ir)
        }
        IrNode::Ref(rule_id) => {
            let rule = &ir.rules[*rule_id as usize];
            is_leading_regex(&rule.body, ir)
        }
        _ => false,
    }
}

/// Extract the leading regex pattern string from a node.
fn extract_leading_regex_pattern<'a>(node: &'a IrNode, ir: &'a GrammarIR) -> Option<&'a str> {
    match node {
        IrNode::Regex(sid) => Some(ir.get_string(*sid)),
        IrNode::Seq(children) if !children.is_empty() => {
            extract_leading_regex_pattern(&children[0], ir)
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            extract_leading_regex_pattern(inner, ir)
        }
        IrNode::Ref(rule_id) => {
            let rule = &ir.rules[*rule_id as usize];
            extract_leading_regex_pattern(&rule.body, ir)
        }
        _ => None,
    }
}

/// Classify the fallback regex to determine key class.
fn classify_fallback_key(fallback: &IrNode, ir: &GrammarIR) -> Option<KeyClass> {
    let pattern = extract_leading_regex_pattern(fallback, ir)?;
    match classify_regex(pattern) {
        RegexClass::Identifier | RegexClass::CssIdent => Some(KeyClass::Identifier),
        RegexClass::QuotedString {
            quote_char,
            allows_escapes: _,
        } => Some(KeyClass::QuotedString { quote_char }),
        RegexClass::CssQuotedString => Some(KeyClass::QuotedString { quote_char: b'"' }),
        _ => None,
    }
}

/// Validate a key string against the key class.
fn validate_key_for_class(key: &str, class: &KeyClass) -> bool {
    match class {
        KeyClass::Identifier => {
            let bytes = key.as_bytes();
            !bytes.is_empty()
                && (bytes[0].is_ascii_alphabetic() || bytes[0] == b'_' || bytes[0] == b'-')
        }
        KeyClass::QuotedString { .. } => !key.is_empty(),
    }
}

/// Detect a common separator across all literal-led branches.
fn detect_separator(
    all_literals: &[(usize, Vec<String>)],
    branches: &[AltBranch],
    ir: &GrammarIR,
) -> Option<String> {
    // Strategy 1: Fused suffix — all literals share a trailing non-alphanumeric byte.
    let first_lits = &all_literals[0].1;
    if let Some(first_lit) = first_lits.first() {
        if let Some(&last_byte) = first_lit.as_bytes().last() {
            if !last_byte.is_ascii_alphanumeric() && last_byte != b'_' && last_byte != b'-' {
                let suffix = String::from_utf8(vec![last_byte]).ok()?;
                let all_have_suffix = all_literals.iter().all(|(_, lits)| {
                    lits.iter()
                        .all(|l| l.as_bytes().last().copied() == Some(last_byte))
                });
                if all_have_suffix {
                    return Some(suffix);
                }
            }
        }
    }

    // Strategy 2: Shared 2nd Seq child literal across all branches.
    let sep = extract_seq_separator(branches, all_literals, ir);
    sep
}

/// Extract separator from 2nd Seq child if all branches share it.
fn extract_seq_separator(
    branches: &[AltBranch],
    all_literals: &[(usize, Vec<String>)],
    ir: &GrammarIR,
) -> Option<String> {
    let mut common_sep: Option<String> = None;
    for (idx, _) in all_literals {
        let branch = &branches[*idx];
        if let IrNode::Seq(children) = &branch.node {
            if children.len() >= 2 {
                if let IrNode::Literal(sid) = &children[1] {
                    let sep = ir.get_string(*sid).to_string();
                    if let Some(ref cs) = common_sep {
                        if *cs != sep {
                            return None;
                        }
                    } else {
                        common_sep = Some(sep);
                    }
                    continue;
                }
            }
        }
        return None;
    }
    common_sep
}
