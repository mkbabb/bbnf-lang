//! Shared `@pretty` hint definitions — single source of truth.
//!
//! Used by the prettify codegen (validation + application) and the LSP
//! (completions, hover, diagnostics).

/// Description of a single `@pretty` hint keyword.
pub struct HintDef {
    pub name: &'static str,
    pub description: &'static str,
}

/// All recognised `@pretty` hint keywords.
///
/// **Order matters for completion UX** — structural hints first, then
/// separator/break hints, then meta.
pub const HINT_DEFS: &[HintDef] = &[
    HintDef {
        name: "group",
        description: "Wrap in Group — break to multiple lines if too wide",
    },
    HintDef {
        name: "indent",
        description: "Indent content by one level",
    },
    HintDef {
        name: "dedent",
        description: "Dedent content by one level",
    },
    HintDef {
        name: "block",
        description: "Hardline separator between list elements",
    },
    HintDef {
        name: "blankline",
        description: "Double hardline between elements (visual separation)",
    },
    HintDef {
        name: "nobreak",
        description: "Space separator (never break)",
    },
    HintDef {
        name: "softbreak",
        description: "Softline separator (break only if needed)",
    },
    HintDef {
        name: "hardbreak",
        description: "Hardline separator between tuple elements",
    },
    HintDef {
        name: "compact",
        description: "Space separator, suppress all breaks",
    },
    HintDef {
        name: "fast",
        description: "Hardline separator, skip SmartJoin optimization",
    },
    HintDef {
        name: "off",
        description: "Disable auto-heuristics for this rule",
    },
];

/// Return the list of valid hint name strings.
pub fn valid_hint_names() -> Vec<&'static str> {
    HINT_DEFS.iter().map(|d| d.name).collect()
}

/// Check whether `name` is a recognised hint keyword.
pub fn is_valid_hint(name: &str) -> bool {
    HINT_DEFS.iter().any(|d| d.name == name)
}

/// Look up the description for a hint keyword.
pub fn hint_description(name: &str) -> Option<&'static str> {
    HINT_DEFS.iter().find(|d| d.name == name).map(|d| d.description)
}

/// Find the closest hint name to `name` (for "did you mean?" suggestions).
pub fn closest_hint(name: &str) -> Option<&'static str> {
    let name_lower = name.to_lowercase();
    HINT_DEFS
        .iter()
        .map(|d| (d.name, edit_distance(&name_lower, d.name)))
        .filter(|&(_, dist)| dist <= 3)
        .min_by_key(|&(_, dist)| dist)
        .map(|(n, _)| n)
}

/// Simple Levenshtein distance for short strings.
fn edit_distance(a: &str, b: &str) -> usize {
    let a_bytes = a.as_bytes();
    let b_bytes = b.as_bytes();
    let m = a_bytes.len();
    let n = b_bytes.len();

    let mut prev = (0..=n).collect::<Vec<_>>();
    let mut curr = vec![0; n + 1];

    for i in 1..=m {
        curr[0] = i;
        for j in 1..=n {
            let cost = if a_bytes[i - 1] == b_bytes[j - 1] { 0 } else { 1 };
            curr[j] = (prev[j] + 1)
                .min(curr[j - 1] + 1)
                .min(prev[j - 1] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    prev[n]
}
