//! Shared `@pretty` hint definitions — single source of truth.
//!
//! Used by the prettify codegen (validation + application) and the LSP
//! (completions, hover, diagnostics).

/// Description of a single `@pretty` hint keyword.
pub struct HintDef {
    pub name: &'static str,
    /// Short one-line description.
    pub description: &'static str,
    /// Rich markdown documentation for hover display.
    pub documentation: &'static str,
}

/// All recognised `@pretty` hint keywords.
///
/// **Order matters for completion UX** — structural hints first, then
/// separator/break hints, then meta.
pub const HINT_DEFS: &[HintDef] = &[
    HintDef {
        name: "group",
        description: "Wrap in Group — break to multiple lines if too wide",
        documentation: "\
Wraps the formatted output of this rule in a **Group**. \
When the group's content fits within `maxWidth`, it renders flat (single line). \
When it exceeds `maxWidth`, all softlines within the group break to newlines.\n\n\
Typically combined with `indent` and `sep(\"...\")` for list-like structures:\n\n\
```bbnf\n@pretty array group indent sep(\", \") ;\n```\n\n\
**Flat:** `[1, 2, 3]`  \n\
**Broken:**\n```\n[\n  1,\n  2,\n  3\n]\n```",
    },
    HintDef {
        name: "indent",
        description: "Indent content by one level",
        documentation: "\
Increases indentation by one level for the rule's body content. \
Uses the configured indent string (spaces or tab).\n\n\
Almost always paired with `group` — indentation only appears when the group breaks:\n\n\
```bbnf\n@pretty block group indent ;\n```\n\n\
Without `group`, indentation is unconditional.",
    },
    HintDef {
        name: "dedent",
        description: "Dedent content by one level",
        documentation: "\
Decreases indentation by one level. Rarely needed — used to counteract \
inherited indentation in deeply nested structures.\n\n\
```bbnf\n@pretty closingBrace dedent ;\n```",
    },
    HintDef {
        name: "block",
        description: "Hardline separator between list elements",
        documentation: "\
Joins list elements (from `*` or `+` repetitions) with **hardlines** — \
each element starts on a new line, unconditionally.\n\n\
Used for top-level statement lists, rule bodies, and declaration blocks:\n\n\
```bbnf\n@pretty statements block ;\n```\n\n\
```\nstatement1;\nstatement2;\nstatement3;\n```",
    },
    HintDef {
        name: "blankline",
        description: "Double hardline between elements (visual separation)",
        documentation: "\
Joins list elements with a **blank line** (double hardline) between them. \
Provides visual separation for top-level declarations.\n\n\
```bbnf\n@pretty rules blankline ;\n```\n\n\
```\nrule1 = ... ;\n\nrule2 = ... ;\n\nrule3 = ... ;\n```",
    },
    HintDef {
        name: "nobreak",
        description: "Space separator (never break)",
        documentation: "\
Joins elements with a single **space** that never breaks to a newline, \
regardless of line width. Use for tightly-coupled tokens that must stay \
on the same line.\n\n\
```bbnf\n@pretty typeAnnotation nobreak ;\n```\n\n\
`number` stays as `number`, never splits.",
    },
    HintDef {
        name: "softbreak",
        description: "Softline separator (break only if needed)",
        documentation: "\
Joins elements with a **softline** — renders as a space when the enclosing \
group fits on one line, or a newline when the group breaks.\n\n\
This is the default break behavior inside a `group`. Explicit `softbreak` is \
useful when you want softline joins *without* other hint defaults.\n\n\
```bbnf\n@pretty params group indent softbreak ;\n```",
    },
    HintDef {
        name: "hardbreak",
        description: "Hardline separator between tuple elements",
        documentation: "\
Joins **tuple** elements (from concatenation sequences) with hardlines. \
Unlike `block` (which targets `Vec` from repetitions), `hardbreak` \
targets fixed-arity sequences.\n\n\
```bbnf\n@pretty ifStatement hardbreak ;\n```\n\n\
```\nif (condition)\n  body\nelse\n  altBody\n```",
    },
    HintDef {
        name: "compact",
        description: "Space separator, suppress all breaks",
        documentation: "\
Joins all elements with spaces and **suppresses all line breaks**. \
The output stays on a single line regardless of width.\n\n\
```bbnf\n@pretty inlineExpr compact ;\n```\n\n\
`a + b * c` — never wraps.",
    },
    HintDef {
        name: "fast",
        description: "Hardline separator, skip SmartJoin optimization",
        documentation: "\
Like `block`, joins elements with hardlines — but skips the **SmartJoin** \
optimization pass that measures widths for text justification. \
Use for large lists where formatting speed matters more than optimal layout.\n\n\
```bbnf\n@pretty lines fast ;\n```",
    },
    HintDef {
        name: "off",
        description: "Disable auto-heuristics for this rule",
        documentation: "\
Disables the automatic formatting heuristics for this rule. The rule's \
output is emitted verbatim (concatenated with no extra whitespace or breaks) \
unless other explicit hints are also applied.\n\n\
Use when the default pretty-printer behavior adds unwanted formatting:\n\n\
```bbnf\n@pretty rawToken off ;\n```",
    },
];

/// Return the list of valid hint name strings.
pub fn valid_hint_names() -> Vec<&'static str> {
    HINT_DEFS.iter().map(|d| d.name).collect()
}

/// Check whether `name` is a recognised hint keyword, `sep("...")`, or `split("...")` hint.
pub fn is_valid_hint(name: &str) -> bool {
    is_sep_hint(name) || is_split_hint(name) || HINT_DEFS.iter().any(|d| d.name == name)
}

/// Check whether `name` is a `sep("...")` hint.
pub fn is_sep_hint(name: &str) -> bool {
    name.starts_with("sep(\"") && name.ends_with("\")")
}

/// Extract the separator string from a `sep("...")` hint.
/// Returns `None` if `name` is not a valid `sep(...)` hint.
pub fn extract_sep_string(name: &str) -> Option<&str> {
    if is_sep_hint(name) {
        // Strip `sep("` prefix and `")` suffix.
        Some(&name[5..name.len() - 2])
    } else {
        None
    }
}

/// Check whether `name` is a `split("...")` hint.
pub fn is_split_hint(name: &str) -> bool {
    name.starts_with("split(\"") && name.ends_with("\")")
}

/// Extract the delimiter string from a `split("...")` hint.
/// Returns `None` if `name` is not a valid `split(...)` hint.
pub fn extract_split_delim(name: &str) -> Option<&str> {
    if is_split_hint(name) {
        // Strip `split("` prefix and `")` suffix.
        Some(&name[7..name.len() - 2])
    } else {
        None
    }
}

/// Look up the short description for a hint keyword.
pub fn hint_description(name: &str) -> Option<&'static str> {
    HINT_DEFS.iter().find(|d| d.name == name).map(|d| d.description)
}

/// Look up the rich documentation for a hint keyword.
pub fn hint_documentation(name: &str) -> Option<&'static str> {
    HINT_DEFS.iter().find(|d| d.name == name).map(|d| d.documentation)
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
