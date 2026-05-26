use crate::regen::{self, RuntimeTarget};
use anyhow::{bail, Result};
use std::path::Path;

const CSS_L4_SOURCES: &[&str] = &[
    "grammar/css/l4/color.bbnf",
    "grammar/css/l4/easing.bbnf",
    "grammar/css/l4/filters.bbnf",
    "grammar/css/l4/func-body.bbnf",
    "grammar/css/l4/gradients.bbnf",
    "grammar/css/l4/keyframes.bbnf",
    "grammar/css/l4/keywords.bbnf",
    "grammar/css/l4/media.bbnf",
    "grammar/css/l4/properties.bbnf",
    "grammar/css/l4/selectors.bbnf",
    "grammar/css/l4/stylesheet.bbnf",
    "grammar/css/l4/tokens.bbnf",
    "grammar/css/l4/transforms.bbnf",
    "grammar/css/l4/value-unit.bbnf",
    "grammar/css/l4/values.bbnf",
];

const WORKSPACE_METADATA: &[&str] = &["Cargo.toml", "skinny/Cargo.toml"];
const CSS_L4_ROOTS: &[&str] = &["grammar/css/l4/stylesheet.bbnf"];

const TARGETS: &[RuntimeTarget] = &[
    RuntimeTarget {
        grammar_name: "css_l4",
        profile: "css_l4_at_rules_and_media",
        entry_rule: "stylesheet",
        source_roots: CSS_L4_ROOTS,
        output_dir: "crates/runtime/src/grammars/css_l4_at_rules_and_media",
        check_command: "check-css-l4-at-rules-and-media",
        source_inputs: CSS_L4_SOURCES,
        metadata_inputs: WORKSPACE_METADATA,
    },
    RuntimeTarget {
        grammar_name: "css_l4",
        profile: "css_l4_declaration_values",
        entry_rule: "stylesheet",
        source_roots: CSS_L4_ROOTS,
        output_dir: "crates/runtime/src/grammars/css_l4_declaration_values",
        check_command: "check-css-l4-declaration-values",
        source_inputs: CSS_L4_SOURCES,
        metadata_inputs: WORKSPACE_METADATA,
    },
    RuntimeTarget {
        grammar_name: "css_l4",
        profile: "css_l4_declaration_values_extended",
        entry_rule: "stylesheet",
        source_roots: CSS_L4_ROOTS,
        output_dir: "crates/runtime/src/grammars/css_l4_declaration_values_extended",
        check_command: "check-css-l4-declaration-values-extended",
        source_inputs: CSS_L4_SOURCES,
        metadata_inputs: WORKSPACE_METADATA,
    },
    RuntimeTarget {
        grammar_name: "css_l4",
        profile: "css_l4_nested_layout",
        entry_rule: "stylesheet",
        source_roots: CSS_L4_ROOTS,
        output_dir: "crates/runtime/src/grammars/css_l4_nested_layout",
        check_command: "check-css-l4-nested-layout",
        source_inputs: CSS_L4_SOURCES,
        metadata_inputs: WORKSPACE_METADATA,
    },
    RuntimeTarget {
        grammar_name: "css_l4",
        profile: "css_l4_stylesheet_selectors",
        entry_rule: "stylesheet",
        source_roots: CSS_L4_ROOTS,
        output_dir: "crates/runtime/src/grammars/css_l4_stylesheet_selectors",
        check_command: "check-css-l4-stylesheet-selectors",
        source_inputs: CSS_L4_SOURCES,
        metadata_inputs: WORKSPACE_METADATA,
    },
    RuntimeTarget {
        grammar_name: "css_l4",
        profile: "css_l4_vendor_and_custom_atrules",
        entry_rule: "stylesheet",
        source_roots: CSS_L4_ROOTS,
        output_dir: "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules",
        check_command: "check-css-l4-vendor-and-custom-atrules",
        source_inputs: CSS_L4_SOURCES,
        metadata_inputs: WORKSPACE_METADATA,
    },
    RuntimeTarget {
        grammar_name: "css_l4",
        profile: "css_l4_visual_functions",
        entry_rule: "stylesheet",
        source_roots: CSS_L4_ROOTS,
        output_dir: "crates/runtime/src/grammars/css_l4_visual_functions",
        check_command: "check-css-l4-visual-functions",
        source_inputs: CSS_L4_SOURCES,
        metadata_inputs: WORKSPACE_METADATA,
    },
];

pub(crate) fn regen_css(root: &Path) -> Result<()> {
    regen::write_targets(root, TARGETS)
}

pub(crate) fn check_at_rules_and_media(root: &Path) -> Result<()> {
    check(root, "check-css-l4-at-rules-and-media")
}

pub(crate) fn check_declaration_values(root: &Path) -> Result<()> {
    check(root, "check-css-l4-declaration-values")
}

pub(crate) fn check_declaration_values_extended(root: &Path) -> Result<()> {
    check(root, "check-css-l4-declaration-values-extended")
}

pub(crate) fn check_nested_layout(root: &Path) -> Result<()> {
    check(root, "check-css-l4-nested-layout")
}

pub(crate) fn check_stylesheet_selectors(root: &Path) -> Result<()> {
    check(root, "check-css-l4-stylesheet-selectors")
}

pub(crate) fn check_vendor_and_custom_atrules(root: &Path) -> Result<()> {
    check(root, "check-css-l4-vendor-and-custom-atrules")
}

pub(crate) fn check_visual_functions(root: &Path) -> Result<()> {
    check(root, "check-css-l4-visual-functions")
}

fn check(root: &Path, command: &str) -> Result<()> {
    let Some(target) = TARGETS
        .iter()
        .copied()
        .find(|target| target.check_command == command)
    else {
        bail!("unknown CSS L4 check command `{command}`");
    };
    regen::check_target(root, target)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeSet;

    #[test]
    fn css_l4_roster_has_seven_distinct_companions() {
        let commands = TARGETS
            .iter()
            .map(|target| target.check_command)
            .collect::<BTreeSet<_>>();
        assert_eq!(commands.len(), 7);
        assert!(commands.contains("check-css-l4-at-rules-and-media"));
        assert!(commands.contains("check-css-l4-declaration-values"));
        assert!(commands.contains("check-css-l4-declaration-values-extended"));
        assert!(commands.contains("check-css-l4-nested-layout"));
        assert!(commands.contains("check-css-l4-stylesheet-selectors"));
        assert!(commands.contains("check-css-l4-vendor-and-custom-atrules"));
        assert!(commands.contains("check-css-l4-visual-functions"));
    }

    #[test]
    fn css_l4_roster_names_all_fifteen_sources() {
        assert_eq!(CSS_L4_SOURCES.len(), 15);
        for target in TARGETS {
            assert_eq!(target.source_inputs, CSS_L4_SOURCES);
        }
    }
}
