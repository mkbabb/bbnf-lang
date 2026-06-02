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
const REQUEST_FACTS_RUNTIME_FILES: &[&str] = &[
    "config.rs",
    "generated.rs",
    "mod.rs",
    "parser.rs",
    "sink.rs",
];
const REQUEST_FACTS_REQUIREMENTS: codegen::RuntimeFrontendRequirements =
    codegen::RuntimeFrontendRequirements::full_request_facts();

// P3 collapse (R-A0-2 / R16): the 7 byte-identical css_l4 replicas (all
// `generated.rs` md5 `b654562c…`) were ONE grammar (`css_l4`) over ONE root
// (`stylesheet.bbnf`) wearing 7 fabricated profile labels. They collapse to the
// SINGLE canonical config — `css_l4_declaration_values`, the bench-consumed
// `track1_rich` path (`css_canon_bench.rs`, `nonjson_css_l4.rs`). Minting fake
// `.bbnf` roots to fake distinctness is the EXACT addendum-2 overfit; the disk
// evidence is collapse-to-one.
const TARGETS: &[RuntimeTarget] = &[RuntimeTarget {
    grammar_name: "css_l4",
    profile: "css_l4_declaration_values",
    entry_rule: "stylesheet",
    source_roots: CSS_L4_ROOTS,
    output_dir: "crates/runtime/src/grammars/css_l4_declaration_values",
    check_command: "check-css-l4-declaration-values",
    source_inputs: CSS_L4_SOURCES,
    metadata_inputs: WORKSPACE_METADATA,
    emitter: codegen::RuntimeEmitterKind::RequestFacts,
    expected_files: REQUEST_FACTS_RUNTIME_FILES,
    frontend_requirements: REQUEST_FACTS_REQUIREMENTS,
    output_labels: Some(codegen::RuntimeOutputLabels {
        fact_schema: "css-l4-declaration-value-facts-v1",
        row_id: "css_l4/declaration_values/direct_to_struct/main",
        output_plane: "css_l4_declaration_value_fact_stream",
    }),
}];

pub(crate) fn regen_css(root: &Path) -> Result<()> {
    regen::write_targets(root, TARGETS)
}

pub(crate) fn check_declaration_values(root: &Path) -> Result<()> {
    check(root, "check-css-l4-declaration-values")
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

    // P3 collapse: the 7 byte-identical css_l4 replicas reduced to ONE config.
    #[test]
    fn css_l4_roster_is_one_collapsed_config() {
        assert_eq!(TARGETS.len(), 1);
        assert_eq!(TARGETS[0].grammar_name, "css_l4");
        assert_eq!(TARGETS[0].check_command, "check-css-l4-declaration-values");
    }

    #[test]
    fn css_l4_roster_names_all_fifteen_sources() {
        assert_eq!(CSS_L4_SOURCES.len(), 15);
        for target in TARGETS {
            assert_eq!(target.source_inputs, CSS_L4_SOURCES);
        }
    }
}
