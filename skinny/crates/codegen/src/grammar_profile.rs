use crate::CodegenError;

pub(crate) const COMPILED_RUNTIME_FILES: &[&str] = &[
    "config.rs",
    "generated.rs",
    "host.rs",
    "mod.rs",
    "parser.rs",
    "value.rs",
    "view.rs",
    "visitor.rs",
];

pub(crate) const REQUEST_FACTS_RUNTIME_FILES: &[&str] = &[
    "config.rs",
    "generated.rs",
    "mod.rs",
    "parser.rs",
    "sink.rs",
];

/// The roster for a grammar lowered through the GENERAL recursive-descent
/// emitter (`general_descent`): a sink-only program WITHOUT a `value` rule (the
/// shape JSON's `json_sink_direct` requires). 5 files, mirroring the
/// request-facts roster but with a `view.rs` lazy projection instead of a
/// `sink.rs`.
pub(crate) const GENERAL_DESCENT_RUNTIME_FILES: &[&str] = &[
    "config.rs",
    "generated.rs",
    "mod.rs",
    "parser.rs",
    "view.rs",
];

pub(crate) fn validate_generated_roster<'a>(
    profile_id: &str,
    expected: &[&str],
    actual: impl IntoIterator<Item = &'a str>,
) -> Result<(), String> {
    let actual = actual.into_iter().collect::<Vec<_>>();
    if actual == expected {
        Ok(())
    } else {
        Err(format!(
            "generated runtime file roster for `{}` was [{}], expected [{}]",
            profile_id,
            actual.join(", "),
            expected.join(", ")
        ))
    }
}

pub(crate) fn expected_files_for_error(profile_id: &str) -> CodegenError {
    CodegenError::Lowering(format!(
        "runtime profile `{profile_id}` requires a RuntimeGenerationRequest profile contract"
    ))
}
