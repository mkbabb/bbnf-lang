use crate::CodegenError;
use ir::BackendIr;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct GrammarProfile {
    id: &'static str,
    generated_runtime_files: &'static [&'static str],
    mode: RuntimeGenerationMode,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum RuntimeGenerationMode {
    PassCompiled,
    FrontendFacts,
}

impl GrammarProfile {
    pub(crate) const fn new(
        id: &'static str,
        generated_runtime_files: &'static [&'static str],
        mode: RuntimeGenerationMode,
    ) -> Self {
        Self {
            id,
            generated_runtime_files,
            mode,
        }
    }

    pub(crate) fn id(&self) -> &'static str {
        self.id
    }

    pub(crate) fn generated_runtime_files(&self) -> &'static [&'static str] {
        self.generated_runtime_files
    }

    pub(crate) fn mode(&self) -> RuntimeGenerationMode {
        self.mode
    }

    fn matches_grammar_name(&self, grammar_name: &str) -> bool {
        self.id == grammar_name
    }
}

pub(crate) fn select_runtime_profile(
    backend: &BackendIr,
) -> Result<&'static GrammarProfile, CodegenError> {
    select_runtime_profile_for_name(&backend.grammar_name)
}

pub(crate) fn select_runtime_profile_for_name(
    grammar_name: &str,
) -> Result<&'static GrammarProfile, CodegenError> {
    for profile in runtime_profiles() {
        if profile.matches_grammar_name(grammar_name) {
            return Ok(profile);
        }
    }
    let supported = runtime_profiles()
        .iter()
        .map(|profile| profile.id())
        .collect::<Vec<_>>()
        .join(", ");
    Err(CodegenError::Lowering(format!(
        "runtime emission currently supports grammar profiles [{supported}], found `{grammar_name}`"
    )))
}

pub(crate) fn validate_generated_roster<'a>(
    profile: &GrammarProfile,
    actual: impl IntoIterator<Item = &'a str>,
) -> Result<(), String> {
    let actual = actual.into_iter().collect::<Vec<_>>();
    let expected = profile.generated_runtime_files();
    if actual == expected {
        Ok(())
    } else {
        Err(format!(
            "generated runtime file roster for `{}` was [{}], expected [{}]",
            profile.id(),
            actual.join(", "),
            expected.join(", ")
        ))
    }
}

fn runtime_profiles() -> [&'static GrammarProfile; 8] {
    [
        &JSON_PROFILE,
        &CSS_L4_DECLARATION_VALUES_PROFILE,
        &CSS_L4_DECLARATION_VALUES_EXTENDED_PROFILE,
        &CSS_L4_STYLESHEET_SELECTORS_PROFILE,
        &CSS_L4_VISUAL_FUNCTIONS_PROFILE,
        &CSS_L4_AT_RULES_AND_MEDIA_PROFILE,
        &CSS_L4_VENDOR_AND_CUSTOM_ATRULES_PROFILE,
        &CSS_L4_NESTED_LAYOUT_PROFILE,
    ]
}

static JSON_PROFILE: GrammarProfile = GrammarProfile::new(
    "json",
    &[
        "config.rs",
        "generated.rs",
        "host.rs",
        "mod.rs",
        "parser.rs",
        "value.rs",
        "view.rs",
        "visitor.rs",
    ],
    RuntimeGenerationMode::PassCompiled,
);

static CSS_L4_DECLARATION_VALUES_PROFILE: GrammarProfile = GrammarProfile::new(
    "css_l4_declaration_values",
    &[
        "config.rs",
        "generated.rs",
        "mod.rs",
        "parser.rs",
        "sink.rs",
    ],
    RuntimeGenerationMode::FrontendFacts,
);

static CSS_L4_DECLARATION_VALUES_EXTENDED_PROFILE: GrammarProfile = GrammarProfile::new(
    "css_l4_declaration_values_extended",
    &[
        "config.rs",
        "generated.rs",
        "mod.rs",
        "parser.rs",
        "sink.rs",
    ],
    RuntimeGenerationMode::FrontendFacts,
);

static CSS_L4_STYLESHEET_SELECTORS_PROFILE: GrammarProfile = GrammarProfile::new(
    "css_l4_stylesheet_selectors",
    &[
        "config.rs",
        "generated.rs",
        "mod.rs",
        "parser.rs",
        "sink.rs",
    ],
    RuntimeGenerationMode::FrontendFacts,
);

static CSS_L4_VISUAL_FUNCTIONS_PROFILE: GrammarProfile = GrammarProfile::new(
    "css_l4_visual_functions",
    &[
        "config.rs",
        "generated.rs",
        "mod.rs",
        "parser.rs",
        "sink.rs",
    ],
    RuntimeGenerationMode::FrontendFacts,
);

static CSS_L4_AT_RULES_AND_MEDIA_PROFILE: GrammarProfile = GrammarProfile::new(
    "css_l4_at_rules_and_media",
    &[
        "config.rs",
        "generated.rs",
        "mod.rs",
        "parser.rs",
        "sink.rs",
    ],
    RuntimeGenerationMode::FrontendFacts,
);

static CSS_L4_VENDOR_AND_CUSTOM_ATRULES_PROFILE: GrammarProfile = GrammarProfile::new(
    "css_l4_vendor_and_custom_atrules",
    &[
        "config.rs",
        "generated.rs",
        "mod.rs",
        "parser.rs",
        "sink.rs",
    ],
    RuntimeGenerationMode::FrontendFacts,
);

static CSS_L4_NESTED_LAYOUT_PROFILE: GrammarProfile = GrammarProfile::new(
    "css_l4_nested_layout",
    &[
        "config.rs",
        "generated.rs",
        "mod.rs",
        "parser.rs",
        "sink.rs",
    ],
    RuntimeGenerationMode::FrontendFacts,
);
