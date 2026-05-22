use crate::{
    css_l4_at_rules_and_media_provider, css_l4_declaration_values_extended_provider,
    css_l4_declaration_values_provider, css_l4_nested_layout_provider,
    css_l4_stylesheet_selectors_provider, css_l4_vendor_and_custom_atrules_provider,
    css_l4_visual_functions_provider, json_provider, CodegenError,
};
use ir::BackendIr;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct GrammarProfile {
    id: &'static str,
    generated_runtime_files: &'static [&'static str],
    provider: RuntimeProvider,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum RuntimeProvider {
    Json,
    CssL4DeclarationValues,
    CssL4DeclarationValuesExtended,
    CssL4StylesheetSelectors,
    CssL4VisualFunctions,
    CssL4AtRulesAndMedia,
    CssL4VendorAndCustomAtRules,
    CssL4NestedLayout,
}

impl GrammarProfile {
    pub(crate) const fn new(
        id: &'static str,
        generated_runtime_files: &'static [&'static str],
        provider: RuntimeProvider,
    ) -> Self {
        Self {
            id,
            generated_runtime_files,
            provider,
        }
    }

    pub(crate) fn id(&self) -> &'static str {
        self.id
    }

    pub(crate) fn generated_runtime_files(&self) -> &'static [&'static str] {
        self.generated_runtime_files
    }

    pub(crate) fn provider(&self) -> RuntimeProvider {
        self.provider
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
        json_provider::runtime_profile(),
        css_l4_declaration_values_provider::runtime_profile(),
        css_l4_declaration_values_extended_provider::runtime_profile(),
        css_l4_stylesheet_selectors_provider::runtime_profile(),
        css_l4_visual_functions_provider::runtime_profile(),
        css_l4_at_rules_and_media_provider::runtime_profile(),
        css_l4_vendor_and_custom_atrules_provider::runtime_profile(),
        css_l4_nested_layout_provider::runtime_profile(),
    ]
}
