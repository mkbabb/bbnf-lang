use bbnf::grammar::generated::css_l4::CssL4Parser;
use bbnf::runtime::css_l4::{CssColor, CssColorSpace, CssColorType, CssTypedValue};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ColorRole {
    Function(CssColorType),
    Predefined(CssColorSpace),
}

fn first_color_role(input: &str) -> ColorRole {
    let document = CssL4Parser::parse(input).expect("CSS color fixture");
    document
        .walk_values()
        .find_map(|(_, value)| match value {
            CssTypedValue::Color(CssColor::Function(function)) => {
                Some(ColorRole::Function(function.kind))
            }
            CssTypedValue::Color(CssColor::Predefined(predefined)) => {
                Some(ColorRole::Predefined(predefined.space))
            }
            _ => None,
        })
        .expect("typed color projection")
}

#[test]
fn rgb_and_color_srgb_keep_distinct_semantic_roles() {
    assert_eq!(
        first_color_role("a { color: rgb(1 2 3); }"),
        ColorRole::Function(CssColorType::Rgb),
    );
    assert_eq!(
        first_color_role("a { color: color(srgb 1 0 1); }"),
        ColorRole::Predefined(CssColorSpace::Srgb),
    );
}
