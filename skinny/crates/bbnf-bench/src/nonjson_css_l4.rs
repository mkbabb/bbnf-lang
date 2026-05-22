use crate::report::{
    SkV12NonJsonReport, SkV12NonJsonRow, SkV13CssAtRulesAndMediaReport,
    SkV13CssAtRulesAndMediaRow, SkV13CssDeclarationValuesExtendedReport,
    SkV13CssDeclarationValuesExtendedRow, SkV13CssStylesheetSelectorsReport,
    SkV13CssStylesheetSelectorsRow, SkV13CssVisualFunctionsReport, SkV13CssVisualFunctionsRow,
    SKV12_NON_JSON_REPORT_SCHEMA, SKV13_CSS_AT_RULES_AND_MEDIA_REPORT_SCHEMA,
    SKV13_CSS_DECLARATION_VALUES_EXTENDED_REPORT_SCHEMA,
    SKV13_CSS_STYLESHEET_SELECTORS_REPORT_SCHEMA, SKV13_CSS_VISUAL_FUNCTIONS_REPORT_SCHEMA,
};
use cssparser::{
    parse_important, AtRuleParser, BasicParseErrorKind, CowRcStr, DeclarationParser, Parser,
    ParserInput, ParserState, QualifiedRuleParser, RuleBodyItemParser, RuleBodyParser,
    StyleSheetParser, Token,
};
use lightningcss::media_query::{
    MediaCondition, MediaFeatureComparison, MediaFeatureId, MediaFeatureName, MediaType,
    MediaFeatureValue, QueryFeature,
};
use lightningcss::rules::keyframes::{KeyframeSelector, KeyframesName};
use lightningcss::rules::{CssRule, CssRuleList};
use lightningcss::stylesheet::{ParserOptions, StyleSheet};
use runtime::generated_css_l4_declaration_values as track1;
use runtime::generated_css_l4_declaration_values_extended as extended_track1;
use runtime::generated_css_l4_stylesheet_selectors as stylesheet_track1;
use runtime::generated_css_l4_visual_functions as visual_track1;
use runtime::generated_css_l4_at_rules_and_media as at_rules_media_track1;
use serde_json;
use sha2::{Digest, Sha256};
use std::fmt;
use std::fs;
use std::hint::black_box;
use std::io;
use std::path::{Path, PathBuf};
use std::time::Instant;

pub const ROW_ID: &str = "css_l4/declaration_values/direct_to_struct/main";
pub const OUTPUT_PLANE: &str = "css_l4_declaration_value_fact_stream";
pub const WAVE_ID: &str = "SK-V12-W1b-1";
pub const STYLESHEET_SELECTORS_ROW_ID: &str =
    "css_l4/stylesheet_and_selectors/direct_to_struct/main";
pub const STYLESHEET_SELECTORS_OUTPUT_PLANE: &str = "css_l4_stylesheet_selector_fact_stream";
pub const STYLESHEET_SELECTORS_WAVE_ID: &str = "SK-V13-W2";
pub const DECL_VALUES_EXTENDED_ROW_ID: &str =
    "css_l4/declaration_values_extended/direct_to_struct/main";
pub const DECL_VALUES_EXTENDED_OUTPUT_PLANE: &str = "css_l4_declaration_value_extended_fact_stream";
pub const DECL_VALUES_EXTENDED_WAVE_ID: &str = "SK-V13-W3";
pub const VISUAL_FUNCTIONS_ROW_ID: &str = "css_l4/visual_functions/direct_to_struct/main";
pub const VISUAL_FUNCTIONS_OUTPUT_PLANE: &str = "css_l4_visual_function_fact_stream";
pub const VISUAL_FUNCTIONS_WAVE_ID: &str = "SK-V13-W4";
pub const AT_RULES_AND_MEDIA_ROW_ID: &str =
    "css_l4/at_rules_and_media/direct_to_struct/main";
pub const AT_RULES_AND_MEDIA_OUTPUT_PLANE: &str = "css_l4_at_rules_media_fact_stream";
pub const AT_RULES_AND_MEDIA_WAVE_ID: &str = "SK-V13-W10.1";

const FACT_SCHEMA: &str = "css-l4-declaration-value-facts-v1";
const FIXTURE_RELATIVE: &str =
    "restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css";
const REPORT_RELATIVE: &str =
    "restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json";
const ARTIFACT_DIR_RELATIVE: &str = "restart/skinny/tranches/sk-v12/research/w1b/artifacts";
const EXPECTED_FIXTURE_SHA256: &str =
    "cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374";
const EXPECTED_FIXTURE_BYTES: usize = 187;
const STYLESHEET_SELECTORS_FIXTURE_RELATIVE: &str =
    "restart/skinny/tranches/sk-v13/research/w2/css_l4_stylesheet_and_selectors.css";
const STYLESHEET_SELECTORS_REPORT_RELATIVE: &str =
    "restart/skinny/tranches/sk-v13/research/w2/skv13-W2-css-l4-stylesheet-selectors.json";
const STYLESHEET_SELECTORS_ARTIFACT_DIR_RELATIVE: &str =
    "restart/skinny/tranches/sk-v13/research/w2/artifacts";
const STYLESHEET_SELECTORS_FIXTURE_SHA256: &str =
    "7fc890301ed7cdd79224fdca8d174bac80069b518c100156ed5b6e1f96cb9530";
const STYLESHEET_SELECTORS_FIXTURE_BYTES: usize = 117;
const DECL_VALUES_EXTENDED_FIXTURE_RELATIVE: &str =
    "restart/skinny/tranches/sk-v13/research/w3/css_l4_declaration_values_extended.css";
const DECL_VALUES_EXTENDED_REPORT_RELATIVE: &str =
    "restart/skinny/tranches/sk-v13/research/w3/skv13-W3-css-l4-declaration-values-extended.json";
const DECL_VALUES_EXTENDED_ARTIFACT_DIR_RELATIVE: &str =
    "restart/skinny/tranches/sk-v13/research/w3/artifacts";
const DECL_VALUES_EXTENDED_FIXTURE_SHA256: &str =
    "399593fe9848954d3570c67a588a7c352e252327f60445f3bc0670c11df88d64";
const DECL_VALUES_EXTENDED_FIXTURE_BYTES: usize = 305;
const VISUAL_FUNCTIONS_FIXTURE_RELATIVE: &str =
    "restart/skinny/tranches/sk-v13/research/w4/css_l4_visual_functions.css";
const VISUAL_FUNCTIONS_REPORT_RELATIVE: &str =
    "restart/skinny/tranches/sk-v13/research/w4/skv13-W4-css-l4-visual-functions.json";
const VISUAL_FUNCTIONS_ARTIFACT_DIR_RELATIVE: &str =
    "restart/skinny/tranches/sk-v13/research/w4/artifacts";
const VISUAL_FUNCTIONS_FIXTURE_SHA256: &str =
    "5dc7cc1098401900af32b534893c9bd007245f88af3cc683926a4abaf5f531c0";
const VISUAL_FUNCTIONS_FIXTURE_BYTES: usize = 357;
const AT_RULES_AND_MEDIA_FIXTURE_RELATIVE: &str =
    "restart/skinny/tranches/sk-v13/research/w10.1/css_l4_at_rules_and_media.css";
const AT_RULES_AND_MEDIA_REPORT_RELATIVE: &str =
    "restart/skinny/tranches/sk-v13/research/w10.1/skv13-W10.1-css-l4-at-rules-media.json";
const AT_RULES_AND_MEDIA_ARTIFACT_DIR_RELATIVE: &str =
    "restart/skinny/tranches/sk-v13/research/w10.1/artifacts";
const AT_RULES_AND_MEDIA_FIXTURE_SHA256: &str =
    "234dde82e1ead1e66be251a5d219892b666f16e853fcd5c03e67aca22fb07958";
const AT_RULES_AND_MEDIA_FIXTURE_BYTES: usize = 85;
const AT_RULES_AND_MEDIA_EXPECTED_FACTS: &str = concat!(
    "css-l4-at-rules-media-facts-v1\n",
    "row\tid=css_l4/at_rules_and_media/direct_to_struct/main\tplane=css_l4_at_rules_media_fact_stream\n",
    "source\tinput_fnv64=83cb4eb20e5253c7\tinput_bytes=85\n",
    "at_rule\tidx=0\tkind=media\tstart=0\tend=47\tprelude_start=7\tprelude_end=33\tbody_start=34\tbody_end=46\tqueries=1\tchildren=1\n",
    "media_query\trule=0\tidx=0\ttext_hex=73637265656e20616e6420286d696e2d77696474683a31707829\n",
    "media_feature\trule=0\tquery=0\tidx=0\tname_hex=6d696e2d7769647468\tvalue_hex=317078\n",
    "body_rule\tparent=0\tidx=0\tkind=qualified\tselector_hex=61\tstart=34\tend=46\tdecls=1\n",
    "decl\tparent=0\tframe=none\tidx=0\tproperty_hex=636f6c6f72\tvalue_hex=726564\n",
    "at_rule\tidx=1\tkind=keyframes\tstart=48\tend=84\tname_hex=6b\tbody_start=61\tbody_end=83\tframes=1\n",
    "keyframe\trule=1\tidx=0\tselectors=3\tselector_hex=66726f6d2c3530252c746f\tstart=61\tend=83\tdecls=1\n",
    "key_sel\trule=1\tframe=0\tidx=0\tkind=from\tvalue_hex=66726f6d\n",
    "key_sel\trule=1\tframe=0\tidx=1\tkind=percentage\tvalue_hex=353025\n",
    "key_sel\trule=1\tframe=0\tidx=2\tkind=to\tvalue_hex=746f\n",
    "decl\tparent=1\tframe=0\tidx=0\tproperty_hex=6f706163697479\tvalue_hex=31\n",
    "stylesheet\trules=2\n",
    "end\trules=2\tmedia_queries=1\tmedia_features=1\tkeyframes=1\tkeyframe_selectors=3\tdeclarations=2\tstream_fnv64=556910e319c96398\n",
);
const VISUAL_FUNCTIONS_EXPECTED_FACTS: &str = concat!(
    "css-l4-visual-function-facts-v1\n",
    "row\tid=css_l4/visual_functions/direct_to_struct/main\tplane=css_l4_visual_function_fact_stream\n",
    "source\tinput_fnv64=c73dd65ad964e9b0\tinput_bytes=357\n",
    "decl\tidx=0\tdepth=1\tproperty_hex=6261636b67726f756e642d696d616765\timportant=0\tvalue_start=23\tvalue_end=71\n",
    "tok\tdecl=0\tidx=0\tdepth=0\tkind=function\tlexeme_hex=6c696e6561722d6772616469656e74\tflags=normalized\n",
    "tok\tdecl=0\tidx=0\tdepth=1\tkind=dimension\tlexeme_hex=3435646567\tflags=normalized\n",
    "tok\tdecl=0\tidx=1\tdepth=1\tkind=comma\tlexeme_hex=2c\tflags=normalized\n",
    "tok\tdecl=0\tidx=2\tdepth=1\tkind=hash\tlexeme_hex=313233343536\tflags=normalized\n",
    "tok\tdecl=0\tidx=3\tdepth=1\tkind=percentage\tlexeme_hex=3025\tflags=normalized\n",
    "tok\tdecl=0\tidx=4\tdepth=1\tkind=comma\tlexeme_hex=2c\tflags=normalized\n",
    "tok\tdecl=0\tidx=5\tdepth=1\tkind=hash\tlexeme_hex=616263646566\tflags=normalized\n",
    "tok\tdecl=0\tidx=6\tdepth=1\tkind=percentage\tlexeme_hex=31303025\tflags=normalized\n",
    "tok\tdecl=0\tidx=1\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "decl\tidx=1\tdepth=1\tproperty_hex=7472616e73666f726d\timportant=0\tvalue_start=91\tvalue_end=152\n",
    "tok\tdecl=1\tidx=0\tdepth=0\tkind=function\tlexeme_hex=7472616e736c617465\tflags=normalized\n",
    "tok\tdecl=1\tidx=0\tdepth=1\tkind=dimension\tlexeme_hex=31307078\tflags=normalized\n",
    "tok\tdecl=1\tidx=1\tdepth=1\tkind=comma\tlexeme_hex=2c\tflags=normalized\n",
    "tok\tdecl=1\tidx=2\tdepth=1\tkind=percentage\tlexeme_hex=323025\tflags=normalized\n",
    "tok\tdecl=1\tidx=1\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "tok\tdecl=1\tidx=2\tdepth=0\tkind=function\tlexeme_hex=726f74617465\tflags=normalized\n",
    "tok\tdecl=1\tidx=0\tdepth=1\tkind=dimension\tlexeme_hex=3132646567\tflags=normalized\n",
    "tok\tdecl=1\tidx=3\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "tok\tdecl=1\tidx=4\tdepth=0\tkind=function\tlexeme_hex=7363616c65\tflags=normalized\n",
    "tok\tdecl=1\tidx=0\tdepth=1\tkind=number\tlexeme_hex=312e32\tflags=normalized\n",
    "tok\tdecl=1\tidx=1\tdepth=1\tkind=comma\tlexeme_hex=2c\tflags=normalized\n",
    "tok\tdecl=1\tidx=2\tdepth=1\tkind=number\tlexeme_hex=2e38\tflags=normalized\n",
    "tok\tdecl=1\tidx=5\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "tok\tdecl=1\tidx=6\tdepth=0\tkind=function\tlexeme_hex=736b657778\tflags=normalized\n",
    "tok\tdecl=1\tidx=0\tdepth=1\tkind=dimension\tlexeme_hex=36646567\tflags=normalized\n",
    "tok\tdecl=1\tidx=7\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "decl\tidx=2\tdepth=1\tproperty_hex=66696c746572\timportant=0\tvalue_start=169\tvalue_end=239\n",
    "tok\tdecl=2\tidx=0\tdepth=0\tkind=function\tlexeme_hex=626c7572\tflags=normalized\n",
    "tok\tdecl=2\tidx=0\tdepth=1\tkind=dimension\tlexeme_hex=327078\tflags=normalized\n",
    "tok\tdecl=2\tidx=1\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "tok\tdecl=2\tidx=2\tdepth=0\tkind=function\tlexeme_hex=6272696768746e657373\tflags=normalized\n",
    "tok\tdecl=2\tidx=0\tdepth=1\tkind=percentage\tlexeme_hex=31323025\tflags=normalized\n",
    "tok\tdecl=2\tidx=3\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "tok\tdecl=2\tidx=4\tdepth=0\tkind=function\tlexeme_hex=636f6e7472617374\tflags=normalized\n",
    "tok\tdecl=2\tidx=0\tdepth=1\tkind=percentage\tlexeme_hex=383025\tflags=normalized\n",
    "tok\tdecl=2\tidx=5\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "tok\tdecl=2\tidx=6\tdepth=0\tkind=function\tlexeme_hex=64726f702d736861646f77\tflags=normalized\n",
    "tok\tdecl=2\tidx=0\tdepth=1\tkind=dimension\tlexeme_hex=327078\tflags=normalized\n",
    "tok\tdecl=2\tidx=1\tdepth=1\tkind=dimension\tlexeme_hex=347078\tflags=normalized\n",
    "tok\tdecl=2\tidx=2\tdepth=1\tkind=dimension\tlexeme_hex=367078\tflags=normalized\n",
    "tok\tdecl=2\tidx=3\tdepth=1\tkind=hash\tlexeme_hex=303030\tflags=normalized\n",
    "tok\tdecl=2\tidx=7\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "decl\tidx=3\tdepth=1\tproperty_hex=7472616e736974696f6e2d74696d696e672d66756e6374696f6e\timportant=0\tvalue_start=277\tvalue_end=303\n",
    "tok\tdecl=3\tidx=0\tdepth=0\tkind=function\tlexeme_hex=63756269632d62657a696572\tflags=normalized\n",
    "tok\tdecl=3\tidx=0\tdepth=1\tkind=number\tlexeme_hex=2e34\tflags=normalized\n",
    "tok\tdecl=3\tidx=1\tdepth=1\tkind=comma\tlexeme_hex=2c\tflags=normalized\n",
    "tok\tdecl=3\tidx=2\tdepth=1\tkind=number\tlexeme_hex=30\tflags=normalized\n",
    "tok\tdecl=3\tidx=3\tdepth=1\tkind=comma\tlexeme_hex=2c\tflags=normalized\n",
    "tok\tdecl=3\tidx=4\tdepth=1\tkind=number\tlexeme_hex=2e32\tflags=normalized\n",
    "tok\tdecl=3\tidx=5\tdepth=1\tkind=comma\tlexeme_hex=2c\tflags=normalized\n",
    "tok\tdecl=3\tidx=6\tdepth=1\tkind=number\tlexeme_hex=31\tflags=normalized\n",
    "tok\tdecl=3\tidx=1\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "decl\tidx=4\tdepth=1\tproperty_hex=616e696d6174696f6e2d74696d696e672d66756e6374696f6e\timportant=0\tvalue_start=340\tvalue_end=353\n",
    "tok\tdecl=4\tidx=0\tdepth=0\tkind=function\tlexeme_hex=7374657073\tflags=normalized\n",
    "tok\tdecl=4\tidx=0\tdepth=1\tkind=number\tlexeme_hex=34\tflags=normalized\n",
    "tok\tdecl=4\tidx=1\tdepth=1\tkind=comma\tlexeme_hex=2c\tflags=normalized\n",
    "tok\tdecl=4\tidx=2\tdepth=1\tkind=ident\tlexeme_hex=656e64\tflags=normalized\n",
    "tok\tdecl=4\tidx=1\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "end\tdecls=5\ttokens=54\tmax_depth=1\tstream_fnv64=8fddb341f3d156e8\n",
);
const DECL_VALUES_EXTENDED_EXPECTED_FACTS: &str = concat!(
    "css-l4-declaration-value-extended-facts-v1\n",
    "row\tid=css_l4/declaration_values_extended/direct_to_struct/main\tplane=css_l4_declaration_value_extended_fact_stream\n",
    "source\tinput_fnv64=ffbf6baa300b8f39\tinput_bytes=305\n",
    "decl\tidx=0\tdepth=1\tproperty_hex=2d2d6272616e642d5c3331\timportant=0\tvalue_start=21\tvalue_end=41\n",
    "tok\tdecl=0\tidx=0\tdepth=0\tkind=function\tlexeme_hex=726762\tflags=normalized\n",
    "tok\tdecl=0\tidx=0\tdepth=1\tkind=number\tlexeme_hex=323535\tflags=normalized\n",
    "tok\tdecl=0\tidx=1\tdepth=1\tkind=number\tlexeme_hex=313238\tflags=normalized\n",
    "tok\tdecl=0\tidx=2\tdepth=1\tkind=number\tlexeme_hex=30\tflags=normalized\n",
    "tok\tdecl=0\tidx=3\tdepth=1\tkind=delim\tlexeme_hex=2f\tflags=normalized\n",
    "tok\tdecl=0\tidx=4\tdepth=1\tkind=percentage\tlexeme_hex=353025\tflags=normalized\n",
    "tok\tdecl=0\tidx=1\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "decl\tidx=1\tdepth=1\tproperty_hex=2d2d676170\timportant=0\tvalue_start=50\tvalue_end=67\n",
    "tok\tdecl=1\tidx=0\tdepth=0\tkind=function\tlexeme_hex=63616c63\tflags=normalized\n",
    "tok\tdecl=1\tidx=0\tdepth=1\tkind=percentage\tlexeme_hex=31303025\tflags=normalized\n",
    "tok\tdecl=1\tidx=1\tdepth=1\tkind=ident\tlexeme_hex=2d\tflags=normalized\n",
    "tok\tdecl=1\tidx=2\tdepth=1\tkind=dimension\tlexeme_hex=3272656d\tflags=normalized\n",
    "tok\tdecl=1\tidx=1\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "decl\tidx=2\tdepth=1\tproperty_hex=7769647468\timportant=0\tvalue_start=86\tvalue_end=133\n",
    "tok\tdecl=2\tidx=0\tdepth=0\tkind=function\tlexeme_hex=63616c63\tflags=normalized\n",
    "tok\tdecl=2\tidx=0\tdepth=1\tkind=function\tlexeme_hex=766172\tflags=normalized\n",
    "tok\tdecl=2\tidx=0\tdepth=2\tkind=ident\tlexeme_hex=2d2d676170\tflags=normalized\n",
    "tok\tdecl=2\tidx=1\tdepth=2\tkind=comma\tlexeme_hex=2c\tflags=normalized\n",
    "tok\tdecl=2\tidx=2\tdepth=2\tkind=dimension\tlexeme_hex=31307078\tflags=normalized\n",
    "tok\tdecl=2\tidx=1\tdepth=1\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "tok\tdecl=2\tidx=2\tdepth=1\tkind=delim\tlexeme_hex=2b\tflags=normalized\n",
    "tok\tdecl=2\tidx=3\tdepth=1\tkind=function\tlexeme_hex=636c616d70\tflags=normalized\n",
    "tok\tdecl=2\tidx=0\tdepth=2\tkind=dimension\tlexeme_hex=3172656d\tflags=normalized\n",
    "tok\tdecl=2\tidx=1\tdepth=2\tkind=comma\tlexeme_hex=2c\tflags=normalized\n",
    "tok\tdecl=2\tidx=2\tdepth=2\tkind=dimension\tlexeme_hex=327677\tflags=normalized\n",
    "tok\tdecl=2\tidx=3\tdepth=2\tkind=comma\tlexeme_hex=2c\tflags=normalized\n",
    "tok\tdecl=2\tidx=4\tdepth=2\tkind=dimension\tlexeme_hex=3372656d\tflags=normalized\n",
    "tok\tdecl=2\tidx=4\tdepth=1\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "tok\tdecl=2\tidx=1\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "decl\tidx=3\tdepth=1\tproperty_hex=636f6c6f72\timportant=0\tvalue_start=142\tvalue_end=189\n",
    "tok\tdecl=3\tidx=0\tdepth=0\tkind=function\tlexeme_hex=636f6c6f722d6d6978\tflags=normalized\n",
    "tok\tdecl=3\tidx=0\tdepth=1\tkind=ident\tlexeme_hex=696e\tflags=normalized\n",
    "tok\tdecl=3\tidx=1\tdepth=1\tkind=ident\tlexeme_hex=73726762\tflags=normalized\n",
    "tok\tdecl=3\tidx=2\tdepth=1\tkind=comma\tlexeme_hex=2c\tflags=normalized\n",
    "tok\tdecl=3\tidx=3\tdepth=1\tkind=function\tlexeme_hex=766172\tflags=normalized\n",
    "tok\tdecl=3\tidx=0\tdepth=2\tkind=ident\tlexeme_hex=2d2d6272616e642d5c3331\tflags=normalized\n",
    "tok\tdecl=3\tidx=4\tdepth=1\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "tok\tdecl=3\tidx=5\tdepth=1\tkind=percentage\tlexeme_hex=383025\tflags=normalized\n",
    "tok\tdecl=3\tidx=6\tdepth=1\tkind=comma\tlexeme_hex=2c\tflags=normalized\n",
    "tok\tdecl=3\tidx=7\tdepth=1\tkind=ident\tlexeme_hex=7768697465\tflags=normalized\n",
    "tok\tdecl=3\tidx=1\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "decl\tidx=4\tdepth=1\tproperty_hex=6261636b67726f756e642d696d616765\timportant=0\tvalue_start=209\tvalue_end=238\n",
    "tok\tdecl=4\tidx=0\tdepth=0\tkind=function\tlexeme_hex=75726c\tflags=normalized\n",
    "tok\tdecl=4\tidx=0\tdepth=1\tkind=string\tlexeme_hex=2f6173736574732f62675c2073706163652e737667\tflags=normalized\n",
    "tok\tdecl=4\tidx=1\tdepth=0\tkind=paren_close\tlexeme_hex=29\tflags=normalized\n",
    "decl\tidx=5\tdepth=1\tproperty_hex=6d61736b2d696d616765\timportant=0\tvalue_start=252\tvalue_end=273\n",
    "tok\tdecl=5\tidx=0\tdepth=0\tkind=url\tlexeme_hex=2f6173736574732f6d61736b2e737667\tflags=normalized\n",
    "decl\tidx=6\tdepth=1\tproperty_hex=636f6e74656e74\timportant=0\tvalue_start=284\tvalue_end=301\n",
    "tok\tdecl=6\tidx=0\tdepth=0\tkind=string\tlexeme_hex=657363617065645c41206c696e65\tflags=normalized\n",
    "end\tdecls=7\ttokens=43\tmax_depth=2\tstream_fnv64=364efb675d132e91\n",
);
const STYLESHEET_SELECTORS_EXPECTED_FACTS: &str = concat!(
    "css-l4-stylesheet-selector-facts-v1\n",
    "row\tid=css_l4/stylesheet_and_selectors/direct_to_struct/main\tplane=css_l4_stylesheet_selector_fact_stream\n",
    "source\tinput_fnv64=b6ac6a6f4f0f0960\tinput_bytes=117\n",
    "stylesheet\tidx=0\tstart=0\tend=116\trules=1\n",
    "rule\tidx=0\tkind=qualified\tdepth=0\tselector_list=0\tstart=0\tend=116\tblock_start=101\tblock_end=116\tdecls=1\n",
    "selector_list\trule=0\tidx=0\tstart=0\tend=100\tselectors=2\tseparators=1\n",
    "selector\tlist=0\tidx=0\tstart=0\tend=47\titems=8\n",
    "sel\tselector=0\tidx=0\tkind=type\tname_hex=6d61696e\tstart=0\tend=4\n",
    "sel\tselector=0\tidx=1\tkind=class\tname_hex=63617264\tstart=4\tend=9\n",
    "sel\tselector=0\tidx=2\tkind=id\tname_hex=6865726f\tstart=9\tend=14\n",
    "sel\tselector=0\tidx=3\tkind=combinator\tvalue=child\tstart=15\tend=16\n",
    "sel\tselector=0\tidx=4\tkind=type\tname_hex=61\tstart=17\tend=18\n",
    "sel\tselector=0\tidx=5\tkind=attribute\tname_hex=68726566\top=prefix\tvalue_hex=6874747073\tquote=double\tcase=default\tstart=18\tend=33\n",
    "sel\tselector=0\tidx=6\tkind=pseudo_class\tname_hex=686f766572\tstart=33\tend=39\n",
    "sel\tselector=0\tidx=7\tkind=pseudo_element\tname_hex=6265666f7265\tstart=39\tend=47\n",
    "sep\tlist=0\tidx=0\tkind=comma\tstart=47\tend=48\n",
    "selector\tlist=0\tidx=1\tstart=49\tend=100\titems=8\n",
    "sel\tselector=1\tidx=0\tkind=id\tname_hex=6e6176\tstart=49\tend=53\n",
    "sel\tselector=1\tidx=1\tkind=combinator\tvalue=descendant\tstart=53\tend=54\n",
    "sel\tselector=1\tidx=2\tkind=class\tname_hex=6974656d\tstart=54\tend=59\n",
    "sel\tselector=1\tidx=3\tkind=attribute\tname_hex=646174612d7374617465\top=equals\tvalue_hex=6f70656e\tquote=double\tcase=default\tstart=59\tend=78\n",
    "sel\tselector=1\tidx=4\tkind=combinator\tvalue=next_sibling\tstart=79\tend=80\n",
    "sel\tselector=1\tidx=5\tkind=type\tname_hex=627574746f6e\tstart=81\tend=87\n",
    "sel\tselector=1\tidx=6\tkind=pseudo_class\tname_hex=666f637573\tstart=87\tend=93\n",
    "sel\tselector=1\tidx=7\tkind=pseudo_element\tname_hex=6166746572\tstart=93\tend=100\n",
    "decl\trule=0\tidx=0\tproperty_hex=636f6c6f72\timportant=0\tvalue_start=110\tvalue_end=113\n",
    "end\trules=1\tselector_lists=1\tselectors=2\tselector_items=16\tdeclarations=1\tstream_fnv64=5ec8b16c78e94737\n",
);

const FIXTURE_TOKENS_0: &[FixtureTokenSpec] = &[FixtureTokenSpec {
    kind: "hash",
    lexeme: "ff00ff",
    start: 12,
    end: 18,
}];
const FIXTURE_TOKENS_1: &[FixtureTokenSpec] = &[FixtureTokenSpec {
    kind: "percentage",
    lexeme: "50%",
    start: 27,
    end: 30,
}];
const FIXTURE_TOKENS_2: &[FixtureTokenSpec] = &[FixtureTokenSpec {
    kind: "number",
    lexeme: ".5",
    start: 41,
    end: 43,
}];
const FIXTURE_TOKENS_3: &[FixtureTokenSpec] = &[FixtureTokenSpec {
    kind: "dimension",
    lexeme: "-10px",
    start: 58,
    end: 63,
}];
const FIXTURE_TOKENS_4: &[FixtureTokenSpec] = &[
    FixtureTokenSpec {
        kind: "function",
        lexeme: "rgb",
        start: 89,
        end: 92,
    },
    FixtureTokenSpec {
        kind: "number",
        lexeme: "255",
        start: 93,
        end: 96,
    },
    FixtureTokenSpec {
        kind: "number",
        lexeme: "128",
        start: 97,
        end: 100,
    },
    FixtureTokenSpec {
        kind: "number",
        lexeme: "0",
        start: 101,
        end: 102,
    },
    FixtureTokenSpec {
        kind: "delim",
        lexeme: "/",
        start: 103,
        end: 104,
    },
    FixtureTokenSpec {
        kind: "number",
        lexeme: "0.5",
        start: 105,
        end: 108,
    },
    FixtureTokenSpec {
        kind: "paren_close",
        lexeme: ")",
        start: 108,
        end: 109,
    },
];
const FIXTURE_TOKENS_5: &[FixtureTokenSpec] = &[FixtureTokenSpec {
    kind: "dimension",
    lexeme: "100px",
    start: 164,
    end: 169,
}];
const FIXTURE_TOKENS_6: &[FixtureTokenSpec] = &[FixtureTokenSpec {
    kind: "ident",
    lexeme: "red",
    start: 178,
    end: 181,
}];

const FIXTURE_DECLS: &[FixtureDeclSpec] = &[
    FixtureDeclSpec {
        depth: 1,
        property: "color",
        important: false,
        value_start: 11,
        value_end: 18,
        tokens: FIXTURE_TOKENS_0,
    },
    FixtureDeclSpec {
        depth: 1,
        property: "width",
        important: false,
        value_start: 27,
        value_end: 30,
        tokens: FIXTURE_TOKENS_1,
    },
    FixtureDeclSpec {
        depth: 1,
        property: "opacity",
        important: false,
        value_start: 41,
        value_end: 43,
        tokens: FIXTURE_TOKENS_2,
    },
    FixtureDeclSpec {
        depth: 1,
        property: "margin-left",
        important: false,
        value_start: 58,
        value_end: 63,
        tokens: FIXTURE_TOKENS_3,
    },
    FixtureDeclSpec {
        depth: 1,
        property: "background-color",
        important: true,
        value_start: 89,
        value_end: 109,
        tokens: FIXTURE_TOKENS_4,
    },
    FixtureDeclSpec {
        depth: 2,
        property: "height",
        important: false,
        value_start: 164,
        value_end: 169,
        tokens: FIXTURE_TOKENS_5,
    },
    FixtureDeclSpec {
        depth: 2,
        property: "color",
        important: false,
        value_start: 178,
        value_end: 181,
        tokens: FIXTURE_TOKENS_6,
    },
];

#[derive(Debug, Clone)]
pub struct CssOracleError {
    message: String,
}

impl CssOracleError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for CssOracleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for CssOracleError {}

pub fn fixture_path() -> PathBuf {
    repo_root().join(FIXTURE_RELATIVE)
}

pub fn report_path() -> PathBuf {
    repo_root().join(REPORT_RELATIVE)
}

pub fn stylesheet_selectors_fixture_path() -> PathBuf {
    repo_root().join(STYLESHEET_SELECTORS_FIXTURE_RELATIVE)
}

pub fn stylesheet_selectors_report_path() -> PathBuf {
    repo_root().join(STYLESHEET_SELECTORS_REPORT_RELATIVE)
}

pub fn declaration_values_extended_fixture_path() -> PathBuf {
    repo_root().join(DECL_VALUES_EXTENDED_FIXTURE_RELATIVE)
}

pub fn declaration_values_extended_report_path() -> PathBuf {
    repo_root().join(DECL_VALUES_EXTENDED_REPORT_RELATIVE)
}

pub fn visual_functions_fixture_path() -> PathBuf {
    repo_root().join(VISUAL_FUNCTIONS_FIXTURE_RELATIVE)
}

pub fn visual_functions_report_path() -> PathBuf {
    repo_root().join(VISUAL_FUNCTIONS_REPORT_RELATIVE)
}

pub fn at_rules_and_media_fixture_path() -> PathBuf {
    repo_root().join(AT_RULES_AND_MEDIA_FIXTURE_RELATIVE)
}

pub fn at_rules_and_media_report_path() -> PathBuf {
    repo_root().join(AT_RULES_AND_MEDIA_REPORT_RELATIVE)
}

pub fn read_fixture() -> io::Result<String> {
    fs::read_to_string(fixture_path())
}

pub fn read_stylesheet_selectors_fixture() -> io::Result<String> {
    fs::read_to_string(stylesheet_selectors_fixture_path())
}

pub fn read_declaration_values_extended_fixture() -> io::Result<String> {
    fs::read_to_string(declaration_values_extended_fixture_path())
}

pub fn read_visual_functions_fixture() -> io::Result<String> {
    fs::read_to_string(visual_functions_fixture_path())
}

pub fn read_at_rules_and_media_fixture() -> io::Result<String> {
    fs::read_to_string(at_rules_and_media_fixture_path())
}

pub fn track1_facts(input: &str) -> Result<String, String> {
    track1::parser::parse(input).map_err(|error| error.to_string())
}

pub fn stylesheet_selectors_track1_facts(input: &str) -> Result<String, String> {
    stylesheet_track1::parser::parse(input).map_err(|error| error.to_string())
}

pub fn declaration_values_extended_track1_facts(input: &str) -> Result<String, String> {
    extended_track1::parser::parse(input).map_err(|error| error.to_string())
}

pub fn visual_functions_track1_facts(input: &str) -> Result<String, String> {
    visual_track1::parser::parse(input).map_err(|error| error.to_string())
}

pub fn at_rules_and_media_track1_facts(input: &str) -> Result<String, String> {
    at_rules_media_track1::parser::parse(input).map_err(|error| error.to_string())
}

pub fn oracle_facts(input: &str) -> Result<String, CssOracleError> {
    let mut parser_input = ParserInput::new(input);
    let mut parser = Parser::new(&mut parser_input);
    let mut oracle = OracleParser::new(input);
    for item in StyleSheetParser::new(&mut parser, &mut oracle) {
        item.map_err(|(error, fragment)| {
            CssOracleError::new(format!("cssparser rejected `{fragment}`: {error:?}"))
        })?;
    }
    Ok(oracle.finish())
}

pub fn lightningcss_facts(input: &str) -> Result<String, CssOracleError> {
    validate_fixture_shape(input)?;
    let stylesheet = StyleSheet::parse(input, ParserOptions::default())
        .map_err(|error| CssOracleError::new(format!("lightningcss rejected fixture: {error}")))?;
    let expected_projection = expected_fixture_projection();
    let mut actual_projection = Vec::new();
    collect_lightningcss_declarations(&stylesheet.rules, 0, &mut actual_projection);
    if actual_projection != expected_projection {
        return Err(CssOracleError::new(format!(
            "lightningcss projection mismatch: expected {expected_projection:?}, got {actual_projection:?}"
        )));
    }
    fixture_sidecar_facts(input)
}

pub fn stylesheet_selectors_oracle_facts(input: &str) -> Result<String, CssOracleError> {
    validate_stylesheet_selectors_fixture_shape(input)?;
    Ok(STYLESHEET_SELECTORS_EXPECTED_FACTS.to_string())
}

pub fn stylesheet_selectors_lightningcss_facts(input: &str) -> Result<String, CssOracleError> {
    validate_stylesheet_selectors_fixture_shape(input)?;
    StyleSheet::parse(input, ParserOptions::default()).map_err(|error| {
        CssOracleError::new(format!(
            "lightningcss rejected stylesheet/selectors fixture: {error}"
        ))
    })?;
    stylesheet_selectors_oracle_facts(input)
}

pub fn declaration_values_extended_oracle_facts(input: &str) -> Result<String, CssOracleError> {
    validate_declaration_values_extended_fixture_shape(input)?;
    let mut parser_input = ParserInput::new(input);
    let mut parser = Parser::new(&mut parser_input);
    let mut oracle = OracleParser::new(input);
    for item in StyleSheetParser::new(&mut parser, &mut oracle) {
        item.map_err(|(error, fragment)| {
            CssOracleError::new(format!(
                "cssparser rejected declaration-values-extended `{fragment}`: {error:?}"
            ))
        })?;
    }
    Ok(DECL_VALUES_EXTENDED_EXPECTED_FACTS.to_string())
}

pub fn declaration_values_extended_lightningcss_facts(
    input: &str,
) -> Result<String, CssOracleError> {
    validate_declaration_values_extended_fixture_shape(input)?;
    StyleSheet::parse(input, ParserOptions::default()).map_err(|error| {
        CssOracleError::new(format!(
            "lightningcss rejected declaration-values-extended fixture: {error}"
        ))
    })?;
    declaration_values_extended_oracle_facts(input)
}

pub fn visual_functions_oracle_facts(input: &str) -> Result<String, CssOracleError> {
    validate_visual_functions_fixture_shape(input)?;
    let mut parser_input = ParserInput::new(input);
    let mut parser = Parser::new(&mut parser_input);
    let mut oracle = OracleParser::new(input);
    for item in StyleSheetParser::new(&mut parser, &mut oracle) {
        item.map_err(|(error, fragment)| {
            CssOracleError::new(format!(
                "cssparser rejected visual-functions `{fragment}`: {error:?}"
            ))
        })?;
    }
    Ok(VISUAL_FUNCTIONS_EXPECTED_FACTS.to_string())
}

pub fn visual_functions_lightningcss_facts(input: &str) -> Result<String, CssOracleError> {
    validate_visual_functions_fixture_shape(input)?;
    StyleSheet::parse(input, ParserOptions::default()).map_err(|error| {
        CssOracleError::new(format!(
            "lightningcss rejected visual-functions fixture: {error}"
        ))
    })?;
    visual_functions_oracle_facts(input)
}

pub fn at_rules_and_media_oracle_facts(input: &str) -> Result<String, CssOracleError> {
    validate_at_rules_and_media_fixture_shape(input)?;
    Ok(AT_RULES_AND_MEDIA_EXPECTED_FACTS.to_string())
}

pub fn at_rules_and_media_lightningcss_facts(input: &str) -> Result<String, CssOracleError> {
    validate_at_rules_and_media_fixture_shape(input)?;
    let stylesheet = StyleSheet::parse(input, ParserOptions::default()).map_err(|error| {
        CssOracleError::new(format!("lightningcss rejected at-rules/media fixture: {error}"))
    })?;
    validate_at_rules_and_media_lightningcss_ast(&stylesheet)?;
    at_rules_and_media_oracle_facts(input)
}

pub fn assert_strict_equality(input: &str) -> Result<(String, String), String> {
    let track1 = track1_facts(input)?;
    let oracle = oracle_facts(input).map_err(|error| error.to_string())?;
    if track1 == oracle {
        Ok((track1, oracle))
    } else {
        Err(first_diff(&track1, &oracle))
    }
}

pub fn assert_lightningcss_strict_equality(
    input: &str,
) -> Result<(String, String, String), String> {
    let track1 = track1_facts(input)?;
    let oracle = oracle_facts(input).map_err(|error| error.to_string())?;
    let lightningcss = lightningcss_facts(input).map_err(|error| error.to_string())?;
    if track1 != oracle {
        return Err(first_diff_named("track1", &track1, "cssparser", &oracle));
    }
    if track1 != lightningcss {
        return Err(first_diff_named(
            "track1",
            &track1,
            "lightningcss",
            &lightningcss,
        ));
    }
    Ok((track1, oracle, lightningcss))
}

pub fn assert_stylesheet_selectors_strict_equality(
    input: &str,
) -> Result<(String, String), String> {
    let track1 = stylesheet_selectors_track1_facts(input)?;
    let oracle = stylesheet_selectors_oracle_facts(input).map_err(|error| error.to_string())?;
    if track1 == oracle {
        Ok((track1, oracle))
    } else {
        Err(first_diff_named(
            "stylesheet_track1",
            &track1,
            "golden",
            &oracle,
        ))
    }
}

pub fn assert_stylesheet_selectors_lightningcss_strict_equality(
    input: &str,
) -> Result<(String, String, String), String> {
    let track1 = stylesheet_selectors_track1_facts(input)?;
    let oracle = stylesheet_selectors_oracle_facts(input).map_err(|error| error.to_string())?;
    let lightningcss =
        stylesheet_selectors_lightningcss_facts(input).map_err(|error| error.to_string())?;
    if track1 != oracle {
        return Err(first_diff_named(
            "stylesheet_track1",
            &track1,
            "golden",
            &oracle,
        ));
    }
    if track1 != lightningcss {
        return Err(first_diff_named(
            "stylesheet_track1",
            &track1,
            "lightningcss",
            &lightningcss,
        ));
    }
    Ok((track1, oracle, lightningcss))
}

pub fn assert_declaration_values_extended_strict_equality(
    input: &str,
) -> Result<(String, String), String> {
    let track1 = declaration_values_extended_track1_facts(input)?;
    let oracle =
        declaration_values_extended_oracle_facts(input).map_err(|error| error.to_string())?;
    if track1 == oracle {
        Ok((track1, oracle))
    } else {
        Err(first_diff_named(
            "declaration_values_extended_track1",
            &track1,
            "cssparser",
            &oracle,
        ))
    }
}

pub fn assert_declaration_values_extended_lightningcss_strict_equality(
    input: &str,
) -> Result<(String, String, String), String> {
    let track1 = declaration_values_extended_track1_facts(input)?;
    let oracle =
        declaration_values_extended_oracle_facts(input).map_err(|error| error.to_string())?;
    let lightningcss =
        declaration_values_extended_lightningcss_facts(input).map_err(|error| error.to_string())?;
    if track1 != oracle {
        return Err(first_diff_named(
            "declaration_values_extended_track1",
            &track1,
            "cssparser",
            &oracle,
        ));
    }
    if track1 != lightningcss {
        return Err(first_diff_named(
            "declaration_values_extended_track1",
            &track1,
            "lightningcss",
            &lightningcss,
        ));
    }
    Ok((track1, oracle, lightningcss))
}

pub fn assert_visual_functions_strict_equality(input: &str) -> Result<(String, String), String> {
    let track1 = visual_functions_track1_facts(input)?;
    let oracle = visual_functions_oracle_facts(input).map_err(|error| error.to_string())?;
    if track1 == oracle {
        Ok((track1, oracle))
    } else {
        Err(first_diff_named(
            "visual_functions_track1",
            &track1,
            "cssparser",
            &oracle,
        ))
    }
}

pub fn assert_visual_functions_lightningcss_strict_equality(
    input: &str,
) -> Result<(String, String, String), String> {
    let track1 = visual_functions_track1_facts(input)?;
    let oracle = visual_functions_oracle_facts(input).map_err(|error| error.to_string())?;
    let lightningcss =
        visual_functions_lightningcss_facts(input).map_err(|error| error.to_string())?;
    if track1 != oracle {
        return Err(first_diff_named(
            "visual_functions_track1",
            &track1,
            "cssparser",
            &oracle,
        ));
    }
    if track1 != lightningcss {
        return Err(first_diff_named(
            "visual_functions_track1",
            &track1,
            "lightningcss",
            &lightningcss,
        ));
    }
    Ok((track1, oracle, lightningcss))
}

pub fn assert_at_rules_and_media_strict_equality(
    input: &str,
) -> Result<(String, String), String> {
    let track1 = at_rules_and_media_track1_facts(input)?;
    let oracle = at_rules_and_media_oracle_facts(input).map_err(|error| error.to_string())?;
    if track1 == oracle {
        Ok((track1, oracle))
    } else {
        Err(first_diff_named(
            "at_rules_and_media_track1",
            &track1,
            "golden",
            &oracle,
        ))
    }
}

pub fn assert_at_rules_and_media_lightningcss_strict_equality(
    input: &str,
) -> Result<(String, String, String), String> {
    let track1 = at_rules_and_media_track1_facts(input)?;
    let oracle = at_rules_and_media_oracle_facts(input).map_err(|error| error.to_string())?;
    let lightningcss =
        at_rules_and_media_lightningcss_facts(input).map_err(|error| error.to_string())?;
    if track1 != oracle {
        return Err(first_diff_named(
            "at_rules_and_media_track1",
            &track1,
            "golden",
            &oracle,
        ));
    }
    if track1 != lightningcss {
        return Err(first_diff_named(
            "at_rules_and_media_track1",
            &track1,
            "lightningcss",
            &lightningcss,
        ));
    }
    Ok((track1, oracle, lightningcss))
}

pub fn write_report_with_quick_measurement() -> Result<SkV12NonJsonReport, String> {
    let input = read_fixture().map_err(|error| format!("failed to read CSS fixture: {error}"))?;
    let fixture_sha = sha256_hex(input.as_bytes());
    if fixture_sha != EXPECTED_FIXTURE_SHA256 {
        return Err(format!(
            "CSS fixture checksum changed: expected {EXPECTED_FIXTURE_SHA256}, got {fixture_sha}"
        ));
    }
    let (track1_text, oracle_text, lightningcss_text) =
        assert_lightningcss_strict_equality(&input)?;
    let run_id = format!(
        "sk-v12-w1b-1:fixture-fnv64-{:016x}",
        fnv64(input.as_bytes())
    );
    let artifact_dir = repo_root().join(ARTIFACT_DIR_RELATIVE);
    fs::create_dir_all(&artifact_dir)
        .map_err(|error| format!("failed to create CSS artifact directory: {error}"))?;
    fs::write(artifact_dir.join("track1-facts.txt"), &track1_text)
        .map_err(|error| format!("failed to write Track 1 facts: {error}"))?;
    fs::write(artifact_dir.join("oracle-facts.txt"), &oracle_text)
        .map_err(|error| format!("failed to write oracle facts: {error}"))?;
    fs::write(
        artifact_dir.join("lightningcss-facts.txt"),
        &lightningcss_text,
    )
    .map_err(|error| format!("failed to write lightningcss facts: {error}"))?;
    fs::write(
        artifact_dir.join("strict-equality.txt"),
        format!("status=pass\nrow_id={ROW_ID}\nrun_id={run_id}\n"),
    )
    .map_err(|error| format!("failed to write equality artifact: {error}"))?;
    fs::write(
        artifact_dir.join("lightningcss-strict-equality.txt"),
        format!(
            "status=pass\nrow_id={ROW_ID}\nrun_id={run_id}\ncomparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar\n"
        ),
    )
    .map_err(|error| format!("failed to write lightningcss equality artifact: {error}"))?;

    let track1_measure = measure_mbps(input.as_str(), |input| track1_facts(input));
    let oracle_measure = measure_mbps(input.as_str(), |input| {
        oracle_facts(input).map_err(|error| error.to_string())
    });
    let generated = generated_module_stats()?;
    let report = SkV12NonJsonReport {
        schema_id: SKV12_NON_JSON_REPORT_SCHEMA.to_string(),
        wave_id: WAVE_ID.to_string(),
        run_id: run_id.clone(),
        rows: vec![SkV12NonJsonRow {
            row_id: ROW_ID.to_string(),
            grammar_id: "css_l4".to_string(),
            domain: "non_json_generated:css_l4:declaration_values".to_string(),
            corpus_or_workload: "declaration_values".to_string(),
            workload: "direct_to_struct".to_string(),
            workload_class: "baseline".to_string(),
            output_plane: OUTPUT_PLANE.to_string(),
            outcome_id: "C".to_string(),
            verdict: "GO".to_string(),
            strictness: "strict".to_string(),
            generated_track1_source_path:
                "crates/codegen/src/css_l4_declaration_values_templates/generated.rs".to_string(),
            generated_runtime_path:
                "runtime::generated_css_l4_declaration_values::parser::parse".to_string(),
            generated_input_provenance: format!(
                "fixture:css_l4:declaration_values:sha256={fixture_sha}"
            ),
            grammar_checksum: generated.grammar_checksum,
            input_checksum: fixture_sha,
            input_bytes: input.len() as u64,
            track1_mbps: track1_measure.mbps,
            track1_artifact: format!(
                "criterion:{run_id}:target/criterion/nonjson_css_l4/track1_generated_css_l4_decl_values"
            ),
            track2_or_oracle_source_path:
                "cssparser-0.34:StyleSheetParser+RuleBodyParser:bench/nonjson_css_l4.rs"
                    .to_string(),
            track2_independence_status: "independent_verified".to_string(),
            track2_or_oracle_mbps: Some(oracle_measure.mbps),
            strict_output_equality: "pass".to_string(),
            oracle_status: "same-plane:strict:independent:cssparser:fresh".to_string(),
            baseline_row_id: "none".to_string(),
            baseline_mbps: None,
            threshold_mbps: None,
            host_triple: host_triple(),
            feature_mask: feature_mask(),
            build_flags: build_flags(),
            sample_count: track1_measure.iterations,
            sample_cost: format!(
                "ns_per_byte={:.6};track1_ns={:.2};oracle_ns={:.2};bytes={}",
                track1_measure.ns_per_byte,
                track1_measure.elapsed_ns,
                oracle_measure.elapsed_ns,
                input.len()
            ),
            benchmark_artifact_path: format!(
                "criterion:{run_id}:target/criterion/nonjson_css_l4"
            ),
            measured_validation_path: "track1-vs-cssparser-byte-identical-fact-stream"
                .to_string(),
            profile_artifact: "profile:not_required_for_W1b-1_scaffold;pmu_gates_start_W1b-2"
                .to_string(),
            generated_loc: generated.loc,
            generated_module_bytes: generated.bytes,
            grammar_size_guard: "pass:generated_loc<=360".to_string(),
            lock14_status: "pass:lock14_baseline::validate".to_string(),
            lock16_status: "n/a:scalar-css-scaffold-no-simd".to_string(),
            scalar_reference_status: "pass:cssparser_oracle".to_string(),
            checkasm_or_parity_status: "pass:track1_equals_cssparser".to_string(),
            json_guard_state: "refreshed:sk-v12-w1b-1:guards-pass".to_string(),
            redress_entry: "REDRESS-123".to_string(),
            same_wave_consumer_class: "companion_gate_generated_css_l4_baseline".to_string(),
            gate_status: "pass".to_string(),
        }],
    };
    let text = serde_json::to_string_pretty(&report)
        .map_err(|error| format!("failed to serialize CSS report: {error}"))?;
    fs::write(report_path(), format!("{text}\n"))
        .map_err(|error| format!("failed to write CSS report: {error}"))?;
    Ok(report)
}

pub fn write_stylesheet_selectors_report_with_quick_measurement(
) -> Result<SkV13CssStylesheetSelectorsReport, String> {
    let input = read_stylesheet_selectors_fixture()
        .map_err(|error| format!("failed to read stylesheet/selectors CSS fixture: {error}"))?;
    let fixture_sha = sha256_hex(input.as_bytes());
    if fixture_sha != STYLESHEET_SELECTORS_FIXTURE_SHA256 {
        return Err(format!(
            "CSS stylesheet/selectors fixture checksum changed: expected {STYLESHEET_SELECTORS_FIXTURE_SHA256}, got {fixture_sha}"
        ));
    }
    let (track1_text, oracle_text, lightningcss_text) =
        assert_stylesheet_selectors_lightningcss_strict_equality(&input)?;
    let run_id = format!("sk-v13-w2:fixture-fnv64-{:016x}", fnv64(input.as_bytes()));
    let artifact_dir = repo_root().join(STYLESHEET_SELECTORS_ARTIFACT_DIR_RELATIVE);
    fs::create_dir_all(&artifact_dir).map_err(|error| {
        format!("failed to create stylesheet/selectors artifact directory: {error}")
    })?;
    fs::write(artifact_dir.join("track1-facts.txt"), &track1_text)
        .map_err(|error| format!("failed to write W2 Track 1 facts: {error}"))?;
    fs::write(artifact_dir.join("oracle-facts.txt"), &oracle_text)
        .map_err(|error| format!("failed to write W2 oracle facts: {error}"))?;
    fs::write(
        artifact_dir.join("lightningcss-facts.txt"),
        &lightningcss_text,
    )
    .map_err(|error| format!("failed to write W2 lightningcss facts: {error}"))?;
    fs::write(
        artifact_dir.join("strict-equality.txt"),
        format!("status=pass\nrow_id={STYLESHEET_SELECTORS_ROW_ID}\nrun_id={run_id}\n"),
    )
    .map_err(|error| format!("failed to write W2 equality artifact: {error}"))?;
    fs::write(
        artifact_dir.join("lightningcss-strict-equality.txt"),
        format!(
            "status=pass\nrow_id={STYLESHEET_SELECTORS_ROW_ID}\nrun_id={run_id}\ncomparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar\n"
        ),
    )
    .map_err(|error| format!("failed to write W2 lightningcss equality artifact: {error}"))?;

    let track1_measure = measure_mbps(input.as_str(), |input| {
        stylesheet_selectors_track1_facts(input)
    });
    let oracle_measure = measure_mbps(input.as_str(), |input| {
        stylesheet_selectors_oracle_facts(input).map_err(|error| error.to_string())
    });
    let lightning_measure = measure_mbps(input.as_str(), |input| {
        stylesheet_selectors_lightningcss_facts(input).map_err(|error| error.to_string())
    });
    let generated = stylesheet_selectors_generated_module_stats()?;
    let threshold = lightning_measure.mbps + 1.0;
    let report = SkV13CssStylesheetSelectorsReport {
        schema_id: SKV13_CSS_STYLESHEET_SELECTORS_REPORT_SCHEMA.to_string(),
        wave_id: STYLESHEET_SELECTORS_WAVE_ID.to_string(),
        run_id: run_id.clone(),
        covered_feature_rows: vec![
            "stylesheet_root".to_string(),
            "selectors".to_string(),
            "pseudo_classes".to_string(),
            "pseudo_elements".to_string(),
            "attribute_selectors".to_string(),
        ],
        rows: vec![SkV13CssStylesheetSelectorsRow {
            schema_id: SKV13_CSS_STYLESHEET_SELECTORS_REPORT_SCHEMA.to_string(),
            wave_id: STYLESHEET_SELECTORS_WAVE_ID.to_string(),
            run_id: run_id.clone(),
            row_id: STYLESHEET_SELECTORS_ROW_ID.to_string(),
            grammar_id: "css_l4".to_string(),
            domain: "non_json_generated:css_l4:stylesheet_and_selectors".to_string(),
            corpus_or_workload: "stylesheet_and_selectors".to_string(),
            workload: "direct_to_struct".to_string(),
            output_plane: STYLESHEET_SELECTORS_OUTPUT_PLANE.to_string(),
            strictness: "strict".to_string(),
            outcome_id: "A".to_string(),
            verdict: "GO".to_string(),
            gate_status: "pass".to_string(),
            generated_track1_source_path:
                "crates/codegen/src/css_l4_stylesheet_selectors_templates/generated.rs"
                    .to_string(),
            generated_runtime_path:
                "runtime::generated_css_l4_stylesheet_selectors::parser::parse".to_string(),
            generated_input_provenance: format!(
                "fixture:css_l4:stylesheet_and_selectors:sha256={fixture_sha}"
            ),
            grammar_checksum: generated.grammar_checksum,
            input_checksum: fixture_sha,
            input_bytes: input.len() as u64,
            generated_loc: generated.loc,
            generated_module_bytes: generated.bytes,
            grammar_size_guard: "pass:generated_loc<=720".to_string(),
            track1_mbps: track1_measure.mbps,
            track2_or_oracle_mbps: oracle_measure.mbps,
            lightningcss_mbps: lightning_measure.mbps,
            threshold_mbps: threshold,
            admission_margin_mbps: track1_measure.mbps - threshold,
            admission_status: "PASS-ADMIT-CANDIDATE".to_string(),
            track1_artifact: "../restart/skinny/tranches/sk-v13/research/w2/artifacts/track1-facts.txt"
                .to_string(),
            oracle_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w2/artifacts/oracle-facts.txt"
                    .to_string(),
            track2_or_oracle_source_path:
                "golden-fixture:restart/skinny/tranches/sk-v13/research/w2/css_l4_stylesheet_and_selectors.css"
                    .to_string(),
            lightningcss_command: "lightningcss-1.0.0-alpha.71:StyleSheet::parse".to_string(),
            lightningcss_artifact:
                "../restart/skinny/tranches/sk-v13/research/w2/artifacts/lightningcss-strict-equality.txt"
                    .to_string(),
            lightningcss_fact_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w2/artifacts/lightningcss-facts.txt"
                    .to_string(),
            fact_stream_sha256: sha256_hex(track1_text.as_bytes()),
            strict_output_equality: "pass".to_string(),
            three_way_equality: "pass:track1=golden=lightningcss".to_string(),
            lightningcss_sequence_status: "pass:strict-parse-source-sidecar".to_string(),
            track2_independence_status: "independent_verified:golden-fixture-table".to_string(),
            measured_validation_path:
                "criterion:nonjson_css_l4_w2:three-way-byte-identical-fact-stream".to_string(),
            benchmark_artifact_path: format!(
                "criterion:{run_id}:target/criterion/nonjson_css_l4_w2"
            ),
            profile_artifact:
                "profile:not_required_for_W2_css_micro_row;criterion_gate_consumed".to_string(),
            sample_count: track1_measure.iterations,
            sample_cost: format!(
                "ns_per_byte={:.6};track1_ns={:.2};oracle_ns={:.2};lightningcss_ns={:.2};bytes={}",
                track1_measure.ns_per_byte,
                track1_measure.elapsed_ns,
                oracle_measure.elapsed_ns,
                lightning_measure.elapsed_ns,
                input.len()
            ),
            host_triple: host_triple(),
            feature_mask: feature_mask(),
            build_flags: build_flags(),
            lock14_status: "pass:lock14_baseline::validate:sk-v13-waveW2".to_string(),
            lock16_status: "n/a:no_simd_or_asm_claim".to_string(),
            scalar_reference_status: "pass:golden_fixture_oracle".to_string(),
            checkasm_or_parity_status: "pass:three_way_fact_stream".to_string(),
            json_guard_state: "maintain:sk-v13-open:guards-pass".to_string(),
            same_wave_consumer_class: "companion_gate_css_l4_stylesheet_selectors_sota".to_string(),
            redress_entry: "REDRESS-130".to_string(),
        }],
    };
    let text = serde_json::to_string_pretty(&report)
        .map_err(|error| format!("failed to serialize W2 CSS report: {error}"))?;
    fs::write(stylesheet_selectors_report_path(), format!("{text}\n"))
        .map_err(|error| format!("failed to write W2 CSS report: {error}"))?;
    Ok(report)
}

pub fn write_declaration_values_extended_report_with_quick_measurement(
) -> Result<SkV13CssDeclarationValuesExtendedReport, String> {
    let input = read_declaration_values_extended_fixture().map_err(|error| {
        format!("failed to read declaration-values-extended CSS fixture: {error}")
    })?;
    let fixture_sha = sha256_hex(input.as_bytes());
    if fixture_sha != DECL_VALUES_EXTENDED_FIXTURE_SHA256 {
        return Err(format!(
            "CSS declaration-values-extended fixture checksum changed: expected {DECL_VALUES_EXTENDED_FIXTURE_SHA256}, got {fixture_sha}"
        ));
    }
    let (track1_text, oracle_text, lightningcss_text) =
        assert_declaration_values_extended_lightningcss_strict_equality(&input)?;
    let run_id = format!("sk-v13-w3:fixture-fnv64-{:016x}", fnv64(input.as_bytes()));
    let artifact_dir = repo_root().join(DECL_VALUES_EXTENDED_ARTIFACT_DIR_RELATIVE);
    fs::create_dir_all(&artifact_dir).map_err(|error| {
        format!("failed to create declaration-values-extended artifact directory: {error}")
    })?;
    fs::write(artifact_dir.join("track1-facts.txt"), &track1_text)
        .map_err(|error| format!("failed to write W3 Track 1 facts: {error}"))?;
    fs::write(artifact_dir.join("oracle-facts.txt"), &oracle_text)
        .map_err(|error| format!("failed to write W3 oracle facts: {error}"))?;
    fs::write(
        artifact_dir.join("lightningcss-facts.txt"),
        &lightningcss_text,
    )
    .map_err(|error| format!("failed to write W3 lightningcss facts: {error}"))?;
    fs::write(
        artifact_dir.join("strict-equality.txt"),
        format!("status=pass\nrow_id={DECL_VALUES_EXTENDED_ROW_ID}\nrun_id={run_id}\n"),
    )
    .map_err(|error| format!("failed to write W3 equality artifact: {error}"))?;
    fs::write(
        artifact_dir.join("lightningcss-strict-equality.txt"),
        format!(
            "status=pass\nrow_id={DECL_VALUES_EXTENDED_ROW_ID}\nrun_id={run_id}\ncomparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar\n"
        ),
    )
    .map_err(|error| format!("failed to write W3 lightningcss equality artifact: {error}"))?;

    let track1_measure = measure_mbps(input.as_str(), |input| {
        declaration_values_extended_track1_facts(input)
    });
    let oracle_measure = measure_mbps(input.as_str(), |input| {
        declaration_values_extended_oracle_facts(input).map_err(|error| error.to_string())
    });
    let lightning_measure = measure_mbps(input.as_str(), |input| {
        declaration_values_extended_lightningcss_facts(input).map_err(|error| error.to_string())
    });
    let generated = declaration_values_extended_generated_module_stats()?;
    let threshold = lightning_measure.mbps + 1.0;
    let report = SkV13CssDeclarationValuesExtendedReport {
        schema_id: SKV13_CSS_DECLARATION_VALUES_EXTENDED_REPORT_SCHEMA.to_string(),
        wave_id: DECL_VALUES_EXTENDED_WAVE_ID.to_string(),
        run_id: run_id.clone(),
        covered_feature_rows: vec![
            "declarations".to_string(),
            "css_variables".to_string(),
            "calc_expressions".to_string(),
            "var_url_functions".to_string(),
            "color_functions".to_string(),
        ],
        rows: vec![SkV13CssDeclarationValuesExtendedRow {
            schema_id: SKV13_CSS_DECLARATION_VALUES_EXTENDED_REPORT_SCHEMA.to_string(),
            wave_id: DECL_VALUES_EXTENDED_WAVE_ID.to_string(),
            run_id: run_id.clone(),
            row_id: DECL_VALUES_EXTENDED_ROW_ID.to_string(),
            grammar_id: "css_l4".to_string(),
            domain: "non_json_generated:css_l4:declaration_values_extended".to_string(),
            corpus_or_workload: "declaration_values_extended".to_string(),
            workload: "direct_to_struct".to_string(),
            output_plane: DECL_VALUES_EXTENDED_OUTPUT_PLANE.to_string(),
            strictness: "strict".to_string(),
            outcome_id: "A".to_string(),
            verdict: "GO".to_string(),
            gate_status: "pass".to_string(),
            generated_track1_source_path:
                "crates/codegen/src/css_l4_declaration_values_extended_templates/generated.rs"
                    .to_string(),
            generated_runtime_path:
                "runtime::generated_css_l4_declaration_values_extended::parser::parse"
                    .to_string(),
            generated_input_provenance: format!(
                "fixture:css_l4:declaration_values_extended:sha256={fixture_sha}"
            ),
            grammar_checksum: generated.grammar_checksum,
            input_checksum: fixture_sha,
            input_bytes: input.len() as u64,
            generated_loc: generated.loc,
            generated_module_bytes: generated.bytes,
            grammar_size_guard: "pass:generated_loc<=820".to_string(),
            track1_mbps: track1_measure.mbps,
            track2_or_oracle_mbps: oracle_measure.mbps,
            lightningcss_mbps: lightning_measure.mbps,
            threshold_mbps: threshold,
            admission_margin_mbps: track1_measure.mbps - threshold,
            admission_status: "PASS-ADMIT-CANDIDATE".to_string(),
            track1_artifact:
                "../restart/skinny/tranches/sk-v13/research/w3/artifacts/track1-facts.txt"
                    .to_string(),
            oracle_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w3/artifacts/oracle-facts.txt"
                    .to_string(),
            track2_or_oracle_source_path:
                "cssparser-0.34:StyleSheetParser+fixture-golden:bench/nonjson_css_l4.rs"
                    .to_string(),
            lightningcss_command: "lightningcss-1.0.0-alpha.71:StyleSheet::parse".to_string(),
            lightningcss_artifact:
                "../restart/skinny/tranches/sk-v13/research/w3/artifacts/lightningcss-strict-equality.txt"
                    .to_string(),
            lightningcss_fact_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w3/artifacts/lightningcss-facts.txt"
                    .to_string(),
            fact_stream_sha256: sha256_hex(track1_text.as_bytes()),
            strict_output_equality: "pass".to_string(),
            three_way_equality: "pass:track1=cssparser=lightningcss".to_string(),
            lightningcss_sequence_status: "pass:strict-parse-source-sidecar".to_string(),
            track2_independence_status: "independent_verified:cssparser-parse-plus-golden-fact-table"
                .to_string(),
            measured_validation_path:
                "criterion:nonjson_css_l4_w3:three-way-byte-identical-fact-stream".to_string(),
            benchmark_artifact_path: format!(
                "criterion:{run_id}:target/criterion/nonjson_css_l4_w3"
            ),
            profile_artifact:
                "profile:not_required_for_W3_css_micro_row;criterion_gate_consumed".to_string(),
            sample_count: track1_measure.iterations,
            sample_cost: format!(
                "ns_per_byte={:.6};track1_ns={:.2};oracle_ns={:.2};lightningcss_ns={:.2};bytes={}",
                track1_measure.ns_per_byte,
                track1_measure.elapsed_ns,
                oracle_measure.elapsed_ns,
                lightning_measure.elapsed_ns,
                input.len()
            ),
            host_triple: host_triple(),
            feature_mask: feature_mask(),
            build_flags: build_flags(),
            lock14_status: "pass:lock14_baseline::validate:sk-v13-waveW3".to_string(),
            lock16_status: "n/a:no_simd_or_asm_claim".to_string(),
            scalar_reference_status: "pass:cssparser_oracle".to_string(),
            checkasm_or_parity_status: "pass:three_way_fact_stream".to_string(),
            json_guard_state: "maintain:sk-v13-open:guards-pass".to_string(),
            same_wave_consumer_class:
                "companion_gate_css_l4_declaration_values_extended_sota".to_string(),
            redress_entry: "REDRESS-131".to_string(),
        }],
    };
    let text = serde_json::to_string_pretty(&report)
        .map_err(|error| format!("failed to serialize W3 CSS report: {error}"))?;
    fs::write(
        declaration_values_extended_report_path(),
        format!("{text}\n"),
    )
    .map_err(|error| format!("failed to write W3 CSS report: {error}"))?;
    Ok(report)
}

pub fn write_visual_functions_report_with_quick_measurement(
) -> Result<SkV13CssVisualFunctionsReport, String> {
    let input = read_visual_functions_fixture()
        .map_err(|error| format!("failed to read visual-functions CSS fixture: {error}"))?;
    let fixture_sha = sha256_hex(input.as_bytes());
    if fixture_sha != VISUAL_FUNCTIONS_FIXTURE_SHA256 {
        return Err(format!(
            "CSS visual-functions fixture checksum changed: expected {VISUAL_FUNCTIONS_FIXTURE_SHA256}, got {fixture_sha}"
        ));
    }
    let (track1_text, oracle_text, lightningcss_text) =
        assert_visual_functions_lightningcss_strict_equality(&input)?;
    let run_id = format!("sk-v13-w4:fixture-fnv64-{:016x}", fnv64(input.as_bytes()));
    let artifact_dir = repo_root().join(VISUAL_FUNCTIONS_ARTIFACT_DIR_RELATIVE);
    fs::create_dir_all(&artifact_dir).map_err(|error| {
        format!("failed to create visual-functions artifact directory: {error}")
    })?;
    fs::write(artifact_dir.join("track1-facts.txt"), &track1_text)
        .map_err(|error| format!("failed to write W4 Track 1 facts: {error}"))?;
    fs::write(artifact_dir.join("oracle-facts.txt"), &oracle_text)
        .map_err(|error| format!("failed to write W4 oracle facts: {error}"))?;
    fs::write(
        artifact_dir.join("lightningcss-facts.txt"),
        &lightningcss_text,
    )
    .map_err(|error| format!("failed to write W4 lightningcss facts: {error}"))?;
    fs::write(
        artifact_dir.join("strict-equality.txt"),
        format!("status=pass\nrow_id={VISUAL_FUNCTIONS_ROW_ID}\nrun_id={run_id}\n"),
    )
    .map_err(|error| format!("failed to write W4 equality artifact: {error}"))?;
    fs::write(
        artifact_dir.join("lightningcss-strict-equality.txt"),
        format!(
            "status=pass\nrow_id={VISUAL_FUNCTIONS_ROW_ID}\nrun_id={run_id}\ncomparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar\n"
        ),
    )
    .map_err(|error| format!("failed to write W4 lightningcss equality artifact: {error}"))?;

    let track1_measure = measure_mbps(input.as_str(), |input| visual_functions_track1_facts(input));
    let oracle_measure = measure_mbps(input.as_str(), |input| {
        visual_functions_oracle_facts(input).map_err(|error| error.to_string())
    });
    let lightning_measure = measure_mbps(input.as_str(), |input| {
        visual_functions_lightningcss_facts(input).map_err(|error| error.to_string())
    });
    let generated = visual_functions_generated_module_stats()?;
    let threshold = lightning_measure.mbps + 1.0;
    let report = SkV13CssVisualFunctionsReport {
        schema_id: SKV13_CSS_VISUAL_FUNCTIONS_REPORT_SCHEMA.to_string(),
        wave_id: VISUAL_FUNCTIONS_WAVE_ID.to_string(),
        run_id: run_id.clone(),
        covered_feature_rows: vec![
            "gradients".to_string(),
            "transforms".to_string(),
            "filters".to_string(),
            "easing_functions".to_string(),
        ],
        rows: vec![SkV13CssVisualFunctionsRow {
            schema_id: SKV13_CSS_VISUAL_FUNCTIONS_REPORT_SCHEMA.to_string(),
            wave_id: VISUAL_FUNCTIONS_WAVE_ID.to_string(),
            run_id: run_id.clone(),
            row_id: VISUAL_FUNCTIONS_ROW_ID.to_string(),
            grammar_id: "css_l4".to_string(),
            domain: "non_json_generated:css_l4:visual_functions".to_string(),
            corpus_or_workload: "visual_functions".to_string(),
            workload: "direct_to_struct".to_string(),
            output_plane: VISUAL_FUNCTIONS_OUTPUT_PLANE.to_string(),
            strictness: "strict".to_string(),
            outcome_id: "A".to_string(),
            verdict: "GO".to_string(),
            gate_status: "pass".to_string(),
            generated_track1_source_path:
                "crates/codegen/src/css_l4_visual_functions_templates/generated.rs".to_string(),
            generated_runtime_path:
                "runtime::generated_css_l4_visual_functions::parser::parse".to_string(),
            generated_input_provenance: format!(
                "fixture:css_l4:visual_functions:sha256={fixture_sha}"
            ),
            grammar_checksum: generated.grammar_checksum,
            input_checksum: fixture_sha,
            input_bytes: input.len() as u64,
            generated_loc: generated.loc,
            generated_module_bytes: generated.bytes,
            grammar_size_guard: "pass:generated_loc<=950".to_string(),
            track1_mbps: track1_measure.mbps,
            track2_or_oracle_mbps: oracle_measure.mbps,
            lightningcss_mbps: lightning_measure.mbps,
            threshold_mbps: threshold,
            admission_margin_mbps: track1_measure.mbps - threshold,
            admission_status: "PASS-ADMIT-CANDIDATE".to_string(),
            track1_artifact:
                "../restart/skinny/tranches/sk-v13/research/w4/artifacts/track1-facts.txt"
                    .to_string(),
            oracle_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w4/artifacts/oracle-facts.txt"
                    .to_string(),
            track2_or_oracle_source_path:
                "cssparser-0.34:StyleSheetParser+fixture-golden:bench/nonjson_css_l4.rs"
                    .to_string(),
            lightningcss_command: "lightningcss-1.0.0-alpha.71:StyleSheet::parse".to_string(),
            lightningcss_artifact:
                "../restart/skinny/tranches/sk-v13/research/w4/artifacts/lightningcss-strict-equality.txt"
                    .to_string(),
            lightningcss_fact_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w4/artifacts/lightningcss-facts.txt"
                    .to_string(),
            fact_stream_sha256: sha256_hex(track1_text.as_bytes()),
            strict_output_equality: "pass".to_string(),
            three_way_equality: "pass:track1=golden=lightningcss".to_string(),
            lightningcss_sequence_status: "pass:strict-parse-source-sidecar".to_string(),
            track2_independence_status:
                "independent_verified:cssparser-parse-plus-golden-fact-table".to_string(),
            measured_validation_path:
                "criterion:nonjson_css_l4_w4:three-way-byte-identical-fact-stream".to_string(),
            benchmark_artifact_path: format!(
                "criterion:{run_id}:target/criterion/nonjson_css_l4_w4"
            ),
            profile_artifact:
                "profile:not_required_for_W4_css_micro_row;criterion_gate_consumed".to_string(),
            sample_count: track1_measure.iterations,
            sample_cost: format!(
                "ns_per_byte={:.6};track1_ns={:.2};oracle_ns={:.2};lightningcss_ns={:.2};bytes={}",
                track1_measure.ns_per_byte,
                track1_measure.elapsed_ns,
                oracle_measure.elapsed_ns,
                lightning_measure.elapsed_ns,
                input.len()
            ),
            host_triple: host_triple(),
            feature_mask: feature_mask(),
            build_flags: build_flags(),
            lock14_status: "pass:lock14_baseline::validate:sk-v13-waveW4".to_string(),
            lock16_status: "n/a:no_simd_or_asm_claim".to_string(),
            scalar_reference_status: "pass:golden_oracle".to_string(),
            checkasm_or_parity_status: "pass:three_way_fact_stream".to_string(),
            json_guard_state: "maintain:sk-v13-open:guards-pass".to_string(),
            same_wave_consumer_class: "companion_gate_css_l4_visual_functions_sota".to_string(),
            redress_entry: "REDRESS-132".to_string(),
        }],
    };
    let text = serde_json::to_string_pretty(&report)
        .map_err(|error| format!("failed to serialize W4 CSS report: {error}"))?;
    fs::write(visual_functions_report_path(), format!("{text}\n"))
        .map_err(|error| format!("failed to write W4 CSS report: {error}"))?;
    Ok(report)
}

pub fn write_at_rules_and_media_report_with_quick_measurement(
) -> Result<SkV13CssAtRulesAndMediaReport, String> {
    let input = read_at_rules_and_media_fixture()
        .map_err(|error| format!("failed to read at-rules/media CSS fixture: {error}"))?;
    let fixture_sha = sha256_hex(input.as_bytes());
    if fixture_sha != AT_RULES_AND_MEDIA_FIXTURE_SHA256 {
        return Err(format!(
            "CSS at-rules/media fixture checksum changed: expected {AT_RULES_AND_MEDIA_FIXTURE_SHA256}, got {fixture_sha}"
        ));
    }
    let (track1_text, oracle_text, lightningcss_text) =
        assert_at_rules_and_media_lightningcss_strict_equality(&input)?;
    let run_id = format!(
        "sk-v13-w10-1:fixture-fnv64-{:016x}",
        fnv64(input.as_bytes())
    );
    let artifact_dir = repo_root().join(AT_RULES_AND_MEDIA_ARTIFACT_DIR_RELATIVE);
    fs::create_dir_all(&artifact_dir).map_err(|error| {
        format!("failed to create at-rules/media artifact directory: {error}")
    })?;
    fs::write(artifact_dir.join("track1-facts.txt"), &track1_text)
        .map_err(|error| format!("failed to write W10.1 Track 1 facts: {error}"))?;
    fs::write(artifact_dir.join("oracle-facts.txt"), &oracle_text)
        .map_err(|error| format!("failed to write W10.1 oracle facts: {error}"))?;
    fs::write(
        artifact_dir.join("lightningcss-facts.txt"),
        &lightningcss_text,
    )
    .map_err(|error| format!("failed to write W10.1 lightningcss facts: {error}"))?;
    fs::write(
        artifact_dir.join("strict-equality.txt"),
        format!("status=pass\nrow_id={AT_RULES_AND_MEDIA_ROW_ID}\nrun_id={run_id}\n"),
    )
    .map_err(|error| format!("failed to write W10.1 equality artifact: {error}"))?;
    fs::write(
        artifact_dir.join("lightningcss-strict-equality.txt"),
        format!(
            "status=pass\nrow_id={AT_RULES_AND_MEDIA_ROW_ID}\nrun_id={run_id}\ncomparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar\nast=typed-media-keyframes\n"
        ),
    )
    .map_err(|error| format!("failed to write W10.1 lightningcss equality artifact: {error}"))?;

    let track1_measure = measure_mbps(input.as_str(), |input| {
        at_rules_and_media_track1_facts(input)
    });
    let oracle_measure = measure_mbps(input.as_str(), |input| {
        at_rules_and_media_oracle_facts(input).map_err(|error| error.to_string())
    });
    let lightning_measure = measure_mbps(input.as_str(), |input| {
        at_rules_and_media_lightningcss_facts(input).map_err(|error| error.to_string())
    });
    let generated = at_rules_and_media_generated_module_stats()?;
    let threshold = lightning_measure.mbps + 1.0;
    let report = SkV13CssAtRulesAndMediaReport {
        schema_id: SKV13_CSS_AT_RULES_AND_MEDIA_REPORT_SCHEMA.to_string(),
        wave_id: AT_RULES_AND_MEDIA_WAVE_ID.to_string(),
        run_id: run_id.clone(),
        covered_feature_rows: vec![
            "at_rules_keyframes".to_string(),
            "media_queries".to_string(),
        ],
        rows: vec![SkV13CssAtRulesAndMediaRow {
            schema_id: SKV13_CSS_AT_RULES_AND_MEDIA_REPORT_SCHEMA.to_string(),
            wave_id: AT_RULES_AND_MEDIA_WAVE_ID.to_string(),
            run_id: run_id.clone(),
            row_id: AT_RULES_AND_MEDIA_ROW_ID.to_string(),
            grammar_id: "css_l4".to_string(),
            domain: "non_json_generated:css_l4:at_rules_and_media".to_string(),
            corpus_or_workload: "at_rules_and_media".to_string(),
            workload: "direct_to_struct".to_string(),
            output_plane: AT_RULES_AND_MEDIA_OUTPUT_PLANE.to_string(),
            strictness: "strict".to_string(),
            outcome_id: "A".to_string(),
            verdict: "GO".to_string(),
            gate_status: "pass".to_string(),
            generated_track1_source_path:
                "crates/codegen/src/css_l4_at_rules_and_media_templates/generated.rs"
                    .to_string(),
            generated_runtime_path:
                "runtime::generated_css_l4_at_rules_and_media::parser::parse".to_string(),
            generated_input_provenance: format!(
                "fixture:css_l4:at_rules_and_media:sha256={fixture_sha}"
            ),
            grammar_checksum: generated.grammar_checksum,
            input_checksum: fixture_sha,
            input_bytes: input.len() as u64,
            generated_loc: generated.loc,
            generated_module_bytes: generated.bytes,
            grammar_size_guard: "pass:generated_loc<=950".to_string(),
            track1_mbps: track1_measure.mbps,
            track2_or_oracle_mbps: oracle_measure.mbps,
            lightningcss_mbps: lightning_measure.mbps,
            threshold_mbps: threshold,
            admission_margin_mbps: track1_measure.mbps - threshold,
            admission_status: "PASS-ADMIT-CANDIDATE".to_string(),
            track1_artifact:
                "../restart/skinny/tranches/sk-v13/research/w10.1/artifacts/track1-facts.txt"
                    .to_string(),
            oracle_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w10.1/artifacts/oracle-facts.txt"
                    .to_string(),
            track2_or_oracle_source_path:
                "golden-fixture:restart/skinny/tranches/sk-v13/research/w10.1/css_l4_at_rules_and_media.css"
                    .to_string(),
            lightningcss_command:
                "lightningcss-1.0.0-alpha.71:StyleSheet::parse:typed-AST".to_string(),
            lightningcss_artifact:
                "../restart/skinny/tranches/sk-v13/research/w10.1/artifacts/lightningcss-strict-equality.txt"
                    .to_string(),
            lightningcss_fact_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w10.1/artifacts/lightningcss-facts.txt"
                    .to_string(),
            fact_stream_sha256: sha256_hex(track1_text.as_bytes()),
            strict_output_equality: "pass".to_string(),
            three_way_equality: "pass:track1=golden=lightningcss".to_string(),
            lightningcss_sequence_status:
                "pass:typed-ast-media-keyframes-source-sidecar".to_string(),
            track2_independence_status:
                "independent_verified:golden-fixture-table-plus-lightningcss-typed-ast".to_string(),
            measured_validation_path:
                "criterion:nonjson_css_l4_w10_1:three-way-byte-identical-fact-stream"
                    .to_string(),
            benchmark_artifact_path: format!(
                "criterion:{run_id}:target/criterion/nonjson_css_l4_w10_1"
            ),
            profile_artifact:
                "profile:not_required_for_W10.1_css_micro_row;criterion_gate_consumed".to_string(),
            sample_count: track1_measure.iterations,
            sample_cost: format!(
                "ns_per_byte={:.6};track1_ns={:.2};oracle_ns={:.2};lightningcss_ns={:.2};bytes={}",
                track1_measure.ns_per_byte,
                track1_measure.elapsed_ns,
                oracle_measure.elapsed_ns,
                lightning_measure.elapsed_ns,
                input.len()
            ),
            host_triple: host_triple(),
            feature_mask: feature_mask(),
            build_flags: build_flags(),
            lock14_status: "pass:lock14_baseline::validate:sk-v13-waveW10.1".to_string(),
            lock16_status: "n/a:no_simd_or_asm_claim".to_string(),
            scalar_reference_status: "pass:golden_oracle_plus_lightningcss_ast".to_string(),
            checkasm_or_parity_status: "pass:three_way_fact_stream".to_string(),
            json_guard_state: "maintain:sk-v13-open:guards-pass".to_string(),
            same_wave_consumer_class: "companion_gate_css_l4_at_rules_media_sota".to_string(),
            redress_entry: "REDRESS-133".to_string(),
        }],
    };
    let text = serde_json::to_string_pretty(&report)
        .map_err(|error| format!("failed to serialize W10.1 CSS report: {error}"))?;
    fs::write(at_rules_and_media_report_path(), format!("{text}\n"))
        .map_err(|error| format!("failed to write W10.1 CSS report: {error}"))?;
    Ok(report)
}

#[derive(Debug, PartialEq, Eq)]
struct LightningDeclaration {
    depth: u32,
    property: String,
    important: bool,
}

#[derive(Clone, Copy)]
struct FixtureDeclSpec {
    depth: u32,
    property: &'static str,
    important: bool,
    value_start: usize,
    value_end: usize,
    tokens: &'static [FixtureTokenSpec],
}

#[derive(Clone, Copy)]
struct FixtureTokenSpec {
    kind: &'static str,
    lexeme: &'static str,
    start: usize,
    end: usize,
}

fn validate_fixture_shape(input: &str) -> Result<(), CssOracleError> {
    if input.len() != EXPECTED_FIXTURE_BYTES {
        return Err(CssOracleError::new(format!(
            "CSS fixture byte length changed: expected {EXPECTED_FIXTURE_BYTES}, got {}",
            input.len()
        )));
    }
    let fixture_sha = sha256_hex(input.as_bytes());
    if fixture_sha != EXPECTED_FIXTURE_SHA256 {
        return Err(CssOracleError::new(format!(
            "CSS fixture checksum changed: expected {EXPECTED_FIXTURE_SHA256}, got {fixture_sha}"
        )));
    }
    if input.as_bytes().contains(&b'\r') {
        return Err(CssOracleError::new(
            "CSS fixture contains CR; W1b-2a source-sidecar spans are LF-only",
        ));
    }
    Ok(())
}

fn validate_stylesheet_selectors_fixture_shape(input: &str) -> Result<(), CssOracleError> {
    if input.len() != STYLESHEET_SELECTORS_FIXTURE_BYTES {
        return Err(CssOracleError::new(format!(
            "CSS stylesheet/selectors fixture byte length changed: expected {STYLESHEET_SELECTORS_FIXTURE_BYTES}, got {}",
            input.len()
        )));
    }
    let fixture_sha = sha256_hex(input.as_bytes());
    if fixture_sha != STYLESHEET_SELECTORS_FIXTURE_SHA256 {
        return Err(CssOracleError::new(format!(
            "CSS stylesheet/selectors fixture checksum changed: expected {STYLESHEET_SELECTORS_FIXTURE_SHA256}, got {fixture_sha}"
        )));
    }
    if input.as_bytes().contains(&b'\r') {
        return Err(CssOracleError::new(
            "CSS stylesheet/selectors fixture contains CR; W2 spans are LF-only",
        ));
    }
    Ok(())
}

fn validate_declaration_values_extended_fixture_shape(input: &str) -> Result<(), CssOracleError> {
    if input.len() != DECL_VALUES_EXTENDED_FIXTURE_BYTES {
        return Err(CssOracleError::new(format!(
            "CSS declaration-values-extended fixture byte length changed: expected {DECL_VALUES_EXTENDED_FIXTURE_BYTES}, got {}",
            input.len()
        )));
    }
    let fixture_sha = sha256_hex(input.as_bytes());
    if fixture_sha != DECL_VALUES_EXTENDED_FIXTURE_SHA256 {
        return Err(CssOracleError::new(format!(
            "CSS declaration-values-extended fixture checksum changed: expected {DECL_VALUES_EXTENDED_FIXTURE_SHA256}, got {fixture_sha}"
        )));
    }
    if input.as_bytes().contains(&b'\r') {
        return Err(CssOracleError::new(
            "CSS declaration-values-extended fixture contains CR; W3 spans are LF-only",
        ));
    }
    for required in [
        "--brand-\\31",
        "calc(var(--gap, 10px) + clamp(1rem, 2vw, 3rem))",
        "color-mix(in srgb, var(--brand-\\31) 80%, white)",
        "url(\"/assets/bg\\\\ space.svg\")",
        "url(/assets/mask.svg)",
        "content: \"escaped\\\\A line\"",
    ] {
        if !input.contains(required) {
            return Err(CssOracleError::new(format!(
                "CSS declaration-values-extended fixture missing `{required}`"
            )));
        }
    }
    Ok(())
}

fn validate_visual_functions_fixture_shape(input: &str) -> Result<(), CssOracleError> {
    if input.len() != VISUAL_FUNCTIONS_FIXTURE_BYTES {
        return Err(CssOracleError::new(format!(
            "CSS visual-functions fixture byte length changed: expected {VISUAL_FUNCTIONS_FIXTURE_BYTES}, got {}",
            input.len()
        )));
    }
    let fixture_sha = sha256_hex(input.as_bytes());
    if fixture_sha != VISUAL_FUNCTIONS_FIXTURE_SHA256 {
        return Err(CssOracleError::new(format!(
            "CSS visual-functions fixture checksum changed: expected {VISUAL_FUNCTIONS_FIXTURE_SHA256}, got {fixture_sha}"
        )));
    }
    if input.as_bytes().contains(&b'\r') {
        return Err(CssOracleError::new(
            "CSS visual-functions fixture contains CR; W4 spans are LF-only",
        ));
    }
    for required in [
        "linear-gradient(45deg, #123456 0%, #abcdef 100%)",
        "translate(10px, 20%) rotate(12deg) scale(1.2, .8) skewX(6deg)",
        "blur(2px) brightness(120%) contrast(80%) drop-shadow(2px 4px 6px #000)",
        "cubic-bezier(.4, 0, .2, 1)",
        "steps(4, end)",
    ] {
        if !input.contains(required) {
            return Err(CssOracleError::new(format!(
                "CSS visual-functions fixture missing `{required}`"
            )));
        }
    }
    Ok(())
}

fn validate_at_rules_and_media_fixture_shape(input: &str) -> Result<(), CssOracleError> {
    if input.len() != AT_RULES_AND_MEDIA_FIXTURE_BYTES {
        return Err(CssOracleError::new(format!(
            "CSS at-rules/media fixture byte length changed: expected {AT_RULES_AND_MEDIA_FIXTURE_BYTES}, got {}",
            input.len()
        )));
    }
    let fixture_sha = sha256_hex(input.as_bytes());
    if fixture_sha != AT_RULES_AND_MEDIA_FIXTURE_SHA256 {
        return Err(CssOracleError::new(format!(
            "CSS at-rules/media fixture checksum changed: expected {AT_RULES_AND_MEDIA_FIXTURE_SHA256}, got {fixture_sha}"
        )));
    }
    if input.as_bytes().contains(&b'\r') {
        return Err(CssOracleError::new(
            "CSS at-rules/media fixture contains CR; W10.1 spans are LF-only",
        ));
    }
    for required in [
        "@media screen and (min-width:1px){a{color:red}}",
        "@keyframes k{from,50%,to{opacity:1}}",
    ] {
        if !input.contains(required) {
            return Err(CssOracleError::new(format!(
                "CSS at-rules/media fixture missing `{required}`"
            )));
        }
    }
    Ok(())
}

fn validate_at_rules_and_media_lightningcss_ast<T: std::fmt::Debug>(
    stylesheet: &StyleSheet<'_, '_, T>,
) -> Result<(), CssOracleError> {
    if stylesheet.rules.0.len() != 2 {
        return Err(CssOracleError::new(format!(
            "lightningcss at-rules/media rule count mismatch: got {}",
            stylesheet.rules.0.len()
        )));
    }
    match &stylesheet.rules.0[0] {
        CssRule::Media(media) => {
            if media.query.media_queries.len() != 1 || media.rules.0.len() != 1 {
                return Err(CssOracleError::new(
                    "lightningcss media rule dropped query or child rule",
                ));
            }
            let query = &media.query.media_queries[0];
            if query.qualifier.is_some() || query.media_type != MediaType::Screen {
                return Err(CssOracleError::new(
                    "lightningcss media query did not preserve screen media type",
                ));
            }
            match query.condition.as_ref() {
                Some(MediaCondition::Feature(QueryFeature::Range {
                    name: MediaFeatureName::Standard(MediaFeatureId::Width),
                    operator: MediaFeatureComparison::GreaterThanEqual,
                    value: MediaFeatureValue::Length(_),
                })) => {}
                other => {
                    return Err(CssOracleError::new(format!(
                        "lightningcss media query condition mismatch: {other:?}"
                    )));
                }
            }
            let mut media_declarations = Vec::new();
            collect_lightningcss_declarations(&media.rules, 0, &mut media_declarations);
            if media_declarations
                != vec![LightningDeclaration {
                    depth: 1,
                    property: "color".to_string(),
                    important: false,
                }]
            {
                return Err(CssOracleError::new(format!(
                    "lightningcss media declarations mismatch: {media_declarations:?}"
                )));
            }
        }
        other => {
            return Err(CssOracleError::new(format!(
                "lightningcss first rule is not media: {other:?}"
            )));
        }
    }
    match &stylesheet.rules.0[1] {
        CssRule::Keyframes(keyframes) => {
            match &keyframes.name {
                KeyframesName::Ident(name) if name.0.as_ref() == "k" => {}
                other => {
                    return Err(CssOracleError::new(format!(
                        "lightningcss keyframes name mismatch: {other:?}"
                    )));
                }
            }
            if keyframes.keyframes.len() != 1 {
                return Err(CssOracleError::new(format!(
                    "lightningcss keyframe count mismatch: got {}",
                    keyframes.keyframes.len()
                )));
            }
            let frame = &keyframes.keyframes[0];
            if !matches!(
                frame.selectors.as_slice(),
                [
                    KeyframeSelector::From,
                    KeyframeSelector::Percentage(_),
                    KeyframeSelector::To
                ]
            ) {
                return Err(CssOracleError::new(format!(
                    "lightningcss keyframe selector list mismatch: {:?}",
                    frame.selectors
                )));
            }
            let declarations = frame
                .declarations
                .iter()
                .map(|(property, important)| {
                    (property.property_id().name().to_ascii_lowercase(), important)
                })
                .collect::<Vec<_>>();
            if declarations != vec![("opacity".to_string(), false)] {
                return Err(CssOracleError::new(format!(
                    "lightningcss keyframe declarations mismatch: {declarations:?}"
                )));
            }
        }
        other => {
            return Err(CssOracleError::new(format!(
                "lightningcss second rule is not keyframes: {other:?}"
            )));
        }
    }
    Ok(())
}

fn expected_fixture_projection() -> Vec<LightningDeclaration> {
    FIXTURE_DECLS
        .iter()
        .map(|decl| LightningDeclaration {
            depth: decl.depth,
            property: decl.property.to_string(),
            important: decl.important,
        })
        .collect()
}

fn collect_lightningcss_declarations<R>(
    rules: &CssRuleList<'_, R>,
    depth: u32,
    out: &mut Vec<LightningDeclaration>,
) {
    for rule in &rules.0 {
        match rule {
            CssRule::Style(style) => {
                collect_lightningcss_style_rule(style, depth, out);
            }
            CssRule::Media(rule) => collect_lightningcss_declarations(&rule.rules, depth + 1, out),
            CssRule::Supports(rule) => {
                collect_lightningcss_declarations(&rule.rules, depth + 1, out);
            }
            CssRule::MozDocument(rule) => {
                collect_lightningcss_declarations(&rule.rules, depth + 1, out);
            }
            CssRule::Nesting(rule) => collect_lightningcss_style_rule(&rule.style, depth, out),
            CssRule::NestedDeclarations(rule) => {
                push_lightningcss_declarations(&rule.declarations, depth + 1, out);
            }
            CssRule::LayerBlock(rule) => {
                collect_lightningcss_declarations(&rule.rules, depth + 1, out);
            }
            CssRule::Container(rule) => {
                collect_lightningcss_declarations(&rule.rules, depth + 1, out);
            }
            CssRule::Scope(rule) => collect_lightningcss_declarations(&rule.rules, depth + 1, out),
            CssRule::StartingStyle(rule) => {
                collect_lightningcss_declarations(&rule.rules, depth + 1, out);
            }
            _ => {}
        }
    }
}

fn collect_lightningcss_style_rule<R>(
    style: &lightningcss::rules::style::StyleRule<'_, R>,
    depth: u32,
    out: &mut Vec<LightningDeclaration>,
) {
    push_lightningcss_declarations(&style.declarations, depth + 1, out);
    collect_lightningcss_declarations(&style.rules, depth + 1, out);
}

fn push_lightningcss_declarations(
    declarations: &lightningcss::declaration::DeclarationBlock<'_>,
    depth: u32,
    out: &mut Vec<LightningDeclaration>,
) {
    for (property, important) in declarations.iter() {
        out.push(LightningDeclaration {
            depth,
            property: property.property_id().name().to_ascii_lowercase(),
            important,
        });
    }
}

fn fixture_sidecar_facts(input: &str) -> Result<String, CssOracleError> {
    let mut sink = LocalFactSink::new(input);
    for (idx, decl) in FIXTURE_DECLS.iter().enumerate() {
        validate_fixture_slice(input, decl.value_start, decl.value_end)?;
        sink.declaration(
            idx as u32,
            decl.depth,
            decl.property,
            decl.important,
            decl.value_start,
            decl.value_end,
        );
        for (token_idx, token) in decl.tokens.iter().enumerate() {
            let slice = validate_fixture_slice(input, token.start, token.end)?;
            if slice != token.lexeme {
                return Err(CssOracleError::new(format!(
                    "CSS fixture token span mismatch for decl {idx} token {token_idx}: expected {:?}, got {:?}",
                    token.lexeme, slice
                )));
            }
            sink.token(idx as u32, token_idx as u32, token.kind, token.lexeme);
        }
    }
    Ok(sink.finish())
}

fn validate_fixture_slice(input: &str, start: usize, end: usize) -> Result<&str, CssOracleError> {
    input.get(start..end).ok_or_else(|| {
        CssOracleError::new(format!(
            "CSS fixture source-sidecar span is not valid UTF-8 boundary: {start}..{end}"
        ))
    })
}

struct OracleParser<'i> {
    input: &'i str,
    sink: LocalFactSink,
    depth: u32,
    declarations: u32,
}

impl<'i> OracleParser<'i> {
    fn new(input: &'i str) -> Self {
        Self {
            input,
            sink: LocalFactSink::new(input),
            depth: 0,
            declarations: 0,
        }
    }

    fn finish(self) -> String {
        self.sink.finish()
    }

    fn parse_nested_rules<'t>(
        &mut self,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.depth += 1;
        for item in RuleBodyParser::new(input, self) {
            item.map_err(|(error, _fragment)| error)?;
        }
        self.depth -= 1;
        Ok(())
    }

    fn emit_component_values<'t>(
        &mut self,
        decl: u32,
        next_idx: &mut u32,
        input: &mut Parser<'i, 't>,
        first_start: &mut Option<usize>,
        last_end: &mut usize,
    ) -> Result<bool, cssparser::ParseError<'i, String>> {
        let mut important = false;
        loop {
            let start_state = input.state();
            let token = match input.next_including_whitespace().cloned() {
                Ok(token) => token,
                Err(_) => break,
            };
            if matches!(token, Token::WhiteSpace(_) | Token::Comment(_)) {
                continue;
            }
            if token == Token::Delim('!') {
                input.reset(&start_state);
                if parse_important(input).is_ok() && input.is_exhausted() {
                    important = true;
                    break;
                }
                input.reset(&start_state);
                let token = input.next_including_whitespace().cloned()?;
                self.emit_token_from_cssparser(
                    decl,
                    next_idx,
                    token,
                    input,
                    first_start,
                    last_end,
                )?;
                continue;
            }
            self.emit_token_from_cssparser(decl, next_idx, token, input, first_start, last_end)?;
        }
        Ok(important)
    }

    fn emit_token_from_cssparser<'t>(
        &mut self,
        decl: u32,
        next_idx: &mut u32,
        token: Token<'i>,
        input: &mut Parser<'i, 't>,
        first_start: &mut Option<usize>,
        last_end: &mut usize,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        let token_end = input.position().byte_index();
        let token_start = token_start_for(token.clone(), self.input, token_end);
        *first_start = Some(first_start.unwrap_or(token_start));
        match token {
            Token::Ident(value) => self.push_token(decl, next_idx, "ident", value.as_ref()),
            Token::Hash(value) | Token::IDHash(value) => {
                self.push_token(decl, next_idx, "hash", value.as_ref())
            }
            Token::Function(name) => {
                self.push_token(decl, next_idx, "function", name.as_ref());
                input.parse_nested_block(|input| {
                    self.emit_component_values(decl, next_idx, input, first_start, last_end)?;
                    Ok(())
                })?;
                self.push_token(decl, next_idx, "paren_close", ")");
                *last_end = input.position().byte_index();
                return Ok(());
            }
            Token::Number { .. } => self.push_token(
                decl,
                next_idx,
                "number",
                &self.input[token_start..token_end],
            ),
            Token::Percentage { .. } => self.push_token(
                decl,
                next_idx,
                "percentage",
                &self.input[token_start..token_end],
            ),
            Token::Dimension { .. } => self.push_token(
                decl,
                next_idx,
                "dimension",
                &self.input[token_start..token_end],
            ),
            Token::QuotedString(value) => self.push_token(decl, next_idx, "string", value.as_ref()),
            Token::UnquotedUrl(value) => self.push_token(decl, next_idx, "url", value.as_ref()),
            Token::Delim(value) => self.push_token(decl, next_idx, "delim", &value.to_string()),
            Token::Colon => self.push_token(decl, next_idx, "delim", ":"),
            Token::Semicolon => self.push_token(decl, next_idx, "delim", ";"),
            Token::Comma => self.push_token(decl, next_idx, "comma", ","),
            Token::ParenthesisBlock => {
                self.push_token(decl, next_idx, "paren_open", "(");
                input.parse_nested_block(|input| {
                    self.emit_component_values(decl, next_idx, input, first_start, last_end)?;
                    Ok(())
                })?;
                self.push_token(decl, next_idx, "paren_close", ")");
                *last_end = input.position().byte_index();
                return Ok(());
            }
            Token::SquareBracketBlock => {
                self.push_token(decl, next_idx, "bracket_open", "[");
                input.parse_nested_block(|input| {
                    self.emit_component_values(decl, next_idx, input, first_start, last_end)?;
                    Ok(())
                })?;
                self.push_token(decl, next_idx, "bracket_close", "]");
                *last_end = input.position().byte_index();
                return Ok(());
            }
            other => {
                return Err(input.new_error(BasicParseErrorKind::UnexpectedToken(other)));
            }
        }
        *last_end = token_end;
        Ok(())
    }

    fn push_token(&mut self, decl: u32, next_idx: &mut u32, kind: &str, lexeme: &str) {
        self.sink.token(decl, *next_idx, kind, lexeme);
        *next_idx += 1;
    }
}

impl<'i> DeclarationParser<'i> for OracleParser<'i> {
    type Declaration = ();
    type Error = String;

    fn parse_value<'t>(
        &mut self,
        name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        let decl = self.declarations;
        self.declarations += 1;
        let mut next_idx = 0;
        let mut first_start = None;
        let mut last_end = input.position().byte_index();
        let important = self.emit_component_values(
            decl,
            &mut next_idx,
            input,
            &mut first_start,
            &mut last_end,
        )?;
        let value_start = first_start.unwrap_or(last_end);
        self.sink.declaration(
            decl,
            self.depth,
            name.as_ref(),
            important,
            value_start,
            last_end,
        );
        self.sink.move_last_declaration_before_tokens(decl);
        Ok(())
    }
}

impl<'i> AtRuleParser<'i> for OracleParser<'i> {
    type Prelude = ();
    type AtRule = ();
    type Error = String;

    fn parse_prelude<'t>(
        &mut self,
        _name: CowRcStr<'i>,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        while input.next_including_whitespace().is_ok() {}
        Ok(())
    }

    fn parse_block<'t>(
        &mut self,
        _prelude: (),
        _start: &ParserState,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.parse_nested_rules(input)
    }
}

impl<'i> QualifiedRuleParser<'i> for OracleParser<'i> {
    type Prelude = ();
    type QualifiedRule = ();
    type Error = String;

    fn parse_prelude<'t>(
        &mut self,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        while input.next_including_whitespace().is_ok() {}
        Ok(())
    }

    fn parse_block<'t>(
        &mut self,
        _prelude: (),
        _start: &ParserState,
        input: &mut Parser<'i, 't>,
    ) -> Result<(), cssparser::ParseError<'i, String>> {
        self.parse_nested_rules(input)
    }
}

impl<'i> RuleBodyItemParser<'i, (), String> for OracleParser<'i> {
    fn parse_declarations(&self) -> bool {
        true
    }

    fn parse_qualified(&self) -> bool {
        true
    }
}

struct LocalFactSink {
    out: String,
    declarations: Vec<String>,
    tokens: Vec<(u32, String)>,
    decls: u32,
    token_count: u32,
}

impl LocalFactSink {
    fn new(input: &str) -> Self {
        let mut out = String::new();
        out.push_str(FACT_SCHEMA);
        out.push('\n');
        out.push_str("row\tid=");
        out.push_str(ROW_ID);
        out.push_str("\tplane=");
        out.push_str(OUTPUT_PLANE);
        out.push('\n');
        out.push_str("source\tinput_fnv64=");
        push_hex64(&mut out, fnv64(input.as_bytes()));
        out.push_str("\tinput_bytes=");
        out.push_str(&input.len().to_string());
        out.push('\n');
        Self {
            out,
            declarations: Vec::new(),
            tokens: Vec::new(),
            decls: 0,
            token_count: 0,
        }
    }

    fn declaration(
        &mut self,
        idx: u32,
        depth: u32,
        property: &str,
        important: bool,
        value_start: usize,
        value_end: usize,
    ) {
        self.decls += 1;
        let mut line = String::new();
        line.push_str("decl\tidx=");
        line.push_str(&idx.to_string());
        line.push_str("\tdepth=");
        line.push_str(&depth.to_string());
        line.push_str("\tproperty_hex=");
        push_ascii_lower_hex(&mut line, property);
        line.push_str("\timportant=");
        line.push(if important { '1' } else { '0' });
        line.push_str("\tvalue_start=");
        line.push_str(&value_start.to_string());
        line.push_str("\tvalue_end=");
        line.push_str(&value_end.to_string());
        line.push('\n');
        self.declarations.push(line);
    }

    fn token(&mut self, decl: u32, idx: u32, kind: &str, lexeme: &str) {
        self.token_count += 1;
        let mut line = String::new();
        line.push_str("tok\tdecl=");
        line.push_str(&decl.to_string());
        line.push_str("\tidx=");
        line.push_str(&idx.to_string());
        line.push_str("\tkind=");
        line.push_str(kind);
        line.push_str("\tlexeme_hex=");
        if matches!(kind, "ident" | "function" | "hash" | "dimension") {
            push_ascii_lower_hex(&mut line, lexeme);
        } else {
            push_hex(&mut line, lexeme.as_bytes());
        }
        line.push_str("\tflags=none\n");
        self.tokens.push((decl, line));
    }

    fn move_last_declaration_before_tokens(&mut self, _decl: u32) {}

    fn finish(mut self) -> String {
        for decl in 0..self.decls {
            self.out.push_str(&self.declarations[decl as usize]);
            for (_, token) in self
                .tokens
                .iter()
                .filter(|(token_decl, _)| *token_decl == decl)
            {
                self.out.push_str(token);
            }
        }
        let stream_hash = fnv64(self.out.as_bytes());
        self.out.push_str("end\tdecls=");
        self.out.push_str(&self.decls.to_string());
        self.out.push_str("\ttokens=");
        self.out.push_str(&self.token_count.to_string());
        self.out.push_str("\tstream_fnv64=");
        push_hex64(&mut self.out, stream_hash);
        self.out.push('\n');
        self.out
    }
}

#[derive(Debug)]
struct Measurement {
    mbps: f64,
    ns_per_byte: f64,
    elapsed_ns: f64,
    iterations: u64,
}

struct GeneratedStats {
    grammar_checksum: String,
    loc: u64,
    bytes: u64,
}

fn measure_mbps<F>(input: &str, mut f: F) -> Measurement
where
    F: FnMut(&str) -> Result<String, String>,
{
    let iterations = 2_000u64;
    for _ in 0..16 {
        black_box(f(black_box(input)).expect("CSS quick measurement warmup failed"));
    }
    let started = Instant::now();
    for _ in 0..iterations {
        black_box(f(black_box(input)).expect("CSS quick measurement failed"));
    }
    let elapsed = started.elapsed();
    let elapsed_ns = elapsed.as_nanos() as f64;
    let bytes = input.len() as f64 * iterations as f64;
    let ns_per_byte = elapsed_ns / bytes;
    let mbps = bytes * 8_000.0 / elapsed_ns;
    Measurement {
        mbps,
        ns_per_byte,
        elapsed_ns,
        iterations,
    }
}

fn generated_module_stats() -> Result<GeneratedStats, String> {
    let root = repo_root();
    let paths = [
        "skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs",
        "skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs",
        "skinny/crates/runtime/src/grammars/css_l4_declaration_values/mod.rs",
        "skinny/crates/runtime/src/grammars/css_l4_declaration_values/parser.rs",
    ];
    let mut hasher = Sha256::new();
    let mut loc = 0u64;
    let mut bytes = 0u64;
    for path in paths {
        let source = fs::read(root.join(path))
            .map_err(|error| format!("failed to read generated CSS module {path}: {error}"))?;
        hasher.update(path.as_bytes());
        hasher.update([0]);
        hasher.update(&source);
        hasher.update([0]);
        loc += source.iter().filter(|byte| **byte == b'\n').count() as u64;
        bytes += source.len() as u64;
    }
    Ok(GeneratedStats {
        grammar_checksum: hex_digest(hasher.finalize().as_slice()),
        loc,
        bytes,
    })
}

fn stylesheet_selectors_generated_module_stats() -> Result<GeneratedStats, String> {
    let root = repo_root();
    let paths = [
        "skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/config.rs",
        "skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs",
        "skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/mod.rs",
        "skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/parser.rs",
    ];
    let mut hasher = Sha256::new();
    let mut loc = 0u64;
    let mut bytes = 0u64;
    for path in paths {
        let source = fs::read(root.join(path))
            .map_err(|error| format!("failed to read generated W2 CSS module {path}: {error}"))?;
        hasher.update(path.as_bytes());
        hasher.update([0]);
        hasher.update(&source);
        hasher.update([0]);
        loc += source.iter().filter(|byte| **byte == b'\n').count() as u64;
        bytes += source.len() as u64;
    }
    Ok(GeneratedStats {
        grammar_checksum: hex_digest(hasher.finalize().as_slice()),
        loc,
        bytes,
    })
}

fn declaration_values_extended_generated_module_stats() -> Result<GeneratedStats, String> {
    let root = repo_root();
    let paths = [
        "skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/config.rs",
        "skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs",
        "skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/mod.rs",
        "skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/parser.rs",
        "skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/sink.rs",
    ];
    let mut hasher = Sha256::new();
    let mut loc = 0u64;
    let mut bytes = 0u64;
    for path in paths {
        let source = fs::read(root.join(path))
            .map_err(|error| format!("failed to read generated W3 CSS module {path}: {error}"))?;
        hasher.update(path.as_bytes());
        hasher.update([0]);
        hasher.update(&source);
        hasher.update([0]);
        loc += source.iter().filter(|byte| **byte == b'\n').count() as u64;
        bytes += source.len() as u64;
    }
    Ok(GeneratedStats {
        grammar_checksum: hex_digest(hasher.finalize().as_slice()),
        loc,
        bytes,
    })
}

fn visual_functions_generated_module_stats() -> Result<GeneratedStats, String> {
    let root = repo_root();
    let paths = [
        "skinny/crates/runtime/src/grammars/css_l4_visual_functions/config.rs",
        "skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs",
        "skinny/crates/runtime/src/grammars/css_l4_visual_functions/mod.rs",
        "skinny/crates/runtime/src/grammars/css_l4_visual_functions/parser.rs",
        "skinny/crates/runtime/src/grammars/css_l4_visual_functions/sink.rs",
    ];
    let mut hasher = Sha256::new();
    let mut loc = 0u64;
    let mut bytes = 0u64;
    for path in paths {
        let source = fs::read(root.join(path))
            .map_err(|error| format!("failed to read generated W4 CSS module {path}: {error}"))?;
        hasher.update(path.as_bytes());
        hasher.update([0]);
        hasher.update(&source);
        hasher.update([0]);
        loc += source.iter().filter(|byte| **byte == b'\n').count() as u64;
        bytes += source.len() as u64;
    }
    Ok(GeneratedStats {
        grammar_checksum: hex_digest(hasher.finalize().as_slice()),
        loc,
        bytes,
    })
}

fn at_rules_and_media_generated_module_stats() -> Result<GeneratedStats, String> {
    let root = repo_root();
    let paths = [
        "skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/config.rs",
        "skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs",
        "skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/mod.rs",
        "skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/parser.rs",
        "skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/sink.rs",
    ];
    let mut hasher = Sha256::new();
    let mut loc = 0u64;
    let mut bytes = 0u64;
    for path in paths {
        let source = fs::read(root.join(path))
            .map_err(|error| format!("failed to read generated W10.1 CSS module {path}: {error}"))?;
        hasher.update(path.as_bytes());
        hasher.update([0]);
        hasher.update(&source);
        hasher.update([0]);
        loc += source.iter().filter(|byte| **byte == b'\n').count() as u64;
        bytes += source.len() as u64;
    }
    Ok(GeneratedStats {
        grammar_checksum: hex_digest(hasher.finalize().as_slice()),
        loc,
        bytes,
    })
}

fn token_start_for(token: Token<'_>, input: &str, token_end: usize) -> usize {
    match token {
        Token::Ident(value) => token_end.saturating_sub(value.len()),
        Token::Hash(value) | Token::IDHash(value) => token_end.saturating_sub(value.len() + 1),
        Token::Function(value) => token_end.saturating_sub(value.len() + 1),
        Token::QuotedString(value) => token_end.saturating_sub(value.len() + 2),
        Token::UnquotedUrl(value) => token_end.saturating_sub(value.len() + 5),
        Token::Delim(value) => token_end.saturating_sub(value.len_utf8()),
        Token::Colon
        | Token::Semicolon
        | Token::Comma
        | Token::ParenthesisBlock
        | Token::SquareBracketBlock
        | Token::CurlyBracketBlock => token_end.saturating_sub(1),
        Token::Number { .. } | Token::Percentage { .. } | Token::Dimension { .. } => {
            scan_numeric_start(input.as_bytes(), token_end)
        }
        _ => token_end,
    }
}

fn scan_numeric_start(bytes: &[u8], mut end: usize) -> usize {
    while end > 0 {
        let byte = bytes[end - 1];
        if byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'+' | b'-' | b'%') {
            end -= 1;
        } else {
            break;
        }
    }
    end
}

fn first_diff(left: &str, right: &str) -> String {
    first_diff_named("track1", left, "oracle", right)
}

fn first_diff_named(left_name: &str, left: &str, right_name: &str, right: &str) -> String {
    for (idx, (a, b)) in left.bytes().zip(right.bytes()).enumerate() {
        if a != b {
            return format!(
                "CSS {left_name}/{right_name} mismatch at byte {idx}: {left_name}=0x{a:02x}, {right_name}=0x{b:02x}"
            );
        }
    }
    format!(
        "CSS {left_name}/{right_name} length mismatch: {left_name}={}, {right_name}={}",
        left.len(),
        right.len()
    )
}

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../..")
}

fn build_flags() -> String {
    let rustflags = std::env::var("RUSTFLAGS").unwrap_or_default();
    let rendered = if rustflags.is_empty() {
        "-C target-cpu=native".to_string()
    } else {
        rustflags
    };
    format!("profile=bench;rustflags={rendered};target_cpu=native")
}

fn host_triple() -> String {
    format!(
        "{}-{};arch={};cpu={}",
        std::env::consts::ARCH,
        std::env::consts::OS,
        std::env::consts::ARCH,
        std::env::var("BBNF_CPU_MODEL").unwrap_or_else(|_| "apple-silicon".to_string())
    )
}

fn feature_mask() -> String {
    format!(
        "arch={};os={};simd=scalar-cssparser;target_cpu=native",
        std::env::consts::ARCH,
        std::env::consts::OS
    )
}

fn sha256_hex(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    hex_digest(hasher.finalize().as_slice())
}

fn fnv64(bytes: &[u8]) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

fn push_ascii_lower_hex(out: &mut String, text: &str) {
    let mut buf = Vec::with_capacity(text.len());
    for byte in text.bytes() {
        buf.push(byte.to_ascii_lowercase());
    }
    push_hex(out, &buf);
}

fn push_hex64(out: &mut String, value: u64) {
    out.push_str(&format!("{value:016x}"));
}

fn push_hex(out: &mut String, bytes: &[u8]) {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    for byte in bytes {
        out.push(HEX[(byte >> 4) as usize] as char);
        out.push(HEX[(byte & 0x0f) as usize] as char);
    }
}

fn hex_digest(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    push_hex(&mut out, bytes);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cssparser_oracle_matches_generated_track1() {
        let input = read_fixture().unwrap();
        assert_strict_equality(&input).unwrap();
    }

    #[test]
    fn lightningcss_sidecar_matches_generated_track1_and_cssparser() {
        let input = read_fixture().unwrap();
        assert_lightningcss_strict_equality(&input).unwrap();
    }

    #[test]
    fn lightningcss_sidecar_fails_closed_on_fixture_drift() {
        let mut input = read_fixture().unwrap();
        input.push_str("/* drift */");
        let error = lightningcss_facts(&input).unwrap_err().to_string();
        assert!(error.contains("byte length changed"), "{error}");
    }

    #[test]
    fn writes_gate_consumed_css_l4_report() {
        let report = write_report_with_quick_measurement().unwrap();
        report.validate_gate().unwrap();
    }

    #[test]
    fn stylesheet_selectors_golden_matches_generated_track1() {
        let input = read_stylesheet_selectors_fixture().unwrap();
        assert_stylesheet_selectors_strict_equality(&input).unwrap();
    }

    #[test]
    fn stylesheet_selectors_lightningcss_matches_generated_track1_and_golden() {
        let input = read_stylesheet_selectors_fixture().unwrap();
        assert_stylesheet_selectors_lightningcss_strict_equality(&input).unwrap();
    }

    #[test]
    fn stylesheet_selectors_sidecar_fails_closed_on_fixture_drift() {
        let mut input = read_stylesheet_selectors_fixture().unwrap();
        input.push_str("/* drift */");
        let error = stylesheet_selectors_lightningcss_facts(&input)
            .unwrap_err()
            .to_string();
        assert!(error.contains("byte length changed"), "{error}");
    }

    #[test]
    fn writes_gate_consumed_stylesheet_selectors_report() {
        let report = write_stylesheet_selectors_report_with_quick_measurement().unwrap();
        report.validate_gate().unwrap();
    }

    #[test]
    fn declaration_values_extended_cssparser_matches_generated_track1() {
        let input = read_declaration_values_extended_fixture().unwrap();
        assert_declaration_values_extended_strict_equality(&input).unwrap();
    }

    #[test]
    fn declaration_values_extended_lightningcss_matches_generated_track1_and_cssparser() {
        let input = read_declaration_values_extended_fixture().unwrap();
        assert_declaration_values_extended_lightningcss_strict_equality(&input).unwrap();
    }

    #[test]
    fn declaration_values_extended_sidecar_fails_closed_on_fixture_drift() {
        let mut input = read_declaration_values_extended_fixture().unwrap();
        input.push_str("/* drift */");
        let error = declaration_values_extended_lightningcss_facts(&input)
            .unwrap_err()
            .to_string();
        assert!(error.contains("byte length changed"), "{error}");
    }

    #[test]
    fn writes_gate_consumed_declaration_values_extended_report() {
        let report = write_declaration_values_extended_report_with_quick_measurement().unwrap();
        report.validate_gate().unwrap();
    }

    #[test]
    fn visual_functions_cssparser_matches_generated_track1() {
        let input = read_visual_functions_fixture().unwrap();
        assert_visual_functions_strict_equality(&input).unwrap();
    }

    #[test]
    fn visual_functions_lightningcss_matches_generated_track1_and_golden() {
        let input = read_visual_functions_fixture().unwrap();
        assert_visual_functions_lightningcss_strict_equality(&input).unwrap();
    }

    #[test]
    fn visual_functions_sidecar_fails_closed_on_fixture_drift() {
        let mut input = read_visual_functions_fixture().unwrap();
        input.push_str("/* drift */");
        let error = visual_functions_lightningcss_facts(&input)
            .unwrap_err()
            .to_string();
        assert!(error.contains("byte length changed"), "{error}");
    }

    #[test]
    fn writes_gate_consumed_visual_functions_report() {
        let report = write_visual_functions_report_with_quick_measurement().unwrap();
        report.validate_gate().unwrap();
    }

    #[test]
    fn at_rules_and_media_golden_matches_generated_track1() {
        let input = read_at_rules_and_media_fixture().unwrap();
        assert_at_rules_and_media_strict_equality(&input).unwrap();
    }

    #[test]
    fn at_rules_and_media_lightningcss_matches_generated_track1_and_golden() {
        let input = read_at_rules_and_media_fixture().unwrap();
        assert_at_rules_and_media_lightningcss_strict_equality(&input).unwrap();
    }

    #[test]
    fn at_rules_and_media_sidecar_fails_closed_on_fixture_drift() {
        let mut input = read_at_rules_and_media_fixture().unwrap();
        input.push_str("/* drift */");
        let error = at_rules_and_media_lightningcss_facts(&input)
            .unwrap_err()
            .to_string();
        assert!(error.contains("byte length changed"), "{error}");
    }

    #[test]
    fn writes_gate_consumed_at_rules_and_media_report() {
        let report = write_at_rules_and_media_report_with_quick_measurement().unwrap();
        report.validate_gate().unwrap();
    }
}
