//! Type definitions, caches, and default parsers for code generation.

use crate::analysis::FirstSets;
use crate::types::*;

use crate::analysis::Dependencies;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};
use syn::Type;

#[derive(Clone, Debug, Default)]
pub struct ParserAttributes {
    pub paths: Vec<std::path::PathBuf>,
    pub ignore_whitespace: bool,
    pub debug: bool,
    pub use_string: bool,
    pub remove_left_recursion: bool,
}

pub struct GeneratedNonterminalParser {
    pub name: String,
    pub ty: String,
    pub parser: String,
}

impl GeneratedNonterminalParser {
    pub fn new(name: impl Into<String>, ty: impl Into<String>, parser: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ty: ty.into(),
            parser: parser.into(),
        }
    }
}

pub struct GeneratedGrammarAttributes<'a> {
    pub ast: &'a AST<'a>,

    pub deps: &'a Dependencies<'a>,
    pub non_acyclic_deps: &'a Dependencies<'a>,
    pub acyclic_deps: &'a Dependencies<'a>,

    pub first_sets: Option<&'a FirstSets<'a>>,

    pub ref_counts: Option<&'a HashMap<&'a Expression<'a>, usize>>,
    pub aliases: Option<&'a HashMap<&'a Expression<'a>, &'a Expression<'a>>>,

    /// Rules whose enum variant is elided — their parser returns the inner
    /// variant directly instead of wrapping in `Enum::rule_name(Box<...>)`.
    pub transparent_rules: Option<&'a HashSet<String>>,

    /// Rules whose body can be expressed entirely as a `SpanParser` (no
    /// recursion, no heterogeneous output). Dual methods are generated:
    /// `rule_sp() -> SpanParser` and `rule() -> Parser<Enum>`.
    pub span_eligible_rules: Option<&'a HashSet<String>>,

    /// Subset of span-eligible rules that actually have `_sp()` methods generated.
    /// Only these can be referenced as `Self::rule_sp().into_parser()` in codegen.
    pub sp_method_rules: Option<&'a HashSet<String>>,

    pub ident: &'a syn::Ident,
    pub enum_ident: &'a syn::Ident,

    pub enum_type: &'a Type,
    pub boxed_enum_type: &'a Type,

    pub parser_container_attrs: &'a ParserAttributes,
}

pub static DEFAULT_PARSERS: std::sync::LazyLock<HashMap<&'static str, GeneratedNonterminalParser>> =
    std::sync::LazyLock::new(|| {
        let mut default_parsers = HashMap::new();
        let name = "LITERAL";
        default_parsers.insert(
            name,
            GeneratedNonterminalParser::new(name, "::parse_that::Span<'a>", "::parse_that::parse::string_span"),
        );
        let name = "REGEX";
        default_parsers.insert(
            name,
            GeneratedNonterminalParser::new(name, "::parse_that::Span<'a>", "::parse_that::parse::regex_span"),
        );
        let name = "NUMBER";
        default_parsers.insert(
            name,
            GeneratedNonterminalParser::new(
                name,
                "::parse_that::Span<'a>",
                "::parse_that::parsers::utils::number_span()",
            ),
        );
        let name = "DOUBLE_QUOTED_STRING";
        default_parsers.insert(
            name,
            GeneratedNonterminalParser::new(
                name,
                "::parse_that::Span<'a>",
                r##"::parse_that::parsers::utils::quoted_span(r#"""#)"##,
            ),
        );
        default_parsers
    });

pub type TypeCache<'a> = HashMap<&'a Expression<'a>, Type>;

pub type GeneratedParserCache<'a> = HashMap<&'a Expression<'a>, proc_macro2::TokenStream>;
pub type InlineCache<'a, 'b> = HashMap<&'a Expression<'a>, &'b Expression<'a>>;

pub struct CacheBundle<'a, 'b, 'c>
where
    'a: 'b,
    'a: 'c,
    'b: 'c,
{
    pub parser_cache: Rc<RefCell<GeneratedParserCache<'c>>>,
    pub type_cache: Rc<RefCell<TypeCache<'c>>>,
    pub inline_cache: Rc<RefCell<InlineCache<'a, 'b>>>,
}
