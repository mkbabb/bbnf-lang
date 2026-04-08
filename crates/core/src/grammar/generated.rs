//! AUTO-GENERATED from grammar/bbnf/bbnf.bbnf — do not edit manually.
//! Regenerate: scripts/bootstrap-bbnf.sh

use ::parse_that::*;

pub struct BbnfBootstrap;

#[allow(non_upper_case_globals)]
pub const GRAMMAR_BbnfBootstrap: [&'static str; 1usize] = [
    "// BBNF \u{2014} Better Backus-Naur Form\n// Self-hosted grammar definition.\n\n@import { value_expr, type_annotation } from \"expressions\" ;\n@import { type_name } from \"types\" ;\n\n// \u{2500}\u{2500}\u{2500} Terminals \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\nidentifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ ;\n\nliteral = \"\\\"\" , /(\\\\.|[^\"\\\\])*/  , \"\\\"\"\n        | \"\'\"  , /(\\\\.|[^\'\\\\])*/  , \"\'\"\n        | \"`\"  , /(\\\\.|[^`\\\\])*/  , \"`\" ;\n\nregex = \"/\" , /(\\\\.|[^\\/])+/ , \"/\" ;\n\nbig_comment = ( \"/*\" , /[^\\*]*/ , \"*/\" ) ?w ;\ncomment = ( \"//\" , /.*/ ) ?w ;\n\n// \u{2500}\u{2500}\u{2500} Expressions \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\nlhs = identifier ;\n\n// Grammar function call args: each arg is a single binary_factor\n// (alternation of binary_factors, no comma-concatenation).\n// This avoids ambiguity between call arg commas and concatenation commas.\ncall_arg = ( binary_factor ?w , \"|\" ? ) + ;\n\nterm = \"\u{3b5}\" | \"epsilon\"\n     | identifier , ( \"(\" , call_arg ?w , ( \",\" ?w , call_arg ?w ) * , \")\" ) ?\n     | literal\n     | regex\n     | \"@{\" , rhs ?w , \"}\"\n     | \"(\" , rhs ?w , \")\"\n     | \"[\" , rhs ?w , \"]\"\n     | \"{\" , rhs ?w , \"}\" ;\n\nmodifier = \"?w\" | \"?\" | \"*\" | \"+\" ;\nfactor = big_comment ? , term ?w , modifier ? , big_comment ? ;\n\n// Map syntax: factor -> value_expr : type\nmapped_factor = factor , ( \"->\" ?w , ( value_expr , type_annotation ? ) ) ? ;\n\nbinary_operators = \"<<\" | \">>\" | \"-\" ;\nbinary_factor = mapped_factor , ( binary_operators ?w , mapped_factor ) * ;\n\nconcatenation = ( binary_factor ?w , \",\" ? ) + ;\nalternation = ( concatenation ?w , \"|\" ? ) + ;\n\n// Closures at rule level: |params| rhs (grammar functions)\nclosure = \"|\" , identifier , ( \",\" ?w , identifier ) * , \"|\" ?w , rhs ;\nrhs = closure | alternation ;\n\n// \u{2500}\u{2500}\u{2500} Rules and Directives \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\nrule = lhs , \"=\" ?w , rhs ?w , ( \";\" | \".\" ) ;\n\nimport_path = \"\\\"\" , /(\\\\.|[^\"\\\\])*/ , \"\\\"\" ;\nimport_items = \"{\" ?w , ( identifier , ( \",\" ?w , identifier ) * ) ?w , \"}\" ;\nimport_directive = \"@import\" ?w , (\n      import_items ?w , \"from\" ?w , import_path\n    | import_path\n) ?w , ( \";\" | \".\" ) ? ;\n\nrecover_directive = \"@recover\" ?w , identifier ?w , rhs ?w , ( \";\" | \".\" ) ? ;\n\npretty_hint = identifier , ( \"(\" , /[^)]*/ , \")\" ) ? ;\npretty_directive = \"@pretty\" ?w , ( \"*\" | identifier ) ?w , (pretty_hint+) ?w , ( \";\" | \".\" ) ? ;\n\nws_directive = \"@ws\" ?w , regex ?w , ( \";\" | \".\" ) ? ;\ntoken_directive = \"@token\" ?w , identifier ?w , ( \";\" | \".\" ) ? ;\ndebug_directive = \"@debug\" ?w , ( \"*\" | identifier ) ?w , ( \";\" | \".\" ) ? ;\nhost_directive = \"@host\" ?w , identifier ?w , ( \":\" ?w , type_name ?w ) ? , ( \";\" | \".\" ) ? ;\n\ndirective = import_directive\n          | recover_directive\n          | pretty_directive\n          | ws_directive\n          | token_directive\n          | debug_directive\n          | host_directive ;\n\n// Grammar: top-level items in any order.\ngrammar_item = comment | big_comment | directive | rule ;\ngrammar = ( grammar_item ?w ) * ;\n\n@pretty grammar block ;\n@pretty rule group ;\n@pretty alternation group ;\n",
];
#[derive(Debug)]
pub enum BbnfBootstrapEnum<'a> {
    type_name(::parse_that::Span<'a>),
    regex(::parse_that::Span<'a>),
    identifier(::parse_that::Span<'a>),
    literal(::parse_that::Span<'a>),
    big_comment(::parse_that::Span<'a>),
    modifier(::parse_that::Span<'a>),
    value_ident(::parse_that::Span<'a>),
    cmp_op(::parse_that::Span<'a>),
    mul_op(::parse_that::Span<'a>),
    int_lit(::parse_that::Span<'a>),
    float_lit(::parse_that::Span<'a>),
    bool_lit(::parse_that::Span<'a>),
    string_lit(::parse_that::Span<'a>),
    add_op(::parse_that::Span<'a>),
    binary_operators(::parse_that::Span<'a>),
    import_path(::parse_that::Span<'a>),
    comment(::parse_that::Span<'a>),
    type_annotation((::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)),
    ws_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    lhs(&'a BbnfBootstrapEnum<'a>),
    host_directive(
        (
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
            Option<(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)>,
            ::parse_that::Span<'a>,
        ),
    ),
    token_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    debug_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    pretty_hint((&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)),
    import_items(
        (
            ::parse_that::Span<'a>,
            (
                &'a BbnfBootstrapEnum<'a>,
                &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
            ),
            ::parse_that::Span<'a>,
        ),
    ),
    value_input(
        (
            ::parse_that::Span<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    value_path(
        (
            &'a BbnfBootstrapEnum<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    pretty_directive(
        (
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
            &'a [BbnfBootstrapEnum<'a>],
            ::parse_that::Span<'a>,
        ),
    ),
    import_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    value_mul(
        (
            &'a BbnfBootstrapEnum<'a>,
            &'a [(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    value_or(
        (
            &'a BbnfBootstrapEnum<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    value_add(
        (
            &'a BbnfBootstrapEnum<'a>,
            &'a [(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    value_cmp(
        (
            &'a BbnfBootstrapEnum<'a>,
            &'a [(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    value_and(
        (
            &'a BbnfBootstrapEnum<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    value_closure(
        (
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
        ),
    ),
    value_fn_call(
        (
            &'a BbnfBootstrapEnum<'a>,
            ::parse_that::Span<'a>,
            Option<
                (
                    &'a BbnfBootstrapEnum<'a>,
                    &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
                ),
            >,
            ::parse_that::Span<'a>,
        ),
    ),
    value_atom(&'a BbnfBootstrapEnum<'a>),
    value_unary(&'a BbnfBootstrapEnum<'a>),
    alternation(&'a [(&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)]),
    call_arg(&'a [(&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)]),
    concatenation(&'a [(&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)]),
    closure(
        (
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
        ),
    ),
    term(&'a BbnfBootstrapEnum<'a>),
    binary_factor(
        (
            &'a BbnfBootstrapEnum<'a>,
            &'a [(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    factor(
        (
            Option<&'a BbnfBootstrapEnum<'a>>,
            &'a BbnfBootstrapEnum<'a>,
            Option<&'a BbnfBootstrapEnum<'a>>,
            Option<&'a BbnfBootstrapEnum<'a>>,
        ),
    ),
    mapped_factor(
        (
            &'a BbnfBootstrapEnum<'a>,
            Option<
                (
                    ::parse_that::Span<'a>,
                    (&'a BbnfBootstrapEnum<'a>, Option<&'a BbnfBootstrapEnum<'a>>),
                ),
            >,
        ),
    ),
    recover_directive(
        (
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
            &'a BbnfBootstrapEnum<'a>,
            ::parse_that::Span<'a>,
        ),
    ),
    rule(
        (
            &'a BbnfBootstrapEnum<'a>,
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
            ::parse_that::Span<'a>,
        ),
    ),
    directive(&'a BbnfBootstrapEnum<'a>),
    grammar_item(&'a BbnfBootstrapEnum<'a>),
    grammar(&'a [BbnfBootstrapEnum<'a>]),
    debug_directive_0(::parse_that::Span<'a>),
    pretty_directive_0(::parse_that::Span<'a>),
    import_directive_0(
        (&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>),
    ),
    value_atom_0(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    value_unary_0((::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)),
    term_0(::parse_that::Span<'a>),
    term_1(
        (
            &'a BbnfBootstrapEnum<'a>,
            Option<
                (
                    ::parse_that::Span<'a>,
                    &'a BbnfBootstrapEnum<'a>,
                    &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
                    ::parse_that::Span<'a>,
                ),
            >,
        ),
    ),
    term_2((::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)),
    #[doc(hidden)]
    __Phantom(::core::marker::PhantomData<&'a ()>),
}
#[automatically_derived]
impl<'a> ::core::clone::Clone for BbnfBootstrapEnum<'a> {
    #[inline]
    fn clone(&self) -> BbnfBootstrapEnum<'a> {
        match self {
            BbnfBootstrapEnum::type_name(__self_0) => {
                BbnfBootstrapEnum::type_name(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::regex(__self_0) => {
                BbnfBootstrapEnum::regex(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::identifier(__self_0) => {
                BbnfBootstrapEnum::identifier(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::literal(__self_0) => {
                BbnfBootstrapEnum::literal(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::big_comment(__self_0) => {
                BbnfBootstrapEnum::big_comment(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::modifier(__self_0) => {
                BbnfBootstrapEnum::modifier(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_ident(__self_0) => {
                BbnfBootstrapEnum::value_ident(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::cmp_op(__self_0) => {
                BbnfBootstrapEnum::cmp_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::mul_op(__self_0) => {
                BbnfBootstrapEnum::mul_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::int_lit(__self_0) => {
                BbnfBootstrapEnum::int_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::float_lit(__self_0) => {
                BbnfBootstrapEnum::float_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::bool_lit(__self_0) => {
                BbnfBootstrapEnum::bool_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::string_lit(__self_0) => {
                BbnfBootstrapEnum::string_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::add_op(__self_0) => {
                BbnfBootstrapEnum::add_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::binary_operators(__self_0) => {
                BbnfBootstrapEnum::binary_operators(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::import_path(__self_0) => {
                BbnfBootstrapEnum::import_path(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::comment(__self_0) => {
                BbnfBootstrapEnum::comment(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::type_annotation(__self_0) => {
                BbnfBootstrapEnum::type_annotation(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::ws_directive(__self_0) => {
                BbnfBootstrapEnum::ws_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::lhs(__self_0) => {
                BbnfBootstrapEnum::lhs(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::host_directive(__self_0) => {
                BbnfBootstrapEnum::host_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::token_directive(__self_0) => {
                BbnfBootstrapEnum::token_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::debug_directive(__self_0) => {
                BbnfBootstrapEnum::debug_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::pretty_hint(__self_0) => {
                BbnfBootstrapEnum::pretty_hint(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::import_items(__self_0) => {
                BbnfBootstrapEnum::import_items(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_input(__self_0) => {
                BbnfBootstrapEnum::value_input(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_path(__self_0) => {
                BbnfBootstrapEnum::value_path(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::pretty_directive(__self_0) => {
                BbnfBootstrapEnum::pretty_directive(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::import_directive(__self_0) => {
                BbnfBootstrapEnum::import_directive(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::value_mul(__self_0) => {
                BbnfBootstrapEnum::value_mul(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_or(__self_0) => {
                BbnfBootstrapEnum::value_or(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_add(__self_0) => {
                BbnfBootstrapEnum::value_add(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_cmp(__self_0) => {
                BbnfBootstrapEnum::value_cmp(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_and(__self_0) => {
                BbnfBootstrapEnum::value_and(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_closure(__self_0) => {
                BbnfBootstrapEnum::value_closure(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_fn_call(__self_0) => {
                BbnfBootstrapEnum::value_fn_call(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_atom(__self_0) => {
                BbnfBootstrapEnum::value_atom(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_unary(__self_0) => {
                BbnfBootstrapEnum::value_unary(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::alternation(__self_0) => {
                BbnfBootstrapEnum::alternation(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::call_arg(__self_0) => {
                BbnfBootstrapEnum::call_arg(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::concatenation(__self_0) => {
                BbnfBootstrapEnum::concatenation(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::closure(__self_0) => {
                BbnfBootstrapEnum::closure(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::term(__self_0) => {
                BbnfBootstrapEnum::term(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::binary_factor(__self_0) => {
                BbnfBootstrapEnum::binary_factor(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::factor(__self_0) => {
                BbnfBootstrapEnum::factor(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::mapped_factor(__self_0) => {
                BbnfBootstrapEnum::mapped_factor(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::recover_directive(__self_0) => {
                BbnfBootstrapEnum::recover_directive(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::rule(__self_0) => {
                BbnfBootstrapEnum::rule(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::directive(__self_0) => {
                BbnfBootstrapEnum::directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::grammar_item(__self_0) => {
                BbnfBootstrapEnum::grammar_item(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::grammar(__self_0) => {
                BbnfBootstrapEnum::grammar(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::debug_directive_0(__self_0) => {
                BbnfBootstrapEnum::debug_directive_0(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::pretty_directive_0(__self_0) => {
                BbnfBootstrapEnum::pretty_directive_0(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::import_directive_0(__self_0) => {
                BbnfBootstrapEnum::import_directive_0(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::value_atom_0(__self_0) => {
                BbnfBootstrapEnum::value_atom_0(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_unary_0(__self_0) => {
                BbnfBootstrapEnum::value_unary_0(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::term_0(__self_0) => {
                BbnfBootstrapEnum::term_0(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::term_1(__self_0) => {
                BbnfBootstrapEnum::term_1(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::term_2(__self_0) => {
                BbnfBootstrapEnum::term_2(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::__Phantom(__self_0) => {
                BbnfBootstrapEnum::__Phantom(::core::clone::Clone::clone(__self_0))
            }
        }
    }
}
#[allow(non_camel_case_types)]
pub(crate) struct __BbnfBootstrapEnumCtx<'a> {
    __slab: ::parse_that::BumpSlab,
    __s0: ::std::cell::UnsafeCell<
        Vec<(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)>,
    >,
    __s1: ::std::cell::UnsafeCell<Vec<BbnfBootstrapEnum<'a>>>,
    __s2: ::std::cell::UnsafeCell<
        Vec<(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)>,
    >,
    __s3: ::std::cell::UnsafeCell<
        Vec<(&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)>,
    >,
    __phantom: ::core::marker::PhantomData<&'a ()>,
}
#[allow(non_snake_case)]
impl<'a> __BbnfBootstrapEnumCtx<'a> {
    pub(crate) fn with_capacity(n: usize) -> Self {
        Self {
            __slab: ::parse_that::BumpSlab::with_capacity(n * 32),
            __s0: ::std::cell::UnsafeCell::new(Vec::with_capacity(64)),
            __s1: ::std::cell::UnsafeCell::new(Vec::with_capacity(64)),
            __s2: ::std::cell::UnsafeCell::new(Vec::with_capacity(64)),
            __s3: ::std::cell::UnsafeCell::new(Vec::with_capacity(64)),
            __phantom: ::core::marker::PhantomData,
        }
    }
    #[inline(always)]
    fn slab(&self) -> &::parse_that::BumpSlab {
        &self.__slab
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __s0(&self) -> &mut Vec<(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)> {
        unsafe { &mut *self.__s0.get() }
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __c0(
        &'a self,
        depth: usize,
    ) -> &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)] {
        let s = self.__s0();
        let slice = self.__slab.alloc_slice_clone(&s[depth..]);
        s.truncate(depth);
        slice
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __s1(&self) -> &mut Vec<BbnfBootstrapEnum<'a>> {
        unsafe { &mut *self.__s1.get() }
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __c1(&'a self, depth: usize) -> &'a [BbnfBootstrapEnum<'a>] {
        let s = self.__s1();
        let slice = self.__slab.alloc_slice_clone(&s[depth..]);
        s.truncate(depth);
        slice
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __s2(&self) -> &mut Vec<(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)> {
        unsafe { &mut *self.__s2.get() }
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __c2(
        &'a self,
        depth: usize,
    ) -> &'a [(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)] {
        let s = self.__s2();
        let slice = self.__slab.alloc_slice_clone(&s[depth..]);
        s.truncate(depth);
        slice
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __s3(&self) -> &mut Vec<(&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)> {
        unsafe { &mut *self.__s3.get() }
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __c3(
        &'a self,
        depth: usize,
    ) -> &'a [(&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)] {
        let s = self.__s3();
        let slice = self.__slab.alloc_slice_clone(&s[depth..]);
        s.truncate(depth);
        slice
    }
}
#[allow(non_snake_case)]
#[inline(always)]
fn __BbnfBootstrapEnum_alloc<'a>(
    state: &::parse_that::ParserState<'a>,
) -> &'a __BbnfBootstrapEnumCtx<'a> {
    if true {
        if !!state.context_ptr.is_null() {
            { panic!("slab parser requires parse_with_context()"); }
        }
    }
    unsafe { &*(state.context_ptr as *const __BbnfBootstrapEnumCtx<'a>) }
}
impl BbnfBootstrap {
    #[allow(non_snake_case)]
    fn __import_directive<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __sp_start = state.offset;
                {
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ws_inner = if state.src[state.offset..].starts_with("@import")
                    {
                        let __start = state.offset;
                        state.offset += 7usize;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        None
                    };
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __ws_inner
                }?;
                let __sp12 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                let __v13 = {
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ws_inner = (|| {
                        {
                            let __cp = state.offset;
                            let __result = ((|| {
                                let __v9 = {
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ws_inner = (|| {
                                        let __sp_start = state.offset;
                                        {
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ws_inner = if state.offset < state.src.len()
                                                && state.src.as_bytes()[state.offset] == 123u8
                                            {
                                                let __start = state.offset;
                                                state.offset += 1;
                                                Some(
                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                )
                                            } else {
                                                None
                                            };
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __ws_inner
                                        }?;
                                        let __sp6 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        let __v7 = {
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ws_inner = (|| {
                                                let __v4 = ::parse_that::scan_ident(state)
                                                    .map(|__inner| {
                                                        let __v = BbnfBootstrapEnum::identifier(__inner);
                                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                    })?;
                                                let __v5 = {
                                                    let __depth2 = __BbnfBootstrapEnum_alloc(state)
                                                        .__s0()
                                                        .len();
                                                    loop {
                                                        let __prev3 = state.offset;
                                                        match (|| {
                                                            let __sp_start = state.offset;
                                                            {
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                let __ws_inner = if state.offset < state.src.len()
                                                                    && state.src.as_bytes()[state.offset] == 44u8
                                                                {
                                                                    let __start = state.offset;
                                                                    state.offset += 1;
                                                                    Some(
                                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                                    )
                                                                } else {
                                                                    None
                                                                };
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                __ws_inner
                                                            }?;
                                                            let __sp0 = ::parse_that::Span::new(
                                                                __sp_start,
                                                                state.offset,
                                                                state.src,
                                                            );
                                                            let __v1 = ::parse_that::scan_ident(state)
                                                                .map(|__inner| {
                                                                    let __v = BbnfBootstrapEnum::identifier(__inner);
                                                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                                })?;
                                                            Some((__sp0, __v1))
                                                        })() {
                                                            Some(__value) => {
                                                                __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                                                if state.offset == __prev3 {
                                                                    break;
                                                                }
                                                            }
                                                            None => {
                                                                state.offset = __prev3;
                                                                break;
                                                            }
                                                        }
                                                    }
                                                    Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth2))
                                                }?;
                                                Some((__v4, __v5))
                                            })();
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __ws_inner
                                        }?;
                                        let __sp_start = state.offset;
                                        if state.offset < state.src.len()
                                            && state.src.as_bytes()[state.offset] == 125u8
                                        {
                                            let __start = state.offset;
                                            state.offset += 1;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        }?;
                                        let __sp8 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        Some((__sp6, __v7, __sp8))
                                    })()
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::import_items(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        });
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __ws_inner
                                }?;
                                let __sp_start = state.offset;
                                {
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ws_inner = if state
                                        .src[state.offset..]
                                        .starts_with("from")
                                    {
                                        let __start = state.offset;
                                        state.offset += 4usize;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    };
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __ws_inner
                                }?;
                                let __sp10 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                let __v11 = (|| {
                                    let __sp_start = state.offset;
                                    if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 34u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    {
                                        let __start = state.offset;
                                        let __result: Option<()> = (|| {
                                            {
                                                let mut __rep_count: u32 = 0;
                                                loop {
                                                    let __save = state.offset;
                                                    let __ok = (|| -> Option<()> {
                                                        {
                                                            let __save_alt = state.offset;
                                                            let __alt_ok = (|| -> Option<()> {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                {
                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                    if !(!(__b == b'\n')) {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                }
                                                                Some(())
                                                            })();
                                                            let __alt_ok = if __alt_ok.is_none() {
                                                                state.offset = __save_alt;
                                                                (|| -> Option<()> {
                                                                    {
                                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                                        if !(!((__b == b'"' || __b == b'\\'))) {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                    }
                                                                    Some(())
                                                                })()
                                                            } else {
                                                                __alt_ok
                                                            };
                                                            if __alt_ok.is_none() {
                                                                return None;
                                                            }
                                                        }
                                                        Some(())
                                                    })();
                                                    if __ok.is_none() {
                                                        state.offset = __save;
                                                        break;
                                                    }
                                                    if state.offset == __save {
                                                        break;
                                                    }
                                                    __rep_count += 1;
                                                }
                                                if __rep_count < 0 {
                                                    return None;
                                                }
                                            }
                                            Some(())
                                        })();
                                        if __result.is_some() {
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            state.offset = __start;
                                            None
                                        }
                                    }?;
                                    if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 34u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    Some(
                                        ::parse_that::Span::new(__sp_start, state.offset, state.src),
                                    )
                                })()
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::import_path(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                Some((__v9, __sp10, __v11))
                            })())
                                .map(|__sv| {
                                    &*__BbnfBootstrapEnum_alloc(state)
                                        .slab()
                                        .alloc(BbnfBootstrapEnum::import_directive_0(__sv))
                                });
                            if __result.is_some() {
                                return __result;
                            }
                            state.offset = __cp;
                        }
                        {
                            let __cp = state.offset;
                            let __result = (|| {
                                let __sp_start = state.offset;
                                if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 34u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                }?;
                                {
                                    let __start = state.offset;
                                    let __result: Option<()> = (|| {
                                        {
                                            let mut __rep_count: u32 = 0;
                                            loop {
                                                let __save = state.offset;
                                                let __ok = (|| -> Option<()> {
                                                    {
                                                        let __save_alt = state.offset;
                                                        let __alt_ok = (|| -> Option<()> {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            {
                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                if !(!(__b == b'\n')) {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                            }
                                                            Some(())
                                                        })();
                                                        let __alt_ok = if __alt_ok.is_none() {
                                                            state.offset = __save_alt;
                                                            (|| -> Option<()> {
                                                                {
                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                    if !(!((__b == b'"' || __b == b'\\'))) {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                }
                                                                Some(())
                                                            })()
                                                        } else {
                                                            __alt_ok
                                                        };
                                                        if __alt_ok.is_none() {
                                                            return None;
                                                        }
                                                    }
                                                    Some(())
                                                })();
                                                if __ok.is_none() {
                                                    state.offset = __save;
                                                    break;
                                                }
                                                if state.offset == __save {
                                                    break;
                                                }
                                                __rep_count += 1;
                                            }
                                            if __rep_count < 0 {
                                                return None;
                                            }
                                        }
                                        Some(())
                                    })();
                                    if __result.is_some() {
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        state.offset = __start;
                                        None
                                    }
                                }?;
                                if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 34u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                }?;
                                Some(
                                    ::parse_that::Span::new(__sp_start, state.offset, state.src),
                                )
                            })()
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::import_path(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            if __result.is_some() {
                                return __result;
                            }
                            state.offset = __cp;
                        }
                        None
                    })();
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __ws_inner
                }?;
                let __sp_start = state.offset;
                {
                    let __cp = state.offset;
                    if (|| (|| {
                        let __r = if state.offset < state.src.len()
                            && state.src.as_bytes()[state.offset] == 59u8
                        {
                            let __start = state.offset;
                            state.offset += 1;
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            None
                        };
                        if __r.is_some() {
                            return __r;
                        }
                        let __r = if state.offset < state.src.len()
                            && state.src.as_bytes()[state.offset] == 46u8
                        {
                            let __start = state.offset;
                            state.offset += 1;
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            None
                        };
                        if __r.is_some() {
                            return __r;
                        }
                        None
                    })())()
                        .is_none()
                    {
                        state.offset = __cp;
                    }
                    Some(::parse_that::Span::new(__cp, state.offset, state.src))
                }?;
                let __sp14 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                Some((__sp12, __v13, __sp14))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::import_directive(__x))
    }
    pub fn import_directive<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__import_directive)
    }
    #[allow(non_snake_case)]
    fn __value_mul<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __v19 = Self::__value_unary(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __v20 = {
                    let __depth17 = __BbnfBootstrapEnum_alloc(state).__s2().len();
                    loop {
                        let __prev18 = state.offset;
                        match (|| {
                            let __v15 = {
                                ::parse_that::trim_leading_whitespace_mut(state);
                                let __ws_inner = (|| {
                                    {
                                        let __cp = state.offset;
                                        let __result = if state.offset < state.src.len()
                                            && state.src.as_bytes()[state.offset] == 42u8
                                        {
                                            let __start = state.offset;
                                            state.offset += 1;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    {
                                        let __cp = state.offset;
                                        let __result = if state.offset < state.src.len()
                                            && state.src.as_bytes()[state.offset] == 47u8
                                        {
                                            let __start = state.offset;
                                            state.offset += 1;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    {
                                        let __cp = state.offset;
                                        let __result = if state.offset < state.src.len()
                                            && state.src.as_bytes()[state.offset] == 37u8
                                        {
                                            let __start = state.offset;
                                            state.offset += 1;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    None
                                })()
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::mul_op(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    });
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __ws_inner
                            }?;
                            let __v16 = Self::__value_unary(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            Some((__v15, __v16))
                        })() {
                            Some(__value) => {
                                __BbnfBootstrapEnum_alloc(state).__s2().push(__value);
                                if state.offset == __prev18 {
                                    break;
                                }
                            }
                            None => {
                                state.offset = __prev18;
                                break;
                            }
                        }
                    }
                    Some(__BbnfBootstrapEnum_alloc(state).__c2(__depth17))
                }?;
                Some((__v19, __v20))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::value_mul(__x))
    }
    pub fn value_mul<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_mul)
    }
    #[allow(non_snake_case)]
    fn __value_add<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __v25 = Self::__value_mul(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __v26 = {
                    let __depth23 = __BbnfBootstrapEnum_alloc(state).__s2().len();
                    loop {
                        let __prev24 = state.offset;
                        match (|| {
                            let __v21 = {
                                ::parse_that::trim_leading_whitespace_mut(state);
                                let __ws_inner = (|| {
                                    {
                                        let __cp = state.offset;
                                        let __result = if state.offset < state.src.len()
                                            && state.src.as_bytes()[state.offset] == 43u8
                                        {
                                            let __start = state.offset;
                                            state.offset += 1;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    {
                                        let __cp = state.offset;
                                        let __result = if state.offset < state.src.len()
                                            && state.src.as_bytes()[state.offset] == 45u8
                                        {
                                            let __start = state.offset;
                                            state.offset += 1;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    None
                                })()
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::add_op(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    });
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __ws_inner
                            }?;
                            let __v22 = Self::__value_mul(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            Some((__v21, __v22))
                        })() {
                            Some(__value) => {
                                __BbnfBootstrapEnum_alloc(state).__s2().push(__value);
                                if state.offset == __prev24 {
                                    break;
                                }
                            }
                            None => {
                                state.offset = __prev24;
                                break;
                            }
                        }
                    }
                    Some(__BbnfBootstrapEnum_alloc(state).__c2(__depth23))
                }?;
                Some((__v25, __v26))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::value_add(__x))
    }
    pub fn value_add<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_add)
    }
    #[allow(non_snake_case)]
    fn __value_cmp<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __v31 = Self::__value_add(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __v32 = {
                    let __depth29 = __BbnfBootstrapEnum_alloc(state).__s2().len();
                    loop {
                        let __prev30 = state.offset;
                        match (|| {
                            let __v27 = {
                                ::parse_that::trim_leading_whitespace_mut(state);
                                let __ws_inner = (|| {
                                    {
                                        let __cp = state.offset;
                                        let __result = if state
                                            .src[state.offset..]
                                            .starts_with("==")
                                        {
                                            let __start = state.offset;
                                            state.offset += 2usize;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    {
                                        let __cp = state.offset;
                                        let __result = if state
                                            .src[state.offset..]
                                            .starts_with("!=")
                                        {
                                            let __start = state.offset;
                                            state.offset += 2usize;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    {
                                        let __cp = state.offset;
                                        let __result = if state
                                            .src[state.offset..]
                                            .starts_with("<=")
                                        {
                                            let __start = state.offset;
                                            state.offset += 2usize;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    {
                                        let __cp = state.offset;
                                        let __result = if state
                                            .src[state.offset..]
                                            .starts_with(">=")
                                        {
                                            let __start = state.offset;
                                            state.offset += 2usize;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    {
                                        let __cp = state.offset;
                                        let __result = if state.offset < state.src.len()
                                            && state.src.as_bytes()[state.offset] == 60u8
                                        {
                                            let __start = state.offset;
                                            state.offset += 1;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    {
                                        let __cp = state.offset;
                                        let __result = if state.offset < state.src.len()
                                            && state.src.as_bytes()[state.offset] == 62u8
                                        {
                                            let __start = state.offset;
                                            state.offset += 1;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    None
                                })()
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::cmp_op(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    });
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __ws_inner
                            }?;
                            let __v28 = Self::__value_add(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            Some((__v27, __v28))
                        })() {
                            Some(__value) => {
                                __BbnfBootstrapEnum_alloc(state).__s2().push(__value);
                                if state.offset == __prev30 {
                                    break;
                                }
                            }
                            None => {
                                state.offset = __prev30;
                                break;
                            }
                        }
                    }
                    Some(__BbnfBootstrapEnum_alloc(state).__c2(__depth29))
                }?;
                Some((__v31, __v32))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::value_cmp(__x))
    }
    pub fn value_cmp<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_cmp)
    }
    #[allow(non_snake_case)]
    fn __value_and<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __v37 = Self::__value_cmp(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __v38 = {
                    let __depth35 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                    loop {
                        let __prev36 = state.offset;
                        match (|| {
                            let __sp_start = state.offset;
                            {
                                ::parse_that::trim_leading_whitespace_mut(state);
                                let __ws_inner = if state
                                    .src[state.offset..]
                                    .starts_with("&&")
                                {
                                    let __start = state.offset;
                                    state.offset += 2usize;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __ws_inner
                            }?;
                            let __sp33 = ::parse_that::Span::new(
                                __sp_start,
                                state.offset,
                                state.src,
                            );
                            let __v34 = Self::__value_cmp(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            Some((__sp33, __v34))
                        })() {
                            Some(__value) => {
                                __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                if state.offset == __prev36 {
                                    break;
                                }
                            }
                            None => {
                                state.offset = __prev36;
                                break;
                            }
                        }
                    }
                    Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth35))
                }?;
                Some((__v37, __v38))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::value_and(__x))
    }
    pub fn value_and<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_and)
    }
    #[allow(non_snake_case)]
    fn __value_expr<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __cp = state.offset;
                let __result = (|| {
                    let __sp_start = state.offset;
                    if state.offset < state.src.len()
                        && state.src.as_bytes()[state.offset] == 124u8
                    {
                        let __start = state.offset;
                        state.offset += 1;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        None
                    }?;
                    let __sp43 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v44 = ::parse_that::scan_ident(state)
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::value_ident(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    let __v45 = {
                        let __depth41 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                        loop {
                            let __prev42 = state.offset;
                            match (|| {
                                let __sp_start = state.offset;
                                {
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ws_inner = if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 44u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    };
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __ws_inner
                                }?;
                                let __sp39 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                let __v40 = ::parse_that::scan_ident(state)
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::value_ident(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                Some((__sp39, __v40))
                            })() {
                                Some(__value) => {
                                    __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                    if state.offset == __prev42 {
                                        break;
                                    }
                                }
                                None => {
                                    state.offset = __prev42;
                                    break;
                                }
                            }
                        }
                        Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth41))
                    }?;
                    let __sp_start = state.offset;
                    if state.offset < state.src.len()
                        && state.src.as_bytes()[state.offset] == 124u8
                    {
                        let __start = state.offset;
                        state.offset += 1;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        None
                    }?;
                    let __sp46 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v47 = Self::__value_expr(state)
                        .map(|__v| {
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    Some((__sp43, __v44, __v45, __sp46, __v47))
                })()
                    .map(|__v| BbnfBootstrapEnum::value_closure(__v));
                if __result.is_some() {
                    return __result;
                }
                state.offset = __cp;
            }
            {
                let __cp = state.offset;
                let __result = (|| {
                    let __v52 = Self::__value_and(state)
                        .map(|__v| {
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    let __v53 = {
                        let __depth50 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                        loop {
                            let __prev51 = state.offset;
                            match (|| {
                                let __sp_start = state.offset;
                                {
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ws_inner = if state
                                        .src[state.offset..]
                                        .starts_with("||")
                                    {
                                        let __start = state.offset;
                                        state.offset += 2usize;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    };
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __ws_inner
                                }?;
                                let __sp48 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                let __v49 = Self::__value_and(state)
                                    .map(|__v| {
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                Some((__sp48, __v49))
                            })() {
                                Some(__value) => {
                                    __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                    if state.offset == __prev51 {
                                        break;
                                    }
                                }
                                None => {
                                    state.offset = __prev51;
                                    break;
                                }
                            }
                        }
                        Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth50))
                    }?;
                    Some((__v52, __v53))
                })()
                    .map(|__v| BbnfBootstrapEnum::value_or(__v));
                if __result.is_some() {
                    return __result;
                }
                state.offset = __cp;
            }
            None
        })()
    }
    pub fn value_expr<'a>() -> Parser<'a, &'a BbnfBootstrapEnum<'a>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let __v = Self::__value_expr(state)?;
            Some(&*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))
        })
    }
    #[inline(always)]
    pub fn value_expr_unboxed<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_expr)
    }
    #[allow(non_snake_case)]
    fn __value_atom<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                {
                    let __cp = state.offset;
                    let __result = {
                        let __start = state.offset;
                        let __result: Option<()> = (|| {
                            {
                                let __save_alt = state.offset;
                                let __alt_ok = (|| -> Option<()> {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'0')
                                    {
                                        return None;
                                    }
                                    state.offset += 1;
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'X' || __b == b'x')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    {
                                        let __loop_start = state.offset;
                                        let __end = state.src_bytes.len();
                                        let mut __pos = state.offset;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if __b.is_ascii_hexdigit() {
                                                __pos += 1;
                                            } else {
                                                break;
                                            }
                                        }
                                        if __pos < __loop_start + 1 as usize {
                                            return None;
                                        }
                                        state.offset = __pos;
                                    }
                                    {
                                        let __end = state.src_bytes.len();
                                        let mut __pos = state.offset;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if (__b.is_ascii_alphanumeric() || __b == b'_') {
                                                __pos += 1;
                                            } else {
                                                break;
                                            }
                                        }
                                        state.offset = __pos;
                                    }
                                    Some(())
                                })();
                                let __alt_ok = if __alt_ok.is_none() {
                                    state.offset = __save_alt;
                                    (|| -> Option<()> {
                                        {
                                            let __loop_start = state.offset;
                                            let __end = state.src_bytes.len();
                                            let mut __pos = state.offset;
                                            while __pos < __end {
                                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                if __b.is_ascii_digit() {
                                                    __pos += 1;
                                                } else {
                                                    break;
                                                }
                                            }
                                            if __pos < __loop_start + 1 as usize {
                                                return None;
                                            }
                                            state.offset = __pos;
                                        }
                                        {
                                            let __end = state.src_bytes.len();
                                            let mut __pos = state.offset;
                                            while __pos < __end {
                                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                if (__b.is_ascii_alphanumeric() || __b == b'_') {
                                                    __pos += 1;
                                                } else {
                                                    break;
                                                }
                                            }
                                            state.offset = __pos;
                                        }
                                        Some(())
                                    })()
                                } else {
                                    __alt_ok
                                };
                                if __alt_ok.is_none() {
                                    return None;
                                }
                            }
                            Some(())
                        })();
                        if __result.is_some() && state.offset > __start {
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            state.offset = __start;
                            None
                        }
                    }
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::int_lit(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = {
                        let __start = state.offset;
                        let __result: Option<()> = (|| {
                            {
                                let __end = state.src_bytes.len();
                                let mut __pos = state.offset;
                                while __pos < __end {
                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                    if __b.is_ascii_digit() {
                                        __pos += 1;
                                    } else {
                                        break;
                                    }
                                }
                                state.offset = __pos;
                            }
                            if state.src_bytes.get(state.offset).copied() != Some(b'.') {
                                return None;
                            }
                            state.offset += 1;
                            {
                                let __loop_start = state.offset;
                                let __end = state.src_bytes.len();
                                let mut __pos = state.offset;
                                while __pos < __end {
                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                    if __b.is_ascii_digit() {
                                        __pos += 1;
                                    } else {
                                        break;
                                    }
                                }
                                if __pos < __loop_start + 1 as usize {
                                    return None;
                                }
                                state.offset = __pos;
                            }
                            {
                                let __save = state.offset;
                                let __ok = (|| -> Option<()> {
                                    {
                                        let __b = *state.src_bytes.get(state.offset)?;
                                        if !((__b == b'E' || __b == b'e')) {
                                            return None;
                                        }
                                        state.offset += 1;
                                    }
                                    {
                                        let __save = state.offset;
                                        let __ok = (|| -> Option<()> {
                                            {
                                                let __b = *state.src_bytes.get(state.offset)?;
                                                if !((__b == b'+' || __b == b'-')) {
                                                    return None;
                                                }
                                                state.offset += 1;
                                            }
                                            Some(())
                                        })();
                                        if __ok.is_none() {
                                            state.offset = __save;
                                        }
                                    }
                                    {
                                        let __loop_start = state.offset;
                                        let __end = state.src_bytes.len();
                                        let mut __pos = state.offset;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if __b.is_ascii_digit() {
                                                __pos += 1;
                                            } else {
                                                break;
                                            }
                                        }
                                        if __pos < __loop_start + 1 as usize {
                                            return None;
                                        }
                                        state.offset = __pos;
                                    }
                                    Some(())
                                })();
                                if __ok.is_none() {
                                    state.offset = __save;
                                }
                            }
                            {
                                let __end = state.src_bytes.len();
                                let mut __pos = state.offset;
                                while __pos < __end {
                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                    if (__b.is_ascii_alphanumeric() || __b == b'_') {
                                        __pos += 1;
                                    } else {
                                        break;
                                    }
                                }
                                state.offset = __pos;
                            }
                            Some(())
                        })();
                        if __result.is_some() && state.offset > __start {
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            state.offset = __start;
                            None
                        }
                    }
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::float_lit(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = (|| {
                        {
                            let __cp = state.offset;
                            let __result = if state
                                .src[state.offset..]
                                .starts_with("true")
                            {
                                let __start = state.offset;
                                state.offset += 4usize;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            };
                            if __result.is_some() {
                                return __result;
                            }
                            state.offset = __cp;
                        }
                        {
                            let __cp = state.offset;
                            let __result = if state
                                .src[state.offset..]
                                .starts_with("false")
                            {
                                let __start = state.offset;
                                state.offset += 5usize;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            };
                            if __result.is_some() {
                                return __result;
                            }
                            state.offset = __cp;
                        }
                        None
                    })()
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::bool_lit(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = (|| {
                        let __sp_start = state.offset;
                        if state.offset < state.src.len()
                            && state.src.as_bytes()[state.offset] == 34u8
                        {
                            let __start = state.offset;
                            state.offset += 1;
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            None
                        }?;
                        {
                            let __start = state.offset;
                            let __result: Option<()> = (|| {
                                {
                                    let mut __rep_count: u32 = 0;
                                    loop {
                                        let __save = state.offset;
                                        let __ok = (|| -> Option<()> {
                                            {
                                                let __save_alt = state.offset;
                                                let __alt_ok = (|| -> Option<()> {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                    {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                    {
                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                        if !(!(__b == b'\n')) {
                                                            return None;
                                                        }
                                                        state.offset += 1;
                                                    }
                                                    Some(())
                                                })();
                                                let __alt_ok = if __alt_ok.is_none() {
                                                    state.offset = __save_alt;
                                                    (|| -> Option<()> {
                                                        {
                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                            if !(!((__b == b'"' || __b == b'\\'))) {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                        }
                                                        Some(())
                                                    })()
                                                } else {
                                                    __alt_ok
                                                };
                                                if __alt_ok.is_none() {
                                                    return None;
                                                }
                                            }
                                            Some(())
                                        })();
                                        if __ok.is_none() {
                                            state.offset = __save;
                                            break;
                                        }
                                        if state.offset == __save {
                                            break;
                                        }
                                        __rep_count += 1;
                                    }
                                    if __rep_count < 0 {
                                        return None;
                                    }
                                }
                                Some(())
                            })();
                            if __result.is_some() {
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                state.offset = __start;
                                None
                            }
                        }?;
                        if state.offset < state.src.len()
                            && state.src.as_bytes()[state.offset] == 34u8
                        {
                            let __start = state.offset;
                            state.offset += 1;
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            None
                        }?;
                        Some(
                            ::parse_that::Span::new(__sp_start, state.offset, state.src),
                        )
                    })()
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::string_lit(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = (|| {
                        let __v66 = (|| {
                            let __v58 = ::parse_that::scan_ident(state)
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::value_ident(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            let __v59 = {
                                let __depth56 = __BbnfBootstrapEnum_alloc(state)
                                    .__s0()
                                    .len();
                                loop {
                                    let __prev57 = state.offset;
                                    match (|| {
                                        let __sp_start = state.offset;
                                        if state.src[state.offset..].starts_with("::") {
                                            let __start = state.offset;
                                            state.offset += 2usize;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        }?;
                                        let __sp54 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        let __v55 = ::parse_that::scan_ident(state)
                                            .map(|__inner| {
                                                let __v = BbnfBootstrapEnum::value_ident(__inner);
                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                            })?;
                                        Some((__sp54, __v55))
                                    })() {
                                        Some(__value) => {
                                            __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                            if state.offset == __prev57 {
                                                break;
                                            }
                                        }
                                        None => {
                                            state.offset = __prev57;
                                            break;
                                        }
                                    }
                                }
                                Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth56))
                            }?;
                            Some((__v58, __v59))
                        })()
                            .map(|__inner| {
                                let __v = BbnfBootstrapEnum::value_path(__inner);
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        let __sp_start = state.offset;
                        if state.offset < state.src.len()
                            && state.src.as_bytes()[state.offset] == 40u8
                        {
                            let __start = state.offset;
                            state.offset += 1;
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            None
                        }?;
                        let __sp67 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v68 = {
                            let __cp = state.offset;
                            match (|| (|| {
                                let __v64 = Self::__value_expr(state)
                                    .map(|__v| {
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                let __v65 = {
                                    let __depth62 = __BbnfBootstrapEnum_alloc(state)
                                        .__s0()
                                        .len();
                                    loop {
                                        let __prev63 = state.offset;
                                        match (|| {
                                            let __sp_start = state.offset;
                                            {
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                let __ws_inner = if state.offset < state.src.len()
                                                    && state.src.as_bytes()[state.offset] == 44u8
                                                {
                                                    let __start = state.offset;
                                                    state.offset += 1;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                };
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __ws_inner
                                            }?;
                                            let __sp60 = ::parse_that::Span::new(
                                                __sp_start,
                                                state.offset,
                                                state.src,
                                            );
                                            let __v61 = Self::__value_expr(state)
                                                .map(|__v| {
                                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                })?;
                                            Some((__sp60, __v61))
                                        })() {
                                            Some(__value) => {
                                                __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                                if state.offset == __prev63 {
                                                    break;
                                                }
                                            }
                                            None => {
                                                state.offset = __prev63;
                                                break;
                                            }
                                        }
                                    }
                                    Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth62))
                                }?;
                                Some((__v64, __v65))
                            })())() {
                                Some(__v) => Some(Some(__v)),
                                None => {
                                    state.offset = __cp;
                                    Some(None)
                                }
                            }
                        }?;
                        let __sp_start = state.offset;
                        if state.offset < state.src.len()
                            && state.src.as_bytes()[state.offset] == 41u8
                        {
                            let __start = state.offset;
                            state.offset += 1;
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            None
                        }?;
                        let __sp69 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__v66, __sp67, __v68, __sp69))
                    })()
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::value_fn_call(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = (|| {
                        let __sp_start = state.offset;
                        if state.src[state.offset..].starts_with("input") {
                            let __start = state.offset;
                            state.offset += 5usize;
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            None
                        }?;
                        let __sp74 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v75 = {
                            let __depth72 = __BbnfBootstrapEnum_alloc(state)
                                .__s0()
                                .len();
                            loop {
                                let __prev73 = state.offset;
                                match (|| {
                                    let __sp_start = state.offset;
                                    if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 46u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    let __sp70 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v71 = ::parse_that::scan_ident(state)
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::value_ident(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        })?;
                                    Some((__sp70, __v71))
                                })() {
                                    Some(__value) => {
                                        __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                        if state.offset == __prev73 {
                                            break;
                                        }
                                    }
                                    None => {
                                        state.offset = __prev73;
                                        break;
                                    }
                                }
                            }
                            Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth72))
                        }?;
                        Some((__sp74, __v75))
                    })()
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::value_input(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = (|| {
                        let __v80 = ::parse_that::scan_ident(state)
                            .map(|__inner| {
                                let __v = BbnfBootstrapEnum::value_ident(__inner);
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        let __v81 = {
                            let __depth78 = __BbnfBootstrapEnum_alloc(state)
                                .__s0()
                                .len();
                            loop {
                                let __prev79 = state.offset;
                                match (|| {
                                    let __sp_start = state.offset;
                                    if state.src[state.offset..].starts_with("::") {
                                        let __start = state.offset;
                                        state.offset += 2usize;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    let __sp76 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v77 = ::parse_that::scan_ident(state)
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::value_ident(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        })?;
                                    Some((__sp76, __v77))
                                })() {
                                    Some(__value) => {
                                        __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                        if state.offset == __prev79 {
                                            break;
                                        }
                                    }
                                    None => {
                                        state.offset = __prev79;
                                        break;
                                    }
                                }
                            }
                            Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth78))
                        }?;
                        Some((__v80, __v81))
                    })()
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::value_path(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = ((|| {
                        let __sp_start = state.offset;
                        if state.offset < state.src.len()
                            && state.src.as_bytes()[state.offset] == 40u8
                        {
                            let __start = state.offset;
                            state.offset += 1;
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            None
                        }?;
                        let __sp82 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v83 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = Self::__value_expr(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp_start = state.offset;
                        if state.offset < state.src.len()
                            && state.src.as_bytes()[state.offset] == 41u8
                        {
                            let __start = state.offset;
                            state.offset += 1;
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            None
                        }?;
                        let __sp84 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp82, __v83, __sp84))
                    })())
                        .map(|__sv| {
                            &*__BbnfBootstrapEnum_alloc(state)
                                .slab()
                                .alloc(BbnfBootstrapEnum::value_atom_0(__sv))
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                None
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::value_atom(__x))
    }
    pub fn value_atom<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_atom)
    }
    #[allow(non_snake_case)]
    fn __value_unary<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                {
                    let __cp = state.offset;
                    let __result = ((|| {
                        let __sp_start = state.offset;
                        (|| {
                            let __r = if state.offset < state.src.len()
                                && state.src.as_bytes()[state.offset] == 33u8
                            {
                                let __start = state.offset;
                                state.offset += 1;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            };
                            if __r.is_some() {
                                return __r;
                            }
                            let __r = if state.offset < state.src.len()
                                && state.src.as_bytes()[state.offset] == 45u8
                            {
                                let __start = state.offset;
                                state.offset += 1;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            };
                            if __r.is_some() {
                                return __r;
                            }
                            None
                        })()?;
                        let __sp85 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v86 = Self::__value_atom(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__sp85, __v86))
                    })())
                        .map(|__sv| {
                            &*__BbnfBootstrapEnum_alloc(state)
                                .slab()
                                .alloc(BbnfBootstrapEnum::value_unary_0(__sv))
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = Self::__value_atom(state)
                        .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v));
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                None
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::value_unary(__x))
    }
    pub fn value_unary<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_unary)
    }
    #[allow(non_snake_case)]
    fn __alternation<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __depth93 = __BbnfBootstrapEnum_alloc(state).__s3().len();
                loop {
                    let __prev94 = state.offset;
                    match (|| {
                        let __v91 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = {
                                let __depth89 = __BbnfBootstrapEnum_alloc(state)
                                    .__s3()
                                    .len();
                                loop {
                                    let __prev90 = state.offset;
                                    match (|| {
                                        let __v87 = {
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ws_inner = Self::__binary_factor(state)
                                                .map(|__v| {
                                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                });
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __ws_inner
                                        }?;
                                        let __sp_start = state.offset;
                                        {
                                            let __cp = state.offset;
                                            if (|| {
                                                if state.offset < state.src.len()
                                                    && state.src.as_bytes()[state.offset] == 44u8
                                                {
                                                    let __start = state.offset;
                                                    state.offset += 1;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                }
                                            })()
                                                .is_none()
                                            {
                                                state.offset = __cp;
                                            }
                                            Some(::parse_that::Span::new(__cp, state.offset, state.src))
                                        }?;
                                        let __sp88 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        Some((__v87, __sp88))
                                    })() {
                                        Some(__value) => {
                                            __BbnfBootstrapEnum_alloc(state).__s3().push(__value);
                                            if state.offset == __prev90 {
                                                break;
                                            }
                                        }
                                        None => {
                                            state.offset = __prev90;
                                            break;
                                        }
                                    }
                                }
                                if (__BbnfBootstrapEnum_alloc(state).__s3().len()
                                    - __depth89) >= 1usize
                                {
                                    Some(__BbnfBootstrapEnum_alloc(state).__c3(__depth89))
                                } else {
                                    __BbnfBootstrapEnum_alloc(state).__s3().truncate(__depth89);
                                    None
                                }
                            }
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::concatenation(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp_start = state.offset;
                        {
                            let __cp = state.offset;
                            if (|| {
                                if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 124u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                }
                            })()
                                .is_none()
                            {
                                state.offset = __cp;
                            }
                            Some(::parse_that::Span::new(__cp, state.offset, state.src))
                        }?;
                        let __sp92 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__v91, __sp92))
                    })() {
                        Some(__value) => {
                            __BbnfBootstrapEnum_alloc(state).__s3().push(__value);
                            if state.offset == __prev94 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __prev94;
                            break;
                        }
                    }
                }
                if (__BbnfBootstrapEnum_alloc(state).__s3().len() - __depth93) >= 1usize
                {
                    Some(__BbnfBootstrapEnum_alloc(state).__c3(__depth93))
                } else {
                    __BbnfBootstrapEnum_alloc(state).__s3().truncate(__depth93);
                    None
                }
            }
        })()
            .map(|__x| BbnfBootstrapEnum::alternation(__x))
    }
    pub fn alternation<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__alternation)
    }
    #[allow(non_snake_case)]
    fn __call_arg<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __depth97 = __BbnfBootstrapEnum_alloc(state).__s3().len();
                loop {
                    let __prev98 = state.offset;
                    match (|| {
                        let __v95 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = Self::__binary_factor(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp_start = state.offset;
                        {
                            let __cp = state.offset;
                            if (|| {
                                if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 124u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                }
                            })()
                                .is_none()
                            {
                                state.offset = __cp;
                            }
                            Some(::parse_that::Span::new(__cp, state.offset, state.src))
                        }?;
                        let __sp96 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__v95, __sp96))
                    })() {
                        Some(__value) => {
                            __BbnfBootstrapEnum_alloc(state).__s3().push(__value);
                            if state.offset == __prev98 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __prev98;
                            break;
                        }
                    }
                }
                if (__BbnfBootstrapEnum_alloc(state).__s3().len() - __depth97) >= 1usize
                {
                    Some(__BbnfBootstrapEnum_alloc(state).__c3(__depth97))
                } else {
                    __BbnfBootstrapEnum_alloc(state).__s3().truncate(__depth97);
                    None
                }
            }
        })()
            .map(|__x| BbnfBootstrapEnum::call_arg(__x))
    }
    pub fn call_arg<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__call_arg)
    }
    #[allow(non_snake_case)]
    fn __binary_factor<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __v103 = Self::__mapped_factor(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __v104 = {
                    let __depth101 = __BbnfBootstrapEnum_alloc(state).__s2().len();
                    loop {
                        let __prev102 = state.offset;
                        match (|| {
                            let __v99 = {
                                ::parse_that::trim_leading_whitespace_mut(state);
                                let __ws_inner = (|| {
                                    {
                                        let __cp = state.offset;
                                        let __result = if state
                                            .src[state.offset..]
                                            .starts_with("<<")
                                        {
                                            let __start = state.offset;
                                            state.offset += 2usize;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    {
                                        let __cp = state.offset;
                                        let __result = if state
                                            .src[state.offset..]
                                            .starts_with(">>")
                                        {
                                            let __start = state.offset;
                                            state.offset += 2usize;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    {
                                        let __cp = state.offset;
                                        let __result = if state.offset < state.src.len()
                                            && state.src.as_bytes()[state.offset] == 45u8
                                        {
                                            let __start = state.offset;
                                            state.offset += 1;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        };
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    None
                                })()
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::binary_operators(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    });
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __ws_inner
                            }?;
                            let __v100 = Self::__mapped_factor(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            Some((__v99, __v100))
                        })() {
                            Some(__value) => {
                                __BbnfBootstrapEnum_alloc(state).__s2().push(__value);
                                if state.offset == __prev102 {
                                    break;
                                }
                            }
                            None => {
                                state.offset = __prev102;
                                break;
                            }
                        }
                    }
                    Some(__BbnfBootstrapEnum_alloc(state).__c2(__depth101))
                }?;
                Some((__v103, __v104))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::binary_factor(__x))
    }
    pub fn binary_factor<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__binary_factor)
    }
    #[allow(non_snake_case)]
    fn __rhs<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __cp = state.offset;
                let __result = (|| {
                    let __sp_start = state.offset;
                    if state.offset < state.src.len()
                        && state.src.as_bytes()[state.offset] == 124u8
                    {
                        let __start = state.offset;
                        state.offset += 1;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        None
                    }?;
                    let __sp109 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v110 = ::parse_that::scan_ident(state)
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::identifier(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    let __v111 = {
                        let __depth107 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                        loop {
                            let __prev108 = state.offset;
                            match (|| {
                                let __sp_start = state.offset;
                                {
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ws_inner = if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 44u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    };
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __ws_inner
                                }?;
                                let __sp105 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                let __v106 = ::parse_that::scan_ident(state)
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::identifier(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                Some((__sp105, __v106))
                            })() {
                                Some(__value) => {
                                    __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                    if state.offset == __prev108 {
                                        break;
                                    }
                                }
                                None => {
                                    state.offset = __prev108;
                                    break;
                                }
                            }
                        }
                        Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth107))
                    }?;
                    let __sp_start = state.offset;
                    {
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ws_inner = if state.offset < state.src.len()
                            && state.src.as_bytes()[state.offset] == 124u8
                        {
                            let __start = state.offset;
                            state.offset += 1;
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            None
                        };
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __ws_inner
                    }?;
                    let __sp112 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v113 = Self::__rhs(state)
                        .map(|__v| {
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    Some((__sp109, __v110, __v111, __sp112, __v113))
                })()
                    .map(|__v| BbnfBootstrapEnum::closure(__v));
                if __result.is_some() {
                    return __result;
                }
                state.offset = __cp;
            }
            {
                let __cp = state.offset;
                let __result = Self::__alternation(state);
                if __result.is_some() {
                    return __result;
                }
                state.offset = __cp;
            }
            None
        })()
    }
    pub fn rhs<'a>() -> Parser<'a, &'a BbnfBootstrapEnum<'a>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let __v = Self::__rhs(state)?;
            Some(&*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))
        })
    }
    #[inline(always)]
    pub fn rhs_unboxed<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__rhs)
    }
    #[allow(non_snake_case)]
    fn __mapped_factor<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __v147 = (|| {
                    let __v136 = {
                        let __cp = state.offset;
                        match (|| {
                            {
                                ::parse_that::trim_leading_whitespace_mut(state);
                                let __ws_inner = (|| {
                                    let __sp_start = state.offset;
                                    if state.src[state.offset..].starts_with("/*") {
                                        let __start = state.offset;
                                        state.offset += 2usize;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    {
                                        let __start = state.offset;
                                        let __scan = if __start >= state.src_bytes.len() {
                                            0
                                        } else {
                                            (::parse_that::memchr::memchr(
                                                b'*',
                                                &state.src_bytes[__start..],
                                            ))
                                                .unwrap_or(state.src_bytes.len() - __start)
                                        };
                                        state.offset = __start + __scan;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    }?;
                                    if state.src[state.offset..].starts_with("*/") {
                                        let __start = state.offset;
                                        state.offset += 2usize;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    Some(
                                        ::parse_that::Span::new(__sp_start, state.offset, state.src),
                                    )
                                })();
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __ws_inner
                            }
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::big_comment(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })
                        })() {
                            Some(__v) => Some(Some(__v)),
                            None => {
                                state.offset = __cp;
                                Some(None)
                            }
                        }
                    }?;
                    let __v137 = {
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ws_inner = (|| {
                            {
                                let __cp = state.offset;
                                let __result = (if state
                                    .src[state.offset..]
                                    .starts_with("ε")
                                {
                                    let __start = state.offset;
                                    state.offset += 2usize;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                })
                                    .map(|__sv| {
                                        &*__BbnfBootstrapEnum_alloc(state)
                                            .slab()
                                            .alloc(BbnfBootstrapEnum::debug_directive_0(__sv))
                                    });
                                if __result.is_some() {
                                    return __result;
                                }
                                state.offset = __cp;
                            }
                            {
                                let __cp = state.offset;
                                let __result = (if state
                                    .src[state.offset..]
                                    .starts_with("epsilon")
                                {
                                    let __start = state.offset;
                                    state.offset += 7usize;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                })
                                    .map(|__sv| {
                                        &*__BbnfBootstrapEnum_alloc(state)
                                            .slab()
                                            .alloc(BbnfBootstrapEnum::debug_directive_0(__sv))
                                    });
                                if __result.is_some() {
                                    return __result;
                                }
                                state.offset = __cp;
                            }
                            {
                                let __cp = state.offset;
                                let __result = ((|| {
                                    let __v122 = ::parse_that::scan_ident(state)
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::identifier(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        })?;
                                    let __v123 = {
                                        let __cp = state.offset;
                                        match (|| (|| {
                                            let __sp_start = state.offset;
                                            if state.offset < state.src.len()
                                                && state.src.as_bytes()[state.offset] == 40u8
                                            {
                                                let __start = state.offset;
                                                state.offset += 1;
                                                Some(
                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                )
                                            } else {
                                                None
                                            }?;
                                            let __sp118 = ::parse_that::Span::new(
                                                __sp_start,
                                                state.offset,
                                                state.src,
                                            );
                                            let __v119 = {
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                let __ws_inner = Self::__call_arg(state)
                                                    .map(|__v| {
                                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                    });
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __ws_inner
                                            }?;
                                            let __v120 = {
                                                let __depth116 = __BbnfBootstrapEnum_alloc(state)
                                                    .__s0()
                                                    .len();
                                                loop {
                                                    let __prev117 = state.offset;
                                                    match (|| {
                                                        let __sp_start = state.offset;
                                                        {
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            let __ws_inner = if state.offset < state.src.len()
                                                                && state.src.as_bytes()[state.offset] == 44u8
                                                            {
                                                                let __start = state.offset;
                                                                state.offset += 1;
                                                                Some(
                                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                                )
                                                            } else {
                                                                None
                                                            };
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            __ws_inner
                                                        }?;
                                                        let __sp114 = ::parse_that::Span::new(
                                                            __sp_start,
                                                            state.offset,
                                                            state.src,
                                                        );
                                                        let __v115 = {
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            let __ws_inner = Self::__call_arg(state)
                                                                .map(|__v| {
                                                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                                });
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            __ws_inner
                                                        }?;
                                                        Some((__sp114, __v115))
                                                    })() {
                                                        Some(__value) => {
                                                            __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                                            if state.offset == __prev117 {
                                                                break;
                                                            }
                                                        }
                                                        None => {
                                                            state.offset = __prev117;
                                                            break;
                                                        }
                                                    }
                                                }
                                                Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth116))
                                            }?;
                                            let __sp_start = state.offset;
                                            if state.offset < state.src.len()
                                                && state.src.as_bytes()[state.offset] == 41u8
                                            {
                                                let __start = state.offset;
                                                state.offset += 1;
                                                Some(
                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                )
                                            } else {
                                                None
                                            }?;
                                            let __sp121 = ::parse_that::Span::new(
                                                __sp_start,
                                                state.offset,
                                                state.src,
                                            );
                                            Some((__sp118, __v119, __v120, __sp121))
                                        })())() {
                                            Some(__v) => Some(Some(__v)),
                                            None => {
                                                state.offset = __cp;
                                                Some(None)
                                            }
                                        }
                                    }?;
                                    Some((__v122, __v123))
                                })())
                                    .map(|__sv| {
                                        &*__BbnfBootstrapEnum_alloc(state)
                                            .slab()
                                            .alloc(BbnfBootstrapEnum::term_1(__sv))
                                    });
                                if __result.is_some() {
                                    return __result;
                                }
                                state.offset = __cp;
                            }
                            {
                                let __cp = state.offset;
                                let __result = (|| {
                                    {
                                        let __cp = state.offset;
                                        let __result = (|| {
                                            let __sp_start = state.offset;
                                            if state.offset < state.src.len()
                                                && state.src.as_bytes()[state.offset] == 34u8
                                            {
                                                let __start = state.offset;
                                                state.offset += 1;
                                                Some(
                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                )
                                            } else {
                                                None
                                            }?;
                                            {
                                                let __start = state.offset;
                                                let __result: Option<()> = (|| {
                                                    {
                                                        let mut __rep_count: u32 = 0;
                                                        loop {
                                                            let __save = state.offset;
                                                            let __ok = (|| -> Option<()> {
                                                                {
                                                                    let __save_alt = state.offset;
                                                                    let __alt_ok = (|| -> Option<()> {
                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                                        {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                        {
                                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                                            if !(!(__b == b'\n')) {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                        }
                                                                        Some(())
                                                                    })();
                                                                    let __alt_ok = if __alt_ok.is_none() {
                                                                        state.offset = __save_alt;
                                                                        (|| -> Option<()> {
                                                                            {
                                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                                if !(!((__b == b'"' || __b == b'\\'))) {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                            }
                                                                            Some(())
                                                                        })()
                                                                    } else {
                                                                        __alt_ok
                                                                    };
                                                                    if __alt_ok.is_none() {
                                                                        return None;
                                                                    }
                                                                }
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save;
                                                                break;
                                                            }
                                                            if state.offset == __save {
                                                                break;
                                                            }
                                                            __rep_count += 1;
                                                        }
                                                        if __rep_count < 0 {
                                                            return None;
                                                        }
                                                    }
                                                    Some(())
                                                })();
                                                if __result.is_some() {
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    state.offset = __start;
                                                    None
                                                }
                                            }?;
                                            if state.offset < state.src.len()
                                                && state.src.as_bytes()[state.offset] == 34u8
                                            {
                                                let __start = state.offset;
                                                state.offset += 1;
                                                Some(
                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                )
                                            } else {
                                                None
                                            }?;
                                            Some(
                                                ::parse_that::Span::new(__sp_start, state.offset, state.src),
                                            )
                                        })();
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    {
                                        let __cp = state.offset;
                                        let __result = (|| {
                                            let __sp_start = state.offset;
                                            if state.offset < state.src.len()
                                                && state.src.as_bytes()[state.offset] == 39u8
                                            {
                                                let __start = state.offset;
                                                state.offset += 1;
                                                Some(
                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                )
                                            } else {
                                                None
                                            }?;
                                            {
                                                let __start = state.offset;
                                                let __result: Option<()> = (|| {
                                                    {
                                                        let mut __rep_count: u32 = 0;
                                                        loop {
                                                            let __save = state.offset;
                                                            let __ok = (|| -> Option<()> {
                                                                {
                                                                    let __save_alt = state.offset;
                                                                    let __alt_ok = (|| -> Option<()> {
                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                                        {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                        {
                                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                                            if !(!(__b == b'\n')) {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                        }
                                                                        Some(())
                                                                    })();
                                                                    let __alt_ok = if __alt_ok.is_none() {
                                                                        state.offset = __save_alt;
                                                                        (|| -> Option<()> {
                                                                            {
                                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                                if !(!((__b == b'\'' || __b == b'\\'))) {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                            }
                                                                            Some(())
                                                                        })()
                                                                    } else {
                                                                        __alt_ok
                                                                    };
                                                                    if __alt_ok.is_none() {
                                                                        return None;
                                                                    }
                                                                }
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save;
                                                                break;
                                                            }
                                                            if state.offset == __save {
                                                                break;
                                                            }
                                                            __rep_count += 1;
                                                        }
                                                        if __rep_count < 0 {
                                                            return None;
                                                        }
                                                    }
                                                    Some(())
                                                })();
                                                if __result.is_some() {
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    state.offset = __start;
                                                    None
                                                }
                                            }?;
                                            if state.offset < state.src.len()
                                                && state.src.as_bytes()[state.offset] == 39u8
                                            {
                                                let __start = state.offset;
                                                state.offset += 1;
                                                Some(
                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                )
                                            } else {
                                                None
                                            }?;
                                            Some(
                                                ::parse_that::Span::new(__sp_start, state.offset, state.src),
                                            )
                                        })();
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    {
                                        let __cp = state.offset;
                                        let __result = (|| {
                                            let __sp_start = state.offset;
                                            if state.offset < state.src.len()
                                                && state.src.as_bytes()[state.offset] == 96u8
                                            {
                                                let __start = state.offset;
                                                state.offset += 1;
                                                Some(
                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                )
                                            } else {
                                                None
                                            }?;
                                            {
                                                let __start = state.offset;
                                                let __result: Option<()> = (|| {
                                                    {
                                                        let mut __rep_count: u32 = 0;
                                                        loop {
                                                            let __save = state.offset;
                                                            let __ok = (|| -> Option<()> {
                                                                {
                                                                    let __save_alt = state.offset;
                                                                    let __alt_ok = (|| -> Option<()> {
                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                                        {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                        {
                                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                                            if !(!(__b == b'\n')) {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                        }
                                                                        Some(())
                                                                    })();
                                                                    let __alt_ok = if __alt_ok.is_none() {
                                                                        state.offset = __save_alt;
                                                                        (|| -> Option<()> {
                                                                            {
                                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                                if !(!((__b == b'\\' || __b == b'`'))) {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                            }
                                                                            Some(())
                                                                        })()
                                                                    } else {
                                                                        __alt_ok
                                                                    };
                                                                    if __alt_ok.is_none() {
                                                                        return None;
                                                                    }
                                                                }
                                                                Some(())
                                                            })();
                                                            if __ok.is_none() {
                                                                state.offset = __save;
                                                                break;
                                                            }
                                                            if state.offset == __save {
                                                                break;
                                                            }
                                                            __rep_count += 1;
                                                        }
                                                        if __rep_count < 0 {
                                                            return None;
                                                        }
                                                    }
                                                    Some(())
                                                })();
                                                if __result.is_some() {
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    state.offset = __start;
                                                    None
                                                }
                                            }?;
                                            if state.offset < state.src.len()
                                                && state.src.as_bytes()[state.offset] == 96u8
                                            {
                                                let __start = state.offset;
                                                state.offset += 1;
                                                Some(
                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                )
                                            } else {
                                                None
                                            }?;
                                            Some(
                                                ::parse_that::Span::new(__sp_start, state.offset, state.src),
                                            )
                                        })();
                                        if __result.is_some() {
                                            return __result;
                                        }
                                        state.offset = __cp;
                                    }
                                    None
                                })()
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::literal(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    });
                                if __result.is_some() {
                                    return __result;
                                }
                                state.offset = __cp;
                            }
                            {
                                let __cp = state.offset;
                                let __result = (|| {
                                    let __sp_start = state.offset;
                                    if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 47u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    {
                                        let __start = state.offset;
                                        let __result: Option<()> = (|| {
                                            {
                                                let mut __rep_count: u32 = 0;
                                                loop {
                                                    let __save = state.offset;
                                                    let __ok = (|| -> Option<()> {
                                                        {
                                                            let __save_alt = state.offset;
                                                            let __alt_ok = (|| -> Option<()> {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                                {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                                {
                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                    if !(!(__b == b'\n')) {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                }
                                                                Some(())
                                                            })();
                                                            let __alt_ok = if __alt_ok.is_none() {
                                                                state.offset = __save_alt;
                                                                (|| -> Option<()> {
                                                                    {
                                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                                        if !(!(__b == b'/')) {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                    }
                                                                    Some(())
                                                                })()
                                                            } else {
                                                                __alt_ok
                                                            };
                                                            if __alt_ok.is_none() {
                                                                return None;
                                                            }
                                                        }
                                                        Some(())
                                                    })();
                                                    if __ok.is_none() {
                                                        state.offset = __save;
                                                        break;
                                                    }
                                                    if state.offset == __save {
                                                        break;
                                                    }
                                                    __rep_count += 1;
                                                }
                                                if __rep_count < 1 {
                                                    return None;
                                                }
                                            }
                                            Some(())
                                        })();
                                        if __result.is_some() && state.offset > __start {
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            state.offset = __start;
                                            None
                                        }
                                    }?;
                                    if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 47u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    Some(
                                        ::parse_that::Span::new(__sp_start, state.offset, state.src),
                                    )
                                })()
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::regex(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    });
                                if __result.is_some() {
                                    return __result;
                                }
                                state.offset = __cp;
                            }
                            {
                                let __cp = state.offset;
                                let __result = ((|| {
                                    let __sp_start = state.offset;
                                    if state.src[state.offset..].starts_with("@{") {
                                        let __start = state.offset;
                                        state.offset += 2usize;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    let __sp124 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v125 = {
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ws_inner = Self::__rhs(state)
                                            .map(|__v| {
                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                            });
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __ws_inner
                                    }?;
                                    let __sp_start = state.offset;
                                    if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 125u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    let __sp126 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp124, __v125, __sp126))
                                })())
                                    .map(|__sv| {
                                        &*__BbnfBootstrapEnum_alloc(state)
                                            .slab()
                                            .alloc(BbnfBootstrapEnum::value_atom_0(__sv))
                                    });
                                if __result.is_some() {
                                    return __result;
                                }
                                state.offset = __cp;
                            }
                            {
                                let __cp = state.offset;
                                let __result = ((|| {
                                    let __sp_start = state.offset;
                                    if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 40u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    let __sp127 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v128 = {
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ws_inner = Self::__rhs(state)
                                            .map(|__v| {
                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                            });
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __ws_inner
                                    }?;
                                    let __sp_start = state.offset;
                                    if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 41u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    let __sp129 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp127, __v128, __sp129))
                                })())
                                    .map(|__sv| {
                                        &*__BbnfBootstrapEnum_alloc(state)
                                            .slab()
                                            .alloc(BbnfBootstrapEnum::value_atom_0(__sv))
                                    });
                                if __result.is_some() {
                                    return __result;
                                }
                                state.offset = __cp;
                            }
                            {
                                let __cp = state.offset;
                                let __result = ((|| {
                                    let __sp_start = state.offset;
                                    if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 91u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    let __sp130 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v131 = {
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ws_inner = Self::__rhs(state)
                                            .map(|__v| {
                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                            });
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __ws_inner
                                    }?;
                                    let __sp_start = state.offset;
                                    if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 93u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    let __sp132 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp130, __v131, __sp132))
                                })())
                                    .map(|__sv| {
                                        &*__BbnfBootstrapEnum_alloc(state)
                                            .slab()
                                            .alloc(BbnfBootstrapEnum::value_atom_0(__sv))
                                    });
                                if __result.is_some() {
                                    return __result;
                                }
                                state.offset = __cp;
                            }
                            {
                                let __cp = state.offset;
                                let __result = ((|| {
                                    let __sp_start = state.offset;
                                    if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 123u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    let __sp133 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v134 = {
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ws_inner = Self::__rhs(state)
                                            .map(|__v| {
                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                            });
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __ws_inner
                                    }?;
                                    let __sp_start = state.offset;
                                    if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 125u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    let __sp135 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp133, __v134, __sp135))
                                })())
                                    .map(|__sv| {
                                        &*__BbnfBootstrapEnum_alloc(state)
                                            .slab()
                                            .alloc(BbnfBootstrapEnum::value_atom_0(__sv))
                                    });
                                if __result.is_some() {
                                    return __result;
                                }
                                state.offset = __cp;
                            }
                            None
                        })()
                            .map(|__inner| {
                                let __v = BbnfBootstrapEnum::term(__inner);
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            });
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __ws_inner
                    }?;
                    let __v138 = {
                        let __cp = state.offset;
                        match (|| {
                            (|| {
                                {
                                    let __cp = state.offset;
                                    let __result = if state
                                        .src[state.offset..]
                                        .starts_with("?w")
                                    {
                                        let __start = state.offset;
                                        state.offset += 2usize;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    };
                                    if __result.is_some() {
                                        return __result;
                                    }
                                    state.offset = __cp;
                                }
                                {
                                    let __cp = state.offset;
                                    let __result = if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 63u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    };
                                    if __result.is_some() {
                                        return __result;
                                    }
                                    state.offset = __cp;
                                }
                                {
                                    let __cp = state.offset;
                                    let __result = if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 42u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    };
                                    if __result.is_some() {
                                        return __result;
                                    }
                                    state.offset = __cp;
                                }
                                {
                                    let __cp = state.offset;
                                    let __result = if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 43u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    };
                                    if __result.is_some() {
                                        return __result;
                                    }
                                    state.offset = __cp;
                                }
                                None
                            })()
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::modifier(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })
                        })() {
                            Some(__v) => Some(Some(__v)),
                            None => {
                                state.offset = __cp;
                                Some(None)
                            }
                        }
                    }?;
                    let __v139 = {
                        let __cp = state.offset;
                        match (|| {
                            {
                                ::parse_that::trim_leading_whitespace_mut(state);
                                let __ws_inner = (|| {
                                    let __sp_start = state.offset;
                                    if state.src[state.offset..].starts_with("/*") {
                                        let __start = state.offset;
                                        state.offset += 2usize;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    {
                                        let __start = state.offset;
                                        let __scan = if __start >= state.src_bytes.len() {
                                            0
                                        } else {
                                            (::parse_that::memchr::memchr(
                                                b'*',
                                                &state.src_bytes[__start..],
                                            ))
                                                .unwrap_or(state.src_bytes.len() - __start)
                                        };
                                        state.offset = __start + __scan;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    }?;
                                    if state.src[state.offset..].starts_with("*/") {
                                        let __start = state.offset;
                                        state.offset += 2usize;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    }?;
                                    Some(
                                        ::parse_that::Span::new(__sp_start, state.offset, state.src),
                                    )
                                })();
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __ws_inner
                            }
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::big_comment(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })
                        })() {
                            Some(__v) => Some(Some(__v)),
                            None => {
                                state.offset = __cp;
                                Some(None)
                            }
                        }
                    }?;
                    Some((__v136, __v137, __v138, __v139))
                })()
                    .map(|__inner| {
                        let __v = BbnfBootstrapEnum::factor(__inner);
                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                    })?;
                let __v148 = {
                    let __cp = state.offset;
                    match (|| (|| {
                        let __sp_start = state.offset;
                        {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = if state
                                .src[state.offset..]
                                .starts_with("->")
                            {
                                let __start = state.offset;
                                state.offset += 2usize;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            };
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp145 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v146 = (|| {
                            let __v143 = Self::__value_expr(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            let __v144 = {
                                let __cp = state.offset;
                                match (|| {
                                    (|| {
                                        let __sp_start = state.offset;
                                        {
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ws_inner = if state.offset < state.src.len()
                                                && state.src.as_bytes()[state.offset] == 58u8
                                            {
                                                let __start = state.offset;
                                                state.offset += 1;
                                                Some(
                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                )
                                            } else {
                                                None
                                            };
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __ws_inner
                                        }?;
                                        let __sp141 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        let __v142 = (|| {
                                            let __kd_cp140 = state.offset;
                                            if let Some(ref __kd_s) = ::parse_that::scan_ident(state) {
                                                let __kd_bytes = &state.src_bytes[__kd_s.start..__kd_s.end];
                                                let __kd_len = __kd_bytes.len();
                                                if (__kd_len == 2usize && __kd_bytes == &[b'u', b'8']) {
                                                    state.offset = __kd_cp140;
                                                    return if state.src[state.offset..].starts_with("u8") {
                                                        let __start = state.offset;
                                                        state.offset += 2usize;
                                                        Some(
                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                        )
                                                    } else {
                                                        None
                                                    };
                                                }
                                                if (__kd_len == 3usize && __kd_bytes == &[b'u', b'1', b'6'])
                                                {
                                                    state.offset = __kd_cp140;
                                                    return if state.src[state.offset..].starts_with("u16") {
                                                        let __start = state.offset;
                                                        state.offset += 3usize;
                                                        Some(
                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                        )
                                                    } else {
                                                        None
                                                    };
                                                }
                                                if (__kd_len == 3usize && __kd_bytes == &[b'u', b'3', b'2'])
                                                {
                                                    state.offset = __kd_cp140;
                                                    return if state.src[state.offset..].starts_with("u32") {
                                                        let __start = state.offset;
                                                        state.offset += 3usize;
                                                        Some(
                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                        )
                                                    } else {
                                                        None
                                                    };
                                                }
                                                if (__kd_len == 3usize && __kd_bytes == &[b'u', b'6', b'4'])
                                                {
                                                    state.offset = __kd_cp140;
                                                    return if state.src[state.offset..].starts_with("u64") {
                                                        let __start = state.offset;
                                                        state.offset += 3usize;
                                                        Some(
                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                        )
                                                    } else {
                                                        None
                                                    };
                                                }
                                                if (__kd_len == 3usize && __kd_bytes == &[b'i', b'3', b'2'])
                                                {
                                                    state.offset = __kd_cp140;
                                                    return if state.src[state.offset..].starts_with("i32") {
                                                        let __start = state.offset;
                                                        state.offset += 3usize;
                                                        Some(
                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                        )
                                                    } else {
                                                        None
                                                    };
                                                }
                                                if (__kd_len == 3usize && __kd_bytes == &[b'i', b'6', b'4'])
                                                {
                                                    state.offset = __kd_cp140;
                                                    return if state.src[state.offset..].starts_with("i64") {
                                                        let __start = state.offset;
                                                        state.offset += 3usize;
                                                        Some(
                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                        )
                                                    } else {
                                                        None
                                                    };
                                                }
                                                if (__kd_len == 3usize && __kd_bytes == &[b'f', b'3', b'2'])
                                                {
                                                    state.offset = __kd_cp140;
                                                    return if state.src[state.offset..].starts_with("f32") {
                                                        let __start = state.offset;
                                                        state.offset += 3usize;
                                                        Some(
                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                        )
                                                    } else {
                                                        None
                                                    };
                                                }
                                                if (__kd_len == 3usize && __kd_bytes == &[b'f', b'6', b'4'])
                                                {
                                                    state.offset = __kd_cp140;
                                                    return if state.src[state.offset..].starts_with("f64") {
                                                        let __start = state.offset;
                                                        state.offset += 3usize;
                                                        Some(
                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                        )
                                                    } else {
                                                        None
                                                    };
                                                }
                                                if (__kd_len == 4usize
                                                    && __kd_bytes == &[b'b', b'o', b'o', b'l'])
                                                {
                                                    state.offset = __kd_cp140;
                                                    return if state.src[state.offset..].starts_with("bool") {
                                                        let __start = state.offset;
                                                        state.offset += 4usize;
                                                        Some(
                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                        )
                                                    } else {
                                                        None
                                                    };
                                                }
                                                if (__kd_len == 5usize
                                                    && __kd_bytes == &[b'u', b's', b'i', b'z', b'e'])
                                                {
                                                    state.offset = __kd_cp140;
                                                    return if state.src[state.offset..].starts_with("usize") {
                                                        let __start = state.offset;
                                                        state.offset += 5usize;
                                                        Some(
                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                        )
                                                    } else {
                                                        None
                                                    };
                                                }
                                            }
                                            state.offset = __kd_cp140;
                                            ::parse_that::scan_ident(state)
                                        })()
                                            .map(|__inner| {
                                                let __v = BbnfBootstrapEnum::type_name(__inner);
                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                            })?;
                                        Some((__sp141, __v142))
                                    })()
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::type_annotation(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        })
                                })() {
                                    Some(__v) => Some(Some(__v)),
                                    None => {
                                        state.offset = __cp;
                                        Some(None)
                                    }
                                }
                            }?;
                            Some((__v143, __v144))
                        })()?;
                        Some((__sp145, __v146))
                    })())() {
                        Some(__v) => Some(Some(__v)),
                        None => {
                            state.offset = __cp;
                            Some(None)
                        }
                    }
                }?;
                Some((__v147, __v148))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::mapped_factor(__x))
    }
    pub fn mapped_factor<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__mapped_factor)
    }
    #[allow(non_snake_case)]
    fn __rule<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __v149 = ::parse_that::scan_ident(state)
                    .map(|__inner| {
                        let __v = BbnfBootstrapEnum::identifier(__inner);
                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                    })
                    .map(|__inner| {
                        let __v = BbnfBootstrapEnum::lhs(__inner);
                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                    })?;
                let __sp_start = state.offset;
                {
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ws_inner = if state.offset < state.src.len()
                        && state.src.as_bytes()[state.offset] == 61u8
                    {
                        let __start = state.offset;
                        state.offset += 1;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        None
                    };
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __ws_inner
                }?;
                let __sp150 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                let __v151 = {
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ws_inner = Self::__rhs(state)
                        .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v));
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __ws_inner
                }?;
                let __sp_start = state.offset;
                (|| {
                    let __r = if state.offset < state.src.len()
                        && state.src.as_bytes()[state.offset] == 59u8
                    {
                        let __start = state.offset;
                        state.offset += 1;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        None
                    };
                    if __r.is_some() {
                        return __r;
                    }
                    let __r = if state.offset < state.src.len()
                        && state.src.as_bytes()[state.offset] == 46u8
                    {
                        let __start = state.offset;
                        state.offset += 1;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        None
                    };
                    if __r.is_some() {
                        return __r;
                    }
                    None
                })()?;
                let __sp152 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                Some((__v149, __sp150, __v151, __sp152))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::rule(__x))
    }
    pub fn rule<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__rule)
    }
    #[allow(non_snake_case)]
    fn __directive<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                {
                    let __cp = state.offset;
                    let __result = Self::__import_directive(state)
                        .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v));
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = (|| {
                        let __sp_start = state.offset;
                        {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = if state
                                .src[state.offset..]
                                .starts_with("@recover")
                            {
                                let __start = state.offset;
                                state.offset += 8usize;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            };
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp153 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v154 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = ::parse_that::scan_ident(state)
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::identifier(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __v155 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = Self::__rhs(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp_start = state.offset;
                        {
                            let __cp = state.offset;
                            if (|| (|| {
                                let __r = if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 59u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                let __r = if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 46u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                None
                            })())()
                                .is_none()
                            {
                                state.offset = __cp;
                            }
                            Some(::parse_that::Span::new(__cp, state.offset, state.src))
                        }?;
                        let __sp156 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp153, __v154, __v155, __sp156))
                    })()
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::recover_directive(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = (|| {
                        let __sp_start = state.offset;
                        {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = if state
                                .src[state.offset..]
                                .starts_with("@pretty")
                            {
                                let __start = state.offset;
                                state.offset += 7usize;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            };
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp161 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v162 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = (|| {
                                {
                                    let __cp = state.offset;
                                    let __result = (if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 42u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    })
                                        .map(|__sv| {
                                            &*__BbnfBootstrapEnum_alloc(state)
                                                .slab()
                                                .alloc(BbnfBootstrapEnum::debug_directive_0(__sv))
                                        });
                                    if __result.is_some() {
                                        return __result;
                                    }
                                    state.offset = __cp;
                                }
                                {
                                    let __cp = state.offset;
                                    let __result = ::parse_that::scan_ident(state)
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::identifier(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        });
                                    if __result.is_some() {
                                        return __result;
                                    }
                                    state.offset = __cp;
                                }
                                None
                            })();
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __v163 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = {
                                let __depth159 = __BbnfBootstrapEnum_alloc(state)
                                    .__s1()
                                    .len();
                                loop {
                                    let __prev160 = state.offset;
                                    match (|| {
                                        let __v157 = ::parse_that::scan_ident(state)
                                            .map(|__inner| {
                                                let __v = BbnfBootstrapEnum::identifier(__inner);
                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                            })?;
                                        let __sp_start = state.offset;
                                        {
                                            let __cp = state.offset;
                                            if (|| (|| {
                                                let __sp_start = state.offset;
                                                if state.offset < state.src.len()
                                                    && state.src.as_bytes()[state.offset] == 40u8
                                                {
                                                    let __start = state.offset;
                                                    state.offset += 1;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                }?;
                                                {
                                                    let __start = state.offset;
                                                    let __scan = if __start >= state.src_bytes.len() {
                                                        0
                                                    } else {
                                                        (::parse_that::memchr::memchr(
                                                            b')',
                                                            &state.src_bytes[__start..],
                                                        ))
                                                            .unwrap_or(state.src_bytes.len() - __start)
                                                    };
                                                    state.offset = __start + __scan;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                }?;
                                                if state.offset < state.src.len()
                                                    && state.src.as_bytes()[state.offset] == 41u8
                                                {
                                                    let __start = state.offset;
                                                    state.offset += 1;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                }?;
                                                Some(
                                                    ::parse_that::Span::new(__sp_start, state.offset, state.src),
                                                )
                                            })())()
                                                .is_none()
                                            {
                                                state.offset = __cp;
                                            }
                                            Some(::parse_that::Span::new(__cp, state.offset, state.src))
                                        }?;
                                        let __sp158 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        Some((__v157, __sp158))
                                    })()
                                        .map(|__v| BbnfBootstrapEnum::pretty_hint(__v))
                                    {
                                        Some(__value) => {
                                            __BbnfBootstrapEnum_alloc(state).__s1().push(__value);
                                            if state.offset == __prev160 {
                                                break;
                                            }
                                        }
                                        None => {
                                            state.offset = __prev160;
                                            break;
                                        }
                                    }
                                }
                                if (__BbnfBootstrapEnum_alloc(state).__s1().len()
                                    - __depth159) >= 1usize
                                {
                                    Some(__BbnfBootstrapEnum_alloc(state).__c1(__depth159))
                                } else {
                                    __BbnfBootstrapEnum_alloc(state)
                                        .__s1()
                                        .truncate(__depth159);
                                    None
                                }
                            };
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp_start = state.offset;
                        {
                            let __cp = state.offset;
                            if (|| (|| {
                                let __r = if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 59u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                let __r = if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 46u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                None
                            })())()
                                .is_none()
                            {
                                state.offset = __cp;
                            }
                            Some(::parse_that::Span::new(__cp, state.offset, state.src))
                        }?;
                        let __sp164 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp161, __v162, __v163, __sp164))
                    })()
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::pretty_directive(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = (|| {
                        let __sp_start = state.offset;
                        {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = if state
                                .src[state.offset..]
                                .starts_with("@ws")
                            {
                                let __start = state.offset;
                                state.offset += 3usize;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            };
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp165 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v166 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = (|| {
                                let __sp_start = state.offset;
                                if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 47u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                }?;
                                {
                                    let __start = state.offset;
                                    let __result: Option<()> = (|| {
                                        {
                                            let mut __rep_count: u32 = 0;
                                            loop {
                                                let __save = state.offset;
                                                let __ok = (|| -> Option<()> {
                                                    {
                                                        let __save_alt = state.offset;
                                                        let __alt_ok = (|| -> Option<()> {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            {
                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                if !(!(__b == b'\n')) {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                            }
                                                            Some(())
                                                        })();
                                                        let __alt_ok = if __alt_ok.is_none() {
                                                            state.offset = __save_alt;
                                                            (|| -> Option<()> {
                                                                {
                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                    if !(!(__b == b'/')) {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                }
                                                                Some(())
                                                            })()
                                                        } else {
                                                            __alt_ok
                                                        };
                                                        if __alt_ok.is_none() {
                                                            return None;
                                                        }
                                                    }
                                                    Some(())
                                                })();
                                                if __ok.is_none() {
                                                    state.offset = __save;
                                                    break;
                                                }
                                                if state.offset == __save {
                                                    break;
                                                }
                                                __rep_count += 1;
                                            }
                                            if __rep_count < 1 {
                                                return None;
                                            }
                                        }
                                        Some(())
                                    })();
                                    if __result.is_some() && state.offset > __start {
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        state.offset = __start;
                                        None
                                    }
                                }?;
                                if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 47u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                }?;
                                Some(
                                    ::parse_that::Span::new(__sp_start, state.offset, state.src),
                                )
                            })()
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::regex(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp_start = state.offset;
                        {
                            let __cp = state.offset;
                            if (|| (|| {
                                let __r = if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 59u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                let __r = if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 46u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                None
                            })())()
                                .is_none()
                            {
                                state.offset = __cp;
                            }
                            Some(::parse_that::Span::new(__cp, state.offset, state.src))
                        }?;
                        let __sp167 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp165, __v166, __sp167))
                    })()
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::ws_directive(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = (|| {
                        let __sp_start = state.offset;
                        {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = if state
                                .src[state.offset..]
                                .starts_with("@token")
                            {
                                let __start = state.offset;
                                state.offset += 6usize;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            };
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp168 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v169 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = ::parse_that::scan_ident(state)
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::identifier(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp_start = state.offset;
                        {
                            let __cp = state.offset;
                            if (|| (|| {
                                let __r = if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 59u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                let __r = if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 46u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                None
                            })())()
                                .is_none()
                            {
                                state.offset = __cp;
                            }
                            Some(::parse_that::Span::new(__cp, state.offset, state.src))
                        }?;
                        let __sp170 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp168, __v169, __sp170))
                    })()
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::token_directive(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = (|| {
                        let __sp_start = state.offset;
                        {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = if state
                                .src[state.offset..]
                                .starts_with("@debug")
                            {
                                let __start = state.offset;
                                state.offset += 6usize;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            };
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp171 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v172 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = (|| {
                                {
                                    let __cp = state.offset;
                                    let __result = (if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 42u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    })
                                        .map(|__sv| {
                                            &*__BbnfBootstrapEnum_alloc(state)
                                                .slab()
                                                .alloc(BbnfBootstrapEnum::debug_directive_0(__sv))
                                        });
                                    if __result.is_some() {
                                        return __result;
                                    }
                                    state.offset = __cp;
                                }
                                {
                                    let __cp = state.offset;
                                    let __result = ::parse_that::scan_ident(state)
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::identifier(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        });
                                    if __result.is_some() {
                                        return __result;
                                    }
                                    state.offset = __cp;
                                }
                                None
                            })();
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp_start = state.offset;
                        {
                            let __cp = state.offset;
                            if (|| (|| {
                                let __r = if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 59u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                let __r = if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 46u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                None
                            })())()
                                .is_none()
                            {
                                state.offset = __cp;
                            }
                            Some(::parse_that::Span::new(__cp, state.offset, state.src))
                        }?;
                        let __sp173 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp171, __v172, __sp173))
                    })()
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::debug_directive(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = (|| {
                        let __sp_start = state.offset;
                        {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = if state
                                .src[state.offset..]
                                .starts_with("@host")
                            {
                                let __start = state.offset;
                                state.offset += 5usize;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            };
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __sp177 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v178 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = ::parse_that::scan_ident(state)
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::identifier(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __v179 = {
                            let __cp = state.offset;
                            match (|| (|| {
                                let __sp_start = state.offset;
                                {
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ws_inner = if state.offset < state.src.len()
                                        && state.src.as_bytes()[state.offset] == 58u8
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        None
                                    };
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __ws_inner
                                }?;
                                let __sp175 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                let __v176 = {
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ws_inner = (|| {
                                        let __kd_cp174 = state.offset;
                                        if let Some(ref __kd_s) = ::parse_that::scan_ident(state) {
                                            let __kd_bytes = &state.src_bytes[__kd_s.start..__kd_s.end];
                                            let __kd_len = __kd_bytes.len();
                                            if (__kd_len == 2usize && __kd_bytes == &[b'u', b'8']) {
                                                state.offset = __kd_cp174;
                                                return if state.src[state.offset..].starts_with("u8") {
                                                    let __start = state.offset;
                                                    state.offset += 2usize;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                };
                                            }
                                            if (__kd_len == 3usize && __kd_bytes == &[b'u', b'1', b'6'])
                                            {
                                                state.offset = __kd_cp174;
                                                return if state.src[state.offset..].starts_with("u16") {
                                                    let __start = state.offset;
                                                    state.offset += 3usize;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                };
                                            }
                                            if (__kd_len == 3usize && __kd_bytes == &[b'u', b'3', b'2'])
                                            {
                                                state.offset = __kd_cp174;
                                                return if state.src[state.offset..].starts_with("u32") {
                                                    let __start = state.offset;
                                                    state.offset += 3usize;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                };
                                            }
                                            if (__kd_len == 3usize && __kd_bytes == &[b'u', b'6', b'4'])
                                            {
                                                state.offset = __kd_cp174;
                                                return if state.src[state.offset..].starts_with("u64") {
                                                    let __start = state.offset;
                                                    state.offset += 3usize;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                };
                                            }
                                            if (__kd_len == 3usize && __kd_bytes == &[b'i', b'3', b'2'])
                                            {
                                                state.offset = __kd_cp174;
                                                return if state.src[state.offset..].starts_with("i32") {
                                                    let __start = state.offset;
                                                    state.offset += 3usize;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                };
                                            }
                                            if (__kd_len == 3usize && __kd_bytes == &[b'i', b'6', b'4'])
                                            {
                                                state.offset = __kd_cp174;
                                                return if state.src[state.offset..].starts_with("i64") {
                                                    let __start = state.offset;
                                                    state.offset += 3usize;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                };
                                            }
                                            if (__kd_len == 3usize && __kd_bytes == &[b'f', b'3', b'2'])
                                            {
                                                state.offset = __kd_cp174;
                                                return if state.src[state.offset..].starts_with("f32") {
                                                    let __start = state.offset;
                                                    state.offset += 3usize;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                };
                                            }
                                            if (__kd_len == 3usize && __kd_bytes == &[b'f', b'6', b'4'])
                                            {
                                                state.offset = __kd_cp174;
                                                return if state.src[state.offset..].starts_with("f64") {
                                                    let __start = state.offset;
                                                    state.offset += 3usize;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                };
                                            }
                                            if (__kd_len == 4usize
                                                && __kd_bytes == &[b'b', b'o', b'o', b'l'])
                                            {
                                                state.offset = __kd_cp174;
                                                return if state.src[state.offset..].starts_with("bool") {
                                                    let __start = state.offset;
                                                    state.offset += 4usize;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                };
                                            }
                                            if (__kd_len == 5usize
                                                && __kd_bytes == &[b'u', b's', b'i', b'z', b'e'])
                                            {
                                                state.offset = __kd_cp174;
                                                return if state.src[state.offset..].starts_with("usize") {
                                                    let __start = state.offset;
                                                    state.offset += 5usize;
                                                    Some(
                                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                                    )
                                                } else {
                                                    None
                                                };
                                            }
                                        }
                                        state.offset = __kd_cp174;
                                        ::parse_that::scan_ident(state)
                                    })()
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::type_name(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        });
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __ws_inner
                                }?;
                                Some((__sp175, __v176))
                            })())() {
                                Some(__v) => Some(Some(__v)),
                                None => {
                                    state.offset = __cp;
                                    Some(None)
                                }
                            }
                        }?;
                        let __sp_start = state.offset;
                        {
                            let __cp = state.offset;
                            if (|| (|| {
                                let __r = if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 59u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                let __r = if state.offset < state.src.len()
                                    && state.src.as_bytes()[state.offset] == 46u8
                                {
                                    let __start = state.offset;
                                    state.offset += 1;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                None
                            })())()
                                .is_none()
                            {
                                state.offset = __cp;
                            }
                            Some(::parse_that::Span::new(__cp, state.offset, state.src))
                        }?;
                        let __sp180 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp177, __v178, __v179, __sp180))
                    })()
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::host_directive(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                None
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::directive(__x))
    }
    pub fn directive<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__directive)
    }
    #[allow(non_snake_case)]
    fn __grammar_item<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                {
                    let __cp = state.offset;
                    let __result = {
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ws_inner = (|| {
                            let __sp_start = state.offset;
                            if state.src[state.offset..].starts_with("//") {
                                let __start = state.offset;
                                state.offset += 2usize;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            }?;
                            {
                                let __start = state.offset;
                                let __result: Option<()> = (|| {
                                    {
                                        let __end = state.src_bytes.len();
                                        let mut __pos = state.offset;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if !(__b == b'\n') {
                                                __pos += 1;
                                            } else {
                                                break;
                                            }
                                        }
                                        state.offset = __pos;
                                    }
                                    Some(())
                                })();
                                if __result.is_some() {
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    state.offset = __start;
                                    None
                                }
                            }?;
                            Some(
                                ::parse_that::Span::new(__sp_start, state.offset, state.src),
                            )
                        })();
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __ws_inner
                    }
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::comment(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = {
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ws_inner = (|| {
                            let __sp_start = state.offset;
                            if state.src[state.offset..].starts_with("/*") {
                                let __start = state.offset;
                                state.offset += 2usize;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            }?;
                            {
                                let __start = state.offset;
                                let __scan = if __start >= state.src_bytes.len() {
                                    0
                                } else {
                                    (::parse_that::memchr::memchr(
                                        b'*',
                                        &state.src_bytes[__start..],
                                    ))
                                        .unwrap_or(state.src_bytes.len() - __start)
                                };
                                state.offset = __start + __scan;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            }?;
                            if state.src[state.offset..].starts_with("*/") {
                                let __start = state.offset;
                                state.offset += 2usize;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            }?;
                            Some(
                                ::parse_that::Span::new(__sp_start, state.offset, state.src),
                            )
                        })();
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __ws_inner
                    }
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::big_comment(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        });
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = Self::__directive(state)
                        .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v));
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = Self::__rule(state)
                        .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v));
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                None
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::grammar_item(__x))
    }
    pub fn grammar_item<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__grammar_item)
    }
    #[allow(non_snake_case)]
    fn __grammar<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __depth181 = __BbnfBootstrapEnum_alloc(state).__s1().len();
                loop {
                    let __prev182 = state.offset;
                    match {
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ws_inner = Self::__grammar_item(state);
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __ws_inner
                    } {
                        Some(__value) => {
                            __BbnfBootstrapEnum_alloc(state).__s1().push(__value);
                            if state.offset == __prev182 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __prev182;
                            break;
                        }
                    }
                }
                Some(__BbnfBootstrapEnum_alloc(state).__c1(__depth181))
            }
        })()
            .map(|__x| BbnfBootstrapEnum::grammar(__x))
    }
    pub fn grammar<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__grammar)
    }
    #[allow(non_snake_case)]
    fn __type_name_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp10 = state.offset;
                    let __ok = (|| -> bool {
                        {
                            let __s = "u8";
                            let __bytes = __s.as_bytes();
                            let __slc = match state.src_bytes.get(state.offset..) {
                                Some(s) if s.len() >= 2usize => s,
                                _ => return false,
                            };
                            if &__slc[..2usize] != __bytes {
                                return false;
                            }
                            __builder
                                .text(&state.src[state.offset..state.offset + 2usize]);
                            state.offset += 2usize;
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp10;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp9 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    let __s = "u16";
                                    let __bytes = __s.as_bytes();
                                    let __slc = match state.src_bytes.get(state.offset..) {
                                        Some(s) if s.len() >= 3usize => s,
                                        _ => return false,
                                    };
                                    if &__slc[..3usize] != __bytes {
                                        return false;
                                    }
                                    __builder
                                        .text(&state.src[state.offset..state.offset + 3usize]);
                                    state.offset += 3usize;
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp9;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp8 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            let __s = "u32";
                                            let __bytes = __s.as_bytes();
                                            let __slc = match state.src_bytes.get(state.offset..) {
                                                Some(s) if s.len() >= 3usize => s,
                                                _ => return false,
                                            };
                                            if &__slc[..3usize] != __bytes {
                                                return false;
                                            }
                                            __builder
                                                .text(&state.src[state.offset..state.offset + 3usize]);
                                            state.offset += 3usize;
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp8;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp7 = state.offset;
                                            let __ok = (|| -> bool {
                                                {
                                                    let __s = "u64";
                                                    let __bytes = __s.as_bytes();
                                                    let __slc = match state.src_bytes.get(state.offset..) {
                                                        Some(s) if s.len() >= 3usize => s,
                                                        _ => return false,
                                                    };
                                                    if &__slc[..3usize] != __bytes {
                                                        return false;
                                                    }
                                                    __builder
                                                        .text(&state.src[state.offset..state.offset + 3usize]);
                                                    state.offset += 3usize;
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp7;
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp6 = state.offset;
                                                    let __ok = (|| -> bool {
                                                        {
                                                            let __s = "i32";
                                                            let __bytes = __s.as_bytes();
                                                            let __slc = match state.src_bytes.get(state.offset..) {
                                                                Some(s) if s.len() >= 3usize => s,
                                                                _ => return false,
                                                            };
                                                            if &__slc[..3usize] != __bytes {
                                                                return false;
                                                            }
                                                            __builder
                                                                .text(&state.src[state.offset..state.offset + 3usize]);
                                                            state.offset += 3usize;
                                                        };
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp6;
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp5 = state.offset;
                                                            let __ok = (|| -> bool {
                                                                {
                                                                    let __s = "i64";
                                                                    let __bytes = __s.as_bytes();
                                                                    let __slc = match state.src_bytes.get(state.offset..) {
                                                                        Some(s) if s.len() >= 3usize => s,
                                                                        _ => return false,
                                                                    };
                                                                    if &__slc[..3usize] != __bytes {
                                                                        return false;
                                                                    }
                                                                    __builder
                                                                        .text(&state.src[state.offset..state.offset + 3usize]);
                                                                    state.offset += 3usize;
                                                                };
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp5;
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp4 = state.offset;
                                                                    let __ok = (|| -> bool {
                                                                        {
                                                                            let __s = "f32";
                                                                            let __bytes = __s.as_bytes();
                                                                            let __slc = match state.src_bytes.get(state.offset..) {
                                                                                Some(s) if s.len() >= 3usize => s,
                                                                                _ => return false,
                                                                            };
                                                                            if &__slc[..3usize] != __bytes {
                                                                                return false;
                                                                            }
                                                                            __builder
                                                                                .text(&state.src[state.offset..state.offset + 3usize]);
                                                                            state.offset += 3usize;
                                                                        };
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp4;
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp3 = state.offset;
                                                                            let __ok = (|| -> bool {
                                                                                {
                                                                                    let __s = "f64";
                                                                                    let __bytes = __s.as_bytes();
                                                                                    let __slc = match state.src_bytes.get(state.offset..) {
                                                                                        Some(s) if s.len() >= 3usize => s,
                                                                                        _ => return false,
                                                                                    };
                                                                                    if &__slc[..3usize] != __bytes {
                                                                                        return false;
                                                                                    }
                                                                                    __builder
                                                                                        .text(&state.src[state.offset..state.offset + 3usize]);
                                                                                    state.offset += 3usize;
                                                                                };
                                                                                true
                                                                            })();
                                                                            if !__ok {
                                                                                state.offset = __pretty_cp3;
                                                                            }
                                                                            __ok
                                                                        } {
                                                                            {
                                                                                if !{
                                                                                    let __pretty_cp2 = state.offset;
                                                                                    let __ok = (|| -> bool {
                                                                                        {
                                                                                            let __s = "bool";
                                                                                            let __bytes = __s.as_bytes();
                                                                                            let __slc = match state.src_bytes.get(state.offset..) {
                                                                                                Some(s) if s.len() >= 4usize => s,
                                                                                                _ => return false,
                                                                                            };
                                                                                            if &__slc[..4usize] != __bytes {
                                                                                                return false;
                                                                                            }
                                                                                            __builder
                                                                                                .text(&state.src[state.offset..state.offset + 4usize]);
                                                                                            state.offset += 4usize;
                                                                                        };
                                                                                        true
                                                                                    })();
                                                                                    if !__ok {
                                                                                        state.offset = __pretty_cp2;
                                                                                    }
                                                                                    __ok
                                                                                } {
                                                                                    {
                                                                                        if !{
                                                                                            let __pretty_cp1 = state.offset;
                                                                                            let __ok = (|| -> bool {
                                                                                                {
                                                                                                    let __s = "usize";
                                                                                                    let __bytes = __s.as_bytes();
                                                                                                    let __slc = match state.src_bytes.get(state.offset..) {
                                                                                                        Some(s) if s.len() >= 5usize => s,
                                                                                                        _ => return false,
                                                                                                    };
                                                                                                    if &__slc[..5usize] != __bytes {
                                                                                                        return false;
                                                                                                    }
                                                                                                    __builder
                                                                                                        .text(&state.src[state.offset..state.offset + 5usize]);
                                                                                                    state.offset += 5usize;
                                                                                                };
                                                                                                true
                                                                                            })();
                                                                                            if !__ok {
                                                                                                state.offset = __pretty_cp1;
                                                                                            }
                                                                                            __ok
                                                                                        } {
                                                                                            {
                                                                                                if !{
                                                                                                    let __pretty_cp0 = state.offset;
                                                                                                    let __ok = (|| -> bool {
                                                                                                        {
                                                                                                            let __start = state.offset;
                                                                                                            if ::parse_that::scan_ident(state).is_none() {
                                                                                                                return false;
                                                                                                            }
                                                                                                            let __matched = &state.src[__start..state.offset];
                                                                                                            if !__matched.is_empty() {
                                                                                                                __builder.text(__matched);
                                                                                                            }
                                                                                                        };
                                                                                                        true
                                                                                                    })();
                                                                                                    if !__ok {
                                                                                                        state.offset = __pretty_cp0;
                                                                                                    }
                                                                                                    __ok
                                                                                                } {
                                                                                                    return false;
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };
            true
        }
    }
    pub fn type_name_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__type_name_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __regex_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    if state.src_bytes.get(state.offset).copied() != Some(b'/') {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(b'/');
                };
                {
                    let __start = state.offset;
                    if {
                        let __start = state.offset;
                        let __result: Option<()> = (|| {
                            {
                                let mut __rep_count: u32 = 0;
                                loop {
                                    let __save = state.offset;
                                    let __ok = (|| -> Option<()> {
                                        {
                                            let __save_alt = state.offset;
                                            let __alt_ok = (|| -> Option<()> {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                {
                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                    if !(!(__b == b'\n')) {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                }
                                                Some(())
                                            })();
                                            let __alt_ok = if __alt_ok.is_none() {
                                                state.offset = __save_alt;
                                                (|| -> Option<()> {
                                                    {
                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                        if !(!(__b == b'/')) {
                                                            return None;
                                                        }
                                                        state.offset += 1;
                                                    }
                                                    Some(())
                                                })()
                                            } else {
                                                __alt_ok
                                            };
                                            if __alt_ok.is_none() {
                                                return None;
                                            }
                                        }
                                        Some(())
                                    })();
                                    if __ok.is_none() {
                                        state.offset = __save;
                                        break;
                                    }
                                    if state.offset == __save {
                                        break;
                                    }
                                    __rep_count += 1;
                                }
                                if __rep_count < 1 {
                                    return None;
                                }
                            }
                            Some(())
                        })();
                        if __result.is_some() && state.offset > __start {
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            state.offset = __start;
                            None
                        }
                    }
                        .is_none()
                    {
                        return false;
                    }
                    let __matched = &state.src[__start..state.offset];
                    if !__matched.is_empty() {
                        __builder.text(__matched);
                    }
                };
                {
                    if state.src_bytes.get(state.offset).copied() != Some(b'/') {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(b'/');
                };
            };
            true
        }
    }
    pub fn regex_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__regex_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __identifier_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let __start = state.offset;
                if ::parse_that::scan_ident(state).is_none() {
                    return false;
                }
                let __matched = &state.src[__start..state.offset];
                if !__matched.is_empty() {
                    __builder.text(__matched);
                }
            };
            true
        }
    }
    pub fn identifier_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__identifier_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __literal_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp15 = state.offset;
                    let __pretty_bcp16 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'"');
                            };
                            {
                                let __start = state.offset;
                                if {
                                    let __start = state.offset;
                                    let __result: Option<()> = (|| {
                                        {
                                            let mut __rep_count: u32 = 0;
                                            loop {
                                                let __save = state.offset;
                                                let __ok = (|| -> Option<()> {
                                                    {
                                                        let __save_alt = state.offset;
                                                        let __alt_ok = (|| -> Option<()> {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                            {
                                                                return None;
                                                            }
                                                            state.offset += 1;
                                                            {
                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                if !(!(__b == b'\n')) {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                            }
                                                            Some(())
                                                        })();
                                                        let __alt_ok = if __alt_ok.is_none() {
                                                            state.offset = __save_alt;
                                                            (|| -> Option<()> {
                                                                {
                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                    if !(!((__b == b'"' || __b == b'\\'))) {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                }
                                                                Some(())
                                                            })()
                                                        } else {
                                                            __alt_ok
                                                        };
                                                        if __alt_ok.is_none() {
                                                            return None;
                                                        }
                                                    }
                                                    Some(())
                                                })();
                                                if __ok.is_none() {
                                                    state.offset = __save;
                                                    break;
                                                }
                                                if state.offset == __save {
                                                    break;
                                                }
                                                __rep_count += 1;
                                            }
                                            if __rep_count < 0 {
                                                return None;
                                            }
                                        }
                                        Some(())
                                    })();
                                    if __result.is_some() {
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    } else {
                                        state.offset = __start;
                                        None
                                    }
                                }
                                    .is_none()
                                {
                                    return false;
                                }
                                let __matched = &state.src[__start..state.offset];
                                if !__matched.is_empty() {
                                    __builder.text(__matched);
                                }
                            };
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'"');
                            };
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp15;
                        __builder.restore(__pretty_bcp16);
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp13 = state.offset;
                            let __pretty_bcp14 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'\'');
                                    };
                                    {
                                        let __start = state.offset;
                                        if {
                                            let __start = state.offset;
                                            let __result: Option<()> = (|| {
                                                {
                                                    let mut __rep_count: u32 = 0;
                                                    loop {
                                                        let __save = state.offset;
                                                        let __ok = (|| -> Option<()> {
                                                            {
                                                                let __save_alt = state.offset;
                                                                let __alt_ok = (|| -> Option<()> {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                                    {
                                                                        return None;
                                                                    }
                                                                    state.offset += 1;
                                                                    {
                                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                                        if !(!(__b == b'\n')) {
                                                                            return None;
                                                                        }
                                                                        state.offset += 1;
                                                                    }
                                                                    Some(())
                                                                })();
                                                                let __alt_ok = if __alt_ok.is_none() {
                                                                    state.offset = __save_alt;
                                                                    (|| -> Option<()> {
                                                                        {
                                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                                            if !(!((__b == b'\'' || __b == b'\\'))) {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                        }
                                                                        Some(())
                                                                    })()
                                                                } else {
                                                                    __alt_ok
                                                                };
                                                                if __alt_ok.is_none() {
                                                                    return None;
                                                                }
                                                            }
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save;
                                                            break;
                                                        }
                                                        if state.offset == __save {
                                                            break;
                                                        }
                                                        __rep_count += 1;
                                                    }
                                                    if __rep_count < 0 {
                                                        return None;
                                                    }
                                                }
                                                Some(())
                                            })();
                                            if __result.is_some() {
                                                Some(
                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                )
                                            } else {
                                                state.offset = __start;
                                                None
                                            }
                                        }
                                            .is_none()
                                        {
                                            return false;
                                        }
                                        let __matched = &state.src[__start..state.offset];
                                        if !__matched.is_empty() {
                                            __builder.text(__matched);
                                        }
                                    };
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'\'')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'\'');
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp13;
                                __builder.restore(__pretty_bcp14);
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp11 = state.offset;
                                    let __pretty_bcp12 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'`')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'`');
                                            };
                                            {
                                                let __start = state.offset;
                                                if {
                                                    let __start = state.offset;
                                                    let __result: Option<()> = (|| {
                                                        {
                                                            let mut __rep_count: u32 = 0;
                                                            loop {
                                                                let __save = state.offset;
                                                                let __ok = (|| -> Option<()> {
                                                                    {
                                                                        let __save_alt = state.offset;
                                                                        let __alt_ok = (|| -> Option<()> {
                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                                            {
                                                                                return None;
                                                                            }
                                                                            state.offset += 1;
                                                                            {
                                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                                if !(!(__b == b'\n')) {
                                                                                    return None;
                                                                                }
                                                                                state.offset += 1;
                                                                            }
                                                                            Some(())
                                                                        })();
                                                                        let __alt_ok = if __alt_ok.is_none() {
                                                                            state.offset = __save_alt;
                                                                            (|| -> Option<()> {
                                                                                {
                                                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                                                    if !(!((__b == b'\\' || __b == b'`'))) {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                }
                                                                                Some(())
                                                                            })()
                                                                        } else {
                                                                            __alt_ok
                                                                        };
                                                                        if __alt_ok.is_none() {
                                                                            return None;
                                                                        }
                                                                    }
                                                                    Some(())
                                                                })();
                                                                if __ok.is_none() {
                                                                    state.offset = __save;
                                                                    break;
                                                                }
                                                                if state.offset == __save {
                                                                    break;
                                                                }
                                                                __rep_count += 1;
                                                            }
                                                            if __rep_count < 0 {
                                                                return None;
                                                            }
                                                        }
                                                        Some(())
                                                    })();
                                                    if __result.is_some() {
                                                        Some(
                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                        )
                                                    } else {
                                                        state.offset = __start;
                                                        None
                                                    }
                                                }
                                                    .is_none()
                                                {
                                                    return false;
                                                }
                                                let __matched = &state.src[__start..state.offset];
                                                if !__matched.is_empty() {
                                                    __builder.text(__matched);
                                                }
                                            };
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'`')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'`');
                                            };
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp11;
                                        __builder.restore(__pretty_bcp12);
                                    }
                                    __ok
                                } {
                                    return false;
                                }
                            }
                        }
                    }
                }
            };
            true
        }
    }
    pub fn literal_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__literal_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __big_comment_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp19 = state.offset;
                    let __pretty_bcp20 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            let __ows17 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows17..state.offset]);
                            {
                                {
                                    let __s = "/*";
                                    let __bytes = __s.as_bytes();
                                    let __slc = match state.src_bytes.get(state.offset..) {
                                        Some(s) if s.len() >= 2usize => s,
                                        _ => return false,
                                    };
                                    if &__slc[..2usize] != __bytes {
                                        return false;
                                    }
                                    __builder
                                        .text(&state.src[state.offset..state.offset + 2usize]);
                                    state.offset += 2usize;
                                };
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __scan = if __start >= state.src_bytes.len() {
                                            0
                                        } else {
                                            (::parse_that::memchr::memchr(
                                                b'*',
                                                &state.src_bytes[__start..],
                                            ))
                                                .unwrap_or(state.src_bytes.len() - __start)
                                        };
                                        state.offset = __start + __scan;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    }
                                        .is_none()
                                    {
                                        return false;
                                    }
                                    let __matched = &state.src[__start..state.offset];
                                    if !__matched.is_empty() {
                                        __builder.text(__matched);
                                    }
                                };
                                {
                                    let __s = "*/";
                                    let __bytes = __s.as_bytes();
                                    let __slc = match state.src_bytes.get(state.offset..) {
                                        Some(s) if s.len() >= 2usize => s,
                                        _ => return false,
                                    };
                                    if &__slc[..2usize] != __bytes {
                                        return false;
                                    }
                                    __builder
                                        .text(&state.src[state.offset..state.offset + 2usize]);
                                    state.offset += 2usize;
                                };
                            };
                            let __ows18 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows18..state.offset]);
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp19;
                        __builder.restore(__pretty_bcp20);
                    }
                    __ok
                } {
                    return false;
                }
            };
            true
        }
    }
    pub fn big_comment_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__big_comment_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __modifier_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp24 = state.offset;
                    let __ok = (|| -> bool {
                        {
                            let __s = "?w";
                            let __bytes = __s.as_bytes();
                            let __slc = match state.src_bytes.get(state.offset..) {
                                Some(s) if s.len() >= 2usize => s,
                                _ => return false,
                            };
                            if &__slc[..2usize] != __bytes {
                                return false;
                            }
                            __builder
                                .text(&state.src[state.offset..state.offset + 2usize]);
                            state.offset += 2usize;
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp24;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp23 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'?')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'?');
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp23;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp22 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'*');
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp22;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp21 = state.offset;
                                            let __ok = (|| -> bool {
                                                {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'+')
                                                    {
                                                        return false;
                                                    }
                                                    state.offset += 1;
                                                    __builder.char(b'+');
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp21;
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };
            true
        }
    }
    pub fn modifier_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__modifier_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __value_ident_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let __start = state.offset;
                if ::parse_that::scan_ident(state).is_none() {
                    return false;
                }
                let __matched = &state.src[__start..state.offset];
                if !__matched.is_empty() {
                    __builder.text(__matched);
                }
            };
            true
        }
    }
    pub fn value_ident_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__value_ident_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __cmp_op_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp30 = state.offset;
                    let __ok = (|| -> bool {
                        {
                            let __s = "==";
                            let __bytes = __s.as_bytes();
                            let __slc = match state.src_bytes.get(state.offset..) {
                                Some(s) if s.len() >= 2usize => s,
                                _ => return false,
                            };
                            if &__slc[..2usize] != __bytes {
                                return false;
                            }
                            __builder
                                .text(&state.src[state.offset..state.offset + 2usize]);
                            state.offset += 2usize;
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp30;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp29 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    let __s = "!=";
                                    let __bytes = __s.as_bytes();
                                    let __slc = match state.src_bytes.get(state.offset..) {
                                        Some(s) if s.len() >= 2usize => s,
                                        _ => return false,
                                    };
                                    if &__slc[..2usize] != __bytes {
                                        return false;
                                    }
                                    __builder
                                        .text(&state.src[state.offset..state.offset + 2usize]);
                                    state.offset += 2usize;
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp29;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp28 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            let __s = "<=";
                                            let __bytes = __s.as_bytes();
                                            let __slc = match state.src_bytes.get(state.offset..) {
                                                Some(s) if s.len() >= 2usize => s,
                                                _ => return false,
                                            };
                                            if &__slc[..2usize] != __bytes {
                                                return false;
                                            }
                                            __builder
                                                .text(&state.src[state.offset..state.offset + 2usize]);
                                            state.offset += 2usize;
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp28;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp27 = state.offset;
                                            let __ok = (|| -> bool {
                                                {
                                                    let __s = ">=";
                                                    let __bytes = __s.as_bytes();
                                                    let __slc = match state.src_bytes.get(state.offset..) {
                                                        Some(s) if s.len() >= 2usize => s,
                                                        _ => return false,
                                                    };
                                                    if &__slc[..2usize] != __bytes {
                                                        return false;
                                                    }
                                                    __builder
                                                        .text(&state.src[state.offset..state.offset + 2usize]);
                                                    state.offset += 2usize;
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp27;
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp26 = state.offset;
                                                    let __ok = (|| -> bool {
                                                        {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b'<')
                                                            {
                                                                return false;
                                                            }
                                                            state.offset += 1;
                                                            __builder.char(b'<');
                                                        };
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp26;
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp25 = state.offset;
                                                            let __ok = (|| -> bool {
                                                                {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'>')
                                                                    {
                                                                        return false;
                                                                    }
                                                                    state.offset += 1;
                                                                    __builder.char(b'>');
                                                                };
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp25;
                                                            }
                                                            __ok
                                                        } {
                                                            return false;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };
            true
        }
    }
    pub fn cmp_op_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__cmp_op_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __mul_op_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp33 = state.offset;
                    let __ok = (|| -> bool {
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'*') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'*');
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp33;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp32 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'/')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'/');
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp32;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp31 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'%')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'%');
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp31;
                                    }
                                    __ok
                                } {
                                    return false;
                                }
                            }
                        }
                    }
                }
            };
            true
        }
    }
    pub fn mul_op_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__mul_op_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __int_lit_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let __start = state.offset;
                if {
                    let __start = state.offset;
                    let __result: Option<()> = (|| {
                        {
                            let __save_alt = state.offset;
                            let __alt_ok = (|| -> Option<()> {
                                if state.src_bytes.get(state.offset).copied() != Some(b'0')
                                {
                                    return None;
                                }
                                state.offset += 1;
                                {
                                    let __b = *state.src_bytes.get(state.offset)?;
                                    if !((__b == b'X' || __b == b'x')) {
                                        return None;
                                    }
                                    state.offset += 1;
                                }
                                {
                                    let __loop_start = state.offset;
                                    let __end = state.src_bytes.len();
                                    let mut __pos = state.offset;
                                    while __pos < __end {
                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                        if __b.is_ascii_hexdigit() {
                                            __pos += 1;
                                        } else {
                                            break;
                                        }
                                    }
                                    if __pos < __loop_start + 1 as usize {
                                        return None;
                                    }
                                    state.offset = __pos;
                                }
                                {
                                    let __end = state.src_bytes.len();
                                    let mut __pos = state.offset;
                                    while __pos < __end {
                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                        if (__b.is_ascii_alphanumeric() || __b == b'_') {
                                            __pos += 1;
                                        } else {
                                            break;
                                        }
                                    }
                                    state.offset = __pos;
                                }
                                Some(())
                            })();
                            let __alt_ok = if __alt_ok.is_none() {
                                state.offset = __save_alt;
                                (|| -> Option<()> {
                                    {
                                        let __loop_start = state.offset;
                                        let __end = state.src_bytes.len();
                                        let mut __pos = state.offset;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if __b.is_ascii_digit() {
                                                __pos += 1;
                                            } else {
                                                break;
                                            }
                                        }
                                        if __pos < __loop_start + 1 as usize {
                                            return None;
                                        }
                                        state.offset = __pos;
                                    }
                                    {
                                        let __end = state.src_bytes.len();
                                        let mut __pos = state.offset;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if (__b.is_ascii_alphanumeric() || __b == b'_') {
                                                __pos += 1;
                                            } else {
                                                break;
                                            }
                                        }
                                        state.offset = __pos;
                                    }
                                    Some(())
                                })()
                            } else {
                                __alt_ok
                            };
                            if __alt_ok.is_none() {
                                return None;
                            }
                        }
                        Some(())
                    })();
                    if __result.is_some() && state.offset > __start {
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        state.offset = __start;
                        None
                    }
                }
                    .is_none()
                {
                    return false;
                }
                let __matched = &state.src[__start..state.offset];
                if !__matched.is_empty() {
                    __builder.text(__matched);
                }
            };
            true
        }
    }
    pub fn int_lit_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__int_lit_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __float_lit_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let __start = state.offset;
                if {
                    let __start = state.offset;
                    let __result: Option<()> = (|| {
                        {
                            let __end = state.src_bytes.len();
                            let mut __pos = state.offset;
                            while __pos < __end {
                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                if __b.is_ascii_digit() {
                                    __pos += 1;
                                } else {
                                    break;
                                }
                            }
                            state.offset = __pos;
                        }
                        if state.src_bytes.get(state.offset).copied() != Some(b'.') {
                            return None;
                        }
                        state.offset += 1;
                        {
                            let __loop_start = state.offset;
                            let __end = state.src_bytes.len();
                            let mut __pos = state.offset;
                            while __pos < __end {
                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                if __b.is_ascii_digit() {
                                    __pos += 1;
                                } else {
                                    break;
                                }
                            }
                            if __pos < __loop_start + 1 as usize {
                                return None;
                            }
                            state.offset = __pos;
                        }
                        {
                            let __save = state.offset;
                            let __ok = (|| -> Option<()> {
                                {
                                    let __b = *state.src_bytes.get(state.offset)?;
                                    if !((__b == b'E' || __b == b'e')) {
                                        return None;
                                    }
                                    state.offset += 1;
                                }
                                {
                                    let __save = state.offset;
                                    let __ok = (|| -> Option<()> {
                                        {
                                            let __b = *state.src_bytes.get(state.offset)?;
                                            if !((__b == b'+' || __b == b'-')) {
                                                return None;
                                            }
                                            state.offset += 1;
                                        }
                                        Some(())
                                    })();
                                    if __ok.is_none() {
                                        state.offset = __save;
                                    }
                                }
                                {
                                    let __loop_start = state.offset;
                                    let __end = state.src_bytes.len();
                                    let mut __pos = state.offset;
                                    while __pos < __end {
                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                        if __b.is_ascii_digit() {
                                            __pos += 1;
                                        } else {
                                            break;
                                        }
                                    }
                                    if __pos < __loop_start + 1 as usize {
                                        return None;
                                    }
                                    state.offset = __pos;
                                }
                                Some(())
                            })();
                            if __ok.is_none() {
                                state.offset = __save;
                            }
                        }
                        {
                            let __end = state.src_bytes.len();
                            let mut __pos = state.offset;
                            while __pos < __end {
                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                if (__b.is_ascii_alphanumeric() || __b == b'_') {
                                    __pos += 1;
                                } else {
                                    break;
                                }
                            }
                            state.offset = __pos;
                        }
                        Some(())
                    })();
                    if __result.is_some() && state.offset > __start {
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        state.offset = __start;
                        None
                    }
                }
                    .is_none()
                {
                    return false;
                }
                let __matched = &state.src[__start..state.offset];
                if !__matched.is_empty() {
                    __builder.text(__matched);
                }
            };
            true
        }
    }
    pub fn float_lit_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__float_lit_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __bool_lit_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp34 = state.offset;
                    let __ok = (|| -> bool {
                        {
                            let __s = "true";
                            let __bytes = __s.as_bytes();
                            let __slc = match state.src_bytes.get(state.offset..) {
                                Some(s) if s.len() >= 4usize => s,
                                _ => return false,
                            };
                            if &__slc[..4usize] != __bytes {
                                return false;
                            }
                            __builder
                                .text(&state.src[state.offset..state.offset + 4usize]);
                            state.offset += 4usize;
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp34;
                    }
                    __ok
                } {
                    {
                        let __s = "false";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 5usize => s,
                            _ => return false,
                        };
                        if &__slc[..5usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 5usize]);
                        state.offset += 5usize;
                    };
                }
            };
            true
        }
    }
    pub fn bool_lit_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__bool_lit_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __string_lit_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    if state.src_bytes.get(state.offset).copied() != Some(b'"') {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(b'"');
                };
                {
                    let __start = state.offset;
                    if {
                        let __start = state.offset;
                        let __result: Option<()> = (|| {
                            {
                                let mut __rep_count: u32 = 0;
                                loop {
                                    let __save = state.offset;
                                    let __ok = (|| -> Option<()> {
                                        {
                                            let __save_alt = state.offset;
                                            let __alt_ok = (|| -> Option<()> {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                {
                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                    if !(!(__b == b'\n')) {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                }
                                                Some(())
                                            })();
                                            let __alt_ok = if __alt_ok.is_none() {
                                                state.offset = __save_alt;
                                                (|| -> Option<()> {
                                                    {
                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                        if !(!((__b == b'"' || __b == b'\\'))) {
                                                            return None;
                                                        }
                                                        state.offset += 1;
                                                    }
                                                    Some(())
                                                })()
                                            } else {
                                                __alt_ok
                                            };
                                            if __alt_ok.is_none() {
                                                return None;
                                            }
                                        }
                                        Some(())
                                    })();
                                    if __ok.is_none() {
                                        state.offset = __save;
                                        break;
                                    }
                                    if state.offset == __save {
                                        break;
                                    }
                                    __rep_count += 1;
                                }
                                if __rep_count < 0 {
                                    return None;
                                }
                            }
                            Some(())
                        })();
                        if __result.is_some() {
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            state.offset = __start;
                            None
                        }
                    }
                        .is_none()
                    {
                        return false;
                    }
                    let __matched = &state.src[__start..state.offset];
                    if !__matched.is_empty() {
                        __builder.text(__matched);
                    }
                };
                {
                    if state.src_bytes.get(state.offset).copied() != Some(b'"') {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(b'"');
                };
            };
            true
        }
    }
    pub fn string_lit_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__string_lit_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __add_op_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp35 = state.offset;
                    let __ok = (|| -> bool {
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'+') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'+');
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp35;
                    }
                    __ok
                } {
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'-') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'-');
                    };
                }
            };
            true
        }
    }
    pub fn add_op_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__add_op_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __binary_operators_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp38 = state.offset;
                    let __ok = (|| -> bool {
                        {
                            let __s = "<<";
                            let __bytes = __s.as_bytes();
                            let __slc = match state.src_bytes.get(state.offset..) {
                                Some(s) if s.len() >= 2usize => s,
                                _ => return false,
                            };
                            if &__slc[..2usize] != __bytes {
                                return false;
                            }
                            __builder
                                .text(&state.src[state.offset..state.offset + 2usize]);
                            state.offset += 2usize;
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp38;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp37 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    let __s = ">>";
                                    let __bytes = __s.as_bytes();
                                    let __slc = match state.src_bytes.get(state.offset..) {
                                        Some(s) if s.len() >= 2usize => s,
                                        _ => return false,
                                    };
                                    if &__slc[..2usize] != __bytes {
                                        return false;
                                    }
                                    __builder
                                        .text(&state.src[state.offset..state.offset + 2usize]);
                                    state.offset += 2usize;
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp37;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp36 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'-');
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp36;
                                    }
                                    __ok
                                } {
                                    return false;
                                }
                            }
                        }
                    }
                }
            };
            true
        }
    }
    pub fn binary_operators_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__binary_operators_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __import_path_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    if state.src_bytes.get(state.offset).copied() != Some(b'"') {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(b'"');
                };
                {
                    let __start = state.offset;
                    if {
                        let __start = state.offset;
                        let __result: Option<()> = (|| {
                            {
                                let mut __rep_count: u32 = 0;
                                loop {
                                    let __save = state.offset;
                                    let __ok = (|| -> Option<()> {
                                        {
                                            let __save_alt = state.offset;
                                            let __alt_ok = (|| -> Option<()> {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                {
                                                    let __b = *state.src_bytes.get(state.offset)?;
                                                    if !(!(__b == b'\n')) {
                                                        return None;
                                                    }
                                                    state.offset += 1;
                                                }
                                                Some(())
                                            })();
                                            let __alt_ok = if __alt_ok.is_none() {
                                                state.offset = __save_alt;
                                                (|| -> Option<()> {
                                                    {
                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                        if !(!((__b == b'"' || __b == b'\\'))) {
                                                            return None;
                                                        }
                                                        state.offset += 1;
                                                    }
                                                    Some(())
                                                })()
                                            } else {
                                                __alt_ok
                                            };
                                            if __alt_ok.is_none() {
                                                return None;
                                            }
                                        }
                                        Some(())
                                    })();
                                    if __ok.is_none() {
                                        state.offset = __save;
                                        break;
                                    }
                                    if state.offset == __save {
                                        break;
                                    }
                                    __rep_count += 1;
                                }
                                if __rep_count < 0 {
                                    return None;
                                }
                            }
                            Some(())
                        })();
                        if __result.is_some() {
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        } else {
                            state.offset = __start;
                            None
                        }
                    }
                        .is_none()
                    {
                        return false;
                    }
                    let __matched = &state.src[__start..state.offset];
                    if !__matched.is_empty() {
                        __builder.text(__matched);
                    }
                };
                {
                    if state.src_bytes.get(state.offset).copied() != Some(b'"') {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(b'"');
                };
            };
            true
        }
    }
    pub fn import_path_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__import_path_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __comment_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp41 = state.offset;
                    let __pretty_bcp42 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            let __ows39 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows39..state.offset]);
                            {
                                {
                                    let __s = "//";
                                    let __bytes = __s.as_bytes();
                                    let __slc = match state.src_bytes.get(state.offset..) {
                                        Some(s) if s.len() >= 2usize => s,
                                        _ => return false,
                                    };
                                    if &__slc[..2usize] != __bytes {
                                        return false;
                                    }
                                    __builder
                                        .text(&state.src[state.offset..state.offset + 2usize]);
                                    state.offset += 2usize;
                                };
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __result: Option<()> = (|| {
                                            {
                                                let __end = state.src_bytes.len();
                                                let mut __pos = state.offset;
                                                while __pos < __end {
                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                    if !(__b == b'\n') {
                                                        __pos += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                state.offset = __pos;
                                            }
                                            Some(())
                                        })();
                                        if __result.is_some() {
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            state.offset = __start;
                                            None
                                        }
                                    }
                                        .is_none()
                                    {
                                        return false;
                                    }
                                    let __matched = &state.src[__start..state.offset];
                                    if !__matched.is_empty() {
                                        __builder.text(__matched);
                                    }
                                };
                            };
                            let __ows40 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows40..state.offset]);
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp41;
                        __builder.restore(__pretty_bcp42);
                    }
                    __ok
                } {
                    return false;
                }
            };
            true
        }
    }
    pub fn comment_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__comment_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __type_annotation_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows43 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows44 = state.offset;
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b':') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b':');
                    };
                    __builder.text_inline_ws(&state.src[__ows43..__ows44]);
                    let __ows45 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows45..state.offset]);
                };
                if !Self::__type_name_prettify(state, __builder) {
                    return false;
                }
            };
            true
        }
    }
    pub fn type_annotation_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__type_annotation_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __ws_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows46 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows47 = state.offset;
                    {
                        let __s = "@ws";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 3usize => s,
                            _ => return false,
                        };
                        if &__slc[..3usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 3usize]);
                        state.offset += 3usize;
                    };
                    __builder.text_inline_ws(&state.src[__ows46..__ows47]);
                    let __ows48 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows48..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp51 = state.offset;
                        let __pretty_bcp52 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows49 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows49..state.offset]);
                                if !Self::__regex_prettify(state, __builder) {
                                    return false;
                                }
                                let __ows50 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows50..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp51;
                            __builder.restore(__pretty_bcp52);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp54 = state.offset;
                        let __pretty_bcp55 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp53 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp53;
                                    }
                                    __ok
                                } {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'.');
                                    };
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp54;
                            __builder.restore(__pretty_bcp55);
                        }
                        __ok
                    };
                    true
                };
            };
            true
        }
    }
    pub fn ws_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__ws_directive_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __lhs_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let __start = state.offset;
                if ::parse_that::scan_ident(state).is_none() {
                    return false;
                }
                let __matched = &state.src[__start..state.offset];
                if !__matched.is_empty() {
                    __builder.text(__matched);
                }
            };
            true
        }
    }
    pub fn lhs_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__lhs_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __host_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows56 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows57 = state.offset;
                    {
                        let __s = "@host";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 5usize => s,
                            _ => return false,
                        };
                        if &__slc[..5usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 5usize]);
                        state.offset += 5usize;
                    };
                    __builder.text_inline_ws(&state.src[__ows56..__ows57]);
                    let __ows58 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows58..state.offset]);
                };
                {
                    let __ows59 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows60 = state.offset;
                    {
                        let __start = state.offset;
                        if ::parse_that::scan_ident(state).is_none() {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                    __builder.text_inline_ws(&state.src[__ows59..__ows60]);
                    let __ows61 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows61..state.offset]);
                };
                {
                    let _ = {
                        let __pretty_cp69 = state.offset;
                        let __pretty_bcp70 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    let __ows62 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ows63 = state.offset;
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b':')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b':');
                                    };
                                    __builder.text_inline_ws(&state.src[__ows62..__ows63]);
                                    let __ows64 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows64..state.offset]);
                                };
                                {
                                    if !{
                                        let __pretty_cp67 = state.offset;
                                        let __pretty_bcp68 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows65 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder.text_inline_ws(&state.src[__ows65..state.offset]);
                                                if !Self::__type_name_prettify(state, __builder) {
                                                    return false;
                                                }
                                                let __ows66 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder.text_inline_ws(&state.src[__ows66..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp67;
                                            __builder.restore(__pretty_bcp68);
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp69;
                            __builder.restore(__pretty_bcp70);
                        }
                        __ok
                    };
                    true
                };
                {
                    let _ = {
                        let __pretty_cp72 = state.offset;
                        let __pretty_bcp73 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp71 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp71;
                                    }
                                    __ok
                                } {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'.');
                                    };
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp72;
                            __builder.restore(__pretty_bcp73);
                        }
                        __ok
                    };
                    true
                };
            };
            true
        }
    }
    pub fn host_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__host_directive_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __token_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows74 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows75 = state.offset;
                    {
                        let __s = "@token";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 6usize => s,
                            _ => return false,
                        };
                        if &__slc[..6usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 6usize]);
                        state.offset += 6usize;
                    };
                    __builder.text_inline_ws(&state.src[__ows74..__ows75]);
                    let __ows76 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows76..state.offset]);
                };
                {
                    let __ows77 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows78 = state.offset;
                    {
                        let __start = state.offset;
                        if ::parse_that::scan_ident(state).is_none() {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                    __builder.text_inline_ws(&state.src[__ows77..__ows78]);
                    let __ows79 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows79..state.offset]);
                };
                {
                    let _ = {
                        let __pretty_cp81 = state.offset;
                        let __pretty_bcp82 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp80 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp80;
                                    }
                                    __ok
                                } {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'.');
                                    };
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp81;
                            __builder.restore(__pretty_bcp82);
                        }
                        __ok
                    };
                    true
                };
            };
            true
        }
    }
    pub fn token_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__token_directive_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __debug_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows83 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows84 = state.offset;
                    {
                        let __s = "@debug";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 6usize => s,
                            _ => return false,
                        };
                        if &__slc[..6usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 6usize]);
                        state.offset += 6usize;
                    };
                    __builder.text_inline_ws(&state.src[__ows83..__ows84]);
                    let __ows85 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows85..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp89 = state.offset;
                        let __pretty_bcp90 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows87 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows87..state.offset]);
                                {
                                    if !{
                                        let __pretty_cp86 = state.offset;
                                        let __ok = (|| -> bool {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'*');
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp86;
                                        }
                                        __ok
                                    } {
                                        {
                                            let __start = state.offset;
                                            if ::parse_that::scan_ident(state).is_none() {
                                                return false;
                                            }
                                            let __matched = &state.src[__start..state.offset];
                                            if !__matched.is_empty() {
                                                __builder.text(__matched);
                                            }
                                        };
                                    }
                                };
                                let __ows88 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows88..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp89;
                            __builder.restore(__pretty_bcp90);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp92 = state.offset;
                        let __pretty_bcp93 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp91 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp91;
                                    }
                                    __ok
                                } {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'.');
                                    };
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp92;
                            __builder.restore(__pretty_bcp93);
                        }
                        __ok
                    };
                    true
                };
            };
            true
        }
    }
    pub fn debug_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__debug_directive_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __pretty_hint_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __start = state.offset;
                    if ::parse_that::scan_ident(state).is_none() {
                        return false;
                    }
                    let __matched = &state.src[__start..state.offset];
                    if !__matched.is_empty() {
                        __builder.text(__matched);
                    }
                };
                {
                    let _ = {
                        let __pretty_cp94 = state.offset;
                        let __pretty_bcp95 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'(')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'(');
                                };
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __scan = if __start >= state.src_bytes.len() {
                                            0
                                        } else {
                                            (::parse_that::memchr::memchr(
                                                b')',
                                                &state.src_bytes[__start..],
                                            ))
                                                .unwrap_or(state.src_bytes.len() - __start)
                                        };
                                        state.offset = __start + __scan;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    }
                                        .is_none()
                                    {
                                        return false;
                                    }
                                    let __matched = &state.src[__start..state.offset];
                                    if !__matched.is_empty() {
                                        __builder.text(__matched);
                                    }
                                };
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b')')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b')');
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp94;
                            __builder.restore(__pretty_bcp95);
                        }
                        __ok
                    };
                    true
                };
            };
            true
        }
    }
    pub fn pretty_hint_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__pretty_hint_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __import_items_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows96 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows97 = state.offset;
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'{') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'{');
                    };
                    __builder.text_inline_ws(&state.src[__ows96..__ows97]);
                    let __ows98 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows98..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp108 = state.offset;
                        let __pretty_bcp109 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows106 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows106..state.offset]);
                                {
                                    {
                                        let __start = state.offset;
                                        if ::parse_that::scan_ident(state).is_none() {
                                            return false;
                                        }
                                        let __matched = &state.src[__start..state.offset];
                                        if !__matched.is_empty() {
                                            __builder.text(__matched);
                                        }
                                    };
                                    {
                                        let mut __rep_count104 = 0usize;
                                        while __rep_count104 < 4294967295 {
                                            let __rep_cp105 = state.offset;
                                            if !{
                                                let __pretty_cp102 = state.offset;
                                                let __pretty_bcp103 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        {
                                                            let __ows99 = state.offset;
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            let __ows100 = state.offset;
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b',');
                                                            };
                                                            __builder.text_inline_ws(&state.src[__ows99..__ows100]);
                                                            let __ows101 = state.offset;
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            __builder
                                                                .text_inline_ws(&state.src[__ows101..state.offset]);
                                                        };
                                                        {
                                                            let __start = state.offset;
                                                            if ::parse_that::scan_ident(state).is_none() {
                                                                return false;
                                                            }
                                                            let __matched = &state.src[__start..state.offset];
                                                            if !__matched.is_empty() {
                                                                __builder.text(__matched);
                                                            }
                                                        };
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp102;
                                                    __builder.restore(__pretty_bcp103);
                                                }
                                                __ok
                                            } {
                                                state.offset = __rep_cp105;
                                                break;
                                            }
                                            if state.offset == __rep_cp105 {
                                                break;
                                            }
                                            __rep_count104 += 1;
                                        }
                                    };
                                };
                                let __ows107 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows107..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp108;
                            __builder.restore(__pretty_bcp109);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    if state.src_bytes.get(state.offset).copied() != Some(b'}') {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(b'}');
                };
            };
            true
        }
    }
    pub fn import_items_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__import_items_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __value_input_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __s = "input";
                    let __bytes = __s.as_bytes();
                    let __slc = match state.src_bytes.get(state.offset..) {
                        Some(s) if s.len() >= 5usize => s,
                        _ => return false,
                    };
                    if &__slc[..5usize] != __bytes {
                        return false;
                    }
                    __builder.text(&state.src[state.offset..state.offset + 5usize]);
                    state.offset += 5usize;
                };
                {
                    let mut __rep_count112 = 0usize;
                    while __rep_count112 < 4294967295 {
                        let __rep_cp113 = state.offset;
                        if !{
                            let __pretty_cp110 = state.offset;
                            let __pretty_bcp111 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'.');
                                    };
                                    {
                                        let __start = state.offset;
                                        if ::parse_that::scan_ident(state).is_none() {
                                            return false;
                                        }
                                        let __matched = &state.src[__start..state.offset];
                                        if !__matched.is_empty() {
                                            __builder.text(__matched);
                                        }
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp110;
                                __builder.restore(__pretty_bcp111);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp113;
                            break;
                        }
                        if state.offset == __rep_cp113 {
                            break;
                        }
                        __rep_count112 += 1;
                    }
                };
            };
            true
        }
    }
    pub fn value_input_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__value_input_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __value_path_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __start = state.offset;
                    if ::parse_that::scan_ident(state).is_none() {
                        return false;
                    }
                    let __matched = &state.src[__start..state.offset];
                    if !__matched.is_empty() {
                        __builder.text(__matched);
                    }
                };
                {
                    let mut __rep_count116 = 0usize;
                    while __rep_count116 < 4294967295 {
                        let __rep_cp117 = state.offset;
                        if !{
                            let __pretty_cp114 = state.offset;
                            let __pretty_bcp115 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __s = "::";
                                        let __bytes = __s.as_bytes();
                                        let __slc = match state.src_bytes.get(state.offset..) {
                                            Some(s) if s.len() >= 2usize => s,
                                            _ => return false,
                                        };
                                        if &__slc[..2usize] != __bytes {
                                            return false;
                                        }
                                        __builder
                                            .text(&state.src[state.offset..state.offset + 2usize]);
                                        state.offset += 2usize;
                                    };
                                    {
                                        let __start = state.offset;
                                        if ::parse_that::scan_ident(state).is_none() {
                                            return false;
                                        }
                                        let __matched = &state.src[__start..state.offset];
                                        if !__matched.is_empty() {
                                            __builder.text(__matched);
                                        }
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp114;
                                __builder.restore(__pretty_bcp115);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp117;
                            break;
                        }
                        if state.offset == __rep_cp117 {
                            break;
                        }
                        __rep_count116 += 1;
                    }
                };
            };
            true
        }
    }
    pub fn value_path_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__value_path_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __pretty_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows118 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows119 = state.offset;
                    {
                        let __s = "@pretty";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 7usize => s,
                            _ => return false,
                        };
                        if &__slc[..7usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 7usize]);
                        state.offset += 7usize;
                    };
                    __builder.text_inline_ws(&state.src[__ows118..__ows119]);
                    let __ows120 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows120..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp124 = state.offset;
                        let __pretty_bcp125 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows122 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows122..state.offset]);
                                {
                                    if !{
                                        let __pretty_cp121 = state.offset;
                                        let __ok = (|| -> bool {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'*');
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp121;
                                        }
                                        __ok
                                    } {
                                        {
                                            let __start = state.offset;
                                            if ::parse_that::scan_ident(state).is_none() {
                                                return false;
                                            }
                                            let __matched = &state.src[__start..state.offset];
                                            if !__matched.is_empty() {
                                                __builder.text(__matched);
                                            }
                                        };
                                    }
                                };
                                let __ows123 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows123..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp124;
                            __builder.restore(__pretty_bcp125);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    if !{
                        let __pretty_cp134 = state.offset;
                        let __pretty_bcp135 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows132 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows132..state.offset]);
                                {
                                    let __rep_start130 = state.offset;
                                    let __rep_bcp131 = __builder.checkpoint();
                                    let mut __rep_count128 = 0usize;
                                    while __rep_count128 < 4294967295 {
                                        let __rep_cp129 = state.offset;
                                        if !{
                                            let __pretty_cp126 = state.offset;
                                            let __pretty_bcp127 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__pretty_hint_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp126;
                                                __builder.restore(__pretty_bcp127);
                                            }
                                            __ok
                                        } {
                                            state.offset = __rep_cp129;
                                            break;
                                        }
                                        if state.offset == __rep_cp129 {
                                            break;
                                        }
                                        __rep_count128 += 1;
                                    }
                                    if __rep_count128 < 1 {
                                        state.offset = __rep_start130;
                                        __builder.restore(__rep_bcp131);
                                        return false;
                                    }
                                };
                                let __ows133 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows133..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp134;
                            __builder.restore(__pretty_bcp135);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp137 = state.offset;
                        let __pretty_bcp138 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp136 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp136;
                                    }
                                    __ok
                                } {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'.');
                                    };
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp137;
                            __builder.restore(__pretty_bcp138);
                        }
                        __ok
                    };
                    true
                };
            };
            true
        }
    }
    pub fn pretty_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__pretty_directive_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __import_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows139 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows140 = state.offset;
                    {
                        let __s = "@import";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 7usize => s,
                            _ => return false,
                        };
                        if &__slc[..7usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 7usize]);
                        state.offset += 7usize;
                    };
                    __builder.text_inline_ws(&state.src[__ows139..__ows140]);
                    let __ows141 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows141..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp153 = state.offset;
                        let __pretty_bcp154 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows151 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows151..state.offset]);
                                {
                                    if !{
                                        let __pretty_cp149 = state.offset;
                                        let __pretty_bcp150 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                {
                                                    if !{
                                                        let __pretty_cp144 = state.offset;
                                                        let __pretty_bcp145 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            {
                                                                let __ows142 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                __builder
                                                                    .text_inline_ws(&state.src[__ows142..state.offset]);
                                                                if !Self::__import_items_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                let __ows143 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                __builder
                                                                    .text_inline_ws(&state.src[__ows143..state.offset]);
                                                            };
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp144;
                                                            __builder.restore(__pretty_bcp145);
                                                        }
                                                        __ok
                                                    } {
                                                        return false;
                                                    }
                                                };
                                                {
                                                    let __ows146 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    let __ows147 = state.offset;
                                                    {
                                                        let __s = "from";
                                                        let __bytes = __s.as_bytes();
                                                        let __slc = match state.src_bytes.get(state.offset..) {
                                                            Some(s) if s.len() >= 4usize => s,
                                                            _ => return false,
                                                        };
                                                        if &__slc[..4usize] != __bytes {
                                                            return false;
                                                        }
                                                        __builder
                                                            .text(&state.src[state.offset..state.offset + 4usize]);
                                                        state.offset += 4usize;
                                                    };
                                                    __builder.text_inline_ws(&state.src[__ows146..__ows147]);
                                                    let __ows148 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows148..state.offset]);
                                                };
                                                if !Self::__import_path_prettify(state, __builder) {
                                                    return false;
                                                }
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp149;
                                            __builder.restore(__pretty_bcp150);
                                        }
                                        __ok
                                    } {
                                        if !Self::__import_path_prettify(state, __builder) {
                                            return false;
                                        }
                                    }
                                };
                                let __ows152 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows152..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp153;
                            __builder.restore(__pretty_bcp154);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp156 = state.offset;
                        let __pretty_bcp157 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp155 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp155;
                                    }
                                    __ok
                                } {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'.');
                                    };
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp156;
                            __builder.restore(__pretty_bcp157);
                        }
                        __ok
                    };
                    true
                };
            };
            true
        }
    }
    pub fn import_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__import_directive_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __value_mul_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !Self::__value_unary_prettify(state, __builder) {
                    return false;
                }
                {
                    let mut __rep_count164 = 0usize;
                    while __rep_count164 < 4294967295 {
                        let __rep_cp165 = state.offset;
                        if !{
                            let __pretty_cp162 = state.offset;
                            let __pretty_bcp163 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp160 = state.offset;
                                            let __pretty_bcp161 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows158 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows158..state.offset]);
                                                    if !Self::__mul_op_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows159 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows159..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp160;
                                                __builder.restore(__pretty_bcp161);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                    if !Self::__value_unary_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp162;
                                __builder.restore(__pretty_bcp163);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp165;
                            break;
                        }
                        if state.offset == __rep_cp165 {
                            break;
                        }
                        __rep_count164 += 1;
                    }
                };
            };
            true
        }
    }
    pub fn value_mul_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__value_mul_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __value_or_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !Self::__value_and_prettify(state, __builder) {
                    return false;
                }
                {
                    let mut __rep_count171 = 0usize;
                    while __rep_count171 < 4294967295 {
                        let __rep_cp172 = state.offset;
                        if !{
                            let __pretty_cp169 = state.offset;
                            let __pretty_bcp170 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows166 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows167 = state.offset;
                                        {
                                            let __s = "||";
                                            let __bytes = __s.as_bytes();
                                            let __slc = match state.src_bytes.get(state.offset..) {
                                                Some(s) if s.len() >= 2usize => s,
                                                _ => return false,
                                            };
                                            if &__slc[..2usize] != __bytes {
                                                return false;
                                            }
                                            __builder
                                                .text(&state.src[state.offset..state.offset + 2usize]);
                                            state.offset += 2usize;
                                        };
                                        __builder.text_inline_ws(&state.src[__ows166..__ows167]);
                                        let __ows168 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows168..state.offset]);
                                    };
                                    if !Self::__value_and_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp169;
                                __builder.restore(__pretty_bcp170);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp172;
                            break;
                        }
                        if state.offset == __rep_cp172 {
                            break;
                        }
                        __rep_count171 += 1;
                    }
                };
            };
            true
        }
    }
    pub fn value_or_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__value_or_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __value_add_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !Self::__value_mul_prettify(state, __builder) {
                    return false;
                }
                {
                    let mut __rep_count180 = 0usize;
                    while __rep_count180 < 4294967295 {
                        let __rep_cp181 = state.offset;
                        if !{
                            let __pretty_cp178 = state.offset;
                            let __pretty_bcp179 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp176 = state.offset;
                                            let __pretty_bcp177 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows174 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows174..state.offset]);
                                                    {
                                                        if !{
                                                            let __pretty_cp173 = state.offset;
                                                            let __ok = (|| -> bool {
                                                                {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'+')
                                                                    {
                                                                        return false;
                                                                    }
                                                                    state.offset += 1;
                                                                    __builder.char(b'+');
                                                                };
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp173;
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b'-');
                                                            };
                                                        }
                                                    };
                                                    let __ows175 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows175..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp176;
                                                __builder.restore(__pretty_bcp177);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                    if !Self::__value_mul_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp178;
                                __builder.restore(__pretty_bcp179);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp181;
                            break;
                        }
                        if state.offset == __rep_cp181 {
                            break;
                        }
                        __rep_count180 += 1;
                    }
                };
            };
            true
        }
    }
    pub fn value_add_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__value_add_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __value_cmp_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !Self::__value_add_prettify(state, __builder) {
                    return false;
                }
                {
                    let mut __rep_count188 = 0usize;
                    while __rep_count188 < 4294967295 {
                        let __rep_cp189 = state.offset;
                        if !{
                            let __pretty_cp186 = state.offset;
                            let __pretty_bcp187 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp184 = state.offset;
                                            let __pretty_bcp185 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows182 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows182..state.offset]);
                                                    if !Self::__cmp_op_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows183 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows183..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp184;
                                                __builder.restore(__pretty_bcp185);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                    if !Self::__value_add_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp186;
                                __builder.restore(__pretty_bcp187);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp189;
                            break;
                        }
                        if state.offset == __rep_cp189 {
                            break;
                        }
                        __rep_count188 += 1;
                    }
                };
            };
            true
        }
    }
    pub fn value_cmp_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__value_cmp_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __value_and_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !Self::__value_cmp_prettify(state, __builder) {
                    return false;
                }
                {
                    let mut __rep_count195 = 0usize;
                    while __rep_count195 < 4294967295 {
                        let __rep_cp196 = state.offset;
                        if !{
                            let __pretty_cp193 = state.offset;
                            let __pretty_bcp194 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows190 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows191 = state.offset;
                                        {
                                            let __s = "&&";
                                            let __bytes = __s.as_bytes();
                                            let __slc = match state.src_bytes.get(state.offset..) {
                                                Some(s) if s.len() >= 2usize => s,
                                                _ => return false,
                                            };
                                            if &__slc[..2usize] != __bytes {
                                                return false;
                                            }
                                            __builder
                                                .text(&state.src[state.offset..state.offset + 2usize]);
                                            state.offset += 2usize;
                                        };
                                        __builder.text_inline_ws(&state.src[__ows190..__ows191]);
                                        let __ows192 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows192..state.offset]);
                                    };
                                    if !Self::__value_cmp_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp193;
                                __builder.restore(__pretty_bcp194);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp196;
                            break;
                        }
                        if state.offset == __rep_cp196 {
                            break;
                        }
                        __rep_count195 += 1;
                    }
                };
            };
            true
        }
    }
    pub fn value_and_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__value_and_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __value_closure_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    if state.src_bytes.get(state.offset).copied() != Some(b'|') {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(b'|');
                };
                {
                    let __start = state.offset;
                    if ::parse_that::scan_ident(state).is_none() {
                        return false;
                    }
                    let __matched = &state.src[__start..state.offset];
                    if !__matched.is_empty() {
                        __builder.text(__matched);
                    }
                };
                {
                    let mut __rep_count202 = 0usize;
                    while __rep_count202 < 4294967295 {
                        let __rep_cp203 = state.offset;
                        if !{
                            let __pretty_cp200 = state.offset;
                            let __pretty_bcp201 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows197 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows198 = state.offset;
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b',');
                                        };
                                        __builder.text_inline_ws(&state.src[__ows197..__ows198]);
                                        let __ows199 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows199..state.offset]);
                                    };
                                    {
                                        let __start = state.offset;
                                        if ::parse_that::scan_ident(state).is_none() {
                                            return false;
                                        }
                                        let __matched = &state.src[__start..state.offset];
                                        if !__matched.is_empty() {
                                            __builder.text(__matched);
                                        }
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp200;
                                __builder.restore(__pretty_bcp201);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp203;
                            break;
                        }
                        if state.offset == __rep_cp203 {
                            break;
                        }
                        __rep_count202 += 1;
                    }
                };
                {
                    if state.src_bytes.get(state.offset).copied() != Some(b'|') {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(b'|');
                };
                if !Self::__value_expr_prettify(state, __builder) {
                    return false;
                }
            };
            true
        }
    }
    pub fn value_closure_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__value_closure_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __value_fn_call_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !Self::__value_path_prettify(state, __builder) {
                    return false;
                }
                {
                    if state.src_bytes.get(state.offset).copied() != Some(b'(') {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(b'(');
                };
                {
                    let _ = {
                        let __pretty_cp211 = state.offset;
                        let __pretty_bcp212 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !Self::__value_expr_prettify(state, __builder) {
                                    return false;
                                }
                                {
                                    let mut __rep_count209 = 0usize;
                                    while __rep_count209 < 4294967295 {
                                        let __rep_cp210 = state.offset;
                                        if !{
                                            let __pretty_cp207 = state.offset;
                                            let __pretty_bcp208 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    {
                                                        let __ows204 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        let __ows205 = state.offset;
                                                        {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                            {
                                                                return false;
                                                            }
                                                            state.offset += 1;
                                                            __builder.char(b',');
                                                        };
                                                        __builder.text_inline_ws(&state.src[__ows204..__ows205]);
                                                        let __ows206 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows206..state.offset]);
                                                    };
                                                    if !Self::__value_expr_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp207;
                                                __builder.restore(__pretty_bcp208);
                                            }
                                            __ok
                                        } {
                                            state.offset = __rep_cp210;
                                            break;
                                        }
                                        if state.offset == __rep_cp210 {
                                            break;
                                        }
                                        __rep_count209 += 1;
                                    }
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp211;
                            __builder.restore(__pretty_bcp212);
                        }
                        __ok
                    };
                    true
                };
                {
                    if state.src_bytes.get(state.offset).copied() != Some(b')') {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(b')');
                };
            };
            true
        }
    }
    pub fn value_fn_call_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__value_fn_call_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __value_expr_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp213 = state.offset;
                    let __pretty_bcp214 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        if !Self::__value_closure_prettify(state, __builder) {
                            return false;
                        }
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp213;
                        __builder.restore(__pretty_bcp214);
                    }
                    __ok
                } {
                    if !Self::__value_or_prettify(state, __builder) {
                        return false;
                    }
                }
            };
            true
        }
    }
    pub fn value_expr_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__value_expr_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __value_atom_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp233 = state.offset;
                    let __ok = (|| -> bool {
                        {
                            let __start = state.offset;
                            if {
                                let __start = state.offset;
                                let __result: Option<()> = (|| {
                                    {
                                        let __save_alt = state.offset;
                                        let __alt_ok = (|| -> Option<()> {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'0')
                                            {
                                                return None;
                                            }
                                            state.offset += 1;
                                            {
                                                let __b = *state.src_bytes.get(state.offset)?;
                                                if !((__b == b'X' || __b == b'x')) {
                                                    return None;
                                                }
                                                state.offset += 1;
                                            }
                                            {
                                                let __loop_start = state.offset;
                                                let __end = state.src_bytes.len();
                                                let mut __pos = state.offset;
                                                while __pos < __end {
                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                    if __b.is_ascii_hexdigit() {
                                                        __pos += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                if __pos < __loop_start + 1 as usize {
                                                    return None;
                                                }
                                                state.offset = __pos;
                                            }
                                            {
                                                let __end = state.src_bytes.len();
                                                let mut __pos = state.offset;
                                                while __pos < __end {
                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                    if (__b.is_ascii_alphanumeric() || __b == b'_') {
                                                        __pos += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                state.offset = __pos;
                                            }
                                            Some(())
                                        })();
                                        let __alt_ok = if __alt_ok.is_none() {
                                            state.offset = __save_alt;
                                            (|| -> Option<()> {
                                                {
                                                    let __loop_start = state.offset;
                                                    let __end = state.src_bytes.len();
                                                    let mut __pos = state.offset;
                                                    while __pos < __end {
                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                        if __b.is_ascii_digit() {
                                                            __pos += 1;
                                                        } else {
                                                            break;
                                                        }
                                                    }
                                                    if __pos < __loop_start + 1 as usize {
                                                        return None;
                                                    }
                                                    state.offset = __pos;
                                                }
                                                {
                                                    let __end = state.src_bytes.len();
                                                    let mut __pos = state.offset;
                                                    while __pos < __end {
                                                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                        if (__b.is_ascii_alphanumeric() || __b == b'_') {
                                                            __pos += 1;
                                                        } else {
                                                            break;
                                                        }
                                                    }
                                                    state.offset = __pos;
                                                }
                                                Some(())
                                            })()
                                        } else {
                                            __alt_ok
                                        };
                                        if __alt_ok.is_none() {
                                            return None;
                                        }
                                    }
                                    Some(())
                                })();
                                if __result.is_some() && state.offset > __start {
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    state.offset = __start;
                                    None
                                }
                            }
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp233;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp232 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __result: Option<()> = (|| {
                                            {
                                                let __end = state.src_bytes.len();
                                                let mut __pos = state.offset;
                                                while __pos < __end {
                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                    if __b.is_ascii_digit() {
                                                        __pos += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                state.offset = __pos;
                                            }
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return None;
                                            }
                                            state.offset += 1;
                                            {
                                                let __loop_start = state.offset;
                                                let __end = state.src_bytes.len();
                                                let mut __pos = state.offset;
                                                while __pos < __end {
                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                    if __b.is_ascii_digit() {
                                                        __pos += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                if __pos < __loop_start + 1 as usize {
                                                    return None;
                                                }
                                                state.offset = __pos;
                                            }
                                            {
                                                let __save = state.offset;
                                                let __ok = (|| -> Option<()> {
                                                    {
                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                        if !((__b == b'E' || __b == b'e')) {
                                                            return None;
                                                        }
                                                        state.offset += 1;
                                                    }
                                                    {
                                                        let __save = state.offset;
                                                        let __ok = (|| -> Option<()> {
                                                            {
                                                                let __b = *state.src_bytes.get(state.offset)?;
                                                                if !((__b == b'+' || __b == b'-')) {
                                                                    return None;
                                                                }
                                                                state.offset += 1;
                                                            }
                                                            Some(())
                                                        })();
                                                        if __ok.is_none() {
                                                            state.offset = __save;
                                                        }
                                                    }
                                                    {
                                                        let __loop_start = state.offset;
                                                        let __end = state.src_bytes.len();
                                                        let mut __pos = state.offset;
                                                        while __pos < __end {
                                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                            if __b.is_ascii_digit() {
                                                                __pos += 1;
                                                            } else {
                                                                break;
                                                            }
                                                        }
                                                        if __pos < __loop_start + 1 as usize {
                                                            return None;
                                                        }
                                                        state.offset = __pos;
                                                    }
                                                    Some(())
                                                })();
                                                if __ok.is_none() {
                                                    state.offset = __save;
                                                }
                                            }
                                            {
                                                let __end = state.src_bytes.len();
                                                let mut __pos = state.offset;
                                                while __pos < __end {
                                                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                    if (__b.is_ascii_alphanumeric() || __b == b'_') {
                                                        __pos += 1;
                                                    } else {
                                                        break;
                                                    }
                                                }
                                                state.offset = __pos;
                                            }
                                            Some(())
                                        })();
                                        if __result.is_some() && state.offset > __start {
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            state.offset = __start;
                                            None
                                        }
                                    }
                                        .is_none()
                                    {
                                        return false;
                                    }
                                    let __matched = &state.src[__start..state.offset];
                                    if !__matched.is_empty() {
                                        __builder.text(__matched);
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp232;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp230 = state.offset;
                                    let __pretty_bcp231 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            if !{
                                                let __pretty_cp215 = state.offset;
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __s = "true";
                                                        let __bytes = __s.as_bytes();
                                                        let __slc = match state.src_bytes.get(state.offset..) {
                                                            Some(s) if s.len() >= 4usize => s,
                                                            _ => return false,
                                                        };
                                                        if &__slc[..4usize] != __bytes {
                                                            return false;
                                                        }
                                                        __builder
                                                            .text(&state.src[state.offset..state.offset + 4usize]);
                                                        state.offset += 4usize;
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp215;
                                                }
                                                __ok
                                            } {
                                                {
                                                    let __s = "false";
                                                    let __bytes = __s.as_bytes();
                                                    let __slc = match state.src_bytes.get(state.offset..) {
                                                        Some(s) if s.len() >= 5usize => s,
                                                        _ => return false,
                                                    };
                                                    if &__slc[..5usize] != __bytes {
                                                        return false;
                                                    }
                                                    __builder
                                                        .text(&state.src[state.offset..state.offset + 5usize]);
                                                    state.offset += 5usize;
                                                };
                                            }
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp230;
                                        __builder.restore(__pretty_bcp231);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp228 = state.offset;
                                            let __pretty_bcp229 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    {
                                                        if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                                        {
                                                            return false;
                                                        }
                                                        state.offset += 1;
                                                        __builder.char(b'"');
                                                    };
                                                    {
                                                        let __start = state.offset;
                                                        if {
                                                            let __start = state.offset;
                                                            let __result: Option<()> = (|| {
                                                                {
                                                                    let mut __rep_count: u32 = 0;
                                                                    loop {
                                                                        let __save = state.offset;
                                                                        let __ok = (|| -> Option<()> {
                                                                            {
                                                                                let __save_alt = state.offset;
                                                                                let __alt_ok = (|| -> Option<()> {
                                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'\\')
                                                                                    {
                                                                                        return None;
                                                                                    }
                                                                                    state.offset += 1;
                                                                                    {
                                                                                        let __b = *state.src_bytes.get(state.offset)?;
                                                                                        if !(!(__b == b'\n')) {
                                                                                            return None;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                    }
                                                                                    Some(())
                                                                                })();
                                                                                let __alt_ok = if __alt_ok.is_none() {
                                                                                    state.offset = __save_alt;
                                                                                    (|| -> Option<()> {
                                                                                        {
                                                                                            let __b = *state.src_bytes.get(state.offset)?;
                                                                                            if !(!((__b == b'"' || __b == b'\\'))) {
                                                                                                return None;
                                                                                            }
                                                                                            state.offset += 1;
                                                                                        }
                                                                                        Some(())
                                                                                    })()
                                                                                } else {
                                                                                    __alt_ok
                                                                                };
                                                                                if __alt_ok.is_none() {
                                                                                    return None;
                                                                                }
                                                                            }
                                                                            Some(())
                                                                        })();
                                                                        if __ok.is_none() {
                                                                            state.offset = __save;
                                                                            break;
                                                                        }
                                                                        if state.offset == __save {
                                                                            break;
                                                                        }
                                                                        __rep_count += 1;
                                                                    }
                                                                    if __rep_count < 0 {
                                                                        return None;
                                                                    }
                                                                }
                                                                Some(())
                                                            })();
                                                            if __result.is_some() {
                                                                Some(
                                                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                                                )
                                                            } else {
                                                                state.offset = __start;
                                                                None
                                                            }
                                                        }
                                                            .is_none()
                                                        {
                                                            return false;
                                                        }
                                                        let __matched = &state.src[__start..state.offset];
                                                        if !__matched.is_empty() {
                                                            __builder.text(__matched);
                                                        }
                                                    };
                                                    {
                                                        if state.src_bytes.get(state.offset).copied() != Some(b'"')
                                                        {
                                                            return false;
                                                        }
                                                        state.offset += 1;
                                                        __builder.char(b'"');
                                                    };
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp228;
                                                __builder.restore(__pretty_bcp229);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp226 = state.offset;
                                                    let __pretty_bcp227 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        if !Self::__value_fn_call_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp226;
                                                        __builder.restore(__pretty_bcp227);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp224 = state.offset;
                                                            let __pretty_bcp225 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                if !Self::__value_input_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp224;
                                                                __builder.restore(__pretty_bcp225);
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp222 = state.offset;
                                                                    let __pretty_bcp223 = __builder.checkpoint();
                                                                    let __ok = (|| -> bool {
                                                                        if !Self::__value_path_prettify(state, __builder) {
                                                                            return false;
                                                                        }
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp222;
                                                                        __builder.restore(__pretty_bcp223);
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp220 = state.offset;
                                                                            let __pretty_bcp221 = __builder.checkpoint();
                                                                            let __ok = (|| -> bool {
                                                                                {
                                                                                    {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'(')
                                                                                        {
                                                                                            return false;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        __builder.char(b'(');
                                                                                    };
                                                                                    {
                                                                                        if !{
                                                                                            let __pretty_cp218 = state.offset;
                                                                                            let __pretty_bcp219 = __builder.checkpoint();
                                                                                            let __ok = (|| -> bool {
                                                                                                {
                                                                                                    let __ows216 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows216..state.offset]);
                                                                                                    if !Self::__value_expr_prettify(state, __builder) {
                                                                                                        return false;
                                                                                                    }
                                                                                                    let __ows217 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows217..state.offset]);
                                                                                                };
                                                                                                true
                                                                                            })();
                                                                                            if !__ok {
                                                                                                state.offset = __pretty_cp218;
                                                                                                __builder.restore(__pretty_bcp219);
                                                                                            }
                                                                                            __ok
                                                                                        } {
                                                                                            return false;
                                                                                        }
                                                                                    };
                                                                                    {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b')')
                                                                                        {
                                                                                            return false;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        __builder.char(b')');
                                                                                    };
                                                                                };
                                                                                true
                                                                            })();
                                                                            if !__ok {
                                                                                state.offset = __pretty_cp220;
                                                                                __builder.restore(__pretty_bcp221);
                                                                            }
                                                                            __ok
                                                                        } {
                                                                            return false;
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };
            true
        }
    }
    pub fn value_atom_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__value_atom_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __value_unary_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp235 = state.offset;
                    let __pretty_bcp236 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            {
                                if !{
                                    let __pretty_cp234 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'!')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'!');
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp234;
                                    }
                                    __ok
                                } {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'-');
                                    };
                                }
                            };
                            if !Self::__value_atom_prettify(state, __builder) {
                                return false;
                            }
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp235;
                        __builder.restore(__pretty_bcp236);
                    }
                    __ok
                } {
                    if !Self::__value_atom_prettify(state, __builder) {
                        return false;
                    }
                }
            };
            true
        }
    }
    pub fn value_unary_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__value_unary_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __alternation_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        __builder.group_open();
        let __pretty_ok = {
            {
                {
                    let __rep_start247 = state.offset;
                    let __rep_bcp248 = __builder.checkpoint();
                    let mut __rep_count245 = 0usize;
                    while __rep_count245 < 4294967295 {
                        let __rep_cp246 = state.offset;
                        if !{
                            let __pretty_cp243 = state.offset;
                            let __pretty_bcp244 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp239 = state.offset;
                                            let __pretty_bcp240 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows237 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows237..state.offset]);
                                                    if !Self::__concatenation_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows238 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows238..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp239;
                                                __builder.restore(__pretty_bcp240);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                    {
                                        let _ = {
                                            let __pretty_cp241 = state.offset;
                                            let __pretty_bcp242 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    if state.src_bytes.get(state.offset).copied() != Some(b'|')
                                                    {
                                                        return false;
                                                    }
                                                    state.offset += 1;
                                                    __builder.char(b'|');
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp241;
                                                __builder.restore(__pretty_bcp242);
                                            }
                                            __ok
                                        };
                                        true
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp243;
                                __builder.restore(__pretty_bcp244);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp246;
                            break;
                        }
                        if state.offset == __rep_cp246 {
                            break;
                        }
                        __rep_count245 += 1;
                    }
                    if __rep_count245 < 1 {
                        state.offset = __rep_start247;
                        __builder.restore(__rep_bcp248);
                        return false;
                    }
                };
                true
            }
        };
        __builder.group_close();
        __pretty_ok
    }
    pub fn alternation_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__alternation_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __call_arg_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let __rep_start259 = state.offset;
                let __rep_bcp260 = __builder.checkpoint();
                let mut __rep_count257 = 0usize;
                while __rep_count257 < 4294967295 {
                    let __rep_cp258 = state.offset;
                    if !{
                        let __pretty_cp255 = state.offset;
                        let __pretty_bcp256 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    if !{
                                        let __pretty_cp251 = state.offset;
                                        let __pretty_bcp252 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows249 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows249..state.offset]);
                                                if !Self::__binary_factor_prettify(state, __builder) {
                                                    return false;
                                                }
                                                let __ows250 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows250..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp251;
                                            __builder.restore(__pretty_bcp252);
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                };
                                {
                                    let _ = {
                                        let __pretty_cp253 = state.offset;
                                        let __pretty_bcp254 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'|')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'|');
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp253;
                                            __builder.restore(__pretty_bcp254);
                                        }
                                        __ok
                                    };
                                    true
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp255;
                            __builder.restore(__pretty_bcp256);
                        }
                        __ok
                    } {
                        state.offset = __rep_cp258;
                        break;
                    }
                    if state.offset == __rep_cp258 {
                        break;
                    }
                    __rep_count257 += 1;
                }
                if __rep_count257 < 1 {
                    state.offset = __rep_start259;
                    __builder.restore(__rep_bcp260);
                    return false;
                }
            };
            true
        }
    }
    pub fn call_arg_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__call_arg_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __concatenation_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let __rep_start271 = state.offset;
                let __rep_bcp272 = __builder.checkpoint();
                let mut __rep_count269 = 0usize;
                while __rep_count269 < 4294967295 {
                    let __rep_cp270 = state.offset;
                    if !{
                        let __pretty_cp267 = state.offset;
                        let __pretty_bcp268 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    if !{
                                        let __pretty_cp263 = state.offset;
                                        let __pretty_bcp264 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows261 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows261..state.offset]);
                                                if !Self::__binary_factor_prettify(state, __builder) {
                                                    return false;
                                                }
                                                let __ows262 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows262..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp263;
                                            __builder.restore(__pretty_bcp264);
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                };
                                {
                                    let _ = {
                                        let __pretty_cp265 = state.offset;
                                        let __pretty_bcp266 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b',');
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp265;
                                            __builder.restore(__pretty_bcp266);
                                        }
                                        __ok
                                    };
                                    true
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp267;
                            __builder.restore(__pretty_bcp268);
                        }
                        __ok
                    } {
                        state.offset = __rep_cp270;
                        break;
                    }
                    if state.offset == __rep_cp270 {
                        break;
                    }
                    __rep_count269 += 1;
                }
                if __rep_count269 < 1 {
                    state.offset = __rep_start271;
                    __builder.restore(__rep_bcp272);
                    return false;
                }
            };
            true
        }
    }
    pub fn concatenation_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__concatenation_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __closure_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    if state.src_bytes.get(state.offset).copied() != Some(b'|') {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(b'|');
                };
                {
                    let __start = state.offset;
                    if ::parse_that::scan_ident(state).is_none() {
                        return false;
                    }
                    let __matched = &state.src[__start..state.offset];
                    if !__matched.is_empty() {
                        __builder.text(__matched);
                    }
                };
                {
                    let mut __rep_count278 = 0usize;
                    while __rep_count278 < 4294967295 {
                        let __rep_cp279 = state.offset;
                        if !{
                            let __pretty_cp276 = state.offset;
                            let __pretty_bcp277 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows273 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows274 = state.offset;
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b',');
                                        };
                                        __builder.text_inline_ws(&state.src[__ows273..__ows274]);
                                        let __ows275 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows275..state.offset]);
                                    };
                                    {
                                        let __start = state.offset;
                                        if ::parse_that::scan_ident(state).is_none() {
                                            return false;
                                        }
                                        let __matched = &state.src[__start..state.offset];
                                        if !__matched.is_empty() {
                                            __builder.text(__matched);
                                        }
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp276;
                                __builder.restore(__pretty_bcp277);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp279;
                            break;
                        }
                        if state.offset == __rep_cp279 {
                            break;
                        }
                        __rep_count278 += 1;
                    }
                };
                {
                    let __ows280 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows281 = state.offset;
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'|') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'|');
                    };
                    __builder.text_inline_ws(&state.src[__ows280..__ows281]);
                    let __ows282 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows282..state.offset]);
                };
                if !Self::__rhs_prettify(state, __builder) {
                    return false;
                }
            };
            true
        }
    }
    pub fn closure_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__closure_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __term_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp331 = state.offset;
                    let __ok = (|| -> bool {
                        {
                            let __s = "ε";
                            let __bytes = __s.as_bytes();
                            let __slc = match state.src_bytes.get(state.offset..) {
                                Some(s) if s.len() >= 2usize => s,
                                _ => return false,
                            };
                            if &__slc[..2usize] != __bytes {
                                return false;
                            }
                            __builder
                                .text(&state.src[state.offset..state.offset + 2usize]);
                            state.offset += 2usize;
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp331;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp330 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    let __s = "epsilon";
                                    let __bytes = __s.as_bytes();
                                    let __slc = match state.src_bytes.get(state.offset..) {
                                        Some(s) if s.len() >= 7usize => s,
                                        _ => return false,
                                    };
                                    if &__slc[..7usize] != __bytes {
                                        return false;
                                    }
                                    __builder
                                        .text(&state.src[state.offset..state.offset + 7usize]);
                                    state.offset += 7usize;
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp330;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp328 = state.offset;
                                    let __pretty_bcp329 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            {
                                                let __start = state.offset;
                                                if ::parse_that::scan_ident(state).is_none() {
                                                    return false;
                                                }
                                                let __matched = &state.src[__start..state.offset];
                                                if !__matched.is_empty() {
                                                    __builder.text(__matched);
                                                }
                                            };
                                            {
                                                let _ = {
                                                    let __pretty_cp298 = state.offset;
                                                    let __pretty_bcp299 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        {
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'(')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b'(');
                                                            };
                                                            {
                                                                if !{
                                                                    let __pretty_cp285 = state.offset;
                                                                    let __pretty_bcp286 = __builder.checkpoint();
                                                                    let __ok = (|| -> bool {
                                                                        {
                                                                            let __ows283 = state.offset;
                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                            __builder
                                                                                .text_inline_ws(&state.src[__ows283..state.offset]);
                                                                            if !Self::__call_arg_prettify(state, __builder) {
                                                                                return false;
                                                                            }
                                                                            let __ows284 = state.offset;
                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                            __builder
                                                                                .text_inline_ws(&state.src[__ows284..state.offset]);
                                                                        };
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp285;
                                                                        __builder.restore(__pretty_bcp286);
                                                                    }
                                                                    __ok
                                                                } {
                                                                    return false;
                                                                }
                                                            };
                                                            {
                                                                let mut __rep_count296 = 0usize;
                                                                while __rep_count296 < 4294967295 {
                                                                    let __rep_cp297 = state.offset;
                                                                    if !{
                                                                        let __pretty_cp294 = state.offset;
                                                                        let __pretty_bcp295 = __builder.checkpoint();
                                                                        let __ok = (|| -> bool {
                                                                            {
                                                                                {
                                                                                    let __ows287 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    let __ows288 = state.offset;
                                                                                    {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                                        {
                                                                                            return false;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        __builder.char(b',');
                                                                                    };
                                                                                    __builder.text_inline_ws(&state.src[__ows287..__ows288]);
                                                                                    let __ows289 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    __builder
                                                                                        .text_inline_ws(&state.src[__ows289..state.offset]);
                                                                                };
                                                                                {
                                                                                    if !{
                                                                                        let __pretty_cp292 = state.offset;
                                                                                        let __pretty_bcp293 = __builder.checkpoint();
                                                                                        let __ok = (|| -> bool {
                                                                                            {
                                                                                                let __ows290 = state.offset;
                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                __builder
                                                                                                    .text_inline_ws(&state.src[__ows290..state.offset]);
                                                                                                if !Self::__call_arg_prettify(state, __builder) {
                                                                                                    return false;
                                                                                                }
                                                                                                let __ows291 = state.offset;
                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                __builder
                                                                                                    .text_inline_ws(&state.src[__ows291..state.offset]);
                                                                                            };
                                                                                            true
                                                                                        })();
                                                                                        if !__ok {
                                                                                            state.offset = __pretty_cp292;
                                                                                            __builder.restore(__pretty_bcp293);
                                                                                        }
                                                                                        __ok
                                                                                    } {
                                                                                        return false;
                                                                                    }
                                                                                };
                                                                            };
                                                                            true
                                                                        })();
                                                                        if !__ok {
                                                                            state.offset = __pretty_cp294;
                                                                            __builder.restore(__pretty_bcp295);
                                                                        }
                                                                        __ok
                                                                    } {
                                                                        state.offset = __rep_cp297;
                                                                        break;
                                                                    }
                                                                    if state.offset == __rep_cp297 {
                                                                        break;
                                                                    }
                                                                    __rep_count296 += 1;
                                                                }
                                                            };
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b')')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b')');
                                                            };
                                                        };
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp298;
                                                        __builder.restore(__pretty_bcp299);
                                                    }
                                                    __ok
                                                };
                                                true
                                            };
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp328;
                                        __builder.restore(__pretty_bcp329);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp326 = state.offset;
                                            let __pretty_bcp327 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__literal_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp326;
                                                __builder.restore(__pretty_bcp327);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp324 = state.offset;
                                                    let __pretty_bcp325 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        if !Self::__regex_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp324;
                                                        __builder.restore(__pretty_bcp325);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp322 = state.offset;
                                                            let __pretty_bcp323 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                {
                                                                    {
                                                                        let __s = "@{";
                                                                        let __bytes = __s.as_bytes();
                                                                        let __slc = match state.src_bytes.get(state.offset..) {
                                                                            Some(s) if s.len() >= 2usize => s,
                                                                            _ => return false,
                                                                        };
                                                                        if &__slc[..2usize] != __bytes {
                                                                            return false;
                                                                        }
                                                                        __builder
                                                                            .text(&state.src[state.offset..state.offset + 2usize]);
                                                                        state.offset += 2usize;
                                                                    };
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp302 = state.offset;
                                                                            let __pretty_bcp303 = __builder.checkpoint();
                                                                            let __ok = (|| -> bool {
                                                                                {
                                                                                    let __ows300 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    __builder
                                                                                        .text_inline_ws(&state.src[__ows300..state.offset]);
                                                                                    if !Self::__rhs_prettify(state, __builder) {
                                                                                        return false;
                                                                                    }
                                                                                    let __ows301 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    __builder
                                                                                        .text_inline_ws(&state.src[__ows301..state.offset]);
                                                                                };
                                                                                true
                                                                            })();
                                                                            if !__ok {
                                                                                state.offset = __pretty_cp302;
                                                                                __builder.restore(__pretty_bcp303);
                                                                            }
                                                                            __ok
                                                                        } {
                                                                            return false;
                                                                        }
                                                                    };
                                                                    {
                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'}')
                                                                        {
                                                                            return false;
                                                                        }
                                                                        state.offset += 1;
                                                                        __builder.char(b'}');
                                                                    };
                                                                };
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp322;
                                                                __builder.restore(__pretty_bcp323);
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp320 = state.offset;
                                                                    let __pretty_bcp321 = __builder.checkpoint();
                                                                    let __ok = (|| -> bool {
                                                                        {
                                                                            {
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'(')
                                                                                {
                                                                                    return false;
                                                                                }
                                                                                state.offset += 1;
                                                                                __builder.char(b'(');
                                                                            };
                                                                            {
                                                                                if !{
                                                                                    let __pretty_cp306 = state.offset;
                                                                                    let __pretty_bcp307 = __builder.checkpoint();
                                                                                    let __ok = (|| -> bool {
                                                                                        {
                                                                                            let __ows304 = state.offset;
                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                            __builder
                                                                                                .text_inline_ws(&state.src[__ows304..state.offset]);
                                                                                            if !Self::__rhs_prettify(state, __builder) {
                                                                                                return false;
                                                                                            }
                                                                                            let __ows305 = state.offset;
                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                            __builder
                                                                                                .text_inline_ws(&state.src[__ows305..state.offset]);
                                                                                        };
                                                                                        true
                                                                                    })();
                                                                                    if !__ok {
                                                                                        state.offset = __pretty_cp306;
                                                                                        __builder.restore(__pretty_bcp307);
                                                                                    }
                                                                                    __ok
                                                                                } {
                                                                                    return false;
                                                                                }
                                                                            };
                                                                            {
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b')')
                                                                                {
                                                                                    return false;
                                                                                }
                                                                                state.offset += 1;
                                                                                __builder.char(b')');
                                                                            };
                                                                        };
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp320;
                                                                        __builder.restore(__pretty_bcp321);
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp318 = state.offset;
                                                                            let __pretty_bcp319 = __builder.checkpoint();
                                                                            let __ok = (|| -> bool {
                                                                                {
                                                                                    {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b'[')
                                                                                        {
                                                                                            return false;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        __builder.char(b'[');
                                                                                    };
                                                                                    {
                                                                                        if !{
                                                                                            let __pretty_cp310 = state.offset;
                                                                                            let __pretty_bcp311 = __builder.checkpoint();
                                                                                            let __ok = (|| -> bool {
                                                                                                {
                                                                                                    let __ows308 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows308..state.offset]);
                                                                                                    if !Self::__rhs_prettify(state, __builder) {
                                                                                                        return false;
                                                                                                    }
                                                                                                    let __ows309 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows309..state.offset]);
                                                                                                };
                                                                                                true
                                                                                            })();
                                                                                            if !__ok {
                                                                                                state.offset = __pretty_cp310;
                                                                                                __builder.restore(__pretty_bcp311);
                                                                                            }
                                                                                            __ok
                                                                                        } {
                                                                                            return false;
                                                                                        }
                                                                                    };
                                                                                    {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b']')
                                                                                        {
                                                                                            return false;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        __builder.char(b']');
                                                                                    };
                                                                                };
                                                                                true
                                                                            })();
                                                                            if !__ok {
                                                                                state.offset = __pretty_cp318;
                                                                                __builder.restore(__pretty_bcp319);
                                                                            }
                                                                            __ok
                                                                        } {
                                                                            {
                                                                                if !{
                                                                                    let __pretty_cp316 = state.offset;
                                                                                    let __pretty_bcp317 = __builder.checkpoint();
                                                                                    let __ok = (|| -> bool {
                                                                                        {
                                                                                            {
                                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'{')
                                                                                                {
                                                                                                    return false;
                                                                                                }
                                                                                                state.offset += 1;
                                                                                                __builder.char(b'{');
                                                                                            };
                                                                                            {
                                                                                                if !{
                                                                                                    let __pretty_cp314 = state.offset;
                                                                                                    let __pretty_bcp315 = __builder.checkpoint();
                                                                                                    let __ok = (|| -> bool {
                                                                                                        {
                                                                                                            let __ows312 = state.offset;
                                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                            __builder
                                                                                                                .text_inline_ws(&state.src[__ows312..state.offset]);
                                                                                                            if !Self::__rhs_prettify(state, __builder) {
                                                                                                                return false;
                                                                                                            }
                                                                                                            let __ows313 = state.offset;
                                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                            __builder
                                                                                                                .text_inline_ws(&state.src[__ows313..state.offset]);
                                                                                                        };
                                                                                                        true
                                                                                                    })();
                                                                                                    if !__ok {
                                                                                                        state.offset = __pretty_cp314;
                                                                                                        __builder.restore(__pretty_bcp315);
                                                                                                    }
                                                                                                    __ok
                                                                                                } {
                                                                                                    return false;
                                                                                                }
                                                                                            };
                                                                                            {
                                                                                                if state.src_bytes.get(state.offset).copied() != Some(b'}')
                                                                                                {
                                                                                                    return false;
                                                                                                }
                                                                                                state.offset += 1;
                                                                                                __builder.char(b'}');
                                                                                            };
                                                                                        };
                                                                                        true
                                                                                    })();
                                                                                    if !__ok {
                                                                                        state.offset = __pretty_cp316;
                                                                                        __builder.restore(__pretty_bcp317);
                                                                                    }
                                                                                    __ok
                                                                                } {
                                                                                    return false;
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };
            true
        }
    }
    pub fn term_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__term_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __binary_factor_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !Self::__mapped_factor_prettify(state, __builder) {
                    return false;
                }
                {
                    let mut __rep_count338 = 0usize;
                    while __rep_count338 < 4294967295 {
                        let __rep_cp339 = state.offset;
                        if !{
                            let __pretty_cp336 = state.offset;
                            let __pretty_bcp337 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp334 = state.offset;
                                            let __pretty_bcp335 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows332 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows332..state.offset]);
                                                    if !Self::__binary_operators_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows333 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows333..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp334;
                                                __builder.restore(__pretty_bcp335);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                    if !Self::__mapped_factor_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp336;
                                __builder.restore(__pretty_bcp337);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp339;
                            break;
                        }
                        if state.offset == __rep_cp339 {
                            break;
                        }
                        __rep_count338 += 1;
                    }
                };
            };
            true
        }
    }
    pub fn binary_factor_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__binary_factor_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __rhs_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp340 = state.offset;
                    let __pretty_bcp341 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        if !Self::__closure_prettify(state, __builder) {
                            return false;
                        }
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp340;
                        __builder.restore(__pretty_bcp341);
                    }
                    __ok
                } {
                    if !Self::__alternation_prettify(state, __builder) {
                        return false;
                    }
                }
            };
            true
        }
    }
    pub fn rhs_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__rhs_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __factor_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let _ = {
                        let __pretty_cp342 = state.offset;
                        let __pretty_bcp343 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__big_comment_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp342;
                            __builder.restore(__pretty_bcp343);
                        }
                        __ok
                    };
                    true
                };
                {
                    if !{
                        let __pretty_cp346 = state.offset;
                        let __pretty_bcp347 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows344 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows344..state.offset]);
                                if !Self::__term_prettify(state, __builder) {
                                    return false;
                                }
                                let __ows345 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows345..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp346;
                            __builder.restore(__pretty_bcp347);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp348 = state.offset;
                        let __pretty_bcp349 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__modifier_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp348;
                            __builder.restore(__pretty_bcp349);
                        }
                        __ok
                    };
                    true
                };
                {
                    let _ = {
                        let __pretty_cp350 = state.offset;
                        let __pretty_bcp351 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__big_comment_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp350;
                            __builder.restore(__pretty_bcp351);
                        }
                        __ok
                    };
                    true
                };
            };
            true
        }
    }
    pub fn factor_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__factor_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __mapped_factor_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !Self::__factor_prettify(state, __builder) {
                    return false;
                }
                {
                    let _ = {
                        let __pretty_cp357 = state.offset;
                        let __pretty_bcp358 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    let __ows352 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ows353 = state.offset;
                                    {
                                        let __s = "->";
                                        let __bytes = __s.as_bytes();
                                        let __slc = match state.src_bytes.get(state.offset..) {
                                            Some(s) if s.len() >= 2usize => s,
                                            _ => return false,
                                        };
                                        if &__slc[..2usize] != __bytes {
                                            return false;
                                        }
                                        __builder
                                            .text(&state.src[state.offset..state.offset + 2usize]);
                                        state.offset += 2usize;
                                    };
                                    __builder.text_inline_ws(&state.src[__ows352..__ows353]);
                                    let __ows354 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows354..state.offset]);
                                };
                                {
                                    if !Self::__value_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    {
                                        let _ = {
                                            let __pretty_cp355 = state.offset;
                                            let __pretty_bcp356 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__type_annotation_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp355;
                                                __builder.restore(__pretty_bcp356);
                                            }
                                            __ok
                                        };
                                        true
                                    };
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp357;
                            __builder.restore(__pretty_bcp358);
                        }
                        __ok
                    };
                    true
                };
            };
            true
        }
    }
    pub fn mapped_factor_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__mapped_factor_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __recover_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows359 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows360 = state.offset;
                    {
                        let __s = "@recover";
                        let __bytes = __s.as_bytes();
                        let __slc = match state.src_bytes.get(state.offset..) {
                            Some(s) if s.len() >= 8usize => s,
                            _ => return false,
                        };
                        if &__slc[..8usize] != __bytes {
                            return false;
                        }
                        __builder.text(&state.src[state.offset..state.offset + 8usize]);
                        state.offset += 8usize;
                    };
                    __builder.text_inline_ws(&state.src[__ows359..__ows360]);
                    let __ows361 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows361..state.offset]);
                };
                {
                    let __ows362 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows363 = state.offset;
                    {
                        let __start = state.offset;
                        if ::parse_that::scan_ident(state).is_none() {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                    __builder.text_inline_ws(&state.src[__ows362..__ows363]);
                    let __ows364 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows364..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp367 = state.offset;
                        let __pretty_bcp368 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows365 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows365..state.offset]);
                                if !Self::__rhs_prettify(state, __builder) {
                                    return false;
                                }
                                let __ows366 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows366..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp367;
                            __builder.restore(__pretty_bcp368);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp370 = state.offset;
                        let __pretty_bcp371 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp369 = state.offset;
                                    let __ok = (|| -> bool {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp369;
                                    }
                                    __ok
                                } {
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b'.');
                                    };
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp370;
                            __builder.restore(__pretty_bcp371);
                        }
                        __ok
                    };
                    true
                };
            };
            true
        }
    }
    pub fn recover_directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__recover_directive_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __rule_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        __builder.group_open();
        let __pretty_ok = {
            {
                {
                    {
                        let __start = state.offset;
                        if ::parse_that::scan_ident(state).is_none() {
                            return false;
                        }
                        let __matched = &state.src[__start..state.offset];
                        if !__matched.is_empty() {
                            __builder.text(__matched);
                        }
                    };
                    {
                        let __ows372 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows373 = state.offset;
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'=') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'=');
                        };
                        __builder.text_inline_ws(&state.src[__ows372..__ows373]);
                        let __ows374 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows374..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp377 = state.offset;
                            let __pretty_bcp378 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows375 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows375..state.offset]);
                                    if !Self::__rhs_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows376 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows376..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp377;
                                __builder.restore(__pretty_bcp378);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        if !{
                            let __pretty_cp379 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b';')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b';');
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp379;
                            }
                            __ok
                        } {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'.');
                            };
                        }
                    };
                };
                true
            }
        };
        __builder.group_close();
        __pretty_ok
    }
    pub fn rule_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__rule_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp392 = state.offset;
                    let __pretty_bcp393 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        if !Self::__import_directive_prettify(state, __builder) {
                            return false;
                        }
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp392;
                        __builder.restore(__pretty_bcp393);
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp390 = state.offset;
                            let __pretty_bcp391 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                if !Self::__recover_directive_prettify(state, __builder) {
                                    return false;
                                }
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp390;
                                __builder.restore(__pretty_bcp391);
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp388 = state.offset;
                                    let __pretty_bcp389 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        if !Self::__pretty_directive_prettify(state, __builder) {
                                            return false;
                                        }
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp388;
                                        __builder.restore(__pretty_bcp389);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp386 = state.offset;
                                            let __pretty_bcp387 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__ws_directive_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp386;
                                                __builder.restore(__pretty_bcp387);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp384 = state.offset;
                                                    let __pretty_bcp385 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        if !Self::__token_directive_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp384;
                                                        __builder.restore(__pretty_bcp385);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp382 = state.offset;
                                                            let __pretty_bcp383 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                if !Self::__debug_directive_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp382;
                                                                __builder.restore(__pretty_bcp383);
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp380 = state.offset;
                                                                    let __pretty_bcp381 = __builder.checkpoint();
                                                                    let __ok = (|| -> bool {
                                                                        if !Self::__host_directive_prettify(state, __builder) {
                                                                            return false;
                                                                        }
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp380;
                                                                        __builder.restore(__pretty_bcp381);
                                                                    }
                                                                    __ok
                                                                } {
                                                                    return false;
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };
            true
        }
    }
    pub fn directive_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__directive_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __grammar_item_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp404 = state.offset;
                    let __pretty_bcp405 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            if !{
                                let __pretty_cp396 = state.offset;
                                let __pretty_bcp397 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        let __ows394 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows394..state.offset]);
                                        {
                                            {
                                                let __s = "//";
                                                let __bytes = __s.as_bytes();
                                                let __slc = match state.src_bytes.get(state.offset..) {
                                                    Some(s) if s.len() >= 2usize => s,
                                                    _ => return false,
                                                };
                                                if &__slc[..2usize] != __bytes {
                                                    return false;
                                                }
                                                __builder
                                                    .text(&state.src[state.offset..state.offset + 2usize]);
                                                state.offset += 2usize;
                                            };
                                            {
                                                let __start = state.offset;
                                                if {
                                                    let __start = state.offset;
                                                    let __result: Option<()> = (|| {
                                                        {
                                                            let __end = state.src_bytes.len();
                                                            let mut __pos = state.offset;
                                                            while __pos < __end {
                                                                let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                                                if !(__b == b'\n') {
                                                                    __pos += 1;
                                                                } else {
                                                                    break;
                                                                }
                                                            }
                                                            state.offset = __pos;
                                                        }
                                                        Some(())
                                                    })();
                                                    if __result.is_some() {
                                                        Some(
                                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                                        )
                                                    } else {
                                                        state.offset = __start;
                                                        None
                                                    }
                                                }
                                                    .is_none()
                                                {
                                                    return false;
                                                }
                                                let __matched = &state.src[__start..state.offset];
                                                if !__matched.is_empty() {
                                                    __builder.text(__matched);
                                                }
                                            };
                                        };
                                        let __ows395 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows395..state.offset]);
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp396;
                                    __builder.restore(__pretty_bcp397);
                                }
                                __ok
                            } {
                                return false;
                            }
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp404;
                        __builder.restore(__pretty_bcp405);
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp402 = state.offset;
                            let __pretty_bcp403 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                if !Self::__big_comment_prettify(state, __builder) {
                                    return false;
                                }
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp402;
                                __builder.restore(__pretty_bcp403);
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp400 = state.offset;
                                    let __pretty_bcp401 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        if !Self::__directive_prettify(state, __builder) {
                                            return false;
                                        }
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp400;
                                        __builder.restore(__pretty_bcp401);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp398 = state.offset;
                                            let __pretty_bcp399 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__rule_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp398;
                                                __builder.restore(__pretty_bcp399);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };
            true
        }
    }
    pub fn grammar_item_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__grammar_item_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
    #[allow(non_snake_case)]
    fn __grammar_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let mut __rep_count411 = 0usize;
                while __rep_count411 < 4294967295 {
                    let __rep_cp412 = state.offset;
                    let __iter_cp = if __rep_count411 > 0 {
                        Some(__builder.checkpoint())
                    } else {
                        None
                    };
                    if __rep_count411 > 0 {
                        __builder.hardline();
                    }
                    if !{
                        let __pretty_cp410 = state.offset;
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp408 = state.offset;
                                    let __pretty_bcp409 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            let __ows406 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder
                                                .text_inline_ws(&state.src[__ows406..state.offset]);
                                            if !Self::__grammar_item_prettify(state, __builder) {
                                                return false;
                                            }
                                            let __ows407 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder
                                                .text_inline_ws(&state.src[__ows407..state.offset]);
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp408;
                                        __builder.restore(__pretty_bcp409);
                                    }
                                    __ok
                                } {
                                    return false;
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp410;
                        }
                        __ok
                    } {
                        state.offset = __rep_cp412;
                        if let Some(__bcp) = __iter_cp {
                            __builder.restore(__bcp);
                        }
                        break;
                    }
                    if state.offset == __rep_cp412 {
                        break;
                    }
                    __rep_count411 += 1;
                }
            };
            true
        }
    }
    pub fn grammar_prettify<'a>() -> Parser<'a, Vec<::pprint::FmtOp<'a>>> {
        Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
            let mut __builder = ::pprint::FmtBuilder::with_capacity(
                state.src.len().saturating_mul(2),
            );
            if !Self::__grammar_prettify(state, &mut __builder) {
                return None;
            }
            Some(__builder.finish())
        })
    }
}
impl<'a> BbnfBootstrapEnum<'a> {
    /// Debug helper: collect references to all enum-typed children.
    ///
    /// Allocates a `Vec`. Walkers should prefer `walk_children`, which
    /// dispatches per variant directly with the visitor in scope.
    pub fn children(
        node: &'a BbnfBootstrapEnum<'a>,
    ) -> ::std::vec::Vec<&'a BbnfBootstrapEnum<'a>> {
        match node {
            BbnfBootstrapEnum::type_name(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::regex(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::identifier(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::literal(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::big_comment(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::modifier(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::value_ident(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::cmp_op(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::mul_op(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::int_lit(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::float_lit(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::bool_lit(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::string_lit(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::add_op(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::binary_operators(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::import_path(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::comment(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::type_annotation(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).1);
                __children
            }
            BbnfBootstrapEnum::ws_directive(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).1);
                __children
            }
            BbnfBootstrapEnum::lhs(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push(value);
                __children
            }
            BbnfBootstrapEnum::host_directive(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).1);
                if let Some(__opt_1) = (value).2 {
                    __children.push((__opt_1).1);
                }
                __children
            }
            BbnfBootstrapEnum::token_directive(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).1);
                __children
            }
            BbnfBootstrapEnum::debug_directive(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).1);
                __children
            }
            BbnfBootstrapEnum::pretty_hint(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).0);
                __children
            }
            BbnfBootstrapEnum::import_items(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push(((value).1).0);
                for __item_2 in (((value).1).1).iter() {
                    __children.push(__item_2.1);
                }
                __children
            }
            BbnfBootstrapEnum::value_input(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                for __item_1 in ((value).1).iter() {
                    __children.push(__item_1.1);
                }
                __children
            }
            BbnfBootstrapEnum::value_path(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).0);
                for __item_1 in ((value).1).iter() {
                    __children.push(__item_1.1);
                }
                __children
            }
            BbnfBootstrapEnum::pretty_directive(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).1);
                for __item_1 in ((value).2).iter() {
                    __children.push(__item_1);
                }
                __children
            }
            BbnfBootstrapEnum::import_directive(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).1);
                __children
            }
            BbnfBootstrapEnum::value_mul(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).0);
                for __item_1 in ((value).1).iter() {
                    __children.push(__item_1.0);
                    __children.push(__item_1.1);
                }
                __children
            }
            BbnfBootstrapEnum::value_or(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).0);
                for __item_1 in ((value).1).iter() {
                    __children.push(__item_1.1);
                }
                __children
            }
            BbnfBootstrapEnum::value_add(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).0);
                for __item_1 in ((value).1).iter() {
                    __children.push(__item_1.0);
                    __children.push(__item_1.1);
                }
                __children
            }
            BbnfBootstrapEnum::value_cmp(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).0);
                for __item_1 in ((value).1).iter() {
                    __children.push(__item_1.0);
                    __children.push(__item_1.1);
                }
                __children
            }
            BbnfBootstrapEnum::value_and(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).0);
                for __item_1 in ((value).1).iter() {
                    __children.push(__item_1.1);
                }
                __children
            }
            BbnfBootstrapEnum::value_closure(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).1);
                for __item_1 in ((value).2).iter() {
                    __children.push(__item_1.1);
                }
                __children.push((value).4);
                __children
            }
            BbnfBootstrapEnum::value_fn_call(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).0);
                if let Some(__opt_1) = (value).2 {
                    __children.push((__opt_1).0);
                    for __item_3 in ((__opt_1).1).iter() {
                        __children.push(__item_3.1);
                    }
                }
                __children
            }
            BbnfBootstrapEnum::value_atom(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push(value);
                __children
            }
            BbnfBootstrapEnum::value_unary(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push(value);
                __children
            }
            BbnfBootstrapEnum::alternation(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                for __item_0 in (value).iter() {
                    __children.push(__item_0.0);
                }
                __children
            }
            BbnfBootstrapEnum::call_arg(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                for __item_0 in (value).iter() {
                    __children.push(__item_0.0);
                }
                __children
            }
            BbnfBootstrapEnum::concatenation(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                for __item_0 in (value).iter() {
                    __children.push(__item_0.0);
                }
                __children
            }
            BbnfBootstrapEnum::closure(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).1);
                for __item_1 in ((value).2).iter() {
                    __children.push(__item_1.1);
                }
                __children.push((value).4);
                __children
            }
            BbnfBootstrapEnum::term(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push(value);
                __children
            }
            BbnfBootstrapEnum::binary_factor(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).0);
                for __item_1 in ((value).1).iter() {
                    __children.push(__item_1.0);
                    __children.push(__item_1.1);
                }
                __children
            }
            BbnfBootstrapEnum::factor(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                if let Some(__opt_1) = (value).0 {
                    __children.push(__opt_1);
                }
                __children.push((value).1);
                if let Some(__opt_1) = (value).2 {
                    __children.push(__opt_1);
                }
                if let Some(__opt_1) = (value).3 {
                    __children.push(__opt_1);
                }
                __children
            }
            BbnfBootstrapEnum::mapped_factor(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).0);
                if let Some(__opt_1) = (value).1 {
                    __children.push(((__opt_1).1).0);
                    if let Some(__opt_4) = ((__opt_1).1).1 {
                        __children.push(__opt_4);
                    }
                }
                __children
            }
            BbnfBootstrapEnum::recover_directive(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).1);
                __children.push((value).2);
                __children
            }
            BbnfBootstrapEnum::rule(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).0);
                __children.push((value).2);
                __children
            }
            BbnfBootstrapEnum::directive(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push(value);
                __children
            }
            BbnfBootstrapEnum::grammar_item(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push(value);
                __children
            }
            BbnfBootstrapEnum::grammar(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                for __item_0 in (value).iter() {
                    __children.push(__item_0);
                }
                __children
            }
            BbnfBootstrapEnum::debug_directive_0(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::pretty_directive_0(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::import_directive_0(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).0);
                __children.push((value).2);
                __children
            }
            BbnfBootstrapEnum::value_atom_0(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).1);
                __children
            }
            BbnfBootstrapEnum::value_unary_0(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).1);
                __children
            }
            BbnfBootstrapEnum::term_0(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::term_1(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).0);
                if let Some(__opt_1) = (value).1 {
                    __children.push((__opt_1).1);
                    for __item_3 in ((__opt_1).2).iter() {
                        __children.push(__item_3.1);
                    }
                }
                __children
            }
            BbnfBootstrapEnum::term_2(value) => {
                let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
                __children.push((value).1);
                __children
            }
            BbnfBootstrapEnum::__Phantom(_) => ::std::vec::Vec::new(),
        }
    }
    /// Extract terminal text by recursively unwrapping wrapper variants.
    pub fn span_text(node: &'a BbnfBootstrapEnum<'a>) -> &'a str {
        match node {
            BbnfBootstrapEnum::type_name(s) => s.as_str(),
            BbnfBootstrapEnum::regex(s) => s.as_str(),
            BbnfBootstrapEnum::identifier(s) => s.as_str(),
            BbnfBootstrapEnum::literal(s) => s.as_str(),
            BbnfBootstrapEnum::big_comment(s) => s.as_str(),
            BbnfBootstrapEnum::modifier(s) => s.as_str(),
            BbnfBootstrapEnum::value_ident(s) => s.as_str(),
            BbnfBootstrapEnum::cmp_op(s) => s.as_str(),
            BbnfBootstrapEnum::mul_op(s) => s.as_str(),
            BbnfBootstrapEnum::int_lit(s) => s.as_str(),
            BbnfBootstrapEnum::float_lit(s) => s.as_str(),
            BbnfBootstrapEnum::bool_lit(s) => s.as_str(),
            BbnfBootstrapEnum::string_lit(s) => s.as_str(),
            BbnfBootstrapEnum::add_op(s) => s.as_str(),
            BbnfBootstrapEnum::binary_operators(s) => s.as_str(),
            BbnfBootstrapEnum::import_path(s) => s.as_str(),
            BbnfBootstrapEnum::comment(s) => s.as_str(),
            BbnfBootstrapEnum::type_annotation(value) => Self::span_text((value).1),
            BbnfBootstrapEnum::ws_directive(value) => Self::span_text((value).1),
            BbnfBootstrapEnum::lhs(inner) => Self::span_text(inner),
            BbnfBootstrapEnum::host_directive(value) => Self::span_text((value).1),
            BbnfBootstrapEnum::token_directive(value) => Self::span_text((value).1),
            BbnfBootstrapEnum::debug_directive(value) => Self::span_text((value).1),
            BbnfBootstrapEnum::pretty_hint(value) => Self::span_text((value).0),
            BbnfBootstrapEnum::value_input(value) => (value).0.as_str(),
            BbnfBootstrapEnum::value_path(value) => Self::span_text((value).0),
            BbnfBootstrapEnum::pretty_directive(value) => Self::span_text((value).1),
            BbnfBootstrapEnum::import_directive(value) => Self::span_text((value).1),
            BbnfBootstrapEnum::value_mul(value) => Self::span_text((value).0),
            BbnfBootstrapEnum::value_or(value) => Self::span_text((value).0),
            BbnfBootstrapEnum::value_add(value) => Self::span_text((value).0),
            BbnfBootstrapEnum::value_cmp(value) => Self::span_text((value).0),
            BbnfBootstrapEnum::value_and(value) => Self::span_text((value).0),
            BbnfBootstrapEnum::value_closure(value) => Self::span_text((value).1),
            BbnfBootstrapEnum::value_fn_call(value) => Self::span_text((value).0),
            BbnfBootstrapEnum::value_atom(inner) => Self::span_text(inner),
            BbnfBootstrapEnum::value_unary(inner) => Self::span_text(inner),
            BbnfBootstrapEnum::alternation(items) if !items.is_empty() => {
                Self::span_text(items[0].0)
            }
            BbnfBootstrapEnum::call_arg(items) if !items.is_empty() => {
                Self::span_text(items[0].0)
            }
            BbnfBootstrapEnum::concatenation(items) if !items.is_empty() => {
                Self::span_text(items[0].0)
            }
            BbnfBootstrapEnum::closure(value) => Self::span_text((value).4),
            BbnfBootstrapEnum::term(inner) => Self::span_text(inner),
            BbnfBootstrapEnum::binary_factor(value) => Self::span_text((value).0),
            BbnfBootstrapEnum::factor(value) => Self::span_text((value).1),
            BbnfBootstrapEnum::mapped_factor(value) => Self::span_text((value).0),
            BbnfBootstrapEnum::recover_directive(value) => Self::span_text((value).2),
            BbnfBootstrapEnum::rule(value) => Self::span_text((value).0),
            BbnfBootstrapEnum::directive(inner) => Self::span_text(inner),
            BbnfBootstrapEnum::grammar_item(inner) => Self::span_text(inner),
            BbnfBootstrapEnum::grammar(items) if !items.is_empty() => {
                Self::span_text(&items[0])
            }
            BbnfBootstrapEnum::debug_directive_0(s) => s.as_str(),
            BbnfBootstrapEnum::pretty_directive_0(s) => s.as_str(),
            BbnfBootstrapEnum::import_directive_0(value) => Self::span_text((value).0),
            BbnfBootstrapEnum::value_atom_0(value) => Self::span_text((value).1),
            BbnfBootstrapEnum::value_unary_0(value) => Self::span_text((value).1),
            BbnfBootstrapEnum::term_0(s) => s.as_str(),
            BbnfBootstrapEnum::term_1(value) => Self::span_text((value).0),
            BbnfBootstrapEnum::term_2(value) => Self::span_text((value).1),
            _ => "",
        }
    }
    /// Recursively extract an identifier carrier's text. Returns the
    /// empty string if no identifier is reachable.
    pub fn identifier_text(node: &'a BbnfBootstrapEnum<'a>) -> &'a str {
        match node {
            BbnfBootstrapEnum::identifier(s) => s.as_str(),
            BbnfBootstrapEnum::lhs(value) => Self::identifier_text(value),
            BbnfBootstrapEnum::host_directive(value) => Self::identifier_text((value).1),
            BbnfBootstrapEnum::token_directive(value) => Self::identifier_text((value).1),
            BbnfBootstrapEnum::pretty_hint(value) => Self::identifier_text((value).0),
            BbnfBootstrapEnum::closure(value) => Self::identifier_text((value).1),
            BbnfBootstrapEnum::recover_directive(value) => {
                Self::identifier_text((value).1)
            }
            _ => {
                let ch = Self::children(node);
                if let Some(first) = ch.first() {
                    Self::identifier_text(first)
                } else {
                    ""
                }
            }
        }
    }
    /// Recursively extract an identifier carrier's `Span`. Returns
    /// `Span::default()` if no identifier is reachable.
    pub fn identifier_span(node: &'a BbnfBootstrapEnum<'a>) -> ::parse_that::Span<'a> {
        match node {
            BbnfBootstrapEnum::identifier(s) => *s,
            BbnfBootstrapEnum::lhs(value) => Self::identifier_span(value),
            BbnfBootstrapEnum::host_directive(value) => Self::identifier_span((value).1),
            BbnfBootstrapEnum::token_directive(value) => Self::identifier_span((value).1),
            BbnfBootstrapEnum::pretty_hint(value) => Self::identifier_span((value).0),
            BbnfBootstrapEnum::closure(value) => Self::identifier_span((value).1),
            BbnfBootstrapEnum::recover_directive(value) => {
                Self::identifier_span((value).1)
            }
            _ => {
                let ch = Self::children(node);
                if let Some(first) = ch.first() {
                    Self::identifier_span(first)
                } else {
                    ::parse_that::Span::default()
                }
            }
        }
    }
    /// Direct per-variant dispatch: visit each enum-typed child via
    /// the supplied visitor and collect their `Output`s. No intermediate
    /// allocation of a `Vec<&Enum>`.
    pub fn walk_children<__V: BbnfBootstrapEnumVisitor<'a> + ?Sized>(
        node: &'a BbnfBootstrapEnum<'a>,
        v: &mut __V,
    ) -> ::std::vec::Vec<__V::Output> {
        match node {
            BbnfBootstrapEnum::type_name(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::regex(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::identifier(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::literal(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::big_comment(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::modifier(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::value_ident(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::cmp_op(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::mul_op(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::int_lit(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::float_lit(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::bool_lit(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::string_lit(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::add_op(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::binary_operators(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::import_path(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::comment(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::type_annotation(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).1));
                __outputs
            }
            BbnfBootstrapEnum::ws_directive(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).1));
                __outputs
            }
            BbnfBootstrapEnum::lhs(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit(value));
                __outputs
            }
            BbnfBootstrapEnum::host_directive(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).1));
                if let Some(__opt_1) = (value).2 {
                    __outputs.push(v.visit((__opt_1).1));
                }
                __outputs
            }
            BbnfBootstrapEnum::token_directive(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).1));
                __outputs
            }
            BbnfBootstrapEnum::debug_directive(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).1));
                __outputs
            }
            BbnfBootstrapEnum::pretty_hint(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).0));
                __outputs
            }
            BbnfBootstrapEnum::import_items(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit(((value).1).0));
                for __item_2 in (((value).1).1).iter() {
                    __outputs.push(v.visit(__item_2.1));
                }
                __outputs
            }
            BbnfBootstrapEnum::value_input(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                for __item_1 in ((value).1).iter() {
                    __outputs.push(v.visit(__item_1.1));
                }
                __outputs
            }
            BbnfBootstrapEnum::value_path(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).0));
                for __item_1 in ((value).1).iter() {
                    __outputs.push(v.visit(__item_1.1));
                }
                __outputs
            }
            BbnfBootstrapEnum::pretty_directive(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).1));
                for __item_1 in ((value).2).iter() {
                    __outputs.push(v.visit(__item_1));
                }
                __outputs
            }
            BbnfBootstrapEnum::import_directive(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).1));
                __outputs
            }
            BbnfBootstrapEnum::value_mul(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).0));
                for __item_1 in ((value).1).iter() {
                    __outputs.push(v.visit(__item_1.0));
                    __outputs.push(v.visit(__item_1.1));
                }
                __outputs
            }
            BbnfBootstrapEnum::value_or(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).0));
                for __item_1 in ((value).1).iter() {
                    __outputs.push(v.visit(__item_1.1));
                }
                __outputs
            }
            BbnfBootstrapEnum::value_add(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).0));
                for __item_1 in ((value).1).iter() {
                    __outputs.push(v.visit(__item_1.0));
                    __outputs.push(v.visit(__item_1.1));
                }
                __outputs
            }
            BbnfBootstrapEnum::value_cmp(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).0));
                for __item_1 in ((value).1).iter() {
                    __outputs.push(v.visit(__item_1.0));
                    __outputs.push(v.visit(__item_1.1));
                }
                __outputs
            }
            BbnfBootstrapEnum::value_and(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).0));
                for __item_1 in ((value).1).iter() {
                    __outputs.push(v.visit(__item_1.1));
                }
                __outputs
            }
            BbnfBootstrapEnum::value_closure(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).1));
                for __item_1 in ((value).2).iter() {
                    __outputs.push(v.visit(__item_1.1));
                }
                __outputs.push(v.visit((value).4));
                __outputs
            }
            BbnfBootstrapEnum::value_fn_call(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).0));
                if let Some(__opt_1) = (value).2 {
                    __outputs.push(v.visit((__opt_1).0));
                    for __item_3 in ((__opt_1).1).iter() {
                        __outputs.push(v.visit(__item_3.1));
                    }
                }
                __outputs
            }
            BbnfBootstrapEnum::value_atom(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit(value));
                __outputs
            }
            BbnfBootstrapEnum::value_unary(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit(value));
                __outputs
            }
            BbnfBootstrapEnum::alternation(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                for __item_0 in (value).iter() {
                    __outputs.push(v.visit(__item_0.0));
                }
                __outputs
            }
            BbnfBootstrapEnum::call_arg(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                for __item_0 in (value).iter() {
                    __outputs.push(v.visit(__item_0.0));
                }
                __outputs
            }
            BbnfBootstrapEnum::concatenation(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                for __item_0 in (value).iter() {
                    __outputs.push(v.visit(__item_0.0));
                }
                __outputs
            }
            BbnfBootstrapEnum::closure(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).1));
                for __item_1 in ((value).2).iter() {
                    __outputs.push(v.visit(__item_1.1));
                }
                __outputs.push(v.visit((value).4));
                __outputs
            }
            BbnfBootstrapEnum::term(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit(value));
                __outputs
            }
            BbnfBootstrapEnum::binary_factor(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).0));
                for __item_1 in ((value).1).iter() {
                    __outputs.push(v.visit(__item_1.0));
                    __outputs.push(v.visit(__item_1.1));
                }
                __outputs
            }
            BbnfBootstrapEnum::factor(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                if let Some(__opt_1) = (value).0 {
                    __outputs.push(v.visit(__opt_1));
                }
                __outputs.push(v.visit((value).1));
                if let Some(__opt_1) = (value).2 {
                    __outputs.push(v.visit(__opt_1));
                }
                if let Some(__opt_1) = (value).3 {
                    __outputs.push(v.visit(__opt_1));
                }
                __outputs
            }
            BbnfBootstrapEnum::mapped_factor(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).0));
                if let Some(__opt_1) = (value).1 {
                    __outputs.push(v.visit(((__opt_1).1).0));
                    if let Some(__opt_4) = ((__opt_1).1).1 {
                        __outputs.push(v.visit(__opt_4));
                    }
                }
                __outputs
            }
            BbnfBootstrapEnum::recover_directive(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).1));
                __outputs.push(v.visit((value).2));
                __outputs
            }
            BbnfBootstrapEnum::rule(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).0));
                __outputs.push(v.visit((value).2));
                __outputs
            }
            BbnfBootstrapEnum::directive(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit(value));
                __outputs
            }
            BbnfBootstrapEnum::grammar_item(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit(value));
                __outputs
            }
            BbnfBootstrapEnum::grammar(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                for __item_0 in (value).iter() {
                    __outputs.push(v.visit(__item_0));
                }
                __outputs
            }
            BbnfBootstrapEnum::debug_directive_0(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::pretty_directive_0(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::import_directive_0(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).0));
                __outputs.push(v.visit((value).2));
                __outputs
            }
            BbnfBootstrapEnum::value_atom_0(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).1));
                __outputs
            }
            BbnfBootstrapEnum::value_unary_0(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).1));
                __outputs
            }
            BbnfBootstrapEnum::term_0(_) => ::std::vec::Vec::new(),
            BbnfBootstrapEnum::term_1(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).0));
                if let Some(__opt_1) = (value).1 {
                    __outputs.push(v.visit((__opt_1).1));
                    for __item_3 in ((__opt_1).2).iter() {
                        __outputs.push(v.visit(__item_3.1));
                    }
                }
                __outputs
            }
            BbnfBootstrapEnum::term_2(value) => {
                let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
                __outputs.push(v.visit((value).1));
                __outputs
            }
            BbnfBootstrapEnum::__Phantom(_) => ::std::vec::Vec::new(),
        }
    }
    /// Schema-generated directive accessor.
    pub fn as_ws_directive(
        &'a self,
    ) -> ::std::option::Option<cst_directives::WsDirective<'a>> {
        if let BbnfBootstrapEnum::ws_directive((_, __f1, __term)) = self {
            ::std::option::Option::Some(cst_directives::WsDirective {
                value: __f1,
                span: *__term,
            })
        } else {
            ::std::option::Option::None
        }
    }
    /// Schema-generated directive accessor.
    pub fn as_host_directive(
        &'a self,
    ) -> ::std::option::Option<cst_directives::HostDirective<'a>> {
        if let BbnfBootstrapEnum::host_directive((_, __f1, __f2, __term)) = self {
            ::std::option::Option::Some(cst_directives::HostDirective {
                name: BbnfBootstrapEnum::identifier_text(__f1),
                type_annotation: __f2.as_ref().map(|t| t.1),
                span: *__term,
            })
        } else {
            ::std::option::Option::None
        }
    }
    /// Schema-generated directive accessor.
    pub fn as_token_directive(
        &'a self,
    ) -> ::std::option::Option<cst_directives::TokenDirective<'a>> {
        if let BbnfBootstrapEnum::token_directive((_, __f1, __term)) = self {
            ::std::option::Option::Some(cst_directives::TokenDirective {
                name: BbnfBootstrapEnum::identifier_text(__f1),
                span: *__term,
            })
        } else {
            ::std::option::Option::None
        }
    }
    /// Schema-generated directive accessor.
    pub fn as_debug_directive(
        &'a self,
    ) -> ::std::option::Option<cst_directives::DebugDirective<'a>> {
        if let BbnfBootstrapEnum::debug_directive((_, __f1, __term)) = self {
            ::std::option::Option::Some(cst_directives::DebugDirective {
                target: BbnfBootstrapEnum::span_text(__f1),
                span: *__term,
            })
        } else {
            ::std::option::Option::None
        }
    }
    /// Schema-generated directive accessor.
    pub fn as_pretty_directive(
        &'a self,
    ) -> ::std::option::Option<cst_directives::PrettyDirective<'a>> {
        if let BbnfBootstrapEnum::pretty_directive((_, __f1, __f2, __term)) = self {
            ::std::option::Option::Some(cst_directives::PrettyDirective {
                target: BbnfBootstrapEnum::span_text(__f1),
                hints: __f2,
                span: *__term,
            })
        } else {
            ::std::option::Option::None
        }
    }
    /// Schema-generated directive accessor.
    pub fn as_import_directive(
        &'a self,
    ) -> ::std::option::Option<cst_directives::ImportDirective<'a>> {
        if let BbnfBootstrapEnum::import_directive((_, __f1, __term)) = self {
            ::std::option::Option::Some(cst_directives::ImportDirective {
                inner: __f1,
                span: *__term,
            })
        } else {
            ::std::option::Option::None
        }
    }
    /// Schema-generated directive accessor.
    pub fn as_recover_directive(
        &'a self,
    ) -> ::std::option::Option<cst_directives::RecoverDirective<'a>> {
        if let BbnfBootstrapEnum::recover_directive((_, __f1, __f2, __term)) = self {
            ::std::option::Option::Some(cst_directives::RecoverDirective {
                rule_name: BbnfBootstrapEnum::identifier_text(__f1),
                sync_expr: __f2,
                span: *__term,
            })
        } else {
            ::std::option::Option::None
        }
    }
}
/// Auto-generated visitor trait for the parser enum.
///
/// Default `visit()` calls `walk()` which dispatches via
/// `walk_children` (per-variant direct dispatch). Override
/// `visit()` for short-circuiting; override `combine()` for
/// non-default fold semantics.
pub trait BbnfBootstrapEnumVisitor<'a> {
    type Output: Default;
    fn combine(&mut self, outputs: ::std::vec::Vec<Self::Output>) -> Self::Output {
        let _ = outputs;
        Self::Output::default()
    }
    fn visit(&mut self, node: &'a BbnfBootstrapEnum<'a>) -> Self::Output {
        self.walk(node)
    }
    fn walk(&mut self, node: &'a BbnfBootstrapEnum<'a>) -> Self::Output {
        let outputs = BbnfBootstrapEnum::walk_children(node, self);
        if outputs.is_empty() { Self::Output::default() } else { self.combine(outputs) }
    }
}
/// Schema-emitted directive value structs. Returned by the
/// `as_*_directive` accessors on the parser enum.
#[allow(dead_code, non_snake_case)]
pub mod cst_directives {
    use super::BbnfBootstrapEnum;
    #[derive(Clone, Copy)]
    pub struct WsDirective<'a> {
        pub value: &'a BbnfBootstrapEnum<'a>,
        pub span: ::parse_that::Span<'a>,
    }
    #[derive(Clone, Copy)]
    pub struct HostDirective<'a> {
        pub name: &'a str,
        pub type_annotation: ::std::option::Option<&'a BbnfBootstrapEnum<'a>>,
        pub span: ::parse_that::Span<'a>,
    }
    #[derive(Clone, Copy)]
    pub struct TokenDirective<'a> {
        pub name: &'a str,
        pub span: ::parse_that::Span<'a>,
    }
    #[derive(Clone, Copy)]
    pub struct DebugDirective<'a> {
        pub target: &'a str,
        pub span: ::parse_that::Span<'a>,
    }
    #[derive(Clone, Copy)]
    pub struct PrettyDirective<'a> {
        pub target: &'a str,
        pub hints: &'a [BbnfBootstrapEnum<'a>],
        pub span: ::parse_that::Span<'a>,
    }
    #[derive(Clone, Copy)]
    pub struct ImportDirective<'a> {
        pub inner: &'a BbnfBootstrapEnum<'a>,
        pub span: ::parse_that::Span<'a>,
    }
    #[derive(Clone, Copy)]
    pub struct RecoverDirective<'a> {
        pub rule_name: &'a str,
        pub sync_expr: &'a BbnfBootstrapEnum<'a>,
        pub span: ::parse_that::Span<'a>,
    }
}

