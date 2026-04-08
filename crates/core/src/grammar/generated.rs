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
    mul_op(::parse_that::Span<'a>),
    value_ident(::parse_that::Span<'a>),
    int_lit(::parse_that::Span<'a>),
    bool_lit(::parse_that::Span<'a>),
    float_lit(::parse_that::Span<'a>),
    string_lit(::parse_that::Span<'a>),
    add_op(::parse_that::Span<'a>),
    cmp_op(::parse_that::Span<'a>),
    identifier(::parse_that::Span<'a>),
    big_comment(::parse_that::Span<'a>),
    import_path(::parse_that::Span<'a>),
    binary_operators(::parse_that::Span<'a>),
    type_name(::parse_that::Span<'a>),
    modifier(::parse_that::Span<'a>),
    literal(::parse_that::Span<'a>),
    regex(::parse_that::Span<'a>),
    comment(::parse_that::Span<'a>),
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
    lhs(&'a BbnfBootstrapEnum<'a>),
    token_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
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
    debug_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    pretty_hint((&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)),
    host_directive(
        (
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
            Option<(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)>,
            ::parse_that::Span<'a>,
        ),
    ),
    type_annotation((::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)),
    ws_directive(
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
    import_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    pretty_directive(
        (
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
            &'a [BbnfBootstrapEnum<'a>],
            ::parse_that::Span<'a>,
        ),
    ),
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
    rule(
        (
            &'a BbnfBootstrapEnum<'a>,
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
            ::parse_that::Span<'a>,
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
    directive(&'a BbnfBootstrapEnum<'a>),
    grammar_item(&'a BbnfBootstrapEnum<'a>),
    grammar(&'a [BbnfBootstrapEnum<'a>]),
    debug_directive_0(::parse_that::Span<'a>),
    value_atom_0(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    value_unary_0((::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)),
    import_directive_0(
        (&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>),
    ),
    pretty_directive_0(::parse_that::Span<'a>),
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
            BbnfBootstrapEnum::mul_op(__self_0) => {
                BbnfBootstrapEnum::mul_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_ident(__self_0) => {
                BbnfBootstrapEnum::value_ident(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::int_lit(__self_0) => {
                BbnfBootstrapEnum::int_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::bool_lit(__self_0) => {
                BbnfBootstrapEnum::bool_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::float_lit(__self_0) => {
                BbnfBootstrapEnum::float_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::string_lit(__self_0) => {
                BbnfBootstrapEnum::string_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::add_op(__self_0) => {
                BbnfBootstrapEnum::add_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::cmp_op(__self_0) => {
                BbnfBootstrapEnum::cmp_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::identifier(__self_0) => {
                BbnfBootstrapEnum::identifier(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::big_comment(__self_0) => {
                BbnfBootstrapEnum::big_comment(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::import_path(__self_0) => {
                BbnfBootstrapEnum::import_path(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::binary_operators(__self_0) => {
                BbnfBootstrapEnum::binary_operators(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::type_name(__self_0) => {
                BbnfBootstrapEnum::type_name(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::modifier(__self_0) => {
                BbnfBootstrapEnum::modifier(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::literal(__self_0) => {
                BbnfBootstrapEnum::literal(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::regex(__self_0) => {
                BbnfBootstrapEnum::regex(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::comment(__self_0) => {
                BbnfBootstrapEnum::comment(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_input(__self_0) => {
                BbnfBootstrapEnum::value_input(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_path(__self_0) => {
                BbnfBootstrapEnum::value_path(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::lhs(__self_0) => {
                BbnfBootstrapEnum::lhs(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::token_directive(__self_0) => {
                BbnfBootstrapEnum::token_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::import_items(__self_0) => {
                BbnfBootstrapEnum::import_items(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::debug_directive(__self_0) => {
                BbnfBootstrapEnum::debug_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::pretty_hint(__self_0) => {
                BbnfBootstrapEnum::pretty_hint(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::host_directive(__self_0) => {
                BbnfBootstrapEnum::host_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::type_annotation(__self_0) => {
                BbnfBootstrapEnum::type_annotation(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::ws_directive(__self_0) => {
                BbnfBootstrapEnum::ws_directive(::core::clone::Clone::clone(__self_0))
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
            BbnfBootstrapEnum::import_directive(__self_0) => {
                BbnfBootstrapEnum::import_directive(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::pretty_directive(__self_0) => {
                BbnfBootstrapEnum::pretty_directive(
                    ::core::clone::Clone::clone(__self_0),
                )
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
            BbnfBootstrapEnum::rule(__self_0) => {
                BbnfBootstrapEnum::rule(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::recover_directive(__self_0) => {
                BbnfBootstrapEnum::recover_directive(
                    ::core::clone::Clone::clone(__self_0),
                )
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
            BbnfBootstrapEnum::value_atom_0(__self_0) => {
                BbnfBootstrapEnum::value_atom_0(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_unary_0(__self_0) => {
                BbnfBootstrapEnum::value_unary_0(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::import_directive_0(__self_0) => {
                BbnfBootstrapEnum::import_directive_0(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::pretty_directive_0(__self_0) => {
                BbnfBootstrapEnum::pretty_directive_0(
                    ::core::clone::Clone::clone(__self_0),
                )
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
    __s1: ::std::cell::UnsafeCell<
        Vec<(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)>,
    >,
    __s2: ::std::cell::UnsafeCell<Vec<BbnfBootstrapEnum<'a>>>,
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
    fn __s1(&self) -> &mut Vec<(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)> {
        unsafe { &mut *self.__s1.get() }
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __c1(
        &'a self,
        depth: usize,
    ) -> &'a [(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)] {
        let s = self.__s1();
        let slice = self.__slab.alloc_slice_clone(&s[depth..]);
        s.truncate(depth);
        slice
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __s2(&self) -> &mut Vec<BbnfBootstrapEnum<'a>> {
        unsafe { &mut *self.__s2.get() }
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __c2(&'a self, depth: usize) -> &'a [BbnfBootstrapEnum<'a>] {
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
    fn __value_input<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __sp_start = state.offset;
                if state.src[state.offset..].starts_with("input") {
                    let __start = state.offset;
                    state.offset += 5usize;
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                } else {
                    None
                }?;
                let __sp4 = ::parse_that::Span::new(__sp_start, state.offset, state.src);
                let __v5 = {
                    let __depth2 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                    loop {
                        let __prev3 = state.offset;
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
                            let __sp0 = ::parse_that::Span::new(
                                __sp_start,
                                state.offset,
                                state.src,
                            );
                            let __v1 = ::parse_that::scan_ident(state)
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::value_ident(__inner);
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
                Some((__sp4, __v5))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::value_input(__x))
    }
    pub fn value_input<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_input)
    }
    #[allow(non_snake_case)]
    fn __value_path<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __chain_head7 = ::parse_that::scan_ident(state)
                    .map(|__inner| {
                        let __v = BbnfBootstrapEnum::value_ident(__inner);
                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                    })?;
                let __chain_depth6 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                loop {
                    let __chain_prev8 = state.offset;
                    match (|| {
                        let __chain_op9 = (|| {
                            let __start = state.offset;
                            if state.src[state.offset..].starts_with("::") {
                                let __start = state.offset;
                                state.offset += 2usize;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            }?;
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        })()?;
                        let __chain_rhs10 = ::parse_that::scan_ident(state)
                            .map(|__inner| {
                                let __v = BbnfBootstrapEnum::value_ident(__inner);
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op9, __chain_rhs10))
                    })() {
                        Some(__value) => {
                            let (__chain_op9, __chain_rhs10) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s0()
                                .push((__chain_op9, __chain_rhs10));
                            if state.offset == __chain_prev8 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __chain_prev8;
                            break;
                        }
                    }
                }
                Some((
                    __chain_head7,
                    __BbnfBootstrapEnum_alloc(state).__c0(__chain_depth6),
                ))
            }
        })()
            .map(|__x| BbnfBootstrapEnum::value_path(__x))
    }
    pub fn value_path<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_path)
    }
    #[allow(non_snake_case)]
    fn __value_mul<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __chain_head12 = Self::__value_unary(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __chain_depth11 = __BbnfBootstrapEnum_alloc(state).__s1().len();
                loop {
                    let __chain_prev13 = state.offset;
                    match (|| {
                        let __chain_op14 = {
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
                        let __chain_rhs15 = Self::__value_unary(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op14, __chain_rhs15))
                    })() {
                        Some(__value) => {
                            let (__chain_op14, __chain_rhs15) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s1()
                                .push((__chain_op14, __chain_rhs15));
                            if state.offset == __chain_prev13 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __chain_prev13;
                            break;
                        }
                    }
                }
                Some((
                    __chain_head12,
                    __BbnfBootstrapEnum_alloc(state).__c1(__chain_depth11),
                ))
            }
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
            {
                let __chain_head17 = Self::__value_mul(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __chain_depth16 = __BbnfBootstrapEnum_alloc(state).__s1().len();
                loop {
                    let __chain_prev18 = state.offset;
                    match (|| {
                        let __chain_op19 = {
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
                        let __chain_rhs20 = Self::__value_mul(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op19, __chain_rhs20))
                    })() {
                        Some(__value) => {
                            let (__chain_op19, __chain_rhs20) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s1()
                                .push((__chain_op19, __chain_rhs20));
                            if state.offset == __chain_prev18 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __chain_prev18;
                            break;
                        }
                    }
                }
                Some((
                    __chain_head17,
                    __BbnfBootstrapEnum_alloc(state).__c1(__chain_depth16),
                ))
            }
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
            {
                let __chain_head22 = Self::__value_add(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __chain_depth21 = __BbnfBootstrapEnum_alloc(state).__s1().len();
                loop {
                    let __chain_prev23 = state.offset;
                    match (|| {
                        let __chain_op24 = {
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
                        let __chain_rhs25 = Self::__value_add(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op24, __chain_rhs25))
                    })() {
                        Some(__value) => {
                            let (__chain_op24, __chain_rhs25) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s1()
                                .push((__chain_op24, __chain_rhs25));
                            if state.offset == __chain_prev23 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __chain_prev23;
                            break;
                        }
                    }
                }
                Some((
                    __chain_head22,
                    __BbnfBootstrapEnum_alloc(state).__c1(__chain_depth21),
                ))
            }
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
            {
                let __chain_head27 = Self::__value_cmp(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __chain_depth26 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                loop {
                    let __chain_prev28 = state.offset;
                    match (|| {
                        let __chain_op29 = (|| {
                            let __start = state.offset;
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
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        })()?;
                        let __chain_rhs30 = Self::__value_cmp(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op29, __chain_rhs30))
                    })() {
                        Some(__value) => {
                            let (__chain_op29, __chain_rhs30) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s0()
                                .push((__chain_op29, __chain_rhs30));
                            if state.offset == __chain_prev28 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __chain_prev28;
                            break;
                        }
                    }
                }
                Some((
                    __chain_head27,
                    __BbnfBootstrapEnum_alloc(state).__c0(__chain_depth26),
                ))
            }
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
                    let __sp35 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v36 = ::parse_that::scan_ident(state)
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::value_ident(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    let __v37 = {
                        let __depth33 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                        loop {
                            let __prev34 = state.offset;
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
                                let __sp31 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                let __v32 = ::parse_that::scan_ident(state)
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::value_ident(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                Some((__sp31, __v32))
                            })() {
                                Some(__value) => {
                                    __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                    if state.offset == __prev34 {
                                        break;
                                    }
                                }
                                None => {
                                    state.offset = __prev34;
                                    break;
                                }
                            }
                        }
                        Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth33))
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
                    let __sp38 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v39 = Self::__value_expr(state)
                        .map(|__v| {
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    Some((__sp35, __v36, __v37, __sp38, __v39))
                })()
                    .map(|__v| BbnfBootstrapEnum::value_closure(__v));
                if __result.is_some() {
                    return __result;
                }
                state.offset = __cp;
            }
            {
                let __cp = state.offset;
                let __result = {
                    let __chain_head41 = Self::__value_and(state)
                        .map(|__v| {
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    let __chain_depth40 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                    loop {
                        let __chain_prev42 = state.offset;
                        match (|| {
                            let __chain_op43 = (|| {
                                let __start = state.offset;
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
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            })()?;
                            let __chain_rhs44 = Self::__value_and(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            Some((__chain_op43, __chain_rhs44))
                        })() {
                            Some(__value) => {
                                let (__chain_op43, __chain_rhs44) = __value;
                                __BbnfBootstrapEnum_alloc(state)
                                    .__s0()
                                    .push((__chain_op43, __chain_rhs44));
                                if state.offset == __chain_prev42 {
                                    break;
                                }
                            }
                            None => {
                                state.offset = __chain_prev42;
                                break;
                            }
                        }
                    }
                    Some((
                        __chain_head41,
                        __BbnfBootstrapEnum_alloc(state).__c0(__chain_depth40),
                    ))
                }
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
                        let __v50 = Self::__value_path(state)
                            .map(|__v| {
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
                        let __sp51 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v52 = {
                            let __cp = state.offset;
                            match (|| {
                                let __chain_head46 = Self::__value_expr(state)
                                    .map(|__v| {
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                let __chain_depth45 = __BbnfBootstrapEnum_alloc(state)
                                    .__s0()
                                    .len();
                                loop {
                                    let __chain_prev47 = state.offset;
                                    match (|| {
                                        let __chain_op48 = (|| {
                                            let __start = state.offset;
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
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        })()?;
                                        let __chain_rhs49 = Self::__value_expr(state)
                                            .map(|__v| {
                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                            })?;
                                        Some((__chain_op48, __chain_rhs49))
                                    })() {
                                        Some(__value) => {
                                            let (__chain_op48, __chain_rhs49) = __value;
                                            __BbnfBootstrapEnum_alloc(state)
                                                .__s0()
                                                .push((__chain_op48, __chain_rhs49));
                                            if state.offset == __chain_prev47 {
                                                break;
                                            }
                                        }
                                        None => {
                                            state.offset = __chain_prev47;
                                            break;
                                        }
                                    }
                                }
                                Some((
                                    __chain_head46,
                                    __BbnfBootstrapEnum_alloc(state).__c0(__chain_depth45),
                                ))
                            })() {
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
                        let __sp53 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__v50, __sp51, __v52, __sp53))
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
                    let __result = Self::__value_input(state)
                        .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v));
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
                {
                    let __cp = state.offset;
                    let __result = Self::__value_path(state)
                        .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v));
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
                        let __sp54 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v55 = {
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
                        let __sp56 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp54, __v55, __sp56))
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
                        let __sp57 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v58 = Self::__value_atom(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__sp57, __v58))
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
                let __sp70 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                let __v71 = {
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ws_inner = (|| {
                        {
                            let __cp = state.offset;
                            let __result = ((|| {
                                let __v67 = {
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
                                        let __sp64 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        let __v65 = {
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ws_inner = {
                                                let __chain_head60 = ::parse_that::scan_ident(state)
                                                    .map(|__inner| {
                                                        let __v = BbnfBootstrapEnum::identifier(__inner);
                                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                    })?;
                                                let __chain_depth59 = __BbnfBootstrapEnum_alloc(state)
                                                    .__s0()
                                                    .len();
                                                loop {
                                                    let __chain_prev61 = state.offset;
                                                    match (|| {
                                                        let __chain_op62 = (|| {
                                                            let __start = state.offset;
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
                                                            Some(
                                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                                            )
                                                        })()?;
                                                        let __chain_rhs63 = ::parse_that::scan_ident(state)
                                                            .map(|__inner| {
                                                                let __v = BbnfBootstrapEnum::identifier(__inner);
                                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                            })?;
                                                        Some((__chain_op62, __chain_rhs63))
                                                    })() {
                                                        Some(__value) => {
                                                            let (__chain_op62, __chain_rhs63) = __value;
                                                            __BbnfBootstrapEnum_alloc(state)
                                                                .__s0()
                                                                .push((__chain_op62, __chain_rhs63));
                                                            if state.offset == __chain_prev61 {
                                                                break;
                                                            }
                                                        }
                                                        None => {
                                                            state.offset = __chain_prev61;
                                                            break;
                                                        }
                                                    }
                                                }
                                                Some((
                                                    __chain_head60,
                                                    __BbnfBootstrapEnum_alloc(state).__c0(__chain_depth59),
                                                ))
                                            };
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
                                        let __sp66 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        Some((__sp64, __v65, __sp66))
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
                                let __sp68 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                let __v69 = (|| {
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
                                Some((__v67, __sp68, __v69))
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
                let __sp72 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                Some((__sp70, __v71, __sp72))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::import_directive(__x))
    }
    pub fn import_directive<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__import_directive)
    }
    #[allow(non_snake_case)]
    fn __alternation<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __depth79 = __BbnfBootstrapEnum_alloc(state).__s3().len();
                loop {
                    let __prev80 = state.offset;
                    match (|| {
                        let __v77 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = {
                                let __depth75 = __BbnfBootstrapEnum_alloc(state)
                                    .__s3()
                                    .len();
                                loop {
                                    let __prev76 = state.offset;
                                    match (|| {
                                        let __v73 = {
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
                                        let __sp74 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        Some((__v73, __sp74))
                                    })() {
                                        Some(__value) => {
                                            __BbnfBootstrapEnum_alloc(state).__s3().push(__value);
                                            if state.offset == __prev76 {
                                                break;
                                            }
                                        }
                                        None => {
                                            state.offset = __prev76;
                                            break;
                                        }
                                    }
                                }
                                if (__BbnfBootstrapEnum_alloc(state).__s3().len()
                                    - __depth75) >= 1usize
                                {
                                    Some(__BbnfBootstrapEnum_alloc(state).__c3(__depth75))
                                } else {
                                    __BbnfBootstrapEnum_alloc(state).__s3().truncate(__depth75);
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
                        let __sp78 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__v77, __sp78))
                    })() {
                        Some(__value) => {
                            __BbnfBootstrapEnum_alloc(state).__s3().push(__value);
                            if state.offset == __prev80 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __prev80;
                            break;
                        }
                    }
                }
                if (__BbnfBootstrapEnum_alloc(state).__s3().len() - __depth79) >= 1usize
                {
                    Some(__BbnfBootstrapEnum_alloc(state).__c3(__depth79))
                } else {
                    __BbnfBootstrapEnum_alloc(state).__s3().truncate(__depth79);
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
                let __depth83 = __BbnfBootstrapEnum_alloc(state).__s3().len();
                loop {
                    let __prev84 = state.offset;
                    match (|| {
                        let __v81 = {
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
                        let __sp82 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__v81, __sp82))
                    })() {
                        Some(__value) => {
                            __BbnfBootstrapEnum_alloc(state).__s3().push(__value);
                            if state.offset == __prev84 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __prev84;
                            break;
                        }
                    }
                }
                if (__BbnfBootstrapEnum_alloc(state).__s3().len() - __depth83) >= 1usize
                {
                    Some(__BbnfBootstrapEnum_alloc(state).__c3(__depth83))
                } else {
                    __BbnfBootstrapEnum_alloc(state).__s3().truncate(__depth83);
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
            {
                let __chain_head86 = Self::__mapped_factor(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __chain_depth85 = __BbnfBootstrapEnum_alloc(state).__s1().len();
                loop {
                    let __chain_prev87 = state.offset;
                    match (|| {
                        let __chain_op88 = {
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
                        let __chain_rhs89 = Self::__mapped_factor(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op88, __chain_rhs89))
                    })() {
                        Some(__value) => {
                            let (__chain_op88, __chain_rhs89) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s1()
                                .push((__chain_op88, __chain_rhs89));
                            if state.offset == __chain_prev87 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __chain_prev87;
                            break;
                        }
                    }
                }
                Some((
                    __chain_head86,
                    __BbnfBootstrapEnum_alloc(state).__c1(__chain_depth85),
                ))
            }
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
                    let __sp94 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v95 = ::parse_that::scan_ident(state)
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::identifier(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    let __v96 = {
                        let __depth92 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                        loop {
                            let __prev93 = state.offset;
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
                                let __sp90 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                let __v91 = ::parse_that::scan_ident(state)
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::identifier(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                Some((__sp90, __v91))
                            })() {
                                Some(__value) => {
                                    __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                    if state.offset == __prev93 {
                                        break;
                                    }
                                }
                                None => {
                                    state.offset = __prev93;
                                    break;
                                }
                            }
                        }
                        Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth92))
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
                    let __sp97 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v98 = Self::__rhs(state)
                        .map(|__v| {
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    Some((__sp94, __v95, __v96, __sp97, __v98))
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
                let __v132 = (|| {
                    let __v121 = {
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
                    let __v122 = {
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
                                    let __v107 = ::parse_that::scan_ident(state)
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::identifier(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        })?;
                                    let __v108 = {
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
                                            let __sp103 = ::parse_that::Span::new(
                                                __sp_start,
                                                state.offset,
                                                state.src,
                                            );
                                            let __v104 = {
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                let __ws_inner = Self::__call_arg(state)
                                                    .map(|__v| {
                                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                    });
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __ws_inner
                                            }?;
                                            let __v105 = {
                                                let __depth101 = __BbnfBootstrapEnum_alloc(state)
                                                    .__s0()
                                                    .len();
                                                loop {
                                                    let __prev102 = state.offset;
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
                                                        let __sp99 = ::parse_that::Span::new(
                                                            __sp_start,
                                                            state.offset,
                                                            state.src,
                                                        );
                                                        let __v100 = {
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            let __ws_inner = Self::__call_arg(state)
                                                                .map(|__v| {
                                                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                                });
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            __ws_inner
                                                        }?;
                                                        Some((__sp99, __v100))
                                                    })() {
                                                        Some(__value) => {
                                                            __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
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
                                                Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth101))
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
                                            let __sp106 = ::parse_that::Span::new(
                                                __sp_start,
                                                state.offset,
                                                state.src,
                                            );
                                            Some((__sp103, __v104, __v105, __sp106))
                                        })())() {
                                            Some(__v) => Some(Some(__v)),
                                            None => {
                                                state.offset = __cp;
                                                Some(None)
                                            }
                                        }
                                    }?;
                                    Some((__v107, __v108))
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
                                    let __sp109 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v110 = {
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
                                    let __sp111 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp109, __v110, __sp111))
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
                                    let __sp112 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v113 = {
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
                                    let __sp114 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp112, __v113, __sp114))
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
                                    let __sp115 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v116 = {
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
                                    let __sp117 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp115, __v116, __sp117))
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
                                    let __sp118 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v119 = {
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
                                    let __sp120 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp118, __v119, __sp120))
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
                    let __v123 = {
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
                    let __v124 = {
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
                    Some((__v121, __v122, __v123, __v124))
                })()
                    .map(|__inner| {
                        let __v = BbnfBootstrapEnum::factor(__inner);
                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                    })?;
                let __v133 = {
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
                        let __sp130 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v131 = (|| {
                            let __v128 = Self::__value_expr(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            let __v129 = {
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
                                        let __sp126 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        let __v127 = (|| {
                                            let __kd_cp125 = state.offset;
                                            if let Some(ref __kd_s) = ::parse_that::scan_ident(state) {
                                                let __kd_bytes = &state.src_bytes[__kd_s.start..__kd_s.end];
                                                let __kd_len = __kd_bytes.len();
                                                if (__kd_len == 2usize && __kd_bytes == &[b'u', b'8']) {
                                                    state.offset = __kd_cp125;
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
                                                    state.offset = __kd_cp125;
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
                                                    state.offset = __kd_cp125;
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
                                                    state.offset = __kd_cp125;
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
                                                    state.offset = __kd_cp125;
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
                                                    state.offset = __kd_cp125;
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
                                                    state.offset = __kd_cp125;
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
                                                    state.offset = __kd_cp125;
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
                                                    state.offset = __kd_cp125;
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
                                                    state.offset = __kd_cp125;
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
                                            state.offset = __kd_cp125;
                                            ::parse_that::scan_ident(state)
                                        })()
                                            .map(|__inner| {
                                                let __v = BbnfBootstrapEnum::type_name(__inner);
                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                            })?;
                                        Some((__sp126, __v127))
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
                            Some((__v128, __v129))
                        })()?;
                        Some((__sp130, __v131))
                    })())() {
                        Some(__v) => Some(Some(__v)),
                        None => {
                            state.offset = __cp;
                            Some(None)
                        }
                    }
                }?;
                Some((__v132, __v133))
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
                let __v134 = ::parse_that::scan_ident(state)
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
                let __sp135 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                let __v136 = {
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
                let __sp137 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                Some((__v134, __sp135, __v136, __sp137))
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
                        let __sp138 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v139 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = ::parse_that::scan_ident(state)
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::identifier(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __v140 = {
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
                        let __sp141 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp138, __v139, __v140, __sp141))
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
                        let __sp146 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v147 = {
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
                        let __v148 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = {
                                let __depth144 = __BbnfBootstrapEnum_alloc(state)
                                    .__s2()
                                    .len();
                                loop {
                                    let __prev145 = state.offset;
                                    match (|| {
                                        let __v142 = ::parse_that::scan_ident(state)
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
                                        let __sp143 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        Some((__v142, __sp143))
                                    })()
                                        .map(|__v| BbnfBootstrapEnum::pretty_hint(__v))
                                    {
                                        Some(__value) => {
                                            __BbnfBootstrapEnum_alloc(state).__s2().push(__value);
                                            if state.offset == __prev145 {
                                                break;
                                            }
                                        }
                                        None => {
                                            state.offset = __prev145;
                                            break;
                                        }
                                    }
                                }
                                if (__BbnfBootstrapEnum_alloc(state).__s2().len()
                                    - __depth144) >= 1usize
                                {
                                    Some(__BbnfBootstrapEnum_alloc(state).__c2(__depth144))
                                } else {
                                    __BbnfBootstrapEnum_alloc(state)
                                        .__s2()
                                        .truncate(__depth144);
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
                        let __sp149 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp146, __v147, __v148, __sp149))
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
                        let __sp150 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v151 = {
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
                        let __sp152 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp150, __v151, __sp152))
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
                        let __sp155 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp153, __v154, __sp155))
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
                        let __sp156 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v157 = {
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
                        let __sp158 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp156, __v157, __sp158))
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
                        let __sp162 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v163 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = ::parse_that::scan_ident(state)
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::identifier(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __v164 = {
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
                                let __sp160 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                let __v161 = {
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ws_inner = (|| {
                                        let __kd_cp159 = state.offset;
                                        if let Some(ref __kd_s) = ::parse_that::scan_ident(state) {
                                            let __kd_bytes = &state.src_bytes[__kd_s.start..__kd_s.end];
                                            let __kd_len = __kd_bytes.len();
                                            if (__kd_len == 2usize && __kd_bytes == &[b'u', b'8']) {
                                                state.offset = __kd_cp159;
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
                                                state.offset = __kd_cp159;
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
                                                state.offset = __kd_cp159;
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
                                                state.offset = __kd_cp159;
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
                                                state.offset = __kd_cp159;
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
                                                state.offset = __kd_cp159;
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
                                                state.offset = __kd_cp159;
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
                                                state.offset = __kd_cp159;
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
                                                state.offset = __kd_cp159;
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
                                                state.offset = __kd_cp159;
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
                                        state.offset = __kd_cp159;
                                        ::parse_that::scan_ident(state)
                                    })()
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::type_name(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        });
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __ws_inner
                                }?;
                                Some((__sp160, __v161))
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
                        let __sp165 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp162, __v163, __v164, __sp165))
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
                let __depth166 = __BbnfBootstrapEnum_alloc(state).__s2().len();
                loop {
                    let __prev167 = state.offset;
                    match {
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ws_inner = Self::__grammar_item(state);
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __ws_inner
                    } {
                        Some(__value) => {
                            __BbnfBootstrapEnum_alloc(state).__s2().push(__value);
                            if state.offset == __prev167 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __prev167;
                            break;
                        }
                    }
                }
                Some(__BbnfBootstrapEnum_alloc(state).__c2(__depth166))
            }
        })()
            .map(|__x| BbnfBootstrapEnum::grammar(__x))
    }
    pub fn grammar<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__grammar)
    }
    #[allow(non_snake_case)]
    fn __mul_op_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp2 = state.offset;
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
                        state.offset = __pretty_cp2;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp1 = state.offset;
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
                                state.offset = __pretty_cp1;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp0 = state.offset;
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
    fn __bool_lit_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp3 = state.offset;
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
                        state.offset = __pretty_cp3;
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
                    let __pretty_cp4 = state.offset;
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
                        state.offset = __pretty_cp4;
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
    fn __cmp_op_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp10 = state.offset;
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
                        state.offset = __pretty_cp10;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp9 = state.offset;
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
                                state.offset = __pretty_cp9;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp8 = state.offset;
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
                                        state.offset = __pretty_cp8;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp7 = state.offset;
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
                                                state.offset = __pretty_cp7;
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp6 = state.offset;
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
                                                        state.offset = __pretty_cp6;
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp5 = state.offset;
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
                                                                state.offset = __pretty_cp5;
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
    fn __big_comment_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp13 = state.offset;
                    let __pretty_bcp14 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            let __ows11 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows11..state.offset]);
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
                            let __ows12 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows12..state.offset]);
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp13;
                        __builder.restore(__pretty_bcp14);
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
    fn __binary_operators_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp17 = state.offset;
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
                        state.offset = __pretty_cp17;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp16 = state.offset;
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
                                state.offset = __pretty_cp16;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp15 = state.offset;
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
                                        state.offset = __pretty_cp15;
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
    fn __type_name_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp28 = state.offset;
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
                        state.offset = __pretty_cp28;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp27 = state.offset;
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
                                state.offset = __pretty_cp27;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp26 = state.offset;
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
                                        state.offset = __pretty_cp26;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp25 = state.offset;
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
                                                state.offset = __pretty_cp25;
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp24 = state.offset;
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
                                                        state.offset = __pretty_cp24;
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp23 = state.offset;
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
                                                                state.offset = __pretty_cp23;
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp22 = state.offset;
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
                                                                        state.offset = __pretty_cp22;
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp21 = state.offset;
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
                                                                                state.offset = __pretty_cp21;
                                                                            }
                                                                            __ok
                                                                        } {
                                                                            {
                                                                                if !{
                                                                                    let __pretty_cp20 = state.offset;
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
                                                                                        state.offset = __pretty_cp20;
                                                                                    }
                                                                                    __ok
                                                                                } {
                                                                                    {
                                                                                        if !{
                                                                                            let __pretty_cp19 = state.offset;
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
                                                                                                state.offset = __pretty_cp19;
                                                                                            }
                                                                                            __ok
                                                                                        } {
                                                                                            {
                                                                                                if !{
                                                                                                    let __pretty_cp18 = state.offset;
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
                                                                                                        state.offset = __pretty_cp18;
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
    fn __modifier_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp32 = state.offset;
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
                        state.offset = __pretty_cp32;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp31 = state.offset;
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
                                state.offset = __pretty_cp31;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp30 = state.offset;
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
                                        state.offset = __pretty_cp30;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp29 = state.offset;
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
                                                state.offset = __pretty_cp29;
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
    fn __literal_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp37 = state.offset;
                    let __pretty_bcp38 = __builder.checkpoint();
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
                        state.offset = __pretty_cp37;
                        __builder.restore(__pretty_bcp38);
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp35 = state.offset;
                            let __pretty_bcp36 = __builder.checkpoint();
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
                                state.offset = __pretty_cp35;
                                __builder.restore(__pretty_bcp36);
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp33 = state.offset;
                                    let __pretty_bcp34 = __builder.checkpoint();
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
                                        state.offset = __pretty_cp33;
                                        __builder.restore(__pretty_bcp34);
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
                    let mut __rep_count45 = 0usize;
                    while __rep_count45 < 4294967295 {
                        let __rep_cp46 = state.offset;
                        if !{
                            let __pretty_cp43 = state.offset;
                            let __pretty_bcp44 = __builder.checkpoint();
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
                                state.offset = __pretty_cp43;
                                __builder.restore(__pretty_bcp44);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp46;
                            break;
                        }
                        if state.offset == __rep_cp46 {
                            break;
                        }
                        __rep_count45 += 1;
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
                    let mut __rep_count49 = 0usize;
                    while __rep_count49 < 4294967295 {
                        let __rep_cp50 = state.offset;
                        if !{
                            let __pretty_cp47 = state.offset;
                            let __pretty_bcp48 = __builder.checkpoint();
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
                                state.offset = __pretty_cp47;
                                __builder.restore(__pretty_bcp48);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp50;
                            break;
                        }
                        if state.offset == __rep_cp50 {
                            break;
                        }
                        __rep_count49 += 1;
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
    fn __token_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows51 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows52 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows51..__ows52]);
                    let __ows53 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows53..state.offset]);
                };
                {
                    let __ows54 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows55 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows54..__ows55]);
                    let __ows56 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows56..state.offset]);
                };
                {
                    let _ = {
                        let __pretty_cp58 = state.offset;
                        let __pretty_bcp59 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp57 = state.offset;
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
                                        state.offset = __pretty_cp57;
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
                            state.offset = __pretty_cp58;
                            __builder.restore(__pretty_bcp59);
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
    fn __import_items_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows60 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows61 = state.offset;
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'{') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'{');
                    };
                    __builder.text_inline_ws(&state.src[__ows60..__ows61]);
                    let __ows62 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows62..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp72 = state.offset;
                        let __pretty_bcp73 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows70 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows70..state.offset]);
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
                                        let mut __rep_count68 = 0usize;
                                        while __rep_count68 < 4294967295 {
                                            let __rep_cp69 = state.offset;
                                            if !{
                                                let __pretty_cp66 = state.offset;
                                                let __pretty_bcp67 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        {
                                                            let __ows63 = state.offset;
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            let __ows64 = state.offset;
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b',');
                                                            };
                                                            __builder.text_inline_ws(&state.src[__ows63..__ows64]);
                                                            let __ows65 = state.offset;
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            __builder.text_inline_ws(&state.src[__ows65..state.offset]);
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
                                                    state.offset = __pretty_cp66;
                                                    __builder.restore(__pretty_bcp67);
                                                }
                                                __ok
                                            } {
                                                state.offset = __rep_cp69;
                                                break;
                                            }
                                            if state.offset == __rep_cp69 {
                                                break;
                                            }
                                            __rep_count68 += 1;
                                        }
                                    };
                                };
                                let __ows71 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows71..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp72;
                            __builder.restore(__pretty_bcp73);
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
    fn __debug_directive_prettify<'a>(
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
                    __builder.text_inline_ws(&state.src[__ows74..__ows75]);
                    let __ows76 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows76..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp80 = state.offset;
                        let __pretty_bcp81 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows78 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows78..state.offset]);
                                {
                                    if !{
                                        let __pretty_cp77 = state.offset;
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
                                            state.offset = __pretty_cp77;
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
                                let __ows79 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows79..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp80;
                            __builder.restore(__pretty_bcp81);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp83 = state.offset;
                        let __pretty_bcp84 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp82 = state.offset;
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
                                        state.offset = __pretty_cp82;
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
                            state.offset = __pretty_cp83;
                            __builder.restore(__pretty_bcp84);
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
                        let __pretty_cp85 = state.offset;
                        let __pretty_bcp86 = __builder.checkpoint();
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
                            state.offset = __pretty_cp85;
                            __builder.restore(__pretty_bcp86);
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
    fn __host_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows87 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows88 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows87..__ows88]);
                    let __ows89 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows89..state.offset]);
                };
                {
                    let __ows90 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows91 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows90..__ows91]);
                    let __ows92 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows92..state.offset]);
                };
                {
                    let _ = {
                        let __pretty_cp100 = state.offset;
                        let __pretty_bcp101 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    let __ows93 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ows94 = state.offset;
                                    {
                                        if state.src_bytes.get(state.offset).copied() != Some(b':')
                                        {
                                            return false;
                                        }
                                        state.offset += 1;
                                        __builder.char(b':');
                                    };
                                    __builder.text_inline_ws(&state.src[__ows93..__ows94]);
                                    let __ows95 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder.text_inline_ws(&state.src[__ows95..state.offset]);
                                };
                                {
                                    if !{
                                        let __pretty_cp98 = state.offset;
                                        let __pretty_bcp99 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows96 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder.text_inline_ws(&state.src[__ows96..state.offset]);
                                                if !Self::__type_name_prettify(state, __builder) {
                                                    return false;
                                                }
                                                let __ows97 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder.text_inline_ws(&state.src[__ows97..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp98;
                                            __builder.restore(__pretty_bcp99);
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
                            state.offset = __pretty_cp100;
                            __builder.restore(__pretty_bcp101);
                        }
                        __ok
                    };
                    true
                };
                {
                    let _ = {
                        let __pretty_cp103 = state.offset;
                        let __pretty_bcp104 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp102 = state.offset;
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
                                        state.offset = __pretty_cp102;
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
                            state.offset = __pretty_cp103;
                            __builder.restore(__pretty_bcp104);
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
    fn __type_annotation_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows105 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows106 = state.offset;
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b':') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b':');
                    };
                    __builder.text_inline_ws(&state.src[__ows105..__ows106]);
                    let __ows107 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows107..state.offset]);
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
                    let __ows108 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows109 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows108..__ows109]);
                    let __ows110 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows110..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp113 = state.offset;
                        let __pretty_bcp114 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows111 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows111..state.offset]);
                                if !Self::__regex_prettify(state, __builder) {
                                    return false;
                                }
                                let __ows112 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows112..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp113;
                            __builder.restore(__pretty_bcp114);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp116 = state.offset;
                        let __pretty_bcp117 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp115 = state.offset;
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
                                        state.offset = __pretty_cp115;
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
                            state.offset = __pretty_cp116;
                            __builder.restore(__pretty_bcp117);
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
                    let mut __rep_count124 = 0usize;
                    while __rep_count124 < 4294967295 {
                        let __rep_cp125 = state.offset;
                        if !{
                            let __pretty_cp122 = state.offset;
                            let __pretty_bcp123 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp120 = state.offset;
                                            let __pretty_bcp121 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows118 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows118..state.offset]);
                                                    if !Self::__mul_op_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows119 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows119..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp120;
                                                __builder.restore(__pretty_bcp121);
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
                                state.offset = __pretty_cp122;
                                __builder.restore(__pretty_bcp123);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp125;
                            break;
                        }
                        if state.offset == __rep_cp125 {
                            break;
                        }
                        __rep_count124 += 1;
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
                    let mut __rep_count131 = 0usize;
                    while __rep_count131 < 4294967295 {
                        let __rep_cp132 = state.offset;
                        if !{
                            let __pretty_cp129 = state.offset;
                            let __pretty_bcp130 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows126 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows127 = state.offset;
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
                                        __builder.text_inline_ws(&state.src[__ows126..__ows127]);
                                        let __ows128 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows128..state.offset]);
                                    };
                                    if !Self::__value_and_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp129;
                                __builder.restore(__pretty_bcp130);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp132;
                            break;
                        }
                        if state.offset == __rep_cp132 {
                            break;
                        }
                        __rep_count131 += 1;
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
                    let mut __rep_count140 = 0usize;
                    while __rep_count140 < 4294967295 {
                        let __rep_cp141 = state.offset;
                        if !{
                            let __pretty_cp138 = state.offset;
                            let __pretty_bcp139 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp136 = state.offset;
                                            let __pretty_bcp137 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows134 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows134..state.offset]);
                                                    {
                                                        if !{
                                                            let __pretty_cp133 = state.offset;
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
                                                                state.offset = __pretty_cp133;
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
                                                    let __ows135 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows135..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp136;
                                                __builder.restore(__pretty_bcp137);
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
                                state.offset = __pretty_cp138;
                                __builder.restore(__pretty_bcp139);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp141;
                            break;
                        }
                        if state.offset == __rep_cp141 {
                            break;
                        }
                        __rep_count140 += 1;
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
                    let mut __rep_count148 = 0usize;
                    while __rep_count148 < 4294967295 {
                        let __rep_cp149 = state.offset;
                        if !{
                            let __pretty_cp146 = state.offset;
                            let __pretty_bcp147 = __builder.checkpoint();
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
                                                    if !Self::__cmp_op_prettify(state, __builder) {
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
                                    if !Self::__value_add_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp146;
                                __builder.restore(__pretty_bcp147);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp149;
                            break;
                        }
                        if state.offset == __rep_cp149 {
                            break;
                        }
                        __rep_count148 += 1;
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
                    let mut __rep_count155 = 0usize;
                    while __rep_count155 < 4294967295 {
                        let __rep_cp156 = state.offset;
                        if !{
                            let __pretty_cp153 = state.offset;
                            let __pretty_bcp154 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows150 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows151 = state.offset;
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
                                        __builder.text_inline_ws(&state.src[__ows150..__ows151]);
                                        let __ows152 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows152..state.offset]);
                                    };
                                    if !Self::__value_cmp_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp153;
                                __builder.restore(__pretty_bcp154);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp156;
                            break;
                        }
                        if state.offset == __rep_cp156 {
                            break;
                        }
                        __rep_count155 += 1;
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
                    let mut __rep_count162 = 0usize;
                    while __rep_count162 < 4294967295 {
                        let __rep_cp163 = state.offset;
                        if !{
                            let __pretty_cp160 = state.offset;
                            let __pretty_bcp161 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows157 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows158 = state.offset;
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b',');
                                        };
                                        __builder.text_inline_ws(&state.src[__ows157..__ows158]);
                                        let __ows159 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows159..state.offset]);
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
                                state.offset = __pretty_cp160;
                                __builder.restore(__pretty_bcp161);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp163;
                            break;
                        }
                        if state.offset == __rep_cp163 {
                            break;
                        }
                        __rep_count162 += 1;
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
                        let __pretty_cp171 = state.offset;
                        let __pretty_bcp172 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !Self::__value_expr_prettify(state, __builder) {
                                    return false;
                                }
                                {
                                    let mut __rep_count169 = 0usize;
                                    while __rep_count169 < 4294967295 {
                                        let __rep_cp170 = state.offset;
                                        if !{
                                            let __pretty_cp167 = state.offset;
                                            let __pretty_bcp168 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    {
                                                        let __ows164 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        let __ows165 = state.offset;
                                                        {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                            {
                                                                return false;
                                                            }
                                                            state.offset += 1;
                                                            __builder.char(b',');
                                                        };
                                                        __builder.text_inline_ws(&state.src[__ows164..__ows165]);
                                                        let __ows166 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows166..state.offset]);
                                                    };
                                                    if !Self::__value_expr_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp167;
                                                __builder.restore(__pretty_bcp168);
                                            }
                                            __ok
                                        } {
                                            state.offset = __rep_cp170;
                                            break;
                                        }
                                        if state.offset == __rep_cp170 {
                                            break;
                                        }
                                        __rep_count169 += 1;
                                    }
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp171;
                            __builder.restore(__pretty_bcp172);
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
                    let __pretty_cp173 = state.offset;
                    let __pretty_bcp174 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        if !Self::__value_closure_prettify(state, __builder) {
                            return false;
                        }
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp173;
                        __builder.restore(__pretty_bcp174);
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
                    let __pretty_cp193 = state.offset;
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
                        state.offset = __pretty_cp193;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp192 = state.offset;
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
                                state.offset = __pretty_cp192;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp190 = state.offset;
                                    let __pretty_bcp191 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            if !{
                                                let __pretty_cp175 = state.offset;
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
                                                    state.offset = __pretty_cp175;
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
                                        state.offset = __pretty_cp190;
                                        __builder.restore(__pretty_bcp191);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp188 = state.offset;
                                            let __pretty_bcp189 = __builder.checkpoint();
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
                                                state.offset = __pretty_cp188;
                                                __builder.restore(__pretty_bcp189);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp186 = state.offset;
                                                    let __pretty_bcp187 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        if !Self::__value_fn_call_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp186;
                                                        __builder.restore(__pretty_bcp187);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp184 = state.offset;
                                                            let __pretty_bcp185 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                if !Self::__value_input_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp184;
                                                                __builder.restore(__pretty_bcp185);
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp182 = state.offset;
                                                                    let __pretty_bcp183 = __builder.checkpoint();
                                                                    let __ok = (|| -> bool {
                                                                        if !Self::__value_path_prettify(state, __builder) {
                                                                            return false;
                                                                        }
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp182;
                                                                        __builder.restore(__pretty_bcp183);
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp180 = state.offset;
                                                                            let __pretty_bcp181 = __builder.checkpoint();
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
                                                                                            let __pretty_cp178 = state.offset;
                                                                                            let __pretty_bcp179 = __builder.checkpoint();
                                                                                            let __ok = (|| -> bool {
                                                                                                {
                                                                                                    let __ows176 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows176..state.offset]);
                                                                                                    if !Self::__value_expr_prettify(state, __builder) {
                                                                                                        return false;
                                                                                                    }
                                                                                                    let __ows177 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows177..state.offset]);
                                                                                                };
                                                                                                true
                                                                                            })();
                                                                                            if !__ok {
                                                                                                state.offset = __pretty_cp178;
                                                                                                __builder.restore(__pretty_bcp179);
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
                                                                                state.offset = __pretty_cp180;
                                                                                __builder.restore(__pretty_bcp181);
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
                    let __pretty_cp195 = state.offset;
                    let __pretty_bcp196 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            {
                                if !{
                                    let __pretty_cp194 = state.offset;
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
                                        state.offset = __pretty_cp194;
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
                        state.offset = __pretty_cp195;
                        __builder.restore(__pretty_bcp196);
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
    fn __import_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows197 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows198 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows197..__ows198]);
                    let __ows199 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows199..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp211 = state.offset;
                        let __pretty_bcp212 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows209 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows209..state.offset]);
                                {
                                    if !{
                                        let __pretty_cp207 = state.offset;
                                        let __pretty_bcp208 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                {
                                                    if !{
                                                        let __pretty_cp202 = state.offset;
                                                        let __pretty_bcp203 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            {
                                                                let __ows200 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                __builder
                                                                    .text_inline_ws(&state.src[__ows200..state.offset]);
                                                                if !Self::__import_items_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                let __ows201 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                __builder
                                                                    .text_inline_ws(&state.src[__ows201..state.offset]);
                                                            };
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp202;
                                                            __builder.restore(__pretty_bcp203);
                                                        }
                                                        __ok
                                                    } {
                                                        return false;
                                                    }
                                                };
                                                {
                                                    let __ows204 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    let __ows205 = state.offset;
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
                                                    __builder.text_inline_ws(&state.src[__ows204..__ows205]);
                                                    let __ows206 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows206..state.offset]);
                                                };
                                                if !Self::__import_path_prettify(state, __builder) {
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
                                        if !Self::__import_path_prettify(state, __builder) {
                                            return false;
                                        }
                                    }
                                };
                                let __ows210 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows210..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp211;
                            __builder.restore(__pretty_bcp212);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp214 = state.offset;
                        let __pretty_bcp215 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp213 = state.offset;
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
                                        state.offset = __pretty_cp213;
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
                            state.offset = __pretty_cp214;
                            __builder.restore(__pretty_bcp215);
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
    fn __pretty_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows216 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows217 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows216..__ows217]);
                    let __ows218 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows218..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp222 = state.offset;
                        let __pretty_bcp223 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows220 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows220..state.offset]);
                                {
                                    if !{
                                        let __pretty_cp219 = state.offset;
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
                                            state.offset = __pretty_cp219;
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
                                let __ows221 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows221..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp222;
                            __builder.restore(__pretty_bcp223);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    if !{
                        let __pretty_cp232 = state.offset;
                        let __pretty_bcp233 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows230 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows230..state.offset]);
                                {
                                    let __rep_start228 = state.offset;
                                    let __rep_bcp229 = __builder.checkpoint();
                                    let mut __rep_count226 = 0usize;
                                    while __rep_count226 < 4294967295 {
                                        let __rep_cp227 = state.offset;
                                        if !{
                                            let __pretty_cp224 = state.offset;
                                            let __pretty_bcp225 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__pretty_hint_prettify(state, __builder) {
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
                                            state.offset = __rep_cp227;
                                            break;
                                        }
                                        if state.offset == __rep_cp227 {
                                            break;
                                        }
                                        __rep_count226 += 1;
                                    }
                                    if __rep_count226 < 1 {
                                        state.offset = __rep_start228;
                                        __builder.restore(__rep_bcp229);
                                        return false;
                                    }
                                };
                                let __ows231 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows231..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp232;
                            __builder.restore(__pretty_bcp233);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp235 = state.offset;
                        let __pretty_bcp236 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp234 = state.offset;
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
                                        state.offset = __pretty_cp234;
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
                            state.offset = __pretty_cp235;
                            __builder.restore(__pretty_bcp236);
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
                        let __ows359 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows360 = state.offset;
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'=') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'=');
                        };
                        __builder.text_inline_ws(&state.src[__ows359..__ows360]);
                        let __ows361 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows361..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp364 = state.offset;
                            let __pretty_bcp365 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows362 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows362..state.offset]);
                                    if !Self::__rhs_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows363 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows363..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp364;
                                __builder.restore(__pretty_bcp365);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        if !{
                            let __pretty_cp366 = state.offset;
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
                                state.offset = __pretty_cp366;
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
    fn __recover_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows367 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows368 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows367..__ows368]);
                    let __ows369 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows369..state.offset]);
                };
                {
                    let __ows370 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows371 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows370..__ows371]);
                    let __ows372 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows372..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp375 = state.offset;
                        let __pretty_bcp376 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows373 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows373..state.offset]);
                                if !Self::__rhs_prettify(state, __builder) {
                                    return false;
                                }
                                let __ows374 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows374..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp375;
                            __builder.restore(__pretty_bcp376);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp378 = state.offset;
                        let __pretty_bcp379 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp377 = state.offset;
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
                                        state.offset = __pretty_cp377;
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
                            state.offset = __pretty_cp378;
                            __builder.restore(__pretty_bcp379);
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

