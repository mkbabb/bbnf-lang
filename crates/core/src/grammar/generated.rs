//! AUTO-GENERATED from grammar/bbnf/bbnf.bbnf — do not edit manually.
//! Regenerate: scripts/bootstrap-bbnf.sh

use ::parse_that::*;

pub struct BbnfBootstrap;

#[allow(non_upper_case_globals)]
pub const GRAMMAR_BbnfBootstrap: [&'static str; 1usize] = [
    "// BBNF \u{2014} Better Backus-Naur Form\n// Self-hosted grammar definition.\n\n@import { value_expr, type_annotation } from \"expressions\" ;\n\n// \u{2500}\u{2500}\u{2500} Terminals \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\nidentifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ ;\n\nliteral = \"\\\"\" , /(\\\\.|[^\"\\\\])*/  , \"\\\"\"\n        | \"\'\"  , /(\\\\.|[^\'\\\\])*/  , \"\'\"\n        | \"`\"  , /(\\\\.|[^`\\\\])*/  , \"`\" ;\n\nregex = \"/\" , /(\\\\.|[^\\/])+/ , \"/\" ;\n\nbig_comment = ( \"/*\" , /[^\\*]*/ , \"*/\" ) ?w ;\ncomment = ( \"//\" , /.*/ ) ?w ;\n\n// \u{2500}\u{2500}\u{2500} Expressions \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\nlhs = identifier ;\n\n// Grammar function call args: each arg is a single binary_factor\n// (alternation of binary_factors, no comma-concatenation).\n// This avoids ambiguity between call arg commas and concatenation commas.\ncall_arg = ( binary_factor ?w , \"|\" ? ) + ;\n\nterm = \"\u{3b5}\" | \"epsilon\"\n     | identifier , ( \"(\" , call_arg ?w , ( \",\" ?w , call_arg ?w ) * , \")\" ) ?\n     | literal\n     | regex\n     | \"@{\" , rhs ?w , \"}\"\n     | \"(\" , rhs ?w , \")\"\n     | \"[\" , rhs ?w , \"]\"\n     | \"{\" , rhs ?w , \"}\" ;\n\nmodifier = \"?w\" | \"?\" | \"*\" | \"+\" ;\n\nmodifier = \"?w\" | \"?\" | \"*\" | \"+\" ;\nfactor = big_comment ? , term ?w , modifier ? , big_comment ? ;\n\n// Map syntax: factor -> value_expr : type\nmapped_factor = factor , ( \"->\" ?w , ( value_expr , type_annotation ? ) ) ? ;\n\nbinary_operators = \"<<\" | \">>\" | \"-\" ;\nbinary_factor = mapped_factor , ( binary_operators ?w , mapped_factor ) * ;\n\nconcatenation = ( binary_factor ?w , \",\" ? ) + ;\nalternation = ( concatenation ?w , \"|\" ? ) + ;\n\n// Closures at rule level: |params| rhs (grammar functions)\nclosure = \"|\" , identifier , ( \",\" ?w , identifier ) * , \"|\" ?w , rhs ;\nrhs = closure | alternation ;\n\n// \u{2500}\u{2500}\u{2500} Rules and Directives \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\nrule = lhs , \"=\" ?w , rhs ?w , ( \";\" | \".\" ) ;\n\nimport_path = \"\\\"\" , /(\\\\.|[^\"\\\\])*/ , \"\\\"\" ;\nimport_items = \"{\" ?w , identifier , ( \",\" ?w , identifier ) * , \"}\" ;\nimport_directive = \"@import\" ?w , (\n      import_items ?w , \"from\" ?w , import_path\n    | import_path\n) , ( \";\" | \".\" ) ? ;\n\nrecover_directive = \"@recover\" ?w , identifier ?w , rhs ?w , ( \";\" | \".\" ) ? ;\n\npretty_hint = identifier , ( \"(\" , /[^)]*/ , \")\" ) ? ;\npretty_directive = \"@pretty\" ?w , ( \"*\" | identifier ) ?w , (pretty_hint+) ?w , ( \";\" | \".\" ) ? ;\n\nws_directive = \"@ws\" ?w , regex ?w , ( \";\" | \".\" ) ? ;\ntoken_directive = \"@token\" ?w , identifier ?w , ( \";\" | \".\" ) ? ;\ndebug_directive = \"@debug\" ?w , ( \"*\" | identifier ) ?w , ( \";\" | \".\" ) ? ;\nhost_directive = \"@host\" ?w , identifier ?w , ( \";\" | \".\" ) ? ;\n\ndirective = import_directive\n          | recover_directive\n          | pretty_directive\n          | ws_directive\n          | token_directive\n          | debug_directive\n          | host_directive ;\n\n// Grammar: sequence of comments, directives, and rules in any order.\ngrammar = ( comment ? , ( directive | rule ) ?w , comment ? ) * ;\n\n@pretty grammar block ;\n@pretty rule group ;\n@pretty alternation group ;\n",
];
#[derive(Debug)]
pub enum BbnfBootstrapEnum<'a> {
    mul_op(::parse_that::Span<'a>),
    string_lit(::parse_that::Span<'a>),
    bool_lit(::parse_that::Span<'a>),
    value_ident(::parse_that::Span<'a>),
    float_lit(::parse_that::Span<'a>),
    int_lit(::parse_that::Span<'a>),
    add_op(::parse_that::Span<'a>),
    cmp_op(::parse_that::Span<'a>),
    type_name(::parse_that::Span<'a>),
    identifier(::parse_that::Span<'a>),
    regex(::parse_that::Span<'a>),
    literal(::parse_that::Span<'a>),
    modifier(::parse_that::Span<'a>),
    big_comment(::parse_that::Span<'a>),
    binary_operators(::parse_that::Span<'a>),
    import_path(::parse_that::Span<'a>),
    comment(::parse_that::Span<'a>),
    value_input(
        (
            ::parse_that::Span<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    type_annotation((::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)),
    host_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    lhs(&'a BbnfBootstrapEnum<'a>),
    pretty_hint((&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)),
    import_items(
        (
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
            ::parse_that::Span<'a>,
        ),
    ),
    token_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    debug_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
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
    value_atom(&'a BbnfBootstrapEnum<'a>),
    value_unary(&'a BbnfBootstrapEnum<'a>),
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
    grammar(
        &'a [(
            Option<&'a BbnfBootstrapEnum<'a>>,
            &'a BbnfBootstrapEnum<'a>,
            Option<&'a BbnfBootstrapEnum<'a>>,
        )],
    ),
    debug_directive_0(::parse_that::Span<'a>),
    value_atom_0(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    value_unary_0((::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)),
    pretty_directive_0(::parse_that::Span<'a>),
    import_directive_0(
        (&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>),
    ),
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
            BbnfBootstrapEnum::string_lit(__self_0) => {
                BbnfBootstrapEnum::string_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::bool_lit(__self_0) => {
                BbnfBootstrapEnum::bool_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_ident(__self_0) => {
                BbnfBootstrapEnum::value_ident(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::float_lit(__self_0) => {
                BbnfBootstrapEnum::float_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::int_lit(__self_0) => {
                BbnfBootstrapEnum::int_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::add_op(__self_0) => {
                BbnfBootstrapEnum::add_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::cmp_op(__self_0) => {
                BbnfBootstrapEnum::cmp_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::type_name(__self_0) => {
                BbnfBootstrapEnum::type_name(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::identifier(__self_0) => {
                BbnfBootstrapEnum::identifier(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::regex(__self_0) => {
                BbnfBootstrapEnum::regex(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::literal(__self_0) => {
                BbnfBootstrapEnum::literal(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::modifier(__self_0) => {
                BbnfBootstrapEnum::modifier(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::big_comment(__self_0) => {
                BbnfBootstrapEnum::big_comment(::core::clone::Clone::clone(__self_0))
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
            BbnfBootstrapEnum::value_input(__self_0) => {
                BbnfBootstrapEnum::value_input(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::type_annotation(__self_0) => {
                BbnfBootstrapEnum::type_annotation(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::host_directive(__self_0) => {
                BbnfBootstrapEnum::host_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::lhs(__self_0) => {
                BbnfBootstrapEnum::lhs(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::pretty_hint(__self_0) => {
                BbnfBootstrapEnum::pretty_hint(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::import_items(__self_0) => {
                BbnfBootstrapEnum::import_items(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::token_directive(__self_0) => {
                BbnfBootstrapEnum::token_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::debug_directive(__self_0) => {
                BbnfBootstrapEnum::debug_directive(::core::clone::Clone::clone(__self_0))
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
            BbnfBootstrapEnum::value_fn_call(__self_0) => {
                BbnfBootstrapEnum::value_fn_call(::core::clone::Clone::clone(__self_0))
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
            BbnfBootstrapEnum::value_atom(__self_0) => {
                BbnfBootstrapEnum::value_atom(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_unary(__self_0) => {
                BbnfBootstrapEnum::value_unary(::core::clone::Clone::clone(__self_0))
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
    __s4: ::std::cell::UnsafeCell<
        Vec<
            (
                Option<&'a BbnfBootstrapEnum<'a>>,
                &'a BbnfBootstrapEnum<'a>,
                Option<&'a BbnfBootstrapEnum<'a>>,
            ),
        >,
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
            __s4: ::std::cell::UnsafeCell::new(Vec::with_capacity(64)),
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
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __s4(
        &self,
    ) -> &mut Vec<
        (
            Option<&'a BbnfBootstrapEnum<'a>>,
            &'a BbnfBootstrapEnum<'a>,
            Option<&'a BbnfBootstrapEnum<'a>>,
        ),
    > {
        unsafe { &mut *self.__s4.get() }
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __c4(
        &'a self,
        depth: usize,
    ) -> &'a [(
        Option<&'a BbnfBootstrapEnum<'a>>,
        &'a BbnfBootstrapEnum<'a>,
        Option<&'a BbnfBootstrapEnum<'a>>,
    )] {
        let s = self.__s4();
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
    fn __value_mul<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __chain_head7 = Self::__value_unary(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __chain_depth6 = __BbnfBootstrapEnum_alloc(state).__s1().len();
                loop {
                    let __chain_prev8 = state.offset;
                    match (|| {
                        let __chain_op9 = {
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
                        let __chain_rhs10 = Self::__value_unary(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op9, __chain_rhs10))
                    })() {
                        Some(__value) => {
                            let (__chain_op9, __chain_rhs10) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s1()
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
                    __BbnfBootstrapEnum_alloc(state).__c1(__chain_depth6),
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
                let __chain_head12 = Self::__value_mul(state)
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
                        let __chain_rhs15 = Self::__value_mul(state)
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
                let __chain_head17 = Self::__value_add(state)
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
                        let __chain_rhs20 = Self::__value_add(state)
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
                let __chain_head22 = Self::__value_cmp(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __chain_depth21 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                loop {
                    let __chain_prev23 = state.offset;
                    match (|| {
                        let __chain_op24 = (|| {
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
                        let __chain_rhs25 = Self::__value_cmp(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op24, __chain_rhs25))
                    })() {
                        Some(__value) => {
                            let (__chain_op24, __chain_rhs25) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s0()
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
                    __BbnfBootstrapEnum_alloc(state).__c0(__chain_depth21),
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
                    let __sp30 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v31 = ::parse_that::scan_ident(state)
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::value_ident(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    let __v32 = {
                        let __depth28 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                        loop {
                            let __prev29 = state.offset;
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
                                let __sp26 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                let __v27 = ::parse_that::scan_ident(state)
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::value_ident(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                Some((__sp26, __v27))
                            })() {
                                Some(__value) => {
                                    __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                    if state.offset == __prev29 {
                                        break;
                                    }
                                }
                                None => {
                                    state.offset = __prev29;
                                    break;
                                }
                            }
                        }
                        Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth28))
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
                    let __sp33 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v34 = Self::__value_expr(state)
                        .map(|__v| {
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    Some((__sp30, __v31, __v32, __sp33, __v34))
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
                    let __chain_head36 = Self::__value_and(state)
                        .map(|__v| {
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    let __chain_depth35 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                    loop {
                        let __chain_prev37 = state.offset;
                        match (|| {
                            let __chain_op38 = (|| {
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
                            let __chain_rhs39 = Self::__value_and(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            Some((__chain_op38, __chain_rhs39))
                        })() {
                            Some(__value) => {
                                let (__chain_op38, __chain_rhs39) = __value;
                                __BbnfBootstrapEnum_alloc(state)
                                    .__s0()
                                    .push((__chain_op38, __chain_rhs39));
                                if state.offset == __chain_prev37 {
                                    break;
                                }
                            }
                            None => {
                                state.offset = __chain_prev37;
                                break;
                            }
                        }
                    }
                    Some((
                        __chain_head36,
                        __BbnfBootstrapEnum_alloc(state).__c0(__chain_depth35),
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
                        let __v45 = ::parse_that::scan_ident(state)
                            .map(|__inner| {
                                let __v = BbnfBootstrapEnum::value_ident(__inner);
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
                        let __sp46 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v47 = {
                            let __cp = state.offset;
                            match (|| {
                                let __chain_head41 = Self::__value_expr(state)
                                    .map(|__v| {
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                let __chain_depth40 = __BbnfBootstrapEnum_alloc(state)
                                    .__s0()
                                    .len();
                                loop {
                                    let __chain_prev42 = state.offset;
                                    match (|| {
                                        let __chain_op43 = (|| {
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
                                        let __chain_rhs44 = Self::__value_expr(state)
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
                        let __sp48 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__v45, __sp46, __v47, __sp48))
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
                    let __result = ::parse_that::scan_ident(state)
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::value_ident(__inner);
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
                        let __sp49 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v50 = {
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
                        let __sp51 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp49, __v50, __sp51))
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
                        let __sp52 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v53 = Self::__value_atom(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__sp52, __v53))
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
                let __sp65 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                let __v66 = (|| {
                    {
                        let __cp = state.offset;
                        let __result = ((|| {
                            let __v62 = {
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
                                    let __sp58 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v59 = ::parse_that::scan_ident(state)
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::identifier(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        })?;
                                    let __v60 = {
                                        let __depth56 = __BbnfBootstrapEnum_alloc(state)
                                            .__s0()
                                            .len();
                                        loop {
                                            let __prev57 = state.offset;
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
                                                let __sp54 = ::parse_that::Span::new(
                                                    __sp_start,
                                                    state.offset,
                                                    state.src,
                                                );
                                                let __v55 = ::parse_that::scan_ident(state)
                                                    .map(|__inner| {
                                                        let __v = BbnfBootstrapEnum::identifier(__inner);
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
                                    let __sp61 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp58, __v59, __v60, __sp61))
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
                            let __sp63 = ::parse_that::Span::new(
                                __sp_start,
                                state.offset,
                                state.src,
                            );
                            let __v64 = (|| {
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
                            Some((__v62, __sp63, __v64))
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
                })()?;
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
                let __sp67 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                Some((__sp65, __v66, __sp67))
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
                let __depth74 = __BbnfBootstrapEnum_alloc(state).__s3().len();
                loop {
                    let __prev75 = state.offset;
                    match (|| {
                        let __v72 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = {
                                let __depth70 = __BbnfBootstrapEnum_alloc(state)
                                    .__s3()
                                    .len();
                                loop {
                                    let __prev71 = state.offset;
                                    match (|| {
                                        let __v68 = {
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
                                        let __sp69 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        Some((__v68, __sp69))
                                    })() {
                                        Some(__value) => {
                                            __BbnfBootstrapEnum_alloc(state).__s3().push(__value);
                                            if state.offset == __prev71 {
                                                break;
                                            }
                                        }
                                        None => {
                                            state.offset = __prev71;
                                            break;
                                        }
                                    }
                                }
                                if (__BbnfBootstrapEnum_alloc(state).__s3().len()
                                    - __depth70) >= 1usize
                                {
                                    Some(__BbnfBootstrapEnum_alloc(state).__c3(__depth70))
                                } else {
                                    __BbnfBootstrapEnum_alloc(state).__s3().truncate(__depth70);
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
                        let __sp73 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__v72, __sp73))
                    })() {
                        Some(__value) => {
                            __BbnfBootstrapEnum_alloc(state).__s3().push(__value);
                            if state.offset == __prev75 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __prev75;
                            break;
                        }
                    }
                }
                if (__BbnfBootstrapEnum_alloc(state).__s3().len() - __depth74) >= 1usize
                {
                    Some(__BbnfBootstrapEnum_alloc(state).__c3(__depth74))
                } else {
                    __BbnfBootstrapEnum_alloc(state).__s3().truncate(__depth74);
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
                let __depth78 = __BbnfBootstrapEnum_alloc(state).__s3().len();
                loop {
                    let __prev79 = state.offset;
                    match (|| {
                        let __v76 = {
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
                        let __sp77 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__v76, __sp77))
                    })() {
                        Some(__value) => {
                            __BbnfBootstrapEnum_alloc(state).__s3().push(__value);
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
                if (__BbnfBootstrapEnum_alloc(state).__s3().len() - __depth78) >= 1usize
                {
                    Some(__BbnfBootstrapEnum_alloc(state).__c3(__depth78))
                } else {
                    __BbnfBootstrapEnum_alloc(state).__s3().truncate(__depth78);
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
                let __chain_head81 = Self::__mapped_factor(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __chain_depth80 = __BbnfBootstrapEnum_alloc(state).__s1().len();
                loop {
                    let __chain_prev82 = state.offset;
                    match (|| {
                        let __chain_op83 = {
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
                        let __chain_rhs84 = Self::__mapped_factor(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op83, __chain_rhs84))
                    })() {
                        Some(__value) => {
                            let (__chain_op83, __chain_rhs84) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s1()
                                .push((__chain_op83, __chain_rhs84));
                            if state.offset == __chain_prev82 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __chain_prev82;
                            break;
                        }
                    }
                }
                Some((
                    __chain_head81,
                    __BbnfBootstrapEnum_alloc(state).__c1(__chain_depth80),
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
                    let __sp89 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v90 = ::parse_that::scan_ident(state)
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::identifier(__inner);
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    let __v91 = {
                        let __depth87 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                        loop {
                            let __prev88 = state.offset;
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
                                let __sp85 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                let __v86 = ::parse_that::scan_ident(state)
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::identifier(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                Some((__sp85, __v86))
                            })() {
                                Some(__value) => {
                                    __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                    if state.offset == __prev88 {
                                        break;
                                    }
                                }
                                None => {
                                    state.offset = __prev88;
                                    break;
                                }
                            }
                        }
                        Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth87))
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
                    let __sp92 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v93 = Self::__rhs(state)
                        .map(|__v| {
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    Some((__sp89, __v90, __v91, __sp92, __v93))
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
                let __v127 = (|| {
                    let __v116 = {
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
                    let __v117 = {
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
                                    let __v102 = ::parse_that::scan_ident(state)
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::identifier(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        })?;
                                    let __v103 = {
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
                                            let __sp98 = ::parse_that::Span::new(
                                                __sp_start,
                                                state.offset,
                                                state.src,
                                            );
                                            let __v99 = {
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                let __ws_inner = Self::__call_arg(state)
                                                    .map(|__v| {
                                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                    });
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __ws_inner
                                            }?;
                                            let __v100 = {
                                                let __depth96 = __BbnfBootstrapEnum_alloc(state)
                                                    .__s0()
                                                    .len();
                                                loop {
                                                    let __prev97 = state.offset;
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
                                                        let __sp94 = ::parse_that::Span::new(
                                                            __sp_start,
                                                            state.offset,
                                                            state.src,
                                                        );
                                                        let __v95 = {
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            let __ws_inner = Self::__call_arg(state)
                                                                .map(|__v| {
                                                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                                });
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            __ws_inner
                                                        }?;
                                                        Some((__sp94, __v95))
                                                    })() {
                                                        Some(__value) => {
                                                            __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                                            if state.offset == __prev97 {
                                                                break;
                                                            }
                                                        }
                                                        None => {
                                                            state.offset = __prev97;
                                                            break;
                                                        }
                                                    }
                                                }
                                                Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth96))
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
                                            let __sp101 = ::parse_that::Span::new(
                                                __sp_start,
                                                state.offset,
                                                state.src,
                                            );
                                            Some((__sp98, __v99, __v100, __sp101))
                                        })())() {
                                            Some(__v) => Some(Some(__v)),
                                            None => {
                                                state.offset = __cp;
                                                Some(None)
                                            }
                                        }
                                    }?;
                                    Some((__v102, __v103))
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
                                    let __sp104 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v105 = {
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
                                    let __sp106 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp104, __v105, __sp106))
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
                                    let __sp107 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v108 = {
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
                                    let __sp109 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp107, __v108, __sp109))
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
                                    let __sp110 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v111 = {
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
                                    let __sp112 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp110, __v111, __sp112))
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
                                    let __sp113 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v114 = {
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
                                    let __sp115 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp113, __v114, __sp115))
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
                    let __v118 = {
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
                    let __v119 = {
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
                    Some((__v116, __v117, __v118, __v119))
                })()
                    .map(|__inner| {
                        let __v = BbnfBootstrapEnum::factor(__inner);
                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                    })?;
                let __v128 = {
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
                        let __sp125 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v126 = (|| {
                            let __v123 = Self::__value_expr(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            let __v124 = {
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
                                        let __sp121 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        let __v122 = (|| {
                                            let __kd_cp120 = state.offset;
                                            if let Some(ref __kd_s) = ::parse_that::scan_ident(state) {
                                                let __kd_bytes = &state.src_bytes[__kd_s.start..__kd_s.end];
                                                let __kd_len = __kd_bytes.len();
                                                if (__kd_len == 2usize && __kd_bytes == &[b'u', b'8']) {
                                                    state.offset = __kd_cp120;
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
                                                    state.offset = __kd_cp120;
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
                                                    state.offset = __kd_cp120;
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
                                                    state.offset = __kd_cp120;
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
                                                    state.offset = __kd_cp120;
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
                                                    state.offset = __kd_cp120;
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
                                                    state.offset = __kd_cp120;
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
                                                    state.offset = __kd_cp120;
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
                                                    state.offset = __kd_cp120;
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
                                                    state.offset = __kd_cp120;
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
                                            state.offset = __kd_cp120;
                                            None
                                        })()
                                            .map(|__inner| {
                                                let __v = BbnfBootstrapEnum::type_name(__inner);
                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                            })?;
                                        Some((__sp121, __v122))
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
                            Some((__v123, __v124))
                        })()?;
                        Some((__sp125, __v126))
                    })())() {
                        Some(__v) => Some(Some(__v)),
                        None => {
                            state.offset = __cp;
                            Some(None)
                        }
                    }
                }?;
                Some((__v127, __v128))
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
                let __v129 = ::parse_that::scan_ident(state)
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
                let __sp130 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                let __v131 = {
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
                let __sp132 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                Some((__v129, __sp130, __v131, __sp132))
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
                        let __sp133 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v134 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = ::parse_that::scan_ident(state)
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::identifier(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __v135 = {
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
                        let __sp136 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp133, __v134, __v135, __sp136))
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
                        let __sp141 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v142 = {
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
                        let __v143 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = {
                                let __depth139 = __BbnfBootstrapEnum_alloc(state)
                                    .__s2()
                                    .len();
                                loop {
                                    let __prev140 = state.offset;
                                    match (|| {
                                        let __v137 = ::parse_that::scan_ident(state)
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
                                        let __sp138 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        Some((__v137, __sp138))
                                    })()
                                        .map(|__v| BbnfBootstrapEnum::pretty_hint(__v))
                                    {
                                        Some(__value) => {
                                            __BbnfBootstrapEnum_alloc(state).__s2().push(__value);
                                            if state.offset == __prev140 {
                                                break;
                                            }
                                        }
                                        None => {
                                            state.offset = __prev140;
                                            break;
                                        }
                                    }
                                }
                                if (__BbnfBootstrapEnum_alloc(state).__s2().len()
                                    - __depth139) >= 1usize
                                {
                                    Some(__BbnfBootstrapEnum_alloc(state).__c2(__depth139))
                                } else {
                                    __BbnfBootstrapEnum_alloc(state)
                                        .__s2()
                                        .truncate(__depth139);
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
                        let __sp144 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp141, __v142, __v143, __sp144))
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
                        let __sp145 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v146 = {
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
                        let __sp147 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp145, __v146, __sp147))
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
                        let __sp148 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v149 = {
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
                        let __sp150 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp148, __v149, __sp150))
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
                        let __sp151 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v152 = {
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
                        let __sp153 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp151, __v152, __sp153))
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
                        let __sp154 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v155 = {
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
                        let __sp156 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp154, __v155, __sp156))
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
    fn __grammar<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __depth160 = __BbnfBootstrapEnum_alloc(state).__s4().len();
                loop {
                    let __prev161 = state.offset;
                    match (|| {
                        let __v157 = {
                            let __cp = state.offset;
                            match (|| {
                                {
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
                                    })
                            })() {
                                Some(__v) => Some(Some(__v)),
                                None => {
                                    state.offset = __cp;
                                    Some(None)
                                }
                            }
                        }?;
                        let __v158 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = (|| {
                                {
                                    let __cp = state.offset;
                                    let __result = Self::__directive(state)
                                        .map(|__v| {
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        });
                                    if __result.is_some() {
                                        return __result;
                                    }
                                    state.offset = __cp;
                                }
                                {
                                    let __cp = state.offset;
                                    let __result = Self::__rule(state)
                                        .map(|__v| {
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
                        let __v159 = {
                            let __cp = state.offset;
                            match (|| {
                                {
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
                                    })
                            })() {
                                Some(__v) => Some(Some(__v)),
                                None => {
                                    state.offset = __cp;
                                    Some(None)
                                }
                            }
                        }?;
                        Some((__v157, __v158, __v159))
                    })() {
                        Some(__value) => {
                            __BbnfBootstrapEnum_alloc(state).__s4().push(__value);
                            if state.offset == __prev161 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __prev161;
                            break;
                        }
                    }
                }
                Some(__BbnfBootstrapEnum_alloc(state).__c4(__depth160))
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
    fn __type_name_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp20 = state.offset;
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
                        state.offset = __pretty_cp20;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp19 = state.offset;
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
                                state.offset = __pretty_cp19;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp18 = state.offset;
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
                                        state.offset = __pretty_cp18;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp17 = state.offset;
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
                                                state.offset = __pretty_cp17;
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp16 = state.offset;
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
                                                        state.offset = __pretty_cp16;
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp15 = state.offset;
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
                                                                state.offset = __pretty_cp15;
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp14 = state.offset;
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
                                                                        state.offset = __pretty_cp14;
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp13 = state.offset;
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
                                                                                state.offset = __pretty_cp13;
                                                                            }
                                                                            __ok
                                                                        } {
                                                                            {
                                                                                if !{
                                                                                    let __pretty_cp12 = state.offset;
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
                                                                                        state.offset = __pretty_cp12;
                                                                                    }
                                                                                    __ok
                                                                                } {
                                                                                    {
                                                                                        if !{
                                                                                            let __pretty_cp11 = state.offset;
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
                                                                                                state.offset = __pretty_cp11;
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
    fn __literal_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp25 = state.offset;
                    let __pretty_bcp26 = __builder.checkpoint();
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
                        state.offset = __pretty_cp25;
                        __builder.restore(__pretty_bcp26);
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp23 = state.offset;
                            let __pretty_bcp24 = __builder.checkpoint();
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
                                state.offset = __pretty_cp23;
                                __builder.restore(__pretty_bcp24);
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp21 = state.offset;
                                    let __pretty_bcp22 = __builder.checkpoint();
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
                                        state.offset = __pretty_cp21;
                                        __builder.restore(__pretty_bcp22);
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
    fn __modifier_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp30 = state.offset;
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
                        state.offset = __pretty_cp30;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp29 = state.offset;
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
                                state.offset = __pretty_cp29;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp28 = state.offset;
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
                                        state.offset = __pretty_cp28;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp27 = state.offset;
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
                                                state.offset = __pretty_cp27;
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
    fn __big_comment_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp33 = state.offset;
                    let __pretty_bcp34 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            let __ows31 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows31..state.offset]);
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
                            let __ows32 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows32..state.offset]);
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
    fn __binary_operators_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp37 = state.offset;
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
                        state.offset = __pretty_cp37;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp36 = state.offset;
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
                                state.offset = __pretty_cp36;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp35 = state.offset;
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
                                        state.offset = __pretty_cp35;
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
                    let __pretty_cp40 = state.offset;
                    let __pretty_bcp41 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            let __ows38 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows38..state.offset]);
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
                            let __ows39 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows39..state.offset]);
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp40;
                        __builder.restore(__pretty_bcp41);
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
                    let mut __rep_count44 = 0usize;
                    while __rep_count44 < 4294967295 {
                        let __rep_cp45 = state.offset;
                        if !{
                            let __pretty_cp42 = state.offset;
                            let __pretty_bcp43 = __builder.checkpoint();
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
                                state.offset = __pretty_cp42;
                                __builder.restore(__pretty_bcp43);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp45;
                            break;
                        }
                        if state.offset == __rep_cp45 {
                            break;
                        }
                        __rep_count44 += 1;
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
    fn __type_annotation_prettify<'a>(
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
                        if state.src_bytes.get(state.offset).copied() != Some(b':') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b':');
                    };
                    __builder.text_inline_ws(&state.src[__ows46..__ows47]);
                    let __ows48 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows48..state.offset]);
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
    fn __host_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows49 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows50 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows49..__ows50]);
                    let __ows51 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows51..state.offset]);
                };
                {
                    let __ows52 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows53 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows52..__ows53]);
                    let __ows54 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows54..state.offset]);
                };
                {
                    let _ = {
                        let __pretty_cp56 = state.offset;
                        let __pretty_bcp57 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp55 = state.offset;
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
                                        state.offset = __pretty_cp55;
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
                            state.offset = __pretty_cp56;
                            __builder.restore(__pretty_bcp57);
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
                        let __pretty_cp58 = state.offset;
                        let __pretty_bcp59 = __builder.checkpoint();
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
    fn __token_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows70 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows71 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows70..__ows71]);
                    let __ows72 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows72..state.offset]);
                };
                {
                    let __ows73 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows74 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows73..__ows74]);
                    let __ows75 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows75..state.offset]);
                };
                {
                    let _ = {
                        let __pretty_cp77 = state.offset;
                        let __pretty_bcp78 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp76 = state.offset;
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
                                        state.offset = __pretty_cp76;
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
                            state.offset = __pretty_cp77;
                            __builder.restore(__pretty_bcp78);
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
                    let __ows79 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows80 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows79..__ows80]);
                    let __ows81 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows81..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp85 = state.offset;
                        let __pretty_bcp86 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows83 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows83..state.offset]);
                                {
                                    if !{
                                        let __pretty_cp82 = state.offset;
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
                                            state.offset = __pretty_cp82;
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
                                let __ows84 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows84..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp85;
                            __builder.restore(__pretty_bcp86);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp88 = state.offset;
                        let __pretty_bcp89 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp87 = state.offset;
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
                                        state.offset = __pretty_cp87;
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
                            state.offset = __pretty_cp88;
                            __builder.restore(__pretty_bcp89);
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
    fn __ws_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows90 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows91 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows90..__ows91]);
                    let __ows92 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows92..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp95 = state.offset;
                        let __pretty_bcp96 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows93 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows93..state.offset]);
                                if !Self::__regex_prettify(state, __builder) {
                                    return false;
                                }
                                let __ows94 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows94..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp95;
                            __builder.restore(__pretty_bcp96);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp98 = state.offset;
                        let __pretty_bcp99 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp97 = state.offset;
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
                                        state.offset = __pretty_cp97;
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
                            state.offset = __pretty_cp98;
                            __builder.restore(__pretty_bcp99);
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
                    let mut __rep_count106 = 0usize;
                    while __rep_count106 < 4294967295 {
                        let __rep_cp107 = state.offset;
                        if !{
                            let __pretty_cp104 = state.offset;
                            let __pretty_bcp105 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp102 = state.offset;
                                            let __pretty_bcp103 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows100 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows100..state.offset]);
                                                    if !Self::__mul_op_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows101 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows101..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp102;
                                                __builder.restore(__pretty_bcp103);
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
                                state.offset = __pretty_cp104;
                                __builder.restore(__pretty_bcp105);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp107;
                            break;
                        }
                        if state.offset == __rep_cp107 {
                            break;
                        }
                        __rep_count106 += 1;
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
                    let mut __rep_count113 = 0usize;
                    while __rep_count113 < 4294967295 {
                        let __rep_cp114 = state.offset;
                        if !{
                            let __pretty_cp111 = state.offset;
                            let __pretty_bcp112 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows108 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows109 = state.offset;
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
                                        __builder.text_inline_ws(&state.src[__ows108..__ows109]);
                                        let __ows110 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows110..state.offset]);
                                    };
                                    if !Self::__value_and_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp111;
                                __builder.restore(__pretty_bcp112);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp114;
                            break;
                        }
                        if state.offset == __rep_cp114 {
                            break;
                        }
                        __rep_count113 += 1;
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
    fn __value_fn_call_prettify<'a>(
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
                    if state.src_bytes.get(state.offset).copied() != Some(b'(') {
                        return false;
                    }
                    state.offset += 1;
                    __builder.char(b'(');
                };
                {
                    let _ = {
                        let __pretty_cp122 = state.offset;
                        let __pretty_bcp123 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !Self::__value_expr_prettify(state, __builder) {
                                    return false;
                                }
                                {
                                    let mut __rep_count120 = 0usize;
                                    while __rep_count120 < 4294967295 {
                                        let __rep_cp121 = state.offset;
                                        if !{
                                            let __pretty_cp118 = state.offset;
                                            let __pretty_bcp119 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    {
                                                        let __ows115 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        let __ows116 = state.offset;
                                                        {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                            {
                                                                return false;
                                                            }
                                                            state.offset += 1;
                                                            __builder.char(b',');
                                                        };
                                                        __builder.text_inline_ws(&state.src[__ows115..__ows116]);
                                                        let __ows117 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows117..state.offset]);
                                                    };
                                                    if !Self::__value_expr_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp118;
                                                __builder.restore(__pretty_bcp119);
                                            }
                                            __ok
                                        } {
                                            state.offset = __rep_cp121;
                                            break;
                                        }
                                        if state.offset == __rep_cp121 {
                                            break;
                                        }
                                        __rep_count120 += 1;
                                    }
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp122;
                            __builder.restore(__pretty_bcp123);
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
                    let mut __rep_count131 = 0usize;
                    while __rep_count131 < 4294967295 {
                        let __rep_cp132 = state.offset;
                        if !{
                            let __pretty_cp129 = state.offset;
                            let __pretty_bcp130 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp127 = state.offset;
                                            let __pretty_bcp128 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows125 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows125..state.offset]);
                                                    {
                                                        if !{
                                                            let __pretty_cp124 = state.offset;
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
                                                                state.offset = __pretty_cp124;
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
                                                    let __ows126 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows126..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp127;
                                                __builder.restore(__pretty_bcp128);
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
                    let mut __rep_count139 = 0usize;
                    while __rep_count139 < 4294967295 {
                        let __rep_cp140 = state.offset;
                        if !{
                            let __pretty_cp137 = state.offset;
                            let __pretty_bcp138 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp135 = state.offset;
                                            let __pretty_bcp136 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows133 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows133..state.offset]);
                                                    if !Self::__cmp_op_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows134 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows134..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp135;
                                                __builder.restore(__pretty_bcp136);
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
                                state.offset = __pretty_cp137;
                                __builder.restore(__pretty_bcp138);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp140;
                            break;
                        }
                        if state.offset == __rep_cp140 {
                            break;
                        }
                        __rep_count139 += 1;
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
                    let mut __rep_count146 = 0usize;
                    while __rep_count146 < 4294967295 {
                        let __rep_cp147 = state.offset;
                        if !{
                            let __pretty_cp144 = state.offset;
                            let __pretty_bcp145 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows141 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows142 = state.offset;
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
                                        __builder.text_inline_ws(&state.src[__ows141..__ows142]);
                                        let __ows143 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows143..state.offset]);
                                    };
                                    if !Self::__value_cmp_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp144;
                                __builder.restore(__pretty_bcp145);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp147;
                            break;
                        }
                        if state.offset == __rep_cp147 {
                            break;
                        }
                        __rep_count146 += 1;
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
                    let mut __rep_count153 = 0usize;
                    while __rep_count153 < 4294967295 {
                        let __rep_cp154 = state.offset;
                        if !{
                            let __pretty_cp151 = state.offset;
                            let __pretty_bcp152 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows148 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows149 = state.offset;
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b',');
                                        };
                                        __builder.text_inline_ws(&state.src[__ows148..__ows149]);
                                        let __ows150 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows150..state.offset]);
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
                                state.offset = __pretty_cp151;
                                __builder.restore(__pretty_bcp152);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp154;
                            break;
                        }
                        if state.offset == __rep_cp154 {
                            break;
                        }
                        __rep_count153 += 1;
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
    fn __value_expr_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp155 = state.offset;
                    let __pretty_bcp156 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        if !Self::__value_closure_prettify(state, __builder) {
                            return false;
                        }
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp155;
                        __builder.restore(__pretty_bcp156);
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
                    let __pretty_cp174 = state.offset;
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
                        state.offset = __pretty_cp174;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp173 = state.offset;
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
                                state.offset = __pretty_cp173;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp171 = state.offset;
                                    let __pretty_bcp172 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            if !{
                                                let __pretty_cp157 = state.offset;
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
                                                    state.offset = __pretty_cp157;
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
                                        state.offset = __pretty_cp171;
                                        __builder.restore(__pretty_bcp172);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp169 = state.offset;
                                            let __pretty_bcp170 = __builder.checkpoint();
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
                                                state.offset = __pretty_cp169;
                                                __builder.restore(__pretty_bcp170);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp167 = state.offset;
                                                    let __pretty_bcp168 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        if !Self::__value_fn_call_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp167;
                                                        __builder.restore(__pretty_bcp168);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp165 = state.offset;
                                                            let __pretty_bcp166 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                if !Self::__value_input_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp165;
                                                                __builder.restore(__pretty_bcp166);
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp164 = state.offset;
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
                                                                        state.offset = __pretty_cp164;
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp162 = state.offset;
                                                                            let __pretty_bcp163 = __builder.checkpoint();
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
                                                                                            let __pretty_cp160 = state.offset;
                                                                                            let __pretty_bcp161 = __builder.checkpoint();
                                                                                            let __ok = (|| -> bool {
                                                                                                {
                                                                                                    let __ows158 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows158..state.offset]);
                                                                                                    if !Self::__value_expr_prettify(state, __builder) {
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
                                                                                state.offset = __pretty_cp162;
                                                                                __builder.restore(__pretty_bcp163);
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
                    let __pretty_cp176 = state.offset;
                    let __pretty_bcp177 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            {
                                if !{
                                    let __pretty_cp175 = state.offset;
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
                                        state.offset = __pretty_cp175;
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
                        state.offset = __pretty_cp176;
                        __builder.restore(__pretty_bcp177);
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
    fn __pretty_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows178 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows179 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows178..__ows179]);
                    let __ows180 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows180..state.offset]);
                };
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
                                {
                                    if !{
                                        let __pretty_cp181 = state.offset;
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
                                            state.offset = __pretty_cp181;
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
                {
                    if !{
                        let __pretty_cp194 = state.offset;
                        let __pretty_bcp195 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows192 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows192..state.offset]);
                                {
                                    let __rep_start190 = state.offset;
                                    let __rep_bcp191 = __builder.checkpoint();
                                    let mut __rep_count188 = 0usize;
                                    while __rep_count188 < 4294967295 {
                                        let __rep_cp189 = state.offset;
                                        if !{
                                            let __pretty_cp186 = state.offset;
                                            let __pretty_bcp187 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__pretty_hint_prettify(state, __builder) {
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
                                            state.offset = __rep_cp189;
                                            break;
                                        }
                                        if state.offset == __rep_cp189 {
                                            break;
                                        }
                                        __rep_count188 += 1;
                                    }
                                    if __rep_count188 < 1 {
                                        state.offset = __rep_start190;
                                        __builder.restore(__rep_bcp191);
                                        return false;
                                    }
                                };
                                let __ows193 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows193..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp194;
                            __builder.restore(__pretty_bcp195);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp197 = state.offset;
                        let __pretty_bcp198 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp196 = state.offset;
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
                                        state.offset = __pretty_cp196;
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
                            state.offset = __pretty_cp197;
                            __builder.restore(__pretty_bcp198);
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
                    let __ows199 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows200 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows199..__ows200]);
                    let __ows201 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows201..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp209 = state.offset;
                        let __pretty_bcp210 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    if !{
                                        let __pretty_cp204 = state.offset;
                                        let __pretty_bcp205 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows202 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows202..state.offset]);
                                                if !Self::__import_items_prettify(state, __builder) {
                                                    return false;
                                                }
                                                let __ows203 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows203..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp204;
                                            __builder.restore(__pretty_bcp205);
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                };
                                {
                                    let __ows206 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ows207 = state.offset;
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
                                    __builder.text_inline_ws(&state.src[__ows206..__ows207]);
                                    let __ows208 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows208..state.offset]);
                                };
                                if !Self::__import_path_prettify(state, __builder) {
                                    return false;
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp209;
                            __builder.restore(__pretty_bcp210);
                        }
                        __ok
                    } {
                        if !Self::__import_path_prettify(state, __builder) {
                            return false;
                        }
                    }
                };
                {
                    let _ = {
                        let __pretty_cp212 = state.offset;
                        let __pretty_bcp213 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp211 = state.offset;
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
                                        state.offset = __pretty_cp211;
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
                            state.offset = __pretty_cp212;
                            __builder.restore(__pretty_bcp213);
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
    fn __alternation_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        __builder.group_open();
        let __pretty_ok = {
            {
                {
                    let __rep_start224 = state.offset;
                    let __rep_bcp225 = __builder.checkpoint();
                    let mut __rep_count222 = 0usize;
                    while __rep_count222 < 4294967295 {
                        let __rep_cp223 = state.offset;
                        if !{
                            let __pretty_cp220 = state.offset;
                            let __pretty_bcp221 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp216 = state.offset;
                                            let __pretty_bcp217 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows214 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows214..state.offset]);
                                                    if !Self::__concatenation_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows215 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows215..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp216;
                                                __builder.restore(__pretty_bcp217);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                    {
                                        let _ = {
                                            let __pretty_cp218 = state.offset;
                                            let __pretty_bcp219 = __builder.checkpoint();
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
                                                state.offset = __pretty_cp218;
                                                __builder.restore(__pretty_bcp219);
                                            }
                                            __ok
                                        };
                                        true
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
                            state.offset = __rep_cp223;
                            break;
                        }
                        if state.offset == __rep_cp223 {
                            break;
                        }
                        __rep_count222 += 1;
                    }
                    if __rep_count222 < 1 {
                        state.offset = __rep_start224;
                        __builder.restore(__rep_bcp225);
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
                let __rep_start236 = state.offset;
                let __rep_bcp237 = __builder.checkpoint();
                let mut __rep_count234 = 0usize;
                while __rep_count234 < 4294967295 {
                    let __rep_cp235 = state.offset;
                    if !{
                        let __pretty_cp232 = state.offset;
                        let __pretty_bcp233 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    if !{
                                        let __pretty_cp228 = state.offset;
                                        let __pretty_bcp229 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows226 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows226..state.offset]);
                                                if !Self::__binary_factor_prettify(state, __builder) {
                                                    return false;
                                                }
                                                let __ows227 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows227..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp228;
                                            __builder.restore(__pretty_bcp229);
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                };
                                {
                                    let _ = {
                                        let __pretty_cp230 = state.offset;
                                        let __pretty_bcp231 = __builder.checkpoint();
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
                                            state.offset = __pretty_cp230;
                                            __builder.restore(__pretty_bcp231);
                                        }
                                        __ok
                                    };
                                    true
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp232;
                            __builder.restore(__pretty_bcp233);
                        }
                        __ok
                    } {
                        state.offset = __rep_cp235;
                        break;
                    }
                    if state.offset == __rep_cp235 {
                        break;
                    }
                    __rep_count234 += 1;
                }
                if __rep_count234 < 1 {
                    state.offset = __rep_start236;
                    __builder.restore(__rep_bcp237);
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
                let __rep_start248 = state.offset;
                let __rep_bcp249 = __builder.checkpoint();
                let mut __rep_count246 = 0usize;
                while __rep_count246 < 4294967295 {
                    let __rep_cp247 = state.offset;
                    if !{
                        let __pretty_cp244 = state.offset;
                        let __pretty_bcp245 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    if !{
                                        let __pretty_cp240 = state.offset;
                                        let __pretty_bcp241 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows238 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows238..state.offset]);
                                                if !Self::__binary_factor_prettify(state, __builder) {
                                                    return false;
                                                }
                                                let __ows239 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows239..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp240;
                                            __builder.restore(__pretty_bcp241);
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                };
                                {
                                    let _ = {
                                        let __pretty_cp242 = state.offset;
                                        let __pretty_bcp243 = __builder.checkpoint();
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
                                            state.offset = __pretty_cp242;
                                            __builder.restore(__pretty_bcp243);
                                        }
                                        __ok
                                    };
                                    true
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp244;
                            __builder.restore(__pretty_bcp245);
                        }
                        __ok
                    } {
                        state.offset = __rep_cp247;
                        break;
                    }
                    if state.offset == __rep_cp247 {
                        break;
                    }
                    __rep_count246 += 1;
                }
                if __rep_count246 < 1 {
                    state.offset = __rep_start248;
                    __builder.restore(__rep_bcp249);
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
                    let mut __rep_count255 = 0usize;
                    while __rep_count255 < 4294967295 {
                        let __rep_cp256 = state.offset;
                        if !{
                            let __pretty_cp253 = state.offset;
                            let __pretty_bcp254 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows250 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows251 = state.offset;
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b',');
                                        };
                                        __builder.text_inline_ws(&state.src[__ows250..__ows251]);
                                        let __ows252 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows252..state.offset]);
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
                                state.offset = __pretty_cp253;
                                __builder.restore(__pretty_bcp254);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp256;
                            break;
                        }
                        if state.offset == __rep_cp256 {
                            break;
                        }
                        __rep_count255 += 1;
                    }
                };
                {
                    let __ows257 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows258 = state.offset;
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'|') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'|');
                    };
                    __builder.text_inline_ws(&state.src[__ows257..__ows258]);
                    let __ows259 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows259..state.offset]);
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
                    let __pretty_cp308 = state.offset;
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
                        state.offset = __pretty_cp308;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp307 = state.offset;
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
                                state.offset = __pretty_cp307;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp305 = state.offset;
                                    let __pretty_bcp306 = __builder.checkpoint();
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
                                                    let __pretty_cp275 = state.offset;
                                                    let __pretty_bcp276 = __builder.checkpoint();
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
                                                                    let __pretty_cp262 = state.offset;
                                                                    let __pretty_bcp263 = __builder.checkpoint();
                                                                    let __ok = (|| -> bool {
                                                                        {
                                                                            let __ows260 = state.offset;
                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                            __builder
                                                                                .text_inline_ws(&state.src[__ows260..state.offset]);
                                                                            if !Self::__call_arg_prettify(state, __builder) {
                                                                                return false;
                                                                            }
                                                                            let __ows261 = state.offset;
                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                            __builder
                                                                                .text_inline_ws(&state.src[__ows261..state.offset]);
                                                                        };
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp262;
                                                                        __builder.restore(__pretty_bcp263);
                                                                    }
                                                                    __ok
                                                                } {
                                                                    return false;
                                                                }
                                                            };
                                                            {
                                                                let mut __rep_count273 = 0usize;
                                                                while __rep_count273 < 4294967295 {
                                                                    let __rep_cp274 = state.offset;
                                                                    if !{
                                                                        let __pretty_cp271 = state.offset;
                                                                        let __pretty_bcp272 = __builder.checkpoint();
                                                                        let __ok = (|| -> bool {
                                                                            {
                                                                                {
                                                                                    let __ows264 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    let __ows265 = state.offset;
                                                                                    {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                                        {
                                                                                            return false;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        __builder.char(b',');
                                                                                    };
                                                                                    __builder.text_inline_ws(&state.src[__ows264..__ows265]);
                                                                                    let __ows266 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    __builder
                                                                                        .text_inline_ws(&state.src[__ows266..state.offset]);
                                                                                };
                                                                                {
                                                                                    if !{
                                                                                        let __pretty_cp269 = state.offset;
                                                                                        let __pretty_bcp270 = __builder.checkpoint();
                                                                                        let __ok = (|| -> bool {
                                                                                            {
                                                                                                let __ows267 = state.offset;
                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                __builder
                                                                                                    .text_inline_ws(&state.src[__ows267..state.offset]);
                                                                                                if !Self::__call_arg_prettify(state, __builder) {
                                                                                                    return false;
                                                                                                }
                                                                                                let __ows268 = state.offset;
                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                __builder
                                                                                                    .text_inline_ws(&state.src[__ows268..state.offset]);
                                                                                            };
                                                                                            true
                                                                                        })();
                                                                                        if !__ok {
                                                                                            state.offset = __pretty_cp269;
                                                                                            __builder.restore(__pretty_bcp270);
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
                                                                            state.offset = __pretty_cp271;
                                                                            __builder.restore(__pretty_bcp272);
                                                                        }
                                                                        __ok
                                                                    } {
                                                                        state.offset = __rep_cp274;
                                                                        break;
                                                                    }
                                                                    if state.offset == __rep_cp274 {
                                                                        break;
                                                                    }
                                                                    __rep_count273 += 1;
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
                                                        state.offset = __pretty_cp275;
                                                        __builder.restore(__pretty_bcp276);
                                                    }
                                                    __ok
                                                };
                                                true
                                            };
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp305;
                                        __builder.restore(__pretty_bcp306);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp303 = state.offset;
                                            let __pretty_bcp304 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__literal_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp303;
                                                __builder.restore(__pretty_bcp304);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp301 = state.offset;
                                                    let __pretty_bcp302 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        if !Self::__regex_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp301;
                                                        __builder.restore(__pretty_bcp302);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp299 = state.offset;
                                                            let __pretty_bcp300 = __builder.checkpoint();
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
                                                                            let __pretty_cp279 = state.offset;
                                                                            let __pretty_bcp280 = __builder.checkpoint();
                                                                            let __ok = (|| -> bool {
                                                                                {
                                                                                    let __ows277 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    __builder
                                                                                        .text_inline_ws(&state.src[__ows277..state.offset]);
                                                                                    if !Self::__rhs_prettify(state, __builder) {
                                                                                        return false;
                                                                                    }
                                                                                    let __ows278 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    __builder
                                                                                        .text_inline_ws(&state.src[__ows278..state.offset]);
                                                                                };
                                                                                true
                                                                            })();
                                                                            if !__ok {
                                                                                state.offset = __pretty_cp279;
                                                                                __builder.restore(__pretty_bcp280);
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
                                                                state.offset = __pretty_cp299;
                                                                __builder.restore(__pretty_bcp300);
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp297 = state.offset;
                                                                    let __pretty_bcp298 = __builder.checkpoint();
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
                                                                                    let __pretty_cp283 = state.offset;
                                                                                    let __pretty_bcp284 = __builder.checkpoint();
                                                                                    let __ok = (|| -> bool {
                                                                                        {
                                                                                            let __ows281 = state.offset;
                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                            __builder
                                                                                                .text_inline_ws(&state.src[__ows281..state.offset]);
                                                                                            if !Self::__rhs_prettify(state, __builder) {
                                                                                                return false;
                                                                                            }
                                                                                            let __ows282 = state.offset;
                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                            __builder
                                                                                                .text_inline_ws(&state.src[__ows282..state.offset]);
                                                                                        };
                                                                                        true
                                                                                    })();
                                                                                    if !__ok {
                                                                                        state.offset = __pretty_cp283;
                                                                                        __builder.restore(__pretty_bcp284);
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
                                                                        state.offset = __pretty_cp297;
                                                                        __builder.restore(__pretty_bcp298);
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp295 = state.offset;
                                                                            let __pretty_bcp296 = __builder.checkpoint();
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
                                                                                            let __pretty_cp287 = state.offset;
                                                                                            let __pretty_bcp288 = __builder.checkpoint();
                                                                                            let __ok = (|| -> bool {
                                                                                                {
                                                                                                    let __ows285 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows285..state.offset]);
                                                                                                    if !Self::__rhs_prettify(state, __builder) {
                                                                                                        return false;
                                                                                                    }
                                                                                                    let __ows286 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows286..state.offset]);
                                                                                                };
                                                                                                true
                                                                                            })();
                                                                                            if !__ok {
                                                                                                state.offset = __pretty_cp287;
                                                                                                __builder.restore(__pretty_bcp288);
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
                                                                                state.offset = __pretty_cp295;
                                                                                __builder.restore(__pretty_bcp296);
                                                                            }
                                                                            __ok
                                                                        } {
                                                                            {
                                                                                if !{
                                                                                    let __pretty_cp293 = state.offset;
                                                                                    let __pretty_bcp294 = __builder.checkpoint();
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
                                                                                                    let __pretty_cp291 = state.offset;
                                                                                                    let __pretty_bcp292 = __builder.checkpoint();
                                                                                                    let __ok = (|| -> bool {
                                                                                                        {
                                                                                                            let __ows289 = state.offset;
                                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                            __builder
                                                                                                                .text_inline_ws(&state.src[__ows289..state.offset]);
                                                                                                            if !Self::__rhs_prettify(state, __builder) {
                                                                                                                return false;
                                                                                                            }
                                                                                                            let __ows290 = state.offset;
                                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                            __builder
                                                                                                                .text_inline_ws(&state.src[__ows290..state.offset]);
                                                                                                        };
                                                                                                        true
                                                                                                    })();
                                                                                                    if !__ok {
                                                                                                        state.offset = __pretty_cp291;
                                                                                                        __builder.restore(__pretty_bcp292);
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
                                                                                        state.offset = __pretty_cp293;
                                                                                        __builder.restore(__pretty_bcp294);
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
                    let mut __rep_count315 = 0usize;
                    while __rep_count315 < 4294967295 {
                        let __rep_cp316 = state.offset;
                        if !{
                            let __pretty_cp313 = state.offset;
                            let __pretty_bcp314 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp311 = state.offset;
                                            let __pretty_bcp312 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows309 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows309..state.offset]);
                                                    if !Self::__binary_operators_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows310 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows310..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp311;
                                                __builder.restore(__pretty_bcp312);
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
                                state.offset = __pretty_cp313;
                                __builder.restore(__pretty_bcp314);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp316;
                            break;
                        }
                        if state.offset == __rep_cp316 {
                            break;
                        }
                        __rep_count315 += 1;
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
                    let __pretty_cp317 = state.offset;
                    let __pretty_bcp318 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        if !Self::__closure_prettify(state, __builder) {
                            return false;
                        }
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp317;
                        __builder.restore(__pretty_bcp318);
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
                        let __pretty_cp319 = state.offset;
                        let __pretty_bcp320 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__big_comment_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp319;
                            __builder.restore(__pretty_bcp320);
                        }
                        __ok
                    };
                    true
                };
                {
                    if !{
                        let __pretty_cp323 = state.offset;
                        let __pretty_bcp324 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows321 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows321..state.offset]);
                                if !Self::__term_prettify(state, __builder) {
                                    return false;
                                }
                                let __ows322 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows322..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp323;
                            __builder.restore(__pretty_bcp324);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp325 = state.offset;
                        let __pretty_bcp326 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__modifier_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp325;
                            __builder.restore(__pretty_bcp326);
                        }
                        __ok
                    };
                    true
                };
                {
                    let _ = {
                        let __pretty_cp327 = state.offset;
                        let __pretty_bcp328 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__big_comment_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp327;
                            __builder.restore(__pretty_bcp328);
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
                        let __pretty_cp334 = state.offset;
                        let __pretty_bcp335 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    let __ows329 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ows330 = state.offset;
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
                                    __builder.text_inline_ws(&state.src[__ows329..__ows330]);
                                    let __ows331 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows331..state.offset]);
                                };
                                {
                                    if !Self::__value_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    {
                                        let _ = {
                                            let __pretty_cp332 = state.offset;
                                            let __pretty_bcp333 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__type_annotation_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp332;
                                                __builder.restore(__pretty_bcp333);
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
                            state.offset = __pretty_cp334;
                            __builder.restore(__pretty_bcp335);
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
                    let __ows336 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows337 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows336..__ows337]);
                    let __ows338 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows338..state.offset]);
                };
                {
                    let __ows339 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows340 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows339..__ows340]);
                    let __ows341 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows341..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp344 = state.offset;
                        let __pretty_bcp345 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows342 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows342..state.offset]);
                                if !Self::__rhs_prettify(state, __builder) {
                                    return false;
                                }
                                let __ows343 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows343..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp344;
                            __builder.restore(__pretty_bcp345);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp347 = state.offset;
                        let __pretty_bcp348 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp346 = state.offset;
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
                                        state.offset = __pretty_cp346;
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
                            state.offset = __pretty_cp347;
                            __builder.restore(__pretty_bcp348);
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
                        let __ows349 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows350 = state.offset;
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'=') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'=');
                        };
                        __builder.text_inline_ws(&state.src[__ows349..__ows350]);
                        let __ows351 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows351..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp354 = state.offset;
                            let __pretty_bcp355 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows352 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows352..state.offset]);
                                    if !Self::__rhs_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows353 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows353..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp354;
                                __builder.restore(__pretty_bcp355);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        if !{
                            let __pretty_cp356 = state.offset;
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
                                state.offset = __pretty_cp356;
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
                    let __pretty_cp369 = state.offset;
                    let __pretty_bcp370 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        if !Self::__import_directive_prettify(state, __builder) {
                            return false;
                        }
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp369;
                        __builder.restore(__pretty_bcp370);
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp367 = state.offset;
                            let __pretty_bcp368 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                if !Self::__recover_directive_prettify(state, __builder) {
                                    return false;
                                }
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp367;
                                __builder.restore(__pretty_bcp368);
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp365 = state.offset;
                                    let __pretty_bcp366 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        if !Self::__pretty_directive_prettify(state, __builder) {
                                            return false;
                                        }
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp365;
                                        __builder.restore(__pretty_bcp366);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp363 = state.offset;
                                            let __pretty_bcp364 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__ws_directive_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp363;
                                                __builder.restore(__pretty_bcp364);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp361 = state.offset;
                                                    let __pretty_bcp362 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        if !Self::__token_directive_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp361;
                                                        __builder.restore(__pretty_bcp362);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp359 = state.offset;
                                                            let __pretty_bcp360 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                if !Self::__debug_directive_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp359;
                                                                __builder.restore(__pretty_bcp360);
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp357 = state.offset;
                                                                    let __pretty_bcp358 = __builder.checkpoint();
                                                                    let __ok = (|| -> bool {
                                                                        if !Self::__host_directive_prettify(state, __builder) {
                                                                            return false;
                                                                        }
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp357;
                                                                        __builder.restore(__pretty_bcp358);
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
    fn __grammar_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let mut __rep_count382 = 0usize;
                while __rep_count382 < 4294967295 {
                    let __rep_cp383 = state.offset;
                    let __iter_cp = if __rep_count382 > 0 {
                        Some(__builder.checkpoint())
                    } else {
                        None
                    };
                    if __rep_count382 > 0 {
                        __builder.hardline();
                    }
                    if !{
                        let __pretty_cp381 = state.offset;
                        let __ok = (|| -> bool {
                            {
                                {
                                    let _ = {
                                        let __pretty_cp371 = state.offset;
                                        let __pretty_bcp372 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            if !Self::__comment_prettify(state, __builder) {
                                                return false;
                                            }
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp371;
                                            __builder.restore(__pretty_bcp372);
                                        }
                                        __ok
                                    };
                                    true
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
                                                {
                                                    if !{
                                                        let __pretty_cp373 = state.offset;
                                                        let __pretty_bcp374 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            if !Self::__directive_prettify(state, __builder) {
                                                                return false;
                                                            }
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp373;
                                                            __builder.restore(__pretty_bcp374);
                                                        }
                                                        __ok
                                                    } {
                                                        if !Self::__rule_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                    }
                                                };
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
                                    let _ = {
                                        let __pretty_cp379 = state.offset;
                                        let __pretty_bcp380 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            if !Self::__comment_prettify(state, __builder) {
                                                return false;
                                            }
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp379;
                                            __builder.restore(__pretty_bcp380);
                                        }
                                        __ok
                                    };
                                    true
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp381;
                        }
                        __ok
                    } {
                        state.offset = __rep_cp383;
                        if let Some(__bcp) = __iter_cp {
                            __builder.restore(__bcp);
                        }
                        break;
                    }
                    if state.offset == __rep_cp383 {
                        break;
                    }
                    __rep_count382 += 1;
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

