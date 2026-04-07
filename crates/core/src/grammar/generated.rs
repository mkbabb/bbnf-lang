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
    cmp_op(::parse_that::Span<'a>),
    add_op(::parse_that::Span<'a>),
    mul_op(::parse_that::Span<'a>),
    value_ident(::parse_that::Span<'a>),
    int_lit(::parse_that::Span<'a>),
    string_lit(::parse_that::Span<'a>),
    float_lit(::parse_that::Span<'a>),
    bool_lit(::parse_that::Span<'a>),
    binary_operators(::parse_that::Span<'a>),
    modifier(::parse_that::Span<'a>),
    big_comment(::parse_that::Span<'a>),
    literal(::parse_that::Span<'a>),
    identifier(::parse_that::Span<'a>),
    regex(::parse_that::Span<'a>),
    type_name(::parse_that::Span<'a>),
    comment(::parse_that::Span<'a>),
    import_path(::parse_that::Span<'a>),
    value_path(
        (
            &'a BbnfBootstrapEnum<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    value_input(
        (
            ::parse_that::Span<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    import_items(
        (
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
            ::parse_that::Span<'a>,
        ),
    ),
    debug_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    host_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    lhs(&'a BbnfBootstrapEnum<'a>),
    pretty_hint((&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)),
    token_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    ws_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    type_annotation((::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)),
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
            BbnfBootstrapEnum::cmp_op(__self_0) => {
                BbnfBootstrapEnum::cmp_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::add_op(__self_0) => {
                BbnfBootstrapEnum::add_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::mul_op(__self_0) => {
                BbnfBootstrapEnum::mul_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_ident(__self_0) => {
                BbnfBootstrapEnum::value_ident(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::int_lit(__self_0) => {
                BbnfBootstrapEnum::int_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::string_lit(__self_0) => {
                BbnfBootstrapEnum::string_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::float_lit(__self_0) => {
                BbnfBootstrapEnum::float_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::bool_lit(__self_0) => {
                BbnfBootstrapEnum::bool_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::binary_operators(__self_0) => {
                BbnfBootstrapEnum::binary_operators(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::modifier(__self_0) => {
                BbnfBootstrapEnum::modifier(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::big_comment(__self_0) => {
                BbnfBootstrapEnum::big_comment(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::literal(__self_0) => {
                BbnfBootstrapEnum::literal(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::identifier(__self_0) => {
                BbnfBootstrapEnum::identifier(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::regex(__self_0) => {
                BbnfBootstrapEnum::regex(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::type_name(__self_0) => {
                BbnfBootstrapEnum::type_name(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::comment(__self_0) => {
                BbnfBootstrapEnum::comment(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::import_path(__self_0) => {
                BbnfBootstrapEnum::import_path(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_path(__self_0) => {
                BbnfBootstrapEnum::value_path(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_input(__self_0) => {
                BbnfBootstrapEnum::value_input(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::import_items(__self_0) => {
                BbnfBootstrapEnum::import_items(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::debug_directive(__self_0) => {
                BbnfBootstrapEnum::debug_directive(::core::clone::Clone::clone(__self_0))
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
            BbnfBootstrapEnum::token_directive(__self_0) => {
                BbnfBootstrapEnum::token_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::ws_directive(__self_0) => {
                BbnfBootstrapEnum::ws_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::type_annotation(__self_0) => {
                BbnfBootstrapEnum::type_annotation(::core::clone::Clone::clone(__self_0))
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
    fn __value_path<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __chain_head1 = ::parse_that::scan_ident(state)
                    .map(|__inner| {
                        let __v = BbnfBootstrapEnum::value_ident(__inner);
                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                    })?;
                let __chain_depth0 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                loop {
                    let __chain_prev2 = state.offset;
                    match (|| {
                        let __chain_op3 = (|| {
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
                        let __chain_rhs4 = ::parse_that::scan_ident(state)
                            .map(|__inner| {
                                let __v = BbnfBootstrapEnum::value_ident(__inner);
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op3, __chain_rhs4))
                    })() {
                        Some(__value) => {
                            let (__chain_op3, __chain_rhs4) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s0()
                                .push((__chain_op3, __chain_rhs4));
                            if state.offset == __chain_prev2 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __chain_prev2;
                            break;
                        }
                    }
                }
                Some((
                    __chain_head1,
                    __BbnfBootstrapEnum_alloc(state).__c0(__chain_depth0),
                ))
            }
        })()
            .map(|__x| BbnfBootstrapEnum::value_path(__x))
    }
    pub fn value_path<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_path)
    }
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
                let __sp9 = ::parse_that::Span::new(__sp_start, state.offset, state.src);
                let __v10 = {
                    let __depth7 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                    loop {
                        let __prev8 = state.offset;
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
                            let __sp5 = ::parse_that::Span::new(
                                __sp_start,
                                state.offset,
                                state.src,
                            );
                            let __v6 = ::parse_that::scan_ident(state)
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::value_ident(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            Some((__sp5, __v6))
                        })() {
                            Some(__value) => {
                                __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                if state.offset == __prev8 {
                                    break;
                                }
                            }
                            None => {
                                state.offset = __prev8;
                                break;
                            }
                        }
                    }
                    Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth7))
                }?;
                Some((__sp9, __v10))
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
                let __v71 = (|| {
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
                                    let __sp63 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v64 = ::parse_that::scan_ident(state)
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::identifier(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        })?;
                                    let __v65 = {
                                        let __depth61 = __BbnfBootstrapEnum_alloc(state)
                                            .__s0()
                                            .len();
                                        loop {
                                            let __prev62 = state.offset;
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
                                                let __sp59 = ::parse_that::Span::new(
                                                    __sp_start,
                                                    state.offset,
                                                    state.src,
                                                );
                                                let __v60 = ::parse_that::scan_ident(state)
                                                    .map(|__inner| {
                                                        let __v = BbnfBootstrapEnum::identifier(__inner);
                                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                    })?;
                                                Some((__sp59, __v60))
                                            })() {
                                                Some(__value) => {
                                                    __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                                    if state.offset == __prev62 {
                                                        break;
                                                    }
                                                }
                                                None => {
                                                    state.offset = __prev62;
                                                    break;
                                                }
                                            }
                                        }
                                        Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth61))
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
                                    Some((__sp63, __v64, __v65, __sp66))
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
                                            None
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
                        let __sp159 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v160 = {
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
                        let __sp161 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp159, __v160, __sp161))
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
                let __depth165 = __BbnfBootstrapEnum_alloc(state).__s4().len();
                loop {
                    let __prev166 = state.offset;
                    match (|| {
                        let __v162 = {
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
                        let __v163 = {
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
                        let __v164 = {
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
                        Some((__v162, __v163, __v164))
                    })() {
                        Some(__value) => {
                            __BbnfBootstrapEnum_alloc(state).__s4().push(__value);
                            if state.offset == __prev166 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __prev166;
                            break;
                        }
                    }
                }
                Some(__BbnfBootstrapEnum_alloc(state).__c4(__depth165))
            }
        })()
            .map(|__x| BbnfBootstrapEnum::grammar(__x))
    }
    pub fn grammar<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__grammar)
    }
    #[allow(non_snake_case)]
    fn __cmp_op_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp5 = state.offset;
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
                        state.offset = __pretty_cp5;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp4 = state.offset;
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
                                state.offset = __pretty_cp4;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp3 = state.offset;
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
                                        state.offset = __pretty_cp3;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp2 = state.offset;
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
                                                state.offset = __pretty_cp2;
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp1 = state.offset;
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
                                                        state.offset = __pretty_cp1;
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp0 = state.offset;
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
    fn __add_op_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp6 = state.offset;
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
                        state.offset = __pretty_cp6;
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
    fn __mul_op_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp9 = state.offset;
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
                        state.offset = __pretty_cp9;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp8 = state.offset;
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
                                state.offset = __pretty_cp8;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp7 = state.offset;
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
                                        state.offset = __pretty_cp7;
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
                    let __pretty_cp10 = state.offset;
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
                        state.offset = __pretty_cp10;
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
    fn __binary_operators_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp13 = state.offset;
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
                        state.offset = __pretty_cp13;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp12 = state.offset;
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
                                state.offset = __pretty_cp12;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp11 = state.offset;
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
    fn __modifier_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp17 = state.offset;
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
                        state.offset = __pretty_cp17;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp16 = state.offset;
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
                                state.offset = __pretty_cp16;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp15 = state.offset;
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
                                        state.offset = __pretty_cp15;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp14 = state.offset;
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
                                                state.offset = __pretty_cp14;
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
                    let __pretty_cp20 = state.offset;
                    let __pretty_bcp21 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            let __ows18 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows18..state.offset]);
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
                            let __ows19 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows19..state.offset]);
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp20;
                        __builder.restore(__pretty_bcp21);
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
    fn __literal_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp26 = state.offset;
                    let __pretty_bcp27 = __builder.checkpoint();
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
                        state.offset = __pretty_cp26;
                        __builder.restore(__pretty_bcp27);
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp24 = state.offset;
                            let __pretty_bcp25 = __builder.checkpoint();
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
                                state.offset = __pretty_cp24;
                                __builder.restore(__pretty_bcp25);
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp22 = state.offset;
                                    let __pretty_bcp23 = __builder.checkpoint();
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
                                        state.offset = __pretty_cp22;
                                        __builder.restore(__pretty_bcp23);
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
    fn __type_name_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp37 = state.offset;
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
                        state.offset = __pretty_cp37;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp36 = state.offset;
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
                                state.offset = __pretty_cp36;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp35 = state.offset;
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
                                        state.offset = __pretty_cp35;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp34 = state.offset;
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
                                                state.offset = __pretty_cp34;
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp33 = state.offset;
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
                                                        state.offset = __pretty_cp33;
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp32 = state.offset;
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
                                                                state.offset = __pretty_cp32;
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp31 = state.offset;
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
                                                                        state.offset = __pretty_cp31;
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp30 = state.offset;
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
                                                                                state.offset = __pretty_cp30;
                                                                            }
                                                                            __ok
                                                                        } {
                                                                            {
                                                                                if !{
                                                                                    let __pretty_cp29 = state.offset;
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
                                                                                        state.offset = __pretty_cp29;
                                                                                    }
                                                                                    __ok
                                                                                } {
                                                                                    {
                                                                                        if !{
                                                                                            let __pretty_cp28 = state.offset;
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
                                                                                                state.offset = __pretty_cp28;
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
                    let mut __rep_count44 = 0usize;
                    while __rep_count44 < 4294967295 {
                        let __rep_cp45 = state.offset;
                        if !{
                            let __pretty_cp42 = state.offset;
                            let __pretty_bcp43 = __builder.checkpoint();
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
                    let mut __rep_count48 = 0usize;
                    while __rep_count48 < 4294967295 {
                        let __rep_cp49 = state.offset;
                        if !{
                            let __pretty_cp46 = state.offset;
                            let __pretty_bcp47 = __builder.checkpoint();
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
                                state.offset = __pretty_cp46;
                                __builder.restore(__pretty_bcp47);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp49;
                            break;
                        }
                        if state.offset == __rep_cp49 {
                            break;
                        }
                        __rep_count48 += 1;
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
    fn __import_items_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows50 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows51 = state.offset;
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'{') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'{');
                    };
                    __builder.text_inline_ws(&state.src[__ows50..__ows51]);
                    let __ows52 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows52..state.offset]);
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
                    let mut __rep_count58 = 0usize;
                    while __rep_count58 < 4294967295 {
                        let __rep_cp59 = state.offset;
                        if !{
                            let __pretty_cp56 = state.offset;
                            let __pretty_bcp57 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows53 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows54 = state.offset;
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b',');
                                        };
                                        __builder.text_inline_ws(&state.src[__ows53..__ows54]);
                                        let __ows55 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder.text_inline_ws(&state.src[__ows55..state.offset]);
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
                                state.offset = __pretty_cp56;
                                __builder.restore(__pretty_bcp57);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp59;
                            break;
                        }
                        if state.offset == __rep_cp59 {
                            break;
                        }
                        __rep_count58 += 1;
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
                    let __ows60 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows61 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows60..__ows61]);
                    let __ows62 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows62..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp66 = state.offset;
                        let __pretty_bcp67 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows64 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows64..state.offset]);
                                {
                                    if !{
                                        let __pretty_cp63 = state.offset;
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
                                            state.offset = __pretty_cp63;
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
                                let __ows65 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows65..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp66;
                            __builder.restore(__pretty_bcp67);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp69 = state.offset;
                        let __pretty_bcp70 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp68 = state.offset;
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
                                        state.offset = __pretty_cp68;
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
                            state.offset = __pretty_cp69;
                            __builder.restore(__pretty_bcp70);
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
    fn __host_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows71 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows72 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows71..__ows72]);
                    let __ows73 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows73..state.offset]);
                };
                {
                    let __ows74 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows75 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows74..__ows75]);
                    let __ows76 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows76..state.offset]);
                };
                {
                    let _ = {
                        let __pretty_cp78 = state.offset;
                        let __pretty_bcp79 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp77 = state.offset;
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
                                        state.offset = __pretty_cp77;
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
                            state.offset = __pretty_cp78;
                            __builder.restore(__pretty_bcp79);
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
                        let __pretty_cp80 = state.offset;
                        let __pretty_bcp81 = __builder.checkpoint();
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
                            state.offset = __pretty_cp80;
                            __builder.restore(__pretty_bcp81);
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
    fn __token_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows82 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows83 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows82..__ows83]);
                    let __ows84 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows84..state.offset]);
                };
                {
                    let __ows85 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows86 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows85..__ows86]);
                    let __ows87 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows87..state.offset]);
                };
                {
                    let _ = {
                        let __pretty_cp89 = state.offset;
                        let __pretty_bcp90 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp88 = state.offset;
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
                                        state.offset = __pretty_cp88;
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
                            state.offset = __pretty_cp89;
                            __builder.restore(__pretty_bcp90);
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
    fn __ws_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows91 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows92 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows91..__ows92]);
                    let __ows93 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows93..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp96 = state.offset;
                        let __pretty_bcp97 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows94 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows94..state.offset]);
                                if !Self::__regex_prettify(state, __builder) {
                                    return false;
                                }
                                let __ows95 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows95..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp96;
                            __builder.restore(__pretty_bcp97);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp99 = state.offset;
                        let __pretty_bcp100 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp98 = state.offset;
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
                                        state.offset = __pretty_cp98;
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
                            state.offset = __pretty_cp99;
                            __builder.restore(__pretty_bcp100);
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
    fn __type_annotation_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows101 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows102 = state.offset;
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b':') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b':');
                    };
                    __builder.text_inline_ws(&state.src[__ows101..__ows102]);
                    let __ows103 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows103..state.offset]);
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
                    let mut __rep_count110 = 0usize;
                    while __rep_count110 < 4294967295 {
                        let __rep_cp111 = state.offset;
                        if !{
                            let __pretty_cp108 = state.offset;
                            let __pretty_bcp109 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp106 = state.offset;
                                            let __pretty_bcp107 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows104 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows104..state.offset]);
                                                    if !Self::__mul_op_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows105 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows105..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp106;
                                                __builder.restore(__pretty_bcp107);
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
                                state.offset = __pretty_cp108;
                                __builder.restore(__pretty_bcp109);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp111;
                            break;
                        }
                        if state.offset == __rep_cp111 {
                            break;
                        }
                        __rep_count110 += 1;
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
                    let mut __rep_count117 = 0usize;
                    while __rep_count117 < 4294967295 {
                        let __rep_cp118 = state.offset;
                        if !{
                            let __pretty_cp115 = state.offset;
                            let __pretty_bcp116 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows112 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows113 = state.offset;
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
                                        __builder.text_inline_ws(&state.src[__ows112..__ows113]);
                                        let __ows114 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows114..state.offset]);
                                    };
                                    if !Self::__value_and_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp115;
                                __builder.restore(__pretty_bcp116);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp118;
                            break;
                        }
                        if state.offset == __rep_cp118 {
                            break;
                        }
                        __rep_count117 += 1;
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
                    let mut __rep_count126 = 0usize;
                    while __rep_count126 < 4294967295 {
                        let __rep_cp127 = state.offset;
                        if !{
                            let __pretty_cp124 = state.offset;
                            let __pretty_bcp125 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp122 = state.offset;
                                            let __pretty_bcp123 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows120 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows120..state.offset]);
                                                    {
                                                        if !{
                                                            let __pretty_cp119 = state.offset;
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
                                                                state.offset = __pretty_cp119;
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
                                                    let __ows121 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows121..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp122;
                                                __builder.restore(__pretty_bcp123);
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
                                state.offset = __pretty_cp124;
                                __builder.restore(__pretty_bcp125);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp127;
                            break;
                        }
                        if state.offset == __rep_cp127 {
                            break;
                        }
                        __rep_count126 += 1;
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
                    let mut __rep_count134 = 0usize;
                    while __rep_count134 < 4294967295 {
                        let __rep_cp135 = state.offset;
                        if !{
                            let __pretty_cp132 = state.offset;
                            let __pretty_bcp133 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp130 = state.offset;
                                            let __pretty_bcp131 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows128 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows128..state.offset]);
                                                    if !Self::__cmp_op_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows129 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows129..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp130;
                                                __builder.restore(__pretty_bcp131);
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
                                state.offset = __pretty_cp132;
                                __builder.restore(__pretty_bcp133);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp135;
                            break;
                        }
                        if state.offset == __rep_cp135 {
                            break;
                        }
                        __rep_count134 += 1;
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
                    let mut __rep_count141 = 0usize;
                    while __rep_count141 < 4294967295 {
                        let __rep_cp142 = state.offset;
                        if !{
                            let __pretty_cp139 = state.offset;
                            let __pretty_bcp140 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows136 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows137 = state.offset;
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
                                        __builder.text_inline_ws(&state.src[__ows136..__ows137]);
                                        let __ows138 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows138..state.offset]);
                                    };
                                    if !Self::__value_cmp_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp139;
                                __builder.restore(__pretty_bcp140);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp142;
                            break;
                        }
                        if state.offset == __rep_cp142 {
                            break;
                        }
                        __rep_count141 += 1;
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
                    let mut __rep_count148 = 0usize;
                    while __rep_count148 < 4294967295 {
                        let __rep_cp149 = state.offset;
                        if !{
                            let __pretty_cp146 = state.offset;
                            let __pretty_bcp147 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows143 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows144 = state.offset;
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b',');
                                        };
                                        __builder.text_inline_ws(&state.src[__ows143..__ows144]);
                                        let __ows145 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows145..state.offset]);
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
                        let __pretty_cp157 = state.offset;
                        let __pretty_bcp158 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !Self::__value_expr_prettify(state, __builder) {
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
                                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                            {
                                                                return false;
                                                            }
                                                            state.offset += 1;
                                                            __builder.char(b',');
                                                        };
                                                        __builder.text_inline_ws(&state.src[__ows150..__ows151]);
                                                        let __ows152 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows152..state.offset]);
                                                    };
                                                    if !Self::__value_expr_prettify(state, __builder) {
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
                        })();
                        if !__ok {
                            state.offset = __pretty_cp157;
                            __builder.restore(__pretty_bcp158);
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
                    let __pretty_cp159 = state.offset;
                    let __pretty_bcp160 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        if !Self::__value_closure_prettify(state, __builder) {
                            return false;
                        }
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp159;
                        __builder.restore(__pretty_bcp160);
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
                    let __pretty_cp179 = state.offset;
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
                        state.offset = __pretty_cp179;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp178 = state.offset;
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
                                state.offset = __pretty_cp178;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp176 = state.offset;
                                    let __pretty_bcp177 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            if !{
                                                let __pretty_cp161 = state.offset;
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
                                                    state.offset = __pretty_cp161;
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
                                        state.offset = __pretty_cp176;
                                        __builder.restore(__pretty_bcp177);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp174 = state.offset;
                                            let __pretty_bcp175 = __builder.checkpoint();
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
                                                state.offset = __pretty_cp174;
                                                __builder.restore(__pretty_bcp175);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp172 = state.offset;
                                                    let __pretty_bcp173 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        if !Self::__value_fn_call_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp172;
                                                        __builder.restore(__pretty_bcp173);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp170 = state.offset;
                                                            let __pretty_bcp171 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                if !Self::__value_input_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp170;
                                                                __builder.restore(__pretty_bcp171);
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp168 = state.offset;
                                                                    let __pretty_bcp169 = __builder.checkpoint();
                                                                    let __ok = (|| -> bool {
                                                                        if !Self::__value_path_prettify(state, __builder) {
                                                                            return false;
                                                                        }
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp168;
                                                                        __builder.restore(__pretty_bcp169);
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp166 = state.offset;
                                                                            let __pretty_bcp167 = __builder.checkpoint();
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
                                                                                            let __pretty_cp164 = state.offset;
                                                                                            let __pretty_bcp165 = __builder.checkpoint();
                                                                                            let __ok = (|| -> bool {
                                                                                                {
                                                                                                    let __ows162 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows162..state.offset]);
                                                                                                    if !Self::__value_expr_prettify(state, __builder) {
                                                                                                        return false;
                                                                                                    }
                                                                                                    let __ows163 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows163..state.offset]);
                                                                                                };
                                                                                                true
                                                                                            })();
                                                                                            if !__ok {
                                                                                                state.offset = __pretty_cp164;
                                                                                                __builder.restore(__pretty_bcp165);
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
                                                                                state.offset = __pretty_cp166;
                                                                                __builder.restore(__pretty_bcp167);
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
                    let __pretty_cp181 = state.offset;
                    let __pretty_bcp182 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            {
                                if !{
                                    let __pretty_cp180 = state.offset;
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
                                        state.offset = __pretty_cp180;
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
                        state.offset = __pretty_cp181;
                        __builder.restore(__pretty_bcp182);
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
                    let __ows183 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows184 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows183..__ows184]);
                    let __ows185 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows185..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp193 = state.offset;
                        let __pretty_bcp194 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    if !{
                                        let __pretty_cp188 = state.offset;
                                        let __pretty_bcp189 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows186 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows186..state.offset]);
                                                if !Self::__import_items_prettify(state, __builder) {
                                                    return false;
                                                }
                                                let __ows187 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows187..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp188;
                                            __builder.restore(__pretty_bcp189);
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                };
                                {
                                    let __ows190 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ows191 = state.offset;
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
                                    __builder.text_inline_ws(&state.src[__ows190..__ows191]);
                                    let __ows192 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows192..state.offset]);
                                };
                                if !Self::__import_path_prettify(state, __builder) {
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
                        if !Self::__import_path_prettify(state, __builder) {
                            return false;
                        }
                    }
                };
                {
                    let _ = {
                        let __pretty_cp196 = state.offset;
                        let __pretty_bcp197 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp195 = state.offset;
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
                                        state.offset = __pretty_cp195;
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
                            state.offset = __pretty_cp196;
                            __builder.restore(__pretty_bcp197);
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
                    let __ows198 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows199 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows198..__ows199]);
                    let __ows200 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows200..state.offset]);
                };
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
                                {
                                    if !{
                                        let __pretty_cp201 = state.offset;
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
                                            state.offset = __pretty_cp201;
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
                    if !{
                        let __pretty_cp214 = state.offset;
                        let __pretty_bcp215 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows212 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows212..state.offset]);
                                {
                                    let __rep_start210 = state.offset;
                                    let __rep_bcp211 = __builder.checkpoint();
                                    let mut __rep_count208 = 0usize;
                                    while __rep_count208 < 4294967295 {
                                        let __rep_cp209 = state.offset;
                                        if !{
                                            let __pretty_cp206 = state.offset;
                                            let __pretty_bcp207 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__pretty_hint_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp206;
                                                __builder.restore(__pretty_bcp207);
                                            }
                                            __ok
                                        } {
                                            state.offset = __rep_cp209;
                                            break;
                                        }
                                        if state.offset == __rep_cp209 {
                                            break;
                                        }
                                        __rep_count208 += 1;
                                    }
                                    if __rep_count208 < 1 {
                                        state.offset = __rep_start210;
                                        __builder.restore(__rep_bcp211);
                                        return false;
                                    }
                                };
                                let __ows213 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows213..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp214;
                            __builder.restore(__pretty_bcp215);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp217 = state.offset;
                        let __pretty_bcp218 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp216 = state.offset;
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
                                        state.offset = __pretty_cp216;
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
                            state.offset = __pretty_cp217;
                            __builder.restore(__pretty_bcp218);
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
                    let __rep_start229 = state.offset;
                    let __rep_bcp230 = __builder.checkpoint();
                    let mut __rep_count227 = 0usize;
                    while __rep_count227 < 4294967295 {
                        let __rep_cp228 = state.offset;
                        if !{
                            let __pretty_cp225 = state.offset;
                            let __pretty_bcp226 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp221 = state.offset;
                                            let __pretty_bcp222 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows219 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows219..state.offset]);
                                                    if !Self::__concatenation_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows220 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows220..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp221;
                                                __builder.restore(__pretty_bcp222);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                    {
                                        let _ = {
                                            let __pretty_cp223 = state.offset;
                                            let __pretty_bcp224 = __builder.checkpoint();
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
                                                state.offset = __pretty_cp223;
                                                __builder.restore(__pretty_bcp224);
                                            }
                                            __ok
                                        };
                                        true
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp225;
                                __builder.restore(__pretty_bcp226);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp228;
                            break;
                        }
                        if state.offset == __rep_cp228 {
                            break;
                        }
                        __rep_count227 += 1;
                    }
                    if __rep_count227 < 1 {
                        state.offset = __rep_start229;
                        __builder.restore(__rep_bcp230);
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
                let __rep_start241 = state.offset;
                let __rep_bcp242 = __builder.checkpoint();
                let mut __rep_count239 = 0usize;
                while __rep_count239 < 4294967295 {
                    let __rep_cp240 = state.offset;
                    if !{
                        let __pretty_cp237 = state.offset;
                        let __pretty_bcp238 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    if !{
                                        let __pretty_cp233 = state.offset;
                                        let __pretty_bcp234 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows231 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows231..state.offset]);
                                                if !Self::__binary_factor_prettify(state, __builder) {
                                                    return false;
                                                }
                                                let __ows232 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows232..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp233;
                                            __builder.restore(__pretty_bcp234);
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
                                            state.offset = __pretty_cp235;
                                            __builder.restore(__pretty_bcp236);
                                        }
                                        __ok
                                    };
                                    true
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp237;
                            __builder.restore(__pretty_bcp238);
                        }
                        __ok
                    } {
                        state.offset = __rep_cp240;
                        break;
                    }
                    if state.offset == __rep_cp240 {
                        break;
                    }
                    __rep_count239 += 1;
                }
                if __rep_count239 < 1 {
                    state.offset = __rep_start241;
                    __builder.restore(__rep_bcp242);
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
                let __rep_start253 = state.offset;
                let __rep_bcp254 = __builder.checkpoint();
                let mut __rep_count251 = 0usize;
                while __rep_count251 < 4294967295 {
                    let __rep_cp252 = state.offset;
                    if !{
                        let __pretty_cp249 = state.offset;
                        let __pretty_bcp250 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    if !{
                                        let __pretty_cp245 = state.offset;
                                        let __pretty_bcp246 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows243 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows243..state.offset]);
                                                if !Self::__binary_factor_prettify(state, __builder) {
                                                    return false;
                                                }
                                                let __ows244 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows244..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp245;
                                            __builder.restore(__pretty_bcp246);
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                };
                                {
                                    let _ = {
                                        let __pretty_cp247 = state.offset;
                                        let __pretty_bcp248 = __builder.checkpoint();
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
                                            state.offset = __pretty_cp247;
                                            __builder.restore(__pretty_bcp248);
                                        }
                                        __ok
                                    };
                                    true
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp249;
                            __builder.restore(__pretty_bcp250);
                        }
                        __ok
                    } {
                        state.offset = __rep_cp252;
                        break;
                    }
                    if state.offset == __rep_cp252 {
                        break;
                    }
                    __rep_count251 += 1;
                }
                if __rep_count251 < 1 {
                    state.offset = __rep_start253;
                    __builder.restore(__rep_bcp254);
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
                    let mut __rep_count260 = 0usize;
                    while __rep_count260 < 4294967295 {
                        let __rep_cp261 = state.offset;
                        if !{
                            let __pretty_cp258 = state.offset;
                            let __pretty_bcp259 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows255 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows256 = state.offset;
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b',');
                                        };
                                        __builder.text_inline_ws(&state.src[__ows255..__ows256]);
                                        let __ows257 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows257..state.offset]);
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
                                state.offset = __pretty_cp258;
                                __builder.restore(__pretty_bcp259);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp261;
                            break;
                        }
                        if state.offset == __rep_cp261 {
                            break;
                        }
                        __rep_count260 += 1;
                    }
                };
                {
                    let __ows262 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows263 = state.offset;
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'|') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'|');
                    };
                    __builder.text_inline_ws(&state.src[__ows262..__ows263]);
                    let __ows264 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows264..state.offset]);
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
                    let __pretty_cp313 = state.offset;
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
                        state.offset = __pretty_cp313;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp312 = state.offset;
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
                                state.offset = __pretty_cp312;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp310 = state.offset;
                                    let __pretty_bcp311 = __builder.checkpoint();
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
                                                    let __pretty_cp280 = state.offset;
                                                    let __pretty_bcp281 = __builder.checkpoint();
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
                                                                    let __pretty_cp267 = state.offset;
                                                                    let __pretty_bcp268 = __builder.checkpoint();
                                                                    let __ok = (|| -> bool {
                                                                        {
                                                                            let __ows265 = state.offset;
                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                            __builder
                                                                                .text_inline_ws(&state.src[__ows265..state.offset]);
                                                                            if !Self::__call_arg_prettify(state, __builder) {
                                                                                return false;
                                                                            }
                                                                            let __ows266 = state.offset;
                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                            __builder
                                                                                .text_inline_ws(&state.src[__ows266..state.offset]);
                                                                        };
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp267;
                                                                        __builder.restore(__pretty_bcp268);
                                                                    }
                                                                    __ok
                                                                } {
                                                                    return false;
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
                                                                                    let __ows269 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    let __ows270 = state.offset;
                                                                                    {
                                                                                        if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                                        {
                                                                                            return false;
                                                                                        }
                                                                                        state.offset += 1;
                                                                                        __builder.char(b',');
                                                                                    };
                                                                                    __builder.text_inline_ws(&state.src[__ows269..__ows270]);
                                                                                    let __ows271 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    __builder
                                                                                        .text_inline_ws(&state.src[__ows271..state.offset]);
                                                                                };
                                                                                {
                                                                                    if !{
                                                                                        let __pretty_cp274 = state.offset;
                                                                                        let __pretty_bcp275 = __builder.checkpoint();
                                                                                        let __ok = (|| -> bool {
                                                                                            {
                                                                                                let __ows272 = state.offset;
                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                __builder
                                                                                                    .text_inline_ws(&state.src[__ows272..state.offset]);
                                                                                                if !Self::__call_arg_prettify(state, __builder) {
                                                                                                    return false;
                                                                                                }
                                                                                                let __ows273 = state.offset;
                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                __builder
                                                                                                    .text_inline_ws(&state.src[__ows273..state.offset]);
                                                                                            };
                                                                                            true
                                                                                        })();
                                                                                        if !__ok {
                                                                                            state.offset = __pretty_cp274;
                                                                                            __builder.restore(__pretty_bcp275);
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
                                                        state.offset = __pretty_cp280;
                                                        __builder.restore(__pretty_bcp281);
                                                    }
                                                    __ok
                                                };
                                                true
                                            };
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp310;
                                        __builder.restore(__pretty_bcp311);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp308 = state.offset;
                                            let __pretty_bcp309 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__literal_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp308;
                                                __builder.restore(__pretty_bcp309);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp306 = state.offset;
                                                    let __pretty_bcp307 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        if !Self::__regex_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp306;
                                                        __builder.restore(__pretty_bcp307);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp304 = state.offset;
                                                            let __pretty_bcp305 = __builder.checkpoint();
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
                                                                            let __pretty_cp284 = state.offset;
                                                                            let __pretty_bcp285 = __builder.checkpoint();
                                                                            let __ok = (|| -> bool {
                                                                                {
                                                                                    let __ows282 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    __builder
                                                                                        .text_inline_ws(&state.src[__ows282..state.offset]);
                                                                                    if !Self::__rhs_prettify(state, __builder) {
                                                                                        return false;
                                                                                    }
                                                                                    let __ows283 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    __builder
                                                                                        .text_inline_ws(&state.src[__ows283..state.offset]);
                                                                                };
                                                                                true
                                                                            })();
                                                                            if !__ok {
                                                                                state.offset = __pretty_cp284;
                                                                                __builder.restore(__pretty_bcp285);
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
                                                                state.offset = __pretty_cp304;
                                                                __builder.restore(__pretty_bcp305);
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp302 = state.offset;
                                                                    let __pretty_bcp303 = __builder.checkpoint();
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
                                                                                    let __pretty_cp288 = state.offset;
                                                                                    let __pretty_bcp289 = __builder.checkpoint();
                                                                                    let __ok = (|| -> bool {
                                                                                        {
                                                                                            let __ows286 = state.offset;
                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                            __builder
                                                                                                .text_inline_ws(&state.src[__ows286..state.offset]);
                                                                                            if !Self::__rhs_prettify(state, __builder) {
                                                                                                return false;
                                                                                            }
                                                                                            let __ows287 = state.offset;
                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                            __builder
                                                                                                .text_inline_ws(&state.src[__ows287..state.offset]);
                                                                                        };
                                                                                        true
                                                                                    })();
                                                                                    if !__ok {
                                                                                        state.offset = __pretty_cp288;
                                                                                        __builder.restore(__pretty_bcp289);
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
                                                                        state.offset = __pretty_cp302;
                                                                        __builder.restore(__pretty_bcp303);
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp300 = state.offset;
                                                                            let __pretty_bcp301 = __builder.checkpoint();
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
                                                                                            let __pretty_cp292 = state.offset;
                                                                                            let __pretty_bcp293 = __builder.checkpoint();
                                                                                            let __ok = (|| -> bool {
                                                                                                {
                                                                                                    let __ows290 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows290..state.offset]);
                                                                                                    if !Self::__rhs_prettify(state, __builder) {
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
                                                                                state.offset = __pretty_cp300;
                                                                                __builder.restore(__pretty_bcp301);
                                                                            }
                                                                            __ok
                                                                        } {
                                                                            {
                                                                                if !{
                                                                                    let __pretty_cp298 = state.offset;
                                                                                    let __pretty_bcp299 = __builder.checkpoint();
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
                                                                                                    let __pretty_cp296 = state.offset;
                                                                                                    let __pretty_bcp297 = __builder.checkpoint();
                                                                                                    let __ok = (|| -> bool {
                                                                                                        {
                                                                                                            let __ows294 = state.offset;
                                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                            __builder
                                                                                                                .text_inline_ws(&state.src[__ows294..state.offset]);
                                                                                                            if !Self::__rhs_prettify(state, __builder) {
                                                                                                                return false;
                                                                                                            }
                                                                                                            let __ows295 = state.offset;
                                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                            __builder
                                                                                                                .text_inline_ws(&state.src[__ows295..state.offset]);
                                                                                                        };
                                                                                                        true
                                                                                                    })();
                                                                                                    if !__ok {
                                                                                                        state.offset = __pretty_cp296;
                                                                                                        __builder.restore(__pretty_bcp297);
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
                                                                                        state.offset = __pretty_cp298;
                                                                                        __builder.restore(__pretty_bcp299);
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
                    let mut __rep_count320 = 0usize;
                    while __rep_count320 < 4294967295 {
                        let __rep_cp321 = state.offset;
                        if !{
                            let __pretty_cp318 = state.offset;
                            let __pretty_bcp319 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp316 = state.offset;
                                            let __pretty_bcp317 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows314 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows314..state.offset]);
                                                    if !Self::__binary_operators_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows315 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows315..state.offset]);
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
                                    };
                                    if !Self::__mapped_factor_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp318;
                                __builder.restore(__pretty_bcp319);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp321;
                            break;
                        }
                        if state.offset == __rep_cp321 {
                            break;
                        }
                        __rep_count320 += 1;
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
                    let __pretty_cp322 = state.offset;
                    let __pretty_bcp323 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        if !Self::__closure_prettify(state, __builder) {
                            return false;
                        }
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp322;
                        __builder.restore(__pretty_bcp323);
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
                        let __pretty_cp324 = state.offset;
                        let __pretty_bcp325 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__big_comment_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp324;
                            __builder.restore(__pretty_bcp325);
                        }
                        __ok
                    };
                    true
                };
                {
                    if !{
                        let __pretty_cp328 = state.offset;
                        let __pretty_bcp329 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows326 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows326..state.offset]);
                                if !Self::__term_prettify(state, __builder) {
                                    return false;
                                }
                                let __ows327 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows327..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp328;
                            __builder.restore(__pretty_bcp329);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp330 = state.offset;
                        let __pretty_bcp331 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__modifier_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp330;
                            __builder.restore(__pretty_bcp331);
                        }
                        __ok
                    };
                    true
                };
                {
                    let _ = {
                        let __pretty_cp332 = state.offset;
                        let __pretty_bcp333 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__big_comment_prettify(state, __builder) {
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
                        let __pretty_cp339 = state.offset;
                        let __pretty_bcp340 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    let __ows334 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ows335 = state.offset;
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
                                    __builder.text_inline_ws(&state.src[__ows334..__ows335]);
                                    let __ows336 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows336..state.offset]);
                                };
                                {
                                    if !Self::__value_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    {
                                        let _ = {
                                            let __pretty_cp337 = state.offset;
                                            let __pretty_bcp338 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__type_annotation_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp337;
                                                __builder.restore(__pretty_bcp338);
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
                            state.offset = __pretty_cp339;
                            __builder.restore(__pretty_bcp340);
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
                        let __ows341 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows342 = state.offset;
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'=') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'=');
                        };
                        __builder.text_inline_ws(&state.src[__ows341..__ows342]);
                        let __ows343 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows343..state.offset]);
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
                                    if !Self::__rhs_prettify(state, __builder) {
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
                        if !{
                            let __pretty_cp348 = state.offset;
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
                                state.offset = __pretty_cp348;
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
                    let __ows349 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows350 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows349..__ows350]);
                    let __ows351 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows351..state.offset]);
                };
                {
                    let __ows352 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows353 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows352..__ows353]);
                    let __ows354 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows354..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp357 = state.offset;
                        let __pretty_bcp358 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows355 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows355..state.offset]);
                                if !Self::__rhs_prettify(state, __builder) {
                                    return false;
                                }
                                let __ows356 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows356..state.offset]);
                            };
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
                };
                {
                    let _ = {
                        let __pretty_cp360 = state.offset;
                        let __pretty_bcp361 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !{
                                    let __pretty_cp359 = state.offset;
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
                                        state.offset = __pretty_cp359;
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
                            state.offset = __pretty_cp360;
                            __builder.restore(__pretty_bcp361);
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
                    let __pretty_cp374 = state.offset;
                    let __pretty_bcp375 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        if !Self::__import_directive_prettify(state, __builder) {
                            return false;
                        }
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp374;
                        __builder.restore(__pretty_bcp375);
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp372 = state.offset;
                            let __pretty_bcp373 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                if !Self::__recover_directive_prettify(state, __builder) {
                                    return false;
                                }
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp372;
                                __builder.restore(__pretty_bcp373);
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp370 = state.offset;
                                    let __pretty_bcp371 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        if !Self::__pretty_directive_prettify(state, __builder) {
                                            return false;
                                        }
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp370;
                                        __builder.restore(__pretty_bcp371);
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp368 = state.offset;
                                            let __pretty_bcp369 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__ws_directive_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp368;
                                                __builder.restore(__pretty_bcp369);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp366 = state.offset;
                                                    let __pretty_bcp367 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        if !Self::__token_directive_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp366;
                                                        __builder.restore(__pretty_bcp367);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp364 = state.offset;
                                                            let __pretty_bcp365 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                if !Self::__debug_directive_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp364;
                                                                __builder.restore(__pretty_bcp365);
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp362 = state.offset;
                                                                    let __pretty_bcp363 = __builder.checkpoint();
                                                                    let __ok = (|| -> bool {
                                                                        if !Self::__host_directive_prettify(state, __builder) {
                                                                            return false;
                                                                        }
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp362;
                                                                        __builder.restore(__pretty_bcp363);
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
                let mut __rep_count387 = 0usize;
                while __rep_count387 < 4294967295 {
                    let __rep_cp388 = state.offset;
                    let __iter_cp = if __rep_count387 > 0 {
                        Some(__builder.checkpoint())
                    } else {
                        None
                    };
                    if __rep_count387 > 0 {
                        __builder.hardline();
                    }
                    if !{
                        let __pretty_cp386 = state.offset;
                        let __ok = (|| -> bool {
                            {
                                {
                                    let _ = {
                                        let __pretty_cp376 = state.offset;
                                        let __pretty_bcp377 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            if !Self::__comment_prettify(state, __builder) {
                                                return false;
                                            }
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp376;
                                            __builder.restore(__pretty_bcp377);
                                        }
                                        __ok
                                    };
                                    true
                                };
                                {
                                    if !{
                                        let __pretty_cp382 = state.offset;
                                        let __pretty_bcp383 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows380 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows380..state.offset]);
                                                {
                                                    if !{
                                                        let __pretty_cp378 = state.offset;
                                                        let __pretty_bcp379 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            if !Self::__directive_prettify(state, __builder) {
                                                                return false;
                                                            }
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp378;
                                                            __builder.restore(__pretty_bcp379);
                                                        }
                                                        __ok
                                                    } {
                                                        if !Self::__rule_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                    }
                                                };
                                                let __ows381 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder
                                                    .text_inline_ws(&state.src[__ows381..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp382;
                                            __builder.restore(__pretty_bcp383);
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                };
                                {
                                    let _ = {
                                        let __pretty_cp384 = state.offset;
                                        let __pretty_bcp385 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            if !Self::__comment_prettify(state, __builder) {
                                                return false;
                                            }
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp384;
                                            __builder.restore(__pretty_bcp385);
                                        }
                                        __ok
                                    };
                                    true
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp386;
                        }
                        __ok
                    } {
                        state.offset = __rep_cp388;
                        if let Some(__bcp) = __iter_cp {
                            __builder.restore(__bcp);
                        }
                        break;
                    }
                    if state.offset == __rep_cp388 {
                        break;
                    }
                    __rep_count387 += 1;
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

