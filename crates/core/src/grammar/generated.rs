//! AUTO-GENERATED from grammar/bbnf/bbnf.bbnf — do not edit manually.
//! Regenerate: scripts/bootstrap-bbnf.sh

use ::parse_that::*;

pub struct BbnfBootstrap;

#[allow(non_upper_case_globals)]
pub const GRAMMAR_BbnfBootstrap: [&'static str; 1usize] = [
    "// BBNF \u{2014} Better Backus-Naur Form\n// Self-hosted grammar definition.\n\n@import { value_expr, type_annotation } from \"expressions\" ;\n\n// \u{2500}\u{2500}\u{2500} Terminals \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\nidentifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ ;\n\nliteral = \"\\\"\" , /(\\\\.|[^\"\\\\])*/  , \"\\\"\"\n        | \"\'\"  , /(\\\\.|[^\'\\\\])*/  , \"\'\"\n        | \"`\"  , /(\\\\.|[^`\\\\])*/  , \"`\" ;\n\nregex = \"/\" , /(\\\\.|[^\\/])+/ , \"/\" ;\n\nbig_comment = ( \"/*\" , /[^\\*]*/ , \"*/\" ) ?w ;\ncomment = ( \"//\" , /.*/ ) ?w ;\n\n// \u{2500}\u{2500}\u{2500} Expressions \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\nlhs = identifier ;\n\nterm = \"\u{3b5}\" | \"epsilon\"\n     | identifier , ( \"(\" , rhs ?w , ( \",\" ?w , rhs ?w ) * , \")\" ) ?\n     | literal\n     | regex\n     | \"(\" , rhs ?w , \")\"\n     | \"[\" , rhs ?w , \"]\"\n     | \"{\" , rhs ?w , \"}\" ;\n\nmodifier = \"?w\" | \"?\" | \"*\" | \"+\" ;\n\nmodifier = \"?w\" | \"?\" | \"*\" | \"+\" ;\nfactor = big_comment ? , term ?w , modifier ? , big_comment ? ;\n\n// Map syntax: factor -> value_expr : type\nmapped_factor = factor , ( \"->\" ?w , ( value_expr , type_annotation ? ) ) ? ;\n\nbinary_operators = \"<<\" | \">>\" | \"-\" ;\nbinary_factor = mapped_factor , ( binary_operators ?w , mapped_factor ) * ;\n\nconcatenation = ( binary_factor ?w , \",\" ? ) + ;\nalternation = ( concatenation ?w , \"|\" ? ) + ;\n\n// Closures at rule level: |params| rhs (grammar functions)\nclosure = \"|\" , identifier , ( \",\" ?w , identifier ) * , \"|\" ?w , rhs ;\nrhs = closure | alternation ;\n\n// \u{2500}\u{2500}\u{2500} Rules and Directives \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\nrule = lhs , \"=\" ?w , rhs ?w , ( \";\" | \".\" ) ;\n\nimport_path = \"\\\"\" , /(\\\\.|[^\"\\\\])*/ , \"\\\"\" ;\nimport_items = \"{\" ?w , identifier , ( \",\" ?w , identifier ) * , \"}\" ;\nimport_directive = \"@import\" ?w , (\n      import_items ?w , \"from\" ?w , import_path\n    | import_path\n) , ( \";\" | \".\" ) ? ;\n\nrecover_directive = \"@recover\" ?w , identifier ?w , rhs ?w , ( \";\" | \".\" ) ? ;\n\npretty_hint = identifier ;\npretty_directive = \"@pretty\" ?w , ( \"*\" | identifier ) ?w , (pretty_hint+) ?w , ( \";\" | \".\" ) ? ;\n\nws_directive = \"@ws\" ?w , regex ?w , ( \";\" | \".\" ) ? ;\ntoken_directive = \"@token\" ?w , identifier ?w , ( \";\" | \".\" ) ? ;\ndebug_directive = \"@debug\" ?w , ( \"*\" | identifier ) ?w , ( \";\" | \".\" ) ? ;\nhost_directive = \"@host\" ?w , identifier ?w , ( \";\" | \".\" ) ? ;\n\ndirective = import_directive\n          | recover_directive\n          | pretty_directive\n          | ws_directive\n          | token_directive\n          | debug_directive\n          | host_directive ;\n\n// Grammar: sequence of comments, directives, and rules in any order.\ngrammar = ( comment ? , ( directive | rule ) ?w , comment ? ) * ;\n\n@pretty grammar block ;\n@pretty rule group ;\n@pretty alternation group ;\n",
];
#[derive(Debug)]
pub enum BbnfBootstrapEnum<'a> {
    regex(::parse_that::Span<'a>),
    cmp_op(::parse_that::Span<'a>),
    value_input(
        (
            ::parse_that::Span<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    value_ident(::parse_that::Span<'a>),
    float_lit(::parse_that::Span<'a>),
    int_lit(::parse_that::Span<'a>),
    bool_lit(::parse_that::Span<'a>),
    string_lit(::parse_that::Span<'a>),
    concatenation(&'a [(&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)]),
    binary_operators(::parse_that::Span<'a>),
    alternation(&'a [(&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)]),
    identifier(::parse_that::Span<'a>),
    literal(::parse_that::Span<'a>),
    modifier(::parse_that::Span<'a>),
    big_comment(::parse_that::Span<'a>),
    pretty_directive(
        (
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
            &'a [BbnfBootstrapEnum<'a>],
            ::parse_that::Span<'a>,
        ),
    ),
    mul_op(::parse_that::Span<'a>),
    comment(::parse_that::Span<'a>),
    add_op(::parse_that::Span<'a>),
    type_name(::parse_that::Span<'a>),
    debug_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    import_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    import_path(::parse_that::Span<'a>),
    grammar(
        &'a [(
            Option<&'a BbnfBootstrapEnum<'a>>,
            &'a BbnfBootstrapEnum<'a>,
            Option<&'a BbnfBootstrapEnum<'a>>,
        )],
    ),
    ws_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    value_fn_call(
        (
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
    import_items(
        (
            ::parse_that::Span<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
            ::parse_that::Span<'a>,
        ),
    ),
    host_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    token_directive(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    closure(
        (
            ::parse_that::Span<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
        ),
    ),
    type_annotation((::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)),
    value_mul(
        (
            &'a BbnfBootstrapEnum<'a>,
            &'a [(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)],
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
    value_or(
        (
            &'a BbnfBootstrapEnum<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    value_closure(
        (
            ::parse_that::Span<'a>,
            &'a [(::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)],
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
        ),
    ),
    value_atom(&'a BbnfBootstrapEnum<'a>),
    value_unary(&'a BbnfBootstrapEnum<'a>),
    recover_directive(
        (
            ::parse_that::Span<'a>,
            &'a BbnfBootstrapEnum<'a>,
            &'a BbnfBootstrapEnum<'a>,
            ::parse_that::Span<'a>,
        ),
    ),
    rule((::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)),
    term(&'a BbnfBootstrapEnum<'a>),
    directive(&'a BbnfBootstrapEnum<'a>),
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
    binary_factor(
        (
            &'a BbnfBootstrapEnum<'a>,
            &'a [(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)],
        ),
    ),
    pretty_directive_0(::parse_that::Span<'a>),
    debug_directive_0(::parse_that::Span<'a>),
    import_directive_0((&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)),
    value_atom_0(
        (::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>),
    ),
    value_unary_0((::parse_that::Span<'a>, &'a BbnfBootstrapEnum<'a>)),
    term_0(::parse_that::Span<'a>),
    term_1(
        (
            ::parse_that::Span<'a>,
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
            BbnfBootstrapEnum::regex(__self_0) => {
                BbnfBootstrapEnum::regex(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::cmp_op(__self_0) => {
                BbnfBootstrapEnum::cmp_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_input(__self_0) => {
                BbnfBootstrapEnum::value_input(::core::clone::Clone::clone(__self_0))
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
            BbnfBootstrapEnum::bool_lit(__self_0) => {
                BbnfBootstrapEnum::bool_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::string_lit(__self_0) => {
                BbnfBootstrapEnum::string_lit(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::concatenation(__self_0) => {
                BbnfBootstrapEnum::concatenation(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::binary_operators(__self_0) => {
                BbnfBootstrapEnum::binary_operators(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::alternation(__self_0) => {
                BbnfBootstrapEnum::alternation(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::identifier(__self_0) => {
                BbnfBootstrapEnum::identifier(::core::clone::Clone::clone(__self_0))
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
            BbnfBootstrapEnum::pretty_directive(__self_0) => {
                BbnfBootstrapEnum::pretty_directive(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::mul_op(__self_0) => {
                BbnfBootstrapEnum::mul_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::comment(__self_0) => {
                BbnfBootstrapEnum::comment(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::add_op(__self_0) => {
                BbnfBootstrapEnum::add_op(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::type_name(__self_0) => {
                BbnfBootstrapEnum::type_name(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::debug_directive(__self_0) => {
                BbnfBootstrapEnum::debug_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::import_directive(__self_0) => {
                BbnfBootstrapEnum::import_directive(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::import_path(__self_0) => {
                BbnfBootstrapEnum::import_path(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::grammar(__self_0) => {
                BbnfBootstrapEnum::grammar(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::ws_directive(__self_0) => {
                BbnfBootstrapEnum::ws_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_fn_call(__self_0) => {
                BbnfBootstrapEnum::value_fn_call(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::import_items(__self_0) => {
                BbnfBootstrapEnum::import_items(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::host_directive(__self_0) => {
                BbnfBootstrapEnum::host_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::token_directive(__self_0) => {
                BbnfBootstrapEnum::token_directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::closure(__self_0) => {
                BbnfBootstrapEnum::closure(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::type_annotation(__self_0) => {
                BbnfBootstrapEnum::type_annotation(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::value_mul(__self_0) => {
                BbnfBootstrapEnum::value_mul(::core::clone::Clone::clone(__self_0))
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
            BbnfBootstrapEnum::value_or(__self_0) => {
                BbnfBootstrapEnum::value_or(::core::clone::Clone::clone(__self_0))
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
            BbnfBootstrapEnum::recover_directive(__self_0) => {
                BbnfBootstrapEnum::recover_directive(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::rule(__self_0) => {
                BbnfBootstrapEnum::rule(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::term(__self_0) => {
                BbnfBootstrapEnum::term(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::directive(__self_0) => {
                BbnfBootstrapEnum::directive(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::factor(__self_0) => {
                BbnfBootstrapEnum::factor(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::mapped_factor(__self_0) => {
                BbnfBootstrapEnum::mapped_factor(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::binary_factor(__self_0) => {
                BbnfBootstrapEnum::binary_factor(::core::clone::Clone::clone(__self_0))
            }
            BbnfBootstrapEnum::pretty_directive_0(__self_0) => {
                BbnfBootstrapEnum::pretty_directive_0(
                    ::core::clone::Clone::clone(__self_0),
                )
            }
            BbnfBootstrapEnum::debug_directive_0(__self_0) => {
                BbnfBootstrapEnum::debug_directive_0(
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
    __s1: ::std::cell::UnsafeCell<
        Vec<(&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)>,
    >,
    __s2: ::std::cell::UnsafeCell<Vec<BbnfBootstrapEnum<'a>>>,
    __s3: ::std::cell::UnsafeCell<
        Vec<
            (
                Option<&'a BbnfBootstrapEnum<'a>>,
                &'a BbnfBootstrapEnum<'a>,
                Option<&'a BbnfBootstrapEnum<'a>>,
            ),
        >,
    >,
    __s4: ::std::cell::UnsafeCell<
        Vec<(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)>,
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
    fn __s1(&self) -> &mut Vec<(&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)> {
        unsafe { &mut *self.__s1.get() }
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __c1(
        &'a self,
        depth: usize,
    ) -> &'a [(&'a BbnfBootstrapEnum<'a>, ::parse_that::Span<'a>)] {
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
    fn __s3(
        &self,
    ) -> &mut Vec<
        (
            Option<&'a BbnfBootstrapEnum<'a>>,
            &'a BbnfBootstrapEnum<'a>,
            Option<&'a BbnfBootstrapEnum<'a>>,
        ),
    > {
        unsafe { &mut *self.__s3.get() }
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __c3(
        &'a self,
        depth: usize,
    ) -> &'a [(
        Option<&'a BbnfBootstrapEnum<'a>>,
        &'a BbnfBootstrapEnum<'a>,
        Option<&'a BbnfBootstrapEnum<'a>>,
    )] {
        let s = self.__s3();
        let slice = self.__slab.alloc_slice_clone(&s[depth..]);
        s.truncate(depth);
        slice
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __s4(&self) -> &mut Vec<(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)> {
        unsafe { &mut *self.__s4.get() }
    }
    #[inline(always)]
    #[allow(non_snake_case)]
    fn __c4(
        &'a self,
        depth: usize,
    ) -> &'a [(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)] {
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
            {
                panic!("slab parser requires parse_with_context()");
            }
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
    fn __alternation<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __depth12 = __BbnfBootstrapEnum_alloc(state).__s1().len();
                loop {
                    let __prev13 = state.offset;
                    match (|| {
                        let __v10 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = {
                                let __depth8 = __BbnfBootstrapEnum_alloc(state)
                                    .__s1()
                                    .len();
                                loop {
                                    let __prev9 = state.offset;
                                    match (|| {
                                        let __v6 = {
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
                                        let __sp7 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        Some((__v6, __sp7))
                                    })() {
                                        Some(__value) => {
                                            __BbnfBootstrapEnum_alloc(state).__s1().push(__value);
                                            if state.offset == __prev9 {
                                                break;
                                            }
                                        }
                                        None => {
                                            state.offset = __prev9;
                                            break;
                                        }
                                    }
                                }
                                if (__BbnfBootstrapEnum_alloc(state).__s1().len()
                                    - __depth8) >= 1usize
                                {
                                    Some(__BbnfBootstrapEnum_alloc(state).__c1(__depth8))
                                } else {
                                    __BbnfBootstrapEnum_alloc(state).__s1().truncate(__depth8);
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
                        let __sp11 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__v10, __sp11))
                    })() {
                        Some(__value) => {
                            __BbnfBootstrapEnum_alloc(state).__s1().push(__value);
                            if state.offset == __prev13 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __prev13;
                            break;
                        }
                    }
                }
                if (__BbnfBootstrapEnum_alloc(state).__s1().len() - __depth12) >= 1usize
                {
                    Some(__BbnfBootstrapEnum_alloc(state).__c1(__depth12))
                } else {
                    __BbnfBootstrapEnum_alloc(state).__s1().truncate(__depth12);
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
    fn __type_name<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __kd_cp14 = state.offset;
                if let Some(ref __kd_s) = ::parse_that::scan_ident(state) {
                    let __kd_bytes = &state.src_bytes[__kd_s.start..__kd_s.end];
                    let __kd_len = __kd_bytes.len();
                    if (__kd_len == 1usize && __kd_bytes == &[b'u']) {
                        state.offset = __kd_cp14;
                        return (|| {
                            let __sp_start = state.offset;
                            if state.offset < state.src.len()
                                && state.src.as_bytes()[state.offset] == 117u8
                            {
                                let __start = state.offset;
                                state.offset += 1;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            }?;
                            (|| {
                                let __r = if state.src[state.offset..].starts_with("16") {
                                    let __start = state.offset;
                                    state.offset += 2usize;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                let __r = if state.src[state.offset..].starts_with("32") {
                                    let __start = state.offset;
                                    state.offset += 2usize;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                let __r = if state.src[state.offset..].starts_with("64") {
                                    let __start = state.offset;
                                    state.offset += 2usize;
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
                                    && state.src.as_bytes()[state.offset] == 56u8
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
                            Some(
                                ::parse_that::Span::new(__sp_start, state.offset, state.src),
                            )
                        })();
                    }
                    if (__kd_len == 5usize
                        && __kd_bytes == &[b'u', b's', b'i', b'z', b'e'])
                    {
                        state.offset = __kd_cp14;
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
                    if (__kd_len == 4usize && __kd_bytes == &[b'b', b'o', b'o', b'l']) {
                        state.offset = __kd_cp14;
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
                    if (__kd_len == 1usize && __kd_bytes == &[b'i']) {
                        state.offset = __kd_cp14;
                        return (|| {
                            let __sp_start = state.offset;
                            if state.offset < state.src.len()
                                && state.src.as_bytes()[state.offset] == 105u8
                            {
                                let __start = state.offset;
                                state.offset += 1;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            }?;
                            (|| {
                                let __r = if state.src[state.offset..].starts_with("32") {
                                    let __start = state.offset;
                                    state.offset += 2usize;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                let __r = if state.src[state.offset..].starts_with("64") {
                                    let __start = state.offset;
                                    state.offset += 2usize;
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
                            Some(
                                ::parse_that::Span::new(__sp_start, state.offset, state.src),
                            )
                        })();
                    }
                    if (__kd_len == 1usize && __kd_bytes == &[b'f']) {
                        state.offset = __kd_cp14;
                        return (|| {
                            let __sp_start = state.offset;
                            if state.offset < state.src.len()
                                && state.src.as_bytes()[state.offset] == 102u8
                            {
                                let __start = state.offset;
                                state.offset += 1;
                                Some(
                                    ::parse_that::Span::new(__start, state.offset, state.src),
                                )
                            } else {
                                None
                            }?;
                            (|| {
                                let __r = if state.src[state.offset..].starts_with("32") {
                                    let __start = state.offset;
                                    state.offset += 2usize;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                };
                                if __r.is_some() {
                                    return __r;
                                }
                                let __r = if state.src[state.offset..].starts_with("64") {
                                    let __start = state.offset;
                                    state.offset += 2usize;
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
                            Some(
                                ::parse_that::Span::new(__sp_start, state.offset, state.src),
                            )
                        })();
                    }
                }
                state.offset = __kd_cp14;
                None
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::type_name(__x))
    }
    pub fn type_name<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__type_name)
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
                let __sp24 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                let __v25 = (|| {
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
                    {
                        let __cp = state.offset;
                        let __result = ((|| {
                            let __v22 = {
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
                                    ::parse_that::scan_ident(state)
                                        .map(|__v| BbnfBootstrapEnum::identifier(__v))?;
                                    let __sp19 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v20 = {
                                        let __depth17 = __BbnfBootstrapEnum_alloc(state)
                                            .__s0()
                                            .len();
                                        loop {
                                            let __prev18 = state.offset;
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
                                                let __sp15 = ::parse_that::Span::new(
                                                    __sp_start,
                                                    state.offset,
                                                    state.src,
                                                );
                                                let __v16 = ::parse_that::scan_ident(state)
                                                    .map(|__inner| {
                                                        let __v = BbnfBootstrapEnum::identifier(__inner);
                                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                    })?;
                                                Some((__sp15, __v16))
                                            })() {
                                                Some(__value) => {
                                                    __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
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
                                        Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth17))
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
                                    let __sp21 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    Some((__sp19, __v20, __sp21))
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
                            (|| {
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
                                .map(|__v| BbnfBootstrapEnum::import_path(__v))?;
                            let __sp23 = ::parse_that::Span::new(
                                __sp_start,
                                state.offset,
                                state.src,
                            );
                            Some((__v22, __sp23))
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
                let __sp26 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                Some((__sp24, __v25, __sp26))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::import_directive(__x))
    }
    pub fn import_directive<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__import_directive)
    }
    #[allow(non_snake_case)]
    fn __grammar<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __depth30 = __BbnfBootstrapEnum_alloc(state).__s3().len();
                loop {
                    let __prev31 = state.offset;
                    match (|| {
                        let __v27 = {
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
                        let __v28 = {
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
                        let __v29 = {
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
                        Some((__v27, __v28, __v29))
                    })() {
                        Some(__value) => {
                            __BbnfBootstrapEnum_alloc(state).__s3().push(__value);
                            if state.offset == __prev31 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __prev31;
                            break;
                        }
                    }
                }
                Some(__BbnfBootstrapEnum_alloc(state).__c3(__depth30))
            }
        })()
            .map(|__x| BbnfBootstrapEnum::grammar(__x))
    }
    pub fn grammar<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__grammar)
    }
    #[allow(non_snake_case)]
    fn __value_fn_call<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __sp_start = state.offset;
                ::parse_that::scan_ident(state)
                    .map(|__v| BbnfBootstrapEnum::value_ident(__v))?;
                if state.offset < state.src.len()
                    && state.src.as_bytes()[state.offset] == 40u8
                {
                    let __start = state.offset;
                    state.offset += 1;
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                } else {
                    None
                }?;
                let __sp37 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                let __v38 = {
                    let __cp = state.offset;
                    match (|| {
                        let __chain_head33 = Self::__value_expr(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        let __chain_depth32 = __BbnfBootstrapEnum_alloc(state)
                            .__s0()
                            .len();
                        loop {
                            let __chain_prev34 = state.offset;
                            match (|| {
                                let __chain_op35 = (|| {
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
                                let __chain_rhs36 = Self::__value_expr(state)
                                    .map(|__v| {
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                Some((__chain_op35, __chain_rhs36))
                            })() {
                                Some(__value) => {
                                    let (__chain_op35, __chain_rhs36) = __value;
                                    __BbnfBootstrapEnum_alloc(state)
                                        .__s0()
                                        .push((__chain_op35, __chain_rhs36));
                                    if state.offset == __chain_prev34 {
                                        break;
                                    }
                                }
                                None => {
                                    state.offset = __chain_prev34;
                                    break;
                                }
                            }
                        }
                        Some((
                            __chain_head33,
                            __BbnfBootstrapEnum_alloc(state).__c0(__chain_depth32),
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
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                } else {
                    None
                }?;
                let __sp39 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                Some((__sp37, __v38, __sp39))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::value_fn_call(__x))
    }
    pub fn value_fn_call<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_fn_call)
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
                    ::parse_that::scan_ident(state)
                        .map(|__v| BbnfBootstrapEnum::identifier(__v))?;
                    let __sp44 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v45 = {
                        let __depth42 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                        loop {
                            let __prev43 = state.offset;
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
                                let __sp40 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                let __v41 = ::parse_that::scan_ident(state)
                                    .map(|__inner| {
                                        let __v = BbnfBootstrapEnum::identifier(__inner);
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                Some((__sp40, __v41))
                            })() {
                                Some(__value) => {
                                    __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                    if state.offset == __prev43 {
                                        break;
                                    }
                                }
                                None => {
                                    state.offset = __prev43;
                                    break;
                                }
                            }
                        }
                        Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth42))
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
                    let __sp46 = ::parse_that::Span::new(
                        __sp_start,
                        state.offset,
                        state.src,
                    );
                    let __v47 = Self::__rhs(state)
                        .map(|__v| {
                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                        })?;
                    Some((__sp44, __v45, __sp46, __v47))
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
    fn __value_mul<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __chain_head49 = Self::__value_unary(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __chain_depth48 = __BbnfBootstrapEnum_alloc(state).__s4().len();
                loop {
                    let __chain_prev50 = state.offset;
                    match (|| {
                        let __chain_op51 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = if state.offset < state.src.len() {
                                match state.src.as_bytes()[state.offset] {
                                    42u8 => {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    }
                                    47u8 => {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    }
                                    37u8 => {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    }
                                    _ => None,
                                }
                            } else {
                                None
                            }
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::mul_op(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __chain_rhs52 = Self::__value_unary(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op51, __chain_rhs52))
                    })() {
                        Some(__value) => {
                            let (__chain_op51, __chain_rhs52) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s4()
                                .push((__chain_op51, __chain_rhs52));
                            if state.offset == __chain_prev50 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __chain_prev50;
                            break;
                        }
                    }
                }
                Some((
                    __chain_head49,
                    __BbnfBootstrapEnum_alloc(state).__c4(__chain_depth48),
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
                let __chain_head54 = Self::__value_mul(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __chain_depth53 = __BbnfBootstrapEnum_alloc(state).__s4().len();
                loop {
                    let __chain_prev55 = state.offset;
                    match (|| {
                        let __chain_op56 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = if state.offset < state.src.len() {
                                match state.src.as_bytes()[state.offset] {
                                    43u8 => {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    }
                                    45u8 => {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    }
                                    _ => None,
                                }
                            } else {
                                None
                            }
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::add_op(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __chain_rhs57 = Self::__value_mul(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op56, __chain_rhs57))
                    })() {
                        Some(__value) => {
                            let (__chain_op56, __chain_rhs57) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s4()
                                .push((__chain_op56, __chain_rhs57));
                            if state.offset == __chain_prev55 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __chain_prev55;
                            break;
                        }
                    }
                }
                Some((
                    __chain_head54,
                    __BbnfBootstrapEnum_alloc(state).__c4(__chain_depth53),
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
                let __chain_head59 = Self::__value_add(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __chain_depth58 = __BbnfBootstrapEnum_alloc(state).__s4().len();
                loop {
                    let __chain_prev60 = state.offset;
                    match (|| {
                        let __chain_op61 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = (|| {
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
                                None
                            })()
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::cmp_op(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __chain_rhs62 = Self::__value_add(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op61, __chain_rhs62))
                    })() {
                        Some(__value) => {
                            let (__chain_op61, __chain_rhs62) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s4()
                                .push((__chain_op61, __chain_rhs62));
                            if state.offset == __chain_prev60 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __chain_prev60;
                            break;
                        }
                    }
                }
                Some((
                    __chain_head59,
                    __BbnfBootstrapEnum_alloc(state).__c4(__chain_depth58),
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
                let __chain_head64 = Self::__value_cmp(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __chain_depth63 = __BbnfBootstrapEnum_alloc(state).__s0().len();
                loop {
                    let __chain_prev65 = state.offset;
                    match (|| {
                        let __chain_op66 = (|| {
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
                        let __chain_rhs67 = Self::__value_cmp(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op66, __chain_rhs67))
                    })() {
                        Some(__value) => {
                            let (__chain_op66, __chain_rhs67) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s0()
                                .push((__chain_op66, __chain_rhs67));
                            if state.offset == __chain_prev65 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __chain_prev65;
                            break;
                        }
                    }
                }
                Some((
                    __chain_head64,
                    __BbnfBootstrapEnum_alloc(state).__c0(__chain_depth63),
                ))
            }
        })()
            .map(|__x| BbnfBootstrapEnum::value_and(__x))
    }
    pub fn value_and<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_and)
    }
    #[allow(non_snake_case)]
    fn __value_atom<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                {
                    let __cp = state.offset;
                    let __result = ::parse_that::number_span_scan_strict(state)
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
                        let __end = state.src_bytes.len();
                        let mut __pos = __start;
                        while __pos < __end {
                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                            if (__b >= b'0' && __b <= b'9') || __b == b']' || __b == b'*'
                                || __b == b'.' || __b == b'['
                                || (__b >= b'0' && __b <= b'9')
                            {
                                __pos += 1;
                            } else {
                                break;
                            }
                        }
                        if __pos >= __start + 1 {
                            state.offset = __pos;
                            Some(::parse_that::Span::new(__start, __pos, state.src))
                        } else {
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
                    let __result = if state.offset < state.src.len() {
                        match state.src.as_bytes()[state.offset] {
                            102u8 => {
                                if state.src[state.offset..].starts_with("false") {
                                    let __start = state.offset;
                                    state.offset += 5usize;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                }
                            }
                            116u8 => {
                                if state.src[state.offset..].starts_with("true") {
                                    let __start = state.offset;
                                    state.offset += 4usize;
                                    Some(
                                        ::parse_that::Span::new(__start, state.offset, state.src),
                                    )
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
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
                    let __result = Self::__value_fn_call(state)
                        .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v));
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
                        let __sp68 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v69 = {
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
                        let __sp70 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp68, __v69, __sp70))
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
                None
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::value_atom(__x))
    }
    pub fn value_atom<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_atom)
    }
    #[allow(non_snake_case)]
    fn __value_expr<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        if state.offset < state.src.len() {
            match state.src.as_bytes()[state.offset] {
                124u8 => {
                    (|| {
                        let __sp_start = state.offset;
                        {
                            let __start = state.offset;
                            state.offset += 1;
                            Some(
                                ::parse_that::Span::new(__start, state.offset, state.src),
                            )
                        }?;
                        ::parse_that::scan_ident(state)
                            .map(|__v| BbnfBootstrapEnum::value_ident(__v))?;
                        let __sp75 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v76 = {
                            let __depth73 = __BbnfBootstrapEnum_alloc(state)
                                .__s0()
                                .len();
                            loop {
                                let __prev74 = state.offset;
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
                                    let __sp71 = ::parse_that::Span::new(
                                        __sp_start,
                                        state.offset,
                                        state.src,
                                    );
                                    let __v72 = ::parse_that::scan_ident(state)
                                        .map(|__inner| {
                                            let __v = BbnfBootstrapEnum::value_ident(__inner);
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        })?;
                                    Some((__sp71, __v72))
                                })() {
                                    Some(__value) => {
                                        __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                        if state.offset == __prev74 {
                                            break;
                                        }
                                    }
                                    None => {
                                        state.offset = __prev74;
                                        break;
                                    }
                                }
                            }
                            Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth73))
                        }?;
                        let __sp_start = state.offset;
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
                        }?;
                        let __sp77 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v78 = Self::__value_expr(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__sp75, __v76, __sp77, __v78))
                    })()
                        .map(|__v| BbnfBootstrapEnum::value_closure(__v))
                }
                33u8 | 34u8 | 40u8 | 45u8 | 46u8 | 48u8 | 49u8 | 50u8 | 51u8 | 52u8
                | 53u8 | 54u8 | 55u8 | 56u8 | 57u8 | 65u8 | 66u8 | 67u8 | 68u8 | 69u8
                | 70u8 | 71u8 | 72u8 | 73u8 | 74u8 | 75u8 | 76u8 | 77u8 | 78u8 | 79u8
                | 80u8 | 81u8 | 82u8 | 83u8 | 84u8 | 85u8 | 86u8 | 87u8 | 88u8 | 89u8
                | 90u8 | 95u8 | 97u8 | 98u8 | 99u8 | 100u8 | 101u8 | 102u8 | 103u8
                | 104u8 | 105u8 | 106u8 | 107u8 | 108u8 | 109u8 | 110u8 | 111u8 | 112u8
                | 113u8 | 114u8 | 115u8 | 116u8 | 117u8 | 118u8 | 119u8 | 120u8 | 121u8
                | 122u8 => {
                    {
                        let __chain_head80 = Self::__value_and(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        let __chain_depth79 = __BbnfBootstrapEnum_alloc(state)
                            .__s0()
                            .len();
                        loop {
                            let __chain_prev81 = state.offset;
                            match (|| {
                                let __chain_op82 = (|| {
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
                                let __chain_rhs83 = Self::__value_and(state)
                                    .map(|__v| {
                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                    })?;
                                Some((__chain_op82, __chain_rhs83))
                            })() {
                                Some(__value) => {
                                    let (__chain_op82, __chain_rhs83) = __value;
                                    __BbnfBootstrapEnum_alloc(state)
                                        .__s0()
                                        .push((__chain_op82, __chain_rhs83));
                                    if state.offset == __chain_prev81 {
                                        break;
                                    }
                                }
                                None => {
                                    state.offset = __chain_prev81;
                                    break;
                                }
                            }
                        }
                        Some((
                            __chain_head80,
                            __BbnfBootstrapEnum_alloc(state).__c0(__chain_depth79),
                        ))
                    }
                        .map(|__v| BbnfBootstrapEnum::value_or(__v))
                }
                _ => None,
            }
        } else {
            None
        }
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
    fn __value_unary<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            if state.offset < state.src.len() {
                match state.src.as_bytes()[state.offset] {
                    33u8 | 45u8 => {
                        ((|| {
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
                            let __sp84 = ::parse_that::Span::new(
                                __sp_start,
                                state.offset,
                                state.src,
                            );
                            let __v85 = Self::__value_atom(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            Some((__sp84, __v85))
                        })())
                            .map(|__sv| {
                                &*__BbnfBootstrapEnum_alloc(state)
                                    .slab()
                                    .alloc(BbnfBootstrapEnum::value_unary_0(__sv))
                            })
                    }
                    34u8 | 40u8 | 46u8 | 48u8 | 49u8 | 50u8 | 51u8 | 52u8 | 53u8 | 54u8
                    | 55u8 | 56u8 | 57u8 | 65u8 | 66u8 | 67u8 | 68u8 | 69u8 | 70u8 | 71u8
                    | 72u8 | 73u8 | 74u8 | 75u8 | 76u8 | 77u8 | 78u8 | 79u8 | 80u8 | 81u8
                    | 82u8 | 83u8 | 84u8 | 85u8 | 86u8 | 87u8 | 88u8 | 89u8 | 90u8 | 95u8
                    | 97u8 | 98u8 | 99u8 | 100u8 | 101u8 | 102u8 | 103u8 | 104u8 | 105u8
                    | 106u8 | 107u8 | 108u8 | 109u8 | 110u8 | 111u8 | 112u8 | 113u8
                    | 114u8 | 115u8 | 116u8 | 117u8 | 118u8 | 119u8 | 120u8 | 121u8
                    | 122u8 => {
                        Self::__value_atom(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })
                    }
                    _ => None,
                }
            } else {
                None
            }
        })()
            .map(|__x| BbnfBootstrapEnum::value_unary(__x))
    }
    pub fn value_unary<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__value_unary)
    }
    #[allow(non_snake_case)]
    fn __rule<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __sp_start = state.offset;
                ::parse_that::scan_ident(state)
                    .map(|__v| BbnfBootstrapEnum::identifier(__v))?;
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
                let __sp86 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                let __v87 = {
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
                let __sp88 = ::parse_that::Span::new(
                    __sp_start,
                    state.offset,
                    state.src,
                );
                Some((__sp86, __v87, __sp88))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::rule(__x))
    }
    pub fn rule<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__rule)
    }
    #[allow(non_snake_case)]
    fn __term<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                {
                    let __cp = state.offset;
                    let __result = (if state.src[state.offset..].starts_with("epsilon") {
                        let __start = state.offset;
                        state.offset += 7usize;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        None
                    })
                        .map(|__sv| {
                            &*__BbnfBootstrapEnum_alloc(state)
                                .slab()
                                .alloc(BbnfBootstrapEnum::pretty_directive_0(__sv))
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
                        ::parse_that::scan_ident(state)
                            .map(|__v| BbnfBootstrapEnum::identifier(__v))?;
                        let __sp97 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v98 = {
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
                                let __sp93 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                let __v94 = {
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ws_inner = Self::__rhs(state)
                                        .map(|__v| {
                                            &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                        });
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __ws_inner
                                }?;
                                let __v95 = {
                                    let __depth91 = __BbnfBootstrapEnum_alloc(state)
                                        .__s0()
                                        .len();
                                    loop {
                                        let __prev92 = state.offset;
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
                                            let __sp89 = ::parse_that::Span::new(
                                                __sp_start,
                                                state.offset,
                                                state.src,
                                            );
                                            let __v90 = {
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                let __ws_inner = Self::__rhs(state)
                                                    .map(|__v| {
                                                        &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                                    });
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __ws_inner
                                            }?;
                                            Some((__sp89, __v90))
                                        })() {
                                            Some(__value) => {
                                                __BbnfBootstrapEnum_alloc(state).__s0().push(__value);
                                                if state.offset == __prev92 {
                                                    break;
                                                }
                                            }
                                            None => {
                                                state.offset = __prev92;
                                                break;
                                            }
                                        }
                                    }
                                    Some(__BbnfBootstrapEnum_alloc(state).__c0(__depth91))
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
                                let __sp96 = ::parse_that::Span::new(
                                    __sp_start,
                                    state.offset,
                                    state.src,
                                );
                                Some((__sp93, __v94, __v95, __sp96))
                            })())() {
                                Some(__v) => Some(Some(__v)),
                                None => {
                                    state.offset = __cp;
                                    Some(None)
                                }
                            }
                        }?;
                        Some((__sp97, __v98))
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
                    let __result = (if state.src[state.offset..].starts_with("ε") {
                        let __start = state.offset;
                        state.offset += 2usize;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        None
                    })
                        .map(|__sv| {
                            &*__BbnfBootstrapEnum_alloc(state)
                                .slab()
                                .alloc(BbnfBootstrapEnum::pretty_directive_0(__sv))
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
                        let __sp99 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v100 = {
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
                        let __sp101 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp99, __v100, __sp101))
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
                        let __sp102 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v103 = {
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
                        let __sp104 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp102, __v103, __sp104))
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
                        let __sp105 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v106 = {
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
                        let __sp107 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp105, __v106, __sp107))
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
                    let __result = if state.offset < state.src.len() {
                        match state.src.as_bytes()[state.offset] {
                            34u8 => {
                                (|| {
                                    let __sp_start = state.offset;
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
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
                            }
                            39u8 => {
                                (|| {
                                    let __sp_start = state.offset;
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
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
                                })()
                            }
                            96u8 => {
                                (|| {
                                    let __sp_start = state.offset;
                                    {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
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
                                })()
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                        .map(|__inner| {
                            let __v = BbnfBootstrapEnum::literal(__inner);
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
            .map(|__x| BbnfBootstrapEnum::term(__x))
    }
    pub fn term<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__term)
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
                        let __sp108 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v109 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = ::parse_that::scan_ident(state)
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::identifier(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
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
                        let __sp111 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp108, __v109, __v110, __sp111))
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
                        let __sp114 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v115 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = if state.offset < state.src.len() {
                                match state.src.as_bytes()[state.offset] {
                                    42u8 => {
                                        ({
                                            let __start = state.offset;
                                            state.offset += 1;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        })
                                            .map(|__sv| {
                                                &*__BbnfBootstrapEnum_alloc(state)
                                                    .slab()
                                                    .alloc(BbnfBootstrapEnum::pretty_directive_0(__sv))
                                            })
                                    }
                                    65u8 | 66u8 | 67u8 | 68u8 | 69u8 | 70u8 | 71u8 | 72u8 | 73u8
                                    | 74u8 | 75u8 | 76u8 | 77u8 | 78u8 | 79u8 | 80u8 | 81u8
                                    | 82u8 | 83u8 | 84u8 | 85u8 | 86u8 | 87u8 | 88u8 | 89u8
                                    | 90u8 | 95u8 | 97u8 | 98u8 | 99u8 | 100u8 | 101u8 | 102u8
                                    | 103u8 | 104u8 | 105u8 | 106u8 | 107u8 | 108u8 | 109u8
                                    | 110u8 | 111u8 | 112u8 | 113u8 | 114u8 | 115u8 | 116u8
                                    | 117u8 | 118u8 | 119u8 | 120u8 | 121u8 | 122u8 => {
                                        ::parse_that::scan_ident(state)
                                            .map(|__inner| {
                                                let __v = BbnfBootstrapEnum::identifier(__inner);
                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                            })
                                    }
                                    _ => None,
                                }
                            } else {
                                None
                            };
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __v116 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = {
                                let __depth112 = __BbnfBootstrapEnum_alloc(state)
                                    .__s2()
                                    .len();
                                loop {
                                    let __prev113 = state.offset;
                                    match ::parse_that::scan_ident(state)
                                        .map(|__v| BbnfBootstrapEnum::identifier(__v))
                                    {
                                        Some(__value) => {
                                            __BbnfBootstrapEnum_alloc(state).__s2().push(__value);
                                            if state.offset == __prev113 {
                                                break;
                                            }
                                        }
                                        None => {
                                            state.offset = __prev113;
                                            break;
                                        }
                                    }
                                }
                                if (__BbnfBootstrapEnum_alloc(state).__s2().len()
                                    - __depth112) >= 1usize
                                {
                                    Some(__BbnfBootstrapEnum_alloc(state).__c2(__depth112))
                                } else {
                                    __BbnfBootstrapEnum_alloc(state)
                                        .__s2()
                                        .truncate(__depth112);
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
                        let __sp117 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp114, __v115, __v116, __sp117))
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
                        let __sp118 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v119 = {
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
                        let __sp120 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp118, __v119, __sp120))
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
                        let __sp121 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v122 = {
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
                        let __sp123 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp121, __v122, __sp123))
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
                        let __sp124 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v125 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = if state.offset < state.src.len() {
                                match state.src.as_bytes()[state.offset] {
                                    42u8 => {
                                        ({
                                            let __start = state.offset;
                                            state.offset += 1;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        })
                                            .map(|__sv| {
                                                &*__BbnfBootstrapEnum_alloc(state)
                                                    .slab()
                                                    .alloc(BbnfBootstrapEnum::pretty_directive_0(__sv))
                                            })
                                    }
                                    65u8 | 66u8 | 67u8 | 68u8 | 69u8 | 70u8 | 71u8 | 72u8 | 73u8
                                    | 74u8 | 75u8 | 76u8 | 77u8 | 78u8 | 79u8 | 80u8 | 81u8
                                    | 82u8 | 83u8 | 84u8 | 85u8 | 86u8 | 87u8 | 88u8 | 89u8
                                    | 90u8 | 95u8 | 97u8 | 98u8 | 99u8 | 100u8 | 101u8 | 102u8
                                    | 103u8 | 104u8 | 105u8 | 106u8 | 107u8 | 108u8 | 109u8
                                    | 110u8 | 111u8 | 112u8 | 113u8 | 114u8 | 115u8 | 116u8
                                    | 117u8 | 118u8 | 119u8 | 120u8 | 121u8 | 122u8 => {
                                        ::parse_that::scan_ident(state)
                                            .map(|__inner| {
                                                let __v = BbnfBootstrapEnum::identifier(__inner);
                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                            })
                                    }
                                    _ => None,
                                }
                            } else {
                                None
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
                        let __sp126 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp124, __v125, __sp126))
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
                        let __sp127 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v128 = {
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
                        let __sp129 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        Some((__sp127, __v128, __sp129))
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
    fn __factor<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __v130 = {
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
                let __v131 = {
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ws_inner = Self::__term(state)
                        .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v));
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __ws_inner
                }?;
                let __v132 = {
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
                let __v133 = {
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
                Some((__v130, __v131, __v132, __v133))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::factor(__x))
    }
    pub fn factor<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__factor)
    }
    #[allow(non_snake_case)]
    fn __mapped_factor<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            (|| {
                let __v140 = Self::__factor(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __v141 = {
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
                        let __sp138 = ::parse_that::Span::new(
                            __sp_start,
                            state.offset,
                            state.src,
                        );
                        let __v139 = (|| {
                            let __v136 = Self::__value_expr(state)
                                .map(|__v| {
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                })?;
                            let __v137 = {
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
                                        let __sp134 = ::parse_that::Span::new(
                                            __sp_start,
                                            state.offset,
                                            state.src,
                                        );
                                        let __v135 = Self::__type_name(state)
                                            .map(|__v| {
                                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                            })?;
                                        Some((__sp134, __v135))
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
                            Some((__v136, __v137))
                        })()?;
                        Some((__sp138, __v139))
                    })())() {
                        Some(__v) => Some(Some(__v)),
                        None => {
                            state.offset = __cp;
                            Some(None)
                        }
                    }
                }?;
                Some((__v140, __v141))
            })()
        })()
            .map(|__x| BbnfBootstrapEnum::mapped_factor(__x))
    }
    pub fn mapped_factor<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__mapped_factor)
    }
    #[allow(non_snake_case)]
    fn __binary_factor<'a>(
        state: &mut ::parse_that::ParserState<'a>,
    ) -> Option<BbnfBootstrapEnum<'a>> {
        (|| {
            {
                let __chain_head143 = Self::__mapped_factor(state)
                    .map(|__v| &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v))?;
                let __chain_depth142 = __BbnfBootstrapEnum_alloc(state).__s4().len();
                loop {
                    let __chain_prev144 = state.offset;
                    match (|| {
                        let __chain_op145 = {
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ws_inner = if state.offset < state.src.len() {
                                match state.src.as_bytes()[state.offset] {
                                    60u8 => {
                                        if state.src[state.offset..].starts_with("<<") {
                                            let __start = state.offset;
                                            state.offset += 2usize;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        }
                                    }
                                    62u8 => {
                                        if state.src[state.offset..].starts_with(">>") {
                                            let __start = state.offset;
                                            state.offset += 2usize;
                                            Some(
                                                ::parse_that::Span::new(__start, state.offset, state.src),
                                            )
                                        } else {
                                            None
                                        }
                                    }
                                    45u8 => {
                                        let __start = state.offset;
                                        state.offset += 1;
                                        Some(
                                            ::parse_that::Span::new(__start, state.offset, state.src),
                                        )
                                    }
                                    _ => None,
                                }
                            } else {
                                None
                            }
                                .map(|__inner| {
                                    let __v = BbnfBootstrapEnum::binary_operators(__inner);
                                    &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                                });
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __ws_inner
                        }?;
                        let __chain_rhs146 = Self::__mapped_factor(state)
                            .map(|__v| {
                                &*__BbnfBootstrapEnum_alloc(state).slab().alloc(__v)
                            })?;
                        Some((__chain_op145, __chain_rhs146))
                    })() {
                        Some(__value) => {
                            let (__chain_op145, __chain_rhs146) = __value;
                            __BbnfBootstrapEnum_alloc(state)
                                .__s4()
                                .push((__chain_op145, __chain_rhs146));
                            if state.offset == __chain_prev144 {
                                break;
                            }
                        }
                        None => {
                            state.offset = __chain_prev144;
                            break;
                        }
                    }
                }
                Some((
                    __chain_head143,
                    __BbnfBootstrapEnum_alloc(state).__c4(__chain_depth142),
                ))
            }
        })()
            .map(|__x| BbnfBootstrapEnum::binary_factor(__x))
    }
    pub fn binary_factor<'a>() -> Parser<'a, BbnfBootstrapEnum<'a>> {
        Parser::new(Self::__binary_factor)
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
                        state.offset = __pretty_cp5;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp4 = state.offset;
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
                                state.offset = __pretty_cp4;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp3 = state.offset;
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
                                        state.offset = __pretty_cp3;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp2 = state.offset;
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
                                                state.offset = __pretty_cp2;
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp1 = state.offset;
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
                                                        state.offset = __pretty_cp1;
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp0 = state.offset;
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
                    let mut __rep_count8 = 0usize;
                    while __rep_count8 < 4294967295 {
                        let __rep_cp9 = state.offset;
                        if !{
                            let __pretty_cp6 = state.offset;
                            let __pretty_bcp7 = __builder.checkpoint();
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
                                state.offset = __pretty_cp6;
                                __builder.restore(__pretty_bcp7);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp9;
                            break;
                        }
                        if state.offset == __rep_cp9 {
                            break;
                        }
                        __rep_count8 += 1;
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
                    let __end = state.src_bytes.len();
                    let mut __pos = __start;
                    while __pos < __end {
                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                        if (__b >= b'0' && __b <= b'9') || __b == b']' || __b == b'*'
                            || __b == b'.' || __b == b'[' || (__b >= b'0' && __b <= b'9')
                        {
                            __pos += 1;
                        } else {
                            break;
                        }
                    }
                    if __pos >= __start + 1 {
                        state.offset = __pos;
                        Some(::parse_that::Span::new(__start, __pos, state.src))
                    } else {
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
                if ::parse_that::number_span_scan_strict(state).is_none() {
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
                let __byte = match state.src_bytes.get(state.offset) {
                    Some(&b) => b,
                    None => return false,
                };
                match __byte {
                    b'f' => {
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
                    b't' => {
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
                    }
                    _ => {
                        return false;
                    }
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
    fn __concatenation_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let __rep_start20 = state.offset;
                let __rep_bcp21 = __builder.checkpoint();
                let mut __rep_count18 = 0usize;
                while __rep_count18 < 4294967295 {
                    let __rep_cp19 = state.offset;
                    if !{
                        let __pretty_cp16 = state.offset;
                        let __pretty_bcp17 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    if !{
                                        let __pretty_cp12 = state.offset;
                                        let __pretty_bcp13 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                let __ows10 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder.text_inline_ws(&state.src[__ows10..state.offset]);
                                                if !Self::__binary_factor_prettify(state, __builder) {
                                                    return false;
                                                }
                                                let __ows11 = state.offset;
                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                __builder.text_inline_ws(&state.src[__ows11..state.offset]);
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp12;
                                            __builder.restore(__pretty_bcp13);
                                        }
                                        __ok
                                    } {
                                        return false;
                                    }
                                };
                                {
                                    let _ = {
                                        let __pretty_cp14 = state.offset;
                                        let __pretty_bcp15 = __builder.checkpoint();
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
                                            state.offset = __pretty_cp14;
                                            __builder.restore(__pretty_bcp15);
                                        }
                                        __ok
                                    };
                                    true
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp16;
                            __builder.restore(__pretty_bcp17);
                        }
                        __ok
                    } {
                        state.offset = __rep_cp19;
                        break;
                    }
                    if state.offset == __rep_cp19 {
                        break;
                    }
                    __rep_count18 += 1;
                }
                if __rep_count18 < 1 {
                    state.offset = __rep_start20;
                    __builder.restore(__rep_bcp21);
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
    fn __binary_operators_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let __byte = match state.src_bytes.get(state.offset) {
                    Some(&b) => b,
                    None => return false,
                };
                match __byte {
                    b'<' => {
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
                    }
                    b'>' => {
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
                    }
                    b'-' => {
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'-') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'-');
                        };
                    }
                    _ => {
                        return false;
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
    fn __alternation_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        __builder.group_open();
        let __pretty_ok = {
            {
                {
                    let __rep_start32 = state.offset;
                    let __rep_bcp33 = __builder.checkpoint();
                    let mut __rep_count30 = 0usize;
                    while __rep_count30 < 4294967295 {
                        let __rep_cp31 = state.offset;
                        if !{
                            let __pretty_cp28 = state.offset;
                            let __pretty_bcp29 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp24 = state.offset;
                                            let __pretty_bcp25 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows22 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder.text_inline_ws(&state.src[__ows22..state.offset]);
                                                    if !Self::__concatenation_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows23 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder.text_inline_ws(&state.src[__ows23..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp24;
                                                __builder.restore(__pretty_bcp25);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                    {
                                        let _ = {
                                            let __pretty_cp26 = state.offset;
                                            let __pretty_bcp27 = __builder.checkpoint();
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
                                                state.offset = __pretty_cp26;
                                                __builder.restore(__pretty_bcp27);
                                            }
                                            __ok
                                        };
                                        true
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp28;
                                __builder.restore(__pretty_bcp29);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp31;
                            break;
                        }
                        if state.offset == __rep_cp31 {
                            break;
                        }
                        __rep_count30 += 1;
                    }
                    if __rep_count30 < 1 {
                        state.offset = __rep_start32;
                        __builder.restore(__rep_bcp33);
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
                let __byte = match state.src_bytes.get(state.offset) {
                    Some(&b) => b,
                    None => return false,
                };
                match __byte {
                    b'"' => {
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
                    }
                    b'\'' => {
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
                    }
                    b'`' => {
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
                    }
                    _ => {
                        return false;
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
                    let __pretty_cp37 = state.offset;
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
                        state.offset = __pretty_cp37;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp36 = state.offset;
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
                                state.offset = __pretty_cp36;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp35 = state.offset;
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
                                        state.offset = __pretty_cp35;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp34 = state.offset;
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
                                                state.offset = __pretty_cp34;
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
                    let __pretty_cp40 = state.offset;
                    let __pretty_bcp41 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            let __ows38 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows38..state.offset]);
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
    fn __pretty_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows42 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows43 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows42..__ows43]);
                    let __ows44 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows44..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp47 = state.offset;
                        let __pretty_bcp48 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows45 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows45..state.offset]);
                                {
                                    let __byte = match state.src_bytes.get(state.offset) {
                                        Some(&b) => b,
                                        None => return false,
                                    };
                                    match __byte {
                                        b'*' => {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'*');
                                            };
                                        }
                                        b'A' | b'B' | b'C' | b'D' | b'E' | b'F' | b'G' | b'H' | b'I'
                                        | b'J' | b'K' | b'L' | b'M' | b'N' | b'O' | b'P' | b'Q'
                                        | b'R' | b'S' | b'T' | b'U' | b'V' | b'W' | b'X' | b'Y'
                                        | b'Z' | b'_' | b'a' | b'b' | b'c' | b'd' | b'e' | b'f'
                                        | b'g' | b'h' | b'i' | b'j' | b'k' | b'l' | b'm' | b'n'
                                        | b'o' | b'p' | b'q' | b'r' | b's' | b't' | b'u' | b'v'
                                        | b'w' | b'x' | b'y' | b'z' => {
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
                                        _ => {
                                            return false;
                                        }
                                    }
                                };
                                let __ows46 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows46..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp47;
                            __builder.restore(__pretty_bcp48);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    if !{
                        let __pretty_cp57 = state.offset;
                        let __pretty_bcp58 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows55 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows55..state.offset]);
                                {
                                    let __rep_start53 = state.offset;
                                    let __rep_bcp54 = __builder.checkpoint();
                                    let mut __rep_count51 = 0usize;
                                    while __rep_count51 < 4294967295 {
                                        let __rep_cp52 = state.offset;
                                        if !{
                                            let __pretty_cp49 = state.offset;
                                            let __pretty_bcp50 = __builder.checkpoint();
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
                                                state.offset = __pretty_cp49;
                                                __builder.restore(__pretty_bcp50);
                                            }
                                            __ok
                                        } {
                                            state.offset = __rep_cp52;
                                            break;
                                        }
                                        if state.offset == __rep_cp52 {
                                            break;
                                        }
                                        __rep_count51 += 1;
                                    }
                                    if __rep_count51 < 1 {
                                        state.offset = __rep_start53;
                                        __builder.restore(__rep_bcp54);
                                        return false;
                                    }
                                };
                                let __ows56 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows56..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp57;
                            __builder.restore(__pretty_bcp58);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp59 = state.offset;
                        let __pretty_bcp60 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __byte = match state.src_bytes.get(state.offset) {
                                    Some(&b) => b,
                                    None => return false,
                                };
                                match __byte {
                                    b';' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                    }
                                    b'.' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                    _ => {
                                        return false;
                                    }
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp59;
                            __builder.restore(__pretty_bcp60);
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
    fn __mul_op_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let __byte = match state.src_bytes.get(state.offset) {
                    Some(&b) => b,
                    None => return false,
                };
                match __byte {
                    b'*' => {
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'*') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'*');
                        };
                    }
                    b'/' => {
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'/') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'/');
                        };
                    }
                    b'%' => {
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'%') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'%');
                        };
                    }
                    _ => {
                        return false;
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
    fn __comment_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp63 = state.offset;
                    let __pretty_bcp64 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            let __ows61 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows61..state.offset]);
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
                            let __ows62 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows62..state.offset]);
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp63;
                        __builder.restore(__pretty_bcp64);
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
    fn __add_op_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let __byte = match state.src_bytes.get(state.offset) {
                    Some(&b) => b,
                    None => return false,
                };
                match __byte {
                    b'+' => {
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'+') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'+');
                        };
                    }
                    b'-' => {
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'-') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'-');
                        };
                    }
                    _ => {
                        return false;
                    }
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
    fn __type_name_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp71 = state.offset;
                    let __pretty_bcp72 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        {
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'u')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'u');
                            };
                            {
                                let __byte = match state.src_bytes.get(state.offset) {
                                    Some(&b) => b,
                                    None => return false,
                                };
                                match __byte {
                                    b'1' => {
                                        {
                                            let __s = "16";
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
                                    }
                                    b'3' => {
                                        {
                                            let __s = "32";
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
                                    }
                                    b'6' => {
                                        {
                                            let __s = "64";
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
                                    }
                                    b'8' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'8')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'8');
                                        };
                                    }
                                    _ => {
                                        return false;
                                    }
                                }
                            };
                        };
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp71;
                        __builder.restore(__pretty_bcp72);
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp70 = state.offset;
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
                                state.offset = __pretty_cp70;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp69 = state.offset;
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
                                        state.offset = __pretty_cp69;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp67 = state.offset;
                                            let __pretty_bcp68 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    {
                                                        if state.src_bytes.get(state.offset).copied() != Some(b'i')
                                                        {
                                                            return false;
                                                        }
                                                        state.offset += 1;
                                                        __builder.char(b'i');
                                                    };
                                                    {
                                                        let __byte = match state.src_bytes.get(state.offset) {
                                                            Some(&b) => b,
                                                            None => return false,
                                                        };
                                                        match __byte {
                                                            b'3' => {
                                                                {
                                                                    let __s = "32";
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
                                                            }
                                                            b'6' => {
                                                                {
                                                                    let __s = "64";
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
                                                            }
                                                            _ => {
                                                                return false;
                                                            }
                                                        }
                                                    };
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp67;
                                                __builder.restore(__pretty_bcp68);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp65 = state.offset;
                                                    let __pretty_bcp66 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        {
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b'f')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b'f');
                                                            };
                                                            {
                                                                let __byte = match state.src_bytes.get(state.offset) {
                                                                    Some(&b) => b,
                                                                    None => return false,
                                                                };
                                                                match __byte {
                                                                    b'3' => {
                                                                        {
                                                                            let __s = "32";
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
                                                                    }
                                                                    b'6' => {
                                                                        {
                                                                            let __s = "64";
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
                                                                    }
                                                                    _ => {
                                                                        return false;
                                                                    }
                                                                }
                                                            };
                                                        };
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp65;
                                                        __builder.restore(__pretty_bcp66);
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
    fn __debug_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows73 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows74 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows73..__ows74]);
                    let __ows75 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows75..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp78 = state.offset;
                        let __pretty_bcp79 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows76 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows76..state.offset]);
                                {
                                    let __byte = match state.src_bytes.get(state.offset) {
                                        Some(&b) => b,
                                        None => return false,
                                    };
                                    match __byte {
                                        b'*' => {
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b'*')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b'*');
                                            };
                                        }
                                        b'A' | b'B' | b'C' | b'D' | b'E' | b'F' | b'G' | b'H' | b'I'
                                        | b'J' | b'K' | b'L' | b'M' | b'N' | b'O' | b'P' | b'Q'
                                        | b'R' | b'S' | b'T' | b'U' | b'V' | b'W' | b'X' | b'Y'
                                        | b'Z' | b'_' | b'a' | b'b' | b'c' | b'd' | b'e' | b'f'
                                        | b'g' | b'h' | b'i' | b'j' | b'k' | b'l' | b'm' | b'n'
                                        | b'o' | b'p' | b'q' | b'r' | b's' | b't' | b'u' | b'v'
                                        | b'w' | b'x' | b'y' | b'z' => {
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
                                        _ => {
                                            return false;
                                        }
                                    }
                                };
                                let __ows77 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows77..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp78;
                            __builder.restore(__pretty_bcp79);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp80 = state.offset;
                        let __pretty_bcp81 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __byte = match state.src_bytes.get(state.offset) {
                                    Some(&b) => b,
                                    None => return false,
                                };
                                match __byte {
                                    b';' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                    }
                                    b'.' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                    _ => {
                                        return false;
                                    }
                                }
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
    fn __import_directive_prettify<'a>(
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
                    __builder.text_inline_ws(&state.src[__ows82..__ows83]);
                    let __ows84 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows84..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp92 = state.offset;
                        let __pretty_bcp93 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__import_path_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp92;
                            __builder.restore(__pretty_bcp93);
                        }
                        __ok
                    } {
                        {
                            {
                                if !{
                                    let __pretty_cp87 = state.offset;
                                    let __pretty_bcp88 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            let __ows85 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows85..state.offset]);
                                            if !Self::__import_items_prettify(state, __builder) {
                                                return false;
                                            }
                                            let __ows86 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows86..state.offset]);
                                        };
                                        true
                                    })();
                                    if !__ok {
                                        state.offset = __pretty_cp87;
                                        __builder.restore(__pretty_bcp88);
                                    }
                                    __ok
                                } {
                                    return false;
                                }
                            };
                            {
                                let __ows89 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                let __ows90 = state.offset;
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
                                __builder.text_inline_ws(&state.src[__ows89..__ows90]);
                                let __ows91 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder.text_inline_ws(&state.src[__ows91..state.offset]);
                            };
                            if !Self::__import_path_prettify(state, __builder) {
                                return false;
                            }
                        };
                    }
                };
                {
                    let _ = {
                        let __pretty_cp94 = state.offset;
                        let __pretty_bcp95 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __byte = match state.src_bytes.get(state.offset) {
                                    Some(&b) => b,
                                    None => return false,
                                };
                                match __byte {
                                    b';' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                    }
                                    b'.' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                    _ => {
                                        return false;
                                    }
                                }
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
    fn __grammar_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let mut __rep_count107 = 0usize;
                while __rep_count107 < 4294967295 {
                    let __rep_cp108 = state.offset;
                    let __iter_cp = if __rep_count107 > 0 {
                        Some(__builder.checkpoint())
                    } else {
                        None
                    };
                    if __rep_count107 > 0 {
                        __builder.hardline();
                    }
                    if !{
                        let __pretty_cp106 = state.offset;
                        let __ok = (|| -> bool {
                            {
                                {
                                    let _ = {
                                        let __pretty_cp96 = state.offset;
                                        let __pretty_bcp97 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            if !Self::__comment_prettify(state, __builder) {
                                                return false;
                                            }
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp96;
                                            __builder.restore(__pretty_bcp97);
                                        }
                                        __ok
                                    };
                                    true
                                };
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
                                                {
                                                    if !{
                                                        let __pretty_cp98 = state.offset;
                                                        let __pretty_bcp99 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            if !Self::__directive_prettify(state, __builder) {
                                                                return false;
                                                            }
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp98;
                                                            __builder.restore(__pretty_bcp99);
                                                        }
                                                        __ok
                                                    } {
                                                        if !Self::__rule_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                    }
                                                };
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
                                {
                                    let _ = {
                                        let __pretty_cp104 = state.offset;
                                        let __pretty_bcp105 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            if !Self::__comment_prettify(state, __builder) {
                                                return false;
                                            }
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp104;
                                            __builder.restore(__pretty_bcp105);
                                        }
                                        __ok
                                    };
                                    true
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp106;
                        }
                        __ok
                    } {
                        state.offset = __rep_cp108;
                        if let Some(__bcp) = __iter_cp {
                            __builder.restore(__bcp);
                        }
                        break;
                    }
                    if state.offset == __rep_cp108 {
                        break;
                    }
                    __rep_count107 += 1;
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
    #[allow(non_snake_case)]
    fn __ws_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows109 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows110 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows109..__ows110]);
                    let __ows111 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows111..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp114 = state.offset;
                        let __pretty_bcp115 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows112 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows112..state.offset]);
                                if !Self::__regex_prettify(state, __builder) {
                                    return false;
                                }
                                let __ows113 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows113..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp114;
                            __builder.restore(__pretty_bcp115);
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
                                let __byte = match state.src_bytes.get(state.offset) {
                                    Some(&b) => b,
                                    None => return false,
                                };
                                match __byte {
                                    b';' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                    }
                                    b'.' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                    _ => {
                                        return false;
                                    }
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
                        let __pretty_cp125 = state.offset;
                        let __pretty_bcp126 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                if !Self::__value_expr_prettify(state, __builder) {
                                    return false;
                                }
                                {
                                    let mut __rep_count123 = 0usize;
                                    while __rep_count123 < 4294967295 {
                                        let __rep_cp124 = state.offset;
                                        if !{
                                            let __pretty_cp121 = state.offset;
                                            let __pretty_bcp122 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    {
                                                        let __ows118 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        let __ows119 = state.offset;
                                                        {
                                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                            {
                                                                return false;
                                                            }
                                                            state.offset += 1;
                                                            __builder.char(b',');
                                                        };
                                                        __builder.text_inline_ws(&state.src[__ows118..__ows119]);
                                                        let __ows120 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows120..state.offset]);
                                                    };
                                                    if !Self::__value_expr_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp121;
                                                __builder.restore(__pretty_bcp122);
                                            }
                                            __ok
                                        } {
                                            state.offset = __rep_cp124;
                                            break;
                                        }
                                        if state.offset == __rep_cp124 {
                                            break;
                                        }
                                        __rep_count123 += 1;
                                    }
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp125;
                            __builder.restore(__pretty_bcp126);
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
    fn __import_items_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows127 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows128 = state.offset;
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'{') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'{');
                    };
                    __builder.text_inline_ws(&state.src[__ows127..__ows128]);
                    let __ows129 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows129..state.offset]);
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
                    let mut __rep_count135 = 0usize;
                    while __rep_count135 < 4294967295 {
                        let __rep_cp136 = state.offset;
                        if !{
                            let __pretty_cp133 = state.offset;
                            let __pretty_bcp134 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows130 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows131 = state.offset;
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b',');
                                        };
                                        __builder.text_inline_ws(&state.src[__ows130..__ows131]);
                                        let __ows132 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows132..state.offset]);
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
                                state.offset = __pretty_cp133;
                                __builder.restore(__pretty_bcp134);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp136;
                            break;
                        }
                        if state.offset == __rep_cp136 {
                            break;
                        }
                        __rep_count135 += 1;
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
    fn __host_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows137 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows138 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows137..__ows138]);
                    let __ows139 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows139..state.offset]);
                };
                {
                    let __ows140 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows141 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows140..__ows141]);
                    let __ows142 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows142..state.offset]);
                };
                {
                    let _ = {
                        let __pretty_cp143 = state.offset;
                        let __pretty_bcp144 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __byte = match state.src_bytes.get(state.offset) {
                                    Some(&b) => b,
                                    None => return false,
                                };
                                match __byte {
                                    b';' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                    }
                                    b'.' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                    _ => {
                                        return false;
                                    }
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp143;
                            __builder.restore(__pretty_bcp144);
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
                    let __ows145 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows146 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows145..__ows146]);
                    let __ows147 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows147..state.offset]);
                };
                {
                    let __ows148 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows149 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows148..__ows149]);
                    let __ows150 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows150..state.offset]);
                };
                {
                    let _ = {
                        let __pretty_cp151 = state.offset;
                        let __pretty_bcp152 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __byte = match state.src_bytes.get(state.offset) {
                                    Some(&b) => b,
                                    None => return false,
                                };
                                match __byte {
                                    b';' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                    }
                                    b'.' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                    _ => {
                                        return false;
                                    }
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp151;
                            __builder.restore(__pretty_bcp152);
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
                    let mut __rep_count158 = 0usize;
                    while __rep_count158 < 4294967295 {
                        let __rep_cp159 = state.offset;
                        if !{
                            let __pretty_cp156 = state.offset;
                            let __pretty_bcp157 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows153 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows154 = state.offset;
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b',');
                                        };
                                        __builder.text_inline_ws(&state.src[__ows153..__ows154]);
                                        let __ows155 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows155..state.offset]);
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
                                state.offset = __pretty_cp156;
                                __builder.restore(__pretty_bcp157);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp159;
                            break;
                        }
                        if state.offset == __rep_cp159 {
                            break;
                        }
                        __rep_count158 += 1;
                    }
                };
                {
                    let __ows160 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows161 = state.offset;
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b'|') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b'|');
                    };
                    __builder.text_inline_ws(&state.src[__ows160..__ows161]);
                    let __ows162 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows162..state.offset]);
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
    fn __rhs_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp163 = state.offset;
                    let __pretty_bcp164 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        if !Self::__closure_prettify(state, __builder) {
                            return false;
                        }
                        true
                    })();
                    if !__ok {
                        state.offset = __pretty_cp163;
                        __builder.restore(__pretty_bcp164);
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
    fn __type_annotation_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows165 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows166 = state.offset;
                    {
                        if state.src_bytes.get(state.offset).copied() != Some(b':') {
                            return false;
                        }
                        state.offset += 1;
                        __builder.char(b':');
                    };
                    __builder.text_inline_ws(&state.src[__ows165..__ows166]);
                    let __ows167 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows167..state.offset]);
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
                    let mut __rep_count174 = 0usize;
                    while __rep_count174 < 4294967295 {
                        let __rep_cp175 = state.offset;
                        if !{
                            let __pretty_cp172 = state.offset;
                            let __pretty_bcp173 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp170 = state.offset;
                                            let __pretty_bcp171 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows168 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows168..state.offset]);
                                                    if !Self::__mul_op_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows169 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows169..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp170;
                                                __builder.restore(__pretty_bcp171);
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
                                state.offset = __pretty_cp172;
                                __builder.restore(__pretty_bcp173);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp175;
                            break;
                        }
                        if state.offset == __rep_cp175 {
                            break;
                        }
                        __rep_count174 += 1;
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
                    let mut __rep_count182 = 0usize;
                    while __rep_count182 < 4294967295 {
                        let __rep_cp183 = state.offset;
                        if !{
                            let __pretty_cp180 = state.offset;
                            let __pretty_bcp181 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
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
                                                    {
                                                        let __byte = match state.src_bytes.get(state.offset) {
                                                            Some(&b) => b,
                                                            None => return false,
                                                        };
                                                        match __byte {
                                                            b'+' => {
                                                                {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'+')
                                                                    {
                                                                        return false;
                                                                    }
                                                                    state.offset += 1;
                                                                    __builder.char(b'+');
                                                                };
                                                            }
                                                            b'-' => {
                                                                {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                                                    {
                                                                        return false;
                                                                    }
                                                                    state.offset += 1;
                                                                    __builder.char(b'-');
                                                                };
                                                            }
                                                            _ => {
                                                                return false;
                                                            }
                                                        }
                                                    };
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
                                    if !Self::__value_mul_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp180;
                                __builder.restore(__pretty_bcp181);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp183;
                            break;
                        }
                        if state.offset == __rep_cp183 {
                            break;
                        }
                        __rep_count182 += 1;
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
                    let mut __rep_count190 = 0usize;
                    while __rep_count190 < 4294967295 {
                        let __rep_cp191 = state.offset;
                        if !{
                            let __pretty_cp188 = state.offset;
                            let __pretty_bcp189 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp186 = state.offset;
                                            let __pretty_bcp187 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows184 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows184..state.offset]);
                                                    if !Self::__cmp_op_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows185 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows185..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp186;
                                                __builder.restore(__pretty_bcp187);
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
                                state.offset = __pretty_cp188;
                                __builder.restore(__pretty_bcp189);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp191;
                            break;
                        }
                        if state.offset == __rep_cp191 {
                            break;
                        }
                        __rep_count190 += 1;
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
                    let mut __rep_count197 = 0usize;
                    while __rep_count197 < 4294967295 {
                        let __rep_cp198 = state.offset;
                        if !{
                            let __pretty_cp195 = state.offset;
                            let __pretty_bcp196 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows192 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows193 = state.offset;
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
                                        __builder.text_inline_ws(&state.src[__ows192..__ows193]);
                                        let __ows194 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows194..state.offset]);
                                    };
                                    if !Self::__value_cmp_prettify(state, __builder) {
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
                            state.offset = __rep_cp198;
                            break;
                        }
                        if state.offset == __rep_cp198 {
                            break;
                        }
                        __rep_count197 += 1;
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
                    let mut __rep_count204 = 0usize;
                    while __rep_count204 < 4294967295 {
                        let __rep_cp205 = state.offset;
                        if !{
                            let __pretty_cp202 = state.offset;
                            let __pretty_bcp203 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows199 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows200 = state.offset;
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
                                        __builder.text_inline_ws(&state.src[__ows199..__ows200]);
                                        let __ows201 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows201..state.offset]);
                                    };
                                    if !Self::__value_and_prettify(state, __builder) {
                                        return false;
                                    }
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp202;
                                __builder.restore(__pretty_bcp203);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp205;
                            break;
                        }
                        if state.offset == __rep_cp205 {
                            break;
                        }
                        __rep_count204 += 1;
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
                    let mut __rep_count211 = 0usize;
                    while __rep_count211 < 4294967295 {
                        let __rep_cp212 = state.offset;
                        if !{
                            let __pretty_cp209 = state.offset;
                            let __pretty_bcp210 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows206 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows207 = state.offset;
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b',');
                                        };
                                        __builder.text_inline_ws(&state.src[__ows206..__ows207]);
                                        let __ows208 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows208..state.offset]);
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
                                state.offset = __pretty_cp209;
                                __builder.restore(__pretty_bcp210);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp212;
                            break;
                        }
                        if state.offset == __rep_cp212 {
                            break;
                        }
                        __rep_count211 += 1;
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
    fn __value_atom_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp229 = state.offset;
                    let __ok = (|| -> bool {
                        {
                            let __start = state.offset;
                            if ::parse_that::number_span_scan_strict(state).is_none() {
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
                        state.offset = __pretty_cp229;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp228 = state.offset;
                            let __ok = (|| -> bool {
                                {
                                    let __start = state.offset;
                                    if {
                                        let __start = state.offset;
                                        let __end = state.src_bytes.len();
                                        let mut __pos = __start;
                                        while __pos < __end {
                                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                                            if (__b >= b'0' && __b <= b'9') || __b == b']'
                                                || __b == b'*' || __b == b'.' || __b == b'['
                                                || (__b >= b'0' && __b <= b'9')
                                            {
                                                __pos += 1;
                                            } else {
                                                break;
                                            }
                                        }
                                        if __pos >= __start + 1 {
                                            state.offset = __pos;
                                            Some(::parse_that::Span::new(__start, __pos, state.src))
                                        } else {
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
                                state.offset = __pretty_cp228;
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp226 = state.offset;
                                    let __pretty_bcp227 = __builder.checkpoint();
                                    let __ok = (|| -> bool {
                                        {
                                            let __byte = match state.src_bytes.get(state.offset) {
                                                Some(&b) => b,
                                                None => return false,
                                            };
                                            match __byte {
                                                b'f' => {
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
                                                b't' => {
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
                                                }
                                                _ => {
                                                    return false;
                                                }
                                            }
                                        };
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
                                                if !Self::__value_fn_call_prettify(state, __builder) {
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
                                                        if !Self::__value_input_prettify(state, __builder) {
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
                                                            let __pretty_cp221 = state.offset;
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
                                                                state.offset = __pretty_cp221;
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp219 = state.offset;
                                                                    let __pretty_bcp220 = __builder.checkpoint();
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
                                                                                    let __pretty_cp215 = state.offset;
                                                                                    let __pretty_bcp216 = __builder.checkpoint();
                                                                                    let __ok = (|| -> bool {
                                                                                        {
                                                                                            let __ows213 = state.offset;
                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                            __builder
                                                                                                .text_inline_ws(&state.src[__ows213..state.offset]);
                                                                                            if !Self::__value_expr_prettify(state, __builder) {
                                                                                                return false;
                                                                                            }
                                                                                            let __ows214 = state.offset;
                                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                                            __builder
                                                                                                .text_inline_ws(&state.src[__ows214..state.offset]);
                                                                                        };
                                                                                        true
                                                                                    })();
                                                                                    if !__ok {
                                                                                        state.offset = __pretty_cp215;
                                                                                        __builder.restore(__pretty_bcp216);
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
                                                                        state.offset = __pretty_cp219;
                                                                        __builder.restore(__pretty_bcp220);
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp217 = state.offset;
                                                                            let __pretty_bcp218 = __builder.checkpoint();
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
                                                                                state.offset = __pretty_cp217;
                                                                                __builder.restore(__pretty_bcp218);
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
    fn __value_expr_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let __byte = match state.src_bytes.get(state.offset) {
                    Some(&b) => b,
                    None => return false,
                };
                match __byte {
                    b'|' => {
                        if !Self::__value_closure_prettify(state, __builder) {
                            return false;
                        }
                    }
                    b'!' | b'"' | b'(' | b'-' | b'.' | b'0' | b'1' | b'2' | b'3' | b'4'
                    | b'5' | b'6' | b'7' | b'8' | b'9' | b'A' | b'B' | b'C' | b'D' | b'E'
                    | b'F' | b'G' | b'H' | b'I' | b'J' | b'K' | b'L' | b'M' | b'N' | b'O'
                    | b'P' | b'Q' | b'R' | b'S' | b'T' | b'U' | b'V' | b'W' | b'X' | b'Y'
                    | b'Z' | b'_' | b'a' | b'b' | b'c' | b'd' | b'e' | b'f' | b'g' | b'h'
                    | b'i' | b'j' | b'k' | b'l' | b'm' | b'n' | b'o' | b'p' | b'q' | b'r'
                    | b's' | b't' | b'u' | b'v' | b'w' | b'x' | b'y' | b'z' => {
                        if !Self::__value_or_prettify(state, __builder) {
                            return false;
                        }
                    }
                    _ => {
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
    fn __value_unary_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                let __byte = match state.src_bytes.get(state.offset) {
                    Some(&b) => b,
                    None => return false,
                };
                match __byte {
                    b'!' | b'-' => {
                        {
                            {
                                let __byte = match state.src_bytes.get(state.offset) {
                                    Some(&b) => b,
                                    None => return false,
                                };
                                match __byte {
                                    b'!' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'!')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'!');
                                        };
                                    }
                                    b'-' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'-')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'-');
                                        };
                                    }
                                    _ => {
                                        return false;
                                    }
                                }
                            };
                            if !Self::__value_atom_prettify(state, __builder) {
                                return false;
                            }
                        };
                    }
                    b'"' | b'(' | b'.' | b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6'
                    | b'7' | b'8' | b'9' | b'A' | b'B' | b'C' | b'D' | b'E' | b'F' | b'G'
                    | b'H' | b'I' | b'J' | b'K' | b'L' | b'M' | b'N' | b'O' | b'P' | b'Q'
                    | b'R' | b'S' | b'T' | b'U' | b'V' | b'W' | b'X' | b'Y' | b'Z' | b'_'
                    | b'a' | b'b' | b'c' | b'd' | b'e' | b'f' | b'g' | b'h' | b'i' | b'j'
                    | b'k' | b'l' | b'm' | b'n' | b'o' | b'p' | b'q' | b'r' | b's' | b't'
                    | b'u' | b'v' | b'w' | b'x' | b'y' | b'z' => {
                        if !Self::__value_atom_prettify(state, __builder) {
                            return false;
                        }
                    }
                    _ => {
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
    fn __recover_directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let __ows230 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows231 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows230..__ows231]);
                    let __ows232 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows232..state.offset]);
                };
                {
                    let __ows233 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let __ows234 = state.offset;
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
                    __builder.text_inline_ws(&state.src[__ows233..__ows234]);
                    let __ows235 = state.offset;
                    ::parse_that::trim_leading_whitespace_mut(state);
                    __builder.text_inline_ws(&state.src[__ows235..state.offset]);
                };
                {
                    if !{
                        let __pretty_cp238 = state.offset;
                        let __pretty_bcp239 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __ows236 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows236..state.offset]);
                                if !Self::__rhs_prettify(state, __builder) {
                                    return false;
                                }
                                let __ows237 = state.offset;
                                ::parse_that::trim_leading_whitespace_mut(state);
                                __builder
                                    .text_inline_ws(&state.src[__ows237..state.offset]);
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp238;
                            __builder.restore(__pretty_bcp239);
                        }
                        __ok
                    } {
                        return false;
                    }
                };
                {
                    let _ = {
                        let __pretty_cp240 = state.offset;
                        let __pretty_bcp241 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                let __byte = match state.src_bytes.get(state.offset) {
                                    Some(&b) => b,
                                    None => return false,
                                };
                                match __byte {
                                    b';' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b';')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b';');
                                        };
                                    }
                                    b'.' => {
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b'.');
                                        };
                                    }
                                    _ => {
                                        return false;
                                    }
                                }
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp240;
                            __builder.restore(__pretty_bcp241);
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
                        let __ows242 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows243 = state.offset;
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'=') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'=');
                        };
                        __builder.text_inline_ws(&state.src[__ows242..__ows243]);
                        let __ows244 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows244..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp247 = state.offset;
                            let __pretty_bcp248 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows245 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows245..state.offset]);
                                    if !Self::__rhs_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows246 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows246..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp247;
                                __builder.restore(__pretty_bcp248);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let __byte = match state.src_bytes.get(state.offset) {
                            Some(&b) => b,
                            None => return false,
                        };
                        match __byte {
                            b';' => {
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b';')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b';');
                                };
                            }
                            b'.' => {
                                {
                                    if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                    {
                                        return false;
                                    }
                                    state.offset += 1;
                                    __builder.char(b'.');
                                };
                            }
                            _ => {
                                return false;
                            }
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
    fn __term_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp291 = state.offset;
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
                        state.offset = __pretty_cp291;
                    }
                    __ok
                } {
                    {
                        if !{
                            let __pretty_cp289 = state.offset;
                            let __pretty_bcp290 = __builder.checkpoint();
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
                                            let __pretty_cp264 = state.offset;
                                            let __pretty_bcp265 = __builder.checkpoint();
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
                                                            let __pretty_cp251 = state.offset;
                                                            let __pretty_bcp252 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                {
                                                                    let __ows249 = state.offset;
                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                    __builder
                                                                        .text_inline_ws(&state.src[__ows249..state.offset]);
                                                                    if !Self::__rhs_prettify(state, __builder) {
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
                                                        let mut __rep_count262 = 0usize;
                                                        while __rep_count262 < 4294967295 {
                                                            let __rep_cp263 = state.offset;
                                                            if !{
                                                                let __pretty_cp260 = state.offset;
                                                                let __pretty_bcp261 = __builder.checkpoint();
                                                                let __ok = (|| -> bool {
                                                                    {
                                                                        {
                                                                            let __ows253 = state.offset;
                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                            let __ows254 = state.offset;
                                                                            {
                                                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                                {
                                                                                    return false;
                                                                                }
                                                                                state.offset += 1;
                                                                                __builder.char(b',');
                                                                            };
                                                                            __builder.text_inline_ws(&state.src[__ows253..__ows254]);
                                                                            let __ows255 = state.offset;
                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                            __builder
                                                                                .text_inline_ws(&state.src[__ows255..state.offset]);
                                                                        };
                                                                        {
                                                                            if !{
                                                                                let __pretty_cp258 = state.offset;
                                                                                let __pretty_bcp259 = __builder.checkpoint();
                                                                                let __ok = (|| -> bool {
                                                                                    {
                                                                                        let __ows256 = state.offset;
                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                        __builder
                                                                                            .text_inline_ws(&state.src[__ows256..state.offset]);
                                                                                        if !Self::__rhs_prettify(state, __builder) {
                                                                                            return false;
                                                                                        }
                                                                                        let __ows257 = state.offset;
                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                        __builder
                                                                                            .text_inline_ws(&state.src[__ows257..state.offset]);
                                                                                    };
                                                                                    true
                                                                                })();
                                                                                if !__ok {
                                                                                    state.offset = __pretty_cp258;
                                                                                    __builder.restore(__pretty_bcp259);
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
                                                                    state.offset = __pretty_cp260;
                                                                    __builder.restore(__pretty_bcp261);
                                                                }
                                                                __ok
                                                            } {
                                                                state.offset = __rep_cp263;
                                                                break;
                                                            }
                                                            if state.offset == __rep_cp263 {
                                                                break;
                                                            }
                                                            __rep_count262 += 1;
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
                                                state.offset = __pretty_cp264;
                                                __builder.restore(__pretty_bcp265);
                                            }
                                            __ok
                                        };
                                        true
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp289;
                                __builder.restore(__pretty_bcp290);
                            }
                            __ok
                        } {
                            {
                                if !{
                                    let __pretty_cp288 = state.offset;
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
                                        state.offset = __pretty_cp288;
                                    }
                                    __ok
                                } {
                                    {
                                        if !{
                                            let __pretty_cp286 = state.offset;
                                            let __pretty_bcp287 = __builder.checkpoint();
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
                                                            let __pretty_cp268 = state.offset;
                                                            let __pretty_bcp269 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                {
                                                                    let __ows266 = state.offset;
                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                    __builder
                                                                        .text_inline_ws(&state.src[__ows266..state.offset]);
                                                                    if !Self::__rhs_prettify(state, __builder) {
                                                                        return false;
                                                                    }
                                                                    let __ows267 = state.offset;
                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                    __builder
                                                                        .text_inline_ws(&state.src[__ows267..state.offset]);
                                                                };
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp268;
                                                                __builder.restore(__pretty_bcp269);
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
                                                state.offset = __pretty_cp286;
                                                __builder.restore(__pretty_bcp287);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp284 = state.offset;
                                                    let __pretty_bcp285 = __builder.checkpoint();
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
                                                                    let __pretty_cp272 = state.offset;
                                                                    let __pretty_bcp273 = __builder.checkpoint();
                                                                    let __ok = (|| -> bool {
                                                                        {
                                                                            let __ows270 = state.offset;
                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                            __builder
                                                                                .text_inline_ws(&state.src[__ows270..state.offset]);
                                                                            if !Self::__rhs_prettify(state, __builder) {
                                                                                return false;
                                                                            }
                                                                            let __ows271 = state.offset;
                                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                                            __builder
                                                                                .text_inline_ws(&state.src[__ows271..state.offset]);
                                                                        };
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp272;
                                                                        __builder.restore(__pretty_bcp273);
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
                                                        state.offset = __pretty_cp284;
                                                        __builder.restore(__pretty_bcp285);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp282 = state.offset;
                                                            let __pretty_bcp283 = __builder.checkpoint();
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
                                                                            let __pretty_cp276 = state.offset;
                                                                            let __pretty_bcp277 = __builder.checkpoint();
                                                                            let __ok = (|| -> bool {
                                                                                {
                                                                                    let __ows274 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    __builder
                                                                                        .text_inline_ws(&state.src[__ows274..state.offset]);
                                                                                    if !Self::__rhs_prettify(state, __builder) {
                                                                                        return false;
                                                                                    }
                                                                                    let __ows275 = state.offset;
                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                    __builder
                                                                                        .text_inline_ws(&state.src[__ows275..state.offset]);
                                                                                };
                                                                                true
                                                                            })();
                                                                            if !__ok {
                                                                                state.offset = __pretty_cp276;
                                                                                __builder.restore(__pretty_bcp277);
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
                                                                state.offset = __pretty_cp282;
                                                                __builder.restore(__pretty_bcp283);
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp280 = state.offset;
                                                                    let __pretty_bcp281 = __builder.checkpoint();
                                                                    let __ok = (|| -> bool {
                                                                        if !Self::__regex_prettify(state, __builder) {
                                                                            return false;
                                                                        }
                                                                        true
                                                                    })();
                                                                    if !__ok {
                                                                        state.offset = __pretty_cp280;
                                                                        __builder.restore(__pretty_bcp281);
                                                                    }
                                                                    __ok
                                                                } {
                                                                    {
                                                                        if !{
                                                                            let __pretty_cp278 = state.offset;
                                                                            let __pretty_bcp279 = __builder.checkpoint();
                                                                            let __ok = (|| -> bool {
                                                                                if !Self::__literal_prettify(state, __builder) {
                                                                                    return false;
                                                                                }
                                                                                true
                                                                            })();
                                                                            if !__ok {
                                                                                state.offset = __pretty_cp278;
                                                                                __builder.restore(__pretty_bcp279);
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
    fn __directive_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                if !{
                    let __pretty_cp304 = state.offset;
                    let __pretty_bcp305 = __builder.checkpoint();
                    let __ok = (|| -> bool {
                        if !Self::__import_directive_prettify(state, __builder) {
                            return false;
                        }
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
                                if !Self::__recover_directive_prettify(state, __builder) {
                                    return false;
                                }
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
                                        if !Self::__pretty_directive_prettify(state, __builder) {
                                            return false;
                                        }
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
                                                if !Self::__ws_directive_prettify(state, __builder) {
                                                    return false;
                                                }
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp298;
                                                __builder.restore(__pretty_bcp299);
                                            }
                                            __ok
                                        } {
                                            {
                                                if !{
                                                    let __pretty_cp296 = state.offset;
                                                    let __pretty_bcp297 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        if !Self::__token_directive_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp296;
                                                        __builder.restore(__pretty_bcp297);
                                                    }
                                                    __ok
                                                } {
                                                    {
                                                        if !{
                                                            let __pretty_cp294 = state.offset;
                                                            let __pretty_bcp295 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                if !Self::__debug_directive_prettify(state, __builder) {
                                                                    return false;
                                                                }
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp294;
                                                                __builder.restore(__pretty_bcp295);
                                                            }
                                                            __ok
                                                        } {
                                                            {
                                                                if !{
                                                                    let __pretty_cp292 = state.offset;
                                                                    let __pretty_bcp293 = __builder.checkpoint();
                                                                    let __ok = (|| -> bool {
                                                                        if !Self::__host_directive_prettify(state, __builder) {
                                                                            return false;
                                                                        }
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
    fn __factor_prettify<'a>(
        state: &mut ::parse_that::ParserState<'a>,
        __builder: &mut ::pprint::FmtBuilder<'a>,
    ) -> bool {
        {
            {
                {
                    let _ = {
                        let __pretty_cp306 = state.offset;
                        let __pretty_bcp307 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__big_comment_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp306;
                            __builder.restore(__pretty_bcp307);
                        }
                        __ok
                    };
                    true
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
                                if !Self::__term_prettify(state, __builder) {
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
                    let _ = {
                        let __pretty_cp312 = state.offset;
                        let __pretty_bcp313 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__modifier_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp312;
                            __builder.restore(__pretty_bcp313);
                        }
                        __ok
                    };
                    true
                };
                {
                    let _ = {
                        let __pretty_cp314 = state.offset;
                        let __pretty_bcp315 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__big_comment_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp314;
                            __builder.restore(__pretty_bcp315);
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
                        let __pretty_cp321 = state.offset;
                        let __pretty_bcp322 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    let __ows316 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    let __ows317 = state.offset;
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
                                    __builder.text_inline_ws(&state.src[__ows316..__ows317]);
                                    let __ows318 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows318..state.offset]);
                                };
                                {
                                    if !Self::__value_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    {
                                        let _ = {
                                            let __pretty_cp319 = state.offset;
                                            let __pretty_bcp320 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                if !Self::__type_annotation_prettify(state, __builder) {
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
                                };
                            };
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp321;
                            __builder.restore(__pretty_bcp322);
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
                    let mut __rep_count329 = 0usize;
                    while __rep_count329 < 4294967295 {
                        let __rep_cp330 = state.offset;
                        if !{
                            let __pretty_cp327 = state.offset;
                            let __pretty_bcp328 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        if !{
                                            let __pretty_cp325 = state.offset;
                                            let __pretty_bcp326 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows323 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows323..state.offset]);
                                                    if !Self::__binary_operators_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows324 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows324..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp325;
                                                __builder.restore(__pretty_bcp326);
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
                                state.offset = __pretty_cp327;
                                __builder.restore(__pretty_bcp328);
                            }
                            __ok
                        } {
                            state.offset = __rep_cp330;
                            break;
                        }
                        if state.offset == __rep_cp330 {
                            break;
                        }
                        __rep_count329 += 1;
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
}

