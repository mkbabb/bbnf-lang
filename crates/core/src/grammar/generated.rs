//! AUTO-GENERATED from grammar/bbnf/bbnf.bbnf — do not edit manually.
//! Regenerate: scripts/bootstrap-bbnf.sh

#![allow(
    dead_code,
    unused_variables,
    unused_mut,
    unused_parens,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    clippy::all
)]

use ::bbnf::runtime::tape::*;
use ::bbnf::runtime::{Parsed, ParseErr, Root};
use ::parse_that::*;

pub struct BbnfBootstrap;

mod __bbnfbootstrap_emit_impl {
        use super::*;
    pub const GRAMMAR_BbnfBootstrap: [&'static str; 1usize] = [
        "// BBNF \u{2014} Better Backus-Naur Form\n// Self-hosted grammar definition.\n\n@import { value_expr, type_annotation } from \"expressions\" ;\n@import { type_name } from \"types\" ;\n\n// \u{2500}\u{2500}\u{2500} Terminals \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\nidentifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ -> Span ;\n\nliteral = ( \"\\\"\" , /(\\\\.|[^\"\\\\])*/  , \"\\\"\"\n        | \"\'\"  , /(\\\\.|[^\'\\\\])*/  , \"\'\"\n        | \"`\"  , /(\\\\.|[^`\\\\])*/  , \"`\" ) -> Span ;\n\nregex = ( \"/\" , /(\\\\.|[^\\/])+/ , \"/\" ) -> Span ;\n\nbig_comment = ( \"/*\" , /[^\\*]*/ , \"*/\" ) ?w -> Span ;\ncomment = ( \"//\" , /.*/ ) ?w -> Span ;\n\n// \u{2500}\u{2500}\u{2500} Expressions \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\nlhs = identifier ;\n\n// Grammar function call args: each arg is a single binary_factor\n// (alternation of binary_factors, no comma-concatenation).\n// This avoids ambiguity between call arg commas and concatenation commas.\ncall_arg = ( binary_factor ?w , \"|\" ? ) + ;\n\nterm = \"\u{3b5}\" | \"epsilon\"\n     | identifier , ( \"(\" , call_arg ?w , ( \",\" ?w , call_arg ?w ) * , \")\" ) ?\n     | literal\n     | regex\n     | \"@{\" , rhs ?w , \"}\"\n     | \"(\" , rhs ?w , \")\"\n     | \"[\" , rhs ?w , \"]\"\n     | \"{\" , rhs ?w , \"}\" ;\n\nmodifier = \"?w\" | \"?\" | \"*\" | \"+\" ;\nfactor = big_comment ? , term ?w , modifier ? , big_comment ? ;\n\n// Map syntax: factor -> value_expr : type\nmapped_factor = factor , ( \"->\" ?w , ( value_expr , type_annotation ? ) ) ? ;\n\nbinary_operators = \"<<\" | \">>\" | \"-\" ;\nbinary_factor = mapped_factor , ( binary_operators ?w , mapped_factor ) * ;\n\nconcatenation = ( binary_factor ?w , \",\" ? ) + ;\nalternation = ( concatenation ?w , \"|\" ? ) + ;\n\n// Closures at rule level: |params| rhs (grammar functions)\nclosure = \"|\" , identifier , ( \",\" ?w , identifier ) * , \"|\" ?w , rhs ;\nrhs = closure | alternation ;\n\n// \u{2500}\u{2500}\u{2500} Rules and Directives \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\nrule = lhs , \"=\" ?w , rhs ?w , ( \";\" | \".\" ) ;\n\nimport_path = \"\\\"\" , /(\\\\.|[^\"\\\\])*/ , \"\\\"\" ;\nimport_items = \"{\" ?w , ( identifier , ( \",\" ?w , identifier ) * ) ?w , \"}\" ;\nimport_directive = \"@import\" ?w , (\n      import_items ?w , \"from\" ?w , import_path\n    | import_path\n) ?w , ( \";\" | \".\" ) ? ;\n\nrecover_directive = \"@recover\" ?w , identifier ?w , rhs ?w , ( \";\" | \".\" ) ? ;\n\npretty_hint = identifier , ( \"(\" , /[^)]*/ , \")\" ) ? ;\npretty_directive = \"@pretty\" ?w , ( \"*\" | identifier ) ?w , ( pretty_hint ?w ) + , ( \";\" | \".\" ) ? ;\n\nws_directive = \"@ws\" ?w , regex ?w , ( \";\" | \".\" ) ? ;\ntoken_directive = \"@token\" ?w , identifier ?w , ( \";\" | \".\" ) ? ;\ndebug_directive = \"@debug\" ?w , ( \"*\" | identifier ) ?w , ( \";\" | \".\" ) ? ;\nhost_directive = \"@host\" ?w , identifier ?w , ( \":\" ?w , type_name ?w ) ? , ( \";\" | \".\" ) ? ;\n\ndirective = import_directive\n          | recover_directive\n          | pretty_directive\n          | ws_directive\n          | token_directive\n          | debug_directive\n          | host_directive ;\n\n// Grammar: top-level items in any order.\ngrammar_item = comment | big_comment | directive | rule ;\ngrammar = ( grammar_item ?w ) * ;\n\n@pretty grammar block ;\n@pretty rule group ;\n@pretty alternation group ;\n",
    ];
    /// Per-grammar codegen fingerprint — consolidated static
    /// profile emitted by Tranche AV Phase 1. Every downstream
    /// consumer (tape capacity, scanner dispatch, column-set
    /// selection, reorder visitors, keyword tables, shape
    /// dictionary, runtime dedup) reads the matching field.
    pub const GRAMMAR_PROFILE: ::bbnf::runtime::tape::GrammarProfile = ::bbnf::runtime::tape::GrammarProfile {
        push_compound_count: 53u16,
        push_leaf_count: 0u16,
        push_leaf_with_count: 0u16,
        compounds_per_input_byte: 1f32,
        leaves_per_input_byte: 0f32,
        payload_bytes_per_input_byte: 0f32,
        expected_ns_per_byte: 0f32,
        parallel_break_even_bytes: 0u32,
        structural_alphabet: &[],
        structural_digraphs: &[],
        active_columns: &[],
        list_rules: &[],
        keyword_tables: &[],
        shape_dict: &[],
        branch_priors: &[],
        dedup_eligible_rules: &[],
        reorder_unroll_visitors: &[],
    };
    static __DTA_REGEX_0: &str = "0[xX][0-9a-fA-F]+\\w*|[0-9]+\\w*";
    static __DTA_REGEX_1: &str = "[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?\\w*";
    static __DTA_LITERAL_2: &str = "true";
    static __DTA_LITERAL_3: &str = "false";
    static __DTA_ALT_LIN_4: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(2),
        ::bbnf::runtime::tape::DtaStateId(3),
    ];
    static __DTA_LITERAL_5: &str = "\"";
    static __DTA_REGEX_6: &str = "(\\\\.|[^\"\\\\])*";
    static __DTA_LITERAL_7: &str = "\"";
    static __DTA_SEQ_8_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(5),
        ::bbnf::runtime::tape::DtaStateId(6),
        ::bbnf::runtime::tape::DtaStateId(7),
    ];
    static __DTA_REGEX_9: &str = "[_a-zA-Z][_a-zA-Z0-9]*";
    static __DTA_LITERAL_11: &str = "::";
    static __DTA_SEQ_13_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(11),
        ::bbnf::runtime::tape::DtaStateId(12),
    ];
    static __DTA_SEQ_15_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(10),
        ::bbnf::runtime::tape::DtaStateId(14),
    ];
    static __DTA_LITERAL_16: &str = "input";
    static __DTA_LITERAL_17: &str = ".";
    static __DTA_SEQ_19_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(17),
        ::bbnf::runtime::tape::DtaStateId(18),
    ];
    static __DTA_SEQ_21_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(16),
        ::bbnf::runtime::tape::DtaStateId(20),
    ];
    static __DTA_LITERAL_23: &str = "(";
    static __DTA_LITERAL_25: &str = ",";
    static __DTA_SEQ_27_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(25),
        ::bbnf::runtime::tape::DtaStateId(26),
    ];
    static __DTA_SEQ_29_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(24),
        ::bbnf::runtime::tape::DtaStateId(28),
    ];
    static __DTA_LITERAL_31: &str = ")";
    static __DTA_SEQ_32_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 4usize] = [
        ::bbnf::runtime::tape::DtaStateId(22),
        ::bbnf::runtime::tape::DtaStateId(23),
        ::bbnf::runtime::tape::DtaStateId(30),
        ::bbnf::runtime::tape::DtaStateId(31),
    ];
    static __DTA_LITERAL_40: &str = "(";
    static __DTA_LITERAL_42: &str = ")";
    static __DTA_SEQ_43_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(40),
        ::bbnf::runtime::tape::DtaStateId(41),
        ::bbnf::runtime::tape::DtaStateId(42),
    ];
    static __DTA_ALT_LIN_44: [::bbnf::runtime::tape::DtaStateId; 8usize] = [
        ::bbnf::runtime::tape::DtaStateId(33),
        ::bbnf::runtime::tape::DtaStateId(34),
        ::bbnf::runtime::tape::DtaStateId(35),
        ::bbnf::runtime::tape::DtaStateId(36),
        ::bbnf::runtime::tape::DtaStateId(37),
        ::bbnf::runtime::tape::DtaStateId(38),
        ::bbnf::runtime::tape::DtaStateId(39),
        ::bbnf::runtime::tape::DtaStateId(43),
    ];
    static __DTA_LITERAL_45: &str = "*";
    static __DTA_LITERAL_46: &str = "/";
    static __DTA_LITERAL_47: &str = "%";
    static __DTA_ALT_LIN_48: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(45),
        ::bbnf::runtime::tape::DtaStateId(46),
        ::bbnf::runtime::tape::DtaStateId(47),
    ];
    static __DTA_LITERAL_49: &str = "+";
    static __DTA_LITERAL_50: &str = "-";
    static __DTA_ALT_LIN_51: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(49),
        ::bbnf::runtime::tape::DtaStateId(50),
    ];
    static __DTA_LITERAL_52: &str = "==";
    static __DTA_LITERAL_53: &str = "!=";
    static __DTA_LITERAL_54: &str = "<=";
    static __DTA_LITERAL_55: &str = ">=";
    static __DTA_LITERAL_56: &str = "<";
    static __DTA_LITERAL_57: &str = ">";
    static __DTA_ALT_LIN_58: [::bbnf::runtime::tape::DtaStateId; 6usize] = [
        ::bbnf::runtime::tape::DtaStateId(52),
        ::bbnf::runtime::tape::DtaStateId(53),
        ::bbnf::runtime::tape::DtaStateId(54),
        ::bbnf::runtime::tape::DtaStateId(55),
        ::bbnf::runtime::tape::DtaStateId(56),
        ::bbnf::runtime::tape::DtaStateId(57),
    ];
    static __DTA_LITERAL_59: &str = "!";
    static __DTA_LITERAL_60: &str = "-";
    static __DTA_ALT_LIN_61: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(59),
        ::bbnf::runtime::tape::DtaStateId(60),
    ];
    static __DTA_SEQ_63_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(61),
        ::bbnf::runtime::tape::DtaStateId(62),
    ];
    static __DTA_ALT_LIN_65: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(63),
        ::bbnf::runtime::tape::DtaStateId(64),
    ];
    static __DTA_SEQ_69_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(67),
        ::bbnf::runtime::tape::DtaStateId(68),
    ];
    static __DTA_SEQ_71_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(66),
        ::bbnf::runtime::tape::DtaStateId(70),
    ];
    static __DTA_SY_73_PREC: [::bbnf::runtime::tape::DtaPrecedenceEntry; 5usize] = [
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 43,
            second_byte: None,
            precedence: 2,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10),
            op_discriminant: 0,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 45,
            second_byte: None,
            precedence: 2,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(10),
            op_discriminant: 1,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 42,
            second_byte: None,
            precedence: 1,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(9),
            op_discriminant: 0,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 47,
            second_byte: None,
            precedence: 1,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(9),
            op_discriminant: 1,
        },
        ::bbnf::runtime::tape::DtaPrecedenceEntry {
            byte: 37,
            second_byte: None,
            precedence: 1,
            associativity: ::bbnf::runtime::tape::DtaAssociativity::Left,
            op_rule: ::bbnf::runtime::tape::DtaRuleId(9),
            op_discriminant: 2,
        },
    ];
    static __DTA_SEQ_77_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(75),
        ::bbnf::runtime::tape::DtaStateId(76),
    ];
    static __DTA_SEQ_79_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(74),
        ::bbnf::runtime::tape::DtaStateId(78),
    ];
    static __DTA_LITERAL_81: &str = "&&";
    static __DTA_SEQ_83_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(81),
        ::bbnf::runtime::tape::DtaStateId(82),
    ];
    static __DTA_SEQ_85_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(80),
        ::bbnf::runtime::tape::DtaStateId(84),
    ];
    static __DTA_LITERAL_87: &str = "||";
    static __DTA_SEQ_89_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(87),
        ::bbnf::runtime::tape::DtaStateId(88),
    ];
    static __DTA_SEQ_91_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(86),
        ::bbnf::runtime::tape::DtaStateId(90),
    ];
    static __DTA_LITERAL_92: &str = "|";
    static __DTA_LITERAL_94: &str = ",";
    static __DTA_SEQ_96_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(94),
        ::bbnf::runtime::tape::DtaStateId(95),
    ];
    static __DTA_LITERAL_98: &str = "|";
    static __DTA_SEQ_100_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 5usize] = [
        ::bbnf::runtime::tape::DtaStateId(92),
        ::bbnf::runtime::tape::DtaStateId(93),
        ::bbnf::runtime::tape::DtaStateId(97),
        ::bbnf::runtime::tape::DtaStateId(98),
        ::bbnf::runtime::tape::DtaStateId(99),
    ];
    static __DTA_ALT_LIN_103: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(101),
        ::bbnf::runtime::tape::DtaStateId(102),
    ];
    static __DTA_LITERAL_104: &str = ":";
    static __DTA_SEQ_106_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(104),
        ::bbnf::runtime::tape::DtaStateId(105),
    ];
    static __DTA_LITERAL_107: &str = "u8";
    static __DTA_LITERAL_108: &str = "u16";
    static __DTA_LITERAL_109: &str = "u32";
    static __DTA_LITERAL_110: &str = "u64";
    static __DTA_LITERAL_111: &str = "i32";
    static __DTA_LITERAL_112: &str = "i64";
    static __DTA_LITERAL_113: &str = "f32";
    static __DTA_LITERAL_114: &str = "f64";
    static __DTA_LITERAL_115: &str = "bool";
    static __DTA_LITERAL_116: &str = "usize";
    static __DTA_REGEX_117: &str = "[_a-zA-Z][_a-zA-Z0-9]*";
    static __DTA_ALT_LIN_118: [::bbnf::runtime::tape::DtaStateId; 11usize] = [
        ::bbnf::runtime::tape::DtaStateId(107),
        ::bbnf::runtime::tape::DtaStateId(108),
        ::bbnf::runtime::tape::DtaStateId(109),
        ::bbnf::runtime::tape::DtaStateId(110),
        ::bbnf::runtime::tape::DtaStateId(111),
        ::bbnf::runtime::tape::DtaStateId(112),
        ::bbnf::runtime::tape::DtaStateId(113),
        ::bbnf::runtime::tape::DtaStateId(114),
        ::bbnf::runtime::tape::DtaStateId(115),
        ::bbnf::runtime::tape::DtaStateId(116),
        ::bbnf::runtime::tape::DtaStateId(117),
    ];
    static __DTA_REGEX_119: &str = "[_a-zA-Z][_a-zA-Z0-9-]*";
    static __DTA_LITERAL_120: &str = "\"";
    static __DTA_REGEX_121: &str = "(\\\\.|[^\"\\\\])*";
    static __DTA_LITERAL_122: &str = "\"";
    static __DTA_SEQ_123_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(120),
        ::bbnf::runtime::tape::DtaStateId(121),
        ::bbnf::runtime::tape::DtaStateId(122),
    ];
    static __DTA_LITERAL_124: &str = "'";
    static __DTA_REGEX_125: &str = "(\\\\.|[^'\\\\])*";
    static __DTA_LITERAL_126: &str = "'";
    static __DTA_SEQ_127_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(124),
        ::bbnf::runtime::tape::DtaStateId(125),
        ::bbnf::runtime::tape::DtaStateId(126),
    ];
    static __DTA_LITERAL_128: &str = "`";
    static __DTA_REGEX_129: &str = "(\\\\.|[^`\\\\])*";
    static __DTA_LITERAL_130: &str = "`";
    static __DTA_SEQ_131_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(128),
        ::bbnf::runtime::tape::DtaStateId(129),
        ::bbnf::runtime::tape::DtaStateId(130),
    ];
    static __DTA_ALT_LIN_132: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(123),
        ::bbnf::runtime::tape::DtaStateId(127),
        ::bbnf::runtime::tape::DtaStateId(131),
    ];
    static __DTA_LITERAL_133: &str = "/";
    static __DTA_REGEX_134: &str = "(\\\\.|[^\\/])+";
    static __DTA_LITERAL_135: &str = "/";
    static __DTA_SEQ_136_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(133),
        ::bbnf::runtime::tape::DtaStateId(134),
        ::bbnf::runtime::tape::DtaStateId(135),
    ];
    static __DTA_LITERAL_137: &str = "/*";
    static __DTA_REGEX_138: &str = "[^\\*]*";
    static __DTA_LITERAL_139: &str = "*/";
    static __DTA_SEQ_140_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(137),
        ::bbnf::runtime::tape::DtaStateId(138),
        ::bbnf::runtime::tape::DtaStateId(139),
    ];
    static __DTA_LITERAL_141: &str = "//";
    static __DTA_REGEX_142: &str = ".*";
    static __DTA_SEQ_143_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(141),
        ::bbnf::runtime::tape::DtaStateId(142),
    ];
    static __DTA_LITERAL_146: &str = "|";
    static __DTA_SEQ_148_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(145),
        ::bbnf::runtime::tape::DtaStateId(147),
    ];
    static __DTA_LITERAL_150: &str = "ε";
    static __DTA_LITERAL_151: &str = "epsilon";
    static __DTA_LITERAL_153: &str = "(";
    static __DTA_LITERAL_155: &str = ",";
    static __DTA_SEQ_157_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(155),
        ::bbnf::runtime::tape::DtaStateId(156),
    ];
    static __DTA_LITERAL_159: &str = ")";
    static __DTA_SEQ_160_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 4usize] = [
        ::bbnf::runtime::tape::DtaStateId(153),
        ::bbnf::runtime::tape::DtaStateId(154),
        ::bbnf::runtime::tape::DtaStateId(158),
        ::bbnf::runtime::tape::DtaStateId(159),
    ];
    static __DTA_SEQ_162_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(152),
        ::bbnf::runtime::tape::DtaStateId(161),
    ];
    static __DTA_LITERAL_165: &str = "@{";
    static __DTA_LITERAL_167: &str = "}";
    static __DTA_SEQ_168_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(165),
        ::bbnf::runtime::tape::DtaStateId(166),
        ::bbnf::runtime::tape::DtaStateId(167),
    ];
    static __DTA_LITERAL_169: &str = "(";
    static __DTA_LITERAL_171: &str = ")";
    static __DTA_SEQ_172_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(169),
        ::bbnf::runtime::tape::DtaStateId(170),
        ::bbnf::runtime::tape::DtaStateId(171),
    ];
    static __DTA_LITERAL_173: &str = "[";
    static __DTA_LITERAL_175: &str = "]";
    static __DTA_SEQ_176_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(173),
        ::bbnf::runtime::tape::DtaStateId(174),
        ::bbnf::runtime::tape::DtaStateId(175),
    ];
    static __DTA_LITERAL_177: &str = "{";
    static __DTA_LITERAL_179: &str = "}";
    static __DTA_SEQ_180_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(177),
        ::bbnf::runtime::tape::DtaStateId(178),
        ::bbnf::runtime::tape::DtaStateId(179),
    ];
    static __DTA_ALT_LIN_181: [::bbnf::runtime::tape::DtaStateId; 9usize] = [
        ::bbnf::runtime::tape::DtaStateId(150),
        ::bbnf::runtime::tape::DtaStateId(151),
        ::bbnf::runtime::tape::DtaStateId(162),
        ::bbnf::runtime::tape::DtaStateId(163),
        ::bbnf::runtime::tape::DtaStateId(164),
        ::bbnf::runtime::tape::DtaStateId(168),
        ::bbnf::runtime::tape::DtaStateId(172),
        ::bbnf::runtime::tape::DtaStateId(176),
        ::bbnf::runtime::tape::DtaStateId(180),
    ];
    static __DTA_LITERAL_182: &str = "?w";
    static __DTA_LITERAL_183: &str = "?";
    static __DTA_LITERAL_184: &str = "*";
    static __DTA_LITERAL_185: &str = "+";
    static __DTA_ALT_LIN_186: [::bbnf::runtime::tape::DtaStateId; 4usize] = [
        ::bbnf::runtime::tape::DtaStateId(182),
        ::bbnf::runtime::tape::DtaStateId(183),
        ::bbnf::runtime::tape::DtaStateId(184),
        ::bbnf::runtime::tape::DtaStateId(185),
    ];
    static __DTA_SEQ_194_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 4usize] = [
        ::bbnf::runtime::tape::DtaStateId(188),
        ::bbnf::runtime::tape::DtaStateId(189),
        ::bbnf::runtime::tape::DtaStateId(191),
        ::bbnf::runtime::tape::DtaStateId(193),
    ];
    static __DTA_LITERAL_196: &str = "->";
    static __DTA_SEQ_200_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(197),
        ::bbnf::runtime::tape::DtaStateId(199),
    ];
    static __DTA_SEQ_201_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(196),
        ::bbnf::runtime::tape::DtaStateId(200),
    ];
    static __DTA_SEQ_203_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(195),
        ::bbnf::runtime::tape::DtaStateId(202),
    ];
    static __DTA_LITERAL_204: &str = "<<";
    static __DTA_LITERAL_205: &str = ">>";
    static __DTA_LITERAL_206: &str = "-";
    static __DTA_ALT_LIN_207: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(204),
        ::bbnf::runtime::tape::DtaStateId(205),
        ::bbnf::runtime::tape::DtaStateId(206),
    ];
    static __DTA_SEQ_211_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(209),
        ::bbnf::runtime::tape::DtaStateId(210),
    ];
    static __DTA_SEQ_213_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(208),
        ::bbnf::runtime::tape::DtaStateId(212),
    ];
    static __DTA_LITERAL_215: &str = ",";
    static __DTA_SEQ_217_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(214),
        ::bbnf::runtime::tape::DtaStateId(216),
    ];
    static __DTA_LITERAL_220: &str = "|";
    static __DTA_SEQ_222_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(219),
        ::bbnf::runtime::tape::DtaStateId(221),
    ];
    static __DTA_LITERAL_224: &str = "|";
    static __DTA_LITERAL_226: &str = ",";
    static __DTA_SEQ_228_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(226),
        ::bbnf::runtime::tape::DtaStateId(227),
    ];
    static __DTA_LITERAL_230: &str = "|";
    static __DTA_SEQ_232_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 5usize] = [
        ::bbnf::runtime::tape::DtaStateId(224),
        ::bbnf::runtime::tape::DtaStateId(225),
        ::bbnf::runtime::tape::DtaStateId(229),
        ::bbnf::runtime::tape::DtaStateId(230),
        ::bbnf::runtime::tape::DtaStateId(231),
    ];
    static __DTA_ALT_LIN_235: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(233),
        ::bbnf::runtime::tape::DtaStateId(234),
    ];
    static __DTA_LITERAL_237: &str = "=";
    static __DTA_LITERAL_239: &str = ";";
    static __DTA_LITERAL_240: &str = ".";
    static __DTA_ALT_LIN_241: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(239),
        ::bbnf::runtime::tape::DtaStateId(240),
    ];
    static __DTA_SEQ_242_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 4usize] = [
        ::bbnf::runtime::tape::DtaStateId(236),
        ::bbnf::runtime::tape::DtaStateId(237),
        ::bbnf::runtime::tape::DtaStateId(238),
        ::bbnf::runtime::tape::DtaStateId(241),
    ];
    static __DTA_LITERAL_243: &str = "\"";
    static __DTA_REGEX_244: &str = "(\\\\.|[^\"\\\\])*";
    static __DTA_LITERAL_245: &str = "\"";
    static __DTA_SEQ_246_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(243),
        ::bbnf::runtime::tape::DtaStateId(244),
        ::bbnf::runtime::tape::DtaStateId(245),
    ];
    static __DTA_LITERAL_247: &str = "{";
    static __DTA_LITERAL_249: &str = ",";
    static __DTA_SEQ_251_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(249),
        ::bbnf::runtime::tape::DtaStateId(250),
    ];
    static __DTA_SEQ_253_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(248),
        ::bbnf::runtime::tape::DtaStateId(252),
    ];
    static __DTA_LITERAL_254: &str = "}";
    static __DTA_SEQ_255_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(247),
        ::bbnf::runtime::tape::DtaStateId(253),
        ::bbnf::runtime::tape::DtaStateId(254),
    ];
    static __DTA_LITERAL_256: &str = "@import";
    static __DTA_LITERAL_258: &str = "from";
    static __DTA_SEQ_260_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(257),
        ::bbnf::runtime::tape::DtaStateId(258),
        ::bbnf::runtime::tape::DtaStateId(259),
    ];
    static __DTA_ALT_LIN_262: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(260),
        ::bbnf::runtime::tape::DtaStateId(261),
    ];
    static __DTA_LITERAL_263: &str = ";";
    static __DTA_LITERAL_264: &str = ".";
    static __DTA_ALT_LIN_265: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(263),
        ::bbnf::runtime::tape::DtaStateId(264),
    ];
    static __DTA_SEQ_267_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(256),
        ::bbnf::runtime::tape::DtaStateId(262),
        ::bbnf::runtime::tape::DtaStateId(266),
    ];
    static __DTA_LITERAL_268: &str = "@recover";
    static __DTA_LITERAL_271: &str = ";";
    static __DTA_LITERAL_272: &str = ".";
    static __DTA_ALT_LIN_273: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(271),
        ::bbnf::runtime::tape::DtaStateId(272),
    ];
    static __DTA_SEQ_275_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 4usize] = [
        ::bbnf::runtime::tape::DtaStateId(268),
        ::bbnf::runtime::tape::DtaStateId(269),
        ::bbnf::runtime::tape::DtaStateId(270),
        ::bbnf::runtime::tape::DtaStateId(274),
    ];
    static __DTA_LITERAL_277: &str = "(";
    static __DTA_REGEX_278: &str = "[^)]*";
    static __DTA_LITERAL_279: &str = ")";
    static __DTA_SEQ_280_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(277),
        ::bbnf::runtime::tape::DtaStateId(278),
        ::bbnf::runtime::tape::DtaStateId(279),
    ];
    static __DTA_SEQ_282_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(276),
        ::bbnf::runtime::tape::DtaStateId(281),
    ];
    static __DTA_LITERAL_283: &str = "@pretty";
    static __DTA_LITERAL_284: &str = "*";
    static __DTA_ALT_LIN_286: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(284),
        ::bbnf::runtime::tape::DtaStateId(285),
    ];
    static __DTA_LITERAL_289: &str = ";";
    static __DTA_LITERAL_290: &str = ".";
    static __DTA_ALT_LIN_291: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(289),
        ::bbnf::runtime::tape::DtaStateId(290),
    ];
    static __DTA_SEQ_293_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 4usize] = [
        ::bbnf::runtime::tape::DtaStateId(283),
        ::bbnf::runtime::tape::DtaStateId(286),
        ::bbnf::runtime::tape::DtaStateId(288),
        ::bbnf::runtime::tape::DtaStateId(292),
    ];
    static __DTA_LITERAL_294: &str = "@ws";
    static __DTA_LITERAL_296: &str = ";";
    static __DTA_LITERAL_297: &str = ".";
    static __DTA_ALT_LIN_298: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(296),
        ::bbnf::runtime::tape::DtaStateId(297),
    ];
    static __DTA_SEQ_300_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(294),
        ::bbnf::runtime::tape::DtaStateId(295),
        ::bbnf::runtime::tape::DtaStateId(299),
    ];
    static __DTA_LITERAL_301: &str = "@token";
    static __DTA_LITERAL_303: &str = ";";
    static __DTA_LITERAL_304: &str = ".";
    static __DTA_ALT_LIN_305: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(303),
        ::bbnf::runtime::tape::DtaStateId(304),
    ];
    static __DTA_SEQ_307_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(301),
        ::bbnf::runtime::tape::DtaStateId(302),
        ::bbnf::runtime::tape::DtaStateId(306),
    ];
    static __DTA_LITERAL_308: &str = "@debug";
    static __DTA_LITERAL_309: &str = "*";
    static __DTA_ALT_LIN_311: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(309),
        ::bbnf::runtime::tape::DtaStateId(310),
    ];
    static __DTA_LITERAL_312: &str = ";";
    static __DTA_LITERAL_313: &str = ".";
    static __DTA_ALT_LIN_314: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(312),
        ::bbnf::runtime::tape::DtaStateId(313),
    ];
    static __DTA_SEQ_316_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 3usize] = [
        ::bbnf::runtime::tape::DtaStateId(308),
        ::bbnf::runtime::tape::DtaStateId(311),
        ::bbnf::runtime::tape::DtaStateId(315),
    ];
    static __DTA_LITERAL_317: &str = "@host";
    static __DTA_LITERAL_319: &str = ":";
    static __DTA_SEQ_321_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(319),
        ::bbnf::runtime::tape::DtaStateId(320),
    ];
    static __DTA_LITERAL_323: &str = ";";
    static __DTA_LITERAL_324: &str = ".";
    static __DTA_ALT_LIN_325: [::bbnf::runtime::tape::DtaStateId; 2usize] = [
        ::bbnf::runtime::tape::DtaStateId(323),
        ::bbnf::runtime::tape::DtaStateId(324),
    ];
    static __DTA_SEQ_327_CHILDREN: [::bbnf::runtime::tape::DtaStateId; 4usize] = [
        ::bbnf::runtime::tape::DtaStateId(317),
        ::bbnf::runtime::tape::DtaStateId(318),
        ::bbnf::runtime::tape::DtaStateId(322),
        ::bbnf::runtime::tape::DtaStateId(326),
    ];
    static __DTA_ALT_LIN_335: [::bbnf::runtime::tape::DtaStateId; 7usize] = [
        ::bbnf::runtime::tape::DtaStateId(328),
        ::bbnf::runtime::tape::DtaStateId(329),
        ::bbnf::runtime::tape::DtaStateId(330),
        ::bbnf::runtime::tape::DtaStateId(331),
        ::bbnf::runtime::tape::DtaStateId(332),
        ::bbnf::runtime::tape::DtaStateId(333),
        ::bbnf::runtime::tape::DtaStateId(334),
    ];
    static __DTA_ALT_LIN_340: [::bbnf::runtime::tape::DtaStateId; 4usize] = [
        ::bbnf::runtime::tape::DtaStateId(336),
        ::bbnf::runtime::tape::DtaStateId(337),
        ::bbnf::runtime::tape::DtaStateId(338),
        ::bbnf::runtime::tape::DtaStateId(339),
    ];
    static __DTA_STATES: [::bbnf::runtime::tape::DtaState; 343usize] = [
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_0,
        },
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_1,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_2,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_3,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_4,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_5,
        },
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_6,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_7,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_8_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_9,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(4),
            target: ::bbnf::runtime::tape::DtaStateId(9),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_11,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(4),
            target: ::bbnf::runtime::tape::DtaStateId(9),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_13_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(13),
            lo: 0,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_15_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_16,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_17,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(4),
            target: ::bbnf::runtime::tape::DtaStateId(9),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_19_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(19),
            lo: 0,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_21_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(5),
            target: ::bbnf::runtime::tape::DtaStateId(15),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_23,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(19),
            target: ::bbnf::runtime::tape::DtaStateId(65535),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_25,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(19),
            target: ::bbnf::runtime::tape::DtaStateId(65535),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_27_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(27),
            lo: 0,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_29_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(29),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_31,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_32_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(0),
            target: ::bbnf::runtime::tape::DtaStateId(0),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(1),
            target: ::bbnf::runtime::tape::DtaStateId(1),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(2),
            target: ::bbnf::runtime::tape::DtaStateId(4),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(3),
            target: ::bbnf::runtime::tape::DtaStateId(8),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(7),
            target: ::bbnf::runtime::tape::DtaStateId(32),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(6),
            target: ::bbnf::runtime::tape::DtaStateId(21),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(5),
            target: ::bbnf::runtime::tape::DtaStateId(15),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_40,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(19),
            target: ::bbnf::runtime::tape::DtaStateId(65535),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_42,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_43_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_44,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_45,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_46,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_47,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_48,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_49,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_50,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_51,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_52,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_53,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_54,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_55,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_56,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_57,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_58,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_59,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_60,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_61,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(8),
            target: ::bbnf::runtime::tape::DtaStateId(44),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_63_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(8),
            target: ::bbnf::runtime::tape::DtaStateId(44),
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_65,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(12),
            target: ::bbnf::runtime::tape::DtaStateId(65),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(9),
            target: ::bbnf::runtime::tape::DtaStateId(48),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(12),
            target: ::bbnf::runtime::tape::DtaStateId(65),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_69_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(69),
            lo: 0,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_71_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(12),
            target: ::bbnf::runtime::tape::DtaStateId(65),
        },
        ::bbnf::runtime::tape::DtaState::ShuntingYard {
            head: ::bbnf::runtime::tape::DtaStateId(72),
            precedence: &__DTA_SY_73_PREC,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(14),
            target: ::bbnf::runtime::tape::DtaStateId(73),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(11),
            target: ::bbnf::runtime::tape::DtaStateId(58),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(14),
            target: ::bbnf::runtime::tape::DtaStateId(73),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_77_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(77),
            lo: 0,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_79_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(15),
            target: ::bbnf::runtime::tape::DtaStateId(79),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_81,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(15),
            target: ::bbnf::runtime::tape::DtaStateId(79),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_83_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(83),
            lo: 0,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_85_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(16),
            target: ::bbnf::runtime::tape::DtaStateId(85),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_87,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(16),
            target: ::bbnf::runtime::tape::DtaStateId(85),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_89_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(89),
            lo: 0,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_91_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_92,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(4),
            target: ::bbnf::runtime::tape::DtaStateId(9),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_94,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(4),
            target: ::bbnf::runtime::tape::DtaStateId(9),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_96_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(96),
            lo: 0,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_98,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(19),
            target: ::bbnf::runtime::tape::DtaStateId(65535),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_100_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(18),
            target: ::bbnf::runtime::tape::DtaStateId(100),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(17),
            target: ::bbnf::runtime::tape::DtaStateId(91),
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_103,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_104,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(21),
            target: ::bbnf::runtime::tape::DtaStateId(65535),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_106_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_107,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_108,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_109,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_110,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_111,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_112,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_113,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_114,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_115,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_116,
        },
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_117,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_118,
        },
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_119,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_120,
        },
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_121,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_122,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_123_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_124,
        },
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_125,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_126,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_127_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_128,
        },
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_129,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_130,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_131_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_132,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_133,
        },
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_134,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_135,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_136_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_137,
        },
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_138,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_139,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_140_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_141,
        },
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_142,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_143_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(22),
            target: ::bbnf::runtime::tape::DtaStateId(119),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(34),
            target: ::bbnf::runtime::tape::DtaStateId(65535),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_146,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(146),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_148_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(148),
            lo: 1,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_150,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_151,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(22),
            target: ::bbnf::runtime::tape::DtaStateId(119),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_153,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(28),
            target: ::bbnf::runtime::tape::DtaStateId(149),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_155,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(28),
            target: ::bbnf::runtime::tape::DtaStateId(149),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_157_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(157),
            lo: 0,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_159,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_160_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(160),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_162_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(23),
            target: ::bbnf::runtime::tape::DtaStateId(132),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(24),
            target: ::bbnf::runtime::tape::DtaStateId(136),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_165,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(38),
            target: ::bbnf::runtime::tape::DtaStateId(65535),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_167,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_168_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_169,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(38),
            target: ::bbnf::runtime::tape::DtaStateId(65535),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_171,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_172_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_173,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(38),
            target: ::bbnf::runtime::tape::DtaStateId(65535),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_175,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_176_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_177,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(38),
            target: ::bbnf::runtime::tape::DtaStateId(65535),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_179,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_180_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_181,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_182,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_183,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_184,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_185,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_186,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(25),
            target: ::bbnf::runtime::tape::DtaStateId(140),
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(187),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(29),
            target: ::bbnf::runtime::tape::DtaStateId(181),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(30),
            target: ::bbnf::runtime::tape::DtaStateId(186),
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(190),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(25),
            target: ::bbnf::runtime::tape::DtaStateId(140),
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(192),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_194_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(31),
            target: ::bbnf::runtime::tape::DtaStateId(194),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_196,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(19),
            target: ::bbnf::runtime::tape::DtaStateId(103),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(20),
            target: ::bbnf::runtime::tape::DtaStateId(106),
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(198),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_200_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_201_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(201),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_203_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_204,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_205,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_206,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_207,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(32),
            target: ::bbnf::runtime::tape::DtaStateId(203),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(33),
            target: ::bbnf::runtime::tape::DtaStateId(207),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(32),
            target: ::bbnf::runtime::tape::DtaStateId(203),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_211_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(211),
            lo: 0,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_213_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(34),
            target: ::bbnf::runtime::tape::DtaStateId(213),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_215,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(215),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_217_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(217),
            lo: 1,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(35),
            target: ::bbnf::runtime::tape::DtaStateId(218),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_220,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(220),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_222_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(222),
            lo: 1,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_224,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(22),
            target: ::bbnf::runtime::tape::DtaStateId(119),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_226,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(22),
            target: ::bbnf::runtime::tape::DtaStateId(119),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_228_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(228),
            lo: 0,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_230,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(38),
            target: ::bbnf::runtime::tape::DtaStateId(65535),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_232_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(37),
            target: ::bbnf::runtime::tape::DtaStateId(232),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(36),
            target: ::bbnf::runtime::tape::DtaStateId(223),
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_235,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(27),
            target: ::bbnf::runtime::tape::DtaStateId(144),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_237,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(38),
            target: ::bbnf::runtime::tape::DtaStateId(235),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_239,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_240,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_241,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_242_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_243,
        },
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_244,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_245,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_246_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_247,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(22),
            target: ::bbnf::runtime::tape::DtaStateId(119),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_249,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(22),
            target: ::bbnf::runtime::tape::DtaStateId(119),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_251_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(251),
            lo: 0,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_253_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_254,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_255_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_256,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(41),
            target: ::bbnf::runtime::tape::DtaStateId(255),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_258,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(40),
            target: ::bbnf::runtime::tape::DtaStateId(246),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_260_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(40),
            target: ::bbnf::runtime::tape::DtaStateId(246),
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_262,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_263,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_264,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_265,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(265),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_267_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_268,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(22),
            target: ::bbnf::runtime::tape::DtaStateId(119),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(38),
            target: ::bbnf::runtime::tape::DtaStateId(235),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_271,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_272,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_273,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(273),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_275_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(22),
            target: ::bbnf::runtime::tape::DtaStateId(119),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_277,
        },
        ::bbnf::runtime::tape::DtaState::Regex {
            pattern: __DTA_REGEX_278,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_279,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_280_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(280),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_282_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_283,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_284,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(22),
            target: ::bbnf::runtime::tape::DtaStateId(119),
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_286,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(44),
            target: ::bbnf::runtime::tape::DtaStateId(282),
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(287),
            lo: 1,
            hi: 4294967295,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_289,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_290,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_291,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(291),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_293_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_294,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(24),
            target: ::bbnf::runtime::tape::DtaStateId(136),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_296,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_297,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_298,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(298),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_300_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_301,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(22),
            target: ::bbnf::runtime::tape::DtaStateId(119),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_303,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_304,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_305,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(305),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_307_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_308,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_309,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(22),
            target: ::bbnf::runtime::tape::DtaStateId(119),
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_311,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_312,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_313,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_314,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(314),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_316_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_317,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(22),
            target: ::bbnf::runtime::tape::DtaStateId(119),
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_319,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(21),
            target: ::bbnf::runtime::tape::DtaStateId(118),
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_321_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(321),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_323,
        },
        ::bbnf::runtime::tape::DtaState::Literal {
            text: __DTA_LITERAL_324,
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_325,
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(325),
            lo: 0,
            hi: 1,
            counter_optional: None,
        },
        ::bbnf::runtime::tape::DtaState::Seq {
            children: &__DTA_SEQ_327_CHILDREN,
            frame: ::bbnf::runtime::tape::DtaFrameKind::Seq,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(42),
            target: ::bbnf::runtime::tape::DtaStateId(267),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(43),
            target: ::bbnf::runtime::tape::DtaStateId(275),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(45),
            target: ::bbnf::runtime::tape::DtaStateId(293),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(46),
            target: ::bbnf::runtime::tape::DtaStateId(300),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(47),
            target: ::bbnf::runtime::tape::DtaStateId(307),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(48),
            target: ::bbnf::runtime::tape::DtaStateId(316),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(49),
            target: ::bbnf::runtime::tape::DtaStateId(327),
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_335,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(26),
            target: ::bbnf::runtime::tape::DtaStateId(143),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(25),
            target: ::bbnf::runtime::tape::DtaStateId(140),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(50),
            target: ::bbnf::runtime::tape::DtaStateId(335),
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(39),
            target: ::bbnf::runtime::tape::DtaStateId(242),
        },
        ::bbnf::runtime::tape::DtaState::AltLinear {
            branches: &__DTA_ALT_LIN_340,
        },
        ::bbnf::runtime::tape::DtaState::Ref {
            rule: ::bbnf::runtime::tape::DtaRuleId(51),
            target: ::bbnf::runtime::tape::DtaStateId(340),
        },
        ::bbnf::runtime::tape::DtaState::Repeat {
            inner: ::bbnf::runtime::tape::DtaStateId(341),
            lo: 0,
            hi: 4294967295,
            counter_optional: None,
        },
    ];
    static __DTA_RULE_ENTRIES: [::bbnf::runtime::tape::DtaRuleEntry; 53usize] = [
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(0),
            state: ::bbnf::runtime::tape::DtaStateId(0),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(1),
            state: ::bbnf::runtime::tape::DtaStateId(1),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(2),
            state: ::bbnf::runtime::tape::DtaStateId(4),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(3),
            state: ::bbnf::runtime::tape::DtaStateId(8),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(4),
            state: ::bbnf::runtime::tape::DtaStateId(9),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(5),
            state: ::bbnf::runtime::tape::DtaStateId(15),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(6),
            state: ::bbnf::runtime::tape::DtaStateId(21),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(7),
            state: ::bbnf::runtime::tape::DtaStateId(32),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(8),
            state: ::bbnf::runtime::tape::DtaStateId(44),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(9),
            state: ::bbnf::runtime::tape::DtaStateId(48),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(10),
            state: ::bbnf::runtime::tape::DtaStateId(51),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(11),
            state: ::bbnf::runtime::tape::DtaStateId(58),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(12),
            state: ::bbnf::runtime::tape::DtaStateId(65),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(13),
            state: ::bbnf::runtime::tape::DtaStateId(73),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(14),
            state: ::bbnf::runtime::tape::DtaStateId(73),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(15),
            state: ::bbnf::runtime::tape::DtaStateId(79),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(16),
            state: ::bbnf::runtime::tape::DtaStateId(85),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(17),
            state: ::bbnf::runtime::tape::DtaStateId(91),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(18),
            state: ::bbnf::runtime::tape::DtaStateId(100),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(19),
            state: ::bbnf::runtime::tape::DtaStateId(103),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(20),
            state: ::bbnf::runtime::tape::DtaStateId(106),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(21),
            state: ::bbnf::runtime::tape::DtaStateId(118),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(22),
            state: ::bbnf::runtime::tape::DtaStateId(119),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(23),
            state: ::bbnf::runtime::tape::DtaStateId(132),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(24),
            state: ::bbnf::runtime::tape::DtaStateId(136),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(25),
            state: ::bbnf::runtime::tape::DtaStateId(140),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(26),
            state: ::bbnf::runtime::tape::DtaStateId(143),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(27),
            state: ::bbnf::runtime::tape::DtaStateId(144),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(28),
            state: ::bbnf::runtime::tape::DtaStateId(149),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(29),
            state: ::bbnf::runtime::tape::DtaStateId(181),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(30),
            state: ::bbnf::runtime::tape::DtaStateId(186),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(31),
            state: ::bbnf::runtime::tape::DtaStateId(194),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(32),
            state: ::bbnf::runtime::tape::DtaStateId(203),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(33),
            state: ::bbnf::runtime::tape::DtaStateId(207),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(34),
            state: ::bbnf::runtime::tape::DtaStateId(213),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(35),
            state: ::bbnf::runtime::tape::DtaStateId(218),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(36),
            state: ::bbnf::runtime::tape::DtaStateId(223),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(37),
            state: ::bbnf::runtime::tape::DtaStateId(232),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(38),
            state: ::bbnf::runtime::tape::DtaStateId(235),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(39),
            state: ::bbnf::runtime::tape::DtaStateId(242),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(40),
            state: ::bbnf::runtime::tape::DtaStateId(246),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(41),
            state: ::bbnf::runtime::tape::DtaStateId(255),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(42),
            state: ::bbnf::runtime::tape::DtaStateId(267),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(43),
            state: ::bbnf::runtime::tape::DtaStateId(275),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(44),
            state: ::bbnf::runtime::tape::DtaStateId(282),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(45),
            state: ::bbnf::runtime::tape::DtaStateId(293),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(46),
            state: ::bbnf::runtime::tape::DtaStateId(300),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(47),
            state: ::bbnf::runtime::tape::DtaStateId(307),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(48),
            state: ::bbnf::runtime::tape::DtaStateId(316),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(49),
            state: ::bbnf::runtime::tape::DtaStateId(327),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(50),
            state: ::bbnf::runtime::tape::DtaStateId(335),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(51),
            state: ::bbnf::runtime::tape::DtaStateId(340),
        },
        ::bbnf::runtime::tape::DtaRuleEntry {
            rule: ::bbnf::runtime::tape::DtaRuleId(52),
            state: ::bbnf::runtime::tape::DtaStateId(342),
        },
    ];
    static __DTA_SHUNTING_YARD_RULES: [::bbnf::runtime::tape::DtaRuleId; 2usize] = [
        ::bbnf::runtime::tape::DtaRuleId(13),
        ::bbnf::runtime::tape::DtaRuleId(14),
    ];
    /// Dispatch Tape Automaton — emitted by Tranche AV Phase 3.
    /// The runtime driver (V4 PSI stage-B) walks this table from
    /// each rule's entry state; until V4 lands, this data is
    /// inert and `parse()` drives through the legacy fn-per-rule
    /// path.
    pub const DTA_TABLE: ::bbnf::runtime::tape::DtaTable = ::bbnf::runtime::tape::DtaTable {
        states: &__DTA_STATES,
        rule_entries: &__DTA_RULE_ENTRIES,
        shunting_yard_rules: &__DTA_SHUNTING_YARD_RULES,
        counter_optional_rules: &[],
        max_nesting_depth: 8u16,
    };
    /// Shape dictionary — empty for this grammar (selection
    /// admitted no templates). Reserved symbol so downstream
    /// driver code can reference it unconditionally.
    pub const SHAPE_DICT: &[::bbnf::runtime::tape::ShapeEntry] = &[];
    /// AV.4.1 — Allocate this grammar's stage-A PSI stream sized
    /// from [`GRAMMAR_PROFILE`]'s `leaves_per_input_byte`.
    ///
    /// The runtime stage-A driver (AV.3.6 / V4) calls this once
    /// at parse start; every subsequent `PayloadJob::push` lands
    /// in pre-allocated memory.
    #[inline]
    pub fn psi_with_capacity(input_len: usize) -> ::bbnf::runtime::tape::PayloadStream {
        ::bbnf::runtime::tape::PayloadStream::with_capacity_for(
            &GRAMMAR_PROFILE,
            input_len,
        )
    }
    /// AV.4.2 — Drive stage-B's payload fill over the PSI stream.
    ///
    /// Single API for sequential and parallel paths; the dispatch
    /// fork lives inside
    /// [`PayloadStream::fill_columns`](::bbnf::runtime::tape::PayloadStream::fill_columns)
    /// on `GRAMMAR_PROFILE.parallel_break_even_bytes`. Inputs
    /// below the break-even gate run sequentially; above, the
    /// rayon `par_chunks` walk takes over.
    #[inline]
    pub fn fill_payloads(
        psi: &::bbnf::runtime::tape::PayloadStream,
        input: &[u8],
        columns: &mut ::bbnf::runtime::tape::Columns,
    ) -> usize {
        psi.fill_columns(input, columns, &GRAMMAR_PROFILE)
    }
    static __BBNF_SHAPE_TEMPLATES: [::bbnf::runtime::tape::BbnfShapeEntry; 2usize] = [
        ::bbnf::runtime::tape::BbnfShapeEntry {
            rule_name: "big_comment",
            kind: ::bbnf::runtime::tape::BbnfShapeKind::BigComment,
            shape_hash: 3892071320236552774,
            payload_bytes: 8,
        },
        ::bbnf::runtime::tape::BbnfShapeEntry {
            rule_name: "mapped_factor",
            kind: ::bbnf::runtime::tape::BbnfShapeKind::MappedFactorEmpty,
            shape_hash: 16049972439495755825,
            payload_bytes: 0,
        },
    ];
    /// BBNF shape dictionary — non-empty for BBNF self-hosting.
    /// The codegen for `__big_comment` / `__mapped_factor` checks
    /// this dictionary at compile time and emits a single
    /// `push_shape_ref` per match. See AV.5.6 / AV.6.1–6.3.
    pub const BBNF_SHAPE_DICT: &[::bbnf::runtime::tape::BbnfShapeEntry] = &__BBNF_SHAPE_TEMPLATES;
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct int_litView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> int_litView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> int_litView<'p> {
        /// The source text matched by this leaf rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// Get the parsed scalar value.
        ///
        /// Payload-first: reads the pre-computed value from the
        /// tape payload buffer in O(1). Falls back to span text
        /// parsing if no payload is present.
        #[inline]
        pub fn value(&self) -> i64 {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            if let Some(v) = tape.payload_i64(rec) {
                return v;
            }
            self.span_text().parse::<i64>().unwrap_or(0)
        }
        /// Convert the matched span to the scalar type.
        ///
        /// Alias for backward compatibility. Prefer `.value()`.
        #[inline]
        pub fn as_i64(&self) -> i64 {
            self.value()
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct float_litView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> float_litView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> float_litView<'p> {
        /// The source text matched by this leaf rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// Get the parsed scalar value.
        ///
        /// Payload-first: reads the pre-computed value from the
        /// tape payload buffer in O(1). Falls back to span text
        /// parsing if no payload is present.
        #[inline]
        pub fn value(&self) -> f64 {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            if let Some(v) = tape.payload_f64(rec) {
                return v;
            }
            self.span_text().parse::<f64>().unwrap_or(0.0)
        }
        /// Convert the matched span to the scalar type.
        ///
        /// Alias for backward compatibility. Prefer `.value()`.
        #[inline]
        pub fn as_f64(&self) -> f64 {
            self.value()
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct bool_litView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> bool_litView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> bool_litView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct string_litView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> string_litView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> string_litView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct value_identView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> value_identView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> value_identView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct value_pathView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> value_pathView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> value_pathView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<value_identView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| value_identView::from_cursor(c, self.input))
        }
        ///The `value_ident` child as a typed view.
        #[inline]
        pub fn value_ident(&self) -> ::core::option::Option<value_identView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| value_identView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct value_inputView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> value_inputView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> value_inputView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct value_fn_callView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> value_fn_callView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> value_fn_callView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<value_pathView<'p>> {
            self.cursor.child(0usize).map(|c| value_pathView::from_cursor(c, self.input))
        }
        ///The `value_path` child as a typed view.
        #[inline]
        pub fn value_path(&self) -> ::core::option::Option<value_pathView<'p>> {
            self.cursor.child(0usize).map(|c| value_pathView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(2usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        ///Child at position 3 as a typed view.
        #[inline]
        pub fn child_3(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(3usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            4usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct value_atomView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> value_atomView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> value_atomView<'p> {
        ///If variant `int_lit` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_int_lit(&self) -> ::core::option::Option<int_litView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor.child(0).map(|c| int_litView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `int_lit` (branch 0) was chosen.
        #[inline]
        pub fn is_int_lit(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `float_lit` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_float_lit(&self) -> ::core::option::Option<float_litView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor.child(0).map(|c| float_litView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `float_lit` (branch 1) was chosen.
        #[inline]
        pub fn is_float_lit(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If variant `bool_lit` (branch 2) was chosen, return its child view.
        #[inline]
        pub fn as_bool_lit(&self) -> ::core::option::Option<bool_litView<'p>> {
            if self.cursor.meta_idx() == 2u8 {
                self.cursor.child(0).map(|c| bool_litView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `bool_lit` (branch 2) was chosen.
        #[inline]
        pub fn is_bool_lit(&self) -> bool {
            self.cursor.meta_idx() == 2u8
        }
        ///If variant `string_lit` (branch 3) was chosen, return its child view.
        #[inline]
        pub fn as_string_lit(&self) -> ::core::option::Option<string_litView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor.child(0).map(|c| string_litView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `string_lit` (branch 3) was chosen.
        #[inline]
        pub fn is_string_lit(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If variant `value_fn_call` (branch 4) was chosen, return its child view.
        #[inline]
        pub fn as_value_fn_call(&self) -> ::core::option::Option<value_fn_callView<'p>> {
            if self.cursor.meta_idx() == 4u8 {
                self.cursor
                    .child(0)
                    .map(|c| value_fn_callView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `value_fn_call` (branch 4) was chosen.
        #[inline]
        pub fn is_value_fn_call(&self) -> bool {
            self.cursor.meta_idx() == 4u8
        }
        ///If variant `value_input` (branch 5) was chosen, return its child view.
        #[inline]
        pub fn as_value_input(&self) -> ::core::option::Option<value_inputView<'p>> {
            if self.cursor.meta_idx() == 5u8 {
                self.cursor.child(0).map(|c| value_inputView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `value_input` (branch 5) was chosen.
        #[inline]
        pub fn is_value_input(&self) -> bool {
            self.cursor.meta_idx() == 5u8
        }
        ///If variant `value_path` (branch 6) was chosen, return its child view.
        #[inline]
        pub fn as_value_path(&self) -> ::core::option::Option<value_pathView<'p>> {
            if self.cursor.meta_idx() == 6u8 {
                self.cursor.child(0).map(|c| value_pathView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `value_path` (branch 6) was chosen.
        #[inline]
        pub fn is_value_path(&self) -> bool {
            self.cursor.meta_idx() == 6u8
        }
        ///If variant `branch_7` (branch 7) was chosen, return its child view.
        #[inline]
        pub fn as_branch_7(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 7u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_7` (branch 7) was chosen.
        #[inline]
        pub fn is_branch_7(&self) -> bool {
            self.cursor.meta_idx() == 7u8
        }
        ///If sub-variant `value_atom_0` was chosen (branch 0), return its child view.
        #[inline]
        pub fn as_value_atom_0(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_value_atom_0(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If sub-variant `value_atom_1` was chosen (branch 1), return its child view.
        #[inline]
        pub fn as_value_atom_1(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_value_atom_1(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If sub-variant `value_atom_2` was chosen (branch 2), return its child view.
        #[inline]
        pub fn as_value_atom_2(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 2u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_value_atom_2(&self) -> bool {
            self.cursor.meta_idx() == 2u8
        }
        ///If sub-variant `value_atom_2_sv1` was chosen (branch 3), return its child view.
        #[inline]
        pub fn as_value_atom_2_sv1(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_value_atom_2_sv1(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If sub-variant `value_atom_2_sv2` was chosen (branch 5), return its child view.
        #[inline]
        pub fn as_value_atom_2_sv2(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 5u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_value_atom_2_sv2(&self) -> bool {
            self.cursor.meta_idx() == 5u8
        }
        ///If sub-variant `value_atom_2_sv3` was chosen (branch 6), return its child view.
        #[inline]
        pub fn as_value_atom_2_sv3(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 6u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_value_atom_2_sv3(&self) -> bool {
            self.cursor.meta_idx() == 6u8
        }
        ///If sub-variant `value_atom_3` was chosen (branch 7), return its child view.
        #[inline]
        pub fn as_value_atom_3(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 7u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_value_atom_3(&self) -> bool {
            self.cursor.meta_idx() == 7u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
    }
    /// Typed value enum — payload-eligible branches carry typed
    /// values directly; non-eligible branches wrap a cursor view.
    pub enum value_atomValue<'p> {
        int_lit(i64),
        float_lit(f64),
        bool_lit(((u32, u32))),
        string_lit(((u32, u32))),
        value_fn_call(BbnfBootstrapNodeView<'p>),
        value_input(&'p str),
        value_path(&'p str),
        branch_7(BbnfBootstrapNodeView<'p>),
    }
    impl<'p> value_atomView<'p> {
        /// Decode the chosen branch's value. Payload-eligible
        /// branches return typed scalars/aggregates; other
        /// branches return cursor-wrapped sub-views.
        #[inline]
        pub fn value(&self) -> ::core::option::Option<value_atomValue<'p>> {
            match self.cursor.meta_idx() {
                0u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = __tape
                        .payload_i64(__rec)
                        .unwrap_or(<i64 as ::core::default::Default>::default());
                    Some(value_atomValue::int_lit(__value))
                }
                1u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = __tape
                        .payload_f64(__rec)
                        .unwrap_or(<f64 as ::core::default::Default>::default());
                    Some(value_atomValue::float_lit(__value))
                }
                2u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_bytes(__rec, 8usize) {
                        Some(__bytes) => {
                            ({
                                let __raw = u64::from_le_bytes(
                                    <[u8; 8]>::try_from(&__bytes[0usize..8usize]).unwrap(),
                                );
                                (__raw as u32, (__raw >> 32) as u32)
                            })
                        }
                        None => ((0_u32, 0_u32)),
                    };
                    Some(value_atomValue::bool_lit(__value))
                }
                3u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_bytes(__rec, 8usize) {
                        Some(__bytes) => {
                            ({
                                let __raw = u64::from_le_bytes(
                                    <[u8; 8]>::try_from(&__bytes[0usize..8usize]).unwrap(),
                                );
                                (__raw as u32, (__raw >> 32) as u32)
                            })
                        }
                        None => ((0_u32, 0_u32)),
                    };
                    Some(value_atomValue::string_lit(__value))
                }
                4u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        value_atomValue::value_fn_call(
                            BbnfBootstrapNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                5u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_Span(__rec) {
                        Some((lo, hi)) => &self.input[lo as usize..hi as usize],
                        None => {
                            let (lo, hi) = __cursor.span();
                            &self.input[lo as usize..hi as usize]
                        }
                    };
                    Some(value_atomValue::value_input(__value))
                }
                6u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_Span(__rec) {
                        Some((lo, hi)) => &self.input[lo as usize..hi as usize],
                        None => {
                            let (lo, hi) = __cursor.span();
                            &self.input[lo as usize..hi as usize]
                        }
                    };
                    Some(value_atomValue::value_path(__value))
                }
                7u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        value_atomValue::branch_7(
                            BbnfBootstrapNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                _ => None,
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct mul_opView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> mul_opView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> mul_opView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct add_opView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> add_opView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> add_opView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct cmp_opView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> cmp_opView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> cmp_opView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct value_unaryView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> value_unaryView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> value_unaryView<'p> {
        ///If variant `branch_0` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_branch_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_0` (branch 0) was chosen.
        #[inline]
        pub fn is_branch_0(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `value_atom` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_value_atom(&self) -> ::core::option::Option<value_atomView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor.child(0).map(|c| value_atomView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `value_atom` (branch 1) was chosen.
        #[inline]
        pub fn is_value_atom(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If sub-variant `value_unary_0` was chosen (branch 0), return its child view.
        #[inline]
        pub fn as_value_unary_0(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_value_unary_0(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct value_mulView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> value_mulView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> value_mulView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<value_unaryView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| value_unaryView::from_cursor(c, self.input))
        }
        ///The `value_unary` child as a typed view.
        #[inline]
        pub fn value_unary(&self) -> ::core::option::Option<value_unaryView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| value_unaryView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct value_addView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> value_addView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> value_addView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<value_mulView<'p>> {
            self.cursor.child(0usize).map(|c| value_mulView::from_cursor(c, self.input))
        }
        ///The `value_mul` child as a typed view.
        #[inline]
        pub fn value_mul(&self) -> ::core::option::Option<value_mulView<'p>> {
            self.cursor.child(0usize).map(|c| value_mulView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct value_cmpView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> value_cmpView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> value_cmpView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<value_addView<'p>> {
            self.cursor.child(0usize).map(|c| value_addView::from_cursor(c, self.input))
        }
        ///The `value_add` child as a typed view.
        #[inline]
        pub fn value_add(&self) -> ::core::option::Option<value_addView<'p>> {
            self.cursor.child(0usize).map(|c| value_addView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct value_andView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> value_andView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> value_andView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<value_cmpView<'p>> {
            self.cursor.child(0usize).map(|c| value_cmpView::from_cursor(c, self.input))
        }
        ///The `value_cmp` child as a typed view.
        #[inline]
        pub fn value_cmp(&self) -> ::core::option::Option<value_cmpView<'p>> {
            self.cursor.child(0usize).map(|c| value_cmpView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct value_orView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> value_orView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> value_orView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<value_andView<'p>> {
            self.cursor.child(0usize).map(|c| value_andView::from_cursor(c, self.input))
        }
        ///The `value_and` child as a typed view.
        #[inline]
        pub fn value_and(&self) -> ::core::option::Option<value_andView<'p>> {
            self.cursor.child(0usize).map(|c| value_andView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct value_closureView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> value_closureView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> value_closureView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<value_identView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| value_identView::from_cursor(c, self.input))
        }
        ///The `value_ident` child as a typed view.
        #[inline]
        pub fn value_ident(&self) -> ::core::option::Option<value_identView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| value_identView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(2usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        ///Child at position 3 as a typed view.
        #[inline]
        pub fn child_3(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(3usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        ///Child at position 4 as a typed view.
        #[inline]
        pub fn child_4(&self) -> ::core::option::Option<value_exprView<'p>> {
            self.cursor.child(4usize).map(|c| value_exprView::from_cursor(c, self.input))
        }
        ///The `value_expr` child as a typed view.
        #[inline]
        pub fn value_expr(&self) -> ::core::option::Option<value_exprView<'p>> {
            self.cursor.child(4usize).map(|c| value_exprView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            5usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct value_exprView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> value_exprView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> value_exprView<'p> {
        ///If variant `value_closure` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_value_closure(&self) -> ::core::option::Option<value_closureView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| value_closureView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `value_closure` (branch 0) was chosen.
        #[inline]
        pub fn is_value_closure(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `value_or` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_value_or(&self) -> ::core::option::Option<value_orView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor.child(0).map(|c| value_orView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `value_or` (branch 1) was chosen.
        #[inline]
        pub fn is_value_or(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct type_annotationView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> type_annotationView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> type_annotationView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<type_nameView<'p>> {
            self.cursor.child(0usize).map(|c| type_nameView::from_cursor(c, self.input))
        }
        ///The `type_name` child as a typed view.
        #[inline]
        pub fn type_name(&self) -> ::core::option::Option<type_nameView<'p>> {
            self.cursor.child(0usize).map(|c| type_nameView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            1usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct type_nameView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> type_nameView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> type_nameView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct identifierView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> identifierView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> identifierView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct literalView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> literalView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> literalView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct regexView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> regexView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> regexView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct big_commentView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> big_commentView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> big_commentView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct commentView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> commentView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> commentView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct lhsView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> lhsView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> lhsView<'p> {
        /// The source text matched by this leaf rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// Get the sub-span value as a string slice.
        ///
        /// Payload-first: reads the packed (lo, hi) u32 pair from
        /// the tape payload buffer in O(1). Falls back to the
        /// record's own span text if no payload is present.
        #[inline]
        pub fn value(&self) -> &'p str {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            if let Some((lo, hi)) = tape.payload_Span(rec) {
                return &self.input[lo as usize..hi as usize];
            }
            self.span_text()
        }
        /// Alias for backward compatibility. Prefer `.value()`.
        #[inline]
        pub fn as_span(&self) -> &'p str {
            self.value()
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct call_argView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> call_argView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> call_argView<'p> {
        /// Iterator over each repetition element as a typed view.
        #[inline]
        pub fn iter(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The number of elements in this repetition.
        #[inline]
        pub fn len(&self) -> usize {
            self.cursor.child_count()
        }
        /// Whether this repetition matched zero elements.
        #[inline]
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }
        /// The i-th element as a typed view, if present.
        #[inline]
        pub fn get(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct termView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> termView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> termView<'p> {
        ///If variant `branch_0` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_branch_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_0` (branch 0) was chosen.
        #[inline]
        pub fn is_branch_0(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `branch_1` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_branch_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_1` (branch 1) was chosen.
        #[inline]
        pub fn is_branch_1(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If variant `branch_2` (branch 2) was chosen, return its child view.
        #[inline]
        pub fn as_branch_2(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 2u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_2` (branch 2) was chosen.
        #[inline]
        pub fn is_branch_2(&self) -> bool {
            self.cursor.meta_idx() == 2u8
        }
        ///If variant `literal` (branch 3) was chosen, return its child view.
        #[inline]
        pub fn as_literal(&self) -> ::core::option::Option<literalView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor.child(0).map(|c| literalView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `literal` (branch 3) was chosen.
        #[inline]
        pub fn is_literal(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If variant `regex` (branch 4) was chosen, return its child view.
        #[inline]
        pub fn as_regex(&self) -> ::core::option::Option<regexView<'p>> {
            if self.cursor.meta_idx() == 4u8 {
                self.cursor.child(0).map(|c| regexView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `regex` (branch 4) was chosen.
        #[inline]
        pub fn is_regex(&self) -> bool {
            self.cursor.meta_idx() == 4u8
        }
        ///If variant `branch_5` (branch 5) was chosen, return its child view.
        #[inline]
        pub fn as_branch_5(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 5u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_5` (branch 5) was chosen.
        #[inline]
        pub fn is_branch_5(&self) -> bool {
            self.cursor.meta_idx() == 5u8
        }
        ///If variant `branch_6` (branch 6) was chosen, return its child view.
        #[inline]
        pub fn as_branch_6(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 6u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_6` (branch 6) was chosen.
        #[inline]
        pub fn is_branch_6(&self) -> bool {
            self.cursor.meta_idx() == 6u8
        }
        ///If variant `branch_7` (branch 7) was chosen, return its child view.
        #[inline]
        pub fn as_branch_7(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 7u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_7` (branch 7) was chosen.
        #[inline]
        pub fn is_branch_7(&self) -> bool {
            self.cursor.meta_idx() == 7u8
        }
        ///If variant `branch_8` (branch 8) was chosen, return its child view.
        #[inline]
        pub fn as_branch_8(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 8u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `branch_8` (branch 8) was chosen.
        #[inline]
        pub fn is_branch_8(&self) -> bool {
            self.cursor.meta_idx() == 8u8
        }
        ///If sub-variant `term_0` was chosen (branch 0), return its child view.
        #[inline]
        pub fn as_term_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_term_0(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If sub-variant `term_0_sv1` was chosen (branch 1), return its child view.
        #[inline]
        pub fn as_term_0_sv1(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_term_0_sv1(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If sub-variant `term_1` was chosen (branch 2), return its child view.
        #[inline]
        pub fn as_term_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 2u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_term_1(&self) -> bool {
            self.cursor.meta_idx() == 2u8
        }
        ///If sub-variant `term_0_sv2` was chosen (branch 3), return its child view.
        #[inline]
        pub fn as_term_0_sv2(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_term_0_sv2(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If sub-variant `term_0_sv3` was chosen (branch 4), return its child view.
        #[inline]
        pub fn as_term_0_sv3(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 4u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_term_0_sv3(&self) -> bool {
            self.cursor.meta_idx() == 4u8
        }
        ///If sub-variant `term_2` was chosen (branch 5), return its child view.
        #[inline]
        pub fn as_term_2(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 5u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_term_2(&self) -> bool {
            self.cursor.meta_idx() == 5u8
        }
        ///If sub-variant `term_2_sv4` was chosen (branch 6), return its child view.
        #[inline]
        pub fn as_term_2_sv4(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 6u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_term_2_sv4(&self) -> bool {
            self.cursor.meta_idx() == 6u8
        }
        ///If sub-variant `term_2_sv5` was chosen (branch 7), return its child view.
        #[inline]
        pub fn as_term_2_sv5(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 7u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_term_2_sv5(&self) -> bool {
            self.cursor.meta_idx() == 7u8
        }
        ///If sub-variant `term_2_sv6` was chosen (branch 8), return its child view.
        #[inline]
        pub fn as_term_2_sv6(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 8u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_term_2_sv6(&self) -> bool {
            self.cursor.meta_idx() == 8u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
    }
    /// Typed value enum — payload-eligible branches carry typed
    /// values directly; non-eligible branches wrap a cursor view.
    pub enum termValue<'p> {
        branch_0(BbnfBootstrapNodeView<'p>),
        branch_1(BbnfBootstrapNodeView<'p>),
        branch_2(BbnfBootstrapNodeView<'p>),
        literal(((u32, u32))),
        regex(((u32, u32))),
        branch_5(BbnfBootstrapNodeView<'p>),
        branch_6(BbnfBootstrapNodeView<'p>),
        branch_7(BbnfBootstrapNodeView<'p>),
        branch_8(BbnfBootstrapNodeView<'p>),
    }
    impl<'p> termView<'p> {
        /// Decode the chosen branch's value. Payload-eligible
        /// branches return typed scalars/aggregates; other
        /// branches return cursor-wrapped sub-views.
        #[inline]
        pub fn value(&self) -> ::core::option::Option<termValue<'p>> {
            match self.cursor.meta_idx() {
                0u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        termValue::branch_0(
                            BbnfBootstrapNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                1u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        termValue::branch_1(
                            BbnfBootstrapNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                2u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        termValue::branch_2(
                            BbnfBootstrapNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                3u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_bytes(__rec, 8usize) {
                        Some(__bytes) => {
                            ({
                                let __raw = u64::from_le_bytes(
                                    <[u8; 8]>::try_from(&__bytes[0usize..8usize]).unwrap(),
                                );
                                (__raw as u32, (__raw >> 32) as u32)
                            })
                        }
                        None => ((0_u32, 0_u32)),
                    };
                    Some(termValue::literal(__value))
                }
                4u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_bytes(__rec, 8usize) {
                        Some(__bytes) => {
                            ({
                                let __raw = u64::from_le_bytes(
                                    <[u8; 8]>::try_from(&__bytes[0usize..8usize]).unwrap(),
                                );
                                (__raw as u32, (__raw >> 32) as u32)
                            })
                        }
                        None => ((0_u32, 0_u32)),
                    };
                    Some(termValue::regex(__value))
                }
                5u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        termValue::branch_5(
                            BbnfBootstrapNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                6u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        termValue::branch_6(
                            BbnfBootstrapNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                7u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        termValue::branch_7(
                            BbnfBootstrapNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                8u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        termValue::branch_8(
                            BbnfBootstrapNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                _ => None,
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct modifierView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> modifierView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> modifierView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct factorView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> factorView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> factorView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(2usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            3usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct mapped_factorView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> mapped_factorView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> mapped_factorView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<factorView<'p>> {
            self.cursor.child(0usize).map(|c| factorView::from_cursor(c, self.input))
        }
        ///The `factor` child as a typed view.
        #[inline]
        pub fn factor(&self) -> ::core::option::Option<factorView<'p>> {
            self.cursor.child(0usize).map(|c| factorView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct binary_operatorsView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> binary_operatorsView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> binary_operatorsView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct binary_factorView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> binary_factorView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> binary_factorView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<mapped_factorView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| mapped_factorView::from_cursor(c, self.input))
        }
        ///The `mapped_factor` child as a typed view.
        #[inline]
        pub fn mapped_factor(&self) -> ::core::option::Option<mapped_factorView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| mapped_factorView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct concatenationView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> concatenationView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> concatenationView<'p> {
        /// Iterator over each repetition element as a typed view.
        #[inline]
        pub fn iter(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The number of elements in this repetition.
        #[inline]
        pub fn len(&self) -> usize {
            self.cursor.child_count()
        }
        /// Whether this repetition matched zero elements.
        #[inline]
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }
        /// The i-th element as a typed view, if present.
        #[inline]
        pub fn get(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct alternationView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> alternationView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> alternationView<'p> {
        /// Iterator over each repetition element as a typed view.
        #[inline]
        pub fn iter(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The number of elements in this repetition.
        #[inline]
        pub fn len(&self) -> usize {
            self.cursor.child_count()
        }
        /// Whether this repetition matched zero elements.
        #[inline]
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }
        /// The i-th element as a typed view, if present.
        #[inline]
        pub fn get(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct closureView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> closureView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> closureView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<identifierView<'p>> {
            self.cursor.child(1usize).map(|c| identifierView::from_cursor(c, self.input))
        }
        ///The `identifier` child as a typed view.
        #[inline]
        pub fn identifier(&self) -> ::core::option::Option<identifierView<'p>> {
            self.cursor.child(1usize).map(|c| identifierView::from_cursor(c, self.input))
        }
        ///Child at position 2 as a typed view.
        #[inline]
        pub fn child_2(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(2usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        ///Child at position 3 as a typed view.
        #[inline]
        pub fn child_3(&self) -> ::core::option::Option<rhsView<'p>> {
            self.cursor.child(3usize).map(|c| rhsView::from_cursor(c, self.input))
        }
        ///The `rhs` child as a typed view.
        #[inline]
        pub fn rhs(&self) -> ::core::option::Option<rhsView<'p>> {
            self.cursor.child(3usize).map(|c| rhsView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            4usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct rhsView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> rhsView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> rhsView<'p> {
        ///If variant `closure` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_closure(&self) -> ::core::option::Option<closureView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor.child(0).map(|c| closureView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `closure` (branch 0) was chosen.
        #[inline]
        pub fn is_closure(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `alternation` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_alternation(&self) -> ::core::option::Option<alternationView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor.child(0).map(|c| alternationView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `alternation` (branch 1) was chosen.
        #[inline]
        pub fn is_alternation(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct ruleView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> ruleView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> ruleView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<lhsView<'p>> {
            self.cursor.child(0usize).map(|c| lhsView::from_cursor(c, self.input))
        }
        ///The `lhs` child as a typed view.
        #[inline]
        pub fn lhs(&self) -> ::core::option::Option<lhsView<'p>> {
            self.cursor.child(0usize).map(|c| lhsView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct import_pathView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> import_pathView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> import_pathView<'p> {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ((u32, u32)) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, 8usize) {
                Some(__bytes) => {
                    ({
                        let __raw = u64::from_le_bytes(
                            <[u8; 8]>::try_from(&__bytes[0usize..8usize])
                                .expect("aggregate slice is 8 bytes"),
                        );
                        (__raw as u32, (__raw >> 32) as u32)
                    })
                }
                None => ((0_u32, 0_u32)),
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct import_itemsView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> import_itemsView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> import_itemsView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            1usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct import_directiveView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> import_directiveView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> import_directiveView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            1usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct recover_directiveView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> recover_directiveView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> recover_directiveView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            1usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct pretty_hintView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> pretty_hintView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> pretty_hintView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<identifierView<'p>> {
            self.cursor.child(0usize).map(|c| identifierView::from_cursor(c, self.input))
        }
        ///The `identifier` child as a typed view.
        #[inline]
        pub fn identifier(&self) -> ::core::option::Option<identifierView<'p>> {
            self.cursor.child(0usize).map(|c| identifierView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct pretty_directiveView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> pretty_directiveView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> pretty_directiveView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct ws_directiveView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> ws_directiveView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> ws_directiveView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            1usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct token_directiveView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> token_directiveView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> token_directiveView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            1usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct debug_directiveView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> debug_directiveView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> debug_directiveView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            1usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct host_directiveView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> host_directiveView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> host_directiveView<'p> {
        ///Child at position 0 as a typed view.
        #[inline]
        pub fn child_0(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        ///Child at position 1 as a typed view.
        #[inline]
        pub fn child_1(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(1usize)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            2usize
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct directiveView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> directiveView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> directiveView<'p> {
        ///If variant `import_directive` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_import_directive(
            &self,
        ) -> ::core::option::Option<import_directiveView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| import_directiveView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `import_directive` (branch 0) was chosen.
        #[inline]
        pub fn is_import_directive(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `recover_directive` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_recover_directive(
            &self,
        ) -> ::core::option::Option<recover_directiveView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor
                    .child(0)
                    .map(|c| recover_directiveView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `recover_directive` (branch 1) was chosen.
        #[inline]
        pub fn is_recover_directive(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If variant `pretty_directive` (branch 2) was chosen, return its child view.
        #[inline]
        pub fn as_pretty_directive(
            &self,
        ) -> ::core::option::Option<pretty_directiveView<'p>> {
            if self.cursor.meta_idx() == 2u8 {
                self.cursor
                    .child(0)
                    .map(|c| pretty_directiveView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `pretty_directive` (branch 2) was chosen.
        #[inline]
        pub fn is_pretty_directive(&self) -> bool {
            self.cursor.meta_idx() == 2u8
        }
        ///If variant `ws_directive` (branch 3) was chosen, return its child view.
        #[inline]
        pub fn as_ws_directive(&self) -> ::core::option::Option<ws_directiveView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor
                    .child(0)
                    .map(|c| ws_directiveView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `ws_directive` (branch 3) was chosen.
        #[inline]
        pub fn is_ws_directive(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If variant `token_directive` (branch 4) was chosen, return its child view.
        #[inline]
        pub fn as_token_directive(
            &self,
        ) -> ::core::option::Option<token_directiveView<'p>> {
            if self.cursor.meta_idx() == 4u8 {
                self.cursor
                    .child(0)
                    .map(|c| token_directiveView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `token_directive` (branch 4) was chosen.
        #[inline]
        pub fn is_token_directive(&self) -> bool {
            self.cursor.meta_idx() == 4u8
        }
        ///If variant `debug_directive` (branch 5) was chosen, return its child view.
        #[inline]
        pub fn as_debug_directive(
            &self,
        ) -> ::core::option::Option<debug_directiveView<'p>> {
            if self.cursor.meta_idx() == 5u8 {
                self.cursor
                    .child(0)
                    .map(|c| debug_directiveView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `debug_directive` (branch 5) was chosen.
        #[inline]
        pub fn is_debug_directive(&self) -> bool {
            self.cursor.meta_idx() == 5u8
        }
        ///If variant `host_directive` (branch 6) was chosen, return its child view.
        #[inline]
        pub fn as_host_directive(
            &self,
        ) -> ::core::option::Option<host_directiveView<'p>> {
            if self.cursor.meta_idx() == 6u8 {
                self.cursor
                    .child(0)
                    .map(|c| host_directiveView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `host_directive` (branch 6) was chosen.
        #[inline]
        pub fn is_host_directive(&self) -> bool {
            self.cursor.meta_idx() == 6u8
        }
        ///If sub-variant `directive_0` was chosen (branch 0), return its child view.
        #[inline]
        pub fn as_directive_0(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_directive_0(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If sub-variant `directive_0_sv1` was chosen (branch 3), return its child view.
        #[inline]
        pub fn as_directive_0_sv1(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_directive_0_sv1(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If sub-variant `directive_0_sv2` was chosen (branch 4), return its child view.
        #[inline]
        pub fn as_directive_0_sv2(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 4u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_directive_0_sv2(&self) -> bool {
            self.cursor.meta_idx() == 4u8
        }
        ///If sub-variant `directive_0_sv3` was chosen (branch 5), return its child view.
        #[inline]
        pub fn as_directive_0_sv3(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 5u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_directive_0_sv3(&self) -> bool {
            self.cursor.meta_idx() == 5u8
        }
        ///If sub-variant `directive_0_sv4` was chosen (branch 6), return its child view.
        #[inline]
        pub fn as_directive_0_sv4(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 6u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_directive_0_sv4(&self) -> bool {
            self.cursor.meta_idx() == 6u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
    }
    /// Typed value enum — payload-eligible branches carry typed
    /// values directly; non-eligible branches wrap a cursor view.
    pub enum directiveValue<'p> {
        import_directive(&'p str),
        recover_directive(BbnfBootstrapNodeView<'p>),
        pretty_directive(BbnfBootstrapNodeView<'p>),
        ws_directive(&'p str),
        token_directive(&'p str),
        debug_directive(&'p str),
        host_directive(&'p str),
    }
    impl<'p> directiveView<'p> {
        /// Decode the chosen branch's value. Payload-eligible
        /// branches return typed scalars/aggregates; other
        /// branches return cursor-wrapped sub-views.
        #[inline]
        pub fn value(&self) -> ::core::option::Option<directiveValue<'p>> {
            match self.cursor.meta_idx() {
                0u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_Span(__rec) {
                        Some((lo, hi)) => &self.input[lo as usize..hi as usize],
                        None => {
                            let (lo, hi) = __cursor.span();
                            &self.input[lo as usize..hi as usize]
                        }
                    };
                    Some(directiveValue::import_directive(__value))
                }
                1u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        directiveValue::recover_directive(
                            BbnfBootstrapNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                2u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        directiveValue::pretty_directive(
                            BbnfBootstrapNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                3u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_Span(__rec) {
                        Some((lo, hi)) => &self.input[lo as usize..hi as usize],
                        None => {
                            let (lo, hi) = __cursor.span();
                            &self.input[lo as usize..hi as usize]
                        }
                    };
                    Some(directiveValue::ws_directive(__value))
                }
                4u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_Span(__rec) {
                        Some((lo, hi)) => &self.input[lo as usize..hi as usize],
                        None => {
                            let (lo, hi) = __cursor.span();
                            &self.input[lo as usize..hi as usize]
                        }
                    };
                    Some(directiveValue::token_directive(__value))
                }
                5u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_Span(__rec) {
                        Some((lo, hi)) => &self.input[lo as usize..hi as usize],
                        None => {
                            let (lo, hi) = __cursor.span();
                            &self.input[lo as usize..hi as usize]
                        }
                    };
                    Some(directiveValue::debug_directive(__value))
                }
                6u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_Span(__rec) {
                        Some((lo, hi)) => &self.input[lo as usize..hi as usize],
                        None => {
                            let (lo, hi) = __cursor.span();
                            &self.input[lo as usize..hi as usize]
                        }
                    };
                    Some(directiveValue::host_directive(__value))
                }
                _ => None,
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct grammar_itemView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> grammar_itemView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> grammar_itemView<'p> {
        ///If variant `comment` (branch 0) was chosen, return its child view.
        #[inline]
        pub fn as_comment(&self) -> ::core::option::Option<commentView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor.child(0).map(|c| commentView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `comment` (branch 0) was chosen.
        #[inline]
        pub fn is_comment(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If variant `big_comment` (branch 1) was chosen, return its child view.
        #[inline]
        pub fn as_big_comment(&self) -> ::core::option::Option<big_commentView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor.child(0).map(|c| big_commentView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `big_comment` (branch 1) was chosen.
        #[inline]
        pub fn is_big_comment(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        ///If variant `directive` (branch 2) was chosen, return its child view.
        #[inline]
        pub fn as_directive(&self) -> ::core::option::Option<directiveView<'p>> {
            if self.cursor.meta_idx() == 2u8 {
                self.cursor.child(0).map(|c| directiveView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `directive` (branch 2) was chosen.
        #[inline]
        pub fn is_directive(&self) -> bool {
            self.cursor.meta_idx() == 2u8
        }
        ///If variant `rule` (branch 3) was chosen, return its child view.
        #[inline]
        pub fn as_rule(&self) -> ::core::option::Option<ruleView<'p>> {
            if self.cursor.meta_idx() == 3u8 {
                self.cursor.child(0).map(|c| ruleView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        ///Returns `true` if variant `rule` (branch 3) was chosen.
        #[inline]
        pub fn is_rule(&self) -> bool {
            self.cursor.meta_idx() == 3u8
        }
        ///If sub-variant `grammar_item_0` was chosen (branch 0), return its child view.
        #[inline]
        pub fn as_grammar_item_0(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 0u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_grammar_item_0(&self) -> bool {
            self.cursor.meta_idx() == 0u8
        }
        ///If sub-variant `grammar_item_0_sv1` was chosen (branch 1), return its child view.
        #[inline]
        pub fn as_grammar_item_0_sv1(
            &self,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            if self.cursor.meta_idx() == 1u8 {
                self.cursor
                    .child(0)
                    .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
            } else {
                None
            }
        }
        #[inline]
        pub fn is_grammar_item_0_sv1(&self) -> bool {
            self.cursor.meta_idx() == 1u8
        }
        /// The chosen branch's child as a generic node view,
        /// regardless of which variant was selected.
        #[inline]
        pub fn chosen(&self) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(0)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
    }
    /// Typed value enum — payload-eligible branches carry typed
    /// values directly; non-eligible branches wrap a cursor view.
    pub enum grammar_itemValue<'p> {
        comment(((u32, u32))),
        big_comment(((u32, u32))),
        directive(BbnfBootstrapNodeView<'p>),
        rule(BbnfBootstrapNodeView<'p>),
    }
    impl<'p> grammar_itemView<'p> {
        /// Decode the chosen branch's value. Payload-eligible
        /// branches return typed scalars/aggregates; other
        /// branches return cursor-wrapped sub-views.
        #[inline]
        pub fn value(&self) -> ::core::option::Option<grammar_itemValue<'p>> {
            match self.cursor.meta_idx() {
                0u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_bytes(__rec, 8usize) {
                        Some(__bytes) => {
                            ({
                                let __raw = u64::from_le_bytes(
                                    <[u8; 8]>::try_from(&__bytes[0usize..8usize]).unwrap(),
                                );
                                (__raw as u32, (__raw >> 32) as u32)
                            })
                        }
                        None => ((0_u32, 0_u32)),
                    };
                    Some(grammar_itemValue::comment(__value))
                }
                1u8 => {
                    let __cursor = self.cursor.child(0).unwrap_or(self.cursor);
                    let __rec = __cursor.record();
                    let __tape = __cursor.tape();
                    let __value = match __tape.payload_bytes(__rec, 8usize) {
                        Some(__bytes) => {
                            ({
                                let __raw = u64::from_le_bytes(
                                    <[u8; 8]>::try_from(&__bytes[0usize..8usize]).unwrap(),
                                );
                                (__raw as u32, (__raw >> 32) as u32)
                            })
                        }
                        None => ((0_u32, 0_u32)),
                    };
                    Some(grammar_itemValue::big_comment(__value))
                }
                2u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        grammar_itemValue::directive(
                            BbnfBootstrapNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                3u8 => {
                    let __child = self.cursor.child(0)?;
                    Some(
                        grammar_itemValue::rule(
                            BbnfBootstrapNodeView::from_cursor(__child, self.input),
                        ),
                    )
                }
                _ => None,
            }
        }
    }
    /// Generated view over a tape record produced by this rule.
    #[derive(Clone, Copy, Debug)]
    pub struct grammarView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    impl<'p> grammarView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl<'p> grammarView<'p> {
        /// Iterator over each repetition element as a typed view.
        #[inline]
        pub fn iter(
            &self,
        ) -> impl ::core::iter::Iterator<Item = grammar_itemView<'p>> + 'p {
            let input = self.input;
            self.cursor.children().map(move |c| grammar_itemView::from_cursor(c, input))
        }
        /// The number of elements in this repetition.
        #[inline]
        pub fn len(&self) -> usize {
            self.cursor.child_count()
        }
        /// Whether this repetition matched zero elements.
        #[inline]
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }
        /// The i-th element as a typed view, if present.
        #[inline]
        pub fn get(&self, i: usize) -> ::core::option::Option<grammar_itemView<'p>> {
            self.cursor.child(i).map(|c| grammar_itemView::from_cursor(c, self.input))
        }
    }
    /// Generic node view over any tape record for this grammar.
    #[derive(Clone, Copy, Debug)]
    pub struct BbnfBootstrapNodeView<'p> {
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    }
    /// Rule-identity discriminator for `NodeView::rule_kind`
    /// and per-rule view `rule_kind` accessors. One variant
    /// per non-transparent rule (in declaration order),
    /// followed by one variant per distinct sub-variant name
    /// from heterogeneous alt coercion, plus a fallback
    /// `Unknown` for records the discriminator table does
    /// not cover (leaf spans, alt branch indices, etc.).
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum BbnfBootstrapRuleKind {
        int_lit,
        float_lit,
        bool_lit,
        string_lit,
        value_ident,
        value_path,
        value_input,
        value_fn_call,
        value_atom,
        mul_op,
        add_op,
        cmp_op,
        value_unary,
        value_mul,
        value_add,
        value_cmp,
        value_and,
        value_or,
        value_closure,
        value_expr,
        type_annotation,
        type_name,
        identifier,
        literal,
        regex,
        big_comment,
        comment,
        lhs,
        call_arg,
        term,
        modifier,
        factor,
        mapped_factor,
        binary_operators,
        binary_factor,
        concatenation,
        alternation,
        closure,
        rhs,
        rule,
        import_path,
        import_items,
        import_directive,
        recover_directive,
        pretty_hint,
        pretty_directive,
        ws_directive,
        token_directive,
        debug_directive,
        host_directive,
        directive,
        grammar_item,
        grammar,
        value_atom_0,
        value_atom_1,
        value_atom_2,
        value_atom_3,
        value_unary_0,
        term_0,
        term_1,
        term_2,
        directive_0,
        grammar_item_0,
        /// Fallback for records whose variant_idx is not a
        /// known rule- or sub-variant discriminator.
        Unknown,
    }
    impl<'p> BbnfBootstrapNodeView<'p> {
        #[inline]
        pub fn new(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            offset: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self {
            Self {
                cursor: ::bbnf::runtime::tape::TapeCursor::new(tape, offset),
                input,
            }
        }
        #[inline]
        pub fn from_cursor(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> Self {
            Self { cursor, input }
        }
        #[inline]
        pub fn cursor(&self) -> ::bbnf::runtime::tape::TapeCursor<'p> {
            self.cursor
        }
        #[inline]
        pub fn input(&self) -> &'p str {
            self.input
        }
        #[inline]
        pub fn kind(&self) -> ::bbnf::runtime::tape::TapeKind {
            self.cursor.kind()
        }
        #[inline]
        pub fn span(&self) -> (u32, u32) {
            self.cursor.span()
        }
        #[inline]
        pub fn span_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
        #[inline]
        pub fn variant_idx(&self) -> u8 {
            self.cursor.variant_idx()
        }
        /// Dispatch on `variant_idx` to identify which rule
        /// (or sub-variant) produced this record.
        #[inline]
        pub fn rule_kind(&self) -> BbnfBootstrapRuleKind {
            match self.variant_idx() {
                0u8 => BbnfBootstrapRuleKind::int_lit,
                1u8 => BbnfBootstrapRuleKind::float_lit,
                2u8 => BbnfBootstrapRuleKind::bool_lit,
                3u8 => BbnfBootstrapRuleKind::string_lit,
                4u8 => BbnfBootstrapRuleKind::value_ident,
                5u8 => BbnfBootstrapRuleKind::value_path,
                6u8 => BbnfBootstrapRuleKind::value_input,
                7u8 => BbnfBootstrapRuleKind::value_fn_call,
                8u8 => BbnfBootstrapRuleKind::value_atom,
                9u8 => BbnfBootstrapRuleKind::mul_op,
                10u8 => BbnfBootstrapRuleKind::add_op,
                11u8 => BbnfBootstrapRuleKind::cmp_op,
                12u8 => BbnfBootstrapRuleKind::value_unary,
                13u8 => BbnfBootstrapRuleKind::value_mul,
                14u8 => BbnfBootstrapRuleKind::value_add,
                15u8 => BbnfBootstrapRuleKind::value_cmp,
                16u8 => BbnfBootstrapRuleKind::value_and,
                17u8 => BbnfBootstrapRuleKind::value_or,
                18u8 => BbnfBootstrapRuleKind::value_closure,
                19u8 => BbnfBootstrapRuleKind::value_expr,
                20u8 => BbnfBootstrapRuleKind::type_annotation,
                21u8 => BbnfBootstrapRuleKind::type_name,
                22u8 => BbnfBootstrapRuleKind::identifier,
                23u8 => BbnfBootstrapRuleKind::literal,
                24u8 => BbnfBootstrapRuleKind::regex,
                25u8 => BbnfBootstrapRuleKind::big_comment,
                26u8 => BbnfBootstrapRuleKind::comment,
                27u8 => BbnfBootstrapRuleKind::lhs,
                28u8 => BbnfBootstrapRuleKind::call_arg,
                29u8 => BbnfBootstrapRuleKind::term,
                30u8 => BbnfBootstrapRuleKind::modifier,
                31u8 => BbnfBootstrapRuleKind::factor,
                32u8 => BbnfBootstrapRuleKind::mapped_factor,
                33u8 => BbnfBootstrapRuleKind::binary_operators,
                34u8 => BbnfBootstrapRuleKind::binary_factor,
                35u8 => BbnfBootstrapRuleKind::concatenation,
                36u8 => BbnfBootstrapRuleKind::alternation,
                37u8 => BbnfBootstrapRuleKind::closure,
                38u8 => BbnfBootstrapRuleKind::rhs,
                39u8 => BbnfBootstrapRuleKind::rule,
                40u8 => BbnfBootstrapRuleKind::import_path,
                41u8 => BbnfBootstrapRuleKind::import_items,
                42u8 => BbnfBootstrapRuleKind::import_directive,
                43u8 => BbnfBootstrapRuleKind::recover_directive,
                44u8 => BbnfBootstrapRuleKind::pretty_hint,
                45u8 => BbnfBootstrapRuleKind::pretty_directive,
                46u8 => BbnfBootstrapRuleKind::ws_directive,
                47u8 => BbnfBootstrapRuleKind::token_directive,
                48u8 => BbnfBootstrapRuleKind::debug_directive,
                49u8 => BbnfBootstrapRuleKind::host_directive,
                50u8 => BbnfBootstrapRuleKind::directive,
                51u8 => BbnfBootstrapRuleKind::grammar_item,
                52u8 => BbnfBootstrapRuleKind::grammar,
                53u8 => BbnfBootstrapRuleKind::value_atom_0,
                54u8 => BbnfBootstrapRuleKind::value_atom_1,
                55u8 => BbnfBootstrapRuleKind::value_atom_2,
                56u8 => BbnfBootstrapRuleKind::value_atom_3,
                57u8 => BbnfBootstrapRuleKind::value_unary_0,
                58u8 => BbnfBootstrapRuleKind::term_0,
                59u8 => BbnfBootstrapRuleKind::term_1,
                60u8 => BbnfBootstrapRuleKind::term_2,
                61u8 => BbnfBootstrapRuleKind::directive_0,
                62u8 => BbnfBootstrapRuleKind::grammar_item_0,
                _ => BbnfBootstrapRuleKind::Unknown,
            }
        }
        /// Iterator over direct children as `NodeView`s.
        #[inline]
        pub fn children(
            &self,
        ) -> impl ::core::iter::Iterator<Item = BbnfBootstrapNodeView<'p>> + 'p {
            let input = self.input;
            self.cursor
                .children()
                .map(move |c| BbnfBootstrapNodeView::from_cursor(c, input))
        }
        /// The i-th direct child as a `NodeView`, if present.
        #[inline]
        pub fn child(
            &self,
            i: usize,
        ) -> ::core::option::Option<BbnfBootstrapNodeView<'p>> {
            self.cursor
                .child(i)
                .map(|c| BbnfBootstrapNodeView::from_cursor(c, self.input))
        }
        #[inline]
        pub fn is_recovered(&self) -> bool {
            self.cursor.kind().is_recovered()
        }
        /// Source-byte span as a `parse_that::Span<'p>` slice.
        /// Used by CST consumers that historically held
        /// `parse_that::Span` references (`RuleEntry::name_span`,
        /// `ImportedName::span`) alongside the view.
        #[inline]
        pub fn identifier_span(&self) -> ::parse_that::Span<'p> {
            let (lo, hi) = self.cursor.span();
            ::parse_that::Span::new(lo as usize, hi as usize, self.input)
        }
    }
    impl ::bbnf::runtime::Root for BbnfBootstrap {
        type View<'p> = grammarView<'p>;
        #[inline]
        fn make_view<'p>(
            tape: &'p ::bbnf::runtime::tape::Tape,
            input: &'p str,
            root: ::bbnf::runtime::tape::TapeOffset,
        ) -> Self::View<'p> {
            grammarView::new(tape, input, root)
        }
    }
    impl BbnfBootstrap {
        /// The name of the root rule for this grammar.
        #[inline]
        pub fn root_rule_name() -> &'static str {
            "grammar"
        }
    }
    /// DTA regex-scanner adapter — consults parse-that's
    /// cached DFA registry to match a regex pattern at
    /// `input[offset..]`.
    ///
    /// Zero-size: one shared `const` instance per grammar; the
    /// DTA driver's [`dta_run_into`] takes it by `&dyn`.
    struct DtaDfaScanner;
    impl ::bbnf::runtime::tape::RegexScanner for DtaDfaScanner {
        fn scan(
            &self,
            pattern: &str,
            input: &[u8],
            offset: usize,
        ) -> ::core::option::Option<u32> {
            let dfa = ::parse_that::cached_dfa(pattern);
            dfa.find_at(input, offset).map(|end| (end - offset) as u32)
        }
    }
    /// Module-level scanner singleton. `DtaDfaScanner` is a ZST;
    /// `const` binds the one-and-only value at compile time so
    /// every `parse()` call borrows the same instance rather
    /// than materialising a new stack local.
    const DTA_SCANNER: DtaDfaScanner = DtaDfaScanner;
    impl BbnfBootstrap {
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
                                        if ::parse_that::scan_hex_mut(state).is_none() {
                                            return None;
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
                                let __alt_ok = if __alt_ok.is_none() {
                                    state.offset = __save_alt;
                                    (|| -> Option<()> {
                                        {
                                            if ::parse_that::scan_digits_mut(state).is_none() {
                                                return None;
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
                                let _ = ::parse_that::scan_digits_star_mut(state);
                            }
                            if state.src_bytes.get(state.offset).copied() != Some(b'.') {
                                return None;
                            }
                            state.offset += 1;
                            {
                                if ::parse_that::scan_digits_mut(state).is_none() {
                                    return None;
                                }
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
                                        if ::parse_that::scan_digits_mut(state).is_none() {
                                            return None;
                                        }
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
        fn __bool_lit_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp0 = state.offset;
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
                            state.offset = __pretty_cp0;
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
        fn __value_ident_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __start = state.offset;
                    if ::parse_that::scan_ident(
                            state,
                            &::parse_that::DEFAULT_IDENT_CONFIG,
                        )
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
        fn __value_path_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __start = state.offset;
                        if ::parse_that::scan_ident(
                                state,
                                &::parse_that::DEFAULT_IDENT_CONFIG,
                            )
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
                        let mut __rep_count3 = 0usize;
                        while __rep_count3 < 4294967295 {
                            let __rep_cp4 = state.offset;
                            if !{
                                let __pretty_cp1 = state.offset;
                                let __pretty_bcp2 = __builder.checkpoint();
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
                                            if ::parse_that::scan_ident(
                                                    state,
                                                    &::parse_that::DEFAULT_IDENT_CONFIG,
                                                )
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
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp1;
                                    __builder.restore(__pretty_bcp2);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp4;
                                break;
                            }
                            if state.offset == __rep_cp4 {
                                break;
                            }
                            __rep_count3 += 1;
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
                        let mut __rep_count7 = 0usize;
                        while __rep_count7 < 4294967295 {
                            let __rep_cp8 = state.offset;
                            if !{
                                let __pretty_cp5 = state.offset;
                                let __pretty_bcp6 = __builder.checkpoint();
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
                                            if ::parse_that::scan_ident(
                                                    state,
                                                    &::parse_that::DEFAULT_IDENT_CONFIG,
                                                )
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
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp5;
                                    __builder.restore(__pretty_bcp6);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp8;
                                break;
                            }
                            if state.offset == __rep_cp8 {
                                break;
                            }
                            __rep_count7 += 1;
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
                            let __pretty_cp16 = state.offset;
                            let __pretty_bcp17 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    if !Self::__value_expr_prettify(state, __builder) {
                                        return false;
                                    }
                                    {
                                        let mut __rep_count14 = 0usize;
                                        while __rep_count14 < 4294967295 {
                                            let __rep_cp15 = state.offset;
                                            if !{
                                                let __pretty_cp12 = state.offset;
                                                let __pretty_bcp13 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        {
                                                            let __ows9 = state.offset;
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            let __ows10 = state.offset;
                                                            {
                                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                {
                                                                    return false;
                                                                }
                                                                state.offset += 1;
                                                                __builder.char(b',');
                                                            };
                                                            __builder.text_inline_ws(&state.src[__ows9..__ows10]);
                                                            let __ows11 = state.offset;
                                                            ::parse_that::trim_leading_whitespace_mut(state);
                                                            __builder.text_inline_ws(&state.src[__ows11..state.offset]);
                                                        };
                                                        if !Self::__value_expr_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp12;
                                                    __builder.restore(__pretty_bcp13);
                                                }
                                                __ok
                                            } {
                                                state.offset = __rep_cp15;
                                                break;
                                            }
                                            if state.offset == __rep_cp15 {
                                                break;
                                            }
                                            __rep_count14 += 1;
                                        }
                                    };
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp16;
                                __builder.restore(__pretty_bcp17);
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
        fn __value_atom_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp36 = state.offset;
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
                                                    if ::parse_that::scan_hex_mut(state).is_none() {
                                                        return None;
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
                                            let __alt_ok = if __alt_ok.is_none() {
                                                state.offset = __save_alt;
                                                (|| -> Option<()> {
                                                    {
                                                        if ::parse_that::scan_digits_mut(state).is_none() {
                                                            return None;
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
                            state.offset = __pretty_cp36;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp35 = state.offset;
                                let __ok = (|| -> bool {
                                    {
                                        let __start = state.offset;
                                        if {
                                            let __start = state.offset;
                                            let __result: Option<()> = (|| {
                                                {
                                                    let _ = ::parse_that::scan_digits_star_mut(state);
                                                }
                                                if state.src_bytes.get(state.offset).copied() != Some(b'.')
                                                {
                                                    return None;
                                                }
                                                state.offset += 1;
                                                {
                                                    if ::parse_that::scan_digits_mut(state).is_none() {
                                                        return None;
                                                    }
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
                                                            if ::parse_that::scan_digits_mut(state).is_none() {
                                                                return None;
                                                            }
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
                                    state.offset = __pretty_cp35;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp33 = state.offset;
                                        let __pretty_bcp34 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                if !{
                                                    let __pretty_cp18 = state.offset;
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
                                                        state.offset = __pretty_cp18;
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
                                            state.offset = __pretty_cp33;
                                            __builder.restore(__pretty_bcp34);
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp31 = state.offset;
                                                let __pretty_bcp32 = __builder.checkpoint();
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
                                                    state.offset = __pretty_cp31;
                                                    __builder.restore(__pretty_bcp32);
                                                }
                                                __ok
                                            } {
                                                {
                                                    if !{
                                                        let __pretty_cp29 = state.offset;
                                                        let __pretty_bcp30 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            if !Self::__value_fn_call_prettify(state, __builder) {
                                                                return false;
                                                            }
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp29;
                                                            __builder.restore(__pretty_bcp30);
                                                        }
                                                        __ok
                                                    } {
                                                        {
                                                            if !{
                                                                let __pretty_cp27 = state.offset;
                                                                let __pretty_bcp28 = __builder.checkpoint();
                                                                let __ok = (|| -> bool {
                                                                    if !Self::__value_input_prettify(state, __builder) {
                                                                        return false;
                                                                    }
                                                                    true
                                                                })();
                                                                if !__ok {
                                                                    state.offset = __pretty_cp27;
                                                                    __builder.restore(__pretty_bcp28);
                                                                }
                                                                __ok
                                                            } {
                                                                {
                                                                    if !{
                                                                        let __pretty_cp25 = state.offset;
                                                                        let __pretty_bcp26 = __builder.checkpoint();
                                                                        let __ok = (|| -> bool {
                                                                            if !Self::__value_path_prettify(state, __builder) {
                                                                                return false;
                                                                            }
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
                                                                                            if state.src_bytes.get(state.offset).copied() != Some(b'(')
                                                                                            {
                                                                                                return false;
                                                                                            }
                                                                                            state.offset += 1;
                                                                                            __builder.char(b'(');
                                                                                        };
                                                                                        {
                                                                                            if !{
                                                                                                let __pretty_cp21 = state.offset;
                                                                                                let __pretty_bcp22 = __builder.checkpoint();
                                                                                                let __ok = (|| -> bool {
                                                                                                    {
                                                                                                        let __ows19 = state.offset;
                                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                        __builder.text_inline_ws(&state.src[__ows19..state.offset]);
                                                                                                        if !Self::__value_expr_prettify(state, __builder) {
                                                                                                            return false;
                                                                                                        }
                                                                                                        let __ows20 = state.offset;
                                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                        __builder.text_inline_ws(&state.src[__ows20..state.offset]);
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
                                                                                    state.offset = __pretty_cp23;
                                                                                    __builder.restore(__pretty_bcp24);
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
        fn __mul_op_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp39 = state.offset;
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
                            state.offset = __pretty_cp39;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp38 = state.offset;
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
                                    state.offset = __pretty_cp38;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp37 = state.offset;
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
                                            state.offset = __pretty_cp37;
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
        fn __add_op_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp40 = state.offset;
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
                            state.offset = __pretty_cp40;
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
        fn __cmp_op_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp46 = state.offset;
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
                            state.offset = __pretty_cp46;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp45 = state.offset;
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
                                    state.offset = __pretty_cp45;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp44 = state.offset;
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
                                            state.offset = __pretty_cp44;
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp43 = state.offset;
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
                                                    state.offset = __pretty_cp43;
                                                }
                                                __ok
                                            } {
                                                {
                                                    if !{
                                                        let __pretty_cp42 = state.offset;
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
                                                            state.offset = __pretty_cp42;
                                                        }
                                                        __ok
                                                    } {
                                                        {
                                                            if !{
                                                                let __pretty_cp41 = state.offset;
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
                                                                    state.offset = __pretty_cp41;
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
        fn __value_unary_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp48 = state.offset;
                        let __pretty_bcp49 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            {
                                {
                                    if !{
                                        let __pretty_cp47 = state.offset;
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
                                            state.offset = __pretty_cp47;
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
                            state.offset = __pretty_cp48;
                            __builder.restore(__pretty_bcp49);
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
                        let mut __rep_count56 = 0usize;
                        while __rep_count56 < 4294967295 {
                            let __rep_cp57 = state.offset;
                            if !{
                                let __pretty_cp54 = state.offset;
                                let __pretty_bcp55 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp52 = state.offset;
                                                let __pretty_bcp53 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows50 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows50..state.offset]);
                                                        if !Self::__mul_op_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows51 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows51..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp52;
                                                    __builder.restore(__pretty_bcp53);
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
                                    state.offset = __pretty_cp54;
                                    __builder.restore(__pretty_bcp55);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp57;
                                break;
                            }
                            if state.offset == __rep_cp57 {
                                break;
                            }
                            __rep_count56 += 1;
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
                        let mut __rep_count65 = 0usize;
                        while __rep_count65 < 4294967295 {
                            let __rep_cp66 = state.offset;
                            if !{
                                let __pretty_cp63 = state.offset;
                                let __pretty_bcp64 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp61 = state.offset;
                                                let __pretty_bcp62 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows59 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows59..state.offset]);
                                                        {
                                                            if !{
                                                                let __pretty_cp58 = state.offset;
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
                                                                    state.offset = __pretty_cp58;
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
                                                        let __ows60 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows60..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp61;
                                                    __builder.restore(__pretty_bcp62);
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
                                    state.offset = __pretty_cp63;
                                    __builder.restore(__pretty_bcp64);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp66;
                                break;
                            }
                            if state.offset == __rep_cp66 {
                                break;
                            }
                            __rep_count65 += 1;
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
                        let mut __rep_count73 = 0usize;
                        while __rep_count73 < 4294967295 {
                            let __rep_cp74 = state.offset;
                            if !{
                                let __pretty_cp71 = state.offset;
                                let __pretty_bcp72 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp69 = state.offset;
                                                let __pretty_bcp70 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows67 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows67..state.offset]);
                                                        if !Self::__cmp_op_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows68 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder.text_inline_ws(&state.src[__ows68..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp69;
                                                    __builder.restore(__pretty_bcp70);
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
                                    state.offset = __pretty_cp71;
                                    __builder.restore(__pretty_bcp72);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp74;
                                break;
                            }
                            if state.offset == __rep_cp74 {
                                break;
                            }
                            __rep_count73 += 1;
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
                        let mut __rep_count80 = 0usize;
                        while __rep_count80 < 4294967295 {
                            let __rep_cp81 = state.offset;
                            if !{
                                let __pretty_cp78 = state.offset;
                                let __pretty_bcp79 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __ows75 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows76 = state.offset;
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
                                            __builder.text_inline_ws(&state.src[__ows75..__ows76]);
                                            let __ows77 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows77..state.offset]);
                                        };
                                        if !Self::__value_cmp_prettify(state, __builder) {
                                            return false;
                                        }
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp78;
                                    __builder.restore(__pretty_bcp79);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp81;
                                break;
                            }
                            if state.offset == __rep_cp81 {
                                break;
                            }
                            __rep_count80 += 1;
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
                        let mut __rep_count87 = 0usize;
                        while __rep_count87 < 4294967295 {
                            let __rep_cp88 = state.offset;
                            if !{
                                let __pretty_cp85 = state.offset;
                                let __pretty_bcp86 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __ows82 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows83 = state.offset;
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
                                            __builder.text_inline_ws(&state.src[__ows82..__ows83]);
                                            let __ows84 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows84..state.offset]);
                                        };
                                        if !Self::__value_and_prettify(state, __builder) {
                                            return false;
                                        }
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp85;
                                    __builder.restore(__pretty_bcp86);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp88;
                                break;
                            }
                            if state.offset == __rep_cp88 {
                                break;
                            }
                            __rep_count87 += 1;
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
                        if ::parse_that::scan_ident(
                                state,
                                &::parse_that::DEFAULT_IDENT_CONFIG,
                            )
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
                        let mut __rep_count94 = 0usize;
                        while __rep_count94 < 4294967295 {
                            let __rep_cp95 = state.offset;
                            if !{
                                let __pretty_cp92 = state.offset;
                                let __pretty_bcp93 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __ows89 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows90 = state.offset;
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b',');
                                            };
                                            __builder.text_inline_ws(&state.src[__ows89..__ows90]);
                                            let __ows91 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder.text_inline_ws(&state.src[__ows91..state.offset]);
                                        };
                                        {
                                            let __start = state.offset;
                                            if ::parse_that::scan_ident(
                                                    state,
                                                    &::parse_that::DEFAULT_IDENT_CONFIG,
                                                )
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
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp92;
                                    __builder.restore(__pretty_bcp93);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp95;
                                break;
                            }
                            if state.offset == __rep_cp95 {
                                break;
                            }
                            __rep_count94 += 1;
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
        fn __value_expr_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp96 = state.offset;
                        let __pretty_bcp97 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__value_closure_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp96;
                            __builder.restore(__pretty_bcp97);
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
        fn __type_annotation_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows98 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows99 = state.offset;
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b':') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b':');
                        };
                        __builder.text_inline_ws(&state.src[__ows98..__ows99]);
                        let __ows100 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows100..state.offset]);
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
        fn __type_name_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp111 = state.offset;
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
                            state.offset = __pretty_cp111;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp110 = state.offset;
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
                                    state.offset = __pretty_cp110;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp109 = state.offset;
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
                                            state.offset = __pretty_cp109;
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp108 = state.offset;
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
                                                    state.offset = __pretty_cp108;
                                                }
                                                __ok
                                            } {
                                                {
                                                    if !{
                                                        let __pretty_cp107 = state.offset;
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
                                                            state.offset = __pretty_cp107;
                                                        }
                                                        __ok
                                                    } {
                                                        {
                                                            if !{
                                                                let __pretty_cp106 = state.offset;
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
                                                                    state.offset = __pretty_cp106;
                                                                }
                                                                __ok
                                                            } {
                                                                {
                                                                    if !{
                                                                        let __pretty_cp105 = state.offset;
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
                                                                            state.offset = __pretty_cp105;
                                                                        }
                                                                        __ok
                                                                    } {
                                                                        {
                                                                            if !{
                                                                                let __pretty_cp104 = state.offset;
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
                                                                                    state.offset = __pretty_cp104;
                                                                                }
                                                                                __ok
                                                                            } {
                                                                                {
                                                                                    if !{
                                                                                        let __pretty_cp103 = state.offset;
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
                                                                                            state.offset = __pretty_cp103;
                                                                                        }
                                                                                        __ok
                                                                                    } {
                                                                                        {
                                                                                            if !{
                                                                                                let __pretty_cp102 = state.offset;
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
                                                                                                    state.offset = __pretty_cp102;
                                                                                                }
                                                                                                __ok
                                                                                            } {
                                                                                                {
                                                                                                    if !{
                                                                                                        let __pretty_cp101 = state.offset;
                                                                                                        let __ok = (|| -> bool {
                                                                                                            {
                                                                                                                let __start = state.offset;
                                                                                                                if ::parse_that::scan_ident(
                                                                                                                        state,
                                                                                                                        &::parse_that::DEFAULT_IDENT_CONFIG,
                                                                                                                    )
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
                                                                                                            state.offset = __pretty_cp101;
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
        fn __identifier_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __start = state.offset;
                    if ::parse_that::scan_ident(
                            state,
                            &::parse_that::DEFAULT_IDENT_CONFIG,
                        )
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
        fn __literal_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp116 = state.offset;
                        let __pretty_bcp117 = __builder.checkpoint();
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
                            state.offset = __pretty_cp116;
                            __builder.restore(__pretty_bcp117);
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp114 = state.offset;
                                let __pretty_bcp115 = __builder.checkpoint();
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
                                    state.offset = __pretty_cp114;
                                    __builder.restore(__pretty_bcp115);
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp112 = state.offset;
                                        let __pretty_bcp113 = __builder.checkpoint();
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
                                            state.offset = __pretty_cp112;
                                            __builder.restore(__pretty_bcp113);
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
        fn __big_comment_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
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
                                                ({
                                                    static __LO_LUT: [u8; 16] = [
                                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                                    ];
                                                    static __HI_LUT: [u8; 16] = [
                                                        0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                    ];
                                                    ::parse_that::find_next_structural_from(
                                                            &state.src_bytes,
                                                            __start,
                                                            &__LO_LUT,
                                                            &__HI_LUT,
                                                        )
                                                        .map(|(pos, _)| pos - __start)
                                                })
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
        fn __comment_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
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
                                            let __scan = if __start >= state.src_bytes.len() {
                                                0
                                            } else {
                                                ({
                                                    static __LO_LUT: [u8; 16] = [
                                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                                    ];
                                                    static __HI_LUT: [u8; 16] = [
                                                        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                    ];
                                                    ::parse_that::find_next_structural_from(
                                                            &state.src_bytes,
                                                            __start,
                                                            &__LO_LUT,
                                                            &__HI_LUT,
                                                        )
                                                        .map(|(pos, _)| pos - __start)
                                                })
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
        fn __lhs_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __start = state.offset;
                    if ::parse_that::scan_ident(
                            state,
                            &::parse_that::DEFAULT_IDENT_CONFIG,
                        )
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
        fn __call_arg_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    let __rep_start136 = state.offset;
                    let __rep_bcp137 = __builder.checkpoint();
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
                                            let __pretty_cp128 = state.offset;
                                            let __pretty_bcp129 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows126 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows126..state.offset]);
                                                    if !Self::__binary_factor_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows127 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows127..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp128;
                                                __builder.restore(__pretty_bcp129);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                    {
                                        let _ = {
                                            let __pretty_cp130 = state.offset;
                                            let __pretty_bcp131 = __builder.checkpoint();
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
                                                state.offset = __pretty_cp130;
                                                __builder.restore(__pretty_bcp131);
                                            }
                                            __ok
                                        };
                                        true
                                    };
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
                    if __rep_count134 < 1 {
                        state.offset = __rep_start136;
                        __builder.restore(__rep_bcp137);
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
        fn __term_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp186 = state.offset;
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
                            state.offset = __pretty_cp186;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp185 = state.offset;
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
                                    state.offset = __pretty_cp185;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp183 = state.offset;
                                        let __pretty_bcp184 = __builder.checkpoint();
                                        let __ok = (|| -> bool {
                                            {
                                                {
                                                    let __start = state.offset;
                                                    if ::parse_that::scan_ident(
                                                            state,
                                                            &::parse_that::DEFAULT_IDENT_CONFIG,
                                                        )
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
                                                    let _ = {
                                                        let __pretty_cp153 = state.offset;
                                                        let __pretty_bcp154 = __builder.checkpoint();
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
                                                                        let __pretty_cp140 = state.offset;
                                                                        let __pretty_bcp141 = __builder.checkpoint();
                                                                        let __ok = (|| -> bool {
                                                                            {
                                                                                let __ows138 = state.offset;
                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                __builder
                                                                                    .text_inline_ws(&state.src[__ows138..state.offset]);
                                                                                if !Self::__call_arg_prettify(state, __builder) {
                                                                                    return false;
                                                                                }
                                                                                let __ows139 = state.offset;
                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                __builder
                                                                                    .text_inline_ws(&state.src[__ows139..state.offset]);
                                                                            };
                                                                            true
                                                                        })();
                                                                        if !__ok {
                                                                            state.offset = __pretty_cp140;
                                                                            __builder.restore(__pretty_bcp141);
                                                                        }
                                                                        __ok
                                                                    } {
                                                                        return false;
                                                                    }
                                                                };
                                                                {
                                                                    let mut __rep_count151 = 0usize;
                                                                    while __rep_count151 < 4294967295 {
                                                                        let __rep_cp152 = state.offset;
                                                                        if !{
                                                                            let __pretty_cp149 = state.offset;
                                                                            let __pretty_bcp150 = __builder.checkpoint();
                                                                            let __ok = (|| -> bool {
                                                                                {
                                                                                    {
                                                                                        let __ows142 = state.offset;
                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                        let __ows143 = state.offset;
                                                                                        {
                                                                                            if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                                            {
                                                                                                return false;
                                                                                            }
                                                                                            state.offset += 1;
                                                                                            __builder.char(b',');
                                                                                        };
                                                                                        __builder.text_inline_ws(&state.src[__ows142..__ows143]);
                                                                                        let __ows144 = state.offset;
                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                        __builder
                                                                                            .text_inline_ws(&state.src[__ows144..state.offset]);
                                                                                    };
                                                                                    {
                                                                                        if !{
                                                                                            let __pretty_cp147 = state.offset;
                                                                                            let __pretty_bcp148 = __builder.checkpoint();
                                                                                            let __ok = (|| -> bool {
                                                                                                {
                                                                                                    let __ows145 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows145..state.offset]);
                                                                                                    if !Self::__call_arg_prettify(state, __builder) {
                                                                                                        return false;
                                                                                                    }
                                                                                                    let __ows146 = state.offset;
                                                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                    __builder
                                                                                                        .text_inline_ws(&state.src[__ows146..state.offset]);
                                                                                                };
                                                                                                true
                                                                                            })();
                                                                                            if !__ok {
                                                                                                state.offset = __pretty_cp147;
                                                                                                __builder.restore(__pretty_bcp148);
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
                                                                                state.offset = __pretty_cp149;
                                                                                __builder.restore(__pretty_bcp150);
                                                                            }
                                                                            __ok
                                                                        } {
                                                                            state.offset = __rep_cp152;
                                                                            break;
                                                                        }
                                                                        if state.offset == __rep_cp152 {
                                                                            break;
                                                                        }
                                                                        __rep_count151 += 1;
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
                                                            state.offset = __pretty_cp153;
                                                            __builder.restore(__pretty_bcp154);
                                                        }
                                                        __ok
                                                    };
                                                    true
                                                };
                                            };
                                            true
                                        })();
                                        if !__ok {
                                            state.offset = __pretty_cp183;
                                            __builder.restore(__pretty_bcp184);
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp181 = state.offset;
                                                let __pretty_bcp182 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    if !Self::__literal_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp181;
                                                    __builder.restore(__pretty_bcp182);
                                                }
                                                __ok
                                            } {
                                                {
                                                    if !{
                                                        let __pretty_cp179 = state.offset;
                                                        let __pretty_bcp180 = __builder.checkpoint();
                                                        let __ok = (|| -> bool {
                                                            if !Self::__regex_prettify(state, __builder) {
                                                                return false;
                                                            }
                                                            true
                                                        })();
                                                        if !__ok {
                                                            state.offset = __pretty_cp179;
                                                            __builder.restore(__pretty_bcp180);
                                                        }
                                                        __ok
                                                    } {
                                                        {
                                                            if !{
                                                                let __pretty_cp177 = state.offset;
                                                                let __pretty_bcp178 = __builder.checkpoint();
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
                                                                                let __pretty_cp157 = state.offset;
                                                                                let __pretty_bcp158 = __builder.checkpoint();
                                                                                let __ok = (|| -> bool {
                                                                                    {
                                                                                        let __ows155 = state.offset;
                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                        __builder
                                                                                            .text_inline_ws(&state.src[__ows155..state.offset]);
                                                                                        if !Self::__rhs_prettify(state, __builder) {
                                                                                            return false;
                                                                                        }
                                                                                        let __ows156 = state.offset;
                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                        __builder
                                                                                            .text_inline_ws(&state.src[__ows156..state.offset]);
                                                                                    };
                                                                                    true
                                                                                })();
                                                                                if !__ok {
                                                                                    state.offset = __pretty_cp157;
                                                                                    __builder.restore(__pretty_bcp158);
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
                                                                    state.offset = __pretty_cp177;
                                                                    __builder.restore(__pretty_bcp178);
                                                                }
                                                                __ok
                                                            } {
                                                                {
                                                                    if !{
                                                                        let __pretty_cp175 = state.offset;
                                                                        let __pretty_bcp176 = __builder.checkpoint();
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
                                                                                        let __pretty_cp161 = state.offset;
                                                                                        let __pretty_bcp162 = __builder.checkpoint();
                                                                                        let __ok = (|| -> bool {
                                                                                            {
                                                                                                let __ows159 = state.offset;
                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                __builder
                                                                                                    .text_inline_ws(&state.src[__ows159..state.offset]);
                                                                                                if !Self::__rhs_prettify(state, __builder) {
                                                                                                    return false;
                                                                                                }
                                                                                                let __ows160 = state.offset;
                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                __builder
                                                                                                    .text_inline_ws(&state.src[__ows160..state.offset]);
                                                                                            };
                                                                                            true
                                                                                        })();
                                                                                        if !__ok {
                                                                                            state.offset = __pretty_cp161;
                                                                                            __builder.restore(__pretty_bcp162);
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
                                                                            state.offset = __pretty_cp175;
                                                                            __builder.restore(__pretty_bcp176);
                                                                        }
                                                                        __ok
                                                                    } {
                                                                        {
                                                                            if !{
                                                                                let __pretty_cp173 = state.offset;
                                                                                let __pretty_bcp174 = __builder.checkpoint();
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
                                                                                                let __pretty_cp165 = state.offset;
                                                                                                let __pretty_bcp166 = __builder.checkpoint();
                                                                                                let __ok = (|| -> bool {
                                                                                                    {
                                                                                                        let __ows163 = state.offset;
                                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                        __builder
                                                                                                            .text_inline_ws(&state.src[__ows163..state.offset]);
                                                                                                        if !Self::__rhs_prettify(state, __builder) {
                                                                                                            return false;
                                                                                                        }
                                                                                                        let __ows164 = state.offset;
                                                                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                        __builder
                                                                                                            .text_inline_ws(&state.src[__ows164..state.offset]);
                                                                                                    };
                                                                                                    true
                                                                                                })();
                                                                                                if !__ok {
                                                                                                    state.offset = __pretty_cp165;
                                                                                                    __builder.restore(__pretty_bcp166);
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
                                                                                    state.offset = __pretty_cp173;
                                                                                    __builder.restore(__pretty_bcp174);
                                                                                }
                                                                                __ok
                                                                            } {
                                                                                {
                                                                                    if !{
                                                                                        let __pretty_cp171 = state.offset;
                                                                                        let __pretty_bcp172 = __builder.checkpoint();
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
                                                                                                        let __pretty_cp169 = state.offset;
                                                                                                        let __pretty_bcp170 = __builder.checkpoint();
                                                                                                        let __ok = (|| -> bool {
                                                                                                            {
                                                                                                                let __ows167 = state.offset;
                                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                                __builder
                                                                                                                    .text_inline_ws(&state.src[__ows167..state.offset]);
                                                                                                                if !Self::__rhs_prettify(state, __builder) {
                                                                                                                    return false;
                                                                                                                }
                                                                                                                let __ows168 = state.offset;
                                                                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                                                                __builder
                                                                                                                    .text_inline_ws(&state.src[__ows168..state.offset]);
                                                                                                            };
                                                                                                            true
                                                                                                        })();
                                                                                                        if !__ok {
                                                                                                            state.offset = __pretty_cp169;
                                                                                                            __builder.restore(__pretty_bcp170);
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
                                                                                            state.offset = __pretty_cp171;
                                                                                            __builder.restore(__pretty_bcp172);
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
        fn __modifier_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp190 = state.offset;
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
                            state.offset = __pretty_cp190;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp189 = state.offset;
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
                                    state.offset = __pretty_cp189;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp188 = state.offset;
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
                                            state.offset = __pretty_cp188;
                                        }
                                        __ok
                                    } {
                                        {
                                            if !{
                                                let __pretty_cp187 = state.offset;
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
                                                    state.offset = __pretty_cp187;
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
        fn __factor_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let _ = {
                            let __pretty_cp191 = state.offset;
                            let __pretty_bcp192 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                if !Self::__big_comment_prettify(state, __builder) {
                                    return false;
                                }
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp191;
                                __builder.restore(__pretty_bcp192);
                            }
                            __ok
                        };
                        true
                    };
                    {
                        if !{
                            let __pretty_cp195 = state.offset;
                            let __pretty_bcp196 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows193 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows193..state.offset]);
                                    if !Self::__term_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows194 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows194..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp195;
                                __builder.restore(__pretty_bcp196);
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
                                if !Self::__modifier_prettify(state, __builder) {
                                    return false;
                                }
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
                    {
                        let _ = {
                            let __pretty_cp199 = state.offset;
                            let __pretty_bcp200 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                if !Self::__big_comment_prettify(state, __builder) {
                                    return false;
                                }
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp199;
                                __builder.restore(__pretty_bcp200);
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
                            let __pretty_cp206 = state.offset;
                            let __pretty_bcp207 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows201 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows202 = state.offset;
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
                                        __builder.text_inline_ws(&state.src[__ows201..__ows202]);
                                        let __ows203 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows203..state.offset]);
                                    };
                                    {
                                        if !Self::__value_expr_prettify(state, __builder) {
                                            return false;
                                        }
                                        {
                                            let _ = {
                                                let __pretty_cp204 = state.offset;
                                                let __pretty_bcp205 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    if !Self::__type_annotation_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp204;
                                                    __builder.restore(__pretty_bcp205);
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
                                state.offset = __pretty_cp206;
                                __builder.restore(__pretty_bcp207);
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
        fn __binary_operators_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp210 = state.offset;
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
                            state.offset = __pretty_cp210;
                        }
                        __ok
                    } {
                        {
                            if !{
                                let __pretty_cp209 = state.offset;
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
                                    state.offset = __pretty_cp209;
                                }
                                __ok
                            } {
                                {
                                    if !{
                                        let __pretty_cp208 = state.offset;
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
                                            state.offset = __pretty_cp208;
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
                        let mut __rep_count217 = 0usize;
                        while __rep_count217 < 4294967295 {
                            let __rep_cp218 = state.offset;
                            if !{
                                let __pretty_cp215 = state.offset;
                                let __pretty_bcp216 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            if !{
                                                let __pretty_cp213 = state.offset;
                                                let __pretty_bcp214 = __builder.checkpoint();
                                                let __ok = (|| -> bool {
                                                    {
                                                        let __ows211 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows211..state.offset]);
                                                        if !Self::__binary_operators_prettify(state, __builder) {
                                                            return false;
                                                        }
                                                        let __ows212 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows212..state.offset]);
                                                    };
                                                    true
                                                })();
                                                if !__ok {
                                                    state.offset = __pretty_cp213;
                                                    __builder.restore(__pretty_bcp214);
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
                                    state.offset = __pretty_cp215;
                                    __builder.restore(__pretty_bcp216);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp218;
                                break;
                            }
                            if state.offset == __rep_cp218 {
                                break;
                            }
                            __rep_count217 += 1;
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
        fn __concatenation_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
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
                                                    if !Self::__binary_factor_prettify(state, __builder) {
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
        fn __alternation_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            __builder.group_open();
            let __pretty_ok = {
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
                                                        if !Self::__concatenation_prettify(state, __builder) {
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
                        if ::parse_that::scan_ident(
                                state,
                                &::parse_that::DEFAULT_IDENT_CONFIG,
                            )
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
                        let mut __rep_count248 = 0usize;
                        while __rep_count248 < 4294967295 {
                            let __rep_cp249 = state.offset;
                            if !{
                                let __pretty_cp246 = state.offset;
                                let __pretty_bcp247 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        {
                                            let __ows243 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            let __ows244 = state.offset;
                                            {
                                                if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                {
                                                    return false;
                                                }
                                                state.offset += 1;
                                                __builder.char(b',');
                                            };
                                            __builder.text_inline_ws(&state.src[__ows243..__ows244]);
                                            let __ows245 = state.offset;
                                            ::parse_that::trim_leading_whitespace_mut(state);
                                            __builder
                                                .text_inline_ws(&state.src[__ows245..state.offset]);
                                        };
                                        {
                                            let __start = state.offset;
                                            if ::parse_that::scan_ident(
                                                    state,
                                                    &::parse_that::DEFAULT_IDENT_CONFIG,
                                                )
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
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp246;
                                    __builder.restore(__pretty_bcp247);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp249;
                                break;
                            }
                            if state.offset == __rep_cp249 {
                                break;
                            }
                            __rep_count248 += 1;
                        }
                    };
                    {
                        let __ows250 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows251 = state.offset;
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'|') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'|');
                        };
                        __builder.text_inline_ws(&state.src[__ows250..__ows251]);
                        let __ows252 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows252..state.offset]);
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
        fn __rhs_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    if !{
                        let __pretty_cp253 = state.offset;
                        let __pretty_bcp254 = __builder.checkpoint();
                        let __ok = (|| -> bool {
                            if !Self::__closure_prettify(state, __builder) {
                                return false;
                            }
                            true
                        })();
                        if !__ok {
                            state.offset = __pretty_cp253;
                            __builder.restore(__pretty_bcp254);
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
                            if ::parse_that::scan_ident(
                                    state,
                                    &::parse_that::DEFAULT_IDENT_CONFIG,
                                )
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
                            let __ows255 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            let __ows256 = state.offset;
                            {
                                if state.src_bytes.get(state.offset).copied() != Some(b'=')
                                {
                                    return false;
                                }
                                state.offset += 1;
                                __builder.char(b'=');
                            };
                            __builder.text_inline_ws(&state.src[__ows255..__ows256]);
                            let __ows257 = state.offset;
                            ::parse_that::trim_leading_whitespace_mut(state);
                            __builder.text_inline_ws(&state.src[__ows257..state.offset]);
                        };
                        {
                            if !{
                                let __pretty_cp260 = state.offset;
                                let __pretty_bcp261 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        let __ows258 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows258..state.offset]);
                                        if !Self::__rhs_prettify(state, __builder) {
                                            return false;
                                        }
                                        let __ows259 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows259..state.offset]);
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp260;
                                    __builder.restore(__pretty_bcp261);
                                }
                                __ok
                            } {
                                return false;
                            }
                        };
                        {
                            if !{
                                let __pretty_cp262 = state.offset;
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
                                    state.offset = __pretty_cp262;
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
        fn __import_items_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows263 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows264 = state.offset;
                        {
                            if state.src_bytes.get(state.offset).copied() != Some(b'{') {
                                return false;
                            }
                            state.offset += 1;
                            __builder.char(b'{');
                        };
                        __builder.text_inline_ws(&state.src[__ows263..__ows264]);
                        let __ows265 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows265..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp275 = state.offset;
                            let __pretty_bcp276 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows273 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows273..state.offset]);
                                    {
                                        {
                                            let __start = state.offset;
                                            if ::parse_that::scan_ident(
                                                    state,
                                                    &::parse_that::DEFAULT_IDENT_CONFIG,
                                                )
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
                                            let mut __rep_count271 = 0usize;
                                            while __rep_count271 < 4294967295 {
                                                let __rep_cp272 = state.offset;
                                                if !{
                                                    let __pretty_cp269 = state.offset;
                                                    let __pretty_bcp270 = __builder.checkpoint();
                                                    let __ok = (|| -> bool {
                                                        {
                                                            {
                                                                let __ows266 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                let __ows267 = state.offset;
                                                                {
                                                                    if state.src_bytes.get(state.offset).copied() != Some(b',')
                                                                    {
                                                                        return false;
                                                                    }
                                                                    state.offset += 1;
                                                                    __builder.char(b',');
                                                                };
                                                                __builder.text_inline_ws(&state.src[__ows266..__ows267]);
                                                                let __ows268 = state.offset;
                                                                ::parse_that::trim_leading_whitespace_mut(state);
                                                                __builder
                                                                    .text_inline_ws(&state.src[__ows268..state.offset]);
                                                            };
                                                            {
                                                                let __start = state.offset;
                                                                if ::parse_that::scan_ident(
                                                                        state,
                                                                        &::parse_that::DEFAULT_IDENT_CONFIG,
                                                                    )
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
                                                        true
                                                    })();
                                                    if !__ok {
                                                        state.offset = __pretty_cp269;
                                                        __builder.restore(__pretty_bcp270);
                                                    }
                                                    __ok
                                                } {
                                                    state.offset = __rep_cp272;
                                                    break;
                                                }
                                                if state.offset == __rep_cp272 {
                                                    break;
                                                }
                                                __rep_count271 += 1;
                                            }
                                        };
                                    };
                                    let __ows274 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows274..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp275;
                                __builder.restore(__pretty_bcp276);
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
        fn __import_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows277 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows278 = state.offset;
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
                            __builder
                                .text(&state.src[state.offset..state.offset + 7usize]);
                            state.offset += 7usize;
                        };
                        __builder.text_inline_ws(&state.src[__ows277..__ows278]);
                        let __ows279 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows279..state.offset]);
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
                                    {
                                        if !{
                                            let __pretty_cp287 = state.offset;
                                            let __pretty_bcp288 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    {
                                                        if !{
                                                            let __pretty_cp282 = state.offset;
                                                            let __pretty_bcp283 = __builder.checkpoint();
                                                            let __ok = (|| -> bool {
                                                                {
                                                                    let __ows280 = state.offset;
                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                    __builder
                                                                        .text_inline_ws(&state.src[__ows280..state.offset]);
                                                                    if !Self::__import_items_prettify(state, __builder) {
                                                                        return false;
                                                                    }
                                                                    let __ows281 = state.offset;
                                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                                    __builder
                                                                        .text_inline_ws(&state.src[__ows281..state.offset]);
                                                                };
                                                                true
                                                            })();
                                                            if !__ok {
                                                                state.offset = __pretty_cp282;
                                                                __builder.restore(__pretty_bcp283);
                                                            }
                                                            __ok
                                                        } {
                                                            return false;
                                                        }
                                                    };
                                                    {
                                                        let __ows284 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        let __ows285 = state.offset;
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
                                                        __builder.text_inline_ws(&state.src[__ows284..__ows285]);
                                                        let __ows286 = state.offset;
                                                        ::parse_that::trim_leading_whitespace_mut(state);
                                                        __builder
                                                            .text_inline_ws(&state.src[__ows286..state.offset]);
                                                    };
                                                    if !Self::__import_path_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp287;
                                                __builder.restore(__pretty_bcp288);
                                            }
                                            __ok
                                        } {
                                            if !Self::__import_path_prettify(state, __builder) {
                                                return false;
                                            }
                                        }
                                    };
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
                        let _ = {
                            let __pretty_cp294 = state.offset;
                            let __pretty_bcp295 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    if !{
                                        let __pretty_cp293 = state.offset;
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
                                            state.offset = __pretty_cp293;
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
                                state.offset = __pretty_cp294;
                                __builder.restore(__pretty_bcp295);
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
        fn __recover_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows296 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows297 = state.offset;
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
                            __builder
                                .text(&state.src[state.offset..state.offset + 8usize]);
                            state.offset += 8usize;
                        };
                        __builder.text_inline_ws(&state.src[__ows296..__ows297]);
                        let __ows298 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows298..state.offset]);
                    };
                    {
                        let __ows299 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows300 = state.offset;
                        {
                            let __start = state.offset;
                            if ::parse_that::scan_ident(
                                    state,
                                    &::parse_that::DEFAULT_IDENT_CONFIG,
                                )
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        __builder.text_inline_ws(&state.src[__ows299..__ows300]);
                        let __ows301 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows301..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp304 = state.offset;
                            let __pretty_bcp305 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows302 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows302..state.offset]);
                                    if !Self::__rhs_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows303 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows303..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp304;
                                __builder.restore(__pretty_bcp305);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let _ = {
                            let __pretty_cp307 = state.offset;
                            let __pretty_bcp308 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    if !{
                                        let __pretty_cp306 = state.offset;
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
                                            state.offset = __pretty_cp306;
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
                                state.offset = __pretty_cp307;
                                __builder.restore(__pretty_bcp308);
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
        fn __pretty_hint_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __start = state.offset;
                        if ::parse_that::scan_ident(
                                state,
                                &::parse_that::DEFAULT_IDENT_CONFIG,
                            )
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
                        let _ = {
                            let __pretty_cp309 = state.offset;
                            let __pretty_bcp310 = __builder.checkpoint();
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
                                                ({
                                                    static __LO_LUT: [u8; 16] = [
                                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
                                                    ];
                                                    static __HI_LUT: [u8; 16] = [
                                                        0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                    ];
                                                    ::parse_that::find_next_structural_from(
                                                            &state.src_bytes,
                                                            __start,
                                                            &__LO_LUT,
                                                            &__HI_LUT,
                                                        )
                                                        .map(|(pos, _)| pos - __start)
                                                })
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
                                state.offset = __pretty_cp309;
                                __builder.restore(__pretty_bcp310);
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
        fn __pretty_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows311 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows312 = state.offset;
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
                            __builder
                                .text(&state.src[state.offset..state.offset + 7usize]);
                            state.offset += 7usize;
                        };
                        __builder.text_inline_ws(&state.src[__ows311..__ows312]);
                        let __ows313 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows313..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp317 = state.offset;
                            let __pretty_bcp318 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows315 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows315..state.offset]);
                                    {
                                        if !{
                                            let __pretty_cp314 = state.offset;
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
                                                state.offset = __pretty_cp314;
                                            }
                                            __ok
                                        } {
                                            {
                                                let __start = state.offset;
                                                if ::parse_that::scan_ident(
                                                        state,
                                                        &::parse_that::DEFAULT_IDENT_CONFIG,
                                                    )
                                                    .is_none()
                                                {
                                                    return false;
                                                }
                                                let __matched = &state.src[__start..state.offset];
                                                if !__matched.is_empty() {
                                                    __builder.text(__matched);
                                                }
                                            };
                                        }
                                    };
                                    let __ows316 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows316..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp317;
                                __builder.restore(__pretty_bcp318);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let __rep_start327 = state.offset;
                        let __rep_bcp328 = __builder.checkpoint();
                        let mut __rep_count325 = 0usize;
                        while __rep_count325 < 4294967295 {
                            let __rep_cp326 = state.offset;
                            if !{
                                let __pretty_cp323 = state.offset;
                                let __pretty_bcp324 = __builder.checkpoint();
                                let __ok = (|| -> bool {
                                    {
                                        if !{
                                            let __pretty_cp321 = state.offset;
                                            let __pretty_bcp322 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows319 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows319..state.offset]);
                                                    if !Self::__pretty_hint_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows320 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows320..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp321;
                                                __builder.restore(__pretty_bcp322);
                                            }
                                            __ok
                                        } {
                                            return false;
                                        }
                                    };
                                    true
                                })();
                                if !__ok {
                                    state.offset = __pretty_cp323;
                                    __builder.restore(__pretty_bcp324);
                                }
                                __ok
                            } {
                                state.offset = __rep_cp326;
                                break;
                            }
                            if state.offset == __rep_cp326 {
                                break;
                            }
                            __rep_count325 += 1;
                        }
                        if __rep_count325 < 1 {
                            state.offset = __rep_start327;
                            __builder.restore(__rep_bcp328);
                            return false;
                        }
                    };
                    {
                        let _ = {
                            let __pretty_cp330 = state.offset;
                            let __pretty_bcp331 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    if !{
                                        let __pretty_cp329 = state.offset;
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
                                            state.offset = __pretty_cp329;
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
                                state.offset = __pretty_cp330;
                                __builder.restore(__pretty_bcp331);
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
        fn __ws_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows332 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows333 = state.offset;
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
                            __builder
                                .text(&state.src[state.offset..state.offset + 3usize]);
                            state.offset += 3usize;
                        };
                        __builder.text_inline_ws(&state.src[__ows332..__ows333]);
                        let __ows334 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows334..state.offset]);
                    };
                    {
                        if !{
                            let __pretty_cp337 = state.offset;
                            let __pretty_bcp338 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    let __ows335 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows335..state.offset]);
                                    if !Self::__regex_prettify(state, __builder) {
                                        return false;
                                    }
                                    let __ows336 = state.offset;
                                    ::parse_that::trim_leading_whitespace_mut(state);
                                    __builder
                                        .text_inline_ws(&state.src[__ows336..state.offset]);
                                };
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp337;
                                __builder.restore(__pretty_bcp338);
                            }
                            __ok
                        } {
                            return false;
                        }
                    };
                    {
                        let _ = {
                            let __pretty_cp340 = state.offset;
                            let __pretty_bcp341 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    if !{
                                        let __pretty_cp339 = state.offset;
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
                                            state.offset = __pretty_cp339;
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
                                state.offset = __pretty_cp340;
                                __builder.restore(__pretty_bcp341);
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
        fn __token_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows342 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows343 = state.offset;
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
                            __builder
                                .text(&state.src[state.offset..state.offset + 6usize]);
                            state.offset += 6usize;
                        };
                        __builder.text_inline_ws(&state.src[__ows342..__ows343]);
                        let __ows344 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows344..state.offset]);
                    };
                    {
                        let __ows345 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows346 = state.offset;
                        {
                            let __start = state.offset;
                            if ::parse_that::scan_ident(
                                    state,
                                    &::parse_that::DEFAULT_IDENT_CONFIG,
                                )
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        __builder.text_inline_ws(&state.src[__ows345..__ows346]);
                        let __ows347 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows347..state.offset]);
                    };
                    {
                        let _ = {
                            let __pretty_cp349 = state.offset;
                            let __pretty_bcp350 = __builder.checkpoint();
                            let __ok = (|| -> bool {
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
                                true
                            })();
                            if !__ok {
                                state.offset = __pretty_cp349;
                                __builder.restore(__pretty_bcp350);
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
        fn __debug_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows351 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows352 = state.offset;
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
                            __builder
                                .text(&state.src[state.offset..state.offset + 6usize]);
                            state.offset += 6usize;
                        };
                        __builder.text_inline_ws(&state.src[__ows351..__ows352]);
                        let __ows353 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows353..state.offset]);
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
                                    {
                                        if !{
                                            let __pretty_cp354 = state.offset;
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
                                                state.offset = __pretty_cp354;
                                            }
                                            __ok
                                        } {
                                            {
                                                let __start = state.offset;
                                                if ::parse_that::scan_ident(
                                                        state,
                                                        &::parse_that::DEFAULT_IDENT_CONFIG,
                                                    )
                                                    .is_none()
                                                {
                                                    return false;
                                                }
                                                let __matched = &state.src[__start..state.offset];
                                                if !__matched.is_empty() {
                                                    __builder.text(__matched);
                                                }
                                            };
                                        }
                                    };
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
        fn __host_directive_prettify<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            __builder: &mut ::pprint::FmtBuilder<'a>,
        ) -> bool {
            {
                {
                    {
                        let __ows362 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows363 = state.offset;
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
                            __builder
                                .text(&state.src[state.offset..state.offset + 5usize]);
                            state.offset += 5usize;
                        };
                        __builder.text_inline_ws(&state.src[__ows362..__ows363]);
                        let __ows364 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows364..state.offset]);
                    };
                    {
                        let __ows365 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        let __ows366 = state.offset;
                        {
                            let __start = state.offset;
                            if ::parse_that::scan_ident(
                                    state,
                                    &::parse_that::DEFAULT_IDENT_CONFIG,
                                )
                                .is_none()
                            {
                                return false;
                            }
                            let __matched = &state.src[__start..state.offset];
                            if !__matched.is_empty() {
                                __builder.text(__matched);
                            }
                        };
                        __builder.text_inline_ws(&state.src[__ows365..__ows366]);
                        let __ows367 = state.offset;
                        ::parse_that::trim_leading_whitespace_mut(state);
                        __builder.text_inline_ws(&state.src[__ows367..state.offset]);
                    };
                    {
                        let _ = {
                            let __pretty_cp375 = state.offset;
                            let __pretty_bcp376 = __builder.checkpoint();
                            let __ok = (|| -> bool {
                                {
                                    {
                                        let __ows368 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        let __ows369 = state.offset;
                                        {
                                            if state.src_bytes.get(state.offset).copied() != Some(b':')
                                            {
                                                return false;
                                            }
                                            state.offset += 1;
                                            __builder.char(b':');
                                        };
                                        __builder.text_inline_ws(&state.src[__ows368..__ows369]);
                                        let __ows370 = state.offset;
                                        ::parse_that::trim_leading_whitespace_mut(state);
                                        __builder
                                            .text_inline_ws(&state.src[__ows370..state.offset]);
                                    };
                                    {
                                        if !{
                                            let __pretty_cp373 = state.offset;
                                            let __pretty_bcp374 = __builder.checkpoint();
                                            let __ok = (|| -> bool {
                                                {
                                                    let __ows371 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows371..state.offset]);
                                                    if !Self::__type_name_prettify(state, __builder) {
                                                        return false;
                                                    }
                                                    let __ows372 = state.offset;
                                                    ::parse_that::trim_leading_whitespace_mut(state);
                                                    __builder
                                                        .text_inline_ws(&state.src[__ows372..state.offset]);
                                                };
                                                true
                                            })();
                                            if !__ok {
                                                state.offset = __pretty_cp373;
                                                __builder.restore(__pretty_bcp374);
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
                                state.offset = __pretty_cp375;
                                __builder.restore(__pretty_bcp376);
                            }
                            __ok
                        };
                        true
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
                                                        let __scan = if __start >= state.src_bytes.len() {
                                                            0
                                                        } else {
                                                            ({
                                                                static __LO_LUT: [u8; 16] = [
                                                                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                                                ];
                                                                static __HI_LUT: [u8; 16] = [
                                                                    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                ];
                                                                ::parse_that::find_next_structural_from(
                                                                        &state.src_bytes,
                                                                        __start,
                                                                        &__LO_LUT,
                                                                        &__HI_LUT,
                                                                    )
                                                                    .map(|(pos, _)| pos - __start)
                                                            })
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
        /// Parse an input string and return a zero-copy
        /// `Parsed<'_, Self>` that borrows the input directly.
        ///
        /// AW-I.W3: `parse()` dispatches through the DTA driver
        /// wholesale. The per-rule fn-per-rule path retired at
        /// W3.1; the DTA walker (AW-I.W2.1) owns Seq / Literal /
        /// Regex / Ref / AltLinear-with-savepoint / Repeat with
        /// `lo..=hi` bounds / ShuntingYard. `dta_run_into`
        /// drives `DTA_TABLE` over the input bytes, writing
        /// records into the builder's columns and stamping
        /// `frame_depth` inline so `finish()` skips the
        /// `derive_frame_depth` reconstruction pass.
        pub fn parse(
            input: &str,
        ) -> ::core::result::Result<
            ::bbnf::runtime::Parsed<'_, Self>,
            ::bbnf::runtime::ParseErr,
        > {
            let mut builder = ::bbnf::runtime::tape::TapeBuilder::with_capacity(
                GRAMMAR_PROFILE.capacity_for(input.len()),
            );
            builder.enable_inline_frame_depth();
            let mut psi = psi_with_capacity(input.len());
            let root_off = builder
                .dta_run_into(&DTA_TABLE, input.as_bytes(), &DTA_SCANNER, &mut psi)
                .map_err(|e| match e {
                    ::bbnf::runtime::tape::DtaError::Syntax { offset, .. } => {
                        ::bbnf::runtime::ParseErr::Syntax {
                            offset,
                            rule: None,
                        }
                    }
                    ::bbnf::runtime::tape::DtaError::UnexpectedEnd { offset } => {
                        ::bbnf::runtime::ParseErr::Syntax {
                            offset,
                            rule: None,
                        }
                    }
                    ::bbnf::runtime::tape::DtaError::InvalidState { .. } => {
                        ::bbnf::runtime::ParseErr::Syntax {
                            offset: 0,
                            rule: None,
                        }
                    }
                })?;
            psi.fill_columns(input.as_bytes(), builder.columns_mut(), &GRAMMAR_PROFILE);
            let tape = builder.finish().map_err(::bbnf::runtime::ParseErr::Tape)?;
            ::core::result::Result::Ok(
                ::bbnf::runtime::Parsed::new(tape, input, root_off),
            )
        }
    }
    impl<'p> identifierView<'p> {
        /// Identifier text — slice of the owning `Parsed`'s
        /// input covered by this view's record span.
        #[inline]
        pub fn identifier_text(&self) -> &'p str {
            let (lo, hi) = self.cursor.span();
            &self.input[lo as usize..hi as usize]
        }
    }
    /// Walk `cursor`'s sub-tree depth-first and return the text
    /// of the first reachable identifier record. Returns `""`
    /// when no identifier is reachable.
    #[inline]
    pub(crate) fn cst_identifier_text<'p>(
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        input: &'p str,
    ) -> &'p str {
        match cst_find_identifier_cursor(cursor, 22u8) {
            ::core::option::Option::Some(found) => {
                let (lo, hi) = found.span();
                &input[lo as usize..hi as usize]
            }
            ::core::option::Option::None => "",
        }
    }
    /// Walk `cursor`'s sub-tree depth-first and return the
    /// `(lo, hi)` span of the first reachable identifier record.
    /// Returns `(0, 0)` when no identifier is reachable.
    #[inline]
    pub(crate) fn cst_identifier_span<'p>(
        cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
        _input: &'p str,
    ) -> (u32, u32) {
        cst_find_identifier_cursor(cursor, 22u8).map(|c| c.span()).unwrap_or((0, 0))
    }
    /// DFS helper shared by `cst_identifier_text` and
    /// `cst_identifier_span`. Returns the first cursor under
    /// `start` whose `variant_idx` matches `target_idx`.
    #[inline]
    fn cst_find_identifier_cursor<'p>(
        start: ::bbnf::runtime::tape::TapeCursor<'p>,
        target_idx: u8,
    ) -> ::core::option::Option<::bbnf::runtime::tape::TapeCursor<'p>> {
        if start.variant_idx() == target_idx {
            return ::core::option::Option::Some(start);
        }
        for child in start.children() {
            if let ::core::option::Option::Some(found) = cst_find_identifier_cursor(
                child,
                target_idx,
            ) {
                return ::core::option::Option::Some(found);
            }
        }
        ::core::option::Option::None
    }
    /// Schema-emitted directive value structs. Each is a thin
    /// typed projection over a tape record; the source span is
    /// always exposed via `.span`. Compound slots hand back raw
    /// `TapeCursor<'p>` handles so callers can construct
    /// whatever typed view they need without this module having
    /// to enumerate the target rule's view type.
    pub mod cst_directives {
        #[derive(Clone, Copy)]
        pub struct ImportDirective<'p> {
            pub inner: super::BbnfBootstrapNodeView<'p>,
            pub span: ::parse_that::Span<'p>,
        }
        #[derive(Clone, Copy)]
        pub struct RecoverDirective<'p> {
            pub rule_name: &'p str,
            pub sync_expr: super::BbnfBootstrapNodeView<'p>,
            pub span: ::parse_that::Span<'p>,
        }
        #[derive(Clone, Copy)]
        pub struct PrettyDirective<'p> {
            pub target: &'p str,
            pub hints: super::BbnfBootstrapNodeView<'p>,
            pub span: ::parse_that::Span<'p>,
        }
        #[derive(Clone, Copy)]
        pub struct WsDirective<'p> {
            pub value: super::BbnfBootstrapNodeView<'p>,
            pub span: ::parse_that::Span<'p>,
        }
        #[derive(Clone, Copy)]
        pub struct TokenDirective<'p> {
            pub name: &'p str,
            pub span: ::parse_that::Span<'p>,
        }
        #[derive(Clone, Copy)]
        pub struct DebugDirective<'p> {
            pub target: &'p str,
            pub span: ::parse_that::Span<'p>,
        }
        #[derive(Clone, Copy)]
        pub struct HostDirective<'p> {
            pub name: &'p str,
            pub type_annotation: ::core::option::Option<
                super::BbnfBootstrapNodeView<'p>,
            >,
            pub span: ::parse_that::Span<'p>,
        }
        /// Schema-emitted extraction helper. Returns the typed
        /// directive struct if `cursor` points at a record whose
        /// `variant_idx` matches this directive rule's codegen-
        /// assigned discriminator; `None` otherwise.
        #[inline]
        pub fn try_as_import_directive<'p>(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> ::core::option::Option<ImportDirective<'p>> {
            if cursor.variant_idx() != 42u8 {
                return ::core::option::Option::None;
            }
            let mut __children = cursor.children();
            let __kw = __children.next()?;
            let __slot_0 = __children.next()?;
            let __term = __children.next()?;
            let (__kw_lo, _) = __kw.span();
            let (_, __term_hi) = __term.span();
            ::core::option::Option::Some(ImportDirective {
                inner: super::BbnfBootstrapNodeView::from_cursor(__slot_0, input),
                span: ::parse_that::Span::new(
                    __kw_lo as usize,
                    __term_hi as usize,
                    input,
                ),
            })
        }
        /// Schema-emitted extraction helper. Returns the typed
        /// directive struct if `cursor` points at a record whose
        /// `variant_idx` matches this directive rule's codegen-
        /// assigned discriminator; `None` otherwise.
        #[inline]
        pub fn try_as_recover_directive<'p>(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> ::core::option::Option<RecoverDirective<'p>> {
            if cursor.variant_idx() != 43u8 {
                return ::core::option::Option::None;
            }
            let mut __children = cursor.children();
            let __kw = __children.next()?;
            let __slot_0 = __children.next()?;
            let __slot_1 = __children.next()?;
            let __term = __children.next()?;
            let (__kw_lo, _) = __kw.span();
            let (_, __term_hi) = __term.span();
            ::core::option::Option::Some(RecoverDirective {
                rule_name: super::cst_identifier_text(__slot_0, input),
                sync_expr: super::BbnfBootstrapNodeView::from_cursor(__slot_1, input),
                span: ::parse_that::Span::new(
                    __kw_lo as usize,
                    __term_hi as usize,
                    input,
                ),
            })
        }
        /// Schema-emitted extraction helper. Returns the typed
        /// directive struct if `cursor` points at a record whose
        /// `variant_idx` matches this directive rule's codegen-
        /// assigned discriminator; `None` otherwise.
        #[inline]
        pub fn try_as_pretty_directive<'p>(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> ::core::option::Option<PrettyDirective<'p>> {
            if cursor.variant_idx() != 45u8 {
                return ::core::option::Option::None;
            }
            let mut __children = cursor.children();
            let __kw = __children.next()?;
            let __slot_0 = __children.next()?;
            let __slot_1 = __children.next()?;
            let __term = __children.next()?;
            let (__kw_lo, _) = __kw.span();
            let (_, __term_hi) = __term.span();
            ::core::option::Option::Some(PrettyDirective {
                target: {
                    let (__lo, __hi) = __slot_0.span();
                    &input[__lo as usize..__hi as usize]
                },
                hints: super::BbnfBootstrapNodeView::from_cursor(__slot_1, input),
                span: ::parse_that::Span::new(
                    __kw_lo as usize,
                    __term_hi as usize,
                    input,
                ),
            })
        }
        /// Schema-emitted extraction helper. Returns the typed
        /// directive struct if `cursor` points at a record whose
        /// `variant_idx` matches this directive rule's codegen-
        /// assigned discriminator; `None` otherwise.
        #[inline]
        pub fn try_as_ws_directive<'p>(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> ::core::option::Option<WsDirective<'p>> {
            if cursor.variant_idx() != 46u8 {
                return ::core::option::Option::None;
            }
            let mut __children = cursor.children();
            let __kw = __children.next()?;
            let __slot_0 = __children.next()?;
            let __term = __children.next()?;
            let (__kw_lo, _) = __kw.span();
            let (_, __term_hi) = __term.span();
            ::core::option::Option::Some(WsDirective {
                value: super::BbnfBootstrapNodeView::from_cursor(__slot_0, input),
                span: ::parse_that::Span::new(
                    __kw_lo as usize,
                    __term_hi as usize,
                    input,
                ),
            })
        }
        /// Schema-emitted extraction helper. Returns the typed
        /// directive struct if `cursor` points at a record whose
        /// `variant_idx` matches this directive rule's codegen-
        /// assigned discriminator; `None` otherwise.
        #[inline]
        pub fn try_as_token_directive<'p>(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> ::core::option::Option<TokenDirective<'p>> {
            if cursor.variant_idx() != 47u8 {
                return ::core::option::Option::None;
            }
            let mut __children = cursor.children();
            let __kw = __children.next()?;
            let __slot_0 = __children.next()?;
            let __term = __children.next()?;
            let (__kw_lo, _) = __kw.span();
            let (_, __term_hi) = __term.span();
            ::core::option::Option::Some(TokenDirective {
                name: super::cst_identifier_text(__slot_0, input),
                span: ::parse_that::Span::new(
                    __kw_lo as usize,
                    __term_hi as usize,
                    input,
                ),
            })
        }
        /// Schema-emitted extraction helper. Returns the typed
        /// directive struct if `cursor` points at a record whose
        /// `variant_idx` matches this directive rule's codegen-
        /// assigned discriminator; `None` otherwise.
        #[inline]
        pub fn try_as_debug_directive<'p>(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> ::core::option::Option<DebugDirective<'p>> {
            if cursor.variant_idx() != 48u8 {
                return ::core::option::Option::None;
            }
            let mut __children = cursor.children();
            let __kw = __children.next()?;
            let __slot_0 = __children.next()?;
            let __term = __children.next()?;
            let (__kw_lo, _) = __kw.span();
            let (_, __term_hi) = __term.span();
            ::core::option::Option::Some(DebugDirective {
                target: {
                    let (__lo, __hi) = __slot_0.span();
                    &input[__lo as usize..__hi as usize]
                },
                span: ::parse_that::Span::new(
                    __kw_lo as usize,
                    __term_hi as usize,
                    input,
                ),
            })
        }
        /// Schema-emitted extraction helper. Returns the typed
        /// directive struct if `cursor` points at a record whose
        /// `variant_idx` matches this directive rule's codegen-
        /// assigned discriminator; `None` otherwise.
        #[inline]
        pub fn try_as_host_directive<'p>(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> ::core::option::Option<HostDirective<'p>> {
            if cursor.variant_idx() != 49u8 {
                return ::core::option::Option::None;
            }
            let mut __children = cursor.children();
            let __kw = __children.next()?;
            let __slot_0 = __children.next()?;
            let __slot_1 = __children.next()?;
            let __term = __children.next()?;
            let (__kw_lo, _) = __kw.span();
            let (_, __term_hi) = __term.span();
            ::core::option::Option::Some(HostDirective {
                name: super::cst_identifier_text(__slot_0, input),
                type_annotation: __slot_1
                    .children()
                    .next()
                    .map(|c| super::BbnfBootstrapNodeView::from_cursor(c, input)),
                span: ::parse_that::Span::new(
                    __kw_lo as usize,
                    __term_hi as usize,
                    input,
                ),
            })
        }
    }
}
pub use __bbnfbootstrap_emit_impl::*;

