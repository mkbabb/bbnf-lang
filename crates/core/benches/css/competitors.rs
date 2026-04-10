
//! CSS parsing competitor benchmarks.
//!
//! Competitors:
//!   cssparser    — Mozilla/Servo CSS tokenizer, callback/visitor pattern (L0–L1)
//!   lightningcss — Parcel CSS parser built on cssparser, full L2 semantic parse

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

fn load_css(name: &str) -> String {
    let path = format!("../../data/css/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {}: {}", path, e))
}

// ── cssparser (minimal visitor counting rules/declarations) ─────────────────

mod cssparser_visitor {
    use cssparser::{
        AtRuleParser, CowRcStr, DeclarationParser, ParseError, Parser, ParserInput,
        QualifiedRuleParser, RuleBodyItemParser, StyleSheetParser,
    };

    struct RuleCounter {
        pub rule_count: usize,
        pub decl_count: usize,
    }

    impl<'i> QualifiedRuleParser<'i> for RuleCounter {
        type Prelude = ();
        type QualifiedRule = ();
        type Error = ();

        fn parse_prelude<'t>(
            &mut self,
            input: &mut Parser<'i, 't>,
        ) -> Result<Self::Prelude, ParseError<'i, ()>> {
            while input.next().is_ok() {}
            Ok(())
        }

        fn parse_block<'t>(
            &mut self,
            _prelude: Self::Prelude,
            _start: &cssparser::ParserState,
            input: &mut Parser<'i, 't>,
        ) -> Result<Self::QualifiedRule, ParseError<'i, ()>> {
            self.rule_count += 1;
            while input.next().is_ok() {}
            Ok(())
        }
    }

    impl<'i> AtRuleParser<'i> for RuleCounter {
        type Prelude = ();
        type AtRule = ();
        type Error = ();

        fn parse_prelude<'t>(
            &mut self,
            _name: CowRcStr<'i>,
            input: &mut Parser<'i, 't>,
        ) -> Result<Self::Prelude, ParseError<'i, ()>> {
            while input.next().is_ok() {}
            Ok(())
        }

        fn parse_block<'t>(
            &mut self,
            _prelude: Self::Prelude,
            _start: &cssparser::ParserState,
            input: &mut Parser<'i, 't>,
        ) -> Result<Self::AtRule, ParseError<'i, ()>> {
            self.rule_count += 1;
            while input.next().is_ok() {}
            Ok(())
        }

        fn rule_without_block(
            &mut self,
            _prelude: Self::Prelude,
            _start: &cssparser::ParserState,
        ) -> Result<Self::AtRule, ()> {
            self.rule_count += 1;
            Ok(())
        }
    }

    impl<'i> DeclarationParser<'i> for RuleCounter {
        type Declaration = ();
        type Error = ();

        fn parse_value<'t>(
            &mut self,
            _name: CowRcStr<'i>,
            input: &mut Parser<'i, 't>,
        ) -> Result<Self::Declaration, ParseError<'i, ()>> {
            self.decl_count += 1;
            while input.next().is_ok() {}
            Ok(())
        }
    }

    impl<'i> RuleBodyItemParser<'i, (), ()> for RuleCounter {
        fn parse_qualified(&self) -> bool {
            true
        }
        fn parse_declarations(&self) -> bool {
            false
        }
    }

    pub fn parse_css(data: &str) -> (usize, usize) {
        let mut input = ParserInput::new(data);
        let mut parser = Parser::new(&mut input);
        let mut counter = RuleCounter {
            rule_count: 0,
            decl_count: 0,
        };
        let rule_parser = StyleSheetParser::new(&mut parser, &mut counter);
        for result in rule_parser {
            let _ = bencher::black_box(result);
        }
        (counter.rule_count, counter.decl_count)
    }
}

macro_rules! bench_cssparser {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_css($file);
            b.bytes = input.len() as u64;
            let (rules, _decls) = cssparser_visitor::parse_css(&input);
            assert!(rules > 0, concat!($file, ": cssparser found 0 rules"));
            b.iter(|| black_box(cssparser_visitor::parse_css(black_box(&input))));
        }
    };
}

bench_cssparser!(cssparser_normalize, "normalize.css");
bench_cssparser!(cssparser_bootstrap, "bootstrap.css");
bench_cssparser!(cssparser_tailwind, "tailwind.css");

// ── lightningcss (full L2 semantic parse) ───────────────────────────────────

use lightningcss::stylesheet::{ParserOptions, StyleSheet};

fn lightningcss_normalize(b: &mut Bencher) {
    let input = load_css("normalize.css");
    b.bytes = input.len() as u64;
    StyleSheet::parse(&input, ParserOptions::default())
        .expect("normalize.css: lightningcss parse failed");
    b.iter(|| black_box(StyleSheet::parse(black_box(&input), ParserOptions::default()).unwrap()));
}

fn lightningcss_bootstrap(b: &mut Bencher) {
    let input = load_css("bootstrap.css");
    b.bytes = input.len() as u64;
    StyleSheet::parse(&input, ParserOptions::default())
        .expect("bootstrap.css: lightningcss parse failed");
    b.iter(|| black_box(StyleSheet::parse(black_box(&input), ParserOptions::default()).unwrap()));
}

fn lightningcss_tailwind(b: &mut Bencher) {
    let input = load_css("tailwind.css");
    b.bytes = input.len() as u64;
    // lightningcss may fail on synthetic tailwind — skip gracefully
    let test = StyleSheet::parse(&input, ParserOptions::default());
    if test.is_err() {
        eprintln!("lightningcss: skipping tailwind.css (parse error)");
        return;
    }
    b.iter(|| black_box(StyleSheet::parse(black_box(&input), ParserOptions::default()).unwrap()));
}

// ── Groups ──────────────────────────────────────────────────────────────────

benchmark_group!(
    bench_cssparser,
    cssparser_normalize,
    cssparser_bootstrap,
    cssparser_tailwind
);
benchmark_group!(
    bench_lightningcss,
    lightningcss_normalize,
    lightningcss_bootstrap,
    lightningcss_tailwind
);
benchmark_main!(bench_cssparser, bench_lightningcss);
