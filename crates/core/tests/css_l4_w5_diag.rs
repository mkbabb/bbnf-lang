// SK-V16 W5 per-file diagnostic.
use bbnf::grammar::generated::css_l4::CssL4Parser;
use bbnf::runtime::css_l4::{
    visit_document, CssColor, CssFunction, CssRule, CssTypedValue, CssVisitor, Declaration,
    GenericAtRule, KeyframeBlock, KeyframesRule, MediaRule, Selector, StyleRule, StyleSheet,
};
use cssparser::{
    AtRuleParser, CowRcStr, DeclarationParser, Parser, ParserInput, ParserState,
    QualifiedRuleParser, RuleBodyItemParser, RuleBodyParser, StyleSheetParser, Token,
};
use std::path::{Path, PathBuf};

const FILES: &[(&str, &str)] = &[
    ("bootstrap", "bootstrap-5.3.3.min.css"),
    ("tailwindcss", "tailwindcss-0.2.0.min.css"),
    ("material", "material-components-web-14.0.0.min.css"),
    ("animate", "animate-4.1.1.min.css"),
];

fn corpus_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..").join("skinny/corpora/css-l4-sk-v14")
}

#[derive(Default, Debug)]
struct Sum { rules: u64, style_rules: u64, media_rules: u64, keyframes_rules: u64, generic_at_rules: u64, keyframe_blocks: u64, selectors: u64, declarations: u64, values: u64 }

#[derive(Default)]
struct T1 { s: Sum }
impl<'p> CssVisitor<'p> for T1 {
    fn visit_rule(&mut self, _: &CssRule<'p>) { self.s.rules += 1; }
    fn visit_style_rule(&mut self, _: &StyleRule<'p>) { self.s.style_rules += 1; }
    fn visit_media_rule(&mut self, _: &MediaRule<'p>) { self.s.media_rules += 1; }
    fn visit_keyframes_rule(&mut self, _: &KeyframesRule<'p>) { self.s.keyframes_rules += 1; }
    fn visit_generic_at_rule(&mut self, _: &GenericAtRule<'p>) { self.s.generic_at_rules += 1; }
    fn visit_keyframe_block(&mut self, _: &KeyframeBlock<'p>) { self.s.keyframe_blocks += 1; }
    fn visit_selector(&mut self, _: &Selector<'p>) { self.s.selectors += 1; }
    fn visit_declaration(&mut self, _: &Declaration<'p>) { self.s.declarations += 1; }
    fn visit_value(&mut self, _: &CssTypedValue<'p>) { self.s.values += 1; }
    fn visit_color(&mut self, _: &CssColor<'p>) {}
    fn visit_function(&mut self, _: &CssFunction<'p>) {}
}

#[derive(Default)]
struct Probe { s: Sum }
impl Probe {
    fn nested<'i,'t>(&mut self, input:&mut Parser<'i,'t>)->Result<(),cssparser::ParseError<'i,String>>{
        for item in RuleBodyParser::new(input,self){ item.map_err(|(e,_)|e)?; } Ok(())
    }
    fn comps<'i,'t>(&mut self, input:&mut Parser<'i,'t>)->Result<(),cssparser::ParseError<'i,String>>{
        loop {
            let token = match input.next_including_whitespace_and_comments().cloned(){Ok(t)=>t,Err(_)=>break};
            match token {
                Token::WhiteSpace(_)|Token::Comment(_)=>{}
                Token::Function(_)=>{ self.s.values+=1; input.parse_nested_block(|i|self.comps(i))?; }
                Token::ParenthesisBlock|Token::SquareBracketBlock|Token::CurlyBracketBlock=>{ self.s.values+=1; input.parse_nested_block(|i|self.comps(i))?; }
                Token::BadUrl(_)|Token::BadString(_)=>return Err(input.new_unexpected_token_error(token)),
                _=>self.s.values+=1,
            }
        } Ok(())
    }
}
impl<'i> DeclarationParser<'i> for Probe {
    type Declaration=(); type Error=String;
    fn parse_value<'t>(&mut self,_:CowRcStr<'i>,input:&mut Parser<'i,'t>)->Result<(),cssparser::ParseError<'i,String>>{ self.s.declarations+=1; self.comps(input) }
}
impl<'i> AtRuleParser<'i> for Probe {
    type Prelude=(); type AtRule=(); type Error=String;
    fn parse_prelude<'t>(&mut self,name:CowRcStr<'i>,input:&mut Parser<'i,'t>)->Result<(),cssparser::ParseError<'i,String>>{
        self.s.rules+=1; let n=&*name;
        if n.eq_ignore_ascii_case("media"){self.s.media_rules+=1;}
        else if n.eq_ignore_ascii_case("keyframes")||n.eq_ignore_ascii_case("-webkit-keyframes"){self.s.keyframes_rules+=1;}
        else {self.s.generic_at_rules+=1;}
        self.comps(input)
    }
    fn rule_without_block(&mut self,_:(),_:&ParserState)->Result<(),()>{Ok(())}
    fn parse_block<'t>(&mut self,_:(),_:&ParserState,input:&mut Parser<'i,'t>)->Result<(),cssparser::ParseError<'i,String>>{ self.nested(input) }
}
impl<'i> QualifiedRuleParser<'i> for Probe {
    type Prelude=(); type QualifiedRule=(); type Error=String;
    fn parse_prelude<'t>(&mut self,input:&mut Parser<'i,'t>)->Result<(),cssparser::ParseError<'i,String>>{ self.s.rules+=1; self.s.style_rules+=1; self.s.selectors+=1; self.comps(input) }
    fn parse_block<'t>(&mut self,_:(),_:&ParserState,input:&mut Parser<'i,'t>)->Result<(),cssparser::ParseError<'i,String>>{ self.nested(input) }
}
impl<'i> RuleBodyItemParser<'i,(),String> for Probe { fn parse_declarations(&self)->bool{true} fn parse_qualified(&self)->bool{true} }

#[test]
fn css_l4_w5_per_file_diag() {
    for (id, file) in FILES {
        let src = std::fs::read_to_string(corpus_dir().join(file)).expect("read corpus");
        let (t1_ok, t1) = match CssL4Parser::parse(&src) {
            Ok(doc) => { let mut v=T1::default(); visit_document(&doc,&mut v); (true,v.s) }
            Err(e) => { eprintln!("DIAG {id}: track1 PARSE_ERR {e:?}"); (false,Sum::default()) }
        };
        let mut pi = ParserInput::new(&src);
        let mut p = Parser::new(&mut pi);
        let mut probe = Probe::default();
        let mut cp_ok = true;
        for item in StyleSheetParser::new(&mut p, &mut probe) { if item.is_err(){cp_ok=false;break;} }
        eprintln!(
            "DIAG {id}: t1_ok={t1_ok} cp_ok={cp_ok} | rules {}/{} | style {}/{} | media {}/{} | kf {}/{} | generic {}/{} | kfblk {}/{} | sel {}/{} | decl {}/{} | val {}/{}",
            t1.rules,probe.s.rules, t1.style_rules,probe.s.style_rules, t1.media_rules,probe.s.media_rules,
            t1.keyframes_rules,probe.s.keyframes_rules, t1.generic_at_rules,probe.s.generic_at_rules,
            t1.keyframe_blocks,probe.s.keyframe_blocks, t1.selectors,probe.s.selectors,
            t1.declarations,probe.s.declarations, t1.values,probe.s.values,
        );
    }
}
