use super::config;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CssFactError {
    pub offset: usize,
    pub message: &'static str,
}

impl fmt::Display for CssFactError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at byte {}", self.message, self.offset)
    }
}

impl std::error::Error for CssFactError {}

pub struct FactSink {
    out: String,
    rules: u32,
    media_queries: u32,
    media_features: u32,
    keyframes: u32,
    keyframe_selectors: u32,
    declarations: u32,
}

impl FactSink {
    pub fn new(input: &str) -> Self {
        let mut out = String::with_capacity(1536);
        out.push_str(config::FACT_SCHEMA);
        out.push('\n');
        out.push_str("row\tid=");
        out.push_str(config::ROW_ID);
        out.push_str("\tplane=");
        out.push_str(config::OUTPUT_PLANE);
        out.push('\n');
        out.push_str("source\tinput_fnv64=");
        push_hex64(&mut out, fnv64(input.as_bytes()));
        out.push_str("\tinput_bytes=");
        out.push_str(&input.len().to_string());
        out.push('\n');
        Self {
            out,
            rules: 0,
            media_queries: 0,
            media_features: 0,
            keyframes: 0,
            keyframe_selectors: 0,
            declarations: 0,
        }
    }

    pub fn stylesheet(&mut self, rules: u32) {
        self.out.push_str("stylesheet\trules=");
        self.out.push_str(&rules.to_string());
        self.out.push('\n');
    }

    pub fn media_rule(
        &mut self,
        idx: u32,
        start: usize,
        end: usize,
        prelude_start: usize,
        prelude_end: usize,
        body_start: usize,
        body_end: usize,
        queries: u32,
        children: u32,
    ) {
        self.rules += 1;
        self.out.push_str("at_rule\tidx=");
        self.out.push_str(&idx.to_string());
        self.out.push_str("\tkind=media\tstart=");
        self.out.push_str(&start.to_string());
        self.out.push_str("\tend=");
        self.out.push_str(&end.to_string());
        self.out.push_str("\tprelude_start=");
        self.out.push_str(&prelude_start.to_string());
        self.out.push_str("\tprelude_end=");
        self.out.push_str(&prelude_end.to_string());
        self.out.push_str("\tbody_start=");
        self.out.push_str(&body_start.to_string());
        self.out.push_str("\tbody_end=");
        self.out.push_str(&body_end.to_string());
        self.out.push_str("\tqueries=");
        self.out.push_str(&queries.to_string());
        self.out.push_str("\tchildren=");
        self.out.push_str(&children.to_string());
        self.out.push('\n');
    }

    pub fn media_query(&mut self, rule: u32, idx: u32, text: &str) {
        self.media_queries += 1;
        self.out.push_str("media_query\trule=");
        self.out.push_str(&rule.to_string());
        self.out.push_str("\tidx=");
        self.out.push_str(&idx.to_string());
        self.out.push_str("\ttext_hex=");
        push_ascii_lower_hex(&mut self.out, text);
        self.out.push('\n');
    }

    pub fn media_feature(&mut self, rule: u32, query: u32, idx: u32, name: &str, value: &str) {
        self.media_features += 1;
        self.out.push_str("media_feature\trule=");
        self.out.push_str(&rule.to_string());
        self.out.push_str("\tquery=");
        self.out.push_str(&query.to_string());
        self.out.push_str("\tidx=");
        self.out.push_str(&idx.to_string());
        self.out.push_str("\tname_hex=");
        push_ascii_lower_hex(&mut self.out, name);
        self.out.push_str("\tvalue_hex=");
        push_ascii_lower_hex(&mut self.out, value);
        self.out.push('\n');
    }

    pub fn body_rule(
        &mut self,
        parent: u32,
        idx: u32,
        selector: &str,
        start: usize,
        end: usize,
        decls: u32,
    ) {
        self.out.push_str("body_rule\tparent=");
        self.out.push_str(&parent.to_string());
        self.out.push_str("\tidx=");
        self.out.push_str(&idx.to_string());
        self.out.push_str("\tkind=qualified\tselector_hex=");
        push_ascii_lower_hex(&mut self.out, selector);
        self.out.push_str("\tstart=");
        self.out.push_str(&start.to_string());
        self.out.push_str("\tend=");
        self.out.push_str(&end.to_string());
        self.out.push_str("\tdecls=");
        self.out.push_str(&decls.to_string());
        self.out.push('\n');
    }

    pub fn keyframes_rule(
        &mut self,
        idx: u32,
        start: usize,
        end: usize,
        name: &str,
        body_start: usize,
        body_end: usize,
        frames: u32,
    ) {
        self.rules += 1;
        self.keyframes += 1;
        self.out.push_str("at_rule\tidx=");
        self.out.push_str(&idx.to_string());
        self.out.push_str("\tkind=keyframes\tstart=");
        self.out.push_str(&start.to_string());
        self.out.push_str("\tend=");
        self.out.push_str(&end.to_string());
        self.out.push_str("\tname_hex=");
        push_ascii_lower_hex(&mut self.out, name);
        self.out.push_str("\tbody_start=");
        self.out.push_str(&body_start.to_string());
        self.out.push_str("\tbody_end=");
        self.out.push_str(&body_end.to_string());
        self.out.push_str("\tframes=");
        self.out.push_str(&frames.to_string());
        self.out.push('\n');
    }

    pub fn keyframe(
        &mut self,
        rule: u32,
        idx: u32,
        selector: &str,
        selector_count: u32,
        start: usize,
        end: usize,
        decls: u32,
    ) {
        self.out.push_str("keyframe\trule=");
        self.out.push_str(&rule.to_string());
        self.out.push_str("\tidx=");
        self.out.push_str(&idx.to_string());
        self.out.push_str("\tselectors=");
        self.out.push_str(&selector_count.to_string());
        self.out.push_str("\tselector_hex=");
        push_ascii_lower_hex(&mut self.out, selector);
        self.out.push_str("\tstart=");
        self.out.push_str(&start.to_string());
        self.out.push_str("\tend=");
        self.out.push_str(&end.to_string());
        self.out.push_str("\tdecls=");
        self.out.push_str(&decls.to_string());
        self.out.push('\n');
    }

    pub fn keyframe_selector(&mut self, rule: u32, frame: u32, idx: u32, kind: &str, value: &str) {
        self.keyframe_selectors += 1;
        self.out.push_str("key_sel\trule=");
        self.out.push_str(&rule.to_string());
        self.out.push_str("\tframe=");
        self.out.push_str(&frame.to_string());
        self.out.push_str("\tidx=");
        self.out.push_str(&idx.to_string());
        self.out.push_str("\tkind=");
        self.out.push_str(kind);
        self.out.push_str("\tvalue_hex=");
        push_ascii_lower_hex(&mut self.out, value);
        self.out.push('\n');
    }

    pub fn declaration(
        &mut self,
        parent: u32,
        frame: Option<u32>,
        idx: u32,
        property: &str,
        value: &str,
    ) {
        self.declarations += 1;
        self.out.push_str("decl\tparent=");
        self.out.push_str(&parent.to_string());
        self.out.push_str("\tframe=");
        match frame {
            Some(frame) => self.out.push_str(&frame.to_string()),
            None => self.out.push_str("none"),
        }
        self.out.push_str("\tidx=");
        self.out.push_str(&idx.to_string());
        self.out.push_str("\tproperty_hex=");
        push_ascii_lower_hex(&mut self.out, property);
        self.out.push_str("\tvalue_hex=");
        push_ascii_lower_hex(&mut self.out, value);
        self.out.push('\n');
    }

    pub fn finish(mut self) -> String {
        let stream_hash = fnv64(self.out.as_bytes());
        self.out.push_str("end\trules=");
        self.out.push_str(&self.rules.to_string());
        self.out.push_str("\tmedia_queries=");
        self.out.push_str(&self.media_queries.to_string());
        self.out.push_str("\tmedia_features=");
        self.out.push_str(&self.media_features.to_string());
        self.out.push_str("\tkeyframes=");
        self.out.push_str(&self.keyframes.to_string());
        self.out.push_str("\tkeyframe_selectors=");
        self.out.push_str(&self.keyframe_selectors.to_string());
        self.out.push_str("\tdeclarations=");
        self.out.push_str(&self.declarations.to_string());
        self.out.push_str("\tstream_fnv64=");
        push_hex64(&mut self.out, stream_hash);
        self.out.push('\n');
        self.out
    }
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
