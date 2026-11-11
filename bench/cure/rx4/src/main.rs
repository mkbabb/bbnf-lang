//! sk-v27 LW2.CON-2 (S11 BENCH), PREREG §9: `rx4-driver css_l4|css_pretty FILE K`.
//!
//! Parses FILE K times in this one fresh process with the generated `CssL4Parser` or
//! `CssPrettyParser`. The timed region is the K parses; each document is `black_box`ed.
//! Under DIGEST=1 (the equal-work proof, read once per arm and input before timing) the
//! FNV-1a of one document's `Debug` text is the checksum: the two arms are the same generator
//! at two commits, and the digest is what shows that they did the same work.
use std::hint::black_box;

fn fnv(h: u64, bytes: &[u8]) -> u64 {
    bytes.iter().fold(h, |h, b| (h ^ *b as u64).wrapping_mul(0x100_0000_01b3))
}

/// Streams `Debug` text into FNV-1a without building the string (a tailwind AST's text is
/// larger than the machine-law RSS cap).
struct Fnv(u64);
impl std::fmt::Write for Fnv {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0 = fnv(self.0, s.as_bytes());
        Ok(())
    }
}
fn digest_of(h: u64, v: &impl std::fmt::Debug) -> u64 {
    use std::fmt::Write as _;
    let mut w = Fnv(h);
    write!(w, "{v:?}").expect("fmt");
    w.0
}

fn main() {
    let a: Vec<String> = std::env::args().collect();
    let (kind, path, k) = (a[1].as_str(), &a[2], a[3].parse::<usize>().expect("K"));
    let data = std::fs::read_to_string(path).expect("input");
    // DIGEST=1: the equal-work proof (one Debug digest; slow, never in a timed run).
    let digest = std::env::var_os("DIGEST").is_some();
    let h0: u64 = 0xcbf2_9ce4_8422_2325;
    let h = match kind {
        "css_l4" => {
            use bbnf::grammar::generated::css_l4::CssL4Parser;
            for _ in 0..k {
                black_box(CssL4Parser::parse(black_box(&data)).expect("css_l4 parse"));
            }
            if digest { digest_of(h0, &CssL4Parser::parse(&data).expect("css_l4").root) } else { h0 ^ k as u64 }
        }
        "css_pretty" => {
            use bbnf::grammar::generated::css_pretty::CssPrettyParser;
            for _ in 0..k {
                black_box(CssPrettyParser::parse(black_box(&data)).expect("css_pretty parse"));
            }
            if digest { digest_of(h0, &CssPrettyParser::parse(&data).expect("css_pretty").root) } else { h0 ^ k as u64 }
        }
        _ => panic!("kind css_l4|css_pretty"),
    };
    println!("checksum={h:016x}");
}
