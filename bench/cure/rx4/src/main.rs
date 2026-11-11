//! sk-v27 LW2.CON-2 (S11 BENCH), PREREG §9: `rx4-driver css_l4|css_pretty FILE K`.
//!
//! Parses FILE K times in this one fresh process with the generated `CssL4Parser` or
//! `CssPrettyParser`. The timed region is the K parses; each document is `black_box`ed.
//! After the loop, the FNV-1a of one document's `Debug` text is printed as the checksum
//! (outside the timed region): the two arms are the same generator at two commits, and the
//! digest is what shows that they did the same work.
use std::hint::black_box;

fn fnv(h: u64, bytes: &[u8]) -> u64 {
    bytes.iter().fold(h, |h, b| (h ^ *b as u64).wrapping_mul(0x100_0000_01b3))
}

fn main() {
    let a: Vec<String> = std::env::args().collect();
    let (kind, path, k) = (a[1].as_str(), &a[2], a[3].parse::<usize>().expect("K"));
    let data = std::fs::read_to_string(path).expect("input");
    let h0: u64 = 0xcbf2_9ce4_8422_2325;
    let h = match kind {
        "css_l4" => {
            use bbnf::grammar::generated::css_l4::CssL4Parser;
            for _ in 0..k {
                black_box(CssL4Parser::parse(black_box(&data)).expect("css_l4 parse"));
            }
            fnv(h0, format!("{:?}", CssL4Parser::parse(&data).expect("css_l4").root).as_bytes())
        }
        "css_pretty" => {
            use bbnf::grammar::generated::css_pretty::CssPrettyParser;
            for _ in 0..k {
                black_box(CssPrettyParser::parse(black_box(&data)).expect("css_pretty parse"));
            }
            fnv(h0, format!("{:?}", CssPrettyParser::parse(&data).expect("css_pretty").root).as_bytes())
        }
        _ => panic!("kind css_l4|css_pretty"),
    };
    println!("checksum={h:016x}");
}
