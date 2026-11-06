//! LG-b baseline probe (sk-v27 LW1 lockstep gate): the Rust half of the
//! TS<->Rust grammar-file accept/reject reading. One line per input file:
//! `<sha256-free path>\t<ACCEPT|REJECT>`. Reads paths from argv.
use bbnf::grammar::generated::bbnf::BbnfBootstrap;
fn main() {
    for path in std::env::args().skip(1) {
        let src = std::fs::read_to_string(&path).expect("read");
        let verdict = match BbnfBootstrap::parse(&src) {
            Ok(_) => "ACCEPT".to_string(),
            Err(e) => format!("REJECT\t{}", format!("{e:?}").replace(['\n', '\t'], " ").chars().take(160).collect::<String>()),
        };
        println!("{path}\t{verdict}");
    }
}
