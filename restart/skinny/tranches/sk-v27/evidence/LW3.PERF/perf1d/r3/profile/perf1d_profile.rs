//! PERF1D-B2 (b) hold profile (diagnostic tree only, never committed): one parse of each json case,
//! the cure's routed calls counted (calls per parse, bytes per call).
use ::bbnf::grammar::generated::json::*;
use ::bbnf::grammar::generated::json::__perf1d_prof as prof;
fn main() {
    println!("case\tinput_bytes\tquote_calls\tquote_bytes\tquote_bytes_per_call\tbitmap64_calls\tskip_slow_calls\tskip_slow_bytes\tskip_slow_bytes_per_call\tstructural_scans\tstructural_bytes");
    for (case, file) in [("data_s", "data.json"), ("twitter", "twitter.json"), ("citm", "citm_catalog.json"), ("canada", "canada.json")] {
        let input = std::fs::read_to_string(bbnf_corpus::path(&format!("json/{file}"))).unwrap();
        prof::reset();
        let doc = JsonParser::parse(&input).unwrap();
        std::hint::black_box(&doc);
        let [qc, qb, ws, sc, sb, stc, stb] = prof::read();
        let per = |b: u64, c: u64| if c == 0 { 0.0 } else { b as f64 / c as f64 };
        println!("{case}\t{}\t{qc}\t{qb}\t{:.1}\t{ws}\t{sc}\t{sb}\t{:.1}\t{stc}\t{stb}", input.len(), per(qb, qc), per(sb, sc));
    }
}
