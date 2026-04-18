//! AW-V.W2 twin-pair bench — prototype vs. sonic-rs head-to-head.
//!
//! Isomorphic to `crates/core/benches/json/value.rs`: identical
//! corpus ({data, twitter, citm, canada, data_xl}), identical
//! loader, identical `black_box` discipline, identical
//! `benchmark_group!` shape. The only difference is the `bbnf`
//! entries invoke [`bbnf_json_prototype::parse_json`] with a
//! [`ValueVisitor`] (sonic-parity materialisation) rather than
//! routing through `JsonParser::parse` + the tape-walk fold.
//!
//! Hard gate per AW-V.W2: each prototype entry's ns/iter is within
//! **10%** of its sonic-rs twin. Wider gaps diagnose via samply on
//! the prototype binary.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf_json_prototype::{parse_json, Document, TapeVisitor, ValueVisitor};
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

fn load(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("{}: {}", path, e))
}

// ── Prototype (ValueVisitor) ────────────────────────────────────────

macro_rules! bench_prototype_value {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load($file);
            b.bytes = input.len() as u64;
            // Warm-up parse — verify the input parses cleanly so the
            // bench loop is measuring a successful path.
            {
                let mut visitor = ValueVisitor::with_input(input.as_bytes());
                parse_json(input.as_bytes(), &mut visitor)
                    .unwrap_or_else(|e| panic!(concat!($file, ": parse failed: {:?}"), e));
                black_box(visitor.finish());
            }
            b.iter(|| {
                // Zero-copy borrow path — strings reference the
                // input slice directly.
                let mut visitor =
                    ValueVisitor::with_input(black_box(input.as_bytes()));
                parse_json(black_box(input.as_bytes()), &mut visitor).unwrap();
                let doc: Document = visitor.finish();
                black_box(doc);
            });
        }
    };
}

bench_prototype_value!(proto_value_data_s, "data.json");
bench_prototype_value!(proto_value_twitter, "twitter.json");
bench_prototype_value!(proto_value_citm, "citm_catalog.json");
bench_prototype_value!(proto_value_canada, "canada.json");
bench_prototype_value!(proto_value_data_xl, "data_xl.json");

// ── Prototype (TapeVisitor) ─────────────────────────────────────────
//
// Verifies the same parse body inline-emits against the AW-IV tape
// substrate; provides the ratio against the ValueVisitor path for
// substrate-overhead isolation.

macro_rules! bench_prototype_tape {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load($file);
            b.bytes = input.len() as u64;
            {
                let mut visitor = TapeVisitor::new(input.as_bytes());
                parse_json(input.as_bytes(), &mut visitor)
                    .unwrap_or_else(|e| panic!(concat!($file, ": parse failed: {:?}"), e));
                let tape = visitor.finish().unwrap();
                black_box(tape);
            }
            b.iter(|| {
                let mut visitor = TapeVisitor::new(black_box(input.as_bytes()));
                parse_json(black_box(input.as_bytes()), &mut visitor).unwrap();
                let tape = visitor.finish().unwrap();
                black_box(tape);
            });
        }
    };
}

bench_prototype_tape!(proto_tape_data_s, "data.json");
bench_prototype_tape!(proto_tape_twitter, "twitter.json");
bench_prototype_tape!(proto_tape_citm, "citm_catalog.json");
bench_prototype_tape!(proto_tape_canada, "canada.json");
bench_prototype_tape!(proto_tape_data_xl, "data_xl.json");

// ── Sonic-rs twin ───────────────────────────────────────────────────

macro_rules! bench_sonic {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load($file);
            b.bytes = input.len() as u64;
            sonic_rs::from_str::<sonic_rs::Value>(&input)
                .expect(concat!($file, ": sonic-rs parse failed"));
            b.iter(|| sonic_rs::from_str::<sonic_rs::Value>(black_box(&input)).unwrap());
        }
    };
}

bench_sonic!(sonic_data_s, "data.json");
bench_sonic!(sonic_twitter, "twitter.json");
bench_sonic!(sonic_citm, "citm_catalog.json");
bench_sonic!(sonic_canada, "canada.json");
bench_sonic!(sonic_data_xl, "data_xl.json");

benchmark_group!(
    proto_value,
    proto_value_data_s,
    proto_value_twitter,
    proto_value_citm,
    proto_value_canada,
    proto_value_data_xl,
);
benchmark_group!(
    proto_tape,
    proto_tape_data_s,
    proto_tape_twitter,
    proto_tape_citm,
    proto_tape_canada,
    proto_tape_data_xl,
);
benchmark_group!(
    sonic,
    sonic_data_s,
    sonic_twitter,
    sonic_citm,
    sonic_canada,
    sonic_data_xl,
);

benchmark_main!(proto_value, proto_tape, sonic);
