//! Alloc-count probe (PROVE-09). A counting global allocator tallies heap
//! allocations during (1) parser construction and (2) one cold parse, so the
//! vtable-box allocation profile is visible: the combinator vtable boxes are
//! paid at CONSTRUCTION, never per-parse — which is why the smallbox→stable
//! de-nightly is cyc/B-neutral on the hot path.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};

static ALLOCS: AtomicUsize = AtomicUsize::new(0);
static BYTES: AtomicUsize = AtomicUsize::new(0);

struct Counting;
unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, l: Layout) -> *mut u8 {
        ALLOCS.fetch_add(1, Relaxed);
        BYTES.fetch_add(l.size(), Relaxed);
        unsafe { System.alloc(l) }
    }
    unsafe fn dealloc(&self, p: *mut u8, l: Layout) {
        unsafe { System.dealloc(p, l) }
    }
    unsafe fn realloc(&self, p: *mut u8, l: Layout, n: usize) -> *mut u8 {
        ALLOCS.fetch_add(1, Relaxed);
        BYTES.fetch_add(n, Relaxed);
        unsafe { System.realloc(p, l, n) }
    }
}

#[global_allocator]
static A: Counting = Counting;

use parse_that::json::json_parser;

fn main() {
    // (1) parser-tree construction allocs (vtable boxes live here).
    let a0 = ALLOCS.load(Relaxed);
    let b0 = BYTES.load(Relaxed);
    let parser = json_parser();
    let a1 = ALLOCS.load(Relaxed);
    let b1 = BYTES.load(Relaxed);
    std::hint::black_box(&parser);
    println!(
        "construct json_parser(): allocs={} bytes={}",
        a1 - a0,
        b1 - b0
    );

    // (2) per-parse allocs (Vec growth in many/sep_by + arenas) — the hot loop.
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../data/json/twitter.json");
    let data = std::fs::read_to_string(&path).unwrap();
    let a2 = ALLOCS.load(Relaxed);
    let v = parser.parse(&data).unwrap();
    let a3 = ALLOCS.load(Relaxed);
    std::hint::black_box(v);
    println!("parse twitter.json ({} B): allocs={}", data.len(), a3 - a2);
}
