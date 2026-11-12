//! sk-v27 LW3.ORC1-CKPT (ruling ORC-1, 2026-11-12a): a builder checkpoint
//! must not copy the open-frame stack.
//!
//! JSONTestSuite (`n_structure_100000_opening_arrays`,
//! `n_structure_open_array_object`, in custody) names the defect: every
//! nesting level takes a checkpoint, each checkpoint cloned the whole stack of
//! open frames, and the checkpoints of all enclosing levels are live at once,
//! so the memory a parse holds grows with the square of its depth (4,000
//! levels 0.7 GB, 16,000 levels 10.5 GB). The clone was in
//! `builder_template.rs` and in the JSON, bbnf, css_l4 and google_sheets
//! builders the xtask generators write.
//!
//! The falsifier: for each builder, the peak heap a parse holds at depth 4d
//! is at most 8 times its peak at depth d (linear growth reads about 4;
//! quadratic growth about 16). The simple-cohort template is read through the
//! EBNF parser (BNF, CSV, CSS Pretty and Math share it; none of them nests).
//! Both an accepted (balanced) and a rejected (unclosed) input are read: the
//! rejected one keeps every level's checkpoint live until the failure
//! unwinds, which is JSONTestSuite's case.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};

use bbnf::grammar::generated::{
    bbnf::BbnfBootstrap, css_l4::CssL4Parser, ebnf::EbnfParser, google_sheets::GoogleSheetsParser,
    json::JsonParser,
};

struct Counting;

static CURRENT: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicUsize = AtomicUsize::new(0);
/// One measurement at a time: the counters are process-wide.
static SERIAL: Mutex<()> = Mutex::new(());

unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, l: Layout) -> *mut u8 {
        let p = unsafe { System.alloc(l) };
        if !p.is_null() {
            let now = CURRENT.fetch_add(l.size(), Ordering::Relaxed) + l.size();
            PEAK.fetch_max(now, Ordering::Relaxed);
        }
        p
    }
    unsafe fn dealloc(&self, p: *mut u8, l: Layout) {
        unsafe { System.dealloc(p, l) };
        CURRENT.fetch_sub(l.size(), Ordering::Relaxed);
    }
    unsafe fn realloc(&self, p: *mut u8, l: Layout, new: usize) -> *mut u8 {
        let q = unsafe { System.realloc(p, l, new) };
        if !q.is_null() {
            if new >= l.size() {
                let now = CURRENT.fetch_add(new - l.size(), Ordering::Relaxed) + (new - l.size());
                PEAK.fetch_max(now, Ordering::Relaxed);
            } else {
                CURRENT.fetch_sub(l.size() - new, Ordering::Relaxed);
            }
        }
        q
    }
}

#[global_allocator]
static A: Counting = Counting;

/// The peak heap, above the level at entry, that `f` holds on a thread with a
/// 512 MiB stack (root's parsers recurse once per nesting level).
fn peak_of(f: impl FnOnce() -> bool + Send + 'static) -> (bool, usize) {
    let _g = SERIAL.lock().unwrap_or_else(|e| e.into_inner());
    let t = std::thread::Builder::new()
        .stack_size(512 << 20)
        .spawn(move || {
            let base = CURRENT.load(Ordering::Relaxed);
            PEAK.store(base, Ordering::Relaxed);
            let verdict = std::hint::black_box(f());
            (verdict, PEAK.load(Ordering::Relaxed).saturating_sub(base))
        })
        .expect("spawn");
    t.join().expect("parse thread")
}

/// Reads one builder at depths `d` and `4d` and returns the ratio of peaks.
fn growth(name: &str, d: usize, make: fn(usize) -> String, parse: fn(&str) -> bool, want: bool) {
    let small = make(d);
    let large = make(4 * d);
    let (va, a) = peak_of(move || parse(&small));
    let (vb, b) = peak_of(move || parse(&large));
    assert_eq!(va, want, "{name}: depth {d} verdict");
    assert_eq!(vb, want, "{name}: depth {} verdict", 4 * d);
    let ratio = b as f64 / a.max(1) as f64;
    eprintln!("{name}: peak {a} B at depth {d}, {b} B at depth {}; ratio {ratio:.2}", 4 * d);
    assert!(
        ratio <= 8.0,
        "{name}: peak heap grows faster than linearly in depth ({a} B at {d}, {b} B at {}, ratio {ratio:.2}): a checkpoint copies the open-frame stack",
        4 * d
    );
}

const D: usize = 250;

#[test]
fn json_checkpoint_is_independent_of_depth() {
    growth(
        "json unclosed",
        D,
        |d| "[".repeat(d),
        |s| JsonParser::parse(s).is_ok(),
        false,
    );
    growth(
        "json balanced",
        D,
        |d| format!("{}{}", "[".repeat(d), "]".repeat(d)),
        |s| JsonParser::parse(s).is_ok(),
        true,
    );
    growth(
        "json objects unclosed",
        D,
        |d| "[{\"\":".repeat(d),
        |s| JsonParser::parse(s).is_ok(),
        false,
    );
}

#[test]
fn simple_template_checkpoint_is_independent_of_depth() {
    growth(
        "ebnf unclosed",
        D,
        |d| format!("a = {}\"x\" ;", "( ".repeat(d)),
        |s| EbnfParser::parse(s).is_ok(),
        false,
    );
    growth(
        "ebnf balanced",
        D,
        |d| format!("a = {}\"x\"{} ;", "( ".repeat(d), " )".repeat(d)),
        |s| EbnfParser::parse(s).is_ok(),
        true,
    );
}

#[test]
fn bbnf_checkpoint_is_independent_of_depth() {
    growth(
        "bbnf balanced",
        D,
        |d| format!("a = {}\"x\"{} ;", "(".repeat(d), ")".repeat(d)),
        |s| BbnfBootstrap::parse(s).is_ok(),
        true,
    );
    growth(
        "bbnf unclosed",
        D,
        |d| format!("a = {}\"x\" ;", "(".repeat(d)),
        |s| BbnfBootstrap::parse(s).is_ok(),
        false,
    );
}

#[test]
fn google_sheets_checkpoint_is_independent_of_depth() {
    growth(
        "google_sheets balanced",
        D,
        |d| format!("={}1{}", "(".repeat(d), ")".repeat(d)),
        |s| GoogleSheetsParser::parse(s).is_ok(),
        true,
    );
    growth(
        "google_sheets unclosed",
        D,
        |d| format!("={}1", "(".repeat(d)),
        |s| GoogleSheetsParser::parse(s).is_ok(),
        false,
    );
}

#[test]
fn css_l4_checkpoint_is_independent_of_depth() {
    growth(
        "css_l4 balanced",
        D,
        |d| format!("{}{}", "a{".repeat(d), "}".repeat(d)),
        |s| CssL4Parser::parse(s).is_ok(),
        true,
    );
    growth(
        "css_l4 unclosed",
        D,
        |d| "a{".repeat(d),
        |s| CssL4Parser::parse(s).is_ok(),
        false,
    );
}
