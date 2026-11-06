//! LOCK-5 pass-2 probe (c): the "two-views" claim, RUN on the real crates.
//!
//! - Id-view  = the egraph-derived Language (span-erased, Eq+Hash) — keys the
//!   hashcons / any Id-keyed cache.
//! - Spanned-view = the authored node (Serialize, source offsets, NO Eq/Hash).
//!
//! This binary exercises whether merging `bbnf_regex::CharClass` / `Look` as
//! leaves into the Id-view enum:
//!   (1) keeps the `egraph_derive::Language` projection TOTAL (compiles),
//!   (2) keeps the enum's `Eq + Hash` derive (requires CharClass/Look: Eq+Hash),
//!   (3) lets the egraph hashcons collapse two source offsets to ONE e-class,
//!   (4) does NOT false-merge distinct classes,
//!   (5) the Id-view can derive Eq+Hash WITHOUT any span type being forced Eq/Hash.

use egraph::EGraph;
use egraph_derive::Language;

use bbnf_regex::{ByteRange, CharClass, Look};

/// The **Id-view**: span-erased, derives Eq + Hash (for hashcons keying) and
/// Language (auto). `Class` / `Look` are MERGED regex leaves — the 5A claim.
#[derive(Clone, PartialEq, Eq, Hash, Debug, Language)]
enum MiniENode {
    // pre-existing leaf shape (like StringId) — proves nothing new
    Literal(u32),
    // ── 5A merge: regex HIR leaves folded into the ONE Id-view enum ──
    Class(CharClass),
    Look(Look),
    // recursive children so the derive has a non-trivial slice arm to project
    Seq(Box<[::egraph::Id]>),
    Negate(::egraph::Id),
}

/// The **spanned authored view**: carries source offsets, derives Serialize,
/// and does NOT derive Eq/Hash. It projects DOWN to the Id-view by dropping
/// the span. NOTE: the real `bbnf_regex::CharClass` is NOT `serde::Serialize`
/// (proven separately by the `bbnf-ir` build: E0277). So the authored view
/// here carries a serde-safe structural projection of the class, NOT the raw
/// CharClass — that gap is itself the refutation of "IrNode still derives
/// Serialize" once `Class(CharClass)` is a variant.
#[derive(Clone, Debug, serde::Serialize)]
struct SpannedClass {
    start: u32,
    end: u32,
    /// serde-safe projection of the byte ranges (raw CharClass is NOT Serialize)
    ranges: Vec<(u8, u8)>,
    negated: bool,
}

fn bytes_class(ranges: &[(u8, u8)], negated: bool) -> CharClass {
    CharClass::Bytes {
        ranges: ranges.iter().map(|&(a, b)| ByteRange::new(a, b)).collect(),
        negated,
    }
}

/// Lower a spanned authored node to the span-erased Id-view.
fn lower(s: &SpannedClass) -> MiniENode {
    MiniENode::Class(bytes_class(
        &s.ranges.iter().copied().collect::<Vec<_>>(),
        s.negated,
    ))
}

fn main() {
    let mut pass = true;
    let mut check = |name: &str, ok: bool| {
        println!("[{}] {}", if ok { "PASS" } else { "FAIL" }, name);
        pass &= ok;
    };

    // (1)+(2): if this file compiles at all, the Language derive is TOTAL over
    // Class(CharClass)/Look(Look) leaves AND MiniENode still derives Eq+Hash.
    check("compiles => Language projection total over regex leaves + Eq/Hash intact", true);

    let mut eg: EGraph<MiniENode> = EGraph::default();

    // (3) TWO SOURCE OFFSETS, identical class content -> ONE e-class.
    // Two authored spanned nodes at DISTINCT offsets:
    let a = SpannedClass { start: 10, end: 15, ranges: vec![(b'a', b'z')], negated: false };
    let b = SpannedClass { start: 200, end: 205, ranges: vec![(b'a', b'z')], negated: false };
    // serialize both to show the spanned view really carries offsets:
    let ja = serde_json::to_string(&a).unwrap();
    let jb = serde_json::to_string(&b).unwrap();
    check("spanned view Serialize intact + offsets distinct",
          ja.contains("\"start\":10") && jb.contains("\"start\":200") && ja != jb);

    let id_a = eg.add(lower(&a)); // offset 10
    let id_b = eg.add(lower(&b)); // offset 200
    check("two offsets of the same Class collapse to ONE e-class (span-erased Id-view)",
          eg.find(id_a) == eg.find(id_b));

    // (4) a DIFFERENT class must NOT collapse into that e-class.
    let c = SpannedClass { start: 0, end: 5, ranges: vec![(b'0', b'9')], negated: false };
    let id_c = eg.add(lower(&c));
    check("distinct class content stays a distinct e-class (no false-merge)",
          eg.find(id_c) != eg.find(id_a));

    // negated variant of the same ranges is also distinct (CharClass::negated in Eq/Hash)
    let d = bytes_class(&[(b'a', b'z')], true);
    let id_d = eg.add(MiniENode::Class(d));
    check("negated [^a-z] distinct from [a-z] (negated flag lives in Eq/Hash key)",
          eg.find(id_d) != eg.find(id_a));

    // (3') Look leaves hashcons the same way.
    let l1 = eg.add(MiniENode::Look(Look::Start));
    let l2 = eg.add(MiniENode::Look(Look::Start));
    let l3 = eg.add(MiniENode::Look(Look::End));
    check("identical Look collapses to one e-class; distinct Look stays distinct",
          eg.find(l1) == eg.find(l2) && eg.find(l1) != eg.find(l3));

    // (5) Id-view has Eq+Hash but we never asked SpannedClass for Eq/Hash.
    // (compile-time fact; SpannedClass derives only Clone/Debug/Serialize.)
    check("Id-view Eq/Hash obtained WITHOUT forcing Eq/Hash onto the spanned type", true);

    println!("\nRESULT: {}", if pass { "ALL-PASS" } else { "SOME-FAIL" });
    std::process::exit(if pass { 0 } else { 1 });
}
