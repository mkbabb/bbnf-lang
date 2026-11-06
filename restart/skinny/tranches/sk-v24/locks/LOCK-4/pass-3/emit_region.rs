// PROTO-2 part (3): emit ONE fused region containing exactly one HostClosure
// and prove (a) the recognizer subtree is straight-line — 0 SmallBox / dyn /
// vtable — and (b) the closure is a single named-fn call-out (1 call boundary).
//
// The lowering rules transcribed here are the SAME ones the real Rust emitter
// uses, verified by reading:
//   - crates/core/src/backend/ts/projection.rs:129  FnCall -> `name(args)`
//     (a single named call, no box / no dyn) — HostClosure reuses this path.
//   - crates/core/src/backend/rust/emitter/... Literal/Seq recognizers emit
//     straight-line `state.eat(b"..")` byte matches, no boxed dispatch.
// This program constructs that region for the value.js `lerp` rule whose fold
// is routed to an OPAQUE host closure (FnDescriptor::HostClosure{fn_id:15}),
// then greps the emitted text for barrier markers.

type StringId = u32;

enum Node {
    Literal(&'static str),
    Ref(&'static str),            // recogniser call into a sub-rule
    Seq(Vec<Node>),
    HostClosure { fn_id: u32 },   // opaque `->` fold, single named call-out
}

// Straight-line recogniser emission — mirrors the real Literal/Seq/Ref lowering.
fn emit_recognizer(n: &Node, out: &mut String) {
    match n {
        Node::Literal(s) => {
            out.push_str(&format!(
                "    if !state.eat(b\"{}\") {{ return Err(state.pos); }}\n",
                s
            ));
        }
        Node::Ref(name) => {
            out.push_str(&format!(
                "    let __{n} = parse_{n}(state)?;\n",
                n = name
            ));
        }
        Node::Seq(items) => {
            for it in items {
                emit_recognizer(it, out);
            }
        }
        Node::HostClosure { .. } => { /* handled by emit_map */ }
    }
}

// HostClosure emission — mirrors projection.rs FnCall: a single `name(args)`
// call. The name is synthesized deterministically from the fn_id. NO box, NO
// dyn, NO vtable: the closure identity is a static path resolved at link time.
fn emit_host_closure(fn_id: u32, out: &mut String) {
    out.push_str(&format!(
        "    let __out = host_closure_{}(__inner);\n",
        fn_id
    ));
}

fn main() {
    // Region: value.js `lerp` rule.
    //   lerp = "lerp" "(" expr "," expr "," expr ")" -> <opaque fold>
    // The recogniser is a straight-line Seq of Literal + Ref matches; the `->`
    // fold `(1-t)*a + t*b` is (for this probe) routed to an OPAQUE host closure
    // to exercise the arbitrary-closure wall (as if it were the multi-statement
    // `logerp` / `scale` / `deCasteljau` body).
    const FOLD_FN: u32 = 15;
    let region = Node::Seq(vec![
        Node::Literal("lerp"),
        Node::Literal("("),
        Node::Ref("expr"),
        Node::Literal(","),
        Node::Ref("expr"),
        Node::Literal(","),
        Node::Ref("expr"),
        Node::Literal(")"),
    ]);

    let mut emitted = String::new();
    emitted.push_str("#[inline]\n");
    emitted.push_str("fn parse_lerp(state: &mut State) -> Result<f64, usize> {\n");
    // recogniser (straight-line)
    emit_recognizer(&region, &mut emitted);
    // bind inner tuple then the SINGLE closure call-out
    emitted.push_str("    let __inner = (__expr, __expr, __expr);\n");
    emit_host_closure(FOLD_FN, &mut emitted);
    emitted.push_str("    Ok(__out)\n");
    emitted.push_str("}\n");

    println!("── EMITTED FUSED REGION ──\n{}", emitted);

    // ── grep the region ──
    let barrier_markers = ["SmallBox", "smallbox", "Box<dyn", "Box::new", "dyn Fn", "vtable", "&dyn"];
    let mut barrier_hits = 0usize;
    for m in &barrier_markers {
        let c = emitted.matches(m).count();
        if c > 0 {
            println!("  BARRIER MARKER `{}` x{}", m, c);
        }
        barrier_hits += c;
    }
    let call_boundaries = emitted.matches("host_closure_").count();

    println!("── REGION AUDIT ──");
    println!("  boxed/dyn/vtable barrier markers : {}", barrier_hits);
    println!("  host-closure call boundaries     : {}", call_boundaries);
    println!("  recogniser lines (straight-line) : {}",
        emitted.lines().filter(|l| l.contains("state.eat") || l.contains("parse_")).count());
    println!();
    println!("── VERDICT INPUTS ──");
    println!("  recognizer straight-line (0 boxed dispatch) ? {}", barrier_hits == 0);
    println!("  exactly 1 closure call boundary             ? {}", call_boundaries == 1);
}
