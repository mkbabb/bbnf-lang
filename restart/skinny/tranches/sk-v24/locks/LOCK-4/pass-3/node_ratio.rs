// PROTO-2 part (1): NODE-level opaque-barrier ratio for value.js's Pratt
// binary-expression (math) grammar.
//
// This is a FAITHFUL structural mirror of `bbnf_ir::IrNode` (crates/ir/src/
// types/node.rs) and its `count_nodes` (node.rs:202). Same variants, same
// child-walk, same 1+children counting. Runs under bare `rustc` so it dodges
// the cargo target-dir lock + the loadavg-38 heavy compile of crates/core.
//
// The grammar reified is value.js/src/math.ts's fold surface expressed as a
// precedence-climbing binary-expression recognizer + one Map-wrapped call rule
// per math function. Each math function's `->` fold is classified by whether
// its body is expressible as a `bbnf_ir::MapExpr` tree (IntLit/FloatLit/Bool/
// Str/Input/InputProp/FnCall/BinOp/UnaryOp — NO loops, NO ternary, NO
// multi-statement bodies, NO nested closures) or is an OPAQUE host closure.

type StringId = u32;
type RuleId = u32;
type FnId = u32;

// ── Faithful IrNode mirror (only the variants this grammar uses) ────────────
#[derive(Clone)]
enum IrNode {
    Literal(StringId),
    Regex(StringId),
    Epsilon,
    Seq(Vec<IrNode>),
    Alt(Vec<IrNode>), // AltBranch flattened to its .node — count is identical
    Repeat { inner: Box<IrNode>, lo: u32, hi: u32 },
    Ref(RuleId),
    Skip(Box<IrNode>, Box<IrNode>),
    Next(Box<IrNode>, Box<IrNode>),
    Minus(Box<IrNode>, Box<IrNode>),
    Negate(Box<IrNode>),
    Map { inner: Box<IrNode>, fn_id: FnId },
    OptionalWhitespace(Box<IrNode>),
}

impl IrNode {
    fn for_each_child(&self, f: &mut impl FnMut(&IrNode)) {
        match self {
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
            IrNode::Seq(c) => c.iter().for_each(f),
            IrNode::Alt(c) => c.iter().for_each(f),
            IrNode::Repeat { inner, .. }
            | IrNode::Negate(inner)
            | IrNode::OptionalWhitespace(inner)
            | IrNode::Map { inner, .. } => f(inner),
            IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
                f(a);
                f(b);
            }
        }
    }
}

// Exact mirror of bbnf_ir::count_nodes (node.rs:202)
fn count_nodes(node: &IrNode) -> usize {
    let mut count = 0;
    node.for_each_child(&mut |c| count += count_nodes(c));
    1 + count
}

// Count Map leaves whose FnId is in the OPAQUE (non-MapExpr-expressible) set.
fn count_opaque_maps(node: &IrNode, opaque: &[FnId]) -> usize {
    let mut n = 0;
    if let IrNode::Map { fn_id, .. } = node {
        if opaque.contains(fn_id) {
            n += 1;
        }
    }
    node.for_each_child(&mut |c| n += count_opaque_maps(c, opaque));
    n
}

fn count_all_maps(node: &IrNode) -> usize {
    let mut n = 0;
    if let IrNode::Map { .. } = node {
        n += 1;
    }
    node.for_each_child(&mut |c| n += count_all_maps(c));
    n
}

// ── Builders ────────────────────────────────────────────────────────────────
fn lit(s: StringId) -> IrNode { IrNode::Literal(s) }
fn r(id: RuleId) -> IrNode { IrNode::Ref(id) }
fn seq(v: Vec<IrNode>) -> IrNode { IrNode::Seq(v) }
fn alt(v: Vec<IrNode>) -> IrNode { IrNode::Alt(v) }
fn map(inner: IrNode, fn_id: FnId) -> IrNode { IrNode::Map { inner: Box::new(inner), fn_id } }
fn many(inner: IrNode) -> IrNode {
    IrNode::Repeat { inner: Box::new(inner), lo: 0, hi: u32::MAX }
}

// A call rule recognizer: NAME "(" arg ("," arg){n-1} ")" wrapped in Map(fold).
// `expr_rule` is the RuleId of the top expression rule (arguments recurse).
fn call_rule(fold_fn: FnId, argc: usize, expr_rule: RuleId) -> IrNode {
    let mut items = vec![lit(0 /*name*/), lit(1 /*"("*/)];
    for i in 0..argc {
        if i > 0 {
            items.push(lit(2 /*","*/));
        }
        items.push(r(expr_rule));
    }
    items.push(lit(3 /*")"*/));
    map(seq(items), fold_fn)
}

fn main() {
    // RuleIds
    const R_EXPR: RuleId = 0;
    const R_TERM: RuleId = 1;
    const R_FACTOR: RuleId = 2;
    const R_NUMBER: RuleId = 3;

    // FnIds for the 9 math.ts folds. OPAQUE = body not MapExpr-expressible.
    //   EXPRESSIBLE (pure BinOp / nested FnCall, no control flow):
    //     clamp   = Math.min(Math.max(v,min),max)         -> FnCall nest
    //     lerp    = (1-t)*start + t*end                    -> BinOp tree
    //     cubicBezier = [deCasteljau(...), deCasteljau(...)]-> FnCall pair
    //   OPAQUE (loop / ternary-stmt / throw / nested closure / .map):
    //     scale   = if(fromMax===fromMin) throw; ...       -> control flow
    //     lerpArray = for(i..n) out[i]=...                 -> loop
    //     logerp  = start = start===0 ? 1e-9 : start; ...  -> stmt + ternary
    //     deCasteljau = nested for loops                   -> loops
    //     interpBezier = points.map(xy=>xy[0]) ...         -> nested closures
    //     cubicBezierToString = const f=(n)=>{...}; `...`  -> nested closure
    const FN_CLAMP: FnId = 10;
    const FN_SCALE: FnId = 11;
    const FN_LERP: FnId = 12;
    const FN_LERPARRAY: FnId = 13;
    const FN_LOGERP: FnId = 14;
    const FN_DECASTELJAU: FnId = 15;
    const FN_CUBICBEZIER: FnId = 16;
    const FN_INTERPBEZIER: FnId = 17;
    const FN_CBTOSTRING: FnId = 18;
    const FN_ADD_FOLD: FnId = 20; // expr additive fold  ((1-t)*a+t*b-style) EXPRESSIBLE
    const FN_MUL_FOLD: FnId = 21; // term multiplicative fold                EXPRESSIBLE
    const FN_PARSE_F64: FnId = 22; // number -> parse_f64(input)             EXPRESSIBLE (NumberConvert)

    let opaque: Vec<FnId> = vec![
        FN_SCALE, FN_LERPARRAY, FN_LOGERP, FN_DECASTELJAU, FN_INTERPBEZIER, FN_CBTOSTRING,
    ];

    // (name, argc, fold_fn)
    let functions: &[(&str, usize, FnId)] = &[
        ("clamp", 3, FN_CLAMP),
        ("scale", 5, FN_SCALE),
        ("lerp", 3, FN_LERP),
        ("lerpArray", 4, FN_LERPARRAY),
        ("logerp", 3, FN_LOGERP),
        ("deCasteljau", 2, FN_DECASTELJAU),
        ("cubicBezier", 5, FN_CUBICBEZIER),
        ("interpBezier", 2, FN_INTERPBEZIER),
        ("cubicBezierToString", 4, FN_CBTOSTRING),
    ];

    // ── Core precedence-climbing binary-expression recognizer ───────────────
    // expr = term (("+"|"-") term)*  -> additive fold
    let expr_body = map(
        seq(vec![
            r(R_TERM),
            many(seq(vec![alt(vec![lit(4), lit(5)]), r(R_TERM)])),
        ]),
        FN_ADD_FOLD,
    );

    // term = factor (("*"|"/") factor)* -> multiplicative fold
    let term_body = map(
        seq(vec![
            r(R_FACTOR),
            many(seq(vec![alt(vec![lit(6), lit(7)]), r(R_FACTOR)])),
        ]),
        FN_MUL_FOLD,
    );

    // factor = number | <each function rule inline> | "(" expr ")"
    // Function-call rules are reified inline as Alt branches of `factor`
    // (one Map-wrapped recognizer subtree per math function).
    let mut factor_branches: Vec<IrNode> = vec![r(R_NUMBER)];
    for (_name, argc, fold) in functions {
        factor_branches.push(call_rule(*fold, *argc, R_EXPR));
    }
    factor_branches.push(seq(vec![lit(8), r(R_EXPR), lit(9)])); // "(" expr ")"
    let factor_body = alt(factor_branches);

    // number = /[0-9]+(\.[0-9]+)?/ -> parse_f64(input)
    let number_body = map(IrNode::Regex(100), FN_PARSE_F64);

    let rules: Vec<(&str, &IrNode)> = vec![
        ("expr", &expr_body),
        ("term", &term_body),
        ("factor", &factor_body),
        ("number", &number_body),
    ];

    let mut total_nodes = 0usize;
    let mut total_maps = 0usize;
    let mut total_opaque = 0usize;
    println!("── per-rule node census ──");
    for (name, body) in &rules {
        let n = count_nodes(body);
        let m = count_all_maps(body);
        let o = count_opaque_maps(body, &opaque);
        total_nodes += n;
        total_maps += m;
        total_opaque += o;
        println!("  {:<8} nodes={:<4} map_leaves={:<3} opaque_maps={}", name, n, m, o);
    }

    let ratio = total_opaque as f64 / total_nodes as f64 * 100.0;
    let site_ratio_repo = 33.0 / 129.0 * 100.0; // research-D SITE census
    let expansion_k = total_nodes as f64 / (functions.len() as f64); // nodes per fold-site
    let repo_projected = site_ratio_repo / expansion_k;

    println!();
    println!("── MATH GRAMMAR (measured, exact) ──");
    println!("  total IR nodes                 = {}", total_nodes);
    println!("  total Map leaves               = {}", total_maps);
    println!("  opaque (HostClosure) leaves    = {}", total_opaque);
    println!("  NODE opaque ratio (math-only)  = {:.3}%", ratio);
    println!("  fold-sites                     = {}", functions.len());
    println!("  expansion K = nodes / site     = {:.2}", expansion_k);
    println!();
    println!("── REPO-WIDE PROJECTION (research-D: 33 opaque sites of 129) ──");
    println!("  SITE opaque ratio (repo)       = {:.2}%", site_ratio_repo);
    println!("  node ratio = site_ratio / K    = {:.3}%", repo_projected);
    println!();
    println!("── VERDICT INPUTS ──");
    println!("  math-only  < 2% ? {}", ratio < 2.0);
    println!("  repo-proj  < 2% ? {}", repo_projected < 2.0);
    println!("  K needed for repo <2% : {:.2}  (have {:.2})", site_ratio_repo / 2.0, expansion_k);
}
