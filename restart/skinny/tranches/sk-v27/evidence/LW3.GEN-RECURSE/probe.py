import sys
p=sys.argv[1]; s=open(p).read()
old="pub fn branch_pushes_children(ir: &GrammarIR, node: &IrNode, dstate: &DriverState) -> bool {\n    match node {"
new='''thread_local!(static GEN_RECURSE_DEPTH: std::cell::Cell<u32> = const { std::cell::Cell::new(0) });
pub fn branch_pushes_children(ir: &GrammarIR, node: &IrNode, dstate: &DriverState) -> bool {
    let d = GEN_RECURSE_DEPTH.with(|c| { c.set(c.get() + 1); c.get() });
    if d > 4096 { GEN_RECURSE_DEPTH.with(|c| c.set(0)); panic!("GEN-RECURSE probe: alt.rs::branch_pushes_children depth > 4096"); }
    let r = branch_pushes_children_probe(ir, node, dstate);
    GEN_RECURSE_DEPTH.with(|c| c.set(c.get().saturating_sub(1)));
    r
}
fn branch_pushes_children_probe(ir: &GrammarIR, node: &IrNode, dstate: &DriverState) -> bool {
    match node {'''
assert s.count(old)==1; open(p,"w").write(s.replace(old,new))
