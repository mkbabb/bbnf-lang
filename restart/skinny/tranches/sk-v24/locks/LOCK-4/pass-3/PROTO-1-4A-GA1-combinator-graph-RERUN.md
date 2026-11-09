# PROTO-1 — LOCK-4 pass-3, probe 4A G-A1: fuse(Graph)→Graph byte-identity on a combinator-constructed graph (INDEPENDENT RE-RUN)

Family 4A CLOSURE-TO-GRAPH · parse-that COMBINATOR CORE · pass 3 · probe 1

This is an **independent replication** of the same-probe report
`PROTO-1-4A-GA1-combinator-graph.md`. Both runs are base-pinned to the same
HEAD; this run is a second, separately-built confirmation with a freshly
measured timing sample. It exists to turn the SPEC-ONLY-for-four-passes claim
into a twice-measured anchor, not to supersede the prior text.

- **Base-pin / measured-on HEAD:** `ce51c36fcbdf5108eb028005586672d9a6d2ca1b`.
  `git merge-base --is-ancestor ce51c36fc HEAD` → DESCENDS-OK; HEAD == base-pin
  exactly. Clean artifact, **not** wrong-artifact-risk.
- **Isolated build:** dedicated `CARGO_TARGET_DIR` in scratchpad
  (`proto4a-target`); cold build, no shared-target race. One cargo invocation.
- **Result:** `test result: ok. 1 passed; 0 failed; finished in 0.35s`, exit 0.
- **Loadavg (1/5/15m):** at build start 3.62 / 4.29 / 5.36; at run end
  18.94 / 10.62 / 7.77 (sibling agent builds landed concurrently). Cold-paired
  fastest-sample was chosen precisely to survive this load; the byte-identical
  IR ⇒ identical bytecode makes the timing a confirmation, not the crux.

## Question

4A's one novel claim, SPEC-ONLY for four passes: do the extant fuse passes,
run over an `IrNode` built by a **combinator-style smart-constructor builder**
(NOT grammar text), produce a graph structurally **byte-identical** to the
grammar-text-built (hand-fused) reference, and parse at parity? "The passes
already exist" is proven only for the grammar-TEXT front; the combinator front
is the unbuilt claim — the elegant-reduction trap.

## Method (what actually ran)

Test: `crates/core/tests/proto_4a_ga1.rs` (358 LOC incl. an ~85-LOC combinator
builder). Both fronts express the same `json` grammar; the IDENTICAL pipeline
runs over both.

1. **Reference front (grammar text → GrammarIR):** `bbnf::grammar::parse(JSON_SRC)`
   → `calculate_ast_deps` → `tarjan_scc` → `bbnf::lower::lower_to_ir` →
   analysis passes `compute_first_sets` / `compute_scc` / `compute_aliases` /
   `compute_transparent` / `refine_span_eligibility`. This is the same
   lowering `crates/core/tests/lower.rs::lower_grammar` uses.
2. **Combinator front:** ~85-LOC smart-constructor builder — `lit()`→Literal,
   `rgx()`→Regex, `seq()`→Seq, `alt()`→Alt, `skip()`→Skip, `next()`→Next,
   `many()`/`opt()`→Repeat, `ows()`→OptionalWhitespace, `rref()`→Ref — emitting
   `IrNode` directly for all 10 json rules, then the IDENTICAL analysis passes.
   Rule ids/names/order/entry + string pool are shared with the reference so
   StringId/RuleId **bookkeeping** cannot spuriously diverge; the **graph shape**
   is the sole object under test.
3. **Identical fuse pipeline over both:** `fuse_single_use`
   (`crates/ir/src/passes/transform/fuse.rs:44`, `NoopTraceSink`) +
   `fuse_token_dispatch` (`fuse_token/mod.rs:35`).
4. **Structural oracle:** (a) per-rule `serde_json::to_vec(&IrNode)` byte-compare
   over all 10 rules; (b) interned-DAG NodeId-sequence via
   `dag::ensure_dag` + `GrammarDag::iter()` → `Vec<DagNode>` equality. Applied
   both pre- and post-fuse.
5. **Parse parity + cold-paired timing:** `parse_with_ir`
   (`crates/ir/src/vm/interpreter/mod.rs:373`), entry re-pointed at `value`.
   Correctness parity on the small doc and the real `twitter.json`
   (`skinny/crates/test-fixtures/corpus/json/twitter.json`, 631 KB); fastest-
   sample cold-paired timing on the small doc, N=200, two windows, both orders.

### Scope note — what "the identical pipeline" is (honesty)

The production compile pipeline
(`crates/core/src/pipeline/compile/pipeline.rs`) fuses inside a full fixed-point
`structural_normalizer_loop` (canonicalize_aliases → compute_scc →
prune_unreachable → inline_acyclic → **fuse_single_use** → eliminate_epsilon →
merge_literals → factor_common_prefixes), then egraph saturation, then
**fuse_token_dispatch**. This probe deliberately runs the **fuse passes in
isolation** (shared `analyze()` prerequisites → `fuse_single_use` +
`fuse_token_dispatch`) — because the object under test is the fuse passes'
behaviour on a combinator-built graph, not the whole pipeline. The isolation is
applied **identically to both fronts**, so the comparison is exact.

## Numbers (this run — raw `--nocapture` stdout, verbatim)

```
=== PROTO-1 4A G-A1: combinator-vs-grammar-text fuse byte-identity ===
[PRE-FUSE] all 10 rule bodies BYTE-IDENTICAL
[POST-FUSE] all 10 rule bodies BYTE-IDENTICAL
[DAG] ref nodes=37 comb nodes=37 sequence_identical=true
[PARSE small] ref(success=true,off=68) comb(success=true,off=68) parity=true
[PARSE twitter] ref(success=true,off=631514) comb(success=true,off=631514) parity=true
[TIMING] ref_best=331583ns comb_best=331875ns ratio(comb/ref)=1.0009

=== VERDICT ===
pre_fuse_divergences  = 0
post_fuse_divergences = 0  (byte_identical=true)
dag_sequence_identical = true
parse_parity_ok        = true
timing_ratio           = 1.0009 (band[0.98,1.02]=true)
PROTO-1 4A G-A1: PASS
test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.35s
```

| Oracle | This run | Prior run | Pass gate |
|---|---|---|---|
| Pre-fuse serialized `IrNode` divergences (10 rules) | **0** | 0 | — (combinator front reproduces the *lowering* itself, not merely a fuse fixed-point) |
| Post-fuse serialized `IrNode` divergences | **0 / 10** | 0 / 10 | 0 required → **MET** |
| Interned-DAG NodeId-sequence identity | identical, 37 == 37 | identical, 37 == 37 | required → **MET** |
| Parse parity — small doc | success, off=68 both | success, off=68 both | required → **MET** |
| Parse parity — real `twitter.json` (631 KB) | success, off=631514 both | success, off=631514 both | required → **MET** |
| Cold-paired fastest-sample ratio (comb/ref), N=200, 2 windows, both orders | **1.0009** | 0.9981 | ∈ [0.98, 1.02] → **MET** |

Both independent runs land inside the band (1.0009 and 0.9981) on opposite sides
of 1.0 — the expected same-program noise signature of byte-identical IR.

## Verdict

**PASS — RAN_MEASURED. The combinator front inherits the fuse passes for free.**

For the json grammar the combinator-constructed `IrNode` is byte-identical to
the grammar-text-built reference **both before and after** `fuse_single_use` +
`fuse_token_dispatch` (0/10 divergences each phase), the interned DAG produces
an identical 37-node NodeId sequence, both graphs parse `twitter.json`
identically (offset 631514), and the cold-paired fastest-sample ratio is 1.0009,
inside [0.98, 1.02]. Independently reproduced at the same base-pin (prior run
0.9981; both in-band).

This **refutes the refutation** for family 4A: the elegant-reduction trap does
NOT bite here. `IrNode` is the single canonical node type and the fuse passes
are pure structural rewrites keyed on that enum — not on any provenance of how
the tree was built — so a smart-constructor combinator front that emits the same
`IrNode` shapes flows through the extant passes with zero adaptation and lands on
the same graph. 4A's source-front role is **upheld**, not downgraded.

### Scope / honesty caveats (what this does and does not prove)

- **Structural claim isolated deliberately.** Rule ids/names/order/entry and the
  string pool are shared, so StringId/RuleId *bookkeeping* cannot spuriously
  diverge the bytes. The object under test is the **graph shape**
  (Seq/Alt/Skip/Next/Repeat/OptionalWhitespace/Ref nesting) — exactly the
  combinator claim. An independent-pool front would only add a
  renumbering-normalization step; it would not change the structural result.
- **The combinator author had to match the lowering's conventions** — `?w` →
  `OptionalWhitespace`, `<<`/`>>` → `Skip`/`Next` with left-leaning nesting,
  `,` looser than `>>` in `pair` (→ `Seq(string, Next(colon, value))`), `*`/`?`
  → `Repeat`. These matched on the **first build** (pre-fuse divergences = 0),
  which is the substantive evidence: a faithful combinator expression lands on
  the identical lowered graph. Any wrong convention would have diverged pre-fuse
  and fuse would not have reconciled it — the test is built to expose that.
- **`fuse_token_dispatch` is a no-op on json** (no `@token` rules;
  `DirectiveSet::empty()`), so its inheritance is proven trivially (identical
  early-return on both fronts). `fuse_single_use` is the load-bearing pass and
  **does** fire (json's single-use rules inline), so the byte-identity is a real
  fuse-fixed-point equality, not a null transform.
- **Timing corpus.** The tree-walking VM interpreter bars N≥200 over full
  twitter (631 KB) inside a unit test; timing used a ~70-byte all-node-types doc
  at N=200 (2 windows, both orders, fastest-sample) while **correctness** parity
  was taken on full twitter + the small doc. Given byte-identical IR ⇒ identical
  bytecode program, the 1.0009 ratio is the expected same-program result. Note
  `parse_with_ir` recompiles IR→bytecode each call, so the timing is
  compile+parse (identical work on both fronts — a stricter, not weaker, parity).
- **One grammar (json).** This proves the mechanism for the 4A CLOSURE-TO-GRAPH
  family on a representative grammar exercising every core combinator. A grammar
  with `@token` / `@pretty` directives or host `Map{fn_id}` branches would
  exercise the pinned-rule and typed-materialization guards in `fuse.rs`; those
  are out of this probe's parsimony scope and are the natural next probe.

### Artifact

- Test: `crates/core/tests/proto_4a_ga1.rs` (358 LOC; patch delivered as
  `proto_4a_ga1-RERUN.patch`, byte-identical to the prior `proto_4a_ga1.patch`).
- Measured on HEAD `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` (== base-pin).
- Isolated `CARGO_TARGET_DIR`; single cargo invocation; exit 0; 0.35s.
