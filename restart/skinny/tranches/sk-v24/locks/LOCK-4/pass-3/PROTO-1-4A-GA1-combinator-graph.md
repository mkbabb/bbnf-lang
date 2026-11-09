# PROTO-1 — LOCK-4 pass-3, probe 4A G-A1: fuse(Graph)→Graph byte-identity on a combinator-constructed graph

Family 4A CLOSURE-TO-GRAPH · parse-that COMBINATOR CORE · pass 3

- **Base-pin:** measured on HEAD `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` (worktree fast-forwarded from stale `d2b32f6e3` via `git merge --ff-only ce51c36fc`; HEAD == base-pin, ancestry verified). Numbers are clean-artifact.
- **Loadavg at launch:** 7.20 / 10.19 / 8.87 (1/5/15m); at report ~12.97. Machine under concurrent load (sibling agent builds); cold-paired fastest-sample chosen precisely to survive this.
- **Isolated CARGO_TARGET_DIR:** scratchpad `pt4a-target` (cold build; no shared-target race). `test result: ok. 1 passed; finished in 0.31s`, exit 0.

## Question

4A's one novel claim, SPEC-ONLY for four passes: do the extant fuse passes,
run over an `IrNode` built by a **combinator-style smart-constructor builder**
(NOT grammar text), produce a graph structurally **byte-identical** to the
grammar-text-built (hand-fused) reference, and parse at parity? "The passes
already exist" is proven only for the grammar-TEXT front; the combinator front
is the unbuilt claim — the elegant-reduction trap.

## Method (what actually ran)

Test: `crates/core/tests/proto_4a_ga1.rs` (added, ~330 LOC incl. an ~85-LOC
combinator builder). Both fronts express the same `json` grammar.

1. **Reference front (grammar text → GrammarIR):** `bbnf::grammar::parse(JSON_SRC)`
   → `calculate_ast_deps` → `tarjan_scc` → `bbnf::lower::lower_to_ir` →
   analysis passes (`compute_first_sets`, `compute_scc`, `compute_aliases`,
   `compute_transparent`, `refine_span_eligibility`). This is the identical
   pipeline `crates/core/tests/lower.rs::lower_grammar` uses.
2. **Combinator front:** ~85-LOC smart-constructor builder
   (`lit()`→Literal, `rgx()`→Regex, `seq()`→Seq, `alt()`→Alt, `skip()`→Skip,
   `next()`→Next, `many()`/`opt()`→Repeat, `ows()`→OptionalWhitespace,
   `rref()`→Ref) emitting `IrNode` directly for all 10 json rules, then the
   IDENTICAL analysis passes. Rule ids/names/order/entry + string pool are
   shared with the reference so StringId/RuleId **bookkeeping** is neutralized
   and the **structural graph** is the sole object under test.
3. **Identical pipeline over both:** `fuse_single_use` (fuse.rs:44, NoopTraceSink)
   + `fuse_token_dispatch` (fuse_token/mod.rs:35).
4. **Structural oracle:** (a) per-rule `serde_json::to_vec(&IrNode)` byte-compare;
   (b) DAG NodeId-sequence via `dag::ensure_dag` + `GrammarDag::iter()` →
   `Vec<DagNode>` equality (`DagNode: PartialEq`). Both pre- and post-fuse.
5. **Parse parity + cold-paired timing:** `parse_with_ir` (interpreter/mod.rs:373)
   with entry re-pointed at `value`; correctness parity on the small doc and on
   the real `twitter.json` corpus; fastest-sample cold-paired timing on the
   small doc, N=200, two windows, both orders (interpreter throughput bars full
   twitter at N≥200 — noted deviation).

## Numbers

Raw test stdout (`--nocapture`), verbatim:

```
=== PROTO-1 4A G-A1: combinator-vs-grammar-text fuse byte-identity ===
[PRE-FUSE] all 10 rule bodies BYTE-IDENTICAL
[POST-FUSE] all 10 rule bodies BYTE-IDENTICAL
[DAG] ref nodes=37 comb nodes=37 sequence_identical=true
[PARSE small] ref(success=true,off=68) comb(success=true,off=68) parity=true
[PARSE twitter] ref(success=true,off=631514) comb(success=true,off=631514) parity=true
[TIMING] ref_best=313125ns comb_best=312541ns ratio(comb/ref)=0.9981

=== VERDICT ===
pre_fuse_divergences  = 0
post_fuse_divergences = 0  (byte_identical=true)
dag_sequence_identical = true
parse_parity_ok        = true
timing_ratio           = 0.9981 (band[0.98,1.02]=true)
PROTO-1 4A G-A1: PASS
test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.31s
```

| Oracle | Result | Pass gate |
|---|---|---|
| Pre-fuse serialized `IrNode` divergences (10 rules) | **0** | — (shows the combinator front reproduces the *lowering* itself, not merely a fuse fixed-point) |
| Post-fuse serialized `IrNode` divergences | **0 / 10** | 0 required → **MET** |
| DAG NodeId-sequence identity (`Vec<DagNode>`) | identical, 37 == 37 nodes | required → **MET** |
| Parse correctness parity — small doc | success=true, off=68 both | required → **MET** |
| Parse correctness parity — real `twitter.json` (631 KB) | success=true, off=631514 both | required → **MET** |
| Cold-paired fastest-sample ratio (comb/ref), N=200, 2 windows, both orders | **0.9981** | ∈ [0.98, 1.02] → **MET** |

## Verdict

**PASS — RAN_MEASURED. The combinator front inherits the fuse passes for free.**
For the json grammar the combinator-constructed `IrNode` is byte-identical to
the grammar-text-built reference **both before and after** `fuse_single_use` +
`fuse_token_dispatch` (0/10 divergences each phase), the interned DAG produces
an identical 37-node NodeId sequence, both graphs parse `twitter.json`
identically (offset 631514), and the cold-paired fastest-sample ratio is 0.9981,
inside [0.98, 1.02].

This **refutes the refutation** for family 4A: the elegant-reduction trap does
NOT bite here. Because `IrNode` is the single canonical node type and the fuse
passes are pure structural rewrites keyed on that enum (not on any provenance
of how the tree was built), a smart-constructor combinator front that emits the
same `IrNode` shapes flows through the extant passes with zero adaptation and
lands on the same graph. 4A's source-front role is **upheld**, not downgraded.

### Scope / honesty caveats (what this does and does not prove)

- **Structural claim isolated deliberately.** Rule ids/names/order/entry and the
  string pool are shared between the two fronts, so StringId/RuleId *bookkeeping*
  cannot spuriously diverge the bytes. The object under test is the **graph
  shape** (Seq/Alt/Skip/Next/Repeat/OptionalWhitespace/Map/Ref nesting), which
  is exactly the combinator claim. An independent-pool front would only add a
  renumbering-normalization step; it would not change the structural result.
- **The combinator author had to match the lowering's conventions** — `?w` →
  `OptionalWhitespace`, `<<`/`>>` → `Skip`/`Next` with left-leaning nesting,
  `,` looser than `>>` in `pair` (→ `Seq(string, Next(colon, value))`), `*`/`?`
  → `Repeat`. These matched on the **first build** (pre-fuse divergences = 0),
  which is the substantive evidence: a faithful combinator expression of the
  grammar lands on the identical lowered graph. Had any convention been guessed
  wrong, pre-fuse would have diverged and fuse would not have reconciled it —
  the test is built to expose exactly that.
- **`fuse_token_dispatch` is a no-op on json** (no `@token` rules; `lower_grammar`
  uses `DirectiveSet::empty()`), so its inheritance is proven trivially (identical
  early-return on both). `fuse_single_use` is the load-bearing pass here and *does*
  fire (json's single-use rules inline), so the byte-identity is a real
  fuse-fixed-point equality, not a null transform.
- **Timing corpus.** The tree-walking VM interpreter bars N≥200 over full
  twitter (631 KB) in a unit test; timing used a ~70-byte all-node-types doc at
  N=200 (2 windows, both orders, fastest-sample) while **correctness** parity was
  taken on full twitter + the small doc. Given byte-identical IR → identical
  bytecode program, the 0.9981 ratio is the expected same-program result and the
  timing is a confirmation, not the crux.
- **One grammar (json).** This proves the mechanism for the 4A CLOSURE-TO-GRAPH
  family on a representative grammar exercising every core combinator. A grammar
  with `@token` / `@pretty` directives or host `Map{fn_id}` branches would
  exercise the pinned-rule and typed-materialization guards in `fuse.rs`; those
  are out of this probe's parsimony scope and are the natural next probe.

### Artifact

- Test: `crates/core/tests/proto_4a_ga1.rs` (patch delivered alongside).
- Measured on HEAD `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` (== base-pin).

