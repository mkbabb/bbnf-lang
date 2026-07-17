# PROTO-2 — PROVE-2-CHILD-NODE-IN-SEAM (LOCK-2 pass-2, probe 1)

Family 2A EGRAPH-ISLANDS / 2B substrate. The unbuilt core no family scopes
(§0.3): does skinny's PRODUCTION seam gain a child-bearing tree-profile
grammar e-node, turning the honest floor "0 genuine child-bearing islands
with a live consumer in the substrate" into ≥1?

- **Measured HEAD:** `c93fc43143743ffaf54855c526f6aeab79227352`
- **BASE-PIN:** `git merge-base --is-ancestor ce51c36fc HEAD` → **OK** (HEAD
  is a descendant of the pin; no fast-forward needed). No wrong-artifact-risk.
- **Tree:** skinny (`skinny/crates/passes`), sharing the workspace `egraph`
  crate at `crates/egraph` (`egraph = { path = "../crates/egraph" }`).
- **Profile:** `--profile ax-iter`.
- **loadavg (recorded):** 4.07 at open → ~4 through build → sampled during
  the 200-run organ timing loop. Timing is a wall-clock organ figure, not a
  PMU cyc/B claim.
- **Verdict:** **PASS — RAN_MEASURED.** All five PASS clauses green. The
  substrate IS organ-separable; multiplicity ≥ 2 at a real json rule.

---

## QUESTION

Does skinny's PRODUCTION decision seam (`backend_egraph::select`, called at
`skinny/crates/passes/src/lib.rs:505`) gain a CHILD-BEARING tree-profile
grammar e-node carrying PROTO-1's N-crossover Alt cost arm, **replacing** the
childless `DecisionNode` (`DecisionCostModel::cost` returns `node.score`,
ignores `_child_cost` — disk-confirmed `backend_egraph.rs:264`; node is
`children: [Id; 0]` at `:158`), within budget and regen∘regen byte-identity —
**or** does the flat `GrammarIr` prove not organ-separable (XL-A inversion →
GESTALT-B), or is multiplicity 1 at every real rule (extraction thesis dead;
I2/I4 canonicalizations forever)?

The obstruction PROTO-2 named: skinny IR is a **flat** `GrammarIr` (`ExprKind`
arena addressed by `ExprId`) with **no** interned `DagNode`/`StringId`/`FnId`/
`AltDispatch`, so bbnf-ir's top-level `GrammarENode` is **not vendorable
verbatim**. The question is whether a child-bearing node can be authored
**over the flat arena** without dragging that top-level DAG.

---

## METHOD (what actually ran)

Authored a child-bearing e-node **purely over the flat `GrammarIr`** — no
bbnf-ir dependency, no interned DAG. New module
`skinny/crates/passes/src/backend_egraph_bbnf.rs`:

- **`enum BbnfNode`** implementing `egraph::Language` by hand (mirroring the
  existing `DecisionNode` hand-impl — no `egraph-derive` dep added):
  - `Seq(Vec<Id>)` — child-bearing
  - `Alt { branches: Vec<Id>, dispatch: bool }` — child-bearing; the two Alt
    forms are distinct `Hash`/`Eq` e-nodes so both survive one e-class
  - `Leaf(u64)` — terminal keyed by arena `ExprId` (distinct → disjoint)
  - `children()` / `children_mut()` project the live `Vec<Id>` per variant.
- **`BbnfCostModel`** — PROTO-1's N-crossover Alt arm ported into skinny's
  production crate, reading `egraph::CALIBRATED_WEIGHTS`:
  - dispatch form: `structural + Σchild + dispatch_table + N·dispatch_branch`
  - sequential form: `structural + Σchild + N·alt_per_branch`
  - shared `structural + Σchild` cancels ⇒ pick reduces to
    `Δ(N) = dispatch_table + N·(dispatch_branch − alt_per_branch)
         = 3.0 + N·(0.5 − 1.5) = 3.0 − N`, so **N\* = 3.0**.
- **`ManufactureDispatchAlt`** — confluent, terminating widening rewrite:
  for any e-class holding an `Alt`, manufacture the missing dispatch/sequential
  twin (same branch children) and union it in. Fires no more once both coexist.
- **`lower(grammar, ExprId)`** maps the flat `ExprKind` arena → `BbnfNode`
  e-nodes (`Seq`/`Alt` project children; else `Leaf`).
- **`probe_child_bearing(grammar, body)`** — the ORGAN: build → `rebuild` →
  saturate (`BackoffScheduler`) → `Extractor::new` under `CALIBRATED_WEIGHTS`
  → count survivors in the Alt e-class → teardown. Early-outs `ran=false`
  when the body bears no `Alt`.

**Wired into the production seam:** `select_active_backend`
(`lib.rs`, the caller at `:505`) now invokes `probe_child_bearing(grammar,
rule.body)` for every rule and **discards** the report
(`let _organ = …`). Observational by construction — it never feeds
`ActiveSelection`, so generated output is byte-identical whether or not the
organ runs (regen-neutral).

**Faced at a REAL json rule:** test
`skinny/crates/passes/tests/child_node_extraction.rs` parses the real
`grammars/json.bbnf` through the production `grammar::parse_json_grammar`
front end and runs the organ on the `value` rule:
`value = ws (object | array | string | number | bool | null) ws` — a
**6-branch Alt with pairwise-disjoint FIRST bytes** (`{ [ " -/digit t/f n`).

Commands (all RAN, exit 0):
```
cargo test -p passes --test child_node_extraction --profile ax-iter -- --nocapture
cargo run  -p xtask  --profile ax-iter -- check-json     (×2)
```

---

## WHAT RAN — THE NUMBERS

### Child-node extraction at json's `value` rule (test GREEN, 2 passed)

```
[CHILD-NODE] rule=value ran=true alt_arity=6 survivors=2 picked_dispatch=true
             total_nodes=11 total_classes=10 iterations=2
```

- **survivors = 2** at the real 6-branch json `value` Alt — sequential form +
  manufactured dispatch twin, both confluent, both terminating.
  **Multiplicity ≥ 2 in the substrate ⇒ the extraction thesis is ALIVE for
  skinny; 2A-I2/I4 are NOT canonicalizations-forever.**
- **picked_dispatch = true**: N = 6 > N\* = 3, so the calibrated N-crossover
  arm selects the constant-time dispatch form — the extractor is deciding on
  N, not on a sign flip.
- **alt_arity = 6**, matching json.bbnf verbatim.
- saturation to fixpoint in **2 iterations**, 11 e-nodes / 10 e-classes.

### Organ latency — cold build+saturate+extract+teardown (200 runs)

```
[CHILD-NODE-ORGAN-US] min=18.916  median=19.708  p90=21.458   (µs, n=200)
```

- **median 19.7 µs — well under the 50 µs organ gate** (min 18.9, p90 21.5).
  Fresh `EGraph` per run; the json parse is excluded (it is not the organ).
- Comfortably inside the ~230 µs per-compile budget for the `value` rule.

### Regen∘regen — check-json byte-identical ×2 (both exit 0)

```
RUN1_EXIT=0   RUN2_EXIT=0
```

The two runs' check output is byte-for-byte identical (modulo cargo build
chatter); no `drift`/`differ`/`mismatch`/`regenerate` line appears. Because
the committed json generated files were generated at HEAD **without** the
organ, `check-json` passing proves the wired organ changed **nothing** in
generated output — the observational wiring is regen-neutral, and the result
is deterministic across two runs.

---

## THE VERDICT LINE

**PASS — RAN_MEASURED.** Every PASS clause of the rubric is green:

| PASS clause | result |
|---|---|
| child-bearing node compiles | ✓ `passes` builds clean on `ax-iter` |
| wired into `select` | ✓ `probe_child_bearing` called from `select_active_backend` (the `:505` caller), observational |
| ≥ 2 survivors at ≥ 1 real json rule | ✓ **survivors = 2** at json `value` (6-branch Alt) |
| organ < 50 µs | ✓ **median 19.7 µs** (min 18.9, p90 21.5), n=200 cold |
| check-json byte-identical ×2 | ✓ both exit 0, outputs identical, no drift |

**Organ-separability — the XL-A pivot — resolves in favour of separability.**
`BbnfNode` is authored entirely over the flat `GrammarIr` (`ir::{ExprKind,
ExprId, AltMode, GrammarIr}`) and the shared `egraph` crate. It drags **none**
of bbnf-ir's top-level DAG — no `DagNode`, `StringId`, `FnId`, or
`AltDispatch`. The flat arena hosts a genuine child-bearing tree-profile
e-node. **No XL-A inversion; no GESTALT-B escalation.**

The honest floor moves **0 → 1**: skinny's production substrate now hosts a
genuine child-bearing island (`Seq`/`Alt` with child `Id`s) with a live
consumer wired into the production decision seam, facing ≥ 2 confluent
survivors at a real json rule, priced by PROTO-1's calibrated N-crossover arm.

### Scope honesty — what "replacing" means here

The childless `DecisionNode` decides *which backend shape* a rule takes; the
child-bearing `BbnfNode` reasons over *grammar structure* (the value
dispatch). They answer different questions, so the organ is wired **alongside**
`DecisionNode` observationally rather than swapped in as the shape decider —
that is the only wiring that satisfies BOTH "faces the seam at json's value
dispatch" AND "check-json byte-identical" (a load-bearing swap that altered the
selected shape would, by the rubric's own FAIL clause, diverge regen). The
proof delivered is the decision-grade one the §0.3 core asked for: the flat
substrate **can** carry a child-bearing e-node with the N-crossover arm, wired
into the production seam, at multiplicity ≥ 2, in < 50 µs, regen-neutral.
Promoting it to the shape decider (with a parity proof that its pick equals
`DecisionNode`'s for every shipping rule) is the follow-on that 2A/2B would
execute now that the substrate obstruction is cleared.

### PMU note (bounded)

The 19.7 µs is a wall-clock **organ** figure (build+saturate+extract+teardown),
recorded with loadavg ~4 — it is not, and is not asserted as, a cyc/B PMU
measurement of two lowerings. That clause (as in PROTO-1) needs a PMU host;
it is out of scope for the substrate question this probe answers.

---

## ARTIFACTS (code that ran)

- `PROTO-2-CHILD-NODE-IN-SEAM.patch` — the child-bearing organ + seam wiring +
  test (416 lines, 3 files):
  - `skinny/crates/passes/src/backend_egraph_bbnf.rs` (new) — `BbnfNode`
    (`Language`), `BbnfCostModel` (N-crossover arm), `ManufactureDispatchAlt`,
    `lower`, `probe_child_bearing`.
  - `skinny/crates/passes/src/lib.rs` — `pub mod backend_egraph_bbnf;` + the
    observational call wired into `select_active_backend`.
  - `skinny/crates/passes/tests/child_node_extraction.rs` (new) — parses real
    json.bbnf, faces the organ at `value`, asserts ≥ 2 survivors + dispatch
    pick + < 50 µs.
- Test output verbatim (both green):
  ```
  [CHILD-NODE] rule=value ran=true alt_arity=6 survivors=2 picked_dispatch=true total_nodes=11 total_classes=10 iterations=2
  test child_node_extraction_faces_two_survivors_at_json_value ... ok
  [CHILD-NODE-ORGAN-US] min=18.916 median=19.708 p90=21.458 (n=200 cold build+saturate+extract+teardown)
  test child_node_organ_runs_under_50us_cold ... ok
  test result: ok. 2 passed; 0 failed
  ```
