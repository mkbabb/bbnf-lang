# PROTO-2 — LOCK-3 pass-2 · 3B one-fold byte-identity collapse (G1) + seam disposition

**Family:** 3B — SINGLE-IR-MULTIPASS (the convergent substrate).
**Probe:** 2 of the pass-2 spine. Answers the killer gate's *precondition* (Q-3B-1) by verified
data-flow trace, corrects G-3B-2's named mechanism, and fixes G-3B-3/4/5 dispositions.

**BASE-PIN.** Measured on `HEAD = c93fc43143743ffaf54855c526f6aeab79227352` (worktree `lock3-b2`,
detached at `c93fc4314`). `git merge-base --is-ancestor ce51c36fc HEAD` ⇒ **OK, descends from
ce51c36fc.** (The workflow's own worktree HEAD `d2b32f6e3` is *behind* ce51; the prescribed
`git worktree add ../lock3-b2 c93fc43143` was created and all measurement ran there — no
wrong-artifact risk.) arm64; no x86. loadavg at measure 9.65–10.60 (machine shared with sibling
agents).

---

## QUESTION

Can emission be re-expressed as one `emit(&GrammarIr, &Attributes)` fold — deleting `BackendExpr`
and `SinkOnlyExpr` — with all regen/check outputs byte-identical, and does `has_rule("value")`
**retire** (not relocate)? **Make-or-break:** do the four `SinkOnlyFacts` accumulations read any
datum outside `GrammarIr + backend_shape + descriptor`?

---

## METHOD

Static data-flow trace of every `SinkOnlyFacts` accumulation site back to its ultimate source,
verified against the *compiled, passing* tree; plus a RUN of the baseline byte-identity harness to
pin the floor the collapse must preserve. No collapse fold was implemented (see VERDICT scope — it
exceeds the 50-min cap without landing a broken tree; the gate it hinges on is resolvable by trace,
which is what "earns the substrate").

Files read: `passes/src/lib.rs` (`compile`, `LayoutFacts:91`, `single_plan:1283`, `lower_expr:1318`,
`materialize_rule:1362`, `derive_materialization_roles:1403`, `derive_materialization_plan:1139`,
`derive_backend_shape_with_diagnostics:407`), `ir/src/lib.rs` (`BackendShape:360`,
`BackendExpr/Rule:369-413`), `codegen/src/lower/sink_only.rs` (all), `runtime_generator.rs`
(`render:78`, `emit_sink_only:89`, `has_rule("value"):98`), `json_sink_direct/dispatch.rs`,
`general_descent/view.rs`, `lower/mod.rs` (`select_lowering`), `grammar/src/lib.rs:1354`
(`AltMode::Dispatch`), `skinny/grammars/json.bbnf`, generated `json/config.rs` + `sheets/config.rs`.

---

## WHAT RAN (measured)

**Baseline byte-identity** (`skinny/ $ cargo run -q -p xtask -- <check>`), one invocation, all four:

| check | exit | drift |
|---|---|---|
| `check-json`        | 0 | none |
| `check-sheets`      | 0 | none |
| `check-real-typed`  | 0 | none |
| `check-css-l4-declaration-values` | 0 | none (`inputs: css_l4_declaration_values 886e55b8…`) |

4/4 byte-identical, ~7 s wall (warm target), loadavg 9.65. **The floor holds today.** This is what a
byte-identical collapse must reproduce.

---

## THE NUMBERS

### Make-or-break gate — DATA-FLOW TRACE of the four `SinkOnlyFacts` (`sink_only.rs:264-269`)

| Fact | Accumulation site | Reads field | Ultimate source | Outside {GrammarIr, backend_shape, descriptor}? |
|---|---|---|---|---|
| `direct_shapes`     | `:222` `insert(shape)`            | `BackendExpr::DirectBuild.shape` | `materialize_rule:1384` ← `descriptor.shape` = `format!("{prefix}…")`, `prefix = pascal_case(grammar.name)` (`plan:1141`); descriptor from `derive_materialization_roles(GrammarIr)` | **NO** — GrammarIr.name + descriptor |
| `span_kinds`        | `:208` `insert(span_kind)`        | `BackendExpr::RegexProgram.span_kind` | `lower_expr:1343` `backend_span_kind(pattern)` ← `analyze(pattern).hir.kind`; `pattern` from `ExprKind::Regex` | **NO** — GrammarIr |
| `literals`          | `:203` `insert(bytes)`            | `BackendExpr::ByteLiteral.bytes` | `lower_expr:1340` ← `ExprKind::Literal.bytes` | **NO** — GrammarIr |
| `dispatch_alt_count`| `:185` `+= branches.len()` (when `mode==Dispatch`) | `BackendExpr::Alt.{mode,branches}` | `lower_expr:1326` copies `ExprKind::Alt.{mode,branches}`; `mode` born `Dispatch` at parse (`grammar:1354`) | **NO** — GrammarIr |

**GATE VERDICT: PASS.** None of the four reads a `LowerCtx`/`policy_summary` fact. Every one is a
pure function of `GrammarIr` (regex patterns, literal bytes, Alt mode/arity, grammar name) and the
descriptor map (itself a pure derivation of `GrammarIr` via `derive_materialization_roles`). The four
are recomputable *byte-identically* inside a single `GrammarIr`-walking fold that recomputes
`materialize_rule`'s 6-node splice inline. **Q-3B-1 answered: byte-identity holds at the fact layer;
no accumulation carries an emit-order fact the fold cannot reproduce.** This resolves the last edge of
**Axis M** — E4 (the fact accumulation) is pure ⇒ the 3-level multipass substrate is earned.

**The one impurity, correctly out of scope.** The *fifth* `SinkOnlyProgram` member `policy_summary`
(`sink_only.rs:135-150`) *does* read `ctx.per_grammar_policy` (`:137`) and `ctx.same_substrate_union`
(`:143`). But it is not one of the four accumulations — it is a **pure attribute re-export** of facts
`compile()` already computed into `LayoutFacts.{per_grammar_policy, same_substrate_union, backend_shape}`
(`passes:54-57`). In the `emit(&GrammarIr,&Attributes)` fold it is *read from the Attributes bundle*
(the prescribed `LayoutFacts → Attributes` promotion), never recomputed in the walk. The gate is about
the four; the four are clean.

### G-3B-2 — `has_rule("value")` retire: **REFUTED AS STATED (relocates, does not retire into `backend_shape`)**

`emit_sink_only:98` routes `json_sink_direct` vs `general_descent` on `sink_only.has_rule("value")`.
Measured discriminator values:

- `json/config.rs:22` → `W7_DIRECT_BACKEND_SHAPE = "SinkOnly"`
- `sheets/config.rs:3` → `BACKEND_SHAPE = "SinkOnly"`

**Both grammars are uniformly `BackendShape::SinkOnly`** (every rule; `passes:58-61` debug-asserts
`backend_shape == cost_facts.chosen` across all rules). Therefore the `backend_shape` attribute — the
mechanism pass-1 named for the "clean retirement" — **cannot distinguish JSON from Sheets.** The name
probe *is* deletable (`grep 'has_rule("value")' == 0` achievable), but the dispatch it keyed
**relocates to a materialization-structural attribute**, the natural one being `roles.value.is_some()`
from `derive_materialization_roles` (or equivalently "program carries a value-dispatch Alt"), NOT
`backend_shape`. Pass-1's G-3B-2 "clean retirement via the backend_shape attribute" is **incorrect on
mechanism**; the honest disposition is *relocation to the value-role attribute*.

### G-3B-3 — `derive_materialization_roles@1403`: **RELOCATES (born-RED LOCK-7 edge), NOT retired**

`skinny/grammars/json.bbnf` carries **0** `->` projections (measured `grep -c '\->' == 0`). The eight
roles (`container/sequence/pair/string/number/bool/null/value`) are recovered by pure structural
heuristic over `GrammarIr` — literal `{`/`}`/`[`/`]` presence, regex `SpanKind`, `:` literal,
ref-count ≥ 4. RETIREMENT requires "JSON gains `->`" so the grammar is authoritative; that is false at
this commit. **Disposition: RELOCATED** into the attribute-computation layer as a born-RED
structural-inference edge, carried to LOCK-7 grammar-totality. (Q-3B-2: relocation is the honest floor
for this pass; retirement is a LOCK-7 obligation, not smuggled.)

### G-3B-4 — key granularity: **per-`ExprId`** (confirmed)

`materialization.descriptors` is per-`RuleId`, but `ExprKind::Project` (`->`) is per-`ExprId` and
`node_types` is already `HashMap<ExprId, Type>`. JSON has 0 projections so is trivially per-`RuleId`;
Sheets carries `->` at multiple sub-expression positions per rule → the Attributes bundle must key
projection/type at `ExprId`. **Per-`ExprId` subsumes per-`RuleId`; recommend per-`ExprId`.**

### G-3B-5 — net-LOC and extensibility

**Extensibility 5→2: CONFIRMED.** `select_lowering` (`lower/mod.rs`) has 5 `BackendShape` arms, but
only **2** artifact families reach the runtime emitter: `LoweredArtifact::{SinkOnly, Scan}`
(`render:78`). The four tape arms (`eager_tape`/`offset_tape`/`event_tape`/`collapsed_stage`) are
plan-*string* stubs (`lower_rule → tape_plan::render_rule`) the live fleet never emits. Realized emit
fleet = **{sink-fold, scan-island} = 2**.

**Net-LOC ≈ −220: GESTALT-conditional, NOT LOCK-3-local.** Measured gross-deletable surfaces:

| surface | lines |
|---|---|
| `BackendExpr` + `BackendRule` (`ir:369-413`) | 45 |
| `single_plan`+`lower_expr`+`materialize_rule` (`passes:1283-1389`) | 107 |
| `SinkOnlyExpr` enum + `sink_only::lower_expr` (`sink_only.rs:69-100,174-235`) | 94 |
| **gross** | **246** |

But `BackendExpr`/`SinkOnlyExpr` are **shared, tree-walked** consumers, not emission-only
intermediates:
- `BackendExpr` is the substrate `derive_backend_shape_with_diagnostics` (`passes:407-431`) walks to
  *produce* `backend_shape` — the very attribute the fold reads (`contains_direct_build`/
  `prefers_event_tape`, `passes:~967-1011`). Deleting it forces shape-derivation onto `GrammarIr`.
- `SinkOnlyExpr` is **structurally walked** by `json_sink_direct/dispatch.rs` (318 LoC:
  `find_dispatch_alt`, `collect_literals`, `rule_has_byte`, `span_kind`) and by `general_descent/
  view.rs` (431 LoC: `collect_projection_kinds`). These re-express onto `GrammarIr`, they don't delete.

So the emission-slice deletion of `SinkOnlyExpr` **adds back** a comparable re-walk of `GrammarIr` in
`json_sink_direct` + `general_descent`, and the emit fold itself is +80–120 LoC. **LOCK-3-local net ≈
−90 to −120**, not −220. The −220 lands only as a **GESTALT-B** figure, once the sibling
shape-derivation and tape backends co-excise `BackendExpr` and the three isomorphic tree-walk families
(`GrammarIr → BackendExpr → SinkOnlyExpr`) collapse to one. Reported honestly rather than as a
LOCK-3-local excision.

---

## VERDICT LINE

**GATE PASSES — the fold's fact layer is PURE.** The four `SinkOnlyFacts` accumulations
(`direct_shapes`, `span_kinds`, `literals`, `dispatch_alt_count`) read *only* `GrammarIr` + descriptor
(⊆ `GrammarIr + backend_shape + descriptor`); `policy_summary`'s `ctx` reads are a pure attribute
re-export carried in the Attributes bundle, not a fold recomputation. Byte-identity is reproducible at
the fact layer (Q-3B-1 = YES). The substrate is **earned** (E4-pure ⇒ Axis-M last edge resolves for
3B). **Scope-honest limits:** (1) `has_rule("value")` **relocates to the value-role attribute, NOT
`backend_shape`** — G-3B-2's named mechanism is refuted, since JSON and Sheets are both uniformly
`BackendShape::SinkOnly` and thus `backend_shape`-indistinguishable. (2) `derive_materialization_roles`
**RELOCATES** (JSON carries 0 `->`) — born-RED LOCK-7 edge, not retired. (3) net-LOC is **≈ −100
LOCK-3-local / −220 GESTALT-conditional**, extensibility **5→2 confirmed**. (4) The killer gate G-3B-1
(*execute* the collapsed fold and re-run regen×check byte-identically) **remains UNRUN** — this probe
cleared its precondition by verified trace and pinned the floor (4/4 baseline pass) but did not
implement the fold within cap; running the fold is the remaining falsifiable step.

---

## PATCH / DIFF

**None.** No code was changed. The collapse fold was deliberately not implemented within the 50-min
cap: it is a cross-crate re-expression (`ir` + `passes` + 3 `codegen` modules) that cannot land
byte-identically in the remaining budget without risking a broken tree, and the make-or-break question
is resolvable by trace against the *passing* tree (which is what earns the family). The concrete
Stratum-1 edit-map for a follow-on executing session:

1. `passes:91` — promote `LayoutFacts` → `Attributes` bundle (add `materialization: MaterializationPlan`,
   keep `backend_shape`/`per_grammar_policy`/`same_substrate_union`/`node_types@ExprId`).
2. New `emit(&GrammarIr, &Attributes) -> SinkOnlyProgram` (generalize `sink_only::lower_program`):
   walk `GrammarIr.rules`; per node, the `lower_expr:1318` match arms become fold arms; recompute
   `materialize_rule:1362`'s 6-node splice inline from `attrs.materialization.descriptors[rule.id]`;
   accumulate the four facts in the single pass; read `policy_summary` from `attrs`.
3. Re-base `json_sink_direct/dispatch.rs` + `general_descent/view.rs` tree-walks onto `GrammarIr`
   (or onto the retained `SinkOnlyProgram` if only `BackendExpr` is deleted first).
4. `runtime_generator.rs:98` — replace `has_rule("value")` with `attrs.materialization.roles.value
   .is_some()` (the value-role attribute); keep CSS as the declared `attrs.scan` island (`render:83`
   `LoweredArtifact::Scan → emit_scan`, unchanged).
5. Delete `single_plan`/`lower_expr`/`materialize_rule` (`passes:1283-1389`), `BackendExpr`/`BackendRule`
   (`ir:369-413`) **only after** shape-derivation + tape backends are co-transposed (else keep
   `BackendIr` as the shape-derivation working IR — Stratum-1 emission collapse without the full
   type deletion).
6. `regen×check × {json, sheets, real-typed}` byte-identical; CSS via the `attrs.scan` island;
   `grep -rn 'has_rule("value")' == 0`.
