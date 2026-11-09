# PROTO-1 — LOCK-3 pass-1: the level-count adjudication (3A typed-tower vs 3B one-IR + per-pass contracts)

**Probe:** does per-pass contract-on-one-IR localize what a typed-level tower would?
**Family under test:** 3A (a typed-level IR tower) vs 3B (a single collapsed IR guarded by per-pass contracts) — the lock's only hard incompatibility.
**Cap:** 50 min. **Worktree:** off `skinny/`. **Corpora:** live `json` + `css-l4-declaration-values` grammars/fixtures. **loadavg during runs:** 7.9–15.8.

---

## Question

Is the second IR (`BackendIr`/`BackendExpr`/`BackendRule`) **pure redundancy**, AND can a **per-pass contract** on the collapsed IR **localize a defect** (a dropped projection type) **without a distinct typed level**?

## Method (what the brief asked vs what the code is)

The brief's anchors are from an idealized model; the live worktree (`skinny@d2b32f6e`) maps as follows:

| brief anchor | live reality |
|---|---|
| `ExprKind::Project` + `type_ref` | `ExprKind::Annotation { name, value: Option<String> }` (the `->` projection/directive node); `type_ref` = `DirectBuildTypeRef` |
| "Project.ty dropped at passes:203" | `passes:203` is literally `ExprKind::Annotation { .. } => Type::Builtin(BuiltinTy::Unit)` — infer discards the declared `value` |
| L0→L1 (infer) edge | `layout::types::infer` → `TypeFacts::node_types: HashMap<ExprId, Type>` (the attribute table) |
| LayoutFacts→ir::Attributes | `LayoutFacts` (passes:91) already holds `node_types` as a side-table; descriptors already live in `MaterializationPlan` |
| second IR produce/consume | `single_plan`/`lower_expr`/`materialize_rule` (passes) → codegen `sink_only`/`tape_plan` lowerers |

Given the 50-min cap and loadavg, the full collapse (delete `BackendIr`, rewrite the whole codegen emit path as one fold) is not runnable; the **parsimonious probe** exercises the two decisive, cheap, *runnable* claims (b) and (c), and settles (a) by the actual data-flow.

## What ran

**Baseline (HEAD, unmodified).** All four checks green, generated files clean:
`regen-json`=0 · `check-json`=0 · `regen-css`=0 · `check-css-l4-declaration-values`=0.

**Probe (b) — per-pass contract localizes without a typed level.**
Added `pub fn verify(grammar, type_facts)` to `passes` (asserts every `ExprKind::Annotation` with a declared type carries a populated, non-`Unit` `node_type`), wired into `compile()` immediately after `infer`. Runnable test `passes/tests/verify_contract.rs`:

- `cargo build -p passes` → **GREEN** (the dropped type_ref is a missing/`Unit` `HashMap` value, *not* a compile error).
- `cargo test -p passes --test verify_contract` → **3 passed / 0 failed**:
  - `head_infer_drops_projection_type_ref` — HEAD infer collapses `-> Number` to `Type::Builtin(Unit)` at passes:203.
  - `verify_turns_red_and_names_the_infer_edge` — `verify()` returns `Err` naming the **L0→L1 (infer)** edge and the dropped **type_ref**.
  - `verify_is_green_when_type_ref_is_populated` — preserving the type_ref clears the contract (localized fix at the infer edge).

**Probe (c) — zero runtime footprint.**
With `verify()` wired into `compile()`, all four checks re-run: `regen-json`=0 · `check-json`=0 · `regen-css`=0 · `check-css…`=0, and `git status` on `crates/runtime/src/grammars/` is **empty** → emitted `.rs` **byte-identical** on/off.

## What did NOT run (honest)

**Probe (a) — the full collapse was not executed.** Deleting `BackendIr` and re-expressing the emitter as one `emit(&GrammarIr, &Attributes)` fold requires rewriting 2 producer + 2 consumer lowerers (the whole codegen emit path) — beyond the cap. Instead, the **data-flow verdict**: `materialize_rule` (passes:1356) *synthesizes* emit-order framing — `SpanMark{Start}` / body / `SpanMark{End}` / `TapeEmit` / `DirectBuild` / `Return` — that has **no counterpart** in `GrammarIr::ExprKind` (only Seq/Alt/Repeat/Optional/Literal/Regex/Ref/Annotation). So the second IR is **not vacuous as a data structure**; it carries a real emit-order fact. BUT that framing is a **pure deterministic function of (grammar, materialization descriptors)** — it is **tree-redundant** (recomputable inside one fold) though **computation-essential**. Byte-identity after a faithful collapse is therefore *plausible but unproven here*.

## The numbers

| metric | value |
|---|---|
| verify + wiring added | **+43 LOC** (`passes/src/lib.rs`) |
| runnable contract test | **+59 LOC** (`passes/tests/verify_contract.rs`) |
| net probe delta (as run) | **+102 LOC** |
| deletable second-IR surface (static) | **62 LOC** ir defs (`BackendIr`/`BackendExpr`/`BackendRule`) + **114 LOC** passes producers (`single_plan`/`lower_expr`/`materialize_rule`) + codegen `sink_only`/`tape_plan` consumers ≈ **≥176 LOC** net-deletable, offset by an `Attributes` promotion of the already-existing `MaterializationPlan` descriptors |
| `->` lowering-edge sites | **5** — `single_plan`, `lower_expr`(passes), `materialize_rule`, `sink_only::lower_expr`, `tape_plan::render_expr` → **target 2** (one derive-attributes pass + one emit fold). Reduction 5→2 *identified, not executed*. |
| tests | 3 run / 3 pass; build green throughout |

## Verdict

- **(b) PROVEN — RAN/MEASURED.** A per-pass contract on the single IR localizes a dropped `type_ref` to the exact **L0→L1 (infer)** edge while `cargo build` stays green. **3A-as-a-typed-tower is not required for defect localization** — a cheap per-pass assertion does the work a distinct typed level would. Hybrid/3B wins on this axis.
- **(c) PROVEN — RAN/MEASURED.** `verify()` has **zero emit footprint**: all four checks byte-identical with the contract on vs off.
- **(a) PARTIAL — NOT RUN.** "Second IR is pure redundancy" is **refuted as literally stated** (it carries emit-order framing absent from L0), yet those facts are a pure derivation → **relocatable to Attributes + one fold**. The 3B substrate is **viable**, pending a full-collapse byte-identity proof that exceeded the cap.

**One-line adjudication:** the second IR's *facts* are load-bearing but its *tree* is not; a single IR plus per-pass contracts localizes the one defect a typed tower would catch — so **3A-as-a-persisted-tower is refuted for the fleet; 3B (collapse + per-pass contract) is the substrate**, with the collapse's byte-identity the one claim still owed a run.
