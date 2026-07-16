# LOCK-3 · Family 3D — STAGED-RESIDUAL · pass-2 research

Re-pin: the sole living tree is `skinny@c43126422` (HEAD at authorship;
`git rev-parse HEAD`). The directive's `b3cb8c16b` is not this HEAD and is
not present as a tag here — treat it as a stale pointer; every measurement
below is anchored to `c43126422` or to the PROVE-12 worktree base `d2b32f6e3`
where explicitly named. R6's `7f3be27c2` numbers are re-verified live below.
All hosts aarch64 (NEON + scalar oracle; no x86). Line numbers are `c43126422`.

The formulation: **the IR is an explicitly two-stage program — a binding-time
plane over the existing nodes. Stage-0 facts are const at generation time and
must vanish from the emitted recognizer (residualized); stage-1 is the per-byte
runtime.** PROVE-12 is the corollary: it residualized one stage-0 fact (the
delimiter set) and the stage-0 witness disappeared from the disassembly. This
family generalizes that from one hand-found site to a *binding-time analysis*
attached to live IR, with the type sub-plane as a second, symmetric stage-0
residual. It does NOT author a new substrate — the substrate already exists on
live (§1) and is currently mis-gated and under-consumed (§2, §5).

---

## 1. The substrate already ships — three live stage-0 facilities

The two-stage program is not a greenfield invention; skinny already carries
three of its organs, un-unified:

**(a) `BackendShape::CollapsedStage` — the two-stage *shape*.**
`crates/ir/src/cost.rs:131-136` gives it a policy triad distinct from every
other shape: `SubstrateTarget::LocalTempOnly` + `RetentionLifetime::LocalLoop`
(`cost.rs:58-86`) — i.e. "nothing survives the loop; stage-0 is collapsed into
a local temporary." It is a first-class candidate in the decision CSP
(`passes/src/decision_csp.rs:68-78`, `lib.rs:554-560`, priority
`P3CollapsedStage`), it has an emission target
(`runtime_plan::CollapsedStageRule` / `CollapsedStagePlan`, asserted in
`codegen/src/lib.rs:1270,1423-1424`), and a diagnostic when its wrapper is
absent (`passes/src/diagnostics.rs:13,50,59`). **This is the stage-plane's home
on live.** The staged-residual formulation's job is to make its admission
grammar-derived and its collapse the general residualization engine.

**(b) `ScanAlphabet` / `derive_alphabet` — the grammar-derived stage-0
alphabet.** `codegen/src/lower/css_scan.rs:38-102,487-565`. Every field
(`block_open`, `terminator`, `colon`, `balanced_pairs`, `quotes`,
`comment_lead`) is *harvested from the CSS L4 rule literals/regexes* at
generation time (`css_scan.rs:134` calls it from `derive_program`), never a
fixed literal. The per-call-site STOP vectors (`at_rule_stop`,
`qualified_stop`, `declaration_stop`, `colon_before_stop`, `css_scan.rs:82-101`)
and the fixed-9 SKIP array (`skip_alphabet`, `:66-80`) are *rendered from it*
into `generated.rs` by `css_scan_direct` (`mod.rs:112`, `projection.rs:446`).
Per the directive: **declare this as the dependency; do not re-author it.** It
is precisely a stage-0 residual island — a const `&[u8]` feeding a SIMD scan
(`css_scan_direct`) — and it is what PROVE-12 const-folded.

**(c) PROVE-12 — the residualization *transform*, CONFIRMED.** Base
`d2b32f6e3`. It took one stage-0 fact (the delimiter STOP set, a
`ScanAlphabet`-derived constant threaded as a runtime `&[u8]`) and made it a
`const DELIMS: u128` bitmask (`patch.diff` — `delim_mask` const fn +
`find_component_delim::<{...}>`). The stage-0 witness — the slice loads
`ldrb [x22,#N]` and the length guards `cmp x21,#2/#3` — **disappeared** from the
emitted symbol (REPORT §3, VERIFY §4: mask-as-immediate, no slice loads). Every
CSS corpus/lane won: ins/B −4.8…−6.8% deterministic, cyc/B 0.80–0.91×,
byte-identical parity (XOR checksum MATCH), regen bit-for-bit, +18 generated LOC
(SPEC §0 row 12, CONFIRMED). **Residualization = erasure of a stage-0 node is
thereby a proven, measured operation on live emit — the family's central claim
already has one executed instance.**

---

## 2. Q: can a "stage" fact attach to live IR nodes such that that residual
## island is provably a stage-0 node the live emit path exposes?

Yes, and the attach point is concrete. Define a `BindingTime ∈ {Stage0, Stage1}`
*fact* computed over the lowered `BackendExpr` tree (not a new IR — an
annotation, per 3B's shape, so 3D is 3B's type-directed refinement, not a rival
tower). The rule is standard two-level λ (Nielson & Nielson; Jones/Gomard/Sestoft
*Partial Evaluation*): a node is Stage0 iff all its inputs are grammar-constant.

Concretely on live: a node is Stage0 exactly when it is derived from
`ScanAlphabet` (grammar literals) or from a `ByteLiteral`
(`passes/src/lib.rs:1340`) or a `Regex` whose `SpanKind` is fixed — everything
`derive_alphabet` (`css_scan.rs:487`) reads. Stage1 is the input-dependent
per-byte scan loop. The binding-time run on the **real CSS IR** is: walk the
`CssScanProgram` (`css_scan.rs:104-117`), mark every `ScanAlphabet`-sourced STOP
vector as Stage0 (it is a pure function of grammar literals — the
`at_rule_stop`/`declaration_stop` bodies at `:82-101` read only `self.<field>`),
mark the `find_component_delim` scan body as Stage1. **PROVE-12 already
witnesses this partition**: it residualized exactly the Stage0 STOP-set nodes
and left the Stage1 loop. So `binding_times(CssScanProgram)` is not synthetic —
it *reconstructs the partition PROVE-12 hand-found*, and its correctness oracle
is: "the Stage0 set = the set of nodes whose residualization leaves parity
byte-identical." That oracle is runnable today via the PROVE-12 checksum
(REPORT §5).

For JSON the honest answer (R6 §5, verified live: `grammars/json.bbnf` has zero
`->`, `passes/src/lib.rs:204` discards `Project.ty`): the JSON hot path
(string/number kernels, `generated.rs`) has no interpretive stage-0 island left
to residualize — R6 confirms "JSON's hot rules already dispatch straight to
hand-written kernels — nothing to residualize there." So the JSON stage-0
residual is NOT in the scan loop; it is in the **type/shape plane** (§3), which
is where 3D's second, non-overlapping win lives.

**Counterexample that bounds the claim:** `admits_collapsed_stage`
(`passes/src/lib.rs:959-961`) currently reads
`target.avx512bw && matches!(rule_ir.expr, BackendExpr::Entry(_))`. On the
aarch64-only host `avx512bw` is permanently false (SPEC §0 refutation: x86
dead), so **`CollapsedStage` is never admitted on live** — it is a dead shape,
and `lib.rs:495` fires `CollapsedStageNotViable` whenever the author declares it.
The staged-residual formulation's *first obligation* is to re-home this
admission predicate off the dead x86 feature onto a grammar-derived binding-time
fact ("this Entry rule has a stage-0 residual island of size ≥ k"). This is a
named RED gate the family must carry and is the sharpest evidence the current
stage-plane is consumer-less at runtime (Constitution viii, Y.13).

---

## 3. Q: can "type-plane erasure = stage-0 erasure on the type sub-plane" be
## shown by threading the :204 `ty` as a stage-0 residual to a Rust *type*?

This is the family's distinctive thesis and it is *the same mechanism* as
PROVE-12 applied to the type sub-plane. The type plane costs zero at runtime
(Constitution v) — which is exactly the statement "the type plane is entirely
Stage0." So threading `Project.ty` from infer to a Rust type IS a stage-0
residualization: the `->` annotation is a grammar constant (Stage0), and its
correct residual is not a runtime tag but a *Rust type parameter / struct field
type* emitted at generation time, leaving zero runtime witness — symmetric to
PROVE-12 leaving zero delimiter-slice witness.

The end-to-end thread (shared question + 3D's consumption contract):

- **Edge:** `crates/passes/src/lib.rs:204` — `ExprKind::Project { body, .. }`
  returns `self.infer_expr(*body)?`, dropping `ty` via `..`. (`:203`
  `Annotation → Unit` is legitimate untyped — not a target.)
- **Carrier already exists downstream:** lowering keeps `ty` verbatim —
  `lib.rs:1349-1353` `ExprKind::Project → BackendExpr::ValueProject { body,
  target, ty }`. So `ty` is NOT lost at lower; it is lost at *infer* (the type
  fact never becomes a resolved type) and then *folded* at emit
  (general-descent `classify_projection` collapses it to a 4-arm
  `ProjectionKind{Float,Bool,Tag,Span}`, R6 §5b).
- **3D's consumption contract (how it CONSUMES one threaded node-type, not
  re-derives):** thread the `:204` `ty` into `node_types` (`infer_expr` writes it
  at `lib.rs:212`) as a *resolved stage-0 type*, then have the schema-derivation
  consumer read `node_types[project_expr]` rather than re-classifying the target
  string. The honest live type-IR that receives it is `DirectSchemaSet` /
  `DirectFieldSchema.type_ref` (`codegen/src/direct_schema.rs:4-120`) — a
  full-fidelity struct/Vec/Option/Scalar lattice with a working direct-to-struct
  renderer (`json_typed_direct::render`, `json_typed_direct.rs:10`, 1245 LOC).
  Today its only input is **hand-authored**
  (`xtask/src/real_typed_schema.rs:7`). The consumption is: a
  `derive_direct_schema` pass sets `DirectBuildField.target.type_ref` from the
  threaded node-type (currently always `None` on the offset path — R6 §5c,
  `passes/src/lib.rs:1149-1287`), and `emit_typed_from_source`
  (`codegen/src/lib.rs:130`) emits the Rust struct field type. The `ty` never
  appears at runtime → stage-0 erasure to a Rust type, byte-for-byte the same
  emitted parser plus a typed façade.

**The named downstream consumer** the directive asks for is therefore
`emit_typed_from_source`'s schema input (`direct_schema.rs` → `json_typed_direct`)
— it exists, it works, and it is *starved of exactly the fact `:204` drops*.
(The directive's `TypeAnalysis::from_ir` in `backend/driver/analysis.rs` does
NOT exist on live — `grep` finds no `TypeAnalysis`/`from_ir`/`analysis.rs` in
`crates/`; that is a greenfield/designed name. The live consumer that stands in
its place is the `DirectSchemaSet` seam, the same architectural role. This must
be flagged as a named divergence: the shared directive points at a not-yet-live
consumer; 3D binds instead to the live typed emitter.)

---

## 4. Q: if both land — what NEW-site cold paired-PMU delta?

Two disjoint deltas, measured separately (unmeasured plausibility does not count
— PROVE-09):

- **Scan residual (CSS):** already measured — PROVE-12 ins/B −4.8…−6.8%,
  cyc/B 0.80–0.91×. Re-pinned to `c43126422`, the prototype must re-run
  `css_canon_bench` PMU to confirm the number holds on the current tree (the
  base moved `d2b32f6e3`→`c43126422`).
- **Type-plane residual (JSON first-byte dispatch):** the directive's specific
  ask. The type-plane erasure produces a *typed* direct-to-struct parser vs the
  structural `value_from_ref` classify-fold. The measurable is the JSON
  first-byte dispatch (`grammars/json/generated.rs:105` `match byte`, R6 §3):
  does a stage-0-erased typed emitter remove the runtime `value_from_ref` switch
  (`value.rs:143-171`) at leaves it statically knows? Predicted delta is a
  *removal of the classify branch* at typed leaves — but this is UNMEASURED and
  is the prototype's PMU obligation on `json_canon_bench` cold per-parse (no warm
  benches — Constitution iv). Report ins/B; if zero or negative, it is a named
  negative result, not a claim.

---

## 5. Integration seams (path:line, live `c43126422`)

| Seam | Site | Role in 3D |
|---|---|---|
| Type-fact drop (shared edge) | `crates/passes/src/lib.rs:204` | thread `ty` into `node_types` (`:212`) as Stage0 resolved type |
| Type-fact carrier (survives) | `crates/passes/src/lib.rs:1349-1353` | `ValueProject{ty}` — already carries `ty` to backend |
| Type-fact fold (lossy) | `general_descent/view.rs:20-46` `classify_projection` | the 4-arm collapse to replace with `node_types` read |
| Stage-0 alphabet | `codegen/src/lower/css_scan.rs:38-102,487-565` | the grammar-harvested Stage0 fact; declared dependency |
| Residualization transform | PROVE-12 `patch.diff` (`runtime_generator.rs` CSS template) | the proven erasure; generalize to a `residualize(Stage0)` pass |
| Two-stage shape | `ir/src/cost.rs:131-136`; `passes/src/lib.rs:554-560` | `CollapsedStage` + policy triad = the stage-plane home |
| Dead admission gate | `passes/src/lib.rs:959-961` (`avx512bw`) | RED: re-home off x86 onto binding-time fact |
| Typed emitter (type consumer) | `codegen/src/lib.rs:130`; `direct_schema.rs:4-120`; `json_typed_direct.rs:10` | consumes threaded `ty` via derived `DirectSchemaSet` |
| Hand schema (to retire) | `xtask/src/real_typed_schema.rs:7` | replaced by `derive_direct_schema` |
| Decision CSP (stage choice) | `passes/src/decision_csp.rs:68-135` | where per-site staging is "solved by LOCK-2's engine" |

Boundary held (directive §Boundaries): LOCK-3 owns WHERE the stage fact attaches
(the binding-time annotation over `BackendExpr`, and the fact contract at the
L1→L2 lower edge `lib.rs:1349`); the *engine* that solves per-site staging is
LOCK-2's `decision_csp` — 3D declares the variable ("stage-0 residual depth per
site") and the objective (PMU cost), not the solver. `no-ts-ir` holds: the
type-plane residual emits Rust *type declarations* at build time (the typed
emitter), never a runtime IR (XL-D).

---

## 6. Prior art (citations)

- **Two-level λ / binding-time analysis:** Nielson & Nielson, *Two-Level
  Functional Languages* (1992) — the `Stage0/Stage1` type discipline is theirs.
  Jones, Gomard, Sestoft, *Partial Evaluation and Automatic Program Generation*
  (1993) — "residualization = specialize away static inputs" is the exact
  operation PROVE-12 performs; the Futamura projections frame "generator eats a
  grammar" as the first projection.
- **Staged metaprogramming:** MetaML / MetaOCaml (Taha, Sheard) — `<e>`/`.~`
  brackets = the binding-time annotation made syntactic; Rust `const generics` +
  monomorphization are the host's staging primitive PROVE-12 rides
  (`find_component_delim::<{delim_mask(..)}>`).
- **Terra / Zig `comptime`:** the two-stage-program-in-the-types model 3D names
  — Zig's `comptime` params are literally "this value is Stage0"; the
  `CollapsedStage` shape is skinny's analogue.
- **LLVM/rustc:** const-folding + GVN is the ad-hoc residualizer PROVE-12
  refuses to trust ("not deferred to LLVM luck", REPORT §2) — the family's stance
  is *explicit* staging in the generator, LLVM as backstop only.
- **regex-automata:** prefilter/DFA tables are compile-time-derived stage-0
  artifacts feeding a runtime scan — the same shape as `ScanAlphabet`→SIMD; the
  isomorphism to LOCK-5's continuum is that both are grammar-derived Stage0
  tables. (MLIR's progressive-lowering "one IR, many dialects" is the model 3B
  carries; 3D adds the binding-time dimension orthogonally.)

---

## 7. Risks & counterexamples

1. **The dead x86 gate (proven).** `admits_collapsed_stage` = `avx512bw && …`
   (`lib.rs:960`) makes the stage shape unreachable on aarch64. Until re-homed,
   `CollapsedStage` is consumer-less substrate — the strongest live evidence and
   the family's first RED gate. Named risk: re-homing must not smuggle x86 back.
2. **JSON has no scan-side stage-0 to residualize** (R6 §5, verified). 3D's JSON
   win is *entirely* in the type sub-plane; if the type-plane PMU delta (§4) is
   zero, JSON yields no scan residual and the family's JSON claim collapses to
   "typed façade, same speed" — acceptable only if declared as such.
3. **`ty` is `Option<String>`, not a resolved type** (`ir/src/lib.rs`
   `ExprKind::Project`, R6 §5a). Threading it demands a *resolution* step
   (string → `DirectTypeRef`), which risks re-deriving rather than consuming.
   The contract in §3 forces consumption (read `node_types`), but the resolver
   itself is new surface — must be minimal (KISS) and live in the schema-deriver,
   not scattered.
4. **CollapsedStage may double-home with LOCK-2/3B.** The stage *shape* is a
   BackendShape (LOCK-2 cost territory) and the stage *annotation* is an IR fact
   (3B territory). 3D must present as the binding-time *refinement* of 3B's
   annotate-in-place tower, sharing `node_types`, not a fourth transposition.
5. **Residualization generality is unproven beyond one site.** PROVE-12 is one
   hand-found delimiter scan. "Any grammar-derived STOP-set scan" (REPORT §7) is
   asserted, not measured on JSON structural scan or general-descent.

---

## 8. What a prototype must test to move the family

1. **`binding_times(CssScanProgram)` on the real CSS IR** (not a mirror):
   compute the Stage0/Stage1 partition, and prove it *equals* the set PROVE-12
   residualized by re-running the PROVE-12 checksum (REPORT §5) with each Stage0
   node's residualization toggled — parity byte-identical iff the partition is
   correct. Re-pin to `c43126422`.
2. **Re-home `admits_collapsed_stage` off `avx512bw`** onto a grammar-derived
   binding-time predicate (Entry rule has a Stage0 residual island ≥ k), and
   show `CollapsedStage` becomes *admissible on aarch64* via `decision_csp`
   without a stub — RED→GREEN on `CollapsedStageNotViable` (`diagnostics.rs:13`).
3. **Thread `:204` `ty` → `node_types` → `DirectFieldSchema.type_ref`**: one
   grammar with `->` projections (Sheets already has them —
   `grammar/google-sheets/google-sheets.bbnf:6,12,17-18`), derive a
   `DirectSchemaSet` from the threaded fact (retiring the hand schema
   `real_typed_schema.rs:7` for that grammar), emit via `json_typed_direct`, and
   show the emitted parser is byte-identical plus a typed façade with **zero
   runtime `ty` witness** (disasm: no `classify_projection` branch at typed
   leaves).
4. **Cold paired PMU** on both residuals: CSS (re-confirm PROVE-12 on new base)
   and JSON first-byte dispatch (the §4 obligation) — `css_canon_bench` /
   `json_canon_bench`, cold per-parse, ins/B deterministic floor + cyc/B, report
   negatives honestly (PROVE-09 law).
5. **Generalization probe:** apply the PROVE-12 `const fn mask` + const-generic
   seam to one non-CSS grammar-derived STOP scan (the neutralized JSON
   structural scanner, R6 §8a `grammars/json/scan.rs:34`) and measure — to move
   "generalizes to any STOP-set" (REPORT §7) from assertion to a second data
   point.
