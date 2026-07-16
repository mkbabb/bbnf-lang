# LOCK-3 · Family 3D — STAGED-RESIDUAL · pass-2 research

The IR is an explicitly two-stage program: every node carries a binding-time fact
(`Stage::Comptime` vs `Stage::Runtime`); **residualization is the erasure of the
Stage-0 sub-graph**, and the certified PROVE-12 win becomes a *corollary* of one
`binding_times` pass rather than a bespoke CSS mutation. Type-plane erasure is the
same operation restricted to the type sub-plane: the discarded `->` node-type is a
Stage-0 residual whose target is a Rust *type* (zero runtime instructions —
Constitution v, satisfied by construction). This report grounds both islands on the
live tree, names the re-pin, cites the prior art, and states the one prototype.

## 0. Re-pin (the stale-base hazard, XL-H)

Skinny live HEAD = **`779013467`** (`state(sk-v24): bbnf pass-2 …`, base `ce51c36fc`).
The directive's re-pin token `b3cb8c16b` is an *ancestor meta-commit* (the lock-set
amendment) in the same tree, not a different source snapshot. The three seed bases are
all stale: R6@`7f3be27c2`, GN-3@`c255baf21`, **PROVE-12@`d2b32f6e3`**. Every seam below
was re-opened at `779013467` and the line numbers are live. **The consequential drift:
PROVE-12's patch targets a raw-string template (`CSS_GENERATED_RS` in
`runtime_generator.rs`) that NO LONGER EXISTS** — `runtime_generator.rs:224` reads "the
verbatim raw-string courier is gone"; CSS now emits through
`css_scan_direct::render(program)` (`runtime_generator.rs:228`). PROVE-12's residual must
be re-homed into the `css_scan_direct` emitter (§4); its *measured* numbers stand, its
*patch* is wrong-artifact.

## 1. Two Stage-0 residual islands, both already exposed on live

### 1a. The structural-alphabet island (declared dependency — not re-authored)

The grammar-derived stop-set ships today as a proven Stage-0→Stage-1 erasure:
`derive_alphabet` (`crates/codegen/src/lower/css_scan.rs:487-570`) harvests rule-literal
bytes (`ruleBlock` `{`/`}`, `atRuleBody` `;`, declaration `:`, `cssString` quotes, `@ws`
comment `/`) off `ScannedRule::terminal_bytes()` into `ScanAlphabet` (`css_scan.rs:38-56`);
`render_alphabet_consts` (`css_scan_direct/mod.rs:112-155`) erases them to emitted
`const SKIP_ALPHABET: [u8;9]`, `const AT_RULE_STOP/QUALIFIED_STOP/DECLARATION_STOP/
COLON_BEFORE_STOP: &[u8]`. This is a *first-Futamura* residual already in the tree: the
grammar literal is Stage-0 comptime data, the emitted `const` is the Stage-1 boundary.
Family 3D **declares** this island; it does not re-author it.

PROVE-12 deepened exactly this island (measured GO/CONFIRMED, ins/B −3.7…−6.4%,
cyc/B 0.81-0.98×, byte-identical): it takes the `&[u8]` STOP set one erasure further,
const-folding it to a `u128` bitmask carried as a `const DELIMS: u128` generic so the
per-byte `contains` scan collapses to `byte<128 && (DELIMS>>byte)&1` (one immediate-`and`
pair, no slice load, no length guard — REPORT §3 disasm, VERIFY §4 otool-confirmed). In
binding-time terms: the membership test's *set operand* is Stage-0-bound, so
monomorphization is the residualizer. **The staged-residual thesis: PROVE-12 is not a CSS
trick — it is the generic act of erasing a Stage-0-bound operand, and a `binding_times`
pass that marks the STOP-set operand Stage-0 emits it for free at every such site.**

### 1b. The JSON first-byte-dispatch island (the NEW-site the directive names)

The exact JSON analogue is live and un-residualized: `json_sink_direct/dispatch.rs`
(`derive_arms`, `:31-59`) walks the `value` rule's `Alt{Dispatch}` branches, resolves each
`CallRule` callee, and reads its first-byte keys via `first_bytes(program,&callee.expr)`
(`json_sink_direct/first_set.rs`). Header (`dispatch.rs:1-11`): "the 91.5% hot leaf … the
arm SET and KEYS come from the tree, so dropping a rule from the `.bbnf` makes its arms
vanish." The dispatch is emitted as a `match byte` in **three** non-unified monomorphs
(`parse_value_direct` / `parse_object_value_at_direct` / `parse_array_element_at_direct`).
The first-byte key set is grammar-derived (Stage-0) but the emitted dispatch is a naive
`match` — the residualization candidate for the directive's PMU obligation (§6).

## 2. The mechanism — `Stage` fact + one `binding_times` pass

Net-new substrate, minimal: a two-point lattice `enum Stage { Comptime, Runtime }` (a
join-semilattice, Runtime ⊒ Comptime; join by monotone propagation — the LLVM/dataflow
`Changed`-bool fixpoint, `changed-flag-convergence`, NOT a structural hash). One pass,
`binding_times(&GrammarIr) -> StageFacts` (a side-table keyed by `ExprId`/`RuleId`,
colocated with `CostFacts` in `crates/ir/src/cost.rs`, which is already the per-rule
fact home — `cost.rs:4-22`). Rules:

- A `ByteLiteral`/regex-`terminal_bytes` node whose bytes are fully known at gen time is
  **Comptime** (this is what `derive_alphabet` and `first_bytes` already compute — the
  pass *names* the fact they implicitly rely on).
- An operand consumed only to select among Comptime-bound alternatives (a STOP set, a
  dispatch key set) is **Comptime**; residualization = emit it as a `const`/const-generic.
- Input-`source[pos]`-dependent nodes are **Runtime**; join lifts any node with a Runtime
  input to Runtime.
- A `Project.ty` node (§3) is **Comptime on the type sub-plane** — its residual target is
  a Rust type, never an instruction.

Residualization is then one rule: *every maximal Comptime sub-graph is const-emitted; the
Runtime frontier is the emitted code.* PROVE-12's mask, the STOP consts, and the dispatch
table all fall out of the single fold. This is offline **binding-time analysis** (Jones–
Gomard–Sestoft two-level annotation) with the generator as the partial evaluator (Futamura
projection 1); Zig `comptime`, MetaOCaml `.<>.`/`.~`, and LMS `Rep[T]` are the three
industrial realizations of exactly this static/dynamic split (§5).

## 3. Type-plane erasure = Stage-0 erasure on the type sub-plane (the concrete thread)

The shared `->` question, answered in staged terms. `passes/src/lib.rs:204`
(`ExprKind::Project { body, .. } => self.infer_expr(*body)?`) drops `target` and `ty` (the
node is `ExprKind::Project { body, target, ty }`, `ir/lib.rs:261-265`). **Reconciliation of
XL-F:** on the live *skinny* tree there is NO `TypeAnalysis::from_ir` / `backend/driver/
analysis.rs` (0 hits; `crates/backend` does not exist) — that consumer lives in the legacy
bbnf-lang main tree, not here. The live type consumer is
`derive_materialization_plan` → `DirectBuildField.target` (hardcoded `None`,
R6 §5d, `passes/src/lib.rs:1149-1287`) → `json_typed_direct::render` → an emitted Rust
`struct`. Threading `ty` end-to-end is therefore a Stage-0 residual whose *sink is a Rust
type*: the `ty: Option<String>` string is comptime data, erased at generation into a field
type, contributing zero runtime instructions. This is how 3D **consumes** the one threaded
node-type (not re-derive it): the threaded leaf is the residual's payload; the
`binding_times` pass certifies it Comptime-on-the-type-plane, and the codegen struct-emit
is its erasure. GN-3's W2 (thread `Project` → `DirectBuildField.target`, kill the 4-way
`ProjectionKind` fold at `general_descent/view.rs:20-46`) IS the type-plane instance of the
staged-residual mechanism — 3D subsumes it as a corollary, not a parallel effort.

## 4. Integration seams (path:line, live @ `779013467`)

- Stage fact home: `crates/ir/src/cost.rs` (colocate with `CostFacts`); `Stage` enum + a
  `StageFacts` side-table; consumed by codegen, never serialized to runtime.
- `binding_times` pass: new, in `crates/passes/src/lib.rs` beside `infer` and
  `derive_materialization_plan`; reads `ExprKind` (`ir/lib.rs:234-265`).
- Structural residual (PROVE-12 re-homed): `css_scan_direct/mod.rs:112-155`
  (`render_alphabet_consts` — emit the STOP sets as `const … : u128` masks instead of
  `&[u8]`) + the driver render in `css_scan_direct/drivers.rs` (rewrite the scan loop to the
  `<const DELIMS:u128>` shift-and-test). `ScanAlphabet::{at_rule_stop, qualified_stop,
  declaration_stop, colon_before_stop}` already produce the byte sets — add mask accessors.
- Type residual: drop-site `passes/src/lib.rs:204`; plan `None`-fill
  `passes/src/lib.rs:1149-1287`; lower `Project→ValueProject` `passes/src/lib.rs:1349`;
  emitter `json_typed_direct::render` (`json_typed_direct.rs:10`); schema IR
  `direct_schema.rs`.
- JSON dispatch residual (NEW site): `json_sink_direct/dispatch.rs:31-59` +
  `first_set.rs`.
- **Dead "stage" to abrogate, not extend:** `BackendShape::CollapsedStage`
  (`ir/lib.rs:366`), `admits_collapsed_stage` = `target.avx512bw && Entry(_)`
  (`passes/src/lib.rs:959`), `collapsed_stage_author_declared`, `fuse_span_stage`
  (`tape_plan.rs:44`). This is an *existing* per-rule "stage" fact but gated on `avx512bw`
  (x86, dead by refutation law iv) — it is dormant on aarch64. 3D's `Stage` is a distinct,
  live concept; per `abrogate-before-patch`, the CollapsedStage machinery should be
  deleted, not conflated with the binding-time fact (named risk R3).

## 5. Prior art (citations)

- **Binding-time analysis / offline partial evaluation:** Jones, Gomard, Sestoft,
  *Partial Evaluation and Automatic Program Generation* (1993) — the two-level annotated
  language; static/dynamic BTA is exactly `Stage::{Comptime,Runtime}`. **Futamura
  projections** (1971): a generator = self-applied partial evaluator; PROVE-12's
  gen-time const-fold is a first-projection residual.
- **Multi-stage programming with staging in the types:** Taha & Sheard, *MetaML/MetaOCaml*
  (2000) — `.<>.`/`.~` brackets are the two-level types 3D's charter names ("comptime/
  runtime split in the types"). Rompf & Odersky, **LMS** (`Rep[T]` tags binding time in the
  type) — the cleanest match for "type-plane erasure = stage-0 erasure." DeVito et al.,
  **Terra** (PLDI 2013). Industrially: **Zig `comptime`** — comptime params erase to
  specialized code; `find_component_delim::<{delim_mask(..)}>` IS a comptime param realized
  via **rustc const generics + monomorphization**.
- **Residualized automata / prefilters:** `regex-automata` fully-compiled dense/sparse DFAs
  vs lazy hybrid, and Teddy as a compiled Stage-0 prefilter — the STOP-set island is the
  degenerate (single-state) case. **MLIR** progressive lowering + constant-folding as a
  lattice attribute with legality is the compiler-infra analogue; the residual must be
  emitted deterministically, **not deferred to LLVM luck** (PROVE-12's stance).
- **Out of scope by Constitution/GN-3:** CSP/e-graph extraction of the residual — the
  binding-time lattice is decidable monotone dataflow; no solver consumer exists in this
  family (GN-3 §1.5).

## 6. The one prototype (what moves this family)

`binding_times` run on the **REAL** CSS *and* JSON IR (not a synthetic mirror), producing:
(a) the CSS STOP-set nodes marked Comptime → re-home PROVE-12 into `css_scan_direct` and
reproduce its byte-identical, ins/B −3.7…−6.4% result *from the pass* rather than the hand
patch; (b) the JSON first-byte dispatch (`dispatch.rs`) residualized to a const key table,
measured **cold, paired, N≥200, PMU ins/B** on twitter/citm/canada/bootstrap (the directive's
NEW-site obligation — unmeasured plausibility does not count, PROVE-09); (c) the `Project.ty`
threaded through `:204` to `DirectBuildField.target` and emitted as a Rust struct field type,
with `git diff crates/runtime/` **empty** proving the type residual costs zero runtime
instructions (Constitution v). Success = one fold produces all three, with a per-pass
`verify` predicate over `Project`@204 reading RED on HEAD, GREEN once `ty` threads, and
regen byte-identical with the predicate on/off.

## 7. Named risks / counterexamples

- **R1 — the emitter moved (§0).** PROVE-12's patch is dead against live; re-homing into
  `css_scan_direct` is real work, not a cherry-pick. Its *numbers* transfer only if the
  re-homed emit is byte-identical to the old template output at the scan loop.
- **R2 — JSON may not win.** R6 §3 notes JSON's hot leaves already dispatch to hand kernels;
  the const dispatch table could be a wash. The prototype must be allowed to return a *named
  negative* (dispatch already residual-enough) rather than force a claim (PROVE-09 law).
- **R3 — CollapsedStage collision.** The dead avx512 "stage" must be abrogated, not extended;
  reusing its vocabulary would smuggle x86-gated dead code under a live name.
- **R4 — capacity hints are corpus facts, not grammar facts** (GN-3 R1): `vec_with_capacity`
  numbers (875/184/10_800/5_759) are Stage-0 only if the *typed grammar* carries them
  (`-> Vec<T> @cap(N)`); otherwise they are not derivable and the type residual is
  structural-modulo-capacity. Decides whether type-plane byte-identity is honestly reachable.
- **R5 — three un-unified dispatch monomorphs** (`dispatch.rs:8-11`): residualizing one and
  not the others reintroduces the very drift the single fold is meant to kill.
