# LOCK-3 · Family 3A — THE CONTRACT TOWER (pass-2 research)

Pinned to `skinny@3f2350f15` (live HEAD; the directive's `b3cb8c16b` and R6's
`7f3be27c2` have drifted — every `path:line` below re-opened on `3f2350f15`).
Scope is organ-level: designs target the greenfield tower; the port carries the
five live edges enumerated in §2 as the substrate of record.

3A is the **fixed small tower**: a bounded chain of transposition edges, each a
*total function* carrying a *checkable contract* (a per-level parity/round-trip
oracle), CI-enforced. It is the discipline formulation, not a re-architecture: it
does not demand one-fold collapse (3B) nor grammar-as-IR (3C); it demands that
whatever edges exist be *named, total, and oracle-guarded*, and that the declared
`->` type be *consumed by contract* rather than re-derived by heuristic.

---

## 1. The mechanism made concrete

The tower is the existing lowering chain, promoted from an implicit sequence of
functions to a **labeled edge-DAG with a verifier per edge**. Three moving parts:

1. **Level contracts.** Each edge `Lₖ → Lₖ₊₁` gets a `verify(pre, post) -> Result`
   predicate that is (a) *pure* (reads only its two artifacts), (b) *emit-silent*
   (regen byte-identical with every verifier on or off), (c) CI-wired. This is the
   nanopass "output-language is checked against the pass's declared target"
   discipline and rustc's inter-pass `Validator`, expressed as data over the live
   IRs rather than a macro-generated grammar.

2. **The `->` type consumed, not re-derived.** The single edge where 3A pays for
   itself is the infer edge (§3): `ExprKind::Project.ty` is *resolved* by infer
   into `node_types[expr]`, and the downstream projection consumer
   (`classify_projection`) *reads that resolved node-type* instead of string-
   matching the raw `target`/`ty`. The verifier for this edge is the RED→GREEN
   witness of §4.

3. **`differential(edge, corpus)`.** One runner (§5) that generalizes the live
   track1/track2 fingerprint parity into a *per-edge* digest, so an injected mis-
   lowering localizes to the edge it was injected on, and edges with no corpus
   witness are reported `unverified` (distinct from `sound`).

The tower's *size* is the design knob. The live chain is five edges (§2); the 3A
claim to test is that they collapse to **three level-languages** — `TypedHir`
(`GrammarIr` + resolved `node_types`), `MidIr` (`BackendExpr`/`SinkOnlyProgram`),
`Emit` (`EmittedSource`) — with the two remaining edges (parse→IR, tape-plan sub-
lowering) folded as *intra-level* normalizers guarded by the same oracle.

---

## 2. The substrate — the live transposition edge-DAG (enumerated)

Walking `3f2350f15`, the spine has **five labeled edges** and **four re-encodings of
the same "value" datum**:

| Edge | From → To | Site | Value carried |
|---|---|---|---|
| E1 | AST → `GrammarIr` | `ir/src/lib.rs` parse+`normalize` | `ExprKind` (10-arm: Seq/Alt/Repeat/Optional/Literal/Regex/Ref/Annotation/**Project**/Discard) |
| E2 | `GrammarIr` → typed facts | `passes/src/lib.rs:154` `rule_types`; `:212` `node_types` | `Type`; **`Project.ty` dropped at `:204`** |
| E2′ | `GrammarIr` → `MaterializationPlan` | `passes/src/lib.rs:1139` `derive_materialization_plan` → `:1403` `derive_materialization_roles` | per-`RuleId` `MaterializationDescriptor{kind,label,shape,fields}` |
| E3 | `GrammarIr`+plan → `BackendExpr` | `passes/src/lib.rs:1349` `lower_expr`; `:1362` `materialize_rule` | `BackendExpr` (13-arm); node re-encoded as `SpanMark/TapeEmit{TapeKind}/DirectBuild{shape,fields}/Return` seq |
| E4 | `BackendIr` → `LoweredArtifact` | `codegen/src/lib.rs:148-235`; `lower/{sink_only,tape_plan,eager_tape,offset_tape,event_tape,collapsed_stage,css_scan}.rs` | `SinkOnly(SinkOnlyProgram)` \| `Scan(program)` |
| E5 | `LoweredArtifact` → `EmittedSource` | `codegen/src/runtime_generator.rs:78` `render` → `:89`/`:147`/`emit_scan` | Rust source strings |

**Distinct value levels of the node datum:** *four* re-derivations — (a)
`Project.ty` declared string (E1), (b) `MaterializationDescriptor.kind` (E2′), (c)
`BackendExpr::TapeEmit.kind` + `DirectBuild.fields` (E3), (d) `classify_projection`
`ProjectionKind` at emit (E5, general path). 3A's edge-DAG makes each an oracle
boundary; the *design target* is to derive (b)(c)(d) from (a) by one total
function so the datum is authored once.

**`materialize_rule` / `SinkOnlyProgram` reconciliation.** `materialize_rule`
(`:1362`) is a *pure derivation* over `MaterializationDescriptor` — SpanMark(Start)
· body · SpanMark(End) · TapeEmit(kind) · DirectBuild(shape,fields) · Return — it
carries **no datum absent from the plan**; the plan is its only external input.
`SinkOnlyProgram` (`codegen/src/lower/sink_only.rs:31` `has_rule`) is E4's artifact:
a rule set with `entry_rule`, `policy_summary`. So the SpanMark/TapeEmit/DirectBuild
/Return framing is *E3-internal* and the SinkOnly framing is *E4-internal* — two
levels, not two framings of one edge.

---

## 3. The shared `->` thread — how 3A *consumes* one threaded node-type

The datum exists and is `Option<String>`: `ExprKind::Project{body,target,ty}`
(`ir/src/lib.rs:261-265`). Its flow on live:

- **E2 infer**, `passes/src/lib.rs:204` — `ExprKind::Project{body,..} =>
  self.infer_expr(*body)` — records `node_types[project] = type_of(body)` and
  **discards the declared `ty`**. (`:203` `Annotation→Unit` is a legitimate untyped
  directive, not a defect — untouched.)
- **E3 lower**, `passes/src/lib.rs:1349` — `ty` *is* carried structurally into
  `BackendExpr::ValueProject{target,ty}`, then into `SinkOnlyExpr::ValueProject`.
- **E5 consume**, `codegen/src/general_descent/view.rs:35`
  `classify_projection(target, ty)` — re-parses the **raw strings** (`"f64"→Float`,
  `"true"→Bool`, `…u8→Tag`, `Some("Span")→Span`). String re-derivation on the emit
  side; infer never validated or typed it.

**The 3A consumption contract (the executed thread):** infer at `:204` resolves
`Project.ty` to a `Type` (against `rule_types`/builtins) and records
`node_types[project] = resolved`. `classify_projection` is rewritten to take
`&Type` from `node_types[project_expr]` — it *reads the one threaded node-type*
instead of matching `target`/`ty` text. Crucially, 3A does **not** re-derive the
category in the emitter; the category *is* the infer result. Evidence to bank: the
diff at `:204` + the `view.rs:35` signature change, with `classify_projection`'s
match arms deleted (they become a total `Type`→`ProjectionKind` map).

The directive names the greenfield consumer `TypeAnalysis::from_ir`
(`backend/driver/analysis.rs`, XL-F); that file **does not exist on `3f2350f15`**.
Its two live analogues are `classify_projection` (Sheets/general path) and
`derive_materialization_roles` (JSON structural path). The thread lands on the
former directly; it does **not** reach the latter — see the §7 counterexample.

---

## 4. The per-pass `verify` predicate (RED→GREEN, emit-silent)

Wire `verify_project_typed(grammar, facts)` over every `ExprKind::Project`:
predicate = "`node_types[project]` is the *declared* projection type, not merely
`infer_expr(body)`". On `3f2350f15` this reads **RED** for every projecting rule
(Sheets: `google-sheets.bbnf:6,12,17-18,34-42,97`) because `:204` stored the body
type. After the §3 thread it reads **GREEN**. The predicate touches only `facts`;
`derive_materialization_plan`/`lower_expr` are unchanged, so **regen is byte-
identical** with the predicate on or off — the emit-footprint-zero proof is a
`diff` of the emitted tree across the flag. This is the RED→GREEN witness the
directive asks for; a *verifier*, not an emitter change — satisfying the tower's
"contract per level, silent on output" rule.

---

## 5. `differential(edge, corpus)` — one runner over the live digests

Live substrate: `bbnf-bench/src/direct_struct.rs` computes a whole-parse
`JsonDirectDigest{fingerprint:u64}` by `mix()`-folding tag+bytes (`:59,136,158`),
exposed as `track1_digest`/`track2_digest` (`:421/:428`) and
`track1/2_strict_product` (`:461/:467`, via `real_typed_struct::typed_checksum`);
`benches/json_parity.rs:52` fingerprints track2, `:200/:207` asserts track1≡track2.
This is an *end-to-end* mix/XOR checksum at E5 only.

**Generalization:** define `digest_at(edge, artifact)` for each E1..E5 by folding
that level's canonical form with the *same* `mix()` (reusing `typed_checksum` at E5
reproduces the existing digests bit-for-bit — the (a) requirement). Then
`differential(edge, corpus)` runs both tracks (generated vs handcoded) and reports
the *first edge* whose digests diverge. Injecting a mis-lowering — e.g. swap
`TapeKind::String→Number` in `materialize_rule` (`:1381`), or drop a `DirectBuild`
field — leaves E1/E2 digests equal and diverges E3↓, so the fault **localizes to
E3** (the (b) requirement). Corpus-coverage metric: `witnessed(edge×kind)` =
fraction of `(edge, TapeKind/ProjectionKind)` cells exercised by ≥1 corpus input; a
cell with 0 inputs is reported `unverified`, never `sound`. Evidence to bank: the
runner executed on the live JSON corpus (`test-fixtures/`) plus one injected fault
per edge, showing the localization row and the coverage table.

---

## 6. Integration seams (path:line on `3f2350f15`)

- Thread `ty`: `passes/src/lib.rs:204` (infer arm) → `:212` (`node_types.insert`).
- Consumer flip: `codegen/src/general_descent/view.rs:35` `classify_projection`;
  `:52-59` `collect_projection_kinds`.
- Verifier host: `passes/src/lib.rs:126` `TypeFacts` (add `verify` alongside).
- Edge E2′ classifier to retire: `passes/src/lib.rs:1403`
  `derive_materialization_roles` (byte-literal `b"{"`,`b"true"`,`b":"`, `≥4`
  value-ref heuristic).
- Name-gate at E5: `codegen/src/runtime_generator.rs:98` `has_rule("value")`.
- Differential runner home: `bbnf-bench/src/direct_struct.rs` (`mix`,
  `typed_checksum`); parity harness `benches/json_parity.rs:200`.
- `materialize_rule` derivation: `passes/src/lib.rs:1362`; plan `:1139`.
- `DirectTypeKind` (Struct-only schema): `codegen/src/direct_schema.rs:52`.

---

## 7. Named risks & counterexamples

- **JSON has no `->` (the load-bearing counterexample).** `grammars/json.bbnf` (18
  lines) carries **zero projections** (R6 §0); the JSON taxonomy is recovered
  wholly by `derive_materialization_roles`. Threading `Project.ty` therefore does
  **nothing** for the JSON path and **cannot retire** the byte-literal classifier
  there. 3A has two lawful moves, and must pick one as its contract: (i) *declare
  the taxonomy* — add `-> object/array/string/…` to `json.bbnf` so the heuristic is
  retired uniformly (strong form; risks a regen delta to prove emit-neutral); or
  (ii) keep the structural classifier as a **distinct, total tower level with its
  own oracle** for projection-free grammars (honest form; the tower then has two
  entry level-languages, not one). This choice is the family's central open gate.
- **`has_rule("value")` is a name-gate, not shape** (`runtime_generator.rs:98`)
  despite its "never a grammar name" comment: it keys on a rule literally *named*
  `value`. Under the tower it must be **retired**, not relocated — the JSON-vs-
  general split is a property of the resolved E2 types (presence of a recursive sum
  node), which the threaded `node_types` can decide. Relocating it inside a fold
  (3B) would leave the name-gate intact; 3A's contract forbids that.
- **`ty: Option<String>` is untyped text**, and `classify_projection` matches
  literals (`"f64"`, `"true"`). A projection to a user type the classifier does not
  enumerate silently falls to `Span` (`view.rs:45`) — a *silent-epsilon* hazard the
  standing law forbids. The threaded `Type` must make the unknown-projection case
  an error, not a `Span` fallthrough.
- **Digest false-negatives.** `mix()` is a non-cryptographic fold; two distinct
  mis-lowerings could collide a `u64`. The coverage metric (witnessed cells) bounds
  this; a collided cell still shows `unverified` unless a *structural* witness (not
  just the digest) is present.
- **Tower-size claim unproven.** "Five edges collapse to three" is a hypothesis;
  E4's tape-plan sub-lowerings (`lower/tape_plan.rs`, `offset/eager/event_tape`) may
  resist folding into a single MidIr level. Must be measured, not asserted.

---

## 8. Prior art (with citations)

- **Nanopass** — Sarkar/Waddell/Dybvig, *A Nanopass Infrastructure for Compiler
  Education*, ICFP '04; Keep & Dybvig, *A Nanopass Framework for Commercial Compiler
  Development*, ICFP '13 (andykeep.com/pubs/np-preprint.pdf; nanopass.org).
  `define-language` + `define-pass`: each pass declares input/output languages and
  the framework *checks the output is well-formed in the target language* — exactly
  3A's per-edge contract, macro-generated. Lesson: the tower need not be macro-
  magic; the contract can be a `verify` predicate over the existing IRs.
- **rustc MIR `Validator`** (`rustc_mir_transform::validate`, `-Zvalidate-mir`) runs
  between `MirPass`es checking type-compatibility invariants — the emit-silent
  inter-pass verifier 3A wires at each edge (rustc-dev-guide `mir/passes.md`).
- **MLIR** — per-op `verify()` hooks + pass-manager `-verify-each` (Lattner et al.,
  CGO '21). Per-node structural contracts revalidated after every pass; the model
  for E3/E4 node-shape oracles.
- **LLVM** `verifyModule`/`verifyFunction` + `VerifierPass`; **CompCert** (Leroy,
  CACM '09) per-pass *semantic-preservation simulation* — the strongest form of a
  level contract (behavioral, not structural), the aspirational ceiling for the
  `differential` oracle.
- **Differential testing** — Csmith (McKeeman lineage) / EMI equivalence-modulo-
  inputs: mutated inputs localize a miscompiling pass. `differential(edge,corpus)`
  is EMI restricted to the tower's labeled edges with per-edge digests.

---

## 9. What a prototype must test to move 3A

1. **Thread + consume:** implement §3 (`:204` resolves `ty`; `view.rs:35` consumes
   `node_types`), prove regen byte-identical on the Sheets fixture. *Gate: byte-diff
   = ∅.*
2. **RED→GREEN verifier:** wire §4, capture RED on `3f2350f15` and GREEN after (1),
   emit tree identical across the flag. *Gate: the transition + ∅ diff.*
3. **Differential runner:** build §5, reproduce `track1≡track2` digests bit-exact,
   inject one fault per edge (TapeKind swap at `:1381`, dropped DirectBuild field,
   `classify_projection` mis-map) and show each localizes to its edge; emit the
   coverage table with `unverified` cells named. *Gate: 3/3 localized + digest
   reproduction.*
4. **The JSON gate (§7):** decide and *demonstrate* which lawful move retires
   `derive_materialization_roles` — add `->` to `json.bbnf` and prove emit-neutral,
   or specify the second tower level-language with its own oracle. *Gate: heuristic
   retired, or a named second level with a passing oracle.*
5. **Tower-size measurement:** count the edges surviving as oracle boundaries after
   folding; report net-LOC delta and whether E4's tape sub-lowerings fold. *Gate:
   measured edge count + LOC, not asserted.*

Sources:
[Nanopass tutorial](http://wonks.github.io/keep/fall2012/2012/11/02/a-nanopass-framework.html) ·
[Keep & Dybvig ICFP'13](https://andykeep.com/pubs/np-preprint.pdf) ·
[nanopass.org](https://nanopass.org/) ·
[rustc MIR Validator](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_transform/validate/struct.Validator.html) ·
[rustc-dev-guide MIR passes](https://rustc-dev-guide.rust-lang.org/mir/passes.html)
