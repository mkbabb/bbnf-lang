# GN-3 — THE TYPE PLANE

The three walls fall, in order, and the hand-authored schema dies. When this tranche
closes, the direct-to-struct lane's shape is derived from grammar `->` projections by
Hindley–Milner inference at generation time — no `real_typed_schema.rs`, no runtime
cost, no grammar-name gate. The schema stops being a thing a human writes beside the
grammar and becomes a thing the grammar *is*.

---

## 1. Charter

### 1.1 Mission

Turn the value/type projection surface from a monotone structural fold into a
compile-time inference plane that authors the direct-to-struct schema from grammar
alone. The proving instrument (PROVE-08, `prototypes/PROVE-08/`) built and measured the
reusable half — a 221-LOC HM unification core, arrows→leaf resolution, `type_ref`
fill — and adversarial verification (`PROVE-08/VERIFY.md`, CONFIRMED_DELTA) certified
its three findings as LAW: the derivation runs perf-neutral (compile-time only), and
the equality target is unreachable *for a generic `json.bbnf`* against three walls.
GN-3 lands the exact two net-new pieces PROVE-08 named as the only path through those
walls — a sum-type node in the schema IR, and per-document typed grammars — and wires
the derivation into the live regen lane, deleting the 1014-LOC hand file.

The type plane is a schema *producer* that runs in the generator. It emits no runtime
instruction. Its win is not speed; its win is that generality is proven by
construction — the walls are cured by design, not by patch, and there is one schema
path, not two.

### 1.2 The three walls (verifier-exact, the GN-3 charter)

From `PROVE-08/REPORT.md §4` and `PROVE-08/VERIFY.md`, reproduced at HEAD
(`skinny` @ `c255baf21`):

- **Wall 1 — information.** `xtask/src/real_typed_schema.rs` = 1014 LOC, 46 `struct_ty`
  sites, 19 roots (VERIFY: 46/19 exact; REPORT's 45 was off-by-one). It encodes the
  shape of *specific documents* — field names `statuses`/`text`, nesting
  `Vec<Vec<Vec<NumberString>>>`, capacity hints `875`/`184`/`10_800`/`5_759`. A generic
  JSON grammar (`pair*`, runtime keys) contains none of it. **Cure (W3): the information
  moves into the grammar** — `twitter_search.bbnf` and kin carry the document shapes as
  `->` chains, so the schema is derivable because the shape is *declared*.
- **Wall 2 — IR expressiveness.** `schema/direct_schema.rs:52-58`: `DirectTypeKind` has exactly
  one variant, `Struct`. The grammar's natural output is a recursive **sum** —
  `value = object | array | string | number | bool | null` — which the schema IR cannot
  represent. **Cure (W1): add `DirectTypeKind::Sum`**; grow HM `unify` over sums.
- **Wall 3 — front-end.** At PROVE-08's base `d2b32f6e3`, `->` was not in `ExprKind`.
  At HEAD it IS: `ExprKind::Project { body, target, ty }` (`ir/src/lib.rs:261`). The
  wall is half-fallen; the projected type is still discarded at
  `passes/src/lib.rs:204`. **Cure (W2): thread `Project` through `infer` into
  `DirectBuild.type_ref`.**

### 1.3 Precept bindings

Bound by number to the codex (`sk-v23 research/07-precept-codex.md`):

- **§8 / §37 / §40 (Y.13 — substrate lands with its consumer, same wave).** Every wave
  below names its in-wave consumer (§4). No inference primitive lands without a caller
  that reads its output in the same wave.
- **§4 (abrogate before patch).** The hand schema is an intrinsically un-general
  subsystem; W3 asks "can we delete?" and does — `real_typed_schema.rs` is removed, not
  wrapped.
- **§5 / §6 (one path, no legacy).** After W3 there is one schema producer:
  `type_plane::derive`. The hand file does not survive feature-flagged behind an env
  toggle; it is deleted and the build must compile without it.
- **§14 / §15 (no god modules, directory modules).** `schema/type_plane/` is a directory-module
  colocated with its consumers (`schema/direct_schema` + the `schema/json_typed_direct` emitter),
  owning `hm.rs`, `derive.rs`, `sum.rs`; tests live in `crates/codegen/tests/`, never inline.
- **§53 (generated files regen-only, byte-identical).** W3's equality target *is* the
  idempotent-on-disk rule: derived-schema emit ≡ hand-schema emit, byte-for-byte, so the
  committed `bbnf-bench` module is unchanged and the runtime binary is untouched.
- **§56 (no silent epsilon / fail-explicit).** An HM unify clash (a rule whose arrows
  disagree) is an explicit `DeriveError::Unify`, never a silent fallback to a default
  leaf.
- **§43-48 (profiling standing law).** W4 and W5 measure the direct-to-struct lane cold,
  paired, N≥200, PMU cyc/B, per the standing law.

### 1.4 Refutation guards (the §0 laws that constrain GN-3)

The certified swath (`SPEC §0`) forbids classes of claim. GN-3's constraints:

- **No shim perf claim (09 REFUTED).** GN-3 makes **zero** performance claim for the
  type plane itself. The plane is compile-time only; its runtime delta is identically
  zero by construction, and "zero" is a null claim, not a win. Any wave that asserts the
  type plane makes parsing faster is refuted at the seam by `git diff` over the runtime
  crate. The only perf numbers GN-3 touches are PROVE-07's direct-to-struct ratios, which
  must **hold unchanged** from derived schemas (W4) — a non-regression, not a new win.
- **Warm benches forbidden.** W4/W5 use cold-per-parse, N≥200, paired interleaving. No
  figure is quoted warm.
- **No digit-scan SIMD (02 NO_GO), no block-scheduling (04), fold-over-cursor dead,
  tape-navigation is not the get lane.** All vacuously satisfied: GN-3 adds **no runtime
  code at all** — no SIMD, no scanner, no navigation. Any such addition in a GN-3 wave is
  a scope breach and belongs to GN-2 (kernel plane) or GN-4 (value fabric).

### 1.5 Supersessions

PROVE-08 REPORT §4 declared the equality-conjunct "architecturally unreachable." GN-3
**scopes and supersedes** that verdict: it is LAW *for a generic `json.bbnf`* and GN-3
does not relitigate it — a generic grammar cannot recover document shapes, and no `->`
addition to `json.bbnf` will. GN-3 instead lands the two net-new pieces PROVE-08 itself
named as the only path (`REPORT §4` closing paragraph: "reachable only by per-document
typed grammars plus a schema IR extended with a sum-type node — both are net-new
architecture") and reaches equality against *per-document* grammars. The refutation
stands; GN-3 changes the grammar, not the refutation.

CSP and e-graph machinery are **out of scope** (Y.13 / §8, parsimony §1.5). Leaf and
sum unification is a decidable Hindley–Milner problem solved by the proven 221-LOC
union-find core; no CSP or e-graph consumer exists in this tranche, so no such
substrate lands. Full-inference CSP/e-graph hybrids remain a GN-series design input
without a GN-3 home.

---

## 2. Interlocks

| Direction | Tranche.wave | Contract |
|---|---|---|
| depends-on | **GN-1.W-final** (THE LOOM) | Ported greenfield `crates/{codegen,passes,ir,xtask,bbnf-bench}` with `ExprKind::Project` present, `direct_schema` + `json_typed_direct` emitter intact, `real_typed_struct.rs` corpus structs present, regen bit-for-bit. GN-3 assumes the port has NOT auto-installed the PROVE-08 `type_plane/` module; GN-3.W1 installs it. |
| parallel (∥) | **GN-2.W1** (kernel plane) | Both edit `crates/passes/src/lib.rs`. GN-2 edits `derive_recognizers`; GN-3.W2 edits `infer_expr`'s `Project` arm (`:204`) and the materialization plan (`:1136-1391`). Disjoint functions, shared file — orchestrator serializes writes; the second lander rebases. Named risk R3 (§7). |
| feeds | **GN-4.W3** (direct-to-struct public API + serde bridge) | Consumes GN-3's derived `DirectSchemaSet` as the schema source for the public typed-decode API. GN-4.W3 must not re-introduce a hand schema. |
| feeds | **GN-5.W3** (overfit audit) | GN-3 introduces zero grammar-name predicates in derivation; GN-5's CI grep-gate inherits this as a standing invariant. |

**Owner gate:** the ZERO-RUNTIME-COST invariant (`PROVE-08/VERIFY.md` attack 2, HELD)
is owner-verified law and is re-proven on every GN-3 wave (§3). No GN-3 wave closes if
the runtime crate is touched.

---

## 3. Wave manifest

| id | name | scope | LOC (+new/−del) | gate | cap |
|---|---|---|---|---|---|
| **W1** | The Sum Node | `DirectTypeKind::Sum` + HM `unify` over sums + render arm | +420 / −0 | born-RED | 30 min |
| **W2** | The Threaded Arrow | `Project` → `infer` → `DirectBuild.type_ref`; kill the 4-way fold | +280 / −45 | born-RED | 30 min |
| **W3** | The Authored Shape | typed grammars author document shapes; derivation replaces + deletes the hand schema | +260 / −1020 | born-RED | 40 min |
| **W4** | The Grammar-Driven Lane | direct-to-struct fed by derived schemas; §0 numbers hold | +120 / −20 | born-verified | 30 min |
| **W5** | The Re-Measure (challenge) | adversarial audit of GN-3's own claims | +40 / −0 | born-RED | 25 min |

**Invariant gate, every wave** (Y.13 / §37, owner-verified): the type plane is
compile-time only.

```
git diff --name-only <wave-base>..HEAD | grep -E '^(skinny/)?crates/runtime/' | head -1
```

Must be **empty**. For W3/W4 the emitted parser lands under `crates/bbnf-bench/src`
(generated output, a runtime consumer) and must be **byte-identical** to the
pre-existing hand-schema regen — proven by W3's equality gate — so the runtime binary
is unchanged. A wave that adds a runtime instruction is refuted regardless of its own
gate.

---

## 4. Per-wave specification

### W1 — The Sum Node (Wall 2 falls)

**Objective.** Make the grammar's natural recursive sum representable in the schema IR,
and grow the HM core to unify an N-arm alternation into one sum type. The composite
`JsonValue` node acquires an IR home.

**File-level plan** (greenfield `crates/codegen/`, colocation edict per §14/§15):

- `crates/codegen/src/schema/type_plane/mod.rs` (+16, installs the PROVE-08 module glue,
  `patch.diff` lines 457-478) — `pub mod {hm, derive, sum};` `pub use`.
- `crates/codegen/src/schema/type_plane/hm.rs` (+221 installed from `PROVE-08/patch.diff:230-456`,
  then +~50 grown) — the proven union-find core lands verbatim, then `TyCon` grows a
  composite constructor and `Ty` grows a `Sum(Vec<Ty>)` shape; `unify` grows an arm that
  reconciles two sums by set-equal variant unification (order-insensitive: the six-arm
  `object|array|string|number|bool|null` unifies to one canonical sum regardless of
  source order). The leaf vocabulary and the honest clash error (`UnifyError`) are
  unchanged.
- `crates/codegen/src/schema/type_plane/sum.rs` (+~90, new) — the bridge from an HM sum monotype
  to `DirectTypeKind::Sum`: variant ordering canonicalization, discriminant assignment,
  and the `DirectSumVariant { tag_literal, rust_variant, payload: DirectTypeRef }` rows.
- `crates/codegen/src/schema/direct_schema.rs` (+~35) — add `DirectTypeKind::Sum { variants:
  Vec<DirectSumVariant>, unknown: UnknownVariantPolicy }` beside the existing `Struct`
  (`:52-58`); extend `validate()` (`:162`) and `validate_type_ref` (`:293`) with the sum
  arm (every variant's payload type_ref must resolve; the tag set must be
  duplicate-free).
- `crates/codegen/src/schema/json_typed_direct.rs` (+~70, the 1245-LOC renderer) — grow
  `render_construct` (`:313`) and add a `render_sum_arm` sibling to `render_field_arm`
  (`:435`) that emits a Rust `enum` with one arm per variant and a byte-dispatch
  `match` on the first source byte (the same `at_cursor` discriminator the runtime
  already uses, `grammars/json/value.rs:28-47`).
- `crates/codegen/tests/type_plane_sum.rs` (+~70, new) — the consumer.

**Seams (path:line, from recon + PROVE-08):** `DirectTypeKind` @ `schema/direct_schema.rs:52-58`
(Wall 2); HM core @ `PROVE-08/patch.diff:230-456`; render entry @
`schema/json_typed_direct.rs:313`; the byte discriminator to reuse @
`grammars/json/value.rs:28-47` (R6 §2).

**PROVE patch landed:** `PROVE-08/patch.diff` — `schema/type_plane/{mod,hm}.rs` land here (the
proven 237 LOC); `derive.rs` defers to W2. Adaptation: `hm.rs` `TyCon`/`Ty`/`unify`
grow the sum arm; the leaf half is verbatim.

**Gate (born-RED).** `crates/codegen/tests/type_plane_sum.rs` constructs the six-arm
`value` alternation, runs `hm::infer`, asserts the result is one `Ty::Sum` of exactly 6
variants (the two `bool` arrows still pre-unify to one `Bool` leaf within the sum),
lowers it via `sum.rs` to `DirectTypeKind::Sum`, calls `schema.validate()` (must pass),
and calls `json_typed_direct::render` — asserting the emitted string contains
`enum` with 6 arms and compiles under `trybuild` (or an inline `syn::parse_file`
well-formedness check).

```
CARGO_TARGET_DIR=target-tp cargo test -p codegen --profile ax-iter --test type_plane_sum
```

Threshold: `1 passed`; the sum has exactly 6 variants; the rendered enum parses as
valid Rust. **Failure semantics:** before the `Sum` variant exists the test does not
compile (the variant is named and absent) — that is the RED state; the wave is not
green until Sum lands end-to-end (IR → validate → render → parses).

**Consumer in this wave (Y.13):** `json_typed_direct::render`'s new sum arm — the
emitted Rust enum is a runtime artifact exercised by the test's well-formedness check.
The substrate (`DirectTypeKind::Sum`) is read by the renderer in the same wave.

**Invariant:** `git diff` over `crates/runtime/` empty (compile-time only).

---

### W2 — The Threaded Arrow (Wall 3 falls; the two discard sites die)

**Objective.** Stop discarding the projected type. Thread `ExprKind::Project.{target,ty}`
through inference so the materialization plan populates `DirectBuildField.target`
(currently always `None`), and retire the lossy 4-way `ProjectionKind` fold in favor of
the type-plane leaf read.

**The two discard sites that die** (R6 names them; brief binds them):

1. `crates/passes/src/lib.rs:204` — `ExprKind::Project { body, .. } => self.infer_expr(*body)?`
   throws away `target` and `ty`. **Replaced** by an arm that classifies the arrow via
   `type_plane::hm::classify_arrow` and unifies the resolved leaf into the rule's
   inference variable, so the projected type reaches the materialization plan.
2. `crates/codegen/src/emit/general_descent/view.rs:20-46` — `enum ProjectionKind {Float,
   Bool, Tag, Span}` + `classify_projection` (the entire type expressiveness that reaches
   emission today, folding every `->` to 4 categories). **Deleted**; the 24 downstream
   call sites in `emit/general_descent/view.rs` (measured, §recon) read the resolved leaf
   from the threaded `DirectBuildTarget.type_ref` instead of re-classifying.

**File-level plan:**

- `crates/codegen/src/schema/type_plane/derive.rs` (+211 from `PROVE-08/patch.diff:13-229`,
  then +~30 adapted) — lands the proven derivation, adapted: instead of re-lexing arrow
  text by source offset (`rule_defs`, the base-IR workaround because `Project` was absent
  at `d2b32f6e3`), `derive` reads `ExprKind::Project` directly from the typed IR
  post-parse. The `owning`/`rule_defs` offset lexer (`patch.diff:181-213`) is deleted —
  it was Wall-3 scaffolding; at HEAD the arrows are structured.
- `crates/passes/src/lib.rs` (+~15 / −1 at `:204`; +~10 at the plan `:1149-1287`) — the
  `infer_expr` `Project` arm classifies + unifies; `derive_materialization_plan` fills
  `DirectBuildField.target` from the resolved leaf (was hardcoded `None`, R6 §5d).
- `crates/codegen/src/emit/general_descent/view.rs` (−~45 / +~30) — `ProjectionKind` +
  `classify_projection` + `collect_projection_kinds` + `rules_for_kind` deleted; the
  accessor generation (`as_f64`/`as_bool`/`as_tag`/`span`, `build_tag_table`) reads the
  threaded leaf.
- `crates/passes/tests/project_thread.rs` (+~50, new) — the consumer.

**Seams:** discard site @ `passes/src/lib.rs:204`; plan `None`-fill @
`passes/src/lib.rs:1149-1287`; lower `Project→ValueProject` @ `passes/src/lib.rs:1349`;
the 4-way fold @ `emit/general_descent/view.rs:20-46`; `classify_arrow` (reused) @
`PROVE-08/patch.diff:374-406`.

**PROVE patch landed:** `PROVE-08/patch.diff` `derive.rs` (adapted — reads structured
`Project`, drops the offset lexer).

**Gate (born-RED).** Two conjuncts, both artefact-backed (§19):

```
cargo test -p passes --profile ax-iter --test project_thread
grep -rc "ProjectionKind\|classify_projection" crates/codegen/src/emit/general_descent/
cargo xtask check-sheets   # sheets regen reproduces byte-identical
```

Threshold: the test asserts `DirectBuildField.target == Some(_)` for all four leaf rules
(`bool`/`number`/`string`/`null`) — non-`None`, the wire R6 flags dropped; grep count
**== 0** (the fold is gone); sheets regen byte-identical. **Failure semantics:** RED
until the infer thread populates targets AND the fold is fully excised AND sheets — the
sole live consumer of the old fold — still regens byte-for-byte. Deleting the fold
without the sheets replacement breaks sheets emission: that is the gate's teeth.

**Consumer in this wave (Y.13):** the sheets general-descent emitter, whose accessor
generation now reads the threaded leaf instead of the deleted fold, and whose regen
byte-identity is the gate. The threaded `target` is read by a real emitter in-wave.

**Invariant:** `crates/runtime/` untouched; sheets generated output byte-identical.

---

### W3 — The Authored Shape (Wall 1 falls; the hand schema dies)

**Objective.** Move the document shapes into the grammar. Author typed grammar variants
whose `->` chains declare the 19-root, 46-struct document taxonomy; derive the schema
from them; prove derived ≡ hand structurally on twitter/citm/canada; then **delete**
`real_typed_schema.rs` (1014 LOC) and require the build to compile.

**File-level plan:**

- `grammars/twitter_search.bbnf`, `grammars/citm_catalog.bbnf`, `grammars/canada.bbnf`
  (+~200 combined, new) — typed grammars. Each authors its document shape as `->` chains:
  `TwitterSearch{ statuses -> Vec<Tweet> }`, `CanadaGeometry{ coordinates -> Vec<Vec<Vec<f64>>> }`,
  the CITM instrument's 31 fields. Capacity hints and closed enums are carried by
  colocated annotations (see R1/R2 below); struct nesting is expressed by rule reference.
- `crates/codegen/src/schema/type_plane/derive.rs` (+~40) — grow `derive` from the leaf-only
  slice (W1/W2) to full struct + sum + Vec/Option cardinality (from `Repeat`/`Optional`,
  R6 §8b) + nested references, emitting the complete `DirectSchemaSet` the emitter
  consumes.
- `crates/xtask/src/main.rs` (+~20 / −6) — `regen_real_typed` (`:240`) and
  `check_real_typed` (`:248`) swap `real_typed_schema::schema()` for
  `type_plane::derive(&grammar, &typed_source)?.schema`; a new `check-derived-eq-hand`
  subcommand diffs the two emits during transition.
- `crates/xtask/src/real_typed_schema.rs` — **DELETED** (−1014); `mod real_typed_schema;`
  removed from `main.rs:6`.
- `crates/codegen/tests/derived_eq_hand.rs` (+~60, new) — asserts structural equality per
  root before deletion.

**Seams:** hand schema @ `xtask/src/real_typed_schema.rs:7` (1014 LOC / 46 struct / 19
roots, VERIFY-exact); the regen lane @ `xtask/src/main.rs:240-256`; corpus structs
(unchanged targets) @ `crates/bbnf-bench/src/real_typed_struct.rs`; cardinality source @
`Repeat`/`Optional` in `ExprKind`; emitter entry @ `codegen/src/lib.rs:130`
(`emit_typed_from_source`, already takes `&DirectSchemaSet` — the swap is at the caller).

**Gate (born-RED, two-phase — §53 idempotent-on-disk is the equality proof).**

```
# Phase A — structural equality, hand schema still present:
cargo run -p xtask -- check-derived-eq-hand   # emits from derived AND hand, diffs
# Phase B — deletion compiles:
git rm crates/xtask/src/real_typed_schema.rs && \
  cargo build -p xtask && cargo xtask check-real-typed
```

Threshold: Phase A `diff` is **empty** (byte-identical emit) for all three corpora;
Phase B builds clean and `check-real-typed` passes (the committed `bbnf-bench` generated
module is unchanged — the runtime binary never moved). **Failure semantics:** RED until
derived ≡ hand byte-for-byte AND the build compiles with the hand file gone. A partial
derivation that emits a *nearly* equal schema fails Phase A; a derivation that needs the
hand file to still exist fails Phase B. The gate cannot pass on a stub.

**Consumer in this wave (Y.13):** `regen_real_typed` — the live regen lane now consumes
`type_plane::derive` and writes the generated typed-decode module into
`crates/bbnf-bench/src`, which the JSON typed-parse bench binary compiles and runs. The
derived schema drives real emission in-wave.

**Invariant:** the generated `bbnf-bench` module is byte-identical to its committed
(hand-schema) form — zero runtime delta, the equality gate *is* the invariant proof.

---

### W4 — The Grammar-Driven Lane (§0 direct-to-struct numbers hold)

**Objective.** Certify that the direct-to-struct lane fed by *derived* schemas
reproduces PROVE-07's §0 numbers. Because W3 proved the emit is byte-identical, this is
a non-regression certification, not a new win — and that is the honest claim
(refutation guard 09).

**File-level plan:**

- `crates/bbnf-bench/src/` — no source change; the generated typed module is already
  derived-schema-sourced from W3. W4 adds the paired bench harness wiring if GN-1's canon
  did not already carry the typed lane.
- `crates/bbnf-bench/benches/typed_direct.rs` (+~120, or extends the ported canon) — the
  cold paired-PMU harness (§43-48): derived-schema parser vs `serde_json` typed and
  `sonic-rs` typed, N≥200, twitter/citm/canada, per-block loadavg recorded.
- oracle: `crates/bbnf-bench/tests/typed_parity.rs` (+~40 if absent) — derived-schema
  decode ≡ serde decode, field-for-field, across all three corpora.

**Seams:** PROVE-07 §0 targets — citm 1.85-1.92× vs serde, sonic-typed 0.746-0.786×
paired, twitter-vs-serde 2× honestly unmet (thin projection); the typed emitter @
`schema/json_typed_direct.rs:10`; corpus @ `real_typed_struct.rs`.

**Gate (born-verified).** Non-regression against the graduation baseline (§48) and the
§0 table:

```
cargo bench -p bbnf-bench --bench typed_direct   # cold, paired, N>=200
```

Threshold: derived-schema lane within ±3% ins/B of the hand-schema lane (the two emit
identical parsers, so the expected delta is 0 — any measurable delta refutes W3's
byte-identity); the §0 ratios (citm 1.85-1.92× vs serde, sonic-typed 0.746-0.786×)
reproduce with every window's CI in-direction. **Failure semantics:** a measurable
cyc/B delta between derived and hand lanes means W3's equality was not truly
byte-for-byte — the wave fails and reopens W3. A §0 ratio that does not reproduce is a
DIRECTIONAL flag re-measured under quiet-host (carried residual, §5), not a hard fail on
the ratio.

**Consumer in this wave (Y.13):** the typed-direct bench binary + the parity oracle —
both run the derived-schema-emitted parser over real corpora. The derivation's product
is exercised at runtime in-wave.

**Invariant:** the parser under bench is byte-identical to W3's; the type plane
contributed zero runtime instructions.

---

### W5 — The Re-Measure (challenge wave)

**Objective.** Adversarially re-measure and audit GN-3's own claims, non-author, fresh
windows. One challenge wave per tranche (§32). This wave hunts prototype residue the
FRAMING names as a defect: bench-only wiring, dormant kernels, hand-schema crutches,
feature-flag limbo.

**Audit surface:**

1. **Zero-runtime-cost, re-proven from the tranche base.**
   `git diff --name-only GN-3-base..GN-3-close | grep -E 'crates/runtime/'` — must be
   empty. Any runtime file refutes the invariant.
2. **Derived ≡ hand, re-diffed by a non-author** from the deleted file's git history:
   check out `real_typed_schema.rs@GN-3.W3~1`, re-emit, diff against the derived emit —
   independent reproduction of W3's byte-identity.
3. **No leaf-slice cheat.** Confirm the derived schema carries the full 46-struct/19-root
   taxonomy, not the 1-struct/4-leaf slice PROVE-08 could reach. `grep -c "struct_id\|
   DirectTypeSchema"` over the derived `DirectSchemaSet` == the hand count.
4. **No grammar-name gate** (§9, feeds GN-5.W3). `grep -rn '"twitter"\|"citm"\|"canada"\|
   "json"' crates/codegen/src/schema/type_plane/` — zero identity branches (grammar.name used
   only for module-string derivation, per PROVE-08/VERIFY attack 3).
5. **§0 direct-to-struct re-measure, cold paired PMU, fresh N≥200 windows**, non-author
   host — the citm/sonic/serde ratios independently reproduced.

**File-level plan:** `crates/codegen/tests/challenge_gn3.rs` (+~40) — the automatable
conjuncts (1-4) as a single fail-closed test; conjunct 5 is a bench re-run logged to the
tranche ledger.

**Gate (born-RED — a gate that cannot fail is a defect).**

```
cargo test -p codegen --profile ax-iter --test challenge_gn3
```

Threshold: all four automatable conjuncts pass; the manual re-measure (5) reproduces §0
within noise across ≥3 windows. **Failure semantics:** any conjunct failing refutes the
corresponding tranche claim and reopens the owning wave — the challenge wave has real
teeth precisely because it can send W1/W2/W3/W4 back.

**Consumer:** the tranche's own claim ledger; this wave gates GN-3 close.

---

## 5. LOC ledger

| wave | +new | −del | net |
|---|---|---|---|
| W1 | 420 | 0 | +420 |
| W2 | 280 | 45 | +235 |
| W3 | 260 | 1020 | −760 |
| W4 | 120 | 20 | +100 |
| W5 | 40 | 0 | +40 |
| **total** | **1120** | **1085** | **+35** |

The tranche is net-neutral: it *replaces* 1014 LOC of hand-authored document schema
with ~1120 LOC of grammar-driven inference machinery + typed grammars, and the balance
is the parsimony story — the schema stops being written and starts being derived, at
roughly zero net line cost. `generated_loc_budget` (§53): W3 regenerates the
`bbnf-bench` typed module byte-identically, so the generated line count does not move.

## 6. Exit criteria

1. `DirectTypeKind::Sum` exists, validates, and renders a well-formed Rust enum (W1).
2. `DirectBuildField.target` is populated from `Project` for every projected rule; the
   4-way `ProjectionKind` fold is deleted; sheets regen byte-identical (W2).
3. Typed grammars author the twitter/citm/canada shapes; `type_plane::derive` emits a
   schema byte-identically equal to the hand schema; `real_typed_schema.rs` is deleted
   and the build compiles (W3).
4. The direct-to-struct lane, fed by derived schemas, reproduces PROVE-07's §0 numbers
   cold/paired; parity oracle green (W4).
5. The challenge wave's five conjuncts reproduce, non-author (W5).
6. Invariant held on every wave: `crates/runtime/` untouched; the type plane cost zero
   runtime instructions.

## 7. Risks, with named counterexamples

- **R1 — capacity hints are corpus facts, not grammar facts.** The hand schema carries
  `vec_with_capacity(ty("ApacheJob"), 875)`, `vec_with_capacity(f64_ty(), 10_001)`,
  `vec_with_capacity(ty("UnicodeBasicRecord"), 5_759)`, and the nested `184` — 12 sites
  (verified). These numbers come from *counting a specific document*, not from grammar
  structure; W3's byte-identity gate fails on them unless the grammar carries them.
  **Resolution:** capacity is a performance hint, not a shape — the typed grammar carries
  it as an explicit `-> Vec<Tweet> @cap(875)` annotation colocated with the projection
  (the information legitimately moves into the grammar under Wall-1's cure), OR W3's
  equality is defined structurally-modulo-capacity with the hint re-injected from the
  annotation. The gate must state which; the default is the annotation (equality strict).
  This risk is the sharpest — it decides whether "byte-identical" is honestly reachable.
- **R2 — closed enums / unicode variant tables.** The hand schema has 17 StringEnum /
  variant references (e.g. CITM's fixed-key tables). The typed grammar must enumerate the
  closed variant set as `"A" -> A | "B" -> B` alternation. **Counterexample:** a field
  whose enum has dozens of variants bloats the grammar. **Resolution:** acceptable — the
  variant set is genuinely part of the document contract and belongs in the grammar; W1's
  sum node represents it; grammar length is not a gate.
- **R3 — GN-2 ∥ GN-3 both write `crates/passes/src/lib.rs`.** GN-2.W1 edits
  `derive_recognizers`; GN-3.W2 edits `infer_expr` (`:204`) and
  `derive_materialization_plan` (`:1149-1287`). **Counterexample:** both land uncommitted
  and collide at integration. **Resolution:** orchestrator serializes `passes/src/lib.rs`
  writes (§21/§22 — worktree isolation, cherry-pick not merge); GN-3.W2 rebases on GN-2's
  passes commits. Named in §2.
- **R4 — deleting the 4-way fold breaks sheets.** `classify_projection` is the sole
  active consumer of `->` on the general-descent (sheets) path. **Counterexample:** W2
  deletes it and sheets emission loses its `as_f64`/`as_bool`/`as_tag`/`span` accessor
  vocabulary. **Resolution:** W2's gate includes `check-sheets` byte-identity — the
  threaded leaf must reproduce sheets' generated output exactly, or the wave is RED. The
  coupling is the gate's teeth, not an afterthought.
- **R5 — sum unification order-sensitivity.** `object|array|string|number|bool|null` and
  a re-ordered alternation must unify to the *same* canonical sum, or derived ≠ hand.
  **Counterexample:** a grammar author writes the value alternation in a different order
  than the hand schema's variant list. **Resolution:** W1's `unify`-over-sums is
  set-equal (order-insensitive) with canonical variant ordering in `sum.rs`; the W1 test
  asserts two differently-ordered alternations unify identically.
