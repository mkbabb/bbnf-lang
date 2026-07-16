# LOCK-6 · Family 6A — MIRROR-TREE · pass-2 research (research-A)

The 6A thesis, developed alone to its strongest form: hand-maintained mirrored module
trees, idiomatic code per language, with a CI **isomorphism linter** that verifies tree
correspondence + a name map. This report grounds that thesis on the two live rust/ts
mirror surfaces already in-tree, converts the pass-2 steering questions into measured
answers, and states exactly what a prototype must fire.

---

## 0. The live substrate — two mirror pairs already exist (calibration corpus)

The plane is NOT greenfield. Two rust/ts correspondence surfaces are already committed
and gated, and they are the linter's calibration corpus.

**Pair A — the diagnostic-taxonomy mirror (`path!`).** A Rust proc-macro frontend and a
TS/wasm cdylib twin:

- Rust canonical: `bbnf::path::{PathError, PathErrorReason, PathSegment, TypedPath,
  check_path_against_registry}` (the `path::type_check` surface).
- Rust proc-macro: `crates/bbnf-path/src/{lib.rs, registry.rs, path_macro.rs}` — the
  `path!(Json, "statuses", 0, "text")` macro.
- Cdylib twin: `crates/bbnf-path-ts/src/{lib.rs, schema.rs, compile.rs, fixture.rs,
  template_tag.rs}` — `#[wasm_bindgen] compile_path/execute_path`.
- TS frontend: `crates/bbnf-path-ts/ts/{path.ts, index.ts}` — the `PathError` class +
  `makePathTag` template-tag.
- The gate today: `crates/bbnf-path-ts/tests/isomorphic_path_error.rs` — a **hand-rolled**
  field-for-field parity test tying the two surfaces (`feedback_isomorphic-api`).

**Pair B — the tape-layout mirror (F4/PROVE-14, graduating in GN-4.W4).** The offset-tape
SoA planes `crates/runtime/src/tape/mod.rs:126-137` (`source/offsets/flag_cursors/
flag_values`) ↔ the TS `TapeDoc` decoder (`ts/tape/src/tape.ts`, ex-`bbnf-bench/ts-tape/
tape.mjs`). Its gate is a **digest oracle** vs `JSON.parse`, not a name-map (PROVE-14
REPORT §"Byte-exact parity").

Pair A is a *declaration/taxonomy* mirror; Pair B is a *layout/behaviour* mirror. This
split is the load-bearing finding of the whole report (§5): the two demand different
checkers, and 6A owns only the first.

---

## 1. Q2 — the identity fork is a TRICHOTOMY, not a dichotomy

The directive frames the fork as (a) hand-authored-twice (real value, not grammar-anchored)
vs (b) grammar-derived (anchored, but linter compares a generator to itself → tautological).
Traced against Pair A, the correspondence surface splits into **three** disjoint classes,
each with a *different* tautology status and a *different* lawful check:

### Class 1 — grammar-derived names (registry field/rule names): ONE source, tautological to name-check

The field names `"statuses"/"text"/"retweets"/"id"/"sensitive"` and rule names
`Document/Status` are **not** authored on either side. They are projected by the IR
`project_types` pass and serialized by `cargo xtask regen` to
`crates/core/src/grammar/generated/<ident>.registry.json`. Both frontends read the **same
sidecar**:

- Rust proc-macro: `crates/bbnf-path/src/registry.rs:5-9,143-183` (`std::fs::read` of
  `<ident>.registry.json`, deserialized to `bbnf_ir::registry::StructRegistry`).
- Cdylib twin: `crates/bbnf-path-ts/src/fixture.rs` consumes the same `StructRegistry`
  shape (`bbnf-path-ts/src/lib.rs:42-47` documents the shared source; T4 note
  `registry.rs:1-30`).

Name-checking these across the boundary is exactly the directive's tautology: both names
descend from one generator. **Decision: the linter must NOT name-check Class 1.** Instead
it asserts **PROVENANCE** — that both sides consume the identical sidecar path/hash and
neither forks a hand copy. This check is anchored *and* non-tautological, because its
proposition is "single-source consumption held," not "name_A == name_B." (Historically the
proc-macro *did* carry a hand fixture — `bbnf-path/src/lib.rs:31-37`, `registry.rs:1-9`
"AUDIT-F §R1" — and T4 swapped it to the sidecar precisely to kill the fork. The linter
codifies that AUDIT-F verdict as a standing gate.)

### Class 2 — hand-authored-twice taxonomy (the diagnostic ADT + rendering): the non-tautological surface, and where drift LIVES

`PathErrorReason`'s 8 variants and `PathError`'s 5 fields are hand-authored on **three**
homes, none grammar-derived:

- Rust canonical `bbnf::path::PathErrorReason` (8 variants).
- Rust mirror `bbnf_path_ts::PathErrorReasonPayload` / `PathErrorPayload`
  (`schema.rs:58-93`).
- TS mirror `path.ts:37-45` (union) + `path.ts:62-101` (`PathError` class, `reasonText`,
  `format`).

This is the directive's branch (a): real value, NOT grammar-anchored, and the **only**
class where a name-map linter is non-tautological. **This surface has already drifted** —
the concrete counterexample the linter exists to catch:

> `isomorphic_path_error.rs:129-138`: for `$.statuses[0].nope`, the canonical offline
> checker leaves `TypeDesc::Named(_)` intact and surfaces **`UnregisteredRule`**; the
> cdylib resolves `Named(_)` eagerly through `layout(_)` and surfaces **`UnknownField`**
> for the same input. The isomorphism test **pins only the surface where both agree** and
> documents the divergence rather than fail.

A second live asymmetry: `unregistered_grammar_marker_surfaces_unregistered_rule`
(`:210-225`) notes "the Rust frontend does not have a 1:1 surface for this case," tested
TS-side only. And the taxonomy has a lifetime-shape divergence
(`Vec<&'static str>` ↔ `Vec<String>`, `schema.rs:76-79`).

Decisive nuance for the fork: even in Class 2 the **name-map is green while behaviour
diverged** — the 8 variant *names* correspond perfectly, but the *routing logic* that
picks a variant forked. So Class-2 correspondence is real value **for the declaration set
only**; it provably does NOT cover the logic that consumes those declarations.

### Class 3 — idiom residue (no counterpart): the exemption set

Transport/idiom declarations with no cross-language twin: `#[wasm_bindgen] compile_path/
execute_path`, `init()` panic hook, `TEMPLATE_TAG_JS`, `compile_path_native` (test shim)
on the Rust side; `makePathTag` (JS template-literal tag, `path.ts:114-142`),
`PathError extends Error` on the TS side. In Pair B / parse-that: the V8 monomorphizations
(`Object.create` view `parser.ts:177-180`, arity-unrolled arms `leaf.ts:39-57,183-251`),
and the regex boundary (native `RegExp`, no bespoke TS engine — R4 §3).

**Answer to Q2.** Which is it, (a) or (b)? *Both, on disjoint sub-surfaces, and the linter
must route by class:* Class 1 → provenance-check (single-source, anchored, non-tautological);
Class 2 → name-map-check (hand-authored, the genuine correspondence value); Class 3 →
exemption budget (§2). The surface that "stays non-tautological" is Class 2 alone — and it
is exactly the surface the current hand-rolled `reason_variants_are_isomorphic` table
(`:227-268`) checks by hand for Rust↔Rust, while leaving the **TS union + `reasonText`
switch entirely ungated** (a live coverage gap the linter closes).

---

## 2. Q1 — the RED-capable exemption ceiling: an impedance-derived cap, not a raw fraction

The directive's challenge: a budget that only "warns as the fraction grows" cannot fail;
supply a number that makes it a gate that goes RED, or show why none is defensible.

### Measured fraction on the live pair (the calibration)

Pair A correspondence-surface census (public declarations that have, or should have, a
cross-boundary counterpart):

| Class | declarations | example |
|---|---|---|
| 1 grammar-derived (provenance) | registry: 2 rules × ~2-3 fields + markers ×4 | `"statuses"`, `Json→json` |
| 2 hand-authored-twice (name-map) | `PathErrorReason` 8 + `PathError` 5 fields + `OwnedSegment` 4 + `TypedPath` 2 ≈ **19** | `FieldOnScalar` |
| 3 idiom residue (exempt) | ~**6** (`makePathTag`, `init`, `TEMPLATE_TAG_JS`, 2 wasm exports, native shim) | `makePathTag` |

Raw exemption fraction ≈ 6 / (19+6) ≈ **24%** of the *mappable* surface. A single global
"≤25%" ceiling is the naive gate — and the directive is right that it is weak: every new
PR can plausibly claim its addition is "legitimate idiom," so the fraction drifts up under
individually-defensible steps and the gate never fires. A raw fraction is a warning, not a
gate.

### The defensible RED line — three conjuncts, one of which is a derived cap

A budget that can actually go RED needs a **0%-tolerance core + a monotonicity gate + an
impedance-bounded cap**:

1. **Mapped-set: 0% tolerance (the hard RED).** Every Class-2 declaration must have its
   counterpart present and name-corresponding. One missing/renamed/added-on-one-side
   Class-2 declaration → RED. This is the conjunct that fires: it is falsifiable by adding
   a 9th `PathErrorReason` variant to the Rust enum without touching `path.ts` — the linter
   goes RED immediately (whereas today the ungated TS union silently rots).

2. **Exemption allow-list monotonicity (the creep RED).** Class-3 exemptions live on an
   explicit allow-list (one entry, one-line justification, tagged at a declared seam). A
   Class-3 declaration NOT on the list → RED; a list entry whose declaration no longer
   exists → RED. This defeats "warns as it grows": growth is only legal via an explicit
   reviewed diff to the allow-list, never silently.

3. **The impedance-derived cap (the number).** The allow-list size is capped, but the cap
   is **not a magic percent** — it is *derived* from the enumerable cross-language impedance
   surface, so it is "measured, not asserted" (Constitution iii):

   ```
   MAX_EXEMPTIONS(crate) = 2 · (#transport-entry points)      // wasm-bindgen ↔ template-tag pairs
                         + 1 · (#documented-idiom classes)     // V8 monomorph / RegExp / lifetime-shape
   ```

   For `bbnf-path-ts`: 2 exports × 2 (each needs a Rust `#[wasm_bindgen]` + a TS wrapper) +
   ~2 idiom classes = **6** — the observed count. Exceed it → RED. Because the multiplier is
   pinned to grep-countable seams (`#[wasm_bindgen]` attrs; the `makePathTag`/`init`
   idiom-tag), it cannot be inflated by re-labelling ordinary types as "idiom."

**The number, stated plainly (Q1):** the exemption-*fraction* ceiling is a directional
calibration (~25% on the live pair); the *gate* that goes RED is `#exemptions ≤ 2·T + I`
where `T` = transport entry-points and `I` = declared idiom classes — a **per-crate integer
cap derived from the impedance surface**, conjoined with 0% tolerance on the mapped set and
allow-list monotonicity. A pure fraction is rejected as un-RED-able for exactly the reason
the directive names; the impedance cap is defensible because it is enumerable at named
seams and independently auditable.

---

## 3. The mechanism made concrete (6A at its strongest)

**Inputs.** (i) Rust declaration surface via `syn` over the mirror crates' `src/**.rs`;
(ii) TS declaration surface via the **already-built** `dist/*.d.ts` (R4 §1) or the
`crates/*/ts/*.ts` sources parsed with the TS compiler API — declarations only, never
bodies; (iii) the grammar sidecar manifest (`<ident>.registry.json` paths + content hash);
(iv) a checked-in `mirror-map.toml` naming the correspondence + the exemption allow-list.

**Three passes, one per Q2 class:**

- **Provenance pass (Class 1).** Assert every registry-name consumer reads the same sidecar
  hash; flag any hand-authored `StructRegistry`/field literal on either side (grep-gate,
  the AUDIT-F verdict as CI). NO name comparison (tautology avoided).
- **Name-map pass (Class 2).** For each declared correspondence in `mirror-map.toml`,
  verify: type exists both sides; enum-variant sets are set-equal (order-free); struct-field
  names set-equal; the TS `reasonText`/`format` switch is exhaustive over the union (closes
  the ungated TS gap). 0% tolerance → RED on any mismatch. Subsumes and regenerates the
  hand-rolled `reason_variants_are_isomorphic` table.
- **Budget pass (Class 3).** Enforce §2's monotonicity + impedance cap.

**Output.** A machine diff + a RED/GREEN verdict; on GREEN, it *emits* the parity table the
isomorphism test currently hand-maintains (so the test shrinks to a generated fixture —
Constitution vi parsimony).

**Substrate + consumer (Constitution viii).** The consumer is not hypothetical: it is
`isomorphic_path_error.rs` itself, which the linter subsumes, plus the TS union/`reasonText`
surface that has **no gate today**. Substrate (the `mirror-map.toml` + linter) lands with
its consumer (the CI gate replacing the hand table) in the same wave.

---

## 4. Cross-lock adjudications (proposals, not decrees)

**Emitter-derived name map — which lock owns the grammar-derived half?** LOCK-3 (IR
transposition spine) owns it: the sidecar is emitted by `project_types` → `cargo xtask
regen` (`registry.rs:1-30`). The isomorphism plane owns only the **provenance assertion**
over LOCK-3's output, never the names themselves. Proposal: Class-1 names are LOCK-3's
product; 6A's contribution is the single-source gate, not a second name authority. This
dissolves the "unnamed in the specs" ambiguity — there is no grammar-derived *name map* to
own, only a *provenance invariant*.

**Regex exemption (Rust-DFA ↔ TS `RegExp`) vs a LOCK-5 ONE-HIR verdict.** The TS runtime
has **no** NFA/DFA/Thompson engine — it drives native sticky `RegExp` (`leaf.ts:317-369`,
R4 §3). A byte/layout mirror is therefore *impossible in principle*, so this seam cannot be
a mirror-tree correspondence at all; it is inherently an **oracle-isomorphism** seam
(same accept/reject, layout free). Proposal owed to LOCK-5: a ONE-HIR verdict that forbids
a permanent Rust-DFA↔RegExp *layout* mirror is compatible with 6A, because 6A never claims
that seam — it routes it to behavioural checking and records `RegExp` as a declared idiom
exemption (Class 3). The allow-list must NOT freeze a *layout* mirror here before LOCK-5
rules; it may freeze only the *behavioural* correspondence.

**Freeze-ordering DAG.** The name-map's alphabet (the `PathError`/`Value` ADT variant set)
is LOCK-1's grammar-derived Value ADT; the reference-of-last-resort for a contested
correspondence is LOCK-7's meta-circular oracle. Constraint: **this plane freezes AFTER
LOCK-1 (the ADT the map ranges over must be fixed) AND AFTER LOCK-7 (the tie-break oracle
must exist).** 6A's `mirror-map.toml` cannot be authored until LOCK-1 fixes the variant
alphabet, else the map ranges over a moving set.

**Q4 (sharpest) — does ts-rs/typeshare conditional-type FLATTENING bear on layout-generation?**
Two distinct claims, adjudicated separately (owed to LOCK-4 / LOCK-8):

- On **type-generation** (Rust type → TS type): YES it bears. ts-rs/specta/typeshare
  document that generics + `flatten`/`inline` + trait bounds lose fidelity (ts-rs #214;
  specta #386; typeshare's static-analysis info loss). A 6B "generate the TS *types* from
  the grammar" face inherits these limits — this is the LOCK-1/LOCK-8 type-plane concern.
- On **layout-generation** (a shared instruction / tape-layout stream, LOCK-4C VM /
  LOCK-8C): NO it does not bear. A byte-layout or opcode stream has no generic
  instantiation and no conditional type to flatten — it is flat SoA/bytecode. PROVE-14
  proves this *concretely*: the offset-tape (`offsets:u32 / flag_cursors:u32 /
  flag_values:u8 / source`) crosses **byte-exactly** to TS typed arrays with **zero
  type-projection machinery** (REPORT §"The layout that crossed"; 320 cold children,
  fingerprint `986912547` identical). Layout is data, not type; the flattening limitation
  is a red herring for the layout stream.

  **Adjudication:** they are distinct claims. The conditional-type flattening risk binds
  6B-for-types (LOCK-1/8 type plane) and must be priced there; it does NOT bind
  6B/6C-for-layout (LOCK-4C/8C shared-runtime stream), which PROVE-14 shows is
  isomorphic-by-construction independent of any type generator. A pass-2 synthesis must not
  let a type-generation caveat contaminate the layout-generation case.

---

## 5. The domain boundary 6A must claim (where it earns its keep vs over-builds)

The two live pairs prove a clean partition of the isomorphism problem:

- **Declaration/taxonomy surface (Pair A):** enum/field/type names hand-authored on both
  sides. Checkable statically, cheaply, by a name-map linter. **6A's domain.** It earns its
  keep here because the TS union + `reasonText` are ungated today and the taxonomy has
  *already* forked in logic while names stayed aligned.
- **Layout/behaviour surface (Pair B):** byte-tape + validation logic. NOT statically
  name-mappable; the `$.statuses[0].nope` drift is in *routing logic*, invisible to any
  name-map (names were green). Must be an **oracle/differential** check (digest parity,
  PROVE-14; the four-way `value_parity.rs`, GN-4.W5). **NOT 6A's domain** — over-building a
  mirror-tree linter to chase behaviour is where 6A over-reaches.

So 6A at its strongest is *narrow by design*: it owns Class-1 provenance + Class-2 name-map
+ Class-3 budget over the **declaration surface**, and explicitly **cedes logic/layout
correspondence to the oracle family**. A mirror-tree linter that claims to cover behaviour
gives false assurance — the live counterexample is proof.

---

## 6. Named risks / counterexamples

- **R1 — tautology false-green (the fork's trap).** Name-checking Class-1 registry fields
  passes trivially (one source) while real divergence hides in logic. Live counterexample:
  the `$.statuses[0].nope` `UnregisteredRule`↔`UnknownField` split — name-map green,
  behaviour red. Mitigation: route Class 1 to provenance, and *declare non-coverage of
  logic* so no reader mistakes a green mirror for a behavioural guarantee.
- **R2 — residue creep (the un-RED budget).** "Warns as it grows" never fails. Mitigation:
  §2's 0%-tolerance mapped set + monotone allow-list + impedance cap.
- **R3 — impedance-count gaming.** Re-labelling ordinary types as "transport idiom" to lift
  the cap. Mitigation: the `2·T` multiplier is pinned to grep-countable `#[wasm_bindgen]`
  seams; idiom classes are an enumerated closed set (V8-monomorph, RegExp, lifetime-shape),
  extended only by reviewed allow-list diff.
- **R4 — the linter is itself an un-mirrored god-tool.** A Rust CI tool parsing TS needs a
  TS AST. Mitigation: consume built `dist/*.d.ts` (declarations only, already produced by
  `vite build`, R4 §1) — no runtime TS toolchain in the Rust gate; the parser touches
  signatures, never bodies (bodies are Constitution-blessed idiom).
- **R5 — freeze-before-alphabet.** Authoring `mirror-map.toml` before LOCK-1 fixes the
  Value/PathError ADT ranges the map over a moving set → churn. Mitigation: the DAG
  constraint (§4) — freeze after LOCK-1 + LOCK-7.

---

## 7. What a prototype MUST test to move family 6A

1. **RED-fires-on-divergence (the core falsifier).** Seed a 9th `PathErrorReason` variant
   on the Rust enum only; the name-map pass must go RED (proving it covers the currently-
   ungated TS union). Seed an un-registered hand type; the budget pass must go RED.
2. **Tautology avoidance.** Confirm the provenance pass SKIPS registry field-name
   correspondence (both read `<ident>.registry.json`) yet FLAGS a hand-authored
   `StructRegistry` literal — i.e. it checks single-source, not names.
3. **Impedance cap calibration.** Compute `#exemptions` and `2·T + I` on BOTH live pairs
   (`bbnf-path-ts`; the GN-4.W4 `ts/tape` ↔ `runtime/tape`) and confirm the cap ≈ observed
   (≈6 for path; the tape pair's `T` = 1 wasm export). If observed ≫ derived, the cap
   formula is wrong — a negative result to report.
4. **Non-coverage honesty.** Demonstrate the linter is GREEN on the live
   `$.statuses[0].nope` logic divergence (names correspond) — and that this is *documented
   as out-of-contract*, routed to the oracle family, not silently blessed.
5. **Cost.** Linter wall-time in CI + false-positive rate over Class-3 idiom; the
   `dist/.d.ts` parse path must not require a TS runtime in the Rust gate.

---

### Provenance of claims (path:line)
Registry single-source: `crates/bbnf-path/src/registry.rs:5-9,90-98,143-183`,
`crates/bbnf-path-ts/src/lib.rs:42-47`, `crates/bbnf-path/src/lib.rs:31-37`.
Hand-authored taxonomy (3 homes): `bbnf-path-ts/src/schema.rs:58-93`,
`crates/bbnf-path-ts/ts/path.ts:37-45,62-101`.
Live drift: `crates/bbnf-path-ts/tests/isomorphic_path_error.rs:129-138,210-225`;
hand parity table `:227-268`; lifetime-shape divergence `schema.rs:76-79`.
Idiom residue: `ts/path.ts:114-142`, `bbnf-path-ts/src/lib.rs:86-133`.
Layout mirror + byte-exactness: `crates/runtime/src/tape/mod.rs:126-137`, PROVE-14 REPORT
(fingerprint `986912547`, 320 cold children). TS RegExp-only: R4 §3 (`leaf.ts:317-369`).
Prior art: ts-rs #214 / specta #386 (generics+flatten fidelity loss); typeshare static-
analysis info loss; typebridge / drift / blockwatch (cross-language drift linters, CI-anchored).
