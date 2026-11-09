# LOCK-6 · Family 6A — MIRROR-TREE · Pass-3 research

Hand-maintained mirrored module trees, idiomatic bodies per language, and a CI
**isomorphism linter** that (i) verifies tree correspondence, (ii) checks a
name-map from a single canonical source, and (iii) caps deliberate divergence by a
**typed exemption budget** whose slack is a function of declared idiom classes, not
of the observed divergence total. The mechanism is developed on the one live rust/ts
pair already in-tree — the `bbnf-path` proc-macro (Rust frontend) and the
`bbnf-path-ts` cdylib + `ts/path.ts` template-tag (TS frontend) — and driven
behaviorally against canonical Rust (§5, ran this pass: 5/5 green).

---

## 1. Substrate — the one live pair (concrete, in-tree)

The greenfield already carries a working mirror pair; 6A is the discipline that keeps
it honest, not a net-new subsystem.

- **Canonical Rust names** live in `crates/core/src/path/`:
  `PathErrorReason` (`error.rs:44`, 8 variants), `PathError` (`error.rs:90`),
  `TypedPath<G,T>` (`ir.rs:146`), `OwnedPathSegment` (`ir.rs:162`),
  `PathSegment<'a>` (`ir.rs:42`), the `Display` impls (`error.rs:66,129`,
  format string `error.rs:133` = `path[{}] \`{}\` on \`{}\`: {}`).
- **Rust proc-macro frontend** `crates/bbnf-path/src/{lib.rs,path_macro.rs,registry.rs}`
  — lifts `path!(GrammarMarker, "seg", 0, …)` at compile time
  (`lib.rs:73`, expansion documented `lib.rs:58-66`).
- **TS frontend twin**: `crates/bbnf-path-ts/` — a cdylib mirroring the macro at TS
  load/build time. Serde-transport payloads in `src/schema.rs`
  (`OwnedSegmentPayload:26`, `TypedPathPayload:45`, `PathErrorReasonPayload:59`,
  `PathErrorPayload:81`); wasm exports `src/lib.rs:106,122`; native oracle entries
  `compile_path_native`/`execute_path_native` (`lib.rs:69,77`); the TS surface
  `ts/path.ts` (interfaces `:21,31,37,52`; `PathError extends Error` class `:62`; the
  `path\`…\`` template tag `makePathTag` `:114`).
- **The isomorphism is already a checked-in test**:
  `crates/bbnf-path-ts/tests/isomorphic_path_error.rs` (268 LOC) drives *both*
  surfaces against one `StructRegistry` fixture and compares
  `(segment_index, segment_str, struct_name, alternatives, reason)` field-for-field
  (`:148-165`), plus an exhaustive reason-variant pairing (`reason_variants_are_isomorphic`,
  `:228-268`) that fails if either side adds a variant.

This pair *is* MIRROR-TREE in miniature: tree correspondence (Rust `path/` ↔ TS
`path.ts`), a name map (every `*Payload` doc-cites its `bbnf::path::*` twin), and
three already-documented divergences that a budget must license rather than forbid.

---

## 2. Q1 — the exemption budget: grep-countable T and I, an unfitted cap

**Problem restated.** A raw allowlist of "here the mirror is broken" rots: entries
accrete, each individually defensible, until the trees have silently diverged. The
budget must (a) be computed from inputs *independent of the divergence total*, (b)
reproduce the live count without being fit to it, (c) go RED on a synthetic extra
exemption, (d) forbid growth at a defended count.

### The two inputs, each by a grep-countable rule

- **T = structural mirrors** = the count of canonical names carrying a declared twin.
  Grep rule (independent of any exemption): number of `/// Mirrors \`bbnf::path::…\``
  (or `Mirror`) annotations attached to a `pub struct`/`pub enum`/`export
  interface`/`export type`/`export class` declaration.
  On the live pair: 4 payload types (`schema.rs:26,45,59,81`) ↔ 5 TS declarations
  (`path.ts:21,31,37,52,62`; TS splits `PathError` into a data interface + an `Error`
  subclass), plus 8 reason variants and 4 segment kinds enumerated on each side.
  **T is the denominator of coverage — never a source of slack.**

- **I = idiom classes** = the count of *categories* of lawful per-language deviation,
  declared as a **closed enum in a checked-in manifest** (`idiom_classes.toml` or an
  `enum IdiomClass`), grep-counted as its member lines. On the live pair the classes
  are enumerable directly from the code:

  | # | idiom class | evidence in the pair |
  |---|---|---|
  | 1 | **owned-vs-borrowed** | `Vec<&'static str>` → `Vec<String>`/`string[]` (`schema.rs:77`, test `:15-17`); `PathSegment::Field(&str)` → `OwnedPathSegment::Field(String)` |
  | 2 | **transport-twin suffix** | the `*Payload` serde mirror existing at all (`schema.rs` module doc `:1-15`) — a second Rust type per canonical type |
  | 3 | **enum encoding** | Rust nominal enum → TS discriminated union `{kind,value}` (`OwnedPathSegment`, `path.ts:21`) / string-literal union (`PathErrorReason`, `path.ts:37`) |
  | 4 | **error protocol** | Rust `Result<_,PathError>` + `Display` → TS `throw new PathError` subclass + `format()` (`path.ts:62,81`) |
  | 5 | **construction time** | proc-macro `path!` (`bbnf-path/lib.rs:73`) → template-tag `path\`…\`` (`path.ts:114`) |
  | 6 | **numeric width** | `usize` → `number` (`segment_index`, `Index` payloads) |

  **I = 6** on this pair, grep-countable and **fixed before the divergences are
  counted** — its members are architectural facts about the Rust/TS gap (lifetimes,
  serde, structural typing, exceptions, macro-vs-runtime, number type), not a fit to
  what happens to be exempted today.

### The cap and its self-defense

**cap = |I| (one *structural* exemption slot per idiom class), and every exemption
entry MUST cite exactly one class from the closed set I.** The linter is RED iff:

1. an exemption cites no class, or a class not in I (**unclassified divergence**), OR
2. two exemptions cite the same class at the same mirror site with different
   canonical names (**a class cannot license open-ended drift**), OR
3. a structural mirror in T has a live field/variant present on one side and absent on
   the other with **no** exemption entry (**undeclared divergence**).

- **Reproduces the live count without fitting.** The three documented divergences on
  the pair — `alternatives` owned-string (class 1), the `Named`-descent
  `UnregisteredRule`-vs-`UnknownField` resolution asymmetry
  (`isomorphic_path_error.rs:129-138`, class 4/behavioral), and the TS-only
  `unregistered_grammar_marker` surface (`:210-224`, class 4) — each map to a distinct
  declared class; the count falls out of I, it was not tuned to 3.
- **RED on a synthetic extra exemption.** Add a 7th divergence (e.g. drop
  `struct_name` from the TS payload "for ergonomics") with no new idiom class → it
  cites an existing class already spent, or none → rule 1/2 fires RED.
- **Forbids growth at a defended count.** To *legitimately* add an exemption you must
  enlarge I — a reviewed edit to the closed manifest, an audit event, not a silent
  allowlist append. The cap moves only when the *category* of gap moves. Holding I
  fixed, the allowlist cannot grow.

This is the LLVM/`Changed`-flag discipline applied to drift: the budget is a typed
invariant (`every divergence is classified, every class is a reviewed constant`), not
a magic integer, so it cannot be gamed by re-tuning a threshold
(cf. `feedback_aesthetics_critical`, `feedback_pluggable_components` — the class set is
the pluggable decision point, not a hardcoded number).

---

## 3. Q2 — provenance traces for two spine names

**Name A — `PathErrorReason` (and its 8 variants): authored independently on each
side, gated by a test, NOT generated from one source.**
Trace: canonical `enum PathErrorReason` at `crates/core/src/path/error.rs:44`
→ hand-written Rust transport twin `PathErrorReasonPayload`
(`bbnf-path-ts/src/schema.rs:59`, variants retyped by hand) → hand-written TS
string-literal union `PathErrorReason` (`ts/path.ts:37`) → hand-written TS
`reasonText()` message map (`path.ts:90-101`) mirroring the Rust `Display for
PathErrorReason` (`error.rs:66`). There is **no generator**; the seam that keeps the
three copies aligned is purely the exhaustive match in
`isomorphic_path_error.rs:89-100` + `reason_variants_are_isomorphic` (`:228-268`),
which refuses to pass if any of the three drifts. This is precisely the name class
MIRROR-TREE's name-map linter must own: a compile-time-total match today, a
CI-checked name map tomorrow.

**Name B — `struct_name` / field names (`statuses`, `text`, `retweets`): single
grammar-derived source (the registry), hand-fixture only as scaffold.**
Trace: field names originate in the grammar → `StructRegistry` /
`StructField`/`FieldSource` in `crates/ir/src/registry/` (LOCK-3 tower), built by
`cargo xtask regen`. Both frontends consume that one source: the proc-macro walks it
(`bbnf-path/lib.rs:16-22`), the cdylib validates against it. **Today** each frontend
carries a hand *fixture* stand-in (`bbnf-path/src/registry.rs`,
`bbnf-path-ts/src/fixture.rs` — same shape, markers `Bbnf/CssL4/Json/Sheets` at
`fixture.rs:32-42`) because `pub const REGISTRY: StructRegistry` is not yet emitted;
`bbnf-path-ts/src/lib.rs:44-47` names the swap explicitly ("When T4 lands per-grammar
`pub const REGISTRY`… both frontends swap their fixture lookup for the production
const"). So field names are the **single-grammar-derived** provenance class (the
mirror is a shared *upstream*, not two authored copies), whereas the taxonomy names in
Name A are the **dual-authored-under-test** class. The name-map linter must
distinguish these two provenance kinds — one verified against the registry, the other
against its twin.

---

## 4. Q3 — who owns the provenance / name-map linter

**Owner: LOCK-3 (the IR transposition spine), because the ground truth for
grammar-derived names is the `StructRegistry` it emits.** Rationale against the XL-B
tower frame:

- The registry that names structs/fields/variants is `crates/ir/src/registry/`
  (`mod.rs`, `struct.rs`, `strategy.rs`) — inside the middle-IR tower LOCK-3 owns
  (XL-B, XL-F: `TypeAnalysis::from_ir`). A name-map linter for **grammar-derived**
  names (Name B class) is a *tower output check*: it asserts every consumer
  (proc-macro, cdylib, generated Rust, generated/hand TS) draws field/variant names
  from the one `pub const REGISTRY` and none re-authors them. Homing it elsewhere would
  duplicate the tower's authority over names.
- LOCK-6 (this lock) owns the **structural/idiom half** — the T/I budget of §2, tree
  correspondence, and the *dual-authored-under-test* names (Name A class: the
  `PathError` taxonomy, a diagnostic contract, not a grammar product). LOCK-6 consumes
  LOCK-3's registry-name verdict and layers the exemption budget on top.
- **Proposed split, single linter, two rule-sets:** one CI tool (`xtask iso-lint`) run
  at regen, with (a) a **provenance rule** owned by LOCK-3 — "grammar-derived name ⇒
  sourced from `REGISTRY`, zero re-authoring, grep-gated like GN-4's
  no-grammar-name-predicate gate" — and (b) a **structural-mirror rule** owned by
  LOCK-6 — the T/I exemption budget. This honors XL-B ("LOCK-3 owns the middle-IR tower
  + decision-driver seat") without minting an orthogonal subsystem
  (`feedback_system_cohesion`): the linter is one home, its provenance half sits with
  the tower that defines provenance.

---

## 5. Behavioral evidence (the directive: drive TS against canonical Rust)

The in-tree oracle `crates/bbnf-path-ts/tests/isomorphic_path_error.rs` *is* the
behavioral drive: `compile_path_native("$.nope","Json")` (the TS/cdylib path) is
compared field-for-field to
`check_path_against_registry::<bbnf::path::Json,()>(…)` (canonical Rust) over the same
`StructRegistry` fixture (`:139-165`), and again for `FieldOnScalar` (`:168-193`) and
`EmptyPath` (`:196-208`). This is a genuine cross-surface behavioral comparison, not an
intra-Rust one — the cdylib runs its own lex/lower/validate
(`bbnf-path-ts/src/compile.rs`), mirrored at function scope per the proc-macro-crate
constraint (`lib.rs:38-47`).
**Ran it this pass:** `cargo test -p bbnf-path-ts --test isomorphic_path_error` →
**5 passed, 0 failed** — the TS/cdylib surface reproduces canonical Rust's diagnostic
taxonomy field-for-field over the live registry today.

**A latent drift the current linter would miss (counterexample seed):** the rendered
message. Rust `Display` prints alternatives via `{:?}` → `; valid: ["a", "b"]`
(`error.rs:137`); TS `format()` builds `; valid: [${alts}]` with hand quoting
(`path.ts:82-87`). Field-for-field name equality passes while the *rendered string*
can diverge (quote style, escaping). A name-map linter that checks names but not the
render contract is blind here — so the budget's error-protocol idiom class (I#4) must
carry a rendered-string golden, not just a name map.

---

## 6. Prior art (citations)

- **rustc `#[rustc_layout]` / `-Zprint-type-sizes`** and **cbindgen** — machine-derive
  a foreign-language view from one canonical Rust type; the anti-pattern MIRROR-TREE
  deliberately rejects (it keeps bodies idiomatic, mirrors only names/shape). cbindgen
  is the "generate the twin" counterfactual 6B explores; 6A's thesis is that
  *hand-mirrored + linted* beats *generated* where bodies must stay idiomatic.
- **serde `#[serde(tag=…, content=…)]`** (`schema.rs:25`) — the adjacently tagged
  encoding that makes the Rust enum ↔ TS discriminated-union idiom (class 3) byte-stable
  across `serde-wasm-bindgen`. Prior art for the transport-twin idiom.
- **Rust API-evolution lints / `cargo-semver-checks`** — a name/shape diff gated in CI;
  direct prior art for a name-map linter that RED-flags an added/removed variant
  (exactly `reason_variants_are_isomorphic`'s job, promoted to a tool).
- **Bazel/Buck "source of truth + generated mirror" + `buildifier` allowlists** and
  **Chromium `PRESUBMIT` per-category suppression files** — prior art for a *typed*
  exemption budget (each suppression cites a category), the closed-set discipline §2
  borrows.
- **tree-sitter `grammar.js` → generated `parser.c` + hand `scanner.c`** — the exact
  "generated skeleton, hand idiomatic body" split; the hand `scanner.c` is the analog of
  the per-language idiomatic bodies MIRROR-TREE preserves.
- **MLIR ODS / LLVM `.td`** — one declarative record generates C++ getters *and* docs;
  the counterfactual where the name-map is a generation artifact rather than a lint.
  Relevant to the Q3 "who owns names" split.

---

## 7. Risks & named counterexamples

- **R1 — I is gamed by lumping.** If "everything TS-ergonomic" is one idiom class, the
  cap is trivially large. Mitigation: I's members must each name a *language-level*
  cause (lifetime, serde, structural typing, exception, macro-timing, number type) —
  reviewable as a real Rust/TS gap; a catch-all class is a RED audit finding.
- **R2 — name-map linter checks names, misses semantics.** §5's render-divergence and
  the `Named`-descent resolution asymmetry (`:129-138`) show equal names can hide
  unequal behavior. Mitigation: each idiom class ships its own oracle (golden render,
  differential path result), so behavioral isomorphism (the 6D concern) becomes a
  *per-class obligation* of the budget, not an afterthought.
- **R3 — fixture/registry double-home rots (Name B).** Two hand fixtures
  (`registry.rs`, `fixture.rs`) already exist as scaffolding; if `pub const REGISTRY`
  slips they diverge silently. Mitigation: the LOCK-3 provenance rule must grep-gate
  that no field name is authored outside `REGISTRY` once T4 lands (`lib.rs:44-47`);
  until then the two fixtures are themselves a T-mirror the linter must pin.
- **R4 — proc-macro-crate constraint forces logic duplication.** `bbnf-path` is
  `proc-macro=true` and cannot be a lib path-dep (`lib.rs:38-47`), so compile logic is
  mirrored at function scope in the cdylib — a *behavioral* mirror the name-map linter
  does not cover. Counterexample: the two lexers accept different path syntaxes and no
  name drifts. Mitigation: a shared `bbnf_regex::lex_path` (already the intended common
  lexer, `lib.rs:16-18`) reduces this to one source; the linter pins the shared-lexer
  call on both sides.

---

## 8. What a prototype must test to move 6A

1. **Wire `xtask iso-lint` against the live pair** and show the T/I budget: T and I
   grep-counted from source, cap = |I| = 6, live exemptions = 3 each classified;
   demonstrate RED on an injected 7th unclassified divergence and RED on an added TS
   field with no exemption entry (the §2 (b)(c) obligations, as running CLI output).
2. **Provenance rule dry-run**: grep-gate that every field name in both frontends
   traces to the fixture/registry shape; inject a hand-authored `"statuses"` literal in
   the cdylib and show the LOCK-3 rule flags it.
3. **Per-class oracle for idiom #4**: add the rendered-string golden and prove it
   catches the `{:?}`-vs-hand-quote render divergence (§5) that field equality misses.
4. **Cost measurement (the real question 6A vs 6B/6D must settle)**: hand-maintenance
   cost of the mirror under one *added* variant — add a 9th `PathErrorReason` variant
   and count the edit sites (canonical enum, `Display`, payload enum, TS union,
   `reasonText`, the test's two match tables) the linter forces green. That edit-site
   count, measured, is the number that decides MIRROR-TREE (hand+lint) against
   SPEC-GENERATED-SKELETON (6B) and ORACLE-ISOMORPHISM (6D). 6A's claim: the count is
   small and the lint makes it *safe*; the prototype must produce the integer.
</content>
