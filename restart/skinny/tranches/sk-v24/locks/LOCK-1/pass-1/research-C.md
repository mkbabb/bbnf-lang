# LOCK-1 / family 1C — BIDIRECTIONAL-LOCAL (pass 1, research)

The type system for the grammar plane developed as a **bidirectional** discipline: two
judgments per grammar rule — SYNTHESIZE (`⇒`, bottom-up, "read a type off the structure") and
CHECK (`⇐`, top-down, "verify the structure against a declared projection type"). A declared
`-> T` supplies the checking type; a bare `->` synthesizes from the leaf's literal shape. Rules
are the checking/synthesis unit; global solving is confined to the cross-rule reference seam and
to a bounded numeric-width post-solve. The organizing thesis, and the reason this family is a
distinct architectural center rather than a variant of unification-first HM: **the expected type
is known at the failure site, so every diagnostic names the exact grammar span.** Diagnostics are
the deliverable, not a byproduct.

---

## 1. The mechanism, made concrete

### 1.1 Two judgments, one recipe

The frozen construction is the **Pfenning recipe** (Dunfield & Krishnaswami, *Bidirectional
Typing*, ACM Computing Surveys 2021, §3): **introduction forms check, elimination forms
synthesize**, and a single **subsumption** rule bridges them — where a synthesizing subterm sits
in a checking context, its synthesized type is reconciled against the expected type, and *that
reconciliation site is the sole place a type error is raised*. Mapped onto `ExprKind`
(`skinny/crates/ir/src/lib.rs:234-270`):

| `ExprKind` | mode | rule |
|---|---|---|
| `Seq(children)` | check ⇐ | introduction of a product; each child checks against its field slot of the expected struct/tuple type |
| `Alt{branches}` | check ⇐ | introduction of a sum; each branch checks against the corresponding variant of the expected sum (GN-3.W1's `DirectTypeKind::Sum`) |
| `Repeat{body}` / `Optional` | check ⇐ | `Vec<E>` / `Option<E>` introduction; body checks against the element type `E` |
| `Literal` / `Regex` | synth ⇒ | leaf synthesis: `Bytes`, and the existing `RegexKind`→leaf map (`passes/src/lib.rs:194`, `regex_type`) |
| `Ref{target}` | synth ⇒ | elimination; synthesizes the **callee rule's signature** `RuleSig[target]` — the cross-rule seam (§1.3) |
| `Project{body,target,ty}` | **mode switch** | `ty:Some(A)` ⟹ `check(body, A)` against the ascription; bare ⟹ `synth` the leaf via `classify_arrow` |

The existing `Infer::infer_expr` (`skinny/crates/passes/src/lib.rs:178-215`) is already the
synthesis half in embryo — it folds `Seq→Type::Seq`, `Alt→Type::Alt`, `Repeat→Type::List`,
`Optional→Type::Option`, `Ref→Type::Rule(id)`. Family 1C **renames it `synth_expr` and adds a
dual `check_expr(ExprId, &Expected) -> Result<(), TypeError>`**. The two are mutually recursive;
the `Project` arm at `:204` — today `=> self.infer_expr(*body)?`, the discard R6 and GN-3.W2 both
indict — becomes the mode switch: `Some(ty)` drops into `check_expr(*body, resolve(ty))`, bare
falls to `synth_expr` and yields the projected leaf. **This is where the discarded arrow type
re-enters, but via checking, not unify-fill** — the distinction is the whole family.

### 1.2 What checking buys that synthesis cannot

Under pure synthesis (family 1A's unify-first shape), a type clash surfaces at the *unification
event* — often a synthesized interior node far from the author's annotation, and the reported
types are inference variables, not source text. Under checking, the expected type `A` is in hand
*before* the subterm is examined, so a mismatch is reported at the subterm's own `Expr.span`
(`ir/src/lib.rs:230`) against the concrete `A` the author wrote. This is precisely why
TypeScript's contextual typing, Roc, and Elm produce localized errors and ML's `let`-generalized
unifier historically did not (Pierce & Turner, *Local Type Inference*, TOPLAS 2000, §1 motivation:
"the programmer's annotations are exactly the points where the checker changes direction"). For
the grammar plane the payoff is direct: constitution clause vii + GN-3's charter both demand
**"every failure names its grammar span"** — bidirectional checking makes that a structural
property of the algorithm, not an error-message afterthought.

### 1.3 Rule-local, and where "global" enters — the SCC seam

"Rule-local" is made exact by the **binding-group discipline** (GHC's dependency analysis;
rustc's item-level typing): build the rule-reference graph from `ExprKind::Ref.target`
(`ir/src/lib.rs`, the `target: Option<RuleId>` field), condense it to **strongly-connected
components**, and process the condensation in reverse-topological order. An **acyclic** rule is
checked/synthesized purely locally against its declared or callee-supplied signature — no global
state. A **cyclic** SCC (json's `value ↔ object ↔ array ↔ pair`, since `object`/`array` refer
back through `value`) is the *only* construct that cannot bottom out directionally; it is solved
by a **bounded local fixpoint** — a fresh signature variable per rule in the SCC, iterated
check-then-reconcile to convergence using an LLVM-style `Changed` flag (memory:
changed-flag-convergence), never a whole-grammar unification. **This is the precise reading of the
charter's "global solving only at cross-rule seams": the seam is the `Ref` edge; the only global
object is the SCC condensation; solving is bounded to one SCC at a time.** The `Ref` arm at
`passes/src/lib.rs:201` — today returning an opaque `Type::Rule(rule_id)` — becomes a lookup into
the `RuleSig: HashMap<RuleId, Type>` table populated in SCC order. The census of SCC-local
reconciliations vs total rule count is the *measurable* claim that solving stayed local.

### 1.4 The bounded CSP post-solve: numeric widths and overloads

Bidirectional checking pins the *shape* (struct/sum/Vec/leaf); it does **not** by itself pick a
numeric *width*. `-> 0u8`, `-> f64`, `-> 300`, `-> 70000` on sibling arrows form a finite lattice
(`u8 ≤ u16 ≤ u32 ≤ u64`, `f32 ≤ f64`, and the StringSpan≤StringDecoded ownership axis from
`classify_arrow`). Width selection is **not** unification — it is *lattice minimization*: pick the
narrowest legal width satisfying every use-site. This is a bounded, finite-domain CSP and it hands
off to the vendored solver: `crates/csp-solver/src/domain/lattice.rs:15` (`BitsetLatticeDomain`)
holds the width lattice, `crates/csp-solver/src/solver/optimize.rs:12` (`DomainCostEval`) supplies
the "narrowest wins" objective, and AC-3 propagation (`solver/ac3.rs`) closes it. The division of
labor is the family's clean seam: **decidable, diagnostic-rich bidirectional checking for
structure; a bounded post-solve over a finite lattice for width/overload/decode-class**. This
matches the charter's own words ("CSP appears only where unification is undecidable — widths,
overloads — as a bounded post-solve") while keeping the *center* bidirectional rather than
constraint-store-first (that center is family 1B's).

---

## 2. The substrate (what lands, with its in-wave consumer — Y.13)

1. **`synth_expr` / `check_expr` split** on `Infer` (`passes/src/lib.rs:178`). Consumer: the
   `Project` mode-switch arm (`:204`) and the materialization plan
   (`derive_materialization_plan`, `:1136-1391`), which reads the check result to fill
   `DirectBuildField.target` (today hardcoded `None`, R6 §5d).
2. **`RuleSig: HashMap<RuleId, Type>`** + an SCC pre-pass over the `Ref` graph. Consumer: the
   `Ref` arm (`:201`) resolving cross-rule types.
3. **`TypeError { span: SourceSpan, expected, got, rule: RuleId }`** and a new
   `PassDiagnosticCode::TypeMismatch`. This exposes a real substrate gap: `PassDiagnostic`
   (`passes/src/diagnostics.rs:3-8`) carries `rule: Option<RuleId>` and a `String` **but no
   span** — 1C must add `span: Option<SourceSpan>` and thread `Expr.span`/`Rule.span`
   (`ir/src/lib.rs:223,230`) through. Consumer: the diagnostic renderer / the .bbnf-anchored
   error report (the family's headline artifact).
4. **The subsumption reconciler** at the mode switch. Reuses PROVE-08's `Unifier`
   (`prototypes/PROVE-08/patch.diff:311` `unify`, `:347` `bind`, `:359` `resolve`) for the
   *equality* case and `classify_arrow` (`patch.diff:374-406`) as the bare-arrow leaf synthesizer.
   Where widths meet, it emits a lattice variable instead of unifying — the CSP handoff. Consumer:
   `DirectBuildScalar` selection (`patch.diff:172` `build_scalar`, where `U8Tag→Null`,
   `F64→F64` already lives).
5. **The width post-solve** over `BitsetLatticeDomain`. Consumer: `schema_scalar`
   (`patch.diff:159`, `U8Tag→U32` width mapping) — the resolved width becomes the emitted scalar.

No orphan substrate: each addition has a same-wave reader. Everything above runs in the generator
crate (`codegen`/`passes`); `git diff crates/runtime/` stays empty — the zero-runtime-cost
invariant (constitution v, GN-3 owner gate) holds by construction, exactly as PROVE-08/VERIFY
attack 2 certified.

---

## 3. Integration seams (path:line where extant code serves)

- `skinny/crates/passes/src/lib.rs:149` — `Infer::infer` gains the SCC pre-pass; the per-rule loop
  becomes SCC-topological.
- `skinny/crates/passes/src/lib.rs:178-215` — `infer_expr` splits into `synth_expr` + `check_expr`;
  the `Seq/Alt/Repeat/Optional/Ref/Literal/Regex` arms gain checking duals.
- `skinny/crates/passes/src/lib.rs:201` — `Ref` arm: consult `RuleSig[target]` not `Type::Rule`.
- `skinny/crates/passes/src/lib.rs:204` — `Project` arm: the mode switch; the discard dies here.
- `skinny/crates/passes/src/lib.rs:1136-1391` — `derive_materialization_plan` reads the check
  result into `DirectBuildField.target`; the same wire GN-3.W2 names.
- `skinny/crates/passes/src/lib.rs:1349` — `Project→ValueProject` lowering carries the *resolved*
  type, retiring the lossy 4-way `ProjectionKind` fold at
  `codegen/src/emit/general_descent/view.rs:20-46` (GN-3.W2's deletion; sheets regen byte-identity
  is the coupling gate).
- `skinny/crates/ir/src/lib.rs:22` (`SourceSpan{start,end}`), `:223` (`Rule.span`), `:230`
  (`Expr.span`) — the diagnostic anchors, **already present** on every rule and every expression.
- `skinny/crates/ir/src/lib.rs:261-265` — `ExprKind::Project{body,target,ty}` exists at HEAD
  (Wall 3 half-fallen); `ty` is a bare `Option<String>` needing resolution — the check-mode input.
- `skinny/crates/passes/src/diagnostics.rs:3-65` — extend `PassDiagnostic` with a span; add
  `TypeMismatch`.
- `prototypes/PROVE-08/patch.diff` — `hm.rs` `Unifier`/`classify_arrow` reused as the reconciler +
  leaf synthesizer (the proven, verified 221-LOC core; VERIFY: CONFIRMED_DELTA).
- `crates/csp-solver/src/domain/lattice.rs:15`, `solver/optimize.rs:12`, `solver/ac3.rs` — the
  width post-solve engine.
- GN-3 interlocks: W1's `DirectTypeKind::Sum` is what `Alt`'s checking mode targets; W2's
  `DirectBuild.type_ref` is the check result's sink; W3's typed grammars (twitter/citm/canada) are
  where checking has annotations to check *against* (§4 R-C2).

---

## 4. Prior art (citations)

- **Pierce & Turner, "Local Type Inference," TOPLAS 22(1) 2000.** Origin of synthesize/check +
  local (annotation-at-boundaries) inference; the direct-change-of-direction insight that makes
  1C's diagnostics local.
- **Dunfield & Krishnaswami, "Complete and Easy Bidirectional Typechecking for Higher-Rank
  Polymorphism," ICFP 2013.** The canonical modern algorithm: judgments `Γ⊢e⇒A` / `Γ⊢e⇐A`, ordered
  contexts, the subsumption rule as the sole mode-switch site — the shape 1C's reconciler copies.
- **Dunfield & Krishnaswami, "Bidirectional Typing," ACM Computing Surveys 54(5) 2021.** The design
  recipe (Pfenning: intro-forms-check / elim-forms-synthesize) used verbatim in §1.1; the survey's
  treatment of mode assignment, annotation minimization, and error localization.
- **Odersky, Zenger & Zenger, "Colored Local Type Inference," POPL 2001.** Handles *partial* type
  information — directly relevant because grammar annotations are sparse (some rules `->`-typed,
  most not); colored inference is how sparse annotations still propagate.
- **rustc `Expectation`** (`rustc_hir_typeck`: `ExpectHasType` / `NoExpectation` /
  `ExpectCastableToType`) — the production instance of a checking-mode expected type threaded down
  an expression tree; the pragmatic model for `check_expr`'s `Expected` argument.
- **GHC binding-group / dependency analysis** and **rustc item typing** — the SCC-condensation
  discipline (§1.3) that bounds "global" to one recursive group.
- **TypeScript contextual typing** — the widest-deployed bidirectional system; evidence that sparse
  annotations + structural propagation give localized diagnostics at scale.
- **Dolan & Mycroft, "Polymorphism, Subtyping, and Type Inference in MLsub," POPL 2017** and
  **Dolan's *Algebraic Subtyping* (thesis, 2016).** The principal-type account for the width/decode
  *subtyping* lattice at the subsumption seam (§1.4) — join/meet where equality-unify would clash.

---

## 5. Named risks and counterexamples

- **R-C1 — recursion has no direction.** `value = object|array|…` with `object`/`array` referring
  back to `value` cannot be checked purely top-down (infinite descent). *Resolution:* the SCC
  fixpoint (§1.3), bounded to the component, `Changed`-flag convergence. The risk is that a
  pathological grammar makes an SCC large; but grammar SCCs are small (json's is 4 rules) and the
  fixpoint is over a finite lattice — bounded by construction.
- **R-C2 — no annotations, no checking (the bidirectional critique).** `grammars/json.bbnf` has
  **zero `->`** (R6 §0); with nothing to check against, 1C degenerates to pure synthesis =
  today's fold. *Resolution:* 1C is a strict superset — synthesis where un-annotated, checking
  where `->` exists. Its diagnostic value is *proven on the typed grammars GN-3.W3 authors*
  (twitter/citm/canada `->` chains), which is exactly where GN-3 scopes equality as reachable. The
  honest claim: on generic json, 1C ≡ 1A (identical output, zero runtime delta); on typed
  grammars, 1C's errors localize where 1A's do not. This *must be measured*, not asserted (§6.3).
- **R-C3 — span granularity.** `ExprKind::Project.ty` is one `String`; a wrong ascription
  `x -> foo : Bar` should underline `Bar`, but `Project` carries only the whole-expr `span`
  (`ir/src/lib.rs:230`). *Resolution:* either enrich `Project` with `target_span`/`ty_span`
  sub-spans (substrate gap, named), or accept expr-level granularity as the floor. Either way the
  error names *a* grammar span — the charter's bar — but sub-span is the quality ceiling.
- **R-C4 — subtyping, not equality, at the mode switch.** Two arrows `-> 0u8` and `-> 70000` on one
  rule: equality-unify *clashes*; the correct answer joins to `u32`. *Resolution:* the reconciler
  calls subtype-join over the width lattice (MLsub-style), not `unify`, precisely at the numeric
  case — the CSP handoff (§1.4). Equality-unify remains for non-numeric leaves. Getting this seam
  wrong collapses 1C into either "spurious clash" (too strict) or "loses width precision" (too
  loose).
- **R-C5 — `PassDiagnostic` has no span today.** The diagnostics substrate
  (`passes/src/diagnostics.rs`) is `RuleId`-only. *Resolution:* additive field; but it means 1C's
  headline (span-naming diagnostics) requires touching the diagnostics type — declare it, don't
  smuggle it.

---

## 6. What a prototype must test to move this family

1. **The mode switch replaces the discard, via checking.** `check_expr(body, resolve(ty))` at the
   `Project` arm populates `DirectBuildField.target` for the four leaf rules (bool/number/string/
   null) — the same GN-3.W2 gate (`target == Some(_)`, not `None`), but reached by checking. Gate:
   `cargo test -p passes` asserts non-`None` targets; `git diff crates/runtime/` empty.
2. **The diagnostic head-to-head (the family's thesis).** Feed one *erroneous* typed grammar
   (`string -> decode_...(input) : Bool`) to (a) a unify-first pass and (b) `check_expr`; assert
   1C's `TypeError.span` equals the `Project` expr's `SourceSpan` while unify-first reports at a
   synthesized interior. **This is the measurement that justifies the family** (constitution vii —
   concrete artifact, named counterexample, not asserted superiority).
3. **The SCC boundary is real and small.** Build the `Ref` graph, condense, assert json's
   `value/object/array/pair` land in one SCC solved by bounded fixpoint while `bool/null/number/
   string` are pure directional; census the SCC-local reconciliations (should be a handful) vs
   total rules — quantifying "global solving only at cross-rule seams."
4. **Width CSP stays bounded.** A rule carrying `-> 0u8 | -> 300 | -> 70000` drives
   `BitsetLatticeDomain` to select `u32` (narrowest legal), terminating without search blowup;
   assert the lattice domain is finite and AC-3 closes in one pass.
5. **Zero-runtime-cost + regen byte-identity.** `git diff crates/runtime/` empty on every wave;
   sheets general-descent regen byte-identical after the `ProjectionKind` fold is retired
   (`cargo xtask check-sheets`) — the coupling gate GN-3.W2/R4 names.

The discriminating experiment is **#2**: bidirectional-local's entire claim over the sibling
families is *diagnostic locality at the annotation site*, and it is falsifiable — if unify-first
reports the same span on the same erroneous grammar, family 1C carries no distinct value and
should fold into 1A. The prototype must run that comparison, not assume it.
