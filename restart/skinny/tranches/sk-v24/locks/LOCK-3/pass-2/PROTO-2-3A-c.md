# PROTO-2 (3A · probe c) — LOCK-3 pass-2 · the *contract non-vacuity* oracle

**Family:** 3A — CONTRACT-TOWER. Probe **c** (pass-2 spine). Distinct from the already-delivered
`PROTO-2-3A.md` (probe t2, the E3 *refinement* oracle): this probe attacks the **other** UNRUN
load-bearing half of 3A that §5 names — **contract non-vacuity** — i.e. FAMILY-SPECS §1 open
question **(i)** / gate **G-3A-α**:

> Does ≥1 Sheets projection exist where declared `ty` constrains *beyond* `infer_expr(body)` — i.e.
> can the contract EVER be non-vacuous, or is it structurally doomed until LOCK-1?

**BASE-PIN.** Worktree HEAD measured on **`ce51c36fcbdf5108eb028005586672d9a6d2ca1b`** — the pin
itself. Worktree was stale at `d2b32f6e3` (an ancestor); fast-forwarded cleanly with
`git merge --ff-only ce51c36fc`; `git merge-base --is-ancestor ce51c36fc HEAD` ⇒ **descends (equals)**.
`git diff --stat ce51c36fc 3f2350f15 -- skinny/crates` is **empty** (byte-identical over the source
tree), so numbers transfer to the FAMILY-SPECS anchor verbatim. **No wrong-artifact-risk.** arm64;
no x86. parse-that untouched (bbnf-lang IR probe; READ-ONLY rule not engaged). loadavg 5.09–6.03
(machine shared with siblings). Isolated `CARGO_TARGET_DIR` in scratchpad — no shared-target
contention.

**Filename note.** Delivered as `PROTO-2-3A-c.md` to avoid clobbering the committed `PROTO-2.md`
(family 3B) and `PROTO-2-3A.md` (probe t2). Neither was touched.

---

## QUESTION

G-3A-α asserts the declared-vs-inferred contract **is a GATE-THAT-CANNOT-FAIL until LOCK-1**: because
(the spec argues) `Type::Declared(String)` is a *nominal* wrapper never structurally equal to the
*structural* `infer_expr(body)`, the predicate's `≠ body` clause is **vacuously true whenever
`ty=Some`**, so the organ verifies threading-*presence*, not declared-vs-inferred *consistency*.

Two things must be settled by execution, not argument:
1. **Present?** Does any live grammar populate `Project.ty = Some(_)` at all?
2. **Non-vacuous?** For any such site, can the declared string be compared to `infer_expr(body)`
   **today**, with NO LOCK-1 name→Type table — and can that comparison actually *fail* on a wrong
   declaration (proving it is not vacuously true)?

**Structural ground truth (re-greped this pass, live `ce51c36f`).** The spec's premise is subtly
off: the live `Type` enum (`passes/src/lib.rs:235`) has **no `Declared` variant** — variants are
`Var, Builtin, Seq, Alt, List, Option, Rule`. The declared type is not a `Type::Declared(String)`; it
is a raw **`Project.ty: Option<String>`** (`ir/src/lib.rs:261`), an *unparsed* nominal string, while
`ExprKind::Project { body, .. } => self.infer_expr(*body)?` (`passes:204`) returns a *structural*
`Type`. The two universes meet **iff** the string is resolvable to a `Type`. G-3A-α's LOCK-1 name
table is one such resolver — but it is **not the only one**: for the **builtin-named subset**
(`Span/F64/Str/Bytes/U8/Bool/Unit`) a *total, LOCK-1-free* `str → BuiltinTy` map resolves the string
to `Type::Builtin(_)`, which is directly comparable to `infer_expr(body)`. Whether that subset has any
live witness is an empirical question — this probe answers it by running.

---

## METHOD (what ran)

A running probe `skinny/crates/passes/tests/contract_nonvacuity_probe.rs` (2 tests, **pure test,
zero production footprint**), driving the **real** shipping pipeline:

1. **Corpus.** `grammar::parse_grammar` over the live grammar sources the skinny parser accepts:
   **Sheets** (`grammar/google-sheets/google-sheets.bbnf`, the exact corpus §1(i) names) and **JSON**
   (`skinny/grammars/json.bbnf`, negative control), both `include_str!`'d hermetically.
2. **Real inference.** `passes::layout::types::infer(&grammar)` (the shipping `infer_expr`), reading
   `node_types[body] = infer_expr(body)`.
3. **The LOCK-1-free resolver.** A total `parse_builtin(&str) -> Option<BuiltinTy>` — the *whole point*:
   it needs no name table, no `Type::Declared`, no rule map. Anything it can't resolve (nominal /
   aggregate names) is classified `NominalUnresolvable` — the genuine LOCK-1 residue.
4. **Adjudication.** Every `Project` site → `{Untyped | NominalUnresolvable | BuiltinConsistent |
   BuiltinViolation}` by comparing `Type::Builtin(parse_builtin(ty))` to `infer_expr(body)`.
5. **The discriminator (the actual proof).** Mutate the Sheets source so the first `-> input : Span`
   (rule `string`, a regex body inferring to `Span`) becomes `-> input : F64`; re-parse, re-infer,
   re-adjudicate; assert the contract RED-flags **exactly that site and only that site**, while a
   `ty`-blind structural check stays uniformly GREEN.

`cargo test -p passes --profile ax-iter --test contract_nonvacuity_probe -- --nocapture
--test-threads=1`, single invocation, isolated `CARGO_TARGET_DIR`. Compiled in 5.17s; **2 passed, 0
failed; EXIT_CODE=0**.

---

## WHAT RAN — THE NUMBERS (measured)

### Sheets census — the contract is PRESENT, NON-VACUOUS, and SATISFIED today

| metric | value |
|---|---|
| total `Project` sites (Sheets) | **28** |
| declared (`ty = Some`) | **4** — all `"Span"` (rules `string`, `sheet_prefix`, `cell_ref`, `identifier`) |
| builtin-resolvable & checked (no LOCK-1) | **4 / 4** |
| **BuiltinConsistent** (declared == `infer(body)`) | **4** |
| **BuiltinViolation** | **0** |
| `NominalUnresolvable` (needs LOCK-1) | **0 live** (the two aggregate `String`/`CellRef` declares at src lines 11/62 are **commented TODOs**, not live) |
| untyped (`ty = None`; targets `f64`,`true`,`0u8`…) | 24 |
| JSON `Project` sites (negative control) | **0** — nothing to check |

All four `: Span` declared types sit over non-whitespace regex bodies, which `infer_expr`
(`regex_type`) types `Builtin(Span)` — so `parse_builtin("Span") == Builtin(Span)` holds. **The
contract has real content and is satisfied on the shipping grammar, with no LOCK-1 machinery.**

### The discriminator — injected mis-declaration localizes RED, LOCK-1-free

| run | `string`-rule verdict | total `BuiltinViolation` | `ty`-blind structural check |
|---|---|---|---|
| baseline (`string: Span`) | `BuiltinConsistent` | **0** (GREEN) | GREEN |
| **inject `string: Span → F64`** | **`BuiltinViolation`** (`declared=F64`, `inferred=Builtin(Span)`) | **1** — localized to `string`, **only there** | **GREEN (blind)** |

`LOCALIZE: RED at rule=string declared=Some("F64") inferred=Builtin(Span)`. A gate-that-cannot-fail
could not produce this RED; a `ty`-blind check never sees it. **The declared-type contract adds
non-vacuous, localizing discriminating power on the live tree, today.**

---

## VERDICT LINE

**RAN_MEASURED — §1 open question (i) = YES; gate G-3A-α is REFUTED-as-stated.** Executed on the pin
(`ce51c36f`, 2/2 tests GREEN, EXIT 0, loadavg ~5.2): **4 live Sheets projections** (`string`,
`sheet_prefix`, `cell_ref`, `identifier`) carry a declared `ty` (`"Span"`) that is **non-vacuously
checkable today** against `infer_expr(body)` via a **total, LOCK-1-free `str→BuiltinTy` resolver** — no
name→Type table required — and an injected `: Span → : F64` mis-declaration flags **RED at exactly the
`string` rule and only there** while a `ty`-blind check stays GREEN. G-3A-α's letter — *"gate-that-
cannot-fail until LOCK-1," "vacuously true whenever `ty=Some`"* — is **false**: the gate CAN fail and
IS non-vacuous for the builtin-named subset. The contract is therefore convergence-relevant **before**
LOCK-1, not only after it.

**Scope-honest limits (G-3A-α survives, narrowed — do not overclaim).**
1. **The nominal/aggregate residue is real.** The two commented Sheets TODOs (`-> … : String`,
   `-> … : CellRef (aggregate u32,u32,bool,bool)`, src lines 11/62) name types `parse_builtin`
   *cannot* resolve; those **genuinely need LOCK-1's name→Type resolution**. G-3A-α holds for that
   subset — it just does not hold *universally*, which is what it claimed.
2. **Coarse-inference ceiling.** `infer_expr`'s `regex_type` types *every* non-whitespace regex as
   `Span`, so the builtin contract currently only distinguishes `Span` from a wrong builtin (the F64
   injection) — it would **mis-pass** a hypothetical `number … -> f64 : F64` because that numeric
   regex also infers `Span` (visible in the census: `number` body infers `Builtin(Span)`, not F64).
   **Fine-grained** declared-vs-inferred consistency (numeric-regex ⇒ F64) needs richer inference than
   `regex_type` — a distinct born-RED item, not LOCK-1 naming. So the *non-vacuity* claim is proven;
   the *full-fidelity* contract is bounded above by inference precision.
3. Corpus = Sheets + JSON (the two grammars skinny's parser consumes as `.bbnf`); CSS L4 is multi-file
   (`@import`) and out of a single-`parse_grammar` scope this probe. Sheets is the exact corpus §1(i)
   names, so the question is answered on its own terms.

**Composition with probe t2.** t2 proved the E3 *refinement* oracle localizes an injected edge fault;
this probe proves the E1/contract oracle is **non-vacuous** — together they retire two of the three
things §5 flags UNRUN for 3A (E3 localization ✓, contract non-vacuity ✓). The remaining UNRUN 3A gate
is **G-3A-β proper**: an injected-fault matrix that mutates `infer`/`materialize` themselves (this
probe injects at the *grammar-source* layer, which is the honest way to falsify the contract without a
LOCK-1 resolver, but it is not yet the in-pass `infer`-mutation matrix).

**Next falsifiable step (pass-3).** Promote `parse_builtin` + the site adjudicator into the shipping
`differential(E1, corpus)` contract alongside t2's E3 oracle, and land the richer `regex_type`
(numeric ⇒ F64) so the builtin contract gains fine-grained fidelity — at which point the coarse-Span
mis-pass in limit (2) closes and the F64/Span distinction becomes a real live check.

---

## PATCH / DIFF

`PROTO-2-3A-c.patch` (git diff on pin `ce51c36f`). **Production footprint: 0 lines** (pure test; zero
emit change, zero generated files):
- `skinny/crates/passes/tests/contract_nonvacuity_probe.rs` (+252, new): the two-test probe
  (`sheets_contract_is_present_nonvacuous_and_consistent`, `injected_misdeclaration_localizes_red_without_lock1`),
  the LOCK-1-free `parse_builtin` resolver, the site adjudicator, and the source-mutation discriminator.
