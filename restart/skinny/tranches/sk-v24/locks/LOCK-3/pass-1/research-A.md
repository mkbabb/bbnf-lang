# LOCK-3 · Family 3A — THE CONTRACT-TOWER (pass 1, research)

*A small fixed tower of three distinct, progressively-narrowing IR levels; each descent a total
function guarded by a machine-checkable well-formedness contract; each level carrying a
round-trip or differential-parity oracle; the whole verified pass-by-pass in debug and gated in
CI. The family's architectural center is MULTIPLE distinct typed IRs with per-level contracts —
progressive lowering in the MLIR sense, verified in the LLVM/GHC-lint sense. Correctness of the
transposition spine is not asserted once; it is re-checked at every level, every run.*

---

## 1. The mechanism, made concrete

### 1.1 The three levels (fixed, named, distinct Rust types)

The extant pipeline already threads three distinct IR types plus a text sink; 3A promotes that
latent structure to a **declared, contracted tower** and fixes it at three semantic levels.

| Level | Type (home) | Produced by (total fn) | Narrowing on descent |
|---|---|---|---|
| **L1 — HIR** (typed grammar) | `GrammarIr{rules,exprs:Vec<Expr>}`, `ExprKind` — `ir/src/lib.rs:33,234` | `T1 = parse ∘ resolve_refs ∘ infer` : `Source → HIR` (`grammar::parse_grammar` + `GrammarIr::resolve_refs` `ir/src/lib.rs:84`) | `Ref` gains `target:Some` |
| **L2 — MIR** (backend-agnostic decisions) | `BackendIr{recognizers,rules:Vec<BackendRule>,shape_facts}` + `LayoutFacts{backend_shape,cost_facts}` — `ir/src/lib.rs:351`, `passes/src/lib.rs:83,91` | `T2 = decide` : `HIR → MIR` (`passes::compile` `passes/src/lib.rs:32`) | `Project→ValueProject`; `Optional→OptionalBranch`; `Repeat{min,max}→RepeatLoop{min}` (max dropped); `Regex{pattern}→RegexProgram{pattern,span_kind}` (span classified); every rule acquires a `BackendShape` + `CostFacts` |
| **L3 — LIR** (backend-specialized program + text) | `Lowered{sink_only_program:SinkOnlyProgram, rule_plans}` → `EmittedSource{files}` — `codegen/src/lib.rs:43`, `lower/sink_only.rs` | `T3 = lower ∘ print` : `MIR → LIR → Text` (`lower::lower_to_rust` `lower/rust.rs`, then `runtime_generator::emit_sink_only` `codegen/src/lib.rs:210`) | `CallRule` resolved; `DirectBuild` schema-bound; Rust bodies materialized |

The **narrowing is real and already visible in the type delta** `ExprKind` (`ir/src/lib.rs:234`)
→ `BackendExpr` (`:376`): the descent DELETES constructs (`Optional`, `Repeat.max`, `Project`)
and ADDS resolved facts (`span_kind`, `TapeEmit.kind`, `DirectBuildTarget`). That is MLIR
*progressive lowering*: each level is a smaller, more-legal sub-language of its predecessor. 3A's
contribution is to make the smaller-ness **checkable** rather than incidental — and to make each
descent a *total function* rather than the current `Result`-returning arrow that fails closed
deep inside lowering.

**Adjudication — why THREE, not four.** The SPEC names five strata (grammar AST → typed HIR →
mid-IR → backend shapes → emission). 3A collapses the two ends: "grammar AST → typed HIR" is a
single frontend transposition T1 whose only lawful failure mode is a malformed grammar
(`Result` at L1 alone); "backend shapes → emission" is `lower` followed by a **pure
pretty-print** of LIR (`EmittedSource` is text, `check_dir` at `codegen/src/lib.rs:68` treats it
as bytes), not a semantic transposition — so it folds into T3. The result is exactly three
*semantic* descents, each between two typed IRs, which is the number the charter fixes. The
decision driver (`passes::compile`, LOCK-2's seat) is T2 and stays a single arrow; its facts
ride L2 as a **side-table** (`LayoutFacts`) with its own contract, not baked into `BackendExpr`
(that baking-in is 3B's single-mutable-IR move; 3A keeps decisions as annotated,
separately-verifiable facts — this is precisely what makes 3A architecturally orthogonal to 3B).

### 1.2 The contract per level (a Verify trait — LLVM/MLIR verifier discipline)

Each IR level carries a well-formedness predicate `verify : &Self → Result<(),
ContractViolation>` — the analog of LLVM `verifyModule` (`llvm/lib/IR/Verifier.cpp`), MLIR op
`verify()` traits, GHC `-dcore-lint`, rustc `-Zvalidate-mir`, and Cranelift's CLIF `verifier.rs`.
The contracts encode the family's **standing laws as executable predicates** — the insight that
gives 3A teeth:

- **L1 (HIR) contract** — extends the extant `GrammarIr::validate` (`ir/src/lib.rs:110`) and
  `validate_resolved_refs` (`:300`): every `Ref.target` is `Some` and in range; no dangling
  `ExprId`; the nullability fixpoint (`nullability` `:320`, already the LLVM-style `changed`-bool
  form the `changed-flag-convergence` law prescribes) terminates; `Project.target`/`ty` are
  syntactically well-formed. *Nearly present; 3A hoists it behind the `Verify` trait.*
- **L2 (MIR) contract** — the **legalization predicate** (MLIR `ConversionTarget` full/partial
  legalization): MIR contains **no** `Project` (all lowered to `ValueProject`/`DirectBuild`);
  every `RuleId` has a `BackendShape` in `backend_shape` (generalizes the "missing per-grammar
  policy facts" check at `codegen/src/lib.rs:708`); the decision-CSP status is not `unsat`
  (generalizes "decision-CSP status unsat" at `:684`); and — the **no-value-discard law**
  (`typed-materialization-invariant`, `no-value-discard`) — every L1 `Project` reaches a
  `ValueProject`/`DirectBuildTarget`, i.e. `DirectBuildField.target` is `Some` for every
  projected field (today `None` for every offset-tape descriptor — R6 §5c). The W7 fail-closed
  checks now living **inside** lowering (`codegen/src/lib.rs:114,684,708`) are L2 contract
  predicates checked one level too late; 3A moves them into `verify(MIR)` so `T3` becomes a
  *total* function (no fail-closed at emit).
- **L3 (LIR) contract** — legalization complete: no unresolved `CallRule`; `select_lowering`
  (`lower/mod.rs:19`, already an exhaustive match over `BackendShape` — the `no-silent-epsilon`
  law realized in code) covers every shape; every emitted file starts with `GENERATED_HEADER`
  (`codegen/src/lib.rs:99`); `preserve-rich-ast` — the emitted parity fields match the MIR's
  `DirectBuild` shape roster (no field flattened away).

### 1.3 The transposition contract (the checked-pass driver)

Each descent `T_i` is specified by a Hoare pair `{verify_in} T_i {verify_out}`: given input
satisfying the source contract, `T_i` is **total** (no panic, no silent fallback) and produces
output satisfying the target contract. The `no-silent-epsilon` law is the runtime backstop — a
lowering catch-all *panics*, never returns `Epsilon` — and the L2 contract makes that panic
*unreachable* by guaranteeing the input never carries a variant the lowerer cannot handle
(exhaustive matching, as `select_lowering` already does). The driver mirrors `opt -verify-each` /
MLIR's pass-manager post-pass verifier / rustc's per-MIR-pass validate:

```
fn descend<A, B>(name, verify_in: fn(&A)->R, pass: fn(A)->B, verify_out: fn(&B)->R, x: A) -> B
```

running `verify_in`/`verify_out` in debug and CI, eliding them in `release` regen (the standing
debug/CI-vs-release split — `iter-profile-always`, `generated-size-budget`). A legitimate error
(malformed grammar) is `Result` at **T1 only**; after HIR verifies, T2 and T3 are total —
fail-closed-at-emit is thus a design smell 3A retires by lifting the check to its true level.

### 1.4 The oracle per level (round-trip WHERE invertible, differential parity WHERE lossy)

The charter's disjunction "round-trip / parity" is load-bearing and 3A binds it per level,
because **T2 and T3 are not invertible** (decisions/recognizers are added, `ws` collapses to
`Unit` — R6 §7, `Repeat.max` is dropped):

- **L1 round-trip oracle** — `pretty ∘ parse = id` on normalized HIR. The seed exists:
  `GrammarIr::pretty` (`ir/src/lib.rs:124`) + the `pretty_prints_rules_deterministically` and
  `regex_nullability_uses_quantified_atom_shape` tests (`:637,652`). 3A completes it to a total
  idempotent fixpoint over all live grammars (json / css-l4 / sheets / csv). The cheapest,
  strongest level oracle; a translation-validation instance (Pnueli–Siegel–Singerman, TACAS'98).
- **L2/L3 differential-parity oracle** — a naive **reference tree-walking interpreter over
  HIR** whose value/event stream on the empirical-spine corpus must equal the compiled parser's
  output. This is the meta-circular oracle (ties LOCK-7 7D) and is a *different algorithm*
  (backtracking recursive descent) from the compiled specialized parser, so common-mode failure
  is reduced. It is backed by **three independent third oracles already in the tree**: regen
  byte-identity (`EmittedSource::check_dir` `codegen/src/lib.rs:68`; `xtask
  check-json/check-css/check-sheets`), NEON==scalar checkasm parity (`xtask primitive-checkasm`),
  and the XOR-fold parity checksums (CSS `CSS_CANON_PROFILE`; JSON family-B
  `assert_direct_struct_parity`) plus external references (serde_json / lightningcss / sonic-rs).

### 1.5 The CI gate

`cargo xtask verify-tower`: run `verify_{L1,L2,L3}` + the L1 round-trip + the L2/L3 parity oracle
across the corpus; born-RED, fail-closed. This **unifies** oracles that today are scattered
(regen check, checkasm, canon parity checksums) under one tower gate with one exit code — the
artifact the charter's "CI-enforced" clause demands, and the concrete-artifact discipline the
constitution (clause vii) requires.

**The load-bearing crux — L3 is already contracted-and-green; L1/L2 are contracted-and-violated.**
The L3 emission contract EXISTS and PASSES in-tree today: `EmittedSource::check_dir`
(`codegen/src/lib.rs:68`) + `xtask check-json/check-css/check-sheets` assert regen byte-identity,
and `assert_direct_struct_parity` + the CSS `CSS_CANON_PROFILE` XOR-fold assert differential
parity — a working level-contract in all but name. The L1 and L2 contracts, by contrast, are
absent *and the laws they would enforce are silently violated at HEAD*: `Project.ty` is discarded
at `passes/src/lib.rs:203-204` (the `no-value-discard`/`typed-materialization-invariant` breach,
R6 §5a) and `DirectBuildField.target` is `None` for every offset-tape descriptor
(`passes/src/lib.rs:1149-1287`, R6 §5c). 3A's mechanism is therefore precise and *proven-by-
precedent*: **generalize the working L3 level-contract UP the tower to L1/L2, turning two silent,
in-production law-violations into RED gates.** A law without an oracle is not a law — reviewer
discipline already failed to catch these two; the tower makes each law a `verify()` a machine
runs. This is the single sharpest argument for the family, and it distinguishes 3A from a
merely-aspirational verifier tower: the pattern is not hypothetical, it is running at L3 and needs
only lifting.

### 1.6 The multi-backend payoff (Rust now, TS later)

The tower's cut point is `T3`: L2 (MIR + `LayoutFacts`) is **backend-agnostic** (the
constitution's "backend-agnostic middle"; `TypeDesc::Named` per `backend-agnostic-types`), and
each backend supplies its own `T3 = lower_mir` + its own L3 contract + its own parity oracle.
Rust is `runtime_generator` today; the TS backend plugs in as a second `T3` sharing the SAME
verified MIR, with byte-exact tape crossing as its L3 parity oracle (the certified cross-language
tape). The contract-tower is what makes "add a backend" a bounded, verifiable operation rather
than a fork — the MIR verifier is the shared contract both backends must satisfy.

---

## 2. Prior art (with citations)

- **MLIR progressive lowering + dialect conversion** — per-op `verify()` traits, the
  pass-manager verifier after every pass, and `ConversionTarget` full/partial *legalization*
  (Lattner et al., "MLIR: Scaling Compiler Infrastructure for Domain-Specific Computation,"
  CGO 2021). 3A's "each level = a legal sub-language; contract = a legality predicate" is this,
  specialized to a grammar-generator's three IRs.
- **LLVM IR Verifier** — `verifyModule`, `opt -verify-each` runs the verifier between passes in
  expensive-checks builds (`llvm/lib/IR/Verifier.cpp`). Direct model for §1.3's driver.
- **GHC Core Lint** (`-dcore-lint`, `CoreLint.hs`) — type-checks the typed Core IR after each
  optimization pass; the canonical "typed IR re-checked after every transformation," catching
  optimizer bugs. Model for the L1/L2 typed contracts.
- **rustc MIR validator** (`-Zvalidate-mir`, `rustc_mir_transform/src/validate.rs`; the
  `MirPass` trait) — per-pass CFG/type invariant validation. Model for the checked-pass driver.
- **Cranelift CLIF verifier** (`cranelift/codegen/src/verifier.rs`, `enable_verifier`) — debug
  invariant checking of a lowered IR; the debug/release split in §1.3.
- **Nanopass framework** — Sarkar, Waddell, Dybvig, "A Nanopass Infrastructure for Compiler
  Education," ICFP 2004; Andrew Keep's dissertation (2013). Each IR is a *formally specified
  language*; the framework auto-generates a checker + pass scaffolding. THE prior art for "each
  level a total function with a checkable contract." 3A is a three-level, hand-rolled,
  performance-oriented nanopass tower — deliberately NOT auto-generated (parsimony over a macro
  DSL; the three IRs already exist as hand types).
- **Translation validation** — Pnueli, Siegel, Singerman, "Translation Validation," TACAS 1998;
  Necula, "Translation Validation for an Optimizing Compiler," PLDI 2000. The per-run
  round-trip/parity oracle framing (validate each run, don't prove the pass once).
- **CompCert** (Leroy, "Formal verification of a realistic compiler," CACM 2009) — every pass
  semantics-preserving by *proof*; 3A's oracles are the empirical (differential-test) analog of
  CompCert's per-pass simulation lemmas.
- **Differential testing** — McKeeman, "Differential Testing for Software" (1998); Csmith (Yang
  et al., PLDI 2011) — the L2/L3 parity oracle's justification (divergent implementations expose
  miscompiles).

---

## 3. Substrate (what 3A builds; consumer-in-wave, Y.13)

- `crates/ir/src/verify/` (directory-module colocated with the IR it checks — no god module):
  the `Verify` trait + `impl Verify for GrammarIr` (extends `validate` `ir/src/lib.rs:110`) and
  `impl Verify for BackendIr` (hoists the W7 predicates out of `codegen/src/lib.rs:114,684,708`).
  Consumer: `passes::compile` and `lower::lower_to_rust` call the verifiers in debug.
- `passes` — the `descend` checked-pass driver (§1.3) wrapping T1/T2/T3. Consumer: the regen path
  (`emit_from_source` `codegen/src/lib.rs:101`) in debug/CI.
- `crates/bbnf-interp/` (new crate — general infra in its own home, per `general-infra-crates`;
  NOT stuffed into `passes`): the naive HIR tree-walking reference interpreter, the L2/L3 parity
  oracle. Consumer: `xtask verify-tower` + a corpus differential test.
- `xtask verify-tower` subcommand (`xtask/src/`) — the CI gate unifying verifiers + oracles +
  the extant regen/checkasm/parity-checksum gates (R1 §5).
- L1 round-trip completion in `ir` + tests under `crates/ir/tests/` (tests in `tests/`, never
  inline — `no-inline-tests`).

---

## 4. Integration seams (path:line where extant code already serves)

- **Whole tower today** = `codegen/src/lib.rs:101` `emit_from_source`: `:102` parse (T1 in),
  `:103` `passes::compile` (T2), `:176` `lower_to_rust` (T3a), `:210` `emit_sink_only` (T3b/print).
- **L1 contract seed**: `GrammarIr::validate` `ir/src/lib.rs:110`; `validate_resolved_refs` `:300`;
  `resolve_refs` `:84`; `nullability` fixpoint `:320`.
- **L1 round-trip seed**: `GrammarIr::pretty` `ir/src/lib.rs:124`; tests `:637,652`.
- **L2 contract predicates, currently mislocated inside lowering** (hoist to `verify(MIR)`):
  fail-closed emit `codegen/src/lib.rs:114`; "decision-CSP status unsat" `:684`; "missing
  per-grammar policy facts" `:708`. Shape-completeness source: `layout_facts.backend_shape`
  `passes/src/lib.rs:96`; the consistency assert already present at `passes/src/lib.rs:61`.
- **No-value-discard watch-point**: `DirectBuildField.target: Option<DirectBuildTarget>`
  `ir/src/lib.rs:470` (the `None` the L2 typed contract turns to `Some`); the discard site
  `passes/src/lib.rs:203` (`Project` infer drops `ty`); lower `Project→ValueProject`
  `passes/src/lib.rs:1349`.
- **L3 totality (no-silent-epsilon)**: `select_lowering` exhaustive match `lower/mod.rs:19`;
  per-shape `lower_rule` in `lower/{eager_tape,offset_tape,event_tape,collapsed_stage,sink_only}.rs`.
- **L3/emission oracles already present** (unify under the gate): `EmittedSource::check_dir`
  `codegen/src/lib.rs:68`; `xtask` regen/check + `primitive-checkasm` (R1 §5, §8); CSS XOR-fold
  parity checksum; JSON family-B parity.
- **Multi-backend cut**: T3 is the only backend-specific arrow (`runtime_generator`); MIR + its
  verifier are the shared trunk a TS `T3` reuses.

---

## 5. Named risks & counterexamples

- **R1 — three is a Procrustean bed.** The real graph is T1/T2/T3 *plus* recognizer derivation
  (`passes::derive_recognizers`) and a print. *Counterexample:* `passes::compile` does infer AND
  shape/CSP/egraph AND recognizer derivation in one arrow — if MIR is defined "post-decision,"
  the decisions are baked before any MIR verifier can check "did a `->` survive?" *Mitigation:*
  T2 is contracted on its OUTPUT only (post-decision MIR must be legal); decision *internal*
  correctness is LOCK-2's charter, not 3A's — 3A verifies the interface, keeping `LayoutFacts` a
  separately-verifiable side-table.
- **R2 — round-trip is impossible for lossy levels.** MIR→HIR is not invertible (`ws→Unit`,
  `Repeat.max` dropped, recognizers added). *Counterexample:* attempting `BackendIr` round-trip
  loses `cost_facts`. *Mitigation:* the charter's disjunction — L1 gets round-trip, L2/L3 get
  differential parity (§1.4). This binding must be STATED per level, not left ambiguous, or the
  family ships a contract it cannot satisfy.
- **R3 — who validates the validator?** The reference interpreter is a second parser; a shared
  bug (both mishandle the nullable-repeat relaxation, `ir/src/lib.rs:112`) passes the oracle
  falsely. *Mitigation:* divergent algorithm (backtracking vs specialized descent) + three
  independent third oracles (serde_json/lightningcss/sonic-rs, regen byte-identity, checkasm) —
  no single bug survives all four.
- **R4 — the no-value-discard contract is born-broken at HEAD.** `DirectBuildField.target` is
  `None` for every offset-tape descriptor today (R6 §5c); a *universal* "every field has
  `Some(target)`" reddens the entire current tree. *Counterexample:* the JSON offset-tape lane
  has no `->` at all (json.bbnf is pure EBNF — R6 §0). *Mitigation:* the contract is CONDITIONAL
  on the typed lane (a `typed`/`structural` mode on `verify(MIR)`) and tightens to STRICT only
  after the arrow is threaded. This is 3A's sharpest sequencing coupling: the contract-tower and
  arrow-threading must be ordered (thread first, then strict) or the gate is born-RED for the
  wrong reason.
- **R5 — verifier cost + nondeterminism in the regen hot path.** *Counterexample:* verify-each
  on every regen slows `xtask regen-*`; and `LayoutFacts` is a `HashMap`, so a verifier that
  reports facts in iteration order emits nondeterministic diagnostics, breaking regen
  byte-identity. *Mitigation:* fast structural verify always, expensive parity oracle only in
  `verify-tower` CI (LLVM `-verify-each` pattern); verifier + oracle output sorted by `RuleId`.

---

## 6. What a prototype MUST test to move this family

- **P1 (contracts catch real defects) — the decisive test.** Implement `Verify` for `GrammarIr`
  + `BackendIr` encoding §1.2. Seed two deliberate law-violations: (a) a value-discard (leave
  `DirectBuildField.target = None` on a projected field), (b) a silent-epsilon (a lowering
  catch-all returning a no-op). MEASURE: does `verify(MIR)` RED on (a) and the L3 totality
  contract / panic backstop RED on (b)? A contract that passes a seeded defect is decoration.
- **P2 (L1 round-trip is total + idempotent).** Complete `pretty` to a fixpoint; run
  `pretty→parse→normalize→eq` on json/css-l4/sheets/csv. MEASURE: identity on all four, and
  idempotence (`pretty∘parse∘pretty = pretty`). Surface where `pretty` loses precedence
  (`Seq`/`Discard` wrapping is incomplete at `ir/src/lib.rs:135-204`).
- **P3 (L2/L3 differential parity + common-mode resistance).** Build the naive HIR interpreter;
  diff its normalized value/event stream vs the compiled parser on the empirical-spine corpus
  (twitter/canada/citm + css bootstrap/tailwind + sheets). MEASURE: structural parity across the
  corpus; then seed a known miscompile and confirm the oracle catches it where a shared-code
  oracle would not. Interpreter throughput is irrelevant (correctness-only), but it must be
  deterministic.
- **P4 (cost + determinism budget).** MEASURE the fast-verify overhead on `xtask regen-*` (must
  be negligible under `release`, elided) and the `verify-tower` wall-time on the full corpus
  (must fit the CI budget). Confirm verifier + oracle emit byte-identical diagnostics across runs
  (sort `HashMap`-keyed facts by `RuleId`).

P1 answers the move-the-family question first: **do the standing laws, expressed as per-level
verifier predicates, actually catch their own violations?** If yes, the contract-tower is a real
safety net and the family advances; if the contracts are too weak to RED a seeded discard/epsilon,
the tower is decoration and the family must strengthen or retire.
