# PROTO-1 — LOCK-1 · pass-2 · probe 1: The equality-carrier race (1A union-find re-seed vs 1B hardened running CSP)

**Base-pin:** worktree HEAD arrived at `d2b32f6e3` (an *ancestor* of the pin). Fast-forwarded
`git merge --ff-only ce51c36fc` → **HEAD measured on `ce51c36fcbdf5108eb028005586672d9a6d2ca1b`**
(`git merge-base --is-ancestor ce51c36fc HEAD` → ANCESTOR_OK). No wrong-artifact-risk.
**loadavg** during measurement: 6.3–8.9 (18 CPUs); paired timings interleaved to null out drift.

---

## Question

Which equality carrier seats the HM(X) trunk on the greenfield root base — a union-find re-seed
that DELETES running code (**1A**), or the running lattice-CSP hardened additively (**1B**, the
equality-carrier seat)? Pass-1 tilted to 1A on the premise that the running CSP is "unsound as
shipped"; that premise rested on `TypeDomain::join` (`constraint/domain.rs:66`).

## Method

Two carriers, one live corpus set (`grammar/json/json.bbnf`, `grammar/css/pretty.bbnf`,
`grammar/google-sheets/google-sheets.bbnf`; regen runs all 9 in-tree grammars).

- **Baseline** = fresh `cargo xtask regen` at HEAD. (Note: the *checked-in* generated tree at
  ce51c36fc already drifts from a fresh regen — `bnf.rs`, `css_l4.rs`, `google_sheets.rs`,
  233 ins / 172 del — a pre-existing repo condition. The authoritative parity baseline is
  therefore the **freshly-regenerated** tree, hash `611d762c…`, snapshotted before any edit.)
- **Instrumented probe:** counted every `Some(existing) != Some(ty)` disagreement inside the
  live carrier `assign` (`constraint/revise.rs:27`) across a full regen.
- **Option B built and RUN:** refinement-aware clash arm on the live `assign`; regen → hash;
  `diff -rq` vs baseline; net LOC; paired cold timing; G-1B-5 corpus.
- **Option A NOT built (cost-bounded, not stubbed):** sized from the actual PROVE-08 source and
  the live delete/rewire census. A full structural-union-find port with byte-identical parity is
  not reachable inside the 50-min cap and — per the measurements below — not reachable *at all*
  without redefining 69 live sites; that impossibility is itself the finding.

## What ran (and the numbers)

| measurement | result |
|---|---|
| `TypeDomain::join` (`domain.rs:66`) call sites | **0** (`grep '\.join(' …/types/` empty; only doc-comment + other-domain refs) → **doubly dead** |
| live-carrier disagreements on the 3 corpora (regen, 9 grammars) | **69**, and **69/69 have `existing == BoxedEnum`**; **0** concrete-vs-concrete clashes |
| nature of every disagreement | coarse `BoxedEnum` placeholder **refined** to its structural type (`Tuple`/`Vec`/`Option`/`HeterogeneousAltJoin`) as the fixpoint resolves children — a lattice ascent, not an error |
| **Option B — byte-identical?** | **YES.** regen hash `611d762c7d3f6937b604ae7f724d566605c7f800d493e8bedb577de946790e29` == baseline; `diff -rq` on `generated/`+`runtime/` = empty |
| **Option B — runtime diff?** | **empty.** No `crates/runtime/` in the root tree (skinny-only path → trivially empty); generated runtime projections under `crates/core/src/runtime/` byte-for-byte unchanged |
| **Option B — net LOC** | **+18** as delivered (23 add / 5 del, one file; comment-dominated). Functional guard compresses to **net-0** (a `match` of equal length replaces the `if/else`) |
| **Option B — derive wall-time** (paired cold, 5 runs, warmup pair dropped) | **base median 1.43 s · optB median 1.42 s → parity** (pairs: 1.43/1.41, 1.42/1.53, 1.43/1.42, 1.44/1.41, 1.42/1.43; all within ±0.1 s noise) |
| **G-1B-5** `types_heterogeneous_alt` under B | **GREEN — 2 passed, 0 failed** |

### Net-0 functional form (the load-bearing half of B)
```rust
pub(super) fn assign(vars: &mut [Variable<TypeDomain>], var: TypeVarId, ty: TypeDesc) -> bool {
    let slot = &mut vars[var as usize].domain.solved;
    match slot.as_ref() {
        Some(e) if *e == ty => false,
        Some(e) if *e != TypeDesc::BoxedEnum => false, // B1: refuse concrete clash (dormant on corpora)
        _ => { *slot = Some(ty); true }                // None or BoxedEnum refinement (the carrier)
    }
}
```
Same line count as the incumbent `if/else` → **net-0 LOC**. The `TypeObligation::TypeClash`
recording B1 also asks for is **parity-inert** (codegen consumes `ir.types` + `struct_registry`,
never `type_obligations`) and **dormant** (arm never fires on these corpora), so wiring it through
the existing `ObligationSink` (drain at `mod.rs:473`) adds only diagnostic LOC and cannot perturb
the byte-identical result. The delivered patch keeps the arm explicit + documented (+18); the
terse net-0 form is behaviorally identical.

### Why literal-B1 as written would FAIL — and why that matters
The brief's literal B1 ("on `Some(existing)!=Some(ty)` … refuse the overwrite") refuses **all**
disagreements. Measured: that refuses all **69** `BoxedEnum→structural` refinements, freezes those
vars at `BoxedEnum`, and regresses generated output → **not byte-identical**. The correct hardening
is **refinement-aware**: `BoxedEnum` is the coarse placeholder and must still ascend; only a
disagreement between two *concrete* types is a genuine clash to refuse+record. On json/css/sheets
that concrete-clash arm is **dormant (0 firings)**, so B is byte-identical with a sound,
raise-on-clash channel installed. **The running CSP's last-writer-wins is not the defect pass-1
took it for; it is the load-bearing refinement carrier.**

### Option A cost (bounded from source, not built)
- PROVE-08's proven seed is a **flat** unifier: `Ty = Var(u32) | Con(TyCon)` with a **closed
  5-leaf** `TyCon` = {Bool, F64, U8Tag, StringDecoded, StringSpan}. It has **no** structural
  constructor (Tuple/Vec/Option/Sum/Row/BoxedEnum) and **no** refinement notion — `unify` yields
  `UnifyError` on any two distinct `Con`s.
- Live `TypeDesc` has ~18 variants incl. recursive structural ones. Growing the seed with Sum/Row
  **and** the structural arms means recursive unification + occurs-check — substantial **new** code
  with **no** proven seed (PROVE-08 supplies only the leaf half; its patch = 563 added lines incl.
  tests/lowering).
- Displacing the live carrier (as 1A requires) **deletes working, corpus-tested code**: `assign`
  (8 lines) + `join_types`/`effective_payload_type` (93 lines) and **rewires 11 `assign` call
  sites + 80 `HeterogeneousAltJoin` references** across `ir` + `core`.
- **Parity risk is concrete, not hypothetical:** union-find has no `BoxedEnum`-refinement analog,
  so `unify(BoxedEnum, Tuple([…]))` at each of the **69** live sites is a distinct-`Con` clash. A
  faithful port must *re-invent* the refinement lattice inside union-find — reproducing exactly
  what the running CSP already does. Net-positive LOC + deletes tested code + must rebuild the
  behavior it replaced.

## Verdict

**THE EQUALITY-CARRIER SEAT GOES TO 1B (the hardened running lattice-CSP).** Option B RAN:
byte-identical generated output (hash-matched + `diff -rq` empty), empty runtime diff, parity
derive wall-time (1.42 s vs 1.43 s), G-1B-5 green, and the load-bearing guard lands at **net-0 LOC**
additively. Option A's sole pass-1 justification — "the running CSP is unsound as shipped" — is
**void** (`TypeDomain::join` doubly dead; the live disagreement path is a sound
`BoxedEnum→structural` refinement, and the live clash path is the already-sound Alt-join →
`HeterogeneousAltJoin` obligation). 1A must now justify **deleting 101 lines of working,
corpus-tested code + rewiring 91 call/consumer sites + re-deriving the refinement lattice inside
union-find to re-clear 69 live sites** — for zero measured functional gain. On this evidence it
cannot. **Seat 1B; hold 1A's union-find as the stratified-*architecture* trunk only (residue
solver / let-generalization), never as the equality *carrier*.**

*Rules honored: Option B RUNS (not SPEC_ONLY). Perf is cold + paired + loadavg-recorded.
parse-that untouched (read-only; PROVE-08 read from `restart/.../prototypes/`). No x86. Parsimony:
the smallest probe — one instrumented regen + one carrier edit — settles the fork.*
