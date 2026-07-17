# PROTO-2 — LOCK-1 THE SOLVER SPINE, pass 2, probe 2 (X8, third call)

**Family:** 1B carrier hardening (X8, carrier-independent)
**Deliverable:** refinement-aware assign clash arm — raise on concrete-vs-concrete
disagreement without freezing legal BoxedEnum→structural refinements.

## BASE-PIN
- HEAD measured on: `ce51c36fcbdf5108eb028005586672d9a6d2ca1b`
- `git merge-base --is-ancestor ce51c36fc HEAD` → **DESCENDS-OK** (HEAD *is* the
  base-pin ce51c36fc; worktree is exactly at pin, not stale). No fast-forward needed.
- Numbers are **not** wrong-artifact-risk. loadavg at measure: `7.87 7.40 6.48`
  (functional test gate; no perf claim made — see below).

## QUESTION
X8 — the one carrier-independent deliverable — was reported unmet after two passes.
The brief states HEAD `revise.rs:8` is raw LWW
(`if slot==Some(&ty){false}else{*slot=Some(ty);true}`). The additive
raise-on-disagreement hardening is owed regardless of carrier winner: a naive
"refuse all overwrites" freezes ~69 legal BoxedEnum→structural refinements the four
production grammars depend on. Land a refinement-aware arm that refuses **only**
fully-ground-vs-fully-ground disagreement (recording one `TypeObligation::TypeClash`)
while permitting every lattice refinement.

## WHAT WAS FOUND AT PIN (probe-2 discovery)
The X8 implementation was **already fully staged in the index** at the pin across
four source files + one new test — a prior probe built it but had not committed, so
the brief's "raw LWW at revise.rs:8" description was stale relative to the working
tree. Probe 2's work was therefore **verification, not authoring**: drive every PASS
gate to green and confirm the arm is byte-safe and consumer-shaped.

Staged surface (`git diff --cached --stat crates/ir/`):
```
 constraint/domain.rs   |  26 ++  (adds `clash: Option<Box<(TypeDesc,TypeDesc)>>` side-channel)
 constraint/revise.rs   |  96 ++  (refinement-aware assign + is_fully_ground + record_domain_clashes)
 passes/types/mod.rs    |  25 ++  (post-propagation harvest: invert node_vars+vec_context_vars → TypeClash)
 obligation.rs          |  30 ++  (TypeObligation::TypeClash{node,lhs,rhs} + kind + describe)
 tests/type_clash_arm.rs| 131 ++  (unit fixtures for the arm)
 5 files, 298 insertions(+), 10 deletions(-)
```

### The arm (revise.rs)
`assign` now branches three ways on `domain.solved`:
1. `Some(existing)` where `existing == ty` → `false` (no-op, unchanged).
2. `Some(existing)` where `is_fully_ground(existing) && is_fully_ground(&ty)` and they
   differ → **refuse** the overwrite (keep incumbent so the fixed point does not
   oscillate) and stash `(existing, ty)` on `domain.clash`. Returns `false`.
3. else → permit (`domain.solved = Some(ty); true`) — byte-for-byte the historical
   LWW move. Covers first-assignment **and** every refinement (either side still
   carries an under-narrowed layout).

`is_fully_ground` treats `BoxedEnum` / `HeterogeneousAltJoin` / `Enum` / `Named` as
**non**-ground (Enum/Named are legal CSP bridges, not clashable), recurses through
Option/Vec/Tuple, and calls scalars + Span ground. `record_domain_clashes` (pub)
harvests each stashed pair into `TypeObligation::TypeClash` through the same
`ObligationSink` drained at `passes/types/mod.rs`.

## METHOD / WHAT RAN
1. `cargo test -p bbnf --profile ax-iter --test types_heterogeneous_alt
   --test project_types_json --test project_types_sheets --test project_types_css_l4
   --test project_types_bbnf` (single invocation, one target dir).
2. `cargo test -p bbnf-ir --profile ax-iter --test type_clash_arm` (the clash-arm
   unit fixture the brief's PASS line requires — a well-formed grammar never drives
   a fully-ground disagreement onto one variable, so byte-identity tests alone
   cannot exercise the refuse arm; this fixture does, via a raw
   `Csp<TypeDomain>` with two disagreeing persistent `GroundConstraint`s).
3. `git diff -- crates/core/src/runtime/ | wc -l` and the staged equivalent.

## THE NUMBERS
| Gate | Target | Result |
|------|--------|--------|
| clash-arm unit | `bbnf-ir --test type_clash_arm` | **3 passed / 0 failed** |
| — concrete_vs_concrete_emits_exactly_one_type_clash | F64 vs U8 → exactly 1 TypeClash, incumbent kept | ok |
| — boxed_enum_refinement_records_no_clash | BoxedEnum→F64, Tuple([BoxedEnum,U8])→Tuple([F64,U8]), Het→F64 all record 0 clashes | ok |
| — identical_grounds_record_no_clash | F64 vs F64 → 0 clashes | ok |
| types_heterogeneous_alt | `bbnf --test` | **2 passed / 0 failed** |
| project_types_json | `bbnf --test` | **10 passed / 0 failed** |
| project_types_sheets | `bbnf --test` | **4 passed / 0 failed** |
| project_types_css_l4 | `bbnf --test` | **6 passed / 0 failed** |
| project_types_bbnf | `bbnf --test` | **3 passed / 0 failed** |
| runtime carrier untouched | `git diff crates/core/src/runtime/` | **0 lines** (staged + worktree both 0) |

Total: **28 tests, 28 passed, 0 failed.** The four `project_types_*` grammars
compile and project green → the ~69 BoxedEnum→structural refinements STILL land;
none frozen. `concrete_vs_concrete_emits_exactly_one_type_clash` proves the
non-empty obligation stream carries **exactly one** `TypeClash` on a
fully-ground disagreement.

## PASS/FAIL EVALUATION (against the brief's four-part gate)
- [x] `types_heterogeneous_alt` green
- [x] all `project_types_*` emit byte-identical `ir.types` — refinements land, none frozen
- [x] concrete-vs-concrete fixture emits exactly one `TypeObligation::TypeClash`
      (non-empty obligation stream)
- [x] `git diff -- crates/core/src/runtime/` = 0 lines

## VERDICT
**PASS — RAN_MEASURED.** The refinement-aware assign clash arm is landed and
verified carrier-independently: fully-ground disagreement raises exactly one named
`TypeClash`; every lattice refinement (BoxedEnum / Het / under-narrowed compound
→ structural) passes through untouched; the carrier (`crates/core/src/runtime/`)
is byte-untouched. Ships regardless of the 1B/1C carrier winner.

## CO-NAMING NOTE — the joint wave owes 1C's consumer bridge
Grep at pin: the only references to `TypeObligation` / `TypeClash` outside
`passes/types/` are the `bbnf-ir` re-export (`lib.rs`) and the `GrammarIR`
field (`types/grammar.rs`). **No `type_obligations → lsp_types::Diagnostic{range}`
bridge exists in `crates/lsp` or `crates/analysis`.** Per Constitution viii the
clash arm is, until that bridge lands, **consumer-less substrate**: it records a
correct, deduplicated, node-attributed obligation that nothing yet lowers to a
user-visible diagnostic. The joint wave must co-name X8 (this probe) with 1C's
`type_obligations → lsp_types::Diagnostic{range}` bridge — the obligation's only
consumer. `TypeClash` already carries `node: Option<NodeId>` (harvested by
inverting `node_vars ⊕ vec_context_vars`), so the bridge has a range anchor to
lower against with no further arm change.

## DELIVERABLES
- `PROTO-2.md` (this file)
- `X8-refinement-clash.patch` — full staged diff of `crates/ir/` (5 files,
  298 insertions, 10 deletions), applies cleanly on ce51c36fc.
