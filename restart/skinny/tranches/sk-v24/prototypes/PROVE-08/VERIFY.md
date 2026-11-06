# PROVE-08 — ADVERSARIAL VERIFICATION

Verdict: **CONFIRMED_DELTA** — every structural / parity / edict claim reproduces;
the sole drift is the soft compile-time wall-time figure (167.5µs claimed → 231–241µs
measured across two fresh windows). No violation; nothing refuted.

Verifier tree: prover worktree `/Users/mkbabb/Programming/bbnf-lang/.claude/worktrees/wf_386900ed-462-8/skinny`
(base `d2b32f6e3`, patch staged, `target-tp` built). Main repo read only.

## Fresh numbers

| metric | claimed | fresh w1 | fresh w2 | loadavg | verdict |
|---|---|---|---|---|---|
| type_plane test | ok / 1 passed | ok / 1 passed | ok / 1 passed | 7.1→12.2 | reproduces |
| derive wall (µs, N=1/win) | 167.458 | 231.417 | 241.250 | 12.2 | **DELTA ~1.4x** |
| census arrows / rules / unify | 5 / 4 / 1 | 5 / 4 / 1 | 5 / 4 / 1 | — | exact |
| schema_hash | bd0ed1b60b1978e3 | bd0ed1b60b1978e3 | bd0ed1b60b1978e3 | — | exact |

The 167µs is a single-shot compile-time generator timing (HashMap + string parse +
first-call alloc), not a parse-perf win over a baseline — inherently noisy and load
dependent. Direction (sub-ms, compile-time, hundreds of µs) reproduces; the exact
magnitude does not. Hence DELTA, not CONFIRMED, on the number; everything else CONFIRMED.

## Attack list

1. **Warm/paired asymmetry** — N/A. No runtime binary changed; nothing to pair. The
   "win" is a compile-time derivation, so there is no cold-vs-warm parse contamination
   surface. Attack void by construction.
2. **Zero-runtime-cost claim** — HELD. `git diff --cached --name-only` = only
   `crates/codegen/{lib.rs, type_plane/derive.rs, hm.rs, mod.rs}` + `tests/type_plane.rs`.
   `codegen` is the generator crate; `runtime` is a separate crate, untouched. No runtime
   file in the patch → zero parse-path instructions added.
3. **Name-gate** — NONE. Only `.name ==` in the patch is
   `grammar.rules.iter().any(|r| &r.name == rule)` (filters arrows to real *rule* names,
   e.g. "bool"). No gate on grammar identity ("json"). pascal_case/module strings derive
   from grammar.name but never branch behavior on it.
4. **x86 / arch tokens** — NONE in the patch (no target_arch/sse/avx). aarch64-clean.
5. **Stubs / workarounds** — NONE (no todo!/unimplemented!/unreachable!/FIXME in
   type_plane). classify_arrow's bare-identifier→StringSpan fallback is a real
   classification default, not a stub. rule_defs is a legitimate offset lexer.
6. **Fact-fabrication** — REFUTED-as-attack: census arrows=5 comes from the *real*
   front-end `parse_runtime_source_facts` (frontend.projections + typed_projections),
   not a hardcoded constant. Genuine integration.

## The three walls — validity at base AND HEAD

- **Wall 1 (information / document shapes)** — REAL. `xtask/src/real_typed_schema.rs` =
  1014 LOC, **46** `struct_ty(` call sites (report said 45; off-by-one, immaterial),
  **19** roots (exact). Dense with document-specific shapes: capacity hints, unicode
  variant tables, map_entries, nested Vec<Vec<Vec<NumberString>>>. A generic json.bbnf
  (`pair*`, runtime keys) contains none of this. Wall holds.
- **Wall 2 (IR expressiveness / no sum type)** — REAL. `direct_schema.rs:52-58`:
  `DirectTypeKind` has exactly one variant, `Struct`. The grammar's natural
  `value = object|array|string|number|bool|null` sum has no IR home. Wall holds at base
  and HEAD (unchanged).
- **Wall 3 (`->` not in ExprKind at base)** — REAL and precisely attributed. At base
  `d2b32f6e3`, `ExprKind` = Seq/Alt/Repeat/Optional/Literal/Regex/Ref/Annotation — **no
  Project variant** (`git grep "Project {" d2b32f6e3` → empty). At `7f3be27c2` (the exact
  commit the report names as "later") and at current MAIN HEAD `660f3efe9`,
  `ExprKind::Project { body, target, ty }` EXISTS (`ir/src/lib.rs:261`). So threading the
  projected type through inference is post-base work, exactly as claimed. `ValueProject`
  at base is a *different* enum (a runtime step), not ExprKind — does not undercut the wall.

Greenfield consequence (verified sound): "schema from grammar" as an *equality* target
against a generic grammar is a category error. Reachable only via per-document typed
grammars + a sum-type node in DirectTypeKind — net-new architecture, not a threading fix.
The mechanism half (arrows→HM→validated schema + type_ref fill) is proven and reusable.

## Edict audit
aarch64-only ✓ · no name-gates ✓ · parsimony (558 additive LOC, colocated submodule,
no god module) ✓ · no stubs ✓ · no @generated hand-patch ✓. Zero violations.

## Note on HEAD
Task named HEAD `7f3be27c2`; actual main HEAD advanced during verification
(f6004995a → 660f3efe9, concurrent stage-3 commit). Both are past 7f3be27c2; the
Project-exists-at-HEAD finding holds regardless.
