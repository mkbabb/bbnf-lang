# AZ-II Hardening Audit - 2026-04-29

Status: live coordination ledger for resuming AZ-II. This document
folds the parallel audit of B0-B7, AY-AZ-II, docs/instructions,
gestalt, remaining trajectory, risk/perf, meta-audit, and codegen paths
back into one current plan.

Repo head audited: `25693310`.

## Verdict

AZ-II remains open. The repository is not in a terminal direct-to-struct
state.

The current implementation is a partial close:

- StructDirect is live for 9/9 grammars after cutover.O2: JSON,
  Google Sheets, CSS L4, BBNF, CSV, Math, BNF, CSS Pretty, and EBNF.
- EBNF now returns `EbnfDocument<'_>`; generated tape-view residue,
  `Parsed<R>`, and `TapeDirect` fallback semantics remain deletion
  blockers.
- `Parsed<R>`, tape cursor/view code, and `crates/tape` remain live
  runtime substrate.
- BBNF's public `grammar::parse` path returns `BbnfDocument`, but the
  canonical entry still routes through `bootstrap_parser::parse`; the
  regen-derived `BbnfBootstrap::parse` is not yet the self-hosted
  compiler entry point.
- Tooling modernization from B0-B7 landed materially, but bench aliases,
  profiling scripts, IAI CI, and some docs have drifted from the actual
  bench feature matrix.

This is not an AZ-III by default. The active work is AZ-II
`cutover.O`, a terminal hardening wave. O0 tooling preflight, O1
StructDirect builder transactions, and O2 EBNF direct projection have
landed; the active gate is O3 generated view purge. Open AZ-III only if
a later `cutover.O` gate proves that new grammar-general
inference/layout substrate cannot land inside AZ-II without hiding a
larger architectural transposition.

## P0 Gates

### Transactional StructDirect Builder ABI

The most important root finding is not EBNF-specific. StructDirect
speculative parsing restores input position but does not restore builder
state. Alternate, repeat, minus, and negate emission paths can leave open
frames behind after a failed speculative branch.

Evidence:

- `StructBuilder` has no checkpoint/rollback/commit API.
- StructDirect emitters restore `*p` around speculative branches, but not
  the grammar-specific builder stack, pending value, root, or arena
  cursor.
- A focused projection-totality probe failed on CSS with
  `CssStructBuilder::finalise called with 3 open frame(s)`.

Required close:

1. Add grammar-general transactional builder support:
   `checkpoint`, `rollback`, and `commit`, or an RAII speculative scope.
2. Capture all state that can be mutated by branch attempts: open-frame
   stack, root slot, pending leaf/compound state, branch tags, and arena
   cursors.
3. Teach every speculative StructDirect emitter path to use the
   transaction. Do not patch CSS, EBNF, or BBNF locally.
4. Add a wire-contract test that forces a failed branch to mutate the
   builder and then proves finalise is clean after rollback.

Disposition: LANDED in cutover.O.1. `StructBuilder` has
checkpoint/rollback/commit support; grammar-specific builders restore
arena cursors, open-frame stacks, roots, next handles, and CSS pending
value state; speculative StructDirect emitter paths are transactionally
wired. Focused JSON and CSS wire-contract tests cover failed-branch
rollback. The remaining tape-abrogation blockers are generated view
purge, `Parsed<R>` / `TapeDirect` deletion, and crate deletion, not
builder-state leakage.

### EBNF Direct Projection

Disposition: LANDED in cutover.O2. EBNF is no longer a TapeDirect
grammar; the resolver routes it to `EbnfStructBuilder`, and generated
EBNF parse returns `EbnfDocument<'_>`.

The known blocker is grammar-general: high-branch literal alternates
(`letter`, `digit`, `symbol`) expose layout-routing depth that the
current AltDispatch + struct layout path does not satisfy.

Landed close:

1. High-branch literal alternates route through StructDirect
   AltDispatch with `u32` branch tags.
2. Structural `Seq` AltDispatch branches route through the shared
   StructDirect inline branch walker, preserving nested children and
   source/trivia positions through the same emitter machinery used by
   Keyword structural branches.
3. EBNF flips through the normal resolver path, not an EBNF carveout.
4. `EbnfParser::parse -> EbnfDocument`.
5. EBNF parse, serialize, and typed-accessor tests exercise the parse
   path and fail if the resolver silently falls back to tape.

### Tape and Parsed Deletion

Tape deletion is not mechanical yet because StructDirect generated files
still carry tape-shaped view/materializer surfaces and because EBNF still
uses the tape return path.

Required close:

1. Remove `Parsed<R>` from the production return model.
2. Remove `TapeDirect` as a fallback. Unknown grammars should fail
   generation loudly instead of silently selecting tape.
3. Delete or relocate the genuinely general scan/index primitives. Do
   not leave a public tape runtime under a new name.
4. Delete `crates/tape` from the workspace once the last production
   consumer is gone.
5. Gate with `rg` scans over `Parsed`, `TapeDirect`, `TapeCursor`,
   `TapeRec`, `TapeOffset`, `runtime::tape`, and `crates/tape`
   outside historical docs.

## Underwired or Consumerless Substrate

These surfaces are either live blockers or must be explicitly deleted
before declaring terminal close:

| Surface | Current state | Required disposition |
|---|---|---|
| Generated tape views for StructDirect grammars | `BbnfBootstrapNodeView`, `TapeCursor`, `ValueRoot`, and related view helpers still appear in generated files even when parse returns a concrete document | Either route document `to_value` through a consumed projection API or stop generating tape-backed view/materializer artifacts for StructDirect |
| `bootstrap_parser.rs` | Hand-written BBNF bridge is the public compiler entry path | Keep only as a bounded bootstrap bridge with an explicit retirement gate; generated `BbnfBootstrap::parse` must become canonical again |
| `crates/gorgeous/src/jit.rs` | Still references published `bbnf_derive` and emits derive-shaped temp projects | Rewrite onto `cargo xtask regen` or delete the JIT surface if it is no longer product-aligned |
| CSP strategy constraints | Comments describe tier/engine/parent constraints, but install path only wires engine constraints; `shape_dict::install` is effectively no-op | Wire the documented constraints or delete the claims/scaffold |
| Type solver fallback | unresolved rule variables are cycle-broken to `BoxedEnum` | Replace with recursive type/SCC obligations and explicit ambiguity/conflict states |
| `TypeDomain` vs `TypeDescInterner` | CSP domain carries `Option<TypeDesc>` while interner says solver should carry `TypeDescId` | Move interned type IDs into the solver hot path |
| Egraph facts | compile pipeline saturates, writes back, then drops egraph; later passes recompute eclass-like facts | Persist eclass/projection facts once and consume them across materialization, dispatch, type projection, and layout |
| Regex/CSP decisions | choices are solved per occurrence but collapsed per pattern | Carry decisions by call site or generated decision class |
| Prettify/default emitter stubs | several default methods still `unimplemented!` | Either implement through the canonical emitter path or remove unreachable surfaces |

## Tooling and Bench Truth

B0-B7 delivered the intended runway: pinned nightly, nextest, divan,
xtask regen, canonical generated source, and broad cross-repo
modernization. The pre-O0 audit found proof-surface drift:

- `.cargo/config.toml` bench aliases reach feature-gated benches without
  always enabling the required features.
- `bench-iai.yml` calls `json_callgrind` with the old `iai` feature and
  references a missing `scripts/iai-compare.sh`.
- `scripts/prebuild-benches.sh` and
  `scripts/prepare-profile-wave.sh` reference the old
  `json_monolithic_value` bench and stale `generated.rs` freshness
  checks.
- `scripts/bench_regression.sh` is legacy: old directory layout, old
  bench names, and old output parsing.
- Release workflow floats nightly instead of honoring the repository
  toolchain pin.
- The AZ-I/AZ-II 17-entry bench matrix is stale; Sheets and CSS close
  artifacts include abort/stack-overflow or placeholder entries.

cutover.O.0 disposition: the command-surface drift above was repaired
or de-canonicalized before O1. Bench aliases now activate their required
feature tiers, IAI uses the `callgrind` feature and a tracked compare
script, profiling prep names `json_value` and checks per-grammar
generated modules for freshness, the release workflow installs the
repository toolchain pin, and the legacy `bench_regression.sh` wrapper
was deleted. This does not refresh the AZ-II performance baseline; O6
owns fresh JSON/CSS parity and the 17-entry close matrix.

Required close before BB optimization:

1. Compile all bench targets by feature tier.
2. Refresh JSON `sonic-rs` semantic parity and throughput.
3. Refresh CSS `lightningcss` typed semantic parity and throughput.
4. Publish a post-`cutover.O` benchmark artifact instead of carrying
   AZ-II partial placeholder numbers.

## Inference Direction

The project direction is aligned with SOTA: grammar lowering, fact
mining, egraph normalization, CSP/type projection, then direct struct
projection. The gap is authority and persistence of facts.

Adopt the following model:

- Type projection should be an obligation engine: goals, candidates,
  assumptions, returned constraints, ambiguity, and errors, not direct
  `TypeDesc` assignment with fallback.
- Egraph facts should become a durable semantic database consumed by
  later passes.
- CSP should model interacting choices globally: layout, tag width,
  dispatch mode, recognizer tier, shape-dict admission, and field order.
- Synthesis tools are validation/research aids only. Any inferred rule
  must have firing data, parity tests, and a production consumer before
  landing.

Primary references used for this direction:

- [OutsideIn(X): Modular type inference with local assumptions](https://www.microsoft.com/en-us/research/publication/outsideinx-modular-type-inference-with-local-assumptions/)
- [Rust trait solving docs](https://rustc-dev-guide.rust-lang.org/solve/trait-solving.html)
- [Chalk](https://github.com/rust-lang/chalk)
- [egg: Fast and Extensible Equality Saturation](https://arxiv.org/abs/2004.03082)
- [egglog: Better Together, Unifying Datalog and Equality Saturation](https://effect.systems/doc/pldi-2023-egglog/paper.pdf)
- [OR-Tools CP-SAT](https://developers.google.com/optimization/cp/cp_solver)
- [MiniZinc global constraints](https://docs.minizinc.dev/en/2.5.5/lib-globals.html)
- [Ruler: Rewrite Rule Inference Using Equality Saturation](https://ztatlock.net/pub-2021-oopsla-ruler.html)
- [SyGuS 2.1 standard](https://sygus-org.github.io/assets/pdf/SyGuS-IF_2.1.pdf)

## cutover.O Plan

`cutover.O` should stay inside AZ-II and close these gates in order.
Each row is now a dispatch-ready wave spec with up to 10 parallel
fully-contained sibling worktree agents and explicit file bounds:

1. **[O0 - Tooling preflight](../waves/cutover.O0.md).** LANDED: stale bench aliases/scripts/IAI
   CI were fixed or marked noncanonical before using them for close
   evidence.
2. **[O1 - Builder transactions](../waves/cutover.O1.md).** LANDED: shared speculative builder
   ABI is wired through StructDirect speculative emission sites.
3. **[O2 - EBNF generalization](../waves/cutover.O2.md).** LANDED: shared structural
   AltDispatch emission flips EBNF to `EbnfDocument`.
4. **[O3 - Generated view purge](../waves/cutover.O3.md).** in_progress: stop emitting tape-backed view and
   `ValueRoot` materializer artifacts for StructDirect grammars unless
   they are consumed through a document API.
5. **[O4 - Parsed/TapeDirect deletion](../waves/cutover.O4.md).** Remove `Parsed<R>` as a
   production parser result and delete `TapeDirect` fallback semantics.
6. **[O5 - Tape crate deletion](../waves/cutover.O5.md).** Remove `crates/tape` after relocating
   only non-tape scan/index primitives to their natural owner.
7. **[O6 - Bench/parity close](../waves/cutover.O6.md).** Refresh the 17-entry matrix, JSON
   `sonic-rs` parity, CSS `lightningcss` typed parity, and publish the
   terminal `post-AZ-II.json`.
8. **[O7 - Final conversion](../waves/cutover.O7.md).** Convert AZ-II FINAL from PARTIAL CLOSE to
   terminal close only after the gates above pass.

BA and BB remain blocked on the terminal close. BB.scaffold may exist,
but optimization work must not hide structural incompleteness.

## Archive Decision

The following directories should leave the active tranche-planning
surface:

- `docs/tranches/meta-audit/`
- `docs/tranches/next-tranche-research/`

They are historically useful and were consumed by B1/B7/AZ planning, but
they are stale as live planning canon. Do not hard-delete them in place
without rewriting inbound references. The safe sequence is:

1. Keep short archive markers in the existing directories.
2. Extract still-open debt into this ledger, `GESTALT.md`,
   `REMAINING-TRAJECTORY.md`, and `RISK-PERF-MATRIX.md`.
3. Move the directories to `docs/audit/archives/2026-04-22-meta-audit/`
   and `docs/audit/archives/2026-04-23-next-tranche-research/`.
4. Rewrite inbound links from `docs/audit/meta-audit-2026-04-23`,
   `docs/GESTALT.md`, `docs/META-AUDIT-PROMPT.md`, and tranche B docs.
5. Delete the original active paths only after link rewrite is complete.

Until then, treat them as read-only provenance, not planning authority.
