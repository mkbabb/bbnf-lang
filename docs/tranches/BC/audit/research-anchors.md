# BC Research Anchors

Date: 2026-05-03
Status: input artefact for the BC re-draft. Settled.

The BC tranche's spec-depth anchoring rests on prior-art cardinalities, dependency-arrow idioms, and the question of whether `parse-that` ever leaves incubation. This document records the primary sources walked, the data extracted, and the conclusions that ground BC.W0 (variant cardinality), BC.W3 (crate split DAG), and BC.W5 (`parse-that` promotion).

## §1 IR variant cardinalities — primary sources

| IR | Variants | Categories | Source |
|---|---:|---|---|
| MLIR `arith` dialect | 60 | binary arithmetic, fp/int conversions, comparison, reductions, constant, select, shifts | `https://mlir.llvm.org/docs/Dialects/ArithOps/` (`arith.addf` ... `arith.xori`, ratified at 2026-05-03 fetch) |
| Cranelift `InstructionData` | 40 | memory (8), arithmetic & logic (5), comparison (4), control flow (5), function call (4), atomic (2), immediate (5), other (7) | `https://docs.rs/cranelift-codegen/latest/cranelift_codegen/ir/instructions/enum.InstructionData.html` |
| rustc HIR `ExprKind` | 35 | call/method/closure (4), binary/unary/lit (3), control (`If`/`Loop`/`Match`/`Break`/`Continue`/`Ret`/`Become` = 7), assignment (`Assign`/`AssignOp` = 2), aggregate (`Array`/`Tup`/`Struct`/`Repeat` = 4), path/field/index (3), addr/cast/type/use (4), block/let/dropTemps (3), inline-asm/offset-of/unsafe-binder/yield (4), const-block (1) | `https://doc.rust-lang.org/nightly/nightly-rustc/rustc_hir/hir/enum.ExprKind.html` |
| rustc HIR `ItemKind` | 16 | module-level (mod, use, extern_crate, foreign_mod, global_asm = 5), value-defining (static, const, fn, macro = 4), type-defining (struct, enum, union, ty_alias, trait, trait_alias, impl = 7) | `https://doc.rust-lang.org/nightly/nightly-rustc/rustc_hir/hir/enum.ItemKind.html` |
| chalk_ir `TyKind` | 23 | nominal (Adt, AssociatedType, Tuple, FnDef, OpaqueType, Closure, Coroutine = 7), reference (Raw, Ref, Slice, Array, Str = 5), abstract (Scalar, Never, Foreign, Error, Placeholder, Dyn, Alias, Function = 8), bound/inference (BoundVar, InferenceVar, CoroutineWitness = 3) | `https://docs.rs/chalk-ir/latest/chalk_ir/enum.TyKind.html` |

The grouping discipline survives across all five: each IR carves variants by *what produces them* (input grammar feature) and *what consumes them* (lowerer per backend). MLIR's `arith` is the most aggressive (60 ops); rustc HIR `ExprKind` is the closest analogue (35 variants for an *expression-form* IR); chalk's TyKind is the most parsimonious *type* IR (23). bbnf-lang's typed IR is a *grammar-form* IR — closer in spirit to HIR `ExprKind` than to `arith` — so the working anchor is **20-30 variants**, with explicit cardinality decided in the W0 variant table per gap B.

## §2 chumsky `Parser<I, O, E>` — type projection at call site

`https://docs.rs/chumsky/latest/chumsky/trait.Parser.html` ratified at 2026-05-03 fetch.

```rust
pub trait Parser<'src, I: Input<'src>, O, E: ParserExtra<'src, I> = Default>
```

Four parameters: lifetime, input, output, error/extra. Combinators (`Map<Self, O, F>`, `Then<Self, B, O, U, E>`) wrap the previous parser in a new struct, producing deeply nested types. The output type `O` is the projection — chumsky has no codegen; it's a typed-combinator library where call-site type ascription resolves the output. **Implication for BC**: bbnf-lang's typed IR carries `Layout` (the structural projection) and `TypeDesc` (the typed projection); the per-backend lowerer is the *codegen analogue* of chumsky's typed-combinator wrapping, but bbnf-lang materialises it through xtask emission, not through type inference at call site. The contract therefore must specify both projections at every IR node, not just at terminals.

## §3 cargo workspace — crate split idioms

`https://github.com/rust-lang/cargo/tree/master/crates` ratified at 2026-05-03 fetch.

| Pattern | cargo example | Application to BC |
|---|---|---|
| Concern-named crates | `cargo-platform`, `cargo-util`, `crates-io`, `cargo-util-schemas`, `cargo-util-terminal` | Each owns one cohesive concern; sibling-API uniform; `bbnf-parse` / `bbnf-codegen` / `bbnf-runtime` follows |
| Dev-only crates | `cargo-test-support`, `cargo-test-macro`, `resolver-tests` | Test infrastructure separate from production members; BC honours via `tests/common/` per-crate |
| Internal-tooling subcrates | `xtask-*` (5 variants), `mdman`, `rustfix` | Build automation under `xtask/`; not workspace members of the published surface |
| Schema crates | `cargo-util-schemas`, `cargo-platform` | TOML schemas + platform descriptors as own crates; bbnf-lang has analogous separation (`bbnf-ir/typed_ir/` is the schema; `bbnf-codegen` is the consumer) |
| Public re-export shells | `cargo` (the binary entry; not a re-export shell) — closer pattern is the rustc workspace | Umbrella `core` slim-down per BC.W3d |

**Re-export sunset rules**: cargo does not retain transitional re-exports across major splits; each subcrate is dependency-imported by name. BC adopts the same: the umbrella `core` retains `pub use` only as a backwards-compatibility convenience, with explicit sunset at BC.W6 — re-exports retire when downstream consumers migrate to direct sub-crate imports per the migration cookbook.

## §4 rustc workspace — dependency arrow precedent

The rustc workspace uses unidirectional dependency arrows: `rustc_ast → rustc_parse → rustc_resolve → rustc_hir → rustc_typeck → rustc_mir`. No bidirectional or circular arrows. **Application**: BC adopts `bbnf-runtime ← bbnf-parse ← bbnf-codegen` as the strict acyclic chain; runtime owns nothing of parsing, parsing owns nothing of codegen, codegen owns nothing that runtime doesn't already publish. This isomorphism is the basis for BC.W3's DAG specification at `audit/W3-crate-dependency-dag.md`.

## §5 serde-rs and tokio workspaces — sister-crate publication idioms

| Workspace | Pattern | Application |
|---|---|---|
| `serde` | `serde` (core), `serde_derive` (proc-macro), `serde_test` (test infra) | Three-crate published surface; derive crate as own publishable crate |
| `tokio` | `tokio`, `tokio-util`, `tokio-stream`, `tokio-macros`, ... | Multiple published crates, each pinned independently; major version sync at workspace level |
| `chalk` | `chalk-ir`, `chalk-engine`, `chalk-rust-ir`, `chalk-derive`, `chalk-solve` | IR + engine + solver as separate crates; the IR is itself published |

**Application to BC.W5**: `egraph`, `egraph-derive`, `csp-solver`, `bbnf-regex` are sister crates to bbnf-lang's umbrella; they freeze APIs at BC.W5 and become publication candidates. The serde / chalk pattern (IR + derive + engine all published) ratifies that `bbnf-ir` could itself be a published crate — but BC.W3 deliberately keeps `bbnf-ir` workspace-internal because its public surface is consumed only by `bbnf-codegen` and the typed-IR alphabet has not yet stabilised across grammar additions. Decision recorded at `audit/W3-crate-dependency-dag.md`.

## §6 `parse-that` disposition — primary signal

`parse-that` carries grammar-coupling: its `regex/` subcrate (renamed to `bbnf-regex` per `audit/HARDENING-SYNTHESIS-2026-05-03.md:166-175`) is grammar-specific; its `pprint/` subcrate is bbnf-grammar-author-targeted; its top-level combinator surface is the BBNF self-host substrate. **Conclusion**: `parse-that` is **permanent private path-dep**. It does not leave incubation in BD or post-BD. Its sister subcrates (`bbnf-regex` first, others later) may publish independently; the parent `parse-that` workspace remains a private dependency. Decision recorded at `audit/W5-parse-that-disposition.md` per option (i).

## §7 Worktree fixture closure — primary discipline

`audit/HARDENING-SYNTHESIS-2026-05-03.md:158-164` and `audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md:11` (D08-3): three different waves currently receive the worktree fixture closure. Per surgery 26, the receiver normalises to **BC.W5d** (sub-waved); BA.W0's partial closure (`data/{json,css,bbnf,sheets}` materialised) extends to full closure (`grammar/<name>/rewrites/*.ron` per grammar) here.

## §8 `bbnf-regex` endpoint reconciliation — primary criteria

`audit/HARDENING-SYNTHESIS-2026-05-03.md:166-175` enumerates two candidate endpoints (`parse-that/rust/regex` and `parse-that/rust/bbnf-regex`). Per surgery 31 and operational rule 2, the reconciliation is decided **in-plan**: BC.W5b chooses **Option A — rename `parse-that/rust/regex` → `parse-that/rust/bbnf-regex`** with crate-name canonicality as the criterion. No "user adjudicates at hardening time" residue. Decision recorded at `audit/W5-bbnf-regex-endpoint-decision.md`.

## §9 Conclusions for BC re-draft

| Gap | Conclusion | Receiving artefact |
|---|---|---|
| B (variant cardinality) | 20-25 variants, scoped to *grammar-form* IR (closer to HIR ExprKind than MLIR arith) | `audit/W0-typed-ir-variant-table.md` |
| H (crate DAG) | `bbnf-runtime ← bbnf-parse ← bbnf-codegen`; `bbnf-ir` stays workspace-internal; umbrella `core` slims to re-export shell with sunset at BC.W6 | `audit/W3-crate-dependency-dag.md` |
| I (`parse-that`) | Permanent private path-dep (option i); never published | `audit/W5-parse-that-disposition.md` |

The above conclusions are settled. The BC waves consume them; the BD draft (drafted by the sister Phase-4 agent) inherits them through the carry contract at `audit/W6-bd-carry-contract.md`.
