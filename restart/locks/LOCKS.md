# SK-V9 Scoped Lock 14 Allowances

- `sk-v9-real-typed-w1` permits the SK-V9 W1 parent diff only for the
  Apache/CITM measured typed row-table admission. The scoped owner paths are
  `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`, `skinny/RESULTS.md`,
  `skinny/crates/bbnf-bench/target/skv9-w1/criterion/` (out-of-band capture),
  `skinny/REDRESS.md`, `restart/skinny/tranches/sk-v9/HANDOFF.md`, and this
  `restart/locks/LOCKS.md` allowance text.
- The allowance does not authorize grammar, runtime, codegen, SIMD, fixture,
  direct-output, generated typed-output, or generic-crate behavior changes.
  `canada/real_typed_struct` remains blocked pending the full-fixture
  DirectBuild-vs-serde checksum proof.
- Evidence: `skinny/RESULTS.md` run id
  `sk-v9-open:criterion-fnv64-a1e8a51ae806d386`, Apache
  `real_typed_struct A / GO` at 8174 Mbps, and CITM
  `real_typed_struct A / GO` at 35102 Mbps.

# Hardening pass — plan set

You are auditing a freshly drafted plan set for the bbnf-lang BA-restart. The plan composes BA / BB / BC (and optionally BD+) tranches that re-architect the parser fleet from first principles toward grammar-agnostic, multi-backend, sonic-class direct-to-struct parsing.

The plan exists. Your task is to challenge and harden it: ratify what is sound, surface what is unsound, identify what is missing, and recommend what must change before execution begins.

You are not implementing. You are auditing.

## Subjects

The plan documents under audit:

- `docs/tranches/BA/BA.md` — the restart tranche
- `docs/tranches/BA/waves/W*.md` — wave-level specifications
- `docs/tranches/BB/BB.md` — successor tranche
- `docs/tranches/BC/BC.md` — successor-of-successor tranche
- Optionally: `docs/tranches/BD/BD.md` if drafted (TS/WASM emergence)

Read each end-to-end before producing any audit output.

Read also for context (do not audit, but use as ground truth):

- `audit/HARDENING-SYNTHESIS-2026-05-03.md` — codebase audit synthesis from the prior pass
- `audit/SOTA-2026-05-03.md` — sonic-rs / simdjson / lightning-css research
- `audit/CENSUS-2026-05-03.md` — kill-list of grammar-specific code, tape residue, dupes, god modules
- `audit/MODULES-2026-05-03.md` — per-file fates and 17-step pipeline ordering
- `audit/RESTART-SKETCH-2026-05-03.md` — JSON parse trace + post-restart pipeline sketch
- `docs/HARDENING-AUDIT-PROMPT.md` — codebase-audit prompt (for methodological symmetry)

## Gestalt — sixteen locks

The plan must reflect these sixteen architectural commitments faithfully. Any wave that violates one is a fault. Locks 1–14 are the original architectural commitments; Locks 15 and 16 land 2026-05-12 after the V9.2 lazy-tape refutation and the six-agent comparative-profile cohort (see `restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md` for the empirical synthesis).

1. **Tape is the substrate, properly unioned with direct-to-struct; columnar SoA is dead; orthogonal codepaths and parallel substrates are dead.** Tape is the greenfield's parsed event projection, unioned with direct-to-struct typed values that borrow into it (`&'i Tape<'i>` + cursor) when a retained document exists. The projection may be an offset tape, event tape, or collapsed-stage event sink; direct-only `SinkOnly` retains no queryable document identity. The 2,000-commit prior failure was implementation, not concept: orthogonal codepaths (the Vec<OpenFrame>::clone parallel substrate that produced the 86.07% samply pathology); type ambivalence (tape and OpenFrame and direct-to-struct competing for the same role); substrate-first/consumer-later (Era V failure mode); columnar SoA designed in AV.04 archaeology but never activated. The greenfield's tape lives at `runtime/src/tape/`; typed-value records borrow into it; per-grammar runtime modules (template-emitted at `runtime/src/grammars/<name>/`) emit accessors; one materialisation surface; one Visitor pattern; no parallel substrate. A SIMD mask stream is a transient producer, not a retained sidecar; if structural offsets are retained, the structural projection IS the tape. Columnar SoA stays buried. Plans that resurrect parallel substrates (OpenFrame ladders; columnar SoA; type-ambivalent dual representations) or implement tape with consumer-later sequencing are faults; plans that implement tape properly with same-wave consumer wiring + direct-to-struct union are honoured. **2026-05-04 reframe**: the prior restart's wholesale retirement of the tape name was an over-correction against the implementation failure; the user has confirmed tape is the right substrate when implemented properly. Lock 1's spirit (no parallel substrate; no orthogonal codepath; no Vec<OpenFrame>::clone pathology) holds; the no-rename clause is amended.

2. **Layout lowering is the canonical IR pass name**. The term replaces *type projection / type collapsing / type inference / type elaboration / TypeMap / StructLayout / TypeDesc / schema synthesis* everywhere. Old terms appear only in archived docs. The IR module is `passes::layout` (canonical unprefixed path under the `passes` crate per the README's workspace shape); the IR record is `Layout`; the trait that consumes it is `LayoutSink`. HM/CSP type checking is a subroutine of layout lowering, never a public peer pass; `LayoutFacts` is the public side-table. Any plan section referring to a retired term, to the stale `bbnf-ir/` prefix, or to `TypeFacts` as a public artefact is a fault.

3. **Cursor-parse + byte-skip unified, with cursor branch elided when path is empty**. One parse implementation. Cursor consultation generates byte-skip when consult returns `Skip`. The empty-path case (`__EAGER_EMPTY_PATH`) elides cursor calls entirely so the eager fast path pays no consultation cost. Any plan that bifurcates byte-skip and cursor-parse into two implementations is a fault.

4. **Per-domain orthogonal optimization**. CSP type/layout inference, e-graph rewriting, pattern miners, shape analysis, and cost model compose by output-piping. No unified hypergraph. Each lives in its own crate (egraph + csp-solver path-deps until stable). Egglog-style Datalog/equality-saturation fusion is a known SOTA pressure, not an omitted option; V1 rejects that fusion because diagnostics, public proof records, monotone bridge boundaries, and independent stabilization gates must stay owned by the domain that produced them. Fusion remains a post-V1 research comparison, not the governing architecture. Any plan that fuses CSP and e-graph into one solver is a fault. **V1 type system folds higher-rank polymorphism via DK13 algorithmic completeness (Dunfield-Krishnaswami 2013); GADT user-facing surface lands V1: pattern-match arms admit branch-local-equality refinements (`Pattern @ where T = U -> Block` per the §6 BBNF grammar amendment); OutsideIn(X)-style implication constraints solved at `passes/types/` carry the equalities through to `LayoutFacts`; the user-facing diagnostic `BBNF-LOCAL-EQUALITY-ANNOTATION` is emitted when a match-arm refinement annotation is missing or ill-typed. Closures capture by `&'i` reference only; capture-by-move is forbidden in V1; `Fn` / `FnMut` / `FnOnce` discrimination is not exposed at the BBNF surface in V1.**

5. **IR + per-backend lower**. Codegen emits a backend-agnostic typed IR; per-backend lowerers produce native source. There is no source-emit-per-backend duplication; there is no trait-based emitter walking grammar directly. The IR is the contract. Any plan whose Rust codegen and TS codegen do not share an IR is a fault. **TS and WASM backends defer post-V1; V1 ships the Rust impl only via the formal `Backend` trait at `restart/ARCHITECTURE.md` §7.5. The trait enforces this lock's per-backend boundary and enables seamless V2 addition of `WasmBackend` and `TsBackend` without re-architecting BIR or codegen.**

6. **xtask emits committed source artefacts**. No proc-macro façade. css_l4.rs at 107 K LOC is greppable on disk. Build is fast incremental because expansion is not at compile time. Any plan that proposes proc-macro for codegen output (other than the `path` / `path-ts` proc-macro shells, which are different) is a fault. **The egraph crate has no direct dependency on csp-solver; the bridge surface lives at `passes::bridge` and is invoked by passes that compose both crates. egraph and csp-solver compose by output-piping, never by import — Lock 4's per-domain orthogonality holds at the dependency-graph level as well as the algorithmic level.**

7. **`crates/path/` is the consolidated path crate**. The runtime cursor engine merges INTO it; the existing `crates/core/src/path/` directory empties. The Rust `pointer!` proc-macro lives here. **`crates/path-core/` (non-proc-macro) exists as a published sibling crate at V1 J.W3, alongside `crates/path/` (the runtime + Rust proc-macro shell) and `crates/parse-that/` (the parser combinator + regex family) — `path-core` is the sole deduplication mechanism for the path-AST + compile logic. `crates/path-ts/` defers post-V1 alongside the TS-native parse+runtime fork.** Any plan that names `crates/bbnf-path/` (with prefix) is stale; any plan with three proc-macro shells is a fault.

8. **Surpass sonic-rs / simdjson / lightning-css**. AU is never mentioned. Every perf gate names a specific competitor's number on a specific dataset on a specific platform. simdjson On-Demand 7 GB/s (JSON parse). sonic-rs M1 Pro twitter 436 µs (parse-to-typed-struct). lightning-css 4.16 ms Bootstrap (CSS). Plans that reference AU's bench numbers are stale. **V1 SOTA close gates measure the Rust-line only at H.W3 and H.W4; WASM SOTA defers post-V1. The H tranche carries five waves (H.W0-H.W4) after the V1-FOLD-CANDIDATES Tier 4 wave-count drop. No measurement-pending WASM anchor lands in V1; the WASM lower-and-bench programme awaits the V2 `WasmBackend: Backend` impl.**

9. **Slice-borrow primary; bumpalo + owned escape hatches**. Default API is `&'i str` slices + `Cow<'i, str>` for transformations (lightning-css model). Bumpalo arena is opt-in via `parse_in(input, &bump)` (sonic-rs model). Owned (no-borrow) is opt-in via `parse_owned(input)` (serde-json escape). The three are surfaces over the same parse implementation; the lifetime parameter is the discriminant. Any plan that allocates eagerly into bumpalo without justifying why borrowing won't suffice is a fault.

10. **Pratt + SIMD auto-detected; materialization plan also auto-detected via cost model**. No `@pratt` or `@simd` directives. Optimizer mines grammar shape (left-recursive operator chains → Pratt) and leaf-pattern shape (charclass / keyword set / regex → SIMD scanner) and emits accordingly. Cost model decides when SIMD overhead is worth the dispatch cost AND derives `LayoutFacts.backend_shape ∈ {EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` per-rule from existing Grammar IR facts (first-set disjointness, output mode, transitive `@error(recover)` use, `@host fn` decoded-at-parse, `@layout` scope, target features) per ARCHITECTURE.md §7.3's 8-priority decision tree. No new BBNF directive carries the materialization plan; `backend_shape` is a side-table field, not a surface annotation. Any plan that requires grammar authors to annotate Pratt, SIMD, or materialization plan is a fault. **The V1 BBNF grammar formalises six directives: `Directive = ImportDecl | HostFn | ErrorDecl | LayoutDecl | PrettyDecl | TokenDecl ;` — replacing the prior three-directive form `Directive = HostFn | ErrorDecl | LayoutDecl ;`. `@import` carries cross-file grammar composition; `@host fn` carries typed host primitives; `@error(recover = ...)` carries recovery vocabulary; `@layout` carries layout policies (with `@ws` folded into `@layout(ws = ...)`); `@pretty` carries pretty-printing strategy with the verbatim vocabulary `compact`, `group`, `indent`, `hardbreak`, `sep(...)`, and `block` preserved from the 30+ extant grammar sites; `@token` carries atomic-token markers binding to the BIR scanner. `@debug` is a host primitive, not a directive. Standalone `@recover` retires; absorbed by `@error(recover = ...)`. `@pratt`, `@simd`, `@transducer`, `@rewrite`, and `@unicode` retire. **BBNF V1 also includes function values + lambda literals (`|x| body`) + closure capture by `&'i`; function types `fn(T) -> U` are first-class members of the `Type` non-terminal.**

11. **Path-deps for incubating sister crates**. egraph + egraph-derive + csp-solver + parse-that (with `parse-that-regex` as its regex sub-crate) as path-deps in workspace until each API stabilises; promote to registry once stable. bbnf-simd + bootstrap + analysis + lsp stay workspace-internal. ser + gorgeous archive at `archive/<crate>/`, removed from workspace, source preserved. **`parse-that` is the canonical name for the published parser combinator + regex family; the legacy `bbnf-regex` crate renames to `parse-that-regex` and publishes as such. The `crates/bbnf-regex/` directory rename to `crates/parse-that-regex/` is a follow-up commit; documentation uses the new name now.**

12. **ser + gorgeous archive BEFORE A.W0**. Clean slate is the precondition for the A tranche to begin. Any plan that interleaves the archive ceremony with A waves is a fault. **The archive ceremony cites `pre-restart-2026-05-04` as the source-of-truth tag; legacy `BA-` / `BB-` / `BC-` / `BD-` slot drift retires under the canonical `A-` / `B-` / `C-` / `D-` tranche letters.**

13. **No god directories; cohesive encapsulation at every level**. Every directory partitions one cohesive concern; siblings are peer partitions of that concern; sub-modules express finer partitions. Per-level surface APIs are uniform across siblings. The standard is set by sonic-rs (`src/{parser, value, serde, util, lazyvalue, ...}`), lightning-css (`src/{rules, properties, selector, declaration, traits, ...}`), and simdjson (`{dom, ondemand, generic, ...}`): each top-level directory names a concern, expresses it through 4–10 children at the next level, and each child carries a uniform sub-API (e.g., every property module exports `Property` enum + `parse` + `print` + `Visit` impls). bbnf must match this discipline. A 16-sibling directory mixing per-grammar subdirs with generic mechanism files (e.g., today's `crates/core/src/runtime/`) is a god directory and is a fault. Files >500 LOC outside `generated/` are forbidden; directories with >10 immediate children mixing concerns are forbidden; sibling-API divergence (one module exports `parse` + `emit`, the next exports `compile` + `walk`) is forbidden.

14. **Full grammar generalisation; zero overfitting**. The substrate carries ZERO grammar-specific code. Every grammar plugs into the fleet via three declarative surfaces only: (a) a grammar source file (`<name>.bbnf`), (b) workspace metadata declaring its strategy (recognisers, host fns, output-dir, pratt eligibility, simd eligibility, etc., per Lock 5's IR contract), and (c) optionally a per-grammar declaration crate (`crates/<grammar>/`) carrying host-fn implementations. Generic crates — `bbnf-parse`, `bbnf-codegen`, `bbnf-runtime`, `bbnf-ir`, `path`, `path-core`, `egraph`, `csp-solver`, `parse-that-regex`, `parse-that`, `bbnf-simd`, `analysis`, `lsp` — carry ZERO `match grammar { Json => ..., CssL4 => ..., ... }` arms; ZERO grammar-named modules; ZERO grammar-specific types in their public APIs; ZERO per-grammar feature flags. Per-grammar runtime modules (value, document, view, kind) are emitted from a single grammar-agnostic generator template that consumes (grammar source + workspace metadata) and produces typed Rust; hand-written per-grammar runtime files are forbidden. Per-grammar deviations (CSS L4 colour-function emit; BBNF Pratt operators; Sheets array literals) are encoded in the grammar metadata + source, NOT in branching code in any other crate. Adding a new grammar is a config + grammar-source change with NO code change in any generic or other-grammar crate. The current overfitting mess — CSS L4 14-variant `OpenFrame`; BBNF aggregator `pub use bbnf::*`; Sheets arena fallbacks; per-grammar registry arms in `bbnf-ir`; `shape_dict_bbnf.rs`; `crates/core/src/css_types.rs`; per-grammar runtime/<g>/ hand-written modules — is the failure mode this lock prevents from recurring. Any plan, tranche, wave, or commit that introduces grammar-specific code in a generic crate, or any new hand-written per-grammar runtime file, is a fault regardless of its other merits. Verification commands: `rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,parse-that-regex,parse-that,bbnf-simd,analysis,lsp}/src/` returns ZERO; `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` returns ZERO per-grammar dirs (all generated from template); `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' crates/` returns ZERO matches in non-generated source under generic-crate paths.

15. **Build-profile discipline + fusion discipline + i-cache residency: LTO + codegen-units = 1 + force-inline hot leaves + ~20 KiB hot-function ceiling**. Every bbnf-generated runtime crate declares `[profile.release] lto = "fat"` (verify via `cargo build --release -v 2>&1 | grep '\-C lto'` returns `lto=fat`, not `lto=thin`; per `skinny/profile/skinny-expanded/PROFILE-REPORT.md` the binary built with `lto=thin` regressed twitter throughput 11780→5521 Mbps — Lock 15 enforcement gap), `codegen-units = 1`, `panic = "abort"` (or `"unwind"` only if the grammar carries `@error(recover = ...)`), `debug = true` (per `feedback_samply_symbols` for samply-resolvable profiles). Three co-load-bearing dimensions, each falsifiable:
    - **LTO + codegen-units = 1**: sonic-rs's INLINED hot-leaf count is 1-2 because LTO fuses the entire SIMD inner kernel into `parse_object`/`parse_array`; without LTO sonic-rs's NOINLINE wall-clock falls 2.1-3.2× on M5 Max (`skinny/profile/sonic-rs-v2/PROFILE-REPORT.md`). The codegen template inversion yields only half its gain without LTO.
    - **Force-inline hot leaves**: yyjson achieves 0.91 c/B twitter (29.5K Mbps on M5 Max, beating simdjson DOM's 1.142 c/B and 24K Mbps) WITHOUT SIMD via `always_inline` everywhere and the entire parser fitting in ~18 KiB i-cache resident (`skinny/profile/yyjson/PROFILE-REPORT.md`). The codegen template emits `#[inline(always)]` on Grammar IR's call-graph hot path (rules with self-time + transitive-call self-time ≥ threshold per `LayoutFacts.hot_call_graph`, cost-model-derived from prior bench runs); the threshold is grammar-agnostic and not a directive. Diagnostic `BBNF-FORCE-INLINE-MISSED` fires when a rule mined as hot-path lacks `#[inline(always)]` in generated output.
    - **I-cache budget**: target hot-function size ≤ 20 KiB post-LTO. Diagnostic `BBNF-ICACHE-BUDGET-EXCEEDED` fires when `cargo asm` reports fused-function size exceeds budget (yyjson reference: ~18 KiB). **Budget already met for JSON** per Wave 2 Agent 3 evidence (`skinny/profile/wave2-asm/PROFILE-REPORT.md` Appendix C): `parse_value_at` is a single 7,304-byte hot function (RVA `0x2460..0x40e8`, 1,826 mnemonics) under the current workspace `[profile.release]` (`opt-level=3`, `lto=thin`, `codegen-units=1`, `debug=true`). Sub-budget; the open question is whether `lto=fat` enforcement lands the budget-overrun warning for other grammars at codegen time.
    - The discipline applies to (a) the bbnf-generated runtime, (b) the `bbnf-simd` primitive crate, (c) any user-side crate consuming the generated parser in throughput-sensitive contexts.
    - Workspace metadata `[workspace.metadata.bbnf.grammars.<name>.profile]` carries optional per-grammar overrides only with a documented measurement justification. Plans that ship release profiles without LTO are faults; verification: `cargo build --release -v 2>&1 | grep -E '\-C lto=(fat|true)' | wc -l` must equal the number of workspace members.

16. **SIMD/ASM admissibility allowlist; handwritten ASM only for missing intrinsics; abstract primitive lifts from dav1d/ffmpeg/VLC**. Admissible SIMD primitives are an explicit allowlist with citations to published architectures (papers, blog posts with named techniques, or established library implementations). The allowlist for V1 (extensible only by appending; see `MASTER-PLAN.md` §4 admissible-SIMD-primitives table):
    - **arm64 NEON byte classify**: `vqtbl4q_u8` 4-table 64-byte lookup (Lemire 2019); `vqtbl1q_u8` 1-table (sonic-rs baseline).
    - **arm64 NEON movemask**: `vshrn_n_u16` + `vsriq_n_u8` + `vzip1q_u8` interleaved-vector pattern (Validark 2024).
    - **arm64 NEON loads + shifts**: `vld1q_u8_x4` quad-load (Arm A64 ISA); `vbslq_u8` branchless mask-select; `vextq_u8` 1D cross-lane byte-shift (Arm A64 ISA; dav1d filter-overlap lineage; **abstract primitive: cross-chunk byte-context propagation** — applies to ANY grammar with chunk-spanning tokens, not just JSON).
    - **arm64 NEON arithmetic**: `vcntq_u8` + `vaddvq_u8` byte popcount; `vqaddq_u8` / `vqsubq_u8` saturating add/sub (Arm A64 ISA; **abstract primitive: branchless overflow-clamped accumulation** — applies to ANY grammar's number primitive).
    - **arm64 NEON multiply-accumulate**: `udot` / `sdot` 4-byte dot-product (Arm A64 ISA Armv8.2-A; **abstract primitive: byte-window multiply-accumulate, lifted from dav1d's FIR filter** — applies to ANY grammar's digit-block decode, not just JSON: JSON `number`, CSS L4 `<number>`, TOML/INI/SQL integer literals, Sheets formulas).
    - **arm64 NEON LD4-interleaved 4-channel classifier** (NEW 2026-05-12, post-Wave-1-NEON-research): `vld4q_u8` + per-channel `vqtbl1q_u8` + `vshrn`/`vsri`/`zip1` movemask. Validark 2024 demonstrated ~10% drop in simdjson stage1 c/B on Apple arm64. **Abstract primitive: parallel-channel byte classification**; M5 Max-specific lever asmjson cannot use (AVX-512 only). Citation: validark.dev/posts/interleaved-vectors-on-arm/; simdjson PR #2333.
    - **arm64 NEON ternary bitwise** (NEW 2026-05-12; ARMv8.2-A SHA3 extension): `vbcaxq_u8` (Bit-Clear-And-XOR = `Vn EOR (Vm AND NOT Va)`) and `veor3q_u8` (3-way XOR). Equivalent to AVX-512 `vpternlogq` on arm64; collapses 2-op `bic + eor` into 1-op `bcax`. ~12-18% inner-loop reduction op-count. Available on every M-series (M1+) and Neoverse-V1/V2 (Graviton3/4). sonic-rs does NOT use these. Citation: Arm Architecture Reference Manual ARMv8.2-A SHA3.
    - **arm64 NEON set-membership (NEON port of SVE2 `svmatch_u8`)** (NEW 2026-05-12): `vceqq_u8` + `vorrq_u8` reduction tree for set membership against 16-byte alphabet. Portable equivalent of SVE2 `svmatch_u8` (Lemire 2026, Graviton4); same source ships on M5 Max NEON and dispatches to native MATCH on SVE2 hosts. Citation: Lemire 2026 "The fastest way to match characters on ARM processors".
    - **arm64 NEON cache hints**: `STNP` non-temporal pair-store (kernel `clear_page` lineage; tape-stream write); `PRFM PLDL2STRM` / `PLDL1KEEP` tuned prefetch (Arm A64 ISA; tape-walker prefetch ahead-of-cursor).
    - **x86_64 AVX-512 VBMI2**: `_mm512_mask_compressstoreu_epi8` (Lemire 2022; simdjson `icelake/simd.h:157` explicitly leaves unused for portability); `_mm512_ternarylogic_epi64` 3-input boolean (Sneller branchless-AVX-512); `vpermi2b` 128-byte byte-shuffle (simdjson icelake); `_mm512_alignr_epi8` cross-window carry.
    - **x86_64 AVX-512 k-mask arithmetic family** (NEW 2026-05-12, post-Wave-1-research): `_kandn_mask64`, `_kxor_mask64`, `_kxnor_mask64`, `_kshiftrq`, `_ktestq` (Travis Downs kreg-facts blog; AVX-512F base). Keep classifier masks in k0..k7 across state transitions; spill only on EOB. asmjson uses **only** `korq` + `kmovq` (~4 store+load eliminated per chunk). Citation: travisdowns.github.io/blog/2019/12/05/kreg-facts.html + 2020/05/26/kreg2.html.
    - **x86_64 AVX-512 VPCLMULQDQ at 512-bit lane** (NEW 2026-05-12; Ice Lake+ Intel, Zen 3+ AMD): adopt simdjson's prefix-XOR string-bitmap primitive at 4× width vs simdjson's 128-bit `_mm_clmulepi64_si128`. asmjson uses cmp+branch on backslash per byte (no prefix-XOR primitive); we add the primitive AND the width. Citation: WikiChip VPCLMULQDQ; BranchFree.org "Quote pairs with PCLMULQDQ" (2019); Linux kernel CRC-32C reaches 45-60 GB/s vs ~7-8 GB/s SSE4.2 with this primitive — same multiplier on prefix-XOR.
    - **x86_64 AVX-IFMA `vpmadd52luq` / `vpmadd52huq`** (NEW 2026-05-12; Sapphire Rapids+ Intel, Zen 4+ AMD): Eisel-Lemire mantissa multiplication for parse_number. asmjson dispatches number tokens to a Rust `JsonWriter` vtable (no number parse in asm); we keep mantissa-mul in vector lanes and return f64 directly. ~3× on number-heavy corpora (canada, mesh, marine_ik, numbers). Citation: WikiChip AVX-512_IFMA; Lemire 2024 Sapphire Rapids vs Zen 4 JSON.
    - **x86_64 AVX-512 VNNI `vpdpbusd`** (NEW 2026-05-12; Cascade Lake+, Zen 4+): byte×byte→i32 dot product, 4 bytes per int32 lane. For parse_number digit-block accumulation: 16-digit chunk → 4 lanes of `(d3*1000 + d2*100 + d1*10 + d0)` via one dot product. Citation: Lemire 2023 "Parsing integers quickly with AVX-512".
    - **x86_64 AVX-512 BITALG `vpshufbitqmb` + `vpopcntb`** (NEW 2026-05-12; Ice Lake+, Zen 4+): bit-gather 8 selected bits per 64-bit lane into k-mask (inverse of `vpcompressb`); per-byte popcount. One-µop multi-class classify that replaces `vptestmb + vpermb + vpmovb2m` triples. Per-state classification becomes data, not code. Citation: WikiChip AVX-512_BITALG.
    - **x86_64 AVX-512 GFNI**: `vgf2p8affineqb` arbitrary 8-bit affine transformation in 1 µop (Wojciech Mula 2018-2024; Intel GFNI Technology Guide 2018; **abstract primitive: single-op character classification, 2× over PSHUFB**; applies to ANY grammar's structural-byte classify).
    - **x86_64 AVX-2 + BMI2**: `_mm_clmulepi64_si128` prefix-XOR via CLMUL (simdjson original); `_mm256_shuffle_epi8` (sonic-rs `src/util/arch/x86_64.rs`); `_pdep_u64` parallel-deposit + `_pext_u64` parallel-extract (Mula branchfree.org 2018; **abstract primitive: bits-to-indexes / indexes-to-bits compaction** — applies to ANY structural-bitmap-to-offset-stream conversion).
    - **portable scalar**: SWAR 8-byte classify (asmjson #8 lineage; `word.wrapping_sub(0x2020202020202020) >> 7` for whitespace; `word ^ 0x2222222222222222` for quote; **abstract primitive: byte-class detection without SIMD**; correctness floor).
    - **hash primitive**: `ahash` crate (AES-NI on x86_64; NEON-AES on arm64); used by `path!` dictionary lookups and object-key hash; identical mechanism to sonic-rs's `ahash`-shaped hashing; not a bespoke implementation.
    
    Handwritten `asm!` blocks are admissible **only** when the equivalent intrinsic is absent from `core::arch::*` (current set: arm64 `ldp`/`stp`/`stnp` pair-load/store, `PRFM PLDL2STRM`/`PLDL1KEEP` prefetch variants, asmjson-style `r10`-direct-threading FSM entry; ffmpeg `x86inc.asm` macro corpus vendored verbatim at `crates/bbnf-simd/ext/x86/x86inc.asm`); new entries require documented measurement justification + citation to a published architecture. Hand-tuned undocumented intrinsic loops without an architectural name are forbidden as magic.
    
    **Abstract primitive lifts**: dav1d's pixel-arithmetic kernels do not translate to JSON (T14-T17 of the catalog: motion compensation, IDCT, loop filter, film grain — all are pixel-domain). But the *primitive operations* underneath them DO translate: cross-lane permute (`vextq_u8`), multiply-accumulate (`udot`), saturating arithmetic, cache hints, mask-register state machines. Each generalizes to byte-stream parsing for arbitrary grammars; the per-grammar selection is cost-model-derived from Grammar IR (alphabet size, number-token presence, string-token presence, chunk-spanning-token presence). The msac entropy decoder's `cnt/buf/end` cross-chunk refill pattern (`/tmp/dav1d-research/dav1d/src/x86/msac.asm:80-220`) is the one genuinely transferable algorithmic insight beyond what simdjson/sonic-rs/yyjson already demonstrate.
    
    Every SIMD primitive carries a unit-parity test against the scalar reference and a corpus-parity test against the expanded skinny corpus recorded in `restart/skinny/BENCH.md` §3 and `skinny/RESULTS.md` in `crates/bbnf-simd/tests/` (per `feedback_no_inline_tests`). Verification: every `core::arch::*` use-site and every `asm!` block in `crates/bbnf-simd/` traces to a citation in the Lock 16 allowlist or in the current skinny SOTA-BEAT synthesis.

## Lanes

Produce one document per lane plus a synthesis. Each lane has scaffolded items the auditor must produce.

### Lane 1 — Lock-Adherence

Walk every plan document. For each of the twelve locks, cite path:line in the plan where the lock is honored, and path:line where it is violated (or absent). For violations, recommend the surgical edit that closes the violation.

For each lock, end with a verdict: **honored / violated-with-recommendation / silent (must add)**.

### Lane 2 — Sequencing Discipline

The Era V failure mode (substrate-then-substrate-then-ship, never substrate-then-consumer-then-ship) was the genesis of seven dead substrate crates between AV and AX. The new plan must not repeat it.

For every wave in BA / BB / BC:
- What does this wave produce?
- Who consumes it, and when?
- If the consumer arrives in a later wave, is the substrate compileable + tested + benchable in this wave's gate?
- If the consumer never arrives in the plan, why is the substrate landing?

Flag any wave whose deliverable lands without a same-wave or next-wave consumer. Recommend either: (a) add the consumer to the wave, (b) merge the wave into the consuming wave, (c) cut the substrate from the plan.

### Lane 3 — Cohesion

Every wave's exit-criteria must be achievable from prior waves' outputs. Walk the wave dependency graph:
- W0 produces X. W1 consumes X via Y mechanism. Is Y specified?
- W1's exit-criteria reference Z. Is Z produced by W0 or W1's body?
- Are any wave's gates impossible to verify from artifacts the wave creates?

Identify orphan exit-criteria (gates that test invariants no wave produces) and orphan deliverables (wave outputs no later wave consumes).

### Lane 4 — SOTA Anchoring

For every perf gate in the plan, verify it cites a specific SOTA number with platform + dataset:

- ✓ "≤ 500 µs to parse twitter.json on M1 Pro, beating sonic-rs's 436 µs"
- ✗ "≥ AU bench parity"
- ✗ "≥ baseline"
- ✗ "≥ pre-W3"

Flag any gate that does not name a competitor's number. Recommend the specific number to substitute, sourced from `audit/SOTA-2026-05-03.md`.

### Lane 5 — Grammar-Authoritative Discipline

Per-grammar code in supposedly-generic crates is the GESTALT § grammar-authoritative violation. Walk the plan for any wave deliverable that:
- Hardcodes grammar idents in `bbnf-ir`
- Adds per-grammar match arms in non-codegen files
- Adds per-grammar feature flags
- Names a module after a grammar

Also: walk the plan for any wave that does NOT excise the existing violations enumerated in `audit/CENSUS-2026-05-03.md` §2 (css_types.rs, ir/registry/strategy.rs:130-185, ir/passes/audit/payload_coverage.rs:69, ir/passes/recognizers/shape_dict_bbnf.rs).

Recommend per-violation surgery; recommend tranche-and-wave for each excision.

### Lane 6 — Generated-Code Budget

Per-tranche LOC budget for `crates/core/src/grammar/generated/`. The current 168 K LOC across 9 grammars is the starting point. Layout lowering may grow some files (typed-enum variants explode) and shrink others (dispatch indirection retires).

For each wave:
- Does it grow generated LOC? Estimate.
- Is the growth justified (typed payloads carrying real data) or accidental (generator regression)?
- Is there a per-wave budget check in the gate?

Flag any wave that is silent on generated-code impact. Recommend a budget check (e.g., "css_l4.rs ≤ 110 K LOC; bbnf.rs ≤ 22 K; net delta ≤ +5%").

### Lane 7 — Friction Forecast

Forecast where users and grammar authors hit the new API and do not understand it. For each:
- The API surface (signature + docstring as planned)
- The user mental model required
- The point of greatest confusion
- The educational artefact the plan should produce (cookbook entry, doc page, error message hint)

Specifically forecast friction at:
- `pointer!["a", "b", 1]` syntax (compile-time path AST)
- `parse(input)` vs `parse_in(input, bump)` vs `parse_owned(input)` (lifetime escape hatches)
- Layout lowering errors (rule X has no resolvable layout because Y)
- Pratt auto-detection misfiring on a grammar shape the optimizer should not have classified as Pratt

Recommend at least three error messages (verbatim) the plan should commit to.

### Lane 8 — Carry & Deferral Audit

Every plan item deferred to a later tranche must:
- Name the receiving tranche (no "future tranche", no "AZ-V" fictional successors)
- State what blocks it from this tranche (specific dependency)
- Have a corresponding gate in the receiving tranche

Walk every "deferred to BB", "carry from BA.W3", "see BC.W1" in the plan. Verify each:
- Names a real, drafted tranche
- States the blocker concretely
- Lands in the receiving tranche's gate list

Flag every dangling carry. Recommend either: (a) move forward into current tranche, (b) explicit landing in receiving tranche's W?.M? gate, (c) cut entirely.

## Invariants of the audit

§1. **No metalanguage in audit docs**. Reference plan content by path:section (e.g., "BA.W2.M3 fails because…"); never reference commits, conversation history, or the plan's draft history.

§2. **Audit voice is direct, archaic-permissive**. Match the project's voice ("hereupon", "begotten", "thereof"). Avoid corporate hedging ("might want to consider"). State faults directly.

§3. **Citations are path:line, not paraphrase**. Every claim about the plan cites where it lives. Every claim about the codebase cites where it lives.

§4. **Recommendations are surgical**. Not "improve cohesion"; instead "merge BA.W4 into BA.W3 because W4 has no consumer of W3's output that BA itself uses; relocate W5 ↑ to fill the slot".

§5. **Verdicts are ratifiable or actionable**. Not "this is concerning"; instead "honored", "violated-with-rec-X", or "silent-must-add-Y".

## Execution discipline

§ED1. Each lane is one document. Filename: `audit/HARDENING-PLAN-2026-MM-DD-NN-<lane-slug>.md`.

§ED2. Plus one synthesis: `audit/HARDENING-PLAN-SYNTHESIS-2026-MM-DD.md` referencing each lane and tabulating cross-lane verdicts.

§ED3. Lanes can dispatch in parallel; synthesis lands after.

§ED4. HARD CAP per lane: 25 minutes. Synthesis: 15 minutes. At 0.9N commit progress, at N halt and report.

§ED5. The synthesis ends with a punch list: ordered, surgical, ready to act on. Each item names its plan-doc target and the specific edit.

§ED6. No hedges. No "consider". No "might". The plan is either right or wrong on each lane.

## Voice locks

§V1. Archaic diction is welcome.

§V2. State the fault. State the surgery. Move on.

§V3. The auditor is not a collaborator on the plan; the auditor is its first adversary. Ratify what survives; cut what doesn't.

§V4. No restating of the plan back to the user. Cite path:line and proceed.

## Failure modes to avoid

D1. **Restating the plan as audit**. The audit document recapitulates the plan in its own voice instead of identifying faults. Symptom: "BA.W2 plans to do X; this is good because Y." Audit is not summary.

D2. **Soft verdicts**. "Could be tightened", "may benefit from review". Either it's a fault (with surgery) or it's not.

D3. **Paragraph-level critiques**. "The optimization layering section needs more depth." Cite the line; specify the addition.

D4. **Ignoring locks**. The twelve locks above are settled. The audit does not relitigate them; it verifies the plan honors them.

D5. **Carry-blindness**. Treating every "deferred to BB" as legitimate without auditing whether BB has the gate. Era V's failure mode replicated.

D6. **Friction-vagueness**. "Users may find this confusing." Specify the user, the mental model, the point of confusion, the verbatim error message.

D7. **SOTA-erasure**. Accepting "≥ baseline" as a perf gate. Every gate names a competitor's number.

D8. **Genericity-erasure**. Accepting per-grammar code in generic crates because "the plan says we'll fix it later". Cite the planned fix or flag the deferral.

## Reading list (in order)

1. `docs/tranches/BA/BA.md`
2. `docs/tranches/BA/waves/*.md`
3. `docs/tranches/BB/BB.md`
4. `docs/tranches/BC/BC.md`
5. `docs/tranches/BD/BD.md` if drafted
6. `audit/HARDENING-SYNTHESIS-2026-05-03.md`
7. `audit/SOTA-2026-05-03.md`
8. `audit/CENSUS-2026-05-03.md`
9. `audit/MODULES-2026-05-03.md`
10. `audit/RESTART-SKETCH-2026-05-03.md`
11. `docs/tranches/meta-audit/archaeology/era-IV-tape-first.md` — tape arc archaeology (peak)
12. `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md` — substrate-first/consumer-later failure-mode anatomy
13. `docs/tranches/AV/research/04-columnar-soa.md` — kind-partitioned columnar SoA spec (designed, never activated; cited so the auditor can verify Lock 1 is honoured)

## Methodology

Per lane:

1. Open the plan documents (subjects).
2. Walk the lane's question with the plan in front of you.
3. For every claim, cite path:line.
4. For every fault, recommend the surgical edit.
5. End with a per-lane verdict tabulation: items honored, violated, silent.
6. Commit the lane document.

Synthesis:

1. Read all eight lane documents.
2. Tabulate cross-lane verdicts (the same plan section may be honored on Lane 4 and violated on Lane 6).
3. Produce a punch list — one entry per surgery, in execution order, with target path:line and verbatim edit.
4. Recommend whether the plan is ready to execute, ready after surgery, or requires re-draft.

## Provenance

This prompt is for hardening the BA-restart plan set drafted after the 2026-05-03 codebase audit. The twelve locks are user verdicts on the architectural commitments that govern BA-restart. Do not relitigate the locks; verify the plan honors them.

The codebase-side companion prompt is `docs/HARDENING-AUDIT-PROMPT.md` (audits the codebase, not the plan).
