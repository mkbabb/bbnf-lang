# SK-V24 — ROUND ZERO: the approach portfolio and the proving manifest

Minted by the orchestrator (Fable) per HANDOFF §3. Five families, genuinely orthogonal
architectural centers. The union target folds proven mechanisms from all five; two routes
sharing a mechanism share a family; cross-pollination only after independent development.

## The portfolio

- **F1 — THE KERNEL-FUSION MACHINE** (performance center). The generator emits a fused
  scan→classify→materialize pipeline; grammar compiles to SIMD kernel schedules
  (shufti/truffle prefilters, vshrn mask-narrow, tbl classification, Mula/Lemire numerics,
  multi-block pipelining) selected by a cost model. Substrate: AoS tape-first, lazy view atop.
- **F2 — THE TYPED LATTICE** (semantics center). The type plane is the architectural center:
  grammar `->` projections compose through an inference lattice (HM core, CSP for
  overload/width resolution, e-graph for projection canonicalization); codegen consumes typed
  IR; the value plane derives from types — direct-to-struct is the primary API, the tape its
  erasure.
- **F3 — THE STAGED META-EMITTER** (emission center). dav1d discipline generalized: a
  backend-agnostic decision driver + per-target emitters (Rust+NEON now; TS/V8 later).
  Everything is a two-stage program: the generator stage const-evals, beta-reduces, minimizes
  DFAs; the residual program is straight-line Rust; asm earns per-kernel via
  checkasm-liveness.
- **F4 — THE UNIFIED VALUE FABRIC** (consumer center). Designed from the API backwards: one
  Value surface — eager sink, lazy cursor/view, path queries, direct-to-struct, serde bridge —
  over one tape layout unified across languages (same layout decoded in Rust and TS).
  Features proven by consumer benchmarks (query workloads, stop-early, partial decode), not
  parse alone.
- **F5 — THE REGEX-GRAMMAR CONTINUUM** (parse-that center). Erase the combinator/grammar
  boundary: grammar rules lower to the same HIR the regex engine consumes; one
  classify/DFA/prefilter substrate serves both repos; parse-that's engine becomes the shared
  scanning plane consumed by bbnf's emitter. TS variant receives the derivative novelties.

## The proving manifest (initial; recon may amend — provers are worktree/scratch-isolated)

| ID | Family | Feature to prove | Baseline / gate |
|----|--------|------------------|-----------------|
| PROVE-01 | F1 | shufti/truffle NEON prefilter productionized into scan dispatch, fused GATE=16 | hold ≥15× skip-heavy, ≥0.95× dense (reuse PROTO-PB SIMD work) |
| PROVE-02 | F1 | Mula/Lemire SIMD digit-run parse feeding Eisel–Lemire — the canada redress | close the 1.20–1.25× canada loss vs sonic-rs materially |
| PROVE-03 | F1 | SIMD string unescape + fused UTF-8 validation (lookup algorithm) | beat current unescape path, twitter strings |
| PROVE-04 | F1 | multi-block (2×64B) software-pipelined structural/quote scan, vshrn idiom standard | beat single-block scan, paired |
| PROVE-05 | F1 | tbl-based CSS token-class kernel | widen CSS 2.07–3.47× band vs lightningcss |
| PROVE-06 | F4 | lazy path API (pointer get / stop-early) over the tape/view | vs sonic-rs get_from_str |
| PROVE-07 | F4 | direct-to-struct: generator emits typed struct-filler from tape | vs serde_json + sonic-rs typed decode |
| PROVE-08 | F2 | type-plane probe: HM+CSP inference over `->` chains → typed signatures feeding PROVE-07 | compiles; ZERO runtime cost (perf-neutral gate) |
| PROVE-09 | F5 | parse-that rust (SCRATCH COPY): BumpSlab arena de-nightly, alloc minimization in combinators | parse-that suites; alloc counts + cyc/B |
| PROVE-10 | F5 | parse-that rust (SCRATCH COPY): lazy/hybrid DFA with 4096-state bail + prefilter tiers | vs current DFA and the regex crate |
| PROVE-11 | F5 | parse-that TS (SCRATCH COPY): V8 uplift — same-map nodes, monomorphic dispatch | vitest bench vs current 1.0.0 |
| PROVE-12 | F3 | emit-spine residualization probe: const-eval/beta-reduce one hot rule to straight-line Rust | cargo asm diff + paired bench |
| PROVE-13 | F1/F3 | generality proof: CSV grammar through the one generator with kernels | vs csv/csv-core crates; NO grammar-name gates |
| PROVE-14 | F4 | tape unification: same layout decoded in TS via typed arrays | node micro-bench; layout parity proof |

## Protocol (binding on every prover)

Cold-only N=200 per-parse; paired per-sample interleaving vs baseline; per-block loadavg
recorded; ratios certified, absolutes directional. Visual verification mandated for codegen
provers: cargo expand / cargo asm inspection in the report. Worktree isolation with the
cp-out delivery rule; parse-that ONLY via scratch copies (non-clobber law). A prover either
returns measured numbers or is marked spec-only — spec-only anchors nothing. An adversarial
verification fleet re-measures every claimed win; the unproven dies.
