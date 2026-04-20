# AY Critique and Path Forward

Date: 2026-04-20

Scope: audit AY against AW/AX/AZ plans, AU archaeology, current code, current benches, current profiling/tooling instructions, and fresh non-legacy `cargo expand` inspection on JSON and CSS.

This document is intentionally corrective. It does not propose quick fixes, bench cosmetology, or another substrate-first expansion. It states what is true in the repository now, why gains remain marginal, and what an idiomatic continuation should be.

## Executive position

AY is not closeable as written.

The tranche landed real work through W4:

- W1 restored the flat AoS write path and materially recovered the post-AW floor.
- W2 landed named-preservation and wrap-elision work, but with much smaller record-count reduction than planned.
- W3 landed the runtime/value API substrate and emitted non-legacy RD/value paths.
- W4 landed SIMD/string/regex work, but did not deliver the promised BEAT-sonic result.

What did not land is equally important:

- AY did not beat sonic-rs.
- AY did not close its own performance gates.
- AY did not write AY FINAL, did not update AY PROGRESS beyond W1, and did not truthfully rebase AZ on the partially-landed AY substrate.
- Several optimization surfaces are wired, but not globally coordinated.
- The toolchain and edicts still default to expensive full-workspace behavior even though the docs describe a faster discipline.

The repository is therefore in an “AY closeover” state, not an AY close state and not yet a clean AZ-open state.

## What AU got right, and what post-AU got wrong

AU was the local maximum because it optimized the parser that actually executed.

The key AU pattern was:

- profile first,
- prove runtime activation,
- optimize write-path cost and emitted hot-path shape,
- replan when a hypothesis missed.

The important AU-era wins were not “more mechanisms”; they were coherence:

- `branch_pushes_children` and real payload activation became runtime-true,
- the flat AoS tape stayed on the write hot path,
- `push_leaf_with(PayloadData)` unified leaf emission,
- bench and profiling artifacts were treated as tranche truth.

Post-AU, the process drifted:

- AV/AW repeatedly landed substrate without hot-path consumers,
- the SoA structural pivot optimized a read-side microbench, not parse-time write cost,
- later widening/predicate changes altered emitted shape without mandatory downstream rebenching,
- architectural narrative repeatedly substituted for throughput proof.

The central lesson is simple:

Optimize the executing parser, not the architecture sketch.

## Why gains are still marginal

Three reasons dominate.

### 1. The default hot path is still tape-first

Fresh expand inspection shows that current JSON is on the non-legacy RD path:

- `JsonParser::parse()` is a thin shell around shape-dispatched parse fns,
- `parse_with_visitor<V>` exists and is clearly the more sonic-like path,
- `JsonParserValue<'p>`, `Parsed::to_value()`, and `Parsed::get()` are emitted.

But the default user-facing path is still:

1. parse into tape,
2. finish/finalize tape,
3. walk/materialize from tape.

That remains fundamentally different from sonic-rs, where the direct document/value path is primary.

The repository’s own numbers show the consequence:

- W3 eager twitter remains multiple times slower than sonic-rs,
- the lazy lane is not actually matched work, because bbnf still parses the whole document first.

### 2. The value/document layer is still compatibility-oriented

The current tape substrate is good enough as a parser substrate:

- `records: Vec<TapeRec>` + `sib_skip` is the right write-side direction.

But the read/value side is still over-generalized:

- `child_off` is overloaded across child pointers and multiple payload interpretations,
- `packed_cache()` is a transpose cache, not a first-class document representation,
- `TapeCursor` still carries pre/post-order compatibility burden,
- finalization remains a meaningful second structural pass,
- emitted `to_value()` paths still allocate `Vec<...>` compounds and dispatch through large grammar enums.

That is acceptable for grammar/debug/view surfaces. It is not the optimal dominant representation for eager/lazy value APIs.

### 3. Optimization is staged, not globally informed

The pipeline is active for non-`structural` compiles:

- structural normalization,
- pattern hoisting,
- one grammar e-graph saturation,
- follow/lookahead/token-dispatch,
- DAG build,
- regex/structural mining,
- strategy/materialization CSP.

But it is not globally optimized.

The current break is this:

- grammar extraction commits before regex-engine decisions,
- e-graph extraction commits before recognizer/materialization outcomes,
- strategy CSP only sees a narrow downstream decision set,
- inline analysis is separate again,
- shape-dict selection is a separate greedy pass,
- materialization re-derives facts already close to e-graph facts,
- the scheduler is only partly incremental.

This means shared cost weights exist, but shared optimization does not.

## What is wired, what is dead, and what is duplicated

### Wired and active

- Normalizer, e-graph, regex-info, recognizer mining, materialization classification, and strategy CSP are all wired for non-`structural` compiles.
- Regex extraction does share compile-scoped cost configuration and a saturation cache.
- Backend inline analysis does read the shared `CostWeights`.
- Current JSON/CSS generation is RD/shape-emitted, not legacy DTA runtime.

### Dead or effectively dead

- Shape-dict mining/selection still runs, but the product boundary no longer carries a runtime/emitter consumer. That is wasted compile work and misleading architecture.
- Visitor/direct-document infrastructure exists for JSON, but it is not the primary value path.
- Backend kernel modules still appear underused from emitted JSON/CSS output.

### Duplicated or weakly coordinated

- Materialization lattice work mirrors e-graph fact derivation instead of reusing a single fact substrate.
- The strategy CSP is only component-global and only over a narrow set of decisions.
- Shape-dict optimization is grammar-wide but greedy and detached from the rest of the objective.
- There is significant subtree cloning across inline/fuse/pattern-hoist/egraph insertion/extraction surfaces.

## Toolchain and edict critique

The repo’s stated fast workflow and its actual command defaults do not match.

Today:

- docs say use `ax-iter`, shared target dirs, tiered tests, and prepared bench binaries,
- public entrypoints still default to full workspace test/bench behavior,
- profiling prep repeatedly rebuilds instead of preparing once and recording many,
- `[profile.bench]` is still fat-LTO + single-CGU by default, which is suitable for final proof but poor for routine agent iteration,
- large compile surfaces remain monolithic, especially CSS L4 expansion in `crates/core`,
- some perf loops still live under `tests/`, so correctness workflows compile performance payloads.

This is why agent cycles are too long: the repository asks for scoped work in prose but exposes wide work in commands.

## Proper path forward for AY

AY should not proceed as “W5/W6/W8 then W7 theatrics.”

AY needs a closeover phase with three explicit goals.

### AY.C1 Semantic closure

Close the correctness and surface-truth debt on the value/tape side:

- finish the remaining Named/layout/reachability truth,
- replace smoke-level Value/get tests with semantic assertions,
- remove any headline benchmark lane that is not genuinely matched work,
- make the emitted lazy string path real or stop presenting it as a lane.

### AY.C2 Honest performance closure

Run one integrated JSON-focused optimization pass over the actual bottleneck chain:

- tape record count,
- finalize burden,
- direct document/value construction,
- string path,
- number path,
- lazy field lookup shape.

Do not target “beat sonic by 20-40%” as the tranche premise anymore.

Target instead:

- restore a defensible, truthful JSON floor,
- replace the current generic tape-first value path with the canonical hot substrate,
- prove runtime gains on matched work.

If that then beats sonic, good. It cannot remain a planning assumption.

### AY.C3 Documentation closure

- update `docs/tranches/AY/PROGRESS.md` through W4,
- record W5/W6/W8 disposition explicitly instead of letting them remain ghost requirements,
- write AY FINAL against what actually landed,
- rewrite AZ to open on AY-close, not AX-close.

## Architectural direction: one path, one canonical substrate

Do not pivot back to legacy DTA.

Do not split the system into orthogonal parse paths.

The right path is a single RD/shape-emitted parser writing a single canonical packed substrate.

That means:

- one structural/dispatch front end,
- one shape-emitted recursive-descent parse,
- one write target,
- many consumers layered on the same output.

What changes is not the existence of a second runtime. What changes is the definition of the runtime substrate itself.

The current tape is still too generic:

- overloaded fields,
- polymorphic payload interpretation,
- finalize burden,
- compatibility-oriented cursor logic,
- tape-then-walk value materialization.

That is the wrong bias if throughput dominates all other concerns.

The canonical substrate should instead be a packed parse/value/view substrate that the parser writes directly once.

### Canonical packed substrate

Responsibilities:

- maximum parse throughput,
- eager value materialization,
- lazy field/path lookup,
- object/array skipping,
- view/debug/incremental provenance,
- cache-local consumer access.

Properties:

- explicit tag,
- explicit span,
- explicit subtree skip/count/length semantics,
- direct scalar payload storage in final hot form,
- borrowed-or-arena string storage,
- object-as-key/value run layout,
- enough stable structural metadata that debug UX, replay, and incremental work read the same substrate rather than demanding a second one.

This should replace the current “generic tape first, value/view second” bias. It should not coexist as a separate dominant path.

### Consumer model

Every consumer should read this same substrate:

- `view()` reads node headers, spans, and child/entry metadata directly,
- `to_value()` becomes wrapping/projecting rather than reconstructing,
- lazy `get()` uses object navigation metadata rather than generic child iteration,
- debug UX reads rule/span/provenance side data from the same output,
- incremental/replay/recovery build on stable node spans and resumable shape boundaries from the same output.

The parser path remains one path.

## Immediate implication for the current code

Do not keep optimizing the current tape abstraction as though it were the final form.

The right move is to redesign the canonical output layout so that it is already the fastest value/view substrate the parser can produce, then make every API consume that.

That means:

- keep RD/shape emission,
- keep the single parser entry path,
- replace the current general-purpose tape contract with a stricter packed substrate contract,
- preserve whatever provenance/debug metadata is needed for future UX and incremental work inside or alongside that same substrate.

## Sonic-rs / simdjson lessons to actually absorb

The useful lessons are not “copy their tape.”

They are:

- one-pass write of the hot representation,
- spend bits where they save branches and skips,
- let the dominant hot operations dictate layout,
- keep object/array skip information explicit,
- store enough structural truth once so lazy, eager, view, and debug consumers do not rebuild it.

Applied here, that means:

- stop making `packed_cache` a transpose cache and either promote or replace it with the canonical packed node layout,
- stop making `child_off` the universal polymorphic slot for every consumer,
- stop paying finalize work when shape emission already knows enough to write close-time structure directly,
- stop proliferating payload columns that no hot consumer scans directly,
- stop treating tape-to-value reconstruction as an acceptable steady-state hot path.

`pay_f64` is a good cautionary example: a specialization is only good if it shortens a real consumer path. If it only creates another routing distinction, it is negative information density.

## What to do with CSP, e-graph, and structural mining

The next optimizer step should not be another isolated pass.

It should be a two-phase fixed point:

1. structural normalize + e-graph canonicalization,
2. regex/recognizer/materialization/strategy fact derivation,
3. a second extraction/optimization pass that sees those facts and can choose globally better forms.

Concretely:

- make the DAG, not cloned trees, the canonical optimization substrate after lowering,
- persist reusable fact summaries instead of recomputing parallel lattices,
- unify compile-scoped optimization objective data across grammar extraction, regex extraction, strategy CSP, and backend inline analysis,
- either make the e-graph scheduler truly dirty-search-aware or simplify it,
- either reintroduce a real shape-dict consumer or delete the pass.

## What to descale or move out of AY critical path

W5, W6, and W8 are not the right critical-path frame for AY anymore.

They may still matter, but they are orthogonal:

- compile-time improvements,
- broader toolchain speedups,
- document-parallel work.

Those are worthwhile once AY’s semantic and hot-path story is honest.
They should not remain blockers for AY close or prerequisites for AZ.

## Revised tranche sequence

The clean sequence is:

1. AY closeover
2. AY FINAL on truthful baselines
3. AZ rebase to AY-close substrate
4. AZ replay/recovery/incremental work
5. only then broader compile-time/parallel extension work, unless a specific item is proven to shorten the AY closeover itself

## Immediate repo-level recommendations

No speculative code changes are justified until the tranche direction is corrected, but the following are clear:

- change public fast-path commands so they default to tiered/scoped `ax-iter` workflows,
- split bench/profiling profiles so routine iteration does not pay fat-LTO costs,
- batch bench prebuilds and remove fallback double-build profiling paths,
- remove perf loops from default test surfaces,
- stop treating CSS L4 monolithic generation as acceptable compile-time shape.

## Bottom line

AY’s problem is not lack of mechanisms.

AY’s problem is that it still mixes:

- parser substrate work,
- value/document work,
- global optimization aspirations,
- compile-time/tooling work,
- tranche narrative.

AU won by aligning all of those around the executing hot path.

AY should do the same:

- restore truth in the tranche ledger,
- promote direct document/value construction to the primary JSON consumer path,
- keep tape as the parser/debug substrate,
- turn shared cost weights into a genuinely shared objective,
- stop closing waves on substrate existence rather than runtime effect.
