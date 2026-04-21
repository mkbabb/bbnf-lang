# AY Critique and Path Forward

Date: 2026-04-20

Scope: audit AY against AW/AX/BC plans, AU archaeology, current code, current benches, current profiling/tooling instructions, and fresh non-legacy `cargo expand` inspection on JSON and CSS.

This document is intentionally corrective. It does not propose quick fixes, bench cosmetology, or another substrate-first expansion. It states what is true in the repository now, why gains remain marginal, and what an idiomatic continuation should be.

## Executive position

AY is not closeable as written.

The correct AY target is also narrower and sharper than the current tranche prose:

- not “copy sonic-rs,”
- not “copy simdjson,”
- not “revive DTA,”
- not “accumulate every interesting mechanism into one tranche.”

The real target is a generalized hybrid architecture that absorbs:

- sonic-rs-class direct hot-path construction,
- simdjson-class explicit structural/skip semantics where they genuinely pay,
- the repo’s own surviving research on mining, direct-to-struct, Pratt flattening, and structural side information,
- while preserving one parser, one runtime substrate, and one grammar-derived mechanism surface.

The tranche landed real work through W4:

- W1 restored the flat AoS write path and materially recovered the post-AW floor.
- W2 landed named-preservation and wrap-elision work, but with much smaller record-count reduction than planned.
- W3 landed the runtime/value API substrate and emitted non-legacy RD/value paths.
- W4 landed SIMD/string/regex work, but did not deliver the promised BEAT-sonic result.

What did not land is equally important:

- AY did not beat sonic-rs.
- AY did not close its own performance gates.
- AY did not write AY FINAL, did not update AY PROGRESS beyond W1, and did not truthfully rebase its successor tooling tranche on the partially-landed AY substrate.
- Several optimization surfaces are wired, but not globally coordinated.
- The toolchain and edicts still default to expensive full-workspace behavior even though the docs describe a faster discipline.

The repository is therefore in an “AY closeover” state, not an AY close state and not yet a clean BA-open state.

One planning correction follows from that state:

- do not execute full BB before AY,
- do extract a narrow pre-AY runway annex that fixes command/build/bench drag blocking `W5-W7`,
- do keep every parity-critical runtime change inside AY itself.

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

The second lesson is equally important:

Keep only the ideas that survive as general, grammar-derived mechanisms. Do not carry forward failed substrates as nostalgia projects.

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

That remains fundamentally different from the best lessons of sonic-rs and simdjson, where the hot representation is primary rather than reconstructed after a generic intermediate.

The repository’s own numbers show the consequence:

- W3 eager twitter remains multiple times slower than sonic-rs,
- the lazy lane is not actually matched work, because bbnf still parses the whole document first.

### 2. The runtime substrate is still compatibility-oriented

The current tape substrate is good enough as a parser substrate:

- `records: Vec<TapeRec>` + `sib_skip` is the right write-side direction.

But the read/value side is still over-generalized:

- `child_off` is overloaded across child pointers and multiple payload interpretations,
- `packed_cache()` is a transpose cache, not a first-class document representation,
- `TapeCursor` still carries pre/post-order compatibility burden,
- finalization remains a meaningful second structural pass,
- emitted `to_value()` paths still allocate `Vec<...>` compounds and dispatch through large grammar enums.

That is acceptable for transitional compatibility. It is not the right canonical runtime if throughput dominates.

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

### 4. The current critique must be refined away from “be sonic”

The correct standard is not “look like sonic-rs.”

It is:

- match or exceed sonic-rs-class hot-path discipline,
- absorb simdjson-class structural indexing and skip semantics where they improve the single path,
- absorb only the repo’s novel levers that are both general and empirically live,
- reject everything else.

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
- Large parts of the old DTA/PSI conceptual stack survive only as historical ideas; they are not valid as runtime-first architecture in their old form.

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

The corrective action is not “run BB first.”

The corrective action is:

- carve a small pre-AY runway annex for command/profile/bench reform,
- close that annex quickly,
- then execute `AY.W5-W8` without allowing runtime work to drift back out into tooling tranches.

Two further corrections are required:

- crate names get no privilege: if `crates/tape` stops being the honest
  home of the canonical substrate, it should be absorbed or deleted;
- structural scan should become first-class on the canonical path: not a
  niche sidecar, and not a blind eager whole-input tax. Its scan policy
  must be deeply integrated into AY and close with explicit
  non-regression proof.

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

- canonical packed node layout,
- write-time structure closure,
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
- rewrite the successor chain so BA opens directly on AY-close, BB
  follows BA, and BC opens only after BB on the BA-close substrate.

## Architectural direction: one path, one canonical substrate

Do not pivot back to legacy DTA.

Do not split the system into orthogonal parse paths.

Do not treat this as a direct sonic-rs port or a direct simdjson port.

The right path is a generalized hybrid:

- one RD/shape-emitted parser,
- one canonical packed substrate,
- one grammar-derived emitter architecture,
- one optimization stack that chooses among general mechanisms,
- many consumers reading the same output.

That means:

- one structural/dispatch front end, potentially with explicit structural side information where profitable,
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

Its design should be informed by three families of ideas:

- sonic-rs: direct hot-path construction, borrowed-string fast path, per-shape monomorphised emission,
- simdjson: explicit subtree/skip/count semantics and structural-stage leverage,
- bbnf-native research: grammar-derived direct-to-struct, Pratt flattening, structural mining, and any PSI/DTA-era insight that still survives as a profitable general emitter mechanism.

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
- optional structural side tables or compact indices only where they improve the same runtime path rather than introduce a second one,
- enough stable structural metadata that debug UX, replay, and incremental work read the same substrate rather than demanding a second one.

This should replace the current “generic tape first, value/view second” bias. It should not coexist as a separate dominant path.

### Generality invariant

This architecture must remain fully grammar-derived.

The rules are:

- BBNF grammars are the sole source of semantic and structural information within reason.
- The host/type/projection system may enrich that information, but not replace it with hand-authored grammar-specific parser logic.
- No hand-written JSON-only or CSS-only semantic parser path becomes the real product.
- Every mechanism must be expressible as a general emitter/miner/projection decision driven by IR, type inference, recognizer facts, or grammar-declared structure.

That means:

- direct-to-struct must become a true grammar-derived projection mechanism,
- Pratt flattening must be a general operator-shape lowering/emission path,
- structural side information must be mined from grammar/IR facts,
- any SIMD/string/number specialization must be admitted by recognizer/type facts, not hand-routed by grammar name.

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
- reject any “temporary” architecture that becomes a second real runtime.

## Sonic-rs / simdjson / bbnf-native lessons to actually absorb

The useful lessons are not “copy their tape.”

They are:

- one-pass write of the hot representation,
- spend bits where they save branches and skips,
- let the dominant hot operations dictate layout,
- keep object/array skip information explicit,
- store enough structural truth once so lazy, eager, view, and debug consumers do not rebuild it.

And from the repo’s own research:

- retain only the DTA/PSI ideals that can be recast as profitable single-path mechanisms,
- exploit direct-to-struct where type inference/projection can prove the layout,
- flatten Pratt/operator structures where grammar facts make the emitted path simpler and hotter,
- use structural mining and recognizer facts to select mechanisms globally rather than bolting them on locally.

The failed parts matter too:

- old DTA as the dominant runtime path did not justify its dispatch/interpretation cost,
- PSI as a broad architectural center did not pay for itself,
- substrate-without-consumer landings repeatedly hid regression behind mechanism count.

So the rule is:

salvage the ideals, not the substrate.

Applied here, that means:

- stop making `packed_cache` a transpose cache and either promote or replace it with the canonical packed node layout,
- stop making `child_off` the universal polymorphic slot for every consumer,
- stop paying finalize work when shape emission already knows enough to write close-time structure directly,
- stop proliferating payload columns that no hot consumer scans directly,
- stop treating tape-to-value reconstruction as an acceptable steady-state hot path.
- stop treating DTA/PSI history as an all-or-nothing decision; keep only the parts that strengthen the one canonical path.

`pay_f64` is a good cautionary example: a specialization is only good if it shortens a real consumer path. If it only creates another routing distinction, it is negative information density.

### What to salvage from DTA / PSI

Only the following class of ideas remains defensible:

- explicit structural side information when it lowers cost on the same path,
- mined dispatch/precedence/shape facts that let the emitter choose hotter code,
- replay/debug metadata that reads the same canonical substrate,
- grammar-wide structural observations that improve skip/seek/object/array handling.

What should not return:

- an interpreter-shaped dominant hot path,
- a second runtime substrate justified only by “future tooling,”
- generic PSI payload streams on the hot path when the packed substrate can store final-form values directly.

### Pratt flattening and direct-to-struct

These should become first-class AY levers, but only as general mechanisms.

Pratt:

- not a special Sheets trick,
- not a grammar-name admission,
- but a general operator-shape lowering and packed emission strategy driven by grammar facts.

Direct-to-struct:

- not a sidecar novelty,
- not a view-only convenience,
- but the grammar-derived projection mechanism that lets the canonical substrate store typed structure directly where inference/projection proves it safe.

Both are central because they reduce reconstruction work while preserving grammar authorship as the semantic source.

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
- make direct-to-struct, Pratt flattening, and structural-side-information decisions participants in the same global objective rather than isolated post-hoc refinements.

## What to descale or move out of AY critical path

W5, W6, and W8 are not the right critical-path frame for AY anymore.

They may still matter, but they are orthogonal:

- compile-time improvements,
- broader toolchain speedups,
- document-parallel work.

Those are worthwhile once AY’s semantic and hot-path story is honest.
They should not remain blockers for AY close or prerequisites for BA,
BB, or BC.

## Revised tranche sequence

The clean sequence is:

1. AY closeover
2. AY FINAL on truthful baselines
3. BA direct post-AY performance/exceedance work
4. BB post-BA compile/build/bench discipline
5. BC replay/recovery/incremental/debug work on the BA-close
   substrate
6. only then any broader extension work not already absorbed by those
   three

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
