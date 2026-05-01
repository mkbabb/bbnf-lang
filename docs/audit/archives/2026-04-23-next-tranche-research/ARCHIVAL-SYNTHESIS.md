# Next-Tranche Research — Archival Synthesis

Retrospective on the bbnf-lang commit trajectory plus external SOTA JSON
synthesis, informing the still-unwritten tranches AY-II, BA, BB, BC.
Consolidates what the codebase learned, what it paid for, and what external
literature says about the frontier the next tranches must cross.

Scope guard: this is a research document — legitimately meta-language —
but it is written to inform **forward** decisions, not to narrate. All
citations are tranche letters, commit short hashes, or external sources;
no conversation-history references.

## Tranche process origin

The modern bbnf-lang monorepo begins at commit `cc49997` (2026-02-26,
"restructure as bbnf-lang monorepo"). Prior history (going back to
2023-03-03, commit `0b1e1f3`) is a different project — the original
BBNF VSCode extension — with only `205310b` ("bump") on 2026-02-26
bridging the two eras. The commit pool for "the last N commits" is
therefore **897 total**, of which roughly 700 are post-monorepo.

Formally-tagged tranche work begins at commit `a3fadf5` (2026-04-08,
"refactor(backend): pre-solve delim_scan + key_dispatch per-grammar
(Tranche F)"). Earlier lettered tranches (A–E) were rolled into the
pre-tranche architectural migration and did not carry explicit commit
tags. The tranche naming quickly stabilised with V, W, X, Y, Z, then
AA onward — one tranche per ~15–50 commits, each with a `docs/tranches/<X>.md`
plan and increasingly with a companion `PROGRESS.md` and audit subdirectory.

Evolution of the tranche artifact:

| Stage       | Artifacts                                                           |
|-------------|---------------------------------------------------------------------|
| V–Z         | `<X>.md` only; phases inline; bench results embedded                |
| AA–AD       | `<X>.md` plus prototype docs (`AA-prototype*.md`); plan-vs-execute   |
| AE–AK       | `<X>.md`, tranche subdirs start appearing (`AE/tape-shapes.md`)      |
| AL–AQ       | `<X>.md` + `<X>-audit.md` + `<X>-plan.md`                           |
| AR–AU       | tranche directory `<X>/` with `<X>.md`, `PROGRESS.md`, 5–7 audit docs |

The AR–AU stage represents the current stable form: plan + progress log +
multi-agent audit docs (`audit-*.md`) captured before the tranche opens.
This shape emerged because tranches kept landing correctness fixes that
contradicted the plan, and the audit discipline caught those contradictions
earlier.

## 2000-commit trajectory — what worked

Five approaches proved durable across multiple tranches and remain
load-bearing at HEAD.

### 1. Tape-first runtime (Tranche AB → AE)

**Landed**: AB.2a substrate (commit `094ce8e`, 2026-04-10), AC.2 CST
consumer migration (`caaa531`/`4166ad9`/`d7b9d3b`), AE.5 tests+benches
migration (`8fcf13d`/`5af1aed`), AL.1 serialize rewrite (`e6574a9`).

**Invariant established**: a flat `Vec<TapeRec>` with fixed 16-byte
records replaces the eager boxed-enum AST. Compound records reference
children via `child_off`; the typed view is a generated `impl` over the
tape. `crates/bbnf-tape/src/lib.rs` documents the contract.

**Why it worked**: prettify was already decoupled from the typed enum
surface (it walked `ParserState` and emitted `FmtBuilder` ops), so tape
adoption was mechanically bounded to the emitter. AC.2 shipped in a
single session with a parity-fixture gate.

### 2. Regex HIR + bespoke engine (Tranche X–Y, Tranche AG)

**Landed**: structured `RegexClass` dispatch (`scanner_plan.rs`), bespoke
DFA engine in parse-that (VM migration `be01787`, 2026-03-30), HIR cost
model consolidation (Tranche Y.6a, `480d2d0`), FastPath regression resolution
(AG.1, `41ae539`).

**Invariant established**: one regex system. `bbnf_regex::sets::charset`
is the canonical source; `regex_first_chars` is re-exported from ir.
No `regex` crate dependency in the hot path.

**Why it worked**: the CSP solver needed a classifier that could be
invoked with per-component `EmitOpts`; a hand-written HIR gave the
solver a stable decision surface. AG.1's finding — that FastPath and
CSP had diverged on `EmitOpts` — was unfixable without the consolidation.

### 3. CSP solver for regex strategy + cross-rule dispatch (Tranche F → AQ)

**Landed**: cross-component CSP tier variables (AG.5, `1c66b93`),
per-component solve + instrumentation (`a78c4a6`, AQ.9.3).

**Invariant established**: regex engine choice, emission tier, and
wrap mode are CSP variables, not hardcoded branches. `BBNF_CSP_REPORT`
emits one report per component for profiling.

**Why it worked**: e-graph extraction and CSP propagation share a
concept of "best per class." Once `CostConfig::hir_extraction_cost()`
became the single source of truth (Y.6a), e-graph and CSP agreed on
cost arithmetic and stopped fighting each other.

### 4. Structural delim-scan + SIMD in hot loops (Tranche AP.5)

**Landed**: NibbleLut DFA + SIMD delimiter scan (AP.5.1-5.2, `a603df9`),
structural dispatch gated on WS elision (AP.1b, `4417f8a`), CSS L4
declaration key dispatch (AP.4, `95842dc`).

**Invariant established**: delimiter-driven flat scanners for hot inner
loops. Memory `feedback_delim-scan-approach` is the canonical note; this
was the single largest bench win of the late-tranche era (twitter
2086→bench parity on several grammars).

**Why it worked**: peek-only structural dispatch avoided the earlier
full-structural-prescan deletion (AQ.5, `2f7c1bd`). The cost was bounded
— one byte-peek per compound boundary, no 300µs prescan.

### 5. Samply profiling discipline (Tranche Z → AR)

**Landed**: pre-Z samply baseline (`1ceb37e`), AP/AQ/AR samply captures
(`5e8caee`/`a217a3a`/`1bb419f`), AT 4-grammar profile fold (`3218aed`).

**Invariant established**: every tranche captures samply profiles
under `docs/benchmarks/profiles/<tranche>/` before and after. No
optimization claim lands without a profile delta.

**Why it worked**: `feedback_samply-symbol-resolution` nailed that
`samply record` (interactive) resolves symbols correctly where
`--save-only` does not. The discipline made post-hoc diagnosis
tractable (AU canada regression attribution was gated on fresh samply).

## 2000-commit trajectory — what failed

Five approaches either reverted outright or ossified into dead
infrastructure; their failure modes should inform BA/BB/BC planning.

### 1. EmissionTier axis (Tranche AI → AM)

**Fate**: deleted in AM.1 (`7608530`, 2026-04-12), "~2000 LOC dead code"
per AM.md. Briefly activated in AI.1 (`9155adb`), widened in AI.2
(`bc1f7c3`), emission-aware e-graph cost in AI.3 (`78e4226`), then
ripped out.

**Root cause**: the tier lattice was orthogonal to the payload/cursor
decision the emitter actually needed. Tier B "Direct emission" required
the same information the later `TypeDesc::is_scalar_payload` query
answered directly. Adding a tier lattice created a second decision
surface that had to agree with the first.

**Lesson**: one decision surface. Per memory `feedback_no-orthogonal-codepaths`:
arena allocation is singular. Same principle must apply to emission —
the payload-layout planner is the single decision surface, not a tier
axis riding on top.

### 2. Full structural pre-scan (simdjson-style bitmap)

**Fate**: added in early tranches, deleted in AQ.5 (`2f7c1bd`,
"delete structural dispatch infrastructure"). Per AQ.md §4, "structural
pre-scan is no longer viable" — cost 300µs to produce the bitmap, post
SIMD-bitmap WS scan dropped to 110µs, so the pre-scan was net-negative.

**Root cause**: simdjson amortises the pre-scan across many random-access
queries to the same tape. bbnf emits linearly in document order anyway,
so the pre-scan has no amortization opportunity. The WS bitmap caching
inside `ParserState.ws_bitmap` captures the same locality benefit
without the pre-scan cost.

**Lesson**: techniques from external libraries must be composed against
the actual access pattern, not imported wholesale. bbnf's tape is built
linearly; simdjson's tape is **queried** linearly (via ondemand) or
**randomly** (via DOM) — different regime.

### 3. Structural dispatch substrate (first attempt)

**Fate**: AO phase 0 shipped the codegen (`docs/tranches/AO.md` marks
"code complete for Phase 0, never exercised end-to-end"). No production
grammar called `scan_structural`. Rebuilt in AP.1b as "synchronized
peek-only structural dispatch" (`2fa3172`).

**Root cause**: the flag to flip into structural mode was never wired
from grammar analysis to codegen. AO shipped the substrate but not the
activation.

**Lesson**: per memory `feedback_execute-planned-architecture` — landing
substrate without activation produces dead infrastructure. Must end-to-end
a thin vertical slice first, then fill in breadth. AP.1b succeeded because
it shipped activation and substrate together.

### 4. The VM/bytecode interpreter arc (2026-03-15 → 2026-04-12)

See dedicated section below — this is the "1000-commit DTA/PSI era."

### 5. Direct-to-struct projection (dormant through AR, reactivated AU)

**Fate**: `compute_payload_layouts` returned `HashMap::new()` for every
production grammar through AR; `crates/ir/src/passes/payload/layout.rs:56`
and AR audit `audit-direct-struct.md` document the dormancy. The
`-> f64` / `-> 0u8` / `-> true` MapExpr arrow lowering produced
`FnDescriptor::Expr { return_type: None }` — all three type-shorthand
extraction paths in `lower/expression.rs:1229` failed on inlined value
atoms. AU.1 (`83357e4`) fixed `branch_pushes_children` and payload
projection finally fired for JSON leaves.

**Root cause**: the lowering pass and the type-projection pass agreed
on the _happy path_ (standalone rule with bare-ident return type shorthand)
but diverged after inlining. Six grammars, zero activation — the
infrastructure was sound; the trigger never fired.

**Lesson**: memory `feedback_typed-materialization-invariant` names this
exactly — every `->` in the grammar must reach the tape emitter. BA must
make this an acceptance gate, not an aspirational target.

## The DTA/PSI era — anatomy of the near-miss

"DTA/PSI" in the user's framing maps to the **VM/bytecode interpreter
substrate**: bbnf-ir crate creation (`1710d6f`, 2026-03-15) through VM
dispatch tables (`4a8f3db`, Tranche X), DFA engine migration (`be01787`,
2026-03-30), token-dispatch opcode (`45fae01`, 2026-04-03), and the
extended epsilon elimination (`6345dbc`, 2026-04-08). The arc ended
with VM API rewrite at commit `97987a0` (2026-04-12) and de facto
supersession by the tape-first monolithic path in AN/AO/AP.

**Span**: 2026-03-15 to 2026-04-12 — roughly 400 commits in the
post-monorepo era; not literally 1000 but the longest continuous
architectural investment in the modern history. The user's "1000
commits nearly trying to implement" is the subjective weight, not a
literal count.

**Goal**: a canonical IR + bytecode VM that could interpret any BBNF
grammar at runtime, with hot grammars lowered AOT to Rust. Three-tier
execution: IR interpreter (dev), bytecode VM (WASM), native Rust (prod).

**What was salvaged**:

- **bbnf-ir crate itself**. IR passes are now the single analysis
  substrate. All memory references (`feedback_unified-propagate`,
  `feedback_csp-always-optimize`, `project_analysis_consolidation`)
  point at this outcome. AST analysis was ported to IR passes in
  commit `ab1b034` (2026-04-01).
- **CSP + e-graph integration**. The VM era forced a shared cost model
  (`480d2d0`, Y.6a). Surviving the monolithic codegen collapse
  (`92c68f6`, 2026-04-05, "-4515 lines") required both systems to agree.
- **Debug infrastructure**. Source maps, DebugBreak, VM hooks
  (`40fe6f9`, 2026-03-25). Still standing behind the `@debug` directive
  and `DebugBreak` IR node.
- **Token-dispatch opcode + regex DFA in codegen**. The VM needed dispatch
  tables; the same tables now drive AOT codegen.
- **Bootstrap self-hosting**. `feedback_no-workarounds-self-hosting`
  and `project_grammar-authoritative-status` are direct outputs.

**What was discarded**:

- **Monolithic codegen path** (`92c68f6`, −4515 LOC, "delete monolithic
  codegen"). The VM had been generating a fusion codegen that bypassed
  the rich AST — killed for correctness (couldn't preserve semantic
  parity with lightningcss-shape CSS).
- **SharedHelper ghost substrate** (Y.2, `b0721ca`). The VM expected a
  helper surface; after AOT won, the helper was dead.
- **Family recognizer ghost variants** (Y.4, `c8d9eab`). Runtime pattern
  dispatch became AOT structural dispatch.
- **InlineFusion call strategy** (Z.5, `5692ffd`). The VM fused to avoid
  call overhead; AOT inlines directly via `CallStrategy::InlineBody`.
- **Full tier lattice** (AM.1, `7608530`). See EmissionTier failure above.

**Honest diagnosis**: the VM arc was not a failure of architecture — it
was a failure of **sequencing**. The user's `feedback_refactor-first-order`
names the discipline: refactor → optimize → grammar/semantic. The VM
tried to land all three at once. Tape-first in AB/AC/AE succeeded precisely
because it was refactor-only; optimization (AM/AN/AO/AP/AQ) and semantic
parity (AR/AS/AT/AU) followed in separate tranches.

**What to retain for BA/BB/BC**: the IR crate is the substrate. The VM
is not dead — the `vm/` module at `crates/ir/src/vm/` still compiles
and supports debug interpretation. But it is no longer the primary
execution target. BA's code path is AOT-only via the tape emitter;
the VM continues to serve as a **reference semantics** oracle for
regression tests.

## SOTA JSON technique catalog

Techniques extracted from simdjson, sonic-rs, yyjson, lightningcss,
egg/equality-saturation, and ondemand literature. Each entry: technique,
buys, composition with bbnf's IR/egraph/CSP machinery.

### simdjson — two-stage + 64-bit tape

**Technique**: stage 1 builds a structural bitmap over 64-byte SIMD
blocks; stage 2 walks structural positions and writes to a tape where
each element is `('c' << 56) + payload_56bit`. Containers have
paired open/close elements with forward/backward pointers. Strings live
on a separate scratch tape with length-prefix + null-terminated UTF-8.

**Buys**: random access to any value in O(1) via the forward pointer.
DOM lookups need no re-parse.

**Composes with bbnf**: bbnf's `TapeRec` (16 bytes) already matches the
simdjson tape shape at the record level. What bbnf does NOT have is
the **forward/backward container pointer**. Currently `child_off` is a
one-way reference; a closing tape record with a backward pointer to the
opener would enable O(1) parent walks. **Candidate for BA**.

**Sources**: [simdjson tape documentation](https://simdjson.github.io/simdjson/md_doc_tape.html),
[simdjson/simdjson · GitHub](https://github.com/simdjson/simdjson).

### simdjson ondemand — lazy forward-only

**Technique**: structural bitmap stays, but no tape is materialised.
The API returns an iterator that advances through the bitmap, parsing
values lazily. Only the values the user touches are fully validated.
2–4× faster than DOM when the consumer reads a subset.

**Buys**: pay-for-what-you-use. Small consumers over large documents
win big.

**Composes with bbnf**: bbnf's view layer is already lazy at the
**accessor** level — `children()` is a zero-alloc iterator. What it is
not lazy at is the **tape build** step; every record is appended during
parse. An ondemand-style bbnf would defer tape emission until the view
consumes. This is a large re-architecture, not a BA-scope fit; flag
for future tranche (BD+?).

**Sources**: [ondemand vs DOM performance](https://github.com/simdjson/simdjson/discussions/2201),
[Keiser 2024 on-demand paper](https://onlinelibrary.wiley.com/doi/10.1002/spe.3313).

### sonic-rs — no tape, direct-to-struct via serde

**Technique**: sonic-rs explicitly rejects the two-stage approach. It
parses directly to a Rust struct via serde's `Deserialize`. Temporary
data structures are eliminated. `LazyValue` wraps a raw valid JSON
slice for pointer-path extraction (`pointer!["a", "b", 1]`).

**Buys**: fewer allocations than DOM, direct struct population, and a
pointer API that is ergonomic for "extract 3 fields from a huge
document" workloads.

**Composes with bbnf**: bbnf's typed-view layer is the equivalent
**if** payload projection fires. A bbnf view accessor `v.field()` on
a tape with active payload is morally identical to sonic-rs's direct
struct population — both read the JSON once, store typed values, and
expose accessors. The differentiator is that bbnf's shape is
**grammar-derived**, not user-declared. The `pointer!` path equivalent
in bbnf would be an IR-pass-generated path query over the tape —
candidate for BB.

**Sources**: [sonic-rs (cloudwego)](https://github.com/cloudwego/sonic-rs),
[sonic-rs LazyValue docs](https://docs.rs/sonic-rs/latest/sonic_rs/lazyvalue/struct.LazyValue.html),
[sonic-cpp](https://github.com/bytedance/sonic-cpp).

### yyjson — linked-list, no heavy SIMD

**Technique**: pure C89, no mandatory SIMD. Achieves 1.72 GB/s on EPYC
vs simdjson's 1.52 GB/s by exploiting modern CPUs' ILP, branch predictor,
and low misaligned-access penalty. Storage is a linked list of value
cells, not a tape.

**Buys**: portability, simplicity. Proves SIMD is not the frontier.

**Composes with bbnf**: a negative result for the BBNF direction —
bbnf is already SIMD-heavy in scanners; yyjson says SIMD is not
where the next ~10% lives. The frontier is **dispatch** (key dispatch,
structural dispatch) and **allocation** (payload in-place). bbnf's AP.4
key dispatch (`95842dc`) and AP.5 NibbleLut (`a603df9`) are examples
of this frontier.

**Sources**: [yyjson introduction](https://ibireme.github.io/yyjson/),
[yyjson/yyjson · GitHub](https://github.com/ibireme/yyjson).

### lightningcss — grammar from CSS spec

**Technique**: parses all values using the CSS specification grammar
and exposes a specific value type for each property. Built on Mozilla's
`cssparser` and `selectors` crates. A `lightningcss-derive` proc-macro
supports type generation.

**Buys**: no downstream tool re-interprets token streams. Values are
typed at parse time. A transformer that wants to manipulate `<length>`
operates on `Length`, not on `Token::Dimension("10", "px")`.

**Composes with bbnf**: bbnf's position is **strictly stronger** —
the grammar is explicit and typed at source, not encoded in Rust
structs. `feedback_beat-lightningcss-target` is the mandate: match
parity on every typed value in CSS L4, derived from the grammar. The
failure in current AT is that CSS L4 has 0 scalar-payload projections;
the planner accepts no rule. BA must land the activation; BB/BC must
prove parity.

**Sources**: [Lightning CSS](https://lightningcss.dev/),
[lightningcss · GitHub](https://github.com/parcel-bundler/lightningcss),
[lightningcss-derive](https://lib.rs/crates/lightningcss-derive).

### egg — equality saturation + e-class analysis

**Technique**: e-graph stores equivalence classes of e-nodes; rewrites
add equivalences without replacing terms; a cost model extracts the
best term per root. The Ruler/Enumo line of research uses e-graphs to
**enumerate rewrite rules** from a grammar + interpreter.

**Buys**: non-destructive optimisation. Cost-model-guided extraction
of the globally best form.

**Composes with bbnf**: bbnf-egraph is already present and derives
`Language` automatically from IR enum shapes
(`feedback_derive-language-macro`). The **next** frontier is inverse —
using egg to **discover** rewrite rules over bbnf's grammar IR. BA could
land a Ruler-style pass that enumerates candidate grammar transformations
(e.g., `Alt([Map(Lit("a"), 0), Map(Lit("b"), 1)]) ≡ TokenDispatch(…)`)
and validates them against the interpreter.

**Sources**: [egg](https://egraphs-good.github.io/),
[Rewrite Rule Inference (Nandi et al. 2021)](https://arxiv.org/pdf/2108.10436),
[egg SIGPLAN blog](https://blog.sigplan.org/2021/04/06/equality-saturation-with-egg/).

### Parser combinator + tape (the bbnf synthesis)

No external direct precedent. parse-that's bespoke combinators already
emit to the tape via `TapeBuilder::push_{leaf,compound,leaf_with_*}`.
The innovation is the **typed view layer generated per rule** over the
flat tape — neither combinator-to-AST nor simdjson-to-DOM, but
combinator-to-tape-to-typed-view.

**Composes with bbnf**: this IS bbnf at HEAD. What's missing: full
payload activation (per §AU blocker list), aggregate struct projection
(`StructRegistry` still empty at HEAD per AU.4.2), and grammar-derived
pointer paths (sonic-rs-equivalent).

## Grammar-derived semantic parity — the discipline

Concrete definition. Every assertion is an acceptance gate, not
aspiration.

1. **Every `->` reaches the tape emitter.** `MapExpr` nodes with an
   explicit return type (`-> f64`, `-> true`, `-> 0u8`, `-> Named(T)`)
   must appear as scalar or aggregate payloads in the tape. Gate:
   `cargo expand | grep -c 'push_leaf_with_f64'` ≥ the count of
   `-> f64` in the grammar.

2. **Zero hardcoded parse logic.** Kernels (`scan_json_number_f64`,
   `css_ident_fast`, `scan_ws_block_comments`) are selected by
   `scanner_plan.rs` from regex classification; they are never invoked
   by name from the emitter. Gate: `grep -rn 'scan_json_number_f64'
   crates/core/src/backend/rust/emitter/ | wc -l` == 0 (only
   `scanner_plan.rs` names kernels).

3. **IR passes are the ONLY place semantics live.** `crates/ir/src/passes/`
   owns type projection, payload layout, CSP, normalization, dispatch
   selection. Backends consume `GrammarIR` and emit. No backend pass
   re-derives facts the IR already proved. Gate: no
   `ir.strings.iter().position(|s| s == pattern)` in backend code
   (AR critique §7).

4. **AST analysis does not exist as a separate module.**
   `project_analysis_consolidation` mandates IR passes as the single
   source of truth. The old `crates/analysis/` is post-refactor the
   AST-side LSP surface only (`bbnf-analysis`); anything semantic
   moves to IR.

5. **Bootstrap idempotency.** `cargo run --bin regen` produces byte-
   identical `generated.rs`. AU.4.5 currently blocks on +770/-479
   staleness. Gate: bootstrap regen diff is zero bytes.

6. **Parity fixtures as hard gates.** AC.2 introduced tape-first
   parity; AU expands to tape_golden per grammar. Gate: 22/22
   AU golden + N/N per-grammar golden fixtures pass.

The discipline is negative space: anything the grammar does not say,
the backend does not do.

## Gestalt proposal — BA/BB/BC recomposition

Tranche letters B1, AY-II, BA, BB, BC exist only in forward-planning
context at this point; none has a `docs/tranches/<X>.md` at HEAD. The
current stable tranche is AU (in progress per `docs/tranches/AU/PROGRESS.md`).
This section proposes forward structure.

### Dependency chain

```
AU (payload activation + projection truth)
  ↓
AY-II (audit close: payload plumbing end-to-end, string decode, bench parity)
  ↓
B1 (toolchain + regen discipline — must precede BA's grammar work)
  ↓
BA (grammar-derived semantic parity: every `->` reaches the tape; CSS L4 typed)
  ↓
BB (sonic-rs-equivalent pointer-path + ondemand-style lazy view over tape)
  ↓
BC (ruler-style e-graph rewrite rule inference over grammar IR)
```

Each arrow is a hard dependency: the downstream tranche cannot land
until the upstream invariant is gated.

### BA — Grammar-Derived Tape Activation

**Goal**: close the direct-to-struct projection failure first diagnosed
in AR `audit-direct-struct.md`, taken to end-to-end activation across
all 6 production grammars.

**Key invariant**: `compute_payload_layouts` returns a non-empty map
for every grammar that has a typed `->`. Currently zero. Target: JSON
has 2+ layouts (number→f64, bool→bool/u8), CSS L4 has ≥ 20 (all
`-> f64` number rules + hex + unit variants), Sheets has ≥ 10, BBNF has ≥ 5.

**Primary technique union**:

- simdjson-shape tape records (already have) + backward container
  pointer (new).
- lightningcss-shape grammar-derived typing (current gap: 0% for CSS)
  via IR type inference + CSP payload layout.
- sonic-rs-shape in-place struct population via `StructRegistry`
  populated from `project_types` for Named struct rules. AU.4.2
  currently defers to "populate or delete"; BA chooses populate.
- egraph-optimised IR passes (already have) extended with a
  payload-aware cost bonus (AI.3 concept, without the tier axis).

**Dependencies**: AY-II must close the `branch_pushes_children` and
`payload_idx` overflow issues (AU-era); B1 must close bootstrap
regen idempotency.

**Gates**:

1. `grep -c 'push_leaf_with_' generated.rs` ≥ count of scalar-payload
   rules across all grammars.
2. CSS L4 bootstrap ≥ 600 MB/s (current 513).
3. `StructRegistry` non-empty for JSON `pair`, CSS declaration/dimension,
   Sheets cell, BBNF rule.
4. lightningcss typed-value parity: for every `<length>` rule in CSS L4,
   bbnf view returns an equivalent typed Length.

### BB — Lazy Pointer-Path + Typed Query over Tape

**Goal**: match sonic-rs's `pointer!["a", "b", 1]` ergonomics using
grammar-derived paths, with ondemand-style laziness for large documents.

**Key invariant**: a query `Path::from("$.foo.bar[0]")` against a
parsed tape materialises only the tape records on the path. Off-path
records are never read. Pay-for-what-you-use.

**Primary technique union**:

- simdjson ondemand's skip-forward bitmap (new: over bbnf's tape rather
  than input bytes — skip a container by reading its `child_off`).
- sonic-rs's pointer-macro ergonomics (new: grammar-derived, since
  bbnf knows the rule names).
- bbnf IR passes to compile paths: a `Path` is typed against the
  grammar at compile time; invalid paths fail to compile (stronger
  than sonic-rs).
- backward container pointer from BA — enables ondemand's forward
  skip to land in O(1).

**Dependencies**: BA must have delivered payload activation and
backward pointers; otherwise pointer paths cannot type-check.

**Gates**:

1. `Path::compile("$.pair[0].number")` against JSON grammar produces
   an `f64` typed accessor.
2. Benchmark: "extract 3 fields from citm.json" is 3× faster than full
   parse.
3. Invalid path (`$.foo.nope`) is a compile error against the grammar.

### BC — E-graph-driven Grammar Rewrite Inference

**Goal**: close the loop on `feedback_pluggable-components` and
`feedback_csp-always-optimize` — let the e-graph **discover**
grammar-level rewrites, not just apply a fixed rule set.

**Key invariant**: a Ruler-style enumeration pass produces candidate
rewrite rules over the grammar IR; an interpreter validates equivalence;
accepted rules are persisted into `cost_config`.

**Primary technique union**:

- egg's equality saturation + Ruler's CVC-style rule inference
  (Nandi et al. 2021).
- bbnf's IR interpreter (salvaged from the VM era!) as the equivalence
  oracle.
- CSP solver to schedule candidate rule application with cost-model
  awareness.
- grammar-derived discipline: candidate rules are surface syntax
  over `IrNode`, not hardcoded patterns.

**Dependencies**: BA provides the payload-activated IR; BB proves
the view layer correct; BC then has a settled semantic surface to
infer over.

**Gates**:

1. Ruler produces ≥ 5 novel rewrite rules not present at HEAD.
2. All inferred rules pass the interpreter equivalence oracle on
   6 production grammars.
3. After applying inferred rules, at least one grammar's codegen
   shrinks by ≥ 10 LOC (end-to-end win).

## Open research questions

1. **Is the backward container pointer worth the tape bloat?**
   Adding a `parent_off: u32` to `TapeRec` grows records from 16 to
   20 bytes (+25%) or forces a second sidecar Vec. The sonic-rs-equivalent
   pointer path only wins if O(1) skip-forward is possible. Measurement:
   parse citm, walk object parents via a synthetic backward pointer,
   compare to parent-reconstruction via re-scan. If re-scan is within
   2×, skip the pointer.

2. **Can the VM continue to serve as the equivalence oracle for BC?**
   The VM arc ended; but the `crates/ir/src/vm/` module still exists.
   BC needs a provably-correct interpreter — is the VM enough, or does
   it have semantic drift from the AOT emitter? A parity gate across
   all 6 grammars on all test fixtures answers this in one benchmark.

3. **Does payload activation break AQ's canada baseline (1796 MB/s)?**
   AU.1 showed a regression from 1796 → 1294 MB/s after wiring
   `push_leaf_with_f64`. PROGRESS.md flags this as unexplained.
   BA cannot ship until the cost source is nailed — is it cache
   (additional 8B/record), branch prediction on the payload match,
   or LLVM codegen variance? Samply-driven answer required.

4. **How does grammar-derived typing interact with self-hosting?**
   `feedback_hybrid-grammar-host` and `project_grammar_authoritative_status`
   (Phase 3 host fns pending) say semantics split between grammar
   (leaf) and host (context/recursive). BA's "every `->` reaches
   the tape" must not exclude host-dispatched types. The question:
   are host-typed rules a subset, superset, or orthogonal to tape-payload
   rules? If orthogonal, we need two-surface typing.

5. **Should BC's rule inference operate on IR nodes or on grammar
   source?** E-nodes over `IrNode` are concrete; rules over grammar
   source would need re-parsing through the frontend. The tradeoff:
   IR-level rules are guaranteed well-formed but may miss rewrites
   that depend on source-level structure (e.g., comment adjacency).
   Default: IR-level, with a source-level escape hatch gated on a
   BD+ tranche.

## Provenance

Retrospective built from:

- `git log --oneline` over 897 commits (2026-02-26 monorepo restart
  through 2026-04-14 AU session 1).
- `docs/tranches/{V,W,X,Y,Z,AA..AU}.md` + subdirectories.
- `crates/bbnf-tape/src/lib.rs`, `crates/ir/src/lib.rs`,
  `crates/egraph/src/lib.rs`, `crates/csp-solver/src/lib.rs` crate-doc
  headers.
- Memory feedback references as cited (format: `feedback_<name>`).
- External sources listed inline per §SOTA JSON technique catalog.

No conversation history, plan files, or process metadata referenced;
all citations are either commit hashes, tranche letters, crate paths,
or external URLs.
