# BB-VALUE-TAPE-P0 — complete generalized JSON/CSS L4 vertical portfolio

Status: **born RED; profiling and corpus formation**  
Authority: prototype evidence only  
Production movement: prohibited  
Selection floor: four complete orthogonal implementations; all four must close.

## 1. Product question

Find the smallest complete architecture that can derive a parser, regex/scanner
system, exact semantic types, analysis, cost-selected lowering, modular backends,
generated Rust/TypeScript/WASM surfaces, and a generalized hybrid value/tape API
from BBNF module files for both JSON and the complete declared CSS L4 conformance
scope. Tape layout is never the public semantic model. The product must be fast for
parsing, typed navigation, serialization, query, and crossings; safe for borrowed
and owned data; deterministic; and equally derivable for an unseen grammar.

No candidate may branch on grammar name, rule name, benchmark, or corpus. JSON and
CSS are mandatory co-equal controls. A candidate missing either or any lock is
incomplete and does not enter performance ranking. Narrow graph, type, invocation,
JSON, tape, regex, or assembly carriers are assay organs, not portfolio prototypes.

## 1.1 Every-lock completeness

Each VT candidate independently implements the entire constitution:

- PT-E execution, PT-J TypeScript/package, PT-C coordinates/crossings, PT-K Rust
  kernel/scanners, PT-R regex/generation, and PT-V isolated packed consumer proof;
- BB-1 type/constraint spine, BB-2 graph/fixed-point/equality/cost solving, BB-3
  immutable IR/stages/backends/provenance, and BB-7 modules/grammar/bootstrap;
- structure/test isomorphism, JSON and CSS totality, value/tape fabric, Rust/TS/WASM
  parity, generated code, Apple ARM kernels, deterministic artifacts, resources,
  profiling, two hostile gestalt passes, and an exact terminal disposition.

Completeness is per candidate. A shared compiler, solver, regex engine, runtime,
generator, ARM library, package, or test helper would turn four configurations
into one implementation and is forbidden.

The historical nine-lock portfolio is a no-falloff input, not a superseded naming
scheme. Each candidate's private `evidence/lock-coverage.tsv` must trace every
current born-RED bullet, the full task and A/B/C/D families of legacy LOCK-1…9,
XL-A…H, and every atomic `INVENTORY.md` section-D tail item to exact implementation,
external test, mutation, profile, artifact, two critics, adjudication, and terminal
disposition. Every candidate implements all nine lock tasks. An incompatible
legacy family may be retired only by a separate runnable full-contract assay; it
cannot vanish because another family was selected. Aggregate “lock complete” rows,
shared evidence, or any terminal `UNMAPPED`/`UNRUN`/`UNPROVEN` row are RED.

## 2. Measured starting facts

The incumbent CSS grammar is not credited as full CSS L4. It currently comprises
15 files, 1,329 lines, 56,598 bytes, and 276 rule declarations. The coverage wave
must freeze the union of every constituent and pre-CR exception named by the
[CSS Snapshot 2026](https://www.w3.org/TR/css-2026/),
[CSS Syntax Level 3](https://www.w3.org/TR/css-syntax-3/), every CSS Level 4
specification and Level 4 feature named by W3C Current Work, and all normative
parsing dependencies, then bind their WPT tests. The final union is exhaustive,
versioned, and mechanically reproduced; a candidate cannot select a convenient
subset or redefine “CSS L4.” Missing syntax,
error recovery, values,
serialization, or typed projection is RED.

The phase-one W0 publication capture is checked at
`conformance/css-l4/data/inventory.json`: 21
Level-4 specifications, CSS Snapshot 2026, CSS Syntax 3, dated W3C publication
bytes/hashes, and WPT revision `37cd7ff74eb974fd41600bf7bc01d37576abf8b5`.
The corrected membership union at `conformance/css-l4/data/scope.json` is derived
from CSSWG commit `c7573530343759ace8e46438a1fa2c44515b5554` and replays at 84
specifications. Dated publication pins for the added members, both hostile passes,
and normative-row extraction remain RED; neither count earns semantic coverage.

Existing M5 Max profiles are machine evidence, not design verdicts:

- direct sinks spend 62–81% self time in string handling on twitter,
  `unicode_mixed`, and random; exact numbers dominate numeric shapes;
- the fused generated parse hub hides nearly all self time and needs a no-inline or
  PC-level profile before another kernel is prescribed;
- current tape/lazy JSON beats measured native controls on several large/numeric
  corpora but trails simdjson by roughly 2× on several small object-heavy corpora;
- Unicode escapes and short keys remain named hot paths;
- the prior event-cursor prototype regressed on all six measured corpora, so an
  event tape has no inherited advantage;
- current `TapeBuilder` and CSS arena code contain unsafe lifetime transmutation,
  and `parse_with` contains a full-parse fallback. Both are born-RED controls;
- certified historical organs include fused string scanning, stop-early get,
  direct-to-struct sinks, CSS table tokenization, a regex prefilter, and a TS tape
  crossing. Each must be re-earned against current source and the common corpus.

Before prototype design freezes, P0 must recapture all of these measurements from
the live executable with exact source/toolchain/corpus hashes.

## 3. Conformance scope

### JSON

The oracle covers RFC 8259 behavior plus the campaign's explicit policies for
duplicate keys, number range, complete input, UTF-8 byte spans, decoded strings,
diagnostics, and source-order preservation. Positive, negative, depth, resource,
mutation, query, serialization, and round-trip cases are required.

### CSS

“Complete CSS L4” is a versioned coverage claim, not a filename. P0 first freezes
the exhaustive union of Snapshot constituents/exceptions, W3C Level-4 work, Syntax,
and normative dependencies, then creates a machine-readable matrix over every item,
including at minimum syntax/tokenization, values and units, color, selectors,
media/conditional queries, cascade/layers, nesting, custom properties, functions,
math, transforms, filters, gradients, easing/animation/keyframes, properties and
descriptors, at-rules, unknown constructs, error recovery, and serialization.

The parser must preserve the CSS Syntax token/component-value model even when a
typed module rejects a property. Unknown/vendor/custom syntax follows the declared
forward-compatible policy; it is not silently discarded by the value layer.

The matrix binds each normative production/algorithm family to:

- one or more BBNF productions;
- grammar-derived type/value rows;
- positive and negative WPT or independently authored fixtures;
- recovery and diagnostic expectations;
- canonical and source-preserving serialization expectations;
- one responsible module and one terminal status.

No coverage percentage is accepted while a required row is `UNMAPPED`, `UNRUN`, or
`UNPROVEN`.

## 4. Common observable API

Every candidate independently derives an equivalent observation surface without
sharing implementation. The following is an oracle vocabulary, not an input schema
or handwritten type declaration:

```text
SchemaRevision
DocumentRevision
SourceOwner
TypeId / FieldId / VariantId
ValueId / StringId / AggregateRange
Utf8ByteSpan / checked Utf16Map
Document
ValueRef
TapeCursor
TypedPath<T>
Visitor / EventSink
Diagnostic / RecoveryProduct
Serializer / SourcePreservingWriter
```

Required operations:

1. parse complete input and return a typed result;
2. obtain root, type, variant, span, scalar, aggregate length, ordered child,
   record field, and source lexeme;
3. distinguish missing, null/unit, invalid, recovered, and excluded punctuation;
4. iterate arrays, records, variants, CSS component values, tokens, rules,
   declarations, functions, and unknown forward-compatible nodes in source order;
5. preserve JSON duplicate keys and CSS repeated declarations/fallback order;
6. navigate a revision-qualified typed path and reject foreign schema/source IDs;
7. visit or stream normalized events without exposing representation addresses;
8. materialize a detached owned subtree explicitly;
9. serialize canonically and, where promised, source-preservingly;
10. map UTF-8 bytes to checked UTF-16 positions for TypeScript/WASM;
11. expose exact resource/cost counters without changing semantics;
12. fail typed on corrupt tape, invalid range/tag, stale revision, exhaustion, or
    unsupported capability.

The hybrid ownership law is common: source lexemes may borrow only checked ranges
from a document-owned source; decoded or synthesized values live in an owned arena;
small scalars may be inline; aggregates use bounded ranges/indices. Public values
never borrow builder memory, use transmuted lifetimes, or depend on pointer identity.

## 5. BBNF-only source of types and no-overfit law

The BBNF module files are the sole semantic and type input. Modules, imports,
grammar operators, mappings, and language-native decorators must suffice to infer
records, variants, recursive binders, lists, tuples, options, units/dimensions, CSS
token/component families, exclusions, host effects, recovery products,
materialization, serialization, and public Rust/TypeScript shapes. Any prelude or
host signature is itself a versioned BBNF module imported through the same language.

Forbidden inputs include handwritten Rust enums/structs, JSON/CSS rule tables,
sidecar schemas, target-specific type manifests, build-script annotations, hidden
host registries, consumer adapters, and a post-hoc schema repair. A candidate
emits its semantic schema, cost problem, lowering plan, types, and provenance as
derived outputs; it never accepts them as extra source.

A second unseen grammar is opened only after candidate source freezes. If supporting
it requires a new engine branch or new type facts outside BBNF, the candidate is
overfit and RED.

## 6. Four orthogonal implementations

Each lives in its own standalone compiler/runtime/package universe, fixture copy,
lockfile, source-isomorphic tests, receipt producer, and artifact root. Candidates
may share only the BBNF language specification, exact BBNF module bytes, corpus
bytes, measurement protocol, and read-only oracle observations.

### VT-1 — immutable staged compiler with compact tagged word tape

An immutable `Module -> Program -> Graph -> TypeSolution -> Facts -> Plan -> Code`
compiler feeds a direct/packrat parser and one fixed-width record stream carrying
tag, inferred type, span delta, and payload/index.
Side arenas own strings and overflow payloads. Aggregates use begin/end or range
records. Falsifier: tag density or random child access requires auxiliary indexes
that erase the compactness/cost advantage.

### VT-2 — independently verified module with schema-specialized row stores

An untrusted assembler emits a complete semantic/code module; an independent
bounded verifier rederives grammar, types, facts, costs, and code relations before
execution. Generation creates one row store per inferred record/variant family,
while a small generic tape preserves order and source relations. Falsifier:
verification is not independent, generated code/bytes
explode on CSS, schema evolution breaks the generic API, or compilation/cold cost
outweighs steady wins.

### VT-3 — persistent incremental compiler with rope tape

An incremental LR/GLR or equally proved edit engine maintains immutable grammar and
parse revisions. A balanced persistent chunk tree stores records and source segments; revisions
share unchanged chunks. Falsifier: clean parse/traversal cost exceeds the ceiling or
real incremental edit traces do not recover the structural-sharing cost.

### VT-4 — generalized GLL/SPPF engine with eager typed arena and order tape

GLL descriptors and an SPPF preserve real grammar ambiguity until typed semantic
selection; typed values are then materialized once into flat family-owned arenas,
while a compact order
tape preserves source and heterogeneous traversal. Falsifier: allocation/resident
cost loses without a compensating repeated-query/serialization win.

### Non-credit architecture bank

The eight former full-candidate charters remain orthogonal falsification and organ
assays only: stratified-relational/columnar, typed-event transducer, bounded-
streaming/segmented regions, persistent-reply/lazy source tape, AOT direct-to-
struct/proof tape, verified offset image, graph-native relational tape, and
profile-selected hot/cold adaptation. They may challenge a load-bearing choice or
provide an independently measured control. They do not become candidates, do not
increase the count above four, and cannot donate shared implementation. If an
active charter proves intrinsically unable to close, one banked charter may
replace it only by explicit supersession and a new independent full implementation;
the terminal floor remains four complete verticals.

## 7. Exact private topology

Each `VT-n` uses the Goldilocks shape below; filenames strip the candidate name
inside its module:

```text
prototypes/value-tape/vt-n/
  Cargo.toml
  Cargo.lock
  rust-toolchain.toml
  README.md
  src/
    lib.rs
    language/{module,decorator,source}.rs
    program/{syntax,identity,normalize}.rs
    graph/{dependency,type_flow,control,scc}.rs
    types/{infer,row,effect,solve,close}.rs
    regex/{syntax,hir,algebra,nfa,dfa,vm,cost}.rs
    analysis/{facts,cost,solve}.rs
    lower/{plan,code}.rs
    runtime/{execute,document,tape,value,navigate,visit,serialize,wire,budget}.rs
    backend/{rust,typescript,wasm,arm}.rs
    artifact/{emit,publish}.rs
  tests/
    language/**
    program/**
    graph/**
    types/**
    regex/**
    analysis/**
    lower/**
    runtime/**
    backend/**
    artifact/**
    oracle.rs
    mutation.rs
    crater.rs
    assay.rs
    fixtures/{json,css,holdout,bench}/**
  web/
    package.json
    package-lock.json
    src/{index,wire,view}.ts
    test/{index,wire,view}.test.ts
  evidence/README.md
```

A candidate may merge adjacent files when the implementation is too small to
justify the boundary, but source/test isomorphism and the owning invariant remain.
No inline tests, shared helpers, path dependencies, source imports, or workspace
membership are permitted.

## 8. Profiling and cost protocol

### P0.A — incumbent attribution

Build no-inline and inlined diagnostic binaries for current JSON and CSS. Capture
Samply/PC-level profiles, allocation counts, peak/resident bytes, tape/arena high-
waters, branch/cache counters where available, code size, build time, and artifact
bytes. Attribute at least 95% of samples to named semantic operations. An opaque
fused hub blocks design freeze.

### P0.B — corpus stratification

Classify inputs by structural density, token length, ASCII/Unicode/escape mix,
numeric density, nesting, recovery rate, repeated query shape, serialization mode,
and size. Training, tuning, and holdout corpora have disjoint hashes. CSS WPT and
real stylesheets are reported separately; generated stress cases cannot substitute
for either.

### P0.C — paired measurement

On Apple/aarch64, run cold alternating pairs in fresh processes and steady state in
bounded reused processes. Disposition-affecting claims require at least 200 pairs
per representative stratum, three windows, raw samples, load average, exact
toolchain/source/corpus hashes, bootstrap 95% confidence intervals, and real PMU
counters when available. Never synthesize cycles from clock frequency.

The common measurement vocabulary reports separately, while every candidate
derives and implements its own cost problem from the BBNF modules:

```text
semantic coverage and discrepancies
compile wall/RSS and plan wall/RSS
first parse, steady parse, throughput, cycles/instructions per byte
allocations, allocated bytes, retained bytes, peak RSS
construction, root/scalar/child/field/path/query/visit costs
canonical and source-preserving serialization
Rust↔WASM↔TypeScript crossing
source LOC, modules/edges/SCCs, generated LOC, build/package/artifact bytes
unsafe boundaries and verified bytes
```

No weighted score hides a losing dimension. Named product policies may choose among
Pareto candidates only after semantics and resource totality are equal.

## 9. Performance controls and ceilings

Controls include the current generated/tape runtime, current direct sink, selected
historical certified organs re-run at current pins, serde_json for JSON semantics,
current native JSON sidecars where buildable, and current CSS parser controls
selected by the corpus wave. Cross-language controls are not compared as if their
value/diagnostic surfaces were identical.

The mandatory external speed controls are the fastest correct Apple/aarch64 builds
among the latest stable release and the freeze-day default branch of Lightning CSS
and sonic-rs. Discovery on 2026-07-19 found Lightning CSS `v1.32.0`
(`7f8a861bdee476fe90c89a8badeb3fd33a99c51a`) and `master`
`91bfa3fcde39e03ce33a83475f6c55e3c3e50dd2`, plus sonic-rs `v0.5.8`
(`e339451e37a8f2d3f9b93f155985d31fcbcb115a`) and `main`
`03545a9530346fe279b674dd496e037d94204bc5`. VT.W0 must refresh these exact
upstreams, build both release and head fairly, and freeze whichever correct artifact
is fastest for each matched operation. A package version or benchmark chart is not
a control receipt.

Candidate entry ceilings:

- zero semantic/oracle discrepancy and zero discarded required case;
- no panic, UB, hang, partial success, or unbounded resource path;
- no more than 2× the best correct control on any mandatory primary operation;
- no more than 4× code/artifact bytes of the smallest complete candidate;
- no unexplained >25% regression on any corpus stratum;
- any claimed specialization must win its named end-to-end workload by a
  statistically clear margin after compilation/crossing cost.

Production-dominance hard stops apply only after zero-discrepancy totality and the
complete common API are proved:

- CSS must retain the entire frozen Level-4 matrix with no exception and achieve a
  >=5.0x paired speedup lower bound over the fastest correct Lightning CSS control
  on every mandatory matched parse and parse/transform/serialize stratum; 6.0x is
  the target;
- JSON must retain the full grammar-derived typed value/tape surface and achieve a
  >=1.1x paired speedup lower bound over the fastest correct sonic-rs control on
  every mandatory matched parse/value, query/navigation, and serialization
  stratum; 2.0x is the target.

Ratios use the lower bound of the bootstrap 95% confidence interval over at least
200 cold alternating pairs in each of three windows, plus the governed steady-state
run. No aggregate may hide a losing stratum. Control errors remain errors and are
not scored as zero time or an infinite win. A narrow parser, microbenchmark, reduced
diagnostic/value surface, disabled feature, or excluded CSS row earns no ratio.
All four candidates are measured; a complete loser may be BANK/RETIRE, but ADVANCE,
VT.W6, and production movement remain RED until one pure candidate meets both hard
stops with every PT/BB lock closed.

These are retirement floors, not selection scores. Final selection demands a
smaller margin appropriate to the feature and a KISS comparison.

## 10. ARM kernel plane

Every candidate initially implements an independent scalar semantic baseline with
the same externally observed laws. After semantic freeze, independent kernel trials
may provide structural-byte classification,
quote/backslash parity, UTF-8 validation, escape scanning/decoding, digit/number
front ends, keyword matching, whitespace/comment scanning, tape word emission, and
checked copy/transcode.

Follow the dav1d discipline without copying its domain code: narrow per-operation
function tables; scalar defaults; runtime Apple feature selection; handwritten
AArch64 modules grouped by real kernel; and a checkasm-style harness that forces
scalar and each ISA lane over identical randomized/adversarial buffers. Assembly is
not generated for arbitrary grammar logic. BBNF generates tables/plans and selects
eligible kernels by proved preconditions.

No kernel enters a candidate ranking unless disassembly proves the intended
instructions, scalar/assembly observations match, page/tail/alignment cases pass,
and the complete candidate improves. A microbenchmark-only win remains banked.

## 11. Wave execution

### VT.W0 — specification, coverage, and born RED

- complete and freeze the exhaustive CSS Snapshot constituent/exception,
  Level-4, normative-dependency, and WPT union and the JSON oracle;
- map every incumbent grammar rule and missing CSS row;
- recapture profiles and hot leaves;
- seal only the observable contract, measurement vocabulary, resource policy, and
  independent oracle; each candidate must derive its schema, types, cost problem,
  and lowering solely from the BBNF module files;
- create four private compiling placeholders and external RED tests.

Exit: failures are intended semantic/profile assertions, not missing tools or
symbols; no candidate-specific design has seen the holdout.

### VT.W1 — complete independent verticals

Implement VT-1…VT-4 independently in bounded batches. Each must implement every
PT/BB lock from BBNF source bytes through modules, inference, graphs, regex,
analysis/cost selection, execution, value/tape, exact generated Rust types,
Rust/TS/WASM/ARM backends, JSON, the exhaustive frozen CSS L4 inventory, packages, isolated consumer,
resources, and deterministic artifacts before measuring.

Build and continuously verify the private atomic lock-coverage ledger while code
lands. The ledger cannot be backfilled from memory at wave end and cannot accept a
feature without its directly executable proof path.

Exit per candidate: complete correctness twice, no unsafe lifetime escape, and
exact source/receipt ledger.

### VT.W2 — profiling and hard-stop dispositions

Profile all four complete candidates against the same strata. Assign ADVANCE,
BANK, or RETIRE for eventual production selection only; completion credit is not
revoked by losing a fair profile. Do not repair one candidate by importing another.
Open design-frozen holdouts after measurements.

### VT.W3 — holdout and architecture-bank challenge

After all four implementations and tuning policies freeze, open the unseen grammar
and held-out JSON/CSS strata. Use the eight banked charters only as independent
counterexamples against load-bearing decisions. A correct counterexample repairs
or explicitly supersedes the owning vertical; it never creates a shared hybrid or
a fifth candidate.

### VT.W4 — hostile gestalt pass 1

For each of all four candidates, two fresh critics and a fresh adjudicator examine
portfolio optimality, spec fidelity, every API feature, CSS/JSON completeness,
profiles, unsafe boundaries, structure, and greater-product fit. Correct findings
land; source/corpus/receipt ledgers refreeze.

### VT.W5 — hostile gestalt pass 2

Repeat with fresh agents on corrected sources. Any finding resets the clean count.
Only two consecutive clean passes permit convergence.

### VT.W6 — convergence and transpose proposal

Choose one of the four pure candidates. A banked organ may enter the later product
only if independently reimplemented inside the winner and proved to remove a
measured end-to-end limit without adding a second authority; otherwise it remains
banked. Emit a proposal only; production movement needs separate graph and compiler
adjudication.

## 12. Required mutations

At minimum kill: grammar-name dispatch; missing CSS row; skipped WPT; JSON duplicate
loss; declaration reorder; lost unknown CSS component; UTF-8/UTF-16 confusion;
foreign revision; dangling source; transmuted builder lifetime; corrupt tag/range;
wrong schema; partial budget publication; fallback full parse; cache-dependent
semantics; query index drift; persistent revision alias; row-store generator drift;
ambiguity-forest selection drift; fake NEON wrapper; assembly tail overread; stale
packed artifact; source mutation during publication.

## 13. Definition of complete

This portfolio is complete only when all four orthogonal candidates—not four
configuration flags—have each implemented every constitutional lock, inferred
fully formed Rust/TypeScript types solely from the frozen BBNF modules, implemented
the entire frozen JSON/CSS scope and observable API, passed deterministic resource,
bootstrap, ARM, package, consumer, and cross-runtime gates, and produced honest
profiles. Every incomplete candidate has a terminal disposition. The
portfolio may replace an intrinsically blocked charter, but it cannot close with
fewer than four complete implementations. The selected architecture has two
consecutive clean hostile gestalt passes, an unseen
grammar result, a shared BBNF/parse-that cost receipt, and no unmeasured or duplicate
value/tape authority. It also meets the >=5.0x Lightning CSS and >=1.1x sonic-rs
hard stops without any CSS-L4 exception or reduced API surface; 6.0x and 2.0x remain
the respective targets. No production source movement or release is implied.
