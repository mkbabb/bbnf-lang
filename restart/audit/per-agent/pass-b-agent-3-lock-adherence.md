# Pass B — Agent B.3 — Lock-adherence

Date: 2026-05-03
Lens: Apply the 14 locks per Pass-B file.
Source: `docs/HARDENING-PLAN-PROMPT.md` §Gestalt — fourteen locks.

Critical foci per the directive: Lock 1 (tape dead), Lock 4 (per-domain
orthogonal), Lock 5 (IR + per-backend), Lock 6 (xtask emits committed),
Lock 9 (slice-borrow primary), Lock 10 (Pratt+SIMD auto-detect), Lock 13
(no god dirs), Lock 14 (full grammar generalisation).

---

## Lock 1 — Tape and its columnar variants are fully dead

**Verdict**: honoured at production-symbol level; doc-comment narrative is residue.

Evidence — live tape symbols:

```
$ grep -rn "TapeRec\|TapeCursor\|TapeBuilder\|payload_idx\|TapeKind\|FusedBuilder" \
    crates/core/src crates/ir/src \
    | grep -v '^.*//' | grep -v 'tape.*severed' | grep -v 'no.*Tape'
(no matches)
```

No production tape symbols survive. Lock 1 production-side honoured.

Evidence — doc/comment residue (CENSUS §1.2 enumerates ~50 sites):

| Pass-B path | Line | Residue |
|---|---:|---|
| `runtime/bbnf/mod.rs` | 9-17 | "...the tape substrate is severed..." |
| `runtime/json/mod.rs` | 6-8 | "tape substrate severed" boilerplate |
| `runtime/css_l4/mod.rs` | 7-9 | same |
| `runtime/csv/mod.rs` | 9-17 | "regens CSV onto the struct-direct path and then deletes the tape crate" |
| `runtime/google_sheets/mod.rs` | 7-11 | "no `TapeBuilder` / `TapeRec` / `TapeCursor` symbol appears" |
| `runtime/google_sheets/document/canonical.rs` | 14-15 | "cursor-backed `tape::TapeCursor`; that emitter retired" |
| `runtime/google_sheets/document/mod.rs` | 143-145 | same |
| `runtime/css_l4/document.rs` | 168-169 | "post-tape equivalent of the pre-W2-act tape-walk parity surface" |
| `runtime/json/document.rs` | 14, 132 | "older tape projection built a `<Grammar>Value` enum" |
| `runtime/json/document.rs` | 316 | "Mirrors `tape::TapeKind`" |
| `runtime/bbnf/document.rs` | 315 | "implicit `Repeat` shape that tape walkers produced" |
| `runtime/builder.rs` | 4, 7, 48 | "selection between tape and struct happens at codegen time" |
| `runtime/bbnf/serialize.rs` | (mention) | tape mention |
| `runtime/bbnf/view.rs` | 28-33 | "tape-direct `child(i)` accessor; discriminator (replaces tape-era `variant_idx`)" |
| `pipeline/compile/pipeline.rs` | 163 | "Tape-direct ingress: walk the bootstrap tape straight into" |
| `pipeline/compile/mod.rs` | 62 | "Tape-direct ingress" |
| `backend/driver/alt.rs` | 11, 32, 36, 87 | "Under tape-first emission..." |
| `backend/driver/seq.rs` | 11 | "Under tape-first emission..." |
| `backend/driver/analysis.rs` | 168 | "specific `Tape<R>::with_capacity` divisor" |
| `backend/types/mod.rs` | 88-89 | "AM.3 per-branch tape surgery" |
| `backend/rust/ir_types.rs` | 125, 138 | "prettify emitter; tape-first rule emission" |
| `backend/rust/emitter/grammar.rs` | 113, 135, 288, 294-295, 384 | tape-related historical notes incl. `TapeVisitor` |
| `backend/rust/emitter/profile.rs` | (mention) | tape mention |
| `backend/rust/emitter_types.rs` | (mention) | tape mention |
| `backend/rust/ir_enums.rs` | (mention) | tape mention |
| `backend/rust/emitter/shapes/number.rs` | 12, 17, 179 | "TapeKind::Span leaf...TapeRec::PAYLOAD_F64_DIRECT_BIT...tape column write" |
| `backend/rust/emitter/shapes/arglist.rs` | (mention) | tape mention |
| `backend/rust/emitter/shapes/alt_dispatch/mod.rs` | (mention) | tape mention |
| `backend/rust/emitter/shapes/object.rs` | (mention) | tape mention |
| `backend/rust/emitter/shapes/flat/mod.rs` | 24 | tape mention |
| `crates/core/src/grammar/generated/json.rs` | 1154 | "for compositional uniformity with the tape-path" |
| `crates/core/src/grammar/generated/css_l4.rs` | 16208, 27788 | same |

Lock 1 says: "*No tape crate, no `TapeRec`...no 'tape rebranded as
fast-path'*. The substrate IS the typed-enum + slice-borrow." Production
substrate is honoured. The doc-comment narrative is residue —
~30 Pass-B files carry tape-narrative; CENSUS §1 calls scrub-during-regen.

**Critical Lock-1 question**: does `OpenFrame` count as a "tape rebranded"?
The honest answer: yes in spirit. `OpenFrame` is a heap-stack of
partial compounds; it serves the same purpose as the tape's record
stream — a runtime structure that defers the typed projection to
post-parse machinery. Per Lock 1 strict reading, `OpenFrame` violates
"the substrate IS the typed-enum"; per the lock's narrow text (no
named `Tape*` symbol), it doesn't. The synthesis must adjudicate.

---

## Lock 2 — Layout lowering is the canonical IR pass name

**Verdict**: out of Pass-B scope (IR pass; Pass A).

`bbnf-ir/src/passes/layout/` lives in Pass A. Pass-B consumers reference
`StructLayout` from `bbnf_ir::registry` — this is the canonical name.
Per `runtime/builder.rs:50`, the trait surface accepts `&StructLayout`.
Naming honoured downstream; the *generation* of layouts is Pass A.

The Pass-B residue is: `StructLayout` reconstructed at runtime per
RESTART-SKETCH §A.4 #1 — but the *type's name* is correct.

---

## Lock 3 — Cursor-parse + byte-skip unified, with cursor branch elided when path is empty

**Verdict**: honoured structurally; cursor consult on eager path is wasted (RESTART-SKETCH §A.4 #12).

Evidence:

`generated/json.rs:3443-3448` — eager `JsonParser::parse(input)` constructs
a `static __EAGER_EMPTY_PATH: LazyLock<TypedPath<Json,&str>>` and a
`PathCursor::new(&EMPTY_PATH, |_,_,_| ParseFully)`. The eager path
*does* consult the cursor — it gets back constant `ParseFully`. The
cursor calls in `generated/json.rs:1855-1942` (cursor.decide,
cursor.current_kind, cursor.match_field) return constants for the
empty-path eager case but are not constant-folded.

Per Lock 3: "The empty-path case (`__EAGER_EMPTY_PATH`) elides cursor
calls entirely so the eager fast path pays no consultation cost". The
plan's letter is honoured — *one parse function, one cursor consult*.
The plan's spirit is *not* — eager parse pays consultation cost.

This is a Pass-B optimisation gap, not a Lock-3 violation in the
narrow sense. The lock says cursor-parse + byte-skip *unified*; we
have one parser. The follow-on optimisation (constant-fold the cursor
calls when EMPTY_PATH is the binding) is unlanded.

---

## Lock 4 — Per-domain orthogonal optimization (CSP type/layout, e-graph rewriting, miners, shape analysis, cost model compose by output-piping; no unified hypergraph)

**Verdict**: honoured.

Per `crates/egraph/src/csp_scheduler.rs:368` — `CspScheduler` schedules
e-graph rewrite application via CSP-style dirty-domain propagation.
This is *layered*, not unified — CSP drives e-graph; e-graph operates
over IrNode; pattern miners are sequential pre-passes on `passes/recognizers/`.

The crates structure (per §B.1 inventory):
- `crates/egraph/` — domain-agnostic e-graph; consumers implement `Language`
- `crates/egraph-derive/` — `#[derive(Language)]`
- `crates/csp-solver/` — generalised CSP solver; both Rust + Python consumers (per `feedback_csp-solver-crate`)
- pattern miners — `crates/ir/src/passes/recognizers/` (Pass A scope)
- cost model — `crates/egraph/src/cost_config.rs`, `cost_weights.rs`, `extract.rs`

The composition:
- `passes/recognizers/` mines facts (Pass A)
- `passes/csp_strategy/` consumes facts → strategy via `csp-solver` (Pass A)
- `passes/optimize/` consumes strategy → IrNode rewrites via `egraph`
- `backend/strategy/` consumes IR strategy → emit decisions (Pass B)

Output-piping discipline observed; no unified hypergraph; no fused
solver-emitter entry-point. Lock 4 honoured.

---

## Lock 5 — IR + per-backend lower; codegen consumes typed IR, NOT walks grammar IR directly

**Verdict**: partial — Rust backend bypasses; TS+WASM honour.

Per `backend/emitter.rs:1-30`:

```
The compilation driver walks `GrammarIR`, makes target-agnostic decisions
(dispatch strategy, span compression, inlining, etc.), and calls these methods
with pre-resolved data. Each backend implements this trait to produce target code.
```

This is the design statement; intended honour. Per the doc:

```
The Rust backend routes `parse()` through `dta_run` wholesale and
discards per-rule bodies at `emit_rule_function_impl`; it overrides none of
these defaults and the driver's per-rule traversal produces empty tokens
that are discarded downstream. TS + WASM continue to override every method.
```

The Rust backend takes a *separate codepath* — it walks via shape-dispatcher
(`backend/rust/emitter/shapes/`) rather than per-IrNode driver methods.
The 30-method Emitter trait is the IR-walk surface; the Rust backend
uses 3 of them and routes parse through a separate per-shape dispatcher.

This is a soft Lock-5 violation: the codegen *does* consume typed IR
(it doesn't walk grammar source), but two distinct walking patterns
co-exist (per-IrNode for TS/WASM, per-shape for Rust). Per Lock 5:
"There is no source-emit-per-backend duplication; there is no
trait-based emitter walking grammar directly." The Rust shape-dispatcher
walks IR (not grammar) — but it isn't the per-IrNode trait the IR
contract proposes.

The synthesis question: should the Rust backend retire shape-dispatcher
and route through per-IrNode emit_* methods like TS/WASM? Or should
the trait be reshaped to the shape-dispatcher's coarser surface (and
TS/WASM follow)?

Lock 5 narrow-honoured; the cohesion question is open.

---

## Lock 6 — xtask emits committed source artefacts (no proc-macro façade)

**Verdict**: honoured.

Per `xtask/src/main.rs:1-9` — "cargo xtask — workspace build-time
codegen entrypoint... replacing the pre-B2 `bbnf_derive` proc-macro
contract that ran the same pipeline at every consumer's `cargo expand`
time."

Per `xtask/Cargo.toml:1-30` — bin + lib layout per AZ-IV.W0.5.

Per generated tree (§B.6): 168,785 LOC committed under
`crates/core/src/grammar/generated/`. Each file has a sibling
`<g>.registry.json` cache.

The proc-macro shells (`crates/bbnf-path/`, `crates/bbnf-path-ts/`)
are different beasts (Lock 7 says "the bbnf-path / bbnf-path-ts
proc-macro shells, which are different"). They emit `TypedPath<G, T>`
literals at compile time — they don't emit per-grammar parse fns.

Lock 6 honoured.

---

## Lock 7 — `crates/path/` is the consolidated path crate; runtime cursor engine merges INTO it

**Verdict**: violated; today three places.

CENSUS §4.1 documents three path implementations:

| Crate / Module | LOC | Purpose |
|---|---:|---|
| `crates/bbnf-path/` (proc-macro) | 918 | `path!()` macro |
| `crates/bbnf-path-ts/` (cdylib) | 1012 | TS template-tag |
| `crates/core/src/path/` (typed-path IR + executor) | 2,234 | runtime types |
| `crates/core/src/runtime/path.rs` (legacy alphabet) | 163 | duplicate `PathSegment` / `Path` |

Lock 7 names `crates/path/` as the consolidated crate; today the
runtime path types live at `crates/core/src/path/` and `crates/core/src/runtime/path.rs`.
The runtime/path.rs duplicate is fault per CENSUS §4.1.

The Lock-7 letter says: "the existing `crates/core/src/path/`
directory empties." This is the synthesis pivot — fold path machinery
out of `crates/core/src/` into a consolidated `crates/path/` crate.

Today: violated. The runtime cursor engine (`runtime/path.rs` +
`path::cursor` + `path::executor` + `path::ascent` + `path::wildcard`)
sprawls across two locations.

---

## Lock 8 — Surpass sonic-rs / simdjson / lightning-css

**Verdict**: out of Pass-B file-level scope; aspirational gates live in tranche docs.

Pass-B substrate doesn't carry perf gates per se; the gates live in
`docs/tranches/BA/`, `BB/`, `BC/`. The substrate carries the
*mechanism* that meets the gates. Per RESTART-SKETCH §A.7, the 86.07%
samply share at `JsonStructBuilder::checkpoint` is the meta-mechanism
that prevents Lock 8 — the codegen pattern is generic over Alt shape
and doesn't specialise for byte-disjoint Alt; the checkpoint deep-clones.

Pass-B redress that Lock 8 demands:
- byte-disjoint Alt → no checkpoint
- direct-projection emit (no OpenFrame stack)
- O(1) checkpoint tuple, not O(N) clone

Lock 8 not Pass-B-file-judgable; the sub-issues (#3, #5, #7 in
RESTART-SKETCH §A.4) are Pass-B substrate issues.

---

## Lock 9 — Slice-borrow primary; bumpalo + owned escape hatches

**Verdict**: partial.

Evidence — `runtime/json/value.rs`:

```rust
pub enum JsonValue<'p> {
    Null, Bool(bool), Number(f64),
    String(&'p str),       // ← slice borrow primary
    Array(JsonArrayId), Object(JsonObjectId),
}
```

Slice-borrow honoured. `JsonValue<'p>` is a 16-byte tag-and-payload Copy
type per RESTART-SKETCH §B.2. Same shape across BBNF, CSS L4, Sheets.

Evidence — `runtime/json/builder.rs:135` — `stack: Vec::with_capacity(8)`
+ JsonArena slabs. Eager arena allocation in the default `JsonStructBuilder::new()`.

Lock 9 says: "Default API is `&'i str` slices + `Cow<'i, str>` for
transformations. Bumpalo arena is opt-in via `parse_in(input, &bump)`."
Today, `JsonStructBuilder::new()` allocates two `Vec`-slabs (arrays,
objects) in the JsonArena unconditionally — there is no slab-free
default. The arena IS the place where compound IDs land (via
`arena.intern_object`), but the eager arena alloc on the simplest
parse is fault per Lock 9.

The `parse_in(input, &bump)` opt-in surface does not exist on Pass-B
runtime (`runtime/json/parse_with.rs:77` is `parse_with(input, &TypedPath)`,
the lazy-parse surface — not the bumpalo opt-in).

Lock 9 violated structurally — the slice-borrow primary is honoured
in *value types* but the default `parse(input)` allocates arena slabs
unconditionally.

---

## Lock 10 — Pratt + SIMD auto-detected; no `@pratt` / `@simd` directives

**Verdict**: honoured for codegen path; pattern miner pipeline is Pass A.

Pass-B substrate per `backend/rust/emitter/shapes/pratt/` — pratt emit
is data-driven via grammar profile (`backend/rust/emitter/precedence.rs`).
No `@pratt` directive in grammar; the optimiser's `passes/patterns/`
mines operator chains.

simd-scan crate per §B.4.d uses `is_aarch64_feature_detected!` /
`is_x86_feature_detected!` runtime detection; no `@simd` directive.

Per Lock 10: "Cost model decides when SIMD overhead is worth the
dispatch cost." Today the cost model lives in egraph (`cost_weights.rs`
CALIBRATED_WEIGHTS); the Pass-B emit honours its decision.

Lock 10 honoured.

---

## Lock 11 — Path-deps for incubating sister crates; promote to registry once stable

**Verdict**: honoured per workspace `Cargo.toml`.

Workspace members include the sister crates as path-deps:
- `bbnf-ir = { path = "../crates/ir" }` (Pass A)
- `egraph` — workspace member with path-dep on `csp-solver`
- `csp-solver` — workspace member; isomorphic Rust+Python per `feedback_csp-solver-crate`
- `simd-scan` — workspace member
- `bbnf-path` + `bbnf-path-ts` — workspace member proc-macro + cdylib

Lock 11 says: "egraph + egraph-derive + csp-solver + bbnf-regex +
parse-that as path-deps in workspace until each API stabilises;
promote to registry once stable. simd-scan + bootstrap + analysis +
lsp stay workspace-internal."

Per `feedback_general-infra-crates` and `feedback_regex-crate-isomorphic`,
the bbnf-regex + parse-that crates live as sibling repos. Per `feedback_wasm-subcrate-pattern`,
WASM bindings live as sub-crates inside parent.

Lock 11 honoured.

---

## Lock 12 — ser + gorgeous archive BEFORE BA.W0

**Verdict**: out of Pass-B scope; archive ceremony is Pass C / commit chain.

Today `crates/ser/` and `crates/gorgeous/` are still in workspace
(per `Cargo.toml`'s `members = [..., "crates/gorgeous", ...]`). The
archive must happen before the new tranche set begins; that's the
synthesis-orchestrator's call.

Pass B doesn't write to `crates/ser` or `crates/gorgeous`; doesn't
audit them. They appear in the optimiser-crate adjacency only insofar
as `gorgeous/src/builtin.rs:9-22` carries a per-grammar match (CENSUS §2.5)
which is *Pass-C scope* (gorgeous-grammar metadata).

---

## Lock 13 — No god directories; cohesive encapsulation at every level

**Verdict**: violated multiple times in Pass B.

### Lock 13 — runtime/

`crates/core/src/runtime/` has 17 immediate children — 9 per-grammar
subdirs + 8 generic mechanism files. Lock 13 explicit text cites this
as the archetype god directory: "*A 16-sibling directory mixing
per-grammar subdirs with generic mechanism files (e.g., today's
`crates/core/src/runtime/`) is a god directory and is a fault.*"

Fault.

### Lock 13 — backend/rust/emitter/shapes/

Per `feedback_no-god-modules` (sibling-API divergence):

```
shapes/
├── arglist.rs (single-file)
├── array/ (mod.rs + element.rs)
├── alt_dispatch/ (mod.rs + branches.rs)
├── cursor_param.rs (single-file)
├── dispatcher/ (mod.rs + cross_shape.rs + ref_call.rs + support.rs + symbol_composition.rs)
├── flat/ (mod.rs + struct_direct.rs)
├── hregex.rs (single-file)
├── inline/ (mod.rs + structural_branch.rs)
├── keyword/ (mod.rs + payload.rs + struct_direct.rs)
├── number.rs (single-file)
├── object.rs (single-file)
├── pratt/ (mod.rs + struct_direct.rs)
├── scalar.rs (single-file)
├── string.rs (single-file)
├── substrate.rs (single-file)
├── unordered.rs (single-file)
└── wrap/ (mod.rs + struct_direct.rs)
```

Mixed pattern: 8 single-file shapes + 9 directory-form shapes; 4 of
the directory-forms carry `struct_direct.rs` (keyword, flat, pratt,
wrap), the rest don't. Sibling-API divergent per Lock 13.

Fault.

### Lock 13 — file LOC limit (>500 LOC outside generated/ forbidden)

Per CENSUS §5: 23 god modules >500 LOC; 11 of them in Pass-B scope
(per §B.1 idiomaticity §6).

Fault.

---

## Lock 14 — Full grammar generalisation; zero overfitting

**Verdict**: violated systematically.

Lock 14 specifies three verification commands; each fires a fault.

### Lock 14 verification command #1

```
$ rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' \
    crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,bbnf-regex,parse-that,simd-scan,analysis,lsp}/src/
```

Returns non-zero matches (CENSUS §2.2 + grep evidence above):

- `crates/ir/src/registry/strategy.rs:137-185` — `PRODUCTION_MANIFEST_TABLE`
  hardcoded with `JsonParser`, `JsonGrammar`, `GoogleSheetsParser`, `GoogleSheetsGrammar`,
  `CssL4Parser`, `BbnfBootstrap`, `BbnfParser`, `CsvParser`, `CsvGrammar`,
  `MathParser`, `MathGrammar`, `BnfParser`, `BnfGrammar`, `EbnfParser`,
  `EbnfGrammar`, `CssPrettyParser`, `CssPrettyGrammar` + `rust_builder_path:
  "crate::runtime::css_l4::CssStructBuilder"`
- `crates/ir/src/registry/strategy.rs:263` `.find(|entry| entry.matches_ident(grammar_ident))` — runtime resolution

(These are Pass A scope per directive; Pass B notes the resulting
violation in `bbnf-ir`.)

Pass-B side leaks:
- `crates/core/src/backend/rust/ir_types.rs:145` — `BbnfBootstrap` mention
  in doc-comment (KEEP - narrative)
- `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs:527`
  — "direct call to the matching `parse_<shape>_<grammar>_<rule>`" —
  KEEP narrative
- `crates/core/src/backend/rust/emitter/shapes/hregex.rs:24` — "decoded
  value through the grammar's" — KEEP narrative

These are narrative, not match-arms. Pass-B emitter does NOT carry
match arms over grammar idents.

### Lock 14 verification command #2

```
$ find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d
crates/core/src/runtime/bbnf
crates/core/src/runtime/bnf
crates/core/src/runtime/css_l4
crates/core/src/runtime/css_pretty
crates/core/src/runtime/csv
crates/core/src/runtime/ebnf
crates/core/src/runtime/google_sheets
crates/core/src/runtime/json
crates/core/src/runtime/math
```

9 per-grammar dirs. Lock 14 says: "*Per-grammar runtime modules (value,
document, view, kind) are emitted from a single grammar-agnostic
generator template that consumes (grammar source + workspace metadata)
and produces typed Rust; hand-written per-grammar runtime files are
forbidden.*"

Today: 5 trivial cohort grammars (BNF, CSV, EBNF, CSS Pretty, Math)
carry hand-written runtime files (~440 LOC each); 4 specialised
grammars (BBNF, JSON, CSS L4, Sheets) carry hand-written specialised
runtime files (~1500-3100 LOC each).

The trivial cohort *partially* honours Lock 14 via `runtime/builder_template.rs` +
`runtime/arena_template.rs` (the cohort's `builder.rs` is a 54-LOC
template instantiation per CENSUS §9.1). But `document.rs`, `view.rs`,
`kind.rs`, `value.rs`, `mod.rs` are NOT yet templated — they are
hand-written per-grammar.

The four specialised grammars are entirely hand-written.

Fault — 9 per-grammar dirs × ~7 hand-written files = 63 hand-written
per-grammar runtime files. CENSUS §10.2 calls hard-merge: emit per-grammar
`<g>Document`, `<g>View`, `<g>Kind`, `<g>Value`, `<g>::mod` from a
SINGLE codegen template at xtask-regen time.

### Lock 14 verification command #3

```
$ rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' crates/
```

Per the grep above, no source-side match-arms in Pass-B emitter or
runtime substrate. Match arms exist in:
- `crates/ir/src/registry/strategy.rs:262` — string ident comparison
  (data-driven) → not a match-on-ident-arm; honoured
- `crates/ir/src/passes/audit/payload_coverage.rs:69-90` — `enum
  GrammarAuditTag { Json, CssL4, Sheets, Bbnf, Custom(&'static str) }`
  with named variants per CENSUS §2.2 → fault (Pass A scope)
- `crates/ir/src/passes/recognizers/shape_dict_bbnf.rs` — entire file
  named after one grammar per CENSUS §2.2 → fault (Pass A scope)

Pass-B emit + runtime substrate does NOT carry match-on-grammar-ident
arms in source code. Lock-14 verification command #3 returns zero
matches in Pass-B scope.

### Lock 14 — Pass-B summary

The codegen substrate (Pass B) carries narrative-only Lock-14 mentions;
no source-side fault. The runtime substrate carries 9 per-grammar dirs
× 7 hand-written files = systemic fault.

Pass-B Lock-14 redress:
- Generate per-grammar runtime modules (value, document, view, kind,
  mod) from a single grammar-agnostic template
- Hand-written specialised content (CSS L4 14-variant OpenFrame, BBNF
  bounds-recording, Sheets canonical-form) folds into per-grammar
  declaration crates `crates/<grammar>/`, NOT into `crates/core/src/runtime/<g>/`

---

## Lock-adherence summary

| Lock | Verdict | Pass-B remediation |
|---|---|---|
| 1 — tape dead | honoured at production-symbol level; ~30 doc-comment residue sites; OpenFrame is "tape rebranded?" debatable | scrub residue; resolve OpenFrame question |
| 2 — layout lowering | naming honoured downstream; runtime literal reconstruction is fault | move StructLayout from runtime data to compile-time data |
| 3 — cursor-parse + byte-skip unified | honoured structurally; cursor consult on eager wasted | constant-fold cursor calls when EMPTY_PATH bound |
| 4 — per-domain orthogonal optimisation | honoured | none |
| 5 — IR + per-backend lower | partial — Rust bypass; TS+WASM honour | reshape Emitter trait to one walking pattern |
| 6 — xtask emits committed source | honoured | none |
| 7 — `crates/path/` consolidated | violated — 4 path locations | fold runtime/path.rs into path; consolidate per Lock 7 |
| 8 — surpass sonic-rs etc. | substrate-side: Open Frame + checkpoint clone is the blocker | direct-projection emit; O(1) checkpoint |
| 9 — slice-borrow primary | violated structurally — eager arena alloc | introduce `parse(input)` slab-free; arena via `parse_in` |
| 10 — Pratt+SIMD auto-detect | honoured | none |
| 11 — path-deps for incubating sister crates | honoured | API freeze for egraph + csp-solver per `feedback_csp-always-optimize` |
| 12 — ser + gorgeous archive | out of Pass-B; Pass C | none |
| 13 — no god directories | violated — `runtime/`, `shapes/` sub-API divergence, 11 god modules | split `runtime/`; uniformise `shapes/`; split god modules per CENSUS §5 |
| 14 — full grammar generalisation | violated — 9 per-grammar runtime dirs × ~7 hand-written files; bbnf-ir manifest mirror (Pass A) | emit per-grammar runtimes from grammar-agnostic template |

The two most consequential Pass-B lock violations are **Lock 13**
(runtime/ god directory) and **Lock 14** (per-grammar runtime
hand-written files). Both demand a single architectural transposition:
**per-grammar runtime emit from a template**, with the per-grammar
specialised content folding into per-grammar declaration crates. This
is Agent B.4's territory.
