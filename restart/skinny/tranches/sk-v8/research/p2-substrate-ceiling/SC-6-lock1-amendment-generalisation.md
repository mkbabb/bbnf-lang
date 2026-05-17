---
title: "SC-6 — Lock-1 Amendment + Cross-Grammar Generalisation"
tranche: sk-v8
phase: p2-substrate-ceiling
agent: SC-6
lens: substrate-ceiling
scope: "Lock-1 totality-spec consequences of the tape ⊕ structural-projection union substrate"
sibling: "SC-3 (union-substrate design)"
status: research
pass-omega-candidate: true
date: 2026-05-17
sources:
  - restart/locks/LOCKS.md
  - restart/ARCHITECTURE.md §1, §7.2, §7.3, §9
  - restart/skinny/tranches/sk-v8/SPEC.md
---

# SC-6 — Lock-1 Amendment + Cross-Grammar Generalisation

The substrate-ceiling hypothesis holds that bbnf's offset-tape substrate
caps throughput, and that the remedy is a union substrate wherein the SIMD
structural index *is* the tape rather than a layer feeding it. A union that
replaces the offset-tape is a substrate change, and a substrate change is a
Lock-1 matter. This document determines the totality-spec consequences,
writes the Lock-1 amendment proposal, and proves the union generalises
across grammars without violating Lock 14.

---

## §1 — Lock 1 + Lock 14 verbatim + the current substrate spec

### §1.1 — Lock 1 verbatim

From `restart/locks/LOCKS.md:34`:

> 1. **Tape is the substrate, properly unioned with direct-to-struct;
> columnar SoA is dead; orthogonal codepaths and parallel substrates are
> dead.** Tape is the greenfield's parsed event projection, unioned with
> direct-to-struct typed values that borrow into it (`&'i Tape<'i>` +
> cursor) when a retained document exists. The projection may be an offset
> tape, event tape, or collapsed-stage event sink; direct-only `SinkOnly`
> retains no queryable document identity. The 2,000-commit prior failure
> was implementation, not concept: orthogonal codepaths (the
> Vec<OpenFrame>::clone parallel substrate that produced the 86.07% samply
> pathology); type ambivalence (tape and OpenFrame and direct-to-struct
> competing for the same role); substrate-first/consumer-later (Era V
> failure mode); columnar SoA designed in AV.04 archaeology but never
> activated. The greenfield's tape lives at `runtime/src/tape/`;
> typed-value records borrow into it; per-grammar runtime modules
> (template-emitted at `runtime/src/grammars/<name>/`) emit accessors; one
> materialisation surface; one Visitor pattern; no parallel substrate. A
> SIMD mask stream is a transient producer, not a retained sidecar; if
> structural offsets are retained, the structural projection IS the tape.
> Columnar SoA stays buried. Plans that resurrect parallel substrates
> (OpenFrame ladders; columnar SoA; type-ambivalent dual representations)
> or implement tape with consumer-later sequencing are faults; plans that
> implement tape properly with same-wave consumer wiring + direct-to-struct
> union are honoured. **2026-05-04 reframe**: the prior restart's wholesale
> retirement of the tape name was an over-correction against the
> implementation failure; the user has confirmed tape is the right
> substrate when implemented properly. Lock 1's spirit (no parallel
> substrate; no orthogonal codepath; no Vec<OpenFrame>::clone pathology)
> holds; the no-rename clause is amended.

### §1.2 — Lock 14 verbatim

From `restart/locks/LOCKS.md:60`:

> 14. **Full grammar generalisation; zero overfitting**. The substrate
> carries ZERO grammar-specific code. Every grammar plugs into the fleet
> via three declarative surfaces only: (a) a grammar source file
> (`<name>.bbnf`), (b) workspace metadata declaring its strategy
> (recognisers, host fns, output-dir, pratt eligibility, simd eligibility,
> etc., per Lock 5's IR contract), and (c) optionally a per-grammar
> declaration crate (`crates/<grammar>/`) carrying host-fn implementations.
> Generic crates — `bbnf-parse`, `bbnf-codegen`, `bbnf-runtime`,
> `bbnf-ir`, `path`, `path-core`, `egraph`, `csp-solver`,
> `parse-that-regex`, `parse-that`, `bbnf-simd`, `analysis`, `lsp` — carry
> ZERO `match grammar { Json => ..., CssL4 => ..., ... }` arms; ZERO
> grammar-named modules; ZERO grammar-specific types in their public APIs;
> ZERO per-grammar feature flags. Per-grammar runtime modules (value,
> document, view, kind) are emitted from a single grammar-agnostic
> generator template that consumes (grammar source + workspace metadata)
> and produces typed Rust; hand-written per-grammar runtime files are
> forbidden. Per-grammar deviations (CSS L4 colour-function emit; BBNF
> Pratt operators; Sheets array literals) are encoded in the grammar
> metadata + source, NOT in branching code in any other crate. Adding a
> new grammar is a config + grammar-source change with NO code change in
> any generic or other-grammar crate. [...] Any plan, tranche, wave, or
> commit that introduces grammar-specific code in a generic crate, or any
> new hand-written per-grammar runtime file, is a fault regardless of its
> other merits.

### §1.3 — What the V1 spec currently says the substrate is

The V1 totality spec already names a *materialisation pipeline*, not a flat
offset-tape. The relevant clauses:

- **`ARCHITECTURE.md` §9 (Runtime Architecture, lines 1498-1503)**: "Tape
  and direct-to-struct are a single substrate family." Mutation flows
  through a single read-write visitor.

- **`ARCHITECTURE.md` §9.2, the runtime materialisation model (lines
  1561-1569)**:

  ```text
  byte input
    -> mask stream
    -> typed event cursor
    -> { OffsetTape | EventTape | SinkOnly | CollapsedStage }
    -> DocumentView / direct typed output
  ```

  The mask stream is "a transient producer ... not a retained substrate."
  The typed event cursor "is the shared read/write abstraction: it walks
  offsets, event cells, or collapsed state transitions."

- **`ARCHITECTURE.md` §7.2 (lines 1025-1031)**: `SimdScan` has two runtime
  products — "a transient **mask stream** feeds typed events during parse;
  an optional retained tape stores offsets, event cells, or direct payload
  facts after parse. ... The mask stream itself is never a second
  substrate; if retained, it is the tape projection."

- **`ARCHITECTURE.md` §7.3 (lines 1060-1088)**: `LayoutFacts.backend_shape`
  enumerates five `BackendShape` variants — `EagerTape`, `OffsetTape`,
  `EventTape`, `SinkOnly`, `CollapsedStage` — each "a way the substrate may
  project for a given rule." The cost model picks per-rule; no directive
  carries the choice.

The crucial observation: **the V1 spec already contemplates the structural
projection becoming the tape.** Lock 1's own sentence — "if structural
offsets are retained, the structural projection IS the tape" — and §7.2's
"if retained, it is the tape projection" both anticipate exactly the union
SC-3 is designing. The V1 spec does *not* yet say the structural index is
*generated by SIMD codegen as the substrate's primary write path*; it
describes the mask stream as transient and the tape as a downstream
*optional retained* artefact. The union substrate's claim is stronger: the
structural index is not transient — it is the retained substrate, and the
offset-tape as a distinct downstream object ceases to exist. That stronger
claim is what requires a Lock-1 refinement, not merely a reading of the
existing text.

---

## §2 — Does the union violate or satisfy Lock 1?

### §2.1 — The two readings, stated precisely

The union substrate replaces the offset-tape with a structural projection
that is simultaneously (a) the SIMD structural index and (b) the queryable
document substrate. SC-3 designs that object. Two readings of Lock 1 are
available, and they diverge on one structural question.

**Reading A — VIOLATION.** Lock 1 forbids "parallel substrates" and
"orthogonal codepaths." A SIMD structural index is a recognised, named,
distinct data product (the `{ } [ ] : , "` offset stream). If that index
is *retained* and the offset-tape is *also* retained, there are two
substrates. The 2,000-commit pathology was precisely a second
substrate — the `Vec<OpenFrame>::clone` ladder — running alongside the
tape. A retained structural index that coexists with a tape is the same
fault wearing a SIMD costume.

**Reading B — SATISFACTION (stronger).** Lock 1 forbids *parallel*
substrates — two objects, each carrying authoritative document identity,
each consulted independently, the codepaths orthogonal. The union does not
add a second object; it *collapses two roles into one object*. The
offset-tape today conflates two roles: it is (i) the structural skeleton
(where the `{ } [ ] : , "` are) and (ii) the typed-event projection (what
each structural byte means as a parsed event). The union substrate makes
the structural index carry both roles. After the union, there is exactly
*one* substrate. The offset-tape as a distinct object does not survive the
union — it is *replaced*, not *shadowed*.

### §2.2 — The deciding distinction

The deciding question is single and falsifiable:

> **After the union lands, is the offset-tape still constructed?**

- If the offset-tape is still constructed *and* the structural index is
  *also* retained — two retained objects, the parser writes both, consumers
  read whichever — that is a **parallel substrate**. Lock 1 violation.
  This is Reading A's failure mode, and it is a genuine fault: it is the
  `Vec<OpenFrame>` pathology re-skinned. Lock 1 must continue to forbid it.

- If the offset-tape *ceases to be constructed* — the structural index IS
  the substrate, the parser writes it once, every consumer (cursor,
  `ValueRef`, `path!`, visitor, debug trace) reads that one object — that
  is a **singular substrate**. The number of substrates went from one
  (offset-tape) to one (union). Lock 1 is not merely satisfied; it is
  satisfied *better*, because the union also resolves the residual "type
  ambivalence" Lock 1 names: the offset-tape's two-roles-in-one-object
  conflation is itself a mild ambivalence (structural skeleton vs. typed
  projection), and the union makes the projection *intrinsic* to the
  structural object rather than computed off it.

### §2.3 — The argument, concluded

**The union substrate, as SC-3 designs it (a structural projection that
*replaces* the offset-tape), SATISFIES Lock 1 and satisfies it better than
the present spec.** The reasoning chain:

1. Lock 1's spirit is "no parallel substrate; no orthogonal codepath; no
   `Vec<OpenFrame>::clone` pathology" (the 2026-05-04 reframe sentence
   states this explicitly). The spirit is *cardinality* — one substrate —
   and *non-orthogonality* — one codepath consults it.

2. The union does not raise substrate cardinality. It holds it at one. The
   offset-tape is *replaced*, term-for-term, by the union. `runtime/src/tape/`
   continues to be the one substrate location; what changes is the
   substrate's *internal representation*, not its count.

3. Lock 1 already grants the licence. The sentence "if structural offsets
   are retained, the structural projection IS the tape" is a direct
   statement that a retained structural projection is *admissible as the
   tape itself*. The union is the operational realisation of that licence.
   §7.2's "if retained, it is the tape projection" repeats it.

4. The one thing Lock 1 must continue to forbid — and which the union must
   not become — is a structural index retained *alongside* a still-built
   offset-tape. That is the parallel-substrate fault. The amendment must
   make this forbidden case explicit so the union cannot drift into it
   during implementation (the implementation-not-concept failure mode Lock
   1 itself warns of).

5. Therefore the union does not need Lock 1 *changed* in spirit. It needs
   Lock 1 *refined* in letter: the present text says the mask stream is
   "transient" and the tape is an "optional retained" downstream artefact.
   The union promotes the structural projection from transient-producer to
   *the retained substrate itself*. That promotion is consistent with Lock
   1's spirit but is not yet stated in Lock 1's letter. Hence: a REFINEMENT
   amendment, not an ADDITION and not a REMOVAL.

The verdict: **honoured-by-spirit, refinement-required-in-letter.** The
union is a singular substrate and is admitted; a sidecar structural index
is a parallel substrate and stays forbidden; the amendment draws that line.

---

## §3 — The Lock 1 amendment proposal

Schema: LOCKS-AMENDMENTS — each item is one of ADDITION (new clause),
REFINEMENT (sharpening of an extant clause in place), or REMOVAL (deletion
of a clause). Below: one REFINEMENT, marked as a **Pass Omega candidate**
for the totality track's astral-synthesis fold-back.

### Amendment SC-6-L1-R1 — REFINEMENT of Lock 1

**Target.** `restart/locks/LOCKS.md:34`, Lock 1, the sentence pair: "A
SIMD mask stream is a transient producer, not a retained sidecar; if
structural offsets are retained, the structural projection IS the tape."

**Type.** REFINEMENT (sharpening of an extant clause; no spirit change; no
new lock).

**Pass Omega status.** **CANDIDATE.** This amendment originates in the
skinny track (SK-V8 substrate-ceiling investigation). It must fold back
into the totality V1 spec via Pass Omega — the totality astral synthesis
pass — because Lock 1 governs the totality track, not the skinny subset.
Until Pass Omega ratifies it, this is a skinny-track proposal; it does not
bind the V1 spec.

**Refined text** (replaces the targeted sentence pair):

> A SIMD mask stream is a transient producer, not a retained sidecar. The
> *structural projection* — the retained index of a grammar's structural
> bytes — may be the substrate itself: when the cost model retains
> structural offsets, the structural projection IS the tape, and no
> distinct offset-tape object is additionally constructed. This is the
> **substrate union**: one object carries both the structural skeleton and
> the typed-event projection; it is the singular substrate, not a layer
> feeding a separate tape. A structural index retained *alongside* a
> separately-constructed tape — two retained objects, the parser writing
> both — is a **parallel substrate** and remains forbidden under this
> lock's spirit; it is the `Vec<OpenFrame>::clone` pathology re-expressed
> in SIMD form. The discriminant is cardinality: the union holds substrate
> count at one; a sidecar raises it to two. The structural projection is
> emitted per-grammar as a data table (the **StructuralAlphabet**, §4 of
> the SC-6 generalisation analysis), never as grammar-named code in a
> generic crate, so the union honours Lock 14.

**Rationale.** The present letter calls the mask stream "transient" and
the tape an "optional retained" downstream artefact (§7.2:1025-1031). The
union promotes the structural projection from transient-producer to
retained-substrate. That promotion is consistent with Lock 1's spirit
("if structural offsets are retained, the structural projection IS the
tape") but is not stated in its letter. The refinement (a) states the
promotion, (b) names the union, (c) makes the forbidden sidecar case
explicit so the union cannot drift into a parallel substrate during
implementation, and (d) cross-references the StructuralAlphabet so Lock 14
adherence is visible at the Lock-1 site.

**Co-routed surface amendments** (not Lock-text; the totality spec
surfaces that cite the substrate model and must be reconciled when Pass
Omega ratifies SC-6-L1-R1):

- `ARCHITECTURE.md` §9.2 materialisation model (lines 1561-1569): the
  pipeline `byte input -> mask stream -> typed event cursor -> { ... }`
  gains a `UnionTape` node, or `OffsetTape` is re-specified so its
  retained form *is* the structural index rather than a copy taken off it.
  Co-routed; ARCHITECTURE owns the surgery, the Lock text owns the spirit.

- `ARCHITECTURE.md` §7.3 `BackendShape` enum (lines 1062-1088): no new
  variant. The union is the *representation* of `OffsetTape` (and the
  retained form of `EventTape`), not a sixth shape. The `derive_backend_shape`
  algorithm (§7.3:1090-1098) is unchanged; only the lowering of `OffsetTape`
  changes — it lowers to the union representation. Co-routed; verify-only
  delta if the algorithm already admits it.

- `ARCHITECTURE.md` §7.2 `SimdScan` runtime-products paragraph (lines
  1025-1031): the sentence "an optional retained tape stores offsets,
  event cells, or direct payload facts after parse" is refined to state
  the retained form IS the structural index, not a copy. Co-routed.

**Non-amendments** (stated to bound scope): Lock 1's columnar-SoA-is-dead
clause is untouched (the union is row-shaped, not columnar). Lock 1's
no-rename clause (already amended 2026-05-04) is untouched. The
direct-to-struct union clause is untouched (`SinkOnly` retains no document
and is orthogonal to the structural-projection question).

---

## §4 — Cross-grammar generalisation: the StructuralAlphabet

The union substrate's structural projection is, for JSON, an index of the
seven structural bytes `{ } [ ] : , "`. That seven-byte set is
*JSON-specific data*. The generalisation requires that the *concept* —
"the set of bytes whose positions form the structural skeleton" — be a
grammar-neutral abstraction, and the *instance* — which bytes, for this
grammar — be data emitted by codegen. The abstraction is the
**StructuralAlphabet**.

### §4.1 — The abstraction

A **StructuralAlphabet** is a per-grammar data table, emitted by codegen
into the per-grammar runtime module (`runtime/src/grammars/<name>/`), that
declares:

- the **structural byte set** — the bytes whose positions the SIMD
  structural scan retains as the union substrate's skeleton;
- per structural byte, its **event class** — open / close / separator /
  key-value-delimiter / string-delimiter / atom-boundary — so the typed
  event cursor can interpret a retained offset without re-reading grammar;
- the **escape/quote discipline** — which byte (if any) is the string
  delimiter and which byte (if any) is the escape, so the structural scan
  can mask string interiors (the simdjson prefix-XOR step generalises:
  the escape byte and quote byte are parameters, not constants);
- the **pad/clamp policy** — the `EOB_PAD_CLAMP` discipline (Lock 16) the
  scan kernel applies at end-of-buffer.

The StructuralAlphabet is *derived from Grammar IR* by a recogniser pass
(`passes::recognizers`), the same pass that today derives `RecognizerFacts`
and `backend_shape`. Its derivation: collect every `ByteLiteral` and
single-byte charclass that appears at a structural position in the grammar
(open of a group, close of a group, separator in a repetition, delimiter
in a pair) and that the first-set analysis proves byte-disjoint at its use
site. The result is a `StructuralAlphabet` record in `RecognizerFacts`,
consumed by the codegen template and emitted as a `static` data table.

The generic substrate crates (`bbnf-runtime`, `bbnf-simd`) consume a
`&StructuralAlphabet` at runtime; they never name a grammar. The SIMD
structural-scan kernel takes the alphabet's byte set as an argument to
`BYTE_CLASS_FROM_EQ_SET_64` (Lock 16's grammar-neutral primitive — the set
is a 64-byte argument, not a hardcoded constant). This is exactly Lock
16's existing posture for `BYTE_CLASS_FROM_EQ_SET_64`: the equality set is
data passed in, not code branched on.

### §4.2 — JSON instance

```text
StructuralAlphabet "json":
  structural bytes : { 0x7B '{', 0x7D '}', 0x5B '[', 0x5D ']',
                       0x3A ':', 0x2C ',', 0x22 '"' }
  event classes    : '{' -> Open(Object)    '}' -> Close(Object)
                     '[' -> Open(Array)     ']' -> Close(Array)
                     ':' -> KeyValueDelim   ',' -> Separator
                     '"' -> StringDelim
  string delimiter : 0x22 '"'
  escape byte      : 0x5C '\'
  pad/clamp        : EOB_PAD_CLAMP, 64-byte tail pad
```

This is the present skinny `{ } [ ] : , "` index, expressed as data.

### §4.3 — CSS L4 instance

CSS L4's structural skeleton is richer: it has block braces, the
declaration terminator, the selector/declaration separator, the function
parentheses, the attribute-selector brackets, and two string delimiters.

```text
StructuralAlphabet "css_l4":
  structural bytes : { 0x7B '{', 0x7D '}', 0x3B ';', 0x3A ':',
                       0x28 '(', 0x29 ')', 0x5B '[', 0x5D ']',
                       0x2C ',', 0x22 '"', 0x27 '\'' }
  event classes    : '{' -> Open(Block)      '}' -> Close(Block)
                     '(' -> Open(Function)   ')' -> Close(Function)
                     '[' -> Open(AttrSel)    ']' -> Close(AttrSel)
                     ';' -> DeclTerminator   ':' -> PropValueDelim
                     ',' -> Separator
                     '"' -> StringDelim      '\'' -> StringDelim
  string delimiters: { 0x22 '"', 0x27 '\'' }   (two delimiters; the scan
                     masks interiors of whichever opened first)
  escape byte      : 0x5C '\'
  pad/clamp        : EOB_PAD_CLAMP, 64-byte tail pad
```

Two consequences of CSS L4 that the abstraction must (and does) absorb: the
*event class* set is grammar-specific (CSS has `DeclTerminator`, JSON does
not), and there may be *more than one* string delimiter. The
StructuralAlphabet's event-class field is an open enum of class *kinds*,
not a fixed JSON-shaped set; the string-delimiter field is a *set*, not a
single byte. Both are data. The CSS L4 colour-function emit, the comment
`/* */` handling, and the at-rule prelude are *not* structural-alphabet
matters — they are grammar-source + metadata per Lock 14(b); the
StructuralAlphabet carries only the byte-skeleton, not semantic deviations.

### §4.4 — Sheets instance

Google Sheets formulas have a different skeleton again: parentheses for
function calls, the argument separator, the range colon, cell-reference
syntax, the string delimiter, and the array-literal braces.

```text
StructuralAlphabet "google_sheets":
  structural bytes : { 0x28 '(', 0x29 ')', 0x2C ',', 0x3A ':',
                       0x7B '{', 0x7D '}', 0x22 '"' }
  event classes    : '(' -> Open(FnCall)    ')' -> Close(FnCall)
                     '{' -> Open(ArrayLit)  '}' -> Close(ArrayLit)
                     ',' -> Separator       ':' -> RangeDelim
                     '"' -> StringDelim
  string delimiter : 0x22 '"'
  escape byte      : 0x22 '"'   (Sheets escapes a quote by doubling it;
                     the escape byte equals the quote byte — the union
                     scan's escape parameter handles the doubled-quote
                     idiom, see note)
  pad/clamp        : EOB_PAD_CLAMP, 64-byte tail pad
```

Sheets exercises the abstraction's outer edge: its escape *is* the quote
byte (a doubled `""` denotes a literal quote). The StructuralAlphabet's
escape field is a byte parameter; when it equals the quote byte, the scan
kernel applies the doubled-delimiter rule rather than the
backslash-escape rule. *Which* rule is itself a small enum field on the
alphabet (`EscapeKind ∈ { Backslash, DoubledDelimiter, None }`) — still
data, still grammar-neutral, still emitted by codegen. The Sheets `:`
range delimiter and `{}` array literals are skeleton bytes; the
A1-notation of cell references is *not* a skeleton byte and is left to the
grammar-source scanner.

### §4.5 — Arbitrary user grammar

An arbitrary user grammar gets its StructuralAlphabet by the *same
derivation path* as JSON, CSS L4, and Sheets — there is no special case.
The recogniser pass walks the user's Grammar IR, collects every
`ByteLiteral` / single-byte charclass at a structural position whose
first-set is provably byte-disjoint at use, classifies each into an event
class kind from the grammar's group/repetition/pair structure, identifies
string delimiters from rules whose body is a delimited span, and emits the
record. If the grammar has *no* byte-disjoint structural skeleton — a
grammar with no literal bracketing, e.g. a whitespace-significant or
purely-regex grammar — the StructuralAlphabet is *empty*, and
`derive_backend_shape` (§7.3:1090-1098, step 4: first-set overlap ⇒
`EagerTape`) routes that grammar to `EagerTape`, which does not use the
union structural scan at all. The union substrate is *applicable when the
grammar admits it and inert when it does not* — and that applicability is
itself cost-model-derived data, not a code branch.

### §4.6 — Lock 14 confirmation

Lock 14 holds. The verification, in Lock 14's own idiom:

- **Zero grammar-named code in the generic substrate.** `bbnf-runtime`,
  `bbnf-simd`, `bbnf-codegen`, `bbnf-ir` carry no `match grammar { Json
  => '{', ... }`. The structural byte sets live in per-grammar `static`
  data tables emitted into `runtime/src/grammars/<name>/`, and the generic
  crates consume `&StructuralAlphabet` polymorphically.
- **Three declarative surfaces only.** The StructuralAlphabet is *derived*
  from surface (a) the grammar source `.bbnf` file, with surface (b)
  workspace metadata supplying simd-eligibility. No fourth surface; no
  hand-written per-grammar structural table.
- **Adding a grammar is config + grammar-source.** A new grammar's
  StructuralAlphabet is emitted by re-running codegen; no generic-crate
  edit, no other-grammar-crate edit.
- **Verification command** (in Lock 14's style): `rg -n
  "'{'|'}'|'\['|StructuralAlphabet\s*\{" crates/{runtime,simd,codegen,ir}/src/`
  returns ZERO literal structural-byte constants and ZERO hand-built
  alphabet records — every alphabet is generated; the generic crates only
  reference the *type* `StructuralAlphabet`, never construct an instance.

---

## §5 — Primitive-layer generalisation: host ASM/SIMD capability dispatch

"Scale up the arithmetic": the union substrate's structural scan must run
on host-specific kernels — arm64 NEON `vqtbl4q_u8`, x86-64 AVX-512 GFNI
`vgf2p8affineqb`, AVX-IFMA `vpmadd52luq`, the AVX-512 `vpdpbusd` UDOT-class
dot-product, VPCLMULQDQ prefix-XOR — and those kernels are *host-specific
ASM/SIMD*, not portable code. The generalisation requires that a
grammar-neutral substrate consume host-specific kernels *without ever
branching on a grammar* and *without ever branching on a host inside the
substrate logic*. The abstraction is a **capability-dispatched primitive
layer**.

### §5.1 — The two-layer vocabulary (extant)

The architecture already factors this, per `ARCHITECTURE.md`
§7.3:1110 — the dav1d / asmjson two-layer pattern:

- **Layer 0** — host-ISA macro corpus, vendored verbatim
  (`crates/bbnf-simd/ext/x86/x86inc.asm`, dav1d, BSD-2). Pure ISA
  scaffolding; no bbnf concept.
- **Layer 1** — grammar-neutral primitive macros
  (`crates/bbnf-simd/ext/x86/bbnf.asm`). The vocabulary named in
  §7.3:1110: `BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`,
  `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `BULK_EMIT_COMPRESSED`,
  `EOB_PAD_CLAMP`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`.

Every Layer-1 primitive has a *grammar-neutral signature*. None names
`{` or `,`; `BYTE_CLASS_FROM_EQ_SET_64` takes the equality set as a 64-byte
argument. That is the StructuralAlphabet's byte set, passed in. The union
substrate's structural scan *is* a composition of Layer-1 primitives:
`EOB_PAD_CLAMP` → `BYTE_CLASS_FROM_EQ_SET_64` (alphabet bytes) →
`BITMAP_PREFIX_XOR_64` (escape/quote masking) → `BITMAP_NEXT_SET_BIT`
(offset extraction) → `BULK_EMIT_COMPRESSED` (write the retained index).
That composition contains zero grammar names and zero host names.

### §5.2 — The capability-dispatch abstraction

Each Layer-1 primitive is *one named operation* with *N host
implementations* and *one scalar reference*. The selection among the N+1
is a **capability dispatch** keyed on a runtime CPUID feature mask, made
*once* at substrate entry, never inside the structural-scan inner loop and
never on a grammar key.

The abstraction has three parts, all extant or near-extant in the spec:

1. **`PrimitiveFacts` side table** (`ARCHITECTURE.md` §7.3:1055): already
   "stores scalar oracle, target feature mask, ABI/checkasm status,
   same-wave consumer, and corpus-row impact." This is the registry of
   which host implementations exist for each primitive and what they
   require. Generalisation: every union-substrate Layer-1 primitive has a
   `PrimitiveFacts` row; the structural scan is admitted only when its
   composition's primitives all have a row.

2. **CPUID dispatch** (`ARCHITECTURE.md` §7.3:1055, "CPUID dispatch" named
   as a `PrimitiveFacts` consumer): at substrate construction, a single
   feature-mask read selects the implementation tier. The tiers form a
   *fallback chain*, not a grammar tree: AVX-512 GFNI → AVX-2 PSHUFB →
   NEON `vqtbl4q_u8` → SWAR scalar. The chain is keyed on host capability
   only.

3. **The scalar oracle floor**: every primitive has a SWAR scalar
   reference (Lock 16's "portable scalar" allowlist row). The substrate is
   *correct* on any host because the bottom of every fallback chain is the
   scalar oracle. Host-specific kernels are *accelerations of a
   grammar-neutral, host-neutral correctness floor*, never the floor
   itself.

### §5.3 — Why no grammar branch and no host branch leaks into the substrate

The substrate logic — the union structural scan — calls
`BYTE_CLASS_FROM_EQ_SET_64(alphabet.bytes, chunk)`. That call site is:

- **grammar-neutral**: `alphabet.bytes` is a `&[u8; 64]` argument; the
  substrate does not know it is JSON's `{ } [ ] : , "` or CSS's eleven
  bytes. Lock 14 holds.
- **host-neutral**: `BYTE_CLASS_FROM_EQ_SET_64` is a *name*; the CPUID
  dispatch behind it (resolved once, at substrate entry, into a function
  pointer or a monomorphised generic) chose GFNI or NEON or SWAR. The
  substrate does not branch on host; it calls the name. Lock 16 holds —
  the primitive is on the allowlist, carries a scalar oracle, carries a
  checkasm parity test.

"Scaling up the arithmetic" — the UDOT (`vpdpbusd` / NEON `udot`)
digit-block multiply-accumulate, the AVX-IFMA `vpmadd52luq` mantissa
multiply, the VPCLMULQDQ prefix-XOR — generalises identically. Each is a
Layer-1 primitive (`BITMAP_PREFIX_XOR_64` is the CLMUL one; a
`DIGIT_BLOCK_ACCUMULATE` primitive would be the UDOT/IFMA one) with N host
implementations and a scalar oracle, selected by capability dispatch. The
union substrate consumes them by *grammar-neutral primitive name*; the
per-grammar selection of *which* primitives a grammar's scan composes is
cost-model-derived from Grammar IR facts (alphabet size, string-token
presence, number-token presence) per Lock 16's closing paragraph — derived
data, not a code branch. The arithmetic scales up by *adding primitive
rows to the Layer-1 vocabulary and `PrimitiveFacts`*, never by adding
grammar arms or host arms to the substrate.

---

## §6 — Risks

**R1 — Sidecar drift during implementation.** Lock 1's own warning is
"implementation, not concept." The union *concept* is a singular
substrate; an implementation that builds the structural index *and then
also* materialises an offset-tape from it (for, say, an interim consumer
that was written against the old offset-tape API) re-creates the parallel
substrate. Mitigation: the amendment makes the sidecar case explicitly
forbidden; the gate is a `rg`/code-review check that the offset-tape
constructor is *deleted*, not merely *unused*. Same-wave consumer wiring
(Lock 1's own discipline) applies — the union lands only when the cursor,
`ValueRef`, and `path!` consumers are migrated in the same wave.

**R2 — `BackendShape` confusion.** A reader may expect the union to be a
sixth `BackendShape` variant. It is not — it is the *representation* of
`OffsetTape` and the retained form of `EventTape`. If the amendment is
read as introducing a `UnionTape` shape, the five-variant enum drifts to
six and `derive_backend_shape` grows a branch. Mitigation: §3's
co-routed-amendment note states explicitly "no new variant."

**R3 — Empty-alphabet grammars.** A whitespace-significant or purely-regex
user grammar has an empty StructuralAlphabet. If the union substrate is
assumed universal, such a grammar has no substrate. Mitigation: §4.5 — an
empty alphabet routes to `EagerTape` via `derive_backend_shape` step 4;
the union is applicable-when-admitted, inert-otherwise, and that
applicability is cost-model data. This must be a tested path, not an
assumed one.

**R4 — Event-class enum sprawl.** The StructuralAlphabet's event-class
field is an open enum (`Open`, `Close`, `Separator`, `DeclTerminator`,
`RangeDelim`, ...). If every grammar contributes its own class kinds, the
enum becomes a grammar-named-by-proxy surface — a Lock 14 erosion.
Mitigation: the event-class enum must be a *fixed, small, grammar-neutral*
set of *structural roles* (`Open`, `Close`, `Separator`,
`PairDelimiter`, `StringDelimiter`, `Terminator`, `AtomBoundary`) — CSS's
`DeclTerminator` maps to `Terminator`, Sheets' `RangeDelim` maps to
`PairDelimiter`. The grammar-specific *meaning* stays in grammar-source;
the alphabet carries only the neutral role. §4.3/§4.4's instance tables
should be re-expressed against the fixed role set before Pass Omega folds
this.

**R5 — Pass Omega timing.** This amendment is a skinny-track proposal; it
does not bind V1 until Pass Omega ratifies. If skinny implements the union
before Pass Omega runs, the skinny substrate and the V1 spec diverge.
Mitigation: the amendment is marked Pass Omega candidate (§3); SK-V8
SPEC.md:475 already requires residuals to name Pass Omega destinations;
the union-substrate implementation in skinny must carry an explicit
"pending Pass Omega ratification of SC-6-L1-R1" note so the divergence is
tracked, not silent.

**R6 — Capability-dispatch granularity.** If the CPUID dispatch is made
per-chunk or per-rule rather than once at substrate entry (§5.2), the
host-branch leaks into the inner loop and the substrate pays dispatch tax
on the hot path — the exact overhead the union is meant to remove.
Mitigation: the dispatch is resolved once, into a function pointer or a
monomorphised generic, at substrate construction; `PrimitiveFacts` and the
checkasm gate verify the inner loop contains no feature test.

---

## §7 — Sources

- `restart/locks/LOCKS.md:34` — Lock 1 verbatim (substrate union).
- `restart/locks/LOCKS.md:60` — Lock 14 verbatim (grammar generalisation).
- `restart/locks/LOCKS.md:69-94` — Lock 16 (SIMD/ASM admissibility
  allowlist; Layer-0/Layer-1 vocabulary; `BYTE_CLASS_FROM_EQ_SET_64`,
  GFNI, UDOT, VPCLMULQDQ, AVX-IFMA citations).
- `restart/ARCHITECTURE.md` §9, §9.1, §9.2 (lines 1498-1611) — runtime
  architecture; tape; direct-to-struct union; the
  `byte input -> mask stream -> typed event cursor -> { ... }`
  materialisation model.
- `restart/ARCHITECTURE.md` §7.2 (lines 911-1031) — Backend IR;
  `SimdScan` two-runtime-products paragraph ("if retained, it is the tape
  projection").
- `restart/ARCHITECTURE.md` §7.3 (lines 1033-1110) — side tables;
  `LayoutFacts.backend_shape`; the five-variant `BackendShape` enum;
  `derive_backend_shape` algorithm; `PrimitiveFacts`; the two-layer
  reusable vocabulary and per-shape lowering.
- `restart/ARCHITECTURE.md` §1 (lines 49-87) — workspace shape (substrate
  crate location `runtime/src/tape/`).
- `restart/prompts/sub-orchestrators/AMENDMENT-DISPATCH.md` — the
  amendment-dispatch discipline (ADDITION / REFINEMENT / verify-then-patch;
  Lock-amendment routing).
- `restart/skinny/tranches/sk-v8/SPEC.md:72, :475` — `BackendShape` enum
  amendment discipline; residuals naming Pass Omega destinations.
