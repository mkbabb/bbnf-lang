# PROTO-2 — Construction census: grammar-expressible vs host-bound GrammarIr build steps

LOCK-7 pass-1, probe 2 (family **7C HOST-FUNCTION LATTICE** ↔ 7B Plane A). Adjudicates **INC-2**,
sets bbnf's totality floor, bounds 7B Plane A. **RAN_MEASURED**: analytical census over live source
at HEAD (2026-07-16), anchored by a 3-assertion probe compiled and executed against the `grammar`
crate (exit 0, 3/3 passed). Loadavg **20.34** recorded during the run. Live source was **not**
mutated — probe written to `crates/grammar/tests/`, run, then removed; text preserved as
`PROTO-2.probe.rs` + `PROTO-2.patch`.

> **Supersession note.** This re-adjudicates the 05:01 probe-2 census (whose text is folded in below
> as *Reading R0*). R0 concluded `construction floor = 0`. This dispatch carries an **explicit
> invariant** — *"any step that interns/edge-builds with mutable arena state MUST classify HOST …
> if the census marks such a step GRAMMAR without a grammar operator that expresses the interning,
> the census is wrong."* Applying that invariant honestly to the real `add_expr` **overturns R0 on
> step 4**: the arena builder is host-bound in current bbnf. Nothing from R0 is lost — its
> catamorphism argument survives as the reason step 4 is a **SOFT** (dischargeable) host, not a hard
> one. The 04:05 `PROTO-2-7D-regex-oracle.{md,patch}` (a different family-7D probe) are untouched.

---

## Question (INC-2, verbatim)

> RECOGNIZE != BUILD: bbnf.bbnf *recognizes*, but the emitted parser must *BUILD* GrammarIr. For each
> construction step, is it `->`-expressible (7B: 7C-floor = 0, pure grammar) or recursive-with-state
> and typed-host-fn-bound (7C: floor > 0)? `count(HOST)` is 7C's bbnf floor AND the INC-2 verdict.

## Method (analytical census; the probe only anchors two empirical facts)

1. Read the **FnDescriptor six-variant lattice** (`crates/ir/src/types/fn_descriptor.rs:20-57`):
   `EnumWrap · BoxWrap · NumberConvert · HexConvert · SpanCapture · Expr`. Module doc (lines 1-6,
   17-19): the first five are **compiler-produced during lowering/specialization**; `Expr` is the
   **only user-facing variant** — *"all `->` map syntax produces it."*
2. Read every GrammarIr build primitive: the arena allocator (`skinny/crates/ir/src/lib.rs`
   `add_expr:51`, `add_rule:57`), the linker (`resolve_refs:84`), and all 14 construction sites in
   the hand parser (`skinny/crates/grammar/src/lib.rs:1332-1531`).
3. Classify each of the **8 briefed steps** GRAMMAR / HOST with a `path:line` justification against
   the lattice **and** the &mut-Ctx/arena-state test.
4. **Run** a probe discriminating *append-vs-intern* (step 4) and *construct-vs-link* (step 5).

## What ran (measured)

`CARGO_TARGET_DIR=<scratch> cargo test -p grammar --test proto2_lock7_census` — cold, scratch target
(no lock contention). **`Finished in 0.35s`; 3 passed / 0 failed.** Loadavg 20.34 (1-min) at run.

- `add_expr_is_append_only_not_interned` **ok** — `a = "x" , "x" ;` yields **two distinct**
  `Literal{b"x"}` ExprIds → **no structural interner; the built IR is a TREE**.
- `arena_ids_are_post_order_synthesized` **ok** — every child ExprId `<` its parent Seq ExprId →
  **ids are a post-order numbering = a synthesized attribute**.
- `cross_rule_link_is_a_separate_global_pass` **ok** — forward+cyclic ref (`a=b; b=a|"x";`) resolves
  only via the whole-rule table; `a = undefined_rule ;` is a **link** error, not a construct error.

---

## The 8-step census

**Legend** — `G` grammar-`->`-expressible (stateless synthesized projection) · `Hs` **soft** host
(threads state but fold-reducible; dischargeable by a value-language primitive) · `Hh` **hard** host
(genuine recursive-with-global-state; irreducible in a synthesized `->`) · `IO` CompileIO ·
`—` no live step. State column: does the step need cursor / `&mut Ctx` / arena beyond recognition?

| # | step | lattice variant | realizing path:line | state? | class |
|---|---|---|---|---|---|
| 1 | leaf span capture `-> Span` | `SpanCapture` (fn_descriptor.rs:44) | grammar `-> Span` bbnf.bbnf:9,13,15,17,18 → `Project{target:"Span"}` lib.rs:1485 | **none** (span = (start,end) of the match) | **G** |
| 2 | numeric decode `-> i64/f64` | `NumberConvert{allow_leading_dot}` (fn_descriptor.rs:35) | grammar `-> i64`/`-> f64` expressions.bbnf:6-7 → `Project{ty}` lib.rs:1485 | **none** (pure fn of matched text) | **G** |
| 3 | enum / box wrap | `EnumWrap{variant}` :22 / `BoxWrap` :26 | compiler lowering (fn_descriptor.rs:1-6,17); semantics = `Variant(input)` / `Box::new(input)` = value_fn_call expressions.bbnf:14 | **none** (pure wrap of one child value) | **G** |
| 4 | **arena interning of ExprId** | **none** (not a Map descriptor) | `add_expr` ir:51-55 `id=ExprId(exprs.len()); exprs.push` — threaded `&mut GrammarIr` at every site (lib.rs:1351,1371,1393,1400,1423,1439,1456,1463,1485,1501,1511,1524) | **`&mut GrammarIr`** (arena) + edge-build (children stored as ExprIds) | **Hs** |
| 5 | **cross-rule forward/cyclic ref resolution** | **none** (whole-grammar post-pass) | `resolve_refs` ir:84-108 — builds `HashMap<name,RuleId>` over the complete rule set, mutates every `Ref.target` None→Some; called lib.rs:33. Constructor defers the edge: `Ref{target:None}` lib.rs:1525 | **global symbol table** + `&mut` (forward+cyclic; needs whole grammar) | **Hh** |
| 6 | closure β-reduction `\|params\| rhs` | none (unimplemented) | bbnf.bbnf:51-52 **recognizes** `closure`; skinny builder has **no** lower path (`\|` is only an alt separator, parse_alt lib.rs:1341; `@host` rejected 1324-1328). **No live β-reduction site.** | — | **—** (0 live) |
| 7 | `@import` module resolution | none (source-set I/O) | `resolve_runtime_import_closure` lib.rs:963 (adjacency + cycle detect over multi-file source set); driven from grammar_provider.rs:101 `parse_runtime_source_facts`. Resolves the **runtime span-tape frontend**, not the GrammarIr node-builder | **file I/O + global source closure** | **IO** |
| 8 | `@{}` capture | `SpanCapture` (fn_descriptor.rs:44, *intended* home) | bbnf.bbnf:33 `"@{",rhs,"}"` recognized; runtime harvest records `RuntimeHostCapture` lib.rs:170,422-431; IR-path returns **`BBNF-UNSUPPORTED-HOST-CAPTURE`** lib.rs:222. **Dormant** — no `@{` arm in parse_atom (1498-1531) | none *if activated* (raw-span, stateless) | **—** (0 live; SpanCapture=G on activation) |

### Why steps 1–3 are G (and not the "compiler-produced" fear)

The lattice module-doc calls `SpanCapture/NumberConvert/EnumWrap/BoxWrap` *"produced by the compiler
during lowering"* — but that is **provenance, not host-binding**. Each is a **stateless total
function of a single already-matched value**: `SpanCapture` returns the match's `(start,end)`;
`NumberConvert` parses matched digits; `EnumWrap/BoxWrap` wrap one child. None touches the cursor,
`&mut Ctx`, or the arena for its *semantics*. Each is either a first-class `->` operator
(`-> Span`, `-> i64` at bbnf.bbnf:9 / expressions.bbnf:6) or a compiler **specialization** of the
user-facing `Expr` projection (fn_descriptor.rs:17-19). They lower to `Project` nodes that then ride
the arena (step 4), but the *projection value itself* is pure `->`. **Floor 0.**

### Why step 4 is Hs — the invariant bites, but softly (this is the R0 correction)

`add_expr` **edge-builds with mutable arena state**: it threads `&mut GrammarIr` and stores children
as `ExprId` indices (`Alt.branches`, `Seq(children)`, `Discard{left,right}`, `Repeat{body}`,
`Optional`, `Project{body}`). bbnf exposes **no operator that mints an ExprId or appends to the
arena** (expressions.bbnf's value language has `+`, calls, paths — no list/arena primitive). By the
dispatch invariant this step **cannot** be marked GRAMMAR without exhibiting such an operator, and
none exists → **HOST**. This is precisely where **R0 erred** (it marked the arena `G` on a
catamorphism argument while exhibiting no bbnf operator).

But the probe shows the host-binding is **soft**: the arena is **append-only, un-interned** (PROOF A
— identical leaves → distinct ids) and **post-order** (PROOF A'). So the built structure is a **tree**,
and post-order flattening of a tree **is** a catamorphism. R0's mathematics is correct; what it
missed is that *current bbnf's value language cannot express that fold*. Step 4 is therefore
**dischargeable by a language extension** (an arena/list-append value primitive), **not** by a
per-grammar recursive host fn — distinguishing it sharply from step 5.

### Why step 5 is Hh — irreducible

`resolve_refs` needs the **complete rule set** (forward + cyclic — PROOF B) and a **global**
`HashMap<name,RuleId>`. bbnf's `->` is **synthesized-only** (a value from a node's children +
`input`); it has **no inherited / global-environment attribute**, so name→RuleId is beyond `->` **in
principle**, not merely beyond today's value language. The one genuine graph edge (`Ref→RuleId`,
forward-referencing, not a child edge) is what makes the IR more than a tree — and the constructor
**already separates it** (`Ref{target:None}` at build, `resolve_refs` as a distinct pass). **TreeFold.**

---

## The numbers

```
step 1  -> Span            SpanCapture     G    floor 0   (stateless projection)
step 2  -> i64/f64         NumberConvert   G    floor 0   (stateless leaf convert)
step 3  enum/box wrap      EnumWrap/BoxWrap G    floor 0   (stateless value wrap)
step 4  arena ExprId       (no variant)    Hs   floor +1  SOFT  — add_expr, &mut arena, fold-reducible
step 5  cross-rule ref     (no variant)    Hh   floor +1  HARD  — resolve_refs, global env, TreeFold
step 6  closure β-reduce   (unimplemented) —    floor 0   ABSENT — recognized, never lowered
step 7  @import            (source I/O)    IO   +1 (IO)   CompileIO — not the node-builder
step 8  @{} capture        SpanCapture     —    floor 0   DORMANT — BBNF-UNSUPPORTED; =G on activation

count(HOST)  in-constructor mandatory  = 2   { add_expr (Hs), resolve_refs (Hh) }
             of which SOFT (extension-dischargeable) = 1   (add_expr)
             of which HARD (typed-host-fn-bound)     = 1   (resolve_refs)
CompileIO  (out-of-constructor)         = 1   ( @import )
β-reduction                             = 0   (absent in skinny)
@{} live host binding                   = 0   (dormant; SpanCapture-class)
steps classified                        = 8 / 8, zero "unknown"
```

## VERDICT

**`count(HOST) = 2 > 0` → INC-2 resolves to 7C-bound. 7B Plane A cannot be host-free at the build
boundary in current bbnf.** The construction *values* (steps 1–3) are pure `->`; the **build
mechanism itself is not**. Two steps are host-bound, and they are host-bound for **different reasons**
— the distinction is the actionable payload of this census:

- **Step 4 `add_expr` — SOFT host, floor +1.** Threads `&mut GrammarIr` and edge-builds ExprIds, so
  the invariant forbids a GRAMMAR mark (no bbnf operator mints ids). *But* the arena is a
  non-interned post-order tree (PROOF A/A'), so it is a catamorphism — **dischargeable by a
  value-language arena/list primitive**, not a per-grammar host fn. This is the corrected reading of
  R0: R0's "it's a synthesized attribute" is true *in principle* and false *for today's `->`*.
- **Step 5 `resolve_refs` — HARD host, floor +1, TreeFold.** Forward/cyclic cross-rule name
  resolution over a global symbol table (PROOF B); no synthesized `->` can express it **in principle**
  (bbnf has no inherited/global attribute). Architecturally **separable** (already its own pass).

**bbnf's totality floor, fixed:**

```
@host add_expr     : (&mut GrammarIr, ExprKind, SourceSpan) -> ExprId              // Hs — arena builder;
                                                                                   //      or dissolve via a value-lang list/arena primitive
@host resolve_refs : (&mut GrammarIr) -> Result<(), UnresolvedRef>                 // Hh — HostContext::TreeFold; whole-grammar link
```

- structural node-*value* floor = **0** (steps 1–3 pure `->`; step 8 SpanCapture-class);
- arena-build floor = **1 SOFT** (`add_expr`, dischargeable by language extension);
- whole-grammar-link floor = **1 HARD** (`resolve_refs`, TreeFold);
- CompileIO = **1** (`@import`); β-reduction **absent**.

The 7B-Plane-C "IR interning is as hard as the original problem" premise is **half-true**: it does
**not** hold for the constructor's node-building (a non-interned tree — the fear is empirically false
there, PROOF A), and it **does** hold for the one separable linking edge (`Ref→RuleId`), which 7C
houses at the hard floor of **1**. INC-2 therefore lands at **7C-bound, floor = 2** — with the SOFT
half retractable to 0 by a value-language extension, leaving an **irreducible floor of 1**
(`resolve_refs`) that 7B Plane A can never make host-free.
