# PROTO-2 — GrammarIr construction-semantics census (the 7B↔7C build boundary)

LOCK-7 pass-1, probe 2 (family 7C HOST-FUNCTION LATTICE ↔ 7B Plane A). Adjudicates **INC-2** and
fixes bbnf's totality floor. **RAN_MEASURED**: census over live source at HEAD (2026-07-16) plus a
3-assertion probe compiled and executed against the `grammar` crate; every classification claim is
either a verified `path:line` or a passing test. Loadavg recorded. No live source mutated — the
probe was written to `crates/grammar/tests/`, run, then removed; its text is preserved as
`PROTO-2.probe.rs` (the patch).

> Naming note: the 04:05 `PROTO-2.{md,patch}` were a *prior brief-set's* family-7D regex-oracle
> probe, superseded by the 04:51 FAMILY-SPECS re-synthesis (§6 names Brief 2 = "construction
> census"). Preserved verbatim as `PROTO-2-7D-regex-oracle.{md,patch}`; nothing lost.

---

## Question (INC-2, verbatim)

> Can bbnf's **GrammarIr CONSTRUCTION** be expressed in pure grammar `->` projections
> (7B: floor = 0), or does it require typed host functions (7C: floor > 0)?

INC-2 in FAMILY-SPECS §5 frames the fear precisely: *"bbnf's GrammarIr construction (arena
interning, ExprId edges, closure β-reduction) is either `->`-expressible (7B) or
recursive-with-state and host-fn-bound (7C) … 7B Plane C is BLOCKED-candidate precisely because
graph-shaped interned IR ⇒ the 'CFG-expressible IR' primitive is as hard as the original
problem."* This probe tests that fear against the real constructor.

## Method

1. **Enumerate** every GrammarIr-construction site the hand parser performs
   (`skinny/crates/grammar/src/lib.rs`, 2142 LOC; builder IR in `skinny/crates/ir/src/lib.rs`):
   grep for `intern`/`Interner`, `add_expr`/`add_rule` (ExprId/RuleId alloc + edge-linking),
   `resolve*`/closure/β-reduction, `@import`, and every `-> value_expr` builder.
2. **Cross-map** each site to the `grammar/bbnf/bbnf.bbnf` (+ `expressions.bbnf`) rule it realizes.
3. **Classify** each into {GRAMMAR-`->` · typed-host-fn (ParseState/TreeFold) · CompileIO}.
4. **Run** a probe that empirically discriminates interner-vs-append and construct-vs-link.

## What ran (measured)

`CARGO_TARGET_DIR=<scratch> cargo test -p grammar` at HEAD, scratch target (no lock contention).
Loadavg 9.85 → 8.93 (1-min) across the run; cold build.

- **Existing suite, construction path** — `parses_skinny_json_rules` **ok**,
  `resolves_forward_and_cyclic_refs` **ok**, `rejects_unresolved_refs` **ok**. (One *unrelated*
  pre-existing red: `w5b_frontend_public_ws_remains_retired`, lib.rs:1866 — a runtime-frontend-ws
  harvester assertion, not a construction site; noted, not mine.)
- **Probe `proto2_construction_census` — 3 passed / 0 failed:**
  - `add_expr_does_not_intern_identical_leaves` **ok** — `a = "x" , "x" ;` yields **two** distinct
    `Literal{b"x"}` ExprIds. **No structural interner exists.**
  - `arena_ids_are_post_order_synthesized` **ok** — every child ExprId `<` its parent Seq ExprId.
    **IDs are a post-order numbering = a synthesized attribute.**
  - `cross_rule_link_is_a_separate_global_pass` **ok** — forward+cyclic ref resolves only via the
    whole-rule table; an undefined ref is a *link* error, not a *parse/construct* error.

## The classification table

**Legend** — `G` = GRAMMAR-`->` expressible (pure synthesized attribute); `H` = typed-host-fn;
`IO` = CompileIO. `ir:51` = `add_expr` (ExprId alloc), `ir:57` = `add_rule` (RuleId alloc).

### A · Construction sites — the `parse_grammar` node-builder (ALL `G`)

| # | site (grammar/src/lib.rs) | ir | bbnf.bbnf rule | construction op | class |
|---|---|---|---|---|---|
| C0 | `GrammarIr::new` 1270 | ir:42 | grammar:85 | empty arena init | G |
| C1 | `add_rule(name,body,span)` 1287 | ir:57 | rule:56 | RuleId push + body edge | G |
| C2 | `Alt{branches,Dispatch}` 1351 | ir:51 | alternation:48 | ExprId, branch edges (child ids) | G |
| C3 | `Annotation{ws}` (`?w`) 1371 | ir:51 | term/modifier:38 | ExprId leaf | G |
| C4 | `Seq(∅)` 1393 | ir:51 | concatenation:47 | ExprId | G |
| C5 | `Seq(children)` 1400 | ir:51 | concatenation:47 | ExprId, child edges | G |
| C6 | `Discard{left,right,keep_left}` 1423 | ir:51 | binary_factor:45 / binary_operators:44 | ExprId, 2 edges | G |
| C7 | `Repeat{min0}` (`*`) 1439 | ir:51 | modifier:38 | ExprId, body edge | G |
| C8 | `Optional` (`?`) 1456 | ir:51 | modifier:38 | ExprId, body edge | G |
| C9 | `Repeat{min1}` (`+`) 1463 | ir:51 | modifier:38 | ExprId, body edge | G |
| C10 | `Project{body,target,ty}` 1485 | ir:51 | mapped_factor:42 | ExprId, body edge + span strings | G |
| C11 | `Literal{bytes,case}` 1501 | ir:51 | literal:11 / term:31 | ExprId leaf | G |
| C12 | `Regex{pattern}` 1511 | ir:51 | regex:15 / term:32 | ExprId leaf | G |
| C13 | `Ref{name,target:None}` 1524 | ir:51 | identifier:9 / term:30 | ExprId leaf, **cross-rule edge DEFERRED** | G |
| C14 | paren pass-through 1516–1520 | — | term:34 `( rhs )` | returns child id, emits **no** node | G (transparent) |

**15 construction sites. All G.** The arena (`add_expr`: `id = ExprId(exprs.len()); exprs.push`)
is **append-only with zero dedup** — proof C-1. Each `parse_*` returns its ExprId as a *synthesized
attribute*; parents embed children's ids bottom-up (C2/C5/C6/C7–C10 edges) — proof C-2. Every one
maps onto an existing bbnf rule, and `mapped_factor:42` already supplies the `-> value_expr : type`
hook that would carry the projection. `Ref` (C13) stores **`target: None`** — the only graph edge
is *not built here*.

### B · Whole-grammar linking — a SEPARATE pass, not node-building

| # | site | ir | scope | op | class |
|---|---|---|---|---|---|
| L1 | `resolve_refs()` — called `parse_grammar` grammar:33 | ir:84 | **whole grammar (cross-rule)** | build `HashMap<name,RuleId>` symbol table, mutate every `Ref.target` | **H (TreeFold)** |

`resolve_refs` needs the *complete rule set* (forward + cyclic refs — proof C-3) and a mutable
symbol table. It is **not** a per-production synthesized projection: bbnf's `->` is
synthesized-only (a match → a value from its children + `input`); it has **no inherited /
global-environment attribute**. So name→RuleId resolution is beyond today's `->`. Critically, the
constructor **already factors it out**: `parse_rules` builds `Ref{target:None}` (pure), then
`resolve_refs()` runs as a distinct post-parse graph transform. It is TreeFold, and **separable**.

### C · Non-construction paths (the frontend-closure harvest — not the GrammarIr builder)

| # | site | bbnf.bbnf | op | class |
|---|---|---|---|---|
| I1 | `resolve_runtime_import_closure` grammar:963 | import_directive:60 (`@import`) | multi-file source-set closure + adjacency + cycle detect | **IO** |
| I2 | closure β-reduction `\|params\| rhs` → GrammarCall | closure:51 (recognized) | **ABSENT** in skinny builder — bbnf.bbnf *recognizes* `closure`, the builder never lowers it | — (0 live; would be IO) |
| I3 | `@{` HostCapture — scan grammar:420–431 → `BBNF-UNSUPPORTED-HOST-CAPTURE` ir-path lib.rs:222 | term:33 (`@{`) | host capture, **unlowered/dormant** | H (dormant) |

`@import` (I1) resolves a *source-set* closure for the runtime span-tape frontend — **not** the
`GrammarIr` builder (confirms FAMILY-SPECS §0). Grammar-function `|params| rhs` β-reduction (I2) is
**unimplemented** in the skinny constructor — it is recognized by `closure:51`/`value_closure:33`
but never inlined; there is **no live β-reduction site to classify**.

## The numbers

```
N_host  (construction, node-building)        = 0     ← every ExprId/RuleId site is synthesized
N_host  (mandatory whole-grammar linking)    = 1     ← resolve_refs (ir:84), TreeFold, SEPARABLE
N_compileio                                  = 1     ← @import source-set closure (grammar:963)
   of which closure β-reduction               = 0     ← absent/unimplemented in skinny
dormant typed-host-fn (neither build nor IO) = 1     ← @{ HostCapture, unlowered (ir-path:222)
construction sites classified                = 15 / 15, zero "unknown"
```

## VERDICT

**7B Plane A reaches parity in PURE grammar for GrammarIr CONSTRUCTION: `N_host_construction == 0`.**
Structural construction — arena allocation and intra-rule ExprId edge-building — **is
`->`-expressible**. The INC-2 fear is *empirically false for the skinny tree*: **there is no
interner** (C-1: identical leaves → distinct ExprIds), ids are **post-order synthesized** (C-2),
and the sole graph edge `Ref→RuleId` is **deferred out of the constructor** into a separate global
pass (C-3). "Graph-shaped interned IR" does not exist here — the built IR is a **tree in an
append-only arena**, which *is* a catamorphism / synthesized attribute.

**The single recursive-with-state primitive is NOT construction** — it is cross-rule name
resolution **`resolve_refs` (ir:84)**: a whole-grammar `HashMap<name,RuleId>` symbol-table link,
**TreeFold**-class, beyond bbnf's synthesized-only `->`. It is architecturally **separable** (the
codebase already runs it as its own pass after `parse_grammar`), so it does **not** block 7B Plane
A's *construction* parity.

**INC-2 adjudication — a clean split at the phase boundary:**
- **CONSTRUCTION (node-building) → 7B wins.** Floor = 0 host fns; pure `->`.
- **LINKING (name resolution) → 7C-bound, floor = 1.** The named primitive that mandates a typed
  host fn, with augment-BBNF signature:

  ```
  @host resolve_refs : (&mut GrammarIr) -> Result<(), UnresolvedRef>    // HostContext::TreeFold
  ```

  (Alternatively discharged by extending `->` with an *inherited* whole-grammar name-environment
  attribute — a strictly larger language change than the one host fn; the host fn is the parsimonious floor.)

**bbnf's totality floor, fixed:** structural-construction floor **= 0**; whole-grammar-linking
floor **= 1 TreeFold** (`resolve_refs`); CompileIO **= 1** (`@import`); β-reduction **absent**.
7B Plane A is *not* BLOCKED at the build boundary — the "IR interning is as hard as the original
problem" premise (7B Plane C's BLOCKED-candidate rationale) **does not hold for the constructor**;
it holds, if anywhere, only for the one separable linking pass, which 7C houses at floor 1.
