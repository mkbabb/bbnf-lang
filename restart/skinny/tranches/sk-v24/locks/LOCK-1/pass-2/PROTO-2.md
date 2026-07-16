# PROTO-2 — Row-inference vs hint: is List ≠ Row engine-agnostic?

LOCK-1 (THE SOLVER SPINE — type inference & solving), pass-2, probe 2.
Family **1D / Open R** (the structural axis).

- **HEAD measured on:** `ce51c36fcbdf5108eb028005586672d9a6d2ca1b`
- **Base-pin:** `ce51c36fc`. Worktree opened stale at `d2b32f6e3`; `git merge-base
  --is-ancestor ce51c36fc HEAD` = NO. Fast-forwarded with `git merge --ff-only
  ce51c36fc`, re-verified = **YES**. **No wrong-artifact-risk.**
- **Result:** RAN, measured. `test result: ok. 1 passed`.
- Loadavg at run ≈ 12 (machine loaded — irrelevant; the numbers are categorical
  classifications, not timing).

## Question

Can record-vs-list be **DERIVED by inference from body structure alone** — no
`{`/`[` byte match, no per-grammar name gate — closing the masked fallback? If
yes, Open R dissolves the engine axis's claim on structure (and 1D's raison
d'être). If a per-grammar hint is required, the workaround merely RELOCATES into
the type layer → 1D's structural-μ hook is BLOCKED-candidate.

## Method (what ran)

**Vocabulary added** (engine-neutral, into the real IR):

- `TypeDesc::Row(Box<TypeDesc>)` — `crates/ir/src/types/type_desc.rs`. The
  structural dual of `Vec`: `Vec` is index→T, `Row` is key→T.
- `LayoutKind::Row` — `crates/ir/src/registry/struct.rs`.
- Both backends resolve `Row` to their native keyed shape (Rust →
  `Vec<(Span,T)>`; TS → `Record<string,T>`) — proving it is engine-agnostic
  vocabulary, not an engine hook.

**One structural classifier** — `TypeShapeAnalysis`, a store-style pure-IR
analysis in `crates/ir/src/passes/types/constraint/shape.rs`. It reads three
engine-neutral facts, and nothing else:

1. **A repetition spine** — a `Repeat { hi>1 }` reachable in the rule body
   (the `*`). No spine → not a collection → refuse.
2. **The repeated element's projected type** — resolved through the element's
   `Ref` into `ir.types`, and (because the Seq/Ref projection collapses a
   `Named` role behind a `Ref` to `BoxedEnum`) through the *referent rule's own
   type* where the role survives. Never a byte of the element's literal surface.
3. **A `->`-declared key role** — `TypeDesc::Named("key")`. Record iff the
   element carries the key role; list iff a bare single value; **refuse**
   (Unknown) for a repeated 2-leg element with no declared key.

**Corpus** — two grammars compiled through the real pipeline (`compile_grammar`
→ VM target → `project_types`), in `crates/core/tests/proto2_row_shape.rs`:

- **BARE** JSON structure — zero `->`.
- **TYPED** — byte-identical structure plus a *single* `->`-declared key role on
  the pair's key leg. The role is declared rule-level
  (`keystr = /"[^"]*"/ -> input : key`, the return-type-annotation form real
  `json.bbnf` uses for its `: String` custom type). NB: the inline
  `(string -> key)` form the brief names trips the typed-materialization
  invariant (`lower/expression/wrap.rs:242` — an arrow applied to a `Ref` inside
  a `Seq` finds no value_expr head), and a bare `-> key` collapses to `Span`.
  The rule-level `: key` annotation is the same `->`-declared fact — an
  engine-agnostic `TypeDesc::Named("key")` on the pair's leading leg — and is the
  **only** delta from BARE.

## The numbers

Compiled IR types (dumped by the test):

```
BARE (zero ->)                       TYPED (+ keystr -> input : key)
  string : Span                        string : Span
  number : Span                        number : Span
  object : Vec(Enum)                   keystr : Named(9)          # 9 = id("key")
  array  : Vec(Enum)                   object : Vec(Enum)
  pair   : Tuple([Span, BoxedEnum])    array  : Vec(Enum)
  value  : HeterogeneousAltJoin(...)   pair   : Tuple([BoxedEnum, BoxedEnum])
                                       value  : HeterogeneousAltJoin(...)
```

Two facts jump out of the dump:

1. **The status-quo inference already collapses `object: Vec(Enum)` and
   `array: Vec(Enum)` identically.** As-shipped, the type system CANNOT tell
   record from list — this IS the masked fallback the probe targets.
2. **The pair's projected tuple erases the key role** (`Tuple([BoxedEnum,
   BoxedEnum])`), because the Seq/Ref projection collapses `Named` behind a
   `Ref`. The role survives only on `keystr`'s own type (`Named(9)`); the
   classifier recovers it there.

Classifier outputs:

| grammar | class(object) | class(array) |
|---------|---------------|--------------|
| **TYPED** (+ `-> key`) | **Row** | **List** |
| **BARE** (zero `->`)   | **Unknown** (honest refusal) | **List** |

- **Pass/fail (numeric):** TYPED `class(object).kind==Row` **AND**
  `class(array).kind==List` → **PASS** (both hold).
- **Grep gate:** `grep -E '"\{"|has_rule("value")'
  crates/ir/src/passes/types/constraint/shape.rs` → **ZERO hits** (exit 1).
  No brace-byte read, no rule-name predicate anywhere in the classifier.
- **Row lowers through `TypeAnalysis::from_ir`** (`backend/driver/analysis.rs`):
  the classifier's `derive_collection_type(object)` = `TypeDesc::Row(_)`; written
  into the rule type, it is carried into `analysis.rule_types` — assertion
  passed. `json/value.rs` runtime dispatch is **not** consulted: the classifier
  is a pure-IR analysis; `git diff crates/core/src/runtime/` is **empty**.
- **BARE `object` → Unknown**: the pair is a composite 2-leg tuple with no
  declared key role — indistinguishable from a list-of-pairs — so the classifier
  refuses rather than brace-guessing. `array` is still `List` (a repeat of a bare
  alternation value — a list is unambiguous with no key possible).

## Was a per-grammar hint required?

**No.** The Row/List separation used exactly two engine-neutral ingredients:

1. **Generic structure** — repeat-spine detection, delimiter-scaffold stripping
   by shape, ref resolution, arity/composite discrimination. No `{`/`[` byte, no
   `has_rule(...)`, no `rule.name` compare (grep-gate proven).
2. **One `->`-declared type fact** — the `key` role, a `TypeDesc::Named` the
   grammar author writes in the grammar's own, backend-agnostic type vocabulary
   (every backend resolves it to its native key type). This is the sanctioned
   "key-leg presence, a `->`-declared fact" — **not** a per-grammar name gate,
   **not** external config, **not** a byte peek.

The honest boundary the dump exposes: pure *syntactic* body shape (bare, no `->`)
is **insufficient** — BARE `object` refuses, because a repeated 2-tuple is a
record and a list-of-pairs at once. The record-vs-list bit does not live in the
bytes (`{` vs `[`) the engine axis recognizes; it lives in the **declared type
layer**, which is engine-agnostic. The engine axis therefore holds **no exclusive
claim** on the structural record/list distinction.

## Verdict

**NO per-grammar hint required → structure (as carried by the `->`/type layer)
is orthogonal to the engine → Open R DISSOLVES 1D.**

Record-vs-list is derivable with (a) generic structural inference and (b) a
single engine-agnostic `->`-declared key role, with zero brace-byte reads and
zero grammar-name predicates (grep-gate clean). The masked `json_templates`-style
fallback is closed by a structural classifier; the discrimination the engine axis
claimed lives in the declared-type layer, not in `{`/`[` recognition. 1D's
structural-μ hook has no engine-exclusive substrate to stand on.

Caveat (does not change the verdict): the record bit requires the key role to be
*declared* (`->`). This is not a relocated hint — it is the honest locus of
record-ness in an engine-neutral type fact. A per-grammar name gate or byte peek
was never used and never needed.

## Conditions LOCK-5's 5B re-open (XL-C, §5.7)

Per the brief, **either verdict conditions LOCK-5's 5B re-open.** This probe's
verdict is **DISSOLVE (Open R dissolves 1D)** — the "NO hint" branch. LOCK-5's 5B
re-opens on the finding that the record/list structural axis is engine-agnostic
and lives in the type layer; the engine-axis (1D) structural-μ hook is not merely
BLOCKED-candidate but **dissolved** for this axis.

## Artifacts

- Classifier: `crates/ir/src/passes/types/constraint/shape.rs` (+ `mod.rs`
  wiring).
- Vocabulary: `TypeDesc::Row` (`crates/ir/src/types/type_desc.rs`),
  `LayoutKind::Row` (`crates/ir/src/registry/struct.rs`); exhaustive-match arms
  in the IR intern pass + Rust/TS backend projections (engine-native resolution).
- Probe test: `crates/core/tests/proto2_row_shape.rs` — RUNS, 1 passed.
- Full patch: `PROTO-2.patch` (this directory).
