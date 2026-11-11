# LOCK-5 — THE REGEX CONTINUUM — PROTO-2

**Probe 2 · 5A merged-type: two-views + total Language + leak census — RUN on HEAD**
Family 5A (ONE-HIR clean-merge horn) vs 5B (domain-fit separation horn).

- **HEAD measured on:** `7cdbff4ef411154dd9bf4b80731c0616b1579d22`
  (`locks(sk-v24): pass-1 pt full fold …`). Base-pin `7cdbff4ef` was **newer**
  than the worktree tip `d2b32f6e3`; HEAD was an ancestor, so a clean
  `git merge --ff-only 7cdbff4ef` fast-forwarded. `git merge-base --is-ancestor
  7cdbff4ef HEAD` ⇒ **YES**. Numbers are **not** wrong-artifact-risk.
- **Verdict status:** `RAN_MEASURED`. Every number below comes from an actual
  `cargo build` / `cargo run`, not static reading.
- **Platform:** arm64 (no x86). No perf claims (this is a type/correctness probe).
- parse-that was **READ-ONLY** (only `CharClass`/`Look`/`SaturationCache`
  sources read); the two-views probe copied nothing from parse-that — it
  path-deps the real crate under an isolated `CARGO_TARGET_DIR` in the scratchpad.

---

## Question

Does merging `bbnf_regex::CharClass` / `bbnf_regex::Look` into the ONE authored
`IrNode` keep the `egraph-derive` `Language` projection **total** and let the
cache key on the auto-derived **Id-view** (the two-views claim that dissolves
the god-type) — and is the per-arm **leak census actually 0** when *built* on
the HEAD enum, not read?

Pass condition (5A clean-merge earned): REGEX-LEAK == 0 **AND** Language total
**AND** the spanned duplicate collapses to one e-class **AND** `Serialize`
intact.
Fail (5B domain-fit separation justified): ≥1 REGEX-LEAK **OR** the derive
breaks **OR** the cache cannot key without forcing `Eq`/`Hash` onto the spanned
`IrNode`.

---

## Method (what actually ran)

1. **(a) merge + build.** Added `IrNode::Class(bbnf_regex::CharClass)` and
   `IrNode::Look(bbnf_regex::Look)` to `crates/ir/src/types/node.rs` (Leaves
   block, at the `pub enum IrNode {` site — node.rs:30). Ran
   `cargo build -p bbnf-ir --profile ax-iter` (isolated `CARGO_TARGET_DIR`).
   Captured the full diagnostic set; enumerated every broken match; graded
   each new arm.
2. **(b) derive totality.** Read the `egraph_derive::Language` macro
   (`crates/egraph-derive/src/lib.rs`) field classifier and the existing
   `GrammarENode` (`crates/ir/src/egraph/node.rs`).
3. **(c) two-views collapse.** Built a **standalone probe crate**
   (`scratchpad/proto2-cvec`) that path-deps the **real** `egraph`,
   `egraph_derive`, and `bbnf_regex` (patch table mirrors the working main
   `.cargo/config`), defines an Id-view enum with the merged `Class(CharClass)`
   / `Look(Look)` leaves, and runs hashcons-collapse assertions. Read
   `SaturationCache` (`…/regex/src/egraph/saturation_cache.rs`) and
   `EGraph::add` (`crates/egraph/src/egraph.rs:155`) to establish what the cache
   actually keys on.

---

## The numbers

### (a) Leak census — RAN, `cargo build -p bbnf-ir`

The merge **does not compile**. Diagnostic histogram:

| error | count | meaning |
|---|---|---|
| `E0004` non-exhaustive patterns | **53** | match arms that must be written for the two new variants |
| `E0277` unsatisfied trait bound | **4** | `CharClass: Serialize`, `CharClass: Deserialize`, `Look: Serialize`, `Look: Deserialize` — **the `IrNode` derive breaks** |

**The `Serialize`/`Deserialize` derive on `IrNode` breaks.** `IrNode` derives
`#[derive(Serialize, Deserialize, …)]`. `bbnf_regex::CharClass` and `Look` carry
**no serde impls** — the crate's `serde` feature wires serde into
`sets/charset.rs` (`CharSet128`) only, **not** `hir/mod.rs` (`CharClass`,
`Look`). parse-that is READ-ONLY, so this cannot be patched from bbnf-lang.
⇒ **refutation on the Serialize axis** (E0277 ×4).

**Per-arm grading of the 53 sites.** Grade = *can a mechanical merge write a
correct one-liner from the leaf's zero-child structure (TRIVIAL-TOTAL), or does
the arm need regex-specific semantic dispatch, where a structural `_ =>
empty/false` would be a silent-epsilon correctness bug (REGEX-LEAK)?*

**REGEX-LEAK ≈ 29 sites** (correct arm must reach into the class's byte ranges /
know the leaf's regex semantics). Directly code-inspected & confirmed leaking:

| pass (file:line) | why it leaks |
|---|---|
| `passes/sets/first_sets.rs:92,193,223,275,327,377` (×6) | FIRST of `Regex` calls `regex_first_chars(pattern)`; `Class(cc)`'s FIRST **is** `cc.to_positive_byte_ranges()` — a structural `_ => empty` mis-computes FIRST ⇒ broken dispatch tables (silent epsilon). `Look` is zero-width/nullable — regex-aware. |
| `passes/regex_info.rs:92` | `collect_regex_ids` keys on `StringId`; `Class`/`Look` **have no StringId**. This is the pass that feeds `SaturationCache`; a merged `Class` leaf never reaches the `Hir`-keyed cache at all. |
| `passes/types/{generate.rs:155, mod.rs:354,508, registry.rs:150,419, subvariants.rs:54}` (×6) | inference must map the leaf to a `TypeDesc` (`Class` → Span/str-like; `Look` → zero-width/unit). Not derivable from leaf-ness. |
| `passes/payload/scalar_routing.rs:45` | scalar-payload routing (`can_reach_u32_max`) must know what the leaf materializes. |
| `passes/materialization/{classify.rs:181,587, pin_sweep.rs:94}` (×3) | materialization lattice must classify what a `Class` produces. |
| `passes/recognizers/{grammar_facts.rs:562, node_facts.rs:27, shape_dispatch/unordered.rs:124,162}` (×4) | recognizer facts (nullability/consumption): `Look` zero-width vs `Class` consumes-1 — regex semantics. |
| `passes/sets/{dispatch/annotate.rs:42, dispatch/eligibility.rs:109, dispatch/first_set.rs:16, follow.rs:92, structural_alphabet.rs:278,342}` (×6) | FIRST/FOLLOW/alphabet all need the class's byte content. |
| `passes/audit/payload_coverage.rs:532` | payload audit must know the leaf's payload. |
| `vm/compiler/node.rs:11` | bytecode compiler must **codegen** a byte-range matcher for `Class` — regex-specific lowering. |

**TRIVIAL-TOTAL ≈ 24 sites** (structural leaf, one-liner from zero-child-ness).
Directly confirmed representative: `passes/span.rs:80,233` (leaves span-eligible
`true`), `passes/inspect/walk.rs:28` (leaf no-op). By pass semantics:
`types/node.rs:153,181` (`for_each_child*`), `passes/sets/{deps.rs:22,
sort.rs:42,282}`, `passes/transform/{alias, fuse, inline, pattern_dedup, prune}`
(leaves inert), `passes/metadata.rs:24`, `passes/csp_strategy/components.rs:294`,
`dag/mod.rs:160`. Note the three **lowering** sites — `dag/build.rs:59`,
`egraph/build_egraph.rs:25`, `dag/mod.rs:160` — are structural one-liners **but**
they force the merge to be mirrored into **two further enums** (`DagNode`,
`GrammarENode`); "trivial" per arm, yet they show the "ONE authored type"
premise is already false today (IrNode → DagNode → GrammarENode are three
hand-mirrored enums).

> Directly read & graded: `first_sets.rs`, `regex_info.rs`, `types/generate.rs`,
> `payload/scalar_routing.rs`, `span.rs`, `inspect/walk.rs`. The rest are graded
> from pass purpose; the ~29/~24 split is approximate, but the pass/fail fact —
> **REGEX-LEAK ≫ 1** — is robust.

### (b) `egraph-derive` `Language` totality — RAN (compile) + macro-read

`egraph_derive`'s `classify_field` sorts a field into `SingleId`/`SliceId`/
`ArrayId` **only** for `Id`/`Box<[Id]>`/`Vec<Id>`/`[Id;N]`; **everything else is
`FieldKind::Leaf`** ⇒ that variant's arm is `… => &[]`. So `Class(CharClass)` /
`Look(Look)` classify exactly like the existing `Literal(StringId)` leaf: **the
projection stays total**, no per-field attrs. Because `CharClass` and `Look`
both `#[derive(… Eq, Hash)]`, the Id-view enum still derives `Eq + Hash`.
**Confirmed by compiling the probe** — the merged Id-view enum builds clean.
⇒ **5A holds on the Language-totality axis.**

### (c) Two-views collapse — RAN, `scratchpad/proto2-cvec`, exit 0, **ALL-PASS**

```
[PASS] compiles => Language projection total over regex leaves + Eq/Hash intact
[PASS] spanned view Serialize intact + offsets distinct
[PASS] two offsets of the same Class collapse to ONE e-class (span-erased Id-view)
[PASS] distinct class content stays a distinct e-class (no false-merge)
[PASS] negated [^a-z] distinct from [a-z] (negated flag lives in Eq/Hash key)
[PASS] identical Look collapses to one e-class; distinct Look stays distinct
[PASS] Id-view Eq/Hash obtained WITHOUT forcing Eq/Hash onto the spanned type
RESULT: ALL-PASS
```

The egraph **hashcons** (`EGraph::add` → `self.memo.get(&node)`,
`crates/egraph/src/egraph.rs:155`) collapses two `Class(cc)` inserts — modelled
as two authored spanned nodes at offsets 10 and 200 — to **one** e-class,
because the Id-view carries no span (span-erased) and `CharClass` supplies the
`Eq+Hash` key. The spanned authored view keeps `Serialize` and distinct offsets;
`Eq`/`Hash` are **not** forced onto it. ⇒ **5A holds on the collapse axis.**

**BUT the *named* cache cannot key on the Id-view.** `bbnf_regex::egraph::
SaturationCache` is `FxHashMap<Hir, Hir>` — it keys on `bbnf_regex::hir::Hir`,
**not** on any `IrNode`/`GrammarENode` Id-view. The brief's "SaturationCache
keys on the auto-derived Id-view" is **type-level false**: the collapse that
works is the **egraph memo**, a *different* cache than the one named. To make the
merged `Class` leaves reach `SaturationCache` at all they would have to be `Hir`
values — exactly the domain-fit separation 5B argues for. In the probe I had to
give the spanned view a serde-safe `Vec<(u8,u8)>` projection instead of the raw
`CharClass` **because** raw `CharClass` is not `Serialize` — the same E0277 gap
from (a), surfacing again.

---

## Verdict

**5A's clean-merge horn is REFUTED; 5B's domain-fit separation is justified.**
Two independent fail conditions fire, each sufficient on its own:

1. **REGEX-LEAK count ≈ 29 ≫ 0.** Named leaking passes: `sets/first_sets.rs`
   (FIRST), `passes/regex_info.rs` (`collect_regex_ids`, the SaturationCache
   feeder), `passes/types/generate.rs` (inference), `passes/payload/
   scalar_routing.rs`, `passes/materialization/classify.rs`,
   `vm/compiler/node.rs`. A structural wildcard on any of these is a
   silent-epsilon correctness bug — "nothing to map" is **not** earned.
2. **The derive breaks.** `IrNode`'s `Serialize`/`Deserialize` fail to derive
   once `Class(CharClass)`/`Look(Look)` are variants (E0277 ×4); the payload
   types carry no serde impls and parse-that is READ-ONLY.

Plus a third, softer strike: **the cache cannot key on the Id-view** — the named
`SaturationCache` is `Hir`-keyed, not Id-keyed.

**Where 5A does hold** (recorded for fairness): the `egraph-derive` `Language`
projection **is total** over the merged regex leaves, the Id-view keeps
`Eq+Hash`, and the egraph memo **does** collapse span-distinct duplicates to one
e-class without forcing `Eq/Hash` onto the spanned type. The two-views geometry
is sound *on the egraph axis*; it is the **tax axis (per-pass leak census)** and
the **serialization axis** that dissolve the "clean merge / god-type dissolved"
claim. Net: 5A is half-right on structure and wrong on cost — the 53-arm,
~29-leak, 4-serde-break bill is the domain-fit tax 5B predicted.

---

## Deliverables in this directory

- `PROTO-2.md` — this report.
- `proto2-irnode.patch` — the `git diff` of the (a) merge into
  `crates/ir/src/types/node.rs` (the change that produced the 53+4 diagnostics).
- `proto2-cvec/` — the standalone two-views probe crate (Cargo.toml + src/main.rs)
  that RAN ALL-PASS against the real `egraph` / `egraph_derive` / `bbnf_regex`.
