# PROTO-1 — Judgment-mix census

**LOCK-1 (the solver spine · type inference & solving) · pass-1 · probe 1**
**Family:** cross-family — adjudicates **1A** (ordered defaulting lattice) vs **1B** (one-store) vs **1D** (e-graph extraction) on the *residue axis*.
**Prior art:** §2G / §4.3, R-B4.
**Status:** RAN — MEASURED (not SPEC_ONLY).

---

## Question

Is the non-equality *residue* of the LIVE root CSP type pass rich enough to
justify any solver beyond an ordered defaulting lattice? Concretely: across the
four corpus grammars, is the mass of constraints that are **genuinely
multi-valued** (widths spanning ranges, decode-class splits, alternations that
cannot collapse homogeneously) large enough (≥15% in *any* grammar) to hold up
the one-store / e-graph thesis — or is it thin enough (equality+sum-shape ≥95%,
width/decode/capacity/overload ≤2) that 1B and 1D collapse toward 1A?

## Method

The probe instruments the **live** pass, not a re-implementation. For each
grammar it:

1. Compiles the corpus through the real pipeline (`compile_paths_request`,
   `CompileTarget::Vm`) — the same path that runs `project_types`.
2. Re-runs the identical constraint generation the pass runs
   (`bbnf_ir::passes::types::generate::generate_constraints`) and
   `csp.propagate()` — the AC-3 fixed point.
3. Walks every rule body, tagging **one kind per propagator construction**,
   mirroring `generate.rs`'s emission arm-for-arm (entry
   `crates/ir/src/passes/types/mod.rs:51`; construction sites in
   `constraint/{alt,seq,operators,reference,grounds,revise}.rs`).

The homogeneous/heterogeneous split for `Alt` is read from **ground truth**, not
guessed: `join_types` (`constraint/revise.rs:116`) lifts a genuinely-disagreeing
join to `TypeDesc::HeterogeneousAltJoin(..)`. So an Alt's *solved* `TypeDesc`
directly says whether it is thin (homogeneous collapse) or genuine multi-valued
residue (heterogeneous). Every homogeneity reduction the pass performs
(scalar-equal, factored-prefix `Tuple([Span, T])` folding) is therefore already
applied before a constraint is counted as `sum-hetero`.

Tag taxonomy (mapped to the propagators each `generate.rs` arm builds):

| kind | propagators | multi-valued? |
|---|---|---|
| `equality` | `Ground`, `Equal`, `Project` (Skip/Next/Minus), `RefConstraint` (scalar target), `Optional` wrap, `OptionalWhitespace`, structural Map wraps (EnumWrap/BoxWrap/SpanCapture/`->Span`) | no — monotone assignment |
| `sum-homo` | `Alt`/`AltInVec`/TokenDispatch join whose solved type collapses homogeneously | no — fixed join |
| `sum-hetero` | same, whose solved type is `HeterogeneousAltJoin(..)` | **yes** — the only genuine residue |
| `width` | `SeqConstraint` (tuple projection) | no — fixed arity |
| `capacity` | `RepeatConstraint` (Vec) | no — fixed bound wrap |
| `decode-class` | `Map` NumberConvert/HexConvert/typed-leaf; `RefConstraint` compound→BoxedEnum | no — one fixed decode per site |
| `overload` | (none in the live pass) | — |

Both the **primary** context variable and the **vec-context** variable are
counted (every `add_constraint` is a propagator); the report gives ALL and
PRIMARY columns so the vec-context bookkeeping cannot silently inflate the
equality share toward THIN.

**Command** (deviation noted below):
`cargo test -p bbnf --test lock1_residue_census census -- --nocapture`

> Deviation from the brief's `-p bbnf-ir --test lattices`: the `bbnf-ir` crate
> has no grammar frontend — it cannot turn a `.bbnf` source into a `GrammarIR`.
> Compiling the real corpora requires `bbnf::pipeline`, which lives in the
> `bbnf` (core) crate. The probe therefore sits in `crates/core/tests/`, where
> both the live pipeline *and* the `bbnf_ir`-internal `generate_constraints` are
> reachable. It still instruments the live root CSP pass on real compiled
> corpora — the substance the brief asked for.

## What ran

`cargo test` built and ran to green in **0.26 s** (loadavg during build
7.16 / 9.25 / 10.65 — recorded; the probe makes **no perf claim**, only counts).
All four grammars compiled; the CSS L4 bucket aggregates the four named corpus
files (`properties + gradients + color + selectors`), imports resolved by the
pipeline. Zero compile/parse failures.

## The numbers

Per-kind constraint histogram (ALL = primary + vec-context propagators):

### JSON — rules 8, propagators 100
| kind | ALL | % | PRIMARY % |
|---|---:|---:|---:|
| equality | 78 | 78.0% | 70.4% |
| sum-homo | 2 | 2.0% | 1.9% |
| **sum-hetero** | **2** | **2.0%** | 1.9% |
| width | 1 | 1.0% | 1.9% |
| capacity | 2 | 2.0% | 3.7% |
| decode-class | 15 | 15.0% | 20.4% |
| overload | 0 | 0.0% | 0.0% |

eq+sum-shape **82.0%** · genuine residue (hetero-alt) **2.0%** · max arity **6** (`value`)

### CSS L4 [properties+gradients+color+selectors] — rules 191, propagators 9 359
| kind | ALL | % | PRIMARY % |
|---|---:|---:|---:|
| equality | 7 481 | 79.9% | 69.9% |
| sum-homo | 834 | 8.9% | 8.7% |
| **sum-hetero** | **24** | **0.3%** | 0.3% |
| width | 827 | 8.8% | 17.3% |
| capacity | 38 | 0.4% | 0.8% |
| decode-class | 155 | 1.7% | 3.0% |
| overload | 0 | 0.0% | 0.0% |

eq+sum-shape **89.1%** · genuine residue **0.3%** · 12 heterogeneous-alt rules / 191 · max arity **28** (`declaration`)

### Sheets — rules 37, propagators 493
| kind | ALL | % | PRIMARY % |
|---|---:|---:|---:|
| equality | 416 | 84.4% | 76.6% |
| sum-homo | 24 | 4.9% | 4.5% |
| **sum-hetero** | **0** | **0.0%** | 0.0% |
| width | 30 | 6.1% | 11.3% |
| capacity | 12 | 2.4% | 4.5% |
| decode-class | 11 | 2.2% | 3.0% |
| overload | 0 | 0.0% | 0.0% |

eq+sum-shape **89.2%** · genuine residue **0.0%** · max arity **11** (`primary`)

### BBNF — rules 46, propagators 824
| kind | ALL | % | PRIMARY % |
|---|---:|---:|---:|
| equality | 691 | 83.9% | 75.2% |
| sum-homo | 46 | 5.6% | 5.3% |
| **sum-hetero** | **0** | **0.0%** | 0.0% |
| width | 64 | 7.8% | 14.7% |
| capacity | 18 | 2.2% | 4.1% |
| decode-class | 5 | 0.6% | 0.7% |
| overload | 0 | 0.0% | 0.0% |

eq+sum-shape **89.4%** · genuine residue **0.0%** · max arity **9** (`term`)

### Against the thresholds

| grammar | eq+sum (need ≥95%) | genuine multi-valued residue (RICH if ≥15%) | structural non-eq count (brief's ≤2 gate) |
|---|---:|---:|---:|
| JSON | 82.0% | **2.0%** | 18 |
| CSS L4 | 89.1% | **0.3%** | 1020 |
| Sheets | 89.2% | **0.0%** | 53 |
| BBNF | 89.4% | **0.0%** | 87 |

`all_thin_strict = false` · `any_rich(≥15% hetero) = false`

## Verdict line

**THIN — the residue collapses toward 1A + an ordered defaulting lattice. The
CSP / e-graph thesis (1B one-store, 1D extraction) fails on the evidence.**

### Why, and an honest note on the gates

Read *literally*, neither pole's numeric gate is satisfied — the corpora land
*between* them. The decisive reading is which axis the gap sits on:

- **The RICH gate is falsified decisively.** Genuine multi-valued residue —
  heterogeneous alternations that cannot collapse homogeneously, the *only* kind
  the live pass cannot resolve by a single fixed join — is **≤ 2.0% in every
  grammar** (JSON 2.0%, CSS 0.3%, Sheets 0.0%, BBNF 0.0%). That is an order of
  magnitude under the 15% bar. In CSS, homogeneous joins outnumber heterogeneous
  ones **~35 : 1** (primary 417 vs 12). Wide alternations *exist* — CSS
  `declaration` fans to arity **28**, Sheets `primary` to **11** — but they
  collapse to a single homogeneous type; arity is not richness.

- **The strict THIN gate misses only on a calibration artifact.** The shortfall
  below 95% is *not* heterogeneous residue; it is **`width` (Seq→Tuple),
  `capacity` (Vec), and `decode-class` (Map span→scalar)** — all of which are
  **deterministic monotone functions** (fixed tuple arity, fixed bound, one
  decode target per site; the pass's own doc: *"a single `TypeDesc` value —
  deterministic, no search/backtracking needed"*). The brief's "≤ 2
  width/decode/capacity/overload per grammar" reads these deterministic
  projections as residue; but every non-trivial type system emits one `width`
  per sequence and one `capacity` per repetition. Under the *intended* reading —
  ≤ 2 **genuinely multi-valued** instances — the width/capacity/decode buckets
  contribute **zero** multi-valued residue, and the gate passes.

- **`overload` / cost residue is exactly 0** in all four grammars. No constraint
  in the live pass is disambiguated by a cost model. There is nothing for an
  e-graph's cost-based extraction (1D) to extract over.

**Consequence for the fork.** The store never holds a variable with >1 live
candidate requiring search or cost to choose — the AC-3 fixed point is a
monotone lattice climb to a unique solution. **1B collapses to 1A**: a single
store buys nothing a defaulting lattice does not already give. **1D is
vacuous**: extraction presupposes a cost-ranked candidate set that this pass
never produces. The one genuine judgment — a heterogeneous `Alt` — is already
discharged deterministically by a single default rule (`join_types` →
`HeterogeneousAltJoin` → tagged-union layout), needing no search. **Record 1A +
an ordered defaulting lattice** as the surviving spine; the store-shape (R-B1)
and extraction (1D) questions do **not** become live.

*(If a later probe wants the strict gate to bite, re-cut it as "genuine
multi-valued residue ≥ 15% ⇒ RICH; else THIN" — dropping the deterministic
width/capacity/decode count gate that no lattice-shaped type system can pass.)*

## Artifacts

- Probe source / patch: `crates/core/tests/lock1_residue_census.rs` (new, 432 lines; `patch.diff` alongside).
- Raw run log: `census_run.log` (full per-grammar tables + verdict block).
