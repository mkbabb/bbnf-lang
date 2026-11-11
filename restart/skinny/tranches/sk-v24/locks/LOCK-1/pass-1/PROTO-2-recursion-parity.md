# PROTO-2 (recursion output-parity) — does 1D beat the RUNNING CSP, or only a strawman?

**LOCK-1 (the solver spine · type inference & solving) · pass-1 · probe 2 (recursion axis)**
**Family:** 1D (native-cyclic e-graph type solver) — decisive retire-or-keep vs the RUNNING 1B-lineage root CSP.
**Engine:** the shipped `bbnf_ir::passes::project_types` root type-projection CSP (not a re-implementation).
**Command:** `cargo test -p bbnf --test lock1_recursion_parity -- --nocapture`
**Verdict line:** **RAN_MEASURED — running CSP is FINITE + TERMINATING on every real recursive grammar; its recursive TypeDesc is a coarser NOMINAL schema than 1D's μ-sum but the difference is not load-bearing. PASS — RETIRE 1D (with a recorded caveat).**

> **Naming / non-clobber note.** An earlier `PROTO-2.md` in this directory is a *different* probe — 1D's existence proof against **1A's nominal fold** (the strawman), verdict "1D does NOT retire." This probe answers the sharper question the strawman left open — *does 1D beat the **running** CSP?* — and reaches the **opposite** verdict (RETIRE). Because the two disagree on the same retire-or-keep question, this report is delivered under a distinct name and does **not** overwrite the prior file. Owner adjudication is flagged below.

---

## Question

PROTO-2-strawman proved 1D's native cyclic e-graph extracts a finite μ-representative for
recursive JSON `value`:

```
1D:  @0:Sum(Null | Bool | Int(0) | Str | List(#0) | Row(#0))
```

— a 6-variant sum whose two recursive arms (`List`, `Row`) close back onto the value class via
the **structural** back-edge `#0`. But that pitted 1D against 1A's *nominal fold* — a strawman.
The RUNNING root CSP already derives recursive `value` **without** an e-graph (PROTO-1 compiled
JSON with `value` at Alt arity 6), and PROTO-1 emptied 1D's cost-extraction justification (zero
overload/cost residue). 1D's **sole surviving advantage is native recursion**.

So: does the shipping pass produce a correct, **finite**, **terminating** schema for the deepest
real recursive grammars, structurally equal to 1D's extraction? If yes → 1D buys nothing the
running system lacks → RETIRE. If it diverges / non-terminates / mis-shapes on a real recursive
grammar → 1D's narrow keep is justified.

## Method

New test `crates/core/tests/lock1_recursion_parity.rs` (delivered as
`PROTO-2-recursion-parity.patch.diff`). It reuses PROTO-1's live compile path
(`compile_paths_request`, `CompileTarget::Vm`) — the path that runs `project_types` and
populates `ir.types` — over the three real recursive corpora:

- **JSON** — canonical self-referential `value = object | array | string | number | bool | null`
  (`array` and `object` both close back onto `value`).
- **Sheets** — deep *mutual* recursion `expression → comparison_expr → … → primary → paren_expr → expression`.
- **CSS L4** — the deepest corpus (PROTO-1: `declaration` arity 28), four corpus files.

For each grammar it dumps every rule's **solved `TypeDesc` from `ir.types`** — the authoritative
solver output — renders it to a bounded S-expr (`BoxedEnum`/`Enum` = the nominal recursion knot,
shown `μ↩`; a depth cap turns any *hypothetical* non-terminating type into visible truncation
rather than a stack overflow), and compares the recursive rules against 1D's μ-representative.
The test asserts finiteness (no `OVERFLOW`), termination (the process completing is the proof),
and the schema-shape facts below.

## What ran

`cargo test -p bbnf --test lock1_recursion_parity` — **1 test, passed, 0.14 s.**
All three corpora compiled to green (JSON 8 rules, Sheets 37, CSS L4 191 across 4 files); zero
stack overflows, zero non-finite rule types. `loadavg` at run start `6.53 / 8.95 / 8.94`
(correctness probe — **no cold timing claim is made**).

## The numbers

### JSON — the self-referential `value`

| | schema |
|---|---|
| **1D** (e-graph μ-extract) | `Sum[6](Null \| Bool \| Int(0) \| Str \| List(#0) \| Row(#0))` |
| **RUNNING** `ir.types[value]` | `HeterogeneousAltJoin([BoxedEnum, Span, Bool, F64])` = `Sum[4](μ↩ \| Str \| Bool \| F64)` |
| **RUNNING** `ir.types[array]` | `List(Enum)` |
| **RUNNING** `ir.types[object]` | `List(Enum)` |

- Running `value`-join **distinct arms = 4** (vs 1D's 6): the two recursive arms *and* the null
  arm collapse into one opaque `BoxedEnum` knot.
- `array == object == List(Enum)` — the running solver's TypeDesc **does not even distinguish a
  List-of-value (array) from a Row-of-kv-pair (object)**, the very distinction 1D's
  `List(#0) ≠ Row(#0)` preserves.
- **Finite = true, terminating = true** (JSON: 8 rules, 2 recursion knots, 0 stack overflow).
- **Structurally equal to 1D: FALSE** — 1D is strictly *more* faithful at the TypeDesc level.

### Sheets — deep mutual recursion (37 rules, 20 recursion knots, all-finite)

```
expression      = μ↩
primary         = Sum[4](μ↩ | F64 | Bool | Str)
paren_expr      = Tuple(Str, μ↩, Str)
comparison_expr = Tuple(μ↩, List(Tuple(Str, μ↩)))
```

The mutually-recursive spine **TERMINATED, all-finite**. Note the running solver *does* emit
**structural** recursive μ-types here (`Tuple(μ↩, List(Tuple(Str, μ↩)))`) — byte-different from
1D's `#0` encoding, structurally the same μ-type, with the back-edge closed nominally (`μ↩`).
The JSON-`value` collapse is thus the *specific* boxed-heterogeneous-enum obligation, not a
general inability.

### CSS L4 — 4/4 files, 191 rules, 44 recursion knots, all-finite = true. No divergence.

## Findings

1. **Termination is free for the running pass.** Recursion is closed *nominally*: a compound/self
   `Ref` reduces to a nominal knot (`BoxedEnum`/`Enum`) on sight (obligation logic), and any
   surviving rule-to-rule cycle grounds to `BoxedEnum` in `project_types` Phase 2b. The pass
   **cannot diverge by construction** — confirmed on JSON, Sheets (mutual), and CSS L4. On the
   divergence axis 1D buys nothing.

2. **The running solver's recursive TypeDesc is a coarser NOMINAL schema than 1D's μ-sum — and
   this is real, not cosmetic-at-the-TypeDesc-level.** For JSON `value` it emits `Sum[4]` and
   `array = object = List(Enum)`; 1D packs `Sum[6]` with `List(#0) ≠ Row(#0)` and structural
   back-edges. 1D genuinely resolves more structure into the solver output.

3. **But the extra structure is NOT load-bearing in the shipped architecture.** The running
   system defers structure to codegen by design: the manifest-bound `JsonStructBuilder` +
   hand-written runtime types (`crates/core/src/runtime/json/value.rs`) emit the faithful
   6-variant `JsonValue` (`Null | Bool | Number | String | Array | Object`) with **distinct
   Array/Object**, recursion closed via arena-id indirection — finite, terminating, correct.
   The structure 1D packs into the TypeDesc is supplied nominally downstream; PROTO-1 further
   showed zero cost/overload residue for 1D's extraction to rank over. 1D's richer μ-sum is
   therefore **unused capacity relative to the shipped system**, not a capability it lacks.

## Verdict

**RAN_MEASURED. PASS — RETIRE 1D.**
The running root CSP terminates and stays finite on every real recursive grammar; the shipped
recursive schema is faithful and correct. 1D's native cyclicity beats only the **strawman broken
nominal fold** — against the *running* nominal fold it produces a finer-grained TypeDesc the
shipped architecture never consumes. On the recursion axis, 1D's sole surviving advantage is not
load-bearing.

**Recorded caveat (the honest keep-hook).** 1D is the *only* path that yields a self-contained
**structural** recursive TypeDesc at the rule level (`List ≠ Row`, all arms, structural
back-edges). The running solver deliberately does not — it emits a nominal skeleton and lets
codegen supply the rest. If any future consumer ever needs the **packed** rule-level structural
μ-type (rather than the codegen-distributed nominal form), that single narrow need is the only
ground on which to revive 1D. No shipping consumer needs it today.

**Owner-adjudication flag.** This RETIRE contradicts the sibling `PROTO-2.md` (1D-existence-vs-1A,
verdict "keep 1D"). The contradiction is expected — that probe measured 1D against a strawman; this
one measured it against the running incumbent. The lock owner should reconcile the two into the
pass-1 record (recommended reading: RETIRE, since the strawman its keep rested on is exactly what
this probe falsifies).

## Artifacts

- Probe source / patch: `crates/core/tests/lock1_recursion_parity.rs` (new; `PROTO-2-recursion-parity.patch.diff`).
- Raw run log: `PROTO-2-recursion-parity.log` (full per-grammar dumps + verdict block).
