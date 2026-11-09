# PROTO-1 · t1 — LOCK-3 pass-2 · 3B: EXECUTE the `has_rule("value")` retirement (G-3B-2)

**Family:** 3B — SINGLE-IR-MULTIPASS (the convergent substrate).
**Probe:** t1. Executes the seam-retirement PROTO-2 named but left UNRUN — the first *run* code on
the G-3B-1 fold path (PROTO-2 was trace-only; this changes real emit-dispatch code and re-runs
regen×check). Answers G-3B-2 by execution, not disposition.

**BASE-PIN.** Measured on `HEAD = c93fc43143743ffaf54855c526f6aeab79227352` (worktree
`wf_fce2ec2b-53a-33`). `git merge-base --is-ancestor ce51c36fc HEAD` ⇒ **DESCENDS-OK** (no
fast-forward needed; c93fc4314 already descends from the pin). **No wrong-artifact-risk.** arm64; no
x86. parse-that untouched (READ-ONLY; not needed). Own worktree target dir (no shared-target
contention). loadavg: baseline 4.23 → faithful run 2.52 (1-min), machine up ~1.5h, siblings concurrent.

**Naming note.** Delivered as `PROTO-1-t1.md` / `.patch` (probe token `t1` suffixed) to avoid
clobbering the existing pass-2 `PROTO-1.md` (the `:204`-thread probe) and `PROTO-2.md` (the fold
fact-purity trace) already in this directory. This is a *distinct* probe (the emit-dispatch retirement).

---

## QUESTION

G-3B-2, executed: does the `has_rule("value")` name-gate at `runtime_generator.rs:98` — the emit-path
fork routing JSON → `json_sink_direct` vs Sheets → `general_descent` — **RETIRE** into a *structural*
attribute with **regen×check × {json, sheets, real-typed} byte-identical**? PROTO-2 asserted (by trace)
"relocates to the value-role attribute, NOT `backend_shape`" but ran no code. This probe runs it.

Sub-question that turned load-bearing: **which** structural fact? PROTO-2 named `dispatch_alt_count` /
"program carries a value-dispatch Alt" as the natural surface. Is that surface sufficient?

---

## METHOD (what ran)

Smallest probe that answers it, on the real tree, LOCK-3-local (codegen crate only):

1. **Baseline floor.** Cold `check-json` + `check-sheets` on the clean tree — the byte-identity floor
   the retirement must reproduce.
2. **Crude relocation (the surface PROTO-2 named).** Replaced `!sink_only.has_rule("value")` with
   `sink_only.dispatch_alt_count == 0` (a pre-existing structural field on `SinkOnlyProgram`, one of
   PROTO-2's four "pure facts"). Rebuild; `check-json` / `check-sheets` / `check-real-typed`.
3. **Faithful relocation.** Replaced the crude predicate with `has_value_dispatch()` = a sink-only
   port of `derive_materialization_roles.value`: a rule whose direct `CallRule` callees cover **≥ 4
   of the six value CATEGORIES** (container `{}` / sequence `[]` / string-regex / number-regex / bool
   `true`+`false` / null). Rebuild; the same three checks.
4. Retirement grep: `has_rule("value")` count on the emit path.

Every `check-*` regenerates the runtime from the (rebuilt) codegen and byte-compares against the
committed files — so the check IS the byte-identity gate; a mis-routed grammar turns it RED.

---

## THE NUMBERS

### Byte-identity floor (clean tree)

| check | exit |
|---|---|
| `check-json`   | **0** |
| `check-sheets` | **0** |

Floor holds (loadavg 4.23).

### Step 2 — CRUDE relocation `dispatch_alt_count == 0`: **REFUTED (measured)**

| check | exit | drift |
|---|---|---|
| `check-json`       | 0 | none |
| `check-sheets`     | **1** | **`Error: lowering failed: sink-only renderer missing Number RegexProgram in BIR`** |
| `check-real-typed` | 0 | none |

**Sheets carries dispatch alts too** (`dispatch_alt_count > 0`), so the crude predicate returns TRUE
for Sheets, mis-routing it into `json_sink_direct`, which then aborts (it requires a Number
`RegexProgram` that Sheets' operator tower has not). **PROTO-2's named surface — "the program carries
a value-dispatch Alt" / `dispatch_alt_count` — is INSUFFICIENT.** The name-gate does NOT retire into
any datum already on `SinkOnlyProgram`; "has *a* dispatch Alt" ≠ "is the JSON value tower."

### Step 3 — FAITHFUL relocation (≥4 value-category coverage = `roles.value`): **byte-identical GREEN**

| check | exit | drift |
|---|---|---|
| `check-json`       | **0** | none |
| `check-sheets`     | **0** | none |
| `check-real-typed` | **0** | none |

3/3 byte-identical (loadavg 2.52). JSON's value rule covers all 6 categories (≥4 ✓ → `json_sink_direct`);
Sheets covers < 4 (→ `general_descent`) — **exactly the routing the retired name-gate produced.** The
discriminator that works is the *specific* value-role census, i.e. `derive_materialization_roles.value`'s
`≥ 4 value-target refs`, ported to the sink-only surface — structural, never the rule NAME "value".

### Retirement grep (emit path)

`grep -c 'has_rule("value")' runtime_generator.rs` = **0** (retired). `has_rule` stays live via the
legitimate dynamic `has_rule(&program.entry_rule)` lookups. The sole remaining literal-name `has_rule`
in codegen is a **test assertion** (`lib.rs:1403` `has_rule("object") || has_rule("array")`) — a
test-guard, not the emit-path gate; carried, not in scope.

### Footprint

`git diff --stat` = **2 files, +109 / −6** in `crates/codegen/` only (LOCK-3-local): the
`runtime_generator.rs:98` dispatch line + comment, and `has_value_dispatch` + its category-census
helpers on `SinkOnlyProgram`. **Zero generated files touched** (byte-identity is the whole point).

---

## VERDICT

**RAN_MEASURED.** The `has_rule("value")` emit-path name-gate is **EXECUTED-RETIRED** into a structural
discriminator with **regen×check × {json, sheets, real-typed} byte-identical** — the first *run* code
on the 3B fold path (PROTO-2 was trace-only). G-3B-2 is discharged by execution, not disposition, for
the emit-dispatch seam: **the name is gone; the routing is byte-preserved.**

**Two measured refinements to the pass-2 disposition:**

1. **PROTO-2's named surface is REFUTED (measured).** `dispatch_alt_count` / "carries a value-dispatch
   Alt" is INSUFFICIENT — Sheets also carries `Alt{Dispatch}`, and the crude predicate turns
   `check-sheets` RED (`missing Number RegexProgram`). The honest discriminator is the *specific*
   value-role census (**≥ 4 of 6 value categories = `roles.value`**), not any dispatch count. This
   tightens G-3B-2: the retirement is a real value-role attribute, not the coarser fact PROTO-2 reached
   for.

2. **Home / DRY caveat (honest scope).** This probe ports `derive_materialization_roles.value` into
   codegen to prove executability LOCK-3-locally. That duplicates passes-side logic (a DRY /
   system-cohesion smell). The *production* home is threading the passes-computed `roles.value.is_some()`
   onto `SinkOnlyProgram` (the `LayoutFacts → Attributes` promotion of PROTO-2's edit-map step 1/4), so
   the census is computed **once** in the annotate pass and the fold reads it by lookup. The probe
   proves the retirement is byte-identical and structural; the single-home wiring is the G-3B-1 fold's
   obligation. G-3B-2 remains, correctly, a value-role *attribute* addition (born-RED / carried), NOT a
   free rename — now with the exact predicate pinned by a green run.

The killer gate **G-3B-1** (execute the *whole* `emit(&GrammarIr,&Attributes)` fold and re-run
regen×check) remains beyond a 50-min probe; this probe lands one real, green, byte-identical *slice* of
it (the emit-dispatch fork) and corrects the discriminator PROTO-2 would have used.

---

## PATCH / DIFF

`PROTO-1-t1.patch` (co-delivered): `crates/codegen/src/runtime_generator.rs` (+8/−6) and
`crates/codegen/src/lower/sink_only.rs` (+101) — the emit-dispatch retirement and the
`has_value_dispatch` value-category census. Core change at `runtime_generator.rs:98`:

```rust
// was: if !sink_only.has_rule("value") {           // NAME-gate
if !sink_only.has_value_dispatch() {                 // STRUCTURAL value-role census (>=4 categories)
    return emit_general_descent(profile_id, sink_only);
}
```
