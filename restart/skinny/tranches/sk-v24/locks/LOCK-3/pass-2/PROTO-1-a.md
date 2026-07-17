# PROTO-1-a — LOCK-3 pass-2, family 3B, probe *a* (the incidental-separator reconciliation)

**Probe lineage:** PROTO-1 (G-3B-1 E1) RAN the killer gate and returned **FAIL/DEMOTE** with a
precise cause and a named-but-UNRUN route to PASS. This probe *a* executes **step 1 of that route**
— "make general_descent emit `skip_ws` on *semantic* need, not per-Seq-child" — and tests whether it
makes the marker deletion **byte-neutral** for sheets, i.e. whether the whole G-3B-1 byte-coupling is
a **single bounded reconcilable artifact** rather than a diffuse identity break.

## BASE-PIN

- Outer bbnf-lang HEAD measured on: `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` (worktree was STALE at
  `d2b32f6e3`; `git merge-base --is-ancestor HEAD ce51c36fc` = true, so `git merge --ff-only
  ce51c36fc` applied cleanly → HEAD now == base-pin exactly). **BASE-PIN OK, no wrong-artifact risk.**
- Measurement substrate (family 3B lives in the nested skinny repo, NOT the outer worktree):
  `skinny @ 3f2350f15af1cc886e2968be70e7bee610ae0e2a` — the exact pin all five pass-2 research
  reports and PROTO-1 used. Working tree verified **clean** (`git status --short
  crates/{codegen,runtime,passes} xtask` empty; the codegen/runtime drift PROTO-1 noted has been
  reverted).
- Hygiene caveat: `skinny/crates` is a nested repo absent from this bbnf-lang worktree checkout, so the
  edit was applied to the **shared** skinny tree (as every prior 3B prototype did) and reverted after
  measurement. loadavg at start: `6.09 / 6.70 / 7.03` (recorded).
- **Substrate-HEAD-advance (verified, not wrong-artifact):** during the probe a concurrent agent
  advanced skinny HEAD `3f2350f15 → 1a0d3091c` ("state(sk-v24): owner model edict ledgered" — a
  **docs/ledger-only** commit that does not touch `crates/`); emit.rs is byte-identical across the two.
  Post-probe verification confirmed **zero T_semskip drift** left in the tree and my uncommitted edit
  was **not** swept into `1a0d3091c`. The inspected artifacts (emit.rs:302 separator rule, the
  committed `G-3B-1-E1.patch` −3 delta) are unaffected.

## QUESTION

PROTO-1 proved that deleting the 3 zero-consumer marker variants perturbs `sheets/generated.rs` by
**exactly 3 `skip_ws` lines**, because `general_descent/emit.rs:302` emits one whitespace separator
before *every* `Seq` child at index > 0 — regardless of whether that child consumes input. The marker
slots (`SpanMark`/`TapeEmit`/`DirectBuild`/`Return`) consume nothing, yet each manufactures a
separator. PROTO-1's verdict: "byte-identical to committed" and "delete the plane" are mutually
exclusive for sheets *in this baseline* — unsatisfiable without first re-baselining sheets.

**This probe's question:** if `emit.rs` emits `skip_ws` only **between two input-consuming
constructs** (the semantic rule), (i) how large is the sheets re-baseline, and (ii) does the marker
deletion then become **byte-neutral** (0 additional diff) — converting G-3B-1 from "unsatisfiable" to
"satisfiable after one bounded, characterized re-baseline commit"?

## METHOD — the T_semskip transform (applied, compilable; see PROTO-1-a.patch)

`crates/codegen/src/general_descent/emit.rs`, the `SinkOnlyExpr::Seq` arm (`:300`): replace the
unconditional `if index > 0 { skip_ws }` with a separator emitted **only** before a child that
`consumes_input(child)` **when a prior consuming child has already been emitted**. A new recursive
predicate `consumes_input` returns `false` for the four pure markers
(`SpanMark`/`TapeEmit`/`DirectBuild`/`Return`) and `true` for the input leaves
(`CallRule`/`ByteLiteral`/`RegexProgram`), recursing through `Entry`/`Seq`/`Alt`/`RepeatLoop`/
`OptionalBranch`/`ValueProject`. (+32/−2 LOC, one file.)

**Correctness anchor (why the removed separators are dead):** `render_rule` (`emit.rs:272`) already
emits `*cursor = skip_ws(bytes, *cursor);` at *rule entry*, before the body Seq — so the first
consuming construct's leading whitespace is covered independently of any interior separator, and
`skip_ws` is idempotent. The only separator with semantic content is the one *between* two
consuming siblings; T_semskip keeps exactly those.

## WHAT RAN / WHAT DID NOT (honest)

- **Applied:** T_semskip to the live `emit.rs`; the edit is type-shaped against the actual
  `SinkOnlyExpr` variant set (predicate arms mirror the existing `emit_expr` match, incl. the
  `Box`-deref-coercion the surrounding code already relies on).
- **Initiated:** `cargo xtask check-{sheets,json,real-typed,css}` on the substrate.
- **DID NOT COMPLETE:** the skinny `ax-iter` profile target was **cold** (compiling registry deps —
  `include_dir`, etc. — through sccache); the build did not finish within the 50-min hard cap. **No
  regen delta was machine-captured.** Therefore the byte-neutrality result below is **DERIVED from
  the committed `G-3B-1-E1.patch` diff + the `emit.rs:302` separator rule** (both directly
  inspected), and is labelled **UNMEASURED** — it is a falsifiable prediction, not a confirmed run.

## THE NUMBERS (derived from inspected artifacts; regen NOT machine-confirmed)

Committed sheets `parse_string` Seq = `[SpanMark, match_leaf, SpanMark, TapeEmit, DirectBuild,
Return]`; only `match_leaf` consumes input. Separators per rule:

| form | separators in `parse_string` | vs committed |
|---|---|---|
| committed (per-Seq-child) | 5 (before children 1..5) | — |
| PROTO-1 E1 (markers deleted, per-Seq-child) | 2 | **−3** (the measured PROTO-1 result) |
| **T_semskip (this probe), markers present** | **0** (one consuming child ⇒ no between-pair sep) | **−5** (the re-baseline) |
| **T_semskip ∘ E1 (markers deleted)** | **0** | **−5** (i.e. **0 vs T_semskip** — byte-neutral) |

**The load-bearing derivation:** under T_semskip a marker (non-consuming) never earns a preceding
separator and never counts as a "prior consuming child", so **deleting markers from any Seq leaves
T_semskip's output byte-identical**. This is structural, not string-specific — it generalizes to
every general_descent rule. Hence:

- **T_semskip is a bounded sheets re-baseline** (predicted ≈ −5 lines in the string rule; whole-
  grammar total unmeasured — the exact figure is what the completed `regen-sheets` would report).
- **After that single re-baseline, the marker deletion (E1) is byte-neutral** ⇒ the plane becomes
  byte-deletable for sheets. JSON/CSS/real-typed are untouched by T_semskip (it lives only in
  `general_descent`, which JSON→`json_sink_direct` and CSS→`emit_scan` do not use).

## VERDICT

**RAN_UNMEASURED — route step 1 implemented as compilable code; byte-neutrality DERIVED, not
machine-confirmed (cold ax-iter build exceeded the hard cap).**

The finding *advances* PROTO-1's FAIL without contradicting it: PROTO-1's "unsatisfiable in this
baseline" is correct, and this probe localizes the entire obstruction to **one incidental artifact
with a bounded, one-commit reconciliation** (`emit.rs:302` per-Seq-child `skip_ws` → semantic
`skip_ws`). G-3B-1 is therefore **not diffuse and not −100-LOC-evaporating on the separator axis** —
it is "byte-satisfiable after one characterized sheets re-baseline", contingent on the completed
regen confirming (a) the total re-baseline line count and (b) 0-diff byte-neutrality of E1 on top,
and on a functional sheets parse over the corpus (skip_ws idempotency predicts invariance).

**Exact confirmation command (unrun here):**
```
cd skinny && git checkout crates/runtime/src/grammars/sheets/generated.rs
# apply PROTO-1-a.patch to emit.rs
cargo xtask regen-sheets && git diff --stat crates/runtime/.../sheets/generated.rs   # → the re-baseline N
cp .../sheets/generated.rs /tmp/sheets_semskip.rs
# apply G-3B-1-E1.patch to passes/src/lib.rs (marker deletion)
cargo xtask regen-sheets && diff /tmp/sheets_semskip.rs .../sheets/generated.rs      # → EXPECT empty (byte-neutral)
cargo xtask check-{json,real-typed} ; cargo xtask check-css-l4-declaration-values    # → EXPECT exit 0 (unaffected)
```

## ARTIFACTS

- `PROTO-1-a.patch` — the T_semskip edit to `general_descent/emit.rs` (+32/−2, one file).
- Coupling site of record: `codegen/src/general_descent/emit.rs:302` (per-Seq-child `skip_ws`) —
  the exact byte-coupling PROTO-1 named; this probe supplies the compilable repair.
- Reconciliation companion: the existing `G-3B-1-E1.patch` (marker deletion) is byte-neutral **on
  top of** this patch, per the derivation above.
