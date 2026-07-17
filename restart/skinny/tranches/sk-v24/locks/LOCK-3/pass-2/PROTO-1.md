# PROTO-1 — LOCK-3 pass-2, probe G-3B-1 (family 3B: the IR transposition spine)

**Probe:** execute the whole `(GrammarIr, Attributes)` fold — regen check byte-identical.
**Run:** PROTO-2, pass-2. Prior state: UNRUN after two passes (trace PROTO-2 + one slice
PROTO-1-t1, no fold). This is the spine's single load-bearing gate.

## BASE-PIN

- HEAD measured on: `c93fc43143743ffaf54855c526f6aeab79227352`
- `git merge-base --is-ancestor ce51c36fc HEAD` → **ce51c36fc IS ancestor** (base-pin PASS, no
  fast-forward needed).
- loadavg at start: `7.90 10.29 8.91`; at E1 regen: `8.02 10.56 9.65` (cold paired, recorded).
- **Wrong-artifact caveat (not base-pin):** the worktree carried *uncommitted drift I did not
  author* — `codegen/src/lower/sink_only.rs` (+101), `codegen/src/runtime_generator.rs`,
  and a new `grammar/tests/oracle_selfparse_probe.rs`. The committed generated `.rs` were
  self-consistent with this drift (baseline `check-*` all exit 0), so byte-identity is measured
  against a coherent baseline; but the source tree is NOT the clean `ce51c36fc` snapshot. All E1
  numbers below are the *delta of my single source edit* against that baseline, isolated by
  `git checkout HEAD -- <my 2 files>` re-measurement.

## QUESTION

Can the `BackendExpr` materialization plane be deleted and re-expressed as ONE
`emit(&GrammarIr, &Attributes)` fold with all four regen checks byte-identical — and do the two
structural tree-walkers (`json_sink_direct/dispatch.rs`, `general_descent/view.rs`) re-base onto
`GrammarIr` without breaking identity?

## METHOD

The materialization plane is 4 `BackendExpr` variants — `SpanMark`, `TapeEmit`, `DirectBuild`,
`Return` — injected by `materialize_rule` (passes/src/lib.rs:1362) as a fixed 6-element
`Seq([SpanMark(Start), body, SpanMark(End), TapeEmit, DirectBuild, Return])` emit-order template.

Static read of every downstream consumer established the *output-relevance map* of the four:

| variant     | passes (shape select)                 | json_sink_direct (dispatch.rs) | general_descent (emit.rs / view.rs) |
|-------------|---------------------------------------|--------------------------------|-------------------------------------|
| SpanMark    | none                                  | no-op (empty FirstBytes)       | no-op body — **but occupies a Seq slot** |
| Return      | none                                  | no-op                          | no-op body — **but occupies a Seq slot** |
| TapeEmit    | `contains_tape_kind` (passes:677,727) | no-op                          | no-op body — **but occupies a Seq slot** |
| DirectBuild | `contains_direct_build` (passes:955)  | gates `direct_shapes` (mod.rs:133), feeds count in comment (mod.rs:186) | emits `mark(tape,…)` (emit.rs:388) |

Key structural fact (emit.rs:301–304): the general_descent recognizer emits one
`*cursor = skip_ws(bytes, *cursor);` **separator before every `Seq` child at index > 0**. The
marker variants emit no code of their own, but each is a `Seq` child — so each manufactures one
`skip_ws` separator line in the generated recognizer.

**Probe E1 (the smallest deletion that isolates the byte-coupling):** drop the two *zero-consumer*
decoration variants (`SpanMark` start/end) plus `Return` from the emit-order template — keeping
`TapeEmit` + `DirectBuild` (the shape-channel the plane truly homes). Then run the full contract:
`regen-{json,sheets,css,real-typed}` → `git diff --stat` on generated `.rs` → `check-*` ×4 →
`bbnf-bench` parity/strict_product.

## WHAT RAN (measured, not spec)

1. **Baseline** (committed tree): `check-json`, `check-sheets`,
   `check-css-l4-declaration-values`, `check-real-typed` — **all exit 0** (generated `.rs`
   byte-identical to committed). `strict_product` witness test `ok`.
2. **E1** applied to `materialize_rule`; all four `regen-*` ran; `git diff --stat` captured;
   all four `check-*` ran; `bbnf-bench` full suite ran; then my 2 files reverted to HEAD and the
   suite re-run for attribution.

## THE NUMBERS

E1 `git diff` on generated `.rs` (baseline → E1):

| target                          | generated delta          |
|---------------------------------|--------------------------|
| json/generated.rs               | **0** (byte-identical)   |
| css_l4_declaration_values       | **0** (byte-identical)   |
| real_typed (DirectBuild module) | **0** (byte-identical)   |
| **sheets/generated.rs**         | **−3 lines** (diverged)  |

The three diverging bytes are all one token — `*cursor = skip_ws(bytes, *cursor);` — removed from
`parse_string` (Seq-child separators for the 3 deleted marker slots). `mark()`, `match_leaf`,
and every semantic statement are UNCHANGED.

- **Attribution:** classified **incidental**. `skip_ws` is idempotent; the removed separators sat
  between markers that consume no input, so consecutive `skip_ws` calls were functionally dead.
  No semantic byte changed.
- **Parity:** `direct_strict_product_reuses_generated_products_without_digest_plane` → **ok in
  BOTH baseline and E1** (strict_product digest unchanged). 7 other `bbnf-bench` tests
  (`report::tests::*`, `css_l4_w8`, `json_w9`) fail **identically with my edit reverted** → they
  are pre-existing worktree/environment failures, **not** caused by E1.
- **net-LOC:** the full transposition (delete all 4 variants + re-express `single_plan` as an
  `emit(&GrammarIr,&Attributes)` fold + re-base the two 318/431-LOC walkers) was **not carried to
  completion** — E1 is the parsimonious probe that isolates the gate. PASS bar (net-LOC ≤ −80,
  dispatch.rs/view.rs re-based) is therefore **not met**.

## VERDICT

**FAIL / DEMOTE 3B to "structurally-motivated, unproven"** under the strict byte-identity gate.

The plane's marker variants are **not byte-inert**. Deleting even the two zero-consumer decoration
variants perturbs the general_descent (sheets) recognizer by 3 `skip_ws` separator lines, because
`emit.rs:302` manufactures one separator per `Seq` child regardless of whether the child emits
code — so the committed `sheets/generated.rs` has *frozen incidental separators that the plane
manufactures as Seq-slot fillers*. A deletion-based transposition therefore cannot reproduce the
committed sheets bytes; the gate as written ("delete + four byte-identical regens") is
**unsatisfiable without first re-baselining sheets** to the cleaner (fewer-`skip_ws`) form.

**Nuance that keeps 3B alive as a structure (not a byte-identity claim):**
- The **spine-critical path is byte-robust.** JSON (`json_sink_direct/dispatch.rs`, the 91.5% hot
  leaf), CSS, and real-typed are **byte-identical** under marker deletion — dispatch.rs already
  treats the plane as no-ops, so it re-bases onto `GrammarIr` cleanly (CLAIM B holds for the JSON
  walker). Only `general_descent` (view.rs's sibling emit.rs) is byte-fragile, and only via the
  incidental Seq-separator rule.
- The plane is a **denormalized copy of descriptor data**: `TapeEmit.kind` and `DirectBuild.shape`
  are already single-homed in `MaterializationPlan.descriptors` (the Attributes bundle). The
  transposition is semantically sound — nothing is lost, `strict_product` is invariant.

**What the gate actually requires to flip to PASS** (route, unrun here):
1. Make general_descent emit `skip_ws` on *semantic* need (between input-consuming children),
   not per-Seq-child — this is itself a generated-byte change to sheets, so it must land as a
   deliberate sheets re-baseline commit, after which deletion becomes byte-neutral.
2. Re-home `contains_tape_kind`/`contains_direct_build` onto `Attributes` (descriptor lookups)
   so shape selection stops sniffing injected markers.
3. Then delete the 4 variants + fold `single_plan` + re-base the two walkers.

Because step 1 mutates a committed generated file, **"byte-identical to committed" and "delete the
plane" are mutually exclusive for sheets in this baseline.** That is the load-bearing finding: the
spine is convergent and semantically deletable, but its byte-identity contract is refuted by an
incidental artifact the plane itself manufactures. 3B stands as *structurally-motivated*; it is
*not* byte-proven.

## ARTIFACTS

- Patch: `G-3B-1-E1.patch` (my source edit + the isolated sheets delta; excludes pre-existing
  worktree drift).
- Coupling site of record: `codegen/src/general_descent/emit.rs:301-304` (Seq-child `skip_ws`
  separator) — the exact byte-coupling that blocks the gate.
