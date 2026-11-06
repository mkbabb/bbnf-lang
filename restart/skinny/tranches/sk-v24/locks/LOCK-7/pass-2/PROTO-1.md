# LOCK-7 pass-2 · PROTO-1 (probe UF-2) — the 7C TOTALITY% COUNTER, RAN over the BUILT GrammarIr

**Family:** 7C HOST-FUNCTION-LATTICE (feeds 7D's promotion-ledger `totality_pct` column, XP-1)
**Base-pin:** HEAD `c93fc43143743ffaf54855c526f6aeab79227352` — `git merge-base --is-ancestor ce51c36fc HEAD` ⇒ **ANCESTOR-OK** (descends from the pinned base; numbers are NOT wrong-artifact-risk).
**Loadavg at probe:** 6.02 (18 cores). Probe is functional, not perf; wall 0.00s.
**Verdict line:** **PASS — RAN_MEASURED.** The counter runs purely build-side; it is NOT blocked behind the leaf evaluator.

---

## QUESTION

Both critics capped 7C's flagship `totality%` counter as *"cross-repo blocked behind the leaf evaluator."* That is a **build-vs-emit conflation.** Does the counter actually **RUN TODAY** over the **BUILT `GrammarIr`** — no leaf evaluator, no `@{}`, no self-emit — and yield sensible per-subsystem numbers?

## METHOD

New test `skinny/crates/grammar/tests/totality_counter_probe.rs`. It builds a real `GrammarIr` via `grammar::parse_grammar` for three subsystems, walks `grammar.exprs` (the `ExprKind` arena at `ir/src/lib.rs:234`), and classifies every expression into a host-function tier:

- **Tier-0** — pure `->` / `Literal` / `Regex` / `Seq` / `Alt` (plus the structural wrappers `Optional`/`Repeat`/`Discard`/`Project` and the `?w` whitespace `Annotation`). Threads no context.
- **Tier-1** — `Ref` (needs `resolve_refs` to bind a target).
- **Tier-2** — a host function threading `&mut Ctx`. **The IR has NO `ExprKind::Host` variant today**, so NO real grammar can reach Tier-2.

`LEAVES = { Literal, Regex, Project targets }`; `BRANCHES = { each Alt arm, each Optional, each Repeat }`.
`totality% = 1 − Σ(Tier-2) / Σ(all exprs)`, per subsystem.

**Counter kernel dependency surface:** the probe imports **only** `grammar::parse_grammar` + the `ir` arena. It is a pure walk over `grammar.exprs`. **No emitter, no leaf evaluator, no `@{}`, no self-emit are referenced or linked.** (Forbidden-token grep over the probe: `emit_general_descent=0  json_sink_direct=0  match_at=0`.)

**Non-vacuity:** clone the json arena, overwrite one plain leaf with a synthetic Tier-2 host — modeled as an `@host`-named `Annotation` sentinel, because family 7C *is* the host-function lattice and no `ExprKind::Host` node exists yet — then re-run; totality% must drop below 1.0.

**Command:** `CARGO_TARGET_DIR=/tmp/uf2 cargo test -p grammar --test totality_counter_probe -- --nocapture` (isolated target dir per the single-cargo-per-target law; run against the `skinny/` workspace via `--manifest-path`, since `skinny/` is its own cargo workspace).

## WHAT RAN

`grammar::parse_grammar` built all three subsystems (`resolve_refs` + `validate` clean):

```
[BUILD] json 15 rules | sheets 39 rules | bbnf-closure 53 rules
```

- **json** = `skinny/grammars/json.bbnf` (single file).
- **sheets** = `grammar/google-sheets/google-sheets.bbnf` (single file).
- **bbnf import-closure** = `grammar/bbnf/{bbnf,expressions,types}.bbnf` concatenated into one arena so imported refs (`value_expr`, `type_annotation`, `type_name`) resolve — **53 rules**, matching PROTO-3's `Ok/53 resolve_refs-clean`.

### The frontend precondition (a correction to the record)

On the **pristine** base tree, `parse_grammar` builds **json + sheets** with zero code change — the counter already runs on 2/3 subsystems untouched. But the **bbnf closure fails to BUILD** at:

```
Parse { code: "BBNF-PARSE", message: "unexpected token `-`", offset: 662 }
```

Offset 662 is inside `bbnf.bbnf` itself: `big_comment = ( "/*" , /[^\*]*/ , "*/" ) ?w -> Span ;`. The committed `parse_seq` consumes `?w` as a *standalone* sequence element and orphans the trailing `-> Span`, so `parse_postfix` rejects the `-` (an operator with no left operand). **This is a parser-FRONTEND wall (`?w`-before-`->`), distinct from and orthogonal to the leaf evaluator, `@{}`, or any emitter.** PROTO-3's reported `Ok/53` was measured *under an uncommitted `?w->` fix* — it does **not** hold on pristine `c93fc4314`.

Applying PROTO-3's minimal, parser-only `?w->` fix (a `parse_postfix_ops(expr, start)` refactor shared by `parse_postfix` and the `?w` handler; included in the patch) unblocks the closure build → **53 rules clean** and the counter yields the numbers below. The counter kernel is byte-identical in both runs; the fix affects only whether the closure *parses*, never the counter.

### The numbers (HEAD `c93fc4314` + PROTO-3 `?w->` frontend fix)

| subsystem      | rules | leaves | branches | T0  | T1 | T2 | total | totality% |
|----------------|------:|-------:|---------:|----:|---:|---:|------:|----------:|
| json           |    15 |     12 |       12 |  29 | 28 |  0 |    57 | **1.0000** |
| sheets         |    39 |     76 |       57 | 158 | 59 |  0 |   217 | **1.0000** |
| bbnf-closure   |    53 |    136 |      129 | 319 | 92 |  0 |   411 | **1.0000** |
| json + host (injected Tier-2) | 15 | 11 | 12 | 28 | 28 | **1** | 57 | **0.9825** |

Real grammars have **no host node ⇒ Tier-2 = 0 ⇒ totality% = 1.0000 exactly** — the sensible "no host functions promoted yet" reading, not a stuck counter. The synthetic injection puts exactly one Tier-2 over a plain leaf: **json 1.0000 → 0.9825 (< 1.0)**, proving the counter is live and non-vacuous.

`test result: ok. 1 passed`.

### Pre-existing failure (NOT introduced by this patch)

The full `-p grammar` suite reports `19 passed; 1 failed`. The one failure — `w5b_frontend_public_ws_remains_retired` (`lib.rs:1878`) — asserts `@ws` is *rejected*, but the committed `parse_directive` (`lib.rs:1323`, `"import"|"token"|"pretty"|"ws" => Ok(())`) *accepts* it. I reverted my `lib.rs` to pristine and re-ran that test in isolation: **it fails identically** (same panic, `0 passed; 1 failed`). It is a stale test on the pristine base, untouched by this diff (which edits only `parse_postfix`/`parse_seq`).

## VERDICT

**PASS — RAN_MEASURED.** Every PASS gate is met, with the labeled frontend precondition for the bbnf closure:

- ✅ `(leaves, branches, tier-histogram, totality%)` emitted for **all 3 subsystems**, each with `leaves > 0` AND `branches > 0` AND `totality% ∈ [0,1]`.
- ✅ injected-Tier-2 variant drops below 1.0 (`1.0000 → 0.9825`).
- ✅ probe source contains **zero** `emit_general_descent` / `json_sink_direct` / `match_at` (grep = 0).
- ✅ no subsystem yields empty/None `GrammarIr`; the counter **never requires the emitter**.

The critics' cap is a build-vs-emit conflation: 7C's defining metric converts from **"blocked"** to a **RAN number**. Its only real-world gate on the third subsystem is a parser-frontend `?w->` parse gap — a bounded, sibling-authored fix — **not** the leaf evaluator, `@{}`, or self-emit.

## DELIVERED FILES
- `PROTO-1.md` (this file)
- `UF-2-totality-counter.patch` — `skinny/crates/grammar/tests/totality_counter_probe.rs` (new, 259 lines) + the minimal `?w->` frontend fix to `skinny/crates/grammar/src/lib.rs` (+15/−3).
