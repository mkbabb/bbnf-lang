# LOCK-7 pass-2 · PROTO-1 (probe UF-2) — the 7C TOTALITY% COUNTER — INDEPENDENT RE-RUN

**Family:** 7C HOST-FUNCTION-LATTICE (the survivor) — feeds 7D's promotion-ledger `totality_pct` column.
**Base-pin:** HEAD `c93fc43143743ffaf54855c526f6aeab79227352` — `git merge-base --is-ancestor ce51c36fc HEAD` ⇒ **ANCESTOR-OK** (descends from the pinned base; HEAD's `lib.rs` blob = `e1d0f806a`, exactly the patch's base index, so the patch applied clean-on-base). Numbers are **NOT** wrong-artifact-risk.
**Loadavg at probe:** 10.26 → 5.93 across the run (18 logical cores). Probe is functional, not perf; wall: grammar crate compiled in 0.46s, test body 0.00s.
**Verdict line:** **PASS — RAN_MEASURED.** The 7C flagship metric runs purely build-side over the BUILT `GrammarIr`; its floor is exercisable (a real sub-1.0 reading under injection). NOT vacuous, NOT SPEC-ONLY, NOT blocked behind the leaf evaluator.
**Relation to prior record:** this is an **independent reproduction**. `PROTO-1.md` (prior run, same HEAD) is corroborated **byte-for-byte** — I reset the grammar crate to pristine HEAD, re-applied `UF-2-totality-counter.patch` from scratch, and re-ran; every number matches.

---

## QUESTION

The survivor family's flagship metric (`totality%`) is harness-authored but had never been RUN, and its own note (patch line ~250) warns the real corpus reads `totality% == 1.0` exactly because no genuine Tier-2 host exists yet. **Does the counter produce sensible per-subsystem numbers, and can its floor be exercised (a non-1.0 reading) — proving the metric is not vacuous?**

## METHOD

In a worktree base-pinned at (descending from) `ce51c36fc`, apply `restart/skinny/tranches/sk-v24/locks/LOCK-7/pass-2/UF-2-totality-counter.patch`. The patch adds `skinny/crates/grammar/tests/totality_counter_probe.rs` (259 lines) and the `parse_postfix_ops` `?w`-attach fix in `skinny/crates/grammar/src/lib.rs` (+15/−3).

The test builds three subsystems via `grammar::parse_grammar` over corpora and walks `grammar.exprs` (the `ir::ExprKind` arena). Classification:

- **Tier-0** — pure `->` / `Literal` / `Regex` / `Seq` / `Alt` + structural wrappers (`Optional`/`Repeat`/`Discard`/`Project`) + the `?w` (`ws`) whitespace `Annotation`. Threads no context.
- **Tier-1** — `Ref` (needs `resolve_refs` to bind a target).
- **Tier-2** — a host function threading `&mut Ctx`. **No `ExprKind::Host` variant exists in the arena today**, so no real grammar can reach Tier-2. The synthetic non-vacuity injection models the not-yet-existing host node as an `@host`-named (`HOST_SENTINEL = "host"`) `Annotation` over a plain leaf.

`LEAVES = {Literal, Regex, Project}`; `BRANCHES = {each Alt arm, each Optional, each Repeat}`; `totality% = 1 − Σ(T2)/Σ(all)`.

**Counter dependency surface:** the probe imports only `grammar::parse_grammar` + the `ir` arena — no emitter, no leaf evaluator, no `@{}`, no self-emit. It is a pure build-side walk.

**Command:**
```
CARGO_TARGET_DIR=/tmp/uf2 cargo test --manifest-path skinny/Cargo.toml -p grammar --test totality_counter_probe -- --nocapture
```
(isolated target dir per the single-cargo-per-target law; `skinny/` is its own cargo workspace, hence `--manifest-path`.)

## WHAT RAN

`git apply --check` OK → `git apply` OK on pristine HEAD (blob `e1d0f806a` matches the patch index). Build clean; the test compiled and executed:

```
[BUILD] json 15 rules | sheets 39 rules | bbnf-closure 53 rules (resolve_refs+validate clean)
test totality_counter_probe ... ok
test result: ok. 1 passed; 0 failed
```

- **json** = `skinny/grammars/json.bbnf` — **15 rules** (grep of LHS `name =` heads = 15; the brief's "~10" was a loose estimate — 15 is the true, sensible count for a JSON grammar).
- **sheets** = `grammar/google-sheets/google-sheets.bbnf` — **39 rules** (exact match to target).
- **bbnf import-closure** = `grammar/bbnf/{bbnf,expressions,types}.bbnf` concatenated into one arena — **53 rules** (exact match to target; `resolve_refs` + `validate` clean).

The bundled `?w->` `lib.rs` fix is a parser-frontend precondition for the bbnf-closure only: on pristine base the closure fails to parse at `big_comment = ( "/*" , /[^\*]*/ , "*/" ) ?w -> Span ;` (the committed `parse_seq` orphans the trailing `-> Span` after `?w`). The fix extracts `parse_postfix_ops(expr, start)`, shared by `parse_postfix` and the `?w` handler, so the postfix `->` binds to the `?w` element. The **counter kernel is byte-identical with or without the fix**; the fix only decides whether the closure *parses*.

### The numbers (HEAD `c93fc4314` + bundled `?w->` frontend fix)

| subsystem                       | rules | leaves | branches | T0  | T1 | T2 | total | totality% |
|---------------------------------|------:|-------:|---------:|----:|---:|---:|------:|----------:|
| json                            |    15 |     12 |       12 |  29 | 28 |  0 |    57 | **1.0000** |
| sheets                          |    39 |     76 |       57 | 158 | 59 |  0 |   217 | **1.0000** |
| bbnf-closure                    |    53 |    136 |      129 | 319 | 92 |  0 |   411 | **1.0000** |
| json + host (injected Tier-2)   |    15 |     11 |       12 |  28 | 28 | **1** |    57 | **0.9825** |

Real grammars have **no host node ⇒ T2 = 0 ⇒ totality% = 1.0000 exactly** — the sensible "no host functions promoted yet" reading, not a stuck counter. The synthetic injection overwrites one plain leaf with a Tier-2 `@host` sentinel: **json 1.0000 → 0.9825** (`1 − 1/57 = 0.98245…`), strictly below 1.0. **This is the floor being exercised** — the exact number a declared floor (`totality% must be ≥ 1.0` / "no un-promoted Tier-2 host") would trip on. The counter is live and non-vacuous.

All three real subsystems satisfy the sensibility gate: `total > 0`, `leaves > 0`, `branches > 0`, `totality% ∈ [0,1]`.

## VERDICT

**PASS — RAN_MEASURED.** Both PASS gates met:

- (a) All three subsystems **build** (json 15, sheets 39, bbnf-closure 53 rules) and each census is **sensible** (`total>0`, `leaves>0`, `branches>0`, `totality% ∈ [0,1]`).
- (b) Injecting one synthetic Tier-2 host `Annotation` over a plain leaf drops that subsystem's totality% **strictly below 1.0** (json 1.0000 → 0.9825), tripping a declared floor.

The counter is a build-side metric that RUNS and whose floor is exercisable — 7C's flagship stays a RAN number, **not** demoted to SPEC-ONLY. This report feeds 7D's ledger `totality_pct` column.

## DELIVERED FILES
- `PROTO-1-UF2.md` (this file — independent reproduction; corroborates the prior `PROTO-1.md` byte-for-byte at the same HEAD).
- `UF-2-totality-counter.patch` (already present in this dir; identical to the patch I applied — `git apply --check` OK, applied `lib.rs` diff = +15/−3, test file = 259 lines).
