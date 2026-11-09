# LOCK-7 · Pass-2 · PROTO-2 — 7C-T2

**Probe:** 7C-T2 — delete the `has_parse_time_host_decode` string heuristic, regen the fleet bit-for-bit.
**Family:** 7C (HOST-FUNCTION-LATTICE — the survivor).
**BASE-PIN:** measured on HEAD `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` (== pinned base `ce51c36fc`; `git merge-base --is-ancestor ce51c36fc HEAD` → YES, not stale). No wrong-artifact risk.
**Verdict:** **RAN — PASS.** The string name-gate is NOT load-bearing; 7C's "one-file cure, ships now" is a RAN result.

---

## QUESTION

Is the `has_parse_time_host_decode` substring heuristic (`passes/src/lib.rs:794`, firing on
`name.contains("host") && (value.contains("decode") || value.contains("parse"))`) load-bearing?
Does replacing it with a structural predicate leave every committed grammar's generated `.rs`
byte-identical — making 7C's one-file cure a RAN result rather than SPEC_ONLY?

This was the cheapest signal in the lock and had never run across two passes (highest-ROI missing probe).

## METHOD

1. Base-pin verified (above); loadavg recorded at every run.
2. Isolated `CARGO_TARGET_DIR` inside the scratchpad (no cross-agent lock contention; parse-that untouched; no x86).
3. **Baseline (control):** ran all four `check-*` targets on the unmodified `ce51c36fc` tree to prove
   the committed generated trees are not already stale.
4. **Change:** in `passes/src/lib.rs` replaced the `ExprKind::Annotation` arm of
   `has_parse_time_host_decode` — deleting the `name`/`value` string sniff — with a **shape-derived
   predicate** over the built node: `ExprKind::Annotation { value, .. } => value.is_some()`.
   Rationale: the IR has no `ExprKind::Host`/registry node yet; the only annotation the grammar parser
   ever constructs is the `?w` whitespace marker, which is structurally `{ name: "ws", value: None }`
   (`grammar/src/lib.rs:1371`). A parse-time host directive is distinguished structurally by carrying a
   payload in the `value` slot. The predicate inspects that slot's presence instead of sniffing string
   contents. Both consumers left untouched: `:620 ShapeRationale::HostFnParseTime` and
   `:764 requires_eager_tape` (now at shifted line numbers after the edit, same call sites).
5. **Re-check:** ran all four `check-*` targets with the modified code. Each is a byte-identical
   assertion — `codegen::EmittedSource::check_dir` (`codegen/src/lib.rs:74`) compares emitted-in-memory
   bytes against the committed `.rs` via `actual != *source`; no writes, no self-comparison.
6. **Freshness proof:** injected a syntax error into the edited arm and confirmed `cargo build -p xtask`
   FAILS at that line — proving every run recompiles `passes` from source (no stale-binary confound).
7. **Negative control:** inverted the predicate to `value.is_none()` (fires `true` on every `?w`
   marker) and re-ran the fleet.
8. **Mechanism probe:** temporary `eprintln` in `backend_candidates` recording, per rule,
   `backend_rule.is_some()`, `has_parse_time_host_decode(...)`, `eager_forced`. Reverted after reading.

Commands (per xtask/src/main.rs & regen_css.rs): `cargo run -p xtask -- {check-json, check-sheets,
check-real-typed, check-css-l4-declaration-values}`. The `check-*` (non-destructive) variant was used
for CSS rather than `regen-css` so the assertion is against the *committed* tree, not a self-overwrite.

## WHAT RAN — THE NUMBERS

Corpora: committed generated trees for json / sheets / css_l4 / real-typed.

| Run | Predicate | check-json | check-sheets | check-real-typed | check-css-l4-decl-values | loadavg |
|-----|-----------|-----------|--------------|------------------|--------------------------|---------|
| Baseline (control) | original string sniff | PASS (0) | PASS (0) | PASS (0) | PASS (0) | 5.51 4.05 4.31 |
| **Change** | `value.is_some()` (structural) | **PASS (0)** | **PASS (0)** | **PASS (0)** | **PASS (0)** | 4.76 4.00 4.29 |
| Negative control | `value.is_none()` (inverted) | PASS (0) | PASS (0) | PASS (0) | PASS (0) | 3.94 3.91 4.24 |
| Final clean re-verify | `value.is_some()` | PASS (0) | PASS (0) | PASS (0) | PASS (0) | 3.39 3.75 4.11 |

- CSS `generated.rs`-tree emission hash constant across ALL runs: `886e55b80e0dd269868019808c884ca7ad35c4760899ed550dd57883bd87a5d8`.
- Freshness proof: injected `FRESHNESS_PROBE_SYNTAX_ERROR` → `error: could not compile 'passes' (lib)` at `lib.rs:803`. Binary provably reflects source.
- `cargo build -p passes` clean (exit 0, no warnings) with the final structural predicate.

**Structural facts (from the parser + mechanism probe):**
- The grammar parser constructs exactly ONE `ExprKind::Annotation` kind: the `?w` marker,
  `{ name: "ws", value: None }` (`grammar/src/lib.rs:1371`). No host annotation is ever built.
  Therefore the original heuristic — which *requires* `name.contains("host")` — can **never fire**
  on any grammar through this parser (`"ws".contains("host")` is false).
- `?w` annotation-node counts in the committed fleet: json **0**, sheets **13**, css_l4 **~92** across
  9 source files. So sheets/css DO carry annotation nodes — the predicate is genuinely reached on them.
- Mechanism probe (correct predicate): `host_decode=false` for **all 39 sheets rules and all 15 json
  rules**; `backend_rule` is `Some` for every rule (predicate is consulted, not short-circuited). The 5
  eager-forced sheets rules (range_end, primary, error_literal, compare_op, cell_or_range) derive
  `eager_forced=true` from the OTHER `requires_eager_tape` disjuncts (recovery / layout / dispatch
  overlap), NOT from host_decode.

## INTERPRETATION

Three independent lines of evidence converge:

1. **Constant-false agreement.** The string heuristic returns false on the entire committed fleet
   (no host annotation exists). The structural predicate `value.is_some()` also returns false on the
   entire fleet (every annotation is `?w` → `value: None`). The two predicates are *pointwise identical*
   across every rule of every committed grammar — so the emitted bytes cannot differ. Confirmed: all
   four targets byte-identical.

2. **Robustness (negative control).** Even the *inverted* predicate, which forces `host_decode=true`
   on every `?w`-bearing rule in sheets/css, leaves all four targets byte-identical. The committed
   fleet's generated `.rs` does not flow from this predicate at all — the eager-tape selection for
   annotation-bearing rules is fixed by the other disjuncts. The deletion is therefore doubly safe.

3. **No stale-artifact confound.** Freshness proof + base-pin at the exact pinned HEAD.

The heuristic is a latent, dead name-gate. Deleting the string sniff and keying on node shape is a
strictly-neutral, one-file change. 7C's "one-file cure, ships now" is proven RAN, not SPEC_ONLY.

## PASS / FAIL

**PASS** — ALL `check-*` targets pass bit-for-bit against the committed `.rs` with the string
name-gate deleted. The `has_parse_time_host_decode` substring heuristic is **NOT load-bearing** and is
deletable; the structural predicate reproduces the heuristic's decision (constant-false) exactly on
every committed grammar. No grammar's eager-tape selection shifted.

## FILES

- Patch: `7C-T2.patch` (single file, `skinny/crates/passes/src/lib.rs`, +8 / −4).
- Instrumentation (eprintln mechanism probe, freshness syntax error) was temporary and fully reverted;
  it is not in the patch.
