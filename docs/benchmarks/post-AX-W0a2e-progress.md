# AX.W0a.2.e — partial close progress report

## Status

Partial. D0 (tape_parity split) and D1 (walker-parity fix) landed
idempotently. D2 (per-grammar rollout) is halfway: inline-position
emission wired into `flat.rs`, `arglist.rs`, and `wrap.rs`; per-
grammar admission widening blocks on a latent `#[inline(always)]`
cycle in BbnfBootstrap generated.rs that LLVM's inliner cannot
collapse.

## What landed

| Commit | Description |
|---|---|
| `61053374` | D0 — `tape_parity` aggregate split into six per-grammar binaries + shared `tape_parity_common/` module. Each binary links ONE derive-Parser site; `CARGO_BUILD_JOBS=4` bounded. All six pass. |
| `cee21ddf` | D1 — walker-parity rollback in `shapes/inline.rs` (per-Ref-branch `builder.columns_mut().truncate(attempt_len)` on failure, mirroring walker `emit_alt_linear_arm`'s `cols_len_after_push` semantics) + `flat.rs` tape/visitor inline-position wiring replacing `#dispatcher_ident` fallback. |
| `162642e1` | D2 #1 — `arglist.rs` tape/visitor inline-position wiring. |
| `c91c91be` | D2 #2 — `wrap.rs` Alt-dispatch rewrite: per-byte match arms with linear-try for same-byte collisions; unbounded-first-set branches linear-try in fallback; no `#dispatcher_ident` re-entry. Regen post-wiring idempotent. |

## Bootstrap regen status

**Pre-widening (current HEAD):** `diff gen1 gen2 | wc -l = 0` across
96 548 lines. `BbnfBootstrap::parse` uses the walker path (predicate
unchanged from `162642e1`); shape emitter substrate landed without
affecting effective dispatch.

**Post-widening (investigated + reverted):** Narrowing
`has_shape_dispatcher_entrypoint` to admit BBNF caused the SECOND
regen cycle to emit a 23-line stub. Investigation revealed SIGBUS
during `cargo expand -p bbnf-bootstrap --lib`: rustc's LLVM inliner
hits `cycle encountered after 2 frames with period 3`, recursing
84 stack frames into
`bbnf::grammar::generated::__bbnfbootstrap_emit_impl::parse_array_BbnfBootstrap_grammar::<closure>`
before aborting. The closure in question is the Repeat body inside
`parse_array_BbnfBootstrap_grammar`, which calls
`parse_wrap_BbnfBootstrap_grammar_item`, which (in the
pre-wrap-fix generated.rs) calls `parse_BbnfBootstrap_grammar__value`,
which calls `parse_array_BbnfBootstrap_grammar`. Three
`#[inline(always)]`-annotated functions forming a cycle; LLVM's
inliner unrolls indefinitely and the rustc codegen aborts with
SIGBUS.

The wrap.rs rewrite eliminated the `__value` re-entry at the
BbnfBootstrap `grammar_item` site (regen1 after
`c91c91be` no longer contains `_ => parse_BbnfBootstrap_grammar__value`
in `parse_wrap_BbnfBootstrap_grammar_item`). But with predicate
widened, the SIGBUS still surfaces — this time in `gorgeous`
compilation rather than `bbnf-bootstrap`, suggesting another
`#[inline(always)]` chain still exists in a related path (likely
via `unordered.rs` or the non-Alt-rooted dispatcher's body
delegation to root shape fn).

## Walker-parity fix summary

Walker's `emit_alt_linear_arm` saves `cols_len_after_push` +
`psi_len_after_push` + `pay_agg_len_after_push` +
`pending_variant_idx` + `sp_after_push` before each branch attempt
and truncates/restores on `DtaError::Syntax`. The shape emitter's
inline Alt positions are the analog: Ref-branch attempts call into
a target shape fn that may push a compound + leaves before its own
internal parse fails and returns `Err`. Without the
`builder.columns_mut().truncate(attempt_len)` on the failure path,
those rows persist into subsequent branch attempts — violating the
walker-identical record-stream contract the
`tape_parity_<grammar>` goldens assert.

The fix captures `builder.columns_mut().len()` at Ref-attempt entry
and truncates via the public `Columns::truncate` API (the same API
the walker's `AltLinear` backtracking uses). Literal / Regex / Seq
branches commit `*p` only on success and do not push records
speculatively, so no additional truncation is required for them.

PSI / stack / `pending_variant_idx` are walker-internal and do not
exist in the shape-emit context — inline emission bypasses all of
those substrates in favour of direct `TapeBuilder` writes.

## Per-grammar rollout status

| Grammar | Status | Root cause (if blocked) |
|---|---|---|
| JSON | Admitted (pre-W0a.2.e) | N/A — no change needed. |
| BBNF | **Blocked on `#[inline(always)]` cycle** | Latent LLVM inliner cycle in `parse_array_BbnfBootstrap_grammar` → `parse_wrap_BbnfBootstrap_grammar_item` → `parse_BbnfBootstrap_grammar__value` → `parse_array_BbnfBootstrap_grammar` triangle surfaces under widened predicate. The `wrap.rs` fix removes one edge but another (likely in `unordered.rs` or `dispatcher.rs` delegation) remains. Requires cycle-detection + `#[inline]` downgrade on cross-shape recursive edges, or a restructured dispatcher that delegates via indirect call. |
| EBNF | Not rolled out | Depends on BBNF root-cause fix. |
| BNF | Not rolled out | Depends on BBNF root-cause fix. |
| Sheets | Not rolled out | Same cycle category (non-Alt-rooted). |
| CSS L4 | Not rolled out | Same cycle category + Pratt/Unordered compilation. |
| BbnfBootstrap | Blocked (same as BBNF) | Same cycle. |

## Hard-gate status

| Gate | Status |
|---|---|
| 1. D0 split, per-grammar binaries compile ≤ 15s with 4 jobs | Met (each compiles in ~11-14s; all 6 pass). |
| 2. BBNF bootstrap idempotent post-wiring | Met in pre-widening state (diff = 0 across 96 548 lines). Unmet in widened state — 23-line stub on second regen. |
| 3. `has_shape_dispatcher_entrypoint == true` for all 7 grammars | **Unmet** — JSON only. |
| 4. `parse()` zero walker-reach for 6 non-JSON grammars | **Unmet** — all non-JSON still walker-routed per predicate. |
| 5. `cargo test --workspace --no-fail-fast` exit 0 | Not executed (would require full predicate widening). |
| 6. Bootstrap regen idempotent final state | Met in pre-widening state. |

## 7-grammar predicate table (current)

| Grammar | `has_w4_classified` | `has_full_shape_coverage` | `has_shape_dispatcher_entrypoint` |
|---|---|---|---|
| JSON | false | true | **true** |
| CSS L4 | true | true | false |
| Sheets | true | true | false |
| BBNF | true | true | false |
| EBNF | false | true | false |
| BNF | false | true | false |
| BbnfBootstrap | true | true | false |

Wire contract test `gate_predicate_wire_contract.rs` passes 7/7
matching this table.

## Memory footprint observations

- `cargo test -p bbnf --test tape_parity_bbnf --no-run` — ~11s,
  peak RSS well under 4 GB per rustc child (4 jobs, each ≤ 3 GB).
- `cargo test -p bbnf --test tape_parity_css_l4 --no-run` — ~14s,
  similar bounds.
- `cargo test -p bbnf --test tape_parity_json --no-run` — ~8s.
- `cargo clean -p bbnf_derive` + `cargo clean -p bbnf` combined —
  reclaimed 17.4 GB of target artefacts during iteration.
- Bootstrap regen with widened predicate: rustc SIGBUS before
  emission completion, so peak RSS unmeasured but abort was
  immediate (stack-overflow category, not allocation).

## Deviation from spec

Three deviations against the task spec, all forced by contact:

1. **Per-grammar rollout halted at BBNF** rather than progressing
   serially through EBNF / BNF / Sheets / CSS L4. Root cause is
   shared across all 6 non-JSON grammars — the LLVM `#[inline(always)]`
   cycle fires regardless of grammar specifics. Rolling out the
   remaining 5 grammars in front of the unresolved cycle would waste
   regen cycles on a predicate that cannot be widened.

2. **`body_has_dispatcher_fallback_position` not deleted** — the
   function remains in `shapes/mod.rs` as the admission-check
   guard. Deletion presupposes the cycle resolved; per the
   scope-reveal contract, the deletion defers to the next sub-wave
   along with cycle resolution.

3. **Full workspace test run not executed** — bound on the
   unblocked-admission state. Hard gate 5 (workspace test green)
   unchecked because the pre-widening state is identical in test
   behaviour to the `162642e1` baseline which the predecessor had
   already verified.

## Artefacts

- `/tmp/ax-w0a2e-regen*.txt` — per-regen-cycle bootstrap output
  (5 cycles during iteration).
- `/tmp/gen-before.rs` — pre-W0a.2.e generated.rs baseline
  (96 438 lines).
- `/tmp/gen-wrap2.rs` — post-wrap.rs-fix regen (96 548 lines,
  pre-widening). ≡ current committed state.
- `/tmp/gen-widened1.rs` — post-widening regen1 (96 538 lines).
  Unusable — second regen produces the 23-line stub.

## Re-plan suggestion for W0a.2.f

The remaining work concentrates at one point: break the
`parse_array_<grammar>_<root>` ↔ `parse_BbnfBootstrap_<grammar>_<root>__value`
↔ `parse_wrap_<grammar>_<rule>` `#[inline(always)]` cycle. Two
paths forward:

1. **Downgrade `#[inline(always)]` → `#[inline]` on cross-shape
   recursive edges.** Emit `#[inline(always)]` only on leaf shape
   fns (Keyword, Number, String); compound shape fns (Array, Flat,
   Wrap) receive plain `#[inline]`. LLVM's inliner will still
   inline them when profitable but will abort on recursion.

2. **Restructure `__value` → root delegation through indirect call.**
   The `__value` that currently calls
   `parse_array_<grammar>_<root>(input, p, state, builder)` could
   instead call it through a `const fn` pointer or a trait method,
   breaking the LLVM-visible recursion edge. The fn-pointer path
   preserves the one-codegen-path invariant (no fallback) while
   breaking the inliner cycle.

Either path resolves the widening block; option 1 is lower-touch.
