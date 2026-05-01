# AX.W0a.2.f — partial close progress report

## Status

Partial. D1 (inline-attr downgrade) + D2 (array-element structural
emission) landed idempotently with bootstrap regen byte-identical
across two cycles (96 839 lines). The LLVM `#[inline(always)]`
cycle that SIGBUSed the predecessor's BBNF rollout is broken —
compound shape fns now emit plain `#[inline]` and `cargo expand`
completes without SIGBUS.

Admission widening landed briefly (commit `030fb8aa`) then
reverted (commit `7a311a28`) after surfacing two cascading
downstream blockers documented in §Remaining-blockers. Per
the W0a.2.f task spec's escape clause — "if a grammar's rollout
reveals a new architectural blocker, halt with detailed diag"
— the revert preserves test-green master while the follow-on
sub-wave closes the surfaced blockers on disjoint file bounds.

Final state (committed):

- **Workspace test suite**: 1 614 passed, 0 failed.
- **Bootstrap regen**: idempotent across two cycles at 96 839
  lines (diff = 0).
- **Per-grammar tape_parity**: JSON 7/7, BBNF 4/4 (walker-path
  unchanged).
- **Predicate table**: JSON admits shape-dispatch; 6 non-JSON
  grammars remain walker-routed pending W0a.2.g downstream
  wiring.

## What landed

| Commit | Description |
|---|---|
| `8ae3330b` | D1 — every compound shape fn (Object / Array / Flat / Wrap / ArgList / Pratt / Unordered / AltDispatch / Scalar-Ref-delegation / top-level dispatchers + visitor-path analogues) downgraded from `#[inline(always)]` to plain `#[inline]`. Leaf shape fns (Keyword / Number / String / HRegex / literal-Scalar) retain `#[inline(always)]`. `shapes/mod.rs` doc header reflects the split. |
| `a12fbf6f` | D2-probe #1 — `ax_w0a2f_fallback_probe` enumerates per-grammar fallback-position rules and tags each by ShapeTag so native-handling shapes (Wrap / Keyword / AltDispatch / HRegex / Number / String / Scalar) separate from fallback-risk shapes (Flat / ArgList / Array / Pratt / Unordered / Object). |
| `030fb8aa` | D4 — `body_has_dispatcher_fallback_position` deleted; `has_shape_dispatcher_entrypoint` admits every grammar whose classified entry reaches only classified Refs. Wire-contract test + `aw_v_w5_2_per_ref_routing`'s Sheets-rejection test updated to reflect the flip. 7/7 predicate-table green. **Later reverted at `7a311a28` after downstream blockers surfaced.** |
| `9e8d0603` | D2 #2 — `shapes/array.rs::emit_element_position_tape` replaces `extract_element_ref` + `#dispatcher_ident` fallback with structural per-position emission. Alt / Regex / Negate / Minus / TokenDispatch at Repeat-element positions route through `inline::emit_inline_position_tape`; Refs via `emit_ref_call_tape`; Literal via byte-match; Seq / Next / Skip recurse. Eliminates the runtime infinite-recursion observed on BBNF during the admission widening probe. |
| `637e129b` | D2-probe #2 — `ax_w0a2f_extract_probe` dumps each grammar's entry-rule body tree so downstream sub-commits can reason about per-position emission shape at a glance. |
| `e3664d5d` | Docs — `post-AX-W0a2f-progress.md` + `post-AX-W0a2f-predicate-table.md` + `post-AX-W0a2f-expand-bbnf.txt` capture the partial-close state and per-grammar blocker analysis. |
| `7a311a28` | Revert of `030fb8aa` — admission widening rolled back so `cargo test --workspace` stays green while the Keyword Ref-branch + BNF walker-parity blockers resolve in W0a.2.g. |
| `3497d986` | D1 follow-on — `array.rs.expected` / `object.rs.expected` goldens regenerated for the `#[inline(always)] → #[inline]` downgrade; `regen_shape_goldens.rs` `#[ignore]`-gated one-shot helper lands for future regen cycles. |
| `2490dd4f` | Final — bootstrap regen idempotent; 96 839 lines, byte-identical across two cycles. Cumulative 291-line increase vs pre-W0a.2.f entirely doc-comment + `#[inline]` attribute propagation; emitted parse-fn bodies structurally unchanged. |

## Bootstrap regen status

**Final (HEAD = `2490dd4f`):** `diff gen1 gen2 | wc -l = 0`
across 96 839 lines. Two cycles byte-identical under the D1
inline-attr downgrade; the 291-line increase versus W0a.2.e
baseline (96 548 lines) is entirely doc-comment + `#[inline]`
attribute propagation — the emitted parse-fn bodies are
structurally unchanged.

**Mid-rollout probe (under reverted `030fb8aa`):** Cycle 1
produced 96 829 lines without SIGBUS — the `#[inline(always)]`
→ `#[inline]` cycle-break succeeded. Cycle 2 collapsed to a
23-line stub because the new shape-dispatch `BbnfBootstrap::parse`
cannot parse `bbnf.bbnf` itself when the Keyword emitter does
not handle Ref-led Alt branches (see §Remaining-blockers #1
below). The revert at `7a311a28` restored the self-host loop
closure.

## Per-grammar rollout status

| Grammar | Status | Root cause (if blocked) |
|---|---|---|
| JSON | Admitted pre-W0a.2.f | Alt-rooted Criterion 1 — unchanged. `tape_parity_json` 7/7 pass. |
| CSS L4 | Admitted — downstream wiring incomplete | Entry `stylesheet = OW(Repeat(OW(Ref(ruleItem))))`. Admission widens cleanly; walker-parity not re-verified in this sub-wave (goldens may diverge). Requires re-bench + per-rule walker-parity audit. |
| Sheets | Admitted — downstream wiring incomplete | Entry `formula = Seq[Regex("=?"), Ref(comparison_expr)]`. Flat handles the inline Regex via `emit_inline_position_tape` (W0a.2.e). Walker-parity not re-verified. |
| BBNF | Admitted — Keyword emitter gap blocks parse | Entry `grammar = Repeat(OW(Alt[Ref(comment), Ref(big_comment), Ref(directive), Ref(rule)]))`. Array element emission now handles the Alt correctly (per `emit_element_position_tape`), BUT `directive` classifies as Keyword with Ref-led branches (`import_directive | recover_directive | ...`); Keyword emitter's `IrNode::Alt` arm filters to `IrNode::Literal`-only bodies, producing an empty match in `parse_keyword_BBNF_directive` which unconditionally returns `Syntax`. Every attempt to parse a BBNF directive line fails. |
| EBNF | Admitted — walker-parity divergence unverified | Entry `grammar = Repeat(Seq[Regex, Ref(rule), Regex])`. Structural; needs per-rule audit. |
| BNF | Admitted — walker-parity divergence CONFIRMED | `tape_parity_bnf` fails: shape-dispatch emits 40 records + variant=4; walker golden is 34 records + variant=0. Structural mismatch in either variant_idx stamping or Repeat compound shape. Requires walker-parity audit on the Array-list emission. |
| BbnfBootstrap | Admitted — same Keyword gap as BBNF | Same root cause: Keyword-classified `directive` with Ref-led branches. Also blocks the self-host regen (see §Bootstrap regen status). |

## Hard-gate status

| Gate | Status (final HEAD) |
|---|---|
| 1. `cargo expand` completes without SIGBUS post-downgrade | **Met** — verified via `cargo expand -p bbnf --test tape_parity_bbnf > /tmp/expand-bbnf.txt` (91 524 lines, no abort). Artefact: `docs/benchmarks/post-AX-W0a2f-expand-bbnf.txt`. |
| 2. BBNF bootstrap idempotent ≥ 90k lines | **Met** — 96 839 lines, byte-identical across two cycles in the final state. |
| 3. `has_shape_dispatcher_entrypoint == true` for all 7 grammars | **Unmet in final HEAD** (admission widening reverted). The predicate narrowing is architecturally sound but the downstream emitter gaps it surfaced (§Remaining-blockers) must land first. Mid-rollout transient state met this gate but broke gates 5 + 6. |
| 4. `parse()` zero walker-reach for all 6 non-JSON grammars | **Unmet in final HEAD** — same rationale as gate 3. JSON's `parse()` already walker-free (pre-W0a.2.f). |
| 5. `cargo test --workspace --no-fail-fast` exit 0 | **Met** — 1 614 passed, 0 failed. |
| 6. Bootstrap regen idempotent in final state | **Met** — see gate 2. |
| 7. `body_has_dispatcher_fallback_position` deleted | **Unmet in final HEAD** — the deletion was bundled with the admission widening and reverted together. Ships in W0a.2.g alongside the Keyword + walker-parity fixes that unblock the downstream wiring. |

## 7-grammar predicate table (final HEAD)

| Grammar | `has_w4_classified` | `has_full_shape_coverage` | `has_shape_dispatcher_entrypoint` |
|---|---|---|---|
| JSON | false | true | **true** |
| CSS L4 | true | true | false |
| Sheets | true | true | false |
| BBNF | true | true | false |
| EBNF | false | true | false |
| BNF | false | true | false |
| BbnfBootstrap | true | true | false |

The admission widening commit (`030fb8aa`) briefly flipped the
six false entries to `true` and was reverted at `7a311a28` to
keep master test-green. Mid-rollout predicate table snapshot
preserved at `docs/benchmarks/post-AX-W0a2f-predicate-table.md`
as evidence the widening would close hard gates 3 + 7 once
W0a.2.g resolves the downstream emitter gaps documented in
§Remaining-blockers.

## Remaining blockers — scope reveal

### 1. Keyword emitter does not handle Ref-led Alt branches

`crates/core/src/backend/rust/emitter/shapes/keyword.rs` line ~120:
the `IrNode::Alt` arm filters `branches.iter()` to
`IrNode::Literal`-only bodies. The detector
(`crates/ir/src/passes/recognizers/shape_dispatch/keyword.rs`)
admits rules whose branches are literal-LED — including
`Ref(target_rule)` where `target_rule`'s body starts with a
literal. BBNF's `directive = import_directive |
recover_directive | ...` is the canonical case: every branch is a
Ref whose target's body starts with `@<name>` literal.

**Fix scope**: extend `emit_parse_keyword`'s Alt arm to accept Ref
branches; delegate each Ref to its target's shape fn via
`emit_ref_call_tape`. The Ref delegation needs `state` threaded
through, which changes the Keyword fn signature from `(input, p,
first_byte, builder)` to `(input, p, state, builder)` (or a new
variant fn). Six call sites propagate the change: `dispatcher.rs`
Alt-dispatch emission for `b't' | b'f' | b'n'`,
`emit_ref_call_tape`'s Number/Keyword special case, and the
visitor-path analogues. Estimated ~200 LOC + a wire-contract test
asserting `parse_keyword_BBNF_directive(input, …)` succeeds on
each admitted keyword.

**Why not fixed in W0a.2.f**: the signature-propagation touches
more than the allow-list permits (visitor-path keyword emissions
live inside emission sites the spec didn't name), and the fix is
architecturally orthogonal to the admission widening — it's a
detector/emitter contract reconciliation that belongs in its own
sub-wave. Shipping a partial Keyword fix here would break JSON
`bool`/`null` call sites or leave the signature mismatch half-
propagated.

### 2. Walker-parity regression on BNF `tape_parity_bnf`

`tape_parity_bnf::bnf_minimal_tape_parity` fails: shape-dispatch
emits 40 records with root `variant_idx=4`, walker golden is 34
records with `variant_idx=0`.

The 40-vs-34 record delta is six additional records per element
iteration. BNF's entry `grammar = Repeat(OW(Ref(rule)))` — the
Array-list emitter for this grammar now emits:

```text
iter_open + iter_child
skip_space (leading, no record)
Ref(rule) call → rule's records
skip_space (trailing, no record)
Seq compound push  ← +1 record per iter
```

Walker emission for a bare `Ref(rule)` inside `Repeat(OW(...))`
does NOT push a per-iter Seq compound — it pushes only the rule's
records directly. The `has_iter_ow` path in
`emit_parse_array_list` is walker-accurate ONLY when the inner is
`OW(...)` wrapping a non-Ref element (which emits its own per-iter
compound from the OW lowering). For OW wrapping a bare Ref, the
iter-compound push diverges.

**Fix scope**: refine `has_iter_ow` to distinguish
`OW(Ref(rule))` (no iter compound) from `OW(Alt | Seq | ...)`
(emits iter compound per walker's OW-Seq lowering). The
refinement is a ~30 LOC tweak to `emit_parse_array_list` plus
re-bench of the tape_parity goldens.

**Why not fixed in W0a.2.f**: requires reading walker's Repeat-
OW-Ref lowering to determine the exact compound emission
contract; outside the inline-attr + admission-widening brief.

### 3. Variant-idx stamping divergence (related to #2)

`tape_parity_bnf` golden has `variant_idx=0` on the root Rule
compound; shape-dispatch emits `variant_idx=4`. Walker stamps 0
when the Repeat's Rule compound is directly under the root
(`parse_array_BbnfGrammar_grammar` in the non-OW-outer path);
shape emission stamps the rule's own `id & 0xFF`. The walker's
semantics: the root's `variant_idx` on the outer Rule compound
comes from whichever Ref stamped `pending_variant_idx` last —
which for the TOP-LEVEL call is `u8::MAX` (no pending stamp), so
it lowers to 0. Shape-dispatch has no equivalent `pending_variant_idx`
mechanism; it stamps the rule's id unconditionally.

**Fix scope**: either (a) make the Array-list emitter stamp 0 on
its outer Rule compound (walker-identical for the non-OW-outer
case), or (b) re-generate the tape goldens to accept the new
stamping. Option (a) preserves walker-parity for downstream
callers; option (b) shifts the contract. Plan belongs to the
follow-on sub-wave.

### 4. Unknown blockers for CSS L4 / Sheets / EBNF / BbnfBootstrap

The tape_parity tests for these grammars were not re-run after
admission widening + array-element wiring. Based on blocker #2's
pattern (walker-parity divergence surfaces only after admission
widens), every one of these grammars is expected to reveal its
own walker-parity issues. The probing order:

1. Sheets (smallest per-rule count, single-regex-position formula).
2. EBNF (Repeat(Seq[Regex, Ref, Regex])).
3. CSS L4 (largest detector output; 98 fallback-position rules).
4. BbnfBootstrap (inherits BBNF blockers + structural-pipeline
   variant reshaping).

## Memory footprint observations

- D1 inline-attr downgrade alone: no regen → no rebuild of
  `generated.rs` consumers. Workspace-wide `cargo check` ~2s,
  no memory pressure.
- Admission widening + array-element wiring: `cargo test -p bbnf
  --test tape_parity_bbnf --no-run` ~12s, peak RSS ~2 GB per
  rustc child under `CARGO_BUILD_JOBS=4`.
- `cargo expand -p bbnf --test tape_parity_bbnf > /tmp/expand.txt`:
  91 524 lines, no SIGBUS post-D1. Confirms the LLVM-inliner cycle
  has been broken — the hard gate 1 close.

## Artefacts

- `/tmp/gate-test.txt`, `/tmp/gate-test2.txt` — predicate wire-
  contract test output pre/post admission widening.
- `/tmp/tape-bbnf.txt` — BBNF tape_parity run demonstrating the
  Keyword-Ref gap (infinite recursion pre-array-fix; stack-
  overflow post-array-fix when Keyword fn returns Err).
- `/tmp/tape-bbnf3.txt` — BBNF tape_parity post-array-fix;
  `Syntax { offset: 0 }` on `"start = …"` due to
  `parse_keyword_BbnfGrammar_directive` returning Err for every
  first_byte.
- `/tmp/tape-bnf.txt` — BNF tape_parity post-widening;
  40-vs-34-record divergence proving walker-parity mismatch.
- `/tmp/expand-bbnf.txt`, `/tmp/expand-bbnf2.txt` — cargo-expand
  slices of `tape_parity_bbnf`'s generated code; document the
  `parse_BbnfGrammar_grammar → __value → parse_array_...` call
  graph pre- and post-array-element fix.
- `/tmp/fallback-probe.txt`, `/tmp/fallback-probe2.txt` —
  `ax_w0a2f_fallback_probe` enumerates per-grammar fallback-
  position rules with ShapeTag and native-handling status.
- `/tmp/extract-probe.txt` — `ax_w0a2f_extract_probe` prints each
  grammar's entry rule body tree; used to classify the per-
  element shape category.
- `/tmp/probe-output.txt` — `ax_w0a2b_probe` confirms all 7
  grammars have zero entry-reachable unclassified Refs (the
  admission predicate's unclassified-Ref check is always
  satisfied; the remaining blocker was the now-retired
  `body_has_dispatcher_fallback_position`).

## Re-plan suggestion for W0a.2.g

Three-agent wave on disjoint file bounds:

1. **Keyword Ref-branch extension.** Own
   `crates/core/src/backend/rust/emitter/shapes/keyword.rs` +
   `dispatcher.rs` Keyword-call sites + the visitor-path
   keyword analogues + `emit_ref_call_tape`'s Keyword arm.
   Ship a two-signature split: pure-Literal branches keep the
   legacy `(input, p, first_byte, builder)` signature for JSON
   binary-compat; Ref-branch Alts emit
   `(input, p, state, builder)` with byte-dispatch + Ref-call
   delegation. `emit_ref_call_tape` inspects the target's body
   at codegen time to pick the signature.

2. **Array-list walker-parity refinement.** Own
   `shapes/array.rs::emit_parse_array_list`. Distinguish
   `OW(Ref(rule))` (no iter-compound emission) from
   `OW(Alt | Seq | Regex | ...)` (emits iter-compound per
   walker's OW-Seq lowering). Emit `variant_idx=0` on the
   outer Rule compound when the caller is the root dispatcher
   for a non-OW-outer entry (walker parity). Regenerate
   `tests/fixtures/tape_golden/bnf/minimal.json` if the
   walker-parity change introduces a deliberate shift (with
   rationale in PROGRESS.md).

3. **Per-grammar tape_parity audit + bootstrap regen.** Run
   the 6 non-JSON tape_parity tests; fix the surfaced
   walker-parity regressions per grammar (CSS L4 / Sheets /
   EBNF / BbnfBootstrap). Close the W0a bootstrap regen
   idempotency gate after all 6 pass.

Dispatch order: Agent 1 + Agent 2 in parallel on disjoint
files; Agent 3 opens after both land. Estimated effort: 1–2
agent-hours per wave.
