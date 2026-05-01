# AX.W0a.2.h — partial close + cross-scope halt

## Status

**Partial — halting with detailed diagnostic per the task spec's
"fundamental shape-emission design flaw" clause.**

D1 (admission widening) + three real shape-emitter bug fixes landed.
D2 (bootstrap regen + golden regen) HALTED when the shape-authoritative
pivot surfaced a cross-file architectural constraint the allow-list
forbids: the bootstrap proc-macro's tape walker (`crates/core/src/
grammar/host.rs`) identifies grammar rules by `variant_idx` on
`TapeKind::Rule` compounds the shape emitter elides. Adding the
compound cascades into IR sub-variant dedupe, which requires updates
to `crates/core/src/lower/expression.rs` and `crates/core/src/graph/
deps.rs` beyond the allow-list's surgical-emitter scope.

Runtime shape-routed `BbnfBootstrap::parse` succeeds on the full
bbnf.bbnf source plus every probe snippet (see §Probe evidence).
**Shape-dispatched parse is correct**; the bootstrap regen loop is
blocked by the CST walker's compound-identity contract.

## What landed (committed)

| Commit | Description |
|---|---|
| `29bfd055` | D1 — retire `body_has_dispatcher_fallback_position`; widen admission. 7/7 grammars admit via `has_shape_dispatcher_entrypoint`. Wire-contract matrix flipped + `aw_v_w5_2_per_ref_routing` Sheets-rejection renamed to `admission_admits_sheets_under_widened_predicate`. |
| `da3e03a5` | D3 — four surgical shape-emitter bug fixes under widened admission. Inline + AltDispatch: Seq Alt-branches containing Refs emit walker-parity structural records (was dead `return Err(());`). Inline: OW preservation via direct-node match (was stripped by unwrap_trivia). Keyword: Seq-bodied branches admit (BBNF `literal` canonical case). Flat: `Repeat { lo=0, hi=1 }` optional wraps inner in attempt-rewind (was propagating inner Err, breaking `"|" ?` patterns). |
| `67257495` | D3 — `ax_w0a2h_probe.rs` `#[ignore]`-gated probe test. Exercises 20 BBNF snippet inputs + the full bbnf.bbnf under shape-dispatched `BbnfBootstrap::parse`. |

## Probe evidence

With the shape-emitter fixes landed and a freshly-regen'd cycle-1
generated.rs (97 558 lines), shape-dispatched `BbnfBootstrap::parse`
successfully parses every probe input:

```
empty: OK
comment: OK
just_rule: OK (foo = "bar" ;)
just_rule_no_quotes: OK (foo = bar ;)
just_rule_alt: OK (foo = "a" | "b" ;)
just_rule_regex: OK (foo = /abc/ ;)
import_path_short: OK (@import "x";)
import_items_simple: OK (@import {a} from "x";)
import_items: OK (@import { a } from "foo" ;)
import_items_multi: OK (@import { a, b } from "foo" ;)
two_comments: OK
comment_then_import: OK
comment_then_import_items: OK
first_72_of_bbnf: OK
bbnf.bbnf: OK (3448 bytes)
```

Pre-fix (cycle-1 with admission widened but emitter-surgical bugs
intact) rejected: `just_rule` at offset 0, `import_items*` at offset
0, `comment_then_import_items` at offset 5, `bbnf.bbnf` at offset 72.
Post-fix: every input succeeds.

## Cross-scope halt condition

The bootstrap regen loop's self-host condition requires
`BbnfBootstrap::parse` to produce a tape that `crates/core/src/
grammar/host.rs` can walk with `find_descendant_by_kind(rule_item,
BbnfBootstrapRuleKind::rhs)`. The walker identifies a `rhs` compound
by its `variant_idx` stamp on a `TapeKind::Rule` record.

Shape emission's Wrap emitter (`crates/core/src/backend/rust/emitter/
shapes/wrap.rs::emit_parse_wrap`) elides the Wrap rule's compound —
designed for walker parity where the DTA's `ByteDispatch` state emits
no compound either. The chosen branch's shape fn owns the tape
record.

Under shape-dispatched `BbnfBootstrap::parse`:

- `rhs = closure | alternation` compiles as a Wrap rule.
- Shape emission dispatches directly to `parse_flat_closure` or
  `parse_flat_alternation`.
- The alternation's Rule compound is pushed with
  `variant_idx = alternation.id & 0xFF` — NOT `rhs`.
- `find_descendant_by_kind(rule_item, rhs)` walks the tape and fails
  to find a compound with `rule_kind() == rhs`.
- Panic: `rule: missing rhs descendant`.

### Attempted fix + cascade

Adding a `TapeKind::Rule` compound in the Wrap emitter stamped with
`variant_idx = wrap_rule.id & 0xFF` restores the rhs descendant walk.
Cycle 1 regen produces 97 561 lines; shape-dispatched parse succeeds
on bbnf.bbnf. But cycle 2's regen (using the cycle-1 emitter to
re-parse bbnf.bbnf) produces a 169 446-line generated.rs with
structurally different sub-variant projection:

- Cycle 1: `grammar_item_0`, `directive_0`, `term_1`, `term_2`,
  `value_atom_0` sub-variants present; Pratt classification preserved
  on 22 rule sites (`value_path`, `value_input`, `value_mul`, etc.).
- Cycle 2: sub-variant names vanish; Pratt classification disappears
  entirely (0 `parse_pratt_` sites emitted).

The sub-variant projection drops because the shape-authoritative
tape's Wrap Rule compound coalesces the homogeneous `Alt(Ref,…)`
branches into a single `variant_idx` — IR's `collect_sub_variants_walk`
no longer sees heterogeneous types per branch and skips sub-variant
emission. Pratt classification depends on operator-chain detection
which re-keys on the coalesced projection and fails to find the chain
pattern.

### Blocked files

Full closure of the cascade requires coordinated updates to:

- `crates/core/src/lower/expression.rs:214-215` — drops stale
  `directive_0` / `grammar_item_0` pattern-match references that no
  longer exist in the emitted `BbnfBootstrapRuleKind` enum.
- `crates/core/src/graph/deps.rs:68-79` — drops `term_1` dispatch
  arm; move identifier+call-args collection into the transparent-
  `term` wrapper arm.
- `crates/core/src/graph/deps.rs:123-129` — drops `term_2 |
  value_atom_0` grouped-term arm; consolidate into a fallback that
  descends via `find_descendant_by_kind(rhs)`.
- Investigation into why Pratt classification depends on the
  walker-shaped tape's sub-variant identities, and re-keying the
  operator-chain detector to the shape-authoritative projection.

None are in the W0a.2.h allow-list. Per the task spec's
"fundamental shape-emission design flaw" halt clause, the wave
partial-closes here with the D1 landing + D3 surgical fixes + this
diag.

## Hard-gate status

| Gate | Status |
|---|---|
| 1. Admission widened; all 7 grammars admit | **Met** — commit `29bfd055`. Predicate wire-contract passes 7/7. |
| 2. Bootstrap regen idempotent (two cycles byte-identical) | **Unmet** — cycle 1 (97 558 lines) ≠ cycle 2 (169 446 lines); cycle 2 = cycle 3 idempotent at the 169k fixed point BUT drops Pratt classification and sub-variants. Regression from HEAD. |
| 3. End-to-end `*_parity.rs` tests green | **Partially met** — `bbnf_parity` (2 tests) passes under the committed walker-routed generated.rs + shape-emitter fixes. `bbnf_ast_parity`, `json_parity`, `css_l4_parity`, `sheets_parity`, and others blocked by the derive-time `BbnfBootstrap::parse` → host.rs "missing rhs descendant" panic when the shape-dispatched path is activated via the widened admission. |
| 4. `cargo test --workspace --no-fail-fast` exit 0 | **Partially met** — under HEAD-committed walker-routed generated.rs + emitter fixes + wire-contract flip, every non-derive-triggered test passes. Derive-dependent tests (gorgeous integration, `*_parity.rs` tests importing other grammars) compile-fail because the gorgeous crate's derive calls BbnfBootstrap::parse and panics at host.rs. |
| 5. `parse()` zero walker-reach for 6 non-JSON grammars | **Unmet in final HEAD** — admission widens at predicate level, but the committed generated.rs is still walker-routed (HEAD's pre-W0a.2.h). Re-generating requires passing the bootstrap loop, which is blocked. |
| 6. `tape_parity_<grammar>` tests pass | **Met under HEAD-committed gen** — walker-routed goldens unchanged. Shape-authoritative regen pending cross-scope fix. |
| 7. W0a closure bench artefact | **Unmet** — deferred pending bootstrap regen close. |

## 7-grammar predicate table (final HEAD)

| Grammar | `has_w4_classified` | `has_full_shape_coverage` | `has_shape_dispatcher_entrypoint` |
|---|---|---|---|
| JSON | false | true | **true** |
| CSS L4 | true | true | **true** |
| Sheets | true | true | **true** |
| BBNF | true | true | **true** |
| EBNF | false | true | **true** |
| BNF | false | true | **true** |
| BbnfBootstrap | true | true | **true** |

Predicate widening landed at commit `29bfd055`. All 7 grammars admit.

## Re-plan suggestion for W0a.2.i

Single-agent wave on expanded allow-list covering the cross-scope
files. Work items:

1. **Wrap Rule compound emission** — restore the `TapeKind::Rule`
   compound in `shapes/wrap.rs::emit_parse_wrap` with `variant_idx =
   rule.id & 0xFF`. Required for `find_descendant_by_kind(rhs)`.

2. **IR sub-variant re-projection** — investigate why `collect_sub_
   variants_walk` drops sub-variants under the shape-authoritative
   tape's coalesced Rule-compound projection. Either:
   - Update the detector to identify heterogeneous Alts from the
     structural IR body (independent of tape shape), OR
   - Restore the sub-variant `variant_idx` stamping in shape emission
     (per-branch stamping on the Rule compound, not just the rule-
     level stamp).

3. **Expression.rs / deps.rs schema sync** — drop stale sub-variant
   references (`grammar_item_0`, `directive_0`, `term_1`, `term_2`,
   `value_atom_0`) that the coalesced projection no longer produces,
   OR restore them via fix #2. Either path closes the lower-layer
   compile errors.

4. **Pratt classification re-keying** — investigate whether the
   operator-chain detector's keying on sub-variant identities causes
   the Pratt loss on cycle 2. Re-key on grammar-structural signals
   (IR body shape) if so.

5. **Bootstrap regen to idempotent fixed point** — with fixes 1-4,
   re-run the regen loop and verify cycle N = cycle N+1 byte-
   identical at a stable Pratt-preserving classification.

6. **Re-execute D2 + D3 + D4 + D6 of W0a.2.h** — tape_parity golden
   regen, `*_parity.rs` tests green, non-Alt-root tail-call
   verification, W0a close bench.

Estimated effort: ~2-3 agent-hours on the expanded file bounds.

## Workspace test suite — current state

Under HEAD-committed walker-routed generated.rs:

```
cargo test -p bbnf --test gate_predicate_wire_contract
test result: ok. 7 passed; 0 failed; 0 ignored

cargo test -p bbnf --test bbnf_parity (with gorgeous dev-dep
temporarily disabled to isolate derive-time panic)
test result: ok. 2 passed; 0 failed; 0 ignored
```

The probe test confirms shape-dispatched parse correctness once a
compatible generated.rs lands — every architectural pivot for D3 is
ready for the follow-on wave to consume.

## Artefacts

- `crates/core/tests/ax_w0a2h_probe.rs` — `#[ignore]`-gated probe.
- `/tmp/ax-w0a2h-probe9.txt` — full probe output under widened
  admission + all four emitter fixes.
- `/tmp/ax-w0a2h-gen1f.rs` — cycle-1 regen output (97 558 lines)
  demonstrating shape-authoritative emission that parses bbnf.bbnf
  successfully at runtime.
- `/tmp/ax-w0a2h-gen2h.rs` — cycle-2 regen output with Wrap Rule
  compound (169 446 lines) showing the sub-variant / Pratt cascade.
