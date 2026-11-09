# G1 Wave Ledger — JSON projection: `SinkOnlyExpr` AST-walk emitter (R-C C1)

Date: 2026-06-02. Wave: SK-V18 G1 (the inflection wave — hand-written codegen begins
being replaced by the grammar-driven generator). Gate: SPEC §4 (G1) as amended by
`AMENDMENT.md` (split byte-equivalence, ratified). Outcome: **GREEN** (all binding gates
held honestly; one pre-existing, out-of-scope CSS reproducibility failure noted in §6).

---

## 1 — What the walk replaced (fixed-literal → tree-derived)

`crates/codegen/src/json_sink_direct.rs` was a single 525-LOC file of 7 fixed-literal
`push_str` blocks. It is now a **directory module** (`crates/codegen/src/json_sink_direct/`)
that recursively walks the `SinkOnlyProgram` tree (`lower/sink_only.rs`) and DERIVES the
structure from the grammar:

| Module | Replaces (old fixed literal) | Derivation from the tree |
|---|---|---|
| `mod.rs` | `render()` + `render_header` + the `PREFIXES`/`INLINE_CFG` conventions | rule-by-name index; the 3 sink-prefix variants as a `PREFIXES` table (not LCD-unified) |
| `first_set.rs` | (new) | first-byte-set analysis over `SinkOnlyExpr`, resolving `CallRule` through the program — the dispatch keys come from each branch's leading `ByteLiteral`/`RegexProgram` |
| `dispatch.rs` | `render_value_dispatch` (the 91.5% hot leaf, 3 fixed `match byte {…}` fns) | `derive_arms` walks the `value` rule's `Alt{Dispatch}` branches → `DispatchArm{keys, kind}`; the arm SET, KEYS, sink verbs, and number range are tree-derived. Emitted in 3 sink-prefix variants via the `PREFIXES` loop |
| `skeleton.rs` | `render_entry`, `render_container_rules`, `render_string_rule`, `render_number_rules`, `render_utility_rules` | container open/close bytes derived from `object`/`array` rules' `ByteLiteral`s; string/number shells keyed off the rule's `RegexProgram{span_kind}` → external scanner callee |
| `recognize.rs` | `JSON_PARSE_ONLY_GENERATED_RS` (the retired 353-LOC iterative explicit-stack courier in `runtime_generator.rs`) | the SAME `value`-Alt walk under the recognize policy (sink calls suppressed): `recognize_value`/`object`/`array`/`string`/`number`, recursive descent |

`runtime_generator.rs`: the `JSON_PARSE_ONLY_GENERATED_RS` const + its splice DELETED; the
parse-only path is now emitted by the walk.

Net structure that is now grammar-DERIVED (was fixed-literal): the value dispatch arm set +
keys, the container loops' delimiters, the literal arms (`b't'`/`b'f'`/`b'n'`), the number
digit-range, and the parse-only recognizer shape.

---

## 2 — Byte-equivalence diff result (the binding proof)

**sink_direct path — HARD byte-equivalence: ACHIEVED.** The regenerated `json/generated.rs`
sink_direct region (`parse_value_direct` + `parse_object_value_at_direct` +
`parse_array_element_at_direct` + the container/string/number/utility emit) is
**BYTE-IDENTICAL** to the shipped pre-G1 baseline (`git show HEAD:…/json/generated.rs`),
diff EMPTY (`diff -q` exit 0), 454 == 454 lines for the region, same `#[inline(always)]`/
`#[inline(never)]` cfg pairs, same `sink.object_*`/`sink.array_*` call sites. Confirmed
again after the final source refactor (`SINK_DIRECT_BYTE_EQUIV: IDENTICAL`).

**parse-only path — STRUCTURAL + BEHAVIORAL equivalence (per AMENDMENT):** the recognizer is
walk-derived (recognize policy), recursive-descent (not the retired iterative courier),
51/51 held (runtime `generated_parse_only_accepts_and_rejects_json` + the parse_only
accept/reject suite pass), `verbatim_blob_present == false`, `regen --check` clean. Byte
delta vs the iterative courier is EXPECTED and is the honest §6 finding F3 (the
`SinkOnlyExpr` tree encodes a recursive lowering; reproducing the iterative machine
byte-exact would be the L1 reject).

Full-file line count: 1199 → 961 LOC (the iterative courier replaced by the leaner derived
recognizer; the sink_direct region is byte-unchanged). `line_delta_vs_oracle` is SOFT
advisory only.

---

## 3 — The `.bbnf`-mutation falsifier (proves derivation, NOT a fixed stringifier)

Dropped the `bool` rule from `grammars/json.bbnf` (removed the `bool` rule + its reference
in the `value` alternation), ran `regen-json`, inspected the sink_direct region:

| | baseline (with `bool`) | mutated (`bool` dropped) |
|---|---:|---:|
| sink_direct `b't' =>` arms | 3 | **0** |
| sink_direct `b'f' =>` arms | 3 | **0** |
| sink_direct `sink.bool*` calls | 6 | **0** |
| sink_direct `b'n' =>` (null) arms | 3 | 3 (unchanged) |
| recognize `b't'`/`b'f'` arms | 1/1 | **0/0** |

The `b't'`/`b'f'`/`sink.bool` arms VANISHED from BOTH the sink path and the recognize path;
the `b'n'` (null) arms persisted — proving only the dropped rule's arms vanished, not the
whole dispatch. The grammar was then REVERTED (clean diff vs original) and regenerated; the
sink_direct region restored BYTE-IDENTICAL to baseline (diff exit 0, b't'/b'f' back to 3/3).
**`g1_bbnf_mutation_falsifier_fires == true`.**

---

## 4 — Build + parity

- `cargo run -p xtask -- check-json` (regen --check): **exit 0, clean**.
- `cargo run -p xtask -- regen-json`: **exit 0**; emitted sink_direct byte-identical.
- codegen `-p codegen` tests: **31 pass / 1 fail** — the 1 fail is
  `css_l4_generated_runtimes_reproducible_from_request` (CSS, pre-existing, §6).
  Every JSON codegen test passes, including `emits_distinct_json_parse_only_path_without_tape_builder`
  (updated to assert the recognize-policy recognizer) and
  `direct_parser_is_authored_from_sink_only_lowering`.
- runtime `-p runtime` tests: **22 pass / 0 fail** — JSON parse/parse_direct/parse_only
  correctness incl. `generated_parse_only_accepts_and_rejects_json` (the 51/51 accept/reject
  guard). `g1_json_guard_rows_held == true`.
- workspace build (`cargo build --profile ax-iter --workspace`): green (see verification log).

---

## 5 — Telemetry

```text
g1_json_generated_byte_equivalent[sink_direct]  = true   (regenerated == shipped baseline, diff EMPTY, 454==454; HARD byte-equiv held)
g1_json_generated_structural_equiv[parse_only]  = true   (recognize-policy walk; 51/51; blob-free; regen --check clean — per AMENDMENT)
g1_bbnf_mutation_falsifier_fires                = true   (drop bool ⇒ b't'/b'f'/sink.bool arms vanish in BOTH sink + recognize; revert restores byte-exact)
g1_hot_leaf_preserved                           = true   (parse_object_value_at_direct + parse_array_element_at_direct byte-identical: same inline cfg + sink.object_*/array_* call sites)
g1_dispatch_triple_not_lcd_collapsed            = true   (3 monomorphized variants via the PREFIXES table; not unified)
verbatim_blob_present                           = false  (JSON_PARSE_ONLY_GENERATED_RS DELETED; no grammar-body r#"..."# blob in json_sink_direct/; the 8 raw-string fragments are fixed runtime-support idioms — house pattern, like json_typed_direct.rs's PARSER_RUNTIME)
g1_leaf_primitive_count                         = 2      (string leaf: match_tiny_plain_string_direct + match_string_at_quote_trusted_utf8/unescape_string; number leaf: match_number_span_from_first + materialize_*)
g1_leaf_primitive_loc[string_shell]             = 32     (parse_string_direct shell, walk-derived; the scanner body is external)
g1_leaf_primitive_loc[number_shell]             = 12     (parse_number_direct shell, walk-derived; the scanner body is external)
g1_leaf_primitive_profiled_leaf_extent          = ~74    (the dispatch skeleton parse_object_value_at_direct 37 + parse_array_element_at_direct 37 — the 91.5% hot leaf; leaf shells 32/12 < this; the (d) god-kernel check PASSES — the primitives do NOT contain the dispatch/loop skeleton)
g1_leaf_primitive_abcd_pass                     = true   (a: callee SELECTED from RegexProgram span_kind per AMENDMENT clarification — json.bbnf has zero -> arrows; b: number dispatch first-set b'-'|b'0'..=b'9' derived from the number regex pattern, widens under digit-class mutation; c: blob-free; d: shell LOC ≤ hot-leaf extent)
g1_metalang_leak_count                          = 0      (parse_w11_1_number on regenerated file == 0; the w7_ match was config::w7_direct_policy_triad, a legitimate W7 policy symbol, NOT a leak)
g1_json_guard_rows_held                         = true   (51/51 JSON accept/reject; runtime tests 22/22 green)
line_delta_vs_oracle                            = -238   (1199 → 961; SOFT advisory only — the iterative courier retired)
```

---

## 6 — Findings (honest carries)

**F1 (a)-gate seam (per AMENDMENT, folded).** `grammars/json.bbnf` has ZERO `->` arrows.
The leaf scanner is SELECTED from the rule's `RegexProgram` span_kind (String/Number → the
external scanner callee), not read from a literal `->`. The named primitives are the existing
external callees (`match_tiny_plain_string_direct` + `match_string_at_quote_trusted_utf8`/
`unescape_string`; `match_number_span_from_first` + `materialize_u64/i64/f64`), NOT the
SPEC's non-existent `decode_json_string_to_arena`. Implemented exactly as the AMENDMENT
clarifies.

**F2 the 3-prefix loop (house convention, not a blob).** The value/object/array sink prefix
(`sink.` / `sink.object_` / `sink.array_`) is NOT a tree fact — the grammar has ONE `value`
rule; the positional context is a codegen specialization. It is emitted via the `PREFIXES`
table (3 monomorphized variants), the SAME legitimate pattern the prior `render_number_emitter`
already used. The dispatch triple is NOT LCD-unified.

**F3 parse-only is not byte-derivable from the recursive tree (the binding §6 finding,
already amended).** The retired `JSON_PARSE_ONLY_GENERATED_RS` was an ITERATIVE
explicit-stack frame machine; the `SinkOnlyExpr` tree encodes a RECURSIVE lowering. Forcing
the recursive walk to reproduce the iterative courier byte-exact would be the L1 reject. Per
the ratified AMENDMENT, the parse-only path is held to STRUCTURAL + BEHAVIORAL equivalence
(recognize policy, 51/51, blob-free, derived), NOT byte-equivalence. This is satisfied.

**§6 OUT-OF-SCOPE finding — pre-existing CSS reproducibility failure.** The codegen test
`css_l4_generated_runtimes_reproducible_from_request` (`lib.rs:589`, a `check_dir` mismatch)
fails at the current HEAD+worktree. It is in the CSS request-facts path
(`emit_request_facts` → `normalize(CSS_GENERATED_RS)`), which G1 NEVER touches: zero CSS
files are dirty (only `json/generated.rs` changed in the runtime tree), and my codegen diff
is JSON-only (`json_sink_direct/`, the JSON courier deletion, the JSON test). The failure is
a stale on-disk CSS runtime vs the generator — a pre-G2 condition, orthogonal to G1. It is
recorded here for the orchestrator; it does NOT gate G1 (which is scoped to JSON) and is the
kind of CSS regen G2 addresses.

---

## 7 — Diff summary

- `crates/codegen/src/json_sink_direct.rs` → DELETED (moved to directory module).
- `crates/codegen/src/json_sink_direct/{mod,dispatch,first_set,skeleton,recognize}.rs` → NEW (the walk).
- `crates/codegen/src/runtime_generator.rs` → `JSON_PARSE_ONLY_GENERATED_RS` const + splice DELETED.
- `crates/codegen/src/lib.rs` → `emits_distinct_json_parse_only_path_without_tape_builder`
  retargeted to assert the recognize-policy recognizer (the iterative courier symbols are
  gone by design).
- `crates/runtime/src/grammars/json/generated.rs` → regenerated (sink_direct byte-identical;
  parse-only recognizer replaces the courier; 1199 → 961 LOC). Never hand-patched.
- `grammars/json.bbnf` → transiently mutated for the falsifier, REVERTED (clean).

Not committed (the orchestrator commits).
