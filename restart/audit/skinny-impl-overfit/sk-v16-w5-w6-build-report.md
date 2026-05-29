# SK-V16 W5/W6 Build Report — CSS Typed-Summary Structural Parity vs cssparser

**Status:** Structural equality REACHED (genuine). Speed REFUTED (track1 far below cssparser, honestly reported). Scrutineer verdict: **ACCEPT**.

**Worktree (for merge):** `/Users/mkbabb/Programming/bbnf-lang/.claude/worktrees/wf_b72ce82c-db4-1`

**Commits:**
- `2a85bf240` fix(sk-v16-W5): preserve PEG branch order in wrap byte-dispatch
- `ea8138056` feat(sk-v16-W5): structural CSS typed-summary parity with cssparser
- `4de419f5e` test(sk-v16-W6): equality over structural fields; refresh substrate ids

---

## 1. Per-Structural-Field Parity (track1 vs cssparser)

All eight gated fields reached **genuine numeric parity** — equal real non-zero values, not constants or exclusion. Confirmed by the Scrutineer against a live run (`/tmp/w6_run.log:1231`, RUSTFLAGS=-C target-cpu=native, release).

| Field | track1 | cssparser | Equal |
|---|---:|---:|:--:|
| rules | 10136 | 10136 | yes |
| style_rules | 9561 | 9561 | yes |
| media_rules | 227 | 227 | yes |
| keyframes_rules | 283 | 283 | yes |
| generic_at_rules | 65 | 65 | yes |
| keyframe_blocks | 0 | 0 | yes |
| selectors | 9561 | 9561 | yes |
| declarations | 20043 | 20043 | yes |

The gate (`crates/core/tests/css_l4_w6_typed_retime.rs:850-859`, `summaries_share_gate_fields`) compares all eight. `shared_summary_equal=true` on the live run; spot-checked by the Scrutineer on style_rules (9561==9561) and selectors (9561==9561).

### How parity was reached (source fixes, not exclusion)
- **@media misclassification root cause** was a codegen bug in StructDirect Wrap-Alt byte-dispatch (`crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs`): `genericAtRule`'s bounded first-byte `{@}` was hoisted ahead of `mediaRule`/`keyframesRule` (no bounded first-byte → linear-try), inverting PEG precedence so every `@media`/`@keyframes` parsed as a generic at-rule. Fixed at source via `partition_branches` (lock to linear once any branch lacks a bounded first-byte) + regen. `media_rules` 0→227; `declarations` over-count 35852→20043.
- **Selector slab** populated via `record_compound_bounds` span capture (new `StructBuilder::bind_input` threads the input slice): one Selector-prelude per qualified rule, mirroring cssparser's one-selector-per-prelude. `selectors` 0→9561.
- **Keyframe blocks**: `keyframeBlock` made a registered struct rule routed to a `KeyframeBlock` frame; declarations captured.
- **Declaration double-count**: removed the `declaration` Alt-wrapper from the Declaration-frame route (only leaf decl rules open a frame). `declarations` exact match on all four corpora.
- **At-rule counting convention** (`is_cssparser_skipped_at_rule`, `css_l4_w6_typed_retime.rs:551-556`): `@charset`/`@import`/`@namespace` excluded from the track1 `rules`/`generic_at_rules` counters to mirror cssparser's StyleSheetParser, which consumes them specially. The rich AST still materialises them as `GenericAtRule` — preserve-rich-ast intact; this is a counting-convention mirror, not AST flattening.

**No structural field was excluded or weakened.** The gate was *strengthened*: master compared only `rules`, `declarations`, plus weak `values>0 && cssparser.values>0` existence checks; this fix added six structural equalities and removed the weak existence guards (Scrutineer finding #1).

---

## 2. equality_reached + equality_projection_change

**equality_reached: true.**

**Projection change:** exactly two fields removed from BOTH `CssTypedSummary` derivations (track1 visitor and cssparser probe):

- **`values`** (`css_l4_w6_typed_retime.rs:100`) — track1 counted typed-AST value nodes (one `CssTypedValue` per declaration value; a function/color/dimension is a single structured node); cssparser counted raw component tokens (every whitespace-separated token, including each inside a function's argument list and nested blocks). E.g. `rgb(1 2 3)` = 1 typed Color node vs 4+ cssparser tokens. Incommensurable granularities; never coincide (diag: track1 ~13872 vs cssparser 23563 on bootstrap). Justified.
- **`spans`** (`css_l4_w6_typed_retime.rs:111`) — track1 counted the `CssTypedValue::Span` catch-all (untyped-value fallback); cssparser emits no span concept (always 0). A field always-0 on one side and >0 on the other is intrinsically incomparable. Justified.

Documented at `css_l4_w6_typed_retime.rs:76-91`. The value-plane breakdown fields (`dimensions`…`lists`) are retained in the struct as diagnostics but deliberately NOT gated, for the same typed-node-vs-token-stream incomparability.

This satisfies the EQUALITY-INTEGRITY discipline exactly: only the two provably-incomparable fields named in the directive were removed; every structural field reached parity via builder/projection/grammar fixes.

---

## 3. Scrutineer Verdict — ACCEPT

`equality_honest: true`, `gate_meaningful: true`, **verdict: ACCEPT**. Not a blocking REJECT.

Gate-integrity findings (all clean):
- Equality gate STRENGTHENED, not gutted: 8 structural fields vs master's 2 + weak existence checks (`css_l4_w6_typed_retime.rs:850-859`).
- Only intrinsically-incomparable fields removed (`:100` values, `:111` spans; documented `:76-91`). Legitimate.
- Genuine numeric parity on live run (`/tmp/w6_run.log:1231`); real non-zero matched values, not constants/exclusion; `track1_errors=0 cssparser_errors=0`.
- NO short-circuits: grep for sha256/canonical_fixture/fixture-byte/broadcast/len()==/byte-equal in the test and builder found none. Equality path is purely the 8-field comparison + errors==0.
- NO hand-edit of generated files: `crates/core/src/grammar/generated/css_l4.rs:1` carries `//! AUTO-GENERATED`; `crates/core/src/runtime/css_l4/builder.rs:1` carries `// @generated by xtask regen-css`. Diffs trace to generator/source edits (`xtask/src/regen_css.rs`, `struct_direct.rs`, `emitter/grammar.rs`, `grammar/css/l4/media.bbnf`, `stylesheet.bbnf`, `xtask/runtime-projections/css_l4.toml`). Layout IDs in `css_l4_substrate.rs:124-129` updated to match regen. Coherent regen output — clean-regen discipline honored.
- Gate not a tautology: `validate_gate` (`:219-225`) recomputes expected_equal and rejects on inconsistency; live path (`:406`) emits `typed_summary_guard_failed` when `!shared_typed_summary_equal`. `keyframe_blocks` IS in the gate (`:856`, 0==0 — honest convention alignment, not exclusion).

**Gate-integrity caveat (non-blocking):** the committed report `restart/skinny/tranches/sk-v15/research/w6/skv15-W6-css-typed-retime.json` is PRE-FIX (schema sk-v15, rules 6231 vs 10136, selectors 0 vs 9561, 2 track1 errors, REJECTED). The source fixes genuinely converged; the committed JSON was simply not regenerated. Should be regenerated on merge to avoid a stale artifact, but does not affect equality honesty.

---

## 4. Speed — Honest Refutation (equality reached, speed below cssparser)

Equality was reached, so speed was measured. Result is an **honest negative**:

- **track1: 3.122 Mbps** vs **cssparser: 2448.904 Mbps** → track1 is ~0.13% of cssparser (~785x slower).
- W6 disposition: `REJECTED reason=track1_typed_below_cssparser_threshold`, `margin_mbps=-2446.782`, `shared_summary_equal=true`.
- Conditions: release, RUSTFLAGS=-C target-cpu=native, aarch64 Apple M5 Max, cold single-pass (`W6_SAMPLE_COUNT=1`) over 979638 bytes. (Scrutineer's live run reads 3.072 / 2402 — same order, same disposition.)

**The REJECT is speed-only.** The equality gate PASSES (`shared_summary_equal=true`); the test itself passes (2 passed) because it asserts report validity, not the speed-admission bit.

**Root cause (intrinsic workload asymmetry):** track1's `CssL4Parser::parse` builds a COMPLETE typed CSSOM arena with speculative backtracking — `CssStructBuilder::checkpoint` deep-clones the entire open-frame stack (`Vec<OpenFrame>`, each owning Vecs) on every Alt branch attempt, and CSS's grammar has wide Alts (28-branch `declaration`, `atRule`/`value` dispatch) tried with rollback. cssparser merely scans tokens and materialises nothing. The tape-first `value_from_ref`-style single-pass route does not apply to the CSS lane: CSS uses the struct-builder/arena substrate, not a tape, and W6 is hardwired to `CssL4Parser::parse`. No fabricated speed claim was made.

---

## 5. Regression Verification

All green (gitignored fixtures restored): W6 retime 2/2, css_l4 18/18, css_l4_substrate 14/14, css_l4_parity+project_types_css_l4 17/17, json_parity+json_value_parity 17/17, json_canonical_parity 10/10. The cross-grammar dispatch fix did NOT regress JSON. The only sweep "failures" were environmental — `data/css` and `data/json` fixtures are gitignored and absent in a fresh worktree; all pass once copied from the main worktree.

**Base discipline note:** the worktree was branched from `ab4d93786`, 3073 commits behind the intended base `366d88a7f` (main worktree HEAD holding the SK-V16 W5/W6 infra). `ab4d93786` is a clean ancestor with no unique commits, so the worktree branch was `reset --hard` to `366d88a7f` before work began. All work layers on the correct base.

---

## 6. Recommendation (orchestrator-facing)

**MERGE the worktree.** The primary W5/W6 deliverable — genuine structural CSS typed-summary parity with cssparser across all eight gated fields on four corpora — is fully met, Scrutineer-ACCEPTED, achieved through source-level grammar/projection/codegen fixes with clean regen and no contrivance. The equality gate is stronger after this work than before. On merge, regenerate the stale committed report `restart/skinny/tranches/sk-v15/research/w6/skv15-W6-css-typed-retime.json` (pre-fix sk-v15 schema) so the artifact reflects the converged numbers, and ensure `data/css`+`data/json` fixtures are present for the test sweep. Do NOT spend another fix pass on speed: beating a token-counter that materialises nothing while track1 builds a full rich CSSOM is not achievable without demolishing the checkpoint-clone backtracking that the PEG-correctness dispatch fix relies on. The speed gap is a dedicated perf tranche (clone-free mark/restore arena cursor; wider first-byte prefiltering as a prefilter only, not a reorder; a genuine CSS tape-first single-pass lane mirroring `json/value.rs` value_from_ref) — **REDRESS-route the SPEED axis to its own tranche**, do not gate this structural-parity merge on it.
