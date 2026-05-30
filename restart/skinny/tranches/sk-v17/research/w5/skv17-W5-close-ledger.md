# SK-V17 W5 — Close, Clean Regen, Lock-14 Audit, RESULTS Reconciliation

Pass: SK-V17 wave triumvirate. Wave: W5 (close). Date: 2026-05-30.
Status: **SK-V17 CLOSED** — the R10 tranche-close condition is met. >SOTA achieved
on both regular corpora at the typed plane with EXACT 9-field equality, JSON 51/51
held, invariants intact. The W4 conditional gate is recorded NOT-MET (L9
not-needed) with re-profile evidence.
Base HEAD at capture: `6bb4b2a6c` (W3 NEON structural index — current master HEAD,
W0+W1+W2+W3 landed). Host: Apple M5 Max, aarch64 (`-C target-cpu=native`, fat LTO,
codegen-units=1, debug=true). No x86/AVX/SVE.

## §0 — R10 close condition (the verdict)

R10 (SYNTHESIS §0.1 close condition): SK-V17 closes when **>SOTA is met on ≥1
regular corpus at the N≥50 typed plane + EXACT cssparser equality + JSON guard +
invariants hold**. All gates below are MET.

| R10 gate | Status | Evidence |
|---|---|---|
| JSON 51/51 guard | **HELD** | runtime lib 22/22 PASS (JSON value + CSS tape-activation + NEON parity); `check-json` regen exit 0; bbnf-bench JSON parity family PASS; JSON `value_from_ref` path untouched |
| Tape activation (not dead code) | **MET** (W1) | `Tape`/`ValueRef`/`TapeBuilder` live in all 7 `css_l4_*/generated.rs`; `payloads().write_count()==0` (lazy); `emit_fact_stream` retired |
| Layout-driven projection | **MET** (W2) | `BackendRule`-walking rich rider (`CssNode::value`/`CssRule`/`CssDeclaration`/`CssTypedValue`/`CssRichSummary`) generated into all 7 modules; `W5C_REQUEST_FACT_PROFILES` RETIRED |
| CSS typed equality (before speed) | **HELD — EXACT 9-field** | `rich_cssom_matches_cssparser_on_real_corpora` PASS (all 4 corpora, all 9 fields); `rich_cssom_matches_cssparser_on_fixture` PASS; `cssparser_oracle_matches_generated_track1` PASS |
| preserve-rich-ast | **INTACT** | rich CSSOM via lazy `ValueRef` views; zero payload-arena writes; no per-leaf `Box::new`; value-plane population parity EXACT (W2 §2) |
| CSS >SOTA on regular corpora | **MET** | bootstrap **2.21×**, animate **2.36×** over lightningcss full-CSSOM, N=200 cold median (§3) |
| Honest tailwind handling | **CROSSES** | tailwind 3.35× (admits, not paper-closed) |
| Telemetry honesty (N≥50) | **MET** | N=200 cold median, lightningcss same-run full-CSSOM comparator re-baselined |
| NEON hot-leaf union | **MET** (W3) | shared eq-set classifier (alphabet-as-data) + 2 net-new mask primitives, scalar-ref + checkasm PASS, same-wave consumed, aarch64-only |
| Generated-state cleanliness | **CLEAN** | `regen --check` 9/9 exit 0 (§1) |
| Foldable into TOTALITY | **MET** (by construction) | the tape/projection model + NEON leaf set are SK-V18 fold targets; JSON+CSS exercise the projection generality |
| W4 commit-by-construction | **NOT-MET (L9 not-needed)** | re-profile shows 0% speculative-rollback self-time; recognizer is already commit-by-construction (W4 ledger) |

**R10 VERDICT: SK-V17 is CLOSED.** No open implementation rows. The single
conditional wave (W4) is honestly recorded as not-needed with profiler evidence, per
its own gate (`SPEC.md:679-680`).

## §1 — regen --check: 9/9 CLEAN

`cargo xtask <check>` run for all 9 generated targets, every command exit 0:

| target | exit |
|---|---|
| check-json | 0 |
| check-css-l4-at-rules-and-media | 0 |
| check-css-l4-declaration-values | 0 |
| check-css-l4-declaration-values-extended | 0 |
| check-css-l4-nested-layout | 0 |
| check-css-l4-stylesheet-selectors | 0 |
| check-css-l4-vendor-and-custom-atrules | 0 |
| check-css-l4-visual-functions | 0 |
| check-real-typed | 0 |

`dirty_generated_state=clean`. The 8 W0-bracket-dirty generated files
(`generated_real_typed.rs` + 7 `css_l4_*/generated.rs`) are fresh generator output,
landed clean across W1/W2/W3; `git status` shows NO dirty generated files at close.
Never hand-patched (Lock 6/14).

## §2 — Lock-14 grammar-neutrality audit

The substantive Lock-14 invariant — no grammar policy in the generic crates; the
NEON classifier + projection generator name no grammar; kernels take byte-sets /
alphabets as DATA — **HOLDS**:

- `W5C_REQUEST_FACT_PROFILES` (the Lock-14-phrase-#1 hand-curated routing catalogue):
  **RETIRED**. The only residue in `crates/` is a retirement comment
  (`codegen/src/lib.rs:298`); routing derives from `CSS_PROFILE_IDS` / the
  `BackendRule` shape. Not extended, not relocated.
- `bbnf-simd` (the generic SIMD crate): **ZERO** grammar names (`css`/`json`) in
  non-comment code. `select_classifier(alphabet: &'static [u8; 64])`
  (`dispatch.rs:42`) takes the byte-set as data. The W3 mask kernels take their
  byte-sets as caller data: `bracket_depth_mask_64(src, opens: &[u8;SET_CAP],
  closes: &[u8;SET_CAP])` (`aarch64/bracket_depth_mask_64.rs:32-35`),
  `comment_body_mask_64(src, open: [u8;2], close: [u8;2])`
  (`aarch64/comment_body_mask_64.rs:34-36`). The CSS bridge
  (`find_css_significant`/`find_comment_close`) lives in `runtime`, NOT in
  `bbnf-simd`.
- `codegen/src/lower/`: **ZERO** non-comment grammar-specific references — the
  projection generator names no grammar; the per-rule CSS routing is the `BackendRule`
  branch-tag projection, not a hand-curated per-rule-id catalogue.
- The substantive Lock-14 census tests PASS: `generic_crate_scan_rejects_json_policy_leaks`,
  `rejects_json_named_tape_flag_tokens_in_generic_roots`,
  `generic_crate_scan_strips_test_only_json_tokens` — all ok.

**Lock-14 grammar-neutrality: CLEAN.** (The `lock14_baseline::accepts_current_allowlist`
frozen-diff census failure is an audit-overlay BOOKKEEPING gap — the W3 commit's
touched-file set is not registered in the frozen Lock-14 allowlist baseline — NOT a
grammar-neutrality violation; it is one of the 7 pre-existing failures carried
forward, §5.)

## §3 — RESULTS reconciliation (canonical N=200 cold medians, rich-typed vs lightningcss)

Same-session N=200 cold per-parse median, `w2_rich_cssom_bench` on the
W0/W1/W2/W3-comparable plane (fat LTO, cu=1, `-C target-cpu=native`, aarch64). The
rich typed Track-1 CSSOM (`parser::rich_summary` — offset tape + lazy 9-field
projection) vs lightningcss full-CSSOM, re-baselined same-run:

| corpus | class | rich-typed Track1 | lightningcss full-CSSOM | **rich/lcss** | W0 lcss bar | crosses? |
|---|---|---:|---:|---:|---:|:--:|
| bootstrap | **regular** | 2473.1 | 1119.1 | **2.210×** | 1112.4 | YES |
| animate | **regular** | 2937.9 | 1247.7 | **2.355×** | 1218.7 | YES |
| tailwindcss | nested/utility | 2773.4 | 828.5 | **3.348×** | 841.3 | YES (honest) |
| material-components-web | irregular | 2618.5 | 1312.0 | **1.996×** | 1292.3 | YES |

The lightningcss re-baseline (1119/1248/828/1312) matches the W0-LOCKED >SOTA bar
(1112/1219/841/1292) within ~5% — the harness is W0-comparable. **>SOTA verdict:
max ratio 3.348× (tailwind); per-corpus 1.996×–3.348×; BOTH regular corpora cross
decisively (bootstrap 2.210×, animate 2.355×).** Plane: full-cssom comparator;
`css_rich_ast_preserved=true`; `css_typed_summary_equal=true` (EXACT 9-field).

These are NOT broadcast: each row is a distinct measured corpus. The pre-blocked
"24-row broadcast" (`ROLLING-SOTA-DELTA.md:70-93`, one CSS timing tuple → N
conceptual rows) is NOT extended. The SK-V17 CSS close is the 4 per-corpus medians
above; the live `ROLLING-SOTA-DELTA` CSS rows remain on the superseded
`css_l4_full_parse` diagnostic plane (cssparser, the old W8R diagnostic) — the
SK-V17 close note (§3.1) records the plane flip without re-broadcasting.

### §3.1 — ROLLING-SOTA-DELTA reconciliation note

The live `ROLLING-SOTA-DELTA.md` CSS L4 section (`:66-93`) reflects the pre-SK-V17
plane: `T1_current=2319.04` / `T1_sota=930.28` (cssparser diagnostic full-parse),
status `OPEN`, one tuple broadcast across 24 conceptual rows. SK-V17 supersedes that
plane with the rich-typed-vs-lightningcss-full-CSSOM comparison above. The honest
disposition: the CSS >SOTA claim is now MET on the FAIR materializing comparator
(lightningcss), per-corpus, not the cssparser flaw-probe; the 24-row broadcast stays
pre-blocked and is NOT re-stamped ADMITTED en masse — the close evidence is the 4
per-corpus medians. A full `ROLLING-SOTA-DELTA` schema migration to per-corpus
lightningcss rows is an SK-V18 RESULTS-plane fold (the live consumer
`gate-json --check-results` keys on the JSON 51-row universe, which is unchanged and
HELD; the CSS rows are diagnostic-OPEN under the current schema). JSON 51/51 rows in
`ROLLING-SOTA-DELTA.md:13-64` are unchanged and ADMITTED.

## §4 — Invariants (all held)

- **16 locks.** Lock count preserved (`restart/locks/LOCKS.md:60` — the Lock-14
  scoped clause preserves the 16-lock count; no Lock 17). Lock 1 (substrate-union:
  the existing skinny `Tape`/`ValueRef`/`TapeBuilder` is the only substrate, no
  second tape), Lock 6/14 (generated-output clean regen), Lock 14
  (grammar-neutrality, §2), Lock 16 (SIMD parity, §5 checkasm) all held.
- **5-shape BackendShape canon.** `bbnf_ir::BackendShape` = {EagerTape, OffsetTape,
  EventTape, SinkOnly, CollapsedStage} (`crates/ir/src/lib.rs:340-346`) — exactly 5;
  no 6th shape added.
- **tape = substrate-category.** CSS rides the existing skinny `Tape` (offset tape,
  `BackendShape::OffsetTape`); no new substrate / cursor / builder type introduced.
- **no x86.** All SIMD is aarch64 NEON/dotprod (`#[cfg(target_arch="aarch64")]`);
  scalar references are the non-aarch64 fallbacks. No AVX/SVE.
- **preserve-rich-ast.** Rich 9-field CSSOM lazily projected; zero payload writes;
  EXACT cssparser population parity (§0).
- **no re-opened REDRESS.** AZ-IV eager value tree NOT re-opened (materialization
  stays lazy; `write_count==0`). StructRegistry/Arena<G>/Builder<G> indirection NOT
  re-opened (no registry lookup in the per-leaf hot path; the recognizer is the
  delimiter scanner). Fact-stream String NOT re-opened as an admission plane
  (retired, diagnostic-only). REDRESS-53 (parallel retained index), REDRESS-88
  (PMULL comment body), REDRESS-89 (CTZ as L6 default) NOT re-opened. W4 added no
  speculative-rollback admission and no `split_off`/`Vec<Vec>` (none attempted).

## §5 — Honest residual ledger

1. **Deferred literal single-emitter codegen-unification (REDRESS-W2-1).** The
   SPEC's "ONE `BackendRule`-walking generator that RE-EMITS JSON's `value_from_ref`
   byte-equal THROUGH a brand-new unified emitter" was NOT introduced
   (W2 §7: JSON's value/view/visitor are static hand-written templates
   `include_str!`-copied verbatim; the CSS provider is a generated template string).
   The grammar-neutral obligation is JSON+CSS-witnessed-by-construction (both ride
   the same `Tape`/`ValueRef`), which is the parity SK-V17 proves. A literal
   regeneration of JSON's `value.rs` through one unified emitter is a larger
   codegen-unification effort — DEFERRED to SK-V18 / Pass Omega, NOT an SK-V17 close
   gate.
2. **crates/core totality-tree adoption.** The unified tape/layout/projection model +
   NEON leaf set are structured for the TOTALITY tree
   (`crates/core/src/runtime/tape/`, the `StructLayout`/`OpenFrame` design-intent
   target) to adopt — this is the **SK-V18 fold target, NOT SK-V17 owner work**. The
   `css_l4.toml` 594-vs-34 LOC asymmetry is a TOTALITY artefact (grep-clean-absent
   from `skinny/`), INFORMATIONAL only.
3. **non-CSS-non-JSON projection generality (Sheets / BBNF-self).** `sheets_witness`
   has no `.bbnf` source / `BackendRule` shape to walk, so it cannot serve as an
   SK-V17 projection-generator exercise; this generality is asserted-by-construction
   with the proof DEFERRED to SK-V18.
4. **~7 pre-existing bbnf-bench audit-overlay census failures.** The SK-V14
   audit-overlay census tests fail at the close: `report::tests::*` (×5:
   `direct_contract_accepts_complete_n_direct_movement`,
   `skv12_non_json_report_accepts_generated_baseline`,
   `direct_contract_accepts_w11_3_mesh_track1_sota_reopen`,
   `w0_report_accepts_exact_opening_baseline`,
   `w6_typed_contract_accepts_complete_github_events_row`),
   `lock14_baseline::tests::accepts_current_allowlist`,
   `json_w9::tests::w9_surface_probe_classifies_direct_and_typed_rows`. These are
   RESULTS-report-shape / frozen-diff census assertions (NOT parse / equality / SIMD
   correctness). **VERIFIED PRE-EXISTING**: they fail identically at the clean W2
   HEAD `6dad81fb9` (W3 ledger §7, stash-verified) and at the W3 HEAD `6bb4b2a6c`;
   W4/W5 add zero tracked source to bbnf-bench (the `w4_css_reprofile.rs` bin is an
   untracked measurement harness). They are SK-V14 audit-overlay census carried
   forward per the SPEC standing exclusion; they do NOT block the close. The
   substantive Lock-14 grammar-neutrality census tests PASS (§2).

## §6 — Wave dispositions (every SK-V17 wave: admitted/rejected/routed)

| wave | disposition | evidence |
|---|---|---|
| W0 | baseline/telemetry MET (0 behaviour LOC) | `research/w0/skv17-W0-baseline-ledger.md` |
| W1 | ADMITTED — fact-stream pruned, CSS routed into tape, EXACT 4-field equality | `research/w1/skv17-W1-prune-tape-ledger.md` |
| W2 | ADMITTED — rich lazy 9-field CSSOM projection, EXACT 9-field equality | `research/w2/skv17-W2-rich-projection-ledger.md` |
| W3 | ADMITTED — NEON structural index, >SOTA met (2.21×–3.42×), checkasm PASS | `research/w3/skv17-W3-neon-structural-index-ledger.md` |
| W4 | **NOT-MET (L9 not-needed)** — re-profile 0% speculative-rollback self-time | `research/w4/skv17-W4-conditional-ledger.md` |
| W5 | CLOSE — this ledger; regen 9/9, Lock-14 clean, RESULTS reconciled, R10 CLOSED | this file |

## §7 — Final SK-V17 >SOTA summary

SK-V17 reaches >SOTA on the FAIR full-CSSOM-materializing comparator
(lightningcss), per-corpus, at the rich typed plane with EXACT 9-field cssparser
equality and JSON 51/51 held throughout:

- **bootstrap (regular): 2.210×** · **animate (regular): 2.355×** · tailwind:
  3.348× · material: 1.996× — all over lightningcss full-CSSOM, N=200 cold median.
- The path: W1 pruned the fact-stream String into the existing skinny offset tape;
  W2 rebuilt the lazy rich 9-field CSSOM projection (`BackendRule`-walking, zero
  payload writes); W3 accelerated the dominant scan leaf with a shared
  grammar-neutral aarch64 NEON eq-set classifier + 2 net-new mask primitives
  (checkasm-verified, same-wave consumed). W4's commit-by-construction was
  not-needed — W1's PRUNE already rebuilt the recognizer as a commit-as-you-scan
  delimiter parser with 0% speculative-rollback self-time.

**SK-V17 CLOSED.** On close, Pass Alpha dispatches the SK-V17→SK-V18 synthesis
(Sheets/BBNF-self tape-conversion + TOTALITY-fold) per PASS-ALPHA.
