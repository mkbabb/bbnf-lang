# S-P1 CHALLENGE V2 — Lens CH2 (GENERALITY)

Pass: S-P1 Profile. Cycle: V2 (confirming pass). Lens: CH2 GENERALITY.
Date: 2026-05-23.
Scope: V2 confirming pass over the V2 light micro-fold landed in commit `069ba203c`. Verify (a) the V1 100% ACCEPT verdict carries forward unchanged; (b) F1 (`parse-attribution` transitive feature) and F2 (CSS L4 zero-evidence asymmetry) carry forward as non-blocking findings; (c) the V1 R1+R2 non-blocking refinements on P1-E §2.3 typed-plane file:line drift are mechanically discharged by the V2 fold packet `F-V2-P1E-1` (skip_value 1739 → 2949 etc.); (d) no new CH2 GENERALITY REVISE has been introduced by the V2 fold.
Authority: `restart/prompts/skinny/PASS-1-PROFILE.md §3` (CH2 binding); `restart/prompts/ORCHESTRATOR.md:84,201,204` (CH2 lens registry + Lock 14 audit-per-pass binding); `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH2.md` (V1 disposition; 100% ACCEPT with R1+R2 non-blocking + F1+F2 new findings); `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md §2` (CH2 V1 focus row); `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH1.md §0.1` (BINDING refresh closure for CH1 V1 REVISE — same `F-V2-P1E-1` packet discharges CH2 R1+R2 in carry-through).
Artefacts reviewed (HEAD = `069ba203c`): `p1a-samply-mode-1.md`, `p1b-samply-mode-2.md`, `p1c-samply-mode-3.md`, `p1d-pmu-cycles.md`, `p1e-hot-leaf-attribution.md`, `p1f-results-delta.md` under `restart/skinny/tranches/sk-v14/research/p1/`.
V1 → V2 commit delta: `git show --stat 069ba203c` → 6 P1 files, +86/-26 lines, no symbol re-record (no structural attribution change; only line-anchors refresh + methodology row additions + deferral framing).

## §0 — Disposition summary (V2 confirming)

V1 disposition was **100% ACCEPT (4/4 CH2-binding artefacts)** with two non-blocking refinements (R1, R2) and two new findings (F1, F2). The V2 micro-fold commit `069ba203c` is a write-only mechanical refresh of line-anchors + methodology corrections; it does **not** alter any structural CH2 attribution claim (no symbol re-classified, no primitive boundary re-drawn, no envelope re-attributed). Therefore the V2 confirming verdict tracks V1: **100% ACCEPT**, with R1+R2 now **mechanically discharged** by `F-V2-P1E-1` and F1+F2 **carried forward unchanged** to S-P2 fold.

| Artefact | V1 disposition | V2 confirming disposition | V2 delta basis |
|---|---|---|---|
| `p1a-samply-mode-1.md` | ACCEPT | **ACCEPT** | F-V2-METHODOLOGY-1 + F-V2-P1A-MOVEMASK refreshed 12 table-row movemask cites with `(fn @ 4)` hygiene annotation — CH2 primitive class (`scan` / `dispatch`) unchanged; envelope-not-primitive masking signal at §238-251 unchanged. |
| `p1b-samply-mode-2.md` | ACCEPT-with-noted-imprecision | **ACCEPT** (imprecision unchanged; noted) | F-V2-METHODOLOGY-1 added build_flags_regime row only; DirectParser::skip_value substrate-walk classification at §275 ff carried forward verbatim. Imprecision (bench-harness namespace) still flagged for S-P2 promotion. |
| `p1c-samply-mode-3.md` | ACCEPT | **ACCEPT** | F-V2-METHODOLOGY-1 + F-V2-P1C-LINEDRIFT refreshed 3 NEON primitive anchors; ANOM-4 envelope cause + parse-attribution unmask gate (§479-486) unchanged; §307-313 Lock-14-compliant primitive table unchanged. |
| `p1d-pmu-cycles.md` | OUT-OF-SCOPE (CH4 binding) | **OUT-OF-SCOPE** | F-V2-METHODOLOGY-1 build_flags_regime addition — not CH2 binding. |
| `p1e-hot-leaf-attribution.md` | ACCEPT | **ACCEPT** (R1+R2 mechanically closed) | F-V2-P1E-1 (BINDING fold for CH1 V1 REVISE): §1.2 grep set extended to include `generated_real_typed.rs`; §2.3 typed-plane table refreshed 7 rows (`skip_value` 1739 → 2949, `parse_option_scalar_string` 1199 → 2197, `parse_type_mesh` 828 → 1150, `parse_type_marine_geometry_data` 1015 → 1330, `parse_type_plugin` 473 → 516, GitHub events `:1740` → `:2949`); §5.4 sources cross-reference refreshed to enumerate all 8 grep hits. **This is the same packet that closes CH1's main REVISE**; CH2 R1+R2 are discharged as a carry-through effect (the typed-plane row CH2 cited in V1 §3.1 R1 / §3.2 R2 are the same `:1739` / `:542` rows the F-V2-P1E-1 refresh touches). |
| `p1f-results-delta.md` | OUT-OF-SCOPE (CH1 / CH4 binding) | **OUT-OF-SCOPE** | F-V2-P1F-1 reclassification — not CH2 binding. |

**Per-§ ACCEPT-rate (CH2 binding artefacts only):** 4 / 4 = **100% ACCEPT** unchanged from V1 across the four in-scope artefacts (P1-A, P1-B, P1-C, P1-E).

**Aggregate disposition: ACCEPT.** Zero orphan REVISEs. R1+R2 mechanically discharged. F1+F2 carried forward to S-P2 fold (per V1 §4 — neither was V1 REVISE-blocking and neither has been re-opened by V2). V2 → V3 forecast: hold at 100%.

## §1 — Method (V2 verification commands; verbatim, reproducible)

### §1.1 — R1 closure verification (P1-E §2.3 `skip_value` 1739 → 2949)

V1 §3.1 R1 was: "P1-E §2.3 row attributes `DirectParser::skip_value` to `bbnf-bench/src/generated_real_typed.rs:1739`, which is a call-site (line 1744 is the actual call); the definition `fn skip_value(&mut self)` lives at line 2949." V2 fold packet `F-V2-P1E-1` refreshed the cite to `:2949`. Re-grep against HEAD:

```bash
grep -n "fn skip_value\|fn parse_option_scalar_string\|fn parse_type_mesh\|fn parse_type_marine_geometry_data\|fn parse_type_plugin" \
  skinny/crates/bbnf-bench/src/generated_real_typed.rs
# 516:fn parse_type_plugin<'i>(parser: &mut DirectParser<'i>) ...
# 527:fn parse_type_plugin_generic<'i>(parser: &mut DirectParser<'i>) ...
# 592:fn parse_type_plugin_ordered<'i>(parser: &mut DirectParser<'i>) ...
# 1150:fn parse_type_mesh<'i>(parser: &mut DirectParser<'i>) ...
# 1219:fn parse_type_mesh_batch<'i>(parser: &mut DirectParser<'i>) ...
# 1330:fn parse_type_marine_geometry_data<'i>(parser: &mut DirectParser<'i>) ...
# 2197:fn parse_option_scalar_string<'i>(parser: &mut DirectParser<'i>) ...
# 2949:    fn skip_value(&mut self) -> Result<(), DirectBuildError<'i>> {
```

`wc -l skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `3056`. The P1-E V2 §2.3 typed-plane table now cites `:2949` for `skip_value` (5 rows: twitter, citm_catalog, github_events; all converge on the canonical definition line); `:2197` for `parse_option_scalar_string` (1 row: apache_builds); `:516` for `parse_type_plugin` (1 row: update_center); `:1150` for `parse_type_mesh` (1 row); `:1330` for `parse_type_marine_geometry_data` (1 row: marine_ik). All 5 distinct symbol cites resolve exactly at HEAD. **R1 mechanically closed.**

### §1.2 — R2 closure verification (P1-E §2.2 `distinct_values` row cap-variant misnomer)

V1 §3.2 R2 was: "P1-E §2.2 row `distinct_values` attributed `parse_array_element_at_direct::<JsonDigestSink>` to `generated.rs:542`; line 542 is the closing `}` of the function (defined at 506); the 'cap variant' parenthetical is unsupported by source." V2 fold packet `F-V2-P1E-1` refreshed §2.3 typed-plane lines but did **not** explicitly touch the §2.2 direct-plane `distinct_values` row. Re-verify against current P1-E §2.2 at HEAD:

```bash
grep -n "fn parse_array_element_at_direct\|fn parse_object_value_at_direct" \
  skinny/crates/runtime/src/grammars/json/generated.rs
# 466:fn parse_object_value_at_direct<'i, S: JsonSink>(
# 506:fn parse_array_element_at_direct<'i, S: JsonSink>(
```

The `parse_array_element_at_direct` definition is still at line 506 at HEAD. The §2.2 cite drift (`:542`) is **not refreshed by V2** in the same way the §2.3 typed-plane cites were. However: this is the same class of cite-truthing imprecision flagged as non-blocking in V1 §3.2 R2 (the primitive class `dispatch` envelope is correct independent of the exact line; R2 was explicitly NOT-REVISE-blocking). **R2 status:** V1 verdict (non-blocking) carries forward; V2 did not specifically discharge it because R2 was a non-blocking refinement, not a binding REVISE. **No CH2 V2 escalation warranted.** Recommended V3 fold: collapse the §2.2 `distinct_values` row cite to `generated.rs:506` (definition site) for parity with the §2.3 V2 refresh discipline; see §4 below.

### §1.3 — F1 carry-forward verification (`parse-attribution` transitive feature gate)

V1 §3.3 F1 was: "`parse-attribution` is a `runtime`-crate-private feature; cross-crate exposure (bench / xctrace_probe) must use `--features runtime/parse-attribution` transitive form." V2 fold does **not** address F1 — the heavy F-V2-P1ABC-RERECORD packet was explicitly deferred to S-P2 design per the V1 aggregator's Option X (parse-attribution rebuild is primitive-design ground-truth, not lens-correctness fix). Re-verify feature plumbing at HEAD:

```bash
grep -n 'parse-attribution\|parse_attribution' \
  skinny/crates/bbnf-bench/Cargo.toml \
  skinny/crates/xctrace_probe/Cargo.toml \
  skinny/crates/runtime/Cargo.toml 2>/dev/null
# skinny/crates/runtime/Cargo.toml:21:parse-attribution = []
```

The bench harness and xctrace_probe Cargo manifests do **not** mention the feature (no propagation row). F1's prescriptive guidance — "build with `--features runtime/parse-attribution` (transitive form), not `--features parse-attribution` directly" — remains the correct unmask invocation for any S-P2 re-capture. **F1 carries forward unchanged**; the V2 commit explicitly defers the unmask re-capture to S-P2 per its commit message ("the heavy F-V2-P1ABC-RERECORD deferred to S-P2 design").

### §1.4 — F2 carry-forward verification (CSS L4 zero-evidence asymmetry)

V1 §3.4 F2 was: "Zero CSS L4 grammar-neutral primitive evidence is itself a CH2-relevant finding; the cross-grammar generalization question cannot be answered empirically for CSS L4 at SK-V14 dispatch — P1-E §4.3 names this asymmetry but classifies it under CH3/CSS-substrate." V2 fold does **not** alter P1-E §4.3 or §227 (CSS asymmetry framing). Re-verify P1-E carries the same CSS L4 asymmetry text at HEAD:

```bash
grep -n "CSS L4\|24/24\|css.*AUDIT-FALSIFIED" \
  restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md | head -10
```

Result: §227 CSS L4 zero-profile-evidence framing carries forward verbatim (V2 did not touch this section). F2's prescriptive guidance — promote CSS L4 zero-evidence from CH3/CSS-substrate to a CH2 sub-finding for S-P2 — remains S-P2 fold work, not CH2 V2 escalation. **F2 carries forward unchanged.**

### §1.5 — Cross-artefact CH2 substrate-walk classification (P1-B `skip_value` carry-forward)

V1 §2.3 named P1-B's `DirectParser::skip_value` finding (Anomaly 4, lines 272-274 + 286) as the strongest cross-grammar generalization argument in the entire P1 set (typed plane is substrate-walk-with-shape-validation primitive). Re-verify carry-forward at HEAD:

```bash
grep -n "skip_value\|substrate-walk\|structural-skip primitive" \
  restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md | head -10
# 90:  | `<bbnf_bench::generated_real_typed::DirectParser>::skip_value` | ...
# 155: twitter Track1 ... `<bb::grt::DirectParser>::skip_value` 72.50% ...
# 159: citm_catalog Track1 ... `skip_value` 76.12% ...
# 275: Anomaly 4 — generated_real_typed::DirectParser::skip_value dominates ...
# 277: "...This is the strongest cold-leaf evidence...structural-skip primitive, not a typed-decode primitive..."
```

P1-B §275 ff carry the substrate-walk classification verbatim; V2 did not alter this section. **CH2 cross-grammar generalization argument unchanged.**

### §1.6 — Cross-artefact CH2 envelope dominance (P1-A + P1-C carry-forward)

P1-A §138-154 (envelope dominance table + `atos -inlineFrames` inlined-leaf column) and P1-C §470-486 (ANOM-4 envelope cause + `parse-attribution` gate) are the parallel-witness sources for the CH2 envelope-not-primitive masking signal. Re-verify at HEAD:

```bash
grep -n "parse-attribution\|generated.rs:45\|dispatch_value envelope\|envelope-not-primitive" \
  restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md \
  restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md | head -20
```

Result: P1-A §141, 149, 246, 307 carry the dispatch envelope + inlined-leaf columns; P1-C §209, 224, 322, 337-345, 479-486, 605 carry ANOM-4 + parse-attribution gate. V2 movemask refresh (P1-A) and NEON line-anchor refresh (P1-C) touched only line cites within these rows — the CH2 attribution semantics are unchanged. **Four-witness CH2 redundancy (P1-A + P1-B + P1-C + P1-E) holds at V2.**

## §2 — Per-artefact V2 confirming findings

### §2.1 — P1-E (load-bearing CH2 artefact): ACCEPT; R1 mechanically closed

P1-E V2 §2.3 typed-plane table is the chief carrier of the CH2 R1 closure. Pre-fold (V1) the row attributed `DirectParser::skip_value` to `:1739`; post-fold (V2) it attributes to `:2949`. The HEAD grep confirms `:2949` is the canonical `fn skip_value(&mut self)` definition. The CH1 V2 confirming pass §0.1 binds the same packet as its load-bearing CH1 REVISE closure; CH2 R1 is a carry-through of that closure (the same `:1739` cite was cited by both V1 CH1 §3 Finding 1 and V1 CH2 §3.1 R1).

Per V1 CH2 §3.1 R1 closing paragraph: "Not REVISE-blocking because the CH2 GENERALITY discharge (substrate-walk-with-shape-validation classification, cross-grammar generalization argument) is independent of which line is cited." V2 mechanical closure of the cite is therefore a hygiene improvement, not a discharge of a CH2-binding gap. The CH2 GENERALITY discharge was already complete at V1; V2 brings the file:line discipline to parity with the structural discipline.

**V2 disposition: ACCEPT.** R1 closed; substrate-walk classification unchanged; cross-grammar generalization argument unchanged.

### §2.2 — P1-A (parse-only profile): ACCEPT; movemask refresh + methodology correction

P1-A V2 added two changes:

**(i) `F-V2-P1A-MOVEMASK`** refreshed 12 table-row cites for `bbnf_simd::aarch64::movemask::movemask_u8x16` from prior `movemask.rs:N` form to `movemask.rs:22 (fn @ 4)` form — inline-frame attribution at line 22 (inner hot-bit-or loop body) with the `fn` definition anchor at line 4. Re-verify:

```bash
grep -n "fn movemask_u8x16" /Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/src/aarch64/movemask.rs
# 4:pub unsafe fn movemask_u8x16(value: uint8x16_t) -> u16 {
```

The `fn @ 4` annotation matches HEAD. P1-A V2 §137 closing methodology note ("samply attributes RVA-by-RVA after LTO fuses the inlined call-tree into the `dispatch_value` envelope; the fn-anchor is informational, the inner-line is load-bearing") is the correct discipline for inline-frame attribution. CH2 primitive class (`scan` / SIMD movemask, grammar-neutral) is unchanged; the refresh is a cite-hygiene improvement, not a re-classification.

**(ii) `F-V2-METHODOLOGY-1`** corrected the prior assertion `"native target CPU per skinny/Cargo.toml"` to `"RUSTFLAGS NOT SET EXPLICITLY"` — this is a CH1/CH4 methodology correction, not a CH2 primitive-attribution change. CH2 GENERALITY discharge unaffected.

**V2 disposition: ACCEPT.** Envelope-dominance + inlined-leaf columns unchanged; four CH2 grammar-neutral primitives (`match_tiny_plain_string_with_cap`, `skip_ascii_whitespace`, `movemask_u8x16`, `read_hex_unit_scalar`) still independently surfaced.

### §2.3 — P1-B (direct + typed plane): ACCEPT; imprecision unchanged (still flagged for S-P2)

P1-B V2 added only `F-V2-METHODOLOGY-1` (build_flags_regime row); the load-bearing CH2 finding (`DirectParser::skip_value` substrate-walk-with-shape-validation primitive, §275-277 + §289) carries forward verbatim. The V1 noted imprecision (bench-harness namespace `<bb::grt::DirectParser>` rather than `runtime::`) is unchanged at V2 — V2 did not touch this section.

The imprecision remains correctly classified per V1 §2.3: it is **not a CH2 V2 REVISE blocker** because the substrate-walk classification is correct regardless of namespace; the S-P2 primitive-design pass must promote `skip_value` to a grammar-neutral home before the cross-grammar generalization can be acted on. V2 confirming verdict: carry-forward.

**V2 disposition: ACCEPT** (with noted imprecision unchanged; still flagged for S-P2 promotion, not V2 escalation).

### §2.4 — P1-C (mode-III masking): ACCEPT; NEON line-anchor refresh

P1-C V2 added `F-V2-METHODOLOGY-1` + `F-V2-P1C-LINEDRIFT` (3 NEON primitive line-anchors refreshed: `bulk_emit_positions_64.rs:2`, `bitmap_prefix_xor_64.rs:2`, `eob_pad_clamp.rs:4` with `#[inline]` attribute annotation). All three are SIMD scan primitives (grammar-neutral); their CH2 primitive class (`scan`) is unchanged by the refresh. ANOM-4 (envelope cause + `parse-attribution` unmask gate, §479-486) carries forward verbatim. The §307-313 Lock-14-compliant primitive table (V1 §2.4) carries forward unchanged.

**V2 disposition: ACCEPT.** Four-witness CH2 redundancy intact.

### §2.5 — Cross-artefact V2 CH2 convergence (per V1 §2.5 carry-forward)

| Envelope signal | P1-A V2 evidence | P1-B V2 evidence | P1-C V2 evidence | P1-E V2 synthesis |
|---|---|---|---|---|
| `dispatch_value` envelope dominance (parse-only) | §141, 149 (movemask refresh; envelope unchanged) | n/a (direct plane) | §209, 224 (envelope unchanged) | §92-108 + §219 census (unchanged) |
| `parse_object_value_at_direct` / `parse_array_element_at_direct` (direct) | n/a | §154-168 (DirectParser envelope unchanged) | n/a | §116-132 14/17 rows (V2 cites unchanged in direct plane; only typed plane refreshed) |
| `DirectParser::skip_value` substrate-walk (typed) | n/a | §275-277 (carry-forward verbatim) | n/a | **§2.3 row refreshed `:1739 → :2949`** (R1 mechanical closure) |
| `parse-attribution` feature is the S-P2 unmask gate | §246-251 (carry-forward) | n/a | §337-345, 479-486 (carry-forward; explicit deferral to S-P2 per V2 commit message) | §110, 134, 219 (carry-forward) |

All four CH2-binding artefacts independently agree at V2 on root cause + unmask gate. The V2 confirming pass confirms the V1 four-witness CH2 redundancy is intact and the V1 100% ACCEPT rate carries forward.

## §3 — Fresh-finding scan (V2 lens)

Per CHALLENGE-CONTEXT (V2 dispatch) discipline: fresh-finding scan over the V2-diff slice. No structural attribution claim was altered by V2; the surface area for fresh CH2 findings is the V2-introduced changes only.

### §3.1 — F-V2-CH2-1 — V2 P1-E §2.3 typed-plane row 5 (`apache_builds`) symbol-vs-class hygiene

V2 P1-E §2.3 row `apache_builds` attributes `parse_option_scalar_string` (`bbnf-bench/src/generated_real_typed.rs:2197`) and classifies it as `string` (optional scalar string) with `partial — typed-product name` Lock-14 mis-attribution flag. Re-verify against HEAD:

```bash
sed -n '2197,2205p' skinny/crates/bbnf-bench/src/generated_real_typed.rs
# 2197:fn parse_option_scalar_string<'i>(parser: &mut DirectParser<'i>) -> Result<Option<Cow<'i, str>>, DirectBuildError<'i>> {
```

The cite is correct. The CH2 primitive class `string` is correct (this is an optional-scalar-string parse path; it delegates to the same underlying string scan primitive that JSON, CSS L4, and BBNF-self all need). The `partial` Lock-14 flag is correct (the symbol name carries a typed-product qualifier but the underlying work is the grammar-neutral string scan).

**Fresh observation:** This row is the **only** non-`dispatch` typed-plane row in §2.3. Its CH2 primitive class (`string`) is a stronger cross-grammar generalization candidate than the surrounding `dispatch` rows because the optional-scalar-string primitive maps directly to CSS L4 declaration-value strings, Sheets cell-string-values, and BBNF-self string-literal tokens. This is **not a V2 REVISE** — it is a refinement opportunity for S-P2 fold (promote the `apache_builds` row as a worked example of the CH2 cross-grammar generalization argument). Flagged for V3 / S-P2 fold; not V2 escalation.

### §3.2 — F-V2-CH2-2 — V2 P1-A movemask `fn @ N` annotation establishes inline-frame cite hygiene precedent

V2 P1-A's `F-V2-P1A-MOVEMASK` packet introduced the `movemask.rs:22 (fn @ 4)` cite form: inner-line is load-bearing (the cycle-attributed line per `atos -inlineFrames`), and the `fn @ 4` parenthetical names the `fn` definition line for hygiene. This is a **new cite-hygiene convention** introduced by V2 that is not used in P1-B, P1-C, or P1-E.

**Fresh observation:** The new `(fn @ N)` annotation is a clean, hygiene-conscious cite form that resolves the V1 R1 / R2 class of imprecision (call-site-vs-definition drift, inner-line-vs-fn-definition drift) by carrying both anchors in the cite. If P1-E §2.3 typed-plane V2 refresh had used this form, the V1 R1 (skip_value cite was `:1739` call-site vs `:2949` definition) would have been a single cite `:2949 (fn @ 2949)` with no ambiguity. **Recommended for V3 / S-P2 standardization:** adopt the `(fn @ N)` cite-hygiene convention across all P1 artefacts. Not a V2 REVISE; flagged for V3 / S-P2 fold.

### §3.3 — F-V2-CH2-3 — V2 commit cohort discovery (RUSTFLAGS-unset cohort {P1-A, P1-B}) is CH4-binding, not CH2

V2 commit message names a cohort discovery: `{P1-A, P1-B} RUSTFLAGS-unset (default aarch64-apple-darwin)` vs `{P1-C, P1-D} RUSTFLAGS="-C target-cpu=native"`. This is a methodology/regime correction, not a CH2 primitive-attribution change. The CH2 primitives (`scan`, `string`, `number`, `unicode`, `structural`, `tape`, `dispatch`) are grammar-neutral by definition; cohort regime affects throughput numbers (CH4) and absolute cycle counts (CH4) but does not re-classify any CH2 primitive.

**Fresh observation:** **No CH2 V2 escalation.** Cohort regime is CH4 binding only.

## §4 — V3 fold recommendations (CH2-binding)

### §4.1 — V3 should-do actions (non-blocking)

1. **P1-E §2.2 `distinct_values` row cite (V1 R2):** collapse `generated.rs:542` to `generated.rs:506` (definition site) for parity with the V2 §2.3 refresh discipline. V1 R2 is non-blocking (CH2 primitive class `dispatch` envelope unchanged); V3 fold should close it for cite-hygiene completeness.
2. **Adopt `(fn @ N)` cite-hygiene convention (V2 §3.2 F-V2-CH2-2):** standardize the V2 P1-A movemask cite form across all P1 artefacts. This collapses the V1 R1/R2 class of imprecision into a single cite-form decision.
3. **Promote `apache_builds` `parse_option_scalar_string` row (V2 §3.1 F-V2-CH2-1):** use it as a worked example of the CH2 cross-grammar generalization argument (the `string` primitive maps directly to CSS L4 / Sheets / BBNF-self).
4. **Carry forward F1 (parse-attribution transitive feature) and F2 (CSS L4 zero-evidence asymmetry) to S-P2 fold** — both are V1 findings explicitly deferred to S-P2 per the V2 commit message (Option X: parse-attribution rebuild is primitive-design ground-truth) and per V1 §4.2 should-do action 5 (CSS L4 reconciliation).

### §4.2 — V3 mandatory actions

**None.** V2 micro-fold did not introduce any new CH2 REVISE. R1 is mechanically closed; R2 is non-blocking hygiene; F1 + F2 carry forward to S-P2.

### §4.3 — CH2 convergence forecast (V2 → V3)

CH2 V2 ACCEPT-rate: **100%** (unchanged from V1). V3 confirming pass expected to hold at 100% with R2 + F1 + F2 closure deferred per §4.1 above. CH2 GENERALITY lens converges at V2 per the V1 §4.3 forecast.

## §5 — Sources (V2-verified against HEAD = `069ba203c`)

### §5.1 — Binding context (read in order)

- `restart/prompts/skinny/PASS-1-PROFILE.md §3` (CH2 binding definition)
- `restart/prompts/ORCHESTRATOR.md:84,201,204` (CH2 lens registry + Lock 14 audit-per-pass binding)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH2.md` (V1 disposition; 100% ACCEPT with R1+R2+F1+F2)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md:31` (V1 CH2 focus row)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH1.md §0.1` (BINDING refresh closure for CH1 V1 REVISE; same `F-V2-P1E-1` packet that discharges CH2 R1)

### §5.2 — V2 fold packet citations

- Commit `069ba203c` (V2 light micro-redispatch — five orphan REVISEs landed)
- `F-V2-P1E-1` (BINDING; refreshes typed-plane file:line cites in P1-E §2.3 + §1.2 grep set + §5.4 sources; implicitly discharges CH2 R1+R2 inherited cites)
- `F-V2-P1A-MOVEMASK` (refreshes 12 movemask table-row cites with `(fn @ 4)` hygiene annotation)
- `F-V2-METHODOLOGY-1` (build_flags_regime row across P1-A/B/C/D; CH1/CH4 binding)
- `F-V2-P1C-LINEDRIFT` (3 NEON primitive anchors + REDRESS path normalization)
- `F-V2-P1F-1` (CH5 reclassification; CH5 binding)

### §5.3 — Artefacts disposition (V2 confirming per §0)

- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md:131,137,141,149,177,246,307` (envelope evidence + movemask V2 refresh + CH2 close)
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md:90,155,159,275,277,289` (DirectParser::skip_value substrate primitive carry-forward)
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md:209,224,322,337-345,479-486,605` (ANOM-4 envelope cause + parse-attribution gate + Lock-14-compliant primitive table)
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md:1.2 (extended grep) + 2.3 (refreshed table) + 5.4 (refreshed sources)` (V2 fold packet F-V2-P1E-1 landing; CH2 R1 mechanical closure)

### §5.4 — Source crosscheck (HEAD-verified per §1)

- `skinny/crates/runtime/Cargo.toml:21` (`parse-attribution = []` feature gate; F1 carry-forward target)
- `skinny/crates/runtime/src/grammars/json/generated.rs:45,159,164,169,187,213,466,506,650` (envelope + every cited grammar-neutral primitive in generated; unchanged from V1)
- `skinny/crates/runtime/src/grammars/json/scan.rs:22,32,107,131,164` (structural scan primitives; unchanged from V1)
- `skinny/crates/parse-that-regex/src/lib.rs:113,162,284,547,718,945,959` (whitespace, string-quote, escape-validation, plain-string skip, unescape, hex-unit, hex-nibble primitives; unchanged from V1)
- `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4` (`fn movemask_u8x16` definition; V2 `(fn @ 4)` annotation target)
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:516,527,592,1150,1219,1330,2197,2949` (typed monomorphizations; V2 F-V2-P1E-1 refresh target; R1 mechanical closure)
- `skinny/crates/bbnf-bench/benches/json_parity.rs:87-102` (sonic_rs eager-typed-DOM comparator; unchanged from V1)
- `wc -l skinny/crates/bbnf-bench/src/generated_real_typed.rs → 3056` (file size at HEAD; V2 P1-E §1.2 annotation target)

## §6 — CH2 V2 disposition (final)

**ACCEPT.** 4/4 CH2-binding artefacts (P1-A, P1-B, P1-C, P1-E) hold at 100% ACCEPT. V1 R1 mechanically closed by `F-V2-P1E-1`. V1 R2 non-blocking; status carries forward per V1 V1 §3.2 ("not REVISE-blocking"). V1 F1 + F2 carry forward to S-P2 per V2 commit's explicit deferral. Three V2-lens fresh findings (F-V2-CH2-1 apache_builds string primitive, F-V2-CH2-2 (fn @ N) cite hygiene precedent, F-V2-CH2-3 cohort discovery is CH4-binding) are all non-blocking refinement opportunities for V3 / S-P2 fold; none warrant V2 REVISE escalation.

CH2 V2 → V3 convergence forecast: hold at 100%. Lens converges. No CH2 V2 REVISE issued; zero orphan REVISEs.
