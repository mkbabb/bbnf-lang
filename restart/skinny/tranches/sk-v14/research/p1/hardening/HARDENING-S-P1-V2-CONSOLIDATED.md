# SK-V14 S-P1 Profile — V2 CHALLENGE Consolidated

Aggregator: SK-V14 S-P1 V2 hardening aggregator (write-only).
Date (UTC): 2026-05-23.
Scope: seven-lens CHALLENGE V2 over the V2 light micro-fold landed in
commit `069ba203c413d46e7a5d465a128a983254e53841`
(`docs(sk-v14-p1-profile): V2 light micro-redispatch — five orphan
REVISEs landed`); same six P1 axis artefacts under review as V1
(`p1a-samply-mode-1.md` … `p1f-results-delta.md`) with V2 deltas of
+86/-26 lines across the six files, no symbol re-record.
Authority: `restart/prompts/ORCHESTRATOR.md §3W` (lens registry) + `§3Z`
(convergence rule); `restart/prompts/skinny/PASS-1-PROFILE.md §3`
(CH1-CH6 specialisations + §6 dispatch gate);
`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune
lens binding from S-P0); V1 baseline
`restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
(NOT-CONVERGED-V2-REQUIRED; sub-axis aggregate 91.4 %; three orphan
REVISEs); V2 dispatch context inherited from
`restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md`
§0-§5.
Input ledger: seven V2 lens dispositions under
`restart/skinny/tranches/sk-v14/research/p1/hardening/V2/`
(`CH1.md` 300 lines, `CH2.md` 253, `CH3.md` 320, `CH4.md` 286, `CH5.md`
243, `CH6.md` 326, `CH7.md` 393 — 2121 lens lines).

## §0 — V2 cycle verdict

### §0.1 Per-lens dispositions (verbatim from each V2 CH file)

| Lens | V1 rate | V2 sub-axes | V2 ACCEPT | V2 REVISE | V2 REJECT | V2 rate | V2 verdict |
|---|---:|---:|---:|---:|---:|---:|---|
| CH1 CORRECTNESS | 89.6 % | 115 (25+19+25+14+18+14 per artefact) | 114 | 0 (1 ACCEPT-WITH-NOTE on P1-D §1.1 self-referential line drift, not REVISE) | 0 | **99.13 %** | ACCEPT (V1 BINDING REVISE on P1-E typed-plane mechanically closed; one new V2-introduced cosmetic line-drift annotated, not blocking) |
| CH2 GENERALITY | 100 % | 4 in-scope artefacts (P1-A/B/C/E) | 4 | 0 | 0 | **100 %** | ACCEPT (V1 R1 mechanically closed by F-V2-P1E-1; R2 non-blocking carry-forward; F1+F2 deferred to S-P2; 3 non-blocking observations) |
| CH3 REGRESSION (REDRESS) | 100 % | 6 artefacts × 43 §4 anomalies | 6 | 0 | 0 | **100 %** | ACCEPT (F-1 path normalisation closed in F-V2-P1C-LINEDRIFT; ANOM-1/2/3 + REDRESS-126 pre-block guard intact at shifted line geometry; 5 V2-lens documentary findings, none blocking) |
| CH4 COST | 93.5 % | 31 V1 sub-axes (all lifted) + 18 new V2 disclosure sub-axes | 49 | 0 | 0 | **100 %** | ACCEPT (V1 CF-1 RUSTFLAGS orphan REVISE closed via per-artefact `build_flags_regime` disclosure across P1-A/B/C/D; cohort discovery {P1-A,P1-B}-unset vs {P1-C,P1-D}-native canonical; 4-point refusal lattice encoded) |
| CH5 HIDDEN COUPLING | 83 % | 6 artefacts | 6 | 0 | 0 | **100 %** | ACCEPT (V1 P1-F orphan REVISE reclassified to ACCEPT-WITH-CONTRACTED-DEFERRAL via F-V2-P1F-1 PASS-ALPHA §4.4 precedent; CH5-A cite drift closed by F-V2-P1E-1; CH5-C/CH5-D carried to S-P2 by design; substrate-union framing intact) |
| CH6 ANTI-PAPER-CLOSE | 100 % | 6 artefacts (77/77 V1 flame-path evidence binding) | 6 | 0 | 0 | **100 %** | ACCEPT (F-V2-P1ABC-RERECORD deferral verified as CONTRACTED-DEFERRAL not paper-close by all three discriminators: named cause, named route, named decision precedent; parse-attribution feature plumbing intact at 11+ functions; 1 NEW positive structural finding via F-V2-P1F-1 framing; §3.6 /tmp volatility process-class observation for V3+ consideration) |
| CH7 OVERFIT-PRUNE | 100 % | 7 subclauses (4 explicit + 3 supporting verifications) | 7 | 0 | 0 | **100 %** | ACCEPT (audit-overlay column population strengthened: V1 258 cells → V2 261 cells, +3 from F-V2-P1F-1 §4.1 contracted-deferral framing; 9-grammar census preserved; fake-`@generated` recurrence zero; PRUNE-1 W14.* row consistency preserved at 47 hits; §3Z **CLOSES on CH7 standalone at V2** via V1=100 % + V2=100 % chain) |

### §0.2 Aggregate ACCEPT-rate

Two aggregation methods (per `ORCHESTRATOR.md §3Z`):

- **Sub-axis-weighted (load-bearing for §3Z convergence):**
  (114+4+6+31+6+6+7) / (115+4+6+31+6+6+7) = **174 / 175 = 99.43 %**.
- **Per-lens mean (informational; equal weight per lens):**
  (99.13 + 100 + 100 + 100 + 100 + 100 + 100) / 7 = **99.88 %**.

Both aggregation methods comfortably clear the §3Z ≥95 % floor; net V1→V2 lift is +8.03 pp on sub-axis weighting (91.4 % → 99.43 %) and +4.72 pp on per-lens mean (95.16 % → 99.88 %). V2 is the **first** ≥95 % cycle on both methods.

### §0.3 REJECT roster

**Zero REJECT findings** across all 7 V2 lenses. V2 introduces no falsification of any P1 axis claim; the V1 REJECT-zero posture is preserved.

### §0.4 REVISE roster (orphan accounting)

**Zero orphan REVISEs at V2.** All three V1 orphan REVISEs were routed through the five V2 light packets and verified closed in lens reports:

| V1 orphan REVISE | V2 fold packet | V2 closure verification | V2 status |
|---|---|---|---|
| CH1 P1-E typed-plane file:line drift (`skip_value :1739 → :2949` +1210, etc.; 5 stale typed-plane cites) | **F-V2-P1E-1** (BINDING) | CH1 V2 §0.1 — 7 typed-plane rows refreshed against HEAD `generated_real_typed.rs` (3056 lines); 8 grep hits enumerated at `:516/527/592/1150/1219/1330/2197/2949`; §1.2 grep set extended to include the typed-plane file; CH2 R1/R2 + CH5 CH5-A carry-through cites mechanically discharged | **CLOSED** |
| CH4 CF-1 RUSTFLAGS regime drift (P1-A asserted "native per Cargo.toml" but no shell carries it; P1-B explicit unset; P1-C/D explicit native — silent cross-row drift) | **F-V2-METHODOLOGY-1** (Option A LIGHT) | CH4 V2 §3.1 — `build_flags_regime` row landed in all four P1-A/B/C/D frontmatters; cohort lattice canonical at V2 ({P1-A, P1-B} unset; {P1-C, P1-D} native); 4-point cross-regime refusal rule encoded; P1-A V1 misstatement explicitly corrected with `skinny/Cargo.toml:78-86` Cargo evidence | **CLOSED** |
| CH5 P1-F `track2_entry_point` schema-column gap (CH5 hidden-coupling guard absent from RESULTS.md; 0 matches across 186 lines) | **F-V2-P1F-1** (reclassification) | CH5 V2 §2 — 5-paragraph contracted-deferral framing block landed at `p1f-results-delta.md:179-186`; cites SYNTHESIS §2:240-242/255 declarations + §3 C-2:272 wave deliverable + PASS-ALPHA §4.4:112-122 precedent; V1 REVISE reclassifies to ACCEPT-WITH-CONTRACTED-DEFERRAL (C-2 wave deliverable, not P1 attribution failure) | **CLOSED-VIA-CONTRACTED-DEFERRAL** |

**Two additional light packets landed (closing V1 ACCEPT-WITH-NOTE items):**

| V1 ACCEPT-WITH-NOTE | V2 fold packet | V2 closure verification | V2 status |
|---|---|---|---|
| CH1 P1-C NEON line-anchors off-by-one (`bulk_emit_positions_64_neon` 3→2; `bitmap_prefix_xor_64_neon` 3→2; `eob_pad_clamp_neon` 5→4) + CH3 F-1 REDRESS path drift (`restart/skinny/REDRESS.md` → `skinny/REDRESS.md` on p1c :500 + :590) | **F-V2-P1C-LINEDRIFT** | CH1 V2 §0.3 — 3 NEON cites refreshed with `(fn signature; #[inline] attribute at line N-1)` annotation; CH3 V2 §2 — both REDRESS path normalisations landed at post-shift anchors `:509 + :599` (verified by git diff and zero residual `restart/skinny/REDRESS.md` matches across all six P1 artefacts) | **CLOSED** |
| CH1 P1-A `movemask_u8x16` + `match_tiny_plain_string_with_cap` inner-loop attribution annotation | **F-V2-P1A-MOVEMASK** | CH1 V2 §0.2 — "Line-anchor convention" paragraph at `:137` + 12 table cells annotated with `(fn @ N)` form; `movemask.rs:22 (fn @ 4)` correctly captures inner-loop hot-bit-or at line 22 with fn anchor at 4; `generated.rs:160,176 (fn @ 169)` captures call-site + inner-loop + fn-anchor in one cite | **CLOSED** |

### §0.5 V2-introduced non-REVISE findings (V3 process candidates, not orphans)

Three non-blocking observations surfaced during V2 confirming; all are documentary / process-class and none warrants a V3 micro-fold:

1. **CH1 P1-D §1.1 self-referential line drift** (CH1 V2 §0.5 + §2.4 + §3 Finding 1). The V2 fold inserted an 11-line `build_flags_regime` block at frontmatter lines 21-30, shifting downstream content +11. The §1.1 confirmation block at `:81-89` cites the two `cargo build` invocations as "lines 41 and 62", but at HEAD those invocations are at `:53` and `:74`. The substance (both invocations carry `RUSTFLAGS="-C target-cpu=native"` verbatim) is correct at HEAD; only the citation line numbers are stale by +11/+12 because the V2 author worked off the pre-V2 line numbering. Two paragraphs to refresh (`s/41/53/` + `s/62/74/`) — 30-second find/replace; if folded in V3 would lift CH1 from 99.13 % to 100 % × 6/6. ACCEPT-WITH-NOTE, not REVISE.

2. **CH6 §3.6 volatile /tmp filesystem state** (process-class). The V1 path-existence verifications (77/77 flame profiles + xctrace cpu-state.xml + identity manifests under `/tmp/skv14-p1*/`) cannot be re-executed at V2 confirming because macOS wipes `/tmp` between agent sessions; the V1 CH6 §1.1-§1.4 evidence tables stand as the binding path-existence record. For future hardening cycles, consider relocating sidecar `.syms.json` + `identity.txt` artefacts to a non-volatile location (e.g. `restart/skinny/tranches/sk-v14/research/p1/artefacts/`). Process-class; not CH6-blocking at V2 or V3.

3. **CH7 §3.3 cargo-metadata command-portability hygiene** + **CH2 §3.2 F-V2-CH2-2 cite-hygiene convention standardization**. The verbatim `cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'` returns 9 grammars from repo root but `null` from `skinny/` (inner workspace declares grammars as a map keyed by `json` only). Recommend the V3 confirming or aggregator annotate with `cd /Users/mkbabb/Programming/bbnf-lang` prefix or `--manifest-path` form. Separately, F-V2-CH2-2 surfaced the V2 P1-A `(fn @ N)` cite-hygiene convention as a worth-standardizing pattern across all P1 artefacts. Cosmetic hygiene; not CH2/CH7-blocking.

### §0.6 Convergence vote

Per `ORCHESTRATOR.md §3Z` (≥95 % × 2 cycles, zero orphan REVISEs):

- **V1 (cycle 1):** sub-axis 91.4 %, per-lens 95.16 %, three orphan REVISEs — DID NOT meet on either sub-clause.
- **V2 (cycle 2):** sub-axis **99.43 %**, per-lens **99.88 %**, **zero orphan REVISEs** — MEETS both sub-clauses; first ≥95 % cycle.
- **V3 (cycle 3):** required to confirm ≥95 % for the second consecutive cycle and to discharge §3Z's "× 2 cycles" sub-clause.

CH6 and CH7 lenses **standalone** satisfy §3Z at V2 (CH6: V1 100 % + V2 100 %, zero REVISE at both; CH7: V1 100 % + V2 100 %, zero REVISE at both — CH7 V2 §0 explicitly records "**§3Z convergence-gate CLOSES on CH7 at V2**"). The remaining five lenses (CH1, CH2, CH3, CH4, CH5) are at first ≥95 % cycle and require V3 confirming for two-cycle discharge.

**Cycle verdict: CONVERGED-EXPECTING-V3-CONFIRM.** V3 pure confirming pass over unchanged V2 artefacts (same 7 lenses re-applied) closes §3Z LOCK at V max=5 ceiling; the heavy F-V2-P1ABC-RERECORD remains correctly deferred to S-P2 per V1 aggregator Option X.

## §1 — V1 → V2 fold landing matrix

Five V2 light packets prescribed in `HARDENING-S-P1-V1-CONSOLIDATED.md §2`; all five verified landed in V2 commit `069ba203c`:

### §1.1 — F-V2-P1E-1 (CH1 BINDING REVISE closure) — LANDED

**Closes:** CH1 V1 orphan REVISE (Finding 1 on P1-E typed-plane file:line drift); CH2 V1 §3.1 R1 + §3.2 R2 (carry-through cite drift); CH5 V1 §3 Finding CH5-A (source-line drift on `DirectParser::skip_value`).

**Verified landing (CH1 V2 §0.1, CH2 V2 §1.1, CH5 V2 §2):**
- `p1e-hot-leaf-attribution.md §1.2` grep set extended at `:63-76` to include `generated_real_typed.rs` (the file V1 missed).
- §2.3 typed-plane table refreshed: 7 rows updated with verbatim HEAD-grep lines (`twitter / citm_catalog / github_events` all → `:2949`; `apache_builds` → `:2197`; `update_center` → `:516`; `mesh` → `:1150`; `marine_ik` → `:1330`).
- §5.4 Sources cross-reference refreshed to enumerate all 8 grep hits with per-suffix tags (`516 [_plugin]`, `527 [_generic]`, `592 [_ordered]`, `1150 [_mesh]`, `1219 [_batch]`, `1330 [_marine_geometry_data]`, `2197 [parse_option_scalar_string]`, `2949 [DirectParser::skip_value]`).
- V2 fold note at `:15` explicitly tagging the micro-fold as a CH1 V1 REVISE closure with implicit CH2 R1+R2 + CH5 CH5-A discharge.

**Convergence impact:** CH1 P1-E lifts from 67 % → 100 % (+33 pp); CH1 aggregate sub-axis weighted lifts from 89.6 % to ≈99.1 % via this packet alone. CH2 R1 mechanically closes; CH5-A discharges. **Mechanical closure verified by HEAD grep against `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (`wc -l` = 3056); all 5 BINDING REVISE rows from V1 §3 Finding 1 closed.**

### §1.2 — F-V2-METHODOLOGY-1 (CH4 CF-1 closure) — LANDED

**Closes:** CH4 V1 orphan REVISE (CF-1 RUSTFLAGS regime drift affecting P1-A + P1-B).

**Verified landing (CH4 V2 §2 + §3.1):**
- `build_flags_regime` row landed at P1-A `:10`, P1-B `:10`, P1-C `:17-25`, P1-D `:21-31` (all four primary-capture frontmatters).
- Cohort lattice canonical at V2: {P1-A, P1-B} → `RUSTFLAGS-unset` (default aarch64-apple-darwin baseline); {P1-C, P1-D} → `RUSTFLAGS="-C target-cpu=native"`.
- P1-A misstatement explicitly **corrected** at `:10` from V1's "native target CPU per skinny/Cargo.toml" to "RUSTFLAGS NOT SET EXPLICITLY (default aarch64-apple-darwin baseline; native-CPU NOT pinned)"; anchored on direct Cargo evidence — `skinny/Cargo.toml:78-86` `[profile.release]` block does **NOT** carry `target-cpu` (verified by grep this turn).
- 4-point cross-regime refusal rule encoded at P1-A `:10`, P1-B `:10` + `:185`, P1-C `:23-25`, P1-D `:21-31`; all four cite the canonical `twitter` Track 1 direct comparator example (P1-B 11037 Mbps vs P1-D 11627 Mbps; 5.3 % Mbps + 2.1 % c/B drift) as the refusal target.
- P1-A cohort discovery is itself the V2 micro-redispatch contribution: V1 (erroneously) implied P1-A sat in the native cohort by virtue of "per Cargo.toml"; V2 corrects this to RUSTFLAGS-unset, matching P1-B's explicit unset disclosure.

**Convergence impact:** CH4 lifts from 93.5 % to 100 % (CF-1 closes via per-row regime disclosure; cross-artefact comparator refusal is the consumer-side enforcement); P1-A `Build flags` sub-axis + P1-B `Build flags` sub-axis both lift REVISE → ACCEPT.

### §1.3 — F-V2-P1F-1 (CH5 REVISE reclassification) — LANDED

**Closes:** CH5 V1 orphan REVISE (Finding CH5-B on P1-F `track2_entry_point` schema-column gap; §0 disposition table P1-F row).

**Verified landing (CH5 V2 §2 + §3, CH7 V2 §3.2):**
- 5-paragraph "Contracted-deferral framing (V2 reclassification)" block landed at `p1f-results-delta.md:179-186`.
- Cites SYNTHESIS.md §2:232-258 (column declarations with `**NEW (CH5)**` / `**NEW (R1)**` / `**NEW (R2)**` / `**NEW (audit overlay)**` annotations); SYNTHESIS.md §3 row C-2:272 (R1+R2 wave deliverable + `xtask gate-json` consumer); PASS-ALPHA.md §4.4:112-122 (Pass Alpha contracted-deferral pattern verbatim: "This layer is authored downstream by skinny pass S-P3 in `sk-v{N+1}/SPEC.md`").
- Reclassification correctly extends to all four NEW SK-V14 schema columns (`comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, `track2_entry_point`) — not just the one Track 2 entry-point column.
- The CH5 narrative coverage stands: Track 1 entry-points (`parse_object_value_at_direct::<JsonDigestSink>`, `parse_only`) and Track 2 entry-points (`DirectParser::skip_value` at `generated_real_typed.rs:2949`, `HandParser::value`) remain distinguishable by inspection, sharing no `runtime::tape::*` ancestor beyond the public types.

**Convergence impact:** CH5 lifts from 83 % to 100 % (orphan REVISE reclassifies as contracted-deferral, not unresolved). CH7 audit-overlay cell count strengthens (V1 258 → V2 261, +3 cells from the contracted-deferral framing paragraph reinforcing per-row column population). CH6 reads the F-V2-P1F-1 block as **CH6-positive** — it establishes the corpus-level textual vocabulary for distinguishing contracted-deferral from paper-close, parallel to the same precedent the V1 aggregator Option X invokes for the heavy F-V2-P1ABC-RERECORD deferral.

### §1.4 — F-V2-P1C-LINEDRIFT (CH1 NEON + CH3 path) — LANDED

**Closes:** CH1 V1 Finding 2 (3 NEON line off-by-ones); CH3 V1 Finding F-1 (2 REDRESS.md path drift cites at p1c §500 + §590); CH6 V1 §4 Finding 3 (path-layout standardisation, partial).

**Verified landing (CH1 V2 §0.3, CH3 V2 §2):**
- 3 NEON cites refreshed at `p1c-samply-mode-3.md` post-shift anchors: `bulk_emit_positions_64_neon :3 → :2 (fn signature; #[inline] attribute at line 1)`; `bitmap_prefix_xor_64_neon :3 → :2`; `eob_pad_clamp_neon :5 → :4`. All three verified by HEAD grep against `skinny/crates/bbnf-simd/src/aarch64/{bulk_emit_positions_64,bitmap_prefix_xor_64,eob_pad_clamp}.rs`.
- REDRESS path normalised `restart/skinny/REDRESS.md` → `skinny/REDRESS.md` at p1c `:509 + :599` (V1 anchors were `:500 + :590`; +9-line shift from the `build_flags_regime` block insertion at the head). Verified by git diff (exactly two `restart/skinny/REDRESS.md → skinny/REDRESS.md` edits in the V2 commit) + `ls skinny/REDRESS.md` (canonical path exists; `restart/skinny/REDRESS.md` does not and never has).
- Zero `restart/skinny/REDRESS.md` cites remain in any of the six P1 axis artefacts at V2.

**Convergence impact:** CH1 P1-C lifts from 84 % to 100 % (+16 pp); CH3 V1 ACCEPT-WITH-NOTE on P1-C cleared to ACCEPT (CH3 V2 §8: "V1 ACCEPT-WITH-NOTE cleared"). The annotation convention ("`fn` signature; `#[inline]` attribute at line N-1") codifies the samply RVA-to-line attribution policy explicitly.

### §1.5 — F-V2-P1A-MOVEMASK (CH1 ACCEPT-WITH-NOTE closure) — LANDED

**Closes:** CH1 V1 ACCEPT-WITH-NOTE on P1-A `movemask_u8x16` + `match_tiny_plain_string_with_cap::<16>` inner-loop attribution (P1-A V1 92 % → V2 100 %).

**Verified landing (CH1 V2 §0.2):**
- "Line-anchor convention" paragraph landed at `p1a-samply-mode-1.md:137` codifying the samply RVA-to-line attribution policy ("samply attributes RVA-by-RVA after LTO fuses the inlined call-tree into the `dispatch_value` envelope; the fn-anchor is informational, the inner-line is load-bearing").
- 12 table cells annotated with `(fn @ N)` convention. `movemask.rs:22 (fn @ 4)` captures inner-loop hot-bit-or at line 22 + fn definition anchor at 4. `generated.rs:160,176 (fn @ 169)` captures call-site + inner-loop + fn-anchor in one cite.
- All cites verified by HEAD grep against `skinny/crates/bbnf-simd/src/aarch64/movemask.rs` (`fn movemask_u8x16` at line 4) + `skinny/crates/runtime/src/grammars/json/generated.rs` (3 `match_tiny_plain_string*` variants at 159/164/169).

**Convergence impact:** CH1 P1-A lifts from 92 % to 100 % (+8 pp). The annotation convention is forward-applicable to all P1 artefacts (CH2 V2 §3.2 F-V2-CH2-2 flags it as a worth-standardizing pattern).

### §1.6 — Aggregate landing verification

All five V2 light packets landed; the V2 commit body cites four of them explicitly (F-V2-P1E-1, F-V2-METHODOLOGY-1, F-V2-P1F-1, F-V2-P1C-LINEDRIFT, F-V2-P1A-MOVEMASK) and the V2 hardening lens reports independently verify each landing against HEAD source via `grep`, `wc -l`, and `git diff`. Aggregate +86/-26 line delta across the six P1 axis files; no symbol re-record performed (V2 was a write-only mechanical refresh, not a re-capture cycle).

The heavy **F-V2-P1ABC-RERECORD** packet is correctly **NOT landed in V2** per V1 aggregator Option X recommendation (deferred to S-P2 entry artefact as primitive-design ground-truth; parse-attribution Cargo feature gate verified intact at 11+ functions in `skinny/crates/runtime/src/grammars/json/generated.rs` per CH6 V2 §1.2, ready for S-P2 demand).

## §2 — V3 disposition

Per `ORCHESTRATOR.md §3Z` V max=5 ceiling, the V2 verdict CONVERGED-EXPECTING-V3-CONFIRM requires V3 to discharge the "× 2 cycles" sub-clause on the five lenses currently at first ≥95 % cycle (CH1, CH2, CH3, CH4, CH5). CH6 and CH7 already discharge §3Z standalone at V2.

### §2.1 Option A (RECOMMENDED) — V3 pure confirming pass

V3 dispatch as a pure confirming pass over unchanged V2 artefacts; same 7 lenses re-applied; V2 + V3 closes §3Z LOCK; +30 min wall.

**Forecast (CH1 V2 §5, CH2 V2 §4.3, CH3 V2 §6, CH4 V2 §5, CH5 V2 §5, CH6 V2 §4-5, CH7 V2 §4.3):**

| Lens | V2 rate | V3 expected | Notes |
|---|---:|---:|---|
| CH1 | 99.13 % | 100 % | P1-D §1.1 self-referential line drift §3 Finding 1 closes via 30-second `s/41/53/` + `s/62/74/` find/replace (CH1 V2 §4.1) |
| CH2 | 100 % | 100 % | Hold; R2 + F1 + F2 deferred to S-P2 per V1 §4.2 + V2 §4.1 |
| CH3 | 100 % | 100 % | Hold; lens closed in substance at V2 (CH3 V2 §6: "V3 carries no CH3 work") |
| CH4 | 100 % | 100 % | Hold; CF-V2-2 (schema-level refusal binding) is pre-S-P2 hook, not V3 blocker |
| CH5 | 100 % | 100 % | Hold; CH5-C + CH5-D carried to S-P2; substrate-union framing intact |
| CH6 | 100 % | 100 % | Hold; CH6 already two-cycle converged at V2 |
| CH7 | 100 % | 100 % | Hold; CH7 already two-cycle converged at V2 |

V3 expected sub-axis-weighted aggregate: **≥99.4 %** (≈99.4 % minimum hold; potentially 100 % if CH1 V3 micro-folds the P1-D self-referential line drift). V3 expected per-lens mean: **100 %** (if CH1 lifts to 100 %) or **99.88 %** (hold without CH1 lift). Both above §3Z floor for the second consecutive cycle.

**Convergence forecast:** V2 + V3 chain closes §3Z two-consecutive-cycle requirement on all 7 lenses; V3 is the binding LOCK cycle. No V4/V5 expected; V max=5 ceiling not reached.

### §2.2 Option B (NOT RECOMMENDED) — V3 belt-and-braces fold

Alternative: V3 micro-folds the 3 non-blocking process observations (§0.5) — CH1 P1-D line drift mechanical refresh, CH6 §3.6 /tmp non-volatile relocation, CH2 F-V2-CH2-2 cite-hygiene convention standardization across all P1 artefacts, CH7 §3.3 cargo metadata `cd` prefix annotation. Adds ~10 min micro-fold work plus the V3 confirming overhead; would lift V2's lone CH1 ACCEPT-WITH-NOTE to ACCEPT for a clean 100 % × 6/6 on CH1.

**Trade-off:** Option B routes process observations into V3 rather than S-P2 first wave; saves ~10 min downstream but conflates V3's confirming role with a micro-fold extension. Per the V1 aggregator's discipline (HARDENING-S-P1-V1-CONSOLIDATED §3.2: "lens-correctness convergence axis (mechanical and bounded) vs primitive-census axis (open-ended; S-P2's contract)"), V3 should remain bounded as a confirming cycle.

### §2.3 Recommendation

**Option A — V3 pure confirming pass.** The three §0.5 observations are V2-introduced process notes, not orphan REVISEs that would block V3 closure; they correctly route to S-P2 as part of the broader S-P2 first-wave bundle (alongside the heavy F-V2-P1ABC-RERECORD). V3 dispatches the same 7 lenses against the unchanged V2 artefacts; expected aggregate ≥99.4 % with zero new orphan REVISEs; V2 + V3 closes §3Z LOCK on all 7 lenses at V max=5 ceiling. S-P2 dispatch then opens per `PASS-1-PROFILE.md §6` + SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP, absorbing both the heavy re-record and the cosmetic process observations as its first wave.

## §3 — Convergence forecast + S-P2 readiness

### §3.1 V2 + V3 chain closes §3Z

Per the convergence rule "≥95 % × 2 cycles, zero orphan REVISEs" applied per-lens (`ORCHESTRATOR.md §3Z`):

- **CH6:** V1 100 % + V2 100 %, zero REVISE both cycles → **§3Z CLOSED at V2** (CH6 V2 §5).
- **CH7:** V1 100 % + V2 100 %, zero REVISE both cycles → **§3Z CLOSED at V2** (CH7 V2 §4.3 explicit "convergence-gate CLOSES at V2").
- **CH1, CH2, CH3, CH4, CH5:** V1 below floor (89.6/100/100/93.5/83) or first ≥95 % cycle pending; V2 first ≥95 % cycle (99.13/100/100/100/100) → V3 second consecutive cycle required to close §3Z.

V3 confirming forecast: all 5 currently-pending lenses hold at ≥99 % with zero new orphan REVISEs; V2 + V3 discharges "× 2 cycles" on all 7 lenses at V max=5 ceiling. **S-P2 dispatch gate opens at V3 close** per `PASS-1-PROFILE.md §6` + `ORCHESTRATOR.md §3Z`.

### §3.2 S-P2 entry artefacts

S-P2 inherits three categories of input:

**(a) S-P1 profile artefacts (6 P1 files at V2 HEAD `069ba203c`):**
- `p1a-samply-mode-1.md` (343 lines; parse_only × 17 corpora; atos pipeline; LTO-fused dispatch_value envelope; V2 movemask annotation + RUSTFLAGS-unset cohort disclosure)
- `p1b-samply-mode-2.md` (323 lines; direct × 17 + typed × 11; 56 profiles; DirectParser::skip_value typed-plane substrate-walk finding; V2 RUSTFLAGS-unset cohort + cross-regime refusal guard)
- `p1c-samply-mode-3.md` (616 lines; mode-III × 17 × 4 probes; 8 ANOMs incl. alternate_scalar_plan misnaming; V2 RUSTFLAGS-native cohort + 3 NEON line-anchor refresh + REDRESS path normalisation)
- `p1d-pmu-cycles.md` (669 lines; 231 PMU rows; cycles+inst REACHABLE; PMC counters UNREACHABLE; V2 RUSTFLAGS-native cohort + two-target-dir regime re-confirmation)
- `p1e-hot-leaf-attribution.md` (321 lines; CH2 Lock-14 mis-attribution census; V2 typed-plane file:line refresh on `generated_real_typed.rs`; substrate-union §4.4 paragraph intact)
- `p1f-results-delta.md` (269 lines; 75 rows; 8 schema escalations; V2 contracted-deferral framing for 4 NEW SK-V14 schema columns)

**(b) Heavy F-V2-P1ABC-RERECORD packet** (first wave deliverable per V1 aggregator Option X):
1. Rebuild `xtask`, `bbnf-bench`, `xctrace_probe`, `profile_direct` with `cargo build --release -p bbnf-bench --features runtime/parse-attribution` (transitive form per CH2 F1; feature lives at `skinny/crates/runtime/Cargo.toml:21`; bench-harness propagates through dep declaration).
2. Re-record P1-A samply (17 corpora × 1 plane = 17 profiles) under `--features runtime/parse-attribution`. Re-extract top-N tables.
3. Re-record P1-B samply (17 direct + 11 typed = 28 corpora × 2 planes = 56 profiles). Re-extract.
4. Re-record P1-C samply (4 mode-III probes). Re-extract.
5. Re-record `github_events parse_only` Track 1 with longer iter count (target ≥4000 samples) to crack the 8-sample inlined-std `<u16 as From<u8>>::from` noise envelope.
6. Append the `parse-attribution=on` top-N decomposition tables to P1-A §2.1, P1-B §2.1+§2.2, P1-C §2.1+§2.2 — converting CH6 "named + routed" status to "named + routed + executed".

Plumbing intact at V2 HEAD (CH6 V2 §1.2 verified): 11+ functions gated with `#[cfg_attr(feature = "parse-attribution", inline(never))]` in `skinny/crates/runtime/src/grammars/json/generated.rs` at lines 33-34/43-44/58-59/79-80/86-87/117-118/138-139/157-158/185-186/201-202/211; feature declaration at `runtime/Cargo.toml:21`. Route executable on S-P2 demand.

Cost estimate (wall, single-host sequential per V1 §2.6): ≈135 min sequential; ≈60 min parallel (3 captures concurrent on independent target dirs).

**(c) 3 CH7 V2 + V2 process-class observations** (S-P2 first-wave grooming items per §0.5):
1. CH7 §3.3 cargo-metadata cd prefix (working-directory annotation on the verbatim grammar-census command).
2. CH7 §4.2.1 fake-`@generated` CI lint extension (distinguish documentary citations from load-bearing claims).
3. CH6 §3.6 /tmp non-volatile relocation (relocate `.syms.json` + `identity.txt` sidecars to `restart/skinny/tranches/sk-v14/research/p1/artefacts/` so future hardening cycles can re-verify path existence).

### §3.3 No CH-driven blockers on S-P2 dispatch

V2 confirms no lens surfaces a primary-capture falsification, primitive mis-classification, REDRESS-family silent re-open, methodology-irreproducibility, hidden-coupling (parallel substrate / sidecar / Track 1≡Track 2 collapse), paper-close pattern, or overfit-pattern recurrence. The S-P1 P1 axis artefacts hold as a sound input substrate for S-P2 primitive design.

CH6 explicitly records (V2 §5): "**CH6 gate is OPEN for S-P2 dispatch.** No CH6 finding blocks S-P2; the contracted-deferral of F-V2-P1ABC-RERECORD to S-P2 entry artefact is CH6-defensible (§3.1) and CH6-positive structurally (§3.5)."

CH7 explicitly records (V2 §4.4): "The S-P1 V2 outputs are CH7-CONVERGED for §3Z on this lens. … **No CH7-driven blocker on S-P2 dispatch.**"

The remaining five lenses (CH1, CH2, CH3, CH4, CH5) record analogous "no V3 blocker, lens converges, S-P2 ready" verdicts subject to the V3 confirming pass discharging "× 2 cycles". Once V3 lands per §2.3 Option A recommendation, S-P2 dispatch opens per `PASS-1-PROFILE.md §6` + the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP.

## §4 — Sources

V2 lens dispositions (all verified existing at write-time):
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH1.md` (300 lines; 99.13 %; new ACCEPT-WITH-NOTE on P1-D §1.1 self-referential line drift, not REVISE)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH2.md` (253 lines; 100 %; R1 mechanically closed; R2/F1/F2 carry-forward to S-P2)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH3.md` (320 lines; 100 %; F-1 closed; ANOM-1/2/3 + REDRESS-126 pre-block intact at shifted geometry)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH4.md` (286 lines; 100 %; CF-1 closed; cohort lattice canonical; 4-point refusal rule encoded)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH5.md` (243 lines; 100 %; CH5-B reclassifies via contracted-deferral; CH5-A closes via cite refresh)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH6.md` (326 lines; 100 %; F-V2-P1ABC-RERECORD verified CONTRACTED-DEFERRAL; §3Z CLOSED on CH6 at V2)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V2/CH7.md` (393 lines; 100 %; audit-overlay strengthened V1 258 → V2 261 cells; §3Z CLOSED on CH7 at V2)

V2 P1 axis artefacts under review (HEAD = `069ba203c413d46e7a5d465a128a983254e53841`; V1→V2 delta +86/-26 lines, no symbol re-record):
- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` (V2: 343 lines)
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` (V2: 323 lines)
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` (V2: 616 lines)
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` (V2: 669 lines)
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md` (V2: 321 lines)
- `restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md` (V2: 269 lines)

V1 baseline + dispatch context:
- `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md` (546 lines; V1 verdict NOT-CONVERGED-V2-REQUIRED; §0.2 sub-axis 91.4 %, per-lens 95.16 %; §2 V2 fold packet specs; §3.2 Option X heavy deferral)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md` (53 lines; V1 dispatch context — V2 inherits this binding)

Binding authorities:
- `restart/prompts/skinny/PASS-1-PROFILE.md §3` (CH1-CH6 specialisations) + `§6` (S-P2 dispatch gate)
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH1-CH6 lens registry) + `§3Z` (≥95 % × 2 cycles, zero orphan REVISEs convergence rule; V max=5 ceiling)
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune lens definition)
- `restart/prompts/pass-contracts/PASS-ALPHA.md §4.4` (contracted-deferral pattern verified verbatim in F-V2-P1F-1 framing block at p1f-results-delta.md:183)
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md §2` Telemetry Binding (lines 232-258; 4 NEW column declarations: `track2_entry_point` at :240, `comparator_plane` at :241, `per_iter_equality` at :242, `audit_overlay_verdict` at :255) + `§3` C-2 row (line 272; R1+R2 wave deliverable)
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (S-P0 prune list; 74 findings; PRUNE-1..PRUNE-7)
- `skinny/REDRESS.md` (5041 lines; REDRESS-126 anchors verified at :3768, :3864, :3869; canonical path verified by `find` — `restart/skinny/REDRESS.md` does not and never has existed)

Source-code verification (HEAD-grep-validated by lens reports per CHALLENGE-CONTEXT §3 executable-verification mandate):
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (3056 lines; 8 grep hits at 516/527/592/1150/1219/1330/2197/2949 — confirms P1-E V2 typed-plane refresh)
- `skinny/crates/bbnf-simd/src/aarch64/{movemask,bulk_emit_positions_64,bitmap_prefix_xor_64,eob_pad_clamp}.rs` (fn at lines 4/2/2/4 — confirms P1-A movemask annotation + P1-C NEON refreshes)
- `skinny/crates/runtime/src/grammars/json/generated.rs:159,164,169` (3 `match_tiny_plain_string*` variants — confirms P1-A V2 annotation) + lines 17/27/33-34/43-44/58-59/79-80/86-87/117-118/138-139/157-158/185-186/201-202/211 (11+ parse-attribution feature gates — confirms heavy F-V2-P1ABC-RERECORD plumbing route intact)
- `skinny/crates/runtime/Cargo.toml:21` (`parse-attribution = []` feature declaration)
- `skinny/Cargo.toml:78-86` (`[profile.release]` block; no `target-cpu` directive — confirms P1-A V2 RUSTFLAGS-unset cohort correction)
- `skinny/REDRESS.md` (canonical path verified by `find` and `ls`)
- V1 → V2 commit delta: `git diff a3dfcaf38 069ba203c -- restart/skinny/tranches/sk-v14/research/p1/` (6 P1 files, +86/-26 lines, no symbol re-record)

Filesystem state observations:
- `/tmp/skv14-p1*/` wiped between V1 commit (`a3dfcaf38`, ~2026-05-23 02:00 UTC) and V2 confirming (~2026-05-23 evening UTC); V1 CH6 §1.1-§1.4 path-existence evidence tables stand as binding record per CH6 V2 §1.1 + §3.6.

Commit verification:
- V1 commit: `a3dfcaf38` (`docs(sk-v14-p1-hardening-V1): challenge V1 + consolidated`)
- V2 light micro-redispatch commit: `069ba203c413d46e7a5d465a128a983254e53841` (`docs(sk-v14-p1-profile): V2 light micro-redispatch — five orphan REVISEs landed`); 6 files changed, 86 insertions, 26 deletions; F-V2-METHODOLOGY-1 + F-V2-P1A-MOVEMASK + F-V2-P1C-LINEDRIFT + F-V2-P1E-1 + F-V2-P1F-1 all landed atomically.
