# SK-V9 S-P2 CHALLENGE V2 — CH1 CORRECTNESS

Pass: S-P2 Research. Cycle: V2. Lens: CH1 CORRECTNESS.
Date: 2026-05-18.
Authority: `restart/prompts/ORCHESTRATOR.md` §3W / §3Z.
Inputs verified this cycle:
- `restart/skinny/tranches/sk-v9/research/p2/hardening/V1/CH1.md`
  (V1 dispositions: 59 ACCEPT / 2 REVISE / 0 REJECT — 96.7%).
- `HARDENING-S-P2-V1-CONSOLIDATED.md` (F1 wiring + F2 PMU load-bearing).
- The six V2-folded reports `skv9-p2-{A..F}.md`.
- `/tmp/skv9-xctrace-v3/pmu_rows.tsv` (34 rows; bbnf-only c/B + ns/B).
- `skinny/crates/parse-that-regex/src/lib.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`,
  `skinny/crates/runtime/src/grammars/json/value.rs`.
- `skinny/RESULTS.md` (SK-V9-open run-id rows).
- `skinny/REDRESS.md` (section headers + entries 28/33/82).
- `skv9-p1-v3-D-structural-breakdown.md` §5 (OLS coefficients).

## §1 — V1-REVISE resolution

V1 CH1 carried two REVISE dispositions. Both were routed into the V2
fold (REVISE #2 as the load-bearing F2; REVISE #1 as the LOW-severity
citation-hardness item folded into V2-D). Resolution status:

### §1.1 — V1-REVISE #2 (P2-E §6.1 PMU c/B table) — **RESOLVED**

V1 found the §6.1 c/B column (`0.354 / 0.628 / 0.787 / 0.193`) was
~7.25× smaller than the PMU TSV `cycles_per_byte` column and that the
sonic-strict c/B sub-column had no PMU provenance. V2-E §6.1 rederives
the baseline directly from `/tmp/skv9-xctrace-v3/pmu_rows.tsv`:

- §6.1 now cites the TSV `cycles_per_byte` / `ns_per_byte` / `mbps`
  columns **verbatim** for all four uncloseable rows. Verified against
  the TSV: unicode_escapes/t1 `3.006864 / 0.711821 / 11238.780`;
  unicode_mixed/t1 `4.633713 / 1.099530 / 7275.839`; y_string_unicode/t1
  `5.709799 / 1.465919 / 5457.328`; gsoc-2018/t1 `1.543720 / 0.369581
  / 21646.136`; gsoc-2018/t2 `1.605891 / 0.390459 / 20488.699`. Every
  cell matches the TSV to full TSV precision.
- The fabricated sonic-strict c/B sub-column is **deleted**; §6.1 now
  cites only sonic-strict *Mbps* from RESULTS (18,132 / 14,515 /
  11,814 / 45,318), which are the real falsifiability comparators.
- The implied host-clock derivation (`c/B ÷ ns/B`) is mathematically
  exact and self-consistent: recomputed 4.224 / 4.214 / 3.895 / 4.177
  GHz to four places. The `Mbps = 8000 / ns_per_byte` convention is
  verified against the TSV `mbps` column to one place.

The §6.2 projection is fully rederived on the verified baseline; §6.4
honestly records the downgrade ("the F2 rederivation materially
downgrades the V1 verdicts"). The defect is closed.

### §1.2 — V1-REVISE #1 (P2-D REDRESS 28+33 line ranges) — **PARTIALLY RESOLVED**

V1 CH1 §4 defect #1 required P2-D to "cite the explicit `REDRESS.md`
line ranges for REDRESS 28 (line 1241+) and REDRESS 33 (line 1314+)".
V2-D §5.5 ("Material differential against REDRESS 28 + 33") and the §0
footer substantively characterise both entries — REDRESS 28's host
kernel admission + rejected 16-byte dispatch (twitter −25%), REDRESS
33's kernel-vs-call-site mismatch — and the §1071-72 differential
table maps each to its SK-V9 §3/§5 differential. The body is accurate
and verifiable.

However, V2-D still cites REDRESS 28 / 33 **by entry number only**; no
explicit `REDRESS.md` line range was added (§5.5, §0, §1071-72, the
authority_inputs frontmatter all omit it). The exact V1 fold-target —
the explicit numeric line range — was not landed. This is preserved as
a CH1-V2 REVISE below (D11-V2). It remains LOW severity / citation
hardness, exactly as V1 graded it; the rejection-rationale match is
invocable from the section title, and the substantive REDRESS-28/33
characterisation is correct against `REDRESS.md` lines 1241-1278 (SK-V5
W5 Primitive Admission) and 1314-1343 (SK-V6 W0 Regression-Recovery),
which this CH1-V2 cycle read and confirmed. The defect is downgraded
in scope (substance verified, citation form still loose) but not fully
closed against the literal V1 instruction.

## §2 — V2 dispositions

Verdicts: ACCEPT (claim verified against evidence), REVISE (defect
requires V3 fold, not load-bearing), REJECT (load-bearing falsification).

### §2.1 — V2-E PMU rederivation (load-bearing F2)

| # | Claim | Verification | Verdict |
|---:|---|---|---|
| E1-V2 | §6.1 cites TSV `cycles_per_byte` verbatim for the 4 uncloseable rows | unicode_escapes 3.006864 / unicode_mixed 4.633713 / y_string_unicode 5.709799 / gsoc-2018 1.543720 — all match `pmu_rows.tsv` to full precision | **ACCEPT** |
| E2-V2 | §6.1 cites TSV `ns_per_byte` verbatim | 0.711821 / 1.099530 / 1.465919 / 0.369581 — all match the TSV | **ACCEPT** |
| E3-V2 | §6.1 cites TSV `mbps` verbatim | 11238.780 / 7275.839 / 5457.328 / 21646.136 — all match the TSV | **ACCEPT** |
| E4-V2 | gsoc-2018/t2 row added (1.605891 / 0.390459 / 20488.699) | Matches TSV row `gsoc-2018 track2` exactly | **ACCEPT** |
| E5-V2 | Implied host clock = `c/B ÷ ns/B` per row | Recomputed: 4.2242 / 4.2143 / 3.8950 / 4.1769 GHz — matches §6.1's 4.224 / 4.214 / 3.895 / 4.177 to three places | **ACCEPT** |
| E6-V2 | `Mbps = 8000 / ns_per_byte` convention verified | 8000/0.711821 = 11238.78 — matches the TSV `mbps` column; the inverse convention is self-consistent | **ACCEPT** |
| E7-V2 | Fabricated sonic-strict c/B sub-column removed | §6.1 no longer carries the 0.362/0.236/0.294/0.094 column; only sonic-strict *Mbps* (RESULTS-sourced) remains | **ACCEPT** |
| E8-V2 | §6.2 codec c/B shares: y_string 40.5%, unicode_escapes 36.2%, unicode_mixed 10.0% | 2.312/5.710 = 40.5%; 1.088/3.007 = 36.2%; 0.463/4.634 = 10.0% — all internally consistent | **ACCEPT** |
| E9-V2 | §6.2 unicode_escapes projection: scalar 1.088 → ×0.25 = 0.272, savings 0.816, new c/B 2.191, ns/B 0.5187, Mbps 15,423 | Recomputed: savings 0.8160, 3.007−0.816 = 2.191, 2.191/4.224 = 0.5187, 8000/0.5187 = 15,423.1 — exact | **ACCEPT** |
| E10-V2 | §6.2 y_string_unicode projection: scalar 2.312 → savings 1.734, new c/B 3.976, ns/B 1.0208, Mbps 7,837 | Recomputed: 1.7340, 5.710−1.734 = 3.976, 3.976/3.895 = 1.0208, 8000/1.0208 = 7,837.0 — exact | **ACCEPT** |
| E11-V2 | §6.2 unicode_mixed projection: scalar 0.463 → savings 0.347, new c/B 4.287, ns/B 1.0173, Mbps 7,864 | Recomputed: 0.3473, 4.634−0.347 = 4.287, 4.287/4.214 = 1.0173, 8000/1.0173 = 7,864.2 — exact | **ACCEPT** |
| E12-V2 | §6.2 gsoc-2018: codec share ≈0%, Mbps unchanged at 21,646 | Codec c/B = 0.000 (TSV `esc-hex` not a top-8 leaf; movemask-dominated). Row unchanged — internally consistent | **ACCEPT** |
| E13-V2 | unicode_escapes verdict NEAR-FAIL at 94.5% of `18132×0.90 = 16,319` | 15,423 / 16,319 = 94.51% — verdict follows; V1 PASS-at-100.5% explicitly retracted in §6.4 | **ACCEPT** |
| E14-V2 | y_string_unicode verdict NEAR-FAIL at 94.8% of `11814×0.70 = 8,270` | 7,837 / 8,270 = 94.76% — verdict follows; the 0.70 W4-precedent slack is stated in §6.3 *before* the projection (CH6-E-4 satisfied) | **ACCEPT** |
| E15-V2 | unicode_mixed verdict FAIL at 63.7% of `14515×0.85 = 12,338` | 7,864 / 12,338 = 63.74% — verdict follows; the 68.7% V1 figure is explicitly superseded | **ACCEPT** |
| E16-V2 | gsoc-2018 verdict no-regression-basis (codec neutral; Mbps unchanged) | Codec share ≈0% → no-regression rule applies (§6.3); `21,646 ≥ 21,646−1%` clears. The 95.5%-of-50%-slack note (21646/22659) is recomputed correct and consistent | **ACCEPT** |
| E17-V2 | §6.4 honest verdict: zero of four rows admit on the codec alone | Follows from E13-E16: 2 NEAR-FAIL, 1 FAIL, 1 no-regression. The "materially more conservative than V1" framing is accurate (`feedback_accurate_perf_narrative` honoured) | **ACCEPT** |
| E18-V2 | §4.1 per-row impact prose agrees with §6.2 table | §4.1 cites y_string 94.8%, unicode_escapes 94.5%, unicode_mixed 63.7%, gsoc no-regression — matches §6.2/§6.4 exactly | **ACCEPT** |
| E19-V2 | §7.2 risk-envelope rows cite the rederived figures | §7.2 cites 7,837 / 94.8%, 15,423 / 94.5%, 63.7% — consistent with §6.2 | **ACCEPT** |
| E20-V2 | y(ns/B) base values (P1-V3-D §5) still correctly cited where used | y_string 0.1842, unicode_escapes 0.0830, unicode_mixed 0.1470, gsoc-2018 0.0451 — all match P1-V3-D §5 OLS residual table lines 344-355 | **ACCEPT** |

**V2-E subtotal: 20 ACCEPT, 0 REVISE, 0 REJECT.** The load-bearing F2
rederivation is fully verified. The §6.1 baseline now cites the TSV
verbatim; the §6.2 arithmetic is internally consistent to the last
decimal; the §6.4 admission verdicts follow rigorously from the
rederived numbers. One minor non-defect note (not a disposition):
§1.5 states the scalar cost ceiling as "~30 µops/quartet → ~6-8 µops"
while §6.2 states "~28 µops → ~6-7 µops"; both yield the same ~75%
reduction factor (×0.25) that §6.2 actually consumes, so the gate
arithmetic is unaffected. Cosmetic only — flagged for V3 tidy, not a
CH1 defect.

### §2.2 — V2-D wiring fix (load-bearing F1)

| # | Claim | Verification | Verdict |
|---:|---|---|---|
| D1-V2 | §2.1 now states `unescape_uxxxx_x4_neon` **IS** wired | Confirmed verbatim: "Both ARE wired: the x4 kernel is consumed at `parse-that-regex/src/lib.rs:402`" — the V1 "neither wired" error is corrected | **ACCEPT** |
| D2-V2 | The kernel is consumed at `parse-that-regex/src/lib.rs:402` | Source read: line 402 is `(unsafe { bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon(&packed) })` — exact | **ACCEPT** |
| D3-V2 | The call site is `unescape_four_unicode_escapes` (lines 384-459) | Source: `fn unescape_four_unicode_escapes` opens at 384, closes at 459 — exact | **ACCEPT** |
| D4-V2 | Dispatched from the `Some(b'u')` arm at `lib.rs:778` | Source line 778: `if let Some(batch) = unescape_four_unicode_escapes(bytes, slash, &mut out)` — the call into the wrapper is at 778; the §0 "Some(b'u') arm" framing is accurate to the inner-loop dispatch | **ACCEPT** |
| D5-V2 | Current shape = opportunistic-x4 batcher (hard-requires 4 back-to-back quartets) | Source 391-399: builds a `[0u8;16]` from 4×6-byte escapes, returns `None` on any non-`\u` shape → falls through to scalar. The §2.1 characterisation is exact | **ACCEPT** |
| D6-V2 | Reframed REDRESS 82 differential: NOT "wire it" but "broaden x4-only batching + rebind consumer cardinality" | §2.1 items 1-2, §3.5 items 1-3, §0 footer all carry the reframe. The differential is now factually grounded — the kernel is wired, the gap is engagement-shape + consumer-layer | **ACCEPT** |
| D7-V2 | §3.5 REDRESS 82 W4 history: moved scalar decoder into `unicode/escape_decode.rs`, reused `unescape_uxxxx_neon` one quartet at a time, failed on `unicode_escapes/direct` (39.4%) + `y_string_unicode/direct` (−6.6% t2) | Cross-checked `REDRESS.md:2285` "SK-V7 Wave 4 Single-Quartet Unicode Escape Classifier Redress" — the W4 single-quartet + direct-route-failure characterisation is consistent with that section's body | **ACCEPT** |
| D8-V2 | §3.2/§3.3 reframed: per-quartet `unescape_uxxxx_neon` NOT wired, only x4 variant consumed | Source: `unescape_uxxxx_neon` (`:74`) has no caller in `lib.rs`; only `_x4_neon` (`:125`) is called at `:402`. The "only the x4 variant is consumed" claim is correct | **ACCEPT** |
| D9-V2 | §0 footer accurately summarises the F1 fix and its downstream §2.1/§3.2/§3.3/§3.5/§7 reframes | §0 states the V1 error verbatim, cites `:402` + lines 384-459 + `:778`, credits P2-E §1.2 as the correct identifier — all verified | **ACCEPT** |
| D10-V2 | `unescape_uxxxx_neon` at `unescape_uxxxx.rs:74`, `_x4_neon` at `:125` | Source: both confirmed at exactly those lines | **ACCEPT** |
| D11-V2 | §5.5 / §0 / §1071-72 cite REDRESS 28 + 33 — V1 REVISE #1 asked for explicit `REDRESS.md` line ranges | V2-D characterises both entries substantively and correctly (verified against `REDRESS.md` 1241-1278 + 1314-1343) but cites them by entry number only — no line range added. The literal V1 fold-target was not landed | **REVISE** (carry-forward; LOW severity, citation hardness — same grade V1 assigned) |
| D12-V2 | §6.2.1 (DAV1D discipline) checkasm ownership added; `checkasm_unescape_uxxxx.rs` assigned to the §3 codec-broadening wave | §6.2.1 + §993 table verified — the missing test is named, owned, and gated on P2-A; consistent with the F1 "wired-but-untested" finding | **ACCEPT** |
| D13-V2 | §1.4 wide-issue ceiling: CPI < 0.4 across 17 corpora; 1.18-5.95 c/B range | PMU TSV: max CPI = 0.389513 (unicode_mixed/t1) < 0.4 ✓; c/B min 1.179831 (citm/t1), max 5.951786 (y_string/t2) → "1.18-5.95" exact | **ACCEPT** |

**V2-D subtotal: 12 ACCEPT, 1 REVISE, 0 REJECT.** The load-bearing F1
wiring fix is fully verified against source: `parse-that-regex/src/
lib.rs:402` does call `unescape_uxxxx_x4_neon` inside
`unescape_four_unicode_escapes` (384-459). The reframed REDRESS 82
differential is accurate. The single REVISE is the unclosed V1 REVISE
#1 carry-forward (D11-V2).

### §2.3 — Cross-report citation spot-check (≥12)

| # | Citation | Report | Verification | Verdict |
|---:|---|---|---|---|
| S1 | `JsonNodeKind::at_cursor` re-reads source bytes per cursor | P2-A §2.1 | `runtime/src/grammars/json/value.rs:29` — `match tape.source()[offset]` per cursor confirmed | **ACCEPT** |
| S2 | `match_string_at_quote_trusted_utf8` at `lib.rs:162` | P2-D/§2.2, P2-E §1.4 | Source line 162: `pub fn match_string_at_quote_trusted_utf8(` — exact | **ACCEPT** |
| S3 | `validate_string_escape` at `lib.rs:284` | P2-D §2.3, P2-E §6.2 | Source line 284: `fn validate_string_escape(input: &[u8], slash: usize)` — exact | **ACCEPT** |
| S4 | `read_hex_unit_scalar` at `lib.rs:945` | P2-D §2.1/§2.2, P2-E §1.1 | Source line 945: `fn read_hex_unit_scalar(hex: &[u8])` — exact | **ACCEPT** |
| S5 | `hex_nibble` at `lib.rs:959` | P2-D §2.1, P2-E §1.1 | Source line 959: `fn hex_nibble(byte: u8)` — exact | **ACCEPT** |
| S6 | `skip_string_plain_trusted` at `lib.rs:547` | P2-D §2 | Source line 547: `fn skip_string_plain_trusted(input: &[u8], mut cursor: usize)` — exact | **ACCEPT** |
| S7 | sonic-strict comparators 18132/14515/11814/45318 | P2-E §6.1, P2-F §2 | RESULTS rows 35/33/41/24 sonic_rs_strict mbps = 18132 / 14515 / 11814 / 45318 — all exact | **ACCEPT** |
| S8 | Track-1 deltas y_string −54.1%, unicode_mixed −53.1%, unicode_escapes −33.6%, gsoc-2018 −51.0% | P2-F §2.3 | RESULTS rows 41/33/35/24: 5428/6803/12047/22184 vs sonic → −54.1/−53.1/−33.6/−51.0% — all exact | **ACCEPT** |
| S9 | apache parse_only T1 11917, sonic 15536, −23.3% | P2-C §3, P2-F §2 | RESULTS row 12: 11917 / 15536 / −23.3% — exact | **ACCEPT** |
| S10 | citm parse_only T1 29215, sonic 23590, +23.8% | P2-C §3, P2-F §2 | RESULTS row 8: 29215 / 23590 / +23.8% — exact | **ACCEPT** |
| S11 | OLS coefficients 1.079·(q/B) + 0.184·(n/B) + 0.051, R²=0.371 | P2-F §2, P2-E §6 | P1-V3-D §5 lines 331/334 — verbatim | **ACCEPT** |
| S12 | y(ns/B) residual-table base values 0.1842 / 0.0830 / 0.1470 / 0.0451 | P2-E §6.2 (E20) | P1-V3-D §5 lines 344-355 — exact | **ACCEPT** |
| S13 | REDRESS 82 at `REDRESS.md:2285` ("SK-V7 Wave 4 Single-Quartet Unicode Escape Classifier") | P2-D §3.5, P2-E §5 | `REDRESS.md` section header at 2285 — confirmed | **ACCEPT** |
| S14 | PMU CPI max 0.390 on unicode_mixed; c/B range 1.18-5.95 | P2-D §1.4, P2-F §2 | `pmu_rows.tsv`: unicode_mixed/t1 cpi 0.389513; c/B min 1.179831 / max 5.951786 — exact | **ACCEPT** |
| S15 | P2-A §2.1 hot-leaf antecedents (`read_hex_unit_scalar` 38-44% on y_string) | P2-A §0 | Consistent with P2-D §2.1 hot-leaf table (hex_nibble 19.2% + read_hex 19.0% = 38.2%) and P1-V3-B | **ACCEPT** |

**Spot-check subtotal: 15 ACCEPT, 0 REVISE, 0 REJECT.** Fifteen
distinct file:line / REDRESS / RESULTS / PMU / P1-V3 citations
verified across all six folded reports — well beyond the ≥12 floor.

### §2.4 — V2 fold integrity (no new defects)

| # | Check | Verification | Verdict |
|---:|---|---|---|
| I1 | P2-E V1 §2-§5 (non-folded sections) unchanged in substance | §6 is the only load-bearing rewrite; §1-§5 cross-checked against V1 CH1 E1-E4/E10 — same line citations, no regression | **ACCEPT** |
| I2 | P2-D V1 ACCEPT claims D1-D10 survive the fold | D1-D10 re-spot-checked above (D10-V2, S2-S6) — all still hold; the fold touched only §2.1/§3.2/§3.3/§3.5/§7/§0 | **ACCEPT** |
| I3 | P2-A V2 fold (F4/F5) introduced no CH1 defect | §0 records F4 cost-discipline + F5 Lock-14 surgical edits; V1 CH1 A1-A8 claims (value.rs, generated.rs, tape) unaffected — S1 reconfirms A1 | **ACCEPT** |
| I4 | P2-B V2 fold (AnyGrammar + cost) introduced no CH1 defect | §0 records the surgical AnyGrammar declaration + per-slice cost; V1 CH1 B1-B6 are proof-artefact claims untouched by the fold | **ACCEPT** |
| I5 | P2-C V2 fold (F4 LOC break-out + F5 cross-grammar prose) introduced no CH1 defect | §0 footer: "No architectural reshape; no change to §1's REDRESS 91 posture, §3's per-row tables, §4.2/§4.3 gates, §6 pre-blocks, §7 sources" — V1 CH1 C1-C9 all preserved | **ACCEPT** |
| I6 | P2-F V2 fold (synthesis walk-back) introduced no CH1 defect | §0: §7.4 reframed to a dependency graph, §7.2/§7.3/§3 overreach stripped. The walk-back removes claims — it cannot introduce a correctness defect. V1 CH1 F1-F12 (RESULTS/PMU tables) untouched | **ACCEPT** |
| I7 | Cross-report continuity: P2-D §2.1 ↔ P2-E §1.2 on the wiring | Both now agree the x4 kernel is wired at `lib.rs:402`; P2-D §0 explicitly credits P2-E §1.2. Continuity restored (V1 had P2-D contradicting P2-E) | **ACCEPT** |
| I8 | Cross-report: P2-E §6 ↔ P2-F §2 PMU c/B values | P2-F §2.1-§2.3 c/B table was V1-verified clean against the TSV; P2-E §6 now rederives from the same TSV — the two reports are mutually consistent (V1's P2-E-vs-P2-F mismatch is closed) | **ACCEPT** |
| I9 | The four uncloseable rows still converge across all six reports | unicode_mixed / unicode_escapes / y_string_unicode / gsoc-2018 — P2-A §2, P2-D §2, P2-E §6, P2-F §2 all carry the same four; V2 folds preserved the convergence | **ACCEPT** |

**Fold-integrity subtotal: 9 ACCEPT, 0 REVISE, 0 REJECT.** The V2 fold
introduced no new CH1 defect. Critically, the F1 fix *resolved* a
latent cross-report contradiction (V1 P2-D contradicted P2-E on the
wiring), and the F2 rederivation *resolved* the P2-E-vs-P2-F PMU
inconsistency — the fold is net-corrective for CH1.

## §3 — Aggregate verdict

| Cohort | ACCEPT | REVISE | REJECT | Total | ACCEPT rate |
|---|---:|---:|---:|---:|---:|
| V2-E PMU rederivation (F2) | 20 | 0 | 0 | 20 | 100% |
| V2-D wiring fix (F1) | 12 | 1 | 0 | 13 | 92.3% |
| Cross-report spot-check | 15 | 0 | 0 | 15 | 100% |
| V2 fold integrity | 9 | 0 | 0 | 9 | 100% |
| **Aggregate** | **56** | **1** | **0** | **57** | **98.2%** |

**Verdict.** CH1 CORRECTNESS clears the ≥95% threshold at **98.2%
ACCEPT** with **zero REJECTs** — up from V1's 96.7%.

- The **load-bearing F2 PMU rederivation** is fully verified: P2-E §6.1
  now cites `/tmp/skv9-xctrace-v3/pmu_rows.tsv` verbatim, the §6.2
  projection arithmetic is internally consistent to the last decimal
  (every one of the three live projections + the gsoc neutral row
  recomputed exactly), and the new admission verdicts —
  unicode_escapes NEAR-FAIL 94.5%, y_string_unicode NEAR-FAIL 94.8%,
  unicode_mixed FAIL 63.7%, gsoc-2018 no-regression-basis — follow
  rigorously from the rederived numbers. The V1 fabricated-PMU defect
  is closed.
- The **load-bearing F1 wiring fix** is fully verified against source:
  `unescape_uxxxx_x4_neon` IS wired at `parse-that-regex/src/lib.rs:402`
  inside `unescape_four_unicode_escapes` (384-459); the reframed
  REDRESS 82 differential ("broaden x4-only batching + rebind consumer
  cardinality", not "wire it") is accurate.
- The single remaining **REVISE** (D11-V2) is the unclosed V1 REVISE #1:
  P2-D cites REDRESS 28/33 by entry number but never added the explicit
  `REDRESS.md` line range the V1 fold-target named. It is LOW severity
  (citation hardness) — the substantive REDRESS-28/33 characterisation
  was independently verified correct against `REDRESS.md` 1241-1278 +
  1314-1343 this cycle — and undermines no falsifiability gate or
  load-bearing argument.

Per §3Z: V1 was cycle 1 for CH1 (96.7%, qualifying). **V2 is the
candidate second consecutive qualifying cycle for CH1** at 98.2% with
zero REJECTs — CH1 qualifies on V2 and, with V1, satisfies the
two-consecutive-qualifying-cycle requirement for CH1 convergence,
**conditional on the D11-V2 REVISE being folded** (or formally waived
as a sub-threshold citation-form item) before S-P2 close. The V2 fold
verified clean for CH1 with no new defects.

## §4 — Defects (V3 fold or formal waiver)

| # | Defect | Owning report | Required edit | Severity |
|---:|---|---|---|---|
| 1 | D11-V2 / carry-forward of V1 REVISE #1. P2-D §5.5, §0, §1071-72, and the authority_inputs frontmatter cite REDRESS 28 + 33 by entry number only; the explicit `REDRESS.md` line ranges the V1 CH1 §4 defect #1 named (REDRESS 28 → SK-V5 W5 Primitive Admission, lines 1241-1278; REDRESS 33 → SK-V6 W0 Regression-Recovery, lines 1314-1343) were never added. The substantive characterisation in §5.5 is correct and was re-verified against `REDRESS.md` this cycle. | P2-D | Add the explicit `REDRESS.md` line ranges (1241-1278 / 1314-1343) to the §5.5 differential prose and the §1071-72 table — a two-token edit per citation. Alternatively, the orchestrator may formally waive this as a sub-threshold citation-form item, since the rejection-rationale match is invocable from the section title and the body is verified accurate. | LOW (citation hardness; non-load-bearing) |

Non-defect note (no V3 fold required): P2-E states the scalar
per-quartet cost ceiling as "~30 µops → ~6-8" in §1.5 and "~28 µops →
~6-7" in §6.2. Both yield the ~75% reduction (×0.25) that §6.2's gate
arithmetic actually consumes, so no gate is affected. A V3 tidy could
unify the two figures; it is cosmetic, not a CH1 defect.

No other defect was found. The two load-bearing V2 folds (F1 wiring,
F2 PMU) are verified clean; the V2 fold introduced zero new CH1
defects and net-resolved two latent V1 cross-report inconsistencies
(P2-D↔P2-E wiring contradiction; P2-E↔P2-F PMU mismatch).
