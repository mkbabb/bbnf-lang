# SK-V18 S-P3 CHALLENGE — V9 / CH5 (SOTA-PRESERVATION lens)

Lens: SOTA-PRESERVATION. Is the >SOTA-regression gate bound at G2 AND G6 (corpus-in-timer, the
per-corpus floors), the hot leaves (CSS 94.1% scalar scan; JSON 91.5% sink leaf) preserved as gated
primitives, with NO wave silently regressing them? Doc: `SPEC.md` (1660 lines), read against S-P2
`SYNTHESIS-RESEARCH.md §3` and the S-P0 `SYNTHESIS-AUDIT-OVERFIT.md` addenda.

Posture: prior P3 cycles reached reject=0 (V6 12A/1R, V7 12A/1R, V8 14A/0R/0). The V6→V7 REVISE
(R-CH5-1, the PROVE `json_css_preservation_held` CSS clause) was folded and verified at V7/V8; V8
was clean. CH5's V9 charge is the RESIDUAL precision REVISE drive to a 2-consecutive-clean fixed
point (V8+V9), plus any genuine REJECT. Proportionate: a wording nit on a 1660-line doc is a REVISE
ONLY if it would mislead an implementer.

This is an INDEPENDENT V9 re-grep, not a rubber-stamp of V8. Every SOTA figure under the lens
re-checked against the S-P1 ledger this pass; every G2/G3/G6/PROVE/H1 SOTA seam re-traced.

Grounding re-checked on disk this pass (`SYNTHESIS-PROFILE.md`):
- CSS 94.1% = `find_component_delim` 79.5% (line 90/98) + `consume_balanced_at` 14.6% (line 91),
  scalar-scan share 4121/4379 = 94.1% (line 96). JSON 91.52% = `parse_object_value_at_direct`
  79.82% (line 62) + `parse_array_element_at_direct` 11.70% (line 63), combined line 70.
- Per-corpus `track1_rich/lcss` directional antecedents: bootstrap 2.190, tailwindcss 3.375,
  material-components-web 1.658 (min), animate 2.101 (lines 44-47). loadavg 4.35 (line 15);
  "Absolute Mbps DIRECTIONAL/depressed, NOT re-locked; same-run ratios + hot-leaf rank load-robust"
  (lines 21-22). G6 VERDICT=WIRE (94.1% ≫ ~8% threshold, line 100); JSON G5 has no hot leaf
  (line 146). EVERY SPEC figure under my lens matches the S-P1 ledger BYTE-FOR-BYTE.
- S-P2 §3 G2 (lines 167-175): the EXPLICIT >SOTA-regression gate distinct from parity, cold
  corpus-in-timer, the bench re-measure is the binding regression falsifier. S-P2 §3 G5/G6 (lines
  194-199): checkasm correctness-only, any Mbps figure from the corpus-in-timer symmetric harness,
  speedup CLAIM deferred to H1 — "addendum 5 not enforced one wave too late."

## Enumeration of every wave-gate / telemetry / close claim under the SOTA-PRESERVATION lens

### G2 — CSS >SOTA-regression gate (the PRIMARY locus)

**[C-G2.1] >SOTA gate bound at G2, corpus-in-timer, per-corpus, distinct from parity.** §0.1#6
(104-128), §5 G2.2-conjunct-4 (960-970), telemetry `g2_sota_ratio_held` + `corpus_in_timer`
(1035-1037), gate-json REJECT (1044-1050). Binding falsifier: SAME-RUN `track1_rich/lightningcss >
1.0×` per corpus AND no same-run regression vs the G2-entry-captured pre-G2 baseline
(`track1_rich_over_lcss_ratio_pre_g2`), cold, corpus-in-timer. Gate-before-speed enforced
(`g2_cssparser_oracle_parity` passes BEFORE any speed row, 959/1047). The S-P1 absolutes are demoted
to DIRECTIONAL antecedents — keying the close on an un-re-locked absolute is named as the
unfalsifiable-gate hazard and replaced by the same-run comparison (uniform host depression cancels
both sides). **ACCEPT** — the sharpest, most falsifiable form; strictly HARDENS the S-P2 §3 "≥ the
S-P1 ratio" formulation into a load-robust same-run delta, closing §5-risk-7. Sound.

**[C-G2.2] CSS 94.1% hot leaf preserved as a GATED primitive (`css_balanced_component_scan`).**
§1 (a)-(d) gate (358-379), §5 G2.3 (984-1003), telemetry `g2_balanced_scan_primitive_abcd_pass` /
`_loc` / `_profiled_leaf_extent` / `_arg_mutation_fires` (1029-1032). Admitted ONLY under all four:
(a) grammar-INVOKED-by-name, (b) emitted ARG byte-sets VARY under invoking-rule mutation, (c)
`verbatim_blob_present == false`, (d) PROFILE-PROVEN-NARROW-LEAF (`g2_balanced_scan_primitive_loc <=
g2_balanced_scan_profiled_leaf_extent`, the god-kernel bound — both numerator and denominator emitted
as telemetry, so machine-checked not asserted). **ACCEPT** — hot leaf preserved as a gated primitive,
NOT a silent blob, with the over-scoping REJECT mechanically enforced.

**[C-G2.3] Throughput-equivalence PROVEN by named falsifier, not asserted.** §0.1#6 (119-126):
"the 94.1% CSS scalar scan … re-emitted with bodies whose throughput-equivalence is PROVEN by the
named falsifiers — NOT asserted" — `g2_cssparser_oracle_parity` THEN the same-run ratio gate (the
speed falsifier admissible ONLY after the parity falsifier passes). The SPEC states plainly "G2
RE-DERIVES the 94.1% scan, so oracle parity does NOT prove throughput preservation" (968).
**ACCEPT** — oracle-parity (correct output) correctly distinguished from throughput-preservation
(the bench re-measure); the exact S-P2 §3 G2 caveat, faithfully carried.

**[C-G2.4] The ≥1-regular-corpus mandate + tailwind/mcw residual policy.** §0.5 table (303-308),
`g2_sota_ratio_held` (1035): PASS REQUIRES ≥1 REGULAR corpus (animate OR bootstrap) crossing > 1.0×
with no regression; tailwindcss below 1.0× is an honest residual (NOT tranche-blocking provided a
regular corpus holds); mcw/full-corpus regression reported honestly. **ACCEPT** — a non-trivial
per-corpus floor with an honest-residual escape for the hardest corpus, NOT a corpus-average paper
(§11 pre-blocks "a corpus-average substituting for per-corpus ratios", 1625). Falsifiable and honest.

### G3 — the un-fork preserves the CSS >SOTA (path change, not output change)

**[C-G3.1] G3 conjunct 8 re-confirms the CSS >SOTA directionally against the G2-recorded baseline.**
§6 G3.2-conjunct-8 (1127-1129), `g3_css_sota_ratio_held` (1165), gate-json REJECT (1171). The
un-fork changes the PATH not the OUTPUT, so conjunct 6 (byte-equivalent generated output) plus
conjunct 8 (same-run ratio > 1.0×, re-confirmed against the G2-RECORDED `*_pre_g2` baseline — never
re-measured here, per close-cond #6) jointly hold the CSS leaf. **ACCEPT** — the un-fork cannot
regress the CSS >SOTA because the generated CSS body is byte-equivalent to the G2-closed file; the
ratio is re-confirmed directionally, the temporally-sound placement (the pre-G2 code is gone, so the
ratio is NOT re-derived). Consistent with §0.2 (181) and §10.

### G6 — NEON retarget of the 94.1% leaf / the deferred speedup figure

**[C-G6.1] The corpus-in-timer >SOTA figure plane is BOUND at G6; the figure VALUE is deferred to
H1 — correctly.** §8 exit gate (1361-1364), `g6_speedup_median_mbps` (1388), §0.4 `corpus_in_timer`
(255). G6 emits NO `corpus_in_timer` column and NO non-null speedup; the figure is null pre-H1; G6
outcome is `C` until H1 produces it on the symmetric `css_canon_bench` plane. Addendum-5
plane-mismatch (a figure off the checkasm plane) is REJECT at G6 (1375, 1398-1400). This is the S-P2
§3 timed-plane binding EXACTLY: "addendum 5 not enforced one wave too late" — the figure PLANE is
bound at G6 (the plane-mismatch REJECT fires at G6), the figure MAGNITUDE is produced at H1.
**ACCEPT** — the lens "bound at G6" is satisfied by the live plane-constraint at G6; the magnitude
deferral is sound and is what the addendum mandates.

**[C-G6.2] The NEON retarget preserves the 94.1% leaf BEHAVIOR byte-exact (the leaf-preservation
proof for the optimization wave).** §8 (1357-1360): `neon_significant_skip_matches_scalar == PASS`
over the REAL 71KB-495KB corpora (micro-case-only PASS does NOT satisfy); `checkasm_differential ==
PASS` covering the three retarget seams bit-exact ((a) the ≤13-byte two-fan OR-reduce salvage; (b)
the skip stops AT `([{'"/`; (c) error positions reproduced from the scalar shell). The differential
parity is the leaf-preservation proof: the NEON skip lands at the SAME delimiter as the scalar
reference over the real corpora, so the parse structure — hence the rich projection the lightningcss
bar is measured against — is byte-unchanged. **ACCEPT** — the G6 optimization cannot SILENTLY alter
the 94.1% leaf because the byte-exact differential over the real corpora is the binding correctness
gate (dav1d discipline, scalar reference FIRST). The >SOTA-bearing projection output is preserved by
construction-of-correctness, then re-measured at H1.

**[C-G6.3] A non-speedup or load-depressed NEON result is honestly `S`/`C`, NEVER paper-closed `A`.**
§0.3 (225-229): "A NEON speedup CLAIM (G6) is `A` ONLY when the timed-plane binding holds AND the H1
quiet-bar holds (`host_loadavg < 1.0`) … a corpus-in-timer figure produced under `host_loadavg >= 1.0`
… is `S` … a checkasm PASS without ANY corpus-in-timer figure is `C` … NEVER `A`." §10 H1 falsifier
(1566): a DIRECTIONAL figure is outcome `S`, not `A`. This forecloses the one residual SOTA hazard a
NEON wave carries: realized speedup is bounded by inert-run length (S-P2 §5-risk-6), so a correct-but-
not-faster NEON wire is possible — and the SPEC handles it as an honest `S` residual, never an
inflated close. **ACCEPT** — the optimization-wave honesty is bound: a NEON wire that does not beat
the scalar is surfaced honestly, not paper-closed; the CSS >SOTA bar itself (`> 1.0×` vs lightningcss)
is held by the byte-exact projection, independent of whether NEON wins.

**[C-G6.4] The 94.1% leaf reached AT ADMISSION — two-conjunct proof, no silent dead kernel.**
§0.1#10 (154-165), §8 exit gate (1345-1356), telemetry `acceleration_at_admission` /
`simd_admission_caller` / `simd_admission_profile_sampled` (1381-1383). `admission` REQUIRES BOTH (i)
the `generated.rs` caller census (non-empty, NOT `#[cfg(test)]`) AND (ii) the samply
runtime-reachability conjunct (`runtime_simd` in the `css_canon_bench` sample, non-zero self-time).
A census hit in dead/unreachable code the profile does NOT see == `dead` == REJECT. **ACCEPT** —
directly forecloses the R7 "NEON dead at admission" recurrence; a source-grep-only proof FAILS; the
profile-attribution is the binding second conjunct. The hot leaf is wired, not orphaned (no-orphan
law, addendum 6).

**[C-G6.5] G5 (`json/scan.rs`) retire is N/A, not a SOTA regression.** §8 (1325-1327, 1365-1366):
the zero-sampled `json/scan.rs` (R12) is retired/neutralized; G5 authors NOTHING for JSON; outcome
`N` (nothing on product path); `json_guard_held == true` (51/51 within ±1.0% of SK-V18-open).
**ACCEPT** — S-P1 confirms JSON has NO G5 hot leaf (line 146); retiring a zero-sampled scanner cannot
regress a leaf it never served. Honest.

### G1 / PROVE / H1 — the JSON 91.5% leaf + cross-wave preservation

**[C-J.1] JSON 91.5% hot leaf preserved as a GATED primitive across every wave.** §4 G1.3 (856-871),
G1.2-conjunct-3 (842-848), telemetry `g1_hot_leaf_preserved` / `g1_leaf_primitive_abcd_pass` /
`g1_dispatch_triple_not_lcd_collapsed` (878-884); G3 conjunct-7 (1125-1126) re-asserts through the
un-forked renderer; G5/G6 `json_guard_held` (1366) and H1 `json_guard_held` (1590) carry the 51/51
floor. The leaf scanners (`decode_json_string_to_arena`, `parse_number_*`) admitted under the SAME
(a)-(d) gate as CSS, same machine-checked (d) LOC bound (882-884). The dispatch-triple LCD-collapse
REJECT (845-848, 871) protects the monomorphized-sink leaf. **ACCEPT** — the JSON hot leaf preserved
by the same gated-primitive discipline as CSS, with the LCD-flatten regression explicitly pre-blocked.

**[C-J.2] No wave silently regresses the JSON leaf — the ±1.0% guard is PINNED, not floating; the
anchor is well-defined.** The JSON guard floor is anchored to `SK-V18-open` (the PINNED baseline, NOT
floating) at G1 (886), §0.5 (310-311), G5/G6 (1366), PROVE (1511), H1 (1590). `SK-V18-open` is the
tranche-open snapshot of the W0-locked 51/51 strict-vs-sonic-rs rows (§0.2 line 184/198: "the 51/51
cold strict guard carried from the W0 lock"; revert "restore `SK-V18-open` RESULTS" 1405) — an
implementer captures it at the pre-G1 state. G3 re-asserts the leaf byte-equivalence (identical
inline cfg + sink.* call sites) since the un-fork changes the PATH not the OUTPUT (1123-1126).
**ACCEPT** — the floor is pinned (a floating baseline would let a slow drift evade the gate) and the
anchor is unambiguous; the leaf preservation is mechanical (byte-equivalence) at G3, not merely a band.

**[C-J.3] PROVE preserves BOTH hot leaves by BYTE-EQUALITY (not a fresh ratio bench) — the V7 fold
verified.** §9 `json_css_preservation_held` (1511): JSON 91.5% leaf byte-equal + 51/51 within ±1.0%
of SK-V18-open AND CSS `track1_rich/lcss` preserved by byte-equivalence of
`grammars/css_l4/generated.rs` vs the G3-closed shipped file (`dirty_generated_state == clean`) — NOT
a fresh corpus-in-timer re-measure (PROVE runs post-G2, the pre-G2 same-run baseline is gone per
close-cond #6). gate-json REJECT (1519). **ACCEPT** — the V6 CH5 REVISE (R-CH5-1) is VERIFIED FOLDED:
PROVE preserves the CSS leaf by byte-equality, not an un-measurable ratio bench. The shared
`render(program)` body PROVE re-touches must not perturb either hot leaf, enforced by
`dirty_generated_state == clean` on `grammars/{json,css_l4}/generated.rs`. The mechanism is the
correct one for a post-G2 wave with the pre-G2 baseline retired. Consistent with G3 (byte-equivalent)
and H1 (directional re-confirm).

### Close-condition / framing honesty (H1)

**[C-H1.1] CSS framing honesty bound — lazy-rich-vs-eager-cssom, CLOSED enum.** §0.2 (187-195), §0.4
`materialization_framing` (256, enum CLOSED to two values so the gate REJECTs any other), §10
(1545-1549, 1563), gate-json REJECT (1594-1595). An unqualified "beats CSSOM"/"equal-work" re-label
WITHOUT the materialization-depth asymmetry disclosed is REJECT (R-A0-1). **ACCEPT** — the >SOTA claim
cannot be inflated by a re-label; the closed enum makes `undisclosed`/any-other a mechanical RED.
Discharges R14/R-A0-1.

**[C-H1.2] The quiet-bar (`host_loadavg < 1.0`) gates any ABSOLUTE Mbps claim.** §0.3 (225-229), §0.4
`host_loadavg` (272), §10 falsifiers (1577-1580), gate-json REJECT (1596-1599). An absolute
`g6_speedup_median_mbps` with `host_loadavg >= 1.0` or no stamp is RED; DIRECTIONAL with the load
caveat is `S`, not REJECT. **ACCEPT** — the §5-risk-7 load-honesty discipline at the schema; the
S-P1 loadavg-4.35 capture cannot be laundered into an absolute close claim.

**[C-H1.3] H1 RE-CONFIRMS, never RE-MEASURES, the pre-G2 ratio — the temporal-soundness fix.** §0.1#6
(108-113), §0.2 (181), §10 (1567-1572), `css_sota_ratio_held` (1591). The pre-G2 code is GONE
post-G2, so the baseline is CAPTURED AT G2 ENTRY (one quiet run measuring both the pre-G2 checkout and
the post-G2 build), the regression falsifier FIRES at G2 exit, and H1 only re-confirms the G2-recorded
ratio DIRECTIONALLY. A tailwind miss recorded at G2 is re-confirmed as a residual at H1, NOT
re-litigated as a fresh H1 block. **ACCEPT** — resolves a genuine temporal hazard (you cannot
re-derive a pre-G2 figure after the pre-G2 code is deleted) that a naive "re-measure at H1" gate
would leave unfalsifiable. Consistent across §0.5, §5, §10.

### Cross-cutting / sequencing under the lens

**[C-X.1] The relocated-seam guard protects the leaves at the un-fork (G3).** §0.1#2 (71-78), §6
G3.2-conjunct-5 `emit_shape_source == lowered_program` (1114-1121). Without this conjunct, a fork
relocated into the neutral per-profile columns passes conjuncts 1-4 under a green gate — and such a
relocated per-grammar branch could re-introduce a CSS-specific code path that silently regresses a
leaf. The fourth conjunct (the emitter reads output-shape ONLY from the lowered program, never from
`target.*`/`contract.*`) forecloses it; the structural `runtime_target_rows_collapsed` co-gate (R16
full-row PartialEq over BOTH nested structs) is the companion. **ACCEPT** — the un-fork cannot
relocate a leaf-regressing branch into data; the §5-risk-1 binding, faithfully carried (§11 G3
pre-block, 1621).

**[C-X.2] G2 entry dual-gates on P3 so the 94.1% scan is derived ONCE, not 7× (and G6 wires ONE
site).** §5 G2.1 (935-942), `g2_css_replica_singular` (1039), §8 `css_scan_call_site_singular` (1364),
the G6 singular-site falsifier (1372). If P3 has not collapsed the 7 replicas, G2 re-derives the SAME
scan into 7 byte-identical files and G6 emits the NEON call 7 ways — a fork of the very hot leaf. P3
closure is the independent conjunct that prevents this; G6 entry-gates on P3 for exactly this reason
(1316). **ACCEPT** — the singular-scan invariant is the precondition for both the G2 derivation and
the G6 single wire; without it the hot leaf re-forks.

## Residual-REVISE sweep (the CH5 V9 precision charge)

I searched specifically, this V9 pass, for: (1) an unfalsifiable SOTA gate; (2) a leaf regression a
wave could slip silently; (3) a per-corpus floor stated as an average; (4) a G2/G6 figure on the
wrong plane; (5) a hot leaf admitted outside the (a)-(d) gate; (6) a G6-introduced regression that no
gate catches; (7) an undefined baseline anchor. Findings:

- **The G6-NEON-could-be-slower probe.** A NEON retarget that is correct (byte-exact differential)
  but slower than the post-G3 scalar is physically possible (speedup bounded by inert-run length,
  S-P2 §5-risk-6). I traced whether this slips: it does NOT paper-close — §0.3 (225-229) routes a
  non-speedup / load-depressed figure to outcome `S` (or `C` if no figure), NEVER `A`; the CSS >SOTA
  bar (`> 1.0×` vs lightningcss) is held independently by the byte-exact projection at G6 and the
  directional re-confirm at H1. The H1 floor "no regression vs the pre-G2 baseline" governs the
  GENERALIZATION rebuild (G2's re-derivation), and the NEON wave's value is reported honestly at H1.
  This is the intended, honest design — not a silent regression. No REVISE.
- **The `corpus_in_timer` G6-defer / H1-produce split** is internally consistent: §0.4 (255)
  explicitly states G6 emits no `corpus_in_timer` column (deferred to H1); it is consumed at G2 (1037)
  and H1 (1586), both of which emit it. The `g6_speedup_median_mbps` null-pre-H1 / produced-at-H1
  split is carved explicitly out of the "no producer-only field" claim (1400-1401: "every OTHER
  emitted column") and H1 both produces AND consumes it. No producer-only-field violation.
- **The `SK-V18-open` anchor** is well-defined (the W0-locked tranche-open 51/51 snapshot, §0.2
  184/198; "pinned baseline, NOT a floating one"), referenced consistently at G1/§0.5/G6/PROVE/H1. An
  implementer captures it pre-G1. Not undefined; not a REVISE.
- **The S-P1 absolutes** appear in FIVE places (§0.1#6, §0.2, §0.5, §5 G2.2, §10) and are uniformly
  labelled DIRECTIONAL antecedents / NOT the binding floor — no place treats them as the close floor.
  The mcw "1.658 (min)" / "smallest antecedent" matches S-P1 line 46 exactly; all four ratios match.
- **The (d) god-kernel bound** is machine-checked on BOTH G1 and G2 (numerator + denominator both
  emitted: `*_primitive_loc <= *_profiled_leaf_extent`), so neither hot leaf can be over-scoped into
  an arbitrarily large relabeled blob.
- **The V7 CH5 REVISE** (PROVE `json_css_preservation_held` byte-equality framing) is VERIFIED FOLDED
  at §9 line 1511 — consistent with the G3/H1 phrasing of the same check.

I find NO residual REVISE under the SOTA-PRESERVATION lens that would materially clarify or correct
an implementer's reading. Every SOTA gate names its SPEC section, its falsifier, and its plane; both
hot leaves (94.1% CSS / 91.5% JSON) are preserved as (a)-(d)-gated primitives with a machine-checked
LOC bound; no wave can regress a leaf silently (G1 byte-equivalence + ±1.0% pin; G2 same-run +
parity-before-speed; G3 byte-equivalent output + emit_shape_source; G6 byte-exact differential +
admission two-conjunct + honest-`S` non-speedup handling; PROVE byte-equality; H1 quiet-bar + closed
enum + directional re-confirm). The doc has converged on this lens (V8 clean, V9 clean = the
2-consecutive-clean target). Forcing a wording REVISE on a 1660-line doc already through 6+ clean
cycles would be invented churn, which the CH5 charter explicitly forbids.

No REJECT: every SOTA gate is falsifiable (each has a stated RED predicate); the
PRUNE→G1..G6→PROVE→H1 sequence is sound for SOTA-preservation (G2 before G6; P3 before both; the
pre-G2 baseline captured before the pre-G2 code is deleted); no addendum (5 timed-plane/corpus-in-timer,
6 acceleration-wiring, 1 verbatim-blob, the (a)-(d) escape) is violated under this lens; no S-P2 §3
sequencing or S-P0 addenda violation surfaced.

TALLY accept=18 revise=0 reject=0
