# SK-V18 S-P3 CHALLENGE — V8 / CH5 (SOTA-PRESERVATION lens)

Lens: SOTA-PRESERVATION. Is the >SOTA-regression gate bound at G2 AND G6 (corpus-in-timer,
the per-corpus floors), the hot leaves (CSS 94.1% scalar scan; JSON 91.5% sink leaf) preserved
as gated primitives, with NO wave silently regressing them? Doc: `SPEC.md` (1660 lines),
read against S-P2 `SYNTHESIS-RESEARCH.md §3` and the S-P0 `SYNTHESIS-AUDIT-OVERFIT.md` addenda.

Posture: prior cycles V1–V5 (and V6/V7 this P3) reached reject=0; CH5's charge is the RESIDUAL
precision REVISE drive to a 2-consecutive-clean fixed point, plus any genuine REJECT. Proportionate:
a wording nit on a 1642-line doc is a REVISE only if it would mislead an implementer.

Grounding re-checked on disk this pass:
- S-P1 `SYNTHESIS-PROFILE.md`: CSS 94.1% (`find_component_delim` 79.5% + `consume_balanced_at`
  14.6%) §2 line 96-98; JSON 91.52% (`parse_object_value_at_direct` 79.82% + `parse_array_element_at_direct`
  11.70%) line 62-70; per-corpus `track1_rich/lcss` 2.190/3.375/1.658/2.101 line 44-47; loadavg
  4.35 line 15; "absolute Mbps DIRECTIONAL, NOT re-locked; same-run ratios + hot-leaf rank are
  load-robust" line 21-24. EVERY SPEC figure under my lens matches the S-P1 ledger byte-for-byte.
- S-P2 §3 G2 exit-gate (lines 167-175): "EXPLICIT >SOTA-regression gate distinct from parity —
  track1_rich/lightningcss >= the S-P1 ratio … cold, corpus-in-timer … the bench re-measurement
  is the binding regression falsifier." The SPEC G2.2-conjunct-4 carries this AND HARDENS it
  (same-run pre-G2 baseline replacing the un-re-locked absolute — the unfalsifiable-gate cure).
- S-P2 §3 G5/G6 (lines 189-199): timed-plane binding (addendum 5) — checkasm is correctness-only,
  any Mbps FIGURE from the corpus-in-timer symmetric harness, speedup CLAIM deferred to H1. The
  SPEC §8 carries this verbatim in intent.

## Enumeration of every wave-gate / telemetry / close claim under the SOTA-PRESERVATION lens

### G2 — CSS >SOTA-regression gate (the PRIMARY locus)

**[C-G2.1] The >SOTA gate is bound at G2, corpus-in-timer, per-corpus, distinct from parity.**
SPEC §0.1#6 (104-128), §5 G2.2-conjunct-4 (960-970), telemetry `g2_sota_ratio_held` +
`corpus_in_timer` (1035-1037), gate-json REJECT list (1044-1050). The binding falsifier is the
SAME-RUN `track1_rich/lightningcss > 1.0×` per corpus AND no same-run regression vs the
G2-entry-captured pre-G2 baseline (`track1_rich_over_lcss_ratio_pre_g2`), cold, corpus-in-timer.
Gate-before-speed enforced: `g2_cssparser_oracle_parity` (the 9-field EXACT oracle) MUST pass
BEFORE any speed row is read (959, 1047). The S-P1 absolutes are demoted to DIRECTIONAL antecedents,
NOT the floor — keying the close on an un-re-locked absolute is named as the unfalsifiable-gate
hazard and explicitly replaced by the same-run comparison (a uniform host depression cancels on
both sides). **ACCEPT** — this is the sharpest, most falsifiable form of the gate; it strictly
HARDENS the S-P2 §3 "≥ the S-P1 ratio" formulation (which keyed on the load-depressed absolute)
into a load-robust same-run delta, closing the exact §5-risk-7 hazard. Sound.

**[C-G2.2] CSS 94.1% hot leaf preserved as a GATED primitive (`css_balanced_component_scan`).**
SPEC §1 (a)-(d) gate (358-379), §5 G2.3 (984-1003), telemetry `g2_balanced_scan_primitive_abcd_pass`
/ `_loc` / `_profiled_leaf_extent` / `_arg_mutation_fires` (1029-1032). The 94.1% leaf is admitted
ONLY under all four: (a) grammar-INVOKED-by-name, (b) emitted ARG byte-sets VARY under invoking-rule
mutation, (c) `verbatim_blob_present == false`, (d) PROFILE-PROVEN-NARROW-LEAF (`g2_balanced_scan_primitive_loc
<= g2_balanced_scan_profiled_leaf_extent`, a machine-checked god-kernel bound). The (d) numerator/
denominator are BOTH emitted as telemetry, so the LOC bound is machine-checked, not asserted.
**ACCEPT** — the hot leaf is preserved as a gated primitive, NOT a silent blob, with the
god-kernel (over-scoping) REJECT mechanically enforced.

**[C-G2.3] Throughput-equivalence PROVEN by named falsifier, not asserted.** §0.1#6 (119-126):
"the 94.1% CSS scalar scan … re-emitted with bodies whose throughput-equivalence is PROVEN by the
named falsifiers — NOT asserted: … the CSS scan by `g2_cssparser_oracle_parity` … THEN the same-run
`track1_rich/lightningcss > 1.0×` ∧ no same-run regression vs the G2-entry-captured pre-G2 baseline
(the speed falsifier is admissible ONLY after the parity falsifier passes)." **ACCEPT** — oracle-parity
(correct output) is correctly distinguished from throughput-preservation (the bench re-measure), and
the SPEC states plainly "G2 RE-DERIVES the 94.1% scan, so oracle parity does NOT prove throughput
preservation" (968). This is the exact S-P2 §3 G2 caveat, faithfully carried.

**[C-G2.4] The ≥1-regular-corpus mandate + tailwind/mcw residual policy.** §0.5 table (303-308),
`g2_sota_ratio_held` (1035): PASS REQUIRES ≥1 REGULAR corpus (animate OR bootstrap) crossing > 1.0×
with no regression; tailwindcss below 1.0× is an honest residual (NOT tranche-blocking provided a
regular corpus holds); mcw/full-corpus regression reported honestly. **ACCEPT** — a non-trivial
per-corpus floor with an honest-residual escape for the hardest corpus, NOT a corpus-average paper
(§11 pre-blocks "a corpus-average substituting for per-corpus ratios", 1625). Falsifiable and honest.

### G6 — NEON acceleration / the deferred speedup figure

**[C-G6.1] The corpus-in-timer >SOTA figure is bound, but DEFERRED from G6 to H1 — correctly.**
SPEC §8 exit gate (1361-1364), telemetry `g6_speedup_median_mbps` (1388), §0.4 `corpus_in_timer`
(255). G6 emits NO `corpus_in_timer` column and NO non-null speedup; `g6_speedup_median_mbps` is
null pre-H1; the G6 outcome is `C` (correctness) until H1 produces the figure on the symmetric
`css_canon_bench` plane. Addendum-5 plane-mismatch (a speedup off the checkasm plane) is a REJECT
(1375, 1398-1400). This is the S-P2 §3 timed-plane binding (lines 194-199) EXACTLY: "addendum 5
is not enforced one wave too late" — the figure plane is bound at G6, the figure VALUE is produced
at H1. **ACCEPT** — the >SOTA figure IS bound to the corpus-in-timer plane at G6 (the plane-mismatch
REJECT fires at G6); only the magnitude is deferred. The lens question "bound at G6" is satisfied:
the binding is the plane-constraint, which is live at G6.

**[C-G6.2] The 94.1% leaf reached AT ADMISSION — two-conjunct proof, no silent dead kernel.**
SPEC §0.1#10 (154-165), §8 exit gate (1345-1356), telemetry `acceleration_at_admission` +
`simd_admission_caller` + `simd_admission_profile_sampled` (1381-1383). `admission` REQUIRES BOTH
(i) the `generated.rs` caller census (non-empty, NOT `#[cfg(test)]`) AND (ii) the samply
runtime-reachability conjunct (`runtime_simd` entry in the `css_canon_bench` sample with non-zero
self-time). A census hit in dead/unreachable code that the profile does NOT see == `dead` == REJECT.
**ACCEPT** — this directly forecloses the R7 "NEON dead at admission" recurrence: a source-grep-only
proof FAILS; the profile-attribution is the binding second conjunct. The hot leaf is wired, not
orphaned (no-orphan-kernel law, 1335-1336).

**[C-G6.3] dav1d/checkasm correctness gate over the REAL corpora (not micro-cases).** §8 (1357-1360):
`neon_significant_skip_matches_scalar == PASS` over the REAL 71KB-495KB corpora (micro-case-only PASS
does NOT satisfy); `checkasm_differential == PASS`; the three retarget seams bit-exact. **ACCEPT** —
correctness-before-speedup, real-corpus parity; the SK-V5 orphan-kernel failure mode is foreclosed.

**[C-G6.4] G5 (json/scan.rs) retire is N/A, not a SOTA regression.** §8 (1325-1327, 1365-1366):
the zero-sampled `json/scan.rs` (R12) is retired/neutralized; G5 authors NOTHING for JSON; outcome
`N` (nothing on product path). `json_guard_held == true` (51/51 within ±1.0% of SK-V18-open) holds.
**ACCEPT** — S-P1 confirms JSON has NO G5 hot leaf (no JSON classifier to author); retiring a
zero-sampled scanner cannot regress a leaf it never served. Honest.

### G1/G3/H1 — the JSON 91.5% leaf + cross-wave preservation

**[C-J.1] JSON 91.5% hot leaf preserved as a GATED primitive across every wave.** SPEC §4 G1.3
(856-871), G1.2-conjunct-3 (842-848), telemetry `g1_hot_leaf_preserved` + `g1_leaf_primitive_abcd_pass`
+ `g1_dispatch_triple_not_lcd_collapsed` (878-884); G3 conjunct-7 (1125-1126) re-asserts it through
the un-forked renderer; G5/G6 `json_guard_held` (1366) and H1 `json_guard_held` (1590) carry the
51/51 floor. The leaf scanners (`decode_json_string_to_arena`, `parse_number_*`) are admitted under
the SAME (a)-(d) gate as CSS, with the same machine-checked (d) LOC bound (`g1_leaf_primitive_loc <=
g1_leaf_primitive_profiled_leaf_extent`, 882-884). The dispatch-triple LCD-collapse REJECT
(845-848, 871) protects the monomorphized-sink leaf. **ACCEPT** — the JSON hot leaf is preserved by
the same gated-primitive discipline as CSS, with the LCD-flatten regression explicitly pre-blocked.

**[C-J.2] No wave silently regresses the JSON leaf — the ±1.0% guard is pinned, not floating.**
The JSON guard floor is anchored to `SK-V18-open` (the PINNED baseline, NOT a floating one) at
G1 (886), §0.5 (310-311), G5/G6 (1366), H1 (1590), and is carried through every generalization wave.
G3 re-asserts the leaf byte-equivalence (identical inline cfg + sink.* call sites) since the un-fork
changes the PATH, not the OUTPUT (1123-1126). **ACCEPT** — the floor is pinned (a floating baseline
would let a slow drift evade the gate); the leaf preservation is mechanical (byte-equivalence) at
G3, not merely a throughput band.

### Close-condition / framing honesty (H1)

**[C-H1.1] CSS framing honesty bound — lazy-rich-vs-eager-cssom, closed enum.** §0.2 (187-195),
§0.4 `materialization_framing` (256, enum CLOSED to two values so the gate can REJECT any other),
§10 (1545-1549, 1563), gate-json REJECT (1594-1595). An unqualified "beats CSSOM"/"equal-work"
re-label WITHOUT the materialization-depth asymmetry disclosed is a REJECT (R-A0-1). **ACCEPT** —
the >SOTA claim cannot be inflated by a re-label; the closed enum makes `undisclosed`/any-other a
mechanical RED. This discharges R14/R-A0-1.

**[C-H1.2] The quiet-bar (host_loadavg < 1.0) gates any ABSOLUTE Mbps claim.** §0.3 (225-229),
§0.4 `host_loadavg` (272), §10 falsifiers (1577-1580), gate-json REJECT (1596-1599). An absolute
`g6_speedup_median_mbps` with `host_loadavg >= 1.0` or no stamp is RED; reporting DIRECTIONAL with
the load caveat is outcome `S` (honest residual), NOT `A`. **ACCEPT** — this is the §5-risk-7
load-honesty discipline, enforced at the schema; the S-P1 loadavg-4.35 capture cannot be laundered
into an absolute close claim.

**[C-H1.3] H1 RE-CONFIRMS, never RE-MEASURES, the pre-G2 ratio — the temporal-soundness fix.**
§0.1#6 (108-113), §0.2 (181), §10 (1567-1572), `css_sota_ratio_held` (1591). The pre-G2 code is
GONE post-G2, so the pre-G2 baseline is CAPTURED AT G2 ENTRY (one quiet run measuring both the
pre-G2 checkout AND the post-G2 build), the regression falsifier FIRES at G2 exit, and H1 only
re-confirms the G2-recorded ratio DIRECTIONALLY. **ACCEPT** — this resolves a genuine temporal
hazard (you cannot re-derive a pre-G2 figure after the pre-G2 code is deleted) that a naive
"re-measure at H1" gate would have left unfalsifiable. The capture-at-G2-entry mechanism is the
correct and only sound placement. A tailwind miss recorded at G2 is re-confirmed as a residual at
H1, NOT re-litigated as a fresh H1 block — consistent across §0.5, §5, §10.

### Cross-cutting / sequencing under the lens

**[C-X.1] The relocated-seam guard protects the leaves at the un-fork (G3).** §0.1#2 (71-78),
§6 G3.2-conjunct-5 `emit_shape_source == lowered_program` (1114-1121). Without this conjunct, a
fork relocated into the neutral per-profile columns passes conjuncts 1-4 under a green gate — and
such a relocated per-grammar branch could re-introduce a CSS-specific (or JSON-specific) code path
that silently regresses a leaf. The fourth conjunct (the emitter reads output-shape ONLY from the
lowered program, never from `target.*`/`contract.*`) forecloses it. **ACCEPT** — the un-fork cannot
relocate a leaf-regressing branch into data; the structural co-gate (`runtime_target_rows_collapsed`
via R16 full-row PartialEq) plus the `emit_shape_source` grep are jointly binding. This is the
§5-risk-1 binding, faithfully carried, and it directly protects against a silent leaf regression
masquerading as an un-fork (§11 G3 pre-block, 1621).

**[C-X.2] G2 entry dual-gates on P3 so the 94.1% scan is derived ONCE, not 7×.** §5 G2.1 (935-942),
`g2_css_replica_singular` (1039), §8 `css_scan_call_site_singular` (1364). If P3 has not collapsed
the 7 replicas, G2 would re-derive the SAME scan into 7 byte-identical files and G6 would emit the
NEON call 7 ways — a fork of the very hot leaf. P3 closure is the independent conjunct that prevents
this. **ACCEPT** — the singular-scan invariant is the precondition for both the G2 derivation and
the G6 single wire; without it the hot leaf re-forks.

## Residual-REVISE sweep (the CH5 precision charge)

I searched specifically for: (1) an unfalsifiable SOTA gate; (2) a leaf regression a wave could
slip silently; (3) a per-corpus floor stated as an average; (4) a G2/G6 figure on the wrong plane;
(5) a hot leaf admitted outside the (a)-(d) gate. Findings:

- The G2 same-run gate, the G6 admission two-conjunct, the (a)-(d) leaf primitives, the pinned
  JSON ±1.0% floor, the closed `materialization_framing` enum, the quiet-bar, and the
  capture-at-G2-entry baseline are ALL falsifiable and consistently stated across §0/§1/§5/§8/§10/§11.
- The `g6_speedup_median_mbps` null-pre-H1 / produced-at-H1 split is internally consistent: line
  1400-1401 carves it explicitly out of the "no producer-only field" claim ("every OTHER emitted
  column"), and H1 §10 both produces AND consumes it. No producer-only-field violation.
- The S-P1 absolutes appear in FOUR places (§0.1#6, §0.5, §5 G2.2, §10) and are uniformly labelled
  DIRECTIONAL antecedents / NOT the binding floor — no place treats them as the close floor. The
  one numeric I cross-checked (mcw "1.658 min" / "smallest antecedent") matches S-P1 line 46.
- The (d) god-kernel bound is machine-checked on BOTH G1 and G2 (numerator + denominator both
  emitted), so neither hot leaf can be over-scoped into an arbitrarily large relabeled blob.

I find NO residual REVISE under the SOTA-PRESERVATION lens that would materially clarify or correct
an implementer's reading. Every gate names its SPEC section, its falsifier, and its plane; every
hot leaf (94.1% / 91.5%) is preserved as an (a)-(d)-gated primitive with a machine-checked LOC bound;
no wave can regress a leaf silently (G1 byte-equivalence + ±1.0% pin; G2 same-run + parity-before-speed;
G3 byte-equivalent output + emit_shape_source; G6 admission two-conjunct; H1 quiet-bar + closed enum).
The doc has converged on this lens. Forcing a wording REVISE here would be invented churn, which the
CH5 charter explicitly forbids on a 1642-line doc already through 5+ clean cycles.

No REJECT: every SOTA gate is falsifiable (each has a stated RED predicate), the PRUNE→G1..G6→PROVE→H1
sequence is sound for SOTA-preservation (G2 before G6; P3 before both; the pre-G2 baseline captured
before the pre-G2 code is deleted), and no addendum (5 timed-plane/corpus-in-timer, 6 acceleration-wiring,
1 verbatim-blob, the (a)-(d) escape) is violated under this lens.

TALLY accept=14 revise=0 reject=0
