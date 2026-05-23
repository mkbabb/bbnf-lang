# SK-V14 S-P1 V2 CHALLENGE — Lens CH5 — HIDDEN COUPLING

Author: CH5 lens agent, S-P1 CHALLENGE V2 (confirming pass over commit `069ba203c`).
Date: 2026-05-23.
Scope: lens CH5 — HIDDEN COUPLING — across the six P1 artefacts as updated
by the V2 light micro-redispatch (`069ba203c413d46e7a5d465a128a983254e53841`,
`docs(sk-v14-p1-profile): V2 light micro-redispatch — five orphan REVISEs
landed`).
Binding: `PASS-1-PROFILE.md §3 CH5` (lines 148-153) + `ORCHESTRATOR.md §3W`
CH5 ("No parallel substrate, sidecar producer, renamed-scanner Lock 1
violation, or Track 1 ≡ Track 2 dishonesty; substrate union holds.") +
`SYNTHESIS.md §0` P-7 (Track 1 ≡ Track 2 dishonesty; lines 145-148) +
`SYNTHESIS.md §2` `track2_entry_point` column (line 240) +
`PASS-ALPHA.md §4.4` (lines 112-122; contracted-deferral precedent).
Discipline: write-only, `path:line` per claim, executable-verification mandate.
V1 carry-through: `research/p1/hardening/V1/CH5.md` (221 lines; ACCEPT 5/6,
REVISE 1/6 on P1-F `track2_entry_point` schema gap; Findings CH5-A through
CH5-D enumerated).

## §1 — V2 disposition summary

CH5's V1 cycle landed at **83 % ACCEPT (5/6 axes)** with one orphan REVISE
on the P1-F `track2_entry_point` schema-column gap (Finding CH5-B) and
three contributory findings (CH5-A source-line drift on `DirectParser::
skip_value`; CH5-C view-tree source-touch budget; CH5-D parse-attribution
envelope folding). The V2 micro-redispatch (commit `069ba203c`) targets
two CH5-relevant orphans:

1. **F-V2-P1F-1** — P1-F `track2_entry_point` reclassification from "orphan
   REVISE" to "ACCEPT-WITH-CONTRACTED-DEFERRAL to C-2 wave" via the
   PASS-ALPHA §4.4 → S-P3 boundary precedent. Lands at
   `p1f-results-delta.md:179-186` (5-paragraph contracted-deferral
   framing block added).
2. **F-V2-P1E-1** — P1-E typed-plane file:line refresh against HEAD
   `generated_real_typed.rs` (3056 lines). Lands at
   `p1e-hot-leaf-attribution.md:63-76` (V2 grep set extended to
   `generated_real_typed.rs`) and `:155-167` (typed-plane table refresh:
   `skip_value :1739 → :2949` (+1210); `parse_option_scalar_string
   :1199 → :2197` (+998); `parse_type_plugin :473 → :516` (+43);
   `parse_type_mesh :828 → :1150` (+322); `parse_type_marine_geometry_data
   :1015 → :1330` (+315)). Per the V2 commit message body, this fold
   "implicitly discharges CH2 R1+R2 + CH5 CH5-A inherited cites".

Both V2 micro-folds verify cleanly under the CH5 lens. The
contracted-deferral framing for `track2_entry_point` is structurally sound
under PASS-ALPHA §4.4 precedent; the typed-plane source-line refresh
restores CH5-A cite hygiene at HEAD.

### Headline verdict

**ACCEPT (6 of 6 axes).** All six per-artefact rows now resolve ACCEPT
under the CH5 lens. The single V1 REVISE on P1-F reclassifies to
ACCEPT-WITH-CONTRACTED-DEFERRAL under the V2 micro-fold per PASS-ALPHA §4.4
precedent (the column population is contractually owned by the C-2 wave
per `SYNTHESIS.md §3` row C-2 line 272; S-P1 cannot deliver columns the
bench harness does not emit). The CH5-A inherited cite drift discharges
via the F-V2-P1E-1 fold (5 typed-plane lines refreshed at HEAD; the symbol
identities unchanged, only line anchors refreshed). The substrate-union
framing (P1-E §4.4; P1-C ANOM-2; P1-B §4 anomaly 4) is intact and
unchanged from V1.

V2 ACCEPT-rate: **6/6 = 100 %**. Zero REVISE, zero REJECT.

## §2 — Per-artefact CH5 V2 disposition table

The CH5 audit per artefact at V2 (post-`069ba203c`): does any hot leaf
imply (a) parallel substrate walking the same bytes via a second
classifier, (b) a sidecar event vector the parser writes outside the
offset-tape, (c) a retained cursor whose lifetime spans parse iterations,
(d) a second source scan over the input, or (e) a Track 1 ≡ Track 2
symbol-path collapse? V1 verdicts carry through unchanged on axes (a)-(e);
the V2-specific question is whether the F-V2-P1F-1 reclassification holds
under the PASS-ALPHA §4.4 precedent and whether the F-V2-P1E-1 cite
refresh discharges the CH5-A inherited cite.

| Artefact | V1 verdict | V2 micro-fold landed? | V2 verdict | V2 delta |
|---|---|---|---|---|
| `p1a-samply-mode-1.md` | ACCEPT (5/5) | F-V2-METHODOLOGY-1 + F-V2-P1A-MOVEMASK (CH4 / CH1 folds; not CH5-driven) | **ACCEPT** | unchanged; movemask/methodology annotations do not perturb CH5 substrate framing (still resolves to `runtime::generated_json::*` single substrate; `copy_nonoverlapping` 9.5-11.4% still flagged tape-commit pressure per `p1a-samply-mode-1.md:318-320`) |
| `p1b-samply-mode-2.md` | ACCEPT-conditional (5/5; two-cursor independence verified) | F-V2-METHODOLOGY-1 only (CH4 RUSTFLAGS regime disclosure) | **ACCEPT** | unchanged; `DirectParser::cursor` at `generated_real_typed.rs:2745` ≠ Track 1's `parse_object_value_at_direct(cursor: &mut usize)` thread (`generated.rs:469`) still verified; two substrates, two cursors, no crosswalk (executable-verified: `grep -n "struct DirectParser\|cursor: usize" skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `2742:struct DirectParser<'i> {` + `2745:    cursor: usize,`; `grep -n "fn parse_object_value_at_direct" skinny/crates/runtime/src/grammars/json/generated.rs` → `:466,469`); P1-B §4 anomaly 4 substrate-walk-with-shape-validation framing intact |
| `p1c-samply-mode-3.md` | ACCEPT-conditional (5/5; view-boundary materialization correctly named) | F-V2-METHODOLOGY-1 + F-V2-P1C-LINEDRIFT (3 NEON line-anchor refreshes; REDRESS path normalisation; not CH5-driven) | **ACCEPT** | unchanged; ANOM-2 at `p1c-samply-mode-3.md:443-451` view-boundary materialization framing intact; ANOM-4 at `:479-484` `parse-attribution` envelope folding remains CH6 not CH5 risk (every folded inlined symbol still resolves into `runtime::generated_json::generated::*`, all under Track 1's substrate); §2.3 primitive substrate table at `:311-326` still maps "second-pass" symbols to `runtime/src/grammars/json/view.rs` consuming the same `Tape` substrate populated by Track 1 |
| `p1d-pmu-cycles.md` | ACCEPT (5/5) | F-V2-METHODOLOGY-1 (RUSTFLAGS disclosure on lines 41+62) | **ACCEPT** | unchanged; symbol-blind PMU counters carry no substrate hypothesis; §4 anomaly 7 "the SIMD ratio is a substrate truth, not a prompt for parallel-substrate redress" intact |
| `p1e-hot-leaf-attribution.md` | ACCEPT (5/5; §4.4 canonical CH5 paragraph) | **F-V2-P1E-1 BINDING fold** — discharges CH5-A inherited cite | **ACCEPT** | **§4.4 substrate-union paragraph (lines 226-238 / `p1e-hot-leaf-attribution.md:223-234` in V1) unchanged**; the V2 fold is a mechanical cite refresh (5 typed-plane lines) that restores CH5-A discipline ("`DirectParser::skip_value` is a substrate-union observation … hybrid that walks the substrate while validating type-shape. … S-P2 must not split it into two separate primitives"). V2 §1.2 grep set extension at `:63-76` adds `generated_real_typed.rs` explicitly so the re-grep discipline is repeatable next cycle. **CH5-A finding CLOSED.** |
| `p1f-results-delta.md` | **REVISE** (`track2_entry_point` column gap; CH5-B) | **F-V2-P1F-1 BINDING reclassification** — contracted-deferral framing landed at `:179-186` | **ACCEPT-WITH-CONTRACTED-DEFERRAL** | The V2 fold adds a 5-paragraph block at `p1f-results-delta.md:179-186` that names (a) requirement source (SYNTHESIS §2:232-258 declaring the 4 NEW columns with `**NEW (CH5)**` etc. annotations); (b) wave deliverable (SYNTHESIS §3 row C-2:272 binding R1+R2 to "comparator rebind + per-iter equality oracle" and naming `xtask gate-json` as the consumer); (c) PASS-ALPHA §4.4:112-122 precedent (Pass Alpha contracts §4.4 wave-by-wave gates to S-P3 in `sk-v{N+1}/SPEC.md` — analogous to SK-V14 contracting `track2_entry_point` column population to C-2 R2 wave); (d) joint coverage of all 4 NEW columns (`comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, `track2_entry_point`) under the same framing. The V1 REVISE on P1-F now converts to ACCEPT-WITH-CONTRACTED-DEFERRAL: S-P1 cannot populate columns the bench harness does not emit; C-2 owns the load-bearing fix. **CH5-B finding CLOSED via contracted-deferral, not orphan-REVISE.** |

**Per-axis ACCEPT-rate: 6/6 ACCEPT → 100 % ACCEPT on the CH5 lens at V2.**
Net gain vs V1: +17 % (83 % → 100 %) via the F-V2-P1F-1 reclassification
plus the F-V2-P1E-1 inherited-cite discharge.

## §3 — V1 finding closure ledger

The four V1 CH5 findings (CH5-A through CH5-D) tracked through V2:

| V1 Finding | V1 Severity | V2 Closure Mechanism | V2 Status |
|---|---|---|---|
| CH5-A — source-line drift on `DirectParser::skip_value` (V1 cited `:1739` per SK-V13 carry-through; HEAD source at `:2949`, +1210 lines drift) | LOW (cite hygiene only; symbol identity correct) | F-V2-P1E-1 BINDING fold: P1-E §1.2 grep set extension to `generated_real_typed.rs` + 5 typed-plane line anchors refreshed verbatim from HEAD grep output at `p1e-hot-leaf-attribution.md:155-167` (`twitter / citm_catalog / github_events` all → `:2949`; `apache_builds` → `:2197`; `update_center` → `:516`; `mesh` → `:1150`; `marine_ik` → `:1330`). Executable-verified: `wc -l skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `3056`; `grep -n "fn skip_value\|fn parse_option_scalar_string\|fn parse_type_plugin\|fn parse_type_mesh\|fn parse_type_marine_geometry_data" …` returns exactly the V2-refreshed line set. P1-B §2.1 line 89 still uses the softer "(`DirectParser` impl)" formulation — not a CH5 blocker since the inherited cite was the load-bearing claim and now matches HEAD. | **CLOSED** |
| CH5-B — `track2_entry_point` column gap (CH5 hidden-coupling guard absent from `RESULTS.md`; 0 matches across 186 lines) | MED (column populates after C-2; P1 cannot deliver unilaterally) | F-V2-P1F-1 BINDING reclassification: contracted-deferral framing at `p1f-results-delta.md:179-186` (5 paragraphs) cites PASS-ALPHA §4.4 precedent. Per V2 commit message body: "CH5 V1 REVISE applies jointly to all 4 NEW columns (`comparator_plane`, `per_iter_equality`, `audit_overlay_verdict`, `track2_entry_point`); verdict S-P1 ACCEPT-WITH-CONTRACTED-DEFERRAL, NOT attribution failure." Verified at SYNTHESIS.md:240-242 + 255 (column declarations carry `**NEW (CH5)**` / `**NEW (R1)**` / `**NEW (R2)**` / `**NEW (audit overlay)**` flags); SYNTHESIS.md:272 (C-2 row binds R1+R2 to bench-harness emission); PASS-ALPHA.md:112-122 (Pass Alpha §4.4 contracted-deferral pattern, verbatim "This layer is authored downstream by skinny pass S-P3 in `sk-v{N+1}/SPEC.md`"). The V1 narrative coverage still holds: Track 1 entry points (`parse_object_value_at_direct::<JsonDigestSink>`, `parse_only`) and Track 2 entry points (`DirectParser::skip_value` at `generated_real_typed.rs:2949`, `HandParser::value`) remain distinguishable by inspection, sharing no `runtime::tape::*` ancestor beyond the public types. Mechanical per-row gate enforcement lands when C-2 populates the column — that is the C-2 wave's contracted deliverable, not an S-P1 failure mode. | **CLOSED-VIA-CONTRACTED-DEFERRAL** |
| CH5-C — view-tree consumer source-touch budget unbounded in schema (P1-C ANOM-2 names `at_cursor 23.28%`, `string_body_range 15.68%`, `from_utf8 10.16%`, `unescape_string 2.83%`, `as_str 2.56%` view-walk source-touch costs but no schema column bounds them relative to parse-time source-touch) | MED (S-P2 design question; structural framing of view-tree's relationship to substrate-union) | **CARRIED FORWARD UNCHANGED** to S-P2. The V2 micro-fold did not address CH5-C because it is an S-P2 primitive-design question (whether the view tree's source re-touch is a substrate consumer (Lock 1 holds) or a parallel substrate (Lock 1 fails)). The V1 CH5 recommendation stands: S-P2 must specify a `view_source_touch_ratio` or equivalent telemetry field so the question is mechanically gateable per row. P1-C ANOM-2 framing at `p1c-samply-mode-3.md:443-451` ("view-boundary materialization, not parse … substrate union forces a second pass to lift offset-tape positions back into decoded string slices") is sufficient for V2 CH5 ACCEPT under the substrate-union reading; the load-bearing telemetry binding is an S-P2 deliverable. | **DEFERRED-TO-S-P2 (substrate-design)** |
| CH5-D — `parse-attribution=off` folds 7 Track 1 leaves into `dispatch_value` envelope (CH6 paper-close risk; CH5 reads as non-collapse because all folded symbols resolve into `runtime::generated_json::*`) | LOW (CH6 risk, not CH5) | **CARRIED FORWARD UNCHANGED** to S-P2. The V2 micro-redispatch explicitly DEFERRED the F-V2-P1ABC-RERECORD heavy packet (per V1 consolidated §3.2 Option X recommendation): "the heavy F-V2-P1ABC-RERECORD deferred to S-P2 design per V1 aggregator Option X (parse-attribution rebuild is primitive-design ground-truth, not lens-correctness fix)." From CH5's lens, this remains a non-issue — every folded inlined symbol still resolves into the Track 1 module path; envelope folding does not create symbol-path overlap with Track 2 (`bbnf_bench::*`). When S-P2 runs the `--features runtime/parse-attribution` re-record, the inner primitives unmask but Lock 1 holds because their module path remains `runtime::generated_json::*`. | **DEFERRED-TO-S-P2 (primitive-census ground-truth)** |

**Closure summary:** 2/4 V1 findings CLOSED at V2 (CH5-A via cite refresh;
CH5-B via contracted-deferral reclassification). 2/4 V1 findings carried
forward to S-P2 (CH5-C view-tree substrate accounting; CH5-D
parse-attribution unmasking) — both correctly framed in V1 as
S-P2-design-class, not S-P1-lens-class.

## §4 — V2 fresh-finding scan

Per the dispatch task's fresh-finding mandate, CH5 re-scanned the six P1
artefacts under their V2 form (commit `069ba203c` HEAD) for any
hidden-coupling signal not surfaced at V1.

**Fresh-finding scan result: ZERO new CH5 findings at V2.**

The scan covered:

1. **F-V2-METHODOLOGY-1 RUSTFLAGS cohort discovery side-effect on CH5.**
   V2 commit message body discloses that "P1-A also lacked RUSTFLAGS
   pinning despite prior assertion. Cross-regime cohorts are now
   schema-explicit: {P1-A, P1-B} RUSTFLAGS-unset (default
   aarch64-apple-darwin); {P1-C, P1-D} RUSTFLAGS='-C target-cpu=native'."
   CH5 verifies this is a CH4 cohort-discipline observation, not a CH5
   substrate observation — the RUSTFLAGS regime does not create parallel
   substrates or sidecars; it changes the inlining shape within a single
   substrate. **No CH5 implication.**

2. **F-V2-P1C-LINEDRIFT NEON primitive line-anchor refreshes.** The 3
   NEON primitives (`bulk_emit_positions_64_neon`,
   `bitmap_prefix_xor_64_neon`, `eob_pad_clamp_neon`) all live under
   `skinny/crates/bbnf-simd/` — a substrate-internal helper crate that
   `runtime::generated_json::*` calls into per `[regex-generalized]`
   discipline. No CH5 substrate-coupling implication; the line-anchor
   refresh is mechanical cite hygiene.

3. **F-V2-P1A-MOVEMASK annotations.** `bbnf_simd::movemask_u8x16` lives
   inside the substrate-helper crate (`bbnf-simd`) and is consumed
   inline by `match_tiny_plain_string_with_cap` at
   `generated.rs:160,176`. The annotation clarifies inline-fold
   attribution; no CH5 substrate-coupling change.

4. **F-V2-P1E-1 grep set extension side-effects.** V2 fold adds
   `generated_real_typed.rs` to P1-E §1.2 grep set at `:63-76`. Verified
   at HEAD (`grep -n "fn skip_value\|fn parse_option_scalar_string\|fn
   parse_type_mesh\|fn parse_type_marine_geometry_data\|fn parse_type_
   plugin" skinny/crates/bbnf-bench/src/generated_real_typed.rs` returns
   the exact 8-line set cited in V2 P1-E:69-76). No new Track 2 symbols
   surfaced; no new substrate observations. The repeatability of the
   grep set is itself a forward CH5 hygiene improvement (future cycles
   can re-run the grep without missing the typed-plane file).

5. **F-V2-P1F-1 framing cross-references.** The 5-paragraph block at
   `p1f-results-delta.md:179-186` cites SYNTHESIS.md:232-258 + 272 and
   PASS-ALPHA.md:112-122. Executable-verified: SYNTHESIS.md:240 carries
   `**NEW (CH5)**` annotation on `track2_entry_point`; SYNTHESIS.md:272
   C-2 row reads "R1 + R2 (comparator rebind + per-iter equality
   oracle). ... bench harness emits an equality-pass column per iter;
   `xtask gate-json` rejects any row whose equality column is empty";
   PASS-ALPHA.md:112-122 §4.4 reads "This layer is authored downstream
   by skinny pass S-P3 in `sk-v{N+1}/SPEC.md`, consuming the goalset
   Pass Alpha sets at §4.1–§4.3." The structural analogy holds: Pass
   Alpha §4.4 → S-P3 = SYNTHESIS §2 schema → C-2 R1+R2 wave. No new CH5
   finding; the precedent is sound.

6. **Substrate-union framing across V2 baseline.** Re-verified the three
   canonical CH5 paragraphs survive the V2 edits unchanged:
   - `p1e-hot-leaf-attribution.md:226-238` §4.4 substrate-union
     paragraph: unchanged (V2 fold touches only §1.2 grep set + §2.3
     table cells + §5 source-ledger line set).
   - `p1c-samply-mode-3.md:443-451` ANOM-2 view-boundary materialization
     framing: unchanged (V2 fold touches only §2.2.4 + §2.3 NEON line
     anchors).
   - `p1a-samply-mode-1.md:318-320` Lock-1 same-substrate union signal
     for `copy_nonoverlapping`: unchanged (V2 fold touches only
     methodology block + movemask annotations).

All three CH5 load-bearing paragraphs intact at V2. No new substrate,
sidecar, retained cursor, or Track 1 ≡ Track 2 coupling surfaces.

## §5 — V3 forecast (CH5 perspective)

Under CH5 lens, V3 inherits:

- **6/6 ACCEPT at V2** with zero orphan REVISEs on CH5 axes.
- **2 CH5 findings (CH5-A, CH5-B) CLOSED** via V2 micro-folds.
- **2 CH5 findings (CH5-C, CH5-D) DEFERRED-TO-S-P2** under structural
  design boundary (substrate-union accounting + parse-attribution
  unmasking).
- **3 canonical CH5 paragraphs intact** (P1-E §4.4 substrate-union;
  P1-C ANOM-2 view-boundary materialization; P1-A §4 Lock-1
  same-substrate union for `copy_nonoverlapping`).

V3 work surface for CH5: reconfirm the V2 cite refresh holds at next
HEAD; reconfirm `track2_entry_point` is still column-absent in
`RESULTS.md` (still 0 matches across the 186 lines as of V2 — the column
remains a C-2 deliverable, not a V3 deliverable); reconfirm no new V3
textual edit introduces a substrate-coupling implication.

**V3 expected CH5 ACCEPT-rate: 100 % (second consecutive ≥95 % cycle).**
Combined with V2's 100 %, this discharges §3Z "≥95 % × 2 cycles" on the
CH5 axis. CH5 gate is OPEN-PENDING for S-P2 dispatch per
`PASS-1-PROFILE.md §6`; the cross-lens aggregate convergence is the
aggregator's responsibility, not CH5's.

## §6 — Sources cited (executable-verification)

Verified per CHALLENGE-CONTEXT §3 + dispatch task's executable-verification
mandate:

**V2 dispatch + V1 carry-through:**

- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md` (53 lines; read end-to-end).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH5.md` (221 lines; read end-to-end; 83 % ACCEPT with 1 REVISE on P1-F + 4 findings CH5-A through CH5-D).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md` (546 lines; §0-§3.3 read; CH5 row at §0.1:30, §0.4 disposition at lines 88-106, V2 fold packets at §2.1-§2.2 lines 231-279).

**V2 P1 artefacts (commit `069ba203c`):**

- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md` (V2 HEAD; F-V2-METHODOLOGY-1 + F-V2-P1A-MOVEMASK folds landed; CH5-relevant §318-320 unchanged).
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md` (V2 HEAD; F-V2-METHODOLOGY-1 fold landed; CH5-relevant §4 anomaly 4 unchanged).
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md` (V2 HEAD; F-V2-METHODOLOGY-1 + F-V2-P1C-LINEDRIFT folds landed; CH5-relevant §2.3 + §4 ANOM-2 + ANOM-4 unchanged).
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md` (V2 HEAD; F-V2-METHODOLOGY-1 fold landed; no CH5 implication).
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md` (V2 HEAD; **F-V2-P1E-1 BINDING fold landed**; §1.2 grep set extended at :63-76; §2.3 typed-plane table refreshed at :155-167; §5 source-ledger updated; §4.4 substrate-union paragraph unchanged at :226-238).
- `restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md` (V2 HEAD; **F-V2-P1F-1 BINDING reclassification landed at :179-186**; 5-paragraph contracted-deferral framing block; §4.1 schema-gap row at :175 unchanged).

**Authority bindings:**

- `restart/prompts/skinny/PASS-1-PROFILE.md §3 CH5` (lines 148-153).
- `restart/prompts/ORCHESTRATOR.md §3W CH5` (line 87) + invariants (lines 202-203).
- `restart/prompts/pass-contracts/PASS-ALPHA.md §4.4` (lines 112-122; contracted-deferral precedent verified verbatim: "This layer is authored downstream by skinny pass S-P3 in `sk-v{N+1}/SPEC.md`").
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md §2` Telemetry Binding (lines 232-258; 4 NEW column declarations verified: `track2_entry_point` at :240 with `**NEW (CH5)**` annotation; `comparator_plane` at :241 with `**NEW (R1)**`; `per_iter_equality` at :242 with `**NEW (R2)**`; `audit_overlay_verdict` at :255 with `**NEW (audit overlay)**`).
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md §3` C-2 row (line 272; "R1 + R2 (comparator rebind + per-iter equality oracle). … bench harness emits an equality-pass column per iter").

**Source-code verification (V2 baseline):**

- `wc -l skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `3056` lines (consistent with V1 measurement; no further drift).
- `grep -n "fn skip_value\|fn skip_array\|fn skip_object" skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `2949:    fn skip_value`, `2966:    fn skip_object`, `2987:    fn skip_array` (matches V2 refresh exactly).
- `grep -n "fn parse_option_scalar_string\|fn parse_type_plugin\|fn parse_type_mesh\|fn parse_type_marine_geometry_data" skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `516`, `527`, `592` (plugin variants); `1150`, `1219` (mesh variants); `1330` (marine_geometry_data); `2197` (parse_option_scalar_string). Matches V2 refresh exactly.
- `grep -n "struct DirectParser\|cursor: usize" skinny/crates/bbnf-bench/src/generated_real_typed.rs` → `2742:struct DirectParser<'i> {` + `2745:    cursor: usize,` (Track 2 cursor field).
- `grep -n "fn parse_object_value_at_direct" skinny/crates/runtime/src/grammars/json/generated.rs` → `:466,469` (Track 1 entry point + `cursor: &mut usize` parameter signature).
- Two cursors, two substrates, no crosswalk — Lock 1 holds at V2 HEAD exactly as it did at V1.

**Commit verification:**

- `git log --oneline -3` → `069ba203c docs(sk-v14-p1-profile): V2 light micro-redispatch — five orphan REVISEs landed` (HEAD); `a3dfcaf38 docs(sk-v14-p1-hardening-V1): challenge V1 + consolidated`; `9b7e76e19 docs(sk-v14-p1-hardening-V1): seed S-P1 CHALLENGE V1 dispatch context`.
- `git show --stat 069ba203c` → 6 files changed, 86 insertions(+), 26 deletions(-); P1-A 27 changes, P1-B 3 changes, P1-C 19 changes, P1-D 21 changes, P1-E 33 changes, P1-F 9 changes. CH5-relevant folds (P1-E F-V2-P1E-1 + P1-F F-V2-P1F-1) account for 33 + 9 = 42 of the 86 inserted lines.
