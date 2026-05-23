# SK-V14 S-P2 Research — V1 CHALLENGE Consolidated

Aggregator: SK-V14 S-P2 V1 hardening aggregator (write-only).
Date (UTC): 2026-05-23.
Scope: seven-lens CHALLENGE V1 over the six committed S-P2 P2 axis
artefacts (P2-A 367 L; P2-B 217 L; P2-C 143 L; P2-D 257 L; P2-E 342 L;
P2-F 334 L — 1660 lines, atomic commit per `S-P2-DISPATCH-CONTEXT.md`).
Authority: `restart/prompts/ORCHESTRATOR.md §3W` (lens registry) + `§3Z`
(convergence rule); `restart/prompts/skinny/PASS-2-RESEARCH.md §3`
(CH1-CH6 specialisations); `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md
§CH7` (Overfit-Prune carry-forward); dispatch
`restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CHALLENGE-CONTEXT.md`
§0-§4.
Input ledger: seven V1 lens dispositions under
`restart/skinny/tranches/sk-v14/research/p2/hardening/V1/`
(`CH1.md` 461 L; `CH2.md` 365 L; `CH3.md` 489 L; `CH4.md` 204 L;
`CH5.md` 311 L; `CH6.md` 835 L; `CH7.md` 114 L — 2779 lens lines + 33
CHALLENGE-CONTEXT lines).

## §0 — V1 cycle verdict

### §0.1 — Per-lens dispositions (verbatim from each CH file's §0/§1)

| Lens | Definition | Granularity | ACCEPT | REVISE | REJECT | Per-lens ACCEPT-rate | Verdict |
|---|---|---:|---:|---:|---:|---:|---|
| CH1 CORRECTNESS | every candidate → S-P1 hot leaf antecedent; SOTA-comparator cited against pinned-HEAD source + strict plane; ISA cited against architecture reference manual | 38 candidates / 6 artefacts | 31 candidates (2 artefacts) | 7 candidates (4 artefacts) | 0 | **candidate 81.6 % / artefact 33.3 %** | REVISE (three narrow folds: P2-B SHA pinning; zero-P1-antecedent demotion across P2-C/D/F; indirect-/envelope-antecedent disposition on P2-F C6/C7/C10/C12/C13) |
| CH2 GENERALITY | every candidate carries P2-F grammar-neutral verdict bucket; Lock 14 v+1 admission gate holds; JSON-only-no-grammar-neutral = REVISE/REJECT | 6 artefacts (42 cross-axis entries) | 6 | 0 | 0 | **100 %** | ACCEPT (2 non-blocking findings F1 + F2; load-bearing F-V2-P1ABC-RERECORD CH2/CH4 dual-gating; substrate-union YES cross-corroborated) |
| CH3 REGRESSION (REDRESS) | no candidate re-opens REDRESS routes 28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 119/120, 126; pre-blocked routes carry explicit cite + fresh material differential | 6 artefacts × 45 candidate-entries | 6 (45/45 entries pre-block-mapped) | 0 | 0 | **100 %** | ACCEPT (1 ACCEPT-WITH-NOTE on P2-E Gap 5 REDRESS-80 differential; 5 new findings F-1..F-5; quadruple canonical CH3 statement across P2-A/B/C/D) |
| CH4 COST | every candidate carries (a) scalar-reference status, (b) checkasm-parity expectation, (c) same-wave-consumer note; missing any = REJECT | 37 eligible candidates (45 total minus 4 NOT-S-P3-ELIGIBLE + 4 documented-as-pre-blocked) | 34 strict / 35 alt-satisfaction | 3 (P2-F C8 / C10 / C13) | 0 | **strict 91.9 % / alt 94.6 %** | ACCEPT-WITH-REVISE (3 REVISE clustered on Stage-A scalar-reference authoring; C12 ACCEPTed per CH4 §3 — P2-F §4 over-precaution; CF-1/-2/-3/-4 surface honest self-disclosure pattern) |
| CH5 HIDDEN COUPLING | no parallel substrate / sidecar producer / renamed scanner / second source scan / Track 1 ≡ Track 2 collapse; P2-D substrate-union YES verdict holds | 6 artefacts (6 axes a-e) | 6 | 0 | 0 | **100 %** | ACCEPT (6 findings CH5-A..CH5-F; C-P2C-2 Lock 1 dependency satisfied by P2-D YES; substrate-target taxonomy uniformly applied; substrate-union 6-witness corroboration) |
| CH6 ANTI-PAPER-CLOSE | every comparator claim cites comparator source file; every ISA claim cites manual section; every primitive claim carries scalar-reference sketch in §2; no "future wave will detail" deferrals | 47-candidate corpus (1 known C8 overlap counted once → 46 assessed) | 46 | 0 | 0 | **100 %** | ACCEPT (5 NF-CH6 findings; 2 contracted-deferral packets confirmed not-paper-close; 6 V2 cohesion fold targets surfaced) |
| CH7 OVERFIT-PRUNE | grammar-derived (no `// @generated` hand-write); Lock 14 v+1 generic-crate compliance; real source change + strict comparator + per-iter equality oracle; round-trip; no SCAFFOLD-ONLY admit | 6 artefacts × 5 mandates | 6 (30/30 mandate cells PASS) | 0 | 0 | **100 %** | ACCEPT (4 new findings; audit-overlay binding intact across all 6 — 4 direct + 2 via upstream-equivalent surface; 9-grammar enumeration verified live) |

### §0.2 — Aggregate ACCEPT-rate

Two aggregation methods per `ORCHESTRATOR.md §3Z`:

- **Sub-axis / candidate-weighted (load-bearing for §3Z convergence):**
  CH1 candidate count (31 / 38) carries the dominant denominator; CH4
  candidate count (34 / 37 strict; 35 / 37 alt); CH2/CH3/CH5/CH6/CH7
  per-artefact each (6/6 → 30/30). Combined:
  (31 + 34 + 6 + 6 + 6 + 46 + 6) / (38 + 37 + 6 + 6 + 6 + 46 + 6) =
  **135 / 145 = 93.1 %**.
- **Per-lens mean (informational; equal weight per lens):**
  (81.6 + 100 + 100 + 91.9 + 100 + 100 + 100) / 7 = **96.2 %**.

The sub-axis-weighted aggregate (93.1 %) is **below the §3Z ≥ 95 % floor**;
the per-lens mean (96.2 %) is marginally above. Per `ORCHESTRATOR.md §3Z`
the binding rule is "≥ 95 % × 2 consecutive cycles + zero orphan
REVISEs"; this cycle satisfies neither sub-clause (sub-axis aggregate
below floor AND ten orphan REVISE items outstanding across CH1 + CH4).

### §0.3 — REJECT roster (verbatim)

**Zero REJECT findings** across all 7 lenses. The seven-lens sweep
surfaces no falsification of any P2 axis claim. Every REVISE collapses
to disposition-language sharpening or Stage-A scalar-reference authoring;
zero claims fall on architectural grounds.

### §0.4 — REVISE roster (verbatim)

Ten orphan REVISE entries clustered across CH1 + CH4:

**CH1 (7 candidate-rows; 3 fold packets):**

1. **Fold-1 (P2-B §5.1 external-cite SHA pinning).** P2-B §5.1 cites
   FFmpeg `tests/checkasm/checkasm.{c,h}` + dav1d `tests/checkasm/`
   without pinned upstream HEAD SHAs (contrast with P2-A §5.3 four
   pinned comparator HEADs + P2-A §5.4 dav1d HEAD
   `1718ff9aded99f0a89f5c7940d6afb8948301e33` + FFmpeg HEAD
   `085714182302333dd83dcb9c36cf828dc4eba929`). Mechanical inheritance
   from P2-A's anchors. CH1.md §3.1.
2. **Fold-2 (zero-P1-antecedent candidate demotion across P2-C/D/F).**
   Five candidates with explicitly zero SK-V14 P1 antecedent retained
   in `## §2 — Candidate primitives` enumeration without explicit
   non-candidate framing: P2-C C-P2C-1 (PRUNE-2 CSS L4 row absent),
   P2-C C-P2C-6 (no SK-V14 three-input boolean hot expression), P2-C
   C-P2C-7 (REDRESS-126 orphan), P2-D C-P2D-3 (sparse-flag gating,
   "hot-leaf consequence is zero" per §2 self-admission), P2-F C8
   (comment-skip, "NONE" antecedent per §2.8). Demotion to `§2.X —
   Non-candidate inventory` sub-section preferred over per-row stamp
   per `[no-deferrals]`. CH1.md §3.2.
3. **Fold-3 (P2-F indirect-/envelope-antecedent disposition language).**
   Five P2-F candidates (C6 branch-on-first-byte dispatch; C7
   whitespace skip; C10 cross-chunk byte-context; C12 keyword-set
   16-byte alphabet; C13 BCAX 3-way XOR) carry envelope-only or
   indirect-via-CN antecedents requiring explicit per-row stamping —
   "P1 antecedent: envelope-only at SK-V14; admit-gate conditional on
   F-V2-P1ABC-RERECORD" / "P1 antecedent: indirect via CN; direct
   evidence requires F-V2-P1ABC-RERECORD" — to disambiguate from
   speculative kernels. CH1.md §3.3.

**CH4 (3 candidates; clustered on Stage-A scalar-reference authoring):**

4. **P2-F C8 comment-skip primitive.** Scalar reference REQUIRED (new
   code in `parse-that`); same-wave consumer FLAGGED
   (NEUTRAL-PENDING-CONSUMER per P2-F §3); Stage-A authoring + Stage-D
   consumer-pairing both required as same-wave deliverables per
   `[no-deferrals]`. CH4.md §2.F + CF-1.
5. **P2-F C10 cross-chunk byte-context propagation.** Scalar reference
   REQUIRED (byte-by-byte loop with no chunk boundary; SIMD form is
   candidate); same-wave consumer NAMED (CSS keywords up to 12 bytes;
   Sheets 8-byte error literals; BBNF directives). Stage-A scalar
   reference function commit prerequisite under `crates/bbnf-simd/src/scalar/`.
   CH4.md §2.F + §4 P2-F §4.
6. **P2-F C13 branchless 3-way XOR (BCAX).** Scalar reference REQUIRED
   (trivial 2-op form `(a & !b) ^ c`); same-wave consumer FLAGGED
   (fusion primitive — per-row consumer per the consuming primitive C1,
   C2, C12). Stage-A scalar reference + Layer-1 NEON body land together
   per Lock 16 same-commit discipline. CH4.md §2.F + §4 P2-F §4.

### §0.5 — Convergence vote

Per `ORCHESTRATOR.md §3Z` (≥ 95 % × 2 cycles, zero orphan REVISEs):

- V1 is the **first** cycle (no prior ≥ 95 % cycle to chain).
- Sub-axis aggregate **93.1 % below floor**; per-lens mean 96.2 %
  marginally above but does not discharge the "× 2 cycles" requirement.
- **Ten orphan REVISE entries** across CH1 (7 candidate-rows mapping to
  3 fold packets) + CH4 (3 candidates clustered on Stage-A authoring).

**Cycle verdict: NOT-CONVERGED-V2-REQUIRED.** V2 must (a) clear all ten
orphan REVISE items via the three CH1 fold packets + three Stage-A
scalar-reference commits, and (b) drive the sub-axis aggregate above
the 95 % floor with zero new orphan REVISEs; only then can a V3 cycle
attempt the "× 2 cycles" close.

## §1 — Per-artefact convergence digest

For each of the six P2 artefacts, cross-lens disposition pressure
(consolidated from CH1 §2, CH2 §2, CH3 §2, CH4 §2, CH5 §2, CH6 §1, CH7 §1):

### §1.1 — `p2a-sota-teardown.md` (367 lines; 7 candidates C1-C7; SOTA-comparator teardown)

| Lens | Verdict | Pressure |
|---|---|---|
| CH1 | ACCEPT (7/7 candidates) | Strongest comparator-discipline in the cohort — §1.4 names R1-binding plane per comparator per row; §5.3 pins all four upstream HEADs to 2026-05-21-verified SHAs (simdjson, sonic-rs, yyjson, asmjson); `sonic_rs::from_slice::<Value>` audit-falsified anti-pattern flagged explicitly |
| CH2 | ACCEPT | §2.1 summary line 197 reports 7/7 grammar-neutrality; §3 grammar-neutrality table maps each candidate to CSS L4 / Sheets / BBNF-self consumer columns; CH2 F2 binding correctly applied (CSS L4 spec-evidence-only generalisation flagged, not papered over) |
| CH3 | ACCEPT | C2/C3/C5/C7 carry REDRESS differentials (28+33+82-84; 80; 96/97/98; 82) with explicit fresh-material differential per row; C5 is "the **opposite** of REDRESS 96/97/98 — removes parallel-substrate consumers" |
| CH4 | ACCEPT (7/7) | Gold-standard CH4 evidence enumeration per CF-3: §4 paper-close subsection at lines 256-266 names per-candidate exact CH4 requirement (scalar-ref function name + checkasm parity shape) |
| CH5 | ACCEPT | C1/C5 explicitly **consume the existing `StructuralIndex`**, not introducing parallel scan; C2/C3/C7 SIMD masks are transient producers (`local_temp_only`); zero retained sidecars |
| CH6 | ACCEPT (7/7 CH6 PASS) | All comparator anchors verified at HEADs; ISA claims trace to Lemire/Validark/Arm ACLE; per-candidate §2 scalar-reference sketches present (5 existing + 2 required-with-shape + 2 build-invariants) |
| CH7 | ACCEPT | Every C1-C7 names parser/SIMD source file; no `// @generated` hand-write proposed; §3 grammar-neutral verdict per candidate; 4 direct audit-overlay citations |

**Pressure summary:** ACCEPT-class on 7/7 lenses; **zero V1 fold required**.
P2-A is the cohort exemplar for source-pinning + CH4 evidence enumeration.

### §1.2 — `p2b-dav1d-process.md` (217 lines; 5-stage admission process A-E)

| Lens | Verdict | Pressure |
|---|---|---|
| CH1 | REVISE (cite-pin Stages §2.A + §2.B) | Stages §2.A (Scalar-Reference Authoring) + §2.B (Differential Checkasm Cell) cite FFmpeg + dav1d sources without pinned upstream HEAD SHAs; the dav1d `src/x86/msac.asm:80-220` cite at §5.1 IS pinned (Lock 16 :305 lineage); P2-A's pinned HEADs available for inheritance; Stages §2.C–§2.E ACCEPT |
| CH2 | ACCEPT | 5-stage process grammar-neutral by construction; Stage B mandates non-JSON fixture extension; Stage D `grammar_scope` tag enforces Lock 14 v+1 same-wave non-JSON consumer; Stage E manifest is the audit surface |
| CH3 | ACCEPT (load-bearing) | §4 catchall at lines 166-177 binds every dispatch-watch-list REDRESS family to a specific Stage that fails it closed by construction; canonical CH3 statement for the cohort |
| CH4 | n/a (process artefact, not primitive list) | This IS the CH4 enforcement surface S-P3 admission gates against; the 5-stage Stage A-E binding aggregate pass-rate is 92.97 % across 37 eligible × 5 stages |
| CH5 | ACCEPT | Stage E `substrate_target` column rejects-by-construction any candidate naming `parallel_substrate` or value outside Lock 1 v+1 allowlist; Stage E manifest row schema (`LOCKS.md:309-318`) is sixteen-column attribution |
| CH6 | ACCEPT (5 stages PASS) | Stage A literally IS the scalar-reference-first discipline; §5.1 anchors materialised at `crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1-14` + `tests/checkasm_parity.rs:233-289` |
| CH7 | ACCEPT | `p2b:120,156-162` cite Lock 14 v+1 amendment (`LOCKS.md:255-263`) verbatim; Stage E wave-close vocabulary explicitly excludes `inventory_demoted_with_evidence` as *new* admission — historical-only; audit-overlay bound via Lock 14 v+1 amendment (the audit-overlay's primary output) |

**Pressure summary:** ACCEPT-class on 6/7 lenses; **one V1 fold required**
(CH1 Fold-1 SHA pinning on §5.1; mechanical inheritance from P2-A §5.4
HEADs). P2-B is the canonical CH3 + CH7 process-discipline statement.

### §1.3 — `p2c-arch-esoterica.md` (143 lines; 8 instruction-route candidates C-P2C-1..-8)

| Lens | Verdict | Pressure |
|---|---|---|
| CH1 | REVISE (3 candidate-rows: C-P2C-1 / C-P2C-6 / C-P2C-7) | `NOT-S-P3-ELIGIBLE` stamps correct but candidate-table rows lack explicit "not a CH1-grounded candidate at V1 — zero P1 antecedent, retained for cross-tranche identifier stability only" disposition language; ISA cites are strong (ACLE + Neon Intrinsics + Apple sysctl); C-P2C-6 has explicitly zero SK-V14 hot expression per §1.7 self-admission |
| CH2 | ACCEPT | All 8 candidates carry §3 grammar-neutrality verdicts; C-P2C-4 partial-generalisability flag (TBL hex-nibble core neutral; escape-language wrapper per-grammar) is Lock-14-honest; C-P2C-5 explicit `classify_tbl4.rs:33-35` JSON-constant violation pointer is precise CH2 violation surfacing |
| CH3 | ACCEPT (densest REDRESS-cite surface in cohort) | §4 enumerates 11 distinct REDRESS-family pre-block notes (88/89/90/96-98/82-84/SK-V10/60-72/50-55/28+33/80/119-120/126 + PEXT-arch-block) verbatim at p2c:67-78; second canonical S-P2 V1 CH3 statement |
| CH4 | ACCEPT (4 ACCEPT + 1 conditional ACCEPT + 3 NOT-S-P3-ELIGIBLE properly demoted) | C-P2C-2/-3/-4/-5 + C-P2C-8 process-gate pass 3-of-3 discriminators; C-P2C-1/-6/-7 self-disclose absent consumer (not CH4 fail) |
| CH5 | ACCEPT (conditional; C-P2C-2 load-bearing) | C-P2C-2 PRE-BLOCK posture honours REDRESS 88+89+96-98; Lock 1 dependency on P2-D satisfied (P2-D concludes substrate-union YES); admission deferred to S-P3 Union-C wave |
| CH6 | ACCEPT (8/8 CH6 PASS) | All scalar-references named (6 existing + 1 honest demotion C-P2C-1 + 1 process packet C-P2C-8); ISA register §5.1 is exhaustive |
| CH7 | ACCEPT (anti-scaffold-admit exemplar) | 7 of 8 candidates explicitly demoted to `NOT-S-P3-ELIGIBLE` because audit-overlay PRUNE-2 blocks admission; strongest CH7-affirmative pattern in the corpus |

**Pressure summary:** ACCEPT-class on 6/7 lenses; **one V1 fold required**
(CH1 Fold-2 candidate demotion on C-P2C-1 / C-P2C-6 / C-P2C-7).
P2-C is the cohort exemplar for honest CH7 demotion + densest CH3 cite.

### §1.4 — `p2d-substrate-tape.md` (257 lines; 3 active + 1 pre-blocked)

| Lens | Verdict | Pressure |
|---|---|---|
| CH1 | REVISE (1 candidate-row: C-P2D-3 sparse-flag gating) | C-P2D-3 self-admitted as having **no P1 hot leaf** ("hot-leaf consequence is zero"; "listed for completeness so CH4 can dispose it"); per CH1 a candidate with known-zero-P1-evidence row should fold to `§1.6` observation-not-candidate (where (a) and (b) already live); C-P2D-1 + C-P2D-2 are CH1-clean |
| CH2 | ACCEPT | All 3 active YES grammar-neutral; C-P2D-4 N/A REJECT-by-REDRESS-96/97/98 properly framed; substrate primitives grammar-neutral by construction |
| CH3 | ACCEPT (substrate-axis canonical statement) | §4.1 REDRESS 96/97/98 verbatim; §4.2 50-55; §4.3 60-72; §4.4 80+82-84; §4.5 88+89; §4.6 Lock 1 binding; §4.7 cross-check |
| CH4 | ACCEPT (3 active + 1 doc-as-pre-blocked) | C-P2D-3 self-flags as CH4-pre-block falsifier per p2d:131 — ACCEPT-as-honest-completeness; C-P2D-1/-2 carry 3/3 discriminators with substrate-side alt-satisfaction on checkasm |
| CH5 | ACCEPT (load-bearing artefact) | §1.1 substrate-union YES at HEAD; §1.5 architectural-block any new union variant per Pass Omega V1.1 / SK-V13 receiver; §4.7 explicit CH5 cross-check; C-P2D-4 exemplary anti-pattern reference |
| CH6 | ACCEPT (4/4 CH6 PASS) | C-P2D-1/-2/-3 scalar-refs all existing; C-P2D-4 honest exclusion; substrate-union conclusion holds; zero paper-close |
| CH7 | ACCEPT | C-P2D-1/-2/-3 name runtime source paths; HIGH grammar-neutrality with cited mechanism; V3 CH5 substrate-union 6/6 ACCEPT carries audit-overlay binding via downstream surface |

**Pressure summary:** ACCEPT-class on 6/7 lenses; **one V1 fold required**
(CH1 Fold-2 C-P2D-3 demotion to §1.6 observation). P2-D is the cohort's
**substrate-union YES** primary source — load-bearing for CH5 + C-P2C-2.

### §1.5 — `p2e-parse-that-gaps.md` (342 lines; 9 gap candidates incl. 7.5)

| Lens | Verdict | Pressure |
|---|---|---|
| CH1 | ACCEPT (9/9) | Every gap names parse-that-regex consumer at path:line, scalar reference path:line, P1-antecedent class (with envelope-masking disclosed); §4.7 F-V2-P1ABC-RERECORD-dependency note is model CH6-paper-close-mitigation |
| CH2 | ACCEPT | All 9 gaps grammar-neutral per §3 table; §4.3 "Layer-1 primitives carry NO defaults — byte-set/range parameter mandatory; per-grammar default at codegen template" is canonical Lock 14 v+1 enforcement |
| CH3 | ACCEPT-WITH-NOTE (F-1 Gap 5 REDRESS-80) | Gap 5 explicitly flagged for S-P3 decision; S-P2 does not bypass REDRESS 80, records primitive-vs-tweak differential; canonical correct CH3 shape for pre-blocked-route adjacency; aligns with P2-F C5 + P2-C C-P2C-3 |
| CH4 | ACCEPT (9/9) | Every gap carries explicit Scalar reference / Layer placement / Consumer / Substrate-union sub-sections per §2 lines 99-229; §4.5 scalar-reference-first risk binding self-stated |
| CH5 | ACCEPT | §1.3 makes substrate-union constraint explicit at line 76; §4.2 line 268 "CH5-compliant by construction" — every gap returns mask/offset/value with no position emit OR returns bitmask the existing `compact_mask` consumer folds into shared tape |
| CH6 | ACCEPT (9/9 CH6 PASS) | Every gap's scalar function explicitly cited as bit-identical reference; §4.7 contracted deferral parallel to Pass Alpha §4.4 → S-P3 |
| CH7 | ACCEPT | Layer-1 primitives grammar-neutral by construction; mandate (4) round-trip vacuously satisfied (parse-that has no codegen surface); 1 direct audit-overlay citation |

**Pressure summary:** ACCEPT 7/7 lenses; **zero V1 fold required**.
P2-E is the cohort's Layer-1-primitive-discipline exemplar.

### §1.6 — `p2f-grammar-neutral.md` (334 lines; 14 candidates ALL clear Lock 14 v+1)

| Lens | Verdict | Pressure |
|---|---|---|
| CH1 | REVISE (6 candidate-rows: C6/C7/C8/C10/C12/C13) | C8 zero P1 antecedent per §2.8 self-admission; C6 envelope-only (legitimate — IS the dispatch primitive); C7/C10/C12/C13 indirect-via-CN antecedents requiring F-V2-P1ABC-RERECORD for direct measurability; per-row disposition-language stamping required |
| CH2 | ACCEPT (LOAD-BEARING ARBITER) | §3 verdict tally 14/14 clear Lock 14 v+1 admission gate (5 NEUTRAL-WIRED + 8 NEUTRAL-CONFIG-DRIVEN + 1 NEUTRAL-PENDING-CONSUMER); ZERO JSON-OVERFIT-REFRAMABLE; ZERO JSON-OVERFIT-IRREDUCIBLE; 14/14 CSS L4 + 13/14 Sheets (C8 omitted) + 14/14 BBNF-self consumer coverage |
| CH3 | ACCEPT | §4 8-row REDRESS table at p2f:264-274 + C8 CH6 paper-close risk + C10/12/13 scalar-reference CH4 risk + C11 CH5 substrate-union risk + aggregate-CH3 risk; comprehensive |
| CH4 | REVISE (3 candidate-rows: C8 / C10 / C13) | P2-F's own §4 risk subsection at line 277-279 pre-states the REVISE; CH4 audit confirms; C12 disputed inclusion in §4 grouping (§2.12 cites existing `scan_structurals_scalar` scalar reference at `scan.rs:32` — CH4 reads C12 as ACCEPT) |
| CH5 | ACCEPT | §1.3 substrate-union-YES holding assumption explicit; per-candidate substrate-target labels (`local_temp_only` / `existing_tape` / `direct_sink`) all in Lock 1 v+1 allowlist; C11 explicitly forbids split per P1-E §4.4 |
| CH6 | ACCEPT (14/14 CH6 PASS) | All scalar-refs named (10 existing + 3 required-with-shape + 1 build-invariant); ISA register §5.2 keyed to Lock 16 :282-307 manifest |
| CH7 | ACCEPT (LOAD-BEARING ARBITER) | IS the Lock 14 audit surface for the 14 candidates (5 audit-overlay citations); a candidate that fails Lock 14 v+1 here cannot be admitted by P2-A/B/C/D/E into S-P3 |

**Pressure summary:** ACCEPT 5/7 lenses (CH2 + CH7 load-bearing);
**two V1 folds required** (CH1 Fold-2 C8 demotion; CH1 Fold-3 + CH4
C8/C10/C13 indirect-antecedent stamping + Stage-A scalar-reference
commits). P2-F is the cohort's Lock 14 v+1 arbiter.

## §2 — Cross-lens convergence findings

### §2.1 — F-V2-P1ABC-RERECORD elevates to CH2/CH4 dual-gated packet (cross-lens CH2 + CH4 + CH6)

The S-P1 V1 fold packet F-V2-P1ABC-RERECORD (parse-attribution profile
rebuild) was originally CH4-only scoped. S-P2 V1 cross-lens evidence
elevates it to **CH2/CH4 dual-gated**:

- **CH2 (`CH2.md §3.4 F2`):** "parse-attribution rerun is co-required by
  CH2 verdicts on C6 / C-P2C-3 / Gap 5". Without parse-attribution-enabled
  profile evidence, the grammar-neutrality argument on inner primitives
  (string match, number parse, structural scan, tape emit inside the
  dispatch envelope) cannot be empirically discharged.
- **CH6 (`CH6.md §4.6 NF-CH6-6`):** "the consolidator should produce a
  single F-V2-P1ABC-RERECORD binding entry that names: (a) the cargo
  invocation, (b) the samply invocation, (c) the wave slot (Stage 0 of
  the first SK-V14 implementation wave), (d) the consumer dependency
  list".
- **CH4 (`CH4.md §2 / §3 CF-4`):** Stage D row-movement clause for the
  downstream instruction-route candidates folds in this prerequisite;
  process-candidate alternative satisfaction per P2-B §2.D.

Cross-axis consumer dependency list (11+ candidates):

- P2-A C6 (`parse_attribution_envelope_cracker`) — process-gate IS the
  rerun
- P2-C C-P2C-3 (`udot_digit_span_x4`) — NOT-S-P3-ELIGIBLE pending rerun
  naming numeric inner leaf
- P2-C C-P2C-8 (`parse_attribution_profile_rebuild_gate`) — process-gate
  IS the rerun
- P2-E Gap 1 (`scan_string_special_block_sweep_64`) — envelope-masked
  string inner primitive
- P2-E Gap 3 (`ascii_whitespace_skip_64`) — envelope-masked whitespace
  inner primitive
- P2-E Gap 4 (`utf8::validate_block_streaming`) — envelope-masked UTF-8
  inner primitive
- P2-E Gap 5 (`parse_16_digits_dotprod`) — envelope-masked numeric
  inner primitive (REDRESS-80 differential)
- P2-F C6 (branch-on-first-byte dispatch) — envelope IS the dispatch
  primitive
- P2-F C7 (whitespace prefix skip) — envelope-masked indirect
- P2-F C10 (cross-chunk byte-context) — indirect via C1+C4
- P2-F C12 (keyword-set 16-byte alphabet) — indirect via C1
- P2-F C13 (BCAX 3-way XOR) — indirect via C1+C2+C12

**Binding entry (per NF-CH6-6 + CH2 §4.1 mandatory action 2 + `[no-deferrals]`):**

```
Packet: F-V2-P1ABC-RERECORD
Gating:  CH2 (measurability) + CH4 (cost-discriminator) dual-gate
Cargo:   cargo build --release -p bbnf-bench --features runtime/parse-attribution
Samply:  interactive samply record (NOT --save-only) per [samply-symbol-resolution]
         + cfg_attr flip verification at generated.rs:33-34, 43-44, 58-59, 79-80,
           86-87, 117-118, 138-139, 157-158 (8 sites; inline(always) → inline(never))
Wave:    Stage 0 of the first SK-V14 implementation wave admitting any
         dispatch-envelope-internal primitive
Consumers (must-bind, `[no-deferrals]`):
         P2-A C6 + P2-C C-P2C-3 + P2-C C-P2C-8 + P2-E Gap 1 + Gap 3 + Gap 4 +
         Gap 5 + P2-F C6 + C7 + C10 + C12 + C13
Convention: per CH2 F2 elevation, any S-P3 wave admitting these
         primitives MUST ship the rerun in Stage 0 of the same wave
```

### §2.2 — Substrate-union-YES six-witness corroboration

P2-D's load-bearing finding **substrate-union holds at HEAD** is
corroborated across six independent witnesses:

1. **P2-D §1.1 line 27** — `grep -rn "struct.*Tape\b" skinny/crates/runtime/src/`
   returns three hits (`Tape<'input>`, `TapeBuilder<'input>`, `TapeId`).
2. **P2-D §1.5 lines 84-92** — explicit architectural-block of new union
   variant per Pass Omega V1.1 / SK-V13 receiver.
3. **P2-D §4.7 line 204** — "§1.1 + §1.3 + §1.5 jointly conclude **YES,
   the substrate union holds at HEAD**".
4. **P2-F §1.3 line 53** — holding assumption corroborated; "P2-F
   assumes the conclusion is YES … any candidate primitive that touches
   the tape touches the *single* substrate".
5. **CH5 (`CH5.md §2 / §3 CH5-A / CH5-E`)** — 6/6 ACCEPT; substrate
   target taxonomy uniformly applied across P2-D/P2-F/P2-B; no new union
   variant proposed; P2-D's architectural block is binding closure.
6. **P1-V3-CH5 `research/p1/hardening/V3/CH5.md:78-83`** — two-cursor
   independence verification at HEAD; Track 1 cursor in
   `runtime::generated_json::*`; Track 2 cursor in
   `bbnf_bench::generated_real_typed::*`; both index same `&'i [u8]`
   substrate but neither calls the other's parse path.

**Consequence for S-P3:** C-P2C-2 Lock 1 dependency condition (c) is
satisfied at the dispatch level; admission narrows to wave-program
deliverables (a) SIMD-first direct tuple writeback that DELETES scalar
consume, (b) strict same-row non-regression on the 11-row set Item 88/89
falsified, (d) emitted-asm proof of `pmull.1q` + `ctz`.

### §2.3 — Long-string-body SIMD scan consolidation (CH6 NF-CH6-4)

Three artefacts surface a long-string-body SIMD scan primitive under
three distinct names grounded on the same `unescape_string` direct
rank-1 46.7% on `unicode_escapes` hot-leaf:

- P2-A C2 (`long_string_body_simd_scan`) — names existing scalar refs
  `match_tiny_plain_string_with_cap` + `unescape_string`
- P2-E Gap 1 (`scan_string_special_block_sweep_64`) — names
  `scan_string_special_block_scalar`-as-bitwise-OR-fold
- P2-F C1 + C2 (quote-aware classifier composition) — names
  `scan_structurals_scalar`

All three carry CH6 PASS scalar references; all three converge on the
same underlying primitive. **Consolidator binding for S-P3:** track the
C2/Gap1/C1+C2 alignment so S-P3 produces **one canonical primitive name
+ one canonical scalar reference function**, not three near-duplicates.
Per NF-CH6-3, P2-F C2 §2 entry should additionally cite P2-E Gap 6
composition (`scan_string_special_block_sweep_64` + `bitmap_prefix_xor_64_scalar`
+ `escape_mask_64`) as scalar oracle path:line.

### §2.4 — Exemplary anti-paper-close patterns (CH6 NF-CH6-5)

Two patterns set the standard for S-P3 candidate-admission discipline:

- **P2-D C-P2D-4 (EventTape REJECT-by-history).** Explicitly listed as
  candidate AND marked REJECT-by-REDRESS-96/97/98 with verbatim cite;
  artefact does not pretend route is unevaluated, does not paper-close
  on "future tranche will decide", does not omit the route to hide it
  from CH3 scrutiny.
- **P2-C C-P2C-2 (PRE-BLOCKED at SK-V14 V1).** Explicit "PRE-BLOCKED by
  REDRESS 88 + 89 + 96-98" with the specific four unblock conditions
  named (SIMD-first direct tuple writeback that DELETES scalar consume;
  strict same-row non-regression on 11-row set; Lock 1 substrate union
  held per P2-D; emitted-asm proof).

**Standard for S-P3:** any candidate that abuts a pre-blocked REDRESS
surface must follow this pattern — cite the REDRESS entry verbatim, name
the specific failure-mode evidence, enumerate the unblock conditions,
defer admission to S-P3 with the burden of proof preserved.

### §2.5 — Quadruple canonical CH3 statement (CH3 F-5)

Four artefacts carry verbatim per-REDRESS-family enumerations covering
the full dispatch-context watch-list (28+33, 50-55, 60-72, 80, 82-84,
88, 89, 96-98, 119/120, 126):

1. **P2-B §4 catchall** — admission-process axis (Stage-N enforcement).
2. **P2-C §4 enumeration** — arch-instruction axis (11-REDRESS-family +
   PEXT arch-block).
3. **P2-D §4.1-§4.7** — substrate axis (per-REDRESS-family cross-check).
4. **P2-A §4** — SOTA-comparator axis (architecture-pressure differentials).

P2-E §4.1 + P2-F §4 are the per-candidate audit layers underneath.
**Watch-list coverage redundant across four orthogonal axes; no silent
re-open risk surfaces in the V1 artefact set.**

## §3 — V2 fold packet specification

Six V2 fold packets prescribed. All six are **light** mechanical edits
totalling ≈ 35 min wall + ≈ 90 min for three scalar-reference commits.
Per `[no-deferrals]` the three Stage-A scalar-reference commits land in
V2 (not deferred to S-P3 wave program).

### §3.1 — Fold-1: P2-B §5.1 external-cite SHA pinning (LIGHT)

**Closes:** CH1 §3.1 (P2-B REVISE); cross-cuts CH6 §4 path-pinning.

**Scope:** Add to `p2b-dav1d-process.md §5.1` the FFmpeg HEAD SHA + dav1d
HEAD SHA inline anchors:
- FFmpeg HEAD `085714182302333dd83dcb9c36cf828dc4eba929` (pinned in P2-A
  §5.4; inherit verbatim)
- dav1d HEAD `1718ff9aded99f0a89f5c7940d6afb8948301e33` (pinned in P2-A
  §5.4; inherit verbatim)

Insert as line-item anchors next to `tests/checkasm/checkasm.{c,h}` +
`tests/checkasm/` citations.

**Cost:** ≈ 5 min wall (LOW). Mechanical edit.

**Convergence impact:** CH1 P2-B REVISE → ACCEPT. Cohort SHA-pinning
discipline uniform across all 6 artefacts.

### §3.2 — Fold-2: zero-P1-antecedent candidate demotion (LIGHT)

**Closes:** CH1 §3.2 (5 candidate-rows across P2-C/D/F).

**Scope:** For each of the 5 candidates with explicitly zero SK-V14 P1
antecedent, demote out of `## §2 — Candidate primitives` enumeration into
a `## §2.X — Non-candidate inventory` (or `§2.X — Cross-tranche
identifier stability inventory`) sub-section per `[no-deferrals]`
orchestrator preference:

- **P2-C:** demote C-P2C-1 (PRUNE-2 CSS L4 absent), C-P2C-6 (zero SK-V14
  three-input-boolean hot expression), C-P2C-7 (REDRESS-126 orphan; no
  production caller) into `p2c-arch-esoterica.md §2.X — Non-candidate
  inventory`.
- **P2-D:** demote C-P2D-3 (sparse-flag gating; "hot-leaf consequence is
  zero" per §2 self-admission) into `p2d-substrate-tape.md §1.6 —
  Substrate-side observations` (where (a) and (b) already live).
- **P2-F:** demote C8 (comment-skip; "NONE" P1 antecedent per §2.8)
  into `p2f-grammar-neutral.md §2.X — Non-candidate inventory` OR retain
  per Fold-3 stamping if the same-wave consumer (BBNF-self / CSS L4)
  commits in the wave plan; default is demote (per `[no-deferrals]` C8
  cannot ship without same-wave consumer).

**Cost:** ≈ 10 min wall (LOW). Section restructure + per-row prose
preservation.

**Convergence impact:** CH1 candidate-ACCEPT 31/38 → 36/38; CH1
artefact-ACCEPT 33.3% → 66.7% (P2-D + P2-C clear; P2-F partial).

### §3.3 — Fold-3: P2-F indirect-/envelope-antecedent disposition language (LIGHT)

**Closes:** CH1 §3.3 (5 P2-F candidate-rows: C6/C7/C10/C12/C13).

**Scope:** Prepend each candidate's §2 body with the explicit single-line
disposition per CH1 §3.3:

- **C6:** `**P1 antecedent (CH1):** dispatch_value envelope (the
  candidate IS the dispatch primitive; inner-primitive measurability
  deferred to F-V2-P1ABC-RERECORD; envelope-direct grounding legitimate
  per CH2 ACCEPT).`
- **C7:** `**P1 antecedent (CH1):** envelope-masked (whitespace-skip step
  inside dispatch_value); admit-gate conditional on F-V2-P1ABC-RERECORD.`
- **C10:** `**P1 antecedent (CH1):** indirect via C1 + C4 (the fusion
  primitive applied inside the other primitives' inner loops); direct
  evidence requires F-V2-P1ABC-RERECORD.`
- **C12:** `**P1 antecedent (CH1):** indirect via C1 (specialises the
  small-alphabet case of structural-byte classify); direct evidence
  requires F-V2-P1ABC-RERECORD.`
- **C13:** `**P1 antecedent (CH1):** indirect via C1 + C2 + C12 (fusion
  primitive applied inside their inner loops); direct evidence requires
  F-V2-P1ABC-RERECORD.`

**Cost:** ≈ 10 min wall (LOW). Per-row prose insertion.

**Convergence impact:** CH1 P2-F partial-ACCEPT → ACCEPT (C6/C7/C10/C12/C13
clear). With Fold-2 + Fold-3, CH1 candidate-ACCEPT 31/38 → 38/38 = 100 %;
CH1 artefact-ACCEPT 33.3% → 100 %.

### §3.4 — Fold-4 / -5 / -6: Stage-A scalar-reference authoring commits (HEAVY-but-required)

**Closes:** CH4 §4 V2 fold recommendation 1 (3 REVISE: P2-F C8 / C10 / C13).

Per `[no-deferrals]` Stage-A scalar reference functions land in V2
under `crates/bbnf-simd/src/scalar/` (or `crates/parse-that/src/` for
C8) before V2 ACCEPT. The candidate scalar-reference shape is named in
each §2 row; the gap is authoring time, not architectural uncertainty.
Owner: any wave that admits the corresponding SIMD body must land
scalar-first per Lock 16 same-commit discipline.

- **Fold-4 (P2-F C10 scalar reference):** `crates/bbnf-simd/src/scalar/byte_context_64.rs`
  — byte-by-byte loop with no chunk boundary; producing the same
  cross-chunk byte-context as the candidate SIMD primitive.
- **Fold-5 (P2-F C13 scalar reference):** `crates/bbnf-simd/src/scalar/bcax_64.rs`
  — trivial 2-op form `(a & !b) ^ c` over u8x16 / u64 masks; sibling of
  existing `bitmap_prefix_xor_64_scalar`.
- **Fold-6 (P2-F C8 scalar reference):** `crates/parse-that/src/comment_skip.rs`
  — new code: `(input_bytes, position, open_marker, close_marker,
  line_marker) -> position + comment_bytes_consumed` per CH6.md §1.6 C8
  shape. **GATE:** C8 admission additionally requires same-wave non-JSON
  consumer commit (BBNF-self bootstrap OR CSS L4 declaration_values OR
  json-commented) per CH4 §2.F + `[no-deferrals]`; if no consumer commits
  in V2 wave, C8 retains Fold-2 demotion path.

**Cost:** ≈ 30 min wall each × 3 = ≈ 90 min wall (MEDIUM). Authoring +
unit test scaffolding. Optionally parallelisable across 3 worktrees per
`[agent-orchestration]`.

**Convergence impact:** CH4 strict 34/37 → 37/37 = 100 %; CH4
alt-satisfaction 35/37 → 37/37; CH4 ACCEPT-WITH-REVISE → ACCEPT.

## §4 — V2 dispatch shape

### §4.1 — Axes that fold V2 (4 axes)

- **P2-B** — Fold-1 (SHA pinning §5.1). LIGHT mechanical inheritance.
- **P2-C** — Fold-2 (C-P2C-1 / C-P2C-6 / C-P2C-7 demotion to `§2.X —
  Non-candidate inventory`). LIGHT section restructure.
- **P2-D** — Fold-2 (C-P2D-3 demotion to `§1.6 — Substrate-side
  observations`). LIGHT row migration.
- **P2-F** — Fold-2 (C8 demotion OR Fold-3 stamping with same-wave
  consumer commit) + Fold-3 (C6/C7/C10/C12/C13 indirect-/envelope-
  antecedent stamping) + Fold-4/-5/-6 (C10/C13/C8 Stage-A scalar
  reference commits). LIGHT + MEDIUM authoring.

### §4.2 — Axes that lock at V1 (2 axes)

- **P2-A** — ACCEPT 7/7 lenses; gold-standard CH4 evidence enumeration;
  exemplar for source-pinning + scalar-reference discipline. No V2 fold.
- **P2-E** — ACCEPT 7/7 lenses (CH3 ACCEPT-WITH-NOTE Gap 5 REDRESS-80 is
  correct shape per CH3 binding, not a fold target); Layer-1-primitive-
  discipline exemplar; "Layer-1 primitives carry NO defaults" canonical
  Lock 14 v+1 enforcement statement. No V2 fold.

### §4.3 — Cross-axis V2 deliverables (cohort-wide)

- **F-V2-P1ABC-RERECORD binding entry** (per §2.1) — single binding entry
  authored at the consolidator level naming cargo invocation + samply mode
  + wave slot + consumer dependency list. Inherits from S-P1 V1 fold
  packet F-V2-P1ABC-RERECORD; CH2/CH4 dual-gated.
- **Long-string-body SIMD scan consolidation** (per §2.3) — orchestrator
  tracks C2/Gap1/C1+C2 alignment for S-P3 single-canonical-primitive
  resolution. NF-CH6-4 fold target.
- **P2-F C2 §2 entry scalar-oracle path:line upgrade** (per NF-CH6-3) —
  cite P2-E Gap 6 composition (`scan_string_special_block_sweep_64` +
  `bitmap_prefix_xor_64_scalar` + `escape_mask_64`) as scalar oracle.
  LIGHT path-line addition.

## §5 — §3Z LOCK criteria for V2

### §5.1 — V2 fold-only forecast

With all six light + medium V2 fold packets landed (Fold-1 SHA pinning;
Fold-2 candidate demotion across P2-C/D/F; Fold-3 P2-F disposition
stamping; Fold-4/-5/-6 Stage-A scalar-references; ≈ 125 min wall total
sequential / ≈ 50 min wall parallel-3-worktree):

| Lens | V1 rate | Expected V2 rate (fold-only) | Net |
|---|---:|---:|---|
| CH1 | 81.6 % candidate / 33.3 % artefact | ≈ 100 % candidate / 100 % artefact | Fold-1 (P2-B) + Fold-2 (5 demotions across P2-C/D/F) + Fold-3 (P2-F C6/C7/C10/C12/C13 stamping) close all 7 REVISE candidate-rows |
| CH2 | 100 % | 100 % | F-V2-P1ABC-RERECORD dual-gating elevation logged; R1 cross-axis reconciliation discharged by Fold-3 |
| CH3 | 100 % | 100 % | ACCEPT-WITH-NOTE on Gap 5 REDRESS-80 retains correct CH3 shape; not a fold target |
| CH4 | 91.9 % strict / 94.6 % alt | ≈ 100 % strict / 100 % alt | Fold-4/-5/-6 close P2-F C8 / C10 / C13 Stage-A authoring; C12 ACCEPT confirmed per CH4 §3 |
| CH5 | 100 % | 100 % | Substrate-union YES holds; no fold required |
| CH6 | 100 % | 100 % | NF-CH6-1..-6 are cohesion improvements; consolidator deliverables under §4.3 close NF-CH6-3 + NF-CH6-4 + NF-CH6-6 |
| CH7 | 100 % | 100 % | Audit-overlay binding intact; mandate (4) vacuous-satisfaction for p2e structural; no V2 work required |

**Expected sub-axis-weighted V2 aggregate:** ≈ 99.3 % (≥ 144 / 145).
**Expected per-lens mean V2:** ≈ 100 %.

### §5.2 — V2 outcome forecast + V3 close path

**V2 outcome under fold-only:** ≥ 95 % on both aggregation methods; zero
orphan REVISEs (modulo C8 — which either clears via same-wave consumer
commit OR demotes via Fold-2); **first ≥ 95 % cycle achieved**.

**V3 forecast:** V3 inherits V2's ≈ 99 % sub-axis aggregate + 100 %
per-lens ACCEPT on CH2/CH3/CH5/CH6/CH7 + ≈ 100 % on CH1/CH4 after light
+ scalar-authoring folds. V3 work surface: re-verify V2 fold edits land
cleanly; surface any new cite drift introduced by V2 textual edits;
reconfirm Fold-4/-5/-6 scalar-reference functions land with unit tests
that pass `cargo test --release`; reconfirm Fold-1 SHA inheritance
preserves P2-A §5.4 anchors verbatim. V3 expected outcome: ≥ 95 % on
both aggregation methods with zero new orphan REVISEs.

**§3Z convergence:** on the second consecutive ≥ 95 % cycle, S-P2 V_close
→ S-P3 dispatch gate opens per `PASS-2-RESEARCH.md §3` + `ORCHESTRATOR.md §3Z`.

**Predicted close path: V2 → V3 → LOCK.** V2 lifts above the 95 % floor
+ zero orphan REVISEs; V3 attempts the second consecutive ≥ 95 % cycle
to discharge "× 2 cycles". S-P3 dispatch gate opens at V3 LOCK.

## §6 — Sources

V1 lens dispositions (all verified existing at write-time):

- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH1.md` (461 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH2.md` (365 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH3.md` (489 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH4.md` (204 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH5.md` (311 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH6.md` (835 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH7.md` (114 lines)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CHALLENGE-CONTEXT.md` (33 lines)

V1 P2 axis artefacts under review (HEAD per `S-P2-DISPATCH-CONTEXT.md`):

- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md` (367 lines)
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md` (217 lines)
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md` (143 lines)
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md` (257 lines)
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md` (342 lines)
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md` (334 lines)

Binding authorities:

- `restart/prompts/skinny/PASS-2-RESEARCH.md §3` (CH1-CH6 specialisations) +
  `§8.6` (substrate union closing pin)
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH1-CH6 lens registry) +
  `§3Z` (convergence rule) + `§8` (non-negotiables — scalar-reference +
  checkasm-parity + same-wave consumer)
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune
  lens definition; carry-forward from S-P0)
- `restart/skinny/tranches/sk-v14/research/p2/S-P2-DISPATCH-CONTEXT.md`
  (S-P2 dispatch spec; F-V2-P1ABC-RERECORD inheritance)
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` + `HANDOFF.md` (SK-V14
  contract; R1-R10 target list; telemetry binding)
- `restart/locks/LOCKS.md` (Lock 1 substrate-union v+1 manifest;
  Lock 14 v+1 grammar-neutrality; Lock 15 i-cache budget; Lock 16
  SIMD/ASM allowlist + abstract-primitive declarations)

Prior-cycle precedent:

- `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
  (S-P1 V1 consolidator pattern; F-V2-P1ABC-RERECORD packet origin;
  same-format mirror for this aggregator)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH5.md:78-83`
  (two-cursor independence verification; substrate-union YES upstream)
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
  (PRUNE-1 + PRUNE-2 + row-falsification authority; audit-overlay)

Cross-tranche substrate anchors (cited by CH1-CH7 source registers):

- `skinny/crates/runtime/src/grammars/json/generated.rs:33,43,45,159,169,
  187,213,466,506,650` (envelope + grammar-neutral primitive symbols)
- `skinny/crates/runtime/src/grammars/json/scan.rs:22,32,107,131,164`
  (structural scan primitives + tape-emit sites)
- `skinny/crates/parse-that-regex/src/lib.rs:113,162,284,547,718,945,959`
  (whitespace, string-quote, escape-validation, plain-string skip,
  unescape, hex-unit, hex-nibble primitives)
- `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:2`
  (`bulk_emit_positions_64_neon` — P2-F C9 substrate)
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40`
  (`unescape_uxxxx_scalar` — P2-C C-P2C-4 + P2-E Gap 2 scalar reference)
- `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:5,27` (`parse_4_digits`
  + `parse_4_digits_dotprod` — P2-C C-P2C-3 + P2-E Gap 5 scalar + SIMD)
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs:2742-2746,2949-3003`
  (Track 2 `DirectParser` two-cursor independence + skip primitives)
- `skinny/REDRESS.md:2217,2510,2544,2589,2797-2906,2910-2950` (Items
  80, 88, 89, 90, 96/97, 98 — substrate-ceiling pre-block surface)

External (SOTA + ISA) citations pinned at HEAD:

- simdjson HEAD `168ef580757d75270475b379e83c2b39787a6765` (P2-A §5.3)
- sonic-rs HEAD `03545a9530346fe279b674dd496e037d94204bc5` (P2-A §5.3)
- yyjson HEAD `95f4c61bc1e24176f2aa4f430902705a995f1c97` (P2-A §5.3)
- asmjson crate 0.2.5 (P2-A §5.3)
- dav1d HEAD `1718ff9aded99f0a89f5c7940d6afb8948301e33` (P2-A §5.4;
  P2-B §5.1 inherits per Fold-1)
- FFmpeg HEAD `085714182302333dd83dcb9c36cf828dc4eba929` (P2-A §5.4;
  P2-B §5.1 inherits per Fold-1)
- Arm ACLE 2026Q1; Arm Neon Intrinsics Reference 2026Q1; Arm
  Architecture Reference Manual A-profile Issue J.a + Armv8.2-A SHA3 +
  DotProd; Apple Silicon `sysctl hw.optional.*`; Intel Intrinsics Guide
  (x86 secondary); WikiChip VPCLMULQDQ / AVX-512 IFMA / BITALG;
  Lemire 2019/2023/2024/2026 series; Validark 2024 (LD4-interleaved);
  Travis Downs kreg-facts; Mula 2018-2024 GFNI + PDEP/PEXT;
  Langdale & Lemire 2019 VLDB arXiv:1902.08318
