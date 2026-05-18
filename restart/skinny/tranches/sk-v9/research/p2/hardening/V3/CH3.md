# SK-V9 S-P2 V3 — CH3 REGRESSION (REDRESS-Reopen Audit, V3 verify)

Pass: S-P2 Research. Cycle: V3. Lens: CH3 (`restart/prompts/ORCHESTRATOR.md`
§3W). Cohort: S-P2 V3 fold — six artefacts (`skv9-p2-A` … `skv9-p2-F`).
V3 fold authority: `HARDENING-S-P2-V2-CONSOLIDATED.md` §V3-fold-requirements
(CH3 — 3 RESIDUAL-REVISE: fold #1 P2-D §5.3.1 six-row gate; fold #2 P2-F
§5.2 REDRESS-33 inline citation; fold #3 §0-footer cascade note). Fold
commit: `212971a3 docs(sk-v9-p2-v3): fold V2 CHALLENGE residuals — 8
surgical edits` (P2-D 5 edits, P2-F 3 edits; 86 insertions, 19 deletions,
two files only). REDRESS authority surface: `skinny/REDRESS.md` entries
28 (`:324-337`), 33 (`:394-418`), 82, 88, 89, plus the pre-block list
50-72/83-84/90/92 inherited unchanged from V2.

V2 CH3 verdict: **93.0% clean (40/43)** — 0 REJECT, 0 REGRESSION, 3
RESIDUAL-REVISE (D-1, D-10 — one EOR3 six-row-gate defect counted across
two rows; F-7 — missing inline REDRESS 33 citation in P2-F §5.2). The
V2 consolidation routed CH3 to **REVISE — V3 fold the 3 residuals**.
This V3 pass verifies that fold landed and that no V3 edit reopens a
REDRESS route.

## §1 — V2-RESIDUAL resolution

The V3 fold touched exactly two files (P2-D, P2-F) and no others — the
diff was read in full. P2-A, P2-B, P2-C, P2-E carry zero V3 edits and
remain at their V2-converged CH3 state (P2-A/B/C/E were each 100% clean
at V2). All three CH3 residuals were the named target of three of the
five P2-D / three P2-F edits; the remaining edits serve CH1/CH2/CH6 and
are audited for CH3 non-regression in §2.

### §1.1 — V2 RESIDUAL D-1 / D-10 — EOR3 six-row no-regression gate

**V2 defect.** The §5.3.1 SHA3 `veor3q_u8` EOR3 candidate was reframed
at V2 as a Lock-16 `FEAT_SHA3` host-cap-gated specialisation of the
scalar shift-XOR ladder (scalar fallback unconditional) — a material
differential that dissolves the REDRESS-88 "default rewire" objection.
But the §5.3.1 falsification posture did **not** name the W10b six-row
WIN block in its own slice gate; only §4.4 (CSSC CTZ) carried that gate.
V1 folds #5 and #7 asked specifically for the six-row gate on the EOR3
candidate.

**V3 fold — RESOLVED.** P2-D §5.3.1 (verified at lines 854-866) now
reads, immediately after the vector-vs-scalar-vs-PMULL three-way
differential sentence:

> "Mirroring the §4.4 CSSC CTZ slice's falsification posture, the EOR3
> candidate's S-P3 admission carries an explicit no-regression maintain
> gate on the six W10b WIN-block rows (`canada`, `citm_catalog`,
> `instruments`, `marine_ik`, `mesh`, `numbers`) as a hard blocking
> precondition — the prefix-XOR hot body is the surface W10b proved
> regresses the WIN block even when correctness-green, so no EOR3 body
> ships unless those six rows hold."

The six-row identity is byte-identical to the §4.4 CSSC CTZ gate text
(verified — §4.4 lines 695-696 name the same six rows: `canada`,
`citm_catalog`, `instruments`, `marine_ik`, `mesh`, `numbers`). The
gate is stated as a "hard blocking precondition", matching §4.4's
strength. The added rationale ("the prefix-XOR hot body is the surface
W10b proved regresses the WIN block even when correctness-green") is
the correct REDRESS-88/89 grounding — W10b proved correctness-green
asm-body changes can still regress the WIN block. **The fold is
verbatim what the V2 §4 risk-table row 1 prescribed.** D-1 and D-10's
shared residual is closed.

### §1.2 — V2 RESIDUAL F-7 — REDRESS-33 inline citation in P2-F §5.2

**V2 defect.** P2-F §5.2 ("Cost-fact-gated NEON `vqtbl1q_u8`
tiny-string equality at the dispatch site … Pattern not yet used …
Expected impact: lifts twitter, update_center, apache_builds,
github_events") named a REDRESS-33-pre-blocked pattern as an
architecture lesson without an inline citation. The regression route
was already closed (§5.2 authors no intervention; §7.3 + §3 carried the
REDRESS 33 deferral), so V2 graded this a cosmetic-completeness gap, but
V1 fold #14 asked for the citation *in §5.2*.

**V3 fold — RESOLVED.** P2-F §5.2 (verified at lines 354-361) now
appends, after the "Expected impact" sentence:

> "This is a SOTA architecture *lesson*, not an admission: the
> dispatch-site NEON wiring shape it describes is pre-blocked by
> `skinny/REDRESS.md` entry 33 (`REDRESS.md:394-418` — Class A
> `match_tiny_plain_string` NEON at the wrong call site, parse-G fix
> invalidated), and any S-P3 attempt to wire it carries the REDRESS-33
> material-differential gate."

The citation is inline in §5.2 itself (no longer only in §7.3/§3), it
carries the explicit `REDRESS.md:394-418` line range, it states the
lesson-vs-admission distinction the V2 risk-table row 2 prescribed, and
it routes any S-P3 wiring attempt through a material-differential gate.
The `:394-418` range is correct — `skinny/REDRESS.md:394` is the entry
33 head ("33. SK-V5 Wave 3: Class A `match_tiny_plain_string` NEON
wiring is INVALIDATED as the parse-G fix"), verified by direct read.
F-7's residual is closed.

### §1.3 — V2 RESIDUAL fold #3 — cascade-dependency note in P2-D §0 footer

**V2 defect.** P2-D's four "block on P2-A landing OR fail CH5" slices
(§3 codec broadening, §4.4 CSSC CTZ, §5.3.1 EOR3, §5.4 dead-SIMD-scanner
wiring) create a wave-sequencing constraint: if P2-A does not pass
CH3+CH5 in the same wave, all four P2-D slices simultaneously lose their
union-substrate consumer and fall back to REDRESS-rejected parser-owned
shapes. V2 §4 risk-table row 3 named this a structural risk S-P3 must
honour and asked the §0 footer to state it.

**V3 fold — RESOLVED.** P2-D §0 footer (verified at lines 1165-1172)
now carries a dedicated bullet:

> "**Cascade-sequencing constraint** — P2-D's four 'block on P2-A
> landing OR fail CH5' slices (§3 codec broadening, §4.4 CSSC CTZ,
> §5.3.1 EOR3 ladder, §5.4 dead-SIMD-scanner wiring) collectively
> create a wave-sequencing constraint S-P3 must honour: P2-A must land
> in the same wave as any of these P2-D consumer slices, or the slices
> fall back to REDRESS-rejected parser-owned shapes; the wave may not
> be split."

The bullet names all four slices, states the constraint ("P2-A must
land in the same wave"), names the failure mode ("fall back to
REDRESS-rejected parser-owned shapes"), and forbids the split. This is
exactly the V2 risk-table row 3 prescription. The note is a structural
hand-off to S-P3 sequencing, not a new intervention — it authors
nothing, so it opens no REDRESS route.

### §1.4 — V2-RESIDUAL aggregate

All three CH3 V2 RESIDUAL-REVISE items are fully RESOLVED at V3. Each
fold is a verbatim single-bullet / single-sentence surgical addition;
none re-authors an intervention; none changes scope. The 93.0% → clean
delta is +3 dispositions (D-1, D-10, F-7 all move RESIDUAL-REVISE →
RESOLVED).

## §2 — V3 dispositions (≥25)

Disposition columns: *Item* / *V2 verdict* / *V3 disposition* / *V3
verdict*. V3 verdicts: **CONFIRMED** (V2 CONFIRMED/RESOLVED holds, no
V3 edit touches it or V3 edit is CH3-neutral), **RESOLVED** (V2
RESIDUAL-REVISE folded clean at V3), **RESIDUAL-REVISE** (fold
incomplete), **REGRESSION** (V3 edit opened a new REDRESS route).

### §2.1 — P2-A (zero V3 edits — V2-converged carry-forward)

| Item | V2 | V3 disposition | V3 verdict |
|---|---|---|---|
| A-1 | CONFIRMED | Class column co-emitted at `emit_event_offset`; no V3 edit. | **CONFIRMED.** |
| A-2 | RESOLVED | `StructuralIndex` falsifier intact — §2 lines 194-205 verified: lifetime ≤ one `parse`, never `Send`, no `'static`, never named on a tape; `rg` falsifier present. No V3 edit. | **CONFIRMED.** |
| A-4 | RESOLVED | `BackendShape` proven unchanged at variant level — five-variant enum, class column an interior `OffsetTape` refinement. No V3 edit. | **CONFIRMED.** |
| A-8 | RESOLVED | Class column per-grammar opt-in; generic `tape/` crate does not allocate. No V3 edit. | **CONFIRMED.** |
| A-9 | CONFIRMED | Consumer is `JsonNodeKind::at_cursor` production view, not `tape_vs_tape`. No V3 edit. | **CONFIRMED.** |

### §2.2 — P2-B (zero V3 edits — V2-converged carry-forward)

| Item | V2 | V3 disposition | V3 verdict |
|---|---|---|---|
| B-1 | CONFIRMED | Compile-time `EventGrammar` trait proof, `#[cfg(any(test, feature = "proof"))]` gated at the parent `pub mod` site (lib.rs:68 binding verified). No V3 edit. | **CONFIRMED.** |
| B-3 | CONFIRMED | Proof responds to REDRESS 92's "define + prove, reopen out of scope" routing. No V3 edit. | **CONFIRMED.** |
| B-5 | RESOLVED | `_witness`-directory binding intact — §0 + §1.2: `*_witness/` dirs hold only `EventGrammar` proof witnesses; `rg` admission check (no `scan.rs`/`parser.rs`/`generated.rs`/`view.rs` in any `_witness` dir) named as pre-commit + per-wave gate. No V3 edit. | **CONFIRMED.** |

### §2.3 — P2-C (zero V3 edits — V2-converged carry-forward)

| Item | V2 | V3 disposition | V3 verdict |
|---|---|---|---|
| C-1 | CONFIRMED | REDRESS 91 source/product → measured admission; row-table + gate ownership only. No V3 edit. | **CONFIRMED.** |
| C-2 | CONFIRMED | No retained-parse surface; existing generated DirectBuild typed path untouched. No V3 edit. | **CONFIRMED.** |
| C-4 | CONFIRMED | Direct rows hold SK-V9-open verdicts; REDRESS 93 not reopened. No V3 edit. | **CONFIRMED.** |
| C-6 | CONFIRMED — load-bearing | §4.3 falsifiability gate verbatim ("Existing four typed GO rows hold their `A / GO` outcome … no regression below sonic × 1.10⁻¹", verified line 449). No V3 edit. **Typed-GO guard intact.** | **CONFIRMED — load-bearing for §3.** |

### §2.4 — P2-D (5 V3 edits)

| Item | V2 | V3 disposition | V3 verdict |
|---|---|---|---|
| D-1 | RESOLVED-with-RESIDUAL | §5.3.1 EOR3 slice now carries the explicit six-row W10b no-regression maintain gate (lines 857-866), mirroring §4.4 byte-for-byte on the six-row identity, stated as a "hard blocking precondition." The V2 residual is closed. The Lock-16 `FEAT_SHA3` host-cap gate + unconditional scalar fallback reframe is unchanged. | **RESOLVED.** |
| D-2 | CONFIRMED — strengthened | §4.4 CSSC CTZ retains the narrow string-mask scope + six-row WIN-block falsification gate as a hard blocking precondition. No V3 edit; the §5.3.1 fold now *mirrors* §4.4, confirming §4.4 as the pattern source. | **CONFIRMED.** |
| D-4 | RESOLVED | §3.5 codec broadening still bound to the union-substrate consumer ("blocks on P2-A landing OR fails CH5"; absent P2-A a "REDRESS-82-style orphan … held back"). No V3 edit to §3.5 prose. | **CONFIRMED.** |
| D-5/D-6/D-7/D-8 | CONFIRMED | §5 union-substrate consumer, §4 per-block producer widening, §3 x4 batched stateless. No V3 edit. | **CONFIRMED.** |
| D-10 | RESOLVED-with-RESIDUAL | §5.3.1 EOR3 owner path (`bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`) + REDRESS 88 three-axis differential unchanged; the six-row no-regression gate the V2 residual demanded is now present (§1.1). HANDOFF §5 procedure now complete: fresh evidence, owner path, six-row gate, REDRESS 88+89 citation, Lock-16 gate, scalar fallback. | **RESOLVED.** |
| D-EOR3-cite | CH6-origin | §5.3.1 EOR3 latency claim now cites ARM DDI 0487 FEAT_SHA3 / FEAT_PMULL with an explicit "M5 Max P-core specifics are unpublished by Apple — treat absolute cycle counts as host-capability-gated estimate, the monotonic *ordering* EOR3 < PMULL is the load-bearing claim" caveat (lines 815-822). **CH3-neutral** — a documentation citation; it authors no intervention and the monotonic-ordering claim was already the basis of the V2-RESOLVED REDRESS-88 differential. No REDRESS route touched. | **CONFIRMED (CH3-neutral).** |
| D-§6.3 | CH6-origin | §6.3 reworded to distinguish per-primitive checkasm tests (same-wave admission preconditions per §6.2.1) from the deferred host-instrumentation infrastructure (invariants 2-5). **CH3-neutral** — the reword strengthens the no-orphan posture by making per-primitive checkasm a same-wave precondition, not a deferral; this *tightens* CH3, never loosens. No REDRESS route touched. | **CONFIRMED (CH3-neutral) — tightens no-orphan.** |
| D-§5.5/§8 cite | CH1-origin | REDRESS 28/33 citations now carry explicit `REDRESS.md` line ranges (28 → `:324-337`, 33 → `:394-418`) in §5.5 and §8. **CH3-neutral and CH3-positive** — precise REDRESS line ranges *improve* the REDRESS-reopen audit surface. Ranges verified correct against `skinny/REDRESS.md` (entry 28 head at line 324, entry 33 head at line 394). | **CONFIRMED (CH3-positive).** |
| D-§0 footer | CH3 fold #3 | Cascade-sequencing constraint bullet added (§1.3). Authors no intervention; a structural hand-off to S-P3. | **RESOLVED.** |

### §2.5 — P2-E (zero V3 edits — V2-converged carry-forward)

| Item | V2 | V3 disposition | V3 verdict |
|---|---|---|---|
| E-1 | RESOLVED | §5 consumer differential (REDRESS 82 = parser-owned per-quartet classifier vs P2-E full hex-decoder primitive class); §6 PMU rederived from actual TSV; §6.4 0.70-slack conditional-admission rule; honest "zero of four rows admit on codec alone." No V3 edit. | **CONFIRMED.** |
| E-2 | CONFIRMED | Codec on parse_only + retained-tape plane; no direct-string materialiser touch. No V3 edit. | **CONFIRMED.** |
| E-3 | CONFIRMED | Codegen template, const-generic specialisation; TOML `\u`/`\U` compile-validation-only. No V3 edit. | **CONFIRMED.** |

### §2.6 — P2-F (3 V3 edits)

| Item | V2 | V3 disposition | V3 verdict |
|---|---|---|---|
| F-2 | RESOLVED | §7.2 DirectBuild field-fact emit-site clause remains stripped; codec wires at the `parse-that-regex` unescape hot path only. No V3 edit to §7.2. | **CONFIRMED.** |
| F-3 | RESOLVED | §7.3 retitled "admission shapes deferred"; admission 1 (REDRESS 33) + admission 2 (REDRESS 89) remain deleted. No V3 edit to §7.3. | **CONFIRMED.** |
| F-6 | RESOLVED | §3 "Room to widen the lead" remains walked back to a finding — verified lines 277-283 still name DirectBuild Class A wiring as REDRESS 33 and `\uXXXX` fusion as REDRESS 66-69, "names the structural lead as a finding only", defers widening to S-P3. **4 typed-GO rows still unthreatened by P2-F.** No V3 edit to §3. | **CONFIRMED — load-bearing for §3.** |
| F-7 | RESOLVED-with-RESIDUAL | §5.2 now carries the inline REDRESS-33 citation with the `:394-418` line range and the lesson-vs-admission distinction (§1.2). The V2 cosmetic residual is closed. | **RESOLVED.** |
| F-§2.1 ContainerNext cite | CH6-origin | §2.1 ContainerNext reference now cites `generated.rs:341` (enum def), `:134-135` (consumed), `:348-375` (`consume_array_next`). **CH3-neutral** — a competitor-architecture grounding cite; authors no bbnf intervention. No REDRESS route touched. | **CONFIRMED (CH3-neutral).** |
| F-§5.4 CollapsedStage cite | CH6-origin | §5.4 CollapsedStage reference now anchored to `restart/ARCHITECTURE.md` §7.3 (`LayoutFacts.backend_shape`, enum at `ARCHITECTURE.md:1086`). **CH3-neutral** — anchors the fifth `BackendShape` variant to the design corpus; it does *not* propose a new variant (V9.5-PSI binding still rejects Rust-emitted DPDAs; CollapsedStage admission deferred to SK-V7 Wave 3 successor). No REDRESS route touched. | **CONFIRMED (CH3-neutral).** |
| F-§5 asmjson vocab cite | CH2-origin | asmjson §5 primitive-vocabulary reference now anchors the canonical primitive-class taxonomy by path to `skv9-p1-v3-B-xctrace-time-profiler.md` §1.5. **CH3-neutral** — a path anchor hygiene edit; authors no intervention. No REDRESS route touched. | **CONFIRMED (CH3-neutral).** |

### §2.7 — Typed-GO + direct-GO row protection (cross-cut, V3)

The 4 typed-GO rows (`twitter`, `update_center`, `mesh`, `marine_ik` /
`real_typed_struct`) and 3 direct-GO rows (`citm_catalog`, `marine_ik`,
`unicode_basic` / `direct_to_struct`) remain explicitly protected at V3:

- **P2-C §4.3** retains the verbatim guard (line 449): "Existing four
  typed GO rows hold their `A / GO` outcome … no regression below
  sonic × 1.10⁻¹" — a binding falsifiability gate on the only P2 report
  touching the typed row-table. P2-C carries zero V3 edits; the guard
  is byte-identical to V2.
- **P2-F §3** is unchanged at V3 — the "Room to widen the lead"
  walk-back to a finding stands (verified lines 270-283). No V3 P2-F
  edit re-proposes a typed-row substrate change.
- **P2-D §3.5** retains the direct-route no-regression CI guard on
  `unicode_escapes/direct`, `y_string_unicode/direct`,
  `unicode_mixed/direct`. No V3 edit to §3.5.
- **New at V3**: the §5.3.1 EOR3 six-row gate adds `marine_ik` (a
  typed-GO *and* direct-GO row) and `citm_catalog` (a direct-GO row)
  to the EOR3 candidate's no-regression precondition — the V3 fold
  *broadens* admitted-row protection, it cannot threaten it.

**Verdict: the 4 typed-GO + 3 direct-GO rows are explicitly protected
at V3, and the V3 fold strictly widens the protected set.** No V3 edit
puts an admitted row at silent-regression risk.

### §2.8 — V3-edit REDRESS-reopen scan (cross-cut)

Every one of the 8 V3 edits was checked against the REDRESS authority
surface. The 3 CH3 edits (six-row gate, §5.2 citation, §0 cascade
note) *tighten* REDRESS posture — they add gates and citations, they
author no intervention. The 5 non-CH3 edits (EOR3 latency cite, §6.3
reword, REDRESS 28/33 line ranges, ContainerNext cite, CollapsedStage
cite, asmjson vocab anchor) are documentation/citation edits: each
anchors an existing claim to a source, none proposes a new bbnf
intervention, none broadens a consumer, none wires a kernel. The §6.3
reword in particular *strengthens* the no-orphan posture (per-primitive
checkasm is now a same-wave precondition, not a deferral). **No V3 edit
reopens a REDRESS route.** Zero REGRESSION dispositions.

## §3 — Aggregate verdict

V3 cohort CH3 REGRESSION-disposition summary (43-item base carried from
V2, plus the 5 CH3-neutral V3-edit dispositions audited explicitly):

| Report | CONFIRMED | RESOLVED | RESID-REVISE | REGRESSION | Total |
|---|---:|---:|---:|---:|---:|
| P2-A | 9 | 0 | 0 | 0 | 9 |
| P2-B | 6 | 0 | 0 | 0 | 6 |
| P2-C | 6 | 0 | 0 | 0 | 6 |
| P2-D | 8 | 2 | 0 | 0 | 10 |
| P2-E | 5 | 0 | 0 | 0 | 5 |
| P2-F | 6 | 1 | 0 | 0 | 7 |
| **Total** | **40** | **3** | **0** | **0** | **43** |

CONFIRMED + RESOLVED (clean dispositions): **43/43 = 100%**. The 3 V2
RESIDUAL-REVISE items (D-1, D-10, F-7) all moved RESIDUAL-REVISE →
RESOLVED at V3. **Zero RESIDUAL-REVISE. Zero REGRESSION.** The 5
CH3-neutral V3 edits (audited in §2.4 and §2.6) are each CONFIRMED
CH3-neutral or CH3-positive; none of the 8 V3 edits opened a new
REDRESS-reopen route.

**Verdict against the §3Z convergence criterion.** V1 67.4% → V2 93.0%
→ **V3 100% clean (43/43)**. CH3 V3 clears the 95% bar by 5.0 points
with no scoring ambiguity (V2's 93%-vs-95.3% split, which depended on
whether the 3 residuals scored as ACCEPT, no longer exists — there are
zero residuals). V3 is the **first qualifying cycle** of the
≥95%-for-two-consecutive-cycles §3Z requirement (V2 at 93.0% did not
qualify). **CH3 V3 verdict: ACCEPT — 100%.** Per §3Z, CH3 needs one
further consecutive ≥95% cycle (V4) to converge; the V4 CHALLENGE will
be a re-verify with no fold expected (V3 leaves no residual).

P2-A, P2-B, P2-C, P2-E are each fully converged on CH3 (zero V3 edits,
100% carry-forward). P2-D and P2-F absorbed all 8 V3 edits and are now
also 100% clean on CH3.

## §4 — Remaining REDRESS-regression risks

| # | Origin | REDRESS entry | Risk | Status at V3 |
|---:|---|---|---|---|
| 1 | D-1 / D-10 (closed) | 88 + 89 + HANDOFF §5 | The §5.3.1 EOR3 slice now carries the explicit six-row W10b no-regression gate as a hard blocking precondition. **CLOSED at V3** — V2 risk-table row 1 prescribed exactly this sentence; it landed verbatim. The EOR3 body remains Lock-16 host-cap-gated (not the production default), scalar fallback unconditional. | **CLOSED.** No live reopen. |
| 2 | F-7 (closed) | 33 | P2-F §5.2 now inline-cites that the `match_tiny_plain_string`-class dispatch-site NEON wiring is the REDRESS-33 rejected shape, with the `:394-418` line range and the lesson-vs-admission distinction. **CLOSED at V3** — V2 risk-table row 2 prescription landed verbatim. | **CLOSED.** No live reopen. |
| 3 | D-4 / D-1 / D-2 / D-10 | 82 + 88 + 89 (no-orphan chain) | The cascade dependency — P2-D's four "block on P2-A OR fail CH5" slices losing their consumer simultaneously if the wave is split — is **now explicitly recorded in the P2-D §0 footer** as a cascade-sequencing constraint S-P3 must honour. This is a structural wave-sequencing constraint, not a V3-fold defect; it is now documented at the report level so S-P3 sequencing cannot miss it. | **DOCUMENTED.** S-P3 P3-B inheritance — not a regression; the §0 footer note is the V2-prescribed hand-off. |
| 4 | A-8 (residual watch) | Lock 1 cardinality + Lock 14 | P2-A's class column is a representation refinement on `OffsetTape` that every `OffsetTape`-routed grammar inherits; non-JSON grammars have no SK-V9 production consumer. This is a CH4-COST concern, not a CH3 regression — the column is per-grammar opt-in and the substrate is grammar-neutral. P2-A carries zero V3 edits; this is unchanged from V2. | **CH4 hand-off.** Not a CH3 regression — no V3 action.

**The cohort carries no live REDRESS-reopen at V3.** All three V2 CH3
risk-table rows (1, 2, 3) are CLOSED or DOCUMENTED per the V2-prescribed
fold. Risk 4 remains a CH4 hand-off. The V3 fold — 8 surgical edits
across two files — resolved every CH3 V2 residual and introduced zero
new REDRESS exposure: 5 of the 8 edits are documentation/citation
anchors (CH3-neutral), the §6.3 reword *tightens* the no-orphan posture,
and the three CH3-targeted edits (six-row gate, §5.2 citation, §0
cascade note) add gates and citations without authoring any
intervention. The 3 V1 REJECTs (F-2, F-3, F-6) remain RESOLVED — no V3
edit touched their strip sites. The 4 typed-GO + 3 direct-GO rows
remain explicitly protected, and the V3 EOR3 six-row gate strictly
widens the protected set.

— end CH3 V3.
