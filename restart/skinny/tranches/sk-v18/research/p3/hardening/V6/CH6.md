# SK-V18 S-P3 CHALLENGE — CH6 OVERFIT-PRUNE (cycle V6)

Lens: OVERFIT-PRUNE (the spine). Target: `restart/skinny/tranches/sk-v18/SPEC.md` (1642 lines)
against S-P2 sequencing (`research/p2/SYNTHESIS-RESEARCH.md §3`) + the addenda
(`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`). Question: does the SPEC preserve the ONE-generator
framing end to end; the named primitives grammar-parameterized + (`css_balanced_component_scan`)
neutrality-discharged; Sheets a genuine negative control; nothing overfit to JSON/CSS?

Posture: V6 of a 6-cycle harden (V1–V5 all reject=0; the loop hit the v<5 ceiling without a
2-consecutive-clean streak — `HARDENING-S-P3-CONSOLIDATED.md §1`). My charge is to drive out the
RESIDUAL precision REVISEs toward a fixed point and catch any genuine REJECT. Disk-grounded this pass
at live HEAD; every load-bearing spine anchor re-verified on disk.

---

## Disk re-verification (the spine anchors, this pass)

| SPEC claim | Anchor cited | Disk truth | Verdict |
|---|---|---|---|
| ONE neutral 5-shape discriminator | `lower/mod.rs:18` | `BackendShape::{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` at `:20-24` | CONFIRMED |
| The fork G3 DELETES | `grammar_provider.rs:39-43` | `pub enum RuntimeEmitterKind{CompiledLowering,RequestFacts}` at `:40-42`; `emitter` field `:33`; `first_unsupported` gate `:110` | CONFIRMED |
| R16 recipe-pin (NO PartialEq today) | `regen.rs:5` | `#[derive(Clone, Copy, Debug)]` — no PartialEq; `emitter`#9 `:15`, `frontend_requirements`#11 `:17`, `output_labels`#12 `:18`; both nested derive `PartialEq,Eq` at `grammar_provider.rs:45/:91` | CONFIRMED |
| CSS byte-SKIP shell (neutrality discharge) | `generated.rs:693-713` | `fn consume_balanced_at(&self, start, close: u8) -> Result<usize, CssFactError>` at `:693` (skip-recognizer, emits nothing) | CONFIRMED |
| JSON `{}`/`[]` is parse-with-emit | `generated.rs:833-834` | `b'{' => parse_object_direct(…, sink)` / `b'[' => parse_array_direct(…, sink)` | CONFIRMED |
| Sheets `paren_expr` descends | `google-sheets.bbnf:137` | `paren_expr = "(" , expression ?w , ")" ;` (descends into `expression`) | CONFIRMED |
| 7-level precedence tower | `google-sheets.bbnf` | `comparison_expr→concat_expr→add_expr→mul_expr→exp_expr→unary_expr→postfix_expr→primary` at `:103-125`, each chaining the next via `A = B (op B)*` | CONFIRMED |
| Sheets 185 lines; TODO AU.6.7 deferrals | `:62,73-75` | 185 lines; TODO AU.6.7 `cell_ref`/`range`/`LET`/`LAMBDA` deferrals present | CONFIRMED |

Every spine anchor the OVERFIT-PRUNE lens leans on is live at the cited path:line. The
neutrality-discharge is not asserted — it is grounded: the CSS shell at `:693` emits nothing, the
two offered non-CSS dischargers (JSON `:833-834`, Sheets `:137`) are both parse-with-emit descents,
so "invoke the SAME primitive" is structurally UNREACHABLE and the demotion to
`css_balanced_component_scan` is the FORCED outcome (s6/C4). On disk, exactly as the SPEC states.

---

## Enumeration under the OVERFIT-PRUNE lens — every wave-gate / telemetry / close claim

### A. ONE-generator framing, end to end

1. **Close-cond #1 + restatement (`generator_grammar_count == 3` = json+css+sheets, NOT
   json+7-css+sheets, R-A0-2).** Bound at §0.1#1, §0.4 (MUST-be-3-at-PROVE; 7-css inflation = REJECT),
   P3 §3.3 (collapse-to-one default, fake-`.bbnf`-root minting forbidden), PROVE §9 (the THIRD genuine
   grammar via the R16 full-row self-collapse, count==1 per grammar_name), §11 ledger, and the §close
   restatement. Internally consistent across all 8 sites; the anti-inflation invariant is explicit and
   falsifiable. **ACCEPT.**

2. **One un-forked emitter on the lowered `BackendShape`, not a grammar tag (close-cond #2 / G3
   five-conjunct).** `emitter_fork_present==false ∧ generator_grammar_branch_count==0 ∧
   generator_grammar_type_count==0 ∧ runtime_target_rows_collapsed==true ∧
   emit_shape_source==lowered_program`. The fifth conjunct (G3.2 §1105, §0.4) is the load-bearing
   defence against the §5-risk-1 relocated seam — the SPEC correctly names it the binding distinction
   between an honest un-fork and a paper-close that relocates the fork into data, and pins the grep
   field-set IDENTICAL to the standing §2.1 seam-scan. Disk-verified the discriminator (`BackendShape`)
   and the fork (`RuntimeEmitterKind`) are real. **ACCEPT.**

3. **The relocated-seam structural co-gate (R16, the ONLY check the arm-grep cannot see).**
   `runtime_target_rows_collapsed==true` via `RuntimeTarget: PartialEq` full-row over BOTH nested
   structs. §3.3 correctly splits the TWO uses (full-row seam-check incl. nested structs with NO
   exclusion; row-collapse count excluding only `output_dir`/`expected_files`) and forbids the
   hand-rolled shallow-compare. The +1-line recipe is disk-confirmed free (nested structs already
   derive PartialEq,Eq). This is the spine's anti-overfit teeth and it is airtight. **ACCEPT.**

### B. Named primitives grammar-parameterized + neutrality-discharged

4. **The §6 (a)-(d) escape gate (the single largest paper-close surface, R-A0-3).** All four arms
   normatively present and machine-checked: (a) grammar-INVOKED-by-name, (b) emitted-output VARIES under
   invoking-rule mutation (the byte-set/numeric-class mutation, distinguishing derived from relabeled),
   (c) `verbatim_blob_present==false`, (d) PROFILE-PROVEN-NARROW-LEAF (LOC ≤ profiled extent, the
   god-kernel REJECT). G1.3/G2.3 each bind the (d) machine-check as a LOC comparison
   (`*_primitive_loc <= *_profiled_leaf_extent`), not an assertion. **ACCEPT.**

5. **`css_balanced_component_scan` FORCED CSS-scoped demotion (CH6's own obligation; s6/C4).** Section 1
   §380-392, G2.2 §5, G2.3, and §2.1 all carry the FORCED (not contingent) demotion, grounded in the
   on-disk structural incompatibility (byte-SKIP shell vs parse-with-emit dischargers). The gate
   `g2_balanced_scan_neutrality_discharged` is GREEN via the CSS-scoped rename, NOT a fabricated
   cross-grammar caller; the gate REJECTS a neutral name with zero structurally-compatible non-CSS
   caller. The inner eq-set sub-kernel stays genuinely neutral (caller-data); only the SHELL is
   CSS-scoped. This is the exact correct disposition for an overfit-in-waiting. **ACCEPT.**

6. **The named-primitive roster is bounded, not open.** The campaign admits exactly:
   `css_balanced_component_scan` (CSS 94.1% leaf, G2/G6), the JSON `decode_json_string_to_arena` /
   `parse_number_*` leaf scanners (91.5% leaf, G1). Each carries its own (a)-(d) contract +
   `*_abcd_pass` gate arm + the (d) LOC numerator/denominator telemetry. No primitive spans a rule body
   or an unprofiled region without a REJECT. **ACCEPT.**

7. **G6 NEON onto the CSS scan shell — SHARED grammar-neutral primitive, not per-grammar vector code
   (close-cond #10 / §8).** `acceleration_at_admission==admission` requires BOTH conjuncts: the
   `generated.rs` caller census (NOT `#[cfg(test)]`) AND `simd_admission_profile_sampled==true` (the
   runtime-reachability proof). `significant_set_is_caller_data==true` + `css_scan_call_site_singular==
   true` (the P3-collapsed single scan; re-emit-7-ways == FAIL). The kernel is hand-authored ONCE in
   `bbnf-simd`; the generator emits a CALL, not vector code per grammar. The enum is the same two-value
   domain across §0.3/§0.4/§8 (a third state would make the gate non-deterministic — the SPEC names this
   explicitly). No JSON classifier authored (G5 retires the zero-sampled `json/scan.rs`; outcome `N` —
   nothing on product path). **ACCEPT.**

### C. Sheets a genuine negative control

8. **Sheets the negative control via the un-forked generator ONLY (close-cond #9 / PROVE §9).**
   `sheets_grammar_shape==pratt-operator` proven by a CONCRETE STRUCTURAL FALSIFIER (≥7 chained
   precedence-level descent fns + the cyclic `paren_expr→expression` back-edge), NOT "by construction".
   Disk-verified: the 7-level tower exists verbatim (`:103-125`). A `flat-stream` (R-E-3 REJECT) or a
   `tree` emission has FEWER than 7 chained level fns and FAILS the count. The `Nu8`-tagged-alt family
   correctly demoted from the litmus as a SHARED construct. **ACCEPT.**

9. **The binding fallback is `N`, never paper-closed, never stub-proved (§0.3 / §9 / §2 / §11).**
   `sheets_emission_path==shim` ⇒ outcome `N` (generalization NOT real), surfaced honestly; `L` is
   correctly RESERVED for a non-Sheets honest residual; the gate does NOT paper-close a shim as a pass.
   Internally consistent across all SPEC sites (verified: every `negative-control`/`outcome N`
   reference in the SPEC agrees — §213, §220-224, §1353, §1472-1473, §1497, §1506, §1514-1516).
   **ACCEPT.** (Note out of scope: `HARDENING-S-P3-CONSOLIDATED.md §4` line 135 says "outcome L" for
   this same fallback — a defect in the CONSOLIDATED doc, not the SPEC; the SPEC itself is correct and
   self-consistent on `N`.)

10. **Sheets instantiates the G4 trait without LCD-flattening (PROVE entry on G4 DIRECT, seq/C6).**
    `sheets_value_instantiates_g4_trait==true`; PROVE entry-gates on `G3 ∧ G4` with G4 a DIRECT (not
    merely transitive) conjunct — the seq/C6 correction is folded at §2 manifest, §2.1 lattice, and §9
    entry-gate ("PROVE NEVER admits before G4 closes"). The third trait impl is what makes the
    phantom-`<G>` resolution concrete. **ACCEPT.**

11. **Import-closure relaxation is DATA, not a `match grammar` arm (close-cond #9 / §9 task 3 / §2.1).**
    `import_closure_relaxation_is_data==true` (present-iff-grammar-has-imports, derived from facts), a
    `RuntimeFrontendRequirements` data change. Explicitly NOT a `GoogleSheets => …` arm. The SPEC
    enforces this with the `generator_grammar_branch_count==0` arm-census including the
    `(GoogleSheets|Sheets)\w*\s*=>` regex. **ACCEPT.**

### D. Nothing overfit to JSON/CSS

12. **Lock-14 P4 lands BEFORE G2/G3 (neutrality-scanned AS authored).** `FORBIDDEN_GENERIC_TOKENS ⊇
    {CSS_,_RS,EventGrammar,*EventGrammar}`; the re-inject falsifier keys on a `_RS`/`CSS_` token (NOT a
    bare `JsonSink`, which is not in the set — the V-prior CH1 fold). `lock14_gate_scans_codegen==
    true`. The witness-emission scan-root coupling is handled (post-G4 NO `EventGrammar` literal can be
    emitted; P4 stands as defence-in-depth). **ACCEPT.**

13. **The non-JSON first-mover triple (§2.1).** `projection_generality_exercise ∈
    {json, css_l4, google_sheets}`; `simd_non_json_exercise=css_l4` (S-P1 has NO JSON G5 hot leaf,
    correctly stated). Sheets is the negative control proving grammar-DERIVED emission. The CH7/CH6
    Overfit-Prune lens is bound on EVERY wave's §2.1 exit gate. **ACCEPT.**

14. **The two architectural overfit-regressors correctly EXCLUDED.** The full grammar-IR tree-walk
    (R-B/R-C Candidate C — the combinator-shaped descent = lightningcss's own architecture, regresses
    >SOTA) and the forced-common `Value` shape (R-D Candidate B — LCD-flatten) are REJECTED outright and
    re-listed in the §11 pre-blocked ledger. preserve-rich-ast holds on both the trait axis
    (`json_rich_navigation_preserved` + the `shared_trait_non_collapsible` substitution falsifier) and
    the bench axis (lazy `track1_rich`, never flattened). **ACCEPT.**

---

## Residual-REVISE scan (proportionality applied — REVISE only if it would mislead an implementer)

- **§9 line 1409-1410 numerical claim "`-> Nu8u8` 295× across its import closure vs Sheets' 21×".** Raw
  `.bbnf` token grep does not reproduce these (the CSS l4 tree shows 0 `Nu8u8` literal tokens; Sheets
  shows 4 raw `Nu8` line-hits, not 21) — the figures are S-P2-research-derived counts over the EXPANDED
  import closure, not raw-token counts. CRITICAL TEST: does any GATE, telemetry column, or close
  predicate key on this figure? Grep-confirmed NO — the `sheets_grammar_shape==pratt-operator` gate is
  keyed entirely on the ≥7-chained-level-fn STRUCTURAL falsifier (§1448-1454), and the Nu8 count appears
  ONLY as directional justification prose for demoting a SHARED construct from the litmus. An implementer
  never tests the 295×/21× value; the load-bearing demotion claim ("precedence tower is the SOLE
  distinctive construct") is independently grounded on disk. Per proportionality this is NOT
  misleading-to-implementer → **ACCEPT, not REVISE.**

- **Net LOC ≈ −10800 vs per-wave sum ≈ −10685** (P1 −4500 + P2 −700 + P3 −5500 + P4 +15). The ~115-LOC
  gap sits inside the "≈" tolerance (the audit doc itself states P3 ≈ −5460; G-wave generated net ≈ 0;
  Sheets adoption ≈ +200). This is a CH4/CH7 arithmetic-honesty matter, NOT an OVERFIT-PRUNE-lens claim,
  and the "≈" framing is honest. Out of my lens; no REVISE raised here.

No residual REVISE survives under the OVERFIT-PRUNE lens. The ONE-generator framing is preserved end to
end and grammar-DERIVED at every emit site; the named primitives are grammar-parameterized,
(a)-(d)-bounded, and the CSS balanced-scan is FORCED-demoted with the neutrality obligation discharged
by the honest CSS-scoped name (disk-grounded, not fabricated); Sheets is a genuine negative control with
a structural (not by-construction) shape falsifier and a binding `N` fallback; nothing is overfit to
JSON or CSS — the two architectural regressors are excluded, Lock-14 scans the codegen as authored, and
the relocated-seam structural co-gate (R16) catches the one seam the arm-grep cannot. No unfalsifiable
gate, no broken sequence, no addenda violation surfaces under this lens.

TALLY accept=14 revise=0 reject=0
