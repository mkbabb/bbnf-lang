# SK-V18 S-P3 CHALLENGE — CH6 OVERFIT-PRUNE (cycle V7)

Lens: OVERFIT-PRUNE (the spine). Target: `restart/skinny/tranches/sk-v18/SPEC.md` (1657 lines)
against S-P2 sequencing (`research/p2/SYNTHESIS-RESEARCH.md §3`) + the addenda
(`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`). Question: does the SPEC preserve the ONE-generator
framing end to end; the named primitives grammar-parameterized + (`css_balanced_component_scan`)
neutrality-discharged; Sheets a genuine negative control; nothing overfit to JSON/CSS?

Posture: V7 of the S-P3 harden. V6 (CH6) returned reject=0 revise=0 (accept=14). My charge is the
2-consecutive-clean fixed point: independently re-derive the verdict (not rubber-stamp V6), drive out
any RESIDUAL precision REVISE that would mislead an implementer, and catch any genuine REJECT. Be
PROPORTIONATE — a wording nit on a 1657-line doc is a REVISE only if it misleads. Disk-grounded this
pass at live HEAD; every load-bearing spine anchor re-verified ON DISK independently of V6.

---

## Disk re-verification (the spine anchors, this pass — re-grepped independently)

| SPEC claim | Anchor cited | Disk truth (re-verified V7) | Verdict |
|---|---|---|---|
| ONE neutral 5-shape discriminator | `lower/mod.rs:18` | `BackendShape::{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` dispatched at `:20-24` | CONFIRMED |
| The fork G3 DELETES | `grammar_provider.rs:39-43` | `pub enum RuntimeEmitterKind{CompiledLowering,RequestFacts}` at `:40-42`; `emitter` field `:33`; `first_unsupported` gate `:110` (`profile_contract.emitter != RequestFacts`) | CONFIRMED |
| R16 recipe-pin (NO PartialEq today) | `regen.rs:5` | `#[derive(Clone, Copy, Debug)]` — no PartialEq (the +1-line pin is real) | CONFIRMED |
| 7 byte-identical css_l4 replicas | 7× md5 `b654562c…` | `find …css_l4_*/generated.rs -exec md5` → `7 b654562ccff46ed62dd48e9ace325830` (all 7 identical) | CONFIRMED |
| CSS byte-SKIP shell (neutrality discharge) | `generated.rs:693-713` | `fn consume_balanced_at(&self,start,close:u8)->Result<usize,CssFactError>` at `:693`, body closes at `:713`; **0** `sink.*`/`push_leaf`/`emit` in the body (emits NOTHING) | CONFIRMED |
| CSS 94.1% hot leaf | `find_component_delim` | `fn find_component_delim(` at `:657` | CONFIRMED |
| JSON `{}`/`[]` is parse-with-emit | `generated.rs:833-834` | `b'{' => parse_object_direct(…,sink)` / `b'[' => parse_array_direct(…,sink)` at `:833-834` (same dispatch also at `:793-794`) — both carry `sink` | CONFIRMED |
| Sheets `paren_expr` descends | `google-sheets.bbnf:137` | `paren_expr = "(" , expression ?w , ")" ;` at `:137` (descends into `expression`) | CONFIRMED |
| 7-level precedence tower | `google-sheets.bbnf` | `comparison_expr(:103)→concat_expr(:105)→add_expr(:109)→mul_expr(:113)→exp_expr(:115)→unary_expr(:119)→postfix_expr(:121)→primary`, each `A = B (op B)*`; `expression = comparison_expr` `:163` | CONFIRMED |
| Sheets 185 lines; TODO AU.6.7 deferrals | `:62,73-75` | 185 lines; `TODO AU.6.7: -> decode_cell_ref` `:62`, `cell_ref` `-> input : Span` `:63`; LET/LAMBDA deferrals present | CONFIRMED |
| G4-deleted witnesses exist | `grammars/{json,sheets_witness}/event_grammar_witness.rs` | both files present on disk (the test-only `_proof_compiles` axis G4 deletes) | CONFIRMED |
| G6 caller-census RED today | `rg runtime_simd::find … grammars/*/generated.rs` | EMPTY (kernel not yet wired) — the gate is meaningfully RED pre-G6, GREEN only when G6 lands its consumer | CONFIRMED |

Every spine anchor the OVERFIT-PRUNE lens leans on is live at the cited path:line. The
neutrality-discharge is GROUNDED, not asserted: the CSS shell at `:693-713` emits nothing (grep-proven
0 emit calls), and the two offered non-CSS dischargers (JSON `:833-834` parse_object/array_direct WITH
`sink`; Sheets `:137` descending into `expression`) are both parse-with-emit descents — so "invoke the
SAME primitive" is structurally UNREACHABLE and the demotion to `css_balanced_component_scan` is the
FORCED, not contingent, outcome (s6/C4). On disk, exactly as the SPEC states.

---

## Enumeration under the OVERFIT-PRUNE lens — every wave-gate / telemetry / close claim

### A. ONE-generator framing, end to end

1. **`generator_grammar_count == 3` = json+css+sheets, NOT json+7-css+sheets (R-A0-2, the P3
   anti-inflation invariant).** Bound at §0.1#1, §0.4 (MUST-be-3-at-PROVE; 7-css inflation = REJECT),
   §3.3 P3 (collapse-to-one is the DISK-EVIDENCE default — one `stylesheet.bbnf`, byte-identical output;
   fake-`.bbnf`-root minting explicitly forbidden), §9 PROVE (the THIRD genuine grammar; the
   `grammar_name="google_sheets"` row collapses to itself under the R16 full-row `PartialEq`,
   count==1 per `grammar_name`), §11 ledger, and the §close restatement. Internally consistent across
   all sites; falsifiable and anti-inflation. The disk truth (7× identical md5 today) is the RED
   pre-collapse witness, exactly as §3.3 states. **ACCEPT.**

2. **One un-forked emitter on the lowered `BackendShape`, not a grammar tag (close-cond #2 / G3
   five-conjunct).** `emitter_fork_present==false ∧ generator_grammar_branch_count==0 ∧
   generator_grammar_type_count==0 ∧ runtime_target_rows_collapsed==true ∧
   emit_shape_source==lowered_program` (§0.1#2, §6 G3.2, §0.4). The fifth conjunct
   (`emit_shape_source==lowered_program`, G3.2 §5, lines 1111-1118) is the load-bearing defence against
   the §5-risk-1 relocated seam; the SPEC names it the binding distinction between an honest un-fork and
   a paper-close that relocates the fork into data, and pins the grep field-set
   (`target.profile`/`target.emitter`/`target.output_labels`/`target.profile_contract`/`contract.emitter`)
   IDENTICAL between the G3 exit conjunct 5 and the standing §2.1 seam-scan (line 342 / 498) — so the
   standing scan is provably as strong as G3's own. Disk-verified: the `BackendShape` discriminator and
   the `RuntimeEmitterKind` fork are both real. **ACCEPT.**

3. **The relocated-seam structural co-gate (R16, the ONLY check the arm-grep cannot see).**
   `runtime_target_rows_collapsed==true` via `RuntimeTarget: PartialEq` full-row over BOTH nested
   structs. §3.3 (lines 665-676) correctly splits the TWO distinct uses and forbids conflating them:
   (i) the relocated-seam check = FULL-ROW `PartialEq` over EVERY field with NO exclusion (incl. BOTH
   `frontend_requirements` #11 AND `output_labels` #12); (ii) the row-collapse count = the SEPARATE
   projected count excluding ONLY the two artefact-path columns `output_dir`/`expected_files` — and the
   SPEC explicitly states those two are excluded from THIS count only, NOT from the full-row seam-check.
   The "recurses into ONE nested struct" recipe is named the EXACT shallow-compare false-green and
   FORBIDDEN. Disk confirms `regen.rs:5` is `Clone,Copy,Debug` (no PartialEq), so the +1-line pin is
   the real mechanism. **ACCEPT.**

4. **`verbatim_blob_present == false` campaign-wide (the L1 banner-over-blob REJECT).** Bound at
   §0.1#1/#6, §0.4, §1 addendum 1, G1.2#4, G2.2#1, the (c) arm of every named-primitive contract, the
   §11 ledger. The CSS-courier ≈910 LOC is correctly declared cohort-carried, NOT gate-keyed on the
   exact figure (§1 addendum 1, §2.1 of the audit) — the boolean + the `.bbnf`-mutation test is the
   binding gate, so no overfit-to-a-magic-LOC-number creeps in. **ACCEPT.**

### B. The named primitives — grammar-parameterized + (a)-(d) bounded

5. **The §6 (a)-(d) escape gate (the single largest paper-close surface, R-A0-3).** §1 (lines 357-375)
   states all four conjuncts: (a) grammar-INVOKED-by-name, (b) emitted-output-VARIES-under-rule-mutation
   (the BYTE-SET/numeric-class mutation, the discriminator between derived and relabeled), (c)
   `verbatim_blob_present==false`, (d) PROFILE-PROVEN-NARROW-LEAF (primitive LOC ≤ profiled hot-leaf
   extent; skeleton walk-derived). (d) is the size-bound that stops an arbitrarily-large relabeled blob
   that merely varies under mutation — correctly machine-checked, not prose: G1.4 emits
   `g1_leaf_primitive_loc` / `g1_leaf_primitive_profiled_leaf_extent` and the gate REJECTs on
   `loc > extent`; G2.5 mirrors with `g2_balanced_scan_primitive_loc` /
   `g2_balanced_scan_profiled_leaf_extent`. The (b) and (d) arms are what make the escape un-gameable.
   **ACCEPT.**

6. **`css_balanced_component_scan` — FORCED CSS-scoped demotion (s6/C4, the CH6-specific obligation).**
   This is the heart of my lens. §1 (lines 380-392) and §5 G2.2#5 / G2.5 state the demotion is FORCED,
   not contingent: the GROUND re-validation grounded BOTH offered non-CSS dischargers on disk and found
   them structurally incompatible with the CSS byte-SKIP shell. I independently re-grounded all three on
   disk this pass: the CSS shell `consume_balanced_at` (`:693-713`) emits NOTHING (0 emit calls,
   grep-proven); JSON `{}`/`[]` (`:833-834`) dispatches `parse_object_direct`/`parse_array_direct` WITH
   `sink`; Sheets `paren_expr` (`:137`) descends into `expression`. The two dischargers are parse-with-
   emit; the CSS shell is byte-skip. "Invoke the SAME primitive" is therefore UNREACHABLE by the named
   candidates, and the CSS-scoped name is correctly the FORCED outcome. The gate
   (`g2_balanced_scan_neutrality_discharged`) is discharged by the CSS-scoped rename, NOT a fabricated
   cross-grammar caller — the SPEC explicitly states "the gate REJECTS a NEUTRAL name with zero
   structurally-compatible non-CSS caller" (lines 392, 975-976). The inner alphabet-scan sub-kernel (the
   `bbnf-simd` eq-set member scan) remains genuinely neutral caller-data; only the balanced-recognizer
   SHELL is CSS-scoped. This is the honest disposition: a neutrally-named CSS-only primitive would be an
   overfit-in-waiting, and the SPEC refuses it. **ACCEPT.**

7. **The JSON `string`/`number` leaf scanners (`decode_json_string_to_arena`, `parse_number_*`).** §4
   G1.3 (lines 853-868) binds each to the per-primitive (a)-(d) contract; (b) is specifically the
   digit-class widening falsifier (widen the `number` rule's digit class in `.bbnf` ⇒ the `b'0'..=b'9'`
   literal widens — a kernel that does not vary under its own rule's class mutation is a relabeled fixed
   courier). The §11 ledger pre-blocks "a relabeled fragment as a leaf primitive (fails (b)/(d))". The
   structural SKELETON (dispatch match, container loops, the 3 sink-prefix variants) is walk-derived;
   only the proven-hot inner LEAF kernels are gated primitives. The "do NOT LCD-unify the dispatch
   triple" instruction (G1.2#3, G1.3) protects the monomorphized-sink leaf — this is the
   anti-flatten discipline applied to JSON's OWN structure, not an overfit. **ACCEPT.**

### C. Sheets — a genuine negative control (not a third-JSON, not a courier)

8. **The precedence tower is the SOLE Sheets-distinctive construct; `Nu8`-tagged-alt is SHARED, not the
   litmus.** §9 (lines 1417-1421) demotes the `Nu8`-tagged-alt family from the litmus (CSS uses it 295×
   vs Sheets 21× across the import closure, so the generator must already handle it at scale to emit CSS
   at all). The 295×/21× figures are S-P2-research-derived expanded-import-closure counts (V6 noted a
   raw-token grep does not reproduce them); CRITICALLY, NO gate/telemetry/close predicate keys on the
   figure — the `sheets_grammar_shape==pratt-operator` gate keys ENTIRELY on the ≥7-chained-level-fn
   STRUCTURAL falsifier (§9 lines 1457-1464), and the Nu8 count is directional demotion-justification
   prose only. The load-bearing demotion claim ("precedence tower is the SOLE distinctive construct") is
   independently disk-grounded: I confirmed the 7-level chain at `google-sheets.bbnf:103-121`. An
   implementer never tests the 295×/21× value. Per proportionality this is NOT misleading → **ACCEPT.**

9. **`sheets_grammar_shape == pratt-operator` proven by a CONCRETE STRUCTURAL falsifier, NOT "by
   construction".** §9 (lines 1457-1464) machine-checks it: count the emitted per-level descent fns
   (≥7 non-terminal levels each calling the next) AND the cyclic `paren_expr→expression` back-edge; a
   `flat-stream` (the R-E-3 flattened-tower REJECT predicate) or a `tree` emission has FEWER than 7
   chained level fns and FAILS the count. This is the anti-"third-JSON-hollow-litmus" teeth — R-E-3 is
   REJECTED outright (§9 line 1420, §11 ledger). The enum `{pratt-operator|courier|hollow}` is closed
   (§0.4). Disk confirms the 7-level tower is real, so the falsifier is satisfiable on a true emission
   and fails on a hollow one. **ACCEPT.**

10. **The BINDING negative-control fallback — `N`, never paper-close, never stub-prove.** §0.1#9, §0.3
    (the `N` outcome), §9 falsifiers + binding fallback (lines 1481-1492), §11 ledger, §close. If Sheets
    cannot emit via the generator ONLY, generalization is NOT real — surfaced honestly as
    `sheets_emission_path == shim` → outcome `N`, NEVER a `const SHEETS_GENERATED_RS` courier, NEVER a
    `GoogleSheets =>` arm. The §0.3 enum cleanly separates `N` (negative-control fail / generalization
    unreal) from `L` (honest non-Sheets residual loss) from `S` (admission-capable non-SOTA residual) —
    no outcome-class overlap that could let a shim slip through as a soft pass. The gate explicitly does
    NOT paper-close a shim as a pass (§9 line 1515). This is the genuine-negative-control property: the
    control can FAIL, and failure has a binding honest disposition. **ACCEPT.**

11. **`import_closure_relaxation_is_data == true` — DATA change, NOT a `match grammar` arm.** §0.1#9,
    §2.1 (template/provider boundary), §9 task 3 (lines 1444-1448), §11 ledger. The relaxation is
    `present-iff-grammar-has-imports`, DERIVED from grammar facts (a `RuntimeFrontendRequirements` data
    change), explicitly NOT a `match grammar { GoogleSheets => … }` arm. Disk confirms Sheets has no
    `@import` and the `import_closure: true` contract at `grammar_provider.rs:263` is the live gate.
    Routing the relaxation as a per-grammar branch would be the EXACT overfit; the SPEC forbids it and
    the §11 ledger pre-blocks "an `import_closure` `match grammar` arm". **ACCEPT.**

### D. Nothing overfit to JSON/CSS — the two architectural regressors EXCLUDED

12. **The full grammar-IR tree-walk REJECTED (R-B/R-C Candidate C).** §5 (the deep finding, lines
    921-930) names it: a naive grammar-walk lowering produces the combinator-shaped recursive descent
    = lightningcss's own architecture, which categorically regresses >SOTA. REJECTED outright at §5,
    §1 (R-B Candidate C), and re-listed in the §11 G2 ledger ("the full grammar-IR tree-walk (R-B
    Candidate C)"). The hybrid (named primitive CORE + fact-keyed `push_str` projection blocks) is the
    grammar-DERIVED path that preserves the scan SHAPE the profile attributes the win to. This is
    NOT overfit — it derives the scan from grammar-supplied alphabet/structural-byte/branch-tag/entry
    facts. **ACCEPT.**

13. **The forced-common `Value` shape REJECTED (R-D Candidate B — LCD-flatten).** §7 G4 + §5-risk-4.
    The `Cursor` micro-trait shares ONLY the laziness/cursor contract, NEVER navigation. JSON's rich
    tree is preserved by a CONCRETE FALSIFIER, not "by construction": §0.1#4 and §7 G4.2#2 bind
    `json_rich_navigation_preserved==true` to the byte-equal diff (JSON `value.rs` nav surface diffs
    EMPTY vs pre-G4 ∧ 51/51 held) — the SPEC explicitly calls "asserted by construction" the
    unfalsifiable-gate hazard and REJECTs it (§0.1#4 lines 90-93). The ≥2-impl count is made
    necessary-NOT-sufficient by `shared_trait_non_collapsible` (the substitution falsifier: swap JSON's
    nav impl for CSS's ⇒ compile FAILS; a degenerate-equal CSS impl COMPILES ⇒ REJECT, G4.2#3). This
    is the strongest possible anti-LCD-flatten teeth — preserve-rich-ast holds on the trait axis.
    **ACCEPT.**

14. **preserve-rich-ast on the bench axis (lazy `track1_rich`, never flattened).** §0.2 framing,
    §1 (preserve-rich-ast non-negotiable), H1 §10. `track1_rich` stays lazy `ValueRef`-view projection
    re-derived from `(source,offset)` spans, writing NOTHING to the arena
    (`css_l4_declaration_values/generated.rs:297-304`). The H1 disclosure
    `materialization_framing==lazy-rich-vs-eager-cssom` (the closed two-value enum) forbids the
    unqualified "beats CSSOM" re-label (R-A0-1/R14). The recognition-only `track1_full_parse` `A` is
    explicitly stated to NOT by itself discharge the typed close (§0.2, §0.3 line 229) — so a flattened
    recognizer cannot masquerade as the rich-AST win. **ACCEPT.**

15. **Lock-14 scans the codegen AS authored (P4 before G2/G3) — no overfit re-introduced under a blind
    gate.** §0.1#8, §3.4 P4, §2.1. P4's `FORBIDDEN_GENERIC_TOKENS` extension is correctly scoped: the
    `_RS` token is the suffix `GENERATED_RS` (catching `CSS_GENERATED_RS` AND
    `JSON_PARSE_ONLY_GENERATED_RS`, the two grammar-body couriers) so the six surviving
    MOD/HOST/PARSER/SINK scaffold consts in the now-strict-scanned `runtime_generator.rs` do NOT
    false-RED under the `source.contains` substring semantics — a proportionate, precise token scoping
    that prevents the gate from being either too weak (green-by-exclusion) or too strong (false-RED on
    legitimate scaffold). The re-inject falsifier (a `GENERATED_RS`-bearing or `EventGrammar` token, NOT
    a bare `JsonSink` which is NOT in the set) is correctly specified — P4's rerun-ceiling table (line
    471) matches this. **ACCEPT.**

16. **The sequencing preserves the spine (S-P2 §3 lattice ⊇ the SPEC §2.1 lattice).** The SPEC's
    §2.1 lattice folds the seq/C6 (PROVE never admits before G4 closes — G4 is PROVE's DIRECT, not
    merely transitive, predecessor) and seq/C7 (G5/G6 hangs off G3 PARALLEL to G4, NOT under G4)
    corrections. I cross-checked against S-P2 §3 (which still shows PROVE under G4 in its ASCII diagram
    at line 140 with the "PARALLEL to G5/G6" annotation, and the prose conjunct "PROVE entry-gates on
    G3 ∧ G4"). The SPEC §2.1 (lines 535-543) + §8 entry + §9 entry correctly render G5/G6 and PROVE as
    TWO parallel branches off G3 that join at H1, with PROVE's G4 conjunct explicit. This is the
    corrected, more-precise rendering — no broken sequence, no entry-gate that lets a downstream wave
    dispatch over a REDRESSed predecessor. **ACCEPT.**

---

## Residual-REVISE scan (proportionality applied — REVISE only if it would mislead an implementer)

- **§9 line 1418 "`-> Nu8u8` 295× … vs Sheets' 21×".** Re-examined (V6 raised and ACCEPTed this).
  Independently re-confirmed: NO gate/telemetry/close predicate keys on the figure; it is directional
  demotion-justification prose, and the load-bearing demotion claim is structurally disk-grounded (the
  7-level tower is the litmus, machine-checked by the ≥7-chained-level-fn count). An implementer never
  tests the 295×/21× value. NOT misleading → ACCEPT, not REVISE. (Out of the OVERFIT-PRUNE lens's
  load-bearing surface; the count is decoration, the structural falsifier is the gate.)

- **Net LOC ≈ −10800 vs per-wave sum (§2 manifest: P1 −4500, P2 −700, P3 −5500, P4 +15, G-waves net
  ≈0, Sheets ≈+200).** The "≈" framing is honest and the per-wave figures carry their own "≈". This is
  a CH4/CH7 arithmetic-honesty matter, NOT an OVERFIT-PRUNE-lens claim — no overfit hides in the LOC
  total. Out of my lens; no REVISE.

- **JSON neutrality-discharge citation `generated.rs:833-834` (§1 line 386, §5 line 970).** Disk shows
  the IDENTICAL parse-with-emit dispatch at BOTH `:793-794` and `:833-834`. The SPEC cites `:833-834`;
  both are equally valid witnesses of the same property (parse_object/array_direct WITH `sink`). The
  cited line is live and correct — picking one of two equivalent occurrences is not a defect and does
  not mislead. No REVISE.

No residual REVISE survives under the OVERFIT-PRUNE lens. Independently re-derived (not rubber-stamped
from V6): the ONE-generator framing is preserved end to end and grammar-DERIVED at every emit site;
the named primitives are grammar-parameterized, (a)-(d)-bounded with machine-checked LOC-vs-profiled-
extent size gates; the CSS balanced-scan is FORCED-demoted to `css_balanced_component_scan` with the
neutrality obligation discharged by the honest CSS-scoped name (disk-grounded: the CSS shell emits
nothing, the two non-CSS dischargers are parse-with-emit, so the SAME-primitive branch is structurally
unreachable — not fabricated); Sheets is a genuine negative control with a structural (not
by-construction) ≥7-chained-level-fn shape falsifier and a binding `N` fallback that can FAIL honestly;
nothing is overfit to JSON or CSS — the two architectural regressors (full grammar-IR tree-walk;
forced-common `Value` shape) are excluded and re-listed in the §11 ledger, preserve-rich-ast holds on
both the trait axis (the substitution falsifier) and the bench axis (lazy `track1_rich`), Lock-14
scans the codegen as authored with proportionately-scoped forbidden tokens, and the relocated-seam
structural co-gate (R16 full-row `PartialEq` over BOTH nested structs) catches the one seam the
arm-grep cannot. No unfalsifiable gate, no broken sequence, no addenda violation surfaces under this
lens. V6 and V7 both reject=0 revise=0 — the 2-consecutive-clean fixed point is met under CH6.

TALLY accept=16 revise=0 reject=0
