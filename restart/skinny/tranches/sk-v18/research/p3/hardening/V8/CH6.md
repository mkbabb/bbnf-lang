# SK-V18 S-P3 CHALLENGE — CH6 OVERFIT-PRUNE (cycle V8)

Lens: OVERFIT-PRUNE (the spine). Target: `restart/skinny/tranches/sk-v18/SPEC.md` (1660 lines)
against S-P2 sequencing (`research/p2/SYNTHESIS-RESEARCH.md §3`) + the addenda
(`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`). Question: does the SPEC preserve the ONE-generator
framing end to end; the named primitives grammar-parameterized + (`css_balanced_component_scan`)
neutrality-discharged; Sheets a genuine negative control; nothing overfit to JSON/CSS?

Posture: V8 of the S-P3 harden. V6 (CH6) returned reject=0 revise=0 (accept=14); V7 (CH6) returned
reject=0 revise=0 (accept=16). The 2-consecutive-clean fixed point is nominally already met under
CH6. My charge is NOT to rubber-stamp: independently re-derive the verdict on disk, drive out any
RESIDUAL precision REVISE that would mislead an implementer, and catch any genuine REJECT. Be
PROPORTIONATE — a wording nit on a 1660-line doc is a REVISE only if it would mislead. Every
load-bearing spine anchor + every enum domain + the same-run ratio gate's falsifiability re-grounded
ON DISK this pass, independently of V6/V7.

---

## Disk re-verification (the spine anchors + the gate-determinism axes, re-grepped independently)

| SPEC claim | Anchor cited | Disk truth (re-verified V8) | Verdict |
|---|---|---|---|
| ONE neutral 5-shape discriminator | `lower/mod.rs:18` | `BackendShape::{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` dispatched at `:20-24` | CONFIRMED |
| The fork G3 DELETES | `grammar_provider.rs:39-43` | `pub enum RuntimeEmitterKind{CompiledLowering,RequestFacts}` at `:40-42`; `emitter` field `:33`; `first_unsupported` gate `:110` (`profile_contract.emitter != RequestFacts`) | CONFIRMED |
| R16 recipe-pin (NO PartialEq today) | `regen.rs:5` | `#[derive(Clone, Copy, Debug)]` — no PartialEq (the +1-line pin is real) | CONFIRMED |
| CSS byte-SKIP shell (neutrality discharge) | `generated.rs:693-713` | `fn consume_balanced_at(&self, start, close: u8) -> Result<usize, CssFactError>` at `:693`; **0** `sink.*`/`push_leaf`/`.emit` calls in the body (emits NOTHING — grep-proven this pass) | CONFIRMED |
| CSS 94.1% hot leaf | `find_component_delim` | `fn find_component_delim(` at `:657` | CONFIRMED |
| JSON `{}`/`[]` is parse-with-emit | `generated.rs:833-834` | `b'{' => parse_object_direct(…, sink)` / `b'[' => parse_array_direct(…, sink)` at `:833-834` (identical dispatch also at `:793-794` and `:873-874` — all carry `sink`) | CONFIRMED |
| Sheets `paren_expr` descends | `google-sheets.bbnf:137` | `paren_expr = "(" , expression ?w , ")" ;` at `:137` (descends into `expression`); `expression = comparison_expr` `:163` | CONFIRMED |
| 7-level precedence tower | `google-sheets.bbnf:103-121` | `comparison_expr(:103)→concat_expr(:105)→add_expr(:109)→mul_expr(:113)→exp_expr(:115)→unary_expr(:119)→postfix_expr(:121)→primary`, each `A = B (op B)*` | CONFIRMED |
| `acceleration_at_admission` enum domain | §0.3 / §0.4 / §8 | `{admission|dead}` two-value, IDENTICAL at all three (§0.4 line 252 explicitly forbids a third state — "non-deterministic between the schema and the G6 falsifier") | CONFIRMED |
| `sheets_grammar_shape` enum domain | §0.4 / §9 | `{pratt-operator|flat-stream|tree|courier|hollow}` declared §0.4:253; gate keys ENTIRELY on the ≥7-chained-level-fn structural falsifier (§9:1468), NOT the Nu8 count | CONFIRMED |
| `generator_grammar_count == 3` anti-inflation | §0.1#1 / §0.4 / §3.3 / §9 / §close | json+css+sheets (NOT json+7-css+sheets) at every site; consistent | CONFIRMED |

Every spine anchor the OVERFIT-PRUNE lens leans on is live at the cited path:line. The
neutrality-discharge is GROUNDED, not asserted: the CSS shell at `:693-713` emits nothing (grep-proven
0 emit calls THIS pass), and the two offered non-CSS dischargers (JSON `:833-834`
parse_object/array_direct WITH `sink`; Sheets `:137` descending into `expression`) are both
parse-with-emit descents — so "invoke the SAME primitive" is structurally UNREACHABLE and the demotion
to `css_balanced_component_scan` is the FORCED, not contingent, outcome (s6/C4). On disk, exactly as
the SPEC states.

---

## Enumeration under the OVERFIT-PRUNE lens — every wave-gate / telemetry / close claim

### A. ONE-generator framing, end to end

1. **`generator_grammar_count == 3` = json+css+sheets, NOT json+7-css+sheets (R-A0-2, the P3
   anti-inflation invariant).** Bound at §0.1#1, §0.4 (MUST-be-3-at-PROVE; 7-css inflation = REJECT),
   §3.3 P3 (collapse-to-one is the DISK-EVIDENCE default — one `stylesheet.bbnf`, byte-identical output;
   fake-`.bbnf`-root minting explicitly forbidden as "the EXACT overfit addendum 2 forbids"), §9 PROVE
   (the THIRD genuine grammar; the `grammar_name="google_sheets"` row collapses to itself under the R16
   full-row `PartialEq`, count==1 per `grammar_name`), §11 ledger, and the §close restatement.
   Internally consistent across all sites; falsifiable and anti-inflation. **ACCEPT.**

2. **One un-forked emitter on the lowered `BackendShape`, not a grammar tag (close-cond #2 / G3
   five-conjunct).** `emitter_fork_present==false ∧ generator_grammar_branch_count==0 ∧
   generator_grammar_type_count==0 ∧ runtime_target_rows_collapsed==true ∧
   emit_shape_source==lowered_program` (§0.1#2, §6 G3.2, §0.4). The fifth conjunct
   (`emit_shape_source==lowered_program`, G3.2 §5, lines 1114-1121) is the load-bearing defence against
   the §5-risk-1 relocated seam; the SPEC names it the binding distinction between an honest un-fork and
   a paper-close that relocates the fork into data, and pins the grep field-set
   (`target.profile`/`target.emitter`/`target.output_labels`/`target.profile_contract`/`contract.emitter`)
   IDENTICAL between the G3 exit conjunct 5 (line 1118) and the standing §1 addendum-3 + §2.1 seam-scan
   (lines 342, 501) — so the standing scan is provably as strong as G3's own. Disk-verified: the
   `BackendShape` discriminator (`lower/mod.rs:20-24`) and the `RuntimeEmitterKind` fork
   (`grammar_provider.rs:40-42`) are both real. **ACCEPT.**

3. **The relocated-seam structural co-gate (R16, the ONLY check the arm-grep cannot see).**
   `runtime_target_rows_collapsed==true` via `RuntimeTarget: PartialEq` full-row over BOTH nested
   structs. §3.3 (lines 668-679) correctly splits the TWO distinct uses and forbids conflating them:
   (i) the relocated-seam check = FULL-ROW `PartialEq` over EVERY field with NO exclusion (incl. BOTH
   `frontend_requirements` #11 AND `output_labels` #12); (ii) the row-collapse count = the SEPARATE
   projected count excluding ONLY the two artefact-path columns `output_dir`/`expected_files` — and the
   SPEC explicitly states those two are excluded from THIS count only, NOT from the full-row seam-check.
   The "recurses into ONE nested struct" recipe is named the EXACT shallow-compare false-green and
   FORBIDDEN. Disk confirms `regen.rs:5` is `Clone, Copy, Debug` (no PartialEq), so the +1-line pin is
   the real mechanism. **ACCEPT.**

4. **`verbatim_blob_present == false` campaign-wide (the L1 banner-over-blob REJECT).** Bound at
   §0.1#1/#6, §0.4, §1 addendum 1, G1.2#4, G2.2#1, the (c) arm of every named-primitive contract, the
   §11 ledger. The CSS-courier ≈910 LOC is correctly declared cohort-carried, NOT gate-keyed on the
   exact figure (§1 addendum 1) — the boolean + the `.bbnf`-mutation test is the binding gate, so no
   overfit-to-a-magic-LOC-number creeps in. **ACCEPT.**

### B. The named primitives — grammar-parameterized + (a)-(d) bounded

5. **The §6 (a)-(d) escape gate (the single largest paper-close surface, R-A0-3).** §1 (lines 357-374)
   states all four conjuncts: (a) grammar-INVOKED-by-name, (b) emitted-output-VARIES-under-rule-mutation
   (the BYTE-SET/numeric-class mutation, the discriminator between derived and relabeled), (c)
   `verbatim_blob_present==false`, (d) PROFILE-PROVEN-NARROW-LEAF (primitive LOC ≤ profiled hot-leaf
   extent; skeleton walk-derived). (d) is the size-bound that stops an arbitrarily-large relabeled blob
   that merely varies under mutation — machine-checked, not prose: G1.4 emits `g1_leaf_primitive_loc` /
   `g1_leaf_primitive_profiled_leaf_extent` and the gate REJECTs on `loc > extent`; G2.5 mirrors with
   `g2_balanced_scan_primitive_loc` / `g2_balanced_scan_profiled_leaf_extent`. The (b) and (d) arms are
   what make the escape un-gameable. **ACCEPT.**

6. **`css_balanced_component_scan` — FORCED CSS-scoped demotion (s6/C4, the CH6-specific obligation).**
   This is the heart of my lens. §1 (lines 381-393) and §5 G2.2#5 / G2.3 / G2.5 state the demotion is
   FORCED, not contingent: the GROUND re-validation grounded BOTH offered non-CSS dischargers on disk and
   found them structurally incompatible with the CSS byte-SKIP shell. I independently re-grounded all
   three on disk THIS pass: the CSS shell `consume_balanced_at` (`:693-713`) emits NOTHING (0 emit calls,
   grep-proven); JSON `{}`/`[]` (`:793-794`/`:833-834`/`:873-874`) dispatches
   `parse_object_direct`/`parse_array_direct` WITH `sink`; Sheets `paren_expr` (`:137`) descends into
   `expression`. The two dischargers are parse-with-emit; the CSS shell is byte-skip. "Invoke the SAME
   primitive" is therefore UNREACHABLE by the named candidates, and the CSS-scoped name is correctly the
   FORCED outcome. The gate (`g2_balanced_scan_neutrality_discharged`) is discharged by the CSS-scoped
   rename, NOT a fabricated cross-grammar caller — the SPEC explicitly states "the gate REJECTS a NEUTRAL
   name with zero structurally-compatible non-CSS caller" (lines 393, 978). The inner alphabet-scan
   sub-kernel (the `bbnf-simd` eq-set member scan) remains genuinely neutral caller-data; only the
   balanced-recognizer SHELL is CSS-scoped. A neutrally-named CSS-only primitive would be an
   overfit-in-waiting, and the SPEC refuses it. **ACCEPT.**

7. **The JSON `string`/`number` leaf scanners (`decode_json_string_to_arena`, `parse_number_*`).** §4
   G1.3 (lines 856-871) binds each to the per-primitive (a)-(d) contract; (b) is specifically the
   digit-class widening falsifier (widen the `number` rule's digit class in `.bbnf` ⇒ the `b'0'..=b'9'`
   literal widens — a kernel that does not vary under its own rule's class mutation is a relabeled fixed
   courier). The §11 ledger pre-blocks "a relabeled fragment as a leaf primitive (fails (b)/(d))". The
   structural SKELETON (dispatch match, container loops, the 3 sink-prefix variants) is walk-derived;
   only the proven-hot inner LEAF kernels are gated primitives. The "do NOT LCD-unify the dispatch
   triple" instruction (G1.2#3, G1.3) protects the monomorphized-sink leaf — anti-flatten discipline
   applied to JSON's OWN structure, not an overfit. **ACCEPT.**

8. **The named-primitive roster is bounded, not open.** The campaign admits exactly:
   `css_balanced_component_scan` (CSS 94.1% leaf, G2/G6), `decode_json_string_to_arena` /
   `parse_number_*` (91.5% leaf, G1). §1 line 378-380. Each carries its own (a)-(d) contract +
   `*_abcd_pass` gate arm + the (d) LOC numerator/denominator telemetry. No primitive spans a rule body
   or an unprofiled region without a REJECT (the (d) god-kernel arm). **ACCEPT.**

9. **G6 NEON onto the CSS scan shell — SHARED grammar-neutral primitive, not per-grammar vector code
   (close-cond #10 / §8).** `acceleration_at_admission==admission` requires BOTH conjuncts: the
   `generated.rs` caller census (NOT `#[cfg(test)]`) AND `simd_admission_profile_sampled==true` (the
   runtime-reachability proof — a samply re-sample attributing self-time to the `runtime_simd` entry, so
   a `generated.rs` call site in dead/unreachable code that the census sees but the profile does NOT
   == dead). `significant_set_is_caller_data==true` + `css_scan_call_site_singular==true` (the
   P3-collapsed single scan; re-emit-7-ways == FAIL). The kernel is hand-authored ONCE in `bbnf-simd`;
   the generator emits a CALL, not vector code per grammar. The enum is the SAME two-value `{admission|
   dead}` domain across §0.3/§0.4/§8 (re-verified this pass; a third state would make the gate
   non-deterministic — the SPEC names this explicitly at line 252). No JSON classifier authored (G5
   retires the zero-sampled `json/scan.rs`; outcome `N` — nothing on product path; `simd_non_json_
   exercise=css_l4` because S-P1 has NO JSON G5 hot leaf, honest not a dodge). **ACCEPT.**

### C. Sheets — a genuine negative control (not a third-JSON, not a courier)

10. **The precedence tower is the SOLE Sheets-distinctive construct; `Nu8`-tagged-alt is SHARED, not the
    litmus.** §9 (lines 1421-1424) demotes the `Nu8`-tagged-alt family from the litmus (CSS uses it 295×
    vs Sheets 21× across the import closure, so the generator must already handle it at scale to emit CSS
    at all). The 295×/21× figures are S-P2-research-derived expanded-import-closure counts; CRITICALLY,
    NO gate/telemetry/close predicate keys on the figure — the `sheets_grammar_shape==pratt-operator`
    gate keys ENTIRELY on the ≥7-chained-level-fn STRUCTURAL falsifier (§9:1461-1468), and the Nu8 count
    is directional demotion-justification prose only. The load-bearing demotion claim ("precedence tower
    is the SOLE distinctive construct") is independently disk-grounded: I confirmed the 7-level chain at
    `google-sheets.bbnf:103-121` this pass. An implementer never tests the 295×/21× value. Per
    proportionality this is NOT misleading → **ACCEPT** (consistent with V6/V7).

11. **`sheets_grammar_shape == pratt-operator` proven by a CONCRETE STRUCTURAL falsifier, NOT "by
    construction".** §9 (lines 1461-1468) machine-checks it: count the emitted per-level descent fns
    (≥7 non-terminal levels each calling the next) AND the cyclic `paren_expr→expression` back-edge; a
    `flat-stream` (the R-E-3 flattened-tower REJECT predicate) or a `tree` (single recursive value match)
    emission has FEWER than 7 chained level fns and FAILS the count. This is the anti-"third-JSON-hollow-
    litmus" teeth — R-E-3 is REJECTED outright (§9:1424, §11 ledger). The enum domain is CLOSED (§0.4).
    Disk confirms the 7-level tower is real, so the falsifier is satisfiable on a true emission and fails
    on a hollow one. **ACCEPT.**

12. **The BINDING negative-control fallback — `N`, never paper-close, never stub-prove.** §0.1#9, §0.3
    (the `N` outcome), §9 falsifiers + binding fallback (lines 1485-1495), §11 ledger, §close. If Sheets
    cannot emit via the generator ONLY, generalization is NOT real — surfaced honestly as
    `sheets_emission_path == shim` → outcome `N`, NEVER a `const SHEETS_GENERATED_RS` courier, NEVER a
    `GoogleSheets =>` arm. The §0.3 enum cleanly separates `N` (negative-control fail / generalization
    unreal) from `L` (honest non-Sheets residual loss) from `S` (admission-capable non-SOTA residual) —
    no outcome-class overlap that could let a shim slip through as a soft pass. The gate explicitly does
    NOT paper-close a shim as a pass (§9:1519). The control can FAIL, and failure has a binding honest
    disposition — the genuine-negative-control property. **ACCEPT.**

13. **Sheets instantiates the G4 trait without LCD-flattening (PROVE entry on G4 DIRECT, seq/C6).**
    `sheets_value_instantiates_g4_trait==true`, PROVEN by a concrete falsifier (`rg 'impl (Cursor|
    DocumentView) ... for' grammars/sheets/` NON-EMPTY AND the crate compiles, isomorphic to the
    G4.2-conjunct-3 substitution falsifier — §9:1480, 1508), NOT asserted. PROVE entry-gates on
    `G3 ∧ G4` with G4 a DIRECT (not merely transitive) conjunct — the seq/C6 correction is folded at §2
    manifest, §2.1 lattice (lines 544-546), and §9 entry-gate ("PROVE NEVER admits before G4 closes",
    line 1428). The third trait impl makes the phantom-`<G>` resolution concrete. **ACCEPT.**

14. **`import_closure_relaxation_is_data == true` — DATA change, NOT a `match grammar` arm.** §0.1#9,
    §2.1 (template/provider boundary), §9 task 3 (lines 1448-1452), §11 ledger. The relaxation is
    `present-iff-grammar-has-imports`, DERIVED from grammar facts (a `RuntimeFrontendRequirements` data
    change), explicitly NOT a `match grammar { GoogleSheets => … }` arm. The SPEC enforces this with the
    `generator_grammar_branch_count==0` arm-census including the `(GoogleSheets|Sheets)\w*\s*=>` regex.
    Routing the relaxation as a per-grammar branch would be the EXACT overfit; the §11 ledger pre-blocks
    "an `import_closure` `match grammar` arm". **ACCEPT.**

### D. Nothing overfit to JSON/CSS — the architectural regressors EXCLUDED

15. **The full grammar-IR tree-walk REJECTED (R-B/R-C Candidate C).** §5 (the deep finding, lines
    924-933) names it: a naive grammar-walk lowering produces the combinator-shaped recursive descent
    = lightningcss's own architecture, which categorically regresses >SOTA. REJECTED outright at §5, §1
    (R-B Candidate C), and re-listed in the §11 G2 ledger. The hybrid (named primitive CORE + fact-keyed
    `push_str` projection blocks) derives the scan from grammar-supplied alphabet/structural-byte/branch-
    tag/entry facts — grammar-DERIVED, not overfit. **ACCEPT.**

16. **The forced-common `Value` shape REJECTED (R-D Candidate B — LCD-flatten).** §7 G4 + §5-risk-4.
    The `Cursor` micro-trait shares ONLY the laziness/cursor contract, NEVER navigation. JSON's rich tree
    is preserved by a CONCRETE FALSIFIER, not "by construction": §0.1#4 and §7 G4.2#2 bind
    `json_rich_navigation_preserved==true` to the byte-equal diff — the SPEC explicitly calls "asserted
    by construction" the unfalsifiable-gate hazard and REJECTs it (§0.1#4 lines 90-97). The ≥2-impl count
    is made necessary-NOT-sufficient by `shared_trait_non_collapsible` (the substitution falsifier: swap
    JSON's nav impl for CSS's ⇒ compile FAILS; a degenerate-equal CSS impl COMPILES ⇒ REJECT, G4.2#3).
    The strongest possible anti-LCD-flatten teeth — preserve-rich-ast holds on the trait axis. **ACCEPT.**

17. **preserve-rich-ast on the bench axis (lazy `track1_rich`, never flattened) + the same-run ratio
    gate is FALSIFIABLE, not unfalsifiable, and not overfit-to-CSS.** §0.2 framing, §1 (preserve-rich-ast
    non-negotiable), G2.2#4, G3.2#8, H1 §10. `track1_rich` stays lazy `ValueRef`-view projection
    re-derived from `(source,offset)` spans, writing NOTHING to the arena. I scrutinized the same-run
    `track1_rich/lightningcss > 1.0×` gate this pass specifically for unfalsifiability (a REJECT trigger
    under my lens): the pre-G2 baseline is CAPTURED AT G2 ENTRY in one quiet run, the regression falsifier
    FIRES at G2 exit, H1 re-confirms only DIRECTIONALLY (the pre-G2 code is gone post-G2 so it is never
    re-measured), and PROVE preserves CSS by BYTE-EQUALITY not a fresh re-bench (§9:1511) — self-consistent
    across §0.1#6, §0.2, §0.4:265, §0.5, G2:963-969, G3:1165, H1:1568. The gate turns RED on a same-run
    ratio ≤ 1.0× OR a same-run regression — falsifiable. The S-P1 absolutes are DIRECTIONAL antecedents,
    NOT the floor (keying on an un-re-locked absolute is the unfalsifiable hazard the same-run comparison
    REPLACES). The H1 disclosure `materialization_framing==lazy-rich-vs-eager-cssom` (closed two-value
    enum) forbids the unqualified "beats CSSOM" re-label (R-A0-1/R14). The recognition-only
    `track1_full_parse` `A` does NOT by itself discharge the typed close (§0.2, §0.3:230) — a flattened
    recognizer cannot masquerade as the rich-AST win. **ACCEPT.**

18. **Lock-14 scans the codegen AS authored (P4 before G2/G3) — no overfit re-introduced under a blind
    gate.** §0.1#8, §3.4 P4, §2.1. P4's `FORBIDDEN_GENERIC_TOKENS` extension is precisely scoped: the
    `_RS` token is the suffix `GENERATED_RS` (catching `CSS_GENERATED_RS` AND `JSON_PARSE_ONLY_
    GENERATED_RS`, the two grammar-body couriers) so the six surviving MOD/HOST/PARSER/SINK scaffold
    consts in the now-strict-scanned `runtime_generator.rs` do NOT false-RED under the `source.contains`
    substring semantics (§3.4:711-717) — a proportionate token scoping that prevents the gate being
    either too weak (green-by-exclusion) or too strong (false-RED on legitimate scaffold). The re-inject
    falsifier (a `GENERATED_RS`-bearing or `EventGrammar` token, NOT a bare `JsonSink` which is NOT in the
    set) is correctly specified; P4's rerun-ceiling table (line 474) matches. **ACCEPT.**

19. **The sequencing preserves the spine (S-P2 §3 lattice corrected, not violated).** The SPEC's §2.1
    lattice (lines 538-547) folds the seq/C6 (PROVE never admits before G4 closes — G4 is PROVE's DIRECT,
    not merely transitive, predecessor) and seq/C7 (G5/G6 hangs off G3 PARALLEL to G4, NOT under G4)
    corrections. I cross-checked against S-P2 §3 (which still renders PROVE under G4 in its ASCII diagram
    at line 140 with the "PARALLEL to G5/G6" annotation, and the prose conjunct "PROVE entry-gates on
    G3 ∧ G4"). The SPEC §2.1 + §8 entry + §9 entry correctly render G5/G6 and PROVE as TWO parallel
    branches off G3 joining at H1, with PROVE's G4 conjunct explicit. This is the corrected, more-precise
    rendering of the SAME dependency content — no broken sequence, no entry-gate that lets a downstream
    wave dispatch over a REDRESSed predecessor, no addenda §5 violation. **ACCEPT.**

---

## Residual-REVISE scan (proportionality applied — REVISE only if it would mislead an implementer)

- **§9 line 1422 "`-> Nu8u8` 295× … vs Sheets' 21×".** Re-examined independently (V6/V7 both raised and
  ACCEPTed). NO gate/telemetry/close predicate keys on the figure; it is directional demotion-
  justification prose, and the load-bearing demotion claim is structurally disk-grounded (the 7-level
  tower IS the litmus, machine-checked by the ≥7-chained-level-fn count). An implementer never tests the
  295×/21× value. NOT misleading → ACCEPT, not REVISE. (Out of the OVERFIT-PRUNE lens's load-bearing
  surface; the count is decoration, the structural falsifier is the gate.)

- **JSON neutrality-discharge citation `generated.rs:833-834` (§1:388, §5:973-974).** Disk shows the
  IDENTICAL parse-with-emit dispatch at THREE sites (`:793-794`, `:833-834`, `:873-874`). The SPEC cites
  `:833-834`; all three are equally valid witnesses of the same property (parse_object/array_direct WITH
  `sink`). Picking one of three equivalent occurrences is not a defect and does not mislead — the
  property (parse-with-emit, structurally incompatible with the byte-SKIP CSS shell) holds at every one.
  No REVISE.

- **Net LOC ≈ −10800 vs per-wave sum (§2 manifest: P1 −4500, P2 −700, P3 −5500, P4 +15, G-waves net ≈0,
  Sheets ≈+200).** The "≈" framing is honest and each per-wave figure carries its own "≈". This is a
  CH4/CH7 arithmetic-honesty matter, NOT an OVERFIT-PRUNE-lens claim — no overfit hides in the LOC total.
  Out of my lens; no REVISE.

No residual REVISE survives under the OVERFIT-PRUNE lens. Independently re-derived (not rubber-stamped
from V6/V7): the ONE-generator framing is preserved end to end and grammar-DERIVED at every emit site;
the named-primitive roster is bounded, grammar-parameterized, and (a)-(d)-bounded with machine-checked
LOC-vs-profiled-extent size gates; the CSS balanced-scan is FORCED-demoted to
`css_balanced_component_scan` with the neutrality obligation discharged by the honest CSS-scoped name
(disk-grounded THIS pass: the CSS shell `:693-713` emits nothing, the two non-CSS dischargers
`:793-794`/`:833-834`/`:873-874` and `:137` are parse-with-emit, so the SAME-primitive branch is
structurally unreachable — not fabricated); Sheets is a genuine negative control with a structural (not
by-construction) ≥7-chained-level-fn shape falsifier, a concrete trait-instantiation falsifier, and a
binding `N` fallback that can FAIL honestly; nothing is overfit to JSON or CSS — the two architectural
regressors (full grammar-IR tree-walk; forced-common `Value` shape) are excluded and re-listed in the
§11 ledger, preserve-rich-ast holds on both the trait axis (the substitution falsifier) and the bench
axis (lazy `track1_rich`, with the same-run ratio gate verified FALSIFIABLE this pass), Lock-14 scans
the codegen as authored with proportionately-scoped forbidden tokens, and the relocated-seam structural
co-gate (R16 full-row `PartialEq` over BOTH nested structs) catches the one seam the arm-grep cannot.
All three gate enums (`acceleration_at_admission`, `sheets_grammar_shape`, the `materialization_framing`
closed enum) have a consistent closed domain — no non-deterministic gate. No unfalsifiable gate, no
broken sequence, no addenda violation surfaces under this lens. V6, V7, and V8 all reject=0 revise=0 —
the 2-consecutive-clean fixed point is met and re-confirmed under CH6.

TALLY accept=19 revise=0 reject=0
