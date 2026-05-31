# CH2 — GENERALITY (V4)

Lens: CH2 Generality (PASS-ALPHA §3 / SK-V18-GENERALIZATION-HANDOFF §4). Reviewer focus:
**does the goalset respect Lock 14 (one generator ALL grammars); are the interventions
grammar-neutral; will they work for non-JSON grammars (CSS L4 / GoogleSheets / BBNF-self); is the
GoogleSheets 3rd-grammar proof load-bearing?** Subject: SK-V18 = THE GENERALIZATION CYCLE (the
inflection backtrack — fork the hand-written/forked parsers BACK into ONE grammar-driven generator
emitting all grammars from `.bbnf`, over the unified tape/`ValueRef` substrate, shared value API,
PROVEN on a 3rd grammar (GoogleSheets), PRESERVING >SOTA). NOT a new-feature cycle.
Date 2026-05-31. Bracket HEAD `318d9c046`; SK-V17 close `f6a38445b`; V3 audit `7dbe44c22`.

Method: this is the **fourth** hardening iteration. CH2/V1 dispositioned **24A/8R/0** (75.0%) on
three cross-artefact generality folds. CH2/V2 dispositioned **30A/1R/0** (96.8%) — one residual
REVISE (the neutrality-grep alphabet). CH2/V3 (`../V3/CH2.md`) dispositioned **29A/1R/0** (96.7%):
the single V2 REVISE folded verbatim and orphan-free, ONE NEW REVISE (§8.1 / fold-id **F13**): the
F10-widened arm-census grep's *reach claim* — that root-widening to `skinny/xtask/src` means "a
per-grammar branch relocated into a neutral-identifier `RuntimeTarget`/strategy data-table cannot
escape" — is empirically false (the arm-census regex returns NO MATCH against the live neutral-
identifier `RuntimeTarget` table because such a table carries no `Json =>` arm syntax); the real
defense is the **P3 collapse close-gate** (the row-count structural check), which the contract had
but mis-attributed to the grep. The α-A..F artefacts + SYNTHESIS + HANDOFF were **re-authored after
V3** (alpha mtimes 14:18–14:22 vs V3/CH2 14:12) carrying explicit V3→V4 FOLD ledgers (αE F13/F14,
SYNTHESIS §8 (2), HANDOFF inv.5, αC V4-FOLD-2). V4's job: verify the single V3 REVISE (F13) folded
orphan-free, re-disposition every section live at HEAD, and probe one level deeper for the NEXT
generality gap. Every disposition cites `path:line` / SHA / artefact-line, verified live where
checkable.

---

## §0 — Lens verdict (one paragraph)

**The single V3/CH2 §8.1 REVISE (F13) folded verbatim, orphan-free, and is now bound to a dedicated
machine-checkable telemetry gate; the goalset's Lock-14 spine is correct and the GoogleSheets
3rd-grammar proof is genuinely load-bearing.** Verified live at HEAD `318d9c046`: (1) the
arm-census regex empirically returns **0** over `skinny/xtask/src` and **0** over codegen
(`rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>' skinny/crates/codegen/src` → 0; `rg -nE 'Json\s*=>|CssL4\s*=>|(GoogleSheets|Sheets)\w*\s*=>|Bbnf\w*\s*=>' skinny/xtask/src` → 0) — confirming V3's refutation that the regex is syntactically incapable of firing on a neutral-identifier
`RuntimeTarget` data-table; (2) the F13 fold corrected the over-claim across ALL five+ sites
(SYNTHESIS:37,:80–91,:201/G3,:328,:480 `generator_grammar_branch_count`,:520; HANDOFF:19,:260,:310;
αE F13/cross-cutting-5/:61/:116/:145/:196; αC V4-FOLD-2 :393–401) and — the key V4 improvement —
PROMOTED the structural defense to a first-class REJECT-bound telemetry column
`runtime_target_rows_collapsed` (`SYNTHESIS:481`, "must be `true`", yes for P3+G3); (3) the
real GoogleSheets source `grammar/google-sheets/google-sheets.bbnf` is confirmed present (185 LOC,
`error_literal = "#N/A" -> 0u8` `:34`, `cell_ref = /\$?[A-Za-z]{1,3}\$?\d+/` `:63`, "operator
precedence tower" `:92` — a Pratt formula grammar no JSON/CSS rule exercises), absent from the
benched skinny tree (`find skinny -name '*.bbnf'` → only `json.bbnf`), with skinny-tree adoption an
explicit PROVE owner obligation; (4) LOCKS.md item 14 canonical alphabet `Json|CssL4|Bbnf|GoogleSheets`
with the exact verification greps the contract cites verified verbatim at `restart/locks/LOCKS.md:349`.
**Zero REJECTs. One NEW REVISE** (§8): the F13 fix moved the relocated-seam defense from the regex
(correct) to the `runtime_target_rows_collapsed` row-count check **projected onto `(source_roots,
entry_rule)`** — but I empirically falsified that this projection covers the threat. The live 7
css_l4 `RuntimeTarget` rows are identical on `(source_roots, entry_rule)` (`sort -u` → **1**, gate
GREEN) yet carry **7 distinct** `fact_schema`/`row_id`/`output_plane`/`output_dir` values
(`sort -u` over `fact_schema` → **7**). A relocated per-grammar branch riding those varying columns
passes `runtime_target_rows_collapsed == true` green. The contract's own framing — αC:399 "a
data-table that carries N **collapsed-identical** rows" — is empirically false: the rows are NOT
identical, they diverge in 5 of ~13 columns, and the gate projects onto exactly the two columns
that happen to be invariant. This is the SAME necessary-not-sufficient lineage as V1→V2→V3, carried
one level deeper INTO the F13 fix itself: V3 correctly moved the defense from the syntactically-
incapable regex to the structural row-count check; V4 finds the structural check projects onto the
wrong (invariant) column tuple. REVISE not REJECT: the P3 collapse remains the right mechanism, and
the fix is concrete (project onto the full per-grammar config tuple modulo the generated
`output_dir`, OR assert all rows for one `grammar_name` are byte-identical except `output_dir`).

---

## §1 — αA results-extraction (generality lens)

### §1.1 — substrate-generalizes / value-API-does-not split → **ACCEPT** (V3 ACCEPT held)
`alphaA` substrate table. The split — substrate (Lock 1) generalizes + is the foundation;
value-API + codegen do NOT yet — verified live: one `Tape`/`ValueRef`/`PayloadArena`
(`tape/mod.rs:175`), both grammars ride it, no second tape. "Substrate union VALIDATED (the genuine
foundation)." Unchanged. ACCEPT.

### §1.2 — phantom `G`-axis-not-`K`-axis precision → **ACCEPT** (V3 ACCEPT held; creation-not-rename)
`alphaA` phantom-G row. Verified live `tape/mod.rs:175`: `ValueRef<'doc,'input:'doc, K = AnyKind,
G: EventGrammar = AnyGrammar>` — TWO defaulted axes; `K=Kind` is real, `G: EventGrammar` is the
phantom (always `AnyGrammar`). G4 must instantiate-or-delete the RIGHT axis (`G`). The V3 refinement
holds: `CssEventGrammar` does NOT exist at HEAD (`grep` → `SheetsEventGrammar` + `JsonEventGrammar`
only) — so G4's CSS-side INSTANTIATE is creation, DELETE is the abrogate-before-patch default.
Accurate. ACCEPT.

### §1.3 — GoogleSheets close-condition seed (source + skinny-tree obligation) → **ACCEPT** (V3 ACCEPT held)
`alphaA` §"PROVE Sheets". Names `grammar/google-sheets/google-sheets.bbnf`, flags it lives in
totality (verified: skinny tree has only `skinny/grammars/json.bbnf`), states the PROVE wave's
first obligation is bringing the `.bbnf` into the benched tree. ACCEPT.

**αA tally: ACCEPT ×3, REVISE ×0.**

---

## §2 — αB competitor-deltas (generality lens)

### §2.1 — GoogleSheets-as-GENERATION-not-throughput bar → **ACCEPT** (V3 ACCEPT held)
`alphaB:279,283,289–293`. The three-grammar bar table holds: JSON strict-vs-strict, CSS lazy-vs-eager
(framed), **GoogleSheets has NO competitor bar — its bar is GENERATION** (the ONE generator emits a
real GoogleSheets parser from `.bbnf` with a non-identical `generated.rs`; 25-LOC stub retired). The
V2→V3 canonical "Sheets" → "GoogleSheets" rename per `LOCKS.md:349` is held. Correct generality
framing (no fabricated speed win). ACCEPT.

### §2.2 — typed-rows-conditional + CSS LOW-lowering-risk → **ACCEPT** (V3 ACCEPT held)
The typed JSON bar rides a per-corpus hand-tuned schema that does NOT generalize; `parse_only` is
the unconditional grammar-general bar. CSS >SOTA does NOT depend on hand-shaping (scalar hot path,
no fragile kernel) — verified: CSS scan kernels `find_css_significant`/`find_comment_close` live in
`runtime_simd.rs` (not the hot admission path) — so the G2 lowering rebuild is LOW risk for
generality. ACCEPT.

**αB tally: ACCEPT ×2, REVISE ×0.**

---

## §3 — αC redress-digest (generality lens)

### §3.1 — LayoutFacts-derive-not-hardcode (the Lock-14 generality vehicle) → **ACCEPT** (V3 ACCEPT held)
The emitter DERIVES tape ops from `LayoutFacts.backend_shape ∈ {EagerTape,OffsetTape,EventTape,
SinkOnly,CollapsedStage}` (`lower/*.rs` present). The single most important Lock-14 generality
vehicle. Unchanged. ACCEPT.

### §3.2 — relocated-overfit-seam pre-block (clause) → **ACCEPT** (V3 ACCEPT held; clause correct, enforcement is §8)
`alphaC` §2.2 / SYNTHESIS:274. The pre-block names the EXACT Lock-14 failure mode and is correct as
a *prose* obligation ("every residual CSS routing entry names the `.bbnf` rule it derives from").
The clause itself ACCEPTs; the *machine-check* attributed to enforce it is the §8 finding. ACCEPT
(clause).

### §3.3 — P3 collapse-vs-differentiate + V4-FOLD-2 structural enforcement → **ACCEPT-with-§8-sharpening** (V3 ACCEPT; the row-count check projects onto the wrong tuple)
`alphaC:147–162,388–401`. COLLAPSE-to-ONE is the DEFAULT (the 7 directories are demonstrably one
grammar — one `.bbnf`, one entry rule `stylesheet`, verified all 7 `RuntimeTarget` rows carry
`entry_rule:"stylesheet"` + `source_roots: CSS_L4_ROOTS`). The V4-FOLD-2 addendum (`alphaC:393–401`)
correctly relocates the F13 enforcement: "the PRIMARY enforcement is the **P3 structural row-count
collapse** … a data-table that carries N collapsed-identical rows for one `.bbnf` IS the relocated
seam, and the row-count check catches it where the grep cannot. The grep is NECESSARY-NOT-SUFFICIENT."
The MECHANISM (P3 collapse) is the right answer to V3 §8.1 — this is the load-bearing correction.
**But** the close gate as written (`sort -u` over `(source_roots, entry_rule)`) projects onto the
two invariant columns and the framing "N **collapsed-identical** rows" is empirically false (§8).
ACCEPT (mechanism); see §8 (the projection tuple). The §8 fix is a sharpening of THIS gate's
projection, not a refutation of the mechanism.

### §3.4 — retirement clause + EventGrammar witness-type seam → **ACCEPT** (V3 ACCEPT held)
The retirement clause binds BOTH `CSS_GENERATED_RS` (verbatim-blob) AND `RuntimeEmitterKind`
(single-emitter-path); verified live the fork exists (`grammar_provider.rs:40–42`) and the verbatim
blobs exist (`runtime_generator.rs` `CSS_GENERATED_RS`/`JSON_*_RS`). The `EventGrammar`-type seam
addendum is generality-positive. ACCEPT.

**αC tally: ACCEPT ×4, REVISE ×0.** (The row-count-projection sharpening is filed once at §8 to avoid
double-counting; the αC *mechanism* itself ACCEPTs.)

---

## §4 — αD validated/invalidated (generality lens)

### §4.1 — DM2 substrate-READY-not-proven → **ACCEPT** (V3 ACCEPT held)
`alphaD:118`. DM2 DEMOTED to "substrate-READY, not proven" — `sheets_witness/` is a ~25-line
`EventGrammar` stub (verified live `SheetsEventGrammar` type-level witness only). The honest
demotion of the generalization CLAIM. ACCEPT.

### §4.2 — I3/I4/I5 the three generality invalidations → **ACCEPT** (V3 ACCEPT held)
I3 (7 replicas — byte-identical at HEAD), I4 (`RuntimeEmitterKind` fork), I5 (phantom `<G>` +
divergent value API). Each names its CH2 lens. Unchanged. ACCEPT.

### §4.3 — S12 GoogleSheets-litmus owner-surface → **ACCEPT** (V3 ACCEPT held; F14 stale-count is αD-internal, not generality)
`alphaD:143,203,207,261`. S12 names `grammar/google-sheets/google-sheets.bbnf` (185-LOC Pratt
formula grammar), cites its distinguishing shapes (`error_literal :34`, precedence tower `:92`,
`cell_ref :63` — re-verified live), and carries the honest-finding clause. The F14 REVISE (the stale
checkasm "18" at `alphaD:85`) is a CH1/CH7 count-correctness fix, αD-internal, with **zero generality
content** — αE is the count-correct reference (12+2) and does not inherit it; CH2 confirms it does not
leak into any generality claim. The S12 generality substance is clean. ACCEPT.

### §4.4 — no-second-substrate pre-block (G4) → **ACCEPT** (V3 ACCEPT held)
The "no second substrate" clause (`StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside
`Tape`/`ValueRef` is a Lock 1 violation) protects generality. ACCEPT.

**αD tally: ACCEPT ×4, REVISE ×0.**

---

## §5 — αE candidate-shortlist (generality lens — the load-bearing artefact)

αE carries the most explicit V3→V4 FOLD LEDGER (`alphaE:12–21`, F13/F14) + cross-cutting note 5
(`alphaE:226`). The CH2-relevant fold is F13.

### §5.1 — falsifiability triple (PRESERVED->SOTA / DERIVATION-PROOF / DISTINCT-OUTPUT) → **ACCEPT** (V3 ACCEPT held)
`alphaE:48–61`. The triple holds; DISTINCT-GRAMMAR-OUTPUT now carries F3 (md5-necessary-not-
sufficient) + F10 (canonical alphabet + widened roots + type census) + **F13 (the arm census catches
a self-disclosing token, the relocated-into-data-table seam is the P3 row-count check)**. The
mutation-test falsifier is an excellent operational discriminator. ACCEPT (the row-count tuple is §8).

### §5.2 — B1 un-fork + JSON projection → **ACCEPT** (V3 ACCEPT held)
`alphaE:116`. SINGLE-EMITTER-PATH gate reads the canonical four-grammar grep + the type census +
the F13-corrected scope ("the xtask root catches a *self-disclosing grammar-token* branch relocated
into metadata; it does NOT catch a neutral-identifier strategy table — that case is covered by the
P3 `sort -u` row-count structural check"). Targets the verified defect: `json_sink_direct::render`
emits fixed literal bodies (a hand-written template, not a projection). ACCEPT.

### §5.3 — B2 CSS lowering → **ACCEPT** (V3 ACCEPT held)
`alphaE:145`. LOW risk (scalar hot path, no kernel to preserve). DISTINCT-OUTPUT gate carries the
canonical grep + F13's `sort -u` row-count check. ACCEPT (tuple is §8).

### §5.4 — B3 shared trait + phantom (DELETE-default) → **ACCEPT** (V3 ACCEPT held)
`alphaE:154`. DELETE is the abrogate-before-patch DEFAULT; INSTANTIATE is burden-of-proof because
`CssEventGrammar` does NOT exist at HEAD (verified). The preserve-rich-ast structural gate
(`json_rich_navigation_preserved`, `SYNTHESIS:484`) guards the LCD-flatten false-green. ACCEPT.

### §5.5 — B4 GoogleSheets-litmus (source named, md5-NNS, F10 alphabet, F13 row-count) → **ACCEPT** (V3 ACCEPT held; the load-bearing disposition)
`alphaE:196`. The litmus names `grammar/google-sheets/google-sheets.bbnf`, requires three md5-distinct
`generated.rs` AND the canonical grammar-neutral body grep AND the type census AND — F13 —
"the neutral-identifier-data-table threat is machine-checked by the P3 collapse structural invariant
(`sort -u` over the `RuntimeTarget` `(source_roots, entry_rule)` pairs)", with the honest-finding
clause for Pratt-lowering failure. The GoogleSheets proof is non-hollow by construction (a real
different-shape grammar exists and is adopted). ACCEPT — the `(source_roots, entry_rule)` tuple
itself is the §8 finding.

### §5.6 — CANDIDATE A (PRUNE) generality sequencing → **ACCEPT** (V3 ACCEPT held)
P4 (Lock-14 gate meaningful) entry-gates B1. Verified the gate currently excludes the leak surface:
`GENERIC_SCAN_ROOTS` (`lock14_baseline.rs:2409`) lists codegen `lib.rs`/`lower`/`grammar_profile.rs`
but NOT `runtime_generator.rs`/`grammar_provider.rs`/`json_sink_direct.rs` — confirming the P4
"exclusion holes" finding. Right generality sequencing. ACCEPT.

**αE tally: ACCEPT ×6, REVISE ×0.** (The row-count-projection finding is filed once at §8.)

---

## §6 — SYNTHESIS.md (generality lens — the goalset)

### §6.1 — §0.1 G1–G4 + PROVE close conditions → **ACCEPT** (V3 ACCEPT held; G3 now carries the F13 three-part verify)
`SYNTHESIS:253` (G3). G3's verify now reads (i) the arm census (self-disclosing-token scope honestly
stated), (ii) the type census, (iii) the RELOCATED-overfit-seam "machine-checked STRUCTURALLY by the
P3 collapse close-gate: the xtask `RuntimeTarget` table carries exactly ONE row per distinct
(`source_roots`,`entry_rule`) pair … `sort -u`". The three-surface structure is correct; the (iii)
projection tuple is §8. ACCEPT (structure).

### §6.2 — §0.4 pre-blocks → **ACCEPT** (V3 ACCEPT held)
`SYNTHESIS:274–316`. The relocated-seam prose pre-block + no-second-substrate are the right Lock-14
protections. ACCEPT (enforcement-projection at §8).

### §6.3 — §0.5 generalization litmus table → **ACCEPT** (V3 ACCEPT held)
`SYNTHESIS:318–340`. Binary-structural per-axis table with no-stub-prove fallbacks ("if Sheets cannot
be emitted via the generator only: the generalization is NOT real — surface honestly, do NOT
stub-prove"). The relocated-seam row (`:328`) now correctly cites `runtime_target_rows_collapsed` as
the structural check. ACCEPT (projection tuple §8).

### §6.4 — Lock-14 canonical three-surface model + full alphabet → **ACCEPT** (V3 ACCEPT held; re-verified live)
`SYNTHESIS:201`. Cites `LOCKS.md` item 14 verbatim; the full canonical alphabet
`Json|CssL4|GoogleSheets|Bbnf` with `GoogleSheets` un-abbreviated. I re-verified live: `LOCKS.md:349`
carries the type census `JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser` and the arm census
`Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>` exactly. The fold matches the canon. ACCEPT.

### §6.5 — Section 2 telemetry columns (the F13 binding) → **ACCEPT** (V3 column over-claim now CORRECTED)
`SYNTHESIS:480–481`. This is the **key V4 improvement** the V3 REVISE asked for and it landed: the
`generator_grammar_branch_count` column now states honestly "this catches a metadata branch that
SELF-DISCLOSES a grammar token, NOT a neutral-identifier strategy table — the relocated-into-a-data-
table seam is caught STRUCTURALLY by `runtime_target_rows_collapsed` below, not by this regex (CH2 V3
§8.1)", and a NEW dedicated column `runtime_target_rows_collapsed` (boolean, "must be `true`", yes
for P3+G3) binds the structural check as a first-class REJECT-bound gate. The V3 §8.1 over-claim is
fully corrected here. ACCEPT — and the §8 finding is that this NEW column's `(source_roots,
entry_rule)` projection is itself necessary-not-sufficient.

### §6.6 — §0.3 receiver GoogleSheets sourcing (adopt-existing-Pratt) → **ACCEPT** (V3 ACCEPT held)
The "author" → "adopt the existing Pratt grammar" inversion is held; litmus non-hollow by
construction. ACCEPT.

**SYNTHESIS tally: ACCEPT ×6, REVISE ×0.** (The row-count-projection finding is filed once at §8.)

---

## §7 — HANDOFF.md (generality lens)

### §7.1 — backlog (G1–G6 + PROVE) → **ACCEPT** (V3 ACCEPT held)
Maps each item to its V4 finding id + CH2 lens; PROVE adopts the existing Pratt grammar with the
no-stub-prove litmus. ACCEPT.

### §7.2 — six CHALLENGE addenda → **ACCEPT** (V3 ACCEPT held)
verbatim-blob / distinct-grammar-output / single-emitter-path / phantom-generic / timed-plane-symmetry
/ acceleration-wiring carried verbatim. ACCEPT.

### §7.3 — invariant 5 grammar-neutral (F13 folded) → **ACCEPT** (V3 over-claim now CORRECTED)
`HANDOFF:19,260–262,310`. Invariant 5 now reads the F13 four-surface form: (i) `GENERIC_SCAN_ROOTS`
forbidden-token scan; (ii) the arm census (self-disclosing-token scope); (iii) the grammar-named-type
census; (iv) "the STRUCTURAL relocated-seam check `runtime_target_rows_collapsed` — the xtask
`RuntimeTarget` table carries exactly ONE row per distinct (`source_roots`,`entry_rule`) pair
(`sort -u`), because the arm census (ii) is syntactically INCAPABLE of detecting a [neutral-identifier
data-table]." The V3 §8.1 "a per-grammar branch can be RELOCATED into a neutral-identifier data-table
the codegen-scoped grep misses" over-claim is replaced by the correct structural attribution. ACCEPT
(the projection tuple is §8).

### §7.4 — S-P3 wave sequencing (PRUNE→GENERALIZE→PROVE→HONESTY, P4-before-G2/G3) → **ACCEPT** (V3 ACCEPT held)
Right generality sequencing + revert dependency graph + hard caps; `runtime_target_rows_collapsed`
named as a P3/G3 entry condition (`HANDOFF:291,310`). ACCEPT.

**HANDOFF tally: ACCEPT ×4, REVISE ×0.** (The §8 finding is filed once cross-artefact.)

---

## §8 — NEW V4 generality finding (the single open REVISE)

### §8.1 — `runtime_target_rows_collapsed` projects onto the two INVARIANT columns and ignores the FIVE columns the per-profile divergence actually lives in; the gate is necessary-not-sufficient and the contract's own "N collapsed-identical rows" framing is empirically false → **REVISE** (SYNTHESIS:481,:253(iii),:328,:480; HANDOFF inv.5 (iv) :260; αE:196/:145/:226; αC:399)

**The claim.** The F13 fold (folded correctly across all sites) moved the relocated-overfit-seam
defense FROM the arm-census regex (which V3 proved is syntactically incapable) TO the P3 collapse
structural row-count check, and bound it as a REJECT-gated telemetry column:
- `SYNTHESIS:481` — `runtime_target_rows_collapsed | boolean (must be \`true\` — the xtask
  \`RuntimeTarget\` strategy table carries exactly ONE row per distinct (\`source_roots\`,\`entry_rule\`)
  pair … \`sort -u\` over \`(source_roots,entry_rule)\` shows no per-profile divergence)`.
- `SYNTHESIS:253` G3 (iii), `:328`, HANDOFF inv.5 (iv) `:260`, αE:196/:226, αC:399 all restate the
  same `(source_roots, entry_rule)` projection.
- αC:399 frames the threat detection as: "a data-table that carries N **collapsed-identical** rows for
  one `.bbnf` IS the relocated seam, and the row-count check catches it."

**The empirical refutation (live at HEAD `318d9c046`, `skinny/xtask/src/regen_css.rs`).** I ran the
exact prescribed projection AND the projection onto a sibling per-profile column:
```
# the gate's projection — the two columns the check uses:
grep -E 'source_roots:|entry_rule:' regen_css.rs | paste - - | sort -u | wc -l   →  1
# a sibling per-profile column the gate IGNORES:
grep -E 'fact_schema:' regen_css.rs | sort -u | wc -l                            →  7
```
The 7 css_l4 `RuntimeTarget` rows are byte-identical on `(source_roots, entry_rule)` — both
`CSS_L4_ROOTS` / `"stylesheet"` — so `runtime_target_rows_collapsed == true` is GREEN. But the SAME 7
rows carry **7 distinct** values in EACH of `fact_schema` (`"css-l4-at-rules-media-facts-v1"` …
`"css-l4-visual-function-facts-v1"`), `row_id` (`"css_l4/at_rules_and_media/direct_to_struct/main"`
…), `output_plane` (`"css_l4_at_rules_media_fact_stream"` …), and `output_dir`
(`"crates/runtime/src/grammars/css_l4_at_rules_and_media"` …). The `RuntimeTarget` struct
(`regen.rs:6`) has ~13 fields; the gate projects onto 2 of them — and it projects onto exactly the 2
that are invariant across all 7 rows, discarding the 5 where the per-profile divergence demonstrably
lives.

**Why this is a generality hole, not cosmetics.** The relocated-overfit-seam threat (SYNTHESIS:274,
the §0.4 pre-block) is "a per-grammar branch moved into a neutral-identifier strategy data-table."
A relocated branch does NOT need to vary `(source_roots, entry_rule)` to encode per-grammar routing
— it can ride `fact_schema` / `output_plane` / `emitter` (all per-row in the live table). Concretely:
an un-forked emitter that internally dispatches on `target.fact_schema` (or `target.output_plane`)
to select a different generated body per CSS profile is EXACTLY the relocated seam — and it sails
through `runtime_target_rows_collapsed == true` because those 7 distinct values are invisible to a
`sort -u` over `(source_roots, entry_rule)`. The contract's framing "N **collapsed-identical** rows"
(αC:399) is the giveaway: the live rows are NOT collapsed-identical; they differ in 5 columns. The
gate reports collapse=1 precisely BECAUSE it discards the columns the divergence is in. This is the
SAME necessary-not-sufficient theme V1 (md5) → V2 (grep alphabet) → V3 (grep cannot fire on a
data-table) carried one level deeper — into the F13 structural check itself: V3 picked the right
MECHANISM (row-count collapse, not regex); V4 finds it picked the wrong PROJECTION (2 invariant
columns, not the full per-grammar config tuple).

**The defense is one struct-field-set away — the fix is concrete.** The P3 collapse is still the
right mechanism; only the projection must widen to the columns a relocated branch can ride.

**Fix (REVISE-level, projection-tuple correction, NOT architectural):**
1. **Project `runtime_target_rows_collapsed` onto the per-grammar config tuple, modulo the generated
   `output_dir`.** Replace `sort -u over (source_roots, entry_rule)` (SYNTHESIS:481,:253(iii),:328;
   HANDOFF:260; αE:196,:226; αC:399) with: "all `RuntimeTarget` rows sharing one `grammar_name` MUST
   be byte-identical in EVERY field except the generated-artefact path columns (`output_dir`,
   `expected_files`) — i.e. `fact_schema`/`row_id`/`output_plane`/`emitter`/`entry_rule`/`source_roots`/
   `check_command`/`frontend_requirements` collapse to ONE distinct tuple per `grammar_name`."
   Machine-check: `awk`/`jq` (or a tiny xtask assertion) over the rows for each `grammar_name`,
   asserting `count(distinct config-tuple-minus-output_dir) == 1`. Under live HEAD this FAILS today
   (7 distinct `fact_schema`) — which is correct: the gate must be RED pre-P3 and only go GREEN after
   the 7 profiles genuinely collapse to one CSS config.
2. **Correct the "collapsed-identical" framing.** αC:399 ("a data-table that carries N
   **collapsed-identical** rows … the row-count check catches it") is empirically false on the live
   table and must read: "a data-table whose rows for one `grammar_name` are NOT identical modulo
   `output_dir` IS the relocated seam — the per-grammar-config-tuple collapse check catches it; a
   `(source_roots, entry_rule)`-only projection does NOT, because the divergence rides
   `fact_schema`/`output_plane`/`emitter`."
3. **Keep the row-count mechanism + the §0.4 prose obligation.** The structural collapse is still
   the right answer to V3 §8.1; only the projected column set is corrected. The arm census + type
   census (catching self-disclosing tokens) are retained for their real value. The §0.4 prose
   obligation ("every residual routing entry names the `.bbnf` rule it derives from") reviewed at
   admission remains the human backstop.

This is REVISE, not REJECT: the Lock-14 spine is correct, the P3 collapse mechanism is the right
defense, and the threat IS policeable by it — only the projection tuple is too narrow, repeated
across the F13 sites, and must widen to the columns a relocated branch can actually ride. It is the
SAME md5/grep/row-count necessary-not-sufficient lineage carried into the structural check: each
iteration found the prior machine-check projects onto too few of the columns that carry the threat,
and pointed at the next column set that does.

---

## §9 — Cross-artefact generality findings (consolidated)

**The single V3/CH2 §8.1 REVISE (F13) is CLOSED, verbatim and orphan-free:**
- The arm-census grep's reach claim is corrected at EVERY site (SYNTHESIS:37,:80–91,:201/G3,:480,:520;
  HANDOFF:19,:260,:310; αE F13/:61/:116/:145/:196/:226; αC :393–401) from "a relocated branch into a
  neutral-identifier data-table cannot escape" to the accurate "the arm census catches a
  *self-disclosing grammar-token* branch; the neutral-identifier strategy-table seam is the P3
  structural check." Verified live: arm census = 0 over both `skinny/xtask/src` and codegen.
- The structural defense is PROMOTED to a first-class REJECT-bound telemetry column
  `runtime_target_rows_collapsed` (SYNTHESIS:481, "must be `true`", P3+G3) — exactly the binding V3
  asked for.

**What is RIGHT and load-bearing (do NOT churn):** the `SinkOnlyProgram`/`BackendShape` 5-shape
lowering vehicle (the generalization mechanism — `lower/*.rs` present); the falsifiability triple
with the mutation-test falsifier; the verbatim-blob/single-emitter retirement clauses (fork + blobs
verified live); the P3 collapse-default with provenance binding (the right relocated-seam MECHANISM
— only its projection tuple is §8); the GoogleSheets-as-generation-not-throughput bar; the
no-paper-close/no-stub-prove honest-finding discipline; the phantom-`G`-axis-not-`K`-axis precision
+ the `CssEventGrammar`-absent refinement (DELETE-is-default); the adopt-existing-Pratt sourcing
(litmus non-hollow by construction). The GoogleSheets 3rd-grammar proof is genuinely load-bearing: a
real 185-LOC Pratt grammar with `error_literal`/`cell_ref`/precedence-tower shapes that NO JSON/CSS
rule exercises exists and is adopted; the litmus cannot be satisfied by a third JSON
(`sheets_grammar_shape == pratt-operator` REJECT-bound, SYNTHESIS:488).

**One NEW REVISE (§8): the `runtime_target_rows_collapsed` projection tuple.** It is the SAME
md5/grep/row-count necessary-not-sufficient lineage one level deeper INTO the F13 fix: V3 moved the
defense from the syntactically-incapable regex to the structural row-count check (right mechanism);
V4 finds the check projects onto `(source_roots, entry_rule)` — the two columns that are invariant
across the 7 live css_l4 rows — and ignores `fact_schema`/`row_id`/`output_plane`/`output_dir`, which
carry 7 distinct per-profile values (empirically: `sort -u` (source_roots,entry_rule)=1 GREEN while
`sort -u` fact_schema=7). A relocated branch riding those columns passes green. Not orphaned — the
fix is concrete (§8 (1)–(3): project onto the per-grammar config tuple modulo `output_dir`; correct
the "collapsed-identical" framing; keep the mechanism).

**Zero REJECTs.** No section proposes a grammar-specific intervention, re-opens a generality
pre-block, or claims generalization on a hollow proof. The F14 stale-count REVISE is αD-internal
count-correctness with zero generality content and does not leak into any generality claim.

---

## §10 — Disposition ledger

| Artefact | ACCEPT | REVISE | REJECT | V3 (for comparison) |
|---|---|---|---|---|
| αA results-extraction | 3 | 0 | 0 | 3 / 0 / 0 |
| αB competitor-deltas | 2 | 0 | 0 | 2 / 0 / 0 |
| αC redress-digest | 4 | 0 | 0 | 4 / 0 / 0 |
| αD validated-invalidated | 4 | 0 | 0 | 4 / 0 / 0 |
| αE candidate-shortlist | 6 | 0 | 0 | 6 / 0 / 0 |
| SYNTHESIS.md | 6 | 0 | 0 | 6 / 0 / 0 |
| HANDOFF.md | 4 | 0 | 0 | 4 / 0 / 0 |
| §8 cross-artefact (`runtime_target_rows_collapsed` projection tuple) | 0 | 1 | 0 | (V3 §8.1 arm-census reach — now CLOSED) |
| **Total** | **29** | **1** | **0** | 29 / 1 / 0 |

Accept rate 29/30 = **96.7%** (above the §3Z ≥95% bar; V3 96.7%, V2 96.8%, V1 75.0%). The single V3
REVISE (F13 — arm-census reach over-claim) folded verbatim and orphan-free, verified live (arm
census = 0 over both roots), and was PROMOTED to a dedicated REJECT-bound telemetry gate
`runtime_target_rows_collapsed` — the V3 ask landed in full. The single V4 REVISE (§8) is a
one-level-deeper sharpening of the SAME necessary-not-sufficient theme carried INTO the F13 fix: the
structural row-count check picked the right MECHANISM (P3 collapse, not regex) but the wrong
PROJECTION — `(source_roots, entry_rule)` is invariant across the 7 live css_l4 rows
(`sort -u` → 1, GREEN) while `fact_schema`/`row_id`/`output_plane` carry 7 distinct per-profile values
(`sort -u` → 7) where a relocated branch can ride invisibly; the contract's own "N collapsed-identical
rows" framing (αC:399) is empirically false. The fix is concrete (§8 (1)–(3): project onto the
per-grammar config tuple modulo `output_dir`; correct the framing; keep the mechanism). The goalset's
Lock-14 spine is sound; the GoogleSheets 3rd-grammar proof is load-bearing (real 185-LOC Pratt
grammar adopted, `sheets_grammar_shape == pratt-operator` gated); the interventions are
grammar-neutral; they will work for CSS L4 (scalar, no kernel to preserve) and GoogleSheets (Pratt is
the honest stress with an honest-finding fallback). CH2 expects a V5 confirming cycle to close §8 by
widening the `runtime_target_rows_collapsed` projection to the full per-grammar config tuple, reaching
≥95% × 2 consecutive.

TALLY accept=29 revise=1 reject=0
