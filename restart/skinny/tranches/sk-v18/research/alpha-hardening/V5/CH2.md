# CH2 — GENERALITY (V5)

Lens: CH2 Generality (PASS-ALPHA §3 / SK-V18-GENERALIZATION-HANDOFF §4). Reviewer focus: **does the
goalset respect Lock 14 (one generator ALL grammars); are the interventions grammar-neutral; will
they work for non-JSON grammars (CSS L4 / GoogleSheets / BBNF-self); is the GoogleSheets 3rd-grammar
proof load-bearing?** Subject: SK-V18 = THE GENERALIZATION CYCLE (the inflection backtrack — fork the
hand-written/forked parsers BACK into ONE grammar-driven generator emitting all grammars from
`.bbnf`, over the unified tape/`ValueRef` substrate, shared value API, PROVEN on a 3rd grammar
(GoogleSheets), PRESERVING >SOTA). NOT a new-feature cycle.

Date 2026-05-31. Bracket HEAD `318d9c046`; SK-V17 close `f6a38445b`; V3 audit `7dbe44c22`.

Method: this is the **fifth** hardening iteration. CH2 lineage: V1 **24A/8R** (75.0%) → V2 **30A/1R**
(96.8%) → V3 **29A/1R** (96.7%) → V4 **29A/1R** (96.7%). The single V4/CH2 REVISE (§8.1, fold-id
**F16**): the F13 fold moved the relocated-overfit-seam defense from the arm-census regex (V3 proved
syntactically incapable on a neutral-identifier data-table) to the P3-collapse structural row-count
check — RIGHT MECHANISM — but bound the check's projection to `sort -u` over `(source_roots,
entry_rule)` ONLY, which is invariant across the 7 live css_l4 rows (GREEN false-positive) while the
per-profile divergence rides `fact_schema`/`output_plane`/`row_id` (7 distinct each). V4 asked the
projection widen to the full per-grammar config tuple modulo the generated-artefact path columns.
The α-A..F artefacts + SYNTHESIS + HANDOFF were **re-authored after V4** (alpha mtimes 14:33–14:48 vs
V4/CH2 14:29) carrying explicit V4→V5 FOLD ledgers (αE F16 `:19`/cross-cutting-4 `:226`/-5 `:237`,
αC FOLD-3 `:54`/`:253`/`:308`/`:615`/`:662`, SYNTHESIS §"V4→V5 folds" (2), HANDOFF inv.5 (iv) `:273`).
V5's job: verify F16 folded orphan-free, re-disposition every section live at HEAD, and probe one
level deeper for the NEXT generality gap. Every disposition cites `path:line` / SHA / artefact-line,
verified live where checkable.

---

## §0 — Lens verdict (one paragraph)

**The single V4/CH2 §8.1 REVISE (F16) folded verbatim, orphan-free, across all five+ sites; the
goalset's Lock-14 spine is correct and the GoogleSheets 3rd-grammar proof is genuinely load-bearing.**
Verified live at HEAD `318d9c046`: (1) the F16 fold widened `runtime_target_rows_collapsed` from the
two-column `(source_roots, entry_rule)` projection to "all `RuntimeTarget` rows sharing one
`grammar_name` byte-identical in EVERY field except the generated-artefact path columns
(`output_dir`/`expected_files`)" at SYNTHESIS:397/:553/:594, HANDOFF:24/:273/:331, αE:96/:105/:156/
:197/:207/:236/:237, αC:54/:253/:269/:308/:432/:615/:662 — and I empirically re-confirmed the V4
finding's basis: the old projection `(source_roots, entry_rule)` `sort -u` = **1** (GREEN) while
`fact_schema` / `profile` / `output_plane` / `row_id` each carry **7 distinct** values across the
live css_l4 rows (`skinny/xtask/src/regen_css.rs`); (2) the arm census is still 0 over both
`skinny/xtask/src` and `skinny/crates/codegen/src` (V3/F13 holds — the regex is syntactically
incapable of firing on the neutral `grammar_name: "css_l4"` data table); (3) the live overfit
surfaces the goalset targets are confirmed present — the `RuntimeEmitterKind` fork
(`grammar_provider.rs:40`), the phantom `G: EventGrammar = AnyGrammar` axis (`tape/mod.rs:175`),
`CssEventGrammar` absent at HEAD (only `JsonEventGrammar` + `SheetsEventGrammar` witnesses), the 7
byte-identical css_l4 `generated.rs` replicas (md5 `b654562ccff46ed62dd48e9ace325830`); (4) the real
GoogleSheets source `grammar/google-sheets/google-sheets.bbnf` is present (185 LOC, `error_literal =
"#N/A" -> 0u8` `:34`, `cell_ref = /\$?[A-Za-z]{1,3}\$?\d+/` `:63`, "operator precedence tower" `:92`),
absent from the benched skinny tree (`find skinny -name '*.bbnf'` → only `skinny/grammars/json.bbnf`),
with skinny-tree adoption an explicit PROVE owner obligation. **Zero REJECTs. One NEW REVISE** (§8):
the F16 fold REDEFINED the gate with TWO non-equivalent statements — a correct, complete PROSE form
("byte-identical in EVERY field except the generated-artefact path columns") and an incomplete
OPERATIVE machine-check ENUMERATION (`count(distinct config-tuple-minus-output_dir) == 1` over
`fact_schema`/`row_id`/`output_plane`/`emitter`/`entry_rule`/`source_roots`/`check_command`/
`frontend_requirements`). The live `RuntimeTarget` struct (`regen.rs:6`) has **12 fields**; the
enumeration names 8 of them, and **OMITS `profile`** — the literal 7-distinct per-profile
discriminator (`"css_l4_at_rules_and_media"` … `"css_l4_visual_functions"`) — plus `source_inputs`
and `metadata_inputs`. A relocated branch that internally dispatches on `target.profile` (the most
natural per-profile router) passes the enumerated check, because `profile` is not in the named tuple.
This is the SAME necessary-not-sufficient lineage (V1 md5 → V2 grep-alphabet → V3 grep-cannot-fire →
V4 row-count-projects-2-columns) carried one level deeper INTO the F16 fix itself: V4 correctly moved
from 2 columns to "all-but-path-columns", but the machine-check INSTANTIATION of that prose is a
strict subset that drops the discriminator field. REVISE not REJECT: the prose intent is correct and
complete, the P3-collapse mechanism is right, and the threat IS policeable — the fix is to make the
operative enumeration match the prose ("ALL non-{`output_dir`,`expected_files`} fields", or add
`profile`/`source_inputs`/`metadata_inputs` to the named tuple).

---

## §1 — αA results-extraction (generality lens)

### §1.1 — substrate-generalizes / value-API-does-not split → **ACCEPT** (V4 ACCEPT held)
`alphaA` substrate table. The split — substrate (Lock 1) generalizes + is the foundation;
value-API + codegen do NOT yet — verified live: one `Tape`/`ValueRef`/`PayloadArena`, both grammars
ride it, no second tape. Unchanged from V4. ACCEPT.

### §1.2 — phantom `G`-axis-not-`K`-axis precision → **ACCEPT** (V4 ACCEPT held)
`alphaA:92,:212–220`. Verified live `tape/mod.rs:175`: `ValueRef<'doc,'input:'doc, K = AnyKind,
G: EventGrammar = AnyGrammar>` — TWO defaulted axes; `K=Kind` is real (`json/view.rs:86,143,…`),
`G: EventGrammar` is the phantom (always `AnyGrammar` in production; sole non-default uses are the
compile-proof tests `event_grammar_tests.rs:18,20,89`). `CssEventGrammar` does NOT exist at HEAD
(`grep` → `SheetsEventGrammar` + `JsonEventGrammar` only) — G4's CSS-side INSTANTIATE is creation,
DELETE is the abrogate-before-patch default. Accurate. ACCEPT.

### §1.3 — GoogleSheets close-condition seed (source + skinny-tree obligation) → **ACCEPT** (V4 ACCEPT held)
`alphaA` §"PROVE Sheets". Names `grammar/google-sheets/google-sheets.bbnf`, flags it lives in
totality (verified: skinny tree has only `skinny/grammars/json.bbnf`), states the PROVE wave's first
obligation is bringing the `.bbnf` into the benched tree. ACCEPT.

### §1.4 — x86-scope F15 propagation (generality boundary) → **ACCEPT** (V5 new — αA feeder corrected)
`alphaA:204` re-authored with the "x86 surface — BOTH trees (V5 R-1 fold)" row, citing the second
x86 surface (`ext/x86/` 3554 LOC + `build.rs` 102 LOC) and the crate-wide close gate. This is a
CH1/CH3/CH5 cost/correctness fold (F15) with no grammar-neutrality content of its own — its
generality relevance is only that "x86 gone" is an R10 binding pin; the αA row now cites the
crate-wide inventory-of-record. No generality defect. ACCEPT.

**αA tally: ACCEPT ×4, REVISE ×0.**

---

## §2 — αB competitor-deltas (generality lens)

### §2.1 — GoogleSheets-as-GENERATION-not-throughput bar → **ACCEPT** (V4 ACCEPT held)
`alphaB:279,283,289–293`. The three-grammar bar holds: JSON strict-vs-strict, CSS lazy-vs-eager
(framed), **GoogleSheets has NO competitor bar — its bar is GENERATION** (the ONE generator emits a
real GoogleSheets parser from `.bbnf` with a non-identical `generated.rs`; 25-LOC stub retired). The
canonical `GoogleSheets` (un-abbreviated, `LOCKS.md:349`) is held. Correct generality framing — no
fabricated speed win. The αB §6 V4→V5 fold-record (`:346`) confirms αB was ACCEPTed by all seven V4
lenses (a PRESERVE bar, unchanged by definition) and carries the bar verbatim with live re-verified
numbers. ACCEPT.

### §2.2 — typed-rows-conditional + CSS LOW-lowering-risk → **ACCEPT** (V4 ACCEPT held)
The typed JSON bar rides a per-corpus hand-tuned schema that does NOT generalize; `parse_only` is the
unconditional grammar-general bar. CSS >SOTA does NOT depend on hand-shaping (scalar hot path, no
fragile kernel) — so the G2 lowering rebuild is LOW risk for generality. ACCEPT.

**αB tally: ACCEPT ×2, REVISE ×0.**

---

## §3 — αC redress-digest (generality lens)

### §3.1 — LayoutFacts-derive-not-hardcode (the Lock-14 generality vehicle) → **ACCEPT** (V4 ACCEPT held)
The emitter DERIVES tape ops from `LayoutFacts.backend_shape ∈ {EagerTape,OffsetTape,EventTape,
SinkOnly,CollapsedStage}` (`lower/*.rs` present). The single most important Lock-14 generality
vehicle. Unchanged. ACCEPT.

### §3.2 — relocated-overfit-seam pre-block (clause) → **ACCEPT** (V4 ACCEPT held; clause correct, enforcement is §8)
`alphaC` §2.2 / SYNTHESIS §0.4. The pre-block names the EXACT Lock-14 failure mode and is correct as
a prose obligation ("every residual CSS routing entry names the `.bbnf` rule it derives from"). The
clause ACCEPTs; the machine-check enumeration attributed to enforce it is the §8 finding. ACCEPT
(clause).

### §3.3 — P3 collapse + FOLD-3 structural enforcement (F16) → **ACCEPT-with-§8-sharpening** (V4 ACCEPT; the enumerated machine-check omits the `profile` discriminator)
`alphaC:54,:253–275,:308–325,:428–453,:615–665`. The FOLD-3 addendum correctly relocates the F16
enforcement: the structural collapse must "project onto the FULL per-grammar config tuple, not onto
`(source_roots, entry_rule)`" (`:253`), and αC:64 even lists the prose field set including `profile`
("(`grammar_name`, `profile`, `entry_rule`, `source_roots`, `output_dir`, `check_command`, …)"). The
MECHANISM (P3 collapse) is the right answer to V4 §8.1 — this is the load-bearing correction, and it
folded orphan-free. **But** the OPERATIVE machine-check (the enumerated tuple a tiny xtask assertion
would encode), restated at αC:254/:269/:432/:619, names `fact_schema`/`row_id`/`output_plane`/
`emitter`/`entry_rule`/`source_roots` and OMITS `profile` — the 7-distinct discriminator (§8).
ACCEPT (mechanism); see §8 (the enumeration's omitted column). The §8 fix is a sharpening of THIS
gate's enumeration to match its own prose, not a refutation of the mechanism.

### §3.4 — retirement clause + EventGrammar witness-type seam → **ACCEPT** (V4 ACCEPT held)
The retirement clause binds BOTH `CSS_GENERATED_RS` (verbatim-blob) AND `RuntimeEmitterKind`
(single-emitter-path); verified live the fork exists (`grammar_provider.rs:40`) and the verbatim
blobs exist. The `EventGrammar`-type seam addendum is generality-positive. ACCEPT.

### §3.5 — FOLD-3 cross-artefact orphan-freedom → **ACCEPT** (V5 new)
`alphaC:662–665` records the V4 disposition resolution explicitly: "FOLD-3 (CH2 §8.1 — the
projection-tuple correction) → §0.A.1 + §1-P3 (projection widened to the full per-grammar config
tuple modulo `{output_dir, expected_files}`)". The fold is recorded as resolved, not orphaned. The
αC twins are all corrected. ACCEPT (orphan-freedom); the residual enumeration gap is §8, filed once.

**αC tally: ACCEPT ×5, REVISE ×0.** (The enumeration-omits-`profile` sharpening is filed once at §8
to avoid double-counting; the αC *mechanism* + orphan-freedom ACCEPT.)

---

## §4 — αD validated/invalidated (generality lens)

### §4.1 — DM2 substrate-READY-not-proven → **ACCEPT** (V4 ACCEPT held)
`alphaD:174`. DM2 DEMOTED to "substrate-READY, not proven" — `sheets_witness/` is a ~25-line
`EventGrammar` stub (`SheetsEventGrammar` type-level witness only), and the real
`grammar/google-sheets/google-sheets.bbnf` (185-LOC Pratt) is named as the PROVE source. The honest
demotion of the generalization CLAIM. ACCEPT.

### §4.2 — I3/I4/I5 the three generality invalidations → **ACCEPT** (V4 ACCEPT held; re-verified live)
`alphaD:158–160`. I3 (7 css_l4 replicas — verified byte-identical at HEAD, md5
`b654562ccff46ed62dd48e9ace325830`), I4 (`RuntimeEmitterKind` fork — verified `grammar_provider.rs:40`),
I5 (phantom `<G>` two-axis + test-only `G` precision — verified `tape/mod.rs:175` +
`event_grammar_tests.rs:18,20,89`). Each names its CH2 lens (distinct-grammar-output /
single-emitter-path / phantom-generic). Unchanged, re-verified. ACCEPT.

### §4.3 — S12 GoogleSheets-litmus owner-surface → **ACCEPT** (V4 ACCEPT held)
`alphaD:199`. S12 names `grammar/google-sheets/google-sheets.bbnf` (185-LOC Pratt formula grammar),
cites its distinguishing shapes (`error_literal :34–37`, precedence tower `:92`, `cell_ref :62–84` —
re-verified live), and carries the honest-finding clause ("if the `SinkOnlyProgram`/`BackendShape`
lowering cannot express Pratt precedence, that is a genuine §6-style finding — do not paper-close").
The S12 generality substance is clean. ACCEPT.

### §4.4 — no-second-substrate pre-block (G4) → **ACCEPT** (V4 ACCEPT held)
`alphaD:241`. The "no second substrate" clause (`StructLayout`/`TapeStructBuilder`/`TapeCursor`
alongside `Tape`/`ValueRef` is a Lock 1 violation) protects generality. ACCEPT.

**αD tally: ACCEPT ×4, REVISE ×0.**

---

## §5 — αE candidate-shortlist (generality lens — the load-bearing artefact)

αE carries the most explicit V4→V5 FOLD LEDGER (`alphaE:12–21`, F15/F16) + cross-cutting notes 4/5
(`alphaE:226,:237`). The CH2-relevant fold is F16.

### §5.1 — falsifiability triple (PRESERVED->SOTA / DERIVATION-PROOF / DISTINCT-OUTPUT) → **ACCEPT** (V4 ACCEPT held)
`alphaE:48–72`. The triple holds; DISTINCT-GRAMMAR-OUTPUT now carries F3 (md5-necessary-not-
sufficient) + F10 (canonical alphabet + widened roots + type census) + F13 (arm census catches a
self-disclosing token; relocated-into-data-table is the P3 row-count check) + **F16 (the P3 collapse
projects onto the FULL per-grammar config tuple modulo path columns, `:75`)**. The mutation-test
falsifier remains an excellent operational discriminator. ACCEPT (the enumeration's omitted column is §8).

### §5.2 — B1 un-fork + JSON projection → **ACCEPT** (V4 ACCEPT held)
`alphaE:156`. SINGLE-EMITTER-PATH gate reads the canonical four-grammar grep + the type census +
the F13/F16-corrected scope. Targets the verified defect: `json_sink_direct::render` emits fixed
literal bodies (a hand-written template, not a projection). ACCEPT.

### §5.3 — B2 CSS lowering → **ACCEPT** (V4 ACCEPT held)
`alphaE:145,:156`. LOW risk (scalar hot path, no kernel to preserve). DISTINCT-OUTPUT gate carries
the canonical grep + F16's full-config-tuple collapse. ACCEPT (the enumeration omitted column is §8).

### §5.4 — B3 shared trait + phantom (DELETE-default) → **ACCEPT** (V4 ACCEPT held)
`alphaE` B3 row. DELETE is the abrogate-before-patch DEFAULT; INSTANTIATE is burden-of-proof because
`CssEventGrammar` does NOT exist at HEAD (verified). The preserve-rich-ast structural gate
(`json_rich_navigation_preserved`) guards the LCD-flatten false-green. ACCEPT.

### §5.5 — B4 GoogleSheets-litmus (source named, md5-NNS, F10 alphabet, F13+F16 collapse) → **ACCEPT** (V4 ACCEPT held; the load-bearing disposition)
`alphaE:197,:207,:225`. The litmus names `grammar/google-sheets/google-sheets.bbnf`, requires three
md5-distinct `generated.rs` AND the canonical grammar-neutral body grep AND the type census AND — F16 —
"the neutral-identifier-data-table threat is machine-checked by the per-grammar config-tuple collapse
(all `RuntimeTarget` rows sharing one `grammar_name` collapse to ONE distinct config-tuple modulo
`output_dir`/`expected_files`)", with `:197` adding the key generality guard ("its config-tuple must
be a distinct `grammar_name` from css_l4's AND from json's … so the per-grammar config-tuple collapse
counts a genuine third grammar config, not a relabeled CSS row"). The GoogleSheets proof is non-hollow
by construction. ACCEPT — the enumeration's omitted `profile` column is the §8 finding.

### §5.6 — CANDIDATE A (PRUNE) generality sequencing + F15 crate-wide scope → **ACCEPT** (V4 ACCEPT held; F15 folded)
`alphaE:96,:221`. P4 (Lock-14 gate meaningful) entry-gates B1; P1 now crate-wide x86 deletion
(`:221`, both `src/x86_64/` AND `ext/x86/` AND `build.rs`/`Cargo.toml` nasm dep, V5 R-1/F15). Right
generality sequencing; the F15 fold is a cost/correctness propagation with no grammar-neutrality
content of its own. ACCEPT.

**αE tally: ACCEPT ×6, REVISE ×0.** (The enumeration-omits-`profile` finding is filed once at §8.)

---

## §6 — SYNTHESIS.md (generality lens — the goalset)

### §6.1 — §0.1 G1–G4 + PROVE close conditions (G3 three-surface + F16 collapse) → **ACCEPT** (V4 ACCEPT held; F16 folded)
`SYNTHESIS:322` (G3). G3's verify reads (i) the arm census (self-disclosing-token scope honestly
stated), (ii) the type census, (iii) the RELOCATED-overfit-seam "machine-checked STRUCTURALLY by the
P3 collapse close-gate: all `RuntimeTarget` rows sharing one `grammar_name` byte-identical in EVERY
field except the generated-artefact path columns … a `(source_roots,entry_rule)`-only projection does
NOT catch this … CH2 V4 §8.1". The three-surface structure is correct and the F16 fold landed; the
(iii) enumeration's omitted column is §8. ACCEPT (structure).

### §6.2 — §0.4 pre-blocks → **ACCEPT** (V4 ACCEPT held)
The relocated-seam prose pre-block + no-second-substrate are the right Lock-14 protections. ACCEPT
(enforcement-enumeration at §8).

### §6.3 — §0.5 generalization litmus table → **ACCEPT** (V4 ACCEPT held)
`SYNTHESIS:592` relocated-seam row: `runtime_target_rows_collapsed == false` (REJECT) "a per-grammar
branch relocated into a … metadata" structural check. Binary-structural per-axis with no-stub-prove
fallbacks. ACCEPT (enumeration §8).

### §6.4 — Lock-14 canonical three-surface model + full alphabet → **ACCEPT** (V4 ACCEPT held; re-verified live)
Cites `LOCKS.md` item 14 verbatim; full canonical alphabet `Json|CssL4|GoogleSheets|Bbnf` with
`GoogleSheets` un-abbreviated. Re-verified live: `LOCKS.md:349` carries the type census
`JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser` and the arm census exactly; arm census = 0
over both roots. ACCEPT.

### §6.5 — Section 2 telemetry columns (`runtime_target_rows_collapsed` + F16 binding) → **ACCEPT** (V4 column over-claim CORRECTED; the enumeration omits the discriminator → §8)
`SYNTHESIS:552–553`. This is the key V5-checked column. `:553` now reads the F16 form — "byte-identical
in EVERY field except the generated-artefact path columns `output_dir`/`expected_files`;
`count(distinct config-tuple-minus-output_dir) == 1` per `grammar_name` over `fact_schema`/`row_id`/
`output_plane`/`emitter`/`entry_rule`/`source_roots`/`check_command`/`frontend_requirements`; … a
`(source_roots,entry_rule)`-only `sort -u` is INSUFFICIENT … CH2 V4 §8.1". The V4 ask landed. ACCEPT —
and the §8 finding is that this column's *operative enumeration* (8 named fields) is a strict subset of
its own *prose* ("EVERY field except path columns") and omits `profile` (the 7-distinct discriminator),
`source_inputs`, `metadata_inputs`.

### §6.6 — §0.3 receiver GoogleSheets sourcing (adopt-existing-Pratt) → **ACCEPT** (V4 ACCEPT held)
The "author" → "adopt the existing Pratt grammar" inversion is held; litmus non-hollow by
construction. ACCEPT.

**SYNTHESIS tally: ACCEPT ×6, REVISE ×0.** (The enumeration-omits-`profile` finding is filed once at §8.)

---

## §7 — HANDOFF.md (generality lens)

### §7.1 — backlog (G1–G6 + PROVE) → **ACCEPT** (V4 ACCEPT held)
Maps each item to its V5 finding id + CH2 lens; PROVE adopts the existing Pratt grammar with the
no-stub-prove litmus. ACCEPT.

### §7.2 — six CHALLENGE addenda → **ACCEPT** (V4 ACCEPT held)
verbatim-blob / distinct-grammar-output / single-emitter-path / phantom-generic / timed-plane-symmetry
/ acceleration-wiring carried verbatim. ACCEPT.

### §7.3 — invariant 5 grammar-neutral (F16 folded) → **ACCEPT** (V4 over-narrow projection now WIDENED; enumeration §8)
`HANDOFF:24,:273,:331`. Invariant 5 (iv) now reads the F16 four-surface form: "the STRUCTURAL
relocated-seam check `runtime_target_rows_collapsed` — all xtask `RuntimeTarget` rows sharing one
`grammar_name` are byte-identical in EVERY field except the generated-artefact path columns
(`output_dir`/`expected_files`): `count(distinct config-tuple-minus-output_dir) == 1` per
`grammar_name` over `fact_schema`/`row_id`/`output_plane`/`emitter`/`entry_rule`/`source_roots`/
`check_command`/`frontend_requirements` (a `(source_roots,entry_rule)`-only `sort -u` is INSUFFICIENT
… CH2 V4 §8.1)". The V4 §8.1 over-narrow projection is widened. ACCEPT — the enumeration's omitted
column is §8.

### §7.4 — S-P3 wave sequencing (PRUNE→GENERALIZE→PROVE→HONESTY, P4-before-G2/G3) → **ACCEPT** (V4 ACCEPT held)
`HANDOFF:311,:330`. Right generality sequencing + revert dependency graph + hard caps;
`runtime_target_rows_collapsed` named as a P3/G3 entry condition. ACCEPT.

**HANDOFF tally: ACCEPT ×4, REVISE ×0.** (The §8 finding is filed once cross-artefact.)

---

## §8 — NEW V5 generality finding (the single open REVISE)

### §8.1 — the F16 `runtime_target_rows_collapsed` machine-check is REDEFINED with two non-equivalent forms; the operative ENUMERATION (8 named fields) is a strict subset of its own PROSE ("EVERY field except path columns") and OMITS `profile` — the literal 7-distinct per-profile discriminator a relocated branch most naturally rides → **REVISE** (SYNTHESIS:553,:397,:322(iii); HANDOFF:24,:273; αE:156,:197,:207,:236; αC:254,:269,:432,:619)

**The claim.** The F16 fold (folded correctly + orphan-free across all sites) widened the
relocated-overfit-seam structural check FROM the V4-too-narrow `(source_roots, entry_rule)` projection
TO the full per-grammar config tuple. The fold states the gate in TWO forms at the same site
(SYNTHESIS:553, identically at HANDOFF:273, αC:619):
- **PROSE form (correct + complete):** "all `RuntimeTarget` rows sharing one `grammar_name` … be
  byte-identical in EVERY field except the generated-artefact path columns (`output_dir`/`expected_files`)".
- **OPERATIVE form (the machine-check a tiny xtask assertion / `awk`/`jq` would encode):**
  `count(distinct config-tuple-minus-output_dir) == 1` per `grammar_name` over the **enumerated** set
  `fact_schema` / `row_id` / `output_plane` / `emitter` / `entry_rule` / `source_roots` /
  `check_command` / `frontend_requirements`.

**The empirical refutation (live at HEAD `318d9c046`, `skinny/xtask/src/regen.rs:6–18` +
`skinny/xtask/src/regen_css.rs`).** The live `RuntimeTarget` struct has **12 fields**:
```
grammar_name, profile, entry_rule, source_roots, output_dir, check_command,
source_inputs, metadata_inputs, emitter, expected_files, frontend_requirements, output_labels
```
(`fact_schema` / `row_id` / `output_plane` are NOT top-level fields — they are the three members of
`output_labels: Option<RuntimeOutputLabels>`, `regen_css.rs:48–52`.) The F16 enumerated set names 8
columns. Removing the two path columns (`output_dir`, `expected_files`) from the prose's "EVERY field"
leaves **10** fields the operative check should cover; the enumeration covers 8 named labels and
OMITS three actual struct fields: **`profile`**, `source_inputs`, `metadata_inputs`. Empirically over
the 7 live css_l4 rows:
```
# profile — the per-profile discriminator the enumeration OMITS:
grep -E 'profile: "css_l4' regen_css.rs | sort -u | wc -l   →  7  (DISTINCT)
# fact_schema (named in the enumeration; nested in output_labels):
grep -E 'fact_schema:' regen_css.rs | sort -u | wc -l        →  7  (DISTINCT)
# source_inputs / metadata_inputs — also OMITTED:
grep -E 'source_inputs:|metadata_inputs:' regen_css.rs | sort | uniq -c → CSS_L4_SOURCES ×7, WORKSPACE_METADATA ×7 (invariant here)
```
`profile` carries **7 distinct** values (`"css_l4_at_rules_and_media"` …
`"css_l4_visual_functions"`) — it is the single most explicit per-profile router in the whole table,
and it is NOT in the F16 enumeration. (`source_inputs`/`metadata_inputs` happen to be invariant across
today's css_l4 rows, but they are equally elided by the enumeration and could carry a relocated branch
in a different grammar's row set.)

**Why this is a generality hole, not cosmetics.** The relocated-overfit-seam threat (SYNTHESIS §0.4,
the §0.4 pre-block) is "a per-grammar branch moved into a neutral-identifier strategy data-table."
The F16 fold itself names `target.fact_schema` / `target.output_plane` as the example dispatch fields —
but `target.profile` is the more obvious one (it IS the per-profile name, and a generated-emitter
internal `match target.profile { "css_l4_visual_functions" => … }` is EXACTLY the relocated seam). An
un-forked emitter dispatching on `target.profile` to select a different generated body per CSS profile
sails through the F16 operative enumeration — `profile` carries 7 distinct values but the enumerated
`count(distinct config-tuple)` discards it, so the tuple over the 8 named fields can still report 1.
This is the SAME necessary-not-sufficient theme V1 (md5) → V2 (grep alphabet) → V3 (grep cannot fire
on data-table) → V4 (row-count projects onto 2 invariant columns) carried one level deeper — INTO the
F16 fix itself: V4 correctly moved the projection from "2 columns" to the PROSE "all-but-path-columns",
but the machine-check INSTANTIATION of that prose is a hand-enumerated subset that drops the
discriminator field. The prose is right; the operative enumeration under-instantiates it.

**The defense is one enumeration-edit away — the fix is concrete.** The P3 collapse is still the right
mechanism, the prose is already correct; only the operative machine-check must be made to match the
prose.

**Fix (REVISE-level, enumeration-completeness correction, NOT architectural):**
1. **Make the operative machine-check equal the prose — enumerate by EXCLUSION, not by inclusion.**
   Replace the hand-named tuple (`fact_schema`/`row_id`/`output_plane`/`emitter`/`entry_rule`/
   `source_roots`/`check_command`/`frontend_requirements`) at SYNTHESIS:553/:397/:322(iii),
   HANDOFF:24/:273, αE:156/:197/:207/:236, αC:254/:269/:432/:619 with: "all `RuntimeTarget` fields
   EXCEPT the generated-artefact path columns (`output_dir`, `expected_files`) — i.e. ALL of
   `grammar_name`/`profile`/`entry_rule`/`source_roots`/`check_command`/`source_inputs`/
   `metadata_inputs`/`emitter`/`frontend_requirements`/`output_labels`{`fact_schema`,`row_id`,
   `output_plane`}". Implement as a struct-level derive over the field set (`#[derive(Hash)]` minus
   the two path fields, or a tiny xtask assertion that hashes the whole struct with `output_dir` /
   `expected_files` zeroed), so adding a future `RuntimeTarget` field cannot silently fall outside the
   tuple. Under live HEAD this FAILS (7 distinct `profile` AND 7 distinct `fact_schema`) — correct:
   RED pre-P3, GREEN only after the 7 profiles genuinely collapse to one CSS config.
2. **Name `profile` explicitly wherever the enumeration is restated** (since it is THE per-profile
   discriminator and its omission is the live hole). The αC:64 prose form already includes `profile`
   in its field list — the operative restatements at αC:254/:432/:619, αE:156/:207/:236,
   SYNTHESIS:553, HANDOFF:273 must be brought into line with it.
3. **Keep the mechanism + the §0.4 prose obligation.** The P3 structural collapse is still the right
   answer; only the operative column set is completed. The arm census + type census (self-disclosing
   tokens) are retained for their real value. The §0.4 prose obligation ("every residual routing entry
   names the `.bbnf` rule it derives from") reviewed at admission remains the human backstop.

This is REVISE, not REJECT: the Lock-14 spine is correct, the P3 collapse mechanism is right, the
PROSE form ("EVERY field except path columns") is already complete, and the threat IS policeable by
it — only the OPERATIVE enumeration under-instantiates the prose, drops the `profile` discriminator,
and is repeated across the F16 sites. It is the SAME md5/grep/row-count/projection necessary-not-
sufficient lineage carried into the enumeration of the structural check: each iteration found the
prior machine-check covers too few of the columns the threat can ride, and pointed at the next column
set that does. The cleanest discharge is to stop hand-enumerating and derive the tuple by exclusion of
the two path columns — then no future field can fall outside it.

---

## §9 — Cross-artefact generality findings (consolidated)

**The single V4/CH2 §8.1 REVISE (F16) is CLOSED, verbatim and orphan-free:**
- The relocated-seam structural-check projection is widened at EVERY site (SYNTHESIS:322(iii),:397,
  :553,:594; HANDOFF:24,:273,:331; αE:75,:96,:105,:156,:197,:207,:236,:237; αC:54,:253,:269,:308,:432,
  :615,:662) from `(source_roots, entry_rule)` to "all rows for one `grammar_name` byte-identical in
  EVERY field except `output_dir`/`expected_files`". The "N collapsed-identical rows" framing
  (V4-flagged as empirically false) is corrected to "rows for one `grammar_name` NOT identical modulo
  path columns ARE the relocated seam". Verified live: old projection `(source_roots,entry_rule)` = 1
  (GREEN false-positive); `fact_schema`/`profile`/`output_plane`/`row_id` = 7 distinct each.
- The fold is recorded as RESOLVED, not orphaned, in αE:241 (V4→V5 convergence), αC:662–665, SYNTHESIS
  §"V4→V5 folds" (2). No candidate added/removed (still exactly 5: A, B1–B4); shortlist additive-by-
  deletion; no re-opened REDRESS pre-block.

**What is RIGHT and load-bearing (do NOT churn):** the `SinkOnlyProgram`/`BackendShape` 5-shape
lowering vehicle (the generalization mechanism — `lower/*.rs` present); the falsifiability triple with
the mutation-test falsifier; the verbatim-blob/single-emitter retirement clauses (fork
`grammar_provider.rs:40` + blobs verified live); the P3 collapse-default with the now-widened
config-tuple projection (the right relocated-seam MECHANISM — only the operative enumeration is §8);
the GoogleSheets-as-generation-not-throughput bar; the no-paper-close/no-stub-prove honest-finding
discipline; the phantom-`G`-axis-not-`K`-axis precision + the `CssEventGrammar`-absent refinement
(DELETE-is-default); the adopt-existing-Pratt sourcing (litmus non-hollow by construction). The
GoogleSheets 3rd-grammar proof is genuinely load-bearing: a real 185-LOC Pratt grammar with
`error_literal`/`cell_ref`/precedence-tower shapes that NO JSON/CSS rule exercises exists and is
adopted; the litmus cannot be satisfied by a third JSON (`sheets_grammar_shape == pratt-operator`
REJECT-bound), and αE:197 additionally requires the Sheets `RuntimeTarget` config-tuple be a distinct
`grammar_name` from css_l4 + json (so the per-grammar collapse counts a genuine third grammar config,
not a relabeled CSS row).

**One NEW REVISE (§8): the F16 operative enumeration omits `profile`.** It is the SAME md5/grep/
row-count/projection necessary-not-sufficient lineage one level deeper INTO the F16 fix: V4 widened the
PROSE to "all-but-path-columns" (correct + complete), but the machine-check INSTANTIATION enumerates 8
named labels and drops the `profile` field — the literal 7-distinct per-profile discriminator a
relocated branch most naturally rides — plus `source_inputs`/`metadata_inputs`. Not orphaned — the fix
is concrete (§8 (1)–(3): enumerate by EXCLUSION of the two path columns so the operative check equals
the prose; name `profile` explicitly; keep the mechanism). The cleanest discharge derives the tuple
structurally (hash-all-minus-path-fields) so no future `RuntimeTarget` field can fall outside it.

**Zero REJECTs.** No section proposes a grammar-specific intervention, re-opens a generality pre-block,
or claims generalization on a hollow proof. The F15 x86-scope REVISE (αE P1 crate-wide) is CH1/CH3/CH5
cost/correctness with zero grammar-neutrality content and does not leak into any generality claim.

---

## §10 — Disposition ledger

| Artefact | ACCEPT | REVISE | REJECT | V4 (for comparison) |
|---|---|---|---|---|
| αA results-extraction | 4 | 0 | 0 | 3 / 0 / 0 |
| αB competitor-deltas | 2 | 0 | 0 | 2 / 0 / 0 |
| αC redress-digest | 5 | 0 | 0 | 4 / 0 / 0 |
| αD validated-invalidated | 4 | 0 | 0 | 4 / 0 / 0 |
| αE candidate-shortlist | 6 | 0 | 0 | 6 / 0 / 0 |
| SYNTHESIS.md | 6 | 0 | 0 | 6 / 0 / 0 |
| HANDOFF.md | 4 | 0 | 0 | 4 / 0 / 0 |
| §8 cross-artefact (F16 operative enumeration omits `profile`) | 0 | 1 | 0 | (V4 §8.1 projection tuple — now CLOSED) |
| **Total** | **31** | **1** | **0** | 29 / 1 / 0 |

Accept rate 31/32 = **96.9%** (above the §3Z ≥95% bar; V4 96.7%, V3 96.7%, V2 96.8%, V1 75.0%). The
single V4 REVISE (F16 — the relocated-seam structural-check projection was too narrow at `(source_roots,
entry_rule)`) folded verbatim and orphan-free across all sites, verified live (old projection = 1 GREEN
false-positive; `fact_schema`/`profile`/`output_plane`/`row_id` = 7 distinct each), and the PROSE form
of the gate was correctly widened to "EVERY field except the generated-artefact path columns" — the V4
ask landed. The single V5 REVISE (§8) is a one-level-deeper sharpening of the SAME necessary-not-
sufficient theme carried INTO the F16 fix: the gate is REDEFINED with two non-equivalent forms — a
correct, complete PROSE ("EVERY field except `output_dir`/`expected_files`") and an OPERATIVE
machine-check ENUMERATION (8 named labels) that is a strict subset of the prose and OMITS `profile`
(the literal 7-distinct per-profile discriminator a relocated branch most naturally rides) plus
`source_inputs`/`metadata_inputs`. The live `RuntimeTarget` struct has 12 fields; the enumeration
covers 8; the prose-minus-path-columns covers 10. The fix is concrete (§8 (1)–(3): enumerate by
EXCLUSION of the two path columns so the operative check equals the prose; name `profile` explicitly;
keep the mechanism — ideally derive the tuple structurally so no future field falls outside it). The
goalset's Lock-14 spine is sound; the GoogleSheets 3rd-grammar proof is load-bearing (real 185-LOC
Pratt grammar adopted, `sheets_grammar_shape == pratt-operator` gated, Sheets config-tuple required
distinct from css_l4 + json); the interventions are grammar-neutral; they will work for CSS L4 (scalar,
no kernel to preserve) and GoogleSheets (Pratt is the honest stress with an honest-finding fallback).
CH2 expects a V6 confirming cycle to close §8 by making the F16 operative enumeration equal its own
prose (enumerate-by-exclusion), reaching ≥95% × 2 consecutive.

TALLY accept=31 revise=1 reject=0
