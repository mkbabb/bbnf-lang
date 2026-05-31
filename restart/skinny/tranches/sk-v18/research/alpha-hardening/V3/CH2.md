# CH2 — GENERALITY (V3)

Lens: CH2 Generality (PASS-ALPHA §3 / SK-V18-GENERALIZATION-HANDOFF §4). Reviewer focus:
**does the goalset respect Lock 14 (one generator ALL grammars); are the interventions
grammar-neutral; will they work for non-JSON grammars (CSS L4 / Sheets / BBNF-self); is the
Sheets 3rd-grammar proof load-bearing?** Subject: SK-V18 = THE GENERALIZATION CYCLE (the
inflection backtrack — fork the hand-written/forked parsers BACK into ONE grammar-driven
generator emitting all grammars from `.bbnf`, over the unified tape/`ValueRef` substrate,
shared value API, PROVEN on a 3rd grammar (Sheets), PRESERVING >SOTA). NOT a new-feature cycle.
Date 2026-05-31. Bracket HEAD `318d9c046`; SK-V17 close `f6a38445b`; V3 audit `7dbe44c22`.

Method: this is the **third** hardening iteration. CH2/V1 (`../V1/CH2.md`) dispositioned
**24A/8R/0** (75.0%) on three cross-artefact generality folds (Sheets sourcing; md5
necessary-not-sufficient; canonical Lock-14 three-surface model uncited). CH2/V2 (`../V2/CH2.md`)
dispositioned **30A/1R/0** (96.8%) — all 8 V1 folds landed, ONE residual REVISE (§8.1: the
neutrality-grep alphabet `Sheets\w*` misses `GoogleSheets`; `Bbnf` omitted; the xtask
workspace-metadata surface (b) unscanned). The α-A..F artefacts + SYNTHESIS + HANDOFF were
**re-authored after V2** (alpha mtimes 14:01–14:03 vs V2 13:55–13:58) carrying explicit V2→V3
FOLD ledgers (αE F10, αB §6, SYNTHESIS:26–39, HANDOFF:237–251). V3's job: verify the single V2
REVISE folded orphan-free, re-disposition every section live at HEAD, and probe for a
NEXT-LEVEL generality gap. Every disposition cites `path:line` / SHA / artefact-line, verified
live where checkable.

---

## §0 — Lens verdict (one paragraph)

**The single V2/CH2 §8.1 REVISE (F10) folded verbatim and orphan-free; the goalset's Lock-14
spine is correct and the Sheets 3rd-grammar proof is genuinely load-bearing.** Verified live at
HEAD: (1) `LOCKS.md` item 14 canonical alphabet is `Json | CssL4 | Bbnf | GoogleSheets` with
`GoogleSheets\w*` un-abbreviated in BOTH canonical verification greps (the type census
`JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser` AND the arm census) — the F10 fold
matches the canon exactly; (2) every artefact now carries the un-abbreviated `(GoogleSheets|Sheets)\w*`,
the `Bbnf\w*` forward-safety arm, the widened scan root `skinny/xtask/src`, AND the new
grammar-named-*type* census as a second canonical surface (SYNTHESIS:201 (i)/(ii), telemetry
columns `generator_grammar_branch_count`/`generator_grammar_type_count` :423–424, HANDOFF
invariant 5 :234–251 three-surface binding); (3) the real Sheets source
`grammar/google-sheets/google-sheets.bbnf` is confirmed present (185 LOC, `error_literal #N/A`
`:34`, `cell_ref :63`, "operator precedence tower" `:92` — a Pratt formula grammar NO JSON/CSS
rule exercises), confirmed absent from the benched skinny tree (`find skinny -name '*.bbnf'` →
only `json.bbnf`), with skinny-tree adoption now an explicit PROVE owner obligation + Pratt
honest-finding fallback; (4) a NEW V3-precision refinement — `CssEventGrammar` does NOT exist at
HEAD (only `JsonEventGrammar`/`SheetsEventGrammar` witnesses, both inert), so G4's CSS-side
INSTANTIATE is *creation* not rename, DELETE is the abrogate-before-patch default — is accurate
and load-bearing. **Zero REJECTs. One NEW REVISE** (§8): the contract over-claims that
root-widening the *arm-census regex* to `skinny/xtask/src` means "a per-grammar branch RELOCATED
into a neutral-identifier `RuntimeTarget`/strategy data-table cannot escape" (SYNTHESIS:37,:201;
αE:185; telemetry :423; invariant 5 :241–243). I empirically falsified this: the
`match\s+\w+\s*\{[^}]*Json\s*=>…` regex returns **NO MATCH** against the live neutral-identifier
`RuntimeTarget` table at `regen_css.rs:35` — a neutral-identifier data-table by construction has
no `Json =>` arm syntax for the regex to fire on. The actual defense against this exact threat
EXISTS in the contract (αC:158 P3 collapse close-gate: the 7 `RuntimeTarget` css_l4 rows collapse
to ONE CSS provider, removing the per-profile metadata divergence that could carry a relocated
branch) — but the contract mis-attributes the coverage to the grep, not to the collapse gate, and
over-states the grep's reach. This is a verification-surface attribution correction, not a
refutation; the goalset polices the threat, it just labels the wrong mechanism.

---

## §1 — αA results-extraction (generality lens)

### §1.1 — substrate-generalizes / value-API-does-not split → **ACCEPT** (V2 ACCEPT held)
`alphaA` substrate table. The split — substrate (Lock 1) generalizes + is the foundation;
value-API + codegen do NOT yet — verified live: one `Tape`/`ValueRef`/`PayloadArena`
(`tape/mod.rs:175`), both grammars ride it, no second tape. "Substrate union VALIDATED (the
genuine foundation)." Unchanged from V2. ACCEPT.

### §1.2 — phantom `G`-axis-not-`K`-axis precision → **ACCEPT** (V2 ACCEPT held; now creation-not-rename)
`alphaA` phantom-G row. Verified live `tape/mod.rs:175`: `pub struct ValueRef<'doc,'input:'doc,
K = AnyKind, G: EventGrammar = AnyGrammar>` — TWO defaulted axes. `K=Kind` is real (JSON
instantiates NumberKind/StringKind/…); `G: EventGrammar` is the phantom (always `AnyGrammar`,
witnesses inert). G4 must instantiate-or-delete the RIGHT axis (`G`). The V3 refinement is
load-bearing and verified: `CssEventGrammar` does NOT exist at HEAD (`grep` →
`SheetsEventGrammar` `sheets_witness/event_grammar_witness.rs:4` + `JsonEventGrammar`
`json/event_grammar_witness.rs:4` only) — so G4's CSS-side INSTANTIATE is creation-burden, not a
rename, and DELETE is the abrogate-before-patch default. Accurate. ACCEPT.

### §1.3 — Sheets close-condition seed (source + skinny-tree obligation) → **ACCEPT** (V2 ACCEPT held)
`alphaA` §"PROVE Sheets — and where the `.bbnf` comes from". Names
`grammar/google-sheets/google-sheets.bbnf`, flags it lives in totality (verified: skinny tree
has only `skinny/grammars/json.bbnf`), and states the PROVE wave's first obligation is bringing
the Sheets `.bbnf` into the benched tree (a skinny grammar root + xtask Sheets `RuntimeTarget`).
Exactly the V1/V2 fix held. ACCEPT.

**αA tally: ACCEPT ×3, REVISE ×0.**

---

## §2 — αB competitor-deltas (generality lens)

### §2.1 — Sheets-as-GENERATION-not-throughput bar → **ACCEPT** (V2 ACCEPT held; now canonically named)
`alphaB:279,283`. The three-grammar bar table holds: JSON strict-vs-strict, CSS lazy-vs-eager
(framed), **GoogleSheets has NO competitor bar — its bar is GENERATION** (the ONE generator
emits a real GoogleSheets parser from `.bbnf` with a non-identical `generated.rs`, 25-LOC stub
retired). The V2→V3 fold canonically renamed the row "Sheets" → "GoogleSheets" per `LOCKS.md:349`
(`alphaB:13,289–293`) — SPEC-consistent with the grep alphabet, no number changed. This is the
correct generality framing (no fabricated speed win) and the row CH2 most wants preserved. ACCEPT.

### §2.2 — typed-rows-conditional + CSS LOW-lowering-risk → **ACCEPT** (V2 ACCEPT held)
The typed JSON bar rides a per-corpus hand-tuned schema that does NOT generalize; `parse_only`
is the unconditional grammar-general bar. CSS >SOTA does NOT depend on hand-shaping (scalar hot
path, no fragile kernel) — verified: CSS scan kernels `find_css_significant`/`find_comment_close`
live in `runtime_simd.rs:169,112` (not the hot admission path) — so the G2 lowering rebuild is
LOW risk for generality (no per-grammar kernel the lowering must reproduce). ACCEPT.

**αB tally: ACCEPT ×2, REVISE ×0.**

---

## §3 — αC redress-digest (generality lens)

### §3.1 — LayoutFacts-derive-not-hardcode (the Lock-14 generality vehicle) → **ACCEPT** (V2 ACCEPT held)
The emitter DERIVES tape ops from `LayoutFacts.backend_shape ∈ {EagerTape,OffsetTape,EventTape,
SinkOnly,CollapsedStage}` (`lower/*.rs` verified present in `skinny/crates/codegen/src/lower`).
The single most important Lock-14 generality vehicle. Unchanged. ACCEPT.

### §3.2 — relocated-overfit-seam pre-block → **ACCEPT-with-§8-sharpening** (V2 ACCEPT; the threat is real, the grep mis-attributed)
`alphaC` §2.2 / SYNTHESIS:274 ("Relocating per-rule branching into projection DATA is the overfit
re-entry seam and is forbidden — every residual CSS routing entry names the `.bbnf` rule it
derives from"). The pre-block names the EXACT Lock-14 failure mode and is correct as a *prose*
obligation. The §8 finding is NOT that this clause is wrong — it is that the *machine-check*
prescribed to enforce it (the arm-census regex over `skinny/xtask/src`) cannot fire on a
neutral-identifier data-table. The clause itself ACCEPTs; its enforcement attribution is the §8
REVISE. ACCEPT (clause); see §8 (enforcement).

### §3.3 — P3 collapse-vs-differentiate (COLLAPSE-default + provenance binding) → **ACCEPT** (V2 ACCEPT held; this is the REAL data-table defense)
`alphaC:147–162`. COLLAPSE-to-ONE is the DEFAULT (the 7 directories are demonstrably one grammar
— one `.bbnf`, one entry rule `stylesheet`, verified `regen_css.rs:39` all 7 `RuntimeTarget`
rows carry `entry_rule:"stylesheet"` + `CSS_L4_ROOTS`); differentiate admissible ONLY if N
distinct `.bbnf` roots are authored; cosmetic divergence REJECTed. The close gate
(`alphaC:158–162`) binds the structural check: "the CSS replica count is **1** … the directory
count itself collapses to one CSS provider … every such pair traces to a distinct `.bbnf`." **This
is the actual defense against the §8 data-table-relocation threat** — collapsing the 7
`RuntimeTarget` css_l4 rows to one removes the per-profile metadata divergence that could carry a
relocated branch. ACCEPT — and the §8 fix is to attribute the relocated-seam coverage HERE, not to
the grep.

### §3.4 — retirement clause + EventGrammar witness-type seam → **ACCEPT** (V2 ACCEPT held)
The retirement clause binds BOTH `CSS_GENERATED_RS` (verbatim-blob) AND `RuntimeEmitterKind`
(single-emitter-path); verified live the fork exists (`grammar_provider.rs:40–42`
`CompiledLowering`/`RequestFacts`, dispatched `runtime_generator.rs:17,25`) and the verbatim
blobs exist (`runtime_generator.rs:91` `CSS_GENERATED_RS`, `:195/:550/:572/:594` `JSON_*_RS`).
The `EventGrammar`-type seam addendum (`alphaC:178,464`) is generality-positive. ACCEPT.

**αC tally: ACCEPT ×4, REVISE ×0.** (The relocated-seam *enforcement* sharpening is filed once at
§8 to avoid double-counting; the αC *clause* itself ACCEPTs.)

---

## §4 — αD validated/invalidated (generality lens)

### §4.1 — DM2 substrate-READY-not-proven → **ACCEPT** (V2 ACCEPT held)
`alphaD:118`. DM2 DEMOTED to "substrate-READY, not proven" — `sheets_witness/` is a ~25-line
`EventGrammar` stub (verified live: `sheets_witness/event_grammar_witness.rs:4 SheetsEventGrammar`,
type-level witness only, no runtime/value-API/scanner). The honest demotion of the generalization
CLAIM. ACCEPT.

### §4.2 — I3/I4/I5 the three generality invalidations → **ACCEPT** (V2 ACCEPT held)
I3 (7 replicas — verified byte-identical at HEAD: `css_l4_at_rules_and_media/generated.rs` md5
`b654562c…` ≡ `css_l4_visual_functions/generated.rs` md5 `b654562c…`), I4 (`RuntimeEmitterKind`
fork), I5 (phantom `<G>` + divergent value API) — each names its CH2 lens. Unchanged. ACCEPT.

### §4.3 — S12 Sheets-litmus owner-surface (source + Pratt honest-finding) → **ACCEPT** (V2 ACCEPT held; +V3 CssEventGrammar refinement)
`alphaD:143,203,207,261`. S12 names `grammar/google-sheets/google-sheets.bbnf` (185-LOC Pratt
formula grammar) as the source, cites its distinguishing shapes (`error_literal :34–37`,
precedence tower `:92`, `cell_ref/cell_or_range :62–84` — all re-verified live), and carries the
honest-finding clause "if the `SinkOnlyProgram`/`BackendShape` lowering cannot express Pratt
precedence, that is a genuine §6-style finding … do not paper-close." The V3 fold adds the
verified `CssEventGrammar`-absent row (`alphaD:203,261`): `grep` → only `SheetsEventGrammar` +
`JsonEventGrammar`; no `CssEventGrammar` — so G4 CSS-side INSTANTIATE is creation. Accurate and
generality-strengthening. ACCEPT.

### §4.4 — no-second-substrate pre-block (G4) → **ACCEPT** (V2 ACCEPT held)
The "no second substrate" clause (`StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside
`Tape`/`ValueRef` is a Lock 1 violation) protects generality: the shared trait must not fork the
substrate. ACCEPT.

**αD tally: ACCEPT ×4, REVISE ×0.**

---

## §5 — αE candidate-shortlist (generality lens — the load-bearing artefact)

αE carries the most explicit V2→V3 FOLD LEDGER (`alphaE:12–22`, F9–F12). The CH2-relevant fold is
F10 (canonical four-grammar alphabet + widened scan roots + type census).

### §5.1 — falsifiability triple (PRESERVED->SOTA / DERIVATION-PROOF / DISTINCT-OUTPUT) → **ACCEPT** (V2 ACCEPT held)
`alphaE:48–50`. The triple holds; the DISTINCT-GRAMMAR-OUTPUT gate now carries F3
(md5-necessary-not-sufficient) + F10 (canonical alphabet + widened roots + type census). The
mutation-test falsifier (mutate `.bbnf` → `generated.rs` changes; a const courier cannot pass) is
an excellent operational discriminator. ACCEPT.

### §5.2 — B1 un-fork + JSON projection → **ACCEPT** (V2 ACCEPT held)
`alphaE:105`. SINGLE-EMITTER-PATH gate now reads the canonical four-grammar grep across both
codegen AND xtask roots + the type census. Targets the verified defect: `json_sink_direct::render`
consults `program` only in `validate`/`render_header` (`:19,26,30,39,78`) then emits fixed
`out.push_str(...)` literal bodies (`render_entry :96`, `render_value_dispatch :124`) — a
hand-written template, not a projection. ACCEPT. (The grep's xtask-root *over-claim* is filed at
§8; the gate's substance — un-fork + project — is sound.)

### §5.3 — B2 CSS lowering → **ACCEPT** (V2 ACCEPT held)
`alphaE:134`. LOW risk (scalar hot path, no kernel to preserve — verified). DISTINCT-OUTPUT gate
carries the canonical grep + the differentiate-fallback (resolved to COLLAPSE-default by αC §3.3).
ACCEPT.

### §5.4 — B3 shared trait + phantom (DELETE-default; preserve-rich-ast structural gate) → **ACCEPT** (V2 ACCEPT held)
`alphaE` B3. DELETE is the abrogate-before-patch DEFAULT; INSTANTIATE is burden-of-proof because
`CssEventGrammar` does NOT exist at HEAD (verified). The preserve-rich-ast structural gate (JSON's
`get(key)` + typed-`Kind` + visitor must remain reachable THROUGH the shared trait) guards the
LCD-flatten false-green. Both generality-positive. ACCEPT.

### §5.5 — B4 Sheets-litmus (source named, md5-necessary-not-sufficient, F10 alphabet) → **ACCEPT** (V2 ACCEPT held; the load-bearing disposition)
`alphaE:185`. The litmus names `grammar/google-sheets/google-sheets.bbnf`, requires three
md5-distinct `generated.rs` AND the canonical grammar-neutral body grep (`GoogleSheets`
un-abbreviated, `Bbnf` included, codegen+xtask roots) AND the type census, with the honest-finding
clause for Pratt-lowering failure. The Sheets proof is non-hollow by construction (a real
different-shape grammar exists and is adopted). ACCEPT — subject to §8 (the line's own claim "the
xtask metadata surface scanned so a `RuntimeTarget` strategy table in DATA cannot carry the
relocated branch" is the over-claim).

### §5.6 — CANDIDATE A (PRUNE) generality sequencing → **ACCEPT** (V2 ACCEPT held)
P4 (Lock-14 gate meaningful) entry-gates B1 — build the neutrality gate before the thing it must
scan. Verified the gate currently excludes the leak surface: `GENERIC_SCAN_ROOTS`
(`lock14_baseline.rs:2409`) lists `crates/codegen/src/lib.rs`, `…/lower`, `…/grammar_profile.rs`
but NOT `runtime_generator.rs`/`grammar_provider.rs`/`json_sink_direct.rs` (those are in a
SEPARATE `SKV15_W2_EXTRA_COVERAGE_ROOTS` array `:2443`) — confirming the P4 "exclusion holes"
finding. Right generality sequencing. ACCEPT.

**αE tally: ACCEPT ×6, REVISE ×0.** (The F10 xtask-grep over-claim is filed once at §8 as the
cross-artefact REVISE; the candidate substance is clean.)

---

## §6 — SYNTHESIS.md (generality lens — the goalset)

### §6.1 — §0.1 G1–G4 + PROVE close conditions → **ACCEPT** (V2 ACCEPT held)
Every generalization gate structurally falsifiable. G3 (`SYNTHESIS:201`) binds the canonical
three-surface model + the two canonical greps (arm census (i) + type census (ii)) over the FULL
`LOCKS.md:349` alphabet. ACCEPT (the grep's xtask-root reach claim is §8).

### §6.2 — §0.4 pre-blocks (verbatim-blob / phantom / distinct-output / relocated-seam / no-2nd-substrate) → **ACCEPT** (V2 ACCEPT held)
`SYNTHESIS:274–316`. The relocated-seam prose pre-block + the re-entry pre-blocks + no-second-
substrate (`:312–316`) are the right Lock-14 protections. ACCEPT (enforcement at §8).

### §6.3 — §0.5 generalization litmus table → **ACCEPT** (V2 ACCEPT held)
`SYNTHESIS:318–340`. Binary-structural per-axis table with no-stub-prove fallbacks ("if Sheets
cannot be emitted via the generator only: the generalization is NOT real — surface honestly, do
NOT stub-prove", `:330`). The "aarch64-only + meaningful gate" row (`:332`) accurately states
current="gate excludes leaks" → target="Lock-14 gate scans the leak surface" (verified accurate
against `GENERIC_SCAN_ROOTS` :2409). ACCEPT.

### §6.4 — Lock-14 canonical three-surface model + full alphabet → **ACCEPT** (V2 ACCEPT held)
`SYNTHESIS:201`. Cites `LOCKS.md` item 14 (a)/(b)/(c) verbatim; the full canonical alphabet
`Json|CssL4|GoogleSheets|Bbnf` with `GoogleSheets` un-abbreviated. I re-verified the canonical text
lives at `LOCKS.md` item 14: the verification greps there use `GoogleSheetsParser` (type) and
`GoogleSheets\w*\s*=>` (arm) — the fold matches the canon exactly. ACCEPT.

### §6.5 — Section 2 telemetry columns (`generator_grammar_branch_count` / `…_type_count` / `sheets_grammar_shape`) → **ACCEPT** (V2 ACCEPT held)
`SYNTHESIS:423–424` + the `sheets_grammar_shape ∈ {pratt-operator,flat-stream,tree}` guard. Both
machine-checkable, gate-consumed, REJECT-bound. The branch-count column co-gates the neutral
emitter; the type-count column the re-emitted grammar-named type. ACCEPT (the column's "across
BOTH … `skinny/xtask/src` … so a per-grammar branch relocated into a neutral-identifier metadata
data-table is caught" parenthetical is the §8 over-claim — the column is correct, its stated
*reach* over the xtask root is overstated).

### §6.6 — §0.3 receiver Sheets sourcing (adopt-existing-Pratt, not author-stub) → **ACCEPT** (V2 ACCEPT held)
The "author" → "adopt the existing Pratt grammar" inversion is held; the litmus is non-hollow by
construction. ACCEPT.

**SYNTHESIS tally: ACCEPT ×6, REVISE ×0.** (The xtask-grep-reach over-claim is filed once at §8 as
the cross-artefact REVISE rather than counted against three SYNTHESIS sub-sections.)

---

## §7 — HANDOFF.md (generality lens)

### §7.1 — backlog (G1–G6 + PROVE) → **ACCEPT** (V2 ACCEPT held)
Maps each item to its V3 finding id + CH2 lens; PROVE adopts the existing Pratt grammar with the
no-stub-prove litmus. ACCEPT.

### §7.2 — six CHALLENGE addenda → **ACCEPT** (V2 ACCEPT held)
verbatim-blob / distinct-grammar-output / single-emitter-path / phantom-generic /
timed-plane-symmetry / acceleration-wiring carried verbatim. ACCEPT.

### §7.3 — invariant 5 grammar-neutral (three canonical surfaces) → **ACCEPT** (V2 ACCEPT held; the F10 fold)
`HANDOFF:231–251`. Invariant 5 binds THREE canonical surfaces: (i) `GENERIC_SCAN_ROOTS`
forbidden-token scan; (ii) the arm census over the FULL `LOCKS.md:349` alphabet
`Json|CssL4|(GoogleSheets|Sheets)|Bbnf` across codegen AND xtask; (iii) the grammar-named-*type*
census `JsonParser|CssL4Parser|GoogleSheetsParser|BbnfBootstrap` — with the "different leaks"
rationale + the `EventGrammar` witness-token addendum. The alphabet/scan-root/type-census fold is
exactly the V2 §8.1 (i)/(ii)/(iii) fix, verbatim. ACCEPT (the surface-(ii) "where a per-grammar
branch can be RELOCATED into a neutral-identifier data-table the codegen-scoped grep misses"
sentence `:241–243` is the §8 over-claim — the arm-census regex over the xtask root does NOT catch
a neutral-identifier data-table; invariant 5's three-surface STRUCTURE is sound, one rationale
clause overstates).

### §7.4 — S-P3 wave sequencing (PRUNE→GENERALIZE→PROVE→HONESTY, P4-before-G2/G3) → **ACCEPT** (V2 ACCEPT held)
Right generality sequencing + revert dependency graph + hard caps. ACCEPT.

**HANDOFF tally: ACCEPT ×4, REVISE ×0.** (The over-claim is filed at §8.)

---

## §8 — NEW V3 generality finding (the single open REVISE)

### §8.1 — The arm-census regex cannot catch a neutral-identifier metadata data-table; the contract mis-attributes the relocated-seam coverage to the grep → **REVISE** (SYNTHESIS:37,:201,:423; αE:185; HANDOFF inv.5 :241–243)

**The claim.** The F10 fold widened the arm-census grep scan root to `skinny/xtask/src` and the
contract repeatedly asserts this means a relocated branch in metadata is caught:
- SYNTHESIS:37 — "a per-grammar branch relocated into a neutral-identifier `RuntimeTarget`/strategy
  data-table cannot escape a codegen-scoped grep (closing the relocated-overfit-seam §0.4
  pre-block at the gate)."
- SYNTHESIS:201 (i) — "the arm census over codegen AND the xtask `RuntimeTarget`/strategy-table
  metadata, so a per-grammar branch RELOCATED into a neutral-identifier metadata data-table cannot
  escape the codegen-scoped grep — the relocated-overfit-seam §0.4 pre-block made
  machine-checkable."
- `generator_grammar_branch_count` column SYNTHESIS:423 — "across BOTH `skinny/crates/codegen/src`
  AND `skinny/xtask/src` (the canonical surface-(b) workspace-metadata strategy tables, so a
  per-grammar branch relocated into a neutral-identifier metadata data-table is caught)."
- αE:185 — "the xtask metadata surface scanned so a `RuntimeTarget` strategy table in DATA cannot
  carry the relocated branch."
- HANDOFF inv.5 :241–243 — "the xtask root is the canonical surface (b) … where a per-grammar
  branch can be RELOCATED into a neutral-identifier data-table the codegen-scoped grep misses."

**The empirical refutation.** I ran the exact prescribed regex against the live neutral-identifier
metadata table:
```
rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|(GoogleSheets|Sheets)\w*\s*=>|Bbnf\w*\s*=>' \
   skinny/xtask/src/regen_css.rs   →  NO MATCH
```
The live `RuntimeTarget` table (`regen_css.rs:35–`) carries per-profile config in DATA form —
`grammar_name: "css_l4"`, `profile: "css_l4_at_rules_and_media"`, `emitter:
codegen::RuntimeEmitterKind::RequestFacts`, per-row `fact_schema`/`row_id`/`output_plane`
strings — with NO `match grammar { Json => … }` arm syntax anywhere. The arm-census regex is
**syntactically incapable** of firing on it. The logic is self-defeating: a *neutral-identifier*
data-table is, by definition, one that does NOT use the grammar-token arm syntax `Json =>` —
that is precisely what makes it "neutral-identifier." If a relocated branch DID write `Json =>` in
the metadata, it would not be neutral-identifier, and the codegen-scoped grep would already catch
it via root-widening only by accident of the token appearing. So the grep catches a
*self-disclosing* metadata branch (which is not the threat) and CANNOT catch a *neutral-identifier*
metadata data-table (which IS the threat named by the §0.4 relocated-overfit-seam pre-block).

**The defense already exists — but mis-attributed.** The real machine-check against
data-table relocation is the **P3 collapse close-gate** (`alphaC:158–162`): "the CSS replica count
is **1** … the directory count itself collapses to one CSS provider … every such pair traces to a
distinct `.bbnf`." Collapsing the 7 `RuntimeTarget` css_l4 rows to ONE removes the per-profile
metadata divergence that could carry a relocated branch in the first place — a *structural row-count*
check, not a regex. The contract polices the threat HERE; it just labels the grep as the mechanism.

**Fix (REVISE-level, verification-surface attribution, not architectural):**
1. **Scope the grep's stated reach honestly.** Replace every "a relocated branch into a
   neutral-identifier data-table cannot escape / is caught" claim (SYNTHESIS:37,:201,:423; αE:185;
   HANDOFF inv.5 :241–243) with the accurate scope: the xtask-rooted arm-census grep catches a
   metadata branch that *self-discloses a grammar token* (`Json =>` etc.); it does NOT catch a
   neutral-identifier strategy table.
2. **Bind the structural defense to the relocated-seam pre-block.** State that the
   relocated-overfit-seam is machine-checked by the P3 collapse gate (`RuntimeTarget` rows for one
   `.bbnf`/`entry_rule` must collapse to ONE per-grammar config row — no per-profile code-path
   divergence) PLUS the §0.4 prose obligation ("every residual CSS routing entry names the `.bbnf`
   rule it derives from"), reviewed at admission by the CH-lens. The structural invariant: the
   xtask `RuntimeTarget` table must carry exactly one row per distinct (`source_roots`,`entry_rule`)
   pair — verifiable by `sort -u` over `(source_roots,entry_rule)`, NOT by an arm-census regex.
3. **Keep the grep for its real value.** The codegen+xtask arm census + type census still correctly
   catch (a) a `match grammar` arm in codegen, (b) a self-disclosing grammar-token arm anywhere in
   either root, (c) a re-emitted grammar-named type. Those are real and worth the root-widening.
   Only the "neutral-identifier data-table" reach claim is false and must be corrected.

This is REVISE, not REJECT: the goalset's Lock-14 spine is correct, the threat IS policed (by P3
collapse), and the grep IS worth running — only one over-stated machine-check reach claim, repeated
across five sites, must be corrected to match what the regex can actually do. It is the SAME
structural theme as V1/V2 (md5/grep necessary-not-sufficient) carried one level deeper: V1 added the
arm census; V2 widened its alphabet + roots; V3 finds the widened-root claim over-reaches into a
case the regex syntactically cannot detect, and points at the structural check that actually covers
it.

---

## §9 — Cross-artefact generality findings (consolidated)

**The single V2/CH2 §8.1 REVISE (F10) is CLOSED, verbatim and orphan-free:**
- The neutrality-grep alphabet is now the full canonical `Json|CssL4|(GoogleSheets|Sheets)\w*|Bbnf\w*`
  (`GoogleSheets` un-abbreviated — I verified `LOCKS.md` item 14's own greps use `GoogleSheets\w*`,
  and `Sheets\w*` does NOT match `GoogleSheets =>`; `Bbnf` carried for SK-V19) across BOTH
  `skinny/crates/codegen/src` AND `skinny/xtask/src`, paired with the grammar-named-*type* census
  `JsonParser|CssL4Parser|GoogleSheetsParser|BbnfBootstrap`. Bound at SYNTHESIS:201/:423–424, αE:105/
  :134/:185/:214, HANDOFF inv.5 :234–251, αB:13/:289–293. The fold matches the canon exactly.

**What is RIGHT and load-bearing (do NOT churn):** the `SinkOnlyProgram`/`BackendShape` 5-shape
lowering vehicle (the generalization mechanism — verified `lower/*.rs` present); the falsifiability
triple with the mutation-test falsifier; the verbatim-blob/single-emitter retirement clauses (fork
+ blobs verified live at `grammar_provider.rs:40–42` / `runtime_generator.rs:91,195`); the P3
collapse-default with provenance binding (the real data-table defense); the
Sheets-as-generation-not-throughput bar; the no-paper-close/no-stub-prove honest-finding
discipline; the phantom-`G`-axis-not-`K`-axis precision + the V3 `CssEventGrammar`-absent
refinement (DELETE-is-default); the adopt-existing-Pratt sourcing (litmus non-hollow by
construction). The Sheets 3rd-grammar proof is genuinely load-bearing: a real 185-LOC Pratt
grammar with `error_literal`/`cell_ref`/precedence-tower shapes that NO JSON/CSS rule exercises
exists and is adopted; the litmus cannot be satisfied by a third JSON.

**One NEW REVISE (§8): the arm-census regex's neutral-identifier-data-table reach claim.** It is
the SAME md5/grep-necessary-not-sufficient theme one level deeper: the widened-root arm-census grep
cannot fire on a neutral-identifier `RuntimeTarget` strategy table (empirically NO MATCH at
`regen_css.rs:35`); the real defense is the P3 collapse close-gate, which the contract has but
mis-attributes. Not orphaned — the fix is concrete (§8 (1)–(3): scope the grep claim honestly, bind
the structural row-count check to the relocated-seam pre-block, keep the grep for its real value).

**Zero REJECTs.** No section proposes a grammar-specific intervention, re-opens a generality
pre-block, or claims generalization on a hollow proof.

---

## §10 — Disposition ledger

| Artefact | ACCEPT | REVISE | REJECT | V2 (for comparison) |
|---|---|---|---|---|
| αA results-extraction | 3 | 0 | 0 | 3 / 0 / 0 |
| αB competitor-deltas | 2 | 0 | 0 | 2 / 0 / 0 |
| αC redress-digest | 4 | 0 | 0 | 5 / 0 / 0 |
| αD validated-invalidated | 4 | 0 | 0 | 4 / 0 / 0 |
| αE candidate-shortlist | 6 | 0 | 0 | 6 / 0 / 0 |
| SYNTHESIS.md | 6 | 0 | 0 | 6 / 0 / 0 |
| HANDOFF.md | 4 | 0 | 0 | 4 / 0 / 0 |
| §8 cross-artefact (arm-census regex reach over-claim) | 0 | 1 | 0 | (V2 §8.1 grep alphabet — now CLOSED) |
| **Total** | **29** | **1** | **0** | 30 / 1 / 0 |

Accept rate 29/30 = **96.7%** (above the §3Z ≥95% bar; V2 was 96.8%, V1 75.0%). The single V2
REVISE (grep alphabet + scan roots + type census) folded verbatim and orphan-free, verified live
against `LOCKS.md` item 14. The single V3 REVISE (§8) is a one-level-deeper sharpening of the SAME
grep-necessary-not-sufficient theme: the widened-root arm-census regex's claim to catch a
neutral-identifier metadata data-table is empirically false (NO MATCH at `regen_css.rs:35`), the
real defense is the P3 collapse close-gate the contract already has, and the fix is to correct the
attribution across five sites (concrete three-part fix §8 (1)–(3)). The goalset's Lock-14 spine is
sound; the Sheets 3rd-grammar proof is load-bearing (real 185-LOC Pratt grammar adopted); the
interventions are grammar-neutral; they will work for CSS L4 (scalar, no kernel to preserve) and
GoogleSheets (Pratt is the honest stress with an honest-finding fallback). CH2 expects a V4
confirming cycle to close §8 by scoping the grep claim honestly + binding the structural row-count
defense, reaching ≥95% × 2 consecutive.

TALLY accept=29 revise=1 reject=0
