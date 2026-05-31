# CH2 — GENERALITY (V2)

Lens: CH2 Generality (PASS-ALPHA §3). Reviewer focus: **does the goalset respect Lock 14
(one generator ALL grammars); are the interventions grammar-neutral; will they work for
non-JSON grammars (CSS L4 / Sheets / BBNF-self); is the Sheets 3rd-grammar proof
load-bearing?** Subject: SK-V18 = the GENERALIZATION cycle (the inflection backtrack).
Date 2026-05-31. Bracket HEAD `318d9c046`; SK-V17 close `f6a38445b`; V3 audit `7dbe44c22`.

Method: this is the **second** hardening iteration. CH2/V1 (`../V1/CH2.md`) dispositioned
**24 ACCEPT / 8 REVISE / 0 REJECT** (75.0%) across three cross-artefact generality folds:
(1) Sheets sourcing under-specified; (2) distinct-grammar-output md5 necessary-not-sufficient;
(3) canonical Lock-14 three-surface model never cited. The α-A..F artefacts were
**re-authored after V1** (alpha mtimes 13:49–13:51 vs V1 13:44–13:46) carrying explicit
V1→V2 FOLD LEDGERs. V2's job: verify the folds landed, are orphan-free, and re-disposition.
Every disposition cites `path:line` / SHA / artefact-line, verified live at HEAD.

---

## §0 — Lens verdict (one paragraph)

**The three V1/CH2 generality folds all landed, verbatim and orphan-free; the contract is
now a Lock-14-clean generalization goalset.** Verified at HEAD: (1) every Sheets reference
across αA/αD/αE/SYNTHESIS/HANDOFF now names the REAL source `grammar/google-sheets/
google-sheets.bbnf` — confirmed present (`wc -l` = **185**), confirmed a *genuinely different
shape* (`error_literal` `#N/A`/`#REF!`/`#DIV/0!` at `:34-37`, `cell_ref` `:63`, explicit
"operator precedence tower" `:92` — a Pratt formula grammar no JSON/CSS rule exercises),
and the skinny-tree-adoption obligation (new grammar root + xtask `RuntimeTarget`, since
`skinny/grammars/` has only `json.bbnf` today) is now an explicit PROVE-wave owner item with
an honest-finding fallback if the generator cannot lower Pratt. (2) The md5-necessary-not-
sufficient gap is closed: G3 + PROVE close conditions now bind the canonical `match grammar`-
arm grep `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Sheets\w*\s*=>' skinny/crates/
codegen/src` → 0 (`SYNTHESIS:172,176`), a new telemetry column `generator_grammar_branch_count
== 0` co-gates the gate consumer (`SYNTHESIS:392,412,425`), and the gate REJECTS a
grammar-branching body even when md5-distinct (`SYNTHESIS:425-426`). (3) The canonical
Lock-14 three-surface model is now cited verbatim — G3 names "(a) `<name>.bbnf`, (b)
workspace metadata, (c) optionally a per-grammar decl crate" (`SYNTHESIS:172`) and HANDOFF
invariant 5 binds BOTH the forbidden-token scan AND the canonical arm census (`HANDOFF:224-234`).
The αC P3 collapse-vs-differentiate ambiguity (V1/CH2 §3.5) is also resolved: COLLAPSE-to-one
is now the DEFAULT, differentiate admissible ONLY if N distinct `.bbnf` roots are authored,
cosmetic divergence explicitly REJECTed (`alphaC:143-156`). **Zero REJECTs. One residual
REVISE** (the only open generality item): the artefacts' neutrality grep alphabet (`Json|CssL4|
Sheets`) is narrower than the canonical LOCKS.md:349 alphabet (`Json|CssL4|Bbnf|GoogleSheets`)
AND omits the workspace-metadata surface (b) as a scanned root — a `match grammar` could hide
behind a neutral-identifier *strategy table* in xtask metadata that neither grep catches. This
is a sharpening, not a refutation; the goalset's spine is Lock-14-correct.

---

## §1 — αA results-extraction (generality lens)

### §1.1 — §0 substrate-generalizes / value-API-does-not split → **ACCEPT** (V1 ACCEPT held)
`alphaA:13-29`. The substrate (Lock 1) generalizes and is the foundation; value-API + codegen
do NOT yet — verified live (`tape/mod.rs:94` Tape, `:175` ValueRef, `:38` PayloadArena; one
tape both grammars). Unchanged from V1. ACCEPT.

### §1.2 — §phantom-G-axis precision (`G` vs `K`) → **ACCEPT** (V1 ACCEPT held, now sharper)
`alphaA:181`. The artefact carries the load-bearing distinction VERIFIED live at
`tape/mod.rs:175`: `pub struct ValueRef<'doc,'input:'doc, K = AnyKind, G: EventGrammar =
AnyGrammar>` — TWO generic axes. `K=Kind` is real (instantiated); `G: EventGrammar` is the
phantom (always `AnyGrammar`, witnesses inert). G4 must instantiate-or-delete the RIGHT axis
(`G`, not `K`). This precision is now carried through SYNTHESIS (`:393` "the `G: EventGrammar`
axis … NOT the already-real `K=Kind` axis"). ACCEPT.

### §1.3 — §5.3 Sheets close-condition seed (V1/CH2 §1.4 REVISE) → **ACCEPT** (fold landed)
`alphaA:225-240`. V1/CH2 §1.4 REVISEd this for naming no Sheets source. **Fold verified:**
αA §3 now states the source is `grammar/google-sheets/google-sheets.bbnf`, EXISTS in totality
not skinny (`:231`), that the skinny tree consumes only `skinny/grammars/json.bbnf` +
`grammar/css/l4/stylesheet.bbnf` (verified live: `find skinny -name '*.bbnf'` → only
`json.bbnf`; CSS via `regen_css.rs`), and that bringing Sheets into the benched tree (new
skinny grammar root + xtask Sheets `RuntimeTarget`) is "itself part of the litmus, not a given"
(`:237`). Exactly the V1 fix. ACCEPT.

**αA tally: ACCEPT ×3, REVISE ×0.** (V1 REVISE ×1 → folded to ACCEPT.)

---

## §2 — αB competitor-deltas (generality lens)

### §2.1 — §4 Sheets-as-GENERATION-not-throughput bar → **ACCEPT** (V1 ACCEPT held)
`alphaB:276-283`. The three-grammar bar table holds: JSON strict-vs-strict, CSS lazy-vs-eager
(framed), **Sheets has NO competitor bar — its bar is GENERATION** (the ONE generator emits a
real Sheets parser from `.bbnf` with a non-identical `generated.rs`, 25-LOC stub retired). This
is the correct generality framing and the row CH2 most wants preserved — confirmed carried into
SYNTHESIS §0.5 row 3 + the `sheets_real_grammar`/`generator_grammar_count==3` telemetry. ACCEPT.

### §2.2 — §1.4 typed-rows-conditional caveat → **ACCEPT** (V1 ACCEPT held)
`alphaB:160-199`. The typed JSON bar rides a per-corpus hand-tuned schema that does NOT
generalize; `parse_only` is the unconditional grammar-general bar. The V2 FOLD added the N=80
vs N=200 plane discipline (`:12,160`) — a CH1 fold, generality-neutral, no regression to CH2.
ACCEPT.

**αB tally: ACCEPT ×2, REVISE ×0.** (V1: ACCEPT ×3; αB was already CH2-clean, no fold needed.)

---

## §3 — αC redress-digest (generality lens)

### §3.1 — §2.2b LayoutFacts-derive-not-hardcode admission → **ACCEPT** (V1 ACCEPT held)
`alphaC` (the BackendShape derivation clause). The emitter DERIVES tape ops from
`LayoutFacts.backend_shape ∈ {EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` — the
single most important Lock-14 generality vehicle. Unchanged from V1. ACCEPT.

### §3.2 — §2.2 relocated-overfit-seam pre-block → **ACCEPT** (V1 ACCEPT held)
The `W5C_REQUEST_FACT_PROFILES` relocated-seam pre-block (carried into `SYNTHESIS:243-246`:
"Relocating per-rule branching into projection DATA is the overfit re-entry seam and is
forbidden — every residual CSS routing entry names the `.bbnf` rule it derives from"). This is
the precise Lock-14 failure mode (an un-fork that MOVES `match grammar` into a data table is not
generalization). ACCEPT — and see §8 residual: this clause is the reason the metadata surface
(b) must itself be scanned.

### §3.3 — §1 P3 collapse-vs-differentiate (V1/CH2 §3.5 REVISE) → **ACCEPT** (fold landed)
`alphaC:143-156`. V1/CH2 §3.5 REVISEd P3 for oscillating between collapse and differentiate.
**Fold verified:** αC now states "the **DEFAULT and correct obligation is COLLAPSE-to-ONE** —
the 7 directories are demonstrably one grammar (one `.bbnf`, one entry rule)"; differentiate is
admissible "**ONLY IF** N distinct `.bbnf` roots are genuinely authored"; "absent that, 'N
distinct generated.rs' is a **hollow distinct-grammar-output target** satisfiable by cosmetic
divergence, which the diff-census addendum must REJECT" (`:146-150`). The close gate binds the
directory count itself collapsing to one CSS provider and every distinct pair tracing to a
distinct `.bbnf` (`:152-156`). This is exactly the V1 fix — and it correctly binds the
distinct-grammar-output addendum to *provenance*, not cosmetics. ACCEPT.

### §3.4 — §retirement-clause + dual-surface check → **ACCEPT** (V1 ACCEPT held)
The retirement clause binds BOTH `CSS_GENERATED_RS` (verbatim-blob) AND `RuntimeEmitterKind`
(single-emitter-path); verified live the fork still exists (`grammar_provider.rs:40-42`
`CompiledLowering`/`RequestFacts`, dispatched `:110`) and the JSON/CSS verbatim blobs exist
(`runtime_generator.rs:91` `CSS_GENERATED_RS`, `:195/:550/:572/:594` `JSON_*_RS`). The "checked
TWICE: runtime output AND emitter" corollary is the right generalization-cycle posture. ACCEPT.

### §3.5 — §EventGrammar witness-type seam (new V2 fold, CH5 C.4) → **ACCEPT**
`alphaC:174,456`. The V2 fold added the witness/`EventGrammar` grammar-named-type seam: if the
un-forked generator EMITS a grammar-named `EventGrammar` literal (`JsonEventGrammar`/
`SheetsEventGrammar`), that is a new grammar-named coupling surface and the P4 token scan must
catch it (`HANDOFF:233-234` adds `EventGrammar`/`XEventGrammar` to the `runtime_generator.rs`-
scoped forbidden tokens). This is a generality-positive addition — it closes a leak the bare
`match grammar` grep would miss. ACCEPT.

**αC tally: ACCEPT ×5, REVISE ×0.** (V1 REVISE ×1 → folded to ACCEPT; +1 new V2 clause ACCEPT.)

---

## §4 — αD validated/invalidated (generality lens)

### §4.1 — §1 DM2 substrate-ready-not-proven → **ACCEPT** (V1 ACCEPT held)
`alphaD:97`. DM2 DEMOTED to "substrate-READY, not proven" — `sheets_witness/` is a 24-line
stub (verified live: `sheets_witness/` = 2 files, 25 LOC total). The honest demotion of the
generalization CLAIM. ACCEPT.

### §4.2 — §2 I3/I4/I5 the three generality invalidations → **ACCEPT** (V1 ACCEPT held)
I3 (7 replicas), I4 (`RuntimeEmitterKind` fork), I5 (phantom `<G>` + divergent value API) —
each names its CH2 lens. Unchanged. ACCEPT.

### §4.3 — §4 S12 Sheets-litmus owner-surface (V1/CH2 §4.3 REVISE) → **ACCEPT** (fold landed)
`alphaD:122,177,227`. V1/CH2 §4.3 REVISEd S12 for naming no Sheets source. **Fold verified:**
S12 now names "`grammar/google-sheets/google-sheets.bbnf` (185-LOC Pratt operator-precedence
formula grammar)" as the source (`:122`), with on-disk citation of its distinguishing shapes
(`error_literal :34-37`, `cell_ref :63`, `cell_or_range :84`, precedence tower `:92` — all
re-verified live by me), and the honest-finding clause "if the `SinkOnlyProgram`/`BackendShape`
lowering cannot express Pratt precedence, that is a genuine §6-style finding — surface it … do
not paper-close." The V2 FOLD log (`:227`) attributes this to CH2 §4.3. Exactly the V1 fix,
and it STRENGTHENS the litmus (a real different-shape grammar exists). ACCEPT.

### §4.4 — §5 pre-blocked-routes (no-second-substrate G4) → **ACCEPT** (V1 ACCEPT held)
The "no second substrate" clause (carried `SYNTHESIS:283-287`, `HANDOFF:199-202`) protects
generality: the shared trait must not fork the substrate. ACCEPT.

**αD tally: ACCEPT ×4, REVISE ×0.** (V1 REVISE ×1 → folded to ACCEPT.)

---

## §5 — αE candidate-shortlist (generality lens — the load-bearing artefact)

αE carries the most explicit V1→V2 FOLD LEDGER (`alphaE:10-25`, F1–F8). The two CH2 folds are
F2 (Sheets source named) + F3 (md5 necessary-not-sufficient → grammar-neutral-body grep).

### §5.1 — §0 falsifiability triple → **ACCEPT** (V1 ACCEPT held; F3 sharpened)
`alphaE:29-37`. The triple (PRESERVED->SOTA / GRAMMAR-DERIVATION-PROOF / DISTINCT-GRAMMAR-OUTPUT)
holds; gate #3 now carries F3 inline (`:35`): "md5-distinct is NECESSARY-NOT-SUFFICIENT — the
generator body must ALSO be grammar-neutral (no `match grammar` arms), gated by the canonical
Lock-14 grep." The mutation-test falsifier (gate #2) remains an excellent operational
discriminator. ACCEPT.

### §5.2 — CANDIDATE B1 un-fork + JSON projection → **ACCEPT** (V1 ACCEPT held)
`alphaE:77-93`. Gate #3 (SINGLE-EMITTER-PATH, `:90`) now adds the companion grammar-neutral grep
`rg 'Json\s*=>\|CssL4\s*=>' skinny/crates/codegen/src` → 0. Targets the verified defect
(`runtime_generator.rs:35,38,40,43` push the `JSON_*_RS` blobs verbatim). ACCEPT.

### §5.3 — CANDIDATE B2 CSS lowering → **ACCEPT** (V1 ACCEPT held)
`alphaE:97-122`. Gate #3 (`:119`) now carries the neutral-body grep AND the differentiate-fallback
("if P3 chose differentiate, the N CSS profiles must point at distinct `.bbnf` roots") — which
αC §3.3 resolved to COLLAPSE-default. LOW risk (scalar hot path, no kernel to preserve — verified
the CSS scan kernels `find_css_significant`/`find_comment_close` are `#[cfg(test)]`-only). ACCEPT.

### §5.4 — CANDIDATE B3 shared trait + phantom → **ACCEPT** (V1 ACCEPT held; F6/F7 sharpened)
`alphaE:126-147`. The V2 folds added F6 (DELETE is the DEFAULT per abrogate-before-patch;
INSTANTIATE is burden-of-proof because `CssEventGrammar` does NOT exist at HEAD — verified: only
`JsonEventGrammar` + `SheetsEventGrammar` witnesses exist; the test-excluded grep `:140`) and F7
(preserve-rich-ast structural gate: JSON's `get(key)`+typed-`Kind`+visitor must remain reachable
THROUGH the shared trait, a both-impl grep is necessary-not-sufficient `:142`). Both are
generality-positive — they guard the LCD-flatten false-green. ACCEPT.

### §5.5 — CANDIDATE B4 Sheets-litmus (V1/CH2 §5.5 REVISE, the load-bearing disposition) → **ACCEPT** (both folds landed)
`alphaE:151-176`. V1/CH2 §5.5 REVISEd B4 on two generality gaps. **Both folds verified:**
- **F2 (Sheets source named):** `alphaE:157` now names `grammar/google-sheets/google-sheets.bbnf`,
  flags it lives in `grammar/` NOT the benched skinny tree, adds the PROVE owner obligations
  "(a) name the source, (b) wire a skinny Sheets grammar root + an xtask `RuntimeTarget`, (c)
  acknowledge Pratt operator-precedence + error literals + cell references — shapes NO JSON/CSS
  rule exercises," with the honest-finding clause for Pratt-lowering failure. Exactly the V1 fix.
- **F3 (md5 necessary-not-sufficient):** `alphaE:170` gate #1 now reads "md5-distinct is
  NECESSARY-NOT-SUFFICIENT … AND the generator body is grammar-neutral: `rg 'Json\s*=>\|CssL4\s*=>\|
  Sheets\w*\s*=>' skinny/crates/codegen/src` → 0 (the canonical Lock-14 command; three distinct
  files produced by a generator that still `match grammar { Json => …, Css => …, Sheets => … }`
  internally is the actual Lock-14 violation md5-distinctness cannot catch)." Exactly the V1 fix.

Both V1/CH2 §5.5 items are closed verbatim. ACCEPT.

### §5.6 — CANDIDATE A (PRUNE) generality dependency → **ACCEPT** (V1 ACCEPT held)
`alphaE:196`. P4 (Lock-14 gate meaningful) entry-gates B1 — build the neutrality gate before the
thing it must scan. Right generality sequencing. ACCEPT.

**αE tally: ACCEPT ×6, REVISE ×0.** (V1: ACCEPT ×5 + REVISE ×1 [B4] → folded to ACCEPT;
the triple + B1/B2/B3/B4/A all clean.)

---

## §6 — SYNTHESIS.md (generality lens — the goalset)

### §6.1 — §0.1 close-condition gates G1–G4 + PROVE → **ACCEPT** (V1 ACCEPT held)
`SYNTHESIS:170-176`. Every generalization gate structurally falsifiable. ACCEPT.

### §6.2 — §0.4 pre-blocks (verbatim-blob / phantom / distinct-output / relocated-seam) → **ACCEPT** (V1 ACCEPT held)
`SYNTHESIS:243-264`. The relocated-seam pre-block ("every residual CSS routing entry names the
`.bbnf` rule it derives from", `:246`) + the three new re-entry pre-blocks + no-second-substrate
(`:283-287`). ACCEPT.

### §6.3 — §0.5 generalization litmus table → **ACCEPT** (V1 ACCEPT held)
`SYNTHESIS:297-303`. Binary-structural per-axis table with no-stub-prove fallbacks
("if Sheets cannot be emitted via the generator only: the generalization is NOT real — surface
honestly, do NOT stub-prove", `:301`). ACCEPT.

### §6.4 — Lock-14 canonical-model citation (V1/CH2 §6.4 REVISE) → **ACCEPT** (fold landed)
`SYNTHESIS:172,176`. V1/CH2 §6.4 REVISEd for never citing the canonical three-surface model.
**Fold verified:** G3 now reads "ONE grammar-agnostic emitter path emits every grammar per the
**canonical Lock-14 three-surface model** (`LOCKS.md` item 14: every grammar plugs in via (a)
`<name>.bbnf`, (b) workspace metadata declaring its strategy, (c) optionally a per-grammar decl
crate — generic crates carry ZERO `match grammar { Json => …, CssL4 => … }` arms) … AND the
canonical grammar-neutrality grep is 0: `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|
Sheets\w*\s*=>' skinny/crates/codegen/src` returns ZERO (md5-distinctness alone is
necessary-not-sufficient)." I verified the cited canonical text exists verbatim at LOCKS.md:349.
The fold is exact. ACCEPT.

### §6.5 — Section 2 telemetry generality columns (V1/CH2 §6.5 REVISE) → **ACCEPT** (both columns landed)
`SYNTHESIS:392,399`. V1/CH2 §6.5 REVISEd for two missing columns. **Both folds verified:**
- `generator_grammar_branch_count` (integer, must be 0 — "the canonical Lock-14 `match grammar
  { Json=>/CssL4=>/Sheets=> }` arm census in `skinny/crates/codegen/src`; md5-distinctness is
  necessary-not-sufficient, this is the neutral-emitter co-gate", `:392`) — bound to the gate
  consumer (`:412`) and the REJECT list ("`generator_grammar_branch_count > 0` (a grammar-branching
  emitter body even when md5-distinct)", `:425`).
- `sheets_grammar_shape` (enum pratt-operator / flat-stream / tree, "`google-sheets.bbnf` is
  `pratt-operator` (a third JSON/flat-stream would hollow the litmus)", `:399`) — bound to the gate
  consumer (`:417`) and the REJECT list ("`sheets_grammar_shape ∈ {flat-stream,tree}` on a Sheets
  claim (third-JSON hollowing)", `:428`).
Both V1 columns added, machine-checkable, gate-consumed. ACCEPT.

### §6.6 — §0.3 receiver Sheets sourcing (V1/CH2 §6.6 REVISE) → **ACCEPT** (fold landed)
`SYNTHESIS:222`. V1/CH2 §6.6 REVISEd "author a Sheets `.bbnf`" when one already EXISTS. **Fold
verified:** §0.3 PROVE now reads "ADOPT the EXISTING `grammar/google-sheets/google-sheets.bbnf`
(a genuinely-different Pratt formula grammar — STRENGTHENS the litmus; do NOT author a fresh
minimal stub that risks producing 'a third JSON' and hollowing the litmus, per alphaE §142).
Bring it into the benched skinny tree (a new skinny grammar root + xtask target — today it lives
in the totality tree only)." The "author" → "adopt the existing Pratt grammar" inversion is the
exact V1 fix. ACCEPT.

**SYNTHESIS tally: ACCEPT ×6, REVISE ×0.** (V1: ACCEPT ×3 + REVISE ×3 → all 3 folded to ACCEPT.)

---

## §7 — HANDOFF.md (generality lens)

### §7.1 — §What-SK-V18-Opens backlog (G1–G6 + PROVE) → **ACCEPT** (V1 ACCEPT held)
`HANDOFF:70-127`. The backlog maps each item to its V3 finding id + CH2 lens; PROVE
(`:113-122`) adopts the existing Pratt grammar with the no-stub-prove litmus. ACCEPT.

### §7.2 — §Gate-Posture six CHALLENGE addenda → **ACCEPT** (V1 ACCEPT held)
`HANDOFF:159-170`. The six addenda carried verbatim. ACCEPT.

### §7.3 — §Inviolable-invariants #5 grammar-neutral (V1/CH2 §7.3 REVISE) → **ACCEPT** (fold landed)
`HANDOFF:224-234`. V1/CH2 §7.3 REVISEd invariant 5 for binding only the forbidden-token scan,
not the `match grammar`-arm census. **Fold verified:** invariant 5 now cites the canonical
three-surface model (`:224-226`) AND binds BOTH "(i) the `GENERIC_SCAN_ROOTS` forbidden-token
scan AND (ii) the canonical `match grammar`-arm grep (`rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|
CssL4\s*=>|Sheets\w*\s*=>' skinny/crates/codegen/src` → 0) — they catch different leaks: a token
scan misses a `match grammar` using neutral identifiers; the arm census misses a `CSS_GENERATED_RS`
const" (`:227-231`), plus the `EventGrammar` witness-token addition (`:233-234`). Exactly the V1
fix, and the "different leaks" rationale is precisely correct. ACCEPT.

### §7.4 — §Next-Move S-P3 wave sequencing → **ACCEPT** (V1 ACCEPT held)
`HANDOFF:249-265`. PRUNE → GENERALIZE → PROVE → HONESTY with P4-before-G2/G3 + same-wave-
consumer-per-primitive. The V2 fold added the revert dependency graph + hard-cap defaults
(`:287-297`) — generality-neutral, no regression. ACCEPT.

**HANDOFF tally: ACCEPT ×4, REVISE ×0.** (V1: ACCEPT ×3 + REVISE ×1 → folded to ACCEPT.)

---

## §8 — Residual generality finding (the single open REVISE)

### §8.1 — Neutrality grep alphabet + metadata-surface coverage → **REVISE** (SYNTHESIS §0.1 G3 / §0.5 / HANDOFF inv.5)

The canonical Lock-14 verification at `LOCKS.md:349` runs **two** distinct surfaces:
(1) `rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' …/src/` → 0 (grammar-named
*types*), and (2) `rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*
\s*=>'` → 0 (`match grammar` *arms*) — and crucially scopes its arm-census alphabet to **FOUR**
grammars: `Json | CssL4 | Bbnf | GoogleSheets`.

The contract's adopted grep (`SYNTHESIS:172,392,425`; `alphaE:170`; `HANDOFF:229`) uses
`Json\s*=>|CssL4\s*=>|Sheets\w*\s*=>` — it (a) abbreviates `GoogleSheets` to `Sheets` (the
`\w*` prefix-anchor `Sheets\w*` would MISS a `GoogleSheets =>` arm because the canonical name is
`GoogleSheets`, and would also miss a bare `Sheet =>`), and (b) omits the `Bbnf` arm. Two
consequences for generality:

1. **A `GoogleSheets =>` arm escapes the grep.** The verified Sheets witness type is
   `SheetsEventGrammar` (`sheets_witness/event_grammar_witness.rs`) — but the canonical Lock-14
   grammar name is `GoogleSheets` (LOCKS.md:349). A generator that branches `match g {
   GoogleSheets => emit_sheets() }` would pass `Sheets\w*\s*=>` ONLY if the arm literally reads
   `Sheets…` — it does NOT match `GoogleSheets =>` (the regex `Sheets\w*` requires the token to
   START with `Sheets`). The grep must anchor on `(GoogleSheets|Sheets)\w*\s*=>` or drop the
   start-anchor.

2. **The workspace-metadata surface (b) is never scanned for a relocated branch.** The canonical
   model's surface (b) is "workspace metadata declaring its strategy" — in the skinny tree that is
   `skinny/xtask/src/{regen.rs,regen_css.rs,main.rs}` (the `RuntimeTarget`/`source_roots`/
   `entry_rule` tables, verified). The relocated-overfit-seam pre-block (`alphaC §3.2`,
   `SYNTHESIS:246`) explicitly forbids "relocating per-rule branching into projection DATA" — yet
   the neutrality grep is scoped ONLY to `skinny/crates/codegen/src`, NOT to the xtask metadata
   tables where a neutral-identifier strategy table (e.g. a `RuntimeTarget` array with per-grammar
   profile rows) could carry the very `match grammar` semantics in data form. P4 extends
   `GENERIC_SCAN_ROOTS` to `runtime_generator.rs` + templates but the artefacts never add the
   xtask metadata tables to the `match grammar`-arm-census root set. This is the exact Lock-14
   failure mode (data-table relocation) the contract elsewhere polices but does not gate here.

**Fix:** (i) the canonical grep alphabet in G3 / §0.5 / `generator_grammar_branch_count` column /
HANDOFF inv.5 must read `Json\s*=>|CssL4\s*=>|(GoogleSheets|Sheets)\w*\s*=>|Bbnf\w*\s*=>` (full
canonical alphabet, `GoogleSheets` un-abbreviated, `Bbnf` included for SK-V19 forward-safety);
(ii) the neutrality-scan root set must include the xtask workspace-metadata surface
(`skinny/xtask/src/`), not only `skinny/crates/codegen/src`, so a per-grammar strategy table in
metadata cannot carry a relocated branch the codegen-scoped grep misses; (iii) add the canonical
grammar-named-*type* grep (`rg 'JsonParser|CssL4Parser|GoogleSheetsParser'`) as the second
canonical surface — invariant 5 binds the arm census but not the type census, and the
`SheetsEventGrammar`/`JsonEventGrammar` witnesses are exactly grammar-named types that a
generated emitter could re-emit. This is REVISE-level (a verification-surface sharpening); the
goalset's Lock-14 spine is correct.

---

## §9 — Cross-artefact generality findings (consolidated)

**The three V1/CH2 cross-artefact REVISE themes are all CLOSED:**

1. **Sheets sourcing** (V1: αA §5.3, αD S12, αE B4, SYNTHESIS §0.3 / §0.6) — CLOSED. Every Sheets
   reference now names `grammar/google-sheets/google-sheets.bbnf` (verified 185 LOC, Pratt
   formula grammar with `error_literal`/`cell_ref`/precedence tower), mandates skinny-tree
   adoption as a PROVE owner obligation, flags Pratt-lowering as the generality stress + honest-
   finding candidate, and adds the `sheets_grammar_shape == pratt-operator` telemetry guard
   against third-JSON hollowing. The litmus is now non-hollow by construction.

2. **Distinct-grammar-output md5 necessary-not-sufficient** (V1: αE §5.5, SYNTHESIS §6.4/§6.5) —
   CLOSED. The canonical `match grammar`-arm grep is bound to G3 + PROVE + B4 gate #1; the
   `generator_grammar_branch_count == 0` telemetry column gate-consumes it; the gate REJECTS a
   grammar-branching body even when md5-distinct. (Residual §8: the grep ALPHABET and SCAN ROOTS
   need widening — REVISE.)

3. **Canonical Lock-14 three-surface model uncited** (V1: SYNTHESIS §6.4, HANDOFF §7.3) — CLOSED.
   The three-surface model (`<name>.bbnf` + workspace metadata + optional decl crate; ZERO
   `match grammar` arms) is cited verbatim in G3 (`SYNTHESIS:172`) and invariant 5 binds BOTH the
   token scan AND the arm census with the correct "different leaks" rationale.

**Additional V1/CH2 §3.5 fold (P3 collapse-vs-differentiate)** — CLOSED. COLLAPSE-to-one is the
DEFAULT; differentiate admissible ONLY with authored distinct `.bbnf` roots; cosmetic divergence
REJECTed (`alphaC:143-156`).

**What is RIGHT and load-bearing (do NOT churn):** the `SinkOnlyProgram`/`BackendShape` 5-shape
lowering vehicle (the generalization mechanism); the falsifiability triple with the mutation-test
falsifier; the relocated-overfit-seam + verbatim-blob + single-emitter retirement clauses; the
Sheets-as-generation-not-throughput bar; the no-paper-close / no-stub-prove honest-finding
discipline (now gated (a)-(c) per CH6, `SYNTHESIS:181`); the phantom `G`-axis-not-`K`-axis
precision; the DELETE-is-default abrogate-before-patch posture on `<G>`. The goalset's spine
respects Lock 14.

**One REVISE (§8): the neutrality grep alphabet + scan-root coverage.** It is the SAME structural
theme as V1 (md5 necessary-not-sufficient) carried one level deeper: V1 closed "add the arm
census"; V2 finds the arm census as-written has an alphabet gap (`GoogleSheets` un-matched by
`Sheets\w*`) and a scan-root gap (xtask metadata surface (b) unscanned). Not orphaned — the fix
is concrete (§8.1 (i)-(iii)).

**Zero REJECTs.** No section proposes a grammar-specific intervention, re-opens a generality
pre-block, or claims generalization on a hollow proof.

---

## §10 — Disposition ledger

| Artefact | ACCEPT | REVISE | REJECT | V1 (for comparison) |
|---|---|---|---|---|
| αA results-extraction | 3 | 0 | 0 | 3 / 1 / 0 |
| αB competitor-deltas | 2 | 0 | 0 | 3 / 0 / 0 |
| αC redress-digest | 5 | 0 | 0 | 4 / 1 / 0 |
| αD validated-invalidated | 4 | 0 | 0 | 3 / 1 / 0 |
| αE candidate-shortlist | 6 | 0 | 0 | 5 / 1 / 0 |
| SYNTHESIS.md | 6 | 0 | 0 | 3 / 3 / 0 |
| HANDOFF.md | 4 | 0 | 0 | 3 / 1 / 0 |
| §8 residual (grep alphabet + scan roots) | 0 | 1 | 0 | n/a |
| **Total** | **30** | **1** | **0** | 24 / 8 / 0 |

Accept rate 30/31 = **96.8%** (above the §3Z ≥95% bar; V1 was 75.0%). All 8 V1/CH2 REVISEs
folded verbatim and orphan-free (verified at HEAD); the single V2 REVISE (§8) is a one-level-
deeper sharpening of the SAME md5-necessary-not-sufficient theme — the arm-census grep's
alphabet (`GoogleSheets` un-matched by `Sheets\w*`, `Bbnf` omitted) and scan roots (xtask
workspace-metadata surface (b) unscanned) — carrying a concrete three-part fix (§8.1). The
goalset's Lock-14 spine is sound; the Sheets 3rd-grammar proof is load-bearing (a real 185-LOC
Pratt grammar exists and is adopted); the interventions are grammar-neutral; they will work for
CSS L4 (scalar, no kernel to preserve) and Sheets (Pratt is the honest stress). CH2 expects V3
to close §8 by widening the grep alphabet + scan-root set, reaching ≥95% × 2 consecutive.

TALLY accept=30 revise=1 reject=0
