# CH2 GENERALITY — Pass Alpha SK-V17 (cycle V1)

Lens: CH2 Generality (PASS-ALPHA §3). Reviewer focus: does the goalset + every
candidate respect **Lock 14 grammar-neutrality** — works for JSON / Sheets /
BBNF-self, not just CSS, OR is honestly re-framed as a per-grammar template
surface; is the unified tape / layout / projection grammar-general?

Host: aarch64 Apple M5 Max only. HEAD of record: `1c5bd7a25`. Every disposition
cites `path:line` / measured fact.

Subjects reviewed: `research/alpha/alphaA-results-extraction.md`,
`alphaB-competitor-deltas.md`, `alphaC-redress-digest.md`,
`alphaD-validated-invalidated.md`, `alphaE-candidate-shortlist.md`,
`SYNTHESIS.md`, `HANDOFF.md`. (No `alphaF-contract-draft.md` exists at the
`research/alpha/` path; αF's output is the `SYNTHESIS.md` + `HANDOFF.md` per
PASS-ALPHA §2 α-F — those are reviewed in its stead.)

---

## §0 — Lock 14 reference (the bar this lens enforces)

`restart/locks/LOCKS.md:603` (Lock 14 grammar-generalisation clause), load-bearing
phrases:

1. "generic codegen **may not hand-code** `RuntimeGenerationMode`, profile arrays,
   CSS profile matches, JSON/CSS runtime families, JSON punctuation or role mining,
   generic grammar switches, or generic-crate grammar branches."
2. "Future grammar onboarding is **source/metadata-only through CSS plus Sheets or
   BBNF-self witnesses**; no new directive, BIR variant, sixth BackendShape, or
   generic code branch is admissible."
3. "**CSS value parsing may reuse byte kernels below a CSS scalar oracle, but JSON
   string/number semantic APIs are not the CSS parser.**"

CH2 holds every candidate + goalset section against these three phrases.

---

## §1 — The two load-bearing generality defects found

Before the per-section dispositions, two cross-cutting findings drive several
REVISE dispositions. Both are *generality* failures: the goalset's
grammar-neutrality claim rests on surfaces that are either the wrong tree or do
not exist in the benched tree.

### Finding A — SYNTHESIS/HANDOFF regress αE's tree-correction; the named activation substrate is the CORE tree, not the benched SKINNY tree

αE got this exactly right (`alphaE-candidate-shortlist.md:37-51`): in **skinny**
there is no `StructLayout`, no `OpenFrame`, no `TapeStructBuilder`; the architecture
doc's `crates/core/src/runtime/...` + `bbnf_ir::registry::struct.rs StructLayout`
citations are the **TOTALITY core tree, not the benched skinny tree**. Verified:

- `grep StructLayout skinny/crates/` → **empty**; `struct StructLayout` lives only
  at `crates/ir/src/registry/struct.rs` (core tree).
- `TapeStructBuilder` appears only in `crates/core/src/runtime/tape/{mod,record}.rs`
  + `crates/core/tests/tape_substrate.rs` (core tree). **Zero** hits in `skinny/`.
- The benched skinny JSON tape is a DIFFERENT substrate:
  `skinny/crates/runtime/src/tape/` exposing `TapeBuilder` / `ValueRef` /
  `DocumentView` (`skinny/.../json/parser.rs:5`, `json/value.rs:4`, `json/view.rs:6`).
  JSON's lazy cursor is `value_from_ref` at `skinny/.../json/value.rs:143` — that IS
  the benched proof model.

**SYNTHESIS.md regresses on αE's correction.** SYNTHESIS §0.1 "Tape activation"
(`SYNTHESIS.md:55`) names `crates/core/src/runtime/tape/`; §0.1 "Layout-driven
projection" (`SYNTHESIS.md:56`) names `bbnf_ir::registry::struct.rs LayoutKind +
FieldSource`; §0.3 receiver (`SYNTHESIS.md:84-85`) names `(TapeCursor,
&StructLayout, source)` and `regen_css.rs emit_builder`/`OpenFrame template`;
HANDOFF (`HANDOFF.md:13,33`) names `crates/core/src/runtime/tape/` and
`TapeStructBuilder`. **These are core-tree symbols.** The benched roster
(`skinny/crates/bbnf-bench`) measures the **skinny** tree. A goalset whose
activation target is `TapeStructBuilder`/`StructLayout` is targeting a substrate the
benched corpus does not run.

Why this is a CH2 (not only CH1) defect: the grammar-neutrality argument is "JSON
already rides this same substrate/projection, therefore the model is grammar-general"
(SYNTHESIS §0.1 JSON-guard row `:54`; αD O1/O2 `:79-80`). That argument is only
sound if the substrate being activated for CSS is the **same** substrate JSON rides.
JSON rides `skinny/.../tape::{TapeBuilder,ValueRef}`. The SYNTHESIS names
`core/.../tape::TapeStructBuilder`. **The "JSON proves the generality" claim is
attached to the wrong substrate**, so the generality is unverified for the named
target. Either (a) the goalset must name the benched skinny substrate
(`skinny/crates/runtime/src/tape/`, `TapeBuilder`, `ValueRef`, `value_from_ref`) and
the skinny lowering equivalent (`skinny/crates/codegen/src/lower/{offset_tape,
tape_plan}.rs`, `BackendRule`, αE's exact framing), or (b) the goalset must declare
explicitly that SK-V17 ports the core-tree tape down into skinny — a much larger,
differently-bounded task that no candidate's LOC budget covers.

### Finding B — the "JSON/CSS/Sheets/BBNF-self" generality witnesses are a 25-line stub + an absent grammar

Lock 14 phrase #2 admits grammar generality through "**CSS plus Sheets OR
BBNF-self witnesses**." The artefacts repeatedly assert generality across all four:
SYNTHESIS §0.1 layout-projection `:56` ("grammar-neutral by construction across
JSON/CSS/Sheets/BBNF-self"), §0.1 foldable `:64`, αD O1/O2 `:79-80`
("identical across JSON/CSS/sheets/bbnf by construction"), αE §3 `:343-346`.
Verified benched reality:

- `ls skinny/crates/runtime/src/grammars/` → `json`, 7× `css_l4_*`, **`sheets_witness`**.
  **No `bbnf-self` runtime grammar exists** (`grep bbnf_self|BbnfSelf` in
  `skinny/crates/runtime/src/` → only string mentions in `bbnf-bench/src/report.rs`
  and `bin/gate.rs`, not a grammar).
- `sheets_witness` is a **25-line stub**: `event_grammar_witness.rs` (24 lines) +
  `mod.rs` (1 line). It is not a value-producing grammar that exercises the
  lazy-view projection over a non-trivial typed CSSOM-analogue.

So the only witness that can actually exercise a tape+projection in the benched tree
is JSON (real) and CSS (the subject). Sheets is a 25-line event-grammar stub;
BBNF-self is absent. **The claim "grammar-neutral by construction across four
grammars" is demonstrable for at most two (JSON + CSS).** Lock 14 is satisfiable
(CSS + Sheets-witness is the named minimum), but the goalset must NOT assert
demonstrated generality across BBNF-self, and must either (a) downgrade the claim to
"grammar-neutral by construction (witnessed on JSON; CSS is the new rider; Sheets
witness is a stub, BBNF-self is future)", or (b) require the lazy-view generator to
emit for `sheets_witness` as a non-CSS exercise so the generality is *tested*, not
asserted. As written, the generality is a paper claim (CH6-adjacent), and CH2 rejects
unbacked generality assertions.

### Finding C (supporting) — the live CSS surface to be de-fact-streamed is itself a hand-coded CSS profile array (Lock 14 phrase #1)

C0 (αE `:63-109`) and αD O5 (`:83`) target `emit_fact_stream`. But the live routing
table that maps CSS sub-grammars to fact-stream output is
`W5C_REQUEST_FACT_PROFILES` (`skinny/crates/codegen/src/lib.rs:336`) — a hardcoded
`&[RequestFactsProfile]` const enumerating `css_l4_declaration_values`,
`css_l4_declaration_values_extended`, `css_l4_stylesheet_selectors`,
`css_l4_visual_functions`, … with per-profile `fact_schema`/`row_id`/`output_plane`
string literals. This is precisely a "profile array / CSS profile match" — the
construct Lock 14 phrase #1 forbids hand-coding. No candidate names it. The
de-fact-stream candidate (C0) must explicitly retire or replace this hand-coded CSS
profile array (derive the routing from the grammar/BackendRule shape, per αE's own
no-hand-curated-catalogue pre-block `:108-109`), or it leaves a standing Lock 14
violation on the very surface it claims to clean. This is additive to C0's scope, not
a rejection of it.

---

## §2 — Per-section dispositions

### alphaA — results extraction

- **§0 standing + §1 canonical bench (`alphaA:24-74`)** — ACCEPT. No generality
  claim; the ~70/~974/~2539 framing and N≥50 mandate are grammar-neutral facts about
  the benched surface.
- **§4 O(1)-checkpoint genericity (`alphaA:142-192`)** — ACCEPT. The "generic across
  all grammars, no CSS special-case" claim is verified: `8153236e8` regenerated all 9
  grammars and `regen --check` is clean 9/9 (`alphaA:160-161`); the scratch-stack
  hoist is keyed on container kind, not grammar. This is the model of a Lock-14-clean
  generality claim and the correct template for SK-V17 candidates.
- **§5 ledger + §6 banked wins (`alphaA:196-241`)** — ACCEPT. Provenance-accurate;
  no generality overreach.
- **§7 goalset seed, the "named lever sequence" (`alphaA:243-269`)** — REVISE.
  `alphaA:260-261` says the levers are "ALL grammar-neutral," but §7 inherits the
  architecture-doc lever names (`byte_class_index_64`, structural pre-scan) without
  binding them to the benched skinny SIMD surface (`select_classifier` /
  `PrimitiveKernels`, `dispatch.rs:42,50`). Fix: cite the skinny grammar-general
  entry `select_classifier(alphabet)` and `lo6_table_admissible` (`dispatch.rs:42,101`)
  as the neutrality vehicle, the way αE §C2 does (`alphaE:157-213`). Without that, the
  neutrality is asserted against core-tree symbols (Finding A).

### alphaB — competitor deltas

- **§0 plane taxonomy + §2/§3/§4 deltas (`alphaB:16-181`)** — ACCEPT. The
  lightningcss-is-the-fair-bar / cssparser-is-plane-mismatched framing is correct and
  is itself grammar-domain-honest (it generalizes the SK-V6 sonic-lossy finding to the
  CSS domain). No grammar-neutrality defect; the plane taxonomy is the right per-domain
  comparator discipline.
- **§5 JSON comparator guard (`alphaB:184-194`)** — ACCEPT. Correctly requires the
  shared tape/projection substrate touch to re-run JSON rows on the strict plane —
  this is the cross-grammar regression tripwire CH2 wants.
- **§2 per-corpus endpoint mapping (`alphaB:96-119`)** — ACCEPT (inferential, but
  self-flagged `:118-119,242-245` and routed to CH1). Not a CH2 concern.

### alphaC — REDRESS digest

- **§1 AZ-IV admit-under-framing (`alphaC:29-65`)** — ACCEPT. The lazy-view framing
  is grammar-general (cites JSON `value_from_ref` as the proof model) and the
  zero-payload invariant is grammar-neutral.
- **§2 StructLayout split (`alphaC:69-115`)** — REVISE. The "different-framing
  admission" cites `bbnf_ir::registry::struct.rs StructLayout` /
  `crates/ir/src/passes/types/registry.rs:140` / `css_l4/builder.rs:274` — all
  **core-tree** symbols (Finding A). αC is digesting the architecture doc faithfully,
  but for SK-V17 the pre-block re-open tests must be stated against the **benched
  skinny** carrier (`skinny/crates/runtime/src/tape/`, `skinny/.../codegen/src/lower/`)
  or explicitly note "core-tree symbols; skinny equivalent per αE:37-51." As written, a
  CSS wave could satisfy αC's re-open tests on the core tree while the benched skinny
  tree is untouched — a generality/measurability gap. Fix: add the one-line
  skinny-tree mapping αE already authored.
- **§3 fact-stream pre-block (`alphaC:119-157`)** — REVISE (additive). The
  different-framing admission `:145-149` is correct (typed CSSOM via lazy projection;
  fact-stream diagnostic-only). But it omits Finding C: the live routing is the
  hand-coded `W5C_REQUEST_FACT_PROFILES` CSS profile array (`codegen/src/lib.rs:336`),
  which is itself a Lock 14 phrase-#1 construct. Add a re-open clause: "the
  de-fact-stream wave must retire/replace the hand-coded CSS profile array
  `W5C_REQUEST_FACT_PROFILES`, deriving routing from grammar/BackendRule shape; leaving
  it is a standing Lock 14 hand-coded-profile-array violation."
- **§4 broadcast / §5 FNV+fixture / §6 x86 (`alphaC:159-268`)** — ACCEPT. §5b's
  "scratch sizes from input.len() + StructLayout, grammar-general; fixtures as inputs
  not selectors" is the correct Lock-14 anti-overfit framing. §6 aarch64-NEON-only is
  grammar-neutral (NEON vocabulary keyed on alphabet, not grammar). The Lock 14 census
  binding `:230-232` is exactly right.
- **§7/§8 consolidated ledger (`alphaC:270-300`)** — ACCEPT. §8's
  "typed/rich/retained = admit; eager/allocating/fragmented/serialized = pre-block"
  line is grammar-neutral and load-bearing.

### alphaD — validated/invalidated ledger

- **§1 validated wins V1-V6 (`alphaD:25-39`)** — ACCEPT. V5 (O(1) checkpoint
  "Generic, all grammars, no CSS special-case") and V6 (tape substrate "shared,
  grammar-agnostic") are correctly characterized as grammar-neutral. Note V6 cites
  `crates/core/src/runtime/tape/` (core tree) — acceptable here because αD is
  recording where the substrate landed; but see O1.
- **§2 invalidated I1-I7 (`alphaD:48-56`)** — ACCEPT. All grammar-neutral facts.
- **§3 O1 tape wiring (`alphaD:79`)** — REVISE. The framing constraint is the best
  in the artefacts ("`TapeStructBuilder` dispatches only on `StructLayout` (no route
  strings); JSON/sheets/bbnf already implement the same `StructBuilder` trait") — BUT
  (i) it names core-tree `TapeStructBuilder`/`StructLayout` (Finding A: the benched
  JSON tape is skinny `TapeBuilder`/`ValueRef`, a different trait surface); and (ii)
  "JSON/sheets/bbnf already implement the same `StructBuilder` trait" overstates
  Finding B — `sheets_witness` is a 25-line stub and bbnf-self is absent. Fix: name the
  benched skinny substrate, and downgrade to "JSON implements the lazy-cursor model
  (`value_from_ref`); Sheets witness is a stub, BBNF-self is future — CSS is the first
  non-JSON rich rider, which is itself the generality test."
- **§3 O2 lazy view (`alphaD:80`)** — REVISE. "identical across JSON/CSS/sheets/bbnf
  by construction" is the unbacked four-grammar claim (Finding B). Generator generality
  is *by construction* only if it is exercised on ≥1 non-CSS non-JSON grammar; with
  Sheets a stub, the construction is untested. Fix: require the generator to emit for
  at least `sheets_witness` (or state honestly that CSS+JSON are the only exercised
  riders and the four-grammar claim is aspirational).
- **§3 O3 NEON pre-scan (`alphaD:81`)** — ACCEPT. "grammar-general leaf set keyed on
  the grammar's delimiter/alphabet sets, never CSS-specific" is verified against the
  benched `select_classifier(alphabet)` / `lo6_table_admissible` surface
  (`dispatch.rs:42,101`); JSON's scan is already alphabet-keyed
  (`json/scan.rs:15-17` builds the table from bytes). This is correctly Lock-14-clean.
- **§3 O4 commit-by-construction spine (`alphaD:82`)** — ACCEPT. "Grammar-general
  (the emitter, not a CSS patch)" is the correct framing; the change is to which Alts
  emit a checkpoint, a codegen property not a CSS literal.
- **§3 O5 codegen unification (`alphaD:83`)** — REVISE. Correctly targets the
  594-line `css_l4.toml` asymmetry and the 187-fn overfit, and correctly says "one
  projection-driven generator … from `StructLayout`." But (i) `StructLayout` is
  core-tree (Finding A), and (ii) it omits `W5C_REQUEST_FACT_PROFILES` (Finding C) —
  the live hand-coded CSS profile array that O5/C0 must dissolve. Add it to the
  retire-list.

### alphaE — candidate shortlist

- **§0 ground-truth + architecture-doc translation correction (`alphaE:13-51`)** —
  ACCEPT, and commend. αE is the ONLY artefact that correctly identifies Finding A
  (`:37-51`: "no `StructLayout` … in skinny … Candidates are framed against skinny
  paths, not the doc's core-tree paths. CH1 will reject any goalset citing core-tree
  paths as the benched surface."). This disposition adopts αE's correction as the CH2
  standard. (That SYNTHESIS/HANDOFF then regress on it is the REVISE below, not αE's
  fault.)
- **C0 de-fact-stream (`alphaE:63-109`)** — REVISE (additive). Grammar-framing is
  sound (codegen-unification, derives from BackendRule). REVISE only to add Finding C:
  the C0 file-path list (`:73-78`) must include `skinny/crates/codegen/src/lib.rs:336`
  `W5C_REQUEST_FACT_PROFILES` (the hand-coded CSS profile array) as a surface to
  retire, else C0 leaves a Lock-14 phrase-#1 construct standing.
- **C1 tape wiring (`alphaE:111-155`)** — ACCEPT. αE names the **skinny** substrate
  correctly (`skinny/crates/runtime/src/tape/{mod.rs,assembler.rs}`,
  `skinny/.../codegen/src/lower/{offset_tape,tape_plan}.rs`, JSON `value_from_ref`
  `json/value.rs:143`) — this is the benched tree, Finding A respected. The Lock-1
  same-wave and no-second-tape pre-blocks are grammar-neutral. (Note: C1 says the lazy
  view is "isomorphic to JSON's `value_from_ref`" — correct and verified.)
- **C2 NEON structural pre-scan (`alphaE:157-213`)** — ACCEPT, and commend. This is
  the strongest grammar-neutral candidate: it reuses `select_classifier` /
  `PrimitiveKernels` (`dispatch.rs:42,50`), checks `lo6_table_admissible` for the CSS
  alphabet (`:184-186`, verified `dispatch.rs:101`), falls back to scalar honestly on
  collision (NOT a CSS special-case), and explicitly forbids "CSS-specific scanner
  vocabulary (CH2/Lock 14 — keyed on the grammar's delimiter/alphabet sets)"
  (`:209-212`). This is exactly Lock 14 phrase #3 respected (byte kernels reused below
  the grammar's own alphabet, no JSON semantic API bleeding into CSS).
- **C3 commit-by-construction spine (`alphaE:215-257`)** — ACCEPT. Codegen Alt-mode
  emission, grammar-general; correctly notes the no-checkpoint Alts must be proven
  non-depositing by the codegen (a structural property), not heuristically.
- **C4 tailwind tuning: udot + i8mm (`alphaE:259-304`)** — ACCEPT with one CH2
  note (no disposition change). C4 is corpus-named (tailwind) but the *mechanism*
  (udot 4-digit scan, i8mm kernel via `PrimitiveKernels`) is grammar-neutral and the
  pre-block `:303-304` correctly forbids "fixture/per-corpus hand-tuned capacity
  constants … tuning is a generic delimiter-density heuristic, not a tailwind literal."
  CH2 is satisfied: the number leaf and density heuristic are grammar-general; tailwind
  is the falsifiability corpus, not a special-cased code path.
- **§3 cross-cutting falsifiability discipline (`alphaE:332-348`)** — REVISE. The
  Lock-14 bullet `:343-346` says "Each must demonstrably also serve JSON … or
  sheets/bbnf, or be re-framed." This is the right rule, but it must acknowledge
  Finding B: "sheets/bbnf" cannot serve as a demonstration witness because Sheets is a
  25-line stub and BBNF-self is absent. Fix: the demonstration witness is JSON (real)
  for C1/C2/C3; require C1's lazy-view generator to additionally emit for
  `sheets_witness` so the generator's generality is *tested*, not asserted.

### SYNTHESIS.md — the contract draft (αF)

- **§0.1 close condition — JSON guard row (`SYNTHESIS:54`)** — ACCEPT. The
  JSON-tape-as-regression-tripwire framing is the correct grammar-neutral guard.
- **§0.1 "Tape activation" gate (`SYNTHESIS:55`)** — REVISE. Names
  `crates/core/src/runtime/tape/` (core tree). Per Finding A + αE:37-51, the benched
  activation target is `skinny/crates/runtime/src/tape/` (`TapeBuilder`/`ValueRef`).
  Fix: re-point to the skinny substrate, OR add an explicit gate "SK-V17 ports the
  core-tree tape into skinny" with the LOC budget that implies (no candidate budgets
  it today). As written the gate's grep ("tape types in a parse path returns
  non-zero") would pass against core-tree tests, not the benched skinny parse path.
- **§0.1 "Layout-driven projection" gate (`SYNTHESIS:56`)** — REVISE. Names
  `bbnf_ir::registry::struct.rs LayoutKind + FieldSource` and `css_l4/builder.rs:274`
  — core-tree symbols (Finding A; `StructLayout` is absent from skinny). The
  grammar-neutrality claim ("grammar-neutral by construction across
  JSON/CSS/Sheets/BBNF-self") inherits Finding B (Sheets stub, BBNF-self absent). Fix:
  (a) re-frame against the benched skinny lowering (`skinny/.../codegen/src/lower/`,
  `BackendRule`, αE:43-51); (b) downgrade the four-grammar generality to a witnessed
  claim (JSON real, CSS new rider, Sheets-witness exercise required, BBNF-self future).
- **§0.1 "preserve-rich-ast" + "CSS typed equality" gates (`SYNTHESIS:57-58`)** —
  ACCEPT. Grammar-neutral correctness gates; the 8-field equality is the honest
  per-grammar parity oracle.
- **§0.1 "Foldable into TOTALITY" gate (`SYNTHESIS:64`)** — REVISE. "generalize
  beyond CSS to JSON/Sheets/BBNF-self by construction" is the unbacked four-grammar
  claim (Finding B). The fold-into-totality direction is also confounded by Finding A:
  if SK-V17 activates the benched skinny `TapeBuilder` (not core `TapeStructBuilder`),
  the totality fold must reconcile the two tape substrates, which the gate does not
  mention. Fix: state which substrate is canonical and that the fold reconciles
  skinny↔core; downgrade the witness list per Finding B.
- **§0.3 receiver goalset — lazy-view generator + builder seam (`SYNTHESIS:84-85`)**
  — REVISE. `(TapeCursor, &StructLayout, source)` + `regen_css.rs emit_builder` +
  `OpenFrame template` are core-tree symbols (Finding A); the benched skinny seam is
  `skinny/.../codegen/src/lower/{offset_tape,tape_plan}.rs` + `TapeBuilder`. Also omits
  Finding C (`W5C_REQUEST_FACT_PROFILES`). Fix: re-point to skinny symbols + add the
  CSS-profile-array retirement.
- **§0.3 NEON receiver (`SYNTHESIS:88`)** — ACCEPT. "route CSS + JSON through
  `scan_dispatch`/`select_classifier`" + "non-JSON exercise" is Lock-14-clean and
  matches the benched `select_classifier` surface.
- **§0.4 pre-blocks (`SYNTHESIS:91-136`)** — ACCEPT. Comprehensive and
  grammar-neutral; the hidden-coupling escape list is the correct Lock-1 guard.
- **§0.5 per-corpus close conditions (`SYNTHESIS:138-161`)** — REJECT. **The
  `normalize` corpus does not exist in the benched set.** Verified:
  `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:23-50` defines exactly
  `{bootstrap, tailwindcss, material-components-web, animate}`. SYNTHESIS §0.5 sets
  per-corpus close conditions for `normalize` (`:151`) and the tranche-level success
  criterion is "**normalize OR bootstrap** crosses" (`:156-157`). `normalize` was
  inherited from the A-series recognition benchmark (`alphaA:102-104`, a different
  corpus set), NOT the benched SK-V14 set. This is a generality/measurability failure:
  the goalset's success condition is partly bound to a corpus the benched grammar
  surface never runs. Fix: replace `normalize` with `animate` (the actual regular/
  easiest benched corpus, `alphaB:64,98`) throughout §0.5 and the success criterion, OR
  add `normalize` to `css_l4_corpus.rs` and the manifest. Until reconciled, the
  tranche close condition is unmeasurable. (This is also a CH1 defect; CH2 flags it
  because a success criterion on a non-existent grammar input is the sharpest
  grammar-binding failure.)
- **§0.6 strict comparator gate (`SYNTHESIS:163-180`)** — ACCEPT. Grammar-domain
  honest (full-CSSOM fair bar, token-scan flaw probe).
- **Section 2 telemetry binding (`SYNTHESIS:205-247`)** — REVISE. The `css_corpus`
  enum (`:214`) lists "normalize / bootstrap / tailwind / material-components-web" —
  same phantom-`normalize` defect (it should be `animate`), and omits the benched
  `animate`. The `simd_non_json_exercise` boolean (`:232`) is the right Lock-14 column,
  but per Finding B the only real non-CSS non-JSON grammar is the `sheets_witness`
  stub — the schema should name which grammar satisfies the exercise. Fix: correct the
  corpus enum to the benched set; bind `simd_non_json_exercise` to a named grammar.
- **Section 3 trajectory (`SYNTHESIS:249-267`)** — REVISE. `:259` and `:261-263`
  again gate on "regular corpora (normalize/bootstrap)" — same phantom-corpus fix.

### HANDOFF.md (αF)

- **Current state + four-lever route (`HANDOFF:6-51`)** — REVISE. `:13` names
  `crates/core/src/runtime/tape/` (Finding A); `:46-47` gates the cross on
  "normalize/bootstrap" (phantom corpus). Same fixes as SYNTHESIS.
- **Pre-blocked routes + hidden-coupling (`HANDOFF:82-109`)** — ACCEPT.
  Grammar-neutral, comprehensive.
- **Next move + close criterion (`HANDOFF:111-142`)** — REVISE. `:137-140` close
  criterion "normalize OR bootstrap crosses" — phantom corpus. Fix to `animate OR
  bootstrap` (or add normalize to the corpus).

---

## §3 — Disposition summary

Total sections dispositioned: **34.**

- **ACCEPT: 22**
- **REVISE: 11**
- **REJECT: 1** (SYNTHESIS §0.5 — success criterion bound to the non-existent
  `normalize` corpus)

ACCEPT rate: 22/34 = **64.7%** — below the §3Z 95% bar; V1 does NOT converge on the
CH2 lens. The defects are concentrated and mechanical (two root causes A + B + one
supporting C), repaired by re-pointing core-tree paths to the benched skinny tree,
downgrading the four-grammar generality claim to its witnessed truth, and fixing the
phantom `normalize` corpus. αE is the corrected source for Finding A; the αF synthesis
must re-adopt αE's correction it regressed.

### The three load-bearing fixes (V2 must land all)

1. **Finding A (REJECT-grade for the substrate claim):** SYNTHESIS/HANDOFF/αC-§2/
   αD-O1,O2,O5 must name the **benched skinny** tape (`skinny/crates/runtime/src/
   tape/`, `TapeBuilder`/`ValueRef`/`value_from_ref`) and the skinny lowering
   (`skinny/crates/codegen/src/lower/`, `BackendRule`) — NOT core-tree
   `TapeStructBuilder`/`StructLayout`/`bbnf_ir::registry::struct.rs` — OR declare an
   explicit core→skinny port with its LOC budget. αE:37-51 is the template.
2. **Finding B (REVISE-grade for the generality claim):** downgrade
   "grammar-neutral … across JSON/CSS/Sheets/BBNF-self" to its witnessed truth (JSON
   real, CSS new rider; `sheets_witness` is a 25-line stub, BBNF-self absent), and
   require the lazy-view generator to *emit for* `sheets_witness` so the generality is
   tested not asserted. Lock 14's CSS+Sheets minimum is met; the four-grammar boast is
   not.
3. **Finding C + the phantom corpus (REVISE + the lone REJECT):** add
   `W5C_REQUEST_FACT_PROFILES` (`codegen/src/lib.rs:336`, hand-coded CSS profile array,
   Lock 14 phrase #1) to C0/O5's retirement list; replace `normalize` with the benched
   `animate` (or add `normalize` to `css_l4_corpus.rs`) everywhere it appears in the
   close condition + telemetry enum.

What CH2 affirms as already Lock-14-clean and must be preserved: the O(1) checkpoint
genericity (αA §4), the `select_classifier`/`PrimitiveKernels`/`lo6_table_admissible`
NEON surface (αE C2, αD O3, SYNTHESIS §0.3 NEON), the commit-by-construction emitter
framing (C3/O4), and the byte-kernel-below-grammar-alphabet discipline (Lock 14 phrase
#3) that C2 honors exactly.
