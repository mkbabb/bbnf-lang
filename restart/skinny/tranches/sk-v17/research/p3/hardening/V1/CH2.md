# CH2 GENERALITY — SK-V17 S-P3 SYNTHESIS-PLAN CHALLENGE (V1)

Lens: CH2 GENERALITY. Cycle: V1. Date: 2026-05-29.
Reviewer charge: every wave/primitive grammar-neutral (Lock 14) — the NEON classifier
shared, the tape+ValueRef generic; CSS-only sequencing re-framed as per-grammar template;
the plan folds to TOTALITY not CSS-special-cased. Disposition each wave/section
ACCEPT / REVISE / REJECT with path:line + concrete fix.
Subject artefacts: `restart/skinny/tranches/sk-v17/SPEC.md`,
`research/p3/{p3a,p3b,p3c,p3d,p3e,p3f}.md`.
Authority: PASS-3-SYNTHESIS-PLAN §3 CH2; ORCHESTRATOR §3W. Master HEAD `f87ee713a`.

## §1 — CH2 verdict in one line

The plan is grammar-neutral by design and re-frames CSS as the first-mover rider, not a
special case: the NEON classifier is shared (`select_classifier(alphabet)`,
`SPEC.md:98,307,576`), the tape/`ValueRef` is generic (`ValueRef<…,G>`, `p3a:84-93`), the
projection generator walks the SAME `BackendRule` shape JSON walks (`SPEC.md:54-67,495-497`),
and the foldable-to-TOTALITY claim is witness-honestly scoped to JSON+CSS with Sheets/BBNF-self
deferred to SK-V18 (`SPEC.md:105-109,316-324`). The §2.1 generality gate is present, names the
non-JSON proof, forbids JSON/CSS policy in generic crates, and pins the lo6/`;{` collision trap
(`SPEC.md:297-324`). **One load-bearing REVISE** (R-CH2-1): the SPEC W2 projection exit gate
(the generality-load-bearing wave) does NOT measurably require the JSON `value_from_ref` rider
to re-emit byte-equal *through the new generator* — without it, "both riders exercised"
(`SPEC.md:513`) is satisfiable by a CSS-only generator that leaves JSON's hand-written path
untouched, which would NOT prove generic-generator generality. P3-C carries this proof
(`p3c:99(c)`); the SPEC drops it. Two minor REVISE on framing-cohesion. No REJECT.

## §2 — Per-section / per-wave dispositions

### SPEC §0.1 close conditions (rows 2,3,5,9,11) — ACCEPT
- Row 2 tape activation: "NO new cursor/builder type — the EXISTING `Tape`/`ValueRef`/
  `TapeBuilder` is the only substrate" (`SPEC.md:52-53`). Generic, Lock 1, grammar-free. ACCEPT.
- Row 3 layout-driven projection: generator walks the SAME `BackendRule` shape, isomorphic to
  JSON's `value_from_ref` (`json/value.rs:143`), `W5C_REQUEST_FACT_PROFILES` RETIRED + DERIVED
  from `.bbnf`/`BackendRule`, "no per-rule-id match arms in skinny generic crates that JSON
  does not need" (`SPEC.md:54-67`). This is the per-grammar-template re-framing: generic
  generator, CSS rider. ACCEPT.
- Row 5 preserve-rich-ast: lazy `ValueRef` projection, generic mechanism. ACCEPT.
- Row 9 NEON: "grammar-general leaf routes through `dispatch.rs select_classifier`, produces
  only a `Vec<u32>` structural index ... exercises at least one non-JSON grammar (`css_l4`)"
  (`SPEC.md:96-101`). Shared classifier, neutral. ACCEPT.
- Row 11 foldable-to-TOTALITY: "Projection generality exercised by-construction on JSON + CSS
  only; non-CSS-non-JSON (Sheets/BBNF-self) is the SK-V18 proof (`sheets_witness` has no
  `BackendRule` shape ...)" (`SPEC.md:105-109`). Witness-honest Lock 14 phrase #2; correctly
  scoped, not over-claimed. ACCEPT.

### SPEC §2.1 generality + Lock 14 gate (the load-bearing CH2 section) — ACCEPT
`SPEC.md:297-327`. Present and complete: public-API scan, grammar-branch scan
(no behavior by grammar/corpus/rule/role/field/layout name), primitive/table scan pinning
the lo6/`;{`→slot-59 `& 0x3f` collision (`SPEC.md:307` — the real CH2 trap: lo6 on the CSS
alphabet would claim a SIMD win it runs scalar), role/fact boundary (ordinals generic,
meaning in generated modules), template/provider boundary ("`W5C_REQUEST_FACT_PROFILES` is
RETIRED, not relocated", `SPEC.md:315`), and the non-JSON proof
(`projection_generality_exercise ∈ {json, css_l4}`, `simd_non_json_exercise=css_l4`,
`SPEC.md:316-324`). "A wave that lets CSS or JSON policy into a generic crate fails CH2"
(`SPEC.md:324`). Allowed CSS-specific surfaces (grammar inputs, generated output,
per-grammar providers/templates, tests, host/API schema facts, `SPEC.md:326-327`) draw the
boundary correctly. ACCEPT.

### W0 (SPEC §3) — ACCEPT
0 behavior LOC; harness/gate/comparator only. The `css_`-prefixed telemetry columns
(`SPEC.md:154-176`, `p3d:124-145`) live in the bench harness + `gate-json` reporting surface
— allowed `host/API schema facts` (§2.1), NOT grammar policy in a generic hot-path crate.
`gate-json` is a reporting consumer, not a runtime/codegen/simd generic crate. No CH2 leak.
ACCEPT.

### W1 (SPEC §4) tape activation + W5C retirement — ACCEPT
`push_plain_offset` is "one branchless u32 write into the EXISTING `offsets`" (`SPEC.md:430`),
`TapeBuilder` carries no grammar-keyed field (`p3a:82`), routing DERIVED from `BackendRule`
preserved as DATA with "every residual routing entry names its `.bbnf` rule" (`SPEC.md:422-423`).
Binding condition 3 (`SPEC.md:781-783`) forbids relocating per-rule branching into projection
DATA (the Lock-14-phrase-#1 re-entry seam). The exit gate measurably greps
`W5C_REQUEST_FACT_PROFILES` → ZERO (`SPEC.md:442-443`). Grammar-neutral. ACCEPT.

### W2 (SPEC §5) layout-driven lazy projection generator — REVISE (R-CH2-1, load-bearing)
The generator-walk recipe is generic ("child-position → `ValueRef` child, branch tag → meta
dispatch, typed leaf → decode by type, rule reference → child + recurse", `SPEC.md:486-488`),
from "ONE generator, isomorphic to JSON's `value_from_ref`" (`SPEC.md:495-497`). L8 flags are
`BackendRule` branch-tag projections, not a hand-curated catalogue (`SPEC.md:503,779-780`).
This is the correct generic-generator framing.

**Defect.** The W2 exit gate (`SPEC.md:507-517`) lists `projection_generality_exercise ∈
{json, css_l4}` (both riders exercised) at `SPEC.md:513`, but provides NO measurable check
that the JSON `value_from_ref` rider is **re-emitted byte-equal through the NEW generator**.
"Both riders exercised" is then satisfiable by a generator that emits CSS while JSON keeps its
existing hand-written `json/value.rs:143` path — i.e. a CSS-only generator wearing a generic
name. That is exactly the CH2 generality failure (a "generic" generator that is really a CSS
generator). P3-C names this proof — `p3c:99(c)`: "verified by the JSON rider re-emitting
byte-equal AND the CSS rider producing the 8-field-equal typed CSSOM" — and `p3c:102`:
"If the JSON rider behaviour changes, W2 FAILS (the generator broke the existing witness)."
The SPEC W2 exit gate drops both halves.
**Concrete fix.** Add to the SPEC W2 exit gate (`SPEC.md:507-517`) a measurable line:
"The JSON `value_from_ref` rider is re-generated THROUGH the new generator and re-emits
byte-equal vs the SK-V17-open JSON projection (proof the generator is grammar-neutral, not a
CSS generator); if the JSON rider's generated output changes, W2 FAILS CH2." Mirror
`p3c:99(c)`/`102` verbatim. Until this lands, `lazy_view_generated=true` (`SPEC.md:509`)
proves only that CSS emits, not that the generator generalizes.

### W3 (SPEC §6) NEON structural index — ACCEPT
Routes through `select_classifier(alphabet)`, "alphabet is the only grammar datum"
(`SPEC.md:576`), isomorphic to JSON's `scan_structurals` (`json/scan.rs:22`). L5/L6 are
digraph/mask-parameterised (sees masks, never literal CSS bytes — `p3a:115,120,126`). The
shared-kernel generality is proven on the SIMD side: "JSON 51/51 maintained ±1.0%"
(`SPEC.md:594`) demonstrates JSON rides the SAME `select_classifier` kernel without
perturbation (P3-C makes this explicit: "JSON rides the same primitive at `json/scan.rs:219`",
`p3c:112`). lo6-on-CSS pre-blocked (`SPEC.md:599`). `simd_non_json_exercise=css_l4` is the
dischargeable non-JSON SIMD exercise (`SPEC.md:593`). Grammar-neutral, shared classifier
proven. ACCEPT.

### W4 (SPEC §7) commit-by-construction Alt-mode — ACCEPT
L9 is "a grammar-neutral codegen property derived from `BackendRule` Alt shape, JSON-witnessed;
not CSS-keyed" (`p3a:161`); the SPEC emits NO speculative checkpoint "for pure-lexical
keyword-dispatch Alts that deposit nothing structural" (`SPEC.md:646`) — a shape predicate on
`BackendRule`, not a CSS rule list. CONDITIONAL on a re-profile, no CSS-special-casing. ACCEPT.

### W5 (SPEC §8) close + Lock-14 audit — ACCEPT
Task 2 is an explicit Lock-14 audit: "no CSS/JSON policy in generic crates, no renamed residue,
no relocated `W5C_REQUEST_FACT_PROFILES`; CSS L4 non-JSON proof passes" (`SPEC.md:688-689`,
exit `SPEC.md:697-698`). The close-gate generality audit is itself measurable (grep). The
≤150 LOC named-Lock-14-cleanup budget (`SPEC.md:264,677`) is grammar-neutral cleanup. ACCEPT.

### SPEC §9 pre-blocked routes — ACCEPT (CH2 facet)
The Lock-14-phrase-#1 construct (`W5C_REQUEST_FACT_PROFILES`) is globally blocked from
re-entry, including the relocation-into-projection-DATA-or-flag-form seam (`SPEC.md:737-739`).
Binding conditions 2+3 (`SPEC.md:779-783`) carry the flag=`BackendRule`-branch-tag and
routing-derived-from-grammar verbatim. The REJECTed set (lo6-on-CSS, `SPEC.md:793`) is barred.
CH2-clean.

### P3-A shortlist — ACCEPT
Every candidate S1–S9 carries an explicit grammar-neutral verdict (`p3a:71,82,93,104,115,126,
137,148,161`); the Lock-14 vehicle (`select_classifier(alphabet)`, `ValueRef<…,G>`) is named;
the binding conditions (`p3a:204-218`) carry the index==offsets identity, flag=branch-tag,
routing-derived-from-grammar, scalar-balance-default, L9-re-profile gates verbatim. ACCEPT.

### P3-B sequencing — ACCEPT (CH2 facet) + REVISE (R-CH2-2, framing, cross-artefact)
Sequencing is grammar-neutral and re-frames CSS as the rider; W2 NEON exercise is
`css_l4` sharing `select_classifier(alphabet)` (`p3b:191-192`). **Framing defect (REVISE,
not CH2-fatal):** P3-B uses a 5-wave map (W0=infra, W1=substrate+projection, W2=NEON,
W3=L9, W4=close, `p3b:77-83`) while the SPEC + P3-C use a 6-wave map (W0,
W1=tape, W2=projection, W3=NEON, W4=L9, W5=close). The CH2 facet: the
generality-load-bearing **projection-generator wave** is W1 in P3-B but W2 in the SPEC/P3-C,
so the artefact pointing at "where the JSON-byte-equal generality proof lives" is ambiguous.
The SPEC is authoritative (PASS-3 §2), so the proof belongs in SPEC W2; P3-B/P3-C must be
reconciled to the SPEC numbering so the generality gate is not stranded between two wave maps.
**Fix:** reconcile P3-B `p3b:77-83` + P3-C wave headers to the SPEC's W0–W5 six-wave manifest
(`SPEC.md:257-267`); this is primarily CH1/CH4's defect but it has a CH2 facet (it must not
orphan the R-CH2-1 JSON-byte-equal proof). Flag to consolidation for cross-lens fold.

### P3-C falsifiability gates — ACCEPT (and the source of the R-CH2-1 fix)
P3-C correctly carries the generality proof the SPEC drops (`p3c:99(c),102`); CH2-correct as
written. The fix for R-CH2-1 is to promote `p3c:99(c)`/`102` into the SPEC W2 exit gate.

### P3-D telemetry, P3-E preblocked, P3-F spec-draft — ACCEPT (CH2 facet)
P3-D `projection_generality_exercise` column rejects `sheets_witness` as a value
(no `BackendRule`, `p3d:140`), matching the SYNTHESIS §0.4 witness-honest framing — CH2-clean.
P3-E/P3-F carry the Lock-14-phrase-#1 pre-block and the §2.1 generality gate faithfully
(`p3f:69-71,142,146`). No CH2 leak.

## §3 — Minor framing REVISE (R-CH2-3, cohesion)
SPEC close-condition row 3 (`SPEC.md:54-67`) and W2 task 1 (`SPEC.md:495`) both say the
generator emits "for the CSS grammars," which reads CSS-pinned on a fast skim even though the
mechanism is generic (`from ONE generator, isomorphic to JSON's value_from_ref`). Re-word to
"emits `document/value/view/visitor` for each grammar (JSON the existing witness, CSS the
first-mover rider) from ONE `BackendRule`-walking generator" so the generic-generator intent
is unambiguous at the close-gate altitude. Non-blocking cohesion fix; fold on first W2 touch.

## §4 — Pre-blocked-route compliance (CH2 facet)
No CH2-dispositioned wave re-opens a barred route. The lo6-on-CSS REJECT (`SPEC.md:793`),
the `W5C_REQUEST_FACT_PROFILES` relocation seam (`SPEC.md:737-739,524`), and the
relocate-per-rule-branching-into-projection-DATA-or-flag-form seam (`SPEC.md:233,315,522`)
are all held. The R-CH2-1 fix does not re-open any route — it tightens an existing exit gate.

## §5 — Counts + dispositions

ACCEPT: 14 · REVISE: 3 · REJECT: 0 (total dispositioned units: 17).
ACCEPT rate = 14/17 = 82.4%.

ACCEPT (14): SPEC §0.1 rows-2/3/5/9/11 · SPEC §2.1 · W0 · W1 · W3 · W4 · W5 · SPEC §9 ·
P3-A · P3-C · {P3-D+P3-E+P3-F bundle}.
REVISE (3):
- **R-CH2-1 (load-bearing).** SPEC W2 exit gate (`SPEC.md:507-517`) lacks the JSON-rider-
  re-emit-byte-equal-through-the-new-generator check. Fix: add the `p3c:99(c)`/`102` proof
  to the SPEC W2 exit gate. Without it the generic-generator generality is unproven.
- **R-CH2-2 (framing, cross-artefact).** P3-B 5-wave vs SPEC/P3-C 6-wave numbering strands
  the projection-generality wave. Fix: reconcile P3-B/P3-C to the SPEC W0–W5 manifest.
- **R-CH2-3 (cohesion).** "for the CSS grammars" reads CSS-pinned; re-word to per-grammar
  framing (`SPEC.md:54-67,495`).
REJECT: none.

Lens disposition for convergence math: REVISE (R-CH2-1 is a load-bearing measurable-gate gap;
R-CH2-2/3 are framing). No REJECT; no orphan — all three fold concretely.
