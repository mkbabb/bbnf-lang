# CH2 GENERALITY — SK-V17 S-P3 SYNTHESIS-PLAN CHALLENGE (V3)

Lens: CH2 GENERALITY. Cycle: V3. Date: 2026-05-29.
Reviewer charge: every wave/primitive grammar-neutral (Lock 14) — the NEON classifier
shared, the tape+`ValueRef` generic; CSS-only sequencing re-framed as per-grammar template;
the plan folds to TOTALITY not CSS-special-cased. Disposition each wave/section
ACCEPT / REVISE / REJECT with path:line + concrete fix.
Subject artefacts: `restart/skinny/tranches/sk-v17/SPEC.md`,
`research/p3/{p3a,p3b,p3c,p3d,p3e,p3f}.md`.
Authority: PASS-3-SYNTHESIS-PLAN §3 CH2; ORCHESTRATOR §3W/§3Z. Master HEAD `f87ee713a`.

## §1 — CH2 verdict in one line

The V3 packet is grammar-neutral end-to-end; **all three V1 CH2 REVISEs remain folded** and
the **sole V3 SPEC delta (the W2 -2.0% maintain band, `SPEC.md:5,564-569`) is a CH1/CH6
falsifiability fix that is itself grammar-neutral** and perturbs no CH2 surface. The shared
classifier (`select_classifier(alphabet: &'static [u8;64])`, verified live at
`skinny/crates/bbnf-simd/src/dispatch.rs:42`), the generic `ValueRef<…,G:EventGrammar=
AnyGrammar>` (verified live at `skinny/crates/runtime/src/tape/mod.rs:175`), the single
`BackendRule`-walking projection generator with the load-bearing JSON-byte-equal-THROUGH-the-
new-generator gate (`SPEC.md:550-557`), the lo6/`;{`→slot-59 `& 0x3f` collision trap
(`SPEC.md:316-319`), and the witness-honest TOTALITY-fold (JSON+CSS exercised; Sheets/BBNF-self
deferred to SK-V18 on the no-`BackendRule`-shape justification, `SPEC.md:110-114,329-330`) are
all CH2-clean and ground-truth-verified against the skinny tree. **No REVISE, no REJECT this
cycle.** All units ACCEPT.

## §2 — Disposition fold audit (the §4 hardening-without-folding gate)

| Disposition | V3 status | Evidence |
|---|---|---|
| **R-CH2-1** (V1, load-bearing): SPEC W2 exit gate lacked the JSON-`value_from_ref`-rider-re-emit-byte-equal-THROUGH-the-new-generator check; "both riders exercised" was satisfiable by a CSS-only generator wearing a generic name | **HELD (folded V2, intact V3)** | SPEC W2 exit gate carries the named load-bearing line "**JSON rider re-emits byte-equal THROUGH the new generator (R-CH2-1, load-bearing).**" (`SPEC.md:550-557`): diff of the regenerated JSON `value_from_ref` vs the committed file = empty, AND "a CSS-only generator that never re-emits JSON does NOT pass this gate" (`SPEC.md:556-557`). Mirrored in `p3c:102` (verbatim falsifiability clause) and the telemetry gate `p3d:172,258`. |
| **R-CH2-2** (V1, framing): P3-B 5-wave vs SPEC/P3-C 6-wave numbering stranded the projection-generality wave | **HELD (folded V2, intact V3)** | P3-B is authored to the SPEC six-wave manifest verbatim (`p3b:13,37-42,99,145-146`); the projection-generality wave is unambiguously W2 across SPEC/P3-B/P3-C. The V3 P3-B fold note (`p3b:18-24`) only re-keys stale `SPEC.md:` line citations after the V2 W1/W2 enumeration grew; "the six-wave topology is unchanged, no route is re-opened" (`p3b:24`). |
| **R-CH2-3** (V1, cohesion): "for the CSS grammars" read CSS-pinned | **HELD (folded V2, intact V3)** | SPEC close-condition row 3 "emits `document/value/view/visitor` per grammar — JSON the existing witness ... CSS L4 the first-mover rider — by walking the SAME `BackendRule` shape" (`SPEC.md:54-62`); W2 task 1 "per grammar from ONE `BackendRule`-walking generator ... the generator is grammar-generic, NOT a CSS-pinned emitter" (`SPEC.md:532-537`). The sole residual "for the CSS grammars" (`SPEC.md:514`) is an owner-path listing of the generated CSS rider files — correctly CSS-scoped per §2.1 (generated output is an allowed CSS-specific surface). |

No CH2 REVISE was open entering V3; the V3 packet introduces no CH2 regression. The V3 SPEC
delta (`SPEC.md:5`) folds the V2 W2-maintain-budget disposition (a bare 0% "no regression"
floor → the bench-falsifiable -2.0% median band vs the W1 typed-tape baseline). That delta is
a measurable-gate tightening owned by CH1/CH6; it edits no generic-crate behavior, names no
grammar, and rides the same per-corpus typed plane already in the gate — CH2-neutral.

## §3 — Per-section / per-wave dispositions (V3 independent re-review)

### SPEC §0.1 close conditions (rows 2,3,5,9,11) — ACCEPT
- Row 2 tape activation: "NO new cursor/builder type — the EXISTING `Tape`/`ValueRef`/
  `TapeBuilder` is the only substrate (Lock 1)" (`SPEC.md:52-53`). Grammar-free. ACCEPT.
- Row 3 layout-driven projection: per-grammar — "The mechanism is grammar-generic (one
  generator walking `BackendRule`), NOT a CSS-pinned emitter; a generator that leaves JSON's
  hand-written path untouched is the generic-named-CSS-generator failure mode (CH2) and FAILS"
  (`SPEC.md:60-62`). `W5C_REQUEST_FACT_PROFILES` RETIRED + DERIVED from `.bbnf`/`BackendRule`,
  "no per-rule-id match arms in skinny generic crates that JSON does not need"
  (`SPEC.md:66-68`). The R-CH2-3 fold landed here. ACCEPT.
- Row 5 preserve-rich-ast: lazy `ValueRef` projection, generic. ACCEPT.
- Row 9 NEON: "grammar-general leaf routes through `dispatch.rs select_classifier` (`:42`),
  produces only a `Vec<u32>` structural index ... exercises at least one non-JSON grammar
  (`css_l4`)" (`SPEC.md:103-106`). Shared classifier — `select_classifier(alphabet:
  &'static [u8;64])` verified live at `dispatch.rs:42`. ACCEPT.
- Row 11 foldable-to-TOTALITY: "Projection generality exercised by-construction on JSON + CSS
  only; non-CSS-non-JSON (Sheets/BBNF-self) is the SK-V18 proof (`sheets_witness` has no
  `BackendRule` shape and cannot serve as an SK-V17 projection exercise, §0.4)"
  (`SPEC.md:110-114`). Witness-honest, correctly scoped. ACCEPT.

### SPEC §2.1 generality + Lock 14 gate (the load-bearing CH2 section) — ACCEPT
`SPEC.md:305-335`. Complete and unchanged from the V2 converged state: public-API scan
(`:309`), grammar-branch scan (no behavior by grammar/corpus/rule/role/field/layout name,
`:310-311`), primitive/table scan pinning the lo6/`;{`→slot-59 `& 0x3f` collision and naming
the classifier's sole grammar datum ("The L1 classifier's only grammar datum is the
`alphabet: &[u8;64]` passed to `select_classifier` (Lock-14 vehicle); the CSS `;{` pair uses
the eq-set fan, NOT the lo6 table", `SPEC.md:314-317`), role/fact boundary (`:318-320`),
template/provider boundary ("`W5C_REQUEST_FACT_PROFILES` is RETIRED, not relocated", `:323`),
and the non-JSON proof (`projection_generality_exercise ∈ {json, css_l4}`,
`simd_non_json_exercise=css_l4`, Sheets/BBNF-self DEFERRED to SK-V18 with the
no-`BackendRule`-shape justification, `SPEC.md:324-332`). "A wave that lets CSS or JSON policy
into a generic crate fails CH2" (`:331-332`). Allowed CSS-specific surfaces (`:334-335`) draw
the boundary correctly. ACCEPT.

### W0 (SPEC §3) — ACCEPT
0 behavior LOC; harness/gate/comparator only. The `css_`-prefixed telemetry columns
(`SPEC.md:159-181`, `p3d:172-176`) live in the bench harness + `gate-json` reporting surface —
an allowed `host/API schema facts` surface (§2.1:335), not grammar policy in a generic
hot-path crate. `gate-json` is a reporting consumer, not a runtime/codegen/simd generic crate.
No CH2 leak. ACCEPT.

### W1 (SPEC §4) tape activation + W5C retirement — ACCEPT
`push_plain_offset` is one branchless u32 write into the EXISTING `offsets`; routing DERIVED
from `BackendRule` preserved as DATA with "every residual CSS routing entry names the `.bbnf`
rule it derives from" (`SPEC.md:67-68,440-441`). DELETE `W5C_REQUEST_FACT_PROFILES`
(`SPEC.md:403,440`) — the target exists at HEAD (`codegen/src/lib.rs:336`, verified), so the
retirement is real, not phantom. Binding condition 3 (`SPEC.md:842-843`) forbids relocating
per-rule branching into projection DATA or flag form (the Lock-14-phrase-#1 re-entry seam).
Exit gate measurably greps `W5C_REQUEST_FACT_PROFILES` → ZERO (`SPEC.md:467`; `w5c_profile_
array_retired=true`). The V2 `emit_fact_stream` consumer-migration tightening is a
same-wave-consumer discipline fix, grammar-neutral. ACCEPT.

### W2 (SPEC §5) layout-driven lazy projection generator — ACCEPT (R-CH2-1 held; V3 -2.0% band CH2-neutral)
The generic-generator framing is load-bearing-measurable. Exit gate `SPEC.md:550-557` carries
the R-CH2-1 byte-equal-THROUGH-the-new-generator line + the CSS-only-generator failure-mode
rejection verbatim, mirroring `p3c:102`. The generator-walk recipe is generic
(`SPEC.md:523-525`), L8 flags are `BackendRule` branch-tag projections not a hand-curated
catalogue (`SPEC.md:526-527,543,576-577`), the L1/L4 index IS the tape's `offsets`
(`SPEC.md:527-528,544-545`). The V3 delta (`SPEC.md:561-569`: typed plane "no worse than
-2.0% median vs the W1 typed-tape baseline ... a bare 0% floor does not bind") is a
falsifiable maintain band over the SAME per-corpus `track1_typed` plane already in the gate —
no grammar name, no generic-crate branch, no CSS-special-casing. It is a CH1/CH6 measurability
fix and CH2-neutral. ACCEPT.

### W3 (SPEC §6) NEON structural index — ACCEPT
Routes through `select_classifier(alphabet)` (`SPEC.md:628`), "alphabet is the only grammar
datum" (`SPEC.md:630`). L5/L6 are digraph/mask-parameterised (sees masks, never literal CSS
bytes — `p3a:115,126`). Shared-kernel generality proven JSON-side: JSON 51/51 maintain GO
"the shared `select_classifier(alphabet)` kernel must not move a JSON row — JSON rides the
same primitive at `json/scan.rs:219`" (`p3c:112`). lo6-on-CSS pre-blocked (`SPEC.md:653`),
eq-set fan mandated (`SPEC.md:623-630`). `simd_non_json_exercise=css_l4` is the dischargeable
non-JSON SIMD exercise (`p3c:110`). ACCEPT.

### W4 (SPEC §7) commit-by-construction Alt-mode — ACCEPT
L9 is "a grammar-neutral codegen property derived from `BackendRule` Alt shape, JSON-witnessed;
not CSS-keyed" (`p3a:161`); the SPEC emits NO speculative checkpoint for pure-lexical
keyword-dispatch Alts that deposit nothing structural — a shape predicate on `BackendRule`,
not a CSS rule list. CONDITIONAL on a post-W1 re-profile, no CSS-special-casing. ACCEPT.

### W5 (SPEC §8) close + Lock-14 audit — ACCEPT
Task 2 is an explicit Lock-14 audit: "no CSS/JSON policy in generic crates, no renamed
residue, no relocated `W5C_REQUEST_FACT_PROFILES`; CSS L4 non-JSON proof passes"
(`SPEC.md:749-750`). Measurable (grep). Grammar-neutral cleanup. ACCEPT.

### SPEC §9 pre-blocked routes (CH2 facet) — ACCEPT
The Lock-14-phrase-#1 construct (`W5C_REQUEST_FACT_PROFILES`) is globally blocked from
re-entry including the relocation-into-projection-DATA-or-flag-form seam
(`SPEC.md:798-800`). Binding conditions 2+3 (`SPEC.md:840-843`) carry flag=`BackendRule`-
branch-tag and routing-derived-from-grammar verbatim. The REJECTed set (lo6-on-CSS
`SPEC.md:826,653`; udot/i8mm/FNV no-antecedent) is barred. CH2-clean.

### P3-A shortlist — ACCEPT
Every candidate S1–S9 carries an explicit grammar-neutral verdict (`p3a:71,82,93,104,115,
126,137,148,161`): S1 GRAMMAR-NEUTRAL (`select_classifier(alphabet)` the Lock-14 vehicle,
`p3a:71`), S3 GRAMMAR-NEUTRAL by construction (`ValueRef<…,G:EventGrammar=AnyGrammar>` generic
over G, `p3a:93` — verified live at `mod.rs:175`), S6 the canonical Lock-14 nested-balance
primitive (sees only masks, `p3a:126`), S8 GENERALISABLE-WITH-GUARD (flag bit = `BackendRule`
branch-tag, no substrate, `p3a:148`), S9 GRAMMAR-NEUTRAL codegen property derived from
`BackendRule` Alt shape (`p3a:161`). Binding conditions (`p3a:207`) carry the
flag=branch-tag, routing-derived-from-grammar gates verbatim. ACCEPT.

### P3-B sequencing (CH2 facet) — ACCEPT
Re-authored to the SPEC six-wave manifest verbatim (`p3b:13,37-42,99,145-146`); the
projection-generality wave is unambiguously W2. The W2 row carries "the generator walks
`BackendRule`, JSON+CSS riders" (`p3b:145`); the W3 NEON exercise is `css_l4` sharing
`select_classifier(alphabet)` (`p3b:57,146`). The V3 re-key touches only stale `SPEC.md:` line
citations; topology unchanged (`p3b:24`). ACCEPT.

### P3-C falsifiability gates (CH2 facet) — ACCEPT
Carries the generality proof in measurable form: W2 falsifiability "If the JSON rider does not
re-emit byte-equal THROUGH the new generator (its accessor changes by even one byte), W2 FAILS
(R-CH2-1): this is what forbids a CSS-only generator that leaves JSON's hand-written path
untouched — `projection_generality_exercise ∈ {json, css_l4}` is only satisfied when BOTH
riders flow from the ONE generator" (`p3c:102`). The W3 gate proves shared-kernel generality
JSON-side (`p3c:112`). The W2 maintain row `track1_typed@W2(c) >= -2.0% vs W1` (`p3c:156`) is
the source the V3 SPEC band reconciles to — grammar-neutral. ACCEPT.

### P3-D telemetry (CH2 facet) — ACCEPT
`projection_generality_exercise` (`p3d:172`) requires BOTH riders re-emit byte-equal THROUGH
the single W2 generator and rejects `sheets_witness` ("no `BackendRule`"). The `gate-json`
rejection rule (`p3d:258`) rejects "a CSS-only generator naming `css_l4` while JSON's
hand-written projection is bypassed (the CH2 generic-named-CSS-generator failure mode)".
`simd_non_json_exercise` distinct, requires `css_l4` (`p3d:176,260`). `w5c_profile_array_
retired` column (`p3d:177`) forces the retirement to be greppable. CH2-clean. ACCEPT.

### P3-E preblocked ledger (CH2 facet) — ACCEPT
The W5C-relocation seam, the L8-flag-form re-entry ("L8 flag = hand-curated per-rule catalogue
(relocated W5C)" barred), the second-substrate via `StructLayout`/`TapeCursor`, and the
lo6-on-CSS route-elimination (the `;`(0x3b)/`{`(0x7b)→slot-59 `& 0x3f` collision +
scalar-passthrough-as-SIMD-win trap) are all enumerated per-wave with grep-measurable
falsifiers. CH2-clean. ACCEPT.

### P3-F spec-draft (CH2 facet) — ACCEPT
Names the R-CH2-1 fold, the R-CH2-3 per-grammar framing, the §2.1 generality gate, the
Lock-14-phrase-#1 pre-block, and the `sheets_witness` rejection faithfully. No CH2 leak.
ACCEPT.

## §4 — Pre-blocked-route compliance (CH2 facet)
No CH2-dispositioned wave re-opens a barred route. The lo6-on-CSS REJECT (`SPEC.md:653,826`),
the `W5C_REQUEST_FACT_PROFILES` relocation-into-DATA-or-flag-form seam (`SPEC.md:798-800,323,
842-843`), and the L8-flag-form re-entry (`SPEC.md:576-577,840-841`) are all held. The V3
W2-maintain-band delta tightens an existing exit gate; it re-opens no route.

## §5 — Ground-truth verification (V3 independent, against the skinny tree)
The SPEC's three load-bearing CH2 vehicles are verified live, not merely cited:
- `select_classifier(alphabet: &'static [u8; 64])` — `skinny/crates/bbnf-simd/src/dispatch.rs:42`
  (the Lock-14 SIMD vehicle; alphabet is the sole grammar datum). MATCHES `SPEC.md:103,315`.
- `ValueRef<'doc, 'input, K = AnyKind, G: EventGrammar = AnyGrammar>` —
  `skinny/crates/runtime/src/tape/mod.rs:175` (generic over G; JSON+CSS instantiate the SAME
  cursor type). MATCHES `SPEC.md:53,80`, `p3a:93`.
- `W5C_REQUEST_FACT_PROFILES` exists at HEAD — `skinny/crates/codegen/src/lib.rs:336`
  (the retirement target is real). MATCHES `SPEC.md:64,403`.

## §6 — Counts + dispositions

ACCEPT: 16 · REVISE: 0 · REJECT: 0 (total dispositioned units: 16).
ACCEPT rate = 16/16 = 100.0%.

ACCEPT (16): SPEC §0.1 rows-2/3/5/9/11 · SPEC §2.1 · W0 · W1 · W2 · W3 · W4 · W5 ·
SPEC §9 · P3-A · P3-B · P3-C · P3-D · P3-E · P3-F.
REVISE: none.
REJECT: none.

Lens disposition for convergence math: ALL ACCEPT. The three V1 CH2 REVISEs (R-CH2-1
load-bearing, R-CH2-2 framing, R-CH2-3 cohesion) remain concretely folded and intact; the
sole V3 SPEC delta (W2 -2.0% maintain band) is a CH1/CH6 measurability fix that is
grammar-neutral. The SPEC's three load-bearing CH2 vehicles are ground-truth-verified against
the skinny tree. No orphan REVISE, no carry-forward. CH2 is converged for V3 (≥95% ACCEPT
second consecutive cycle: V2 100% → V3 100%).
