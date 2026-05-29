# CH2 GENERALITY — SK-V17 S-P3 SYNTHESIS-PLAN CHALLENGE (V2)

Lens: CH2 GENERALITY. Cycle: V2. Date: 2026-05-29.
Reviewer charge: every wave/primitive grammar-neutral (Lock 14) — the NEON classifier
shared, the tape+`ValueRef` generic; CSS-only sequencing re-framed as per-grammar template;
the plan folds to TOTALITY not CSS-special-cased. Disposition each wave/section
ACCEPT / REVISE / REJECT with path:line + concrete fix.
Subject artefacts: `restart/skinny/tranches/sk-v17/SPEC.md`,
`research/p3/{p3a,p3b,p3c,p3d,p3e,p3f}.md`.
Authority: PASS-3-SYNTHESIS-PLAN §3 CH2; ORCHESTRATOR §3W/§3Z. Master HEAD `f87ee713a`.

## §1 — CH2 verdict in one line

The V2 packet is grammar-neutral end-to-end and **all three V1 CH2 REVISEs are folded**:
R-CH2-1 (the load-bearing JSON-rider-byte-equal W2 gate) is now present in the SPEC W2 exit
gate (`SPEC.md:550-557`), R-CH2-2 (the 5-wave vs 6-wave numbering schism that stranded the
projection-generality wave) is resolved by re-authoring P3-B to the SPEC six-wave manifest
verbatim (`p3b:10-31,86-98,403`), and R-CH2-3 (the CSS-pinned "for the CSS grammars"
wording) is re-worded to per-grammar framing at both the close-condition altitude
(`SPEC.md:55-57`) and W2 task 1 (`SPEC.md:532-537`). The shared classifier
(`select_classifier(alphabet)`, `SPEC.md:103-105,316-319,594`), the generic tape/`ValueRef<G>`
(`p3a:87,93`), the single `BackendRule`-walking projection generator (JSON witness + CSS
first-mover rider, `SPEC.md:54-67,532-537`), the lo6/`;{`→slot-59 collision trap
(`SPEC.md:318-319`; `p3e:154`), and the witness-honest TOTALITY-fold (JSON+CSS exercised,
Sheets/BBNF-self deferred to SK-V18 with the `sheets_witness`-has-no-`BackendRule`
justification, `SPEC.md:110-114,324-332`) are all CH2-clean. **No REVISE, no REJECT this
cycle.** All units ACCEPT.

## §2 — V1 disposition fold audit (the §4 hardening-without-folding gate)

| V1 disposition | V2 status | Evidence |
|---|---|---|
| **R-CH2-1** (load-bearing): SPEC W2 exit gate lacked the JSON-`value_from_ref`-rider-re-emit-byte-equal-THROUGH-the-new-generator check; "both riders exercised" was satisfiable by a CSS-only generator wearing a generic name | **FOLDED** | SPEC W2 exit gate now carries the named load-bearing line "**JSON rider re-emits byte-equal THROUGH the new generator (R-CH2-1, load-bearing)**" (`SPEC.md:550-557`): diff of the regenerated JSON `value_from_ref` vs the committed file = empty, AND a CSS-only generator that never re-emits JSON does NOT pass; `projection_generality_exercise ∈ {json, css_l4}` is "satisfied by a CSS-only generator that never re-emits JSON does NOT pass this gate" (`SPEC.md:556-557`). Mirrors `p3c:99(c),102` verbatim and the strengthened telemetry column `p3d:156`. |
| **R-CH2-2** (framing, cross-artefact): P3-B 5-wave vs SPEC/P3-C 6-wave numbering stranded the projection-generality wave | **FOLDED** | P3-B §0 V2 fold note re-authors to the SPEC six-wave manifest verbatim (`p3b:10-31`): V1 W1 (tape+projection merged) split into SPEC W1 (PRUNE/tape) + W2 (projection); the candidate-to-wave map (`p3b:86-98,403`) now matches `SPEC.md:262-267`. The projection-generality wave is unambiguously SPEC W2 across SPEC/P3-B/P3-C. |
| **R-CH2-3** (cohesion): "for the CSS grammars" read CSS-pinned | **FOLDED** | SPEC close-condition row 3 now "emits `document/value/view/visitor` per grammar — JSON the existing witness ... CSS L4 the first-mover rider — by walking the SAME `BackendRule` shape" (`SPEC.md:55-57`); W2 task 1 now "per grammar from ONE `BackendRule`-walking generator — JSON the witness, CSS the first-mover rider ... the generator is grammar-generic, NOT a CSS-pinned emitter" (`SPEC.md:532-537`). The sole residual "for the CSS grammars" is an owner-path listing of the generated CSS rider files (`SPEC.md:514`) — correctly CSS-scoped (generated output is an allowed CSS-specific surface, §2.1). |

Folding is concrete and load-bearing, not paper-fold: the R-CH2-1 fix is a measurable
exit-gate line (`git diff` of the regenerated JSON accessor = empty), not prose.

## §3 — Per-section / per-wave dispositions (V2 independent re-review)

### SPEC §0.1 close conditions (rows 2,3,5,9,11) — ACCEPT
- Row 2 tape activation: "NO new cursor/builder type — the EXISTING `Tape`/`ValueRef`/
  `TapeBuilder` is the only substrate (Lock 1)" (`SPEC.md:52-53`). Grammar-free. ACCEPT.
- Row 3 layout-driven projection: now explicitly per-grammar — "The mechanism is
  grammar-generic (one generator walking `BackendRule`), NOT a CSS-pinned emitter; a
  generator that leaves JSON's hand-written path untouched is the
  generic-named-CSS-generator failure mode (CH2) and FAILS" (`SPEC.md:60-62`).
  `W5C_REQUEST_FACT_PROFILES` RETIRED + DERIVED from `.bbnf`/`BackendRule`, "no
  per-rule-id match arms in skinny generic crates that JSON does not need"
  (`SPEC.md:66-69`). The R-CH2-3 fold landed here. ACCEPT.
- Row 5 preserve-rich-ast: lazy `ValueRef` projection, generic. ACCEPT.
- Row 9 NEON: "grammar-general leaf routes through `dispatch.rs select_classifier`,
  produces only a `Vec<u32>` structural index ... exercises at least one non-JSON
  grammar (`css_l4`)" (`SPEC.md:101-106`). Shared classifier. ACCEPT.
- Row 11 foldable-to-TOTALITY: "Projection generality exercised by-construction on
  JSON + CSS only; non-CSS-non-JSON (Sheets/BBNF-self) is the SK-V18 proof
  (`sheets_witness` has no `BackendRule` shape and cannot serve as an SK-V17 projection
  exercise, §0.4)" (`SPEC.md:110-114`). Witness-honest, correctly scoped. ACCEPT.

### SPEC §2.1 generality + Lock 14 gate (the load-bearing CH2 section) — ACCEPT
`SPEC.md:305-335`. Complete and tightened from V1: public-API scan
(`SPEC.md:309`), grammar-branch scan (no behavior by grammar/corpus/rule/role/field/layout
name, `SPEC.md:310-311`), primitive/table scan pinning the lo6/`;{`→slot-59 `& 0x3f`
collision and naming the classifier's sole grammar datum ("The L1 classifier's only grammar
datum is the `alphabet: &[u8;64]` passed to `select_classifier` (Lock-14 vehicle); the CSS
`;{` pair uses the eq-set fan, NOT the lo6 table", `SPEC.md:316-319`), role/fact boundary
(ordinals generic, meaning in generated modules, `SPEC.md:320-321`), template/provider
boundary ("`W5C_REQUEST_FACT_PROFILES` is RETIRED, not relocated", `SPEC.md:323`), and the
non-JSON proof (`projection_generality_exercise ∈ {json, css_l4}`,
`simd_non_json_exercise=css_l4`, Sheets/BBNF-self DEFERRED to SK-V18 with the
no-`BackendRule`-shape justification, `SPEC.md:324-332`). "A wave that lets CSS or JSON
policy into a generic crate fails CH2" (`SPEC.md:332`). Allowed CSS-specific surfaces
(`SPEC.md:334-335`) draw the boundary correctly. ACCEPT.

### W0 (SPEC §3) — ACCEPT
0 behavior LOC; harness/gate/comparator only. The `css_`-prefixed telemetry columns
(`SPEC.md:159-181`, `p3d:153-160`) live in the bench harness + `gate-json` reporting surface
— an allowed `host/API schema facts` surface (§2.1), not grammar policy in a generic
hot-path crate. `gate-json` is a reporting consumer, not a runtime/codegen/simd generic
crate. No CH2 leak. ACCEPT.

### W1 (SPEC §4) tape activation + W5C retirement — ACCEPT
`push_plain_offset` is "one branchless u32 write into the EXISTING `offsets`"
(`SPEC.md:446`), routing DERIVED from `BackendRule` preserved as DATA with "every residual
routing entry names its `.bbnf` rule" (`SPEC.md:440-441`). Binding condition 3
(`SPEC.md:837-839`) forbids relocating per-rule branching into projection DATA (the
Lock-14-phrase-#1 re-entry seam). The exit gate measurably greps
`W5C_REQUEST_FACT_PROFILES` → ZERO (`SPEC.md:466-468`). The V2 `emit_fact_stream`
consumer-migration tightening (`SPEC.md:453-459`; `p3e:113`) is a same-wave-consumer
discipline fix, not a generality change — grammar-neutral. ACCEPT.

### W2 (SPEC §5) layout-driven lazy projection generator — ACCEPT (V1 R-CH2-1 folded)
The generic-generator framing is now load-bearing-measurable. Exit gate
`SPEC.md:550-557`: "**JSON rider re-emits byte-equal THROUGH the new generator (R-CH2-1,
load-bearing).** ... If the JSON rider's generated output changes — or if the JSON path is
left untouched by a CSS-only generator (the generic-named-CSS-generator failure mode, CH2)
— W2 FAILS. A `projection_generality_exercise ∈ {json, css_l4}` satisfied by a CSS-only
generator that never re-emits JSON does NOT pass this gate." This is exactly the V1 R-CH2-1
fix, mirroring `p3c:99(c),102`. The generator-walk recipe is generic
(`SPEC.md:524-525`), L8 flags are `BackendRule` branch-tag projections not a hand-curated
catalogue (`SPEC.md:526-528,571-572`), the L1/L4 index IS the tape's `offsets`
(`SPEC.md:528,573`). The ≤650 fit-proof escape hatch (`SPEC.md:501-508`) attributes
over-450 LOC to "four projection methods over N typed leaf kinds" — a grammar-generic
`BackendRule`-walk cause, NOT CSS-specific scope creep; it is a CH4 cost mechanism, not a
generality leak. ACCEPT.

### W3 (SPEC §6) NEON structural index — ACCEPT
Routes through `select_classifier(alphabet)`, "alphabet is the only grammar datum"
(`SPEC.md:625-626`), isomorphic to JSON's `scan_structurals` (`json/scan.rs:22`). L5/L6 are
digraph/mask-parameterised (sees masks, never literal CSS bytes — `p3a:115,126`).
Shared-kernel generality is proven JSON-side: "JSON 51/51 maintained ±1.0%"
(`SPEC.md:643`) demonstrates JSON rides the SAME `select_classifier` kernel without
perturbation (`p3c:112`: "JSON rides the same primitive at `json/scan.rs:219`").
lo6-on-CSS pre-blocked (`SPEC.md:648`), eq-set fan mandated (`SPEC.md:597-598`).
`simd_non_json_exercise=css_l4` is the dischargeable non-JSON SIMD exercise
(`SPEC.md:642`). ACCEPT.

### W4 (SPEC §7) commit-by-construction Alt-mode — ACCEPT
L9 is "a grammar-neutral codegen property derived from `BackendRule` Alt shape,
JSON-witnessed; not CSS-keyed" (`p3a:161`); the SPEC emits NO speculative checkpoint "for
pure-lexical keyword-dispatch Alts that deposit nothing structural" (`SPEC.md:699`) — a
shape predicate on `BackendRule`, not a CSS rule list. CONDITIONAL on a post-W1 re-profile,
no CSS-special-casing. ACCEPT.

### W5 (SPEC §8) close + Lock-14 audit — ACCEPT
Task 2 is an explicit Lock-14 audit: "no CSS/JSON policy in generic crates, no renamed
residue, no relocated `W5C_REQUEST_FACT_PROFILES`; CSS L4 non-JSON proof passes"
(`SPEC.md:744-745`, exit `SPEC.md:753-754`). The close-gate generality audit is measurable
(grep). The ≤150 LOC named-Lock-14-cleanup budget (`SPEC.md:269,733`) is grammar-neutral
cleanup. ACCEPT.

### SPEC §9 pre-blocked routes (CH2 facet) — ACCEPT
The Lock-14-phrase-#1 construct (`W5C_REQUEST_FACT_PROFILES`) is globally blocked from
re-entry including the relocation-into-projection-DATA-or-flag-form seam
(`SPEC.md:793-795`). Binding conditions 2+3 (`SPEC.md:835-839`) carry
flag=`BackendRule`-branch-tag and routing-derived-from-grammar verbatim. The REJECTed set
(lo6-on-CSS, `SPEC.md:848-849`) is barred. CH2-clean.

### P3-A shortlist — ACCEPT
Every candidate S1–S9 carries an explicit grammar-neutral verdict (`p3a:71,82,93,104,115,
126,137,148,161`): S1 GRAMMAR-NEUTRAL (`select_classifier(alphabet)` the Lock-14 vehicle),
S3 GRAMMAR-NEUTRAL by construction (`ValueRef<…,G:EventGrammar=AnyGrammar>` generic over G,
`p3a:93`), S6 the canonical Lock-14 nested-balance primitive (sees only masks, `p3a:126`),
S8 GENERALISABLE-WITH-GUARD (flag bit = `BackendRule` branch-tag, no substrate, `p3a:148`),
S9 GRAMMAR-NEUTRAL codegen property derived from `BackendRule` Alt shape (`p3a:161`). The
binding conditions (`p3a:204-218`) carry the index==offsets identity,
flag=branch-tag, routing-derived-from-grammar, scalar-balance-default, L9-re-profile gates
verbatim. ACCEPT.

### P3-B sequencing (CH2 facet) — ACCEPT (V1 R-CH2-2 folded)
P3-B is re-authored to the SPEC six-wave manifest verbatim (`p3b:10-31`); the
projection-generality wave is unambiguously W2 across SPEC/P3-B/P3-C. Sequencing is
grammar-neutral: the NEON exercise is `css_l4` sharing `select_classifier(alphabet)`
(`p3b:322-323`), the W2 generator walks `BackendRule` with JSON+CSS riders and the JSON
byte-equal check is carried (`p3b:278-280,459`). The V1 numbering schism is resolved.
ACCEPT.

### P3-C falsifiability gates (CH2 facet) — ACCEPT
P3-C carries the generality proof in measurable form: W2(c)
"**JSON rider re-emits BYTE-EQUAL THROUGH the new generator** ... `git diff` of the
regenerated JSON accessor returns EMPTY. If the JSON projection output changes by even one
byte, W2 FAILS" (`p3c:99`), and the falsifiability clause "If the JSON rider does not
re-emit byte-equal THROUGH the new generator ... W2 FAILS (R-CH2-1) ... This
W2(c)/falsifiability language is promoted into the SPEC W2 exit gate (P3-F fold,
`SPEC.md:507-517`)" (`p3c:102`). The W3 gate proves shared-kernel generality JSON-side
(`p3c:112`). ACCEPT.

### P3-D telemetry (CH2 facet) — ACCEPT
The `projection_generality_exercise` column (`p3d:156`) is strengthened to require BOTH
riders re-emit byte-equal THROUGH the single W2 generator — "A bare named value is
producer-only: the gate verifies the JSON `value_from_ref` rider re-emits byte-equal
through the NEW generator (if it changes, W2 FAILS). `sheets_witness` NOT valid — no
`BackendRule`". The `gate-json` rejection rule `p3d:242` rejects "a CSS-only generator
naming `css_l4` while JSON's hand-written projection is bypassed (the CH2
generic-named-CSS-generator failure mode)". `simd_non_json_exercise` distinct from
projection exercise, requires `css_l4` (`p3d:160,244`). CH2-clean — the V1 R-CH2-1 fix is
mirrored into the telemetry gate. ACCEPT.

### P3-E preblocked ledger (CH2 facet) — ACCEPT
The W5C-relocation seam (`p3e:111`), the L8-flag-form re-entry (`p3e:134`, "L8 flag =
hand-curated per-rule catalogue (relocated W5C)" barred), the second-substrate via
`StructLayout`/`TapeCursor` (`p3e:137`), and the lo6-on-CSS route-elimination (`p3e:154`,
the `;`(0x3b)/`{`(0x7b)→slot-59 `& 0x3f` collision + scalar-passthrough-as-SIMD-win trap)
are all enumerated per-wave with grep-measurable falsifiers (`p3e:265-273`). CH2-clean.

### P3-F spec-draft (CH2 facet) — ACCEPT
P3-F §0 names the R-CH2-1 fold ("SPEC W2 exit gate now carries a measurable check ...
`projection_generality_exercise ∈ {json, css_l4}` column is no longer satisfiable by a
[CSS-only generator]", `p3f:16-19`) and the R-CH2-3 per-grammar framing fold
(`p3f:3`). The §2.1 generality gate, the Lock-14-phrase-#1 pre-block, and the
`sheets_witness` rejection are carried faithfully (`p3f:104-111,148,187-189`). No CH2 leak.
ACCEPT.

## §4 — Pre-blocked-route compliance (CH2 facet)
No CH2-dispositioned wave re-opens a barred route. The lo6-on-CSS REJECT
(`SPEC.md:848-849`), the `W5C_REQUEST_FACT_PROFILES` relocation seam
(`SPEC.md:793-795,323,839`), the relocate-per-rule-branching-into-projection-DATA-or-flag-form
seam (`SPEC.md:239,323,571`), and the L8-flag-form re-entry (`SPEC.md:571-572`; `p3e:134`)
are all held. The V2 R-CH2-1 fold tightens an existing exit gate; it re-opens no route.

## §5 — Counts + dispositions

ACCEPT: 16 · REVISE: 0 · REJECT: 0 (total dispositioned units: 16).
ACCEPT rate = 16/16 = 100.0%.

ACCEPT (16): SPEC §0.1 rows-2/3/5/9/11 · SPEC §2.1 · W0 · W1 · W2 · W3 · W4 · W5 ·
SPEC §9 · P3-A · P3-B · P3-C · P3-D · P3-E · P3-F.
REVISE: none.
REJECT: none.

Lens disposition for convergence math: ALL ACCEPT. The three V1 REVISEs (R-CH2-1 load-bearing,
R-CH2-2 framing, R-CH2-3 cohesion) are all concretely folded into the SPEC/P3-B/P3-C/P3-D/P3-F
with measurable evidence; no orphan REVISE, no carry-forward. CH2 is converged for V2.
