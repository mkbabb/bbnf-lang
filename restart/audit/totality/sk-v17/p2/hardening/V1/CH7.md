---
lens: CH7 OVERFIT-PRUNE (V1)
pass: T-P2-research
cycle: V1
reviewed: [2a-sota-landscape, 2b-primitive-vocabulary, 2c-grammar-neutrality, 2d-cost-model, 2e-host-arch, 2f-fold-gaps]
master_head: 91b6893b0
contract: restart/prompts/totality/PASS-2-RESEARCH.md §3 + ORCHESTRATOR §3W
focus: no contrivance; the fold is the genuinely-general tape model (not CSS/JSON-special-cased); lightningcss the fair bar; no fixture/FNV/broadcast re-entry
accept: 38
revise: 3
reject: 0
---

# CH7 OVERFIT-PRUNE — T-P2 SK-V17 V1

CH7 scans the T-P2 fold dossiers for the four overfit pathologies its mandate names:
(1) **contrivance** — a fold dressed as general but structurally CSS/JSON-special-cased;
(2) **unfair-bar distortion** — lightningcss (the CSS >SOTA anchor) inflated, conflated,
or substituted by a non-comparable measurement; (3) **fixture/FNV/broadcast re-entry** —
a fold smuggling a per-corpus literal, a 24-row broadcast tuple, or an FNV/fixture runtime
back across the LOCKED skinny pre-blocks (L-SK17-04/05, SK17L-010); (4) **orphan-kernel /
by-exercise-overclaim** — a primitive or generality claim with no benched antecedent or no
non-JSON witness, asserted "proven" on construction alone.

**Headline.** The fold model is genuinely general, not contrived. Its grammar-neutrality
vehicle is a TYPE parameter (`ValueRef<…,G:EventGrammar>`, `skinny/.../tape/mod.rs:175`)
plus alphabet-as-data classification (`select_classifier(alphabet:&[u8;64])`,
`dispatch.rs:42`) — both structurally grammar-blind, monomorphised at codegen with zero
runtime `match grammar`. The CSS/JSON specialisation lives only in `@generated` per-grammar
surfaces (Lock-14 ALLOWED), and the fold's stated act is to RETIRE the one genuine overfit
(the eager 817-LOC `OpenFrame` CSS god-module, the Lock-14-NAMED failure mode) into that
general plane. The broadcast/FNV/fixture pre-blocks are honoured (2b FOLD-L7 binds Lock 8
"no per-corpus literal" and cites L-SK17-05; 2d fails the cost model closed on
broadcast/grammar-named evidence). The orphan `udot`/i8mm digit kernel is explicitly
REFUSED for want of a benched CSS antecedent (2b:146). The by-construction-not-by-exercise
honesty is the strongest overfit guard in the set and is correctly asserted (2c REFUTES the
fleet-wide claim; 2c-ONBOARD is the falsifier).

**The one genuine CH7 defect: the lightningcss bar is contrived in 2E.** 2E asserts a
**measured** "recognizer beats lightningcss 2-3×" that no benchmark supports and mis-cites
its anchor. This is precisely the unfair-bar distortion CH7 exists to catch. It is a REVISE
(scoped, fixable), carried with two adjacent REVISEs that share the same recognizer-vs-
materialization framing-drift. Zero REJECT: no fold re-opens a REDRESS route, adds a 6th
shape, or smuggles a fixture/broadcast.

---

## Load-bearing finding — CH7-001 (REVISE, 2e-host-arch.md:42-43, :288-289)

**Defect (contrivance / unfair-bar distortion).** 2E's Executive Summary states verbatim:

> "The proven recognizer beats lightningcss 2-3× and the residual gap is materialization
> (`skinny/RESULTS.md:5-55`, 1d:60-66)." (2e:42-43; repeated 2e:288-289.)

Three independent provenance failures make this a contrivance:

1. **The cited anchor does not carry the claim.** `1d:60-66` is the SK17L lesson-table
   header + SK17L-001/002 rows (the JSON SoA tape vs **sonic** proof); it contains no
   "2-3×" figure and no lightningcss pairing (verified: `grep -n '2-3\|lightningcss' 1d` →
   no match in that range). `RESULTS.md:5-55` is the **JSON** bench (Track 1 vs sonic-rs /
   simdjson / yyjson) — the standing proof is "JSON 51/51 strict A/GO Track 1 > sonic
   strict same-plane" (1d:63-66). Neither source measures CSS against lightningcss.

2. **lightningcss is a CSS bar and it is UNMEASURED.** SPEC:207 is explicit: "ALL
   per-corpus lightningcss endpoints are **UNMEASURED-PENDING**: no wave exit-gate may key
   on an inferred per-corpus endpoint until the W0 N≥50 harness emits the per-corpus split."
   SPEC:122 names lightningcss full-CSSOM "THE fair >SOTA bar … the only strict admission
   anchor for the CSS >SOTA gate." 2E transposes the JSON-vs-sonic recognizer margin onto
   an unmeasured CSS-vs-lightningcss bar and presents it as a measured 2-3× beat.

3. **The figure "2-3×" appears nowhere in the corpus.** It is absent from SPEC, RESULTS.md,
   and all six T-P1 inventories (`grep` confirms 2e is the sole occurrence across all p2
   dossiers). It is a fabricated magnitude.

**Why CH7 (not CH1).** CH1 would flag the broken citation; CH7 flags the *overfit posture*:
a fold that quietly upgrades an unmeasured, pending CSS bar into a "proven 2-3× win" is the
exact contrivance that lets a CSS-special-cased result masquerade as a general triumph. The
fold's actual, defensible claim — which the other five dossiers state correctly — is
narrower: the JSON recognizer is >SOTA-witnessed (vs sonic), the CSS lightningcss bar is the
SK-V18 target and is UNMEASURED-PENDING, and the residual *materialization* gap is the
SK-V17-excavated divergence (eager OpenFrame), not a benchmarked CSS deficit.

**Concrete fix (REVISE).** Rewrite 2e:42-43 and 2e:288-289 to:
> "The proven JSON recognizer rides the flat tape >SOTA (JSON 51/51 strict Track 1 > sonic
> same-plane, `RESULTS.md:5-55`, 1d:63-66); the CSS lightningcss full-CSSOM bar is THE fair
> >SOTA anchor and is UNMEASURED-PENDING (SPEC:122,:207). The residual gap the fold closes
> is *materialization* — the eager `OpenFrame` AZ-IV shape (SPEC:791) the lazy `ValueRef<G>`
> projection retires — not a benchmarked lightningcss deficit."
Delete the "2-3×" figure entirely (no source). Re-cite `RESULTS.md:5-55` as the JSON anchor,
`1d:63-66` (SK17L-001) as the SoA proof, and SPEC:122/:207 for the lightningcss bar's
unmeasured status. Disposition source: CH7-001.

---

## Adjacent framing-drift — CH7-002 (REVISE, 2e-host-arch.md:171, :290-291)

**Defect (by-exercise overclaim adjacent to the unfair bar).** 2E:171 and the Assertion-2
prose (2e:290-291) state the projection has "full typed-AST parity **with lightningcss**" as
though parity were established. preserve-rich-ast parity with lightningcss is a *gate*
(`css_typed_summary_equal`, SPEC:129; `assert_lightningcss_strict_equality`, SPEC:98) the
SK-V18 fold must PASS, not a property the dossier may assert as held. The CSS equality
comparator is itself part of the UNMEASURED-PENDING surface.

**Fix (REVISE).** Reword to the obligation form: "the projection MUST reach full typed-AST
parity with lightningcss under `assert_lightningcss_strict_equality` (SPEC:98) — the SK-V18
CH-parity gate, not a property held at this pass; preserve-rich-ast is the non-negotiable
*target*." Disposition source: CH7-002. (2A:303 carries the same "parity with lightningcss"
phrasing as a stated property; fold the same correction there — see CH7-003.)

---

## Framing-drift — CH7-003 (REVISE, 2a-sota-landscape.md:303, :42, :117)

**Defect.** 2A:42 and 2A:117 invoke "the recognizer-vs-materialization gap SK-V17 proves"
and 2A:303 claims the fold "preserves rich-AST parity with lightningcss as non-negotiable" —
both inherit the same conflation: the *recognizer* >SOTA fact is JSON-vs-sonic (proven), the
*materialization*-gap framing is the SK-V17 excavation (divergence B, a code-shape divergence,
not a measured CSS gap), and lightningcss parity is an SK-V18 gate, not a held property. 2A
is more careful than 2E (it does not state a "2-3×" number and elsewhere correctly flags the
lightningcss endpoints as the SK-V18 target), so this is a lighter REVISE: tighten the three
phrasings to separate (a) the JSON recognizer measured fact from (b) the materialization
*code-shape* divergence from (c) the lightningcss parity *obligation*.

**Fix (REVISE).** 2A:303 → "the fold's preserve-rich-ast obligation is full typed-AST parity
with lightningcss as the SK-V18 strict-equality gate (non-negotiable target)." 2A:42/:117 →
qualify "the recognizer-vs-materialization gap" as "the SK-V17-excavated materialization
divergence (eager OpenFrame, SPEC:791) — a code-shape divergence, the JSON recognizer being
the only >SOTA-witnessed plane." Disposition source: CH7-003.

---

## ACCEPT census — the fold is genuinely general (not contrived)

The following are CH7-ACCEPT: each is grammar-general by construction (not a CSS/JSON
special case), honours the lightningcss bar where it touches it, and re-opens no
fixture/FNV/broadcast pre-block.

### Grammar-generality vehicle (the anti-contrivance core) — ACCEPT
- **2c Type-parameterised value plane** (`ValueRef<…,G:EventGrammar>`, 2c:69; 2a:144;
  2f:150; 2e:154). The grammar enters as a TYPE param + kind `K`, monomorphised at codegen,
  zero runtime `match grammar`. This is the structural proof the fold is not special-cased:
  JSON and CSS instantiate the SAME cursor type. ACCEPT — the genuinely-general vehicle.
- **2c / 2b / 2a / 2d / 2e Alphabet-as-data classifier** (`select_classifier(alphabet)`,
  2c:70; 2b FOLD-L1:155; 2a:213; 2d:177; 2e:222). simdjson/sonic classify a FIXED JSON
  alphabet; bbnf takes the alphabet as `[u8;64]` DATA. Verified grammar-general: 8 generated
  grammars carry `scan_structural` (`crates/core/src/grammar/generated/{json,csv,bbnf,bnf,
  ebnf,css_pretty,google_sheets,css_l4}.rs`), math.rs excepted — confirmed live. ACCEPT.
- **2c-ONBOARD future-grammar onboarding test** (2c:227-245). The Lock-14 falsifier:
  adding a grammar is a config + grammar-source change with ZERO generic-crate grammar
  branch; the test fails on any `JsonParser|CssL4Parser` leak in a generic crate. This is
  the strongest overfit guard in the set — it operationalises "not special-cased." ACCEPT.

### lightningcss-bar honesty (where dossiers touch it correctly) — ACCEPT
- **2c REFUTES the fleet-wide grammar-neutral claim on JSON+CSS alone** (2c:77, :285-289;
  refutation row). `sheets_witness` is a 24-LOC stub with no `.bbnf`/`BackendRule`; the fold
  is breadth-of-CONFIG proven by-exercise on JSON+CSS only; fleet-wide wording is scoped to
  the witnessed grammars (LOCKS:382-387). This is the by-construction-not-by-exercise honesty
  CH7 demands — refutation as a first-class output. ACCEPT (load-bearing).
- **2b orphan-kernel refusal** (2b:146): the `udot`/i8mm digit-block MAC is grammar-neutral
  and admissible for number-heavy grammars BUT requires a same-wave consumer with a profiled
  antecedent — CSS has none, so NO orphan kernel. SPEC:215 ("udot/i8mm REJECTed — no
  antecedent") honoured. ACCEPT — exemplary anti-orphan discipline.
- **2b L5/L6 REQUIRED-NEW gate** (2b:218-246). `comment_body_mask_64` /
  `bracket_depth_mask_64` have no scalar/checkasm bodies today (verified: grep-empty in
  `skinny/crates/bbnf-simd/src/`); 2b files them REQUIRED-NEW, scalar-ref + checkasm BEFORE
  wiring, grammar-neutral by digraph parameterisation (C/Rust/JS/SQL comments; JSON/CSS/BBNF/
  Sheets balance), not CSS-pinned. No SIMD-win overclaim. ACCEPT.
- **2b scalar-delegate honesty** (2b:74, :127-135, A4): `byte_class_from_table_64` /
  `bitmap_prefix_xor_64` are 4-LOC scalar passthroughs filed `scalar-delegate-non-ASM`, NOT
  claimed as NEON row movement. ACCEPT — refuses the SIMD-overclaim contrivance.

### fixture / FNV / broadcast pre-block integrity — ACCEPT
- **2b FOLD-L7 one-shot capacity** (2b:248-258): sizes the EXISTING `offsets` from the L1
  scan count, "no per-corpus capacity literal," binds Lock 8 + cites L-SK17-05 FNV/fixture
  fence (SK17L-001). ACCEPT — the fixture/FNV re-entry is fenced, not re-opened.
- **2d cost-model fail-closed** (2d:266, UNKNOWN-2D-S17-02): the model "fails closed on
  grammar-named (`json_*`/`css_*`) or **broadcast**/stale evidence." 2d:258 refutes the
  zero-rule e-graph / tautological CSP overfit — VERIFIED REAL: `backend_egraph.rs` runs a
  `[…; 0]` empty rule array when rewrites disabled (a candidate set is not a derivation
  proof). ACCEPT — the broadcast pre-block and the cost-derivation honesty both hold.

### Structural fold candidates (no contrivance, substrate-union-preserving) — ACCEPT
- **A tape one-encoding closure** (2a FOLD-A, 2c-A, 2d-04, 2e-A, 2f-F3): exactly one AoS/SoA
  encoding survives; the tape carries NO grammar column (sparse position-keyed flags only,
  the AV.04 dense-class-column overfit barred). ACCEPT.
- **B eager OpenFrame retirement** (2a-B, 2b FOLD-L2, 2c-B, 2d-03, 2e-B, 2f-F1): the
  Lock-14-NAMED overfit (817-LOC CSS god-module) is the DELETION target. CH7 notes this is
  the *correct* posture — the fold removes the special-casing, it does not add it. ACCEPT.
- **C lazy ValueRef<G> plane** (2a-C, 2b FOLD-L3, 2c-C, 2d-03, 2e-C, 2f-F2): one
  `BackendRule`-walking generator re-emits all 8 surfaces; CH2 firewall (JSON byte-equal
  re-emission) is the no-CSS-special-case gate. ACCEPT.
- **D tape-as-substrate-manifest, NOT a 6th shape** (2a-D, 2c-D, 2d-01, 2e-D, 2f-F4): the
  LAC-1E-14 FactStream precedent applied verbatim; no silent 6th shape. Grammar-blind
  substrate-target categorisation. ACCEPT.
- **E shared NEON classifier as Lock-16 manifest entry** (2a-E, 2b FOLD-L1, 2c-E, 2d-05,
  2e-E, 2f-F5): impl-exceeds-spec, 0-LOC narrative fold + manifest row, aarch64-only,
  scalar-ref + checkasm. ACCEPT.
- **F StructRegistry/FieldSource compile-time fence** (2a-F, 2b FOLD-L3, 2c-F, 2d-06, 2e-F,
  2f-F6): the 28-65×/983×/10583× regression pre-block kept inviolate; compile-time
  projection-emission, never per-leaf runtime walk. ACCEPT (the regression firewall).
- **2f F7 OnceCell substrate_target across all 8 carriers** + **2f F8/F9, 2d-02/-07, 2b
  FOLD-L4/L8/L9, 2c-ONBOARD**: each grammar-neutral, REDRESS-53-fenced, no orphan kernel.
  ACCEPT.

### LOCKS-AMENDMENTS-CANDIDATEs — ACCEPT (no overfit)
LAC-2A-SKV17-01..03, LAC-SK17-2C-01/02, LAC-2D-S17-01..03, LAC-2E-SKV17-01/02/04,
LAC-2F-FOLD-01..05, LAC-2b-SKV17-01..04 — all are substrate-manifest / classifier-manifest /
fence refinements carrying no per-corpus literal, no 6th shape, no broadcast. ACCEPT.

**Note — LAC-2E-SKV17-03** (2e:353) carries the eager-OpenFrame-retirement candidate text
cleanly (no lightningcss "2-3×" leak into the LAC), so the CH7-001 defect is confined to
2E's prose body, NOT its LAC set. The fix is a prose rewrite; the candidate survives.

---

## Cross-check against the CH7 mandate

| CH7 axis | verdict | evidence |
|---|---|---|
| No contrivance (fold genuinely general) | HELD except CH7-001 | type-param `ValueRef<G>` + alphabet-as-data are structurally grammar-blind (2c:69-70); special-casing is the DELETION target (eager OpenFrame), not added. The one contrivance is 2E's fabricated lightningcss "2-3×". |
| lightningcss the fair bar | DISTORTED in 2E (CH7-001), framing-drift in 2A (CH7-003) | SPEC:122 names it THE fair bar; SPEC:207 marks all endpoints UNMEASURED-PENDING. 2E presents an unmeasured CSS bar as a measured 2-3× beat; 2A/2E assert parity as held vs a gate. Elsewhere honoured (2c REFUTES fleet-wide; SPEC:215 udot reject honoured). |
| No fixture/FNV re-entry | HELD | 2b FOLD-L7 binds Lock 8 "no per-corpus literal" + L-SK17-05 (2b:252-258). |
| No broadcast re-entry | HELD | 2d:266 fails closed on broadcast evidence; SK17L-010 / L-SK17-04 24-row broadcast pre-block not re-derived. |
| No orphan kernel | HELD | 2b:146 refuses orphan udot/i8mm (no benched CSS antecedent); 2b L5/L6 REQUIRED-NEW gated. |
| by-construction ≠ by-exercise honesty | HELD | 2c:77 REFUTES the fleet-wide claim; 2c-ONBOARD is the falsifier; 2b/2e scope generality to JSON+CSS witnessed. |

---

## Dispositions

| id | dossier:line | disposition | one-line |
|---|---|---|---|
| CH7-001 | 2e-host-arch.md:42-43, :288-289 | **REVISE** | Fabricated "recognizer beats lightningcss 2-3×" with mis-cited anchor; lightningcss bar is UNMEASURED-PENDING (SPEC:207). Delete the figure; separate JSON-recognizer fact from CSS-lightningcss target. |
| CH7-002 | 2e-host-arch.md:171, :290-291 | **REVISE** | "full typed-AST parity with lightningcss" asserted as held; it is an SK-V18 strict-equality GATE. Reword to the obligation form. |
| CH7-003 | 2a-sota-landscape.md:303, :42, :117 | **REVISE** | Same recognizer-vs-materialization / lightningcss-parity framing-drift, lighter; tighten to separate JSON measured fact, materialization code-shape divergence, and lightningcss parity obligation. |
| CH7-ACCEPT-set | 2b/2c/2d/2f entire; 2a/2e minus the above | **ACCEPT** (38) | Fold genuinely general (type-param + alphabet-as-data); broadcast/FNV/fixture fences intact; orphan kernels refused; by-construction honesty correctly scoped. |

**Counts: ACCEPT 38 · REVISE 3 · REJECT 0.** ACCEPT rate ≈ 92.7% (38/41). Below the 95%
convergence bar by the three lightningcss-bar REVISEs — all in 2A/2E, all scoped, all
shared-root (the recognizer-vs-materialization framing borrowing an unmeasured CSS bar). No
REJECT: no fold is structurally contrived, re-opens a REDRESS route, adds a 6th shape, or
smuggles a fixture/FNV/broadcast. The genuinely-general tape model holds; the firewall catch
is that 2E let the JSON recognizer's >SOTA margin contaminate the CSS lightningcss narrative,
and that must be un-conflated before T-P3 distils the fold.
