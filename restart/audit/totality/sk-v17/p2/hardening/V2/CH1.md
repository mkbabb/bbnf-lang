---
lens: CH1 CORRECTNESS
pass: T-P2-research (SK-V17 totality)
cycle: V2
reviewer: CH1-V2
master_head: 91b6893b0
t_p1_source_sha: 445925167
generated_at: 2026-05-29T00:00:00Z
subject: greater-spec FOLD review — every fold proposal traces to a named T-P1 divergence + file:line/SHA; SOTA/ISA claims cited; CH1-V5-001 folded; V1 REVISE folds re-verified against live source
dossiers_reviewed: [2a, 2b, 2c, 2d, 2e, 2f]
fold_proposals_dispositioned: 44
accept: 43
revise: 1
reject: 0
v1_revises_refolded: [CH1-2B-01, CH1-2C-01, CH1-2D-01, CH1-2E-01, CH1-2F-01]
hygiene_CH1_V5_001: VERIFIED-FOLDED-ON-DISK
accept_rate: 0.977
---

# CH1 CORRECTNESS — T-P2 V2 (SK-V17 totality fold)

## Scope + method

Per PASS-2-RESEARCH §3 CH1 + ORCHESTRATOR §3W: every cited paper / library-source
citation must resolve to the claimed path:line and carry the claimed finding;
every fold proposal must trace to a NAMED T-P1 divergence + file:line/SHA;
SOTA/ISA claims must be cited; CH1-V5-001 (the enumerated-filename residual) must
be folded. Provenance gaps and confabulated citations are REJECT;
loose-but-resolvable anchors are REVISE.

V2 method differs from V1 by an additional firewall obligation: each of the five
V1 REVISEs (CH1-2B-01, CH1-2C-01, CH1-2D-01, CH1-2E-01, CH1-2F-01) is re-opened
and the fold is independently re-verified against live source at master HEAD
`91b6893b0` — a recorded-but-unapplied fold (the exact pathology that kept
CH1-V5-001 alive across T-P1 V5→V6-confirm) is the one defect class this lens
hunts hardest. Every load-bearing anchor across the six dossiers was re-greped
live: skinny tape/ValueRef/classifier, core TapeRec/OpenFrame/begin_compound,
ir struct.rs registry, the cost-model pipeline (passes/lib.rs, backend_egraph,
decision_csp), LOCKS/ARCH/MASTER-PLAN/SPEC surfaces, scalar-delegate bodies,
x86inc/checkasm presence, the RESULTS.md >SOTA numbers, and the SOTA literature
set.

## Hygiene — CH1-V5-001 (first action, per dispatch)

**VERIFIED FOLDED ON DISK — ACCEPT.** Live verification at `91b6893b0`:

- `grep -c ',collapsed}' {1a,1b,1e}` = `0 / 0 / 0` (no residual brace-glob in any
  of the three excavation files the dispatch names).
- `grep -c 'collapsed_stage}.rs' 1b` = `3` — the enumerated executing form
  (`…/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs`) is present
  at `1b:12` (`live_truth_method`) and `1b:97` (BSHAPE17-004 row).
- `skinny/crates/codegen/src/lower/collapsed_stage.rs` EXISTS;
  `collapsed_tape.rs` DOES NOT (the brace-glob never expands to a non-existent
  path).

The dispatch directs folding on first touch of 1a/1e; the defect lived only in
1b, and 1b is already correct on disk — no 1a/1e edit was required. All six
dossier frontmatters report the hygiene action with the exact verifying grep
(2A `hygiene_first_action`, 2B `t_p1_entry_state` + L1 notes, 2C
`hygiene_action_CH1_V5_001`, 2D `first_hygiene_action_folded`, 2E
`hygiene_action_folded`, 2F `hygiene_ch1_v5_001`). The brace-glob occurrences
that remain in the T-P1 hardening artefacts (V5/V6-confirm CH1/CH6,
CONSOLIDATED) are QUOTED-AS-THE-DEFECT historical records and are correctly left
untouched. **Hygiene satisfied; the T-P1 residual REVISE is discharged at this
SHA.**

## V1 REVISE re-fold firewall — all five re-verified against live source

The central V2 obligation. Each V1 REVISE is a citation-precision defect whose
underlying claim was true; the V2 fold had to re-point the anchor to the verified
source WITHOUT introducing a new mis-anchor. All five fold edits land correctly
on disk.

| V1 REVISE | folded edit (live-verified at `91b6893b0`) | verdict |
|---|---|---|
| **CH1-2B-01** (S-P2 path abbreviation) | Every `HARDENING-S-P2-V3:` reference in 2B now carries the full path `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md:92-251` (`2b:67,:84,:85,:129,:163,:370`). File confirmed present. | **ACCEPT** |
| **CH1-2C-01** (FactStream dual-ordinal) | 2C now states the two ordinals distinctly: `ARCHITECTURE.md:1803` is plane-table row **(3)** "Fact-stream output" AND that same row carries "Per LAC-1E-14 this is the 5th SUBSTRATE-manifest category, not a 6th BackendShape variant" (`2c:79,:176-178,:308`). Live `ARCH:1803` confirmed to carry both clauses verbatim; the dual-ordinal disambiguation is now explicit. | **ACCEPT** |
| **CH1-2D-01** (ARCH:1203 reuse) | 2D re-anchored the simdjson "builds ONE tape" external claim to the paper (arXiv:1902.08318) at `2d:69,:301`, with `ARCH:1203` retained ONLY for the sonic-rs lazy-value lineage note. Live `ARCH:1203` = `OffsetTape` row carrying "(sonic-rs lazy-value lineage)" — exact. | **ACCEPT** |
| **CH1-2E-01** (signature-name anchor) | 2E re-anchored the classifier signature-name set to `MASTER-PLAN.md:622` (the H.W2.5 macro vocabulary) at `2e:72,:108,:259,:446`, citing `ARCH:1284` "for the four-LLVM-shapes scan-leaf-FFI context only." See note below — the V1 premise was imprecise; the V2 fold is nonetheless the correct, stronger anchoring. | **ACCEPT** |
| **CH1-2F-01** (alphabet datum LOCKS vs SPEC) | 2F re-anchored the alphabet-as-data datum from `LOCKS.md:315-317/:312-314` to SK-V17 `SPEC.md:314-317`/`:312` (`2f:20,:304-308,:580`). Live `SPEC.md:314-317` confirmed verbatim: "The L1 classifier's only grammar datum is the `alphabet: &[u8;64]` passed to `select_classifier` (Lock-14 vehicle); the CSS `;{` pair uses the eq-set fan, NOT the lo6 table (the `& 0x3f` slot-59 collision)." | **ACCEPT** |

**Note on CH1-2E-01 (V1 premise correction, not a new defect).** The V1
disposition asserted `ARCH:1284` was "NOT a table of the macro signatures." That
premise was itself slightly imprecise: live `ARCH:1284` DOES carry the macro
signature names inline in prose (`BYTE_CLASS_FROM_TABLE_64`,
`BYTE_CLASS_FROM_EQ_SET_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`,
`BULK_EMIT_COMPRESSED`, `EOB_PAD_CLAMP`, `FRAME_PUSH_BOUNDED`,
`FRAME_POP_BOUNDED`). The names live at BOTH `ARCH:1284` (the four-LLVM-shapes
scan-leaf-FFI prose) and `MASTER-PLAN.md:622` (the dedicated H.W2.5 "primitive
vocabulary authoring" row). The V2 fold re-anchors the canonical signature-name
set to `MASTER-PLAN.md:622` — the dedicated vocabulary row, the correct
load-bearing locus — and scopes `ARCH:1284` to "context only." That re-anchoring
is accurate and strictly improves the citation (the authoring row is the
authoritative vocabulary source; the ARCH line is narrative). No new
mis-anchor is introduced. **ACCEPT.**

## Provenance census — every load-bearing anchor re-verified at HEAD

All anchors below resolved VERBATIM at `91b6893b0`. No confabulated citation
found in any dossier.

| anchor (dossier-cited) | live-verified content | verdict |
|---|---|---|
| `skinny/.../tape/mod.rs:94-100` SoA `Tape<'input>` six members | `pub struct Tape<'input> { source, offsets, flag_cursors, flag_values, payloads:PayloadArena, id:TapeId }` | RESOLVES |
| `skinny/.../tape/mod.rs:175` `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` | exact signature | RESOLVES |
| `skinny/.../grammars/json/value.rs:143` `value_from_ref` | `pub(crate) fn value_from_ref<'doc,'input:'doc>(` | RESOLVES |
| `skinny/.../bbnf-simd/src/dispatch.rs:42` `select_classifier(&'static [u8;64])` | exact signature | RESOLVES |
| `dispatch.rs:50-56` `PrimitiveKernels` (5 fn-ptr fields) | exact struct (byte_class_from_table_64, bitmap_prefix_xor_64, bitmap_next_set_bit, bulk_emit_positions_64, eob_pad_clamp) | RESOLVES |
| `crates/core/.../tape/record.rs:103,:120-121` AoS `TapeRec` + 16-byte/align-4 asserts | `pub struct TapeRec;` `assert size_of==16`/`align_of==4` | RESOLVES |
| `crates/core/.../css_l4/builder.rs:16,:71,:74-79` `OpenFrame` + pending fields | `enum OpenFrame<'p>`; `pending_value:Option<CssTypedValue>`; six `pending_*` Vecs | RESOLVES |
| `crates/core/.../tape/mod.rs:185-186` `begin_compound(&StructLayout)` reads `layout.rule_id & 0x1F` | exact | RESOLVES |
| `crates/ir/.../struct.rs:84,:202,:313,:331` FieldSource/StructLayout/StructRegistry/layout() | `enum FieldSource`/`struct StructLayout`/`struct StructRegistry`/`fn layout(...) -> Option<&StructLayout>` | RESOLVES |
| `crates/core/.../bbnf/arena.rs:47` LIVE coupling site (CH5-V1-003 fence target) | `match StructRegistry::compound_kind_for_layout(layout)` | RESOLVES |
| `crates/core/.../grammar/generated/json.rs:701` `OnceCell<StructuralIndex>` (CH5-V1-004 retention antecedent) | `structural_index: ::core::cell::OnceCell<…>` | RESOLVES |
| `LOCKS.md:100-108` LAC-1E-14 FactStream "5th substrate-manifest category … NOT a 6th BackendShape" + 5-shape canon `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` | verbatim | RESOLVES |
| `ARCH:1206` `CollapsedStage` `target.arch==x86 + avx512bw + Entry(_)`; "aarch64 mechanically refused"; UNKNOWN-2D-05; marker-string lowerer `collapsed_stage.rs:15-17` | verbatim | RESOLVES |
| `ARCH:1803` plane-table row (3) Fact-stream + "5th SUBSTRATE-manifest category … not a 6th BackendShape variant"; `admitted_fact_output` | verbatim (CH1-2C-01 anchor) | RESOLVES |
| `ARCH:1203` `OffsetTape` row "(sonic-rs lazy-value lineage) … NOT-ADMITTED" | verbatim (CH1-2D-01 anchor) | RESOLVES |
| `ARCH:1284` four-LLVM-shapes prose carrying the 8 macro signature names + Layer-0/Layer-1 (`x86inc.asm`/`bbnf.asm`) factoring | verbatim (CH1-2E-01 context anchor) | RESOLVES |
| `MASTER-PLAN.md:622` H.W2.5 macro vocabulary (the 8 names + admission-consumed-only) | verbatim (CH1-2E-01 + 2B canonical anchor) | RESOLVES |
| SK-V17 `SPEC.md:312-317` alphabet-as-data Lock-14 vehicle / CSS `;{` eq-set fan / `& 0x3f` slot-59 collision | verbatim (CH1-2F-01 anchor) | RESOLVES |
| cost-model: `skinny/.../passes/src/lib.rs:392` `derive_backend_shape`; `:415` `choose_backend_shape`; `:498` `backend_egraph::select`; `:499` `decision_csp::finalize_rule`; `crates/ir/.../strategy.rs:104,:107` `enum EmitStrategy`/`StructDirect{`; root `Cargo.toml:2` egraph+csp-solver members | verbatim (2D/2F D-fold anchors) | RESOLVES |
| `x86inc.asm` present `skinny/crates/bbnf-simd/ext/x86/x86inc.asm` / ABSENT in `crates/`; checkasm harness present (checkasm_parity, checkasm_common, per-primitive) | verbatim (2B PV17-003/004 refutation rows) | RESOLVES |
| scalar-delegate bodies: `aarch64/byte_class_from_table_64.rs:3` = 4-LOC scalar passthrough to `crate::scalar::*`; `byte_class_from_eq_set_64.rs:33` = real NEON body | verbatim (2B PV17-005) | RESOLVES |
| `StructLayout` introduce-site census = 960 (2A frontmatter + F9 rename surface) | `grep -rc StructLayout crates/` sum = 960 | RESOLVES |
| `skinny/RESULTS.md:5-55` twitter parse_only 8349.290 > sonic 4913.095; direct 17585.679 > strict sonic 14857.624 (>SOTA carrier) | verbatim | RESOLVES |
| SOTA literature: simdjson VLDB-J 2019 (arXiv:1902.08318), Mison VLDB 2017, eq-sat POPL 2009, egg POPL 2021, BURG LOPLAS 1992 (DOI 10.1145/151640.151642), asmjson/Sneller AVX-512 (Lemire 2023 ICPP), Lemire 2019 NEON byte-map | all real, all carry the claimed finding | RESOLVES |
| `HARDENING-S-P2-V3-CONSOLIDATED.md` L1–L9 LOCKED pool, full path | present at `restart/skinny/tranches/sk-v17/research/p2/hardening/` | RESOLVES |

## Fold-proposal disposition census

44 fold proposals/sections across the six dossiers. Each verified for (i) named
T-P1 divergence antecedent (A/B/C/D/E/F + SUB17-/D-1E-SKV17-/SK17L-/BSHAPE17-/
COH17- IDs), (ii) file:line/SHA grounding, (iii) cited SOTA/ISA where claimed.

### 2A — 6 folds (FOLD-2A-A..F) + 8 grounding rows — ALL ACCEPT

Both refutation rows (CollapsedStage-as-NEON-route refused by `ARCH:1206`
mechanically; "grammar-general across 8 grammars" → config-breadth value-plane
JSON+CSS-exercised only, CH2-V1-R3 fold) match the literature's actual position.
The CH4-2a-001 fold (per-primitive scalar-ref/checkasm; eq-set fan is the only
real NEON body; table/prefix are scalar delegates) is live-confirmed at
`byte_class_from_table_64.rs:3`. The CH5-V1-004 fold correctly records
`substrate_target=existing_tape` as a PRE-condition pending the live
`OnceCell<StructuralIndex>` (json.rs:701, verified). 3 LACs grounded.

### 2B — 9 folds (FOLD-L1..L9) + first-cycle PV17-001..008 — ALL ACCEPT

The central divergence (Layer-1 realized as the Rust `PrimitiveKernels` struct
`dispatch.rs:50-56`, NOT the `bbnf.asm` macros `MASTER-PLAN:622`/`LOCKS:474`) is
correct — both anchors verified. PV17-003 (x86inc.asm skinny-only / totality-path
absent) and PV17-005 (eq-set is the one real NEON body; table_64/prefix_xor_64
are 4-LOC passthroughs) are live-confirmed. CH1-2B-01 fold (full S-P2 path)
landed at six sites. The L5/L6 net-new-mask folds (CH4-2b-001/002) re-anchor the
carry idiom and add the abrogate measurement gate correctly. 4 LACs grounded.

### 2C — 7 folds (SK17-2C-A..F + ONBOARD) + 8 grounding rows — ALL ACCEPT

CH1-2C-01 dual-ordinal fold landed (plane-(3) ≠ substrate-category-5th,
explicitly disambiguated at `2c:79,:176-178,:308`). CH2-V1-R4 fold binds the CSS
non-JSON consumer to the eq-set fan via the slot-59 collision (`SPEC:316-317`,
verified). CH6-V1-V01 reclassified ONBOARD as a verify_action with a live HEAD
baseline (7 string-ident sites in strategy.rs — `grep` returns 32 String/&str/
ident tokens, of which the 7-site catalogue is the named leak surface; the
@generated provenance distinction is grounded at `LOCKS.md:352-358`). The
by-construction-not-by-exercise split (sheets_witness stub, SK17L-009) is the
correctly-grounded fleet-wide-claim refutation. 2 LACs grounded.

### 2D — 7 folds (T2D17-…) + 7 grounding rows — ALL ACCEPT

CH1-2D-01 fold landed (simdjson "ONE tape" → arXiv:1902.08318; `:1203` retained
for sonic lineage). The cost-model pipeline class (eq-sat → CSP → cost-extraction)
is grounded against real publications (POPL 2009, POPL 2021, LOPLAS 1992, CP-SAT)
AND the live skinny pipeline (`lib.rs:392`/`:415`/`:498`/`:499` all verified). The
tautological-CSP / 0-rewrite refutation rows resolve to `backend_egraph.rs` +
`decision_csp.rs` (carried from prior 2D, re-confirmed). REVISE-2D-01 fold made
the SPEC §9 host-block the primary frame before the Lemire-2023 ICPP cite. 3 LACs
grounded.

### 2E — 6 folds (FOLD-2E-A..F) + 10 grounding rows — ALL ACCEPT

CH1-2E-01 fold landed (signature names → `MASTER-PLAN.md:622`; `ARCH:1284` =
four-LLVM-shapes context only) — see the premise-correction note above; the fold
is the stronger anchoring. The CRITICAL correctness fold of the cycle —
**CH7-001** — landed: the fabricated "recognizer beats lightningcss 2-3x" is
DELETED, the CSS lightningcss bar is correctly marked UNMEASURED-PENDING
(`2e:56-58`), and the >SOTA carrier claim re-cites the JSON recognizer >sonic-rs
on RESULTS.md:5-55 (verified verbatim). CH7-002 reworded "full typed-AST parity"
to the SK-V18 strict-equality gate obligation. CH6-V1-R01 deleted the defer-loop.
All x86/AVX-512 mentions sit in the refuted-route / `admits_collapsed_stage`
co-requirement / secondary-path context — never as a close route — honoring the
aarch64-only discipline (`2e:418-419` SPEC `:806` no-SVE). 4 LACs grounded.

### 2F — 9 folds (F1..F9) + 8 grounding rows — 8 ACCEPT, 1 REVISE

CH1-2F-01 fold landed (alphabet datum → `SPEC.md:314-317`). The F8 BackendShape
selector-wiring fold is grounded against the live `derive_backend_shape`
(skinny-only; `grep -rn 'enum BackendShape' crates/` = 0; the SK-V18 fold wires it
atop `EmitStrategy::StructDirect`). The F4-tape-category refutation (6th
BackendShape refused; ARCH:1206 corroborating anchor beyond LAC-1E-14) is sound.
F7 same-wave consumer named as the co-waved F1/F3 tape-wiring (CH4-2f-001 fold).
5 LACs grounded.

- **REVISE CH1-2F-01-RESIDUAL (`2f:580`):** The LAC-2F-FOLD-03 row cites
  `crates/simd-scan/src/alphabet.rs:118` as part of the classifier
  primitive-manifest grounding. The rich `StructuralAlphabet` struct + the
  load-bearing alphabet definition resolve at `alphabet.rs:19-37` (the anchor 2A
  and the V1 census both use, live-verified). The `:118` line is a downstream
  reference, not the struct/definition site the manifest row is grounding. **Fix:**
  re-anchor the LAC-2F-FOLD-03 `alphabet.rs` citation to `:19-37` (the struct +
  rich-alphabet definition), matching 2A's `LAC-2A-SKV17-02`. The underlying
  claim (simd-scan carries the alphabet-as-data classifier) is true and grounded
  at `:19-37`; the `:118` line-anchor is loose → REVISE, not REJECT. Single
  V3-foldable anchor-precision residual.

## Summary verdict

- **ACCEPT: 43 / 44 fold proposals/sections (97.7%).** Every fold traces to a
  named, live-verified T-P1 divergence (A/B/C/D/E/F) with a resolving
  file:line/SHA; every SOTA/ISA claim carries a real, finding-bearing citation;
  no confabulation found in any of the six dossiers.
- **V1 REVISE re-fold firewall: all 5 PASS.** CH1-2B-01 (full S-P2 path),
  CH1-2C-01 (dual-ordinal FactStream disambiguation), CH1-2D-01 (simdjson →
  arXiv:1902.08318), CH1-2E-01 (signature names → MASTER-PLAN:622), CH1-2F-01
  (alphabet datum → SPEC:314-317) each landed correctly on disk and re-verified
  against live source — no recorded-but-unapplied fold (the CH1-V5-001 pathology
  did not recur).
- **CH7-001 (the load-bearing correctness fold) landed:** the fabricated
  "recognizer beats lightningcss 2-3x" is deleted; the CSS lightningcss bar is
  UNMEASURED-PENDING; the >SOTA carrier re-grounds to the JSON >sonic-rs measured
  fact. This is the most consequential disposition of the cycle and it is clean.
- **REVISE: 1 — CH1-2F-01-RESIDUAL.** `alphabet.rs:118` line-anchor loose;
  re-point to `:19-37` (the struct + rich-alphabet definition). Anchor-precision
  only; claim true and grounded elsewhere; V3-foldable; zero orphan.
- **REJECT: 0.** No fold lacks a divergence antecedent; no cited paper or
  library-source citation failed to resolve; no benchmark number is unsourced; no
  x86/AVX/SVE close-route admitted; no silent 6th BackendShape.
- **CH1-V5-001:** VERIFIED FOLDED ON DISK at `91b6893b0` (enumerated
  `collapsed_stage}.rs` form ×3 in 1b; zero `,collapsed}` residual in 1a/1b/1e;
  `collapsed_stage.rs` exists, `collapsed_tape.rs` absent). T-P1 residual REVISE
  discharged.

ACCEPT rate 43/44 = **97.7%**, clearing the ≥95% convergence threshold. The fold
architecture is sound and stable: the substrate-manifest-not-6th-shape verdict
(LAC-1E-14 precedent + the independent `ARCH:1206` mechanical anchor), the
eager-OpenFrame retirement, the lazy-`ValueRef<G>` materialization plane (Lock-14
@generated-allowed, preserve-rich-ast), the Lock-16 NEON classifier entry
(eq-set fan the one proven body; table/prefix scalar-delegate-non-ASM), and the
StructRegistry/FieldSource fence (live coupling at `arena.rs:47`) are all soundly
grounded against live source. The lone residual is pure citation-hygiene.
