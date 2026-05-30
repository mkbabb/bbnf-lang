---
lens: CH1 CORRECTNESS
pass: T-P2-research (SK-V17 totality greater-spec fold)
cycle: V3
reviewer: CH1-V3
master_head: 91b6893b0
t_p1_source_sha: 445925167
generated_at: 2026-05-29T00:00:00Z
subject: greater-spec FOLD review — every fold proposal traces to a named T-P1 divergence + file:line/SHA; SOTA/ISA claims cited; CH1-V5-001 folded; the lone V2 REVISE (CH1-2F-01-RESIDUAL) re-verified folded against live source
dossiers_reviewed: [2a, 2b, 2c, 2d, 2e, 2f]
fold_proposals_dispositioned: 44
accept: 44
revise: 0
reject: 0
v2_revise_refolded: [CH1-2F-01-RESIDUAL]
hygiene_CH1_V5_001: VERIFIED-FOLDED-ON-DISK
accept_rate: 1.000
---

# CH1 CORRECTNESS — T-P2 V3 (SK-V17 totality fold)

## Scope + method

Per PASS-2-RESEARCH §3 CH1 + ORCHESTRATOR §3W: every cited paper / library-source
citation must resolve to the claimed path:line and carry the claimed finding;
every fold proposal must trace to a NAMED T-P1 divergence (A tape AoS↔SoA, B eager
OpenFrame, C value-API per-grammar-EAGER, D BackendShape 5-shape canon must absorb
tape-as-substrate, E NEON JSON-only vs shared classifier, F StructRegistry/
FieldSource fence) + file:line/SHA; SOTA/ISA claims must be cited; CH1-V5-001 (the
enumerated-filename residual) must be folded. Provenance gaps and confabulated
citations are REJECT; loose-but-resolvable anchors are REVISE.

V3 is the second post-V1 cycle. V2 closed all five V1 REVISEs and left exactly
ONE residual: **CH1-2F-01-RESIDUAL** (the `alphabet.rs:118` line-anchor used for
the `StructuralAlphabet` manifest grounding, where the struct definition resolves
at `:19-37` and `:118` is the orthogonal `KernelShape::select`). The central V3
firewall obligation, identical in spirit to the CH1-V5-001 pathology this lens
hunts hardest, is to re-open that one REVISE and independently re-verify the fold
landed on disk WITHOUT introducing a new mis-anchor. Every load-bearing anchor
across the six dossiers was re-greped live at master HEAD `91b6893b0`: skinny
tape/ValueRef/classifier, core TapeRec/OpenFrame/begin_compound, ir struct.rs
registry, the cost-model pipeline, LOCKS/ARCH/MASTER-PLAN/SPEC surfaces, the
scalar-delegate bodies, x86inc/checkasm presence, the RESULTS.md >SOTA numbers,
and the SOTA literature set.

## Hygiene — CH1-V5-001 (first action, per dispatch)

**VERIFIED FOLDED ON DISK — ACCEPT.** Live verification at `91b6893b0`:

- `grep -c ',collapsed}' {1a,1b,1e}` = `0 / 0 / 0` — zero residual brace-glob in
  any of the three excavation files the dispatch names.
- `grep -c 'collapsed_stage}.rs' 1b` = `3` — the enumerated executing form
  (`…/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs`) is present at
  `1b:12` (`live_truth_method`) and `1b:97` (BSHAPE17-004 row).
- `skinny/crates/codegen/src/lower/collapsed_stage.rs` EXISTS;
  `collapsed_tape.rs` DOES NOT (the brace-glob never expands to a non-existent
  path).

The dispatch directs folding on first touch of 1a/1e; the defect lived only in
1b, and 1b is already correct on disk — no 1a/1e edit required. All six dossier
frontmatters report the hygiene action with the exact verifying grep (2A
`live_reverify_at_head` carries `admits_collapsed_stage ARCH:1206`; 2B
`t_p1_entry_state: CONVERGED-V6-confirm-…-CH1-V5-001-resolved`; 2C
`hygiene_action_CH1_V5_001`; 2D `first_hygiene_action_folded`; 2E
`hygiene_action_folded: CH1-V5-001`; 2F `hygiene_ch1_v5_001:
folded-confirmed-resolved`). The brace-glob occurrences remaining in the T-P1
hardening artefacts (V5/V6-confirm CH1/CH6, CONSOLIDATED) are QUOTED-AS-THE-DEFECT
historical records and are correctly left untouched. **Hygiene satisfied; the T-P1
residual REVISE is discharged at this SHA.**

## V2 REVISE re-fold firewall — CH1-2F-01-RESIDUAL re-verified against live source

The central V3 obligation. The lone V2 REVISE was a citation-precision defect
whose underlying claim was true (`crates/simd-scan` carries the alphabet-as-data
classifier manifest); the V3 fold had to re-point the `alphabet.rs` line-anchor to
the verified struct-definition site WITHOUT a new mis-anchor. The fold landed in
THREE dossiers (2D first claimed it via FOLD-2D-05; 2F via LAC-2F-FOLD-03; 2A
confirmed its existing anchor was already correct).

| live fact (re-greped at `91b6893b0`) | result |
|---|---|
| `alphabet.rs:19` = `pub struct StructuralAlphabet {` | RESOLVES |
| `alphabet.rs:19-37` = the struct body: `singletons:&'static [u8]` (:23), `digraph_mask:[u64;4]` (:28), `digraph_pairs:&'static [(u8,u8)]` (:31), `quote_classes:&'static [u8]` (:37) — the rich-alphabet manifest fields | RESOLVES |
| `alphabet.rs:118` = `pub fn select(alphabet: &StructuralAlphabet) -> Self {` (the `KernelShape::select` site) — orthogonal to the struct definition | RESOLVES |

Fold disposition by dossier:

| dossier site | folded edit (live-verified) | verdict |
|---|---|---|
| **2F LAC-2F-FOLD-03 (`2f:582`)** | now anchors `alphabet.rs:19-37` for the `StructuralAlphabet` manifest grounding (`singletons`/`digraph_mask`/`digraph_pairs`/`quote_classes`), with the explicit inline note "`:118` is the orthogonal `KernelShape::select(alphabet)` site cited in the F5 body, not the struct definition". The F5 body retains `:118` for `KernelShape::select` (`2f:292`) — correctly. Frontmatter `2f:20` records the disposition source verbatim. | **ACCEPT** |
| **2D FOLD-2D-05 / T2D17-row (`2d:76`) + LAC-2D-S17-03 (`2d:296`) + source list (`2d:324`)** | all three now cite `alphabet.rs:19-37` for the `StructuralAlphabet` manifest, with `:118` parenthetically scoped to `KernelShape::select` only. Frontmatter `2d:16` records the fold. | **ACCEPT** |
| **2A LAC-2A-SKV17-02 (`2a:439`) + body (`2a:253`,`:469`)** | already cited `alphabet.rs:19-37` since V1; the residual was a 2F/2D-owned re-anchor. 2A frontmatter `2a:32` records `CH1-2F-01-RESIDUAL-CONFIRMED` — no 2A edit required, and none made. | **ACCEPT** |

**No new mis-anchor introduced.** The only remaining `alphabet.rs:118`-for-the-
struct occurrences across the tree are inside `hardening/V2/CH1.md:205,:232` —
the V2 disposition record quoting the defect it raised, correctly preserved as
history. The fold is the strictly-stronger anchoring (struct definition is the
authoritative manifest locus; `:118` is a downstream selector). The CH1-V5-001
recorded-but-unapplied pathology did NOT recur. **CH1-2F-01-RESIDUAL: DISCHARGED.**

## Provenance census — every load-bearing anchor re-verified at HEAD

All anchors below resolved VERBATIM at `91b6893b0`. No confabulated citation
found in any of the six dossiers.

| anchor (dossier-cited) | live-verified content | verdict |
|---|---|---|
| `skinny/.../tape/mod.rs:94-100` SoA `Tape<'input>` six members | `pub struct Tape<'input> { source, offsets:Vec<u32>, flag_cursors:Vec<u32>, flag_values:Vec<u8>, payloads:PayloadArena, id:TapeId }` | RESOLVES |
| `skinny/.../tape/mod.rs:175` `ValueRef<'doc,'input:'doc,K=AnyKind,G:EventGrammar=AnyGrammar>` | exact signature | RESOLVES |
| `skinny/.../grammars/json/value.rs:143` `value_from_ref` | `pub(crate) fn value_from_ref<'doc,'input:'doc>(` | RESOLVES |
| `skinny/.../bbnf-simd/src/dispatch.rs:42` `select_classifier(&'static [u8;64]) -> SelectedClassifier` | exact signature | RESOLVES |
| `dispatch.rs:50-56` `PrimitiveKernels` (5 fn-ptr fields) | exact struct (`byte_class_from_table_64`, `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `eob_pad_clamp`) | RESOLVES |
| `crates/core/.../tape/record.rs:102-103,:120-121` AoS `TapeRec` `#[repr(C,align(4))]` + 16-byte/align-4 const-asserts | exact | RESOLVES |
| `crates/core/.../css_l4/builder.rs:16,:71,:74-79` `enum OpenFrame<'p>` + `pending_value:Option<CssTypedValue<'p>>` + six `pending_*` Vecs | exact | RESOLVES |
| `crates/core/.../tape/mod.rs:185-186` `begin_compound(&StructLayout)` reads `layout.rule_id & 0x1F` | exact | RESOLVES |
| `crates/ir/.../struct.rs:84,:202,:313,:331` `enum FieldSource`/`struct StructLayout`/`struct StructRegistry`/`fn layout(rule_id) -> Option<&StructLayout>` | exact | RESOLVES |
| `crates/core/.../bbnf/arena.rs:47` LIVE coupling site (F-fence / CH5 fence target) | `match StructRegistry::compound_kind_for_layout(layout) {` | RESOLVES |
| `crates/core/.../grammar/generated/json.rs:701` `OnceCell<StructuralIndex>` (substrate_target=existing_tape PRE-condition antecedent) | `pub(crate) structural_index: ::core::cell::OnceCell<` | RESOLVES |
| `LOCKS.md:100-108` LAC-1E-14 FactStream "5th substrate category … NOT a 6th `BackendShape` variant" + 5-shape domain `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` | verbatim | RESOLVES |
| `ARCH:1206` `CollapsedStage` `target.arch==x86`+`avx512bw`+`Entry(_)`; "aarch64 mechanically refused"; UNKNOWN-2D-05; marker lowerer `collapsed_stage.rs:15-17` | verbatim | RESOLVES |
| `ARCH:1803` plane-table row (3) Fact-stream + "Per LAC-1E-14 this is the 5th SUBSTRATE-manifest category, not a 6th BackendShape variant"; `admitted_fact_output` | verbatim (CH1-2C-01 anchor) | RESOLVES |
| `ARCH:1203` `OffsetTape` row "(sonic-rs lazy-value lineage) … NOT-ADMITTED" | verbatim (CH1-2D-01 lineage anchor) | RESOLVES |
| `MASTER-PLAN.md:622` H.W2.5 macro vocabulary (the 8 signature names + admission-consumed-only + `ext/x86/bbnf.asm` Layer-1 / `ext/x86/x86inc.asm` Layer-0) | verbatim (CH1-2E-01 + 2B canonical anchor) | RESOLVES |
| SK-V17 `SPEC.md:312-317` alphabet-as-data Lock-14 vehicle / CSS `;{` eq-set fan / `& 0x3f` slot-59 collision | verbatim (CH1-2F-01 anchor) | RESOLVES |
| `SPEC.md:791-794` AZ-IV eager 118× regression; StructRegistry/Arena/Builder 28-65× / 983× / 10583× | verbatim (F-fence + LAC-2F-FOLD-04 + T2D17-FIELDSOURCE-FENCE anchors) | RESOLVES |
| cost-model: `skinny/.../passes/src/lib.rs:392` `derive_backend_shape`; `:401` `_with_diagnostics`; `:473` `choose_backend_shape`; `crates/ir/.../strategy.rs:104,:107` `enum EmitStrategy`/`StructDirect{`; root `Cargo.toml:2` egraph+csp-solver members | verbatim (2D/2F D-fold anchors) | RESOLVES |
| `x86inc.asm` present `skinny/crates/bbnf-simd/ext/x86/x86inc.asm` / ABSENT in `crates/`; checkasm harness present (`checkasm_parity.rs`, `checkasm_common.rs`, + per-primitive incl. `checkasm_byte_class_from_eq_set_64.rs`, `checkasm_byte_class_from_table_64.rs`, `checkasm_structural_terminator_64.rs`) | verbatim (2B PV17-003/004 refutation rows) | RESOLVES |
| scalar-delegate bodies: `aarch64/byte_class_from_table_64.rs:1-4` = scalar passthrough `crate::scalar::byte_class_from_table_64::byte_class_from_table_64_scalar`; `byte_class_from_eq_set_64.rs:33` = real `#[cfg(target_arch="aarch64")]` NEON body | verbatim (2A/2B/2E scalar-delegate-vs-NEON-body split) | RESOLVES |
| `crates/simd-scan/src/alphabet.rs:19-37` `StructuralAlphabet` struct + fields; `:118` `KernelShape::select` | verbatim (CH1-2F-01-RESIDUAL re-anchor target) | RESOLVES |
| `StructLayout` introduce-site census = 960; `enum BackendShape` in `crates/` = 0; `derive_backend_shape` in `crates/` = 0 (skinny-only) | `grep -rc StructLayout crates/` sum=960; `grep -rn 'enum BackendShape' crates/`=0; selector skinny-only | RESOLVES |
| `skinny/RESULTS.md:5-12` twitter parse_only 8349.290 > sonic 4913.095; direct 17585.679 > strict sonic 14857.624; typed 10705.052 > typed sonic 8952.253; citm/canada GO rows | verbatim (>SOTA carrier) | RESOLVES |
| SOTA literature: simdjson VLDB-J 2019 (arXiv:1902.08318), Mison VLDB 2017, eq-sat POPL 2009, egg POPL 2021, BURG LOPLAS 1992 (DOI 10.1145/151640.151642), OR-Tools CP-SAT, asmjson/Sneller AVX-512 (Lemire 2023 ICPP), Lemire 2019 / Validark 2024 NEON byte-classify | all real; all carry the claimed finding; x86/AVX cites all framed diagnostic/host-blocked | RESOLVES |

## Fold-proposal disposition census

44 fold proposals/sections across the six dossiers. Each verified for (i) a named
T-P1 divergence antecedent (A/B/C/D/E/F + SUB17-/D-1E-SKV17-/SK17L-/BSHAPE17-/
COH17- IDs), (ii) file:line/SHA grounding, (iii) cited SOTA/ISA where claimed.

### 2A — 6 folds (FOLD-2A-A..F) + 8 grounding rows — ALL ACCEPT

Both refutation rows match the literature's actual position: CollapsedStage as a
NEON route is mechanically refused by `ARCH:1206` (`target.arch==x86` co-required);
"grammar-general across 8 grammars" is correctly demoted to config-breadth
value-plane (JSON+CSS exercised only — CH2-V1-R3 fold). FOLD-2A-E (NEON classifier
Lock-16 entry) carries the per-primitive admission (eq-set fan the one real NEON
body, `byte_class_from_table_64.rs:1-4` the live scalar delegate — verified).
FOLD-2A-F (StructRegistry/FieldSource fence) anchors the live coupling at
`arena.rs:47`. LAC-2A-SKV17-02's `alphabet.rs:19-37` anchor was already correct;
the V3 frontmatter confirms it (`CH1-2F-01-RESIDUAL-CONFIRMED`). 3 LACs grounded.

### 2B — 9 folds (FOLD-L1..L9) + PV17-001..008 — ALL ACCEPT

The central divergence (Layer-1 realized as the Rust `PrimitiveKernels` struct
`dispatch.rs:50-56`, NOT the `bbnf.asm` macros at `MASTER-PLAN:622`) is correct —
both anchors verified. PV17-003 (x86inc.asm skinny-only / totality-path absent)
and PV17-005 (eq-set is the one real NEON body; `table_64`/`prefix_xor_64` are
scalar passthroughs) are live-confirmed. CH1-2B-01 (full S-P2 path) carried into
V3. The V2 REVISEs CH2-V2-R1 (L6 mask-only by-construction-not-by-exercise) and
CH5-V2-001 (L5/L6 `retention_lifetime=transient-single-call` canonical token)
both folded. 4 LACs grounded.

### 2C — 7 folds (SK17-2C-A..F + ONBOARD) + 8 grounding rows — ALL ACCEPT

CH1-2C-01 dual-ordinal fold held (plane-table row (3) ≠ substrate-category 5th;
`ARCH:1803` carries both clauses, neither a shape — verified verbatim). CH2-V1-R4
binds the CSS non-JSON consumer to the eq-set fan via the slot-59 collision
(`SPEC:316-317`). The CH6-V2-COUNT-NIT (ONBOARD "8 grammar dirs" → 9) folded
ground-truth-exact (`find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type
d` = 9). The by-construction-not-by-exercise fleet-wide-claim refutation
(sheets_witness stub, SK17L-009) is correctly grounded. 2 LACs grounded.

### 2D — 7 folds (T2D17-…) + 7 grounding rows — ALL ACCEPT

CH1-2D-01 held: simdjson "builds ONE tape" anchored to the PAPER (arXiv:1902.08318)
with `:1203` retained ONLY for the sonic-rs lineage note — the exact
external-technique-vs-local-anchor discipline the V1 REVISE demanded, now
gold-standard at `2d:73,:75,:309`. The cost-model class (eq-sat → CSP → cost
extraction) is grounded against real publications (POPL 2009, POPL 2021, LOPLAS
1992 DOI 10.1145/151640.151642, CP-SAT) AND the live skinny pipeline
(`lib.rs:392`/`:401`/`:473`). T2D17-AARCH64-COLLAPSEDSTAGE-UNKNOWN-2D-05 frames
asmjson as host-blocked SPEC §9 (`:851-852`), diagnostic-only on x86 — never an
aarch64 latent candidate. CH1-2F-01-RESIDUAL folded (FOLD-2D-05 `:19-37`). 3 LACs.

### 2E — 6 folds (FOLD-2E-A..F) + 10 grounding rows — ALL ACCEPT

CH1-2E-01 held (signature names → `MASTER-PLAN.md:622`; `ARCH:1284` four-LLVM-shapes
context only). The load-bearing **CH7-001** stays discharged: the fabricated
"recognizer beats lightningcss 2-3×" is DELETED, the CSS lightningcss bar is
UNMEASURED-PENDING (`2e:57-59`, `:199`, `:412-413`), and the >SOTA carrier
re-grounds to the JSON recognizer >sonic-rs measured fact (RESULTS.md:5-12,
verified verbatim). CH7-002 (parity → SK-V18 strict-equality gate) held.
CH5-V2-001-COHORT-ALIGN folded `retention_lifetime=transient-single-call` onto the
classifier surfaces, cross-dossier-consistent with 2A/2B/2F. All x86/AVX-512
mentions sit in refuted-route / `admits_collapsed_stage` co-requirement /
secondary-path context — never a close route (aarch64-only discipline,
`SPEC:806` no-SVE). 4 LACs grounded.

### 2F — 9 folds (F1..F9) + 8 grounding rows — ALL ACCEPT (V2 REVISE folded)

CH1-2F-01-RESIDUAL folded: LAC-2F-FOLD-03 (`2f:582`) re-anchored
`alphabet.rs:118` → `:19-37` for the `StructuralAlphabet` manifest grounding,
`:118` retained for `KernelShape::select` in the F5 body. The F8 BackendShape
selector-wiring fold is grounded against the live skinny-only
`derive_backend_shape` (`grep -rn 'enum BackendShape' crates/`=0; SK-V18 wires it
atop `EmitStrategy::StructDirect` `strategy.rs:104`). F4-tape-category refutation
(6th BackendShape refused; LAC-1E-14 + corroborating `ARCH:1206`). LAC-2F-FOLD-04
(no-per-leaf-registry-lookup fence; `SPEC:793-795` + `struct.rs:331` +
`tape/mod.rs:185-186`) is sound. LAC-2F-FOLD-05 path-(b) sizing (`LayoutFacts`
grep-zero in crates/, so non-zero/UNKNOWN) is correctly priced. 5 LACs grounded.

## Discipline checks (aarch64-only / no-6th-shape / no-reopened-REDRESS)

- **No silent 6th BackendShape.** Every "6th BackendShape" mention across the six
  dossiers is a REFUTATION (`2c:67,:339`, `2e:102,:449`): Lock 10 bars it, the tape
  is the substrate the 5 shapes project from (substrate-manifest category per
  LAC-1E-14 + corroborating `ARCH:1206`), a 6th variant remains G-Omega gated.
- **aarch64-only honored.** Every x86/AVX-512 reference is framed refuted /
  host-blocked / secondary / architecture-pressure (`2a:308-309,:419` scope
  reconcile WITHOUT admitting x86; `2d:78,:242,:249` CollapsedStage x86-pinned;
  `2e` close-state taxonomy). No SVE close route. No close route admits x86.
- **No re-opened REDRESS.** No dossier grounds AZ-IV eager, StructRegistry
  indirection, fact-stream-as-admission, broadcast, FNV, or x86 as viable; the
  fences (LAC-2F-FOLD-04, T2D17-FIELDSOURCE-FENCE) keep AZ-IV pre-blocked.
- **preserve-rich-ast.** The lazy `ValueRef<G>` materialization plane is grounded
  as the unified projection lifting per-grammar EAGER value enums to the one
  grammar-parametric projection (1d SK17L-002), never flattening the typed AST.

## Summary verdict

- **ACCEPT: 44 / 44 fold proposals/sections (100%).** Every fold traces to a
  named, live-verified T-P1 divergence (A/B/C/D/E/F) with a resolving
  file:line/SHA; every SOTA/ISA claim carries a real, finding-bearing citation;
  no confabulation found in any of the six dossiers.
- **V2 REVISE re-fold firewall: CH1-2F-01-RESIDUAL PASS.** The `alphabet.rs:118`
  → `:19-37` re-anchor landed correctly in 2F (LAC-2F-FOLD-03) and 2D (FOLD-2D-05
  + LAC-2D-S17-03 + source list); 2A's existing `:19-37` confirmed; `:118`
  correctly retained for `KernelShape::select`. No recorded-but-unapplied fold;
  the CH1-V5-001 pathology did not recur.
- **CH7-001 (load-bearing correctness fold) remains discharged:** the fabricated
  "recognizer beats lightningcss 2-3×" stays deleted; the CSS lightningcss bar is
  UNMEASURED-PENDING; the >SOTA carrier re-grounds to the JSON >sonic-rs measured
  fact (verified verbatim at RESULTS.md:5-12).
- **REVISE: 0.** The lone V2 residual is folded; no new loose anchor introduced.
- **REJECT: 0.** No fold lacks a divergence antecedent; no cited paper or
  library-source citation failed to resolve; no benchmark number is unsourced; no
  x86/AVX/SVE close-route admitted; no silent 6th BackendShape.
- **CH1-V5-001:** VERIFIED FOLDED ON DISK at `91b6893b0` (enumerated
  `collapsed_stage}.rs` ×3 in 1b; zero `,collapsed}` residual in 1a/1b/1e;
  `collapsed_stage.rs` exists, `collapsed_tape.rs` absent). T-P1 residual REVISE
  discharged.

ACCEPT rate 44/44 = **100%**, clearing the ≥95% convergence threshold for the
second consecutive cycle (V2 97.7% → V3 100%, zero orphan REVISE). The fold
architecture is sound and stable: the substrate-manifest-not-6th-shape verdict
(LAC-1E-14 precedent + the independent `ARCH:1206` mechanical anchor), the
eager-OpenFrame retirement (`css_l4/builder.rs:16` live target), the lazy-
`ValueRef<G>` materialization plane (Lock-14 @generated-allowed, preserve-rich-ast),
the Lock-16 NEON classifier entry (eq-set fan the one proven body; table/prefix
scalar-delegate-non-ASM), and the StructRegistry/FieldSource fence (live coupling
at `arena.rs:47`, regression numbers at `SPEC:791-794`) are all soundly grounded
against live source at `91b6893b0`. CH1 returns a clean, citation-firewalled
cycle.
