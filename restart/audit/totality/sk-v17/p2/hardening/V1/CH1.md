---
lens: CH1 CORRECTNESS
pass: T-P2-research (SK-V17 totality)
cycle: V1
reviewer: CH1-V1
master_head: 91b6893b0
t_p1_source_sha: 445925167
generated_at: 2026-05-29T00:00:00Z
subject: greater-spec FOLD review — every fold proposal traces to a named T-P1 divergence + file:line/SHA; SOTA/ISA claims cited; CH1-V5-001 folded
dossiers_reviewed: [2a, 2b, 2c, 2d, 2e, 2f]
fold_proposals_dispositioned: 44
accept: 39
revise: 5
reject: 0
hygiene_CH1_V5_001: VERIFIED-FOLDED-ON-DISK
---

# CH1 CORRECTNESS — T-P2 V1 (SK-V17 totality fold)

## Scope + method

Per PASS-2-RESEARCH §3 CH1: every cited paper/library-source citation must
resolve to the claimed path:line and carry the claimed finding; every fold
proposal must trace to a NAMED T-P1 divergence + file:line/SHA; SOTA/ISA claims
must be cited; CH1-V5-001 (the enumerated-filename residual) must be folded.
Provenance gaps and confabulated citations are REJECT; loose-but-resolvable
anchors are REVISE.

Method: live re-verification at master HEAD `91b6893b0` of every load-bearing
anchor across the six dossiers — skinny tape/ValueRef/classifier, core
TapeRec/OpenFrame/value/begin_compound, struct.rs registry, LOCKS/ARCH spec
surfaces, SK-V17 SPEC, scalar-delegate bodies, x86inc/checkasm presence,
cost-model pipeline anchors, and the SOTA literature set. The T-P1 divergence
IDs (SUB17-002..009, D-1E-SKV17-01..06, SK17L-00x, BSHAPE17-00x, COH17-00x)
were grepped present in the locked 1a/1b/1e inventories.

## Hygiene — CH1-V5-001 (first action, per dispatch)

**VERIFIED FOLDED ON DISK — ACCEPT.** The dispatch mandates folding CH1-V5-001
(the `1b:12` + `1b:97` brace-glob `{eager,offset,event,collapsed}_tape.rs` that
expands to the non-existent `collapsed_tape.rs`) on first touch of 1a/1e. Live
verification at `91b6893b0`:

- `1b-codegen-evidence.md:12` (`live_truth_method`) and `:97` (BSHAPE17-004 row)
  carry the enumerated executing form
  `…/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs = 17 each,
  sink_only.rs = 270`. Confirmed verbatim on disk.
- `grep -c ',collapsed}' {1a,1b,1e}` = 0 (exit 1, no residual brace-glob).
- `skinny/crates/codegen/src/lower/collapsed_stage.rs` EXISTS;
  `collapsed_tape.rs` DOES NOT (the brace-glob never expands to a non-existent
  path).

The T-P1 CONVERGED verdict (`HARDENING-T-P1-SKV17-CONVERGED.md:80,:85`) records
that at V6-confirm CH1/CH6 re-flagged the defect as "STILL PRESENT (V5 recorded
the fix, never applied it)". At master HEAD the enumerated form IS present on
disk; the prior-cycle fix landed in the commit the convergence was confirmed
against. The four dossiers that touch the hygiene action (2A frontmatter, 2C
`hygiene_action_CH1_V5_001`, 2E `:69-75`, 2F `:25-39`) correctly report
RESOLVED-ON-DISK with the exact verifying grep. No 1a/1e edit was required (the
defect lived only in 1b, and 1b is already correct). **Hygiene satisfied;
residual REVISE discharged.**

## Provenance census — every load-bearing anchor re-verified at HEAD

All anchors below resolved VERBATIM at `91b6893b0`. No confabulated citation
found in any dossier.

| anchor (dossier-cited) | live-verified content | verdict |
|---|---|---|
| `skinny/.../tape/mod.rs:94-100` SoA `Tape<'input>` six members | `pub struct Tape<'input> { source, offsets, flag_cursors, flag_values, payloads, id }` | RESOLVES |
| `skinny/.../tape/mod.rs:175` `ValueRef<'doc,'input,K,G:EventGrammar>` | exact signature | RESOLVES |
| `skinny/.../grammars/json/value.rs:143` `value_from_ref` | `pub(crate) fn value_from_ref<'doc,'input:'doc>(` | RESOLVES |
| `skinny/.../bbnf-simd/src/dispatch.rs:42` `select_classifier(&[u8;64])` | exact signature | RESOLVES |
| `dispatch.rs:50-56` `PrimitiveKernels` (5 fn-ptr fields) | exact struct (table_64, prefix_xor_64, next_set_bit, bulk_emit_positions_64, eob_pad_clamp) | RESOLVES |
| `crates/core/.../tape/record.rs:103,:120-121` AoS `TapeRec` + 16-byte/align-4 asserts | `pub struct TapeRec;` `assert size_of==16`/`align_of==4` | RESOLVES |
| `crates/core/.../css_l4/builder.rs:16,:71,:74-79` `OpenFrame` + pending fields | `enum OpenFrame<'p>`; `pending_value:Option`; six `pending_*` Vecs | RESOLVES |
| css/json builder LOC (817 / 231) | `wc -l` = 817 / 231 | RESOLVES |
| `css_l4/value.rs:414` `CssTypedValue<'p>` (eager enum) | `pub enum CssTypedValue<'p>` | RESOLVES |
| `crates/core/.../tape/mod.rs:6-9,:54-56,:185-186` AoS-first doc / dispatch-on-StructLayout / `begin_compound(&StructLayout)` reads `rule_id & 0x1F` | exact | RESOLVES |
| `crates/ir/.../struct.rs:84,:202,:313,:331` FieldSource/StructLayout/StructRegistry/layout() | exact | RESOLVES |
| `crates/simd-scan/src/lib.rs:80` `scan_structural(&StructuralAlphabet)`; `alphabet.rs:19-37` rich alphabet | exact | RESOLVES |
| `LOCKS.md:100-116` LAC-1E-14 FactStream "5th substrate-manifest category … NOT a 6th BackendShape" | verbatim | RESOLVES |
| `LOCKS.md:107-108` 5-shape canon `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` | verbatim | RESOLVES |
| `LOCKS.md:118-127` substrate manifest (4 targets / 3 lifetimes / 3 owners) | verbatim | RESOLVES |
| `LOCKS.md:75` `Vec<OpenFrame>::clone` 86.07% samply pathology / "parallel substrates are dead" | verbatim | RESOLVES |
| `LOCKS.md:349` Lock 14 "CSS L4 14-variant `OpenFrame`" named overfit | verbatim | RESOLVES |
| `LOCKS.md:454-461` NEON allowlist `vqtbl4q_u8` (Lemire 2019) | verbatim | RESOLVES |
| `LOCKS.md:474` `x86inc.asm` vendored citation; `MASTER-PLAN.md:622` `bbnf.asm` macro names | verbatim (the 2B central divergence anchor) | RESOLVES |
| `LOCKS.md:507` `scalar-delegate-non-ASM` close-state | verbatim | RESOLVES |
| `ARCH:1088` "the five ways the substrate may project" / "structural projection IS the tape" | verbatim | RESOLVES |
| `ARCH:1206` `admits_collapsed_stage` x86+avx512bw+Entry / aarch64 mechanically refused / UNKNOWN-2D-05 | verbatim | RESOLVES |
| `ARCH:1799-1804` 4-plane output table (plane (3) FactStream = 5th substrate category) | verbatim | RESOLVES |
| `SPEC.md:791,:793-795` AZ-IV 118× / StructRegistry 28-65×/983×/10583× | verbatim | RESOLVES |
| `SPEC.md:314-317` alphabet-as-data Lock-14 vehicle / CSS `;{` eq-set fan | verbatim | RESOLVES |
| `SPEC.md:806,:808` aarch64-only no-SVE / "sixth `BackendShape`" pre-block | verbatim | RESOLVES |
| scalar-delegate bodies: `aarch64/byte_class_from_table_64.rs:3` + `bitmap_prefix_xor_64.rs` = 4-LOC scalar passthroughs; `byte_class_from_eq_set_64.rs:33` real NEON (`vld1q_u8` stripes) | verbatim (2B A4 claim) | RESOLVES |
| `x86inc.asm` present in skinny / ABSENT in totality; `checkasm_common.rs` + `guarded_call` present | verbatim (2B A1/A2/refutation rows) | RESOLVES |
| cost-model: `skinny/.../passes/lib.rs:392` `derive_backend_shape`; `:498` `backend_egraph::select`; `:499` `decision_csp::finalize_rule`; `ir/lib.rs:340` `enum BackendShape`; root `Cargo.toml:2` egraph+csp-solver present | verbatim (2D/2F D-fold anchors) | RESOLVES |
| `crates/ir/.../strategy.rs:104` `enum EmitStrategy` / `:107` `StructDirect {` variant | RESOLVES (note below) | RESOLVES |
| `skinny/RESULTS.md:5-55` Track 1 > sonic-rs strict (8349>4913 twitter parse_only; 17585>14857 direct) | verbatim (2A/2E >SOTA carrier claim) | RESOLVES |
| SOTA literature: simdjson VLDB-J 2019 (arXiv 1902.08318), Mison VLDB 2017, eq-sat POPL 2009, egg POPL 2021, BURG LOPLAS 1992 (DOI 10.1145/151640.151642), asmjson/Sneller AVX-512, Lemire 2019 NEON byte-map | all real, all carry the claimed finding | RESOLVES |
| `HARDENING-S-P2-V3-CONSOLIDATED.md` L1–L9 LOCKED pool | present at `restart/skinny/tranches/sk-v17/research/p2/hardening/` | RESOLVES |

## Fold-proposal disposition census

44 fold proposals/sections across the six dossiers. Each verified for (i)
named T-P1 divergence antecedent, (ii) file:line/SHA grounding, (iii) cited
SOTA/ISA where claimed.

### 2A — 6 folds (FOLD-2A-A..F) + 8 grounding rows

| fold | T-P1 antecedent (verified) | disposition |
|---|---|---|
| FOLD-2A-A flat-tape AoS↔SoA | §A / SUB17-002 / SK17L-001 / D-1E-SKV17-01 | ACCEPT |
| FOLD-2A-B eager OpenFrame retirement | §B / SUB17-003 / SK17L-003 | ACCEPT |
| FOLD-2A-C lazy ValueRef<G> plane | §C / SUB17-004 / SK17L-002 / D-1E-SKV17-02 | ACCEPT |
| FOLD-2A-D tape = substrate-manifest, not 6th shape | §D / SUB17-005 / SK17L-006 / D-1E-SKV17-04 / LAC-1E-14 | ACCEPT |
| FOLD-2A-E shared NEON classifier Lock-16 entry | §E / SUB17-007/008 / D-1E-SKV17-06 | ACCEPT |
| FOLD-2A-F StructRegistry/FieldSource fence | §F / SUB17-009 / D-1E-SKV17-03 | ACCEPT |

Both refutation rows (CollapsedStage-as-NEON-route; JSON-scanner framing) match
the literature's actual position (ARCH:1206 mechanically refuses aarch64;
classifier is alphabet-parametric across 8 grammars). 3 LACs grounded. **All 6
ACCEPT.**

### 2B — 9 folds (FOLD-L1..L9) + 11 grounding rows

All L1–L9 trace to §A/B/C/E/F antecedents with verified IDs; the central
divergence (Layer-1 realized as Rust `PrimitiveKernels`, NOT `bbnf.asm` macros)
is correct — MASTER-PLAN:622 authored the macro names, dispatch.rs:50-56 realized
them in Rust. The scalar-delegate refutation (table_64/prefix_xor_64 are 4-LOC
passthroughs) is live-confirmed. eq-set fan is the one real NEON body. 4 LACs
grounded.

- **REVISE CH1-2B-01 (`2b:118,:151,:309`):** the S-P2 LOCKED pool is cited as
  `HARDENING-S-P2-V3:92-251` WITHOUT the resolving path. The file is
  `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
  (verified present). The abbreviated citation resolves on inspection but is not
  a standalone path:line per §2.1's verifiable-citation requirement. **Fix:**
  expand to the full path on every `HARDENING-S-P2-V3:` reference. Not a
  confabulation; an under-specified-but-real anchor → REVISE, not REJECT.

Remaining 8 folds + 11 rows: **ACCEPT.**

### 2C — 7 folds (SK17-2C-A..F + ONBOARD) + 8 grounding rows

All trace to §A–F + the Lock-14 v+1 gate; the by-construction-not-by-exercise
split (`sheets_witness` 24-LOC stub, SK17L-009) is correctly grounded as the
fleet-wide-claim refutation. 2 LACs grounded.

- **REVISE CH1-2C-01 (`2c:74,:171,:263,:315`):** 2C cites `ARCH:1803` as
  "`FactStream`=output-plane category #3" / "`FactStream`=category #3, NOT a
  shape". The live `ARCH:1803` is plane row **(3) Fact-stream output**, which
  DOES carry "Per LAC-1E-14 this is the 5th SUBSTRATE-manifest category, not a
  6th BackendShape variant." The content is correct, but 2C's prose conflates
  the plane-table ordinal "(3)" with the LAC-1E-14 substrate-manifest ordinal
  "5th" — two different numbering axes (output-plane index vs substrate-category
  index). **Fix:** state "plane (3) in the ARCH output-plane table = the 5th
  substrate-manifest category (LAC-1E-14)" to disambiguate the two ordinals. The
  anchor resolves and the claim is sound; the dual-ordinal phrasing is
  imprecise → REVISE.

Remaining 6 folds + grounding rows: **ACCEPT.**

### 2D — 7 folds (FOLD-2D-01..07) + 7 grounding rows

All trace to §A–F with verified IDs; the cost-model pipeline class (eq-sat →
CSP → cost-extraction) is grounded against real publications and the live skinny
pipeline (passes/lib.rs:392/:498/:499 verified). The tautological-CSP /
0-rewrite refutation (carried from the prior 2D) resolves to the cited skinny
sources. 3 LACs grounded.

- **REVISE CH1-2D-01 (`2d:64,:66,:288`):** 2D cites `ARCH:1203` as "sonic-rs
  lazy-value lineage" for both `T2D17-COST-SELECTS-INTO-UNIFIED-TAPE` (Langdale &
  Lemire) and `T2D17-LAZY-VALUEREF` (Mison/sonic). Live `ARCH:1203` is the
  `OffsetTape` admission-ledger row carrying "(sonic-rs lazy-value lineage)" —
  correct, but the same `:1203` is also leaned on as the simdjson "ONE tape"
  grounding which is a separate external claim (the simdjson arXiv 1902.08318
  paper, not the ARCH line). **Fix:** keep `:1203` for the sonic-rs lineage note;
  attach the "simdjson builds ONE tape" claim to the arXiv paper citation, not to
  `ARCH:1203`. The external papers are real and carry the findings; the
  local-anchor reuse is loose → REVISE.

Remaining folds + grounding rows: **ACCEPT.**

### 2E — 6 folds (FOLD-2E-A..F) + 10 grounding rows

All trace to §A–F with verified IDs; the >SOTA-carrier claim (RESULTS.md:5-55)
and the substrate-manifest/plane-(1) framing are grounded verbatim. 4 LACs
grounded. CH1-V5-001 fold-verification command in the frontmatter is correct.

- **REVISE CH1-2E-01 (`2e:55-58,:219,:296,:352`):** 2E asserts the skinny
  `PrimitiveKernels` names "match ARCH's grammar-neutral signature set
  `BYTE_CLASS_FROM_TABLE_64` … (ARCH `:1284`)". Live `ARCH:1284` is the
  bifurcation/LLVM-compatibility prose (scan-leaf FFI context), NOT a table of
  the macro signatures. The macro-name set (`BYTE_CLASS_FROM_TABLE_64`,
  `BITMAP_PREFIX_XOR_64`, `BULK_EMIT_COMPRESSED`, `EOB_PAD_CLAMP`, …) lives at
  `MASTER-PLAN.md:622` (verified). **Fix:** re-anchor the signature-name set to
  `MASTER-PLAN.md:622`; cite `ARCH:1284` for the scan-leaf-FFI / four-LLVM-shapes
  context only. The substantive claim (NEON under the four LLVM shapes;
  names map 1:1) is sound; the signature-name anchor is mis-pointed → REVISE
  (shared with 2B's reliance on the same name set).

Remaining folds + grounding rows: **ACCEPT.**

### 2F — 9 folds (F1..F9) + 8 grounding rows

All F1–F9 trace to §A–F + sub-fences with verified IDs (SUB17-002..009,
D-1E-SKV17-01..06, COH-014). The "spec already frames the 5 shapes as tape
projections" reading (ARCH:1088) is verbatim-correct; the F7 all-8-carrier
census (json/css_l4/google_sheets/bbnf init sites) and F9 Lock-2 960-site rename
are grounded. 5 LACs grounded.

- **REVISE CH1-2F-01 (`2f:282`):** F5 cites "`LOCKS.md:315-317` — the L1
  classifier's only grammar datum is the `alphabet: &[u8;64]`". The
  alphabet-as-data Lock-14-vehicle text lives at **SK-V17 `SPEC.md:314-317`**
  (verified verbatim), not `LOCKS.md:315-317`. 2F's other citations of this fact
  (`:285` "`LOCKS.md:312-314`") share the same mis-attribution: the
  alphabet/quote-class prose is the SK-V17 SPEC, not LOCKS. **Fix:** re-anchor
  the alphabet-as-data datum to `SPEC.md:314-317` (and `:312` for the
  generated-byte-set ordinals). Wrong file in the citation → REVISE (the claim is
  true and grounded elsewhere; the anchor points at the wrong document).

Remaining 8 folds + grounding rows: **ACCEPT.**

## Summary verdict

- **ACCEPT: 39 / 44 fold proposals/sections.** Every fold traces to a named,
  live-verified T-P1 divergence (A/B/C/D/E/F) with a resolving file:line/SHA;
  every SOTA/ISA claim carries a real, finding-bearing citation; no confabulation
  found in any of the six dossiers.
- **REVISE: 5.** All five are anchor-precision defects (wrong-file or
  mis-pointed-line citations whose underlying claim is true and grounded
  elsewhere), not confabulations — CH1-2B-01 (S-P2 path abbreviation), CH1-2C-01
  (dual-ordinal FactStream phrasing), CH1-2D-01 (ARCH:1203 reuse for a simdjson
  external claim), CH1-2E-01 (signature-name set mis-anchored to ARCH:1284 vs
  MASTER-PLAN:622), CH1-2F-01 (alphabet datum cited to LOCKS vs SPEC:314-317).
- **REJECT: 0.** No fold lacks a divergence antecedent; no cited paper or
  library-source citation failed to resolve; no benchmark number is unsourced.
- **CH1-V5-001:** VERIFIED FOLDED ON DISK at `91b6893b0` (enumerated
  `collapsed_stage}.rs` form present; zero `,collapsed}` residual; no
  `collapsed_tape.rs`). Residual REVISE from T-P1 discharged.

ACCEPT rate 39/44 = 88.6%. This is below the 95% convergence threshold, as
expected for a V1 cycle (PASS-2 §3 expects ≥30% REVISE; the five REVISEs here are
9 of the surface — all anchor-precision, all foldable in V2 by re-pointing the
citation to the verified source). The fold architecture is correct; the
substrate-manifest-not-6th-shape verdict, the eager-OpenFrame-retirement, the
lazy-ValueRef<G> plane, the Lock-16 classifier entry, and the FieldSource fence
are all soundly grounded against live source + the LAC-1E-14 precedent. The
REVISE set is purely citation-hygiene, zero orphan, all V2-foldable.
