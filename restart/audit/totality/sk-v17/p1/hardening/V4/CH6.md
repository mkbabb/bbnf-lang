# CH6 — ANTI-PAPER-CLOSE (V4) — SK-V17 T-P1 EXCAVATION

```yaml
lens: CH6-ANTI-PAPER-CLOSE
pass: T-P1-SKV17-excavation
cycle: V4
generated_at: 2026-05-29T25:40:00Z
reviewer: CH6
master_head: 445925167154de73540e3ea3283d0170371de790
subjects_reviewed:
  - restart/audit/totality/sk-v17/p1/1a-substrate-evidence.md
  - restart/audit/totality/sk-v17/p1/1b-codegen-evidence.md
  - restart/audit/totality/sk-v17/p1/1c-runtime-evidence.md
  - restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md
  - restart/audit/totality/sk-v17/p1/1e-locks-evidence.md
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
live_truth_method: "git rev-parse HEAD = 445925167; grep -c scan_structural over crates/core/src/grammar/generated/{json,css_l4,css_pretty,bbnf,bnf,csv,ebnf,google_sheets,math}.rs (8×1, math=0); grep -n OnceCell/StructuralIndex math.rs (:281,:285 doc-only); sed crates/core/src/runtime/tape/{record,mod}.rs (TapeRec 16B/align4 const-assert; begin_compound(&StructLayout); rule_id&0x1F); grep -rn TapeStructBuilder outside tape/ = 0; grep -rc enum BackendShape crates/ (0) vs skinny/crates/ (2 files); grep -rn fn derive_backend_shape (skinny passes/src/lib.rs:392,401 only); grep -rn StructLayout crates/ | wc -l = 960; grep -c value_from_ref|ValueRef crates/core/src/runtime/{json,css_l4}/value.rs = 0,0; skinny json/value.rs:143 value_from_ref + ValueRef:144; crates/ir/src/registry/struct.rs (FieldSource:84, StructLayout:202, StructRegistry:313, layout:331); crates/simd-scan/src/alphabet.rs (StructuralAlphabet:19, singletons:23, digraph_mask:28, quote_classes:37); skinny dispatch.rs:42 select_classifier; skinny tape/mod.rs (Tape:94, offsets:96, flag_cursors:97, flag_values:98); ARCH:1088,1206; css_l4/builder.rs:64-79 pending fields; deferral-pattern grep over all 8 artefacts (0 hits); Open-Questions verify_action census over 1a-1f."
counts:
  sections_dispositioned: 8
  accept: 8
  revise: 0
  reject: 0
accept_rate: 1.0
```

## Lens charge

CH6 ANTI-PAPER-CLOSE: no "excavated / resolved / wired" claim stands
without a live-evidence citation (cargo asm symbol, bench row, REDRESS
admit, file:line/SHA); no divergence is deferred to "a later inventory"
or "T-P2 will find" in place of present ground truth; every UNKNOWN
carries a verify_action. Ground truth is concrete, resolved to
file:line / SHA.

## V3 → V4 fold posture

V3 CH6 (`hardening/V3/CH6.md`) returned 7 ACCEPT / 1 REVISE / 0 REJECT
(87.5%). The single REVISE was §1: `1d-skinny-lessons.md:91`
SK17L-008's "+`math.rs`" parenthetical, which manufactured a ninth scan
carrier on a doc-comment match — the inverse of the V2 false-negative.
The required fold: strike "+math.rs"; restate the verified eight-name
wired census; name math as the lone field-only NO-scan generated grammar.

**The fold landed and is verified clean.** CH6 V4 re-resolves every
load-bearing citation at master `445925167` and confirms the V3 REVISE is
discharged with no new ground-truth slip introduced. The cycle returns
8/8 ACCEPT.

## V3 §1 REVISE fold — verified discharged

`git rev-parse HEAD` = `445925167154de73540e3ea3283d0170371de790` — matches
all eight artefact frontmatters.

- **The "+math.rs" parenthetical is gone.** `1d-skinny-lessons.md:94`
  (SK17L-008 impl column) now reads: core `scan_structural` is WIRED into
  "EXACTLY 8 generated grammars: json/ebnf/bnf/csv/css_l4/css_pretty/
  google_sheets/bbnf each carry exactly one `scan_structural` call …",
  and explicitly: "`math.rs` is the lone generated grammar carrying the
  `OnceCell<StructuralIndex>` index FIELD but NO `scan_structural` call
  (`grep -c scan_structural math.rs` = 0; the only `OnceCell`/
  `StructuralIndex` hits are doc-comments at `math.rs:281`,`:285`) — so
  the wired census is EIGHT, not nine." No "+math.rs" string survives.
- **Live re-verification of the corrected census.** `grep -c
  scan_structural` over the nine generated grammars: json/css_l4/
  css_pretty/bbnf/bnf/csv/ebnf/google_sheets = **1 each; math = 0**.
  `grep -n OnceCell|StructuralIndex math.rs` → exactly two hits, both
  doc-comments (`math.rs:281,285`). The SK17L-008 claim resolves at
  file:line: the eight-name set is the scan-carrier set; math holds the
  field-only-no-call shape exactly as stated.
- **The fold is recorded in the frontmatter** (`1d-skinny-lessons.md:33`
  `prior_cycle_dispositions_folded.revised`): "V3-SK17L-008-plus-math.rs-
  false-ninth-carrier (FOLD §1/§3-B/1d:91: grep -c scan_structural
  math.rs = 0; math.rs carries only the OnceCell index FIELD +
  doc-comments at :281,:285; struck '+math.rs', restated verified
  EIGHT-name wired census …)". The disposition source is cited; the fold
  is not silent.
- **The cross-tree census rows agree.** SK17L-008's Do-Not-Redrive row
  (`:139`), the structural-scan isomorphism row (`:108`), and the
  live_truth_method frontmatter (`:16`: "{json,ebnf,bnf,csv,css_l4,
  css_pretty,google_sheets,bbnf}:1, math.rs:0, mod.rs:0 (EIGHT wired;
  math is field-only at :281/:285 doc-comments)") all carry the corrected
  eight-name census. The V3 outlier is purged tree-wide.

## Verification posture — every load-bearing citation re-resolved at master `445925167`

- **Tape shape (core AoS).** `TapeRec` const-asserted 16-byte/align-4
  (`record.rs:120-121`), `#[repr(C, align(4))]` (`record.rs:102`),
  grammar-agnostic `begin_compound(&mut self, layout: &StructLayout)`
  reading `(layout.rule_id & 0x1F) as u8` (`mod.rs:185-186`) — **resolve.**
- **Tape shape (skinny SoA).** `Tape<'input>` with `offsets: Vec<u32>`
  (`:96`), `flag_cursors: Vec<u32>` (`:97`), `flag_values: Vec<u8>`
  (`:98`) — the SPARSE position-keyed side-vectors (NOT a dense
  class-column), exactly as the 1f-coherence U-COH17-002 divergence row
  distinguishes — **resolve.**
- **Value-API absence (core) / presence (skinny).** `value_from_ref|
  ValueRef` grep-**zero** in both `crates/core/src/runtime/json/value.rs`
  and `css_l4/value.rs`; skinny `value_from_ref<'doc,'input>` at
  `skinny/.../grammars/json/value.rs:143` taking `ValueRef<'doc,'input>`
  (`:144`) — **resolve** (the V3 CH1 path-defect fix at :143 holds).
- **BackendShape / selector absence in core.** `enum BackendShape` =
  **0** in `crates/`; present in skinny (`bbnf-bench/.../lock14_baseline.rs`,
  `ir/src/lib.rs`); `fn derive_backend_shape` only at
  `skinny/crates/passes/src/lib.rs:392,401` — **resolve.**
- **StructRegistry / FieldSource fence.** `FieldSource` enum
  (`struct.rs:84`), `StructLayout` (`:202`), `StructRegistry` (`:313`),
  `layout(rule_id) -> Option<&StructLayout>` (`:331`); `grep -rn
  StructLayout crates/` = **exactly 960** — the 960-site re-pricing across
  1A/1C/1D/1E/1F is exact, not recall.
- **Live eager path / tape UNWIRED.** `TapeStructBuilder` grep-**zero**
  outside `runtime/tape/` — the core tape substrate is dead code,
  UNWIRED, consistently named so. The CSS builder pending shape:
  `pending_value: Option<CssTypedValue>` (`builder.rs:71`) + six
  `pending_*` Vecs `pending_rules/decls/selectors/values/blocks/components`
  (`builder.rs:74-79`) = SEVEN pending_ fields, **none `Vec<Vec>`** —
  AP17-003 / SK17L-003 count is exact.
- **Alphabet / classifier.** `StructuralAlphabet{singletons (:23),
  digraph_mask (:28), …, quote_classes (:37)}` (`alphabet.rs:19`); skinny
  `select_classifier(alphabet: &'static [u8; 64])` (`dispatch.rs:42`) —
  **resolve.**
- **Spec surfaces.** ARCH:1088 (Lock-1 union "structural projection IS
  the tape (no second sidecar)" + five-shape descriptions), ARCH:1206
  (CollapsedStage **NOT-ADMITTED** x86-only; "aarch64 candidate is
  UNKNOWN-2D-05 … marker-string lowerer at collapsed_stage.rs:15-17") —
  **resolve verbatim.**

A deferral-pattern grep (`will find | later inventory | core fold later |
defer to later`) over all eight artefacts returns **zero** hits outside
verify_action/receiver columns. Every Open Questions table carries a
`verify_action` column with one populated cell per declared UNKNOWN row
(1a: U-SUB17-001/002; 1b: U-BSHAPE17-001/002; 1c: U-RT17-001/002/003;
1d: U-SK17L-001/002/003; 1e: 1E-SKV17-U1/U2; 1f-coherence:
U-COH17-001/002/003 — all populated). No ground truth is parked for a
later inventory.

---

## Dispositions

### §1 — 1d-skinny-lessons SK17L-001..010 + Do-Not-Redrive ledger + UNKNOWNs (V3 §1 REVISE fold) → **ACCEPT**

The V3 REVISE is fully discharged (verified above). SK17L-008 now states
the verified eight-name scan census and names `math.rs` as the lone
field-only NO-scan generated grammar with the doc-comment anchors
(`math.rs:281,285`) resolving live; the "+math.rs" false ninth carrier is
struck; the fold is recorded in the frontmatter with its disposition
source. The remaining digest is sound: SK17L-001/002/004 resolve at the
skinny + core file:lines; the JSON-Empirical vs Grammar-Neutral split is
explicit (CH2 firewall); the Do-Not-Redrive Ledger rows (L-SK17-01..07)
each carry a SPEC/alpha anchor + fold fence; L-SK17-02 (StructRegistry
no-per-leaf-lookup fence) and L-SK17-02b (FieldSource compile-time walk)
are present-tense at `struct.rs:313,331` + `tape/mod.rs:185`. SK17L-009
honestly separates by-construction generality from by-exercise proof
(JSON tape witness only). The three UNKNOWNs carry verify_actions. ACCEPT.

---

### §2 — 1a-substrate SUB17-001..010 + Substrate-Union Firewall + UNKNOWNs → **ACCEPT**

Every SUB17 row resolves at the cited path:line (`record.rs:102,120`;
skinny `tape/mod.rs:94,96-98`; `struct.rs:202,313,331`; `alphabet.rs:19-37`;
ARCH:1206; LOCKS:75,160). The verdicts are anti-paper-close:
SUB17-002/004/005/006 are labelled unimplemented / spec-claims-unimplemented
with the divergence at present file:line; SUB17-003 confirms the tape
UNWIRED (`TapeStructBuilder` grep-zero) with no claimed fold. The
Substrate-Union Firewall states the per-leaf-lookup fence at
`struct.rs:331` + `tape/mod.rs:185` as present ground truth. The two
UNKNOWNs (U-SUB17-001 tape-encoding convergence, U-SUB17-002 OnceCell
substrate_target) carry concrete verify_actions naming surfaces to
re-read; the "T-P2" language sits only in the verify_action/receiver
columns — disposing forward without substituting for present ground truth.
ACCEPT.

---

### §3 — 1b-codegen BSHAPE17-001..008 + do-not-redrive ledger + UNKNOWNs → **ACCEPT**

The headline codegen facts resolve hard: `enum BackendShape` = 0 in
`crates/`, present in skinny (BSHAPE17-001); `derive_backend_shape`
skinny-only at `passes/src/lib.rs:392,401` (BSHAPE17-002); single
`EmitStrategy::StructDirect` (BSHAPE17-003); the scaffold lowerers vs
sink_only (BSHAPE17-004, wc-l method stated). BSHAPE17-006 marks the
StructRegistry pre-block "implemented (pre-block honoured)" with the live
counter-evidence (`begin_compound` takes a pre-resolved `&StructLayout`,
no per-leaf lookup) — an honest present-tense claim that does NOT say the
fold is done, only that the current shape honours the fence the fold must
preserve. BSHAPE17-005 (CollapsedStage) is labelled `unknown` and routed
to UNKNOWN-2D-05 at ARCH:1206 — a recorded open unknown, not a fresh gap.
The two UNKNOWNs carry verify_actions. No "later inventory" deferral.
ACCEPT.

---

### §4 — 1c-runtime RT17-001..007 + per-grammar census + UNKNOWNs → **ACCEPT**

RT17-003 is the strongest anti-paper-close row: tape UNWIRED (grep-zero)
AND scan wired across all 8, both live-cited, labelled
"spec-claims-implemented (UNWIRED confirmed as stated)" — no claimed fold.
RT17-005 enumerates the OnceCell sidecar across all 8 scan-wired grammars.
RT17-002/006 carry the value-API and per-leaf-fence divergences at present
file:line. RT17-004 confirms Lock 14 honoured. The three UNKNOWNs
(U-RT17-001 substrate_target, U-RT17-002 one-substrate closure, U-RT17-003
FieldSource walk timing) each carry a verify_action and explicitly scope
to "all 8, not json alone (math.rs holds the field but does not scan)" —
the V3 census-correction is carried forward correctly. ACCEPT.

---

### §5 — 1e-locks Verified Invariants + L01/L02/L10/L14/L16 + COH-014 catch + LACs + UNKNOWNs → **ACCEPT**

1E re-resolves the 16-lock count, the 5-shape canon, and the all-8 scan
census as a Verified Invariants row. The per-lock verdicts are honest:
L01 "partial / core-tape-UNWIRED, scan-WIRED"; L02 "drifted (name not
migrated)" with the 960-site evidence; L10 "canon holds, CollapsedStage
aarch64-refused" → UNKNOWN-2D-05; L16 "aarch64 NEON proven, x86
diagnostic." The COH-014 continuity catch is carried. The five
LOCKS-AMENDMENTS-CANDIDATEs each carry supporting path:line evidence +
loc/risk/wave_hint, with the explicit "Candidates only; disposition is
T-P3 3C" guard — no premature amendment, no paper-close. The two UNKNOWNs
(1E-SKV17-U1 tape convergence, 1E-SKV17-U2 OnceCell substrate_target)
carry verify_actions. ACCEPT.

---

### §6 — 1f-coherence COH17-001..008 + Gaps row + UNKNOWNs → **ACCEPT**

The all-8 scan census holds (Gaps row + Cross-Tree map). COH17-001..008
resolve at the cited file:lines; COH17-007 (FactStream) is the lone
`unknown` carrying U-COH17-001 with a verify_action. U-COH17-002 is the
load-bearing CATALOGUED DIVERGENCE row: it states the Lock-1
exactly-one-encoding closure obligation as present-tense ("a dual encoding
is NOT a permissible end-state") and rigorously distinguishes the
admissible offset-tape-SoA (skinny `Tape.offsets`, ARCH:1088) from the
dead AV.04 class-column-SoA (LOCKS:75 "columnar SoA is dead") — the
distinction resolves at `skinny/.../tape/mod.rs:96-98` (sparse side-vectors)
and is correct. The "adopt proven SoA" candidate is shown NOT
self-contradictory against LOCKS:75. No claim of a fold that has not
happened. ACCEPT.

---

### §7 — 1f-anti-pattern AP17-001..005 + CH5 verdict → **ACCEPT**

AP17-002 enumerates all 8 scan carriers with per-grammar line anchors.
AP17-003 states the pending count as "six `pending_*` Vecs (`:74-79`) plus
one `pending_value: Option` (`:71`) = SEVEN pending_ fields, NONE of them
`Vec<Vec>`" — verified exact against `builder.rs:64-79` live, and the
parenthetical "(Count corrected per CH5-S4 / CH1-V2-005: six Vec + one
Option, not 'nine Vecs')" records the fold. The god-module verdict
(817-LOC CSS builder, eager `OpenFrame`+`pending_*` machine) is named as
the SK-V18 fold-deletion target citing `tape/mod.rs:16-20` ("the single
generic StructBuilder impl"), not as a permanent surface. AP17-005
(StructRegistry hot-path indirection) is the present-tense fence row. The
CH5 firewall verdict scopes honestly to "within crates/core in this scan"
and names the REDRESS-53 trip conditions; AP17-001/004 hedge un-proven
absences with verify_actions. ACCEPT.

---

### §8 — 1f-past-corpora ledger + COH-014 flag + direction-monotonicity → **ACCEPT**

The Do-Not-Carry-Undercount flag records the all-8 live census with
per-grammar line anchors and states the carry obligation for T-P2; the
Second-Substrate Carrier Enumeration lists all 8 carrier field lines. The
direction-monotonicity note (skinny → totality, never back) is correct and
load-bearing. No divergence deferred; all present-tense. ACCEPT.

---

## Cross-cutting CH6 observations

1. **The V3 REVISE is fully closed.** The "+math.rs" false ninth scan
   carrier (V3 §1) is struck from `1d-skinny-lessons.md`; SK17L-008 now
   carries the verified eight-name census and names math as the lone
   field-only-no-scan grammar (`math.rs` grep-zero `scan_structural`,
   doc-comments at :281,:285 verified live); the fold is recorded in the
   frontmatter with its disposition source. No new slip introduced.

2. **No tape claim is paper-closed as "wired."** Every artefact states the
   crates/core tape is UNWIRED dead code (`TapeStructBuilder` grep-zero
   re-confirmed). The SK-V18 fold is consistently named FUTURE with the
   proven skinny shape as source; the monotonic skinny→totality direction
   holds.

3. **The one load-bearing dual-substrate divergence is catalogued
   present-tense, not deferred.** U-COH17-002 (and its U-SUB17-001 /
   U-RT17-002 / U-SK17L-002 / 1E-SKV17-U1 siblings) states the Lock-1
   exactly-one-encoding closure as a present obligation with the
   offset-tape-vs-class-column distinction resolved at file:line — the
   forward-disposition ("T-P2 names the convergence target") sits only in
   the verify_action column.

4. **Every UNKNOWN carries a verify_action; zero deferral-pattern hits.**
   The "T-P2/T-P3" language is confined to reconciliation/verify_action
   columns across all eight artefacts; no divergence row substitutes a
   future inventory for present file:line ground truth.

## Disposition summary

| § | Subject | Disposition |
|---|---|---|
| 1 | 1d SK17L-001..010 + ledger (V3 §1 "+math.rs" REVISE fold) | **ACCEPT** — fold verified: math.rs grep-0, "+math.rs" struck, 8-name census restated |
| 2 | 1a substrate SUB17-001..010 + firewall + UNKNOWNs | ACCEPT |
| 3 | 1b codegen BSHAPE17-001..008 + ledger + UNKNOWNs | ACCEPT |
| 4 | 1c runtime RT17-001..007 + census + UNKNOWNs | ACCEPT |
| 5 | 1e locks invariants + L01/02/10/14/16 + COH-014 + LACs | ACCEPT |
| 6 | 1f-coherence COH17-001..008 + Gaps + U-COH17-002 divergence | ACCEPT |
| 7 | 1f-anti-pattern AP17-001..005 + CH5 verdict | ACCEPT |
| 8 | 1f-past-corpora ledger + COH-014 + monotonicity | ACCEPT |

**Counts:** 8 dispositioned · 8 ACCEPT · 0 REVISE · 0 REJECT · ACCEPT-rate 100%.

**Verdict:** the SK-V17 T-P1 V4 inventory set is well-grounded on its
substrate / value-API / BackendShape / NEON spine — near-zero recalled
LOC, every load-bearing citation re-resolved at master `445925167`. The
sole V3 REVISE (1D SK17L-008 "+math.rs" false ninth scan carrier) is
folded clean: `grep -c scan_structural math.rs` = 0, the parenthetical is
struck, the verified eight-name census is restated tree-wide, and the fold
is recorded in the frontmatter with its disposition source — with no new
ground-truth slip introduced. The core tape remains honestly UNWIRED dead
code across all artefacts; the SK-V18 fold is named FUTURE; the Lock-1
exactly-one-encoding closure is catalogued present-tense (not deferred);
zero deferral-pattern hits; every UNKNOWN carries a verify_action. CH6
returns 100% ACCEPT — a clean convergent cycle on this lens, with zero
orphan REVISE.
