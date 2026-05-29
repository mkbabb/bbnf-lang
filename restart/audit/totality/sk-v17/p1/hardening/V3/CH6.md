# CH6 — ANTI-PAPER-CLOSE (V3) — SK-V17 T-P1 EXCAVATION

```yaml
lens: CH6-ANTI-PAPER-CLOSE
pass: T-P1-SKV17-excavation
cycle: V3
generated_at: 2026-05-29T24:30:00Z
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
live_truth_method: "git rev-parse HEAD; grep -c scan_structural over crates/core/src/grammar/generated/{json,css_l4,css_pretty,bbnf,bnf,csv,ebnf,google_sheets,math}.rs; sed/grep over crates/core/src/runtime/tape/{mod,record}.rs, crates/core/src/runtime/{json,css_l4}/{builder,value,parse_with}.rs, crates/ir/src/registry/struct.rs, crates/simd-scan/src/{lib,alphabet}.rs, skinny/crates/runtime/src/tape/mod.rs, skinny/crates/runtime/src/grammars/json/value.rs, skinny/crates/bbnf-simd/src/dispatch.rs, skinny/crates/passes/src/lib.rs; grep -rn StructLayout/enum BackendShape/derive_backend_shape over crates/ + skinny/crates/; sed over restart/{ARCHITECTURE.md,locks/LOCKS.md}; wc -l on the builders; grep over all 1X inventories for deferral patterns + verify_action coverage."
counts:
  sections_dispositioned: 8
  accept: 7
  revise: 1
  reject: 0
accept_rate: 0.875
```

## Lens charge

CH6 ANTI-PAPER-CLOSE: no "excavated / resolved / wired" claim stands
without a live-evidence citation (cargo asm symbol, bench row, REDRESS
admit, file:line/SHA); no divergence is deferred to "a later inventory"
or "T-P2 will find" in place of present ground truth; every UNKNOWN
carries a verify_action. Ground truth is concrete, resolved to
file:line / SHA.

## V2 → V3 fold posture

V2 CH6 (`hardening/V2/CH6.md`) reviewed ONLY the three 1F artefacts
(1a–1e were absent and flagged as a coverage gap). V2 raised one REJECT
(§1, CSS-scan-not-wired false-negative grep) and two REVISEs (§5
4-grammar undercount; §7 COH-014 continuity miss). V3 authored the five
missing inventories (1a–1e) AND folded all three V2 dispositions. CH6 V3
re-resolves the load-bearing citations across all eight artefacts and
confirms the V2 dispositions closed; it then adds one new ground-truth
slip the V3 fold introduced.

## Verification posture — every load-bearing citation re-resolved at master `445925167`

`git rev-parse HEAD` = `445925167154de73540e3ea3283d0170371de790` — matches
the frontmatter of all eight inventories. Re-resolved live:

- **Scan census (the V2 §1 REJECT subject).** `grep -c scan_structural`
  over the nine generated grammars: json/css_l4/css_pretty/bbnf/bnf/csv/
  ebnf/google_sheets = **1 each; math = 0**. Line anchors resolve:
  `json.rs:732`, `css_l4.rs:15982`, `google_sheets.rs:3559`,
  `bbnf.rs:4843`. The V2 false-negative ("css_l4 grep absent / json,ebnf,
  bnf,csv only") is **PURGED** across all V3 artefacts — 1A SUB17-003/008,
  1C RT17-003/005, 1D SK17L-008, 1E (Verified Invariants row),
  1f-coherence COH17-003 + Gaps row, 1f-anti-pattern AP17-002,
  1f-past-corpora DO-NOT-CARRY-UNDERCOUNT all carry the corrected all-8
  census. **REJECT closed.**
- **Tape shape.** core AoS `TapeRec` const-asserted 16-byte/align-4
  (`record.rs:120-121`), AoS-first doc (`mod.rs:6-9`), grammar-agnostic
  `begin_compound(&StructLayout)` reading `layout.rule_id & 0x1F`
  (`mod.rs:185-186`) — **resolve.** skinny SoA `Tape<'input>` six members
  (`skinny/.../tape/mod.rs:94-100`), `ValueRef<'doc,'input,K,G>` (`:175`),
  `value_from_ref` (`skinny/.../grammars/json/value.rs:143`) — **resolve.**
- **Value-API absence.** `value_from_ref|ValueRef` grep-**zero** in
  `crates/core/src/runtime/json/value.rs`; `CssTypedValue` enum at
  `css_l4/value.rs:414` — **resolve.**
- **BackendShape / selector absence.** `enum BackendShape` = **0** in
  `crates/`, **4** in `skinny/crates/`; `derive_backend_shape` only at
  `skinny/crates/passes/src/lib.rs:392` — **resolve** (1B BSHAPE17-001/002).
- **StructLayout / FieldSource / StructRegistry.** `FieldSource` enum
  (`struct.rs:84`), `StructLayout` (`:202`), `StructRegistry` (`:313`),
  `BTreeMap` layouts (`:314`), `layout(rule_id)` (`:331`); `grep -rn
  StructLayout crates/` = **exactly 960** — **resolve** (the 1A/1C/1D/1E/1F
  "960-site" re-pricing is exact, not a recall).
- **Live eager path.** `JsonStructBuilder::new()` (`json/parse_with.rs:34`),
  `CssStructBuilder::new()` (`css_l4/parse_with.rs:34`); `TapeStructBuilder`
  grep-**zero** outside `runtime/tape/` — **resolve** (UNWIRED confirmed).
- **Builder LOC + pending count.** CSS builder = **817** LOC, JSON = **231**;
  pending fields = six `Vec` (`pending_rules/decls/selectors/values/blocks/
  components`) + one `pending_value: Option` (`builder.rs:66-79`), **none
  Vec<Vec>** — the AP17-003 correction is exact; the stale `mod.rs:6-9`
  generated-doc "seven pending_* Vec<Vec>" is correctly flagged as the
  source the builder out-grew.
- **Alphabet / classifier.** `StructuralAlphabet{singletons, digraph_mask,
  digraph_pairs, quote_classes}` (`alphabet.rs:19-37`), quote_classes doc
  JSON/CSS-motivated; `scan_structural(input,&StructuralAlphabet)->
  StructuralIndex` (`lib.rs:80`); skinny `select_classifier(&'static
  [u8;64])` (`dispatch.rs:42`) — **resolve** (the "config-breadth not
  proof-breadth" verdict is honest).
- **Spec surfaces.** ARCH:1088 (Lock-1 union + 5-shape descriptions +
  "CollapsedStage fuses … AVX-512-class"), ARCH:1206 (NOT-ADMITTED;
  "aarch64 candidate is UNKNOWN-2D-05 … marker-string lowerer at
  collapsed_stage.rs:15-17"), LOCKS:75 (Lock 1 "parallel substrates are
  dead"), LOCKS:107-108 (5-shape domain), LOCKS:160 (Lock 2 retires
  StructLayout→Layout) — **resolve verbatim.**

The V3 inventories are, by excavation standards, near-zero recalled LOC.
A deferral-pattern grep (`will find | later inventory | core fold later |
defer`) over all eight artefacts returns **zero hits** — the V2 §1
"core fold later" deferral language is gone; no divergence is parked for
a later inventory. Every Open Questions table carries one verify_action
column per UNKNOWN row (1a:2, 1b:2, 1c:3, 1d:3, 1e:2 UNKNOWNs, all with
populated verify_action cells). CH6 therefore disputes only the single
claim that fails the live check.

---

## Dispositions

### §1 — 1d-skinny-lessons SK17L-008: "+math.rs" scan-carrier addendum → **REVISE**

**Location.** `1d-skinny-lessons.md:91` (SK17L-008 impl column):
> "crates/core `scan_structural(&StructuralAlphabet)` is WIRED into ALL 8
> generated grammars: json/ebnf/bnf/csv/css_l4/css_pretty/google_sheets/bbnf
> each carry exactly one `scan_structural` call (`…/generated/*.rs`,
> grep-verified 1 per file; **+`math.rs`**)…"

**Live evidence contradicts the addendum.** `grep -c scan_structural
crates/core/src/grammar/generated/math.rs` = **0**. The only `math.rs`
hits are two doc-comment mentions at `math.rs:281,285`
("`OnceCell<StructuralIndex>` consumed by CTNS-style …" / "regression that
motivates the OnceCell discipline") — prose, not a live `scan_structural`
call or `OnceCell<StructuralIndex>` field. `math.rs` is NOT a scan carrier.

This is a genuine CH6 ground-truth slip — and notably the **inverse** of
the V2 §1 REJECT it was correcting. V2 catalogued a false-**negative**
(claimed css_l4 unwired when it is wired); SK17L-008 over-corrected into a
false-**positive** addendum (claimed math wired when it is not). The body
sentence asserts "ALL 8" and lists the correct eight names, so the headline
census is true; the trailing "+`math.rs`" parenthetical manufactures a
ninth carrier on a doc-comment match. Under CH6 a scan-wiring claim must
resolve at file:line; "+math.rs" does not. Because the eight-name set is
correct and only the parenthetical is wrong, this is REVISE, not REJECT.

**Cross-check — the siblings got it right.** 1A SUB17-008/census,
1C RT17-003 (lists `math` as a runtime *dir* but states scan is wired into
"ALL EIGHT", excluding math), 1E (Verified Invariants: "json/ebnf/bnf/csv/
css_l4/css_pretty/google_sheets/bbnf"), and the three 1F artefacts all
enumerate exactly eight and **none** append math. SK17L-008 is the lone
outlier; the slip is local to 1D.

**Concrete fix.** Delete the "+`math.rs`" parenthetical at
`1d-skinny-lessons.md:91`. State precisely: the scan-wired set is the
**eight** generated grammars (json/ebnf/bnf/csv/css_l4/css_pretty/
google_sheets/bbnf), each `grep -c scan_structural` = 1; `math.rs` carries
**zero** `scan_structural` and only a doc-comment reference to the
`OnceCell<StructuralIndex>` discipline at `math.rs:281,285` (not a live
carrier). This aligns SK17L-008 with the all-8 census every sibling
records and removes the false ninth carrier.

---

### §2 — 1a-substrate SUB17-001..009 + firewall + UNKNOWNs → **ACCEPT**

Every SUB17 row resolves at the cited path:line (re-verified above:
`record.rs:103,120`; skinny `tape/mod.rs:94,175`; `struct.rs:202,313,331`;
`alphabet.rs:19-37`; ARCH:1206; LOCKS:75,160). The verdicts are honest and
anti-paper-close: SUB17-002/004/005/006 are labelled `unimplemented`/
spec-claims-unimplemented with the divergence stated at present file:line;
SUB17-003 confirms the tape UNWIRED (`TapeStructBuilder` grep-zero) without
claiming any fold has happened. The new SUB17-009 (StructRegistry per-leaf
fence) and SUB17-010 (borrow shape) are catalogued at present file:line,
not deferred. The Substrate-Union Firewall table states the
per-leaf-lookup fence at `struct.rs:331` + `tape/mod.rs:185` as present
ground truth. The two UNKNOWNs (U-SUB17-001 tape-encoding convergence,
U-SUB17-002 OnceCell substrate_target) each carry a concrete verify_action
naming the surfaces to re-read; the "T-P2" language sits only in the
verify_action/receiver columns — it disposes forward without substituting
for present ground truth (correct excavation posture, PASS-1 §0). No CH6
violation.

---

### §3 — 1b-codegen BSHAPE17-001..008 + do-not-redrive ledger + UNKNOWNs → **ACCEPT**

The headline codegen facts resolve hard: `enum BackendShape` = 0 in
`crates/` / 4 in skinny (BSHAPE17-001); `derive_backend_shape` skinny-only
at `passes/src/lib.rs:392` (BSHAPE17-002); single `EmitStrategy::StructDirect`
(BSHAPE17-003); the four 17-LOC scaffold lowerers vs 270-LOC sink_only
(BSHAPE17-004, the wc-l method is stated). BSHAPE17-006 marks the
StructRegistry pre-block "implemented (pre-block honoured)" with the
live counter-evidence (`begin_compound` takes a pre-resolved `&StructLayout`,
no per-leaf lookup) — this is an honest present-tense claim, not a
paper-close: it does NOT say the fold is done; it says the current shape
already honours the fence and the fold must preserve it. BSHAPE17-005
(CollapsedStage) is labelled `unknown` and routed to the spec-named
UNKNOWN-2D-05 at ARCH:1206 — a recorded open unknown, not a fresh gap.
The two UNKNOWNs carry verify_actions. No "later inventory" deferral. ACCEPT.

---

### §4 — 1c-runtime RT17-001..007 + per-grammar census + UNKNOWNs → **ACCEPT**

RT17-003 is the strongest anti-paper-close row: it states the tape UNWIRED
(grep-zero) AND the scan wired across all 8, with both live citations, and
labels it "spec-claims-implemented (UNWIRED confirmed as stated)" — it does
not claim a fold. RT17-005 enumerates the OnceCell sidecar in all 8 (the
V2 §5 undercount fix, here verified: OnceCell/structural_index count 6 in
each of seven grammars, 7 in google_sheets — all live fields, math
excluded). RT17-002/006 carry the value-API and per-leaf-fence divergences
at present file:line. RT17-004 confirms Lock 14 honoured with the
`substrate.rs:43,55` data-binding evidence. The three UNKNOWNs each carry a
verify_action scoped to "all 8, not json alone." Note 1C lists `math` as
the ninth runtime dir in its census prose but correctly states scan is
wired into "ALL EIGHT" — it does NOT append math to the scan set (unlike
1D §1). ACCEPT.

---

### §5 — 1d-skinny-lessons SK17L-001..010 (excl. the §1 "+math.rs" slip) + ledger → **ACCEPT**

Setting aside the SK17L-008 "+math.rs" addendum (REVISE §1), the 1D digest
is well-grounded. SK17L-001/002/004 resolve at the skinny + core file:lines
re-verified above; the JSON-Empirical vs Grammar-Neutral split (CH2
firewall) is explicit and load-bearing; the Do-Not-Redrive Ledger rows
(L-SK17-01..07) each carry the SPEC/alphaC anchor and a fold fence. The
L-SK17-02 StructRegistry fence and L-SK17-02b FieldSource-compile-time-walk
seam are present-tense, citing `struct.rs:313,331` + `tape/mod.rs:185` +
SPEC:794-795. SK17L-009 honestly separates by-construction generality from
by-exercise proof (JSON tape witness only). The three UNKNOWNs carry
verify_actions. The only slip is the §1 census-addendum; the rest is sound.
ACCEPT (conditional on §1 fold).

---

### §6 — 1e-locks Verified Invariants + L01/L02/L10/L14/L16 + COH-014 catch + LACs → **ACCEPT**

1E re-resolves the 16-lock count, the 5-shape canon (LOCKS:107-108), and
the all-8 scan census as a Verified Invariants row (the V2 false-negative
correction, here re-confirmed). The per-lock verdicts are honest:
L01 "partial / core-tape-UNWIRED, scan-WIRED"; L02 "drifted (name not
migrated)" with the 960-site evidence; L10 "canon holds, CollapsedStage
aarch64-refused" → UNKNOWN-2D-05; L16 "aarch64 NEON proven, x86 diagnostic."
The "Prior-Totality Contradiction Caught (COH-014)" section explicitly
discharges the V2 §7 REVISE — it surfaces that prior COH-014 enumerated
JSON+Sheets (wider than the V2 "json/ebnf/bnf/csv only" undercount) and
records the all-8 correction so V3+ never re-derives it. The five
LOCKS-AMENDMENTS-CANDIDATEs each carry supporting path:line evidence and a
loc/risk/wave_hint, and the section explicitly states "Candidates only;
disposition is T-P3 3C" — no premature amendment, no paper-close. The two
UNKNOWNs carry verify_actions. ACCEPT.

---

### §7 — 1f-coherence COH17-001..008 + Gaps row + UNKNOWNs (V2 §1 REJECT fold) → **ACCEPT**

The V2 §1 REJECT is fully discharged. The Gaps-row-4
(`1f-coherence-scan.md:118`) now reads "Core CSS structural scan IS wired —
the missing primitive is the TAPE CONSUMER, not the scan", citing
`css_l4.rs:15936,15951,15976-15982` + `grep -c scan_structural css_l4.rs =
1 (same as json/ebnf/bnf/csv)` + "All 8 generated grammars are scan-wired
(not json/ebnf/bnf/csv only)". The Cross-Tree map row (`:95`) now reads
"WIRED into ALL 8 generated grammars (json.rs:732, css_l4.rs:15982, …)".
The deferral language ("SK-V17 W3 wires CSS scan in skinny; core fold
later") is corrected to "scan already grammar-general in core (all 8); the
missing primitive is the tape consumer." COH17-001..008 resolve at the
cited file:lines; COH17-007 (FactStream) is the lone `unknown` and carries
U-COH17-001 with a verify_action; U-COH17-002 is upgraded to a catalogued
divergence (the V2 §2 "T-P2 must reconcile" framing now reads as a
present-tense Lock-1 closure obligation). No claim of a fold that has not
happened. ACCEPT.

---

### §8 — 1f-anti-pattern AP17-001..005 + CH5 verdict; 1f-past-corpora ledger + COH-014 flag (V2 §5/§7 fold) → **ACCEPT**

The V2 §5 undercount is discharged: AP17-002 now enumerates all 8 carriers
(`json.rs:686, css_l4.rs:15936, ebnf.rs:1335, bnf.rs:802, csv.rs:520,
css_pretty.rs:1859, google_sheets.rs:3513, bbnf.rs:4797`) and U-AP17-001's
verify_action scopes to "all 8, NOT json alone." AP17-003 corrects the
pending count to "six `pending_*` Vecs + one `pending_value: Option` =
SEVEN pending_ fields, NONE Vec<Vec>" — verified exact against
`builder.rs:66-79`. AP17-005 (StructRegistry hot-path indirection) is the
present-tense fence row with the `struct.rs:313` + `tape/mod.rs:185`
citations. The CH5 firewall verdict scopes honestly to "within crates/core
in this scan" and names the exact REDRESS-53 trip conditions; AP17-001/004
hedge un-proven absences with verify_actions rather than paper-closing them.

The V2 §7 is discharged in 1f-past-corpora: the "DO-NOT-CARRY-UNDERCOUNT
flag" (`1f-past-corpora.md:85-95`) surfaces that prior COH-014 enumerated
JSON+Sheets (wider than the V2 undercount), records the all-8 live census
with per-grammar line anchors, and states "T-P2 must carry the all-8
census, not the V2 undercount." The Second-Substrate Carrier Enumeration
(`:97-105`) lists all 8 carrier field lines. The direction-monotonicity
note is correct and load-bearing. No divergence deferred; all present-tense.
ACCEPT.

---

## Cross-cutting CH6 observations

1. **The V2 hard defect is fully closed.** The CSS-scan false-negative
   (V2 §1 REJECT) is purged from all eight V3 artefacts and replaced with
   the verified all-8 census; the Lock-1 retained-projection firewall is
   correctly re-scoped from 4 grammars to its true 8. The deferral
   language is gone (zero deferral-pattern hits). This is the central
   CH6-positive V3 finding.

2. **No tape claim is paper-closed as "wired."** Every artefact states the
   crates/core tape is UNWIRED dead code (`TapeStructBuilder` grep-zero
   re-confirmed). The SK-V18 fold is consistently named FUTURE with the
   proven skinny shape as source; the monotonic skinny→totality direction
   holds (1f-past-corpora, 1d L-SK17-07).

3. **The one V3 slip is a false-positive census addendum, not an
   over-claim of a fold.** SK17L-008's "+math.rs" (§1 REVISE) is the
   inverse of the V2 false-negative: it manufactures a ninth scan carrier
   on a doc-comment match. Under CH6 a scan-wiring claim must resolve at
   file:line; math.rs has zero `scan_structural`. Local to 1D; every
   sibling enumerates the correct eight. Low blast radius.

4. **All 1a–1e inventories now exist** (the V2 coverage gap is closed).
   CH6 was able to disposition all eight artefacts this cycle.

5. **Every UNKNOWN carries a verify_action; no ground truth deferred.**
   The "T-P2/T-P3" language sits exclusively in reconciliation/receiver/
   verify_action columns across all artefacts — it disposes divergences
   forward (excavation proposes; synthesis disposes) without substituting
   for present file:line ground truth in any divergence row.

## Disposition summary

| § | Subject | Disposition |
|---|---|---|
| 1 | 1d SK17L-008 "+math.rs" scan-carrier addendum | **REVISE** — math.rs has 0 scan_structural; doc-comment-only; false 9th carrier |
| 2 | 1a substrate SUB17-001..009 + firewall + UNKNOWNs | ACCEPT |
| 3 | 1b codegen BSHAPE17-001..008 + ledger + UNKNOWNs | ACCEPT |
| 4 | 1c runtime RT17-001..007 + census + UNKNOWNs | ACCEPT |
| 5 | 1d SK17L-001..010 (excl. §1 slip) + ledger | ACCEPT (conditional on §1 fold) |
| 6 | 1e locks invariants + L01/02/10/14/16 + COH-014 + LACs | ACCEPT |
| 7 | 1f-coherence COH17-001..008 + Gaps row (V2 §1 fold) | ACCEPT |
| 8 | 1f-anti-pattern + 1f-past-corpora (V2 §5/§7 fold) | ACCEPT |

**Counts:** 8 dispositioned · 7 ACCEPT · 1 REVISE · 0 REJECT · ACCEPT-rate 87.5%.

**Verdict:** the SK-V17 T-P1 V3 inventory set is well-grounded on its
substrate / value-API / BackendShape / NEON spine — near-zero recalled
LOC, every load-bearing citation re-resolved at master `445925167`, the
V2 CSS-scan false-negative fully purged, the 8-carrier firewall scope
correct, all UNKNOWNs carrying verify_actions, and zero ground-truth
deferral. One residual ground-truth slip remains: 1D SK17L-008's
"+math.rs" parenthetical falsely adds a ninth scan carrier on a
doc-comment match (math.rs carries zero `scan_structural`). The single
REVISE must fold into V4 — delete "+math.rs", restate the eight-name
census — before this lens returns ≥95% ACCEPT. No orphan REVISE: the §1
fix is concrete and local to one line.
