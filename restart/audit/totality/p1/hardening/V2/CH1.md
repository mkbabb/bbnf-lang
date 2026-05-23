---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation
cycle: V2
disposition: ACCEPT
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
spot_checks_dispatched:
  - "V1 REVISE discharge: COH-012 fabricated LOCKS.md:46 CH7 cite replaced (zero CH7 hits at HEAD)"
  - "V1 REVISE discharge: google_sheets=10 propagation across COH-011/AP-016/PC-017"
  - "V1 REVISE discharge: COH-011 eight-grammar vs nine-grammar prose vs ARCHITECTURE.md:764-765"
  - "1E §2.11 #4 sustained-UNKNOWN paragraph (1E:33-35) status under V1 CH6 REVISE #4"
  - "1D heavy-fold sub-rows internal consistency (row 117 substrate-union, row 124 D8/D10, row 131 PC-008 cross-cite, row 157 Track 2 substrate-helper)"
  - "1B D8/D10 split internal consistency at passes/src/lib.rs:331 vs :1300-1391"
  - "Executable mandate: LOCKS.md CH7 grep / google_sheets file count / parser-name leak / runtime sub-dir count"
verdict_summary:
  accept_rate: "8/8 (100%) artifacts CH1-clean at V2; three V1 REVISE folds discharged; one V1 CH6 REVISE #4 orphan (1E:33-35) flagged for V3"
  reject: 0
  revise: 0
  accept: 8
  v3_carry_forward: 1  # V1 CH6 REVISE #4 — sustained-UNKNOWN paragraph at 1E:33-35 not added (CH6 lens, not CH1)
head_commit_verified: 87816a2cd0d16ad0cdcf7b6483ef106efe363b52
---

# CH1 — CORRECTNESS lens disposition (T-P1 V2)

## Verdict

ACCEPT. All eight V2 inventories pass CH1 spot-check at full citation
resolution against live source at HEAD `87816a2cd` (T-P1 V2 atomic
micro-fold commit). The three V1 REVISE findings the V1 CH1 lens raised
against the 1F sibling outputs (COH-011 nine-grammar prose, COH-012
fabricated LOCKS.md:46 CH7 citation, google_sheets=10 propagation across
COH-011 / AP-016 / PC-017) are fully discharged by the V2 fold. Internal
consistency between 1B D8/D10 split rows, 1D heavy-fold sub-rows (row 117
substrate-union, row 124 D8/D10 cross-cite, row 131 PC-008 cross-cite,
row 157 Track 2 substrate-helper), and 1F-coherence COH-011 / 1F-anti
AP-016 / 1F-past PC-017 google_sheets census is verified at HEAD.

One V1 CH6 REVISE #4 carry-forward remains unresolved as flagged by the
V2 dispatch context §2: 1E:33-35 (executive summary) does not yet carry
the explicit sustained-UNKNOWN paragraph that V1 CONSOLIDATED §1.5 + CH6
prescribed (listing L03 + L16 + the two NEW SK-V14 UNKNOWNs). This is a
CH6 ANTI-PAPER-CLOSE concern, not a CH1 cite-resolution defect, but the
dispatch context explicitly tasked CH1 V2 to flag this status. Routed to
V3 CH6; does not demote the CH1 V2 ACCEPT-rate.

REJECT: none.

## Executable Verification Output (mandate per dispatch §2)

All four executable verifications pass at HEAD `87816a2cd`:

```
$ grep -n "CH7\|Overfit" restart/locks/LOCKS.md
(no output; exit 1 = zero matches)

$ find crates/core/src/runtime/google_sheets -type f | wc -l
10

$ find crates/core/src/runtime/google_sheets -type f
crates/core/src/runtime/google_sheets/parse_with.rs
crates/core/src/runtime/google_sheets/mod.rs
crates/core/src/runtime/google_sheets/arena.rs
crates/core/src/runtime/google_sheets/value.rs
crates/core/src/runtime/google_sheets/builder.rs
crates/core/src/runtime/google_sheets/view.rs
crates/core/src/runtime/google_sheets/document/path_query.rs
crates/core/src/runtime/google_sheets/document/mod.rs
crates/core/src/runtime/google_sheets/document/canonical.rs
crates/core/src/runtime/google_sheets/document/view.rs

$ rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/core/src/runtime/ | wc -l
30

$ rg -l 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/core/src/runtime/ | wc -l
15

$ find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l
9

$ find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d
crates/core/src/runtime/google_sheets
crates/core/src/runtime/bnf
crates/core/src/runtime/css_l4
crates/core/src/runtime/ebnf
crates/core/src/runtime/math
crates/core/src/runtime/bbnf
crates/core/src/runtime/json
crates/core/src/runtime/csv
crates/core/src/runtime/css_pretty
```

All four expected-values match the dispatch §2 prescriptions: zero
CH7/Overfit hits in LOCKS.md; google_sheets=10 files; 30 parser-name
leak matches across 15 files; 9 runtime sub-grammar directories.

## V1 REVISE Discharge Verification (CH1-bound)

### V1-REVISE-1 — google_sheets=10 propagation across three sibling 1F outputs

V1 CH1 §SC-4 found all three 1F outputs reporting `google_sheets=6` while
the live tree carries 10. V2 fold inspection:

| Site (V2) | Cited count | Cite resolution |
|---|---|---|
| `1F-coherence-scan.md:63` COH-011 | `google_sheets=10` | "live `find crates/core/src/runtime/google_sheets -type f \| wc -l` returns 10; verified at HEAD 2026-05-23" — matches executable output above |
| `1F-coherence-scan.md:92` COH-011 (Divergences Catalogued) | `google_sheets=10` | matches |
| `1F-anti-pattern.md:76` AP-016 | `google_sheets=10` | "verified `find crates/core/src/runtime/google_sheets -type f -name '*.rs' \| wc -l` returns 10; breakdown `8+7+7+7+7+7+10+7+7 = 67` arithmetically reconciles the asserted total" — matches |
| `1F-past-corpora.md:83` PC-017 | `google_sheets=10` | "verified `find crates/core/src/runtime/google_sheets -type f \| wc -l` = 10 at HEAD 2026-05-23; matches S-P0 A6 baseline `sk-v14-audit-overfit-pre-restart-pattern.md:53`" — matches |
| `1F-past-corpora.md:120` PC-017 (Divergences Catalogued) | `google_sheets=10` | matches |

Arithmetic now reconciles: `8+7+7+7+7+7+10+7+7 = 67`. The V1 CH1 §SC-4
internal-consistency defect (column-sum-vs-asserted-total mismatch) is
closed at V2. ACCEPT.

### V1-REVISE-2 — COH-012 LOCKS.md:46 fabricated CH7 citation

V1 CH1 §SC-5 found COH-012 at the prior `1F-coherence-scan.md:64` asserted
`restart/locks/LOCKS.md:46` declares "Lock 14 + CH7 Overfit-Prune lens
binding"; live grep showed zero CH7 hits in LOCKS.md. V2 fold inspected
at `1F-coherence-scan.md:74` (COH-012 row body):

> `restart/prompts/totality/PASS-1-EXCAVATION.md:91-138` registers six lenses CH1..CH6; no CH7 line. `restart/locks/LOCKS.md` carries no CH7 binding clause (verified `grep -n "CH7\|Overfit" restart/locks/LOCKS.md` returns zero hits at HEAD 2026-05-23); `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87` is the de facto CH7 lens authority that the SK-V14 contract relies on; `restart/skinny/tranches/sk-v14/SYNTHESIS.md:22` cites the CH7 binding via the SK-V14 audit-corrected baseline. 1E LAC-1E-12 at `1E-locks-evidence.md:120` carries the anti-fabrication phrasing `LOCKS.md (no CH7 mention)` as the canonical template.

The fabricated `LOCKS.md:46` cite is deleted; replaced with three
resolving citations (`PASS-1-EXCAVATION.md:91-138`,
`PASS-0-OVERFIT-AUDIT.md:62-87`, `SYNTHESIS.md:22`) and the explicit
anti-fabrication phrasing `(no CH7 mention)` with the executable grep
included inline. The Gap row at `1F-coherence-scan.md:119` propagates the
same anti-fabrication phrasing. The U-COH-012 Open Questions row at
`:127` likewise carries the grep-verified phrasing. ACCEPT.

Cross-reference: 1E LAC-1E-12 promotion paragraph at
`1E-locks-evidence.md:126-128` recognises that the V1 COH-012 empirically
reproduced "exactly the anti-pattern CH7 is built to prevent" (the
meta-CH7 collision) and uses this to harden the LAC-1E-12 promotion
candidacy from "non-blocking governance proposal" to
"binding-surface-authority precondition for any S-P2 / T-P3 CH7-citing
artefact". This meta-collision attribution is itself a sound CH1 cite —
the V1 CH1 §SC-5 finding is now the binding template for the V2 fold.

### V1-REVISE-3 — COH-011 eight-grammar vs nine-grammar prose

V1 CH1 §SC-6 found COH-011 prose "the eight-grammar set" contradicted
cited `ARCHITECTURE.md:765` ("nine extant grammars"). V2 fold inspected
at `1F-coherence-scan.md:73` (COH-011 row body):

> `restart/ARCHITECTURE.md:764-765` cites the nine-grammar set `(bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math)` but is not authoritative on per-grammar file counts. Live `find ... wc -l` returns 9; per-grammar census `bbnf=8, bnf=7, css_l4=7, css_pretty=7, csv=7, ebnf=7, google_sheets=10, json=7, math=7 = 67 hand-written files` ...

Three changes verified:
1. Citation widened from `:765` to `:764-765` (covers the full prose).
2. Prose corrected: "eight-grammar set" → "nine-grammar set".
3. Census enumerates 9 grammar names matching the live `find` output.

Live `ARCHITECTURE.md:764-765` (re-verified at HEAD):

> "the exception table is empty for the **nine** extant grammars (`bbnf`, `bnf`, `csv`, `css_l4`, `css_pretty`, `ebnf`, `google_sheets`, `json`, `math`) and stays empty unless a metadata + `@host fn` demonstration of insufficiency lands first."

Prose and cite now align. ACCEPT.

## V2 New CH1 Spot-Checks (dispatched per §2)

### SC-V2-1 — 1D heavy-fold sub-rows internal consistency

The V2 fold added four cross-cited heavy-fold sub-rows in 1D. CH1
verified each for cite resolution and cross-inventory consistency:

**Row 117 (substrate-union T-P3 §3C conditional disposition):**
Carries 1A-DIV-008 framing: `runtime/src/grammars/json/parser.rs:7-12`
vs `codegen/src/json_typed_direct.rs:518-522` (two structurally
independent cursor types at HEAD). Cross-cite to CH5-002 (renamed
StructuralIndex), CH5-005 (Track 2 substrate-helper), CH5-004 (CSS
source-sidecar), CH5-007 (proof-witness). All four sub-cases route to
"T-P3 §3C PENDING" — coherent disposition framing. Verdict cell
correctly downgraded to "proved historically; SK-V14 1A-DIV-008 records
two-cursor structural split at HEAD pending T-P3 §3C disposition" per
V1 CONSOLIDATED §2.11 #3. ACCEPT.

**Row 124 (D8/D10 cross-cite, pass-layer JSON-shape leak):**
Cites `passes/src/lib.rs:331` for D8 and `:1300-1391` for D10. Matches
1B D8/D10 split rows verbatim (1B:50/51/63/64/86/87 all use the same
line ranges). verify_action regex
`b"\\{"\|b"\\}"\|b"\\["\|b"\\]"\|b":"\|b"true"\|b"false"\|b"null"`
exactly matches the alphabet cited in 1B-D8/D10. ACCEPT.

**Row 131 (PC-008 + U-PC-002 cross-cite):**
Cites `1F-past-corpora.md:74` (PC-008 row body) and `:158` (U-PC-002).
Verified: PC-008 lives at line 74 of V2 1F-past-corpora and U-PC-002 at
line 158. Cross-cite resolves cleanly. ACCEPT.

**Row 157 (Track 2 substrate-helper, CH5-005):**
Cites `bbnf-bench/src/track2/json.rs:5,24,43` for `structural_capacity_for`
+ `TapeBuilder` + `OffsetFlags` + `CapacityPlan` imports. Matches 1F-anti
AP-011 row body at `1F-anti-pattern.md:71`
(`skinny/crates/bbnf-bench/src/track2/json.rs:5-8,24-32,43`). Cite shape
consistent. Prose includes "folds into row 100 substrate-union
ratification" — actually row 117 in V2 numbering (the substrate-union
row); the prose "row 100" survives from V1 numbering and is a stale
row-pointer artefact, not a fabrication. NOTE: minor prose-only quibble;
verdict and citation chain are intact. Flagged for V3 cosmetic-correction
only; does not warrant a V2 REVISE since the substantive cite resolves.

### SC-V2-2 — 1B D8/D10 split internal consistency

V1 CH2 flagged 1B-D8 row collapse of recognizer-mining vs role-mining
into a single divergence. V2 fold split into three table appearances:

| Site (V2) | D8 cite | D10 cite | Note |
|---|---|---|---|
| 1B:50/51 (Spec-Claim table) | `:325-355` (`derive_recognizers`) | `:1294-1391` (`derive_materialization_roles`) | distinct fn names, distinct line ranges; "Divergence `P1-1B-D8`" / "Divergence `P1-1B-D10`" |
| 1B:63/64 (Divergences Catalogued) | `:331` | `:1300-1391` (live fn at `:1302`) | "CH2 binding: upstream Sheets / BBNF-self generalization blocker — distinct surface from D10" / vice versa |
| 1B:86/87 (V2 Planning Metadata) | `:331` | `:1300-1391` | "NECESSARY-BUT-INSUFFICIENT" framing relative to PRUNE-4 carried in both rows |

Each row carries explicit "distinct surface from {other}" prose; the
CH2 upstream-blocker framing appears in all three table appearances. The
1B Sheets/BBNF-Self Implications section (1B:71/72) names both surfaces
as a "TWO distinct pass-layer pre-blockers (D8 + D10)" pair. No collapse
remains. ACCEPT.

### SC-V2-3 — 1C undercount discharge

V1 CH1 §SC-3 noted 1C reported "19+" parser-name leaks while live `rg`
returns 30; called this a "lower-bound formulation, honest". V2 dispatch
context §1 says 1C V2 amended to "30 across 15 files (full enum incl. 4
google_sheets/ sites V1 missed); reexports 60+→126; LOC repair `~50 LOC`
→ `47 lines hold 126 grammar-named symbols; ~190 LOC + 2.5× consumer-
rewire band`".

Live verification at HEAD:

```
$ rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/core/src/runtime/ | wc -l
30
$ rg -l 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/core/src/runtime/ | wc -l
15
```

V2 1C now reports the full 30/15 census; the four google_sheets/ sites
V1 undercounted are folded. ACCEPT.

### SC-V2-4 — 1E §2.11 #4 sustained-UNKNOWN paragraph status

Dispatch context §2 flags this as "possibly unresolved fold item". V1
CONSOLIDATED §1.5 + CH6 REVISE #4 prescribed:

> 1E :89 L16 executive summary: add explicit sustained-UNKNOWN paragraph listing L03 + L16 + the two NEW SK-V14 UNKNOWNs at the `1E:33-35` framing to prevent held-disposition language reading as universal closure.

V2 1E executive summary at `1E-locks-evidence.md:33-35` was inspected:

- Paragraph 1 (line 33) covers 4 amendment vectors: 30 Lock 14 violations, Pattern H = 67, CH7 lens, R4 regen-css.
- Paragraph 2 (line 35) covers CSS L4 substrate-classification + LAC-1E-12..16 summary.
- `grep -n "sustained-UNKNOWN\|L16 sustained\|L03.*L16\|UNKNOWN paragraph" restart/audit/totality/p1/1E-locks-evidence.md` returns zero hits.

The V1 CH6 REVISE #4 instruction is **NOT** discharged by the V2
micro-fold. The Open Questions table at 1E:155-162 does carry L03 + L16
+ "NEW SK-V14" UNKNOWN rows, but the executive summary still does not
call them out as sustained; the held-disposition language ("4 amendment
vectors LOCKS.md does not yet absorb") remains readable as universal
closure of prior-cycle UNKNOWNs.

**Classification:** CH6 ANTI-PAPER-CLOSE carry-forward
(cite-resolution intact; framing softness only). Does not block CH1 V2
ACCEPT but is flagged for V3 CH6 disposition. Recommended V3 fold:
prepend one sentence to 1E:33 reading:

> Two V4-carried UNKNOWNs (L03 cursor elision, L16 full allowlist coverage) remain sustained at SK-V14 baseline, joined by two NEW SK-V14 UNKNOWNs (audit-overlay column binding gap, CSS L4 FactStream taxonomy disposition); these do not collapse into the four amendment vectors below — they are governance-track residuals T-P3 §3C disposes separately.

### SC-V2-5 — 1F-coherence divergence_count rekeyed to 7-key schema

V1 CONSOLIDATED §2.11 #6 prescribed reconciliation between the
divergence_count frontmatter and the V2 Planning Metadata row table.
V2 fold rekeyed to a 7-key schema at `1F-coherence-scan.md:37-52`:

```
spec_surface_drift: 5
partially_implemented: 1
unimplemented_cleanup: 1
silent_must_add: 4
impl_exceeds_spec: 1
unknown_open_questions: 3
total_rows: 12
```

Sum-check: `5+1+1+4+1 = 12` matches `total_rows`. Inline comment at
`:38-44` makes the bookkeeping explicit. ACCEPT.

## ARCHITECTURE.md / MASTER-PLAN.md grammar count drift check (dispatch §2)

V1 dispatch flagged potential grammar count drift in ARCH if applicable.
Live check at HEAD:

`ARCHITECTURE.md:764-765` says "nine extant grammars (`bbnf`, `bnf`,
`csv`, `css_l4`, `css_pretty`, `ebnf`, `google_sheets`, `json`,
`math`)". Live `find crates/core/src/runtime -mindepth 1 -maxdepth 1
-type d | wc -l` returns 9, names match exactly. No drift; no V2
1F-coherence disposition revision warranted. ACCEPT.

## Other CH1 spot-checks (non-dispatched, opportunistic)

- 1A V2 frontmatter still cites 1A-DIV-008 substrate-union nuance with T-P3 §3C conditional ratification rule (per dispatch §1 expectation); cite shape well-formed.
- 1B V2 P1-B-D6 SinkOnly 1/5 lowerer cite at `1B-codegen-evidence.md:84` carries forward (V1 CH1 SC-2 verified at HEAD already; no V2 regression).
- 1C V2 still reproduces the 67/67 Pattern H census; arithmetic `8+7+7+7+7+7+10+7+7 = 67` reconciles in the COH-011 / AP-016 / PC-017 row prose explicitly.
- 1F-anti AP-020 new V2 row (CSS source-sidecar comparator plane per CH5-004 binding fold) cites `bbnf-bench/src/nonjson_css_l4.rs:222,234,299,504` — line ranges match AP-009 companion cite at `:222-234,298-303`, framed as a deliberate lift-out rather than collapse into AP-009. Cite-frame coherent. ACCEPT.
- 1F-past PC-008 anchor reinforcement: row at `1F-past-corpora.md:74` now carries explicit U-PC-002 cross-cite + verify_action `rg -n 'JSON_STRUCTURAL|scan_json|JsonParseIndex' skinny/crates/bbnf-simd skinny/crates/runtime`. Row at `:158` (U-PC-002) carries identical verify_action. 1D row 131 cross-cites both. Coherent ledger triangulation. ACCEPT.
- 1F-past PC-001/002/004 verdicts now mirror PC-003 parity ("accepted historical pre-block; current absence UNKNOWN") with explicit verify_action route at `1F-past-corpora.md:67-70` (V1 CONSOLIDATED §2.11 #5 discharge). ACCEPT.

## ACCEPT-rate summary

| Artefact | CH1 V2 disposition | Notes |
|---|---|---|
| 1A-substrate-evidence.md | ACCEPT | BIR 13/20 cites resolve; SimdScan-separate caveat in body; 1A-DIV-008 substrate-union nuance disposition coherent with T-P3 §3C conditional ratification rule |
| 1B-codegen-evidence.md | ACCEPT | 1/5 lowerers verified; D8/D10 split (3 table appearances) carries CH2 upstream-blocker framing and NECESSARY-BUT-INSUFFICIENT relative to PRUNE-4 |
| 1C-runtime-evidence.md | ACCEPT | 30 parser-name leak across 15 files reproduces at HEAD (V1 "19+" lower-bound discharged); 126 reexport count band per dispatch §1 |
| 1D-skinny-lessons.md | ACCEPT | 7-row digest re-stratified; row 117 substrate-union, row 124 D8/D10, row 131 PC-008 cross-cite, row 157 Track 2 substrate-helper all internally consistent; row 157 has minor stale "row 100" pointer (cosmetic; flagged) |
| 1E-locks-evidence.md | ACCEPT (with V3 carry-forward) | LAC-1E-12 promoted; meta-CH7 collision attribution sound; **V1 CH6 REVISE #4 sustained-UNKNOWN paragraph at 1E:33-35 NOT discharged** — carried to V3 CH6 |
| 1F-coherence-scan.md | ACCEPT | COH-011 nine-grammar + google_sheets=10 + COH-012 anti-fabrication template all discharge V1 REVISE; divergence_count rekeyed to 7-key schema with sum-check |
| 1F-anti-pattern.md | ACCEPT | AP-016 google_sheets=10; AP-020 NEW CSS source-sidecar row coherently lifted from CH5-004 binding fold |
| 1F-past-corpora.md | ACCEPT | PC-017 google_sheets=10; PC-008 anchor + U-PC-002 cross-cite; PC-001/002/004 parity with PC-003 |

**ACCEPT-rate: 8/8 = 100.0%.** Above the §3Z ≥95% convergence threshold
on first cycle for V2.

**Cycle disposition:** V2 first-pass ≥95% achieved (100%). Under §3Z
two-cycle-chain rule, V3 must reproduce ≥95% on the same artefact set
to close the CHALLENGE wave. One unresolved V1 carry-forward (1E:33-35
sustained-UNKNOWN paragraph) is a CH6 concern, not CH1; CH1 V2 carries
no own-lens REVISE.

**Predicted trajectory:** V2 (CH1 100%) → V3 (CH1 ≥95% expected if
1E:33-35 carry-forward addressed under V3 CH6 fold; no CH1-specific
regression risk identified) → LOCK on consecutive ≥95% V2+V3 ACCEPT
chain for CH1.
