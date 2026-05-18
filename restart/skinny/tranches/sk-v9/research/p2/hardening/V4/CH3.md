# SK-V9 S-P2 V4 — CH3 REGRESSION (REDRESS-Reopen Audit, V4 confirm)

Pass: S-P2 Research. Cycle: V4. Lens: CH3 (`restart/prompts/ORCHESTRATOR.md`
§3W). Cohort: S-P2 V4 fold — six artefacts (`skv9-p2-A` … `skv9-p2-F`).
V4 fold authority: `HARDENING-S-P2-V3-CONSOLIDATED.md` §"V4 fold
requirement — one trivial 1-token correctness fix" (CH1-origin, non-CH3:
P2-D `match_tiny_plain_string.rs:79`→`:81`). Fold commit: `1eee3375
docs(sk-v9-p2-v4): fold V3 CHALLENGE — match_tiny_plain_string line
fix` (P2-D only, 2 insertions, 2 deletions, one file). REDRESS authority
surface: `skinny/REDRESS.md` entries 28 (`:324-337`), 33 (`:394-418`),
88, 89, plus the inherited pre-block list unchanged from V3.

V3 CH3 verdict: **100% clean (43/43)** — 0 REJECT, 0 REGRESSION, 0
RESIDUAL-REVISE. V3 was the first qualifying cycle of the §3Z
≥95%-for-two-consecutive requirement. The V3 consolidation routed CH3
to **ACCEPT** and forecast V4 as a re-verify with no CH3 fold expected
(V3 left no residual). This V4 pass confirms that the V4 fold is the
1-token correctness fix only — no REDRESS-route reopen — and that the
entire V3 ACCEPT state is preserved unchanged.

## §1 — V3-state preservation

### §1.1 — V4 fold is the 1-token fix only

`git show --stat HEAD` and `git show --name-only HEAD` both confirm the
V4 commit touched **exactly one file** —
`skv9-p2-D-aarch64-asm-opportunities.md` — with 2 insertions and 2
deletions. P2-A, P2-B, P2-C, P2-E, P2-F carry **zero V4 edits**
(`git show HEAD --` against those five paths returns empty). The two
P2-D hunks were read in full:

- **§3.6 prose (line 489)**: `match_tiny_plain_string.rs:79` →
  `match_tiny_plain_string.rs:81`. The surrounding sentence ("A
  further-optimised variant could fold the range tests into a *single*
  TBL … but that's a Wave 2+ optimisation") is byte-identical; only the
  line number changed.
- **§8 sources (line 1117)**: `match_tiny_plain_string.rs:79` →
  `match_tiny_plain_string.rs:81`, with the appended clarifying clause
  "line 79 is the `#[cfg]` attribute". No other token changed.

Both edits are pure citation-coordinate corrections. **Verified against
the live source**: `bbnf-simd/src/aarch64/match_tiny_plain_string.rs`
line 81 is `pub unsafe fn match_tiny_plain_string_neon(`; line 79 is
`#[cfg(target_arch = "aarch64")]` (the attribute). The V4 fix is
factually correct — the V3 citation was off by two lines and now points
at the function signature.

This is a **CH1-origin** edit (the V3 consolidation tracked it as the
single LOW REVISE surfaced by CH1 V3). It is **CH3-neutral**: a citation
coordinate authors no intervention, broadens no consumer, wires no
kernel, and proposes no bbnf shape. No REDRESS route is touched. No
REDRESS-route reopen.

### §1.2 — V3 ACCEPT state preserved

The V3 CH3 ACCEPT rested on three closed residuals, three resolved V1
REJECTs, and the protected admitted-row set. Each is re-verified
unchanged at V4:

**The 3 V2 residuals stay folded.** All three live in P2-D/P2-F sections
the V4 commit did not touch:

- **D-1 / D-10 — §5.3.1 EOR3 six-row gate.** Verified at lines 857-866:
  the gate text reads verbatim "the EOR3 candidate's S-P3 admission
  carries an explicit no-regression maintain gate on the six W10b
  WIN-block rows (`canada`, `citm_catalog`, `instruments`, `marine_ik`,
  `mesh`, `numbers`) as a hard blocking precondition". Byte-identical to
  V3. The V4 hunks are at lines 489 and 1117 — outside §5.3.1. The fold
  stands.
- **F-7 — §5.2 inline REDRESS-33 citation.** Verified at P2-F lines
  357-360: "pre-blocked by `skinny/REDRESS.md` entry 33
  (`REDRESS.md:394-418`)". P2-F has zero V4 edits. The fold stands.
- **fold #3 — §0 cascade-sequencing footer.** Verified at P2-D lines
  1165-1170: the "Cascade-sequencing constraint" bullet, naming the
  four "block on P2-A landing OR fail CH5" slices and forbidding the
  wave split ("the wave may not be split"). Outside the V4 hunk range.
  The fold stands.

**The 3 V1 REJECTs stay resolved.** F-2 (§7.2 DirectBuild emit-site
clause stripped), F-3 (§7.3 retitled "admission shapes deferred",
admissions 1+2 deleted), F-6 (§3 "Room to widen the lead" walked back to
a finding). All three live in P2-F, which has zero V4 edits. Spot-check
of P2-F §3 (lines 270-283) confirms the walk-back text verbatim: "This
synthesis names the structural lead as a finding only … belongs to S-P3
with explicit material-differential gates per `skinny/REDRESS.md`
entries 33 and 66-69." §7.3 head still reads "admission shapes
deferred". The V1 REJECTs remain resolved.

### §1.3 — Typed-GO + direct-GO row protection (cross-cut)

The 4 typed-GO rows (`twitter`, `update_center`, `mesh`, `marine_ik`)
and 3 direct-GO rows (`citm_catalog`, `marine_ik`, `unicode_basic`)
remain explicitly protected at V4 — all guard sites are outside the V4
hunk range:

- **P2-C §4.3** (verified line 449): the falsifiability gate "Existing
  four typed GO rows hold their `A / GO` outcome … no regression below
  sonic × 1.10⁻¹ for twitter, update_center, mesh, marine_ik typed
  Track 1". P2-C has zero V4 edits. Byte-identical to V3.
- **P2-C §4.3 direct-row gate** (verified line 450): "the two existing
  direct rows for Apache (N-direct/NO-GO) and CITM (A/GO) must remain at
  their SK-V9-open verdicts". Unchanged.
- **P2-D §3.5 direct-route guard** (verified line 471): the
  `unicode_escapes/direct`, `y_string_unicode/direct`,
  `unicode_mixed/direct` no-regression CI guard. The V4 §3.6 hunk (line
  489) sits below §3.5; the §3.5 guard text is untouched.
- **P2-D §5.3.1 EOR3 six-row gate** (verified lines 859-861) — broadens
  protection to `marine_ik` and `citm_catalog`. Unchanged at V4.

The V4 fold does not touch a single typed-GO or direct-GO guard. The
protected set is preserved exactly as at V3.

## §2 — V4 dispositions (≥15)

Disposition columns: *Item* / *V3 verdict* / *V4 disposition* / *V4
verdict*. V4 verdicts: **CONFIRMED** (V3 CONFIRMED/RESOLVED holds; the
V4 fold does not touch the item, or touches it CH3-neutrally),
**REGRESSION** (V4 edit opened a new REDRESS route).

| # | Item | V3 | V4 disposition | V4 verdict |
|---|---|---|---|---|
| 1 | P2-D §3.6 line-number edit | n/a (new) | The V4 fold's first hunk: `:79`→`:81` on the §3.6 prose citation of `match_tiny_plain_string.rs`. Pure coordinate correction; verified correct against live source (fn at :81). Authors no intervention. | **CONFIRMED (CH3-neutral, CH1-origin).** |
| 2 | P2-D §8 sources line-number edit | n/a (new) | The V4 fold's second hunk: `:79`→`:81` in the §8 source list, with the clarifying "`#[cfg]` attribute" clause. Pure coordinate correction + a one-clause precision note. Authors no intervention. | **CONFIRMED (CH3-neutral, CH1-origin).** |
| 3 | D-1 EOR3 six-row gate (§5.3.1) | RESOLVED | Gate verbatim at lines 857-866; outside V4 hunk range. | **CONFIRMED.** |
| 4 | D-10 EOR3 owner-path + REDRESS 88 differential | RESOLVED | §5.3.1 owner path + three-axis differential + Lock-16 SHA3 gate + unconditional scalar fallback all intact; no V4 edit. | **CONFIRMED.** |
| 5 | D-2 §4.4 CSSC CTZ six-row gate | CONFIRMED | §4.4 narrow string-mask scope + six-row WIN-block gate intact; no V4 edit. | **CONFIRMED.** |
| 6 | D-4 §3.5 codec broadening / REDRESS-82 orphan bind | CONFIRMED | §3.5 "blocks on P2-A landing OR fails CH5; absent P2-A a REDRESS-82-style orphan held back" intact (lines 455-458); V4 §3.6 hunk is below it, untouched. | **CONFIRMED.** |
| 7 | D-§0 cascade-sequencing footer | RESOLVED | Footer bullet verbatim at lines 1165-1170; the §0-footer §8/§5.5 line-range note (line 1191) is V3-origin and unchanged. No V4 edit. | **CONFIRMED.** |
| 8 | D-§5.5/§8 REDRESS 28/33 line ranges | CONFIRMED (CH3-positive) | `:324-337` / `:394-418` ranges intact; the V4 §8 hunk corrected a *different* citation (`match_tiny_plain_string.rs`), not the REDRESS ranges. Verified REDRESS 28 head at `:324`, 33 head at `:394`. | **CONFIRMED.** |
| 9 | F-2 §7.2 DirectBuild emit-site strip | CONFIRMED | P2-F zero V4 edits; strip site intact. | **CONFIRMED.** |
| 10 | F-3 §7.3 "admission shapes deferred" | CONFIRMED | P2-F §7.3 head verbatim (line 525); admissions 1+2 still deleted. No V4 edit. | **CONFIRMED.** |
| 11 | F-6 §3 "Room to widen the lead" walk-back | CONFIRMED — load-bearing | P2-F §3 walk-back verbatim (lines 270-283); names lead as finding only, defers to S-P3 with REDRESS 33 / 66-69 gates. No V4 edit. **4 typed-GO rows unthreatened.** | **CONFIRMED — load-bearing for §3.** |
| 12 | F-7 §5.2 inline REDRESS-33 citation | RESOLVED | §5.2 citation verbatim at lines 357-360 with `:394-418` range. No V4 edit. | **CONFIRMED.** |
| 13 | C-6 §4.3 typed-GO falsifiability gate | CONFIRMED — load-bearing | Gate verbatim at P2-C line 449; P2-C zero V4 edits. Typed-GO guard intact. | **CONFIRMED — load-bearing for §1.3.** |
| 14 | C-1 / C-2 / C-4 REDRESS 91/93 posture | CONFIRMED | P2-C zero V4 edits; measured-admission framing, no retained-parse surface, REDRESS 93 not reopened. | **CONFIRMED.** |
| 15 | A-1 … A-9 union event-model cohort | CONFIRMED | P2-A zero V4 edits; `StructuralIndex` falsifier, five-variant `BackendShape`, class-column opt-in all carry forward from V3 unchanged. | **CONFIRMED.** |
| 16 | B-1 / B-3 / B-5 retained-grammar proof | CONFIRMED | P2-B zero V4 edits; `EventGrammar` trait proof, REDRESS 92 routing, `_witness`-dir admission gate carry forward. | **CONFIRMED.** |
| 17 | E-1 / E-2 / E-3 unicode-escape codec | CONFIRMED | P2-E zero V4 edits; §5 consumer differential (REDRESS 82), §6.4 0.70-slack rule, codegen-template specialisation carry forward. | **CONFIRMED.** |
| 18 | D-§6.3 per-primitive checkasm same-wave precondition | CONFIRMED (CH3-neutral) | §6.3 reword (V3-origin, tightens no-orphan posture) intact; no V4 edit. | **CONFIRMED.** |

### §2.1 — V4-edit REDRESS-reopen scan (cross-cut)

The V4 commit's two edits were each checked against the REDRESS
authority surface (entries 28, 33, 88, 89, and the inherited pre-block
list). Both edits are line-number coordinate corrections to a single
source-file citation (`match_tiny_plain_string.rs`). Neither edit:

- proposes a new bbnf intervention,
- broadens a consumer or substrate,
- wires a kernel into a hot path,
- alters a falsification gate or admission precondition,
- touches a REDRESS-cited route.

The §8 hunk's appended clause ("line 79 is the `#[cfg]` attribute") is
an explanatory precision note — it *improves* citation accuracy, which
strengthens the REDRESS-reopen audit surface rather than degrading it.
**No V4 edit reopens a REDRESS route. Zero REGRESSION dispositions.**

## §3 — Aggregate verdict

V4 cohort CH3 REGRESSION-disposition summary (43-item V3 base carried
forward, plus the 2 V4-edit dispositions audited explicitly — items 1-2
above):

| Report | CONFIRMED | REGRESSION | RESID-REVISE | Total |
|---|---:|---:|---:|---:|
| P2-A | 9 | 0 | 0 | 9 |
| P2-B | 6 | 0 | 0 | 6 |
| P2-C | 6 | 0 | 0 | 6 |
| P2-D | 12 | 0 | 0 | 12 |
| P2-E | 5 | 0 | 0 | 5 |
| P2-F | 7 | 0 | 0 | 7 |
| **Total** | **45** | **0** | **0** | **45** |

(P2-D's V4 count is the V3 base of 10 plus the 2 V4-edit dispositions.)

CONFIRMED clean dispositions: **45/45 = 100%**. **Zero REGRESSION. Zero
RESIDUAL-REVISE.** The two V4 edits are both CONFIRMED CH3-neutral
(CH1-origin coordinate corrections); neither opened a REDRESS-reopen
route.

**Verdict against the §3Z convergence criterion.** CH3 trajectory: V1
67.4% → V2 93.0% → V3 100% → **V4 100% clean (45/45)**. CH3 V4 clears
the 95% bar by 5.0 points with no scoring ambiguity. V3 was the first
qualifying cycle (≥95%); **V4 is the second consecutive qualifying
cycle**. CH3 therefore satisfies the §3Z ≥95%-for-two-consecutive
requirement. **CH3 V4 verdict: ACCEPT — 100%. CH3 CONVERGED.**

The V4 fold was the smallest possible cycle — a single 1-token
correctness fix in one file, CH1-origin, leaving the entire CH3 surface
untouched. The V3 ACCEPT state (3 folded V2 residuals, 3 resolved V1
REJECTs, the 4 typed-GO + 3 direct-GO protected rows) is preserved
byte-identically. P2-A, P2-B, P2-C, P2-E are fully converged on CH3
(zero edits at V3 and V4). P2-D and P2-F absorbed the V3 fold and (P2-D
only) the trivial V4 fix; both remain 100% clean on CH3.

## §4 — Any new risks

| # | Origin | REDRESS entry | Risk | Status at V4 |
|---:|---|---|---|---|
| 1 | D-1 / D-10 | 88 + 89 + HANDOFF §5 | §5.3.1 EOR3 six-row W10b no-regression gate as hard blocking precondition. | **CLOSED.** Carried forward from V3 unchanged; no V4 edit touched §5.3.1. No live reopen. |
| 2 | F-7 | 33 | §5.2 inline REDRESS-33 citation with `:394-418` range + lesson-vs-admission distinction. | **CLOSED.** P2-F zero V4 edits. No live reopen. |
| 3 | D-4 / D-1 / D-2 / D-10 | 82 + 88 + 89 | Cascade-sequencing constraint (four slices lose consumer if wave split) recorded in §0 footer. | **DOCUMENTED.** S-P3 P3-B inheritance; not a regression. No V4 change. |
| 4 | A-8 | Lock 1 + Lock 14 | Class column is a representation refinement every `OffsetTape` grammar inherits; non-JSON grammars have no SK-V9 consumer. | **CH4 hand-off.** Not a CH3 regression. P2-A zero V4 edits — unchanged from V3. |

**No new risk introduced at V4.** The V4 fold — a single 1-token
citation-coordinate correction in P2-D — adds zero REDRESS exposure: it
authors no intervention, alters no gate, touches no consumer, and the
appended `#[cfg]`-clause precision note marginally strengthens citation
hygiene. All four prior risk rows carry forward exactly: rows 1-2
CLOSED, row 3 DOCUMENTED, row 4 a CH4 hand-off. The cohort carries no
live REDRESS-reopen at V4.

**CH3 V4 = 100% — second consecutive qualifying cycle. The §3Z
two-consecutive-≥95% requirement is satisfied. CH3 is CONVERGED.**

— end CH3 V4.
