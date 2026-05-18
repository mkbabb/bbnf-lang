# SK-V9 S-P1 V6 — CH4 COST Lens (confirmation cycle)

Pass: S-P1 Profile. Cycle: V6 (second-consecutive-qualifying confirmation
on UNCHANGED V5 substantive).
Date: 2026-05-18.
Lens: CH4 COST (LOC budget, risk class, wave alignment, hard cap,
same-wave consumer, revert protocol — per `restart/prompts/ORCHESTRATOR.md`
§3W, §7, §8, §9; "No contrivance — smallest change that achieves
elegance + performance").
Scope: the six V5-folded P1-V3 artefacts at
`restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md`
(V5 fold commit `d76eef63`), with no V5→V6 substantive edit. V6 is
the §3Z step-5-equivalent "no fold required" cycle: it exists solely
to deliver the second-consecutive qualifying CHALLENGE pass on CH4,
which V5 cleared at 100% strict (first qualifying cycle; V4 was 93.3%,
below the 95% bar). The other five lenses (CH1/CH2/CH3/CH5/CH6) already
satisfied two-consecutive at V4+V5 per
`HARDENING-S-P1-V5-CONSOLIDATED.md`; CH4 is the lone outlier needing
V6 to seal pass-level convergence.
Disposition vocabulary: PRESERVED / DRIFTED (per §1); ACCEPT / REVISE /
REJECT (per §2).

V6 substantive attestation: `git log --oneline d76eef63..HEAD --
restart/skinny/tranches/sk-v9/research/p1/` returns only
`c8233b2c docs(sk-v9-p1-v5-challenge): archive CH1-CH6 + consolidated verdict`
— the V5 CHALLENGE archive commit. No substantive V5→V6 edit landed.
`d76eef63` is the head of the substantive surface; V6 reads it byte-
for-byte.

---

## §1 V5-ACCEPT preservation per gap (5 gaps + 2 ACCEPT-WITH-NOTE carries)

Each V5 ACCEPT disposition (V05/V19, V20, V21, V22, V23 promoted from
REVISE / REVISE-OPTIONAL; V24, V25 carried as ACCEPT-WITH-NOTE) is
re-checked against the byte-for-byte V5 surface.

### §1.1 V05 / V19 — Time Profiler re-capture wall

V5 ACCEPT anchor: V3-B §0 footer adds a four-bullet cost block at lines
1158-1174 stating CPU Counters ~12 min, Time Profiler ~22 min, lto=fat
~3-5 min one-time, aggregate ~37-39 min.

V6 read of
`restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-B-xctrace-time-profiler.md`
lines 1158-1174 (verbatim quote of the bullet):

> **V5 fold — re-capture wall cost (CH4-V05/V19/V20):** A full V5+ S-P1
> re-capture across 17 corpora × {track1, track2} carries the following
> deterministic wall costs on the SK-V9 host (Apple M5 Max, 12P+6E, full
> Xcode 26.0, `target-cpu=native`):
> - **xctrace CPU Counters template** (P1-V3-A path): ~12 min wall for
>   34 captures (0.5-3 s steady-state per launch + xctrace startup
>   overhead).
> - **xctrace Time Profiler template** (this report's path): ~22 min wall
>   for 34 captures (longer per-launch sampling + per-symbol DWARF
>   export via `xcrun xctrace export`).
> - **`lto=fat` cold-link cost**: ~3-5 min one-time when the probe
>   binary's profile changes …
> - **Aggregate**: ~37-39 min wall for a full re-capture cycle; an
>   isolated CPU Counters re-capture is ~12 min; an isolated TP
>   re-capture is ~22 min.

All four numbers present, host-fixed, arithmetic consistent
(12 + 22 + 3-5 = 37-39). Status: **PRESERVED**.

### §1.2 V20 — `lto=fat` cold-link cost

V5 ACCEPT anchor: V3-B §0 footer line 1168-1171 cites ~3-5 min
one-time, conditional "when the probe binary's profile changes".

V6 read of the same lines (preserved verbatim in the V05/V19 quote
above). The conditional framing remains; double-counting on re-runs
of unchanged probes remains explicitly ruled out. Status:
**PRESERVED**.

### §1.3 V21 — V3-F edit-dispatch hard cap

V5 ACCEPT anchor: V3-F §4 introduction lines 463-470 add ≤30 min
total batch hard cap, three-file sequence (SPEC.md (8) → HANDOFF.md
(6) → DISPATCH-PROMPT.md (5)), single `git revert` on the batch
commit.

V6 read of
`restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-F-redress-reconciliation.md`
lines 463-470 (verbatim quote):

> **Edit-dispatch hard cap (V5 fold per CH4-D29 / CH4-V21).** The full
> batch of 19 surgical edits below carries a single dispatch hard cap of
> **≤30 minutes total**. Each individual edit is a single-paragraph
> single-list-row in-place modification with no source compilation cost;
> the batch is sequenced commits SPEC.md (8) → HANDOFF.md (6) →
> DISPATCH-PROMPT.md (5) so partial-batch progress can land safely.
> Revert protocol: single `git revert` on the batch commit; the
> underlying V3 evidence is unaffected.

All three axes — hard cap (≤30 min), sequence (SPEC→HANDOFF→
DISPATCH-PROMPT), revert (single `git revert` on batch commit) —
present in the byte-for-byte V5 text. Status: **PRESERVED**.

### §1.4 V22 — two-wave sequence (FOLDED-BY-SUPERSESSION)

V5 ACCEPT anchor: V21's per-file sequence supersedes the V22 explicit
two-wave order; V3-F §6.6 cohesion-risk-low classification remains.

V6 re-reads the V21 sequence quoted above ("SPEC.md (8) → HANDOFF.md
(6) → DISPATCH-PROMPT.md (5)") and confirms the per-file ordering is
inside the ≤30 min cap; the supersession of V22 by V21 holds because
the per-file sequence is strictly stronger than the V4 D30 two-wave
ask (status-vocab first / evidence-bound second collapses into the
single ordered batch). Status: **PRESERVED**.

### §1.5 V23 — `aggregate.py` reproducibility

V5 ACCEPT anchor: V3-B §0 footer lines 1176-1183 cite
`/tmp/skv9-xctrace-v3/aggregate.py` with the exact
`xcrun xctrace export --type tabular --output <out> --input <trace>`
invocation, names input `.xml` format, bucketisation key (symbol),
output format `exports/<corpus>__<track>.symbols.json`. File on disk
attested at 319 LOC, 11188 bytes.

V6 re-verification: `test -f /tmp/skv9-xctrace-v3/aggregate.py` →
present; `wc -l` → 319 lines; size 11188 bytes; mtime 2026-05-18
15:12:25 (unchanged from V5). V3-B §0 lines 1176-1183 read verbatim:

> **V5 fold — `aggregate.py` reproducibility (CH4-V23):** The TP-symbols
> aggregator script lives at `/tmp/skv9-xctrace-v3/aggregate.py` (already
> on disk). Re-running it against the captured `.trace` bundles
> deterministically regenerates `exports/<corpus>__<track>.symbols.json`.
> The script is reproducible-by-instruction: `aggregate.py` reads the
> exported `.xml` from `xcrun xctrace export --type tabular --output
> <out> --input <trace>` and bucketises by symbol; the buckets are the
> per-symbol self-time tables surfaced in §2-§3 of this report.

Script path, invocation, input format, bucketisation key, output format
all preserved. Filesystem evidence preserved. Status: **PRESERVED**.

### §1.6 V24 — OLS out-of-sample fixture (ACCEPT-WITH-NOTE carry)

V5 carry: F1 routes synthetic quote-heavy fixture authoring to S-P3;
the V3-D §0 V5 enumeration of 8 publication errors cites
`regression.py` / `regression_output.json` giving R²/SE/t/p/per-row
residuals — sufficient S-P1 audit surface.

V6 re-reads V3-D §0 lines 25-51 (eight numbered publication errors)
and confirms item 4 ("R² absent in V3: V4 publishes 0.371 (modest fit;
coefficient `b` is not statistically significant at p=0.545)") and
item 7 ("OLS sign-convention provenance gap: V3 lacked the script that
produced the coefficients; V4 commits it, reproducible bit-for-bit").
The audit surface is intact. Status: **PRESERVED**.

### §1.7 V25 — E2 ≤15 min gate wall cold-target plausibility (ACCEPT-WITH-NOTE carry)

V5 carry: ≤15 min is plausible on a warm cargo target; cold target may
exceed. V5 commits no new gate, so V25 is unchanged.

V6 confirms V5 introduced zero new gates: the V5 commit `d76eef63`
diffstat (5 files, +76/-7) carries no `cargo test` insertion, no new
`xtask check-*` invocation, no new admission gate. V3-E §4.3 step 6
remains the only ≤15 min gate-wall locus and is unchanged by V5.
Status: **PRESERVED**.

### §1.8 Preservation rollup

| Gap | V5 disposition | V6 read | Status |
|---|---|---|---|
| V05/V19 | FOLDED (ACCEPT) | byte-for-byte present at V3-B §0 1158-1174 | **PRESERVED** |
| V20 | FOLDED (ACCEPT) | byte-for-byte present at V3-B §0 1168-1171 | **PRESERVED** |
| V21 | FOLDED (ACCEPT) | byte-for-byte present at V3-F §4 463-470 | **PRESERVED** |
| V22 | FOLDED-BY-SUPERSESSION | V21 sequence supersedes; V3-F §6.6 risk-class unchanged | **PRESERVED** |
| V23 | FOLDED (ACCEPT) | byte-for-byte present at V3-B §0 1176-1183; file on disk | **PRESERVED** |
| V24 | ACCEPT-WITH-NOTE carry | F1 routing unchanged; V3-D §0 audit items 4 + 7 intact | **PRESERVED** |
| V25 | ACCEPT-WITH-NOTE carry | no new gate landed in V5 | **PRESERVED** |

7 of 7 V5 dispositions PRESERVED. 0 DRIFTED.

---

## §2 V6 dispositions (verifying unchanged substantive holds)

Twenty dispositions on the seven dispatch-named axes (re-capture
wall, lto=fat cold-link, edit-dispatch hard cap, two-wave supersession,
aggregator reproducibility, new-cost-gap scan, risk tiering,
same-wave-consumer rule) plus cross-cutting cost re-verifications.

### §2.1 Confirmation of V5-named anchors (5 anchors)

| # | Axis | Evidence | Disposition |
|---|---|---|---|
| W01 | V3-B §0 four-bullet cost block carries CPU Counters ~12 min + TP ~22 min + aggregate ~37-39 min | Lines 1158-1174 (quoted §1.1 above); host pinned to "Apple M5 Max, 12P+6E, full Xcode 26.0, `target-cpu=native`" | ACCEPT |
| W02 | V3-B §0 `lto=fat` cold-link cite reads "~3-5 min one-time when the probe binary's profile changes" | Line 1168-1171; "one-time" framing and conditional explicit | ACCEPT |
| W03 | V3-F §4 edit-dispatch hard cap states ≤30 min total batch + SPEC→HANDOFF→DISPATCH-PROMPT sequence + single `git revert` on batch | Lines 463-470 (quoted §1.3 above) | ACCEPT |
| W04 | V22 superseded by V21's per-file sequence inside the ≤30 min cap | V3-F §4 line 467 (per-file order); V3-F §6.6 unchanged (low cohesion risk preserved) | ACCEPT |
| W05 | `/tmp/skv9-xctrace-v3/aggregate.py` cited with exact `xcrun xctrace export --type tabular` invocation; file still on disk (319 LOC, 11188 bytes, mtime 2026-05-18 15:12:25) | V3-B §0 lines 1176-1183; filesystem verify | ACCEPT |

5/5 ACCEPT.

### §2.2 Same-wave consumer + risk tiering re-verification (4 dispositions)

| # | Axis | Evidence | Disposition |
|---|---|---|---|
| W06 | V3-A §6.5 PMU manifest remains diagnostic-only / non-`gate-json` consumer / Lock-1-bound | V3-A lines 424-442: "The per-row PMU manifest … is diagnostic profile evidence; it does not participate in admission gates and does not extend `RESULTS.md` schema … Per `LOCKS.md` Lock 1 … and the §3W 'Same-wave consumer — no orphan kernel' non-negotiable, this manifest is bound to characteriser status … If a later wave wishes to gate on cycles/B, it must either commit a stable in-repo manifest path (superseding the `/tmp/` location) and a matching `gate-json` reader in the same wave, or accept the manifest's current diagnostic-only binding indefinitely." Same-wave-consumer rule is enforced at the manifest's own status line, not deferred. | ACCEPT |
| W07 | E1 dispatch contract preserves LOW risk + ≤30 min + no `cargo test` | V3-E §0/§1/§4.3 unchanged by V5; no V5 edit touches V3-E | ACCEPT |
| W08 | E2 dispatch contract preserves MEDIUM risk + ≤45 min + `cargo test --workspace --profile ax-iter` + per-ISA-family revert granularity | V3-E §2/§4.3 unchanged by V5 | ACCEPT |
| W09 | F1 wave-deferral preserved — no orphan kernel-LOC envelope at S-P1 boundary; wave authorship routed to S-P3 | V3-D §0/§6.1/§6.2/§6.4 (wave authorship deferred); V3-D §6 finding-only frame unchanged by V5 | ACCEPT |

4/4 ACCEPT.

### §2.3 New-cost-gap scan (6 dispositions)

The V5 fold adds two substantive cost-bearing blocks: V3-B §0 four-
bullet wall-cost block (W01) and V3-F §4 dispatch cap (W03). The V5
fold also adds three doc-only edits: V3-A §3 line 237 hedge, V3-C §5.3
line 717 hedge, V3-D §0 footer eight-error enumeration. CH4 scans each
for uncosted "additional mechanism" promises.

| # | New surface (V5 edit) | Cost commitment? | Disposition |
|---|---|---|---|
| W10 | V3-A §3 line 237 hedge ("unambiguous agreement" → V4 §4+B §3.4 falsified) | Pure prose hedge; zero new mechanism | ACCEPT — doc-only, no budget |
| W11 | V3-C §5.3 line 717 hedge ("largest single cycle sink" → "among the largest") + 2.38 c/B marginal-lead cite | Pure prose hedge with existing-data citation; zero new measurement | ACCEPT — doc-only, no budget |
| W12 | V3-D §0 V5 footer eight-error enumeration (CH6-D) | Backward disclosure of already-committed `regression.py` / `regression_output.json` outputs; zero new tooling, zero new wall-cost commitment | ACCEPT — restatement of V4-committed evidence; no new gap surfaces |
| W13 | V3-D §0 V5 enumeration item 8 "per-row residual table absent in V3: V4 publishes per-row residuals showing the four uncloseable rows exceed 130-460% of the regression's full per-byte budget — a hypothesis-sized finding, not a wave-sized intervention" | Explicit finding-form classification ("not a wave-sized intervention") — actively rejects an additional-mechanism promise | ACCEPT — strongest possible CH4 framing; this is the antithesis of an uncosted mechanism |
| W14 | V3-B §0 V5 cost-block discloses ~37-39 min re-capture wall as conditional ("A full V5+ S-P1 re-capture …") rather than as a V5 commitment | Cost-disclosure pattern: states budget for a *hypothetical* future re-capture without committing V5 to spend it. The two lines immediately preceding (line 1156: "No re-capture, no re-measurement, no number revised") rule out V5 consumption | ACCEPT — discloses budget for any S-P3+ consumer, V5 itself spends zero |
| W15 | V3-F §4 V5 hard-cap block discloses ≤30 min as the cap on the *proposed* SPEC/HANDOFF/DISPATCH-PROMPT edit dispatch (not yet executed) | Cost-disclosure pattern: cap is on a future dispatch, sequenced and revert-bound; V5 itself does not execute the 19 edits | ACCEPT — cost binding precedes consumption, per §8 hard-cap non-negotiable |

6/6 ACCEPT. **No new uncosted "additional mechanism" promise surfaces**
anywhere in the V5 substantive. V3-D §0's enumeration is the inverse
case: it explicitly classifies the residual finding as
"hypothesis-sized, not wave-sized", which is the CH4-correct posture.

### §2.4 Cross-artefact V5-substantive re-verification (5 dispositions)

| # | Axis | Evidence | Disposition |
|---|---|---|---|
| W16 | V3-A §0 V4 fold footer + V3-B §0 V4 fold footer + V3-C §0 V4 fold footer + V3-D §0 V4 fold footer + V3-E (no §0; V4 fold integrated via E1/E2 contracts) + V3-F §0 V4 fold footer all carry per-edit attribution; V5 cost-edits visibly extend these footers without replacing the V4 audit trail | V3-B §0: "V4 fold … No re-capture, no re-measurement, no number revised." (line 1156) is preserved verbatim immediately above the V5 cost block — V5 fold is additive, not overwriting | ACCEPT |
| W17 | V5 commit `d76eef63` diffstat (5 files, +76/-7) confirms surgical surface | `git log d76eef63` + diffstat in V5 CH4 §0 attestation; 5 files (V3-A, V3-B, V3-C, V3-D, V3-F), no source files, no generated files | ACCEPT |
| W18 | Risk-tiering preservation across V3-A/B/C/D/E/F: every actionable surface (E1, E2, F-edit dispatch) carries an explicit risk class; finding-only artefacts (B, C, D-§6) carry no actionable surface and correctly carry no risk class | V3-E §0/§2 LOW/MEDIUM unchanged; V3-F §6.1-§6.6 per-edit risk classes unchanged; V3-A §6.5 LOW diagnostic-only unchanged; V3-D wave-deferral preserved | ACCEPT |
| W19 | Per-ISA-family commit granularity (D24) preserved at V3-E §2.1 / §4.3 — bisectable revert blast radius for 14 x86_64 orphan kernels | V3-E §0/§1/§2/§4.3 unchanged by V5; per-family granularity (avx2, avx512_vbmi2, avx512_gfni, avx512_vpclmul, avx512_vnni, avx512_bitalg, avx512_kmask, avx_ifma) preserved | ACCEPT |
| W20 | §3Z V≤5 hard ceiling visible at the CONSOLIDATED layer; V6 is the *confirmation* cycle on unchanged substantive (no V6 fold required) | `HARDENING-S-P1-V5-CONSOLIDATED.md` §"V6 protocol — CH4 confirmation cycle": "V5 substantive is the new state. V6 = unchanged substantive + a fresh CHALLENGE on CH4 … Per §3Z step 5, V6 substantive ≡ V5". Confirmed by git: `git log d76eef63..HEAD -- restart/skinny/tranches/sk-v9/research/p1/` returns only the V5 CHALLENGE archive commit `c8233b2c`, never a substantive edit | ACCEPT |

5/5 ACCEPT.

### §2.5 V6 disposition rollup

Aggregate across §2.1 + §2.2 + §2.3 + §2.4 = 20 dispositions:

| Disposition | Count |
|---|---:|
| ACCEPT | 20 |
| REVISE | 0 |
| REJECT | 0 |

**V6 CH4 ACCEPT rate: 20/20 = 100.0% strict.**

V24 (F1-routed fixture) and V25 (cold-target gate wall) remain
ACCEPT-WITH-NOTE carries from V4/V5, preserved verbatim in §1.6 and
§1.7 above. The notes are clarifying observations on V4-contract-correct
deferrals; they are not gap claims. CH4 V6 does not re-litigate them.

---

## §3 Aggregate verdict

V6 is the second-consecutive-qualifying CHALLENGE cycle on CH4 against
unchanged V5 substantive. The §3Z criterion ("CHALLENGE V{N} returns
≥95% ACCEPT for two consecutive cycles") requires V5 ACCEPT ≥95%
AND V6 ACCEPT ≥95%. V5 returned 100% (first qualifying); V6 returns
100% (second qualifying).

| Cycle | CH4 ACCEPT | ≥95%? | Consecutive count |
|---|---:|---|---:|
| V3 | 14% | ✗ | 0 |
| V4 | 93.3% | ✗ | 0 (gap at 1.7pp) |
| V5 | 100% | ✓ | 1 |
| **V6** | **100%** | **✓** | **2** |

**Two-consecutive convergence: ACHIEVED.**

CH4 V6 verdict: **ACCEPT — all five V5 ACCEPT anchors PRESERVED
byte-for-byte; no new cost gap surfaces; no DRIFT detected. CH4
clears §3Z ≥95% bar for the second consecutive cycle. S-P1
pass-level convergence is satisfied on CH4.**

Combined with CH1/CH2/CH3/CH5/CH6 (already two-consecutive at V4+V5
per `HARDENING-S-P1-V5-CONSOLIDATED.md` §"Verdict"), S-P1 reaches
full pass-level convergence. The orchestrator advances to S-P2
Research per `restart/prompts/skinny/PASS-2-RESEARCH.md`.

The V5 fold remains the smallest possible cost-disclosure surface
that satisfies the V4 CH4 demands. The V6 re-read confirms zero
substantive drift, zero new mechanism promises, zero gate-wall
commitments, zero new manifest or schema surface. The V3-D §0 V5
eight-error enumeration's explicit "hypothesis-sized, not wave-sized"
phrasing (item 8) is the canonical CH4 posture and preserves correctly.

---

## §4 Any new defects

**None.**

CH4 V6 scanned the V5 substantive surface for the eight gap classes
the §8 non-negotiables require:

1. **LOC budget absence on a proposed action.** V5 substantive proposes
   no new action requiring a kernel-LOC envelope. The V3-F edit
   dispatch (the only actionable surface V5 touches) carries an
   explicit ≤30 min cap (W03). E1/E2 caps preserved from V4 (W07/W08).
2. **Risk class absence on a proposed action.** Every actionable surface
   (E1 LOW, E2 MEDIUM, F-edits per-§6 classes, A §6.5 LOW diagnostic-
   only) carries a risk class (W07/W08/W18).
3. **Wave alignment absence.** V3-D §6 wave authorship explicitly
   deferred to S-P3 (W09); F1 contract preserved.
4. **Hard cap absence.** E1 ≤30, E2 ≤45 (incl. ≤15 min gate), F-edits
   ≤30 batch — all present (W03/W07/W08).
5. **Same-wave consumer absence.** V3-A §6.5 PMU manifest carries the
   diagnostic-only / Lock-1 / non-`gate-json` binding plus the
   conditional same-wave-consumer rule for any future promotion (W06).
6. **Revert protocol absence.** E2 carries per-ISA-family `git revert`
   granularity (W19); F-edits carry single `git revert` on batch
   commit (W03).
7. **Orphan REDRESS / orphan kernel.** No orphan surfaces. The PMU
   manifest is bound to characteriser status indefinitely or until a
   same-wave consumer lands (W06). The 14 x86_64 orphan kernels are
   the *subject* of the E2 cleanup, not orphan-status-extending.
8. **Uncosted "additional mechanism" promise.** None. The V3-D §0 V5
   footer's item 8 explicitly classifies the residual delimiter-only
   finding as "hypothesis-sized, not wave-sized" — the inverse of an
   uncosted mechanism (W13). No paragraph in the V5 substantive
   promises a future mechanism without LOC, risk, hard-cap, and
   same-wave consumer (W14/W15).

The two ACCEPT-WITH-NOTE carries (V24 F1-routed fixture deferral; V25
cold-target gate-wall plausibility) remain notes, not gaps. V24's
F1 routing is contract-correct per `PASS-1-PROFILE.md` §9 ("S-P1
produces evidence, and S-P2 produces the hypotheses"); CH4 cannot
demand fixture authoring at S-P1 boundary. V25's cold-target concern
is dispatcher discipline (pre-warm by no-op `cargo check`), not a
V5 gap.

CH4 V6 closes at 100.0% strict ACCEPT with zero DRIFT and zero new
defects. **S-P1 fully converges.**

---

## §5 Sources

- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-A-xctrace-cpu-counters.md` lines 424-442 (V3-A §6.5 PMU manifest diagnostic-only status; same-wave-consumer rule binding)
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-B-xctrace-time-profiler.md` lines 1140-1183 (V3-B §0 V4 fold footer + V5 fold cost block + `aggregate.py` reproducibility)
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-C-hot-leaf-attribution.md` (V5 fold §5.3 line 717 hedge; substrate-neutral primitive vocabulary preserved)
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-D-structural-breakdown.md` lines 17-51 (V3-D §0 V4 fold footer + V5 eight-error enumeration; item 8 "hypothesis-sized, not wave-sized")
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-E-legacy-cleanup-audit.md` (V3-E §0/§1/§2/§4.3 E1/E2 dispatch contracts; unchanged by V5)
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-F-redress-reconciliation.md` lines 457-480 (V3-F §4 V5 edit-dispatch hard cap + sequence + revert; §6.6 cohesion risk class preserved)
- `restart/skinny/tranches/sk-v9/research/p1/hardening/V4/CH4.md` (V4 verdict 93.3% strict; five named REVISE gaps V05/V19/V20/V21/V23 + one REVISE-OPTIONAL V22; ACCEPT-WITH-NOTE V24/V25)
- `restart/skinny/tranches/sk-v9/research/p1/hardening/V5/CH4.md` (V5 verdict 100.0% strict; five-gap closure with PRESERVED-by-V6 anchors)
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V5-CONSOLIDATED.md` (V5 verdict table + V6 protocol — CH4 confirmation cycle on unchanged substantive)
- `restart/prompts/ORCHESTRATOR.md` §3W (CH4 contract row), §3Z (≥95% two-consecutive convergence gate + V≤5 hard ceiling), §7 (orchestrator scope), §8 (non-negotiables incl. same-wave consumer + hard cap + no contrivance + no deferrals), §9 (hard caps table)
- `/tmp/skv9-xctrace-v3/aggregate.py` (filesystem-verified V6: 319 LOC, 11188 bytes, mtime 2026-05-18 15:12:25; V23 reproducibility-by-instruction anchor unchanged since V5)
- `/tmp/skv9-xctrace-v3/regression.py` + `/tmp/skv9-xctrace-v3/regression_output.json` (V4 F5 commits; V3-D §0 V5 eight-error enumeration references these; V24 audit-surface anchor)
- Git: V5 fold commit `d76eef63` ("docs(sk-v9-p1-v5): fold V4 CHALLENGE residuals — 6 surgical edits"); V5 CHALLENGE archive `c8233b2c` ("docs(sk-v9-p1-v5-challenge): archive CH1-CH6 + consolidated verdict"); `git log d76eef63..HEAD -- restart/skinny/tranches/sk-v9/research/p1/` shows only `c8233b2c` — no substantive V5→V6 edit.
