# CH6 — ANTI-PAPER-CLOSE — S-P2 Research V4 (second-consecutive confirm)

Pass: S-P2 Research. Cycle: V4 (second-consecutive confirmation of the
V3 ACCEPT state on unchanged-substantive).
Date: 2026-05-18.
Lens: CH6 per `restart/prompts/ORCHESTRATOR.md` §3W + §3Z (two-consecutive
≥95% rule) + §8 non-negotiable "No deferrals — a wave closes on
measurement, not a future-phase promise."
Cohort: P2-A..P2-F (S-P2; V4 fold commit `1eee3375`).
Authority: `restart/prompts/skinny/PASS-2-RESEARCH.md` §CHALLENGE; the
V3 CH6 disposition `V3/CH6.md` (23 VERIFIED / 0 RESIDUAL / 0 REJECT —
100%, first qualifying cycle); the V4 fold spec
`HARDENING-S-P2-V3-CONSOLIDATED.md` §"V4 fold requirement" (one trivial
1-token correctness fix); the in-tree code at
`skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs`,
`skinny/crates/runtime/src/grammars/json/generated.rs`,
`skinny/crates/parse-that-regex/src/lib.rs`; the design corpus at
`restart/ARCHITECTURE.md` §7.3; `skinny/REDRESS.md` entries 28 + 33.

V4 mandate: CH6 V3 was the first qualifying cycle (100%, above the §3Z
95% bar) after V2 fell short (90.6%). CH6 V4 is the second-consecutive
qualifying cycle on unchanged-substantive. Verify three things: (1) the
V4 fold is the 1-token line fix only, with no new "wired / verified /
complete" claims; (2) the V3 CH6 ACCEPT state survives intact (the three
folded V2 residuals stay folded, P2-E's honest PMU downgrade survives);
(3) the V4 1-token fix is itself honest — a genuine correctness
correction, not a paper-close.

## §1 — V3-state preservation

The V3 CH6 disposition closed at 100% (23/23 VERIFIED, 0 RESIDUAL, 0
REJECT). It rested on three resolved V2 residuals plus two CH6-adjacent
edits. Each is re-probed below against the *current* (V4-folded) report
state. The V4 fold (`git show --stat 1eee3375`) touched **one file** —
`skv9-p2-D-aarch64-asm-opportunities.md`, +2/−2 lines — so the V3
substantive surface is by construction unchanged. The probes confirm it.

**V3-residual 1 — EOR3 latency cite (V2-D-8 → V3 fold #4).** P2-D
§5.3.1 (read directly, lines 810-825) carries: "PMULL.1Q is 4-cycle
latency 1/cycle throughput; EOR3 is 1-cycle latency … (the EOR3/PMULL
latency profile is per ARM DDI 0487 FEAT_SHA3 / FEAT_PMULL instruction
descriptions; M5 Max P-core specifics are unpublished by Apple — treat
the absolute cycle counts as a host-capability-gated estimate, the
monotonic *ordering* EOR3 < PMULL is the load-bearing claim)." The
architecture-manual cite, the Apple-unpublished disclaimer, and the
demotion of the load-bearing claim to the monotonic ordering all
survive. PRESERVED. (The V3/CH6.md quotation read "M5 Max P-core retire
latency is unpublished"; the in-tree wording is "M5 Max P-core
specifics are unpublished by Apple" — same honest scoping, no
substantive drift; V3's paraphrase, not a regression.)

**V3-residual 2 — ContainerNext + CollapsedStage code cites (V2-F-5 →
V3 fold #5).** Re-read the tree: `generated.rs:341` is `enum
ContainerNext {`; `:134-135` is the arm pair `ContainerNext::Next(byte)
=> dispatch_value(…)` / `ContainerNext::Done => return Ok(())`;
`consume_array_next` begins at `:348`. `ARCHITECTURE.md:1086` is the
literal token `CollapsedStage,`. P2-F §2.1 (lines 86-90) and §5.4
(lines 384-387) carry these cites verbatim. All four anchors resolve.
PRESERVED.

**V3-residual 3 — P2-D §6.3 wording (§4-item-4 → V3 fold #6).** P2-D
§6.3 (read directly, lines 1049-1063) carries: "The deferral here is
strictly of the *broader host-instrumentation infrastructure* —
invariants 2-5 … The *per-primitive checkasm tests* are **not**
deferred: per §6.2.1 each missing differential is a same-wave admission
precondition … Deferring the invariant 2-5 infrastructure does **not**
block §3/§4 admission." The infrastructure-vs-per-primitive distinction
is intact; the "not block" claim remains scoped to the invariant-2-5
infrastructure. PRESERVED.

**CH6-adjacent — REDRESS 28/33 line ranges.** `grep -n` on
`skinny/REDRESS.md`: entry `28.` at line 324 ("SK-V3 Wave 0/1 closed
SIMD parity and admitted the host aarch64 primitive"); entry `33.` at
line 394 ("SK-V5 Wave 3: Class A `match_tiny_plain_string` NEON wiring
is INVALIDATED"). The P2-D `:324-337` / `:394-418` ranges enclose the
real entry bodies. PRESERVED.

**P2-E honest PMU downgrade.** The V4 fold did not touch P2-E (commit
stat shows P2-D only). Re-confirmed in-tree: P2-E §6.4 / §0 carry
"unicode_escapes PASS 100.5% → **NEAR-FAIL 94.5%**" (line 940), the
y_string_unicode NEAR-FAIL at 94.8%, unicode_mixed FAIL 63.7%, and the
`/tmp/skv9-xctrace-v3/pmu_rows.tsv` baseline cite (§6.1, line 551). The
strongest anti-paper-close artefact in the cohort — a V1 PASS
surrendered to a NEAR-FAIL after the fabricated c/B column was rejected
— survives V4 entirely intact. PRESERVED.

The full V3 ACCEPT state is preserved. No folded residual reopened, no
downgrade re-inflated.

## §2 — V4 dispositions

The V4 fold is `git show 1eee3375`: commit
`docs(sk-v9-p2-v4): fold V3 CHALLENGE — match_tiny_plain_string line
fix`, **one file, +2/−2**. Both changed lines are in P2-D: §3.6 prose
(line 490) and §8 sources (line 1117), each `:79` → `:81`. Re-probed
below against the three-part predicate (citation resolves / derivation
grounded / claim honest). **VERIFIED-FIX** = the 1-token correction is
genuine; **VERIFIED** = a V3 disposition holds unchanged; **RESIDUAL**
= an open item not in the V4 mandate.

| # | Claim | Verdict |
|---:|---|---|
| V4-1 | The V4 fold touched exactly one file (`skv9-p2-D…md`), +2/−2 — no code file, no other report. | **VERIFIED** — `git show --stat 1eee3375`: 1 file changed, 2 insertions, 2 deletions. The diff body is two `:79`→`:81` token edits. |
| V4-2 | P2-D §3.6 (line 490) now cites `match_tiny_plain_string.rs:81` for the low-6 TBL shape. | **VERIFIED-FIX** — read line 490: "(the Class A `match_tiny_plain_string` shape per `match_tiny_plain_string.rs:81`)". |
| V4-3 | P2-D §8 sources (line 1117) now cites `match_tiny_plain_string.rs:81` with the clarifying note "line 79 is the `#[cfg]` attribute". | **VERIFIED-FIX** — read line 1117: "`…match_tiny_plain_string.rs:81` (`match_tiny_plain_string_neon` — low-6 TBL shape referenced in §3.6; line 79 is the `#[cfg]` attribute)." |
| V4-4 | The line-81 citation is correct against the working tree. | **VERIFIED-FIX** — `sed -n '70,90p'` on `match_tiny_plain_string.rs`: line 79 is `#[cfg(target_arch = "aarch64")]`, line 80 is `#[inline]`, line 81 is `pub unsafe fn match_tiny_plain_string_neon(`. The function declaration is at :81. The fix is a genuine correctness correction. |
| V4-5 | The §8 clarifying note ("line 79 is the `#[cfg]` attribute") is itself honest — line 79 actually is the `#[cfg]` attribute. | **VERIFIED-FIX** — confirmed: line 79 is exactly `#[cfg(target_arch = "aarch64")]`. The note adds precision, not a claim. |
| V4-6 | No new "wired / verified / complete / admitted" claim was introduced by the V4 fold. | **VERIFIED** — the diff body changes only a line number and adds a parenthetical that *describes* line 79; it asserts nothing about wiring, admission, or completeness. The §3.6 sentence still reads "but that's a Wave 2+ optimisation" — the deferral posture is unchanged. |
| V4-7 | The V4 fold did not silently broaden the §3.6 claim. The sentence still scopes the low-6 TBL fold as "a Wave 2+ optimisation; the Wave 1 admission is the existing x4 NEON body." | **VERIFIED** — read line 489-491: the Wave-2+ scoping is verbatim from V3. The token fix did not retitle the deferral. |
| V4-8 | V3-residual 1 (EOR3 latency cite) survives the V4 fold. | **VERIFIED** — P2-D §5.3.1 untouched by the V4 diff; the ARM DDI 0487 cite + Apple-unpublished disclaimer + monotonic-ordering demotion all read in-tree. |
| V4-9 | V3-residual 2 (ContainerNext `generated.rs:341` + CollapsedStage `ARCHITECTURE.md:1086`) survives. | **VERIFIED** — P2-F untouched by the V4 diff; all four anchors re-read against the tree resolve verbatim (§1 above). |
| V4-10 | V3-residual 3 (P2-D §6.3 infrastructure-vs-per-primitive rework) survives. | **VERIFIED** — §6.3 lines 1049-1063 untouched by the V4 diff; the per-primitive checkasm carve-out remains explicit prose. |
| V4-11 | P2-D §6.2.1 same-wave checkasm precondition table survives — the `checkasm_match_tiny_plain_string.rs` row still assigns the standalone differential to the §3 codec-broadening wave. | **VERIFIED** — §6.2.1 (lines 1035) carries the `checkasm_match_tiny_plain_string.rs` row "**§3 codec-broadening wave** (co-located) … authors the standalone test in the same dispatch." The V4 line fix sits in §8, not §6.2.1; the table is intact. The `match_tiny_plain_string` test is gated same-wave, not deferred. |
| V4-12 | P2-D §2.1 `unescape_uxxxx_x4_neon` wiring cite `lib.rs:402` survives. | **VERIFIED** — `sed -n '402p'` on `parse-that-regex/src/lib.rs` carries `unescape_uxxxx_x4_neon(&packed)`. The D-1 fold is untouched. |
| V4-13 | P2-D §5.3.1 EOR3 six-row no-regression maintain gate (canada, citm_catalog, instruments, marine_ik, mesh, numbers) survives as a hard blocking precondition. | **VERIFIED** — §5.3.1 lines 856-866 carry the six-row gate "as a hard blocking precondition … no EOR3 body ships unless those six rows hold." Not a soft target; not a paper-close. |
| V4-14 | P2-A, P2-B, P2-C carry their V2/V3-folded text verbatim — not touched by V3 or V4. | **VERIFIED** — V3 commit touched only P2-D + P2-F; V4 commit touched only P2-D. P2-A/B/C are two cycles unchanged; their V2 CH6 dispositions (all VERIFIED, zero residual) stand. |
| V4-15 | P2-E §6.4 honest downgrade (unicode_escapes PASS 100.5% → NEAR-FAIL 94.5%) survives — the cohort's exemplary anti-paper-close artefact. | **VERIFIED** — P2-E untouched by V3 and V4; §0 line 940 + §6.4 lines 671-698 carry the downgrade. PMU-rederived NEAR-FAIL verdicts intact. |

## §3 — Aggregate verdict

V4 dispositions: **15 probed** — 5 directly on the V4 1-token fold
(V4-1..5, including the in-tree line-81 verification), 2 on the
no-new-claim / no-broadening predicate (V4-6/7), 8 V3 carry-forward
re-probes across all six reports (V4-8..15).

| Verdict | Count | % |
|---|---:|---:|
| VERIFIED / VERIFIED-FIX | 15 | 100% |
| RESIDUAL | 0 | 0% |
| New REJECT | 0 | 0% |

**Per-report:**

| Report | Probed | Verified | Residual | Verdict |
|---|---:|---:|---:|---|
| P2-A union event-model | 1 | 1 | 0 | CONVERGE (V2/V3-stable; not V4-touched) |
| P2-B retained grammar proof | 1 | 1 | 0 | CONVERGE (V2/V3-stable; not V4-touched) |
| P2-C apache + citm admission | 1 | 1 | 0 | CONVERGE (V2/V3-stable; not V4-touched) |
| P2-D aarch64 asm opportunities | 9 | 9 | 0 | CONVERGE (1-token fix verified; 3 V3 residuals + §6.2.1 + six-row gate all hold) |
| P2-E unicode-escape codec | 1 | 1 | 0 | CONVERGE (PMU downgrade discipline intact) |
| P2-F SOTA teardown | 2 | 2 | 0 | CONVERGE (ContainerNext + CollapsedStage cites resolve) |

**CH6 V4 ACCEPT rate: 100% verified, 0 RESIDUAL, 0 REJECT.** The lens
clears the §3Z 95% convergence bar.

**Two-consecutive-cycle status.** CH6 V3 = 100% (first qualifying
cycle). CH6 V4 = 100% (second qualifying cycle). The V4 fold was the
smallest possible — one file, two tokens, a line-number correction with
no substantive surface — and the V3 ACCEPT state is preserved in full.
**CH6 satisfies the §3Z two-consecutive ≥95% rule. CH6 CONVERGES.**

## §4 — Any new paper-close from the V4 fold

I probed the V4 fold for a new paper-close: a citation pointed at a
wrong/non-load-bearing anchor, a claim broadened under cover of a
"correction", a deferral relabelled, a downgrade re-inflated.

**No new paper-close found.** Specifically:

1. **The 1-token fix is a genuine correctness correction.** P2-D
   previously cited `match_tiny_plain_string_neon` at line 79; the tree
   shows line 79 is `#[cfg(target_arch = "aarch64")]` and the function
   is declared at line 81. The fix moves the citation from the
   attribute line to the function line — the opposite of a paper-close,
   which would leave a citation pointing at the wrong anchor. CH1 V3
   surfaced this; the V4 fold closed it. The fix is honest.

2. **The fold added no new claim.** The diff is a line number plus a
   describing parenthetical ("line 79 is the `#[cfg]` attribute"). The
   parenthetical is itself true against the tree. No "wired", no
   "verified", no "admitted", no "complete" — the §3.6 sentence keeps
   its "Wave 2+ optimisation" deferral verbatim. A paper-close fold
   would have used the edit window to inflate the claim; this one did
   not.

3. **The V3 ACCEPT state is fully preserved.** The three folded V2
   residuals (EOR3 latency cite, ContainerNext/CollapsedStage cites,
   §6.3 infrastructure-vs-per-primitive rework) all re-read in-tree
   unchanged. None reopened.

4. **P2-E's honest downgrade survives.** The cohort's strongest
   anti-paper-close artefact — the unicode_escapes PASS 100.5% →
   NEAR-FAIL 94.5% surrender after the fabricated c/B column was
   rejected — was not in the V4 diff and survives intact. No verdict
   was re-inflated under cover of the fold.

5. **The deferral discipline holds.** P2-D §6.2.1 still gates
   `checkasm_match_tiny_plain_string.rs` to the §3 codec-broadening
   wave as a same-wave precondition (not a follow-up); the §6.3 prose
   still scopes "not block" to the invariant-2-5 host-instrumentation
   infrastructure only; the §5.3.1 EOR3 six-row no-regression gate is
   still a "hard blocking precondition". No deferral was loosened by
   the V4 fold.

The V4 fold is clean. The 1-token fix is an honest correctness
correction, no new claim was introduced, the V3 ACCEPT state is
preserved in full, and P2-E's exemplary downgrade survives. **Zero new
paper-close, zero RESIDUAL, zero REJECT. CH6 V4 clears the 95% bar at
100% verified — the second-consecutive qualifying cycle. CH6
CONVERGES.**

---

End of CH6 V4.
