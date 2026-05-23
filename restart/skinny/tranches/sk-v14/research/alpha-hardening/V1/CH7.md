# CH7 Overfit-Prune — Pass Alpha V1 Disposition

Lens binding: `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`. The
five CH7 criteria (CH7-1 grammar-derived only; CH7-2 Lock 14 generic-crate
compliance; CH7-3 real source change + strict-vs-strict + per-iter
equality; CH7-4 round-trip on generated output; CH7-5 no scaffold admit)
are the disposition spine. CH7 REJECT carries plan-revise / redress-revert
weight per the lens definition's final paragraph.

## §0 — Disposition summary

- Artefacts reviewed: 8 (SYNTHESIS, HANDOFF, α-A, α-B, α-C, α-D, α-E,
  DISPATCH-CONTEXT).
- Per-section dispositions issued: 36.
- ACCEPT: 30.
- REVISE: 5.
- REJECT: 1.
- ACCEPT-rate: 30 / 36 = 83.3 %.
- Critical findings: 2.
- Escalation flag: **YES — see §2.1.** CH7 REJECT on C-3's round-trip gate
  per CH7-4 binding triggers plan-revise. The other findings are REVISE
  and survive into V2 fold.

## §1 — Per-artefact disposition table

| Artefact | § | Disposition | Reason |
|---|---|---|---|
| SYNTHESIS.md | §0.1 Close condition | ACCEPT | "strict-vs-strict on the same plane, same corpus, same equality semantics" (`SYNTHESIS.md:40-43`) wires CH7-3 into the close condition. |
| SYNTHESIS.md | §0.2 Goalset row enumeration | ACCEPT | Honest audit-corrected delta states 0 / 51 + 0 / 24 (`SYNTHESIS.md:54-60`); CH7-3 measurement integrity inherited. |
| SYNTHESIS.md | §0.3 R-target goalset (R1-R10) | ACCEPT | R1+R2 instantiate CH7-3 (`SYNTHESIS.md:80-81`); R4 instantiates CH7-4 round-trip (`SYNTHESIS.md:83`); R3 PRUNE-5 instantiates CH7-5 scaffold-to-load-bearing (`SYNTHESIS.md:82`); R3 PRUNE-3+4 instantiate CH7-1/CH7-2 (`SYNTHESIS.md:82`). |
| SYNTHESIS.md | §0.4 Pre-blocks P-1..P-7 | ACCEPT | P-1↔CH7-1, P-2↔CH7-3, P-3↔CH7-3, P-4↔CH7-3, P-5↔CH7-5, P-6↔CH7-2 mapping holds; P-7 cross-binds CH5. Hand-patching ban citing `[clean-regen-discipline]` (`SYNTHESIS.md:100-102`) enforces CH7-1 + CH7-4 together. |
| SYNTHESIS.md | §0.5 Wave-by-wave gates deferred | ACCEPT | Contracted §4.4 deferral to S-P3 per PASS-ALPHA §4 (`SYNTHESIS.md:136-145`); explicitly binds S-P3 to consume §0.4 pre-blocks — CH7 carries through. |
| SYNTHESIS.md | §1 Corrected diagnosis | ACCEPT | Eight survives + four falsifications align with CH7-derived audit verdicts (`SYNTHESIS.md:153-180`). |
| SYNTHESIS.md | §2 Telemetry binding | ACCEPT | `comparator_plane` + `per_iter_equality` + `audit_overlay_verdict` columns are R1/R2/audit-overlay-gated and rejected by `xtask gate-json` when empty (`SYNTHESIS.md:211-225`) — CH7-3 enforced through the harness layer. |
| SYNTHESIS.md | §3 Candidate shortlist (C-1..C-5) | REVISE | Table-row gates for C-3 + C-4 are accurate in spirit but compress the CH7 surface: C-3's gate states "round-trip xtask check returns clean" without naming the byte-equivalent diff target; C-4's gate states "measurable runtime divergence on a named pre-wave row" without naming the row in this table. Acceptable because §6 of α-E names both. REVISE: lift α-E §5 "delete generated → cargo xtask regen-css → diff empty" verbatim into `SYNTHESIS.md:243`; lift α-E §6 named row "`json/numbers/direct_to_struct/main`" into `SYNTHESIS.md:244`. |
| SYNTHESIS.md | §4 S-P3 constraints | ACCEPT | The constraint set inherits CH7 fully: weaker-scoping label ban (`SYNTHESIS.md:268-271`) closes CH7-5; Lock 14 binding (`SYNTHESIS.md:272-275`) closes CH7-2; `G-SIMD-GRAMMAR-POLICY` clause (`SYNTHESIS.md:276-281`) closes the SIMD substrate-union hole CH7-2 cross-references; decision-engine fail-closed (`SYNTHESIS.md:282-284`) closes CH7-5 for the resolver. The `audit_overlay_verdict` gate (`SYNTHESIS.md:285-287`) is the CH7 cross-tranche carry. |
| SYNTHESIS.md | §5 Pre-blocked + unblocked routes | ACCEPT | Carries P-1..P-7 verbatim (`SYNTHESIS.md:309-312`); enumerated unblocks remain compatible with CH7 criteria. |
| SYNTHESIS.md | §6 Close posture | ACCEPT | Restates the indefatigable close per R10; no CH7 leakage. |
| HANDOFF.md | §1 Bracket verdict | ACCEPT | "0 / 43 admitted rows survive strict-vs-strict audit" (`HANDOFF.md:14-15`) frames CH7 inheritance. |
| HANDOFF.md | §2 Authority list | ACCEPT | Lock 14 + CH7 lens binding cited (`HANDOFF.md:38`). |
| HANDOFF.md | §3 Honest baseline | ACCEPT | Survives / does-not-survive split matches audit pack; CH7 audit overlay preserved. |
| HANDOFF.md | §4 Pre-S-P0 readiness | ACCEPT | Documents the V1 + V2 CHALLENGE reconciliation path (`HANDOFF.md:112-113`); does not paper-close. |
| HANDOFF.md | §5 Pass sequence | ACCEPT | Step 4 wires S-P0 with CH7 lens binding (`HANDOFF.md:128-129`); convergence gating intact. |
| HANDOFF.md | §6 Next-move | ACCEPT | CHALLENGE V1 → G-Alpha → S-P0 chain holds; no skipped binding gate. |
| HANDOFF.md | §7 Refusal conditions | ACCEPT | Enumerates P-1..P-7 verbatim (`HANDOFF.md:200-204`); CH7 recurrence-vector coverage complete; weaker-scoping label refusal (`HANDOFF.md:181-184`) directly enforces CH7-5. |
| HANDOFF.md | §8 V1 disposition | ACCEPT | PENDING until CHALLENGE V1 returns; correctly contracts G-Alpha gate. |
| alpha-A.md | §1 parse_only plane | ACCEPT | All 5 W14 admits marked AUDIT-FALSIFIED with v2 §1-4 + v6 §3 citations (`alpha-A.md:46-56`); CH7-3 audit overlay applied per row. |
| alpha-A.md | §2 direct plane | ACCEPT | 4 admits AUDIT-FALSIFIED with REAL-parser + comparator-misbinding overlay (`alpha-A.md:81-94`); CH7-3 distinguishes scaffold from parser-real-but-comparator-misbound — correct application. |
| alpha-A.md | §3 typed plane (per §3 read) | ACCEPT | Same overlay shape, 7 admits flipped; carries audit pack citations. |
| alpha-A.md | §4 CSS L4 (per §4 read) | ACCEPT | All 24 CSS rows + SK-V12 W1b row marked AUDIT-FALSIFIED per v1 §1-5 with template + fake-`@generated` cite (`alpha-D.md:182-216` enumerates rows; alpha-A.md mirrors). CH7-1 + CH7-4 audit applied. |
| alpha-B.md | (competitor deltas, general) | ACCEPT | Out of direct CH7 scope; competitor-side comparator analysis informs CH7-3 (strict-vs-strict plane) but does not author intervention. |
| alpha-C.md | §1 per-entry dispositions | ACCEPT | 5 CSS REDRESS entries + 5 decision-engine entries + 4 JSON direct entries correctly mapped to DEMOTE-AUDIT / SCAFFOLD / REOPEN-AUDIT per audit overlay (`alpha-C.md:50-90`). CH7-5 enforced on W8 / W9. |
| alpha-C.md | §2 pre-blocks P-1..P-7 | ACCEPT | Pattern-level pre-blocks correctly authored; CH7 binding inherited. |
| alpha-D.md | §2 VALIDATED set | ACCEPT | V-6 + V-7 distinguish real `@generated` header (round-trip preserved per v2 §3.1 + §5.1 cite, `alpha-D.md:127-139`) from CSS bypass header (`alpha-D.md:175-200`); this is the CH7-1 + CH7-4 distinction made cleanly. V-8 flags 15 CSS `.bbnf` grammars as load-bearing-via-R4 (`alpha-D.md:152-167`). |
| alpha-D.md | §3 INVALIDATED set | ACCEPT | I-1 cites the bypass-header pattern per v1 §1 Claim 1 CONFIRM (`alpha-D.md:184-200`); CH7-1 violation diagnosed and the deletion ledger built. |
| alpha-D.md | §4 DEMOTED set | ACCEPT | W8 / W9 SCAFFOLD-ONLY verdict cites v4 §4/§5/§6 (per excerpted notes); CH7-5 binding intact for the reopen path. |
| alpha-D.md | §5 STILL-OPEN cohorts | ACCEPT | S-3 names the 30 Lock 14 violations as PRUNE-3+4 obligation (`alpha-D.md:486-503`); S-5 names the R2 per-iter oracle gap (`alpha-D.md:511-515`); CH7-2 + CH7-3 covered. |
| alpha-D.md | §6 Net ledger | ACCEPT | Audit-zero rolling delta restated as the SK-V14 open baseline (`alpha-D.md:534-544`). |
| alpha-E.md | §1 Why prune-first | ACCEPT | Reasons-from-first-principles narrative for prune-first; CH7 grounding ("no re-admit candidate can carry a measurable falsifiability gate" per `alpha-E.md:54-55`) is exact CH7-3 framing. |
| alpha-E.md | §2 shortlist table | REVISE | C-3 falsifiability gate (`alpha-E.md:85`) cites round-trip but the C-3 §5 expansion (`alpha-E.md:332-336`) is where the exact `rm -rf … && cargo xtask regen-css && git diff` command lives. The table is the truth-bearing summary; lift the explicit command into the table-row or add a "see §N" pointer. Same for C-4's named pre-wave row. |
| alpha-E.md | §3 C-1 (Lock-14 cluster) | ACCEPT | CH7-1 + CH7-2 + CH7-4 all named: regen-derived runtime per sub-wave (`alpha-E.md:145-151`), round-trip diff per sub-wave gate (`alpha-E.md:148-150`), Lock 14 baseline gate at every sub-wave commit (`alpha-E.md:165-167`). The sub-wave structure is the CH7-1 enforcement mechanism for the 64-file Pattern H collapse. |
| alpha-E.md | §4 C-2 (comparator + per-iter equality) | ACCEPT | Three plane-correct strict comparators named (`alpha-E.md:198-208`); per-iter equality inside timing region (`alpha-E.md:210-211`); `xtask gate-json` rejection of empty equality column (`alpha-E.md:247-248`). CH7-3 wired end-to-end at the harness layer. Sonic-rs Skipper fallback escalation path (`alpha-E.md:259-267`) is honest about API-surface risk. |
| alpha-E.md | §5 C-3 (regen-css + corpora) | **REJECT** | See §2.1 below. CH7-4 round-trip gate is present in the §5 expansion (`alpha-E.md:332-336`), BUT the gate's regen-then-diff target excludes one critical CH7-1 ledger: the gate verifies `runtime/src/grammars/css_l4_*` byte-equivalence, but the audit pack identifies hand-patched output in BOTH `crates/core/src/runtime/{grammar}/` (Pattern H, per `alpha-D.md:493-495`) AND `skinny/crates/runtime/src/grammars/css_l4_*/` (per `alpha-D.md:188-189`). The C-3 round-trip gate only covers the skinny-side path. Hand-patching in the `crates/core` Pattern H tree would pass the C-3 gate while violating CH7-1. REJECT with explicit redress in §3.1. |
| alpha-E.md | §6 C-4 (W8 + W9 scaffold → load-bearing) | ACCEPT | Named pre-wave row `json/numbers/direct_to_struct/main` (`alpha-E.md:439-444`); hot-leaf attribution change is the falsifier (`alpha-E.md:439-446`); regression > 1% on any SURVIVES row rejects the wave (`alpha-E.md:464-465`). CH7-5 wired with a falsifiable measurement. |
| alpha-E.md | §7 C-5 (clean revert) | ACCEPT | 29 REDRESS entries enumerated per row (`alpha-E.md:497-507`); P-1 + P-4 pre-blocks repeated as forward-binding constraints (`alpha-E.md:561-564`). CH7-1 audit-trail restoration. |
| alpha-E.md | §8 pre-blocks (consolidated) | ACCEPT | P-1..P-7 carried through with per-candidate binding. CH7 binding inherited candidate-side. |
| alpha-E.md | §9 concurrency matrix | ACCEPT | Single-writer ledger discipline for RESULTS / ROLLING-SOTA-DELTA / REDRESS (`alpha-E.md:611-613`); CH7-1 audit trail protected from race. |
| alpha-E.md | §10 cost + caps | REVISE | C-1 sub-waves are 45-min redress (correctly amended), but the C-1 redress cap applies per sub-wave × 8 sub-waves. The table reads ambiguously — clarify whether the 45-min cap is per sub-wave (correct per `[dispatch-hard-cap]` decision-engine amendment) or per cluster. CH7 binding is the per-sub-wave round-trip; the cap must match. |
| alpha-E.md | §11 convergence + escalation | ACCEPT | Escalation paths cover CH7 failure modes — including the C-2 no-strict-comparator path (which is exactly the CH7-3 honest-impossibility escalation), C-3 grammar incompatibility (CH7-4 honest-impossibility), C-4 no hot-leaf change (CH7-5 honest-failure). |
| DISPATCH-CONTEXT.md | (full) | ACCEPT | The α-agents' spec carries the CH7 inheritance correctly; not a target of revision in this lens. |

## §2 — Critical findings

### §2.1 — REJECT: C-3 round-trip gate is CH7-1-blind to Pattern H

**Disposition:** REJECT. CH7 binding is the most consequential lens for
SK-V14 per `PASS-0-OVERFIT-AUDIT.md:86-87`; this REJECT triggers
plan-revise on C-3 + downstream C-1 CSS sub-wave gating, per the lens
definition's final paragraph.

**Finding.** C-3's falsifiability gate per `alpha-E.md:332-336` reads:

> Round-trip: `rm -rf skinny/crates/runtime/src/grammars/css_l4_* &&
> cargo xtask regen-css && git diff --
> skinny/crates/runtime/src/grammars/css_l4_*` produces empty output.

The gate is sound for the skinny-side runtime tree. It is silent on the
Pattern H runtime tree under `crates/core/src/runtime/{json, css_l4,
google_sheets, bbnf, csv, ebnf, bnf, math}/` flagged at `alpha-D.md:486-495`
+ enumerated at `alpha-E.md:133-134` as 64 hand-written per-grammar files.

CH7-1 requires that **every** new generated code is grammar-derived with
no hand-written `@generated` header. CH7-4 requires that **every**
"generated" output passes a round-trip test. The Pattern H tree is the
larger of the two tarpits (64 files vs the skinny-side providers); the
C-3 gate as written cannot detect a hand-patched Pattern H file.

The gate is also silent on the audit-confirmed CSS bypass header pattern
(`alpha-D.md:185-200` cites the `// @generated by skinny bbnf-codegen; do
not edit by hand.` header rendered into hand-curated content). A
post-PRUNE C-3 wave that emits the same header into hand-touched output
would pass the round-trip gate trivially, because the round-trip is
delete-then-regen — and the regen itself can hand-curate, since the
generator is the thing under construction in C-3.

**Why this is CH7 REJECT, not REVISE.** A gate that does not falsify the
recurrence vector it nominally addresses is not a gate — it is paper. P-1
of `SYNTHESIS.md:96-102` is the entire CSS L4 fake-admit cluster's source
pattern. The corresponding falsifier per the lens definition is CH7-4
round-trip: delete + regen ⇒ byte-equivalent. The C-3 gate as written
exercises this for the skinny-side tree only.

**Cross-reference.** SYNTHESIS §3 candidate-table compresses the gate to
"round-trip xtask check returns clean" (`SYNTHESIS.md:243`) — the
compression hides the scope. The CH7 REJECT therefore propagates to
SYNTHESIS §3 as well, but only the C-3 row.

### §2.2 — Critical ACCEPT: P-1..P-7 mapping onto CH7-N is complete and accurate

The CHALLENGE-CONTEXT §CH-7 prescribes the mapping. Verified verbatim
against `SYNTHESIS.md:96-130`:

- P-1 (`SYNTHESIS.md:96-102`) — fake `@generated` header — ↔ CH7-1
  (grammar-derived only). The pre-block explicitly invokes
  `[clean-regen-discipline]` which is the CH7-1 enforcement memory.
- P-2 (`SYNTHESIS.md:103-107`) — `sonic_rs::from_slice::<Value>`
  mislabelled — ↔ CH7-3 (strict-vs-strict comparator on the same plane).
  R1 names the three plane-correct comparators.
- P-3 (`SYNTHESIS.md:108-112`) — tiny-fixture Mbps inflation — ↔ CH7-3
  partly (admit measurement honesty); R5 names the ≥ 800 KB corpora bar.
- P-4 (`SYNTHESIS.md:113-117`) — gate-relabel as admit — ↔ CH7-3 (real
  source change). The pre-block explicitly requires parser/codegen
  source delta per row.
- P-5 (`SYNTHESIS.md:118-121`) — scaffold-research counted as load-bearing
  — ↔ CH7-5 (no scaffold admit). PRUNE-5 is the wire-to-runtime path.
- P-6 (`SYNTHESIS.md:122-127`) — per-grammar provider modules in generic
  codegen — ↔ CH7-2 (Lock 14 generic-crate compliance). PRUNE-3 is the
  collapse.
- P-7 (`SYNTHESIS.md:128-130`) — Track 1 ≡ Track 2 dishonesty — falls
  through CH7 to CH5 (hidden coupling) as the CHALLENGE-CONTEXT predicts.
  SYNTHESIS §4's `G-SIMD-GRAMMAR-POLICY` and same-tape union variants
  (`SYNTHESIS.md:276-281`) close the substrate hole CH7 cross-binds.

The mapping is bijective on P-1 through P-6 and explicitly cross-bound
on P-7. CH7 carries through SYNTHESIS §4 S-P3 constraints intact.

## §3 — Recommended folds for V2

### §3.1 — V2-DISP-α-E-C3 (REJECT remediation, blocking)

Redispatch α-E with the following amendment to C-3's falsifiability gate.
Lift the gate from `alpha-E.md:332-336` and expand it to cover both
runtime trees + the bypass-header recurrence pattern:

> **Round-trip (skinny tree).** `rm -rf
> skinny/crates/runtime/src/grammars/css_l4_* && cargo xtask regen-css
> && git diff -- skinny/crates/runtime/src/grammars/css_l4_*` produces
> empty output.
>
> **Round-trip (core tree, all 8 grammars).** For each of `{json,
> css_l4, google_sheets, bbnf, csv, ebnf, bnf, math}`: `rm -rf
> crates/core/src/runtime/<grammar>/ && cargo xtask regen-<grammar> &&
> git diff -- crates/core/src/runtime/<grammar>/` produces empty output.
> (C-1's sub-wave structure owns the per-grammar xtask emission;
> C-3's round-trip gate consumes those xtasks for CSS.)
>
> **Bypass-header detector.** Every file matching `git grep -l
> '@generated by skinny bbnf-codegen' -- skinny/crates/runtime
> crates/core/src/runtime` must be the byte-for-byte output of a
> registered xtask emission; the round-trip succeeds on every such
> file. Files asserting the header outside the registered xtask scope
> are CH7-1 violations and reject the wave.

The amendment cross-binds C-3 with C-1's per-grammar sub-wave xtask
emission; the dependency is already declared in `alpha-E.md:371-373`.
Update SYNTHESIS §3 row C-3 (`SYNTHESIS.md:243`) to read "round-trip
xtask check returns clean on both runtime trees + bypass-header detector
empty" rather than the current compressed phrasing.

### §3.2 — V2-DISP-α-E-C3-table (REVISE, non-blocking)

Lift the explicit round-trip command + bypass-header detector from §3.1
into the SYNTHESIS §3 table-row for C-3 (or add a "see α-E §5 +
hardening V1 CH7 §3.1" pointer in the gate cell). The table is the
truth-bearing summary consumed by S-P3 — compressing the gate to a
phrase hides the recurrence-vector binding. Same patch for C-4's named
pre-wave row (`json/numbers/direct_to_struct/main`) in SYNTHESIS §3 row
C-4 (`SYNTHESIS.md:244`).

### §3.3 — V2-DISP-α-E-cost-table (REVISE, non-blocking)

Clarify the C-1 redress cap row in `alpha-E.md:622`: state explicitly
that the 45 min is per sub-wave (so the C-1 cluster total is 8 × 45 =
360 min of redress windows, run serialised per the §9 concurrency
matrix), not per cluster. This makes the cap match the
`[dispatch-hard-cap]` decision-engine wave amendment and lets S-P3
budget the bracket correctly.

### §3.4 — V2-DISP-SYNTHESIS-§3-C3-C4 (REVISE, non-blocking)

Apply §3.2 to SYNTHESIS §3 directly: C-3 row gate clarification + C-4
row named-row insertion. SYNTHESIS is the artefact S-P3 consumes
verbatim; the compressed gate phrasing risks losing the CH7-4 binding in
downstream consumption.

### §3.5 — V2-DISP-α-A-cite-spot-check (informational, non-blocking)

α-A's per-row audit-overlay citations are accurate on the spot-check (5
parse_only + 5 direct + 5 typed sampled; every cite resolves to the
named validation §reference). No fold required, but a V2 cross-check of
the remaining 12 + 12 + 12 + 24 = 60 row citations would close CH7-3's
audit-overlay column gate fully. Defer to V2 only if other lenses
surface a CH1 citation-integrity gap; otherwise the spot-check suffices.

## §4 — Bracket-level CH7 verdict

CH7 inheritance is **structurally sound** across SYNTHESIS §0.4 → §4
S-P3 constraints → HANDOFF §7 refusal conditions; the P-1..P-7 ↔ CH7-N
mapping is complete and accurate; six of the seven candidate-side gates
are CH7-falsifiable as written.

One gate (C-3 round-trip per CH7-4) is **insufficient** as authored —
its scope excludes the Pattern H runtime tree the audit pack named as
the larger recurrence vector. This is the CH7 REJECT and the V2 fold
above resolves it. The remediation is narrow (extend the gate scope; add
a bypass-header detector) and does not require re-authoring C-3 or
C-1; it tightens the gate to match the lens binding.

The aggregator should advance V1 → V2 with the §3 folds applied to
α-E + SYNTHESIS § 3 in V2, and re-run CH7 on the V2 cycle. Per
`ORCHESTRATOR.md §3Z`, convergence requires ≥ 95 % ACCEPT on two
consecutive cycles; V1's 83.3 % ACCEPT-rate places the bracket
short of the threshold by one cycle even before the REJECT-triggered
revise lands. The REJECT specifically forecloses V1 convergence per the
CH7 binding's final paragraph.
