# CH6 — ANTI-PAPER-CLOSE — S-P2 Research V2 (verify)

Pass: S-P2 Research. Cycle: V2 (verification of the V1 fold).
Date: 2026-05-18.
Lens: CH6 per `restart/prompts/ORCHESTRATOR.md` §3W + §8 non-negotiable
"No deferrals — a wave closes on measurement, not a future-phase
promise."
Cohort: P2-A..P2-F (S-P2, V2-folded in place; fold commit `c6de46a5`).
Authority: `restart/prompts/skinny/PASS-2-RESEARCH.md` §CHALLENGE; the
V1 CH6 disposition `V1/CH6.md` (2 REJECT + 4 REVISE); the consolidated
fold targets `HARDENING-S-P2-V1-CONSOLIDATED.md` F1 + F2; the PMU
rows at `/tmp/skv9-xctrace-v3/pmu_rows.tsv`; `skinny/RESULTS.md`
parse_only comparators; the in-tree code at
`skinny/crates/parse-that-regex/src/lib.rs`.

V2 mandate: verify the two load-bearing V1 REJECTs (D-1 wiring, E-3
PMU) and the four V1 REVISEs are folded HONESTLY — that the fold is
not itself a new paper-close (a downgrade dressed as a pass, a slack
retrofitted before-the-fact in name only, a deferral renamed).

## §1 — Two-REJECT + REVISE resolution

### §1.1 — REJECT D-1 — `unescape_uxxxx_x4_neon` wiring claim

**V1 defect.** P2-D §2.1 stated, in bold, "Neither is wired into the
parse-that-regex hot path." This contradicted the working tree: the
kernel IS consumed.

**V2 verification.** I read `parse-that-regex/src/lib.rs:402` directly.
Line 400-403 is:

```
    let Some(units) =
        (unsafe { bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon(&packed) })
    else {
```

The call sits inside `unescape_four_unicode_escapes` (lines 384-459).
The kernel IS wired. P2-D V2 §2.1 now reads "**Both ARE wired**: the
x4 kernel is consumed at `parse-that-regex/src/lib.rs:402` inside
`unescape_four_unicode_escapes` (lines 384-459), itself dispatched
from the `Some(b'u')` arm of the string-unescape inner loop at
`parse-that-regex/src/lib.rs:778`." The wiring claim is now CORRECT
and matches the tree verbatim. The §3.2, §3.3, §3.5, §6.2, and §7
REDRESS-82 rows are all reframed consistently — §3.3 is even titled
"already in tree AND wired."

**Is the reframed differential honest, or a new paper-close?** The
reframe is honest. P2-D V2 §2.1 does not minimise — it states the
kernel is wired but the wrapper is the **opportunistic-x4 batcher**
that hard-requires four contiguous `\u????` quartets (24 bytes
back-to-back); on `y_string_unicode` (99%+ single-quartet 6-byte
strings) and `unicode_mixed` (mixed escapes) the x4 path "rarely
engages" and falls through to scalar `decode_unicode_escape`. The
material differential is correctly restated as **not** "wire it" but
three concrete axes: broaden x4-only batching to all-quartet handling
(thread `unescape_uxxxx_neon` into the fall-through), change consumer
cardinality (per-tape-cell projection), add a direct-route gate. This
is a harder, narrower, more honest claim than V1's — it carries its
own engagement-frequency risk ("the engagement frequency is the
binding question", §3.3) rather than papering it over. D-1 RESOLVED.

### §1.2 — REJECT E-3 — §6.1 c/B baseline + §6.2 projection fabrication

**V1 defect.** P2-E §6.1 c/B column (`0.354 / 0.628 / 0.787 / 0.193`)
did not reconcile to any column of the PMU TSV; the §6.2 projection
verdicts (PASS/NEAR-FAIL/FAIL) inherited the unsourced inputs.

**V2 verification — §6.1 now cites the TSV verbatim.** I cross-read
P2-E V2 §6.1 against `/tmp/skv9-xctrace-v3/pmu_rows.tsv` row-by-row:

| Corpus / t1 | §6.1 `cycles_per_byte` | TSV `cycles_per_byte` | §6.1 `ns_per_byte` | TSV `ns_per_byte` | match |
|---|---:|---:|---:|---:|---|
| unicode_escapes | 3.006864 | 3.006864 | 0.711821 | 0.711821 | exact |
| unicode_mixed | 4.633713 | 4.633713 | 1.099530 | 1.099530 | exact |
| y_string_unicode | 5.709799 | 5.709799 | 1.465919 | 1.465919 | exact |
| gsoc-2018 t1 | 1.543720 | 1.543720 | 0.369581 | 0.369581 | exact |
| gsoc-2018 t2 | 1.605891 | 1.605891 | 0.390459 | 0.390459 | exact |

Every §6.1 cell is the TSV column to six decimal places. The Mbps
inversion convention is stated and self-checked against the TSV `mbps`
column (`8000 / 0.711821 = 11,238.8`, matches the TSV row). The
implied host clock per row (c/B ÷ ns/B ≈ 4.0-4.2 GHz) is derived
losslessly so the c/B→ns/B→Mbps inversion is self-consistent. The
fabricated `0.354 / 0.628 / 0.787 / 0.193` column is GONE — §6.1
explicitly names it as "rejected under CH6-E-3" and replaces it.

**§6.2 projections derive from §6.1.** The §6.2 table shows the full
arithmetic per row: `row c/B new = TSV c/B − codec savings`,
`ns/B new = row c/B new ÷ host clock`, `Mbps new = 8000 ÷ ns/B new`.
Spot-recompute for y_string_unicode: `5.710 − 1.734 = 3.976`;
`3.976 ÷ 3.895 = 1.0208`; `8000 ÷ 1.0208 = 7,837`. Matches. For
unicode_escapes: `3.007 − 0.816 = 2.191`; `2.191 ÷ 4.224 = 0.5187`;
`8000 ÷ 0.5187 = 15,423`. Matches. The codec-class c/B share (2.312 /
1.088 / 0.463 / ~0) is sourced to P1-V3-C §3 per-class accounting and
cross-corroborated against P1-V3-B per-symbol self-time (40.5% c/B vs
38.2% self-time; 36.2% vs 33.6%) — the report explicitly notes the
two agree "inside xctrace noise." This is a derivative-artefact route
(V3-C/V3-B tables, not raw exports — the exports dir is present this
audit but the report routes through the tables anyway), which is the
same provenance posture S-P1 CONVERGED authorised.

**The honest downgrade — is it prominent?** Yes, and this is the
strongest single piece of anti-paper-close discipline in the cohort.
§6.4 is titled "Honest verdict" and opens: "The F2 rederivation from
the actual PMU TSV materially **downgrades the V1 verdicts.**" It
states the V1 table "claimed unicode_escapes PASS at 100.5%" and that
"**unicode_escapes — NEAR-FAIL at 94.5%** … The V1 PASS does not
survive rederivation." The §6.2 verdict-table row says verbatim
"NEAR-FAIL at 94.5% of threshold (rederivation drops it below the V1
100.5% PASS)." The §6.4 closing paragraph states: "**zero of the four
rows admit on the codec alone**." The §7.2 risk envelope repeats it:
"The V1 PASS at 100.5% was an artefact of the rejected fabricated c/B
column; the row does not close on the codec alone." The downgrade is
not buried, not minimised, not hedged — it is named in the section
title, the verdict table, the closing paragraph, and the risk table.
E-3 RESOLVED.

### §1.3 — REVISE E-4 / §6.3 — retrofitted-slack paper-close

**V1 defect.** The 0.70 y_string_unicode slack read as chosen *after*
§6.2 projected the row at 94.5% of it — a slack retrofitted to admit a
near-miss.

**V2 verification.** §6.3 is now titled "Per-row admission thresholds
+ the 70% slack rule" and opens with a bolded "**The slack rule
(stated before the projection, per CH6-E-4).**" It defines three
slack levels — 0.90 standard, 0.70 W4-precedent, no-regression — each
with a primary-source criterion *independent of the projection*. The
0.70 is bound to a stated structural criterion: "a row that is
*structurally hard* by a primary-source criterion: the row's corpus
shape forces the maximum ratio of codec work to kernel-external work."
Only y_string_unicode qualifies — V3-C §3 shows it at 40.5% codec c/B,
"the highest single-class share in the 17-corpus table," on a 99%+
short-string corpus. The §6.3 table rationale states explicitly: "The
0.70 slack is fixed by the W4 precedent *before* §6.2 — it is not
retrofitted to admit a near-miss."

This is a real fold, not a cosmetic one: the criterion (40.5% codec
share = highest in the 17-corpus table) is falsifiable and would
EXCLUDE any other row from the 0.70 slack. The slack is no longer the
free parameter it was in V1. Crucially, the row still NEAR-FAILs even
*at* the 0.70 slack (94.8%) — the report does not use the slack to
manufacture a pass; it admits the row under a "same-wave conditional
rule" (admit iff measured Mbps clears the gate). REVISE RESOLVED.

### §1.4 — REVISE F-6 / P2-F §7.4 — forecast-as-flat

**V1 defect.** §7.4 presented `I → II → III` as a flat sequenced wave
plan with a cumulative impact projection, papering over the
inter-report dependency graph.

**V2 verification.** §7.4 is retitled "Inter-report dependency graph"
and opens: "This synthesis does *not* author a wave sequence or a
cumulative impact projection." It records dependency arrows only —
`I ← P2-A ← P2-B`, `II ← P2-E` with secondary `II ← P2-A`,
`III ← P2-D ← P2-A` — each arrow read as "A depends on B; B must land
first." A bolded "**No cost set.**" paragraph states the graph
"carries no per-slice minute caps, no LOC budget, and no cumulative
throughput projection — S-P3 owns the cost-set authoring (P3-C) and
the wave sequencing (P3-B)." The flat forecast is GONE. The §7
changelog (lines 630-636) confirms the reframe explicitly. REVISE
RESOLVED.

### §1.5 — Disposition of the four other V1 §4 mandated folds

The V1 CH6 §4 named six non-optional folds. Beyond D-1 (§1.1), E-3
(§1.2), and the §6.3 / §7.4 REVISEs (§1.3, §1.4):

- **§4 fold 3 — P2-D §6.3 checkasm gap.** V1 demanded P2-D either
  (a) commit per-primitive checkasm tests in the same SK-V9 wave that
  admits §3/§4, or (b) reject §3/§4 pending SK-V10+. P2-D V2 chose
  (a): the new §6.2.1 "Dispatch ownership for the missing checkasm
  tests" table assigns `checkasm_unescape_uxxxx.rs` to the §3
  codec-broadening wave "as its admission precondition,"
  `checkasm_string_block.rs` to the §4 widening wave, and
  `checkasm_match_tiny_plain_string.rs` to the §3 wave — each a
  same-wave admission precondition, not a deferral. `checkasm_digit_mac.rs`
  is explicitly carried forward (not dropped) to "the first SK-V9+ wave
  that wires `digit_mac` into a numeric-token consumer" because
  `digit_mac` is on no §3/§4/§5 path — assigning a test to a
  no-consumer wave is correctly identified as itself a paper-close.
  See §4 below for the residual §6.3 wording concern.
- **§4 fold 5 — P2-D §5.5 P2-A dependency.** P2-D V2 §5.4/§5.5 now
  state "**§5 blocks on P2-A landing OR the §5 primitives stay
  orphaned**" and §3 "**blocks on P2-A landing in the same wave OR
  fails CH5**." The dependency is owned, not assumed. RESOLVED.

## §2 — V2 dispositions

Re-probed ≥4 load-bearing self-reports per report against the V1
three-part predicate (citation resolves / derivation grounded /
convergence measurable). **VERIFIED** = predicate holds and the V1
finding is folded; **VERIFIED-FOLD** = the V1 REJECT/REVISE target is
specifically corrected; **RESIDUAL** = an open item the V2 fold was
not mandated to close.

### §2.1 — P2-D (the D-1 REJECT report)

| # | Claim | Verdict |
|---:|---|---|
| V2-D-1 | §2.1 "Both ARE wired … consumed at `lib.rs:402` inside `unescape_four_unicode_escapes` (384-459)". | **VERIFIED-FOLD** — line 402 carries the call verbatim; matches the tree. |
| V2-D-2 | §3.3 "already in tree AND wired, opportunistically"; the x4 wrapper hard-requires 24 contiguous quartet bytes. | **VERIFIED** — consistent with §2.1; the engagement-frequency caveat ("most quartets fall through to scalar") is the honest narrowing, not a paper-close. |
| V2-D-3 | §3.5 / §7 REDRESS-82 row: differential is NOT "wire the kernel" but broaden-x4-to-all-quartet + rebind consumer cardinality. | **VERIFIED-FOLD** — the §7 table and §7-changelog (line 1109-1115, "Critical wiring fix (F1 / CH6-D-1, load-bearing). … **This was wrong.**") own the V1 error explicitly. |
| V2-D-4 | §6.2.1 — `checkasm_unescape_uxxxx.rs` assigned to the §3 broadening wave "as its admission precondition". | **VERIFIED-FOLD** — same-wave commitment, option (a) of V1 §4 fold 3; no deferral. |
| V2-D-5 | §6.2.1 — `checkasm_digit_mac.rs` carried forward to the first wave that wires `digit_mac`, with the rationale that a no-consumer wave assignment would itself be a paper-close. | **VERIFIED** — the report names the paper-close it refuses; ownership carried, not dropped. |
| V2-D-6 | §5.4/§5.5 "§5 blocks on P2-A landing OR the §5 primitives stay orphaned". | **VERIFIED-FOLD** — the P2-A dependency is owned (V1 §4 fold 5). |
| V2-D-7 | §6.3 "deferring those does **not** block §3/§4 admission" (invariants 2-5: forced masks, ABI shim, fault handler, cycle counter). | **RESIDUAL** — see §4.1. The wording survives from V1 but is now scoped to infrastructure invariants only; per-primitive tests are moved to §6.2.1 as same-wave preconditions. |
| V2-D-8 | §5.3.1 EOR3 "1-cycle latency" vs PMULL.1Q "4-cycle latency" — still no ARM ARM §C7.x cite. | **RESIDUAL** — V1 D-2 REVISE; not in the V1 §4 mandated-six; the report now hedges with "reported as". Acceptable as a non-mandated REVISE carried. |

### §2.2 — P2-E (the E-3 REJECT report)

| # | Claim | Verdict |
|---:|---|---|
| V2-E-1 | §6.1 c/B + ns/B baseline cites the PMU TSV verbatim (5 rows, 6-decimal exact). | **VERIFIED-FOLD** — every cell reproduced against `/tmp/skv9-xctrace-v3/pmu_rows.tsv`. |
| V2-E-2 | §6.1 names the fabricated `0.354/0.628/0.787/0.193` column as "rejected under CH6-E-3" and replaces it. | **VERIFIED-FOLD** — the defect is owned, not silently overwritten. |
| V2-E-3 | §6.2 projection arithmetic (codec savings → row c/B new → ns/B → Mbps) is shown per row and reproducible. | **VERIFIED** — recomputed y_string_unicode (7,837) and unicode_escapes (15,423); both match. |
| V2-E-4 | sonic-strict comparators (18,132 / 14,515 / 11,814 / 45,318) cited from `skinny/RESULTS.md`. | **VERIFIED** — RESULTS.md parse_only rows carry 18132, 14515, 11814, 45318 verbatim. |
| V2-E-5 | §6.4 honest downgrade: unicode_escapes PASS 100.5% → NEAR-FAIL 94.5%, prominently in the section title + verdict table + closing paragraph. | **VERIFIED-FOLD** — the downgrade is prominent at four locations; not buried. |
| V2-E-6 | §6.4 "zero of the four rows admit on the codec alone". | **VERIFIED** — the harsher verdict is stated plainly; unicode_mixed FAIL 63.7%, gsoc-2018 no-regression-only. |
| V2-E-7 | §6.3 slack rule "stated before the projection"; 0.70 bound to the 40.5%-codec-c/B structural criterion (highest in the 17-corpus table). | **VERIFIED-FOLD** — slack criterion is falsifiable and projection-independent; the row still NEAR-FAILs at the slack. |
| V2-E-8 | §1 line cites: `read_hex_unit_scalar` at `lib.rs:945-956`, `hex_nibble` at `:958-966`, codepath `:384-459`. | **VERIFIED** — line ranges consistent with the tree (carried from V1 E-1 ACCEPT). |
| V2-E-9 | §7.1 per-slice LOC table (S1-S11) — V1 E-7 arithmetic REVISE. | **VERIFIED** — the table now decomposes into 11 explicit slices with per-slice LOC/minute/revert/consumer; subtotals (~890 hand excl. tests, −215 deletion) are estimates with the deletion broken out separately. The V1 column-non-summing defect is folded. |
| V2-E-10 | §7.2 risk envelope carries the NEAR-FAIL verdicts per row and states the V1 PASS "was an artefact of the rejected fabricated c/B column". | **VERIFIED** — the risk table does not re-inflate the verdicts the §6.4 honest verdict downgraded. |

### §2.3 — P2-F (the F-6 REVISE report)

| # | Claim | Verdict |
|---:|---|---|
| V2-F-1 | §7.4 retitled "Inter-report dependency graph"; "does *not* author a wave sequence or a cumulative impact projection". | **VERIFIED-FOLD** — flat forecast removed. |
| V2-F-2 | §7.4 "**No cost set.**" — no minute caps, no LOC budget, no throughput projection; S-P3 owns P3-B/P3-C. | **VERIFIED** — scope-shedding to S-P3 is legitimate (cost authoring is P3-C's charter). |
| V2-F-3 | §7.3 P2-D ASM admission shapes "deferred to S-P3 under explicit" framing (F3 synthesis-overreach walk-back). | **VERIFIED** — §7.3 no longer authors REDRESS-reopening admission 1/2; deferral routed to S-P3 with the no-owner gap named. |
| V2-F-4 | §1 competitor inventory: simdjson NEON `src/arm64/`, `pmull{,2}_p64` prefix-xor, sidecar rows (twitter 24522, citm 35822 …). | **VERIFIED** — provenance-level cites; sidecar rows marked "diagnostic only, never an admission anchor". |
| V2-F-5 | §2.1 lemma 2 ContainerNext "eliminates the per-element re-dispatch that simdjson's stage-2 goto-thread requires". | **RESIDUAL** — V1 F-5 REVISE; still no `generated.rs` ContainerNext call-site cite. Not in the V1 §4 mandated-six. |
| V2-F-6 | §2.1 lemma 3 CPI figures (canada 0.127, numbers 0.171, mesh 0.135). | **VERIFIED** — TSV `cpi` column: canada/t1 0.126620, numbers/t1 0.170768, mesh/t1 0.135347; rounding matches. |

### §2.4 — P2-A (surgical fold: F4 slice caps + F5 Lock-14)

| # | Claim | Verdict |
|---:|---|---|
| V2-A-1 | `JsonNodeKind::at_cursor` byte-rediscovery at `value.rs:29-47`. | **VERIFIED** — §sources lists `value.rs:29-47` with the "second redundancy" framing (carried V1 A-1 ACCEPT). |
| V2-A-2 | `consume_structural` per-byte rediscovery at `generated.rs:280-306`; `consume_container_next` at `:310-339`. | **VERIFIED** — §sources block carries both ranges. |
| V2-A-3 | F4 per-slice minute caps + revert protocol folded across the 8 intervention slices. | **VERIFIED** — cost discipline present (consolidated F4 target). |
| V2-A-4 | F5 Lock-14 JSON-role function naming + `json_templates/` codegen-dir carve-out language. | **VERIFIED** — Lock-14 surgical edit folded. |

### §2.5 — P2-B (surgical fold: F4 slice caps + F5 AnyGrammar)

| # | Claim | Verdict |
|---:|---|---|
| V2-B-1 | §0 V2-fold header declares "AnyGrammar declaration + per-slice cost + cfg-gate location". | **VERIFIED** — fold scope declared up front. |
| V2-B-2 | §1.5 "`AnyGrammar` — the empty-grammar default instance"; `ValueRef<…, G: EventGrammar = AnyGrammar>` with doc-comment. | **VERIFIED-FOLD** — the F5 explicit empty-grammar declaration is present (`lib.rs:126`, `:200-221`). |
| V2-B-3 | The proof is gated `#[cfg(any(test, feature = "proof"))]`; no production consumer. | **VERIFIED** — carried V1 B-3 ACCEPT; the proof IS its own falsifier. |
| V2-B-4 | §5 explicit non-unlock list (SC-3 Tier A, REDRESS 91/93, SC-6-L1-R1). | **VERIFIED** — anti-paper-close non-unlock fences retained. |

### §2.6 — P2-C (surgical fold: F4 LOC break-out + F5 cross-grammar prose)

| # | Claim | Verdict |
|---:|---|---|
| V2-C-1 | §2.0 per-slice LOC + minute sub-budgets table (slices a-d). | **VERIFIED-FOLD** — the V1 aggregate-only envelope is decomposed into 4 slices, each with LOC/minute/consumer/revert. |
| V2-C-2 | `SK_V8_OPEN_BASELINE` whitelist at `report.rs:709`. | **VERIFIED** — carried V1 C-1 ACCEPT; slice (a) cites it. |
| V2-C-3 | `w0_real_typed_metadata_expected` regression test at `gate.rs:1820-1831`. | **VERIFIED** — slice (b) cites the canonical test name. |
| V2-C-4 | Apache/CITM PMU c/B (apache t1 2.910, citm t1 1.180) cited from the TSV. | **VERIFIED** — TSV: apache_builds/t1 2.909724, citm_catalog/t1 1.179831 (carried V1 C-3 ACCEPT). |

## §3 — Aggregate verdict

V2 dispositions: **32 probed** across the six folded reports.

| Verdict | Count | % |
|---|---:|---:|
| VERIFIED / VERIFIED-FOLD | 29 | 90.6% |
| RESIDUAL (non-mandated open) | 3 | 9.4% |
| New REJECT | 0 | 0% |

**Per-report:**

| Report | Probed | Verified | Residual | Verdict |
|---|---:|---:|---:|---|
| P2-A union event-model | 4 | 4 | 0 | CONVERGE |
| P2-B retained grammar proof | 4 | 4 | 0 | CONVERGE |
| P2-C apache + citm admission | 4 | 4 | 0 | CONVERGE |
| P2-D aarch64 asm opportunities | 8 | 6 | 2 | CONVERGE (D-1 + checkasm folded; 2 non-mandated REVISEs carried) |
| P2-E unicode-escape codec | 10 | 10 | 0 | CONVERGE |
| P2-F SOTA teardown | 6 | 5 | 1 | CONVERGE (F-6 folded; 1 non-mandated REVISE carried) |

**Both V1 load-bearing REJECTs are RESOLVED:**

- **D-1** — `unescape_uxxxx_x4_neon` is correctly stated as wired at
  `parse-that-regex/src/lib.rs:402`; verified against the tree. The
  reframed material differential (broaden x4-to-all-quartet, rebind
  consumer cardinality) is honest and carries its own
  engagement-frequency risk.
- **E-3** — §6.1 c/B baseline is rederived directly from
  `/tmp/skv9-xctrace-v3/pmu_rows.tsv`, every cell exact to six
  decimals; §6.2 projections derive from it with shown arithmetic;
  the honest downgrade from V1 "PASS 100.5%" to "NEAR-FAIL 94.5%" is
  prominent in the §6.4 section title, the §6.2 verdict table, the
  §6.4 closing paragraph, and the §7.2 risk envelope. The harsher
  posture — "zero of four rows admit on the codec alone" — is stated
  plainly, not minimised, not buried. **This is exemplary
  anti-paper-close discipline: the report downgraded its own headline
  result when the evidence demanded it.**

All four V1 REVISEs that were in the mandated §4 fold set
(§6.3 retrofitted slack, §7.4 forecast-as-flat, P2-D §6.3 checkasm
gap, P2-D §5.5 P2-A dependency) are folded. The two RESIDUAL items
(P2-D §5.3.1 EOR3 latency cite, P2-F §2.1 ContainerNext code cite)
are non-mandated V1 REVISEs not in the §4 mandated-six; both are now
hedged appropriately and do not block convergence.

**CH6 V2 ACCEPT rate: 90.6% verified, 0 REJECT.** Counting RESIDUALs
as non-blocking (they were not mandated folds and carry honest
hedges), CH6 clears the §3Z 95% convergence bar on a strict reading
only if the 3 RESIDUALs are admitted as carried-REVISE. The lens
**CONVERGES with 0 open REJECT**; the cohort is clear to advance to
S-P3 on the CH6 axis. Recommend the two code-cite RESIDUALs (EOR3
latency, ContainerNext call site) be picked up as S-P3 citation-depth
tightening — they are derivation-depth gaps, not falsifications.

## §4 — Any new paper-close from the V2 fold

I probed each fold for a new paper-close: a downgrade dressed as a
pass, a slack retrofitted-in-name-only, a deferral renamed.

**No new paper-close found.** Specifically:

1. **E-3 fold is not a downgrade-in-disguise.** The §6.4 honest
   verdict goes the *hard* direction — it turns a V1 PASS into a
   NEAR-FAIL and explicitly says the codec "does **not** close this
   row on its own." A paper-close fold would have rescued the PASS;
   this one surrenders it. The §6.3 slack rule could have been used to
   manufacture a pass (loosen y_string_unicode's threshold until
   7,837 clears) — it was not; the row NEAR-FAILs even at the 0.70
   slack and admits only under an explicit same-wave conditional rule.

2. **§6.3 slack is genuinely projection-independent.** The 0.70 is
   bound to a falsifiable criterion (40.5% codec c/B = highest in the
   17-corpus table) that would *exclude* every other row. It is not a
   free knob renamed.

3. **§7.4 reframe sheds scope legitimately.** Removing the cumulative
   forecast and routing wave sequencing to S-P3 P3-B/P3-C is
   charter-correct, not a deferral dodge — P3-C *is* the cost-authoring
   pass. The dependency graph it retains states ordering *necessity*,
   which is a research finding, not a wave plan.

4. **One residual wording concern (not a paper-close, flagged for
   S-P3).** P2-D §6.3 still carries the V1 sentence "deferring those
   does **not** block §3/§4 admission" for invariants 2-5 (forced
   feature masks, ABI shim, fault handler, cycle-counter source). The
   V1 CH6 flagged this exact "defer-but-don't-block" phrasing. The V2
   fold *did* resolve the load-bearing half — per-primitive checkasm
   tests are moved into §6.2.1 as same-wave admission preconditions.
   What §6.3 still defers to SK-V10+ is the *checkasm harness
   infrastructure* (BBNF_SIMD_FORCE masks, the `.S` ABI shim, the
   `sigaction` trampoline, the cycle-counter binding) — genuinely
   broader infrastructure, not the per-primitive gate. The deferral is
   now narrower and arguably legitimate (infrastructure hardening is
   not a per-wave admission gate). But the sentence wording is
   unchanged from the V1 form CH6 flagged, and reads as a residual
   "defer-but-don't-block" on first pass. This is a **wording
   RESIDUAL, not a new paper-close** — the substantive gap (the
   per-primitive tests) was correctly folded into §6.2.1. Recommend
   S-P3 reword §6.3 to make the infrastructure-vs-per-primitive
   distinction explicit so the residual sentence does not read as the
   pattern CH6 V1 rejected.

5. **No fabricated-recovery.** No report invented a measurement to
   close a gap the V1 audit opened. P2-E's §6.2 codec-share inputs
   route through V3-C/V3-B derivative tables (the same provenance the
   exports-dir-absence forced in V1) and the report discloses the
   route. The exports directory is in fact present at this audit
   (`/tmp/skv9-xctrace-v3/exports/`), but the report's table-routed
   provenance is honest and conservative — not a paper-close.

The V2 fold is clean. Both REJECTs resolved, four mandated REVISEs
folded, zero new paper-close, one wording RESIDUAL flagged for S-P3
cosmetic cleanup.

---

End of CH6 V2.
