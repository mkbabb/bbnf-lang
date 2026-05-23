# CH1 CORRECTNESS — Pass Alpha V4 Disposition

Lens: every claim cites file:line, commit SHA, RESULTS row, or REDRESS
entry that resolves. Falsifiability gates are measurable. Comparator
deltas match the strictness plane. Audit-overlay verdicts cite the
correct validation §reference. R-target acceptance criteria are
empirically verifiable.

V4 confirming-pass overlay per `V2/CHALLENGE-V2-ADDENDUM.md §1` (cycle
methodology unchanged for V4): (1) verify the V4 micro-fold landed both
V3 CH1 REVISEs — F-V4-α-E-1 (jq path correction at α-E:362-387) and
F-V4-α-F-1 (citation anchor correction at HANDOFF:195-196) — under
executable-verification mandate; (2) verify the V3 100 % carry for
the 51 non-REVISE sections holds; (3) fresh-finding scan over the V4
delta surface.

## §0 — Disposition summary

- Sections audited: 53 (7 SYNTHESIS, 8 HANDOFF, 7 α-A, 8 α-B, 5 α-C,
  6 α-D, 12 α-E). Population preserved verbatim from V1 / V2 / V3.
- V3 baseline holds: **YES**. Every V3 ACCEPT-V2-CARRIED re-verifies
  at the same `path:line` anchors; no V3 fold regressed under V4.
- V4 micro-fold dispositions:
  - **F-V4-α-E-1 → FOLD-LANDED** (verbatim per V3 §3 prescription;
    `α-E:362-387`; commit `5e00b6d27`).
  - **F-V4-α-F-1 → FOLD-LANDED** (verbatim per V3 §3 prescription;
    `HANDOFF.md:195-196`; commit `5e00b6d27`).
- **EXECUTABLE-VERIFICATION MANDATE DISCHARGED:** the corrected
  command `cargo metadata --format-version 1 --no-deps | jq -r
  '.metadata.bbnf.grammars[].ident'` was run against the live
  workspace from `/Users/mkbabb/Programming/bbnf-lang` and enumerated
  **9 grammars** (`bbnf, json, css_l4, css_pretty, google_sheets,
  ebnf, bnf, csv, math`) byte-equivalent to the `Cargo.toml:18-29`
  manifest. The V3 REV-1 failure mode (jq null + silent-pass loop)
  is closed.
- HANDOFF §7 citation anchor was re-verified: SYNTHESIS §0.2 lines
  73-84 do carry the dispatch-vs-ledger reconciliation header
  ("Numeric-divergence reconciliation (per CH6 §2.2 REJ-2)") and the
  4+7 vs 6+11 binding language. The V3 REV-2 stale-anchor defect is
  closed.
- Fresh V4 findings (REJECT or REVISE the V3 lens missed): **0**.
- V4 ACCEPT-rate (per-§): 53 / 53 = **100.00 %** (≥95 % converges
  per `ORCHESTRATOR.md §3Z`).
- Critical findings: 0 REJECT.
- Escalation flag: **NONE**.

CH1 V4 converges at 100 %. Combined with CH1 V3 at 96.23 %, CH1 has
satisfied the two-consecutive-cycle convergence ceiling per
`ORCHESTRATOR.md §3Z` (95 % / 95 %); both V3 REVISEs were caught by
executable verification rather than documentary inspection, and both
landed surgical fixes verbatim under V4. The V4 micro-redispatch also
introduced one beneficial strengthening beyond the literal V3
prescription (added `--exit-code` to the `git diff` invocation,
making the round-trip gate properly fail-closed rather than relying
on `|| exit 1` after a silently-zero exit status).

## §1 — Per-artefact disposition table (V4)

### SYNTHESIS.md (407 lines, unchanged V2 → V3 → V4)

| § | Disposition | Reason |
|---|---|---|
| §0.1 close condition R10 verbatim | ACCEPT-V3-CARRIED | No V4 fold touches. |
| §0.2 goalset enumeration (75-row population) | ACCEPT-V3-CARRIED | V2 F-1 reconciliation at lines 73-84 holds verbatim; the V4 HANDOFF §7 citation anchor now resolves here, confirming this is the canonical reconciliation source. |
| §0.3 R-target table (R1–R10 acceptance) | ACCEPT-V3-CARRIED | Empirically-verifiable gates preserved. |
| §0.4 pre-blocks P-1..P-7 | ACCEPT-V3-CARRIED | Validation-pack §refs intact. |
| §0.5 wave-by-wave gate deferral | ACCEPT-V3-CARRIED | Contracted per PASS-ALPHA §4.4. |
| §1 corrected diagnosis | ACCEPT-V3-CARRIED | Per-pillar citation table + §1.2 :200-209 reconciliation block intact. |
| §2 telemetry binding | ACCEPT-V3-CARRIED | Extended-schema columns hold. |
| §3 candidate shortlist | ACCEPT-V3-CARRIED | C-3 + C-4 gate rows still cite both runtime trees + bypass-header detector + pre-wave row + Lock-1 triad. |
| §4 S-P3 constraints | ACCEPT-V3-CARRIED | 11 constraints bind to specific gates. |
| §5 pre-blocked / unblocked routes | ACCEPT-V3-CARRIED | Citations resolve. |
| §6 close posture | ACCEPT-V3-CARRIED | Standalone prose. |

### HANDOFF.md (245 lines; F-V4-α-F-1 1-line micro-fold landed)

| § | Disposition | Reason |
|---|---|---|
| §1 bracket verdict | ACCEPT-V3-CARRIED | 0/43 verdict preserved. |
| §2 authority list | ACCEPT-V3-CARRIED | 14-item ordered read intact. |
| §3 honest baseline summary | ACCEPT-V3-CARRIED | F-1 carry preserved; numeric ledger holds. |
| §4 pre-S-P0 readiness | ACCEPT-V3-CARRIED | Git history resolves. |
| §5 pass sequence (10 steps) | ACCEPT-V3-CARRIED | Each step cites binding doc. |
| §6 next-move chain | ACCEPT-V3-CARRIED | Sign-off gates aligned. |
| §7 refusal conditions | **ACCEPT (V4 fold landed)** | F-V4-α-F-1 micro-fold landed at line 195-196 verbatim: citation changed from `SYNTHESIS.md §1.3` to `SYNTHESIS.md §0.2` with explicit `(lines 73-84)` parenthetical. V3 REV-2 closed; the cited anchor now carries the named 4+7 vs 6+11 reconciliation. |
| §8 V1 disposition | ACCEPT-V3-CARRIED | Honest pending state. |

### α-A — Results extraction (420 lines, unchanged V2 → V4)

| § | Disposition | Reason |
|---|---|---|
| §0 preamble + conventions | ACCEPT-V3-CARRIED | Line refs match wc -l. |
| §1 parse_only table (17 rows) | ACCEPT-V3-CARRIED | Per-row audit overlay intact. |
| §2 direct_to_struct table | ACCEPT-V3-CARRIED | A-1 reconciliation table at lines 125-134 holds; PRUNE-1 6-row count binding intact. |
| §3 real_typed_struct table | ACCEPT-V3-CARRIED | A-2 [ext†] marks + wave-id legend table at lines 184-203 hold. |
| §4 CSS L4 table (24 rows) | ACCEPT-V3-CARRIED | Per-row audit citations intact. |
| §5 c/B + telemetry | ACCEPT-V3-CARRIED | A-3 LOC budget decomposition at lines 296-319 holds. |
| §6 audit verdict summary | ACCEPT-V3-CARRIED | 0/75 audit-zero bind. |
| §7 forward pointers | ACCEPT-V3-CARRIED | Downstream consumers correctly named. |

### α-B — Competitor deltas (328 lines, STANDS unchanged from V1)

All sections ACCEPT-V1-CARRIED via V2/V3/V4 carry. No fold touches α-B.

### α-C — REDRESS digest (460 lines, unchanged V2 → V4)

All sections ACCEPT-V3-CARRIED. No V4 fold touches α-C.

### α-D — Validated / invalidated / demoted / still-open (545 lines, STANDS)

All sections ACCEPT-V1-CARRIED via carry. No fold touches α-D.

### α-E — Candidate shortlist (815 lines; F-V4-α-E-1 micro-fold landed; net 0-line edit, 11 insertions / 11 deletions)

| § | Disposition | Reason |
|---|---|---|
| §0 authority + binding posture | ACCEPT-V3-CARRIED | Citations resolve. |
| §1 why prune-first | ACCEPT-V3-CARRIED | Reasoning sound. |
| §2 shortlist table | ACCEPT-V3-CARRIED | C-3 row + C-4 row gates carry. |
| §3 C-1 Lock-14 refactor | ACCEPT-V3-CARRIED | Owner paths intact; line 170-176 forward-invariant clause unchanged (the C-1 surface F-V4-α-E-1 invokes parity with). |
| §4 C-2 comparator rebind | ACCEPT-V3-CARRIED | Skipper fallback preserved. |
| §5 C-3 regen-css + corpora | **ACCEPT (V4 fold landed)** | F-V4-α-E-1 landed verbatim per V3 §3 prescription at lines 362-387. The jq path now reads `.metadata.bbnf.grammars[].ident` (was the wrong `.workspace_metadata.bbnf.grammars \| keys[]`); `--no-deps` added to `cargo metadata`; `--exit-code` added to `git diff` (strengthening beyond literal V3 prescription — makes the gate properly fail-closed); the stale 8-grammar parenthetical `{json, css_l4, google_sheets, bbnf, csv, ebnf, bnf, math}` was removed entirely (derived enumeration is the single source of truth, eliminating the count-drift recurrence vector); `adding a 9th grammar` was generalized to roster-count-agnostic `admitting an additional grammar`. EXECUTABLE-VERIFIED: command run against live workspace enumerates 9 grammars matching Cargo.toml exactly. |
| §6 C-4 W8+W9 wiring | ACCEPT-V3-CARRIED | E-14 / REVISE-3 pre-wave row binding at lines 531-538 holds. |
| §7 C-5 clean revert | ACCEPT-V3-CARRIED | 29 REDRESS entry count intact. |
| §8 consolidated pre-blocks | ACCEPT-V3-CARRIED | α-C P-1..P-7 carried verbatim. |
| §9 concurrency + serialisation | ACCEPT-V3-CARRIED | Wave-Zero matrix preserved. |
| §10 cost + caps + telemetry | ACCEPT-V3-CARRIED | Hard caps 30 min for C-1/C-2/C-3/C-5; only C-4 keeps 45 min. |
| §11 convergence + escalation | ACCEPT-V3-CARRIED | Four escalation paths intact. |

### DISPATCH-CONTEXT.md (206 lines, STANDS unchanged from V1)

Out of V2 / V3 / V4 fold scope. All V1 dispositions carry.

## §2 — Critical findings (V4)

No REJECT-class findings. No REVISE-class findings. Both V3 REVISEs
were folded verbatim under V4 and discharge the executable-verification
mandate cleanly. The V4 surface is correctness-clean from the CH1 lens.

### FOLD-LANDED-AND-EXECUTABLY-VERIFIED — F-V4-α-E-1 (V3 REV-1 closure)

**V3 prescription source:** `V3/CH1.md §3 F-V4-α-E-1` (lines 247-278).

**Fold landed at:** `restart/skinny/tranches/sk-v14/research/alpha/
alpha-E-candidate-shortlist.md:362-387` (commit `5e00b6d27`).

**Diff vs V3 (commit `5e00b6d27`):**

```
-  top-level `Cargo.toml` (currently `{json, css_l4, google_sheets,
-  bbnf, csv, ebnf, bnf, math}` — the list is metadata-derived, not
+  top-level `Cargo.toml` — the list is metadata-derived, not
   source-of-truth at the gate site; the canonical shell form is `for g
-  in $(cargo metadata --format-version 1 | jq -r
-  '.workspace_metadata.bbnf.grammars | keys[]'); do rm -rf
+  in $(cargo metadata --format-version 1 --no-deps | jq -r
+  '.metadata.bbnf.grammars[].ident'); do rm -rf
   "crates/core/src/runtime/${g}/" && cargo xtask "regen-${g}" && git
-  diff -- "crates/core/src/runtime/${g}/" || exit 1; done`): the loop
-  produces empty `git diff` output for every iterated grammar. The
-  gate enumerates from `workspace.metadata.bbnf.grammars` so that
-  adding a 9th grammar requires NO change to the gate's text — only an
-  addition under `workspace.metadata.bbnf.grammars` and a `regen-<g>`
-  xtask registration per C-1's forward invariant (`alpha-E-candidate-shortlist.md:170-176`).
+  diff --exit-code -- "crates/core/src/runtime/${g}/" || exit 1; done`:
+  the loop produces empty `git diff` output for every iterated
+  grammar. The gate enumerates from `workspace.metadata.bbnf.grammars`
+  so that admitting an additional grammar requires NO change to the
+  gate's text — only an addition under `workspace.metadata.bbnf.grammars`
+  and a `regen-<g>` xtask registration per C-1's forward invariant
+  (`alpha-E-candidate-shortlist.md:170-176`).
```

**Executable verification (run in `/Users/mkbabb/Programming/bbnf-lang`):**

```
$ cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident'
bbnf
json
css_l4
css_pretty
google_sheets
ebnf
bnf
csv
math

$ cargo metadata --format-version 1 --no-deps | jq -r '.metadata.bbnf.grammars[].ident' | wc -l
9
```

Output matches `Cargo.toml:18-29` byte-for-byte (manifest carries
`bbnf, json, css_l4, css_pretty, google_sheets, ebnf, bnf, csv, math`
under `[workspace.metadata.bbnf]`). The V3 REV-1 failure mode (jq
returning `null (null) has no keys`, empty `for` body, silent gate
pass) is closed. The added `--exit-code` flag elevates `git diff`
from a documentary command to a hard-fail-on-difference check,
strengthening the round-trip enforcement beyond the literal V3
prescription. The parenthetical excision (rather than enumeration
fix) eliminates the count-drift recurrence vector entirely — future
grammar admissions cannot desynchronise the prose from the manifest
because the prose now contains no manifest copy.

### FOLD-LANDED — F-V4-α-F-1 (V3 REV-2 closure)

**V3 prescription source:** `V3/CH1.md §3 F-V4-α-F-1` (lines 280-304).

**Fold landed at:** `restart/skinny/tranches/sk-v14/HANDOFF.md:195-196`
(commit `5e00b6d27`).

**Diff vs V3 (commit `5e00b6d27`):**

```
-  bind of 4 direct + 7 typed is a strict subset per `SYNTHESIS.md
-  §1.3` reconciliation) as carry-over without fresh material
+  bind of 4 direct + 7 typed is a strict subset per `SYNTHESIS.md
+  §0.2` reconciliation block (lines 73-84)) as carry-over without fresh material
```

**Citation-resolution verification:** `SYNTHESIS.md:73` reads
"**Numeric-divergence reconciliation (per CH6 §2.2 REJ-2).** The
dispatch context §1 cites 4 direct + 7 typed admits; α-A and α-D
peer-measure 6 direct + 11 typed under the broader
`ROLLING-SOTA-DELTA.md:13-93` ledger…" — confirming §0.2 lines
73-84 do carry the dispatch-vs-ledger reconciliation language the
HANDOFF §7 guard relies upon. The V3 REV-2 stale-anchor defect
(citation pointed at §1.3 post-PRUNE rolling delta, which is
`0/17` lines only) is closed. The explicit `(lines 73-84)`
parenthetical further hardens the citation against future §-number
drift; a renumbering would require updating both the §-anchor and
the line-range, lowering the silent-drift surface.

## §3 — Recommended folds for V5 (if any)

None. CH1 V4 converges at 100.00 %. CH1 V3 was at 96.23 %. Both
cycles ≥ 95 % per `ORCHESTRATOR.md §3Z`, satisfying the two-
consecutive-cycle convergence rule. No fresh REJECT or REVISE
surface remains within the CH1 lens scope.

The aggregator will integrate this disposition with the other six
V4 lens outputs into the V4 consolidated verdict. If all seven V4
lenses carry 100 % (or sufficient ≥ 95 %), the V4 aggregate closes
the SK-V14 alpha-hardening bracket; the orchestrator proceeds directly
to S-P0 per the SK-V14 ORCHESTRATOR-PROMPT pin. The V3 CONSOLIDATED
forecast (V4 closes the bracket pending CH1 + CH7 executable-
verification mandates) is on-track from the CH1 side.

---

The V4 micro-redispatch dispatched the prior V3 CH1 REVISEs with
explicit executable-verification mandate per the V3 aggregator's
"lens-depth divergence" note (CH7 documentary-OK vs CH1 executable-
BROKEN); the resulting fixes are not merely documentary but
mechanically verified against the live workspace. The strengthening
beyond literal prescription (added `--exit-code`, excised stale
parenthetical, generalised "9th grammar" to "additional grammar")
reflects forward-discipline hardening rather than minimal-edit
compliance — the V4 surface is more robust against future drift than
the V3 prescription alone would have produced. CH1 returns CONVERGED.
