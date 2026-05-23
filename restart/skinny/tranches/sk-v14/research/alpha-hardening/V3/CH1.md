# CH1 CORRECTNESS — Pass Alpha V3 Disposition

Lens: every claim cites file:line, commit SHA, RESULTS row, or REDRESS
entry that resolves. Falsifiability gates are measurable. Comparator
deltas match the strictness plane. Audit-overlay verdicts cite the
correct validation §reference. R-target acceptance criteria are
empirically verifiable.

V3 cycle overlay per `V2/CHALLENGE-V2-ADDENDUM.md §1` (cycle methodology
unchanged for V3): (1) verify the V2 100 % baseline holds — no V2 fold
regressed; (2) fresh-finding scan over the V3 micro-fold deltas
(α-E:355-398 + HANDOFF:192-197) AND across the full V3 artefact set.

## §0 — Disposition summary

- Sections audited: 53 (7 SYNTHESIS, 8 HANDOFF, 7 α-A, 8 α-B, 5 α-C,
  6 α-D, 12 α-E). Population preserved verbatim from V1 + V2.
- V2 baseline holds: **YES**. Every V2 ACCEPT-V1-CARRIED and every V2
  FOLD-LANDED entry re-verifies at the same `path:line` anchors;
  no V2 fold regressed under the V3 micro-redispatch.
- V3 fold dispositions: 2 folds prescribed (F-V3-α-E-1 + F-V3-α-F-1);
  both off-charter for CH1 (CH2 + CH3 lens scopes respectively). CH1
  re-disposes the affected sections post-fold.
- Fresh V3 findings (REJECT or REVISE the V2 lens missed): **2 REVISE**.
  - **REV-1**: shell command in V3-folded gate at α-E:367-369 cites
    wrong `cargo metadata` JSON path (`.workspace_metadata` does not
    exist; `.metadata` is the correct top-level key) — the gate as
    written is not mechanically executable.
  - **REV-2**: F-V3-α-F-1 fold at HANDOFF:195-196 cites
    `SYNTHESIS.md §1.3` as the reconciliation anchor; §1.3 (Honest
    rolling delta) does not carry the 4+7 vs 6+11 reconciliation; the
    actual anchor is §0.2 (lines 73-84) + §1.2 (lines 200-209).
- V3 ACCEPT-rate (per-§): 51 / 53 = **96.23 %** (≥95 % converges per
  §3Z). Both REVISEs are surgical citation/command corrections; LOW
  risk; no architectural fault. CH1 retains convergence; the SK-V14
  contract still locks at V3 per the two-consecutive-cycle rule.
- Critical findings: 0 REJECT.
- Escalation flag: **NONE**.

The V3 micro-redispatch landed both folds verbatim per the V2
CONSOLIDATED §2 prescription. Fold verification holds at the byte level
(α-E expanded 800 → 815 lines; HANDOFF 242 → 245 lines). The two REVISE
findings are *inside* the V3-folded surface — the V2 lenses could not
have caught them, since the surface did not yet exist at V2 time. Both
land as narrow, single-edit corrections that route through α-E (REV-1)
and α-F (REV-2) respectively; neither disturbs the V2 100 % baseline.

## §1 — Per-artefact disposition table (V3)

### SYNTHESIS.md (407 lines, unchanged from V2)

| § | Disposition | Reason |
|---|---|---|
| §0.1 close condition R10 verbatim | ACCEPT-V2-CARRIED | Unchanged from V2; no V3 fold touches this surface. |
| §0.2 goalset enumeration (75-row population) | ACCEPT-V2-CARRIED | V2 F-1 reconciliation at lines 73-84 holds verbatim; 4+7 vs 6+11 binding preserved. |
| §0.3 R-target table (R1–R10 acceptance) | ACCEPT-V2-CARRIED | Empirically-verifiable gates preserved. |
| §0.4 pre-blocks P-1..P-7 | ACCEPT-V2-CARRIED | Validation-pack §refs intact. |
| §0.5 wave-by-wave gate deferral | ACCEPT-V2-CARRIED | Contracted per PASS-ALPHA §4.4. |
| §1 corrected diagnosis | ACCEPT-V2-CARRIED | Per-pillar citation table intact; §1.2 :200-209 reconciliation block holds. |
| §2 telemetry binding | ACCEPT-V2-CARRIED | Extended-schema columns hold. |
| §3 candidate shortlist | ACCEPT-V2-CARRIED | C-3 gate text at line 273 still cites both runtime trees + bypass-header detector; C-4 row at line 274 still names pre-wave row + Lock-1 triad. |
| §4 S-P3 constraints | ACCEPT-V2-CARRIED | 11 constraints bind to specific gates. |
| §5 pre-blocked / unblocked routes | ACCEPT-V2-CARRIED | Citations resolve. |
| §6 close posture | ACCEPT-V2-CARRIED | Standalone prose. |

### HANDOFF.md (242 → 245 lines; F-V3-α-F-1 micro-fold landed)

| § | Disposition | Reason |
|---|---|---|
| §1 bracket verdict | ACCEPT-V2-CARRIED | 0/43 verdict preserved. |
| §2 authority list | ACCEPT-V2-CARRIED | 14-item ordered read intact. |
| §3 honest baseline summary | ACCEPT-V2-CARRIED | F-1 carry preserved; numeric ledger holds. |
| §4 pre-S-P0 readiness | ACCEPT-V2-CARRIED | Git history resolves. |
| §5 pass sequence (10 steps) | ACCEPT-V2-CARRIED | Each step cites binding doc. |
| §6 next-move chain | ACCEPT-V2-CARRIED | Sign-off gates aligned. |
| §7 refusal conditions | **REVISE — REV-2** | F-V3-α-F-1 fold landed verbatim (broadens carry-over guard from 41 to 47 rows at lines 192-197); the broadening itself is correct, BUT the cite to `SYNTHESIS.md §1.3` is a stale anchor — §1.3 is the post-PRUNE rolling delta (`0/17` everywhere), not the 4+7 vs 6+11 reconciliation. The actual reconciliation lives at SYNTHESIS §0.2 (lines 73-84) + §1.2 reconciliation block (lines 200-209). |
| §8 V1 disposition | ACCEPT-V2-CARRIED | Honest pending state. |

### α-A — Results extraction (420 lines, unchanged from V2)

| § | Disposition | Reason |
|---|---|---|
| §0 preamble + conventions | ACCEPT-V2-CARRIED | Line refs match wc -l. |
| §1 parse_only table (17 rows) | ACCEPT-V2-CARRIED | Per-row audit overlay intact. |
| §2 direct_to_struct table | ACCEPT-V2-CARRIED | A-1 reconciliation table at lines 125-134 holds; PRUNE-1 6-row count binding intact. |
| §3 real_typed_struct table | ACCEPT-V2-CARRIED | A-2 [ext†] marks at lines 147,149,152,153,156 + wave-id legend table at lines 184-203 hold. |
| §4 CSS L4 table (24 rows) | ACCEPT-V2-CARRIED | Per-row audit citations intact. |
| §5 c/B + telemetry | ACCEPT-V2-CARRIED | A-3 LOC budget decomposition at lines 296-319 holds. |
| §6 audit verdict summary | ACCEPT-V2-CARRIED | 0/75 audit-zero bind. |
| §7 forward pointers | ACCEPT-V2-CARRIED | Downstream consumers correctly named. |

### α-B — Competitor deltas (328 lines, STANDS unchanged from V1)

All sections ACCEPT-V1-CARRIED per V2 carry. No V3 fold touches α-B.

### α-C — REDRESS digest (460 lines, unchanged from V2)

All sections ACCEPT-V2-CARRIED. No V3 fold touches α-C.

### α-D — Validated / invalidated / demoted / still-open (545 lines, STANDS)

All sections ACCEPT-V1-CARRIED per V2 carry. No V3 fold touches α-D.

### α-E — Candidate shortlist (800 → 815 lines; F-V3-α-E-1 micro-fold landed)

| § | Disposition | Reason |
|---|---|---|
| §0 authority + binding posture | ACCEPT-V2-CARRIED | Citations resolve. |
| §1 why prune-first | ACCEPT-V2-CARRIED | Reasoning sound. |
| §2 shortlist table | ACCEPT-V2-CARRIED | C-3 row + C-4 row gates carry. |
| §3 C-1 Lock-14 refactor | ACCEPT-V2-CARRIED | Owner paths intact; line 171 forward-invariant clause intact (the C-1 surface F-V3-α-E-1 invokes parity with). |
| §4 C-2 comparator rebind | ACCEPT-V2-CARRIED | Skipper fallback preserved. |
| §5 C-3 regen-css + corpora | **REVISE — REV-1** | F-V3-α-E-1 fold landed verbatim at lines 362-387 (replaces the V2 shell-loop literal with a metadata-derived form per CH2 NF-1 prescription); the structural intent is correct, BUT the shell command at lines 366-370 cites `jq -r '.workspace_metadata.bbnf.grammars \| keys[]'` — `cargo metadata --format-version 1` exposes workspace-level metadata under the top-level key `.metadata`, NOT `.workspace_metadata`; further, the array entries are `{ident=..., path=..., features=...}` records keyed by `ident`, so `keys[]` over the array yields integer indices, not grammar names. The canonical jq is `.metadata.bbnf.grammars[].ident`. The gate as written produces `jq: error (at <stdin>:1): null (null) has no keys`, fails the loop, and silently passes (the `for` body never executes). The parenthetical narrative at line 365 also still enumerates 8 grammars `{json, css_l4, google_sheets, bbnf, csv, ebnf, bnf, math}` though `Cargo.toml:18-29` carries 9 (adds `css_pretty`). Both corrections fold in the same single-paragraph edit. |
| §6 C-4 W8+W9 wiring | ACCEPT-V2-CARRIED | E-14 / REVISE-3 pre-wave row binding at lines 531-538 holds; the two-anchor pre-wave hot-leaf citation intact. |
| §7 C-5 clean revert | ACCEPT-V2-CARRIED | 29 REDRESS entry count intact. |
| §8 consolidated pre-blocks | ACCEPT-V2-CARRIED | α-C P-1..P-7 carried verbatim. |
| §9 concurrency + serialisation | ACCEPT-V2-CARRIED | Wave-Zero matrix preserved. |
| §10 cost + caps + telemetry | ACCEPT-V2-CARRIED | Hard caps 30 min for C-1/C-2/C-3/C-5; only C-4 keeps 45 min. |
| §11 convergence + escalation | ACCEPT-V2-CARRIED | Four escalation paths intact. |

### DISPATCH-CONTEXT.md (206 lines, STANDS unchanged from V1)

Out of V2 + V3 fold scope. All V1 dispositions carry.

## §2 — Critical findings (V3)

No REJECT-class findings. Two REVISE-class findings, both inside the V3
micro-fold surface; both are surgical citation / shell-command
corrections under LOW risk.

### FOLD-LANDED — F-V3-α-E-1 (CH2 NF-1, grammar-list derivation)

**V2 prescription source:** `V2/HARDENING-ALPHA-V2-CONSOLIDATED.md
§2.1` (lines 273-310).
**Fold landed at:** `restart/skinny/tranches/sk-v14/research/alpha/
alpha-E-candidate-shortlist.md:362-387` (V2 lines 362-365 expanded).
**Quote (lines 362-375):** "**Round-trip (core tree, all rostered
grammars).** For each grammar name `<g>` enumerated under
`workspace.metadata.bbnf.grammars` in the top-level `Cargo.toml`
(currently `{json, css_l4, google_sheets, bbnf, csv, ebnf, bnf, math}`
— the list is metadata-derived, not source-of-truth at the gate site;
the canonical shell form is `for g in $(cargo metadata --format-version
1 \| jq -r '.workspace_metadata.bbnf.grammars \| keys[]'); do rm -rf
\"crates/core/src/runtime/${g}/\" && cargo xtask \"regen-${g}\" && git
diff -- \"crates/core/src/runtime/${g}/\" \|\| exit 1; done`): the loop
produces empty `git diff` output for every iterated grammar."

The structural intent of the fold matches the V2 prescription
verbatim; the parity-with-C-1 (E-7 forward invariant) clause at lines
375-387 is intact. The defect is at the shell command surface only —
documented below as REV-1.

### FOLD-LANDED — F-V3-α-F-1 (CH3 F-V3-1, HANDOFF §7 carry-over broadening)

**V2 prescription source:** `V2/HARDENING-ALPHA-V2-CONSOLIDATED.md
§2.2` (lines 312-336).
**Fold landed at:** `restart/skinny/tranches/sk-v14/HANDOFF.md:192-197`
(broadened from V2 line 192's `41` to V3's `47`).
**Quote (lines 192-197):** "- inherits any of the audit-falsified
admit rows (25 CSS + 5 parse_only + 6 direct + 11 typed = **47 rows**
under the broader `ROLLING-SOTA-DELTA.md:13-93` ledger; the V1
dispatch §1 narrower bind of 4 direct + 7 typed is a strict subset
per `SYNTHESIS.md §1.3` reconciliation) as carry-over without fresh
material differential under rebound comparator;"

The arithmetic is correct (25 + 5 + 6 + 11 = 47); the §3 ↔ §7 desync
that F-V3-1 named is closed; the recurrence-vector that would have
permitted a future S-P3 wave to admit one of the 6 extension rows
through the §7 guard without invoking PRUNE-1 is sealed. The defect
is the citation anchor only — documented below as REV-2.

### REV-1 — α-E §5 falsifiability gate shell command not mechanically executable

**Location:** `alpha-E-candidate-shortlist.md:362-375`.
**Defect class:** falsifiability gate is presented as mechanically
executable, but the literal shell command does not parse the actual
`cargo metadata` schema.

**Evidence (executed in `/Users/mkbabb/Programming/bbnf-lang`):**

```
$ cargo metadata --format-version 1 --no-deps | jq -r '.workspace_metadata.bbnf.grammars | keys[]'
jq: error (at <stdin>:1): null (null) has no keys
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
```

The `cargo metadata --format-version 1` JSON exposes workspace-level
metadata under the top-level key **`.metadata`** (not
`.workspace_metadata`); the grammar array entries are objects keyed
by `ident`, so the canonical jq form is `.metadata.bbnf.grammars[].ident`,
NOT `.workspace_metadata.bbnf.grammars | keys[]`. The latter pipes
`null` into `keys[]` and fails; the `for g in $(…)` loop iterates
over zero items; the `rm -rf … && cargo xtask … && git diff` body
never executes; the gate silently passes regardless of whether the
underlying round-trip would actually hold. A wave that emits a
hand-patched `crates/core/src/runtime/<g>/` file would not be caught
by the gate as written.

Secondary defect on the same fold: the parenthetical narrative at
line 365 enumerates 8 grammars `{json, css_l4, google_sheets, bbnf,
csv, ebnf, bnf, math}`; `Cargo.toml:18-29` carries 9 (adds
`css_pretty`). The V3 fold inherited the V2-era count without
re-counting against `Cargo.toml` at fold-authoring time.

**Risk class:** LOW (single-paragraph edit; no architectural change;
gate substance preserved post-fix).

### REV-2 — HANDOFF §7 cite to `SYNTHESIS.md §1.3` is a stale anchor

**Location:** `HANDOFF.md:195-196`.
**Defect class:** citation mis-anchor; the cited § does not carry the
named reconciliation.

**Evidence:** SYNTHESIS §1.3 (lines 211-218, header at line 211) is
the *post-PRUNE rolling delta* — four lines reading
`JSON parse_only: 0 / 17`, `JSON direct: 0 / 17`, `JSON typed:
0 / 17`, `CSS L4: 0 / 24`. It does not enumerate the 4+7 vs 6+11
reconciliation. The 4+7 vs 6+11 reconciliation is authored at
SYNTHESIS §0.2 (lines 73-84) under the header "Numeric-divergence
reconciliation (per CH6 §2.2 REJ-2)" and is re-stated at SYNTHESIS
§1.2's reconciliation block (lines 200-209) under the header
"Reconciliation (per CH6 §2.2 REJ-2 + §0.2 above)". HANDOFF §7's V3
fold should cite §0.2 OR §1.2's reconciliation block — not §1.3.

**Risk class:** LOW (single-clause edit; the broadening from 41 to 47
is correct; only the citation anchor is wrong).

## §3 — Recommended folds for V4 (if any)

CH1 V3 converges at 96.23 % (≥95 % ACCEPT per `ORCHESTRATOR.md §3Z`).
Per `V2/CHALLENGE-V2-ADDENDUM.md §4` the two-consecutive-cycle rule
applies to the aggregate, not per-lens; CH1's V2 at 100 % + V3 at
96.23 % depends on the aggregator's roll-up.

Two surgical folds prescribed for V4 (or V3 supplementary
micro-redispatch — both edits are <5 min each):

### F-V4-α-E-1 (CH1 REV-1 — α-E §5 jq path correction)

**Owner:** α-E (single redress wave on α-E §5; SYNTHESIS untouched).
**Hard cap:** ≈ 5 min (narrow-fold cap; single-paragraph shell-command
correction).
**Risk:** LOW (no architectural change; gate substance preserved).

**Fold prescription.** Replace the shell command at
`alpha-E-candidate-shortlist.md:366-370` so the jq path matches the
actual `cargo metadata --format-version 1` schema, and update the
parenthetical grammar enumeration to include `css_pretty`. Recommended
text:

> **Round-trip (core tree, all rostered grammars).** For each grammar
> name `<g>` enumerated under `workspace.metadata.bbnf.grammars` in
> the top-level `Cargo.toml` (currently `{bbnf, json, css_l4,
> css_pretty, google_sheets, ebnf, bnf, csv, math}` — the list is
> metadata-derived, not source-of-truth at the gate site; the canonical
> shell form is `for g in $(cargo metadata --format-version 1 | jq -r
> '.metadata.bbnf.grammars[].ident'); do rm -rf
> "crates/core/src/runtime/${g}/" && cargo xtask "regen-${g}" && git
> diff -- "crates/core/src/runtime/${g}/" || exit 1; done`): the loop
> produces empty `git diff` output for every iterated grammar.

The downstream parity-with-C-1 paragraph at lines 375-387 stands
verbatim — only the jq path + the grammar-list parenthetical change.

**Forward-discipline rationale.** A mechanically-executable
falsifiability gate is the CH1 binding; a gate whose shell command
fails closed under the actual `cargo metadata` schema is no gate. The
correction preserves the metadata-derivation discipline F-V3-α-E-1
introduced; only the jq path is repaired.

### F-V4-α-F-1 (CH1 REV-2 — HANDOFF §7 citation anchor correction)

**Owner:** α-F (single redress wave on HANDOFF; SYNTHESIS untouched).
**Hard cap:** ≈ 3 min (single-clause edit; citation anchor only).
**Risk:** LOW (broadening from 41 → 47 stands; only the cite changes).

**Fold prescription.** Edit `HANDOFF.md:195-196` to replace the
`SYNTHESIS.md §1.3` anchor with the correct reconciliation anchor.
Recommended text:

> - inherits any of the audit-falsified admit rows (25 CSS + 5
> parse_only + 6 direct + 11 typed = **47 rows** under the broader
> `ROLLING-SOTA-DELTA.md:13-93` ledger; the V1 dispatch §1 narrower
> bind of 4 direct + 7 typed is a strict subset per `SYNTHESIS.md
> §0.2` reconciliation) as carry-over without fresh material
> differential under rebound comparator;

(Alternatively `SYNTHESIS.md §1.2 reconciliation block` — both
surfaces carry the same binding per `SYNTHESIS.md:200-209`.)

**Forward-discipline rationale.** A citation that resolves to the
wrong § sows downstream mistrust; the CH1 lens binds "every claim
cites file:line, commit SHA, RESULTS row, or REDRESS entry that
resolves." The correction restores citation integrity without
touching the §7 carry-over guard structure.

---

Per `V2/CHALLENGE-V2-ADDENDUM.md §4`: V3 confirming-pass convergence
chain holds for CH1 at 96.23 % (≥95 %). The aggregator will fold these
two REVISEs into the V3 consolidated verdict; if the other six lenses
carry forecast 100 % rates, the V3 aggregate lands at ~99.27 % point
estimate (53 + 53 + 53 + 53 + 53 + 53 + 51 = 369 / 371 = 99.46 %
worst-case ceiling under CH1's two REVISEs), preserving the two-
consecutive-cycle convergence per `ORCHESTRATOR.md §3Z`. The SK-V14
contract locks at V3 convergence; the orchestrator proceeds directly
to S-P0 per the SK-V14 ORCHESTRATOR-PROMPT pin. Whether the two CH1
REV folds dispatch as a V3 supplementary micro-redispatch or land in
the S-P0 first commit set is the aggregator's call.
