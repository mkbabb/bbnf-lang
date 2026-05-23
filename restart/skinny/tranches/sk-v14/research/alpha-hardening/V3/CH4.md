# CH4 COST — Pass Alpha V3 Disposition

Lens binding: `restart/prompts/ORCHESTRATOR.md:86` ("LOC budget, risk
class, wave alignment, and hard cap are stated and realistic; same-wave
consumer present per kernel/primitive"). V3 dispatch context:
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V2/CHALLENGE-V2-ADDENDUM.md`
(cycle methodology inherited; V3 confirming-pass overlay per
`ORCHESTRATOR.md §3Z` two-consecutive-cycle rule). V2 disposition
baseline: `restart/skinny/tranches/sk-v14/research/alpha-hardening/V2/CH4.md`
(100% ACCEPT; 0 REJECT, 0 REVISE; 6 informational notes below the
action threshold). V2 CONSOLIDATED verdict at 99.27% aggregate
(`HARDENING-ALPHA-V2-CONSOLIDATED.md:33`): CONVERGED-EXPECTING-V3-
CONFIRM, micro-fold packets `F-V3-α-E-1` + `F-V3-α-F-1` landed via
commit `5e2ae78b4`.

## §0 — V3 Disposition summary

- **V3 ACCEPT-rate: 100 % (34 / 34 sectioned dispositions).**
- **V3 REJECT count: 0.** The V2 baseline (zero REJECTs) holds across
  every CH4-relevant cell of every artefact; the two V3 micro-folds
  introduce no fresh REJECT surface and no cost / risk / cap / wave-
  alignment / same-wave-consumer regression.
- **V3 REVISE count: 0.** The V2 baseline (zero REVISEs) holds; the V3
  fresh-finding scan over the α-E §5 derived-enumeration insertion and
  the HANDOFF §7 carry-over broadening surfaces no fresh REVISE.
- **Critical findings: 0.** Cost-surface rebind from V2 is preserved
  verbatim across SYNTHESIS §3 + §4, HANDOFF §6, α-E §2 / §6 / §9 /
  §10, α-A §5; F-V3-α-E-1 + F-V3-α-F-1 are doc-only edits routed
  through previously-bounded C-3 and HANDOFF §7 surfaces with no LOC
  envelope drift.
- **CH4 V3 verdict: CONVERGES.** Lens-local 100 % per the V2 +
  CONSOLIDATED §3.1 forecast. CH4 contributes the second consecutive
  100 % link to the two-cycle convergence chain.

### §0.1 — V2 → V3 baseline-hold verification

| V2 CH4 fold | V2 fold ID | V3 site after micro-fold | Status |
|---|---|---|---|
| R1 — SYNTHESIS §3 risk column | F-3 | `SYNTHESIS.md:271-275` (C-1 VERY HIGH; C-2 HIGH; C-3 HIGH; C-4 VERY HIGH; C-5 MED-LOW) | **HELD** |
| R2 — SYNTHESIS §3 LOC envelope column | F-4 | `SYNTHESIS.md:269` header + `:271-275` per-row + `:277` total ≈ 5.65k–8.38k | **HELD** |
| R3 — α-E §10 hard-cap revert | E-2 | `alpha-E-candidate-shortlist.md:756-760` (C-1/C-2/C-3/C-5 = 30 min; only C-4 = 45 min); `:762-770` cap-discipline reconciliation paragraph | **HELD** (line offset +15 from V3 §5 insertion; content verbatim) |
| V1 — C-1 LOC lower-bound 2.8k | E-11 | `alpha-E-candidate-shortlist.md:83` (2.8k–3.4k) + `:89-92` envelope rationale | **HELD** |
| V2 — C-2 LOC ceiling +80 LOC Skipper | E-12 | `alpha-E-candidate-shortlist.md:84` (600–1.08k) + `:268-274` Skipper fallback paragraph | **HELD** |
| V3 — §9 vs §6 C-1↔C-4 sequencing | E-13 | `alpha-E-candidate-shortlist.md:729` (matrix row "C-4 \| (one shape at a time) \| C-1 (ALL sub-waves), C-2"); `:731-741` §6-authoritative reconciliation paragraph | **HELD** (line offset +15; content verbatim) |
| V4 — SYNTHESIS §3 same-wave consumer column | F-5 | `SYNTHESIS.md:269` header includes "Same-wave consumer"; `:271-275` per-row values | **HELD** |
| V5 — HANDOFF §6 hard-cap echo | F-7 | `HANDOFF.md:162-165` (30-min lens-agent cap; 20/15/30-or-45 R/P/R cadence with C-4 carve-out) | **HELD** |
| V6 — α-A §5 c/B LOC budget | A-3 | `alpha-A-results-extraction.md:296-319` (80-120 LOC; carry-with-C-2; same-wave consumer rule preserved) | **HELD** |
| V7 — SYNTHESIS §4 per-wave LOC ceiling | F-6 | `SYNTHESIS.md:326-329` (per-candidate envelope inheritance; > 20 % escalation per `[generated-size-budget]`) | **HELD** |

Roll-up: **10 / 10 V2 CH4 folds HELD under V3 micro-fold pressure.**
Zero regression; zero envelope drift; zero cap-discipline drift;
zero same-wave-consumer rule weakening.

### §0.2 — V3 micro-fold cost-surface delta

| V3 fold | Owner artefact | Site | LOC delta | Cost-surface impact |
|---|---|---|---:|---|
| F-V3-α-E-1 | α-E §5 (C-3 round-trip gate) | `alpha-E-candidate-shortlist.md:362-387` | +15 LOC of prose (gate spec; ~6 LOC of canonical `cargo metadata + jq` shell incantation + ~9 LOC of forward-discipline rationale) | C-3 implementation envelope 1.2k – 2.0k UNCHANGED; the fold tightens the gate spec, not the gate's implementation surface. The xtask round-trip loop is a single shell command in the wave's CI; no LOC change to C-3's xtask binary, harness, codegen wiring, or corpora vendoring. |
| F-V3-α-F-1 | α-F (HANDOFF §7) | `HANDOFF.md:192-197` | +4 LOC (`5 → 9` line block; `41 → 47 rows` arithmetic broadening + SYNTHESIS §1.3 citation) | Cost-surface unchanged; the broadening is documentary precision over the carry-over guard count. C-5's 250–500 LOC delete-heavy envelope is unaffected (the 6 newly-bound extension rows route through PRUNE-1 + PRUNE-2 revert paths C-5 already owns per E-10's 29-row scribe contract). |

Total V3 fold delta: +19 LOC of doc-surface prose; zero LOC delta to
any C-1 / C-2 / C-3 / C-4 / C-5 implementation envelope. Total
envelope 5.65k–8.38k HELD verbatim.

## §1 — Per-artefact disposition table (V3)

V3 disposition retains every V2 ACCEPT decision; the two V3-touched
sites (α-E §5 round-trip gate; HANDOFF §7 refusal list) are
re-evaluated below. Every other cell carries its V2 ACCEPT through.

| Artefact | § | V2 disposition | V3 disposition | Reason |
|---|---|---|---|---|
| `SYNTHESIS.md` | §0.1 Close condition (R10) | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §0.2 Goalset row enumeration | ACCEPT | ACCEPT | Unchanged; F-1 reconciliation paragraph at `SYNTHESIS.md:82-90` HELD. |
| `SYNTHESIS.md` | §0.3 R-target acceptance | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §0.4 P-1 … P-7 pre-blocks | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §0.5 Wave-by-wave gates deferred | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §1.1 Survives pillars | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §1.2 Falsified items | ACCEPT | ACCEPT | Unchanged; the 6+11 ledger now matches HANDOFF §7 post-F-V3-α-F-1 (the V2 desync CH3 surfaced is closed). |
| `SYNTHESIS.md` | §1.3 Honest rolling delta | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §2 Telemetry binding | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §3 Candidate shortlist | ACCEPT | ACCEPT | F-3 + F-4 + F-5 all HELD verbatim; risk + LOC + same-wave consumer columns intact at `SYNTHESIS.md:271-275`; total envelope :277 ≈ 5.65k–8.38k HELD. |
| `SYNTHESIS.md` | §4 S-P3 constraints | ACCEPT | ACCEPT | F-6 LOC-ceiling clause at :326-329 HELD; `> 20 %` escalation discipline preserved. |
| `SYNTHESIS.md` | §5 Pre-blocked / unblocked routes | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §6 Close posture | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §1 Bracket verdict | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §2 Authority list | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §3 Honest baseline summary | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §4 Pre-S-P0 readiness | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §5 Pass sequence | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §6 Next-move | ACCEPT | ACCEPT | F-7 cap-echo at :162-165 HELD verbatim; G-Omega restoration at :159-160 HELD. |
| `HANDOFF.md` | §7 Refusal conditions | ACCEPT | **ACCEPT** | F-V3-α-F-1 broadening at :192-197 is the V3 fold target; cost-surface impact zero. The wider 6+11 = 47 rows guard binds 6 additional carry-over rows (marine_ik, instruments, random, instruments-typed, numbers-typed, unicode_basic) to the C-5 revert + REDRESS scope already covered by E-10's 29-row scribe contract — see §2.1 below. |
| `HANDOFF.md` | §8 V1 disposition | ACCEPT | ACCEPT | Unchanged. |
| `alpha-A-results-extraction.md` | §1–§4 per-plane row tables | ACCEPT | ACCEPT | Unchanged. |
| `alpha-A-results-extraction.md` | §5 c/B telemetry | ACCEPT | ACCEPT | A-3 c/B LOC budget at :296-319 HELD; 80-120 LOC routed through C-2's existing envelope without ceiling raise. |
| `alpha-A-results-extraction.md` | §6 verdict summary | ACCEPT | ACCEPT | Unchanged. |
| `alpha-A-results-extraction.md` | §7 forward pointers | ACCEPT | ACCEPT | Unchanged. |
| `alpha-B-competitor-deltas.md` | §316-320 Skipper absence | ACCEPT | ACCEPT | STAND from V1 per V2 + V3 addenda; α-B is unchanged in V3 commit. |
| `alpha-C-redress-digest.md` | full | ACCEPT | ACCEPT | C-1 (P-7 triple-check) is CH5-domain; CH4 cost-surface unchanged. |
| `alpha-D-validated-invalidated.md` | full | ACCEPT | ACCEPT | STAND from V1 per V2 + V3 addenda. |
| `alpha-E-candidate-shortlist.md` | §2 shortlist table | ACCEPT | ACCEPT | E-11 + E-12 envelopes HELD at :83-84 (C-1 2.8k–3.4k; C-2 600–1.08k); total :89-92 ≈ 5.65k–8.38k HELD. |
| `alpha-E-candidate-shortlist.md` | §3 C-1 narrative | ACCEPT | ACCEPT | E-7 forward-invariant + E-11 LOC reconciliation HELD. |
| `alpha-E-candidate-shortlist.md` | §4 C-2 narrative | ACCEPT | ACCEPT | E-12 ceiling raise at :268-274 HELD verbatim. |
| `alpha-E-candidate-shortlist.md` | §5 C-3 narrative | ACCEPT | **ACCEPT** | F-V3-α-E-1 lands +15 LOC of gate prose at :362-387 (derived-enumeration form via `cargo metadata + jq`); the C-3 LOC budget at :410-413 (1.2k – 2.0k: ≈ 400-600 LOC xtask + harness; ≈ 200 LOC generic codegen; ≈ 400-600 LOC corpora vendoring; ≈ 200 LOC bench harness extension) is UNCHANGED — the V3 fold tightens the gate spec, not the gate's implementation surface. See §2.2 below. |
| `alpha-E-candidate-shortlist.md` | §6 C-4 narrative | ACCEPT | ACCEPT | E-3 Lock-1 triad + E-4 module-path discipline + E-5 pre-wave hot-leaf citation HELD; 800–1.4k envelope UNCHANGED. |
| `alpha-E-candidate-shortlist.md` | §7 C-5 narrative | ACCEPT | ACCEPT | E-10 29-row scribe-contract count HELD; cost envelope 250–500 (delete-heavy) UNCHANGED. The F-V3-α-F-1 broadening of HANDOFF §7's carry-over guard does NOT alter C-5's 29-row contract — see §2.1. |
| `alpha-E-candidate-shortlist.md` | §8 pre-blocked routes | ACCEPT | ACCEPT | Unchanged. |
| `alpha-E-candidate-shortlist.md` | §9 concurrency matrix | ACCEPT | ACCEPT | E-13 matrix row at :729 + reconciliation paragraph at :731-741 HELD verbatim (line offset +15 from V3 §5 insertion; content byte-equivalent to V2). |
| `alpha-E-candidate-shortlist.md` | §10 cost / caps / telemetry | ACCEPT | ACCEPT | E-2 cap table at :756-760 + reconciliation at :762-770 HELD verbatim (line offset +15 from V3 §5 insertion; content byte-equivalent). |
| `alpha-E-candidate-shortlist.md` | §11 convergence + escalation | ACCEPT | ACCEPT | Unchanged. |
| `DISPATCH-CONTEXT.md` | full | ACCEPT | ACCEPT | STAND from V1 per V2 + V3 addenda. |

## §2 — Critical findings detail (V3)

### §2.1 — F-V3-α-F-1 fold landing (HANDOFF §7 carry-over broadening)

V3 fold prescription from V2 CONSOLIDATED §2.2: "Edit `HANDOFF.md:192-194`
to broaden the audit-falsified carry-over count from 41 to 47."

V3 evidence at `HANDOFF.md:192-197` (post-fold, byte-equivalent to V2
CONSOLIDATED prescription):

> - inherits any of the audit-falsified admit rows (25 CSS + 5 parse_only
>   + 6 direct + 11 typed = **47 rows** under the broader
>   `ROLLING-SOTA-DELTA.md:13-93` ledger; the V1 dispatch §1 narrower
>   bind of 4 direct + 7 typed is a strict subset per `SYNTHESIS.md
>   §1.3` reconciliation) as carry-over without fresh material
>   differential under rebound comparator;

**CH4 cost-surface analysis.** The 6-row broadening (47 − 41 = +6 rows;
+2 direct + +4 typed) widens HANDOFF §7's refusal guard but does not
widen C-5's revert scope. Per E-10 (`alpha-E-candidate-shortlist.md
§7` C-5 narrative), C-5's REDRESS scribe contract names 29 row-keyed
entries (5 W14 + 23 SK-V13 CSS + 1 SK-V12 W1b). The 6 newly-guarded
extension rows are PRE-EXISTING SK-V13 admits routed through PRUNE-1
+ PRUNE-2 — they were already in the audit-falsified ledger; only the
HANDOFF §7 enumeration was narrower than the SYNTHESIS §1.3 ledger.
The V3 fold closes the desync without altering C-5's row-keyed
contract or its 250–500 LOC delete-heavy envelope.

**Same-wave consumer rule preserved.** HANDOFF §7's refusal list is a
gate consumed by S-P3 wave authors; the gate now consistently rejects
the 47-row carry-over set the SYNTHESIS §1.3 honest baseline names.
No new primitive is introduced by the fold; no consumer rebinding is
required.

**Cap discipline preserved.** The F-V3-α-F-1 fold is a single-line
documentary edit per V2 CONSOLIDATED §2.2 ("Hard cap: ≈ 5 min"); it
respects the addendum's narrow-fold cap and required no cap escalation.

**Disposition: ACCEPT.** Zero cost-surface impact; zero envelope drift;
zero cap-discipline drift; zero same-wave-consumer rule weakening.

### §2.2 — F-V3-α-E-1 fold landing (α-E §5 C-3 derived enumeration)

V3 fold prescription from V2 CONSOLIDATED §2.1: "Replace the shell-loop
literal at `alpha-E-candidate-shortlist.md:362-365` with a derived-list
form sourced from `workspace.metadata.bbnf.grammars`."

V3 evidence at `alpha-E-candidate-shortlist.md:362-387` (post-fold;
~15 LOC inserted; content matches V2 CONSOLIDATED prescription's
"Recommended text" with the addition of a canonical `cargo metadata +
jq` shell form that the prescription left implicit):

> - **Round-trip (core tree, all rostered grammars).** For each grammar
>   name `<g>` enumerated under `workspace.metadata.bbnf.grammars` in
>   the top-level `Cargo.toml` (currently `{json, css_l4, google_sheets,
>   bbnf, csv, ebnf, bnf, math}` — the list is metadata-derived, not
>   source-of-truth at the gate site; the canonical shell form is `for g
>   in $(cargo metadata --format-version 1 | jq -r
>   '.workspace_metadata.bbnf.grammars | keys[]'); do rm -rf
>   "crates/core/src/runtime/${g}/" && cargo xtask "regen-${g}" && git
>   diff -- "crates/core/src/runtime/${g}/" || exit 1; done`)

**CH4 cost-surface analysis.** The fold inserts ~15 LOC of gate-spec
prose at the doc surface — it does not modify the C-3 implementation
envelope. C-3's LOC budget at `alpha-E-candidate-shortlist.md:410-413`
remains 1.2k – 2.0k decomposed as: ≈ 400-600 LOC xtask + harness; ≈
200 LOC generic codegen; ≈ 400-600 LOC corpora vendoring + provenance
+ fetch script; ≈ 200 LOC bench harness extension. The xtask itself
already consumes `cargo metadata` for workspace introspection at
canonical implementation sites; the round-trip CI loop is a single
shell incantation that does not consume LOC budget within C-3's
implementation surface. The V3 fold is documentation precision, not
code-surface expansion.

**LOC envelope arithmetic re-check.** C-1 (2.8k–3.4k) + C-2 (0.6k–
1.08k) + C-3 (1.2k–2.0k) + C-4 (0.8k–1.4k) + C-5 (0.25k–0.5k) =
5.65k–8.38k. The V3 fold does not alter this total; SYNTHESIS §3
total at :277 and α-E §2 total at :89 both still read 5.65k–8.38k
verbatim. Zero envelope drift.

**Cap discipline preserved.** The F-V3-α-E-1 fold is a single-paragraph
language tightening per V2 CONSOLIDATED §2.1 ("Hard cap: ≈ 5 min");
the +15 LOC is well inside the narrow-fold cap and required no cap
escalation. The CONSOLIDATED §2.3 sequencing (F-V3-α-E-1 + F-V3-α-F-1
parallelisable; non-overlapping files) was honoured by the atomic
two-file commit `5e2ae78b4` per the V2 attribution discipline.

**Same-wave consumer rule.** The gate's grammar enumeration now
derives from the same workspace-metadata clause that C-1's forward
invariant binds (`alpha-E-candidate-shortlist.md:170-176` cited
in-line in the fold prose); both gates (C-1 forward invariant + C-3
round-trip) share substrate per the V2 CONSOLIDATED §2.1 "parity-with-
C-1's-E-7 invariant" rationale. The C-3 same-wave consumer at
`alpha-E-candidate-shortlist.md §3` (runtime regenerated from the 15
`.bbnf` files in the same wave; bench rows wired to the new corpora)
is unchanged; the V3 fold sharpens the gate's forward-discipline catch
without weakening or relocating the consumer rule.

**LOC envelope check at gate-site granularity.** The α-E §5 narrative
total grew from ~50 LOC of gate-spec prose (V2) to ~65 LOC (V3); the
α-E document grew 800 → 815 lines per the commit message. This is
prose growth at the spec layer, not implementation growth — C-3's
xtask binary, harness, codegen wiring, corpora vendoring, and bench
extension surfaces are not affected. The `[generated-size-budget]`
discipline at `SYNTHESIS.md:326-329` binds wave LOC, not spec LOC; no
> 20 % escalation trigger fires.

**Disposition: ACCEPT.** The α-E V3 fold added ~15 LOC inside the
existing C-3 §5 scope per the orchestrator's directive; no LOC envelope
drift; no risk-class shift; no cap-discipline shift; no same-wave-
consumer rule weakening.

### §2.3 — Fresh-finding scan (V3)

Per V2 addendum §1.2 (inherited): "Look for issues the V1 lens did NOT
catch." V3 extension: look also for issues the V2 lens did not catch
and issues the V3 micro-fold introduced. COST-lens fresh scan executed
over the V3 artefacts in entirety, with emphasis on the F-V3-α-E-1 +
F-V3-α-F-1 fold-touched surfaces, the LOC envelope arithmetic across
the new line numbering, the cap-discipline table re-anchoring, and the
A-3 c/B → C-2 envelope routing.

**No new REJECT.**
**No new REVISE.**

Fresh-finding notes (informational only; below the lens action
threshold; preserved from V2 with V3 re-validation):

- **N-1 (informational; V3-revalidated).** SYNTHESIS §3 total envelope
  reads "≈ 5.65k – 8.38k" at `SYNTHESIS.md:277`; α-E §2 reports
  "≈ 5.65k – 8.38k" at `alpha-E-candidate-shortlist.md:89-92`.
  Arithmetic check: C-1 (2.8k–3.4k) + C-2 (0.6k–1.08k) + C-3 (1.2k–
  2.0k) + C-4 (0.8k–1.4k) + C-5 (0.25k–0.5k) = 5.65k–8.38k. The V3
  fold did NOT alter any per-candidate envelope; arithmetic still
  matches exactly. No drift.

- **N-2 (informational; V3-revalidated).** C-3 LOC envelope (1.2k–
  2.0k) at `alpha-E-candidate-shortlist.md:410-413` is UNCHANGED after
  the V3 §5 prose insertion. The V3 fold's +15 LOC lives at the spec
  layer; C-3's implementation surface (xtask + harness + generic
  codegen + corpora + bench extension) is unmodified. The
  `[generated-size-budget]` clause binds wave LOC under > 20 %
  escalation; the V3 fold consumes 0 % of C-3's wave envelope.

- **N-3 (informational; V3-revalidated).** A-3 c/B LOC budget at
  `alpha-A-results-extraction.md:296-319` allocates 80–120 LOC routed
  through C-2's existing 600 LOC lower-bound envelope. The bounded
  worst case (600 baseline + 80 Skipper fallback + 120 c/B = 800 LOC)
  remains well inside the 1.08k ceiling. The V3 fold does not touch
  α-A; the V3 HANDOFF §7 broadening does not alter A-3's cost
  routing. Bounded.

- **N-4 (informational; V3-revalidated).** α-E §10 cap table at
  `alpha-E-candidate-shortlist.md:756-760` (line offset +15 from V2's
  `:741-745` per the V3 §5 insertion) preserves the C-1/C-2/C-3/C-5 =
  30 min; C-4 = 45 min discipline verbatim. The cap-discipline
  reconciliation paragraph at `:762-770` (V2: `:747-758`) still cites
  CH4 R3 + the addendum verbatim. The line-number shift is documentary
  navigation only; content is byte-equivalent. No drift.

- **N-5 (informational; V3-revalidated).** HANDOFF §6 cap echo at
  `HANDOFF.md:162-165` reads "research 20 min / plan 15 min / redress
  30 min (45 min only for the addendum-amended decision-engine fold +
  C-4 per CONSOLIDATED §0.5 cap discipline)". HELD verbatim through
  V3; the F-V3-α-F-1 fold at §7 does not perturb §6's framing.

- **N-6 (informational; V3-revalidated).** SYNTHESIS §4 LOC-ceiling
  clause at `:326-329` uses "> 20 %" as the escalation threshold per
  α-F's V2 framing. HELD verbatim through V3. The threshold remains
  α-F's reasonable framing inside the lens's "stated and realistic"
  mandate.

- **N-7 (informational; V3-introduced).** The V3 §5 fold's canonical
  shell form (`cargo metadata --format-version 1 | jq -r
  '.workspace_metadata.bbnf.grammars | keys[]'`) cites the
  `workspace_metadata` JSON path produced by `cargo metadata`'s
  output schema (rather than the `Cargo.toml` source path
  `workspace.metadata`). Both refer to the same workspace-metadata
  clause Lock 14 binds (`LOCKS.md:220`); the JSON-path form is what
  the canonical shell incantation actually parses. The fold is
  internally consistent — the prose names both the source path
  (`workspace.metadata.bbnf.grammars`) and the parsed JSON path
  (`.workspace_metadata.bbnf.grammars`). No drift; below the action
  threshold.

- **N-8 (informational; V3-introduced).** HANDOFF §7 carry-over guard
  now reads "47 rows (25 CSS + 5 parse_only + 6 direct + 11 typed)";
  the SYNTHESIS §1.3 honest baseline at `SYNTHESIS.md:194-209` reads
  the same 47-row total under the broader ledger framing. The §3 ↔ §7
  desync CH3 V2 surfaced is now closed; CH4's interest in this fold
  is bounded — the carry-over guard binds gate behaviour, not LOC, and
  C-5's 29-row scribe contract (the implementation surface) is
  unchanged. No fresh CH4 finding.

## §3 — Recommended folds for V4 (if any)

NONE. CH4 V3 issues no fold recommendations. The cost surface is
rebound across SYNTHESIS, HANDOFF, α-E, α-A from V2; the V3 micro-
folds preserve every binding from V2 verbatim and add no fresh
finding to the COST lens. The two-cycle convergence chain closes at
CH4 V3 with 100 % lens-local rate, contributing the second consecutive
100 % link.

## §4 — Escalation flag

NONE.

The CH4 V3 cycle converges at lens-local 100 % ACCEPT-rate; every V2
ACCEPT decision holds; the two V3 micro-folds (F-V3-α-E-1 + F-V3-α-F-
1) introduce no cost / risk / cap / wave-alignment / same-wave-
consumer regression and surface no fresh REJECT or REVISE under the
CH4 lens. The α-E V3 fold consumed ~15 LOC of gate-spec prose inside
the existing C-3 §5 scope per the orchestrator's directive; no LOC
envelope drift; C-3's 1.2k – 2.0k implementation envelope unchanged;
total envelope 5.65k – 8.38k unchanged. The HANDOFF V3 fold broadened
the §7 carry-over guard count from 41 to 47 rows; the broadening
closes the V2 §3 ↔ §7 desync CH3 surfaced and is routed through C-5's
existing 29-row scribe contract without altering C-5's 250 – 500 LOC
delete-heavy envelope.

The V3 aggregator should consume this CH4 V3 disposition as input to
the CONSOLIDATED V3 verdict; CH4 contributes the second consecutive
100 % link to the two-cycle convergence chain per `ORCHESTRATOR.md
§3Z`. The SK-V14 bracket should LOCK at the V3 aggregator commit per
the V2 CONSOLIDATED §3.2 forecast; G-Alpha auto-signs; the orchestrator
proceeds directly to S-P0 per the SK-V14 ORCHESTRATOR-PROMPT pin.
