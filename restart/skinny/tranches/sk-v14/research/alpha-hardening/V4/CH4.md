# CH4 COST — Pass Alpha V4 Disposition

Lens binding: `restart/prompts/ORCHESTRATOR.md:86` ("LOC budget, risk
class, wave alignment, and hard cap are stated and realistic; same-wave
consumer present per kernel/primitive"). V4 dispatch context inherits
the V2 addendum methodology
(`restart/skinny/tranches/sk-v14/research/alpha-hardening/V2/CHALLENGE-V2-ADDENDUM.md`)
overlaid with the V4 micro-redispatch frame: V3 verdict was
NOT-CONVERGED-V4-REQUIRED at 99.27 % aggregate (2 orphan REVISEs from
CH1 V3 catching V3-fold defects via executable verification, both
outside the CH4 lens scope per `V3/HARDENING-ALPHA-V3-CONSOLIDATED.md
:37 + :411`). V4 commit `5e00b6d27` landed two surgical edits totalling
11 ins / 11 del; V3 baseline:
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V3/CH4.md`
(100 % ACCEPT; 0 REJECT; 0 REVISE; 8 informational notes below the
action threshold).

## §0 — V4 Disposition summary

- **V4 ACCEPT-rate: 100 % (34 / 34 sectioned dispositions).**
- **V4 REJECT count: 0.** The V2 + V3 baselines (zero REJECTs) hold
  across every CH4-relevant cell of every artefact; the V4 surgical
  edits introduce no fresh REJECT surface, no LOC / risk / cap / wave-
  alignment / same-wave-consumer regression.
- **V4 REVISE count: 0.** The V2 + V3 baselines (zero REVISEs) hold;
  the V4 fresh-finding scan over the F-V4-α-E-1 + F-V4-α-F-1 surfaces
  surfaces no fresh REVISE under CH4 scope. The two V3 orphan REVISEs
  (CH1 REV-1, CH1 REV-2) were CH1 lens-domain (executable verification
  of a shell command + citation-anchor correctness); both are outside
  the CH4 cost-surface scope.
- **Critical findings: 0.** Cost-surface rebind from V2 + V3 is
  preserved verbatim across SYNTHESIS §3 + §4, HANDOFF §6 + §7, α-E §2
  / §5 / §6 / §9 / §10, α-A §5; F-V4-α-E-1 + F-V4-α-F-1 are
  line-bounded surgical edits within previously-bounded surfaces with
  zero LOC envelope drift (alpha-E net 0 lines; HANDOFF net 0 lines
  per the V4 commit `--numstat` 10/10 + 1/1).
- **CH4 V4 verdict: CONVERGES.** Lens-local 100 % per the V3 +
  CONSOLIDATED §3.1 forecast (`HARDENING-ALPHA-V3-CONSOLIDATED.md
  :411` "CH4 100.00 % (unchanged)"). CH4 contributes the third
  consecutive 100 % link to the two-cycle convergence chain
  (V2 → V3 → V4 all CH4-clean).

### §0.1 — V2 / V3 → V4 baseline-hold verification

| Origin fold | Fold ID | V3 site (V3 §0.1) | V4 site after V4 micro-fold | Status |
|---|---|---|---|---|
| V2 R1 — SYNTHESIS §3 risk column | F-3 | `SYNTHESIS.md:271-275` | `SYNTHESIS.md:271-275` (C-1 VERY HIGH; C-2 HIGH; C-3 HIGH; C-4 VERY HIGH; C-5 MED-LOW) | **HELD** (line offsets unchanged; SYNTHESIS STANDS at V4) |
| V2 R2 — SYNTHESIS §3 LOC envelope column | F-4 | `SYNTHESIS.md:269` header + `:271-275` per-row + `:277` total | `SYNTHESIS.md:269` + `:271-275` + `:277` total ≈ 5.65k–8.38k | **HELD** (verbatim) |
| V2 R3 — α-E §10 hard-cap revert | E-2 | `alpha-E-candidate-shortlist.md:756-760` + `:762-770` | `alpha-E-candidate-shortlist.md:756-760` + `:762-770` (C-1/C-2/C-3/C-5 = 30 min; C-4 = 45 min) | **HELD** (line offset 0; V4 net-0 fold preserves §10 line anchors) |
| V1 — C-1 LOC lower-bound 2.8k | E-11 | `alpha-E-candidate-shortlist.md:83` + `:89-92` | `alpha-E-candidate-shortlist.md:83` (2.8k–3.4k) + `:89-92` envelope rationale | **HELD** |
| V2 — C-2 LOC ceiling +80 LOC Skipper | E-12 | `alpha-E-candidate-shortlist.md:84` + `:268-274` | `alpha-E-candidate-shortlist.md:84` (600–1.08k) + `:268-274` Skipper fallback paragraph | **HELD** |
| V3 — §9 vs §6 C-1↔C-4 sequencing | E-13 | `alpha-E-candidate-shortlist.md:729` + `:731-741` | `alpha-E-candidate-shortlist.md:729` matrix row (C-4 serialises after C-1 ALL sub-waves) + `:731-741` reconciliation paragraph | **HELD** (V4 net-0 keeps line anchors stable) |
| V4 — SYNTHESIS §3 same-wave consumer column | F-5 | `SYNTHESIS.md:269` header + `:271-275` per-row | `SYNTHESIS.md:269` header includes "Same-wave consumer"; `:271-275` per-row values | **HELD** |
| V5 — HANDOFF §6 hard-cap echo | F-7 | `HANDOFF.md:162-165` | `HANDOFF.md:162-165` (30-min lens-agent cap; 20/15/30-or-45 R/P/R cadence with C-4 carve-out) | **HELD** (V4 HANDOFF edit was §7 only; §6 untouched) |
| V6 — α-A §5 c/B LOC budget | A-3 | `alpha-A-results-extraction.md:296-319` | `alpha-A-results-extraction.md:296-319` (80–120 LOC; carry-with-C-2; same-wave consumer rule preserved) | **HELD** (α-A STANDS at V4) |
| V7 — SYNTHESIS §4 per-wave LOC ceiling | F-6 | `SYNTHESIS.md:326-329` | `SYNTHESIS.md:326-329` (per-candidate envelope inheritance; > 20 % escalation per `[generated-size-budget]`) | **HELD** |
| V3 — F-V3-α-E-1 round-trip gate prose | F-V3-α-E-1 | `alpha-E-candidate-shortlist.md:362-387` (V3 inserted +15 LOC of spec prose) | `alpha-E-candidate-shortlist.md:362-387` (same line range; the V4 edit replaces the broken jq incantation in-place with the executable-verified form; spec scope unchanged) | **HELD** at line range; semantic content of the gate (derived enumeration from `workspace.metadata.bbnf.grammars` for cross-grammar recurrence-vector closure) preserved verbatim; only the shell incantation's schema path was corrected. See §2.2 below. |
| V3 — F-V3-α-F-1 carry-over guard broadening | F-V3-α-F-1 | `HANDOFF.md:192-197` (V3 broadened 41 → 47 rows) | `HANDOFF.md:192-197` (same line range; the V4 edit re-anchors the cross-reference from `§1.3` to `§0.2` lines 73-84; the 47-row guard count and broader-ledger framing are byte-equivalent) | **HELD** at line range and 47-row content; only the citation anchor was repaired. See §2.1 below. |

Roll-up: **12 / 12 V2 + V3 CH4 folds HELD under V4 micro-fold pressure.**
Zero regression; zero envelope drift; zero cap-discipline drift; zero
same-wave-consumer rule weakening. No V2/V3 fold line offsets shifted —
V4 commit's 10-ins/10-del on alpha-E and 1-ins/1-del on HANDOFF leave
every anchored line citation in V3/CH4 still valid in V4.

### §0.2 — V4 micro-fold cost-surface delta

| V4 fold | Owner artefact | Site | LOC delta | Cost-surface impact |
|---|---|---|---:|---|
| F-V4-α-E-1 | α-E §5 (C-3 round-trip gate) | `alpha-E-candidate-shortlist.md:362-387` (10 ins / 10 del; net 0) | 0 LOC net | C-3 implementation envelope 1.2k – 2.0k UNCHANGED. The V4 edit corrects the shell command's schema path (V3 used the wrong `.workspace_metadata...|keys[]` against `cargo metadata`'s actual JSON shape; V4 corrects to `--no-deps` + `.metadata.bbnf.grammars[].ident`), adds `git diff --exit-code` to fail-loud on non-empty diff, and drops the stale "currently {json,css_l4,…}" parenthetical so the derived enumeration is the single source of truth. The gate's intent (derived from `workspace.metadata.bbnf.grammars`; grammar-neutral; cross-grammar recurrence-vector closure) is preserved verbatim. The "9th grammar" forward example is generalised to "admitting an additional grammar" — roster-count-agnostic phrasing tightens forward discipline without altering scope. No change to C-3's xtask binary, harness, codegen wiring, corpora vendoring, or bench harness extension surfaces. |
| F-V4-α-F-1 | α-F (HANDOFF §7) | `HANDOFF.md:192-197` (1 ins / 1 del; net 0) | 0 LOC net | Cost-surface unchanged. The V4 edit re-anchors the cross-reference from `SYNTHESIS.md §1.3` to `SYNTHESIS.md §0.2 lines 73-84` — V3's `§1.3` pointed at the post-PRUNE rolling delta (0/17) which does not carry the 47-row breakdown; V4's `§0.2` lines 73-84 names the actual dispatch-vs-ledger reconciliation block carrying the per-row breakdown. The 47-row guard count, the broader-ledger framing, the strict-subset relationship with the V1 dispatch §1 narrower bind, and C-5's 29-row scribe contract envelope (250–500 LOC delete-heavy) are all unchanged. Citation precision repair only. |

Total V4 fold delta: 0 LOC net change to any document; zero LOC delta
to any C-1 / C-2 / C-3 / C-4 / C-5 implementation envelope. Total
envelope 5.65k – 8.38k HELD verbatim. Per the V4 commit message
"docs(sk-v14-alpha): V4 micro-redispatch — V3 fold defects repaired",
both edits are CH1-domain remediations (executable verification +
citation precision) routed through the same CH4-bounded surfaces V3
already cleared; CH4 inherits zero cost-surface change.

## §1 — Per-artefact disposition table (V4)

V4 disposition retains every V2 + V3 ACCEPT decision; the two V4-touched
sites (α-E §5 shell command repair; HANDOFF §7 citation anchor repair)
are re-evaluated below. Every other cell carries its V3 ACCEPT through.

| Artefact | § | V3 disposition | V4 disposition | Reason |
|---|---|---|---|---|
| `SYNTHESIS.md` | §0.1 Close condition (R10) | ACCEPT | ACCEPT | Unchanged (SYNTHESIS STANDS at V4). |
| `SYNTHESIS.md` | §0.2 Goalset row enumeration | ACCEPT | ACCEPT | Unchanged; F-1 reconciliation paragraph at `SYNTHESIS.md:82-90` HELD. |
| `SYNTHESIS.md` | §0.3 R-target acceptance | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §0.4 P-1 … P-7 pre-blocks | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §0.5 Wave-by-wave gates deferred | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §1.1 Survives pillars | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §1.2 Falsified items | ACCEPT | ACCEPT | Unchanged. The HANDOFF §7 V4 citation anchor repair now points at SYNTHESIS §0.2 (the dispatch-vs-ledger reconciliation block); §1.2's content is unaffected. |
| `SYNTHESIS.md` | §1.3 Honest rolling delta | ACCEPT | ACCEPT | Unchanged. §1.3 still carries the rolling delta semantics; the V4 HANDOFF edit moves the citation TARGET away from §1.3 (so §7's quote and §1.3's content no longer need to align) but §1.3 itself is unmodified. |
| `SYNTHESIS.md` | §2 Telemetry binding | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §3 Candidate shortlist | ACCEPT | ACCEPT | F-3 + F-4 + F-5 all HELD verbatim at `:269` + `:271-275` + `:277`; risk + LOC + same-wave consumer columns intact; total envelope ≈ 5.65k–8.38k HELD. |
| `SYNTHESIS.md` | §4 S-P3 constraints | ACCEPT | ACCEPT | F-6 LOC-ceiling clause at `:326-329` HELD; `> 20 %` escalation discipline preserved. |
| `SYNTHESIS.md` | §5 Pre-blocked / unblocked routes | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §6 Close posture | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §1 Bracket verdict | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §2 Authority list | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §3 Honest baseline summary | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §4 Pre-S-P0 readiness | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §5 Pass sequence | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §6 Next-move | ACCEPT | ACCEPT | F-7 cap-echo at `:162-165` HELD verbatim; G-Omega restoration at `:159-160` HELD. V4 edit touched §7 only. |
| `HANDOFF.md` | §7 Refusal conditions | ACCEPT | **ACCEPT** | F-V4-α-F-1 citation anchor repair at `:195-196` re-anchors the cross-reference from `§1.3` to `§0.2 lines 73-84`. The 47-row guard count, broader-ledger framing, and C-5 routing through the 29-row scribe contract are all preserved verbatim. Citation precision repair only; zero cost-surface impact. See §2.1 below. |
| `HANDOFF.md` | §8 V1 disposition | ACCEPT | ACCEPT | Unchanged. |
| `alpha-A-results-extraction.md` | §1–§4 per-plane row tables | ACCEPT | ACCEPT | Unchanged (α-A STANDS at V4). |
| `alpha-A-results-extraction.md` | §5 c/B telemetry | ACCEPT | ACCEPT | A-3 c/B LOC budget at `:296-319` HELD; 80–120 LOC routed through C-2's existing envelope without ceiling raise. |
| `alpha-A-results-extraction.md` | §6 verdict summary | ACCEPT | ACCEPT | Unchanged. |
| `alpha-A-results-extraction.md` | §7 forward pointers | ACCEPT | ACCEPT | Unchanged. |
| `alpha-B-competitor-deltas.md` | §316-320 Skipper absence | ACCEPT | ACCEPT | STAND from V1 per V2 + V3 + V4 addenda; α-B unchanged in V4 commit. |
| `alpha-C-redress-digest.md` | full | ACCEPT | ACCEPT | C-1 (P-7 triple-check) is CH5-domain; CH4 cost-surface unchanged. α-C STANDS at V4. |
| `alpha-D-validated-invalidated.md` | full | ACCEPT | ACCEPT | STAND from V1 per V2 + V3 + V4 addenda. |
| `alpha-E-candidate-shortlist.md` | §2 shortlist table | ACCEPT | ACCEPT | E-11 + E-12 envelopes HELD at `:83-84` (C-1 2.8k–3.4k; C-2 600–1.08k); total `:89-92` ≈ 5.65k–8.38k HELD. |
| `alpha-E-candidate-shortlist.md` | §3 C-1 narrative | ACCEPT | ACCEPT | E-7 forward-invariant + E-11 LOC reconciliation HELD. |
| `alpha-E-candidate-shortlist.md` | §4 C-2 narrative | ACCEPT | ACCEPT | E-12 ceiling raise at `:268-274` HELD verbatim. |
| `alpha-E-candidate-shortlist.md` | §5 C-3 narrative | ACCEPT | **ACCEPT** | F-V4-α-E-1 repairs the V3 shell incantation in-place at `:362-387` (corrected jq schema path + `--no-deps` + `--exit-code`; dropped stale 8-grammar parenthetical; generalised "9th grammar" → "admitting an additional grammar"). C-3 LOC budget at `:410-413` (1.2k – 2.0k decomposed as ≈ 400–600 LOC xtask + harness; ≈ 200 LOC generic codegen; ≈ 400–600 LOC corpora vendoring; ≈ 200 LOC bench harness extension) is UNCHANGED — V4 edit corrects gate spec executability, not gate implementation surface. See §2.2 below. |
| `alpha-E-candidate-shortlist.md` | §6 C-4 narrative | ACCEPT | ACCEPT | E-3 Lock-1 triad + E-4 module-path discipline + E-5 pre-wave hot-leaf citation HELD; 800–1.4k envelope UNCHANGED. |
| `alpha-E-candidate-shortlist.md` | §7 C-5 narrative | ACCEPT | ACCEPT | E-10 29-row scribe-contract count HELD; cost envelope 250–500 (delete-heavy) UNCHANGED. The HANDOFF §7 V4 citation repair does NOT alter C-5's 29-row contract — the row count and routing both preserve verbatim. |
| `alpha-E-candidate-shortlist.md` | §8 pre-blocked routes | ACCEPT | ACCEPT | Unchanged. |
| `alpha-E-candidate-shortlist.md` | §9 concurrency matrix | ACCEPT | ACCEPT | E-13 matrix row at `:729` + reconciliation paragraph at `:731-741` HELD verbatim (V4 net-0 keeps line anchors stable; content byte-equivalent to V3). |
| `alpha-E-candidate-shortlist.md` | §10 cost / caps / telemetry | ACCEPT | ACCEPT | E-2 cap table at `:756-760` + reconciliation at `:762-770` HELD verbatim (V4 net-0 keeps line anchors stable; content byte-equivalent). |
| `alpha-E-candidate-shortlist.md` | §11 convergence + escalation | ACCEPT | ACCEPT | Unchanged. |
| `DISPATCH-CONTEXT.md` | full | ACCEPT | ACCEPT | STAND from V1 per V2 + V3 + V4 addenda. |

## §2 — Critical findings detail (V4)

### §2.1 — F-V4-α-F-1 fold landing (HANDOFF §7 citation anchor repair)

V4 fold prescription from V3 CONSOLIDATED §2.2: "Correct citation
anchor at `HANDOFF:195-196` from `SYNTHESIS.md §1.3` (post-PRUNE
rolling delta 0/17) to `SYNTHESIS.md §0.2 lines 73-84` (the actual
dispatch-vs-ledger reconciliation block carrying the per-row
breakdown)."

V4 evidence at `HANDOFF.md:192-197` (post-fold, per V4 commit
`5e00b6d27`):

> - inherits any of the audit-falsified admit rows (25 CSS + 5
>   parse_only + 6 direct + 11 typed = **47 rows** under the broader
>   `ROLLING-SOTA-DELTA.md:13-93` ledger; the V1 dispatch §1 narrower
>   bind of 4 direct + 7 typed is a strict subset per `SYNTHESIS.md
>   §0.2` reconciliation block (lines 73-84)) as carry-over without
>   fresh material differential under rebound comparator;

**CH4 cost-surface analysis.** The V4 fold is a single-token swap (`§1.3`
→ `§0.2`) plus a "(lines 73-84)" disambiguator. The 47-row guard count,
the broader-ledger framing, the strict-subset relationship, and C-5's
29-row scribe contract (the implementation surface) are all preserved
verbatim. The repair is documentation precision (CH1-domain
correctness); it does not widen, narrow, or relocate any cost-surface
binding.

**LOC envelope.** Zero LOC delta. HANDOFF stays at 245 lines pre/post
V4 (1 ins / 1 del per `git show 5e00b6d27 --numstat`); §7's carry-over
guard text is byte-equivalent at the 47-row count.

**Risk class.** Unchanged. The fold target is HANDOFF §7's refusal
list — a gate consumed by S-P3 wave authors. The 47-row guard binds
gate behaviour against carry-over admits, not LOC; the C-5 cost
envelope (250–500 LOC delete-heavy) is unmoved.

**Same-wave consumer rule preserved.** HANDOFF §7's refusal list is the
gate; C-5's REDRESS scribe contract is the same-wave consumer. The V4
edit does not alter the gate's bound rows or the consumer's row-keyed
contract. No new primitive introduced; no consumer rebinding required.

**Cap discipline preserved.** F-V4-α-F-1 is a single-line documentary
edit per V3 CONSOLIDATED §2.3 ("~3 min cap; HARD CAP: 5 min absolute");
it respects the narrow-fold cap and required no cap escalation.

**Disposition: ACCEPT.** Zero cost-surface impact; zero envelope drift;
zero cap-discipline drift; zero same-wave-consumer rule weakening.

### §2.2 — F-V4-α-E-1 fold landing (α-E §5 shell command + grammar-count repair)

V4 fold prescription from V3 CONSOLIDATED §2.1: "Replace the broken
`cargo metadata | jq` incantation (V3 used the wrong
`.workspace_metadata...|keys[]` returning jq null) with the
schema-correct `.metadata.bbnf.grammars[].ident` form; add `--no-deps`
and `git diff --exit-code`; drop the stale 8-grammar parenthetical;
generalise '9th grammar' to roster-count-agnostic 'admitting an
additional grammar'."

V4 evidence at `alpha-E-candidate-shortlist.md:362-387` (post-fold,
per V4 commit `5e00b6d27`):

> - **Round-trip (core tree, all rostered grammars).** For each grammar
>   name `<g>` enumerated under `workspace.metadata.bbnf.grammars` in
>   the top-level `Cargo.toml` — the list is metadata-derived, not
>   source-of-truth at the gate site; the canonical shell form is `for
>   g in $(cargo metadata --format-version 1 --no-deps | jq -r
>   '.metadata.bbnf.grammars[].ident'); do rm -rf
>   "crates/core/src/runtime/${g}/" && cargo xtask "regen-${g}" && git
>   diff --exit-code -- "crates/core/src/runtime/${g}/" || exit 1; done`:
>   the loop produces empty `git diff` output for every iterated
>   grammar. The gate enumerates from `workspace.metadata.bbnf.grammars`
>   so that admitting an additional grammar requires NO change to the
>   gate's text — only an addition under `workspace.metadata.bbnf.grammars`
>   and a `regen-<g>` xtask registration per C-1's forward invariant
>   (`alpha-E-candidate-shortlist.md:170-176`).

**CH4 cost-surface analysis.** The V4 fold corrects the gate spec's
executability (CH1-domain correctness) without altering the gate's
implementation surface. C-3's LOC budget at `:410-413` remains
1.2k – 2.0k decomposed as: ≈ 400–600 LOC xtask + harness; ≈ 200 LOC
generic codegen; ≈ 400–600 LOC corpora vendoring + provenance + fetch
script; ≈ 200 LOC bench harness extension. The xtask already consumes
`cargo metadata` for workspace introspection at canonical
implementation sites; the round-trip CI loop remains a single shell
incantation that does not consume LOC budget within C-3's
implementation envelope.

**Material change semantics.** The corrected schema path
(`.metadata.bbnf.grammars[].ident` against `cargo metadata
--no-deps`) is the actual JSON shape `cargo metadata` emits; the V3
form was unparseable (yielding jq null). The V4 form is executable per
the V4 commit message ("VERIFIED executable: enumerates 9 grammars
(bbnf, json, css_l4, css_pretty, google_sheets, ebnf, bnf, csv,
math) against live workspace"). The "9 grammars" count is the live
workspace state; the V3 documentation parenthetical hard-coded an
incorrect 8-grammar list. The V4 edit drops the parenthetical so the
derived enumeration remains the single source — this is CH7 / Lock 14
discipline (grammar-neutrality preserved by removing the per-grammar
enumeration from the prose) and CH2 generality (the gate works for
any roster count). CH4 inherits the cost-surface invariance: the
spec's intent (derived enumeration; cross-grammar recurrence-vector
closure; forward-discipline catch at gate-authoring time) is unmoved.

**LOC envelope arithmetic re-check.** C-1 (2.8k–3.4k) + C-2
(0.6k–1.08k) + C-3 (1.2k–2.0k) + C-4 (0.8k–1.4k) + C-5
(0.25k–0.5k) = 5.65k–8.38k. The V4 fold does not alter this total;
SYNTHESIS §3 total at `:277` and α-E §2 total at `:89` both still
read 5.65k–8.38k verbatim. Zero envelope drift.

**Cap discipline preserved.** F-V4-α-E-1 is a single-paragraph language
repair per V3 CONSOLIDATED §2.3 ("≈ 10 min cap; HARD CAP: 15 min
absolute"); the 10 ins / 10 del edit is well inside the narrow-fold
cap. No cap escalation required.

**Same-wave consumer rule.** The gate's grammar enumeration still
derives from the same workspace-metadata clause that C-1's forward
invariant binds (`alpha-E-candidate-shortlist.md:170-176` cited
in-line in the fold prose); both gates (C-1 forward invariant + C-3
round-trip) share substrate. The C-3 same-wave consumer (runtime
regenerated from the 15 `.bbnf` files in the same wave; bench rows
wired to the new corpora) is unchanged; the V4 repair sharpens the
gate's executability without weakening or relocating the consumer
rule.

**Wave-alignment.** The α-E §9 concurrency matrix at `:729` and the
§10 cap table at `:756-760` are both untouched by the V4 fold; line
anchors are stable (V4 net-0 on alpha-E). C-3's parallelisability with
C-2 and C-5 (Wave Zero) is preserved; C-3's serialisation behaviour
with C-1 CSS sub-waves and C-4 is preserved.

**LOC envelope check at gate-site granularity.** The α-E §5 narrative
held its line count exactly (10 ins / 10 del at `:362-387`); the α-E
document remains 815 lines. This is a byte-level repair at the spec
layer with no growth, not implementation growth — C-3's xtask binary,
harness, codegen wiring, corpora vendoring, and bench extension
surfaces are not affected. The `[generated-size-budget]` discipline at
`SYNTHESIS.md:326-329` binds wave LOC, not spec LOC; no `> 20 %`
escalation trigger fires.

**Disposition: ACCEPT.** Zero cost-surface impact; zero envelope drift;
zero risk-class shift; zero cap-discipline shift; zero
same-wave-consumer rule weakening. The repair improves gate spec
executability (CH1-domain) inside the previously-bounded C-3 §5
surface.

### §2.3 — Fresh-finding scan (V4)

Per V2 addendum §1.2 (inherited through V3 and V4): "Look for issues
the prior lens did NOT catch." V4 extension: look also for issues the
V4 micro-fold introduced. COST-lens fresh scan executed over the V4
artefacts in entirety, with emphasis on the F-V4-α-E-1 + F-V4-α-F-1
fold-touched surfaces, the cap-discipline anchoring (line-anchor
stability under V4 net-0 fold), the cross-reference repair semantics
(§1.3 → §0.2 routing), the live grammar-count change in α-E §5 (8 → 9
on live workspace), and the cost-surface routing for C-3 + C-5 under
the repaired gates.

**No new REJECT.**
**No new REVISE.**

Fresh-finding notes (informational only; below the lens action
threshold; V3 notes re-validated under V4 + 2 V4-introduced):

- **N-1 (informational; V4-revalidated).** SYNTHESIS §3 total
  envelope reads "≈ 5.65k – 8.38k" at `SYNTHESIS.md:277`; α-E §2
  reports "≈ 5.65k – 8.38k" at `alpha-E-candidate-shortlist.md:89-92`.
  Arithmetic check: C-1 (2.8k–3.4k) + C-2 (0.6k–1.08k) + C-3
  (1.2k–2.0k) + C-4 (0.8k–1.4k) + C-5 (0.25k–0.5k) = 5.65k–8.38k.
  The V4 fold did NOT alter any per-candidate envelope; arithmetic
  still matches exactly. No drift.

- **N-2 (informational; V4-revalidated).** C-3 LOC envelope
  (1.2k–2.0k) at `alpha-E-candidate-shortlist.md:410-413` is
  UNCHANGED after the V4 §5 shell repair. The V4 fold's 10 ins / 10
  del lives at the spec layer (the corrected shell incantation lands
  on the same lines V3 used); C-3's implementation surface (xtask +
  harness + generic codegen + corpora + bench extension) is
  unmodified. The `[generated-size-budget]` clause binds wave LOC
  under > 20 % escalation; the V4 fold consumes 0 % of C-3's wave
  envelope.

- **N-3 (informational; V4-revalidated).** A-3 c/B LOC budget at
  `alpha-A-results-extraction.md:296-319` allocates 80–120 LOC routed
  through C-2's existing 600 LOC lower-bound envelope. The bounded
  worst case (600 baseline + 80 Skipper fallback + 120 c/B = 800 LOC)
  remains well inside the 1.08k ceiling. The V4 fold does not touch
  α-A; the V4 HANDOFF §7 citation repair does not alter A-3's cost
  routing. Bounded.

- **N-4 (informational; V4-revalidated).** α-E §10 cap table at
  `alpha-E-candidate-shortlist.md:756-760` preserves the
  C-1/C-2/C-3/C-5 = 30 min; C-4 = 45 min discipline verbatim. Line
  anchors did NOT shift this cycle (V4 net-0 on alpha-E preserves
  every line offset from V3). The cap-discipline reconciliation
  paragraph at `:762-770` still cites CH4 R3 + the addendum verbatim.
  No drift.

- **N-5 (informational; V4-revalidated).** HANDOFF §6 cap echo at
  `HANDOFF.md:162-165` reads "research 20 min / plan 15 min / redress
  30 min (45 min only for the addendum-amended decision-engine fold
  + C-4 per CONSOLIDATED §0.5 cap discipline)". HELD verbatim through
  V4; the F-V4-α-F-1 fold at §7 does not perturb §6's framing.

- **N-6 (informational; V4-revalidated).** SYNTHESIS §4 LOC-ceiling
  clause at `:326-329` uses "> 20 %" as the escalation threshold per
  α-F's V2 framing. HELD verbatim through V4. The threshold remains
  α-F's reasonable framing inside the lens's "stated and realistic"
  mandate.

- **N-7 (informational; V4-CLOSED).** V3's N-7 note flagged the V3
  fold's split-path naming (`workspace.metadata.bbnf.grammars` source
  path vs `.workspace_metadata.bbnf.grammars` JSON path) as
  internally consistent but worth tracking. V4 closes this: the V4
  repair lands the canonical JSON path `.metadata.bbnf.grammars[].ident`
  (the actual `cargo metadata` schema, not the `Cargo.toml` source
  schema), and the prose remains consistent — the source-path name
  `workspace.metadata.bbnf.grammars` is the `Cargo.toml` clause; the
  parsed JSON path `.metadata.bbnf.grammars` is the `cargo metadata`
  emission. Both refer to the same Lock 14 clause. N-7's tracking
  concern is resolved.

- **N-8 (informational; V4-revalidated; partial close).** V3's N-8
  flagged the HANDOFF §7 47-row carry-over guard and the SYNTHESIS
  ledger as desync-closed under V3. The V4 citation anchor repair
  re-targets §7's cross-reference from §1.3 to §0.2 lines 73-84,
  pointing at the actual dispatch-vs-ledger reconciliation block
  (V3's §1.3 anchor pointed at the post-PRUNE rolling delta which
  does NOT carry the 47-row breakdown). The repair improves the
  cross-reference's CH1-domain accuracy without altering the
  47-row count or the carry-over guard's scope. CH4's interest is
  bounded (citation precision is CH1, not CH4); the C-5 29-row
  scribe contract remains unchanged.

- **N-9 (informational; V4-introduced).** The V4 fold dropped the
  V3 prose parenthetical "currently `{json, css_l4, google_sheets,
  bbnf, csv, ebnf, bnf, math}`" (8 grammars). The live workspace
  carries 9 grammars (per V4 commit message: bbnf, json, css_l4,
  css_pretty, google_sheets, ebnf, bnf, csv, math). The V4 edit
  reaches Lock 14 + CH2 / CH7 cost-discipline alignment by deleting
  the hard-coded enumeration entirely — the gate now derives the
  list at runtime. This is the cleanest cost-surface posture for the
  gate spec: zero per-grammar prose growth as the roster grows; the
  forward-discipline catch happens at gate-authoring time via the
  derived enumeration, not at every grammar admission via prose
  patching. CH4 records this as a favourable cost-surface property
  introduced by V4, well below the action threshold.

- **N-10 (informational; V4-introduced).** The V4 commit's
  `--numstat` output (10 ins / 10 del on alpha-E; 1 ins / 1 del on
  HANDOFF) confirms net-0 LOC delta on both files. The dispatch
  text noted "α-E V4 net -1 lines; HANDOFF V4 net 0 lines" as the
  expected envelope; the actual net is 0 / 0 (the 11+/11- accounting
  in the commit stat header sums symmetrically). This is documentary
  precision (the dispatch's "-1" was an over-estimate by one); the
  CH4 interest is satisfied either way — the surgical V4 edits
  introduce no LOC drift. Bounded.

## §3 — Recommended folds for V5 (if any)

NONE. CH4 V4 issues no fold recommendations. The cost surface is
rebound across SYNTHESIS, HANDOFF, α-E, α-A from V2 + V3; the V4
micro-folds preserve every binding from V3 verbatim (line anchors
stable; envelope arithmetic unchanged; cap discipline unmoved;
same-wave consumer rule preserved) and add no fresh finding to the
COST lens. The three-cycle ACCEPT chain (V2 + V3 + V4 all CH4-clean)
satisfies the two-consecutive-cycle convergence rule
(`ORCHESTRATOR.md §3Z`) for the COST lens with margin; V5 will
confirm the V4 surface without CH4 perturbation expected.

## §4 — Escalation flag

NONE.

The CH4 V4 cycle converges at lens-local 100 % ACCEPT-rate; every V2
+ V3 ACCEPT decision holds; the two V4 micro-folds (F-V4-α-E-1 +
F-V4-α-F-1) introduce no cost / risk / cap / wave-alignment /
same-wave-consumer regression and surface no fresh REJECT or REVISE
under the CH4 lens. The α-E V4 fold consumed 10 ins / 10 del = net 0
LOC inside the existing C-3 §5 surface to repair the V3 shell
incantation's schema correctness (CH1-domain); no LOC envelope
drift; C-3's 1.2k – 2.0k implementation envelope unchanged; total
envelope 5.65k – 8.38k unchanged. The HANDOFF V4 fold lands 1 ins /
1 del = net 0 LOC to re-anchor the §7 cross-reference from §1.3 to
§0.2 lines 73-84 (citation precision repair, CH1-domain); the
47-row guard count and C-5's 29-row scribe contract are unchanged.

The V4 aggregator should consume this CH4 V4 disposition as input to
the CONSOLIDATED V4 verdict; CH4 contributes the third consecutive
100 % link to the convergence chain per `ORCHESTRATOR.md §3Z`. The
SK-V14 bracket close depends on cross-lens convergence (CH1's V4
re-verification of the repaired shell + citation is the binding
gate per the V3 orphan-REVISE source); CH4 lens-local convergence is
unconditional and contributes no blocker to the V4 / V5 close path.
