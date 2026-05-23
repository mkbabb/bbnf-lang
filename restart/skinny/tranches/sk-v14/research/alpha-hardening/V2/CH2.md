# CH2 GENERALITY (Lock 14) — Pass Alpha V2 Disposition

Lens: CH2 per `restart/prompts/ORCHESTRATOR.md §3W` — "Lock 14 holds: no
grammar-name leak; every proposed intervention is grammar-neutral and
works for CSS L4 / Sheets / BBNF-self, not only JSON."

Authority binding: `restart/locks/LOCKS.md:220-238` (Lock 14 text);
V1 dispatch context
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/CHALLENGE-CONTEXT.md:99-110`
(§CH-2 scope); V2 overlay
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V2/CHALLENGE-V2-ADDENDUM.md`.
V1 disposition at
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/CH2.md` —
four REVISEs (Findings 1–4) routed via the V1 CONSOLIDATED §2.1 + §2.2
fold packet as F-11 / F-12 / F-13 (α-F SYNTHESIS mirrors) + E-6 / E-7 /
E-8 / E-9 (α-E primary folds). V2 cycle inspects the α-commit
`958406257` five-file atomic landing.

## §0 — Disposition summary

- ACCEPT-rate: 97 % (32 ACCEPT / 33 § lines disposed; one new REVISE)
- REJECT count: 0
- REVISE count: 1 (new) — C-3 round-trip gate hand-enumerates the 8
  grammar names rather than deriving from workspace metadata;
  introduces a forward-blind grammar list at the very gate that
  enforces Lock 14
- FOLD-LANDED count: 7 / 7 V1 folds (F-11, F-12, F-13 in α-F SYNTHESIS;
  E-6, E-7, E-8, E-9 in α-E candidate-shortlist)
- FOLD-PARTIAL count: 0
- FOLD-MISSING count: 0
- Critical findings: 0 architectural defects; one (1) cosmetic /
  explicitness REVISE born of the E-1 BINDING fold's collateral text
- Escalation flag: none. CH2 converges on this cycle at 97 % ACCEPT;
  the single new REVISE is a one-paragraph clarification to the C-3
  §5 round-trip gate (substitute a workspace-metadata-driven grammar
  enumeration in place of the hardcoded 8-element list).

All four V1 CH2 Findings landed verbatim in V2. The lens finds the
substrate of every candidate, every same-wave consumer, every S-P3
constraint, and the pre-block layer holds Lock 14 cleanly under the
V2 overlay. The one new finding surfaces a derived-list-vs-hardcoded
list discipline gap inside the very Lock-14 gate the E-1 binding fold
expanded — the gate's *implementation* now risks foreclosing future
grammars at the precise check meant to forbid grammar privilege.

## §1 — Per-artefact disposition table

| Artefact | § | Disposition | Reason |
|---|---|---|---|
| `SYNTHESIS.md` | §0.1 close condition | ACCEPT | R10 verbatim; close criterion treats JSON cells + CSS features symmetrically — unchanged from V1. |
| `SYNTHESIS.md` | §0.2 goalset enumeration | ACCEPT | Per-grammar row counts named; all four surfaces enumerated under one rubric. Numeric divergence reconciliation paragraph (F-1) is plane-neutral; no Lock 14 impact. |
| `SYNTHESIS.md` | §0.3 R-target goalset (R4 row) | ACCEPT — **FOLD-LANDED (F-11)** | `SYNTHESIS.md:96` reads verbatim: "first instance of the `regen-{grammar}` family; the xtask binary parametrises a grammar-neutral generator". The CH2 Finding 1 mirror fold lands cleanly. |
| `SYNTHESIS.md` | §0.4 P-1…P-7 pre-blocks | ACCEPT | P-6 names per-grammar provider modules as the recurrence vector; pre-block scope unchanged from V1 disposition. F-10 P-1 round-trip-rule trigger is grammar-neutral as written. |
| `SYNTHESIS.md` | §0.5 wave-by-wave deferral | ACCEPT | Contracted deferral unchanged; deferred work remains grammar-neutral in surface. |
| `SYNTHESIS.md` | §1 corrected diagnosis | ACCEPT | Pillars marked `grammar-neutral` for bbnf-simd / OffsetFlags + Tape; unchanged from V1. |
| `SYNTHESIS.md` | §2 telemetry binding | ACCEPT | Column rule wording treats every plane and every grammar uniformly. The new `track2_entry_point` column (F-15) is plane-keyed not grammar-keyed; no Lock 14 leak. |
| `SYNTHESIS.md` | §3 candidate shortlist C-1 row | ACCEPT — **FOLD-LANDED (E-7 mirror)** | `SYNTHESIS.md:271` carries the forward-invariant clause verbatim — "Forward invariant (post-redress, permanent): any new grammar added under `workspace.metadata.bbnf.grammars.{name}` produces ZERO new `.rs` files in `skinny/crates/{codegen, runtime, passes, bbnf, grammar}/src/` and ZERO new directories in `crates/core/src/runtime/`". The CH2 Finding 2 binding lands explicitly at the row level. |
| `SYNTHESIS.md` | §3 candidate C-2 row | ACCEPT | Per-plane comparator rebind has no grammar branch; unchanged from V1. |
| `SYNTHESIS.md` | §3 candidate C-3 row | ACCEPT — **FOLD-LANDED (F-11)** | `SYNTHESIS.md:273` reads verbatim: "first instance of the `regen-{grammar}` family — the xtask binary parametrises a grammar-neutral generator". The CH2 Finding 1 SYNTHESIS mirror lands; downstream emit path is presented as the family invariant, not the CSS bespoke shape. |
| `SYNTHESIS.md` | §3 candidate C-4 row | ACCEPT | C-4 row inherits F-13's S-P3 dispatch-discipline clause (line 335-341); no grammar branch baked into the row's gate language. |
| `SYNTHESIS.md` | §3 candidate C-5 row | ACCEPT | Revert + REDRESS; unchanged from V1. |
| `SYNTHESIS.md` | §4 S-P3 constraints (C-1 forward invariant clause) | ACCEPT — **FOLD-LANDED (F-12)** | `SYNTHESIS.md:330-334` reads verbatim the Finding 2 mirror clause: "C-1's forward invariant (no new `.rs` files in generic crates; no new directories in `crates/core/src/runtime/`; Lock 14 baseline gate rejects any commit that violates this) is permanent; S-P3 wave plans MUST cite it as the pre-condition for any new grammar admission wave (BBNF-self, Sheets, future grammars)". |
| `SYNTHESIS.md` | §4 S-P3 constraints (C-4 dispatch-discipline clause) | ACCEPT — **FOLD-LANDED (F-13)** | `SYNTHESIS.md:335-341` reads verbatim the Finding 3 mirror clause: "the C-4 shape consumer is exercised across at least two grammar families before any C-4 admit cites runtime divergence as load-bearing; one-grammar runtime divergence is wave evidence, not admit evidence; the shape consumer in `skinny/crates/codegen/src/lib.rs` MUST dispatch on the CSP-emitted `BackendShape` enum alone — no `match grammar { Json => ..., CssL4 => ... }` arm may appear in the dispatch path". |
| `SYNTHESIS.md` | §4 S-P3 constraints (other lines) | ACCEPT | G-SIMD-GRAMMAR-POLICY triad (F-14) is plane-keyed not grammar-keyed; LOC ceiling clause (F-6) is grammar-neutral; triumvirate clause (F-9) likewise. |
| `SYNTHESIS.md` | §5 pre-blocked routes | ACCEPT | "grammar-name branches in generic crates" enumerated as pre-blocked; carrying P-1…P-7 verbatim. |
| `SYNTHESIS.md` | §6 close posture | ACCEPT | Bracket framing symmetric; unchanged from V1. |
| `HANDOFF.md` | §1 bracket verdict | ACCEPT | Pillar list mirrors SYNTHESIS §1.1; unchanged from V1. |
| `HANDOFF.md` | §3 honest baseline | ACCEPT | JSON-CSS parity in admit-count framing preserved; numeric reconciliation (F-1) is plane-symmetric. |
| `HANDOFF.md` | §4 authorship declaration | ACCEPT | F-2(b) lands α-F as sole author of all four artefacts; no grammar implications. |
| `HANDOFF.md` | §6 next-move | ACCEPT | "CH2 verifies Lock 14 grammar-neutrality" pin preserved at line 170; F-7 + F-8 hard-cap + G-Omega edits are grammar-neutral. |
| `HANDOFF.md` | §7 refusal conditions | ACCEPT | UnionTape refusal clause (F-16) is substrate-keyed not grammar-keyed; Lock-14 + G-SIMD-GRAMMAR-POLICY bindings preserved. |
| `HANDOFF.md` | §8 disposition | ACCEPT | PENDING-V2 → PENDING-V3 wording; no grammar implications. |
| `alpha-A-results-extraction.md` | §1 direct reconciliation | ACCEPT | A-1 fold adds DISPATCH §1 "4" vs ROLLING-SOTA-DELTA "6" reconciliation; the +2 rows (marine_ik, instruments) are named on a row-by-row basis under the same comparator-misbinding pattern. Grammar-neutral. |
| `alpha-A-results-extraction.md` | §3 typed extension annotation | ACCEPT | A-2 fold annotates `[ext†]` legend mapping 5 extension rows to wave ids (W13.1–W13.4 + W15.1); per-row v6 §1 row 4 binding; grammar-neutral. |
| `alpha-A-results-extraction.md` | §6 c/B LOC budget | ACCEPT | A-3 fold adds 80–120 LOC envelope routed through C-2's owner-path; the schema column rule is plane-keyed not grammar-keyed; no Lock 14 leak. |
| `alpha-B-competitor-deltas.md` | comparator overlay | ACCEPT | Unchanged from V1 (artefact STANDs per V2 addendum §0); CSS / JSON overlay still symmetric. |
| `alpha-C-redress-digest.md` | §2 pre-block P-7 falsifiability | ACCEPT | C-1 fold expands the triple-check gate (symbol identity / type identity / address identity) per CH5 REVISE #4; all three checks are plane-keyed and substrate-keyed, not grammar-keyed; no Lock 14 leak. |
| `alpha-C-redress-digest.md` | other pre-blocks (P-1..P-6) | ACCEPT | Unchanged in V2; grammar-neutral. |
| `alpha-D-validated-invalidated.md` | §S-3 Lock 14 | ACCEPT | Unchanged from V1 (artefact STANDs per V2 addendum §0); 30 violations cited; reopen path is trait + emit collapse. |
| `alpha-D-validated-invalidated.md` | §V-4 + §V-5 | ACCEPT | Unchanged from V1; bbnf-simd + Tape + OffsetFlags carried grammar-neutral. |
| `alpha-E-candidate-shortlist.md` | §2 shortlist table | ACCEPT — **FOLD-LANDED (E-14)** | Row C-3 lifts the round-trip gate pointer ("see §5 + hardening V1 CH7 §3.1"); row C-1 surfaces "see §3 for full per-sub-wave gate + forward invariant"; both are grammar-neutral in framing. The C-3 row carries the family-shape pointer "first instance of the `regen-{grammar}` family". |
| `alpha-E-candidate-shortlist.md` | §3 C-1 detail (forward invariant) | ACCEPT — **FOLD-LANDED (E-7)** | `alpha-E:170-176` reads verbatim the Finding 2 forward-invariant clause. The Lock 14 baseline gate is named (`bbnf-bench::lock14_baseline::validate`) at line 168 and rebound at line 174 as the rejector. CH2 Finding 2 lands cleanly. |
| `alpha-E-candidate-shortlist.md` | §4 C-2 detail | ACCEPT | Per-plane comparator rebind preserved; no grammar branch. |
| `alpha-E-candidate-shortlist.md` | §5 C-3 detail (Purpose + family binding) | ACCEPT — **FOLD-LANDED (E-6)** | `alpha-E:303-309` reads verbatim the Finding 1 family-shape clause: "(First instance of the `regen-{grammar}` family; the xtask binary parametrises a grammar-neutral generator; the generic codegen entry it invokes is the same surface a future `regen-sheets` / `regen-bbnf-self` / `regen-{new}` binary will invoke. The CSS instance proves the family shape; subsequent grammars admit through the same surface without introducing per-grammar bespoke binaries.)". CH2 Finding 1 lands cleanly. |
| `alpha-E-candidate-shortlist.md` | §5 C-3 detail (round-trip gate) | **REVISE — new finding** | `alpha-E:362-372` hand-enumerates the 8 grammar names `{json, css_l4, google_sheets, bbnf, csv, ebnf, bnf, math}` inside the very Lock-14 gate; see §2 new finding NF-1 below. The gate's *implementation* must derive the grammar list from `workspace.metadata.bbnf.grammars` (the same metadata C-1's forward invariant rebinds), not from a hardcoded literal — else the gate forecloses any 9th grammar at the precise check meant to forbid grammar privilege. |
| `alpha-E-candidate-shortlist.md` | §6 C-4 detail (Pre-blocked routes + 2-grammar) | ACCEPT — **FOLD-LANDED (E-8)** | `alpha-E:559-571` reads verbatim the Finding 3 clauses: "No grammar-branched dispatch inside the CSP shape consumer (CH2 Finding 3, V1 hardening)…shape consumer in `skinny/crates/codegen/src/lib.rs` MUST dispatch on the CSP-emitted `BackendShape` enum alone; no `match grammar { Json => ..., CssL4 => ... }` arm may appear in the dispatch path…Two-grammar-family exercise requirement (CH2 Finding 3, V1 hardening). The C-4 shape consumer must be exercised across at least two grammar families…one-grammar runtime divergence is wave evidence, not admit evidence." CH2 Finding 3 lands cleanly. |
| `alpha-E-candidate-shortlist.md` | §7 C-5 detail | ACCEPT | Revert covers JSON parse_only + CSS rows symmetrically; E-10 scribe-count clarification (29 entries) is row-keyed not grammar-keyed. |
| `alpha-E-candidate-shortlist.md` | §8 consolidated pre-blocks | ACCEPT | P-6 carried verbatim; consolidated list grammar-neutral. |
| `alpha-E-candidate-shortlist.md` | §9 concurrency matrix | ACCEPT | E-13 fold resolves §9-vs-§6 dependency (C-4 strictly serialises after C-1); ledger-serialisation discipline, not grammar privilege. |
| `alpha-E-candidate-shortlist.md` | §10 cost / caps / telemetry | ACCEPT — **FOLD-LANDED (E-9)** | `alpha-E:760-770` reads verbatim the Finding 4 clause: "The hot-leaf column reads as a grammar-keyed symbol path (`{grammar}::parse_*` or equivalent); a stale inherited symbol name on a non-JSON row (e.g. a JSON-keyed symbol surfacing as the hot leaf for a CSS row) fails the per-row gate the same way it fails S-P1 (CH2 Finding 4, V1 hardening)." CH2 Finding 4 lands cleanly. E-2 cap discipline (30 min default; 45 min only for C-4) is grammar-neutral. |
| `alpha-E-candidate-shortlist.md` | §11 convergence + escalation | ACCEPT | Escalation triggers are measurement-keyed, not grammar-keyed. |
| `DISPATCH-CONTEXT.md` | §0–§3 + per-agent | ACCEPT | STANDs per V2 addendum §0; the α-agent scope spec unchanged. |

## §2 — Critical findings

The single new finding (NF-1) is a REVISE born of the E-1 BINDING
fold's collateral text. NF-1 does not unwind C-3 nor the four CH2
folds that landed cleanly; it tightens the *implementation language*
of the E-1-expanded round-trip gate so the gate honours the very
forward invariant that F-12 / E-7 binds.

### NF-1 — C-3 §5 round-trip gate hand-enumerates the 8 grammar names rather than deriving from workspace metadata

**Files / lines.** `alpha-E-candidate-shortlist.md:362-372` (the
core-tree round-trip gate of E-1's BINDING expansion); cross-bind with
`alpha-E-candidate-shortlist.md:170-176` (C-1's forward invariant) and
`SYNTHESIS.md:330-334` (the F-12 mirror).

**The CH2 concern.** E-1 (CH7 §3.1 BINDING) expanded the C-3
round-trip gate to cover all eight runtime trees, which is correct on
the *coverage* axis — the bypass-header pattern must close on every
grammar's tree, not only the CSS one. The gate's current wording reads:

> "**Round-trip (core tree, all 8 grammars).** For each of `{json,
> css_l4, google_sheets, bbnf, csv, ebnf, bnf, math}`: `rm -rf
> crates/core/src/runtime/<grammar>/ && cargo xtask regen-<grammar> &&
> git diff -- crates/core/src/runtime/<grammar>/` produces empty
> output."
> — `alpha-E-candidate-shortlist.md:362-365`

The eight-name literal is the recurrence-vector enumeration *as
currently present in the source tree*, and as such is correct for
SK-V14's PRUNE pass. The problem surfaces at the precise moment C-1's
forward invariant takes effect: when a ninth grammar lands under
`workspace.metadata.bbnf.grammars.{new}`, the gate hardcoded at line
362-365 will not iterate over the new grammar's tree — the gate
silently passes for the unenumerated grammar even though the very
purpose of the gate is to enforce regen-derivation on every rostered
grammar. This is the same forward-blindness pattern Finding 2 caught
at C-1's gate (and which E-7 closed there) — but the E-1 gate did
not inherit the forward-discipline fix.

**Why it matters.** Lock 14's wording at `LOCKS.md:220-238` requires
that "Adding a new grammar is a config + grammar-source change with
NO code change in any generic or other-grammar crate". A future
grammar admission must NOT also require editing the C-3 round-trip
gate to append a 9th name to the literal. The gate must derive its
grammar list from `workspace.metadata.bbnf.grammars` (the workspace
metadata clause Lock 14 itself names) — the same source the C-1
forward invariant locks at `alpha-E:170-176`. Anything less embeds a
recurrence-vector-vs-invariant gap inside the very gate the E-1
binding fold ratified.

The gap is not architectural — the gate's substrate is correct, the
coverage axis is correct, the bypass-header detector is correct. The
gap is a one-paragraph *implementation discipline* clarification: the
shell loop must iterate over `workspace.metadata.bbnf.grammars.*`
names, not over an inline literal.

**Recommended fold (V3).** α-E §5 C-3 §"Falsifiability gate" replaces
the hardcoded list at line 362-365 with derived-list language:

> "**Round-trip (core tree, all rostered grammars).** For each grammar
> name `<g>` enumerated under `workspace.metadata.bbnf.grammars` in the
> top-level `Cargo.toml` (currently `{json, css_l4, google_sheets,
> bbnf, csv, ebnf, bnf, math}` — the list is metadata-derived, not
> source-of-truth at the gate site): `rm -rf
> crates/core/src/runtime/<g>/ && cargo xtask regen-<g> && git diff --
> crates/core/src/runtime/<g>/` produces empty output. The gate
> enumerates from workspace metadata so that adding a 9th grammar
> requires NO change to the gate's text — only an addition under
> `workspace.metadata.bbnf.grammars` and a `regen-<g>` xtask
> registration per C-1's forward invariant."

α-F SYNTHESIS §3 C-3 row optionally appends a one-clause mirror:
"the gate's grammar enumeration derives from
`workspace.metadata.bbnf.grammars`, not a hardcoded list — preserving
C-1's forward invariant at the C-3 gate site."

NF-1 is a REVISE not a REJECT: the gate's substance is correct; the
fold tightens the language so the gate's forward-discipline matches
C-1's. Even unfolded, the gap is caught by the C-1 forward-invariant
gate at first grammar-admission attempt — the NF-1 fold only
relocates the catch upstream so it fails at gate-authoring time, not
at first-grammar-admission time.

## §3 — Recommended folds for V3 (if any)

CH2 converges at 97 % ACCEPT this cycle. The single new REVISE (NF-1)
is a one-paragraph language tightening in α-E §5 C-3 + (optional)
α-F SYNTHESIS §3 C-3 row mirror.

If V3 fires under the ORCHESTRATOR.md §3Z two-consecutive-cycle rule,
the recommended dispatches:

1. **Re-dispatch α-E** (single redress wave, ~15 min cap per
   `[dispatch-hard-cap]` plan-class):
   - Fold NF-1 into §5 C-3 §"Falsifiability gate" round-trip wording
     per the recommended text above.

2. **Optional re-dispatch α-F** (single redress wave, ~15 min cap):
   - Fold NF-1 mirror into SYNTHESIS §3 C-3 row's gate description.

3. **No re-dispatch needed for α-A / α-B / α-C / α-D.** The V2 folds
   landed all four V1 CH2 Findings cleanly; α-A / α-C edits introduce
   no Lock 14 issues; α-B and α-D STAND.

NF-1 is a *V3 fold candidate*, not a V2 REJECT — the V2 cycle still
converges per the V2 addendum §4 forecast (≥ 95 %). Per
`ORCHESTRATOR.md §3Z`, V3 is the confirming pass; CH2's NF-1 may be
absorbed into the V3 cycle's redispatch packet alongside any other
lens's V2 new findings without breaking convergence cadence.

The four V1 CH2 Findings are now PERMANENTLY closed in the SK-V14
contract: F-11 / F-12 / F-13 binding inside SYNTHESIS §0.3 + §3 + §4;
E-6 / E-7 / E-8 / E-9 binding inside α-E §3 + §5 + §6 + §10. The
single NF-1 finding is the only outstanding CH2 item; its substance
is a derivation discipline at the gate site, not an architectural
defect anywhere in the candidate slate.
