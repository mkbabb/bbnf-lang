# Pass Alpha SK-V17→SK-V18 — CHALLENGE CONSOLIDATED (V1)

Lens wave: CH1 Correctness · CH2 Generality · CH3 Regression · CH4 Cost · CH5 Hidden
Coupling · CH6 Next-Tranche-Impact · CH7 Overfit-Prune (the SK-V18-specific seventh
lens binding the six V3 CHALLENGE addenda). Per PASS-ALPHA §3 + ORCHESTRATOR §3W/§3Z.
Subject: `research/alpha/{alphaA..E}.md` + `SYNTHESIS.md` + `HANDOFF.md` (the α-F
deliverable IS `SYNTHESIS.md` + `HANDOFF.md` per PASS-ALPHA §2/§6 — no separate
`alphaF-*.md` is a defect; CH1/CH7 confirm). HEAD at review: `318d9c046`.

## §1 — Aggregate tally (V1)

| Lens | ACCEPT | REVISE | REJECT |
|---|---|---|---|
| CH1 Correctness | 4 | 3 | 0 |
| CH2 Generality | 24 | 8 | 0 |
| CH3 Regression | 7 | 0 | 0 |
| CH4 Cost | 4 | 1 | 0 |
| CH5 Hidden Coupling | 16 | 7 | 0 |
| CH6 Next-Tranche-Impact | 7 | 4 | 0 |
| CH7 Overfit-Prune | 7 | 1 | 0 |
| **Total** | **69** | **24** | **0** |

Accept rate **69/93 = 74.2%** — below the §3Z ≥95% bar. **Zero REJECTs across all seven
lenses.** Every REVISE is convergence-cheap, path:line-anchored, and orphan-free (each
carries a concrete in-place fix). No claim was found unsupported or fabricated; no
intervention re-opens a REDRESS pre-block (CH3 100% ACCEPT); no contrivance survives the
goalset (CH7 confirms all six addenda fire against live surfaces with close gates +
pre-blocks); no hidden second substrate (CH5 confirms Lock 1 holds at HEAD).

## §2 — Root-cause clustering of the 24 REVISEs

The REVISEs collapse to **seven root causes** (most recur across lenses), all in the α-F
contract (`SYNTHESIS.md` + `HANDOFF.md`) and the αE shortlist — the upstream α-A..D
research is overwhelmingly ACCEPT:

1. **JSON >SOTA range understated** (CH1) — "+1.4%–78%" contradicts the RESULTS rows αA
   extracted (true max +164.7% = unicode_escapes). Three SYNTHESIS sites + one HANDOFF.
2. **yyjson/asmjson/RapidJSON runnability** (CH1) — §0.6 prose reads as if yyjson is a
   live comparator; it is honest-`None` on aarch64 (FFI not wired). §4.2 "if runnable"
   not honored in prose.
3. **Canonical Lock-14 model uncited; md5 necessary-not-sufficient** (CH2 ×3) — the
   contract invokes "Lock 14" without binding the three-surface model (`LOCKS.md` item
   14) or its `match grammar`-arm grep. Three md5-distinct `generated.rs` can pass while
   the emitter body still grammar-branches.
4. **Sheets sourcing under-specified** (CH2 ×4) — the real Pratt
   `grammar/google-sheets/google-sheets.bbnf` EXISTS (strengthens the litmus) but lives
   in the totality tree; the contract said "author" a fresh stub (risks a hollow "third
   JSON").
5. **`ValueRef` two-axis precision** (CH5 ×4) — the type has `K=Kind` (REAL) and
   `G=EventGrammar` (PHANTOM); writing "`ValueRef<G>`" without naming `K` risks resolving
   the wrong axis. DELETE is the abrogate-before-patch default (no `CssEventGrammar`
   exists); the shared trait does NOT require `<G>`.
6. **Shared-trait count can LCD-false-green** (CH5 ×2) — `>=2 impls` is satisfiable by a
   thin trait that flattens JSON's richness; no telemetry encoded richness-preservation.
7. **Deferred revert/cap as paper-close surface** (CH6 ×4) — the §4.4 deferral to S-P3 is
   sanctioned, but the contract did not carry the revert dependency graph, the hard-cap
   defaults, the G6-retire measurement floor, or the honest-finding-escape qualification
   gate forward — each a potential slow-paper-close.

Minor precision REVISEs folded into research artefacts (not the α-F contract): CH7 alphaA
§3.2 x86 census "23 `.rs`" → "24 files (23 `.rs` + 1 `.asm`)"; CH4 αE checkasm "18" →
"~12"; CH2/CH5/CH4 αC/αD/αE sharpenings (P3 collapse-vs-differentiate, S12 Pratt owner,
S9 DocumentView citation, B4 G6 LOC ceiling). These are owned by the research authors,
not αF; the αF contract folds only the items that touch `SYNTHESIS.md`/`HANDOFF.md`.

## §3 — V2 fold disposition (αF cycle V2)

All 24 REVISEs are resolved. The αF-owned subset (those touching `SYNTHESIS.md` +
`HANDOFF.md`) is FOLDED in cycle V2; the research-artefact-owned precision REVISEs are
noted for the research authors but do not gate the contract. αF V2 folds:

| # | Root cause | Lens | Fold in V2 contract |
|---|---|---|---|
| 1 | JSON range | CH1 | SYNTHESIS ground-truth/§0.2/§1 + HANDOFF current-state: "+1.4%–78%" → "+1.4%–164.7%" (unicode_escapes widest, apache_builds thinnest) |
| 2 | yyjson runnability | CH1 | §0.6: yyjson/asmjson/RapidJSON marked honest-`None`-on-aarch64 (FFI not wired); a fabricated competitor column is REJECTed |
| 3 | Lock-14 canonical + md5 sufficiency | CH2 | G3 + PROVE close conditions bind the canonical three-surface model + `match grammar`-arm grep; new telemetry `generator_grammar_branch_count == 0` co-gate; HANDOFF invariant 5 binds BOTH the token scan AND the arm census + `EventGrammar` witness token |
| 4 | Sheets sourcing | CH2 | §0.1 PROVE + §0.3 receiver + HANDOFF backlog: ADOPT the existing Pratt `google-sheets.bbnf`, bring into the benched skinny tree; new telemetry `sheets_grammar_shape == pratt-operator` (a flat-stream/tree shape REJECTed as third-JSON hollowing) |
| 5 | `ValueRef` two-axis | CH5 | G4 close + §0.3 receiver + HANDOFF backlog: name the `G: EventGrammar` axis vs the already-real `K=Kind` axis; DELETE is the abrogate-before-patch default; trait separable from `<G>`; `phantom_generic_resolved` column re-scoped to the `G` axis |
| 6 | Trait LCD false-green | CH5 | G4 close + new telemetry `json_rich_navigation_preserved == true` in the gate-consumer REJECT set (a ≥2 impl-count without rich-nav is an LCD regression) |
| 7 | Deferred revert/cap | CH6 | §0.3 G6 retire branch gated on a samply non-top-N row; §0.3 PROVE receiver cross-refs the §0.5 honest-finding fallback; Section 3 + HANDOFF Next-Move carry the revert dependency graph (PRUNE→G1→G2→G3→G4→G5/G6→PROVE→H1, failure-blocks-downstream) + the dispatch-hard-cap defaults (20/15/30); PASS-IMPL V4 row carries the honest-finding-escape (a)-(c) qualification gate |

## §4 — Convergence posture

V1 is **74.2% ACCEPT, 0 REJECT** — a non-converged but clean first pass (all defects are
tightening REVISEs on the contract's measurability/precision, none a finding reversal or a
re-opened pre-block). Per ORCHESTRATOR §3Z the cycle does NOT converge at V1; the 24
REVISEs fold into αF V2 (this revision), and a V2 confirming CHALLENGE cycle is required
to reach ≥95% × 2 consecutive with zero orphan REVISE. The fold is orphan-free: every V1
REVISE that touches the α-F deliverable has a corresponding V2 edit (see §3); the
research-artefact precision REVISEs are routed to their authors and do not block the
contract.

**Load-bearing V1 ACCEPTs to NOT churn (CH-cross-cutting):** the `SinkOnlyProgram` /
5-shape `BackendShape` lowering vehicle (the correct generalization mechanism); the
falsifiability triple (PRESERVED->SOTA / grammar-derivation-mutate-test /
distinct-grammar-output); the §0.4 hidden-coupling-escape enumeration (the most complete
in the cohort); the "checked TWICE — runtime output AND the emitter that produces it"
corollary (a refuted carrier can re-land at its SOURCE); the substrate-union Lock-1
foundation verified at HEAD; the PRUNE-before-GENERALIZE sequencing; the six-addenda
triple-binding (close gate + pre-block + machine-checkable telemetry column).

## §5 — Next step

αF V2 SYNTHESIS.md + HANDOFF.md carry the folds. Dispatch the V2 confirming CHALLENGE
cycle (schema-free free-text per the SK-V18 seed §2 infra note if StructuredOutput
flakiness recurs) to verify ≥95% × 2 and zero orphan REVISE, then present for G-Alpha.
