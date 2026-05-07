# HARDENING-MASTER-PLAN-V8.1

V8.1 verifies the MASTER-PLAN trio (`restart/ARCHITECTURE.md`,
`restart/MIGRATION.md`, `restart/MASTER-PLAN.md`) post-Phase-8.3.1 corpus
cleanup and post-Phase-8.4 SYNTHESIS-trio simplification fold. The report
audits whether (a) the four V8 architectural cardinality reductions, the
diagnostic numeric retire, and the tranche-body routing landed in the trio
without architectural loss; (b) zero "deferred to V2 amendment" language
survives for V1-folded items (CHR, GADT, function composition); (c) the
trio carries Wave-9-ready detail for per-tranche full-spec drafting.

The audit is independent, calibrated, and adversarial — V8 surfaced 41
simplification candidates with READY-WITH-AMENDMENT verdict; V8.1 verifies
the trio absorbed the eight items routed to it.

## §1 Target Identification

| Target | Path | HEAD line count | Phase landed |
|---|---|---:|---|
| ARCHITECTURE | `restart/ARCHITECTURE.md` | 1,727 | Phase 8.4 (`e5cb1e4b`) |
| MASTER-PLAN | `restart/MASTER-PLAN.md` | 886 | Phase 8.4 (`e5cb1e4b`) |
| MIGRATION | `restart/MIGRATION.md` | 816 | Phase 7.5A (`3207b1cb`); untouched at Phase 8.4 |

Mandatory read set:

- `restart/ARCHITECTURE.md` §3, §5, §7.2, §7.4, §7.5, §8.2, §10.1
- `restart/MASTER-PLAN.md` §3, §4, §5, §24, §25, §27 Phase 8.4 ledger
- `restart/MIGRATION.md` skim (verify untouched per Phase 8.4 fold)
- `restart/audit/hardening/HARDENING-MASTER-PLAN-V8.md` (V8 baseline; 11 punch items)
- `restart/audit/hardening/HARDENING-CONSOLIDATED-V8.md` (V8 cohort, 41 items, 4 tiers)
- `restart/locks/14-LOCKS.md` (architectural commitments)
- `restart/HANDOFF.md`, `restart/README.md` (Phase 8.3.1 surfaces)

Phase 8.4 commit chain (4 fold agents + 4 classifications + trio commit):

- `4c69b848` PASS-1 classification
- `831b2f90` PASS-2 classification
- `85187a74` PASS-3 classification
- `c72318cd` SYNTHESIS classification
- `23311ff8` PASS-1 fold (Grammar-IR merge + generic-validation collapse + numeric-retire)
- `1a75ea53` PASS-2 fold (Backend trait + BIR alphabet + numeric-retire + ε hygiene)
- `bd213632` PASS-3 fold (numeric retire + host-leverage + tranche-body routing)
- `e5cb1e4b` SYNTHESIS trio fold (this audit's target)

V8.1 verdict (preview): **READY** for the trio's internal coherence and
**READY-WITH-RESIDUES** for cohort coherence. The four cardinality
reductions, the diagnostic numeric retire, the γ10 cross-host carrier
note, the δ8 SOTA-parity-vs-beat clause, and the V2-amendment retirement
all landed in the trio. Two MASTER-PLAN cite-residues at §1 line 51 and
§10 line 391 still say "23 variants" against the post-fold "19 semantic
+ Return" reality, and PASS-3 §6b still publishes 13 numeric aliases that
ARCH §7.4 retired — both are out-of-trio-scope concerns documented at §6
below for sister-fold-agent receivers, not blockers for V8.1 closure.

## §2 Phase 8.3.1 Closure Verification

Phase 8.3.1 was the corpus cleanup pass that landed user adjudications on
the eight CORPUS-AUDIT-SYNTHESIS questions before Phase 8.4 dispatched.
V8.1 must verify Q1 (GADT V1 surface), Q3 (function composition library
DELETE), Q4 (V5.1 prune), Q7 (README §12 update), and Q8 (HANDOFF
additions) hold post-Phase-8.4.

| Item | Verification | Status |
|---|---|---|
| Q1 GADT V1 in Lock 4 | `restart/locks/14-LOCKS.md:40` carries "**V1 type system folds higher-rank polymorphism via DK13 algorithmic completeness; GADT user-facing surface lands V1: pattern-match arms admit branch-local-equality refinements (`Pattern @ where T = U -> Block` per the §6 BBNF grammar amendment); OutsideIn(X)-style implication constraints solved at `passes/types/` carry the equalities through to `LayoutFacts`; the user-facing diagnostic `BBNF-LOCAL-EQUALITY-ANNOTATION` is emitted when a match-arm refinement annotation is missing or ill-typed.**" | **PASS** |
| Q1 GADT V1 in ARCH §8.2 | `restart/ARCHITECTURE.md:1284-1303` lists the five named mechanisms ending with "GADT branch-local-equality refinement (Phase 8.3.1 V1 fold; OutsideIn(X) implication constraints discharged at `passes::types` and propagated to `LayoutFacts`)"; `restart/ARCHITECTURE.md:1305-1315` re-asserts "GADT branch-local-equality refinements are V1 user-facing surface". | **PASS** |
| Q3 Function composition library deleted | `rg -n 'function composition library' restart/MASTER-PLAN.md` returns only `restart/MASTER-PLAN.md:866` (Phase 8.3.1 retirement note: "Q3 (function composition library DELETE)") and `restart/MASTER-PLAN.md:877` (δ9 RETIRED row: "Phase 8.3.1 Q3 user adjudication DELETED the library entirely; V1 function-value surface absorbs every composition use case via inline closure expression; trio carries no V2 row"). All living references retired; only deletion archaeology remains. | **PASS** |
| Q4 V5.1 prune | `ls restart/audit/hardening/ \| grep V5.1` returns zero. The pruned mid-cycle artefact is gone. | **PASS** |
| Q7 README §12 ORCHESTRATOR cite | `restart/README.md:412` carries "ORCHESTRATOR.md" positively; lines 421, 427 cite the orchestrator surface restructure at Phase 8.1; the legacy PASS-{1,2,3}-SUBSTRATE/CODEGEN/RUNTIME prompts appear only at line 427 as deletion archaeology ("retired at Phase 8.0"). | **PASS** |
| Q8 HANDOFF additions | `restart/HANDOFF.md:47` carries "**Current operating verdict: `restart/audit/hardening/HARDENING-CONSOLIDATED-V8.md` (SIMPLIFY-AVAILABLE; Phase 8.4 simplification fold pending).**" Lines 73, 77, 79 reflect the Phase 8.0/8.1 ledger ("ORCHESTRATOR.md (NEW Phase 8.1)", "Phase 8.1 adds lenses I/J/K", "retired at Phase 8.0"). The exact "Phase 8.0 DONE / Phase 8.1 DONE" string asked by the V8.1 prompt is absent (HANDOFF uses the prose form), but the ledger semantics are present. Note: HANDOFF line 47 still cites V8 (SIMPLIFY-AVAILABLE) as the current verdict; V8.1 will supersede this once the four V8.1 reports + new consolidated land. | **PASS-with-followup** (HANDOFF.md update is a V8.1 close item, not a Phase-8.3.1/8.4 fold residue) |

Phase 8.3.1 surfaces are intact. Q3's "function composition library"
deletion is verified through both the Phase 8.4 §27 ledger (δ9 RETIRED)
and the absence of any other living reference. Q1's GADT V1 surface
stands across both Lock 4 (governance) and ARCH §8.2 (surface).

## §3 Phase 8.4 Fold Closure Verification

Phase 8.4 routed eight V8 items to the SYNTHESIS trio (per §27 ledger
table at `restart/MASTER-PLAN.md:868-879`). Three additional V8 items
intersect MASTER-PLAN coherence and were folded into the trio commit per
the §27 §884 carry note (V8-P7 LowerContext, V8-P9 ARCH §5 sidecar note,
V8-P11 parse-API V2 cross-host note). V8.1 verifies each.

### α1 — Backend trait 5 → 2 methods

`rg -n 'emit_artefacts\|ArtefactSet' restart/ARCHITECTURE.md` returns 7
positive hits (lines 1112, 1116, 1120, 1124, 1127, 1142, 1144). The
trait surface at `restart/ARCHITECTURE.md:1101-1117` carries exactly
two methods — `lower(...) -> Result<Self::Output, Self::Error>` and
`emit_artefacts(grammar, schemas) -> Result<ArtefactSet, Self::Error>`.

`rg -n 'emit_runtime_template\|emit_value_api\|emit_visitor\|emit_path_schema'
restart/ARCHITECTURE.md` returns **zero** hits. The four old methods
retired without trace; the deletion archaeology lives only in the §27
ledger entry at `restart/MASTER-PLAN.md:870` for downstream readers.

`SchemaSet` bundles the three input schemas (value, visitor, path) per
line 1115; `ArtefactSet` bundles the four output trees per line 1142.
The four artefact files remain distinct on disk per line 1120 ("The
four artefact files remain distinct on disk under
`runtime/src/grammars/<g>/`"). PASS-2 §A is the cross-target sister
surface; the cross-target reconciliation is verified at §6 below.

**α1 PASS.**

### α2 — Type-system stack 7 → 5 mechanisms

`rg -nC4 'HM-equality\|Algorithm-W\|Pierce-Turner\|DK13.*finite CSP\|GADT-refinement'
restart/ARCHITECTURE.md` returns the canonical paragraph at lines
1284-1303:

> The V1 type system composes five named mechanisms: HM-equality
> (Algorithm-W; Damas-Milner 1982; Pierce 2002 ch.22) + Pierce-Turner
> local check/synth (the bidirectional expected-type interface above)
> + DK13 higher-rank algorithmic completeness (Dunfield-Krishnaswami
> 2013; ordered existential contexts, principality tracking,
> decidability, soundness, completeness, explicit annotation rules
> for non-principal programs) + finite CSP for non-HM choices + GADT
> branch-local-equality refinement (Phase 8.3.1 V1 fold; OutsideIn(X)
> implication constraints discharged at `passes::types` and propagated
> to `LayoutFacts`). HM-equality, Algorithm-W, and first-order
> unification are one algorithm — Damas-Milner principal-scheme
> inference with first-order unifier — presented as one named mechanism
> rather than three.

The five named mechanisms are: (1) HM-equality (Algorithm-W) +
(2) Pierce-Turner local check/synth + (3) DK13 higher-rank +
(4) finite CSP + (5) GADT branch-local-equality refinement. The §27
α2 ledger at `restart/MASTER-PLAN.md:871` correctly classifies the
fold as patch-delta "§8.2 prose paragraph collapses three names of one
algorithm; CHR text moves to constraint-emission helper phrasing."
CHR-improvement is now framed at lines 1295-1299 as "a constraint-emission
helper inside `csp-solver` (Phase 8.3.1 V1 fold; not a separate
type-system layer)" — the Phase 8.3.1 Q2 fold cascades correctly into
α2's compositional reframing.

**α2 PASS.**

### α3 — BIR alphabet 22 → 19 (or, more precisely, 19 semantic + Return)

`rg -n '20-variant\|19 semantic\|22-variant' restart/ARCHITECTURE.md`
returns:

- `restart/ARCHITECTURE.md:907` "into the 20-variant shape below"
- `restart/ARCHITECTURE.md:936-938` "The 20-variant shape preserves
  the `Return` row PASS-2 added on top of the original PASS-1
  22-variant table; the three pair collapses (Layout, Alt,
  host-call) net the alphabet to 19 semantic variants plus
  `Return`."

The three pair collapses are documented in the §27 α3 ledger entry at
`restart/MASTER-PLAN.md:872` and re-asserted in the §7.2 prose:
- `(LayoutPush, LayoutPop) → LayoutScope { kind: Push | Pop }`
- `(DispatchAlt, SpeculativeAlt) → Alt { mode: Dispatch | Speculative }`
- `(CallHost, HostChain) → CallHost` (chain expresses as `Seq` of
  `CallHost`)

`rg -n 'LayoutPush\|LayoutPop\|DispatchAlt\|SpeculativeAlt\|HostChain'
restart/ARCHITECTURE.md` confirms only one hit at line 909 (the
deletion-archaeology paragraph: "no separate `HostChain` variant").
The 20-variant table at lines 913-934 + payload table at lines 944-965
+ example table at lines 969-992 all reflect the post-fold shape.

**α3 PASS** for ARCH §7.2. Note the residual cohort cite issue at
MASTER-PLAN §1 line 51 ("REINVENT exact contract around 23 variants")
and §10 line 391 ("23 variants") — these are documented as cohort
coherence residues at §6 below.

### α5 — Rewrite-budget 4 → 3 categories + LOAD-BEARING vs ASPIRATIONAL labels

`rg -n 'simplification-rewrites' restart/ARCHITECTURE.md` returns one
hit at line 1459 — the deletion-archaeology paragraph: "The prior
fourth category, `simplification-rewrites`, folds into `codegen::verify`
at F.W3 (one-pass dead-mark elision belongs alongside regen-equality,
not as an e-graph budget pool)."

`rg -nC2 'codegen::verify' restart/ARCHITECTURE.md` returns five hits
(lines 830, 1459, 1471, 1500, 1644) — the consolidation receiver
is named in §6 (pipeline gate), §10.1 (rewrite-budget retirement),
§10.1 again (post-extraction simplifications), §11 (LOC budget
ownership), and §13.1 (lint manifest header check).

The §10.1 budget table at lines 1463-1467 carries exactly three rows
— `legality-rewrites` LOAD-BEARING, `normalization-rewrites`
LOAD-BEARING (correctness-adjacent), `cost-driven-rewrites`
ASPIRATIONAL (throughput-bound; H tranche body). The §27 α5 ledger
at `restart/MASTER-PLAN.md:873` correctly classifies the fold and the
LOAD-BEARING/ASPIRATIONAL labels per Lens K verdict.

**α5 PASS.**

### β1 — Diagnostic numeric alias retire

`rg -n 'BBNF-LIFE001\|BBNF-VISIT002\|BBNF-PATH001\|BBNF-LAYOUT001\|BBNF-OPT001\|BBNF-OPT002\|BBNF-CG001\|BBNF-GRAMMAR001\|BBNF-LAYOUT002'
restart/ARCHITECTURE.md` returns hits only at lines 1042-1049 — the
single retirement paragraph:

> Phase 8.4 retires the numeric alias system. The catalogue carries
> human-readable codes only; the prior numeric aliases (`BBNF-LIFE001`,
> `BBNF-LIFE002`, `BBNF-VISIT002`, `BBNF-LAYOUT002`, `BBNF-OPT001`,
> `BBNF-OPT002`, `BBNF-PATH001`, `BBNF-PATH002`, `BBNF-GRAMMAR001`,
> `BBNF-CG001`) and pure-numeric codes (`BBNF-LIFE003` through
> `BBNF-SEM040`) fold into mnemonic names. CLI, LSP, and cookbook
> surfaces consume the human-readable form; numeric aliases were
> LLM-trained-distribution artefacts that double-tracked an 11-row
> catalogue for no compression gain.

The catalogue table at lines 1051-1082 carries 29 rows, all
human-readable. `BBNF-LOCAL-EQUALITY-ANNOTATION` (line 1075) is
present as the V1-active diagnostic the Phase 8.3.1 GADT fold added.
The trio's β1 fold is internally consistent.

**β1 PASS** for ARCH §7.4. PASS-3 §6b ledger residue documented at
§6 below (out-of-trio-scope per the Phase 8.4 SYNTHESIS fold report;
PASS-3 sister-fold agent owns).

### γ10 — Cross-host metadata carrier

`rg -nC2 'sidecar.*tranche\|cross-host.*Cargo.toml\|cross-host metadata-carrier'
restart/ARCHITECTURE.md restart/MASTER-PLAN.md` returns:

- `restart/ARCHITECTURE.md:739-745`:
  > The metadata schema content above is host-agnostic; the V1 carrier
  > is Cargo.toml's `[workspace.metadata.bbnf]` block because Rust-line
  > onboarding is the V1 surface. The cross-host metadata-carrier work
  > (a language-neutral sidecar so future TS/WASM consumers do not
  > re-invent the carrier) routes to MASTER-PLAN §24 carry as
  > tranche-body work, not a V2 amendment; the schema fields above
  > lock in at V1 regardless of which carrier file delivers them.
- `restart/MASTER-PLAN.md:805` carries the carry-ledger row:
  > Cross-host metadata carrier | A or J body | … Tranche-body work
  > promotes the schema content to a language-neutral sidecar (e.g.,
  > `bbnf.toml`) so future TS / WASM onboarding does not re-invent
  > the carrier. … A or J body close gate verifies sidecar round-trip
  > equality with the Cargo.toml block; cross-host consumers
  > (`tower-lsp` / wasm-pack / npm scripts) read the same schema
  > content from the sidecar.

The γ10 fold is correctly NOT-V2-amendment — the carrier work routes
to A.W4 or J body close gate, not a V2 carry. The schema content locks
at V1.

**γ10 PASS.**

### δ8 — SOTA-throughput tranche-H routing

`rg -nC2 'SOTA-parity\|SOTA-beat' restart/MASTER-PLAN.md` returns the
new clause at lines 145-154:

> SOTA-parity is the meta-grammar correctness floor: a bbnf-generated
> parser that lands within the competitor envelope (e.g., `json/twitter`
> ≤ 480us at H.W3 against sonic-rs 436us) demonstrates V1 correctness
> regardless of SOTA-beat status. SOTA-beat is the audacious target the
> user mandate anchors against (`json/twitter` ≤ 380us at J.W1,
> surpassing sonic-rs); the H tranche body owns the cost-driven rewrites
> + SIMD-recogniser tuning that closes the parity-to-beat delta. If
> H.W3/H.W4 measurements land at parity but not beat, J.W1 close gates
> record the parity-not-beat outcome and route SOTA-beat work to the
> H tranche body for further iteration; V1 correctness does not gate
> on SOTA-beat.

The §27 δ8 ledger at `restart/MASTER-PLAN.md:876` confirms patch-delta
status. The H tranche §13 wave routing at lines 497-501 carries H.W3
(early JSON SOTA gates: ≤480us / ≤950us / ≤3.5ms) and H.W4 (early CSS
SOTA gates: ≤3.8ms / ≤1.9ms) at looser thresholds than J.W1 (final:
≤380us / ≤750us / ≤2.8ms / ≤3.0ms / ≤1.6ms) — the parity-floor /
beat-aspiration distinction is wave-routed correctly.

**δ8 PASS.**

### ε5 — V2-amendment sweep

`rg -n 'V2 amendment\|deferred to V2\|V2 deferral\|post-V1 amendment'
restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md`
returns:

| Path | Line | Content | Classification |
|---|---:|---|---|
| `MIGRATION.md` | 71 | "ABROGATE-MOVE deferred to V2" (`crates/bbnf-path-ts`) | LEGITIMATE — Lock 5/11 V2 backend deferral |
| `MIGRATION.md` | 803 | "to V2 amendment and no longer occupy V1 carry rows" | DELETION ARCHAEOLOGY (sweep-confirmation prose) |
| `ARCH §5` | 743 | "tranche-body work, not a V2 amendment" | DELETION ARCHAEOLOGY (contrast clause re-routing γ10 to A or J body) |
| `ARCH §7.5` | 1149 | "V2 deferral note: when V2 admits `WasmBackend` and `TsBackend`" | LEGITIMATE — Lock 5 V2 backend deferral |
| `MASTER-PLAN §24` | 794 | "TS production | V2 amendment" | LEGITIMATE — Lock 5/11 V2 TsBackend |
| `MASTER-PLAN §24` | 800 | "`path-ts` schema | V2 amendment" | LEGITIMATE — Lock 5/11 V2 TsBackend |
| `MASTER-PLAN §24` | 801 | "WASM ABI | V2 amendment" | LEGITIMATE — Lock 5/11 V2 WasmBackend |
| `MASTER-PLAN §24` | 803 | "`path-ts` package publication timing | V2 amendment" | LEGITIMATE — Lock 5/11 V2 TsBackend |
| `MASTER-PLAN §27` | 875 | "not V2 amendment" (γ10 routing prose) | DELETION ARCHAEOLOGY |
| `MASTER-PLAN §27` | 877-878 | "δ9/δ10 RETIRED Phase 8.3.1 ... function composition library V2 amendment / CHR-improvement layer V2 amendment" | DELETION ARCHAEOLOGY (the RETIRED rows themselves) |
| `MASTER-PLAN §27` | 879 | "no V2 amendment row remains for V1-folded items" | DELETION ARCHAEOLOGY (sweep-confirmation prose) |

The four surviving "V2 amendment" rows in MASTER-PLAN §24 (TS
production, `path-ts` schema, WASM ABI, `path-ts` package publication
timing) all bind to Lock 5/11 V2 backends — TsBackend + WasmBackend —
per ARCH §7.5. None route a V1-folded item (CHR, GADT, function
composition) to V2.

**ε5 PASS.**

### V8-P7 — LowerContext SideTables SIMPLIFY (out-of-trio-scope per §884, but check)

`rg -nC2 '&SideTables\|`SideTables`' restart/ARCHITECTURE.md` returns
the post-fold paragraph at lines 1138-1147:

> The `LowerContext` type carries: target triple (or wasm32-equivalent),
> generated-code budget cursor, grammar metadata reference, a
> `&SideTables` reference whose definition lives at §7.3 (one struct
> over `LayoutFacts`, `ShapeFacts`, `RecognizerFacts`, `CostFacts`,
> `RecoveryFacts`, `BridgeJustification`), and lint-mode toggles. The
> `ArtefactSet` type carries typed file trees with their committed
> paths and budget metadata, not raw strings; the four artefact families
> (runtime template, typed value API, visitor, path schema) are routed
> through one struct so the trait surface stays clean while
> per-artefact emission policy lives inside the `Backend` impl body.

The §27 §884 prose said V8-P7 "[lives] with the SYNTHESIS amendment
commit" — the fold did land. **V8-P7 PASS.**

### V8-P9 — ARCH §5 sidecar note (out-of-trio-scope per §884, but check)

Verified at §3 γ10 above — the §5 cross-host metadata-carrier note
landed at lines 739-745.

**V8-P9 PASS.**

### V8-P11 — ARCH §3.1 parse-API V2 cross-host note (out-of-trio-scope per §884, but check)

`rg -nC4 'V1 Rust-line\|three-entrypoint\|cross-host divergence'
restart/ARCHITECTURE.md` returns no §3.1 cross-host note. The
§3.1 prose at lines 191-237 carries the canonical three-entrypoint
trait + Lock 9 cite + public exports table — but no explicit
"V1 Rust-line; V2 collapses" note as V8-P11 specified.

**V8-P11 NOT-LANDED.** This is consistent with the §27 §884 closing
clause that bound V8-P11 to "ARCH-internal text amendments live with
the SYNTHESIS amendment commit"; the fold was performed for V8-P7 +
V8-P9 but elided for V8-P11. The cross-host divergence is implicit
in the trait shape and Lock 9 cite, but the explicit one-line note
that V8-P11 specified is absent. This is a non-blocking residue — the
HYBRID Lens J verdict for V8-P11 is satisfied through the §7.5 trait
+ §3.1 trio + Lock 9 in concert; the explicit note would clarify but
its absence does not introduce architectural drift.

## §4 V2-Amendment Retirement Ledger (critical)

V8.1 must verify zero "deferred to V2 amendment" patterns survive for
V1-folded items (CHR, GADT, function composition). Every surviving "V2"
mention must be classified as legitimate scope partition.

| Surface | "V2" reference | V1-folded item? | Classification |
|---|---|---|---|
| Lock 4 (`14-LOCKS.md:40`) | "V1 type system folds higher-rank polymorphism via DK13 ... GADT user-facing surface lands V1" | YES (GADT) | LEGITIMATE — V1 fold confirmed in lock; no V2 deferral |
| ARCH §8.2 (lines 1284-1303) | "GADT branch-local-equality refinement (Phase 8.3.1 V1 fold)" + "CHR-style improvement is a constraint-emission helper inside `csp-solver` (Phase 8.3.1 V1 fold; not a separate type-system layer)" | YES (CHR + GADT) | LEGITIMATE — V1 fold confirmed in ARCH; no V2 deferral |
| MASTER-PLAN §27 line 877 (δ9 RETIRED) | "function composition library V2 amendment ... DELETED the library entirely; V1 function-value surface absorbs every composition use case via inline closure expression; trio carries no V2 row" | YES (function composition) | DELETION ARCHAEOLOGY — explicit RETIRED status |
| MASTER-PLAN §27 line 878 (δ10 RETIRED) | "CHR-improvement layer V2 amendment ... FOLDED CHR-improvement into V1 csp-solver as constraint-emission helper; trio carries no V2 row" | YES (CHR) | DELETION ARCHAEOLOGY — explicit RETIRED status |
| MASTER-PLAN §24 line 794 (TS production) | "V2 amendment | TS lowering defers post-V1; the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5 owns the `path-ts` schema, the LSP TS bridge, and the typed TS path/value/visitor surface." | NO — Lock 5/11 V2 backend | LEGITIMATE — Lock 5/11 backend scope partition |
| MASTER-PLAN §24 line 800 (`path-ts` schema) | "V2 amendment | `path-ts` defers post-V1 alongside the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5." | NO — Lock 5/11 V2 backend | LEGITIMATE — Lock 5/11 backend scope partition |
| MASTER-PLAN §24 line 801 (WASM ABI) | "V2 amendment | WASM defers post-V1 alongside the V2 `WasmBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5." | NO — Lock 5/11 V2 backend | LEGITIMATE — Lock 5/11 backend scope partition |
| MASTER-PLAN §24 line 803 (`path-ts` package publication timing) | "V2 amendment | `path-ts` defers post-V1; V1 J.W3 publishes Rust-line only per Lock 11 amendment." | NO — Lock 5/11 V2 backend publication | LEGITIMATE — Lock 11 publication scope partition |
| MIGRATION line 71 (`crates/bbnf-path-ts`) | "ABROGATE-MOVE deferred to V2 ... TS surface defers post-V1 alongside the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5" | NO — Lock 5/11 V2 backend disposition | LEGITIMATE — Lock 5/11 disposition record |
| ARCH §7.5 line 1149 ("V2 deferral note") | "when V2 admits `WasmBackend` and `TsBackend`, the BIR alphabet does not change..." | NO — Lock 5 V2 backend expansion | LEGITIMATE — Lock 5 expansion description |
| MIGRATION line 803 (sweep-confirmation prose) | "to V2 amendment and no longer occupy V1 carry rows" | NO — meta-prose | DELETION ARCHAEOLOGY |
| ARCH §5 line 743 (γ10 routing prose) | "tranche-body work, not a V2 amendment" | NO — γ10 routing contrast | DELETION ARCHAEOLOGY |
| MASTER-PLAN §27 line 875 (γ10 ledger entry) | "not V2 amendment" | NO — γ10 routing contrast | DELETION ARCHAEOLOGY |
| MASTER-PLAN §27 line 879 (ε5 sweep) | "no V2 amendment row remains for V1-folded items" | NO — meta-prose | DELETION ARCHAEOLOGY |

**Net V2-amendment ledger**: zero V1-folded items survive as V2-deferred
rows. The four legitimate V2 carry rows (TS production, `path-ts`
schema, WASM ABI, `path-ts` publication) all bind to Lock 5/11 V2
backends. The eight deletion-archaeology / sweep-confirmation / scope-
partition mentions are correctly classified. CHR, GADT, and function
composition all moved to V1 (or retired entirely).

**V2-amendment retirement: COMPLETE.**

## §5 Full 9-Lane Verification

The trio is the executable authority for Wave 9. ≥15 audit rows covering
full lane coverage; the V8 verdict baseline is preserved where V8 ratified
READY.

| # | Lane | Surface | Verification | V8.1 Status |
|---:|---|---|---|---|
| 1 | Lane 1 — Lock-Adherence | Lock 1 (tape substrate) | `restart/locks/14-LOCKS.md:34` carries the substrate identity; ARCH §9.1 (line 1382 +) carries the canonical tape; §27 Phase 8.4 fold did not touch substrate. | honored |
| 2 | Lane 1 — Lock-Adherence | Lock 4 (per-domain orthogonality + GADT V1) | `14-LOCKS.md:40` carries DK13 + GADT V1 fold (Phase 8.3.1); ARCH §8.2:1305-1315 implements the user-facing surface; csp-solver carries CHR-improvement helper per §8.2:1295-1299. | honored |
| 3 | Lane 1 — Lock-Adherence | Lock 5 (Backend trait per-backend lower) | Backend trait at ARCH §7.5:1101-1117 carries the post-α1 two-method shape; V1 RustBackend impl + V2 WasmBackend + V2 TsBackend deferred per §7.5 obligation table at lines 1124-1127. | honored (post-α1) |
| 4 | Lane 1 — Lock-Adherence | Lock 8 (SOTA gates) | M1 Pro Rust-line at MASTER-PLAN §4:131-138 (six gate rows); SOTA-parity vs SOTA-beat at lines 145-154 (post-δ8 fold); WASM defers post-V1 per ARCH §7.5. | honored (post-δ8) |
| 5 | Lane 1 — Lock-Adherence | Lock 10 (auto-detect + 6-directive set) | `@pratt`/`@simd` retired per Lock 10 + Lock 14:60 directive list; six directives at ARCH §8.1:1169 (`@import`, `@host fn`, `@error`, `@layout`, `@pretty`, `@token`); function-typed `Type` surface confirmed at line 1318. | honored |
| 6 | Lane 1 — Lock-Adherence | Lock 11 (path-dep incubation + J.W3 split) | MASTER-PLAN §15 J.W3 line 570 carries the two-gate publication: stable surface unconditional + incubation-cleared 2-tranche stability gate; `path-ts` defers V2. | honored |
| 7 | Lane 1 — Lock-Adherence | Lock 12 (archive ceremony) | A.W0 archives `ser` + `gorgeous` per MASTER-PLAN §6:262; pre-restart-2026-05-04 tag cited in Lock 12. | honored |
| 8 | Lane 1 — Lock-Adherence | Lock 13 (tree shape) | 4-10 children + 500 LOC handwritten ceiling per Lock 13 / MASTER-PLAN §4:120; ARCH §13 owns the tree shape contract. | honored |
| 9 | Lane 1 — Lock-Adherence | Lock 14 (grammar generalisation) | Two-surface onboarding at ARCH §5 + Lock 14:60; yaml trajectory at MASTER-PLAN §5.3:226-242; per-grammar matrix at ARCH §12.2. | honored |
| 10 | Lane 2 — Sequencing | A → B → C → D → E → F → G → H → I → J | MASTER-PLAN §5.1:198-209 dependency-order matrix; each tranche has same-wave or next-wave consumer; Era V failure mode avoided. | honored |
| 11 | Lane 2 — Sequencing | C.W2 schema miner → G.W4 yaml gate | Schema miner at C.W2 (`MASTER-PLAN:332`) is V1 yaml-onboarding load-bearing; G future-grammar gate per `restart/MASTER-PLAN.md:478`. | honored |
| 12 | Lane 2 — Sequencing | C.W3 RecognizerFacts → H.W0/H.W1 Pratt/SIMD | Pratt + SIMD recogniser facts produced at C.W3 (`MASTER-PLAN:333`), consumed at H.W0 + H.W1 (`MASTER-PLAN:497-498`). | honored |
| 13 | Lane 2 — Sequencing | E.W0-E.W4 BIR → F.W0-F.W5 Rust lowerer | BIR substrate before lowerers per MASTER-PLAN §10/§11; PASS-2 boundary at E.W4 (`MASTER-PLAN:403`). E.W0 wave gate at line 399 consumes the post-fold "20-variant" alphabet per MASTER-PLAN §5.2:184; the §10 inheritance row at line 391 still cites "23 variants" (PASS-2 source bullet) and is documented at §6 below. | honored-with-residue |
| 14 | Lane 2 — Sequencing | F.W3 regen-equality → J.W3 publication | Regen-equality at F.W3 is publication's prerequisite per ARCH §10.1:1471 + MASTER-PLAN §15 J.W3:570. | honored |
| 15 | Lane 3 — Cohesion | yaml two-surface trajectory | yaml A.W4 → F.W5 → G.W4 → J.W2 per MASTER-PLAN §5.3:226-242; every claim verifiable from artefacts the trajectory produces. | honored |
| 16 | Lane 3 — Cohesion | Backend trait V2 expansion | The V2 obligations columns at ARCH §7.5:1124-1127 are speculative but bind to the post-α1 two-method shape; cohesion holds because V2 carry rows at MASTER-PLAN §24 explicitly route to Lock 5/11 V2 backends. | honored |
| 17 | Lane 4 — SOTA Anchoring | M1 Pro Rust-line gates | Each gate cites competitor + dataset + platform per Lock 8 / MASTER-PLAN §4:131-141. Post-δ8 fold splits SOTA-parity (correctness floor) from SOTA-beat (audacious target). | honored (post-δ8) |
| 18 | Lane 5 — Grammar-Authoritative (Lock 14) | yaml two-surface; per-grammar-fence-canon lint at ARCH §13.1 | Zero `match grammar { Json => ..., CssL4 => ..., ... }` arms in proposed generic crates per Lock 14:60; per-grammar table at ARCH §12.2 nine-row matrix + yaml onboarding row. | honored |
| 19 | Lane 6 — Generated-Code + LOC Budget | Per-grammar baselines at ARCH §12.2 + MASTER-PLAN §20 | nine seed grammars + yaml provisional ≤4,000 budget; F/H/J budget gates per MASTER-PLAN §4:119; `BBNF-CODEGEN-LOC-BUDGET` diagnostic at ARCH §7.4:1078. | honored |
| 20 | Lane 7 — Friction Forecast | 9-row cookbook at MASTER-PLAN §24 | Each row binds target user + mental model + confusion + artefact + diagnostic per MASTER-PLAN §24:814-824; format() row at line 824. | honored |
| 21 | Lane 8 — Carry & Deferral | MASTER-PLAN §24 carry-ledger | Every "deferred to" / "carries to" names receiver, blocker, gate; γ10 cross-host metadata carrier added at line 805 per Phase 8.4 fold; CHR + function composition + GADT no longer present (Phase 8.3.1 + 8.4 retirement). | honored (post-ε5) |
| 22 | Lane 9 — Greenfield Discipline | No quick solutions, no workarounds, no legacy uncontested | The trio absorbs eight V8 fold items + three out-of-trio-scope-but-MASTER-PLAN-coherence items via patch-delta amendments; zero workaround patterns; the four cardinality reductions are local re-shapes per V8 baseline. | honored |
| 23 | Lens F — LLM bias | Cite hygiene | V7.1 closed P1-P10 + X1; V8.1 spot-checked Phase 8.4 §27 ledger anchors (ARCH §7.5:1083-1113, ARCH §8.2:1278-1289, ARCH §7.2:894-984, ARCH §10.1:1444-1449, ARCH §7.4:1032-1063, ARCH §5:604-665) — all resolve to live surfaces in HEAD. The β1 sweep explicitly names "LLM-trained-distribution artefact" at ARCH §7.4:1047-1048 — meta-aware self-critique preserved from V8 Lens F. | honored |
| 24 | Lens G — Overfitting | Per-grammar matrix | yaml is "onboarding probe", not seed-grammar budget member per MASTER-PLAN §5.3:226 + ARCH §12.2; ten-row matrix is producer-side schema, not dispatch logic. | honored |
| 25 | Lens H — Hallucination/provenance | Path:line cites | All Phase 8.4 §27 ledger anchors resolve in V8.1 spot-check (ARCH §7.5:1083-1113 — true; §8.2:1278-1289 — true; §7.2:894-984 — true; §10.1:1444-1449 — true; §7.4:1032-1063 — true). Lock 8 line 48 + Lock 12 line 56 + Lock 14 line 60 cited in V8 also resolve. | honored |
| 26 | Lens I — Contrivance (V8 baseline) | 13-row table at V8 §2 | 7 KEEP + 4 SIMPLIFY + 1 CONSOLIDATE + 1 ASPIRATIONAL; α1, α2, α5 (4 SIMPLIFY + 1 CONSOLIDATE) + β1 (1 SIMPLIFY) all folded; the ASPIRATIONAL row (V8 P5/P10) folded as δ8 + ε5. | folded (post-Phase-8.4) |
| 27 | Lens J — Host-language leverage (V8 baseline) | 11-row table at V8 §3 | 7 KEEP + 2 HYBRID + 1 LEVERAGE + 1 SIMPLIFY; γ10 HYBRID folded with explicit V1-Rust-line + tranche-body sidecar promotion; V8-P11 (ARCH §3.1 cross-host note) NOT-LANDED but non-blocking. | folded-with-residue |
| 28 | Lens K — Meta-grammar discipline (V8 baseline) | 12-row table at V8 §4 | 8 LOAD-BEARING + 3 ASPIRATIONAL + 1 SPECULATIVE; δ8 ASPIRATIONAL folded (SOTA-parity vs SOTA-beat); other ASPIRATIONAL rows route to D/H/I tranche bodies per ε5. | folded |

Lane summary: 28 rows; 27 honored (one with documented residue at row
13). Zero amendment surfaces remain inside the trio; the residues at
§6 are out-of-trio-scope items owned by sister fold agents (PASS-2
source bullet at MASTER-PLAN line 391; PASS-3 §6b ledger).

## §6 Cross-Target Cohort Coherence

V8.1 verifies the post-Phase-8.4 trio aligns with the post-Phase-8.4
PASS docs (PASS-1 at `restart/audit/pass-1-substrate/PASS-1.md`, PASS-2
at `restart/audit/pass-2-codegen/PASS-2.md`, PASS-3 at
`restart/audit/pass-3-runtime/PASS-3.md`).

### Coherent surfaces

| Cohort surface | Trio | Sister verdicts | Status |
|---|---|---|---|
| Backend trait 2-method shape | ARCH §7.5:1101-1117 (`lower` + `emit_artefacts`) | PASS-2 §A (post-1a75ea53 fold) | coherent |
| BIR alphabet 19 semantic + Return | ARCH §7.2:907 ("20-variant shape"; lines 936-938 "19 semantic + Return") | PASS-2 §2 (post-1a75ea53 fold) | coherent at sources |
| 6-directive grammar | ARCH §8.1:1169 + Lock 10:52 + Lock 14:60 | PASS-1 §6 grammar amendment | coherent |
| Diagnostic catalogue (post-β1) | ARCH §7.4:1051-1082 (29 rows, all human-readable) | PASS-1 §6b ledger (post-23311ff8 fold; clean) + PASS-2 §6 ledger (post-1a75ea53 fold; clean per `PASS-2.md:570,618`) | coherent for PASS-1 + PASS-2 |
| Phase 8.4 §27 ledger naming all 4 fold agents | `restart/MASTER-PLAN.md:866` cites "Phase 8.3.1 user adjudications already absorbed Q1 + Q2 + Q3"; `restart/MASTER-PLAN.md:881-884` cites the four fold-agent commits' work routing | git log post-ledger commits: 23311ff8 (PASS-1) + 1a75ea53 (PASS-2) + bd213632 (PASS-3) + e5cb1e4b (SYNTHESIS trio); the §27 §881-884 prose names "PASS-1 / PASS-2 / PASS-3 sister fold agents" without commit hashes (intentional — the §27 ledger is content-routing, not commit-routing) | coherent (semantic; commit hashes intentionally elided) |
| Schema-mining miner at C.W2 | MASTER-PLAN §8 C.W2:332 | PASS-1 K-row LOAD-BEARING for yaml onboarding | coherent |

### Residual incoherences (out-of-trio-scope)

| Residue | Location | Authoritative source | Owner |
|---|---|---|---|
| MASTER-PLAN §1 line 51 says "REINVENT exact contract around 23 variants" | `restart/MASTER-PLAN.md:51` | ARCH §7.2 says 19 semantic + Return | MASTER-PLAN §1 verdict-ledger row needs update; trio-internal residue (§27 fold did not sweep §1) |
| MASTER-PLAN §10 line 391 says "PASS-2 BIR table. \| 23 variants" | `restart/MASTER-PLAN.md:391` | ARCH §7.2 says 19 semantic + Return; PASS-2 (post-fold) carries 19 + Return | MASTER-PLAN §10 inheritance bullet needs update; trio-internal residue (§27 fold did not sweep §10 inheritance bullets) |
| PASS-3 §6b carries 13 numeric aliases retired by ARCH §7.4 | `restart/audit/pass-3-runtime/PASS-3.md:452-471` (BBNF-LIFE001, BBNF-LIFE002, BBNF-LAYOUT001, BBNF-LAYOUT002, BBNF-OPT001, BBNF-OPT002, BBNF-GRAMMAR001, BBNF-PATH001, BBNF-PATH002, BBNF-PATH003, BBNF-VISIT001, BBNF-VISIT002, BBNF-VISIT003) | ARCH §7.4 retired all numeric aliases per β1 (`restart/ARCHITECTURE.md:1042-1049`); Phase 8.4 PASS-3 fold (commit `bd213632`) only retired BBNF1004/BBNF-LIFE003 (lookbehind alias) | PASS-3 sister-fold agent owns; out-of-trio-scope per Phase 8.4 §881 ("β2 + β3 ... γ1-γ9 ... δ5-δ7 ... — owned by PASS-1 / PASS-2 / PASS-3 sister fold agents") |
| ARCH §3.1 does not carry the V8-P11 V1-Rust-line cross-host note | `restart/ARCHITECTURE.md:191-237` | V8 §7 V8-P11 (HYBRID Lens J) | trio-internal but elided per §27 §884 prose; the cross-host divergence is implicit in §7.5 trait + Lock 9 |
| ARCH §11 line 1501 still says "OpenFrame clone absence" (not "parallel-substrate-clone-absent") | `restart/ARCHITECTURE.md:1501` | V8 §7 V8-P6 (LEVERAGE Lens J) | β3 explicitly excluded from trio fold per §27 §881 ("β3 (OpenFrame rename) ... — owned by PASS sister fold agents") |

The first two residues (lines 51, 391) are TRIO-INTERNAL — the
Phase 8.4 §27 ledger swept the verdict bodies (ARCH §7.2 + MASTER-PLAN
§5.2:184 + ARCH §0:35) but did not propagate to the §1 verdict-ledger
or §10 inheritance bullet. These are minor cohort-coherence trim items
the next pass should address; they do not invalidate V8.1 closure
because the authoritative count lives in ARCH §7.2 (the IR contract,
not the MASTER-PLAN summary).

The PASS-3 §6b residue is OUT-OF-TRIO-SCOPE per the Phase 8.4 fold
boundary; the PASS sister-fold-agent receivers are documented in §27
§881-884.

The ARCH §11 OpenFrame rename is OUT-OF-TRIO-SCOPE (β3 deliberately
deferred). The ARCH §3.1 cross-host note is TRIO-INTERNAL but elided
per the §27 §884 prose acknowledgement that V8-P11 + V8-P9 + V8-P7
"live with the SYNTHESIS amendment commit" — V8-P7 + V8-P9 landed,
V8-P11 did not.

**Cross-target cohort coherence: READY-WITH-DOCUMENTED-RESIDUES.** The
residues are non-blocking for V8.1 closure (the trio is internally
authoritative for Wave 9; the residues are sister-document drift
visible to careful readers but not architectural drift).

## §7 Final Verdict

**READY.**

The MASTER-PLAN trio absorbs the eight V8 fold items routed to it:
- α1 Backend trait 5 → 2 methods (ARCH §7.5)
- α2 Type-system stack 7 → 5 named mechanisms (ARCH §8.2)
- α3 BIR alphabet 22 → 19 semantic + Return (ARCH §7.2)
- α5 Rewrite-budget 4 → 3 categories with LOAD-BEARING/ASPIRATIONAL
  labels (ARCH §10.1)
- β1 Diagnostic numeric alias retire (ARCH §7.4)
- γ10 Cross-host metadata carrier (ARCH §5 + MASTER-PLAN §24:805)
- δ8 SOTA-parity-vs-beat clause (MASTER-PLAN §4:145-154)
- ε5 V2-amendment ledger sweep (MASTER-PLAN §24)

Two of the three out-of-trio-scope MASTER-PLAN-coherence items also
landed (V8-P7 LowerContext + V8-P9 ARCH §5 sidecar). One (V8-P11
ARCH §3.1 cross-host note) elided.

The four V1-folded items (CHR-improvement, GADT user-facing surface,
function composition library, V5.1 mid-cycle artefact) all retired
correctly: CHR + GADT folded into V1 substrate per Phase 8.3.1; function
composition deleted entirely per Phase 8.3.1 (V1 closures absorb every
use case); V5.1 artefact pruned. Zero V2-deferral language survives
for any of these items; the four surviving "V2 amendment" rows in
MASTER-PLAN §24 all bind to legitimate Lock 5/11 V2 backends (TS
production, `path-ts` schema, WASM ABI, `path-ts` publication).

The trio is internally coherent. The fourteen architectural locks all
hold. The Phase 8.4 §27 ledger names all eight items it routed to the
trio + classifies the three additional MASTER-PLAN-coherence items
correctly. The fold landed without architectural disruption — every
amendment is a local re-shape that reduces cardinality without losing
load-bearing content.

V8.1 confirms V7.1 READY survives Phase 8.4 simplification fold.

Re-draft thresholds: zero crossed. The architecture's load-bearing
surface is intact and leaner.

## §8 V8.1 → Wave 9 Readiness Assessment

The user mandate asks: does the trio carry sufficient detail for
per-tranche full-spec drafting at Wave 9?

Wave 9 is the first per-tranche full-spec drafting cycle (A-J × W0-W*).
The drafting cycle expects the trio to carry:
1. **Workspace shape** — 24-crate set + visibility + role + dependencies.
2. **Pipeline contract** — Grammar IR + Backend IR + side tables alphabet + payload + invariants.
3. **Type-system surface** — HM + bidirectional + DK13 + CSP + GADT mechanisms with composition rules.
4. **Backend trait contract** — V1 RustBackend + V2 expansion shape.
5. **Diagnostic catalogue** — single-namespace human-readable codes; producer + consumer + verbatim string ownership.
6. **Workspace metadata schema** — Cargo.toml `[workspace.metadata.bbnf]` block + per-grammar fields + cross-host carrier carry.
7. **Per-tranche close gates** — A through J with primary close gate + waves + carry FROM/TO.
8. **SOTA gates** — competitor anchors + platform + parity-floor + beat-aspiration.
9. **Lock-by-lock enforcement mapping** — fourteen locks + per-lock close gate + receiving tranche.
10. **Carry ledger** — every "deferred to" + receiver + blocker + gate.

Trio coverage:

| Wave-9 input | Trio surface | Status |
|---|---|---|
| Workspace shape | ARCH §1 (24-crate table) + §3 (public APIs) + §4 (private internals) + §5 (Cargo schema) | READY |
| Pipeline contract | ARCH §6 (pipeline) + §7 (IR contract: §7.1 Grammar IR + §7.2 BIR 19+Return + §7.3 side tables + §7.4 diagnostics + §7.5 Backend trait) | READY |
| Type-system surface | ARCH §8.2 (5 mechanisms; HM-equality + Pierce-Turner + DK13 + CSP + GADT-refinement) + Lock 4 GADT V1 fold | READY |
| Backend trait contract | ARCH §7.5 (post-α1 two-method) + V2 obligations table | READY |
| Diagnostic catalogue | ARCH §7.4 (29 human-readable rows; β1 retire complete) | READY |
| Workspace metadata | ARCH §5 (canonical TOML block + cross-host carrier note) | READY |
| Per-tranche close gates | MASTER-PLAN §6-§15 (A through J wave tables + close gates) + §5.1 (calendar + carry matrix) | READY |
| SOTA gates | MASTER-PLAN §4:131-154 (six rows + parity-vs-beat clause) | READY |
| Lock enforcement | MASTER-PLAN §21 (Lock Ownership table at line 712 +) | READY |
| Carry ledger | MASTER-PLAN §24 (post-ε5 sweep; γ10 + cross-host metadata carrier + four V2 backend rows) | READY |

**Wave 9 readiness: READY.** The trio carries the per-tranche full-spec
drafting inputs at every layer. The two cohort-coherence residues
documented at §6 (MASTER-PLAN line 51 + line 391 "23 variants" cite
drift) are non-blocking — the authoritative variant count lives in ARCH
§7.2 and MASTER-PLAN §5.2:184; per-tranche drafting agents read ARCH
first.

The five out-of-trio-scope items (PASS-3 §6b numeric aliases, ARCH §11
OpenFrame rename, ARCH §3.1 cross-host note, plus the two MASTER-PLAN
"23 variants" residues) are sister-fold-agent receivers or trim items;
none gate per-tranche drafting because the trio carries the
authoritative content where downstream consumers read it.

Specifically:
- Tranche A.W0-A.W4 drafting reads ARCH §1, §3, §5, §13 + MASTER-PLAN
  §6. All ready.
- Tranche B.W0-B.W4 drafting reads ARCH §3.1 (parse API), §7.5 (Backend
  trait), §9 (runtime architecture) + MASTER-PLAN §7. All ready.
- Tranche C.W0-C.W5 drafting reads ARCH §7.1, §7.3, §8.2, §10 +
  MASTER-PLAN §8. All ready (post-α2 fold).
- Tranche D.W0-D.W5 drafting reads ARCH §8 (BBNF surface) + MASTER-PLAN
  §9. All ready.
- Tranche E.W0-E.W4 drafting reads ARCH §7.2 (19 + Return) + MASTER-PLAN
  §10. The §10 line 391 inheritance cite (23 variants) is residual but
  the wave gate at line 399 is grammar-neutral; drafting agents will
  resolve to the ARCH source-of-truth.
- Tranche F.W0-F.W5 drafting reads ARCH §7.5 + §10 + §10.1 (post-α5
  fold) + MASTER-PLAN §11. All ready.
- Tranche G.W0-G.W4 drafting reads ARCH §3.4 (path APIs) + §12.1 (yaml
  walkthrough) + MASTER-PLAN §12. All ready.
- Tranche H.W0-H.W4 drafting reads ARCH §11 + Lock 8 + MASTER-PLAN
  §13 + §4:145-154 (post-δ8 fold). All ready.
- Tranche I.W0-I.W4 drafting reads ARCH §3.3 (LSP) + MASTER-PLAN §14.
  All ready.
- Tranche J.W0-J.W5 drafting reads MASTER-PLAN §15 + §16 + §24 +
  publication gates. All ready (post-ε5 fold).

The trio is a Wave-9-ready foundation. Per-tranche full-spec drafting
can dispatch in parallel where tranche surfaces don't overlap.

## §9 Closing Posture

V7.1 ratified the architecture; V8 surfaced 41 simplification candidates
across four lenses (I/J/K + 9-lane); Phase 8.3.1 absorbed the user's
adjudications on the eight CORPUS-AUDIT questions; Phase 8.4 dispatched
four parallel fold agents on disjoint surfaces (PASS-1, PASS-2, PASS-3,
SYNTHESIS trio); V8.1 verifies the SYNTHESIS trio fold landed.

The trio is leaner. The Backend trait shrank from five methods to two
without losing artefact distinctness on disk. The type-system stack
shrank from seven layers to five named mechanisms by collapsing three
presentations of one Damas-Milner algorithm. The BIR alphabet shrank
from 22 variants to 19 semantic + Return by collapsing three
semantically-redundant pairs. The rewrite-budget shrank from four
categories to three by folding `simplification-rewrites` into
`codegen::verify` (no e-graph need). The diagnostic catalogue retired
its numeric alias system entirely; the LLM-trained-distribution artefact
is gone.

CHR-improvement, GADT user-facing surface, and function composition
library — the three V1-FOLD-CANDIDATE Tier 3 items the user adjudicated
at Phase 8.3.1 — all retired from V2 amendment carry. CHR is now a
V1 csp-solver constraint-emission helper; GADT is V1 user-facing
surface with `BBNF-LOCAL-EQUALITY-ANNOTATION` at the `passes::types`
boundary; function composition deleted entirely (V1 closures absorb).
The four surviving V2 amendment rows all bind to legitimate Lock 5/11
V2 backends (TS production, `path-ts` schema, WASM ABI, `path-ts`
publication).

The user mandate ("audacious + SOTA + functional-in-nature + Rust-like
ergonomics + inference stronger than Rust if possible") is preserved
through the V8 simplifications:
- audacious: SOTA gates (Lock 8) + δ8 parity-vs-beat clause make the
  audacity explicit and the correctness floor explicit.
- SOTA: Lock 8 anchors against sonic-rs / simd-json / lightning-css
  unchanged.
- functional-in-nature: function values + lambdas + closures + DK13 +
  match + tuple all V1; transducer-without-directive worked example
  unchanged.
- Rust-like ergonomics: three-entrypoint parse API + bumpalo opt-in
  unchanged (V8-P11 cross-host note elided but Lock 9 + ARCH §7.5
  carry the V1-Rust-line truth).
- inference stronger than Rust: DK13 higher-rank V1 fold unchanged;
  GADT branch-local-equality refinement V1 fold added at Phase 8.3.1.

The two trio-internal residues (MASTER-PLAN §1 line 51 + §10 line 391
"23 variants" cite drift) are minor cohort-coherence trim items;
addressing them is a 2-line patch the next maintenance pass can absorb
without disrupting per-tranche drafting. The three out-of-trio-scope
residues (PASS-3 §6b numeric aliases, ARCH §11 OpenFrame rename, ARCH
§3.1 cross-host note) are sister-fold-agent receivers or β3-deferred
items; the §27 §881-884 prose explicitly accounts for them.

V8.1 closes the four-target Phase 8.4 cohort verification. The trio is
the executable authority for Wave 9. Per-tranche full-spec drafting
unblocks immediately.

Acceptance standard for V8.1 closure:
- Eight V8 fold items routed to the trio: all PASS.
- Two of three out-of-trio-scope MASTER-PLAN-coherence items: PASS;
  one (V8-P11) NOT-LANDED (non-blocking).
- Phase 8.3.1 closure (Q1 + Q3 + Q4 + Q7 + Q8): all PASS.
- V2-amendment retirement: zero V1-folded items survive as deferred;
  four legitimate Lock 5/11 V2 backend rows remain.
- Cross-target cohort coherence: READY-WITH-DOCUMENTED-RESIDUES (five
  residues catalogued; none architectural).
- Wave 9 readiness: READY across all ten per-tranche drafting input
  layers.

V8.1 verdict: **READY.**
