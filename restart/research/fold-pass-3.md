# PASS-3 research fold classification - PASS-3 runtime surface

## §1 - Target and source artefacts read

| Field | Value |
|---|---|
| Fold target | `restart/audit/pass-3-runtime/PASS-3.md` |
| Classification report | `restart/research/fold-pass-3.md` |
| Amendment surface allowed after this report | `restart/audit/pass-3-runtime/PASS-3.md` only |
| Current PASS-3 line anchors verified | materialisation gate `PASS-3.md:141`; tape identity `PASS-3.md:184-186`; `ReparsePlan` `PASS-3.md:203-206`; recovery walkthrough `PASS-3.md:223-240`; diagnostics `PASS-3.md:419-435`; benchmark metadata `PASS-3.md:449,459`; cross-pass hand-offs `PASS-3.md:492-504` |

Required artefacts read:

- `restart/research/topic-1-hm-foundations.md`
- `restart/research/topic-2-bidirectional.md`
- `restart/research/topic-3-csp-gadts.md`
- `restart/research/topic-4-egraphs.md`
- `restart/research/topic-5-cost-models.md`
- `restart/research/topic-6-tape.md`
- `restart/research/topic-7-green-red-incremental.md`
- `restart/research/topic-8-simd-dfa.md`
- `restart/research/INDEX.md` §2-§3
- `restart/prompts/RESEARCH-FOLD-ORCHESTRATOR.md` Phase 2
- `restart/prompts/AMENDMENT-DISPATCH.md` §1 verify-then-patch discipline
- `restart/README.md` gestalt and §13 voice
- `restart/locks/14-LOCKS.md`
- `restart/audit/pass-3-runtime/PASS-3.md`
- `restart/audit/hardening/HARDENING-PASS-3-V5.md`
- `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md`
- `restart/audit/hardening/HARDENING-PASS-3-V5.1.md`

Routing rules applied:

- FOLD means this worker can patch PASS-3 without contradicting locks.
- ESCALATE means the actual lock file would need structural change or is contradicted.
- DEFER means the finding belongs to README, INDEX, locks, Architecture, Master, PASS-1, PASS-2, implementation gates, or future tranche specs outside this worker's write scope.
- OUT-OF-SCOPE means the item is not a PASS-3 runtime API, runtime diagnostic, benchmark/runtime report, or user-visible value surface.

## §2 - Item routing table for §5 refinements and §7 surgeries

| Source item | Source target | PASS-3 target | Route | Rationale |
|---|---|---:|---|---|
| T6 R1 / S1 | README materialisation sentence | none | DEFER | README wording is outside scope; PASS-3 absorbs the cost-class consequence through `materialisation_cost.toml`. |
| T6 R2 | README/PASS-3 semantic layout split | `PASS-3.md:184` | FOLD | Keeps PASS-3 semantic, not byte-layout mandatory; exact token layout remains PASS-1/PASS-2. |
| T6 R3 / S2 | PASS-2 `TapeShape`/`ValueShape` | none | DEFER | PASS-2 owns BIR shape; PASS-3 can require generated evidence from those shapes. |
| T6 R4 / S3 | Architecture direct-scalar cache | none | DEFER | Architecture owns direct-builder semantics; PASS-3 can mirror user-visible traceability. |
| T6 R5 / S5 | PASS-3 materialisation gate | `PASS-3.md:141` | FOLD | Add scalar-cache policy, string-normalization policy, repeated-access cost, and objective evidence. |
| T6 R6 / S4 | PASS-3 tape identity invariant | `PASS-3.md:184` | FOLD | Build order is not semantic; generated direct fields trace to `(TapeId, node id, payload class)`. |
| T6 R7 / S6 | Architecture benchmark schema | none | DEFER | Architecture owns shared schema; PASS-3 can tighten bench output metadata. |
| T6 R8 / S7 | PASS-3 benchmark reports | `PASS-3.md:449,459` | FOLD | Add validation mode, source ownership mode, materialisation mode, trace mode, and surface. |
| T6 R9 / S8 | Architecture `runtime/src/tape/payload` | none | DEFER | Crate tree shape is not this worker's file. |
| T6 R10 / S9 | PASS-2 BIR `Rule` row | none | DEFER | PASS-2 owns BIR payload naming. |
| T6 R11 / S11 | Research INDEX Hubbard provenance | none | DEFER | INDEX edits denied. |
| T6 R12 / S10 | README one-authority wording | none | DEFER | README edits denied; PASS-3 folds equivalent identity language. |
| T6 S12 | Future materialisation-cost artefact | `PASS-3.md:141` | FOLD | Bind as a consumer gate, not implementation detail. |
| T6 S13 | Future benchmark report schema | `PASS-3.md:449,459` | FOLD | PASS-3 report metadata can carry the receiver. |
| T6 S14 | Future runtime identity tests | `PASS-3.md:184-186` | FOLD | PASS-3 already owns identity tests over direct root, `ValueRef`, path/select, visitor, debug trace. |
| T6 S15 | Future lazy conversion tests | `PASS-3.md:141` | FOLD | Test classes become materialisation-cost fields, not immediate code. |
| T6 S16 | Future byte-input parse API | `PASS-3.md:449,459` | FOLD | Benchmark metadata distinguishes `parse(&str)` prevalidation from byte/file validation. |
| T7 R1 / S1 | README representation wording | none | DEFER | README edit denied; PASS-3 may permit red-like cursor views as non-owning runtime views. |
| T7 R2 / S2 | README incremental identity | none | DEFER | README edit denied; PASS-3 folds snapshot-scoped `TapeId`. |
| T7 R3 / S3 | README cache survival | `PASS-3.md:203-206` | FOLD | PASS-3 should carry invalidated query facts in `ReparsePlan`. |
| T7 R4 / S4 | PASS-3 identity scope | `PASS-3.md:184` | FOLD | Cross-snapshot identity exists only through a `ReparsePlan` reuse map. |
| T7 R5 / S5 | PASS-3 `ReparsePlan` fields | `PASS-3.md:203-206` | FOLD | Add `reuse_map`, `fallback_reason`, and `invalidated_queries`. |
| T7 R6 / S6 | PASS-3 recovery node shape | `PASS-3.md:227-233` | FOLD | Add `RecoveryKind::{Error, Missing, Substituted}`, sync token, diagnostic code, typed placeholder policy, and `VisitTypes::ERROR`. |
| T7 R7 / S7 | Master incremental ledger | none | DEFER | Master owns tranche gate; PASS-3 already has fallback ledgers and can strengthen local receiver text. |
| T7 R8 / S8 | Master yaml syntax-error row | `PASS-3.md:405-407,223-240` | FOLD | PASS-3 owns yaml runtime behavior and can add syntax-error fallback without editing Master. |
| T7 S9 | INDEX bibliography cleanup | none | DEFER | INDEX edits denied. |
| T8 R1-R5 | README/Architecture parse-that tree/cost text | none | DEFER | Regex crate architecture outside PASS-3. |
| T8 R6-R8 / S1 / S7 / S10 | `RegexProgram` verifier and `SimdScan` exact/prefilter contract | `PASS-3.md:419-420,437-445,496` | FOLD | PASS-3 owns user diagnostics and runtime packaging; it can require exact scalar parity and prefilter verifier routing before tape emission. |
| T8 R9-R12 / S2 / S5 / S8 / S11 | PASS-2 scanner lowering/BIR/risk rows | none | DEFER | PASS-2 owns lowering and BIR names; PASS-3 mirrors user-visible diagnostic consequences. |
| T8 S3 / S4 / S6 / S9 / S12 | README/Architecture/Master regex oracle and SIMD posture | none | DEFER | Non-PASS-3 surfaces. |
| T1 F1-F16 / S1-S16 | README, Architecture, PASS-1, Master type-system text | none | DEFER | Type-system decomposition belongs to PASS-1/SYNTHESIS; PASS-3 must not expose a public type pass. |
| T1 F17 / S11 | Type diagnostic metadata | `PASS-3.md:430-435` | FOLD | User diagnostics can expose `expected_from`, `actual_from`, `obligation_id`, `solver_stage`, and value-shape cause. |
| T1 F18 | TAPL citation hygiene | none | OUT-OF-SCOPE | No PASS-3 citation addition is needed. |
| T2 R1-R6 / P1-P2 / P6 / P8-P15 | README/Architecture/Master/PASS-1 type-surface gates | none | DEFER | Higher-rank and subsumption gate wording is PASS-1/SYNTHESIS. |
| T2 R7 / R11 / P3 / P7 | Subsumption-edge diagnostic | `PASS-3.md:430-435` | FOLD | PASS-3 can expose the chain/subsumption cause in mirrored user diagnostics without owning the checker. |
| T2 R9-R10 / P4-P5 / P10 | INDEX source/lock repairs | none | DEFER | INDEX edits denied. |
| T3 F1-F9 / Srg1-Srg9 | Type-system/INDEX/PASS-1/PASS-2 routing | none | OUT-OF-SCOPE | No direct PASS-3 runtime API target; materialization/recovery choices are already covered through T6/T7/T5. |
| T3 Srg10 | Generic-cycle/local-equality diagnostics | none | DEFER | PASS-1/Architecture diagnostic ownership; PASS-3 can cite type mismatch generally but not invent checker codes here. |
| T4 R1-R12 / S1-S12 | Egraph/CSP bridge and cost-model architecture | none | OUT-OF-SCOPE | Not directly tied to PASS-3 runtime API/diagnostics. |
| T5 refinement 9 / S8 | PASS-3 materialisation cost objective evidence | `PASS-3.md:141` | FOLD | Runtime consumer docs need objective vector/profile and domination reason for direct/tape choices. |
| T5 S11 | PASS-3 optimizer diagnostic objective-profile wording | `PASS-3.md:419-420` | FOLD | `BBNF-OPT001`/`BBNF-OPT002` should explain profile-based rejection without `@pratt`/`@simd`. |
| T5 remaining refinements/surgeries | README, PASS-1, Architecture, Master, PASS-2, INDEX | none | DEFER | Cost-model API, source hygiene, and PASS-2 lowering are outside this worker. |

## §3 - §6 adversarial finding reconciliation

| Finding | Preserved finding text | Classification | Rationale |
|---|---|---|---|
| T6 A1 | "scalar materialisation cost is overstated" | FOLD | Lock 1 survives; PASS-3 materialisation gate must distinguish kind/span, normalized string, parsed payload, lazy scalar parse, cache policy. |
| T6 A2 | "`union` can be misread as two trees" | FOLD | Lock 1 survives; PASS-3 sharpens one `(TapeId, node id, payload class)` identity plus typed projections. |
| T6 A3 | "On-Demand forward-only semantics conflict with bbnf tooling" | FOLD | PASS-3 rejects forward-only semantics by requiring repeatable `ValueRef`, path/select, visitor, debug, and LSP projections. |
| T6 A4 | "in-situ competitors can make benchmark rows unfair" | FOLD | Lock 8 survives; PASS-3 benchmark metadata gains validation/source ownership/materialisation modes. |
| T6 A5 | "Hubbard comparative study is a provenance gap" | DEFER | Research INDEX/source hygiene is denied; no PASS-3 evidence cites it. |
| T6 A6 | "UTF-8 validation entry point needs explicit split" | FOLD | PASS-3 benchmark/report metadata can distinguish `parse(&str)` prevalidation from byte/file input validation. |
| T7 A1 | "`one representation` is too strong unless scoped to ownership" | FOLD | Lock 1 survives; PASS-3 can say red-like cursors and direct typed roots are transient non-owning views. |
| T7 A2 | "stable identity `per parsed token` is underspecified" | FOLD | Snapshot-scoped `TapeId` and `ReparsePlan` reuse map close the ambiguity without lock change. |
| T7 A3 | "bbnf must not claim unique value from ERROR/MISSING nodes" | FOLD | PASS-3 differentiates typed recovery through same tape/direct identity, diagnostics, visitors, path, CLI/LSP parity. |
| T7 A4 | "e-graph cache survival is too parser-local as written" | FOLD | PASS-3 can add invalidated query evidence to `ReparsePlan`; architecture cache policy remains SYNTHESIS/PASS-1. |
| T7 A5 | "Ungar/Adams 1994 and HelpMate sources are provenance gaps" | DEFER | Bibliography cleanup is outside this worker and not cited in PASS-3 amendment. |
| T8 AF1 | "SIMD positive versus DFA negative is under-specified" | FOLD | Lock 1 and Lock 10 survive; PASS-3 diagnostics require exact scan scalar parity or prefilter verifier acceptance before tape emission. |
| T8 AF2 | "Full DFA codegen cannot be mandatory for rich Unicode regex" | DEFER | Regex execution-plan naming belongs to PASS-2/SYNTHESIS; PASS-3 says verifier route, not mandatory DFA. |
| T8 AF3 | "Bespoke regex risks reimplementing `regex-automata` without a clear delta" | DEFER | Oracle/parity policy belongs to README/Master/PASS-2. |
| T8 AF4 | "`SIMD-first everywhere` can train implementers into over-selection" | FOLD | PASS-3 `BBNF-OPT002` can say SIMD is cost/exactness selected, not user-forced. |
| T8 AF5 | "Hyperscan/Vectorscan multi-pattern expectations may be a false friend" | OUT-OF-SCOPE | No PASS-3 runtime API/diagnostic edit required. |
| T1 A1 | "`Full Hindley-Milner with subsumption` is too strong" | DEFER | README/PASS-1 wording outside scope; no lock redraft. |
| T1 A2 | "`CSP-backed unification` confuses the solver contract" | FOLD | PASS-3 diagnostic strings can expose solver stage without promising public `TypeFacts`. |
| T1 A3 | "typed-record narrowing lacks a selected record type theory" | FOLD | PASS-3 can surface finite value-shape/projection cause; formal record theory remains PASS-1/SYNTHESIS. |
| T2 A | "lock numbering drift" | DEFER | INDEX repair only. |
| T2 B | "`full HM with subsumption` is too strong" | DEFER | README/SYNTHESIS/PASS-1 scope; no PASS-3 structural change. |
| T2 C | "DK completeness is cited before its surface exists" | DEFER | Higher-rank gate stays closed outside PASS-3. |
| T2 D | "coercion examples need rule sites" | FOLD | PASS-3 can mirror a chain/subsumption-edge diagnostic cause. |
| T2 E | "Roc source role is overclaimed" | DEFER | Source catalogue hygiene. |
| T3 A1-A7 | CSP/GADT/generic-rule findings and source drift | OUT-OF-SCOPE | PASS-1/SYNTHESIS/INDEX/PASS-2 own the surfaces; no PASS-3 runtime API target beyond folded value diagnostics. |
| T4 A1-A5 | Egraph/CSP bridge findings and source drift | OUT-OF-SCOPE | No PASS-3 runtime API/diagnostic target. |
| T5 A1 | "scalar trait is too strong" | FOLD | PASS-3 materialisation evidence can preserve objective vector/profile as consumer-facing explanation. |
| T5 A2 | "branch iterator double-counts DAG sharing" | DEFER | Cost-model API/PASS-1/PASS-2 ownership. |
| T5 A3 | "shared-with-regex can violate domain opacity" | DEFER | Regex summary API belongs to SYNTHESIS/PASS-2; PASS-3 only consumes diagnostic consequences. |
| T5 A4 | "Topic 5 lock pointer is stale" | DEFER | INDEX cleanup denied. |
| T5 A5 | "SMT-backed cost composition is under-specified" | DEFER | Architecture/PASS-1 cost-model gate; PASS-3 folds only objective evidence fields. |
| T5 A6 | "named source provenance gaps must not become evidence" | DEFER | Bibliography cleanup. |

Escalation result: zero ESCALATE items. Every adversarial point that touches PASS-3 weakens prose, diagnostics, or gates while the actual locks survive.

## §4 - Accepted amendment plan for PASS-3

1. Extend the §3 consumer gate at `PASS-3.md:141` so `materialisation_cost.toml` records field counts, payload arena bytes, tape-token width, scalar-cache policy, string-normalization policy, repeated-access cost, selected objective profile, scalarized score/objective vector, and domination reason.
2. Harden §4 at `PASS-3.md:184-186`: build order is not semantic; every generated direct field traces to `(TapeId, node id, payload class)`; red-like cursor views and direct typed roots are transient projections over one authority; debug/DAP retains mandatory snapshot/tape/node/span identity.
3. Extend `ReparsePlan` at `PASS-3.md:203-206` with `reuse_map`, `fallback_reason`, and `invalidated_queries`; state cross-snapshot identity exists only through that map.
4. Extend the recovery worked path at `PASS-3.md:223-240` with `RecoveryKind::{Error, Missing, Substituted}`, diagnostic code, sync token, typed placeholder policy, `VisitTypes::ERROR`, and yaml syntax-error fallback behavior for `YamlRoot`.
5. Amend §6b diagnostics at `PASS-3.md:419-435`: `BBNF-OPT001`/`BBNF-OPT002` name objective-profile/cost-and-exactness reasons; SIMD exact scans require scalar parity; prefilters require `RegexProgram`, DFA/VM, or scalar verifier acceptance before tape emission; add a type/value-shape mismatch diagnostic exposing check/synth/value-shape causes without public `TypeFacts`.
6. Amend §7 benchmark metadata at `PASS-3.md:449,459` to report parse mode, validation mode, source ownership mode, direct/tape materialisation mode, scalar-cache/string-normalization policy, selected objective profile, trace mode, and surface under test.
7. Amend §8 cross-pass hand-offs at `PASS-3.md:492-504` to route the new identity/recovery/materialisation/diagnostic evidence to PASS-1/PASS-2/SYNTHESIS gates.

Rejected from this worker:

- README, Architecture, Master, Migration, locks, research INDEX, PASS-1, PASS-2, synthesis trio, prompts, corpora, crates, implementation files, and archive edits.
- Any §6 item that would relitigate locks. None required escalation.

## §5 - Gate plan

Required before commit 2:

```text
git status --short
rg -n "TapeId|red-like|green|incremental|fault|recover|@error|DAP|debug|yaml|TapeShape|ValueShape|materiali|SIMD|DFA|prefilter|scalar parity|pointer!|select!|TypeMismatch|higher-rank|@pratt|@simd|path!|Wave 4" restart/audit/pass-3-runtime/PASS-3.md restart/research/fold-pass-3.md
git diff --check
git diff --cached --check
```

Additional local checks:

- `wc -l restart/research/fold-pass-3.md` confirms the classification report remains within the requested 160-350 line band.
- `git diff -- restart/audit/pass-3-runtime/PASS-3.md` is inspected before staging commit 2.
- `git diff --cached` is inspected before each commit per `commit-discipline`.

## §6 - Classification verdict

Verdict: AMEND PASS-3, NO ESCALATION.

The accepted fold is narrow and runtime-facing. Topics 6 and 7 land as stronger PASS-3 identity, materialisation, incremental, recovery, yaml fallback, and debug/DAP wording. Topic 8 lands only as runtime diagnostic/verifier language: exact scans prove scalar parity, prefilters verify before tape emission, and no `@simd` force surface appears. Topics 1 and 2 land only as user-visible value/type diagnostic causality. Topic 5 lands only through materialisation-cost and optimizer objective-profile evidence. Topics 3 and 4 do not require PASS-3 edits in this slice.
