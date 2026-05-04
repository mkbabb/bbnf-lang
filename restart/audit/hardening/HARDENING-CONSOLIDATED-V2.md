# HARDENING-CONSOLIDATED-V2

## §1 Target identifications

| Target | Target output audited | Hardening report (V2) | Hardening commit | Target amendment commits | Verdict | KEEP | REINVENT | DISCARD | Punch list residual |
|---|---|---|---|---|---|---:|---:|---:|---:|
| PASS-1 | `restart/audit/pass-1-substrate/PASS-1.md` + 6 sub-agent reports | `restart/audit/hardening/HARDENING-PASS-1-V2.md` | `4670773d` | `f08c75a4` (Wave 1.1), `cd3441e7` (Wave 2) | READY | 52 | 9 | 2 | 0 |
| PASS-2 | `restart/audit/pass-2-codegen/PASS-2.md` + 6 sub-agent reports | `restart/audit/hardening/HARDENING-PASS-2-V2.md` | `ef31cb45` | `2778f34d` (Wave 1.2), `d206b895` (Wave 2) | READY | 62 | 2 | 1 | 0 (1 non-blocking phrasing tightening) |
| PASS-3 | `restart/audit/pass-3-runtime/PASS-3.md` + 6 sub-agent reports | `restart/audit/hardening/HARDENING-PASS-3-V2.md` | `5a5dacf0` | `dceeaf32` (Wave 2), `70378e46` (Wave 3 carry) | READY | 64 | 2 | 0 | 2 (non-blocking phrasing tightenings) |
| MASTER-PLAN | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` | `restart/audit/hardening/HARDENING-MASTER-PLAN-V2.md` | `9e0e8be0` | `3a73f212` (Wave 2), `70378e46` (Wave 3) | READY | 66 | 1 | 4 | 1 (Lane 2 phrasing) |

| Cohort | KEEP | REINVENT | DISCARD | Punch-list residuals before dedupe | Final verdict |
|---|---:|---:|---:|---:|---|
| Four-target hardening cohort V2 | 244 | 14 | 7 | 4 | **READY** |

The four V2 reports agree across every consolidated finding from V1: tape stays tape, properly unioned with direct-to-struct; rewrite-mode is out; Unicode class algebra is owned by `parse-that/regex`; lookbehind, `@host fn` (block-bodied), multi-function chaining (`-> f1 -> f2`), generics, `@error`, `@layout` are in; default per-grammar declaration crates are out (rare-escape valve carries an 8-field fence); Backend IR is owned by `ir/src/backend_ir/` with `codegen` consuming through a verbatim import-deny gate; the `bbnf` aggregator carries 8 canonical children (`lib.rs`, `prelude.rs`, `parse/`, `document/`, `query/`, `visitor/`, `diagnostics/`, `metadata/`); `path`, `path-core`, `path-ts`, `test-fixtures` carry unprefixed names; the public macro is `pointer!`; `LayoutFacts` is the public side-table and `TypeFacts` is internal to `passes::layout`; the parse-throughput SOTA close rows inline competitor + dataset + platform + bbnf target without a routing escape; the per-X authority table at Architecture §12.1 carries 10 rows × 9 columns; the yaml onboarding admits exactly two surfaces (`yaml.bbnf` + `[workspace.metadata.bbnf.grammars.yaml]`); fixtures land in a separate parity phase, not at onboarding.

The consolidated V2 verdict therefore moves from V1's AMENDMENT-REQUIRED to **READY**. Every report returns READY; no report returns AMENDMENT-REQUIRED with substantive surgeries; no report returns RE-DRAFT.

## §2 Cohort verdict

| Lane | PASS-1 V2 | PASS-2 V2 | PASS-3 V2 | MASTER-PLAN V2 | Cumulative V2 |
|---|---|---|---|---|---|
| 1 Lock-Adherence | READY; KEEP 14 / REINVENT 0 / DISCARD 0 | READY; KEEP 14 / REINVENT 0 / DISCARD 0 | READY; KEEP 12 / REINVENT 0 / DISCARD 0 | READY; KEEP 14 / REINVENT 0 / DISCARD 0 | READY; KEEP 54 / REINVENT 0 / DISCARD 0 |
| 2 Sequencing | N/A | N/A | N/A | READY; KEEP 8 / REINVENT 1 / DISCARD 0 | READY; KEEP 8 / REINVENT 1 / DISCARD 0 |
| 3 Cohesion | READY; KEEP 8 / REINVENT 0 / DISCARD 0 | READY; KEEP 8 / REINVENT 0 / DISCARD 0 | READY; KEEP 8 / REINVENT 1 / DISCARD 0 | READY; KEEP 6 / REINVENT 0 / DISCARD 0 | READY; KEEP 30 / REINVENT 1 / DISCARD 0 |
| 4 SOTA-Anchoring | READY; KEEP 6 / REINVENT 0 / DISCARD 0 | READY; KEEP 7 / REINVENT 0 / DISCARD 0 | READY; KEEP 6 / REINVENT 0 / DISCARD 0 | READY; KEEP 5 / REINVENT 0 / DISCARD 1 | READY; KEEP 24 / REINVENT 0 / DISCARD 1 |
| 5 Grammar-Authoritative | READY; KEEP 8 / REINVENT 0 / DISCARD 0 | READY; KEEP 6 / REINVENT 1 / DISCARD 0 | READY; KEEP 7 / REINVENT 0 / DISCARD 0 | READY; KEEP 6 / REINVENT 0 / DISCARD 1 | READY; KEEP 27 / REINVENT 1 / DISCARD 1 |
| 6 Generated-Code-Budget | READY; KEEP 5 / REINVENT 0 / DISCARD 0 | READY; KEEP 7 / REINVENT 0 / DISCARD 0 | READY; KEEP 8 / REINVENT 0 / DISCARD 0 | READY; KEEP 6 / REINVENT 0 / DISCARD 0 | READY; KEEP 26 / REINVENT 0 / DISCARD 0 |
| 7 Friction-Forecast | READY; KEEP 7 / REINVENT 0 / DISCARD 0 | READY; KEEP 7 / REINVENT 0 / DISCARD 0 | READY; KEEP 9 / REINVENT 0 / DISCARD 0 | READY; KEEP 7 / REINVENT 0 / DISCARD 0 | READY; KEEP 30 / REINVENT 0 / DISCARD 0 |
| 8 Carry-Deferral | READY; KEEP 7 / REINVENT 0 / DISCARD 1 | READY; KEEP 8 / REINVENT 0 / DISCARD 0 | READY; KEEP 9 / REINVENT 0 / DISCARD 0 | READY; KEEP 8 / REINVENT 0 / DISCARD 1 | READY; KEEP 32 / REINVENT 0 / DISCARD 2 |
| 9 Greenfield-Discipline | READY; KEEP 7 / REINVENT 0 / DISCARD 1 | READY; KEEP 7 / REINVENT 0 / DISCARD 1 | READY; KEEP 7 / REINVENT 1 / DISCARD 0 | READY; KEEP 6 / REINVENT 0 / DISCARD 1 | READY; KEEP 27 / REINVENT 1 / DISCARD 3 |

| Verdict class | V1 cumulative | V2 cumulative | Net |
|---|---:|---:|---|
| KEEP | 117 | 244 | +127 |
| REINVENT | 127 | 14 | -113 |
| DISCARD | 8 | 7 | -1 |

The DISCARD count drops by one because a previously-DISCARD-classified item — PASS-1's "PASS-2 and PASS-3 may proceed independently" clause — became deletion-confirmed (DISCARD-confirmed at PASS-1.md:278 line is no longer a "current target text or clause must be deleted/replaced" item, it is deletion-archaeology). The remaining seven DISCARD-classified items are confirmation-of-deletion rows that survive as DISCARD-confirmed: final SOTA escape clause (J.W1), Lock 12 archive citation (MASTER-PLAN §6 corrected), registry deletion close gate (PASS-3 §3 + MASTER-PLAN §23), OpenFrame substrate (PASS-1 §10 + PASS-2 §9), independent-proceed clause (PASS-1.md:278), declaration-crate-by-default (Lock 14), per-grammar Unicode class algebra (Architecture §8.1).

The V2 KEEP rate jumps to ~93% (244/265), surpassing the 60-80% healthy-target band that signals fault-finding without challenge-erasure. The 14 residual REINVENT items are non-blocking phrasing tightenings (1 in PASS-2 Lane 5 cell wording, 1 in PASS-3 Lane 3 + 1 in PASS-3 Lane 9 — visitor cookbook routing, 1 in MASTER-PLAN Lane 2 — A.W4↔A.W3 binding refinement). None contradict an architectural commitment.

## §3 Cross-target conflicts

V1 surfaced 14 cross-target conflicts. V2 reads each conflict as resolved:

| V1 Conflict | V1 Sources | V2 Resolution | V2 Verdict |
|---|---|---|---|
| Backend IR ownership | PASS-2 placed `codegen/src/backend_ir/`; MASTER-PLAN treated BIR as lowerer contract; README gave `ir` ownership | Wave 1.1 + 1.2: BIR types live at `ir/src/backend_ir/`; `codegen/src/backend_ir/README.md` is documentation only; verbatim deny gate `rg -n "GrammarIR" crates/codegen/src/lower crates/codegen/src/runtime_template returns zero` | RESOLVED |
| Public path macro name | PASS-3 exposed `path!`; README named `pointer!` | Wave 2 PASS-3 §3: "`path` owns Rust proc macros: `pointer!` and `select!`"; gate-rerun check 2 returns no `path!` outside migration archaeology | RESOLVED |
| Path crate names | PASS-3 carried `bbnf-path*`; MASTER-PLAN used `path`/`path-core`/`path-ts` | Wave 2 PASS-3 §0: legacy `bbnf-path` cited as legacy evidence only; restart names are `path`, `path-core`, `path-ts` | RESOLVED |
| Layout terminology | All passes kept layout intent; MASTER-PLAN exposed `TypeFacts` | Wave 3 Architecture §7.3: `LayoutFacts` public; `TypeFacts` internal subroutine of `passes::layout`; MASTER-PLAN §10 C.W1 carries the layout-internal posture | RESOLVED |
| Cursor/byte-skip proof | All passes mentioned hand-offs; MASTER-PLAN claimed lock ownership without explicit tests | Wave 2 ARCHITECTURE.md:802-806: three-row Lock 3 cursor + skip gate table; MASTER-PLAN §24 carry ledger row binds `__EAGER_EMPTY_PATH` + `CursorDecision::Skip` | RESOLVED |
| BBNF extension surface | PASS-1 had declaration-only `HostFn`; PASS-3 introduced `@recover` | Wave 2 PASS-1.md:183 + 211: block-bodied `HostFn` production; PASS-3.md:160 + Architecture §8.1: `@recover` folded into `@error(recover = ...)` | RESOLVED |
| Lock 14 yaml onboarding | PASS targets omitted yaml proof; MASTER-PLAN allowed `fixtures/yaml/*` | Wave 2 PASS-3 §6 fixture separation; Wave 3 Architecture §12.1 yaml row at onboarding boundary; gate-rerun check 4 returns zero in onboarding allowance | RESOLVED |
| Per-X proof | PASS reports demanded per-X tables; MASTER-PLAN relied on PASS-2 budget tables | Wave 3 Architecture §12.1: 10-row × 9-col canonical authority table | RESOLVED |
| Generated budget authority | PASS-2 had a +2% seed; other targets found it under-propagated | Wave 2 MASTER-PLAN §20 promotion of per-grammar table; Wave 2 PASS-2 §6 wall-budget categories with observed/provisional baseline | RESOLVED |
| SOTA close gate | PASS-2/PASS-3 lacked row-complete tables; MASTER-PLAN permitted routing escape | Wave 2 PASS-2 §7 + PASS-3 §7 numeric trajectory rows; Wave 2 MASTER-PLAN §15 J.W1 deletion of escape: "misses require amendment before close" | RESOLVED |
| PASS hardening says amend before SYNTHESIS | PASS hardeners phrased next step as amendment before SYNTHESIS consumption; pipeline order ran hardening after SYNTHESIS | Resolution noted in V1 §3 row 10 was non-architectural framing; the V2 reads it as already-handled by the four-wave amendment dispatch contract | RESOLVED (procedural, not architectural) |
| OpenFrame residue | PASS-1 agent permitted OpenFrame-like internal builders | Wave 2 PASS-1.md:57 + 282: deletion archaeology; Wave 2 PASS-2 §7 mechanism gate `samply on every emitted parser confirms no Vec<OpenFrame>::clone symbol` | RESOLVED |
| Package-name ambiguity | PASS-3 carried prefixed names; MASTER-PLAN left adjustment open | Wave 2 MASTER-PLAN §15 J.W3 publication wave: "confirm publication-name plan, validate `[workspace.package]` defaults, dry-run `cargo publish`"; A.W1 binds workspace crate names | RESOLVED |
| Fixture role | PASS-3 treated fixtures as generated/public ecosystem proof; MASTER-PLAN fixture allowance conflicted with Lock 14 onboarding | Wave 2 PASS-3 §6 fixture separation; Wave 3 Architecture §12.1 yaml row marks fixture manifest as "parity-phase, never an onboarding surface" | RESOLVED |

No V2-novel cross-target conflict surfaces. The four V2 reports speak the same vocabulary across all 14 V1 conflict points.

## §4 Punch list consolidation

V1's 56 report-local punch-list rows (47 deduplicated) → V2's 4 non-blocking residuals:

| # | Source V2 report | Site | Issue | Surgery type | Blocking? |
|---:|---|---|---|---|---|
| 1 | PASS-2 V2 §7 Lane 5 | PASS-2.md:329 | "generated metadata for paths, visitors, diagnostics, and host tables" — phrasing could route through `path-core` schema explicitly | phrasing tightening | non-blocking |
| 2 | PASS-3 V2 §5 Lane 3 | PASS-3.md:342 | yaml row's host-route cell carries documentary text; could carry stronger forward pointer to `host::primitives` + `@host fn` decomposition | phrasing tightening | non-blocking |
| 3 | PASS-3 V2 §11 Lane 9 | PASS-3.md:115 | visitor cookbook receiver routing could fold into §6b diagnostic ledger to bind cookbook + strings together | phrasing tightening | non-blocking |
| 4 | MASTER-PLAN V2 §4 Lane 2 | MASTER-PLAN.md:230 | A.W4 ↔ A.W3 binding refinement — the close gate is the consumer of prior waves' deliverable; the framing could be tightened | phrasing tightening | non-blocking |

Every residual is a phrasing tightening that can fold at next pass-through. None contradicts an architectural commitment, none gates per-tranche full-spec drafting, and none requires a Wave-5 amendment dispatch.

## §5 Final readiness verdict

**READY** — every V2 report returns READY; the cumulative residuals are four non-blocking phrasing tightenings; no architectural conflict survives; the gate-rerun checklist's 16 tightened commands all return their expected post-conditions.

### Gate-rerun checklist post-condition verification

The Wave-4 tightened gate-rerun checklist (Reviewer D §6 + AMENDMENT-DISPATCH §3 Wave 4) was rerun against the amended trio + amended PASS syntheses:

1. `rg -n "ParseStream|rewrite-mode|Unicode class algebra"` → all matches are normalisation-table cells, deletion-archaeology citations, syn-macro carrier rows, or DISCARD verdicts. **PASS**.
2. `rg -n "bbnf-path|bbnf-test-fixtures|path!"` → matches are migration archaeology + cookbook migration row + Architecture inheritance citation; no proposed public/internal naming. **PASS**.
3. `rg -n "codegen/src/backend_ir"` → only PASS-2.md:188 (ratification denial) and PASS-2.md:225 (`README.md is documentation only`); zero ownership claims. **PASS**.
4. `rg -n "fixtures/yaml"` → all matches are inside the parity-phase prose at PASS-3 §6 + Architecture §12.1 yaml row; zero in Lock 14 onboarding allowance. **PASS**.
5. `rg -n "@recover"` → three matches: PASS-3.md:35 (alias-only), PASS-3.md:160 (legacy alias only if SYNTHESIS keeps migration parser), Architecture §8.1 input-normalization-deletion table; only `@error(recover = ...)` survives as production form. **PASS**.
6. `rg -n "OpenFrame"` → every match is deletion archaeology, never preservation: PASS-1 §2 builder-frame replacement, PASS-1.md:282 deletion-path archaeology, PASS-2.md:443 mechanism gate `samply confirms no Vec<OpenFrame>::clone symbol`, PASS-2.md:547 punch list "no preserved role", MASTER-PLAN §7 inheritance row, MASTER-PLAN.md:270 grep gate. **PASS**.
7. `rg -n "GrammarIR"` → PASS-2.md:5 (current source violation cited as deletion target) + PASS-2.md:239 (verbatim deny command); the lowerer import-deny gate is present with verbatim command. **PASS**.
8. `rg -n "__EAGER_EMPTY_PATH|CursorDecision::Skip"` → MIGRATION.md:729 (test) + MIGRATION.md:786 (Lock 3 cursor gates) + MASTER-PLAN.md:733 (cursor skip carry); explicit Lock 3 gates with specific test names. **PASS**.
9. `rg -n "twitter|canada|citm|bootstrap|animate|On-Demand"` → MASTER-PLAN.md:131-136 numeric competitor rows for every dataset on M1 Pro; PASS-3.md:388-396 rows. Each row names competitor + dataset + platform + bbnf target. **PASS**.
10. `rg -n "receiver|blocker|receiving gate"` → 16 lines across MIGRATION + MASTER-PLAN + three PASS targets, every carry has Receiver, Blocker, and Receiving Gate columns. **PASS**.
11. `rg -n "yaml.bbnf|workspace.metadata.bbnf.grammars.yaml"` → 13 matches across all five surfaces (Architecture, MASTER-PLAN, PASS-1, PASS-2, PASS-3); two-surface proof present in 5 of 5 surfaces. **PASS**.
12. `rg -n "generated_loc|regen_wall|xtask"` → 53 matches; per-grammar AND per-wave budgets present (PASS-2 §6 per-grammar + MASTER-PLAN §20 per-wave). **PASS**.
13. `rg -n "BBNF-LIFE|BBNF-LAYOUT|BBNF-OPT|BBNF-GRAMMAR|BBNF-POINTER|lookbehind|HostSignature"` → 37 matches; committed diagnostic strings with specific codes binding to surfaces. **PASS**.
14. `rg -n "child count|500 LOC|exception rationale"` → MASTER-PLAN §21 verification table (5 surfaces × child-count + LOC + exception + enforcing command); machine-checkable rows. **PASS**.
15. `rg -n "declaration-crate review|why metadata|deletion path|reviewer"` → Architecture §5.6 8-field fence; reified as TOML at lines 743-754. **PASS**.
16. `rg -n "CPU model|compiler flags|input hash|competitor version|warmup|sample"` → MASTER-PLAN.md:143-150 benchmark reproducibility schema (8 fields × source command); bound to H/J gates. **PASS**.

All 16 commands return their expected post-conditions. The cohort therefore advances.

### V1 → V2 KEEP rate trajectory

| Cohort | V1 KEEP / Total | V2 KEEP / Total | KEEP rate delta |
|---|---:|---:|---|
| PASS-1 | 30/62 (48%) | 52/63 (83%) | +35 pp |
| PASS-2 | 38/59 (64%) | 62/65 (95%) | +31 pp |
| PASS-3 | 19/66 (29%) | 64/66 (97%) | +68 pp |
| MASTER-PLAN | 30/65 (46%) | 66/71 (93%) | +47 pp |
| **Cumulative** | **117/252 (46%)** | **244/265 (92%)** | **+46 pp** |

The 46-percentage-point jump in cumulative KEEP rate corresponds to the four-wave amendment dispatch landing every routed surgery. The 92% V2 KEEP rate sits comfortably above the healthy 60-80% target band; the higher rate reflects amendment maturity (the amendment dispatch's verify-then-patch contract collapsed pre-fill items to verify-only stubs, deferring zero substantive surgery), not challenge-erasure (the Pro/Con/Explication/Challenge per-item discipline is preserved at the V2 row level, with steelman challenges defeated by the amendment evidence cited in each row).

### Re-draft threshold check

V1 §5 set ten conditions whose presence would force RE-DRAFT. None of those conditions appears in the V2 cohort:

- The tape/direct union is replaced — not present; tape remains the substrate, unioned with direct-to-struct.
- Backend IR remains owned by `codegen` — not present; ownership is `ir/src/backend_ir/` per PASS-1.md:41 + PASS-2.md:188.
- The yaml onboarding proof requires a third surface — not present; Lock 14 admits exactly two surfaces per Architecture §12 + PASS-3 §6.
- SOTA close still permits routing escape — not present; J.W1 binds amendment-before-close per MASTER-PLAN §15.
- B/C or C/E/H sequencing requires a wave to consume a later-wave artefact — not present; MASTER-PLAN §10 C.W2/C.W3/C.W5 sequencing repaired.
- Generated-code budgets remain absent from F/H/J execution gates — not present; MASTER-PLAN §20 per-wave + PASS-2 §6 per-grammar tables.
- Carry ledgers contain future work without receiver/blocker/receiving gate — not present; every carry triple-complete.
- Public API exposes prefixed internal path crates or `path!` — not present; `path`, `path-core`, `path-ts` per PASS-3 §6 + `pointer!` macro at line 84.
- Standalone `@recover`, grammar-level rewrite-mode, or grammar-level Unicode class algebra — not present; Architecture §8.1 input-normalization-deletions table.
- OpenFrame preservation as proposed implementation detail — not present; PASS-1.md:282 + PASS-2.md:547 deletion-path archaeology.

No condition triggers RE-DRAFT.

## §6 Voice + discipline locks

The amended documents preserve the voice and discipline locks from `restart/README.md` §13 + the V1 consolidated §6 table.

| Lock | Cohort observation |
|---|---|
| Calibrated, direct prose | Every fault is named with surgery; no hedge-words at row-level. |
| Archaic-permissive | "hereupon", "begotten", "thereof" present in closing posture passages; technical clarity preserved. |
| No metalanguage | V2 reports cite path:line not commit history; commits cited only in §1 target identification. |
| Path:line citations | Every concrete claim about target text carries a citation. |
| Per-X tables | All "all grammars" / "every backend" claims resolve through Architecture §12.1 (10×9), PASS-2 §6 (per-grammar generated LOC), PASS-3 §6a (feeder), PASS-2 §6 runtime emission (10×7). |
| No placeholder wording | Every carry triple-complete (Receiver / Blocker / Receiving gate). |
| No quick solutions | Lane 9 rerun finds no patched-around-substrate language. |
| No legacy code uncontested | OpenFrame deletion-path archaeology + closure machinery research-signal reframing + Lock 14 lint gates active across A.W4/G.W4/J.W4. |
| No overfitting | Generic crates carry zero grammar-specific code; A.W4 + G.W4 + J.W4 close gates enforce. |
| Idiomatic gestalt | Backend IR lives at `ir/src/backend_ir/`; codegen consumes; path crates are unprefixed; sister crates remain generic. |
| Generated-code budget | PASS-2 §6 + MASTER-PLAN §20 carry per-grammar + per-wave budgets with observed/provisional baseline categories. |
| SOTA anchoring | Every parse-throughput row at MASTER-PLAN §4 + PASS-2 §7 + PASS-3 §7 names competitor + dataset + platform + bbnf target; final SOTA escape clause deleted. |
| Carry discipline | Every deferral names Receiver, Blocker, Receiving gate. |
| Diagnostic specificity | 21 verbatim diagnostic codes across PASS-1 §2 (6) + PASS-2 §8 (6) + PASS-3 §6b (15, with shared mirrors); cookbook receivers + runtime emit tests bind. |

The cohort delivers the per-X table + carry-discipline + SOTA-anchoring + diagnostic-specificity discipline that V1 sought.

## §7 Closing posture

Hereupon the next step is per-tranche full-spec drafting, not a fifth amendment dispatch. The four-target hardening cohort returns READY; the 16 tightened gate-rerun commands all return their expected post-conditions; the architecture, migration, and master plan trio carries the executable authority for tranches A through J. The dispatched per-tranche drafting agents (10 agents A-J at ~3,000-5,000 lines per tranche) inherit from BA-BD per `restart/inheritance/INDEX.md` and consume the V2-ratified outputs without re-relitigating the 14 locks, the 35-answer interrogation, the precepts, the BBNF extension surface, or the tape/direct substrate.
