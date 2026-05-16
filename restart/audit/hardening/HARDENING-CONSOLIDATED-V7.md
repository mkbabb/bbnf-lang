# HARDENING-CONSOLIDATED-V7

## §1 Target identifications

V7 hardening verifies that the Phase 7 fold (V1 absorption of post-V1 deferrals per `restart/research/V1-FOLD-CANDIDATES.md`, commit `652f86bb`) preserves V6's READY verdict. Phase 7.1 landed lock + ARCHITECTURE + Backend trait + 6-directive grammar amendments at commits `adbaaaa0` + `9cb92284`; Phase 7.2 landed surface fold across PASS-1 / PASS-2 / PASS-3 / SYNTHESIS trio at commits `cb690115` / `c45d74ec` / `3863e601` / `3dc95460` / `7cd09ea8` / `d9414a2f` / `71e2d540` / `c8fb1506`.

| Target | Audited surface | V7 report | Report commit | Lines | Verdict |
|---|---|---|---|---:|---|
| PASS-1 | `restart/audit/pass-1-substrate/PASS-1.md` | `restart/audit/hardening/HARDENING-PASS-1-V7.md` | `6011e7f2` | 251 | READY |
| PASS-2 | `restart/audit/pass-2-codegen/PASS-2.md` | `restart/audit/hardening/HARDENING-PASS-2-V7.md` | `0fc7079d` | 353 | READY |
| PASS-3 | `restart/audit/pass-3-runtime/PASS-3.md` | `restart/audit/hardening/HARDENING-PASS-3-V7.md` | `7d5fa7fe` | 251 | READY |
| MASTER-PLAN | `restart/ARCHITECTURE.md`, `restart/MIGRATION.md`, `restart/MASTER-PLAN.md` | `restart/audit/hardening/HARDENING-MASTER-PLAN-V7.md` | `35c14997` | 387 | AMENDMENT-REQUIRED |

| Cohort | KEEP/READY | AMENDMENT items | DISCARD | Punch-list rows | Final verdict |
|---|---:|---:|---:|---:|---|
| Four-target V7 cohort | 3 of 4 READY | 10 (MASTER-PLAN cite-hygiene + cross-document coherence) | 0 | 14 (10 MASTER-PLAN + 4 PASS-1 friction-residuals) | **AMENDMENT-REQUIRED** |

The cohort cumulative KEEP fraction across reports: PASS-1 19 KEEP / 0 amend; PASS-2 29 KEEP / 0 amend (4 non-blocking residues); PASS-3 23 KEEP / 0 amend; MASTER-PLAN trio AMENDMENT-REQUIRED with 10 narrow faults. The Phase 7 fold preserves V6 READY across the three single-file targets; the trio carries cite-hygiene faults that follow a fold of this scope.

## §2 Cohort verdict — per-lane consolidated table

| Lane | PASS-1 | PASS-2 | PASS-3 | MASTER-PLAN | Cumulative |
|---|---|---|---|---|---|
| 1 Lock-Adherence | KEEP | KEEP | KEEP | AMEND-REQ (P1, P6) | AMEND-REQ |
| 2 Sequencing | N/A | N/A | N/A | AMEND-REQ (P9) | AMEND-REQ |
| 3 Cohesion | KEEP | KEEP | KEEP | AMEND-REQ (P2, P3, P5, P10) | AMEND-REQ |
| 4 SOTA-Anchoring | KEEP | KEEP | KEEP | KEEP | KEEP |
| 5 Grammar-Authoritative | KEEP | KEEP | KEEP | KEEP | KEEP |
| 6 Generated-Code-Budget | KEEP | KEEP | KEEP | KEEP | KEEP |
| 7 Friction-Forecast | KEEP | KEEP | KEEP | AMEND-REQ (P7) | AMEND-REQ |
| 8 Carry-Deferral | KEEP | KEEP | KEEP | AMEND-REQ (P4) | AMEND-REQ |
| 9 Greenfield-Discipline | KEEP | KEEP | KEEP | AMEND-REQ (P8) | AMEND-REQ |

**5 of 9 lanes return AMEND-REQ at MASTER-PLAN; remaining 4 lanes return KEEP across all 4 targets.**

## §3 Phase 7 fold verification ledger

The eight Phase 7.2 fold commits absorbed the V1-FOLD-CANDIDATES (30 items across 4 tiers). V7 verifies each landed:

| Fold cluster | V7 evidence | Verdict |
|---|---|---|
| **DK13 higher-rank polymorphism** (Tier 1 #2) | PASS-1.md:73; ARCH §8.2 amendment | LANDED |
| **GADT hidden substrate** (Tier 1 #9) | PASS-1.md:81; Lock 4 amendment | LANDED |
| **Internal row polymorphism** (Tier 1 #8) | PASS-1.md:81 | LANDED |
| **Schema-mining miner** (Tier 1 #7) | PASS-1.md:79 | LANDED |
| **Function values + types in `Type`** (Tier 1 #3) | PASS-1.md:75; ARCH §8.1 grammar | LANDED |
| **Function-typed `@host fn` parameters** (Tier 1 #4 — transducer apotheosis) | PASS-1.md:75 | LANDED |
| **Lambda literal `\|x\| body`** (Tier 1 #5) | PASS-1.md §6 grammar | LANDED |
| **Closure capture by `&'i` only** (Tier 1 #6) | PASS-1.md:75/:248; PASS-2.md:194; PASS-3.md:191 | LANDED |
| **Backend trait** (NEW per user redirect) | ARCH §7.5 (1067-1144); PASS-2.md:134-144 | LANDED |
| **6-directive grammar** (`Directive = ImportDecl \| HostFn \| ErrorDecl \| LayoutDecl \| PrettyDecl \| TokenDecl`) | PASS-1 §6 :197/:201/:203; ARCH §8.1 (1158/1210-1235) | LANDED with cohesion residue (P6) |
| **Lock amendments × 7** (Lock 4 / 5 / 6 / 7 / 8 / 10 / 12) | `restart/locks/LOCKS.md` post-Phase-7.1 | LANDED with cohesion residue (P1) |
| **`pointer!` → `path!` rename (~58 sites)** | PASS-3 :22 sites; MASTER-PLAN 6 sites; MIGRATION 0 active sites; **PASS-2 + ARCH §7.5 still carry `pointer!`** | PARTIAL (V7 surfaces this as cross-target residue) |
| **`BBNF-POINTER-*` → `BBNF-PATH-*` codes** | PASS-3 ledger 7 hits; MASTER-PLAN/MIGRATION clean; **ARCH §7.4 catalogue still carries BBNF-POINTER-*** | PARTIAL (P7) |
| **`bbnf-regex` → `parse-that-regex` rename** | All targets clean | LANDED |
| **regex-automata oracle removal** | PASS-1/2/3/MASTER-PLAN/MIGRATION clean; **ARCH §7.2:935 still carries oracle role** | PARTIAL (P8) |
| **Egraph decoupling from csp-solver** | Lock 6 amendment; PASS-2.md:401 cites `passes::bridge` | LANDED |
| **`RegexDfa` → `RegexProgram` rename** | PASS-2.md 8 sites; ARCH §7.2:935 | LANDED |
| **Public `format()` method** | PASS-3.md:77 on DocumentView | LANDED |
| **Match expression + tuples in `@host fn` body** | PASS-1.md §6 grammar + §3 typing | LANDED |
| **D wave count growth (5 → 6)** | MASTER-PLAN.md D.W5 + D.W6 | LANDED |
| **`path-ts` + WASM/TS deferred post-V1** | PASS-3 6 hits; MASTER-PLAN routed; Lock 5/7 amended | LANDED |
| **Tier 4 architectural prerequisites** (rewrite-budget, lint manifest, declaration-crate review form, cookbook contract) | ARCH §10.1 + §13.1; **MASTER-PLAN cites phantom anchors P2/P3/P10** | PARTIAL (P2/P3/P10) |
| **5 lock amendments per audit #7** + 3 NEW locks (Lock 4/6/10) | All landed | LANDED with H wave count residue (P1) |

Eighteen of twenty-one fold clusters land cleanly; three are PARTIAL with cross-target residue surfaced as the V7 punch list.

## §4 Cross-target conflicts

V7 surfaces three classes of cross-target conflict, all narrow:

| # | Conflict | Sources | Resolution |
|---|---|---|---|
| 1 | `pointer!` rename incomplete | PASS-2 + ARCH §7.5 still carry `pointer!`; PASS-3 + MASTER-PLAN + MIGRATION renamed | Single-pass corpus-wide `pointer!` → `path!` rename in PASS-2 + ARCH §7.5 (~6 sites total) |
| 2 | `BBNF-POINTER-*` codes survive in ARCH §7.4 catalogue | ARCH §7.4 vs PASS-3 + MASTER-PLAN | Rename ARCH §7.4 catalogue codes to `BBNF-PATH-*`; rewrite verbatim help-text |
| 3 | regex-automata oracle role in ARCH §7.2:935 | ARCH §7.2:935 contradicts ARCH §12.2 + §13.1 retirement | Remove oracle clause from ARCH §7.2:935; replace with parse-that-regex internal cross-engine parity per audit #4 |

## §5 Punch list consolidation

The 14 V7 punch items distribute across MASTER-PLAN (10 hard items P1-P10) + PASS-1 (4 friction residuals R1-R4) + PASS-2 (4 V7-introduced residues which include P-cross-target overlap):

### MASTER-PLAN trio (10 hard items; cite-hygiene + cross-document coherence)

| # | Surgery | Path:line | Severity |
|---:|---|---|---|
| P1 | Lock 8 cites `H.W5`; H wave dropped 6→5 (H.W0-H.W4 only). Update Lock 8 wave references. | `restart/locks/LOCKS.md:48` | Cite hygiene |
| P2 | "ARCH §13 appendix" for declaration-crate review form template; template lives in ARCH §5 (lines 738-770). Fix the cite. | `restart/MASTER-PLAN.md:771` | Cite hygiene |
| P3 | "ARCH §13 appendix (landed Phase 7.1)" for cookbook page contract template; no such section authored. Either author the section or fix the cite. | `restart/MASTER-PLAN.md:797` | Cite + missing-author |
| P4 | MIGRATION broadens Lock 12 A.W0 archive ceremony to include `bbnf-path-ts`; Lock 12 names only `ser` + `gorgeous`. Drop `bbnf-path-ts` from MIGRATION row. | `restart/MIGRATION.md:71` | Lock-text overreach |
| P5 | C.W4 cite over-promises §10.1: "fail-closed posture, representative-stability protocol now landed" — neither phrase appears. Soften to "rewrite-budget categories landed". | `restart/MASTER-PLAN.md:321` | Cite over-promise |
| P6 | Lock 10 production name `Directive` vs ARCH §8.1 production name `Item`. Reconcile to one production name. | `restart/locks/LOCKS.md:52`; `restart/ARCHITECTURE.md:1158` | Cross-document drift |
| P7 | BBNF-POINTER-* survives in ARCH §7.4 catalogue while MASTER-PLAN §25 uses BBNF-PATH-*. Rename catalogue codes to BBNF-PATH-*. | `restart/ARCHITECTURE.md:1044-1046, 1579` | Rename cascade incomplete |
| P8 | ARCH §7.2:935 keeps `regex-automata` oracle role; contradicts §12.2 + §13.1 retirement. Remove oracle clause. | `restart/ARCHITECTURE.md:935` | User mandate violation |
| P9 | H tranche prose at MASTER-PLAN.md:174/:194 references `H.W5`; H wave dropped 6→5. Downstream cleanup of P1. | `restart/MASTER-PLAN.md:174, 194` | Cite hygiene |
| P10 | "§5.6 fence" referenced 3× but no §5.6 sub-section header authored. Either author the header or fix the cite path. | `restart/ARCHITECTURE.md:1247, 1559, 1584` | Phantom anchor |

### PASS-1 friction residuals (4 non-blocking; could roll into amendment cycle)

| # | Surgery | Path:line | Severity |
|---:|---|---|---|
| R1 | Add lock-number cross-references (`Lock 4`/`Lock 10`) to PASS-1 §3 | PASS-1.md §3 | Friction |
| R2 | Add Backend trait cross-reference to PASS-1 §2 pointing at ARCH §7.5 | PASS-1.md §2 | Friction |
| R3 | Append verbatim parse-error message for closure-capture-by-move | PASS-1.md:248 | Friction |
| R4 | Sync `BBNF-PATTERN-NONEXHAUSTIVE` into ARCH §7.4 catalogue | ARCH §7.4 | Friction |

### PASS-2 V7-introduced residues (4 non-blocking)

These already overlap with P-cross-target items above; tracked for completeness:
- R-V7-1: `pointer!` → `path!` corpus rename (overlaps P-cross-target #1)
- R-V7-2: SOTA citation for Backend trait pattern (LLVM TargetMachine / Cranelift TargetIsa / swc Compiler<W>)
- R-V7-3: optional `BBNF-FNVAL*` dedicated diagnostic codes
- R-V7-4: §10 closing-posture refresh to name Backend trait

## §6 Final readiness verdict

**AMENDMENT-REQUIRED** with 10 narrow MASTER-PLAN cite-hygiene faults + 1 cross-target rename completion (`pointer!` + BBNF-POINTER-* + regex-automata oracle in ARCH).

Re-draft thresholds (`HARDENING-CONSOLIDATED.md` §5; 10 conditions): zero met. Tape/direct union holds. Backend IR ownership holds. yaml two-surface proof holds. Numeric SOTA gates hold. B/C and C/E/H sequencing hold. Generated-code budgets hold. Carry ledgers hold. `pointer!` → `path!` rename mostly holds. `@error(recover)` holds. OpenFrame archaeology holds.

Decision rule: AMENDMENT-REQUIRED with narrow cycle (≤14 items; 3 cross-target cluster + 10 MASTER-PLAN local + 4 PASS-1 friction). Single SYNTHESIS narrow-amendment agent + single PASS-2 narrow-amendment agent (rename only) absorbs the entire punch list in ~60-90 minutes wall.

## §7 Recommended Phase 7.5 narrow-amendment cycle

Phase 7.5 dispatches 2 parallel narrow agents:

**Agent A — SYNTHESIS narrow** (~60-75 min):
- ARCH §7.5: rename remaining `pointer!` → `path!` (PASS-2 + ARCH §7.5 sites; ~6 total)
- ARCH §7.4: rename `BBNF-POINTER-*` catalogue codes to `BBNF-PATH-*`; rewrite verbatim help-text
- ARCH §7.2:935: remove regex-automata oracle clause; replace with parse-that-regex cross-engine parity
- ARCH §1247/§1559/§1584: author missing §5.6 sub-section header OR rewrite cite path
- Lock 8: update H.W5 reference to H.W4 (P1 closure)
- Lock 10: reconcile production name `Directive` vs ARCH §8.1 `Item` (P6)
- MASTER-PLAN.md:771: fix cite to ARCH §5 (P2)
- MASTER-PLAN.md:797: author or remove cite (P3)
- MASTER-PLAN.md:321: soften over-promise (P5)
- MASTER-PLAN.md:174/:194: cleanup H.W5 references (P9)
- MIGRATION.md:71: drop bbnf-path-ts overreach (P4)
- ARCH §7.4: sync BBNF-PATTERN-NONEXHAUSTIVE (R4)

**Agent B — PASS-1 friction** (~30 min, light):
- PASS-1.md §3: add Lock 4/10 cross-references (R1)
- PASS-1.md §2: add ARCH §7.5 cross-reference (R2)
- PASS-1.md:248: append verbatim closure-by-move parse error (R3)

Phase 7.5 closes when both commit. Optional V7.1 rerun (single MASTER-PLAN trio agent, ~45 min) verifies the residue. Total Phase 7.5 + V7.1 wall: ~90-120 min.

After Phase 7.5 + V7.1 READY: per-tranche full-spec drafting (Wave 8+) unblocks.

## §8 LLM-pathology summary across cohort

V7 applied lenses F (LLM bias), G (overfitting), H (hallucination) per the V5+ spec. Cohort findings:

- **Lens F (LLM bias)**: zero pathology in PASS-1, PASS-2, PASS-3. MASTER-PLAN trio surfaces P5 (cite over-promise — language not landed but cited as landed) which is mild over-confident phrasing rather than active hedging.
- **Lens G (overfitting)**: zero pattern-lift detected. Backend trait surface design (PASS-2 V7) verified non-pathological — 5-method shape composes without LLVM TargetMachine method-set baggage; per-backend obligation table maps method to producer concretely; V2 deferral routes mechanical-expansion via trait pre-existence.
- **Lens H (hallucination)**: PASS-1 V7 verified 5 primary citations (Pierce 2002 ch.22, Damas-Milner POPL 1982, Dunfield-Krishnaswami ICFP 2013, Milner JCSS 1978, Pottier-Rémy 2005) — all correct. MASTER-PLAN trio surfaces 4 phantom anchors (P2 — wrong section number; P3 — section authored cite without authoring; P10 — §5.6 fence referenced but no header authored). All are cite-hygiene, not invention. PASS-2 V7 found 1 H7 cross-document residue (`pointer!` rename) consistent with V7 cross-target conflict #1.

Pathology summary: zero invention; zero hedging; zero pattern-lift. Cite hygiene is the dominant residue surface, consistent with the size of the Phase 7 fold (8 commits across 5 surfaces).

## §9 Voice + discipline locks

The Phase 7 fold preserves voice and discipline locks per `restart/README.md` §13. Calibrated, direct prose. Archaic-permissive (hereupon, therein, thereof). No metalanguage. Path:line citations on every concrete claim. Tables liberal where they serve. Per-X tables for "all backends" / "all targets" / "all topics" claims. Receiver / blocker / receiving-gate triple on every carry. No quick solutions. No legacy code uncontested. The V7 cohort verifies the fold did not introduce voice violations.

## §10 V6 → V7 progression

| Cycle | Cohort verdict | Defining work |
|---|---|---|
| V6 | READY (all 4 targets) | Research-fold absorption; eight topic deep-dives folded into trio + PASS surfaces; egglog/fusion rationale at Lock 4. |
| Deferral-audit cohort | (informational) | Eight parallel audits surfaced 30 V1 fold candidates across 4 tiers; user adjudicated 8 open questions. |
| Phase 7.1 | (lock + ARCH amendments) | 7 lock amendments + Backend trait at ARCH §7.5 + 6-directive grammar + parse-that-regex naming. |
| Phase 7.2 | (surface fold; 4 parallel) | DK13 + GADT substrate + row poly + schema miner + function values + lambda + closure-`&'i` + match/tuple + format() + path! rename + parse-that-regex cascade + Backend trait integration + RegexProgram rename + egraph decoupling + WASM/TS defer + D wave growth. |
| V7 | AMENDMENT-REQUIRED (3 of 4 READY; MASTER-PLAN 10 narrow faults) | 4 parallel V7 hardeners verified Phase 7 fold; surfaced cross-target rename incompletion + cite-hygiene + cross-document coherence faults. |

## §11 Closing posture

Phase 7 absorbed 30 V1 fold candidates across 4 tiers — DK13 higher-rank, GADT hidden substrate, row poly, schema miner, function values + lambdas + closures, Backend trait, parse-that-regex anchor, path! rename, format() public method, WASM/TS defer. Three of four V7 audit targets returned READY; the MASTER-PLAN trio carried 10 narrow cite-hygiene faults that follow a fold of this scope. Phase 7.5 (~90-120 min wall) absorbs the residue + V7.1 rerun verifies; per-tranche full-spec drafting (Wave 8+) unblocks after.

The architecture is one narrow amendment cycle from V7 READY. Hereupon Phase 7.5 dispatches.
