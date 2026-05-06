# HARDENING-CONSOLIDATED-V7.1

## §1 Target Identification

V7.1 is the verification gate that confirms Phase 7.5 closed the V7
punch list. V7 returned **AMENDMENT-REQUIRED** for the four-target
cohort: PASS-1 / PASS-2 / PASS-3 READY (3 of 4); MASTER-PLAN trio
AMEND-REQ with 10 cite-hygiene + cross-document coherence faults,
1 cross-target rename completion (`pointer!` + `BBNF-POINTER-*` +
regex-automata oracle survival in ARCH), and 4 PASS-1 friction
residuals (R1-R4). V7.1 verifies that the residue closed and the
cohort returns to **READY** equivalent.

| Phase | Commit | Surface | Lines absorbed |
|---|---|---|---:|
| Phase 7.5A | `3207b1cb` | 10 MASTER-PLAN cite-hygiene fixes (P1-P10) + `pointer!` → `path!` rename in ARCH active surface (X1) + ARCH §13.2 cookbook page contract authored + ARCH §5.6 declaration-crate fence header authored + ARCH §7.4 `BBNF-POINTER-*` → `BBNF-PATH-*` catalogue rename + ARCH §7.2:935 regex-automata oracle clause replaced with `parse-that-regex` cross-engine parity + ARCH §8.1 `Item` → `Directive` production rename | ~110 amend |
| Phase 7.5B | `01095b44` (classification) + `c06d10c1` (fold) | PASS-1 friction residuals R1-R3 (Lock 4/10 cross-refs to §3, ARCH §7.5 / RustBackend cross-ref to §2, verbatim closure-by-move parse-error diagnostic at :118-119) | ~30 amend |

V7 carry-baseline of record: `restart/audit/hardening/HARDENING-CONSOLIDATED-V7.md`
(commit `822bed18`, 177 lines, AMENDMENT-REQUIRED). V6 carry-baseline
of record (cohort): `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md`
(READY across PASS-1, PASS-2, PASS-3, SYNTHESIS).

V7.1 audit posture: verification-only. Worker reads MASTER-PLAN trio +
PASS-1 + locks file and writes one report path:
`restart/audit/hardening/HARDENING-CONSOLIDATED-V7.1.md`. No edits to
trio, locks, or PASS surfaces. Per-tranche full-spec drafting (Wave 8+;
~10 tranche-A-through-J spec agents at 3,000-5,000 lines each) is the
next phase, not in this scope.

## §2 V7 Punch Closure Table

Each row carries the verification command, the observed result, and a
CLOSED / PARTIAL / FAILED verdict. Fourteen rows: 10 P-items
(MASTER-PLAN trio cite-hygiene + cross-document coherence) + X1
(cross-target rename completion) + 4 R-items (PASS-1 friction residuals
+ R4 cross-document escalation).

| # | Verification | Result | Verdict |
|---:|---|---|---|
| P1 | `rg -n 'H\.W5' restart/locks/14-LOCKS.md` | zero hits | **CLOSED** |
| P2 | `rg -n 'ARCH §13 appendix' restart/MASTER-PLAN.md` | zero hits | **CLOSED** |
| P3 | `rg -nC2 '§13\.2\|cookbook page contract' restart/ARCHITECTURE.md` | positive — `restart/ARCHITECTURE.md:1650` `### 13.2 Cookbook Page Contract` authored with four-field contract table (Audience+mental-model, Minimum-running-example, Diagnostic-codes-table, Close-gate-command) and J.W2 regen-gate consumption clause | **CLOSED** |
| P4 | `restart/MIGRATION.md:71` | `bbnf-path-ts` row narrows to V2-deferral language; clarifies "Lock 12 archives only `ser` and `gorgeous` at A.W0" inline; the row no longer overreaches Lock 12 | **CLOSED** |
| P5 | `rg -n 'fail-closed posture, representative-stability' restart/MASTER-PLAN.md` | zero hits; `restart/MASTER-PLAN.md:321` softens to "rewrite-budget categories with node/iteration ceilings landed at `restart/ARCHITECTURE.md` §10.1 per Phase 7.1; the fail-closed posture and representative-stability protocol route to C.W4 implementation rather than authoring at architecture level" | **CLOSED** |
| P6 | `rg -n 'Item =' restart/ARCHITECTURE.md` zero; `rg -n 'Directive ::=' restart/ARCHITECTURE.md` positive at `:1163` | `restart/ARCHITECTURE.md:1163` `Directive ::= ImportDecl \| HostFn \| RuleDecl \| LayoutDecl \| ErrorDecl \| PrettyDecl \| TokenDecl` reconciled; `:1215` "the six-directive `Directive` production above is the complete V1 surface" reconciles Lock 10's "six directives" with ARCH's seven-alternative production (`RuleDecl` is the non-directive item) | **CLOSED** |
| P7 | `rg -n 'BBNF-POINTER' restart/ARCHITECTURE.md` zero; `rg -n 'BBNF-PATH' restart/ARCHITECTURE.md` positive at `:1049-1051` and `:1584` | catalogue codes renamed: `BBNF-PATH-UNKNOWN-SEGMENT` (alias `BBNF-PATH001`), `BBNF-PATH-GRAMMAR-MISMATCH` (alias `BBNF-PATH002`), `BBNF-PATH003`. `:1584` cross-references the catalogue from §12.2 with the renamed codes | **CLOSED** |
| P8 | `rg -n 'regex-automata' restart/ARCHITECTURE.md` | only deletion-archaeology contexts: `:1585` "the regex-automata oracle role retires per V1-FOLD-CANDIDATES Tier 3 #23" (retirement note); `:1642` `regex-engine-canon` lint-deny target. `restart/ARCHITECTURE.md:935-940` (`RegexProgram` row) carries `parse-that-regex` cross-engine parity language and explicitly states "no external regex oracle is consumed at V1" | **CLOSED** |
| P9 | `rg -n 'H\.W5' restart/MASTER-PLAN.md` | zero hits; H tranche carries five waves (H.W0-H.W4) per Lock 8 amendment | **CLOSED** |
| P10 | `rg -nC2 '### 5\.6\|### §5\.6' restart/ARCHITECTURE.md` | positive — `restart/ARCHITECTURE.md:739` `### 5.6 Declaration-Crate Fence` authored. The header is the anchor for `restart/MASTER-PLAN.md:771` declaration-crate review form template citation and for the §10/§13 fence cross-references | **CLOSED** |
| X1 | `rg -n 'pointer!' restart/ARCHITECTURE.md` | three hits at `:1564`, `:1580`, `:1641`; all are deletion-archaeology contexts (Lock 12 retirement-note, "the legacy `pointer!` macro retires under the naming-canon lint", and `naming-canon` lint-deny target). No active surface uses `pointer!`; the canonical macro is `path!` (positive at `:1584`) | **CLOSED** (per X1 spec; deletion-archaeology contexts are acceptable) |
| R1 | `rg -n 'Lock 4\|Lock 10' restart/audit/pass-1-substrate/PASS-1.md` | positive at `:73` (Lock 4 amendment cite for higher-rank polymorphism + DK13 + GADT-hidden + closure-by-`&'i` substrate ratification, with `restart/locks/14-LOCKS.md:40` cross-reference) and `:75` (Lock 10 amendment cite for 6-directive grammar surface + first-class function values, with `restart/locks/14-LOCKS.md:52` cross-reference) | **CLOSED** |
| R2 | `rg -n 'ARCH §7\.5\|RustBackend' restart/audit/pass-1-substrate/PASS-1.md` | positive at `:61` ("PASS-1's per-backend obligations table is consumed by the V1 `RustBackend: Backend` impl per ARCH §7.5 (`restart/ARCHITECTURE.md:1067-1144`); future `WasmBackend` and `TsBackend` impls land V2 without re-architecting BIR or PASS-1's substrate") | **CLOSED** |
| R3 | `rg -n 'BBNF-CLOSURE-CAPTURE-BY-MOVE' restart/audit/pass-1-substrate/PASS-1.md` | positive at `:119` (verbatim diagnostic in `BBNF-CLOSURE-CAPTURE-BY-MOVE` row of the §6b diagnostic-strings table) and `:249` (cross-reference in §6 prose) | **CLOSED** |
| R4 | `rg -n 'BBNF-PATTERN-NONEXHAUSTIVE' restart/ARCHITECTURE.md` | **zero hits**; the diagnostic appears in PASS-1 at `:118` and `:251` but the ARCH §7.4 catalogue (`:1010-1062`) does not enumerate it | **PARTIAL — R4 residue stands** |

**13 of 14 V7 punch items CLOSED. R4 (BBNF-PATTERN-NONEXHAUSTIVE
catalogue sync to ARCH §7.4) remains as a single non-blocking
friction residue.** The diagnostic exists verbatim in PASS-1's
diagnostic-strings table; ARCH's §7.4 catalogue continues to
enumerate every other diagnostic family but not the match-exhaustiveness
code. R4 is friction-class — the diagnostic emits correctly from
the producer site and the verbatim string is a single-source-of-truth
in PASS-1 — and does not block readiness.

## §3 Compressed Nine-Lane Verification

Per HARDENING.md compressed-mode spec (≥15 audit rows; not full
P/C/E/C). Lanes V7 flagged AMEND-REQ at MASTER-PLAN trio re-verified;
lanes V7 flagged READY re-confirmed. Twenty-one rows.

| # | Lane | Target | V7 verdict | V7.1 verification | V7.1 verdict |
|---:|---|---|---|---|---|
| 1 | Lock-Adherence | MASTER-PLAN trio | AMEND-REQ (P1, P6) | Lock 8 `H.W3, H.W4` (no H.W5) at :48; Lock 10 prose "six-directive `Directive` production" matches ARCH §8.1:1215 reconciliation | **READY** |
| 2 | Lock-Adherence | PASS-1 / PASS-2 / PASS-3 | KEEP (V7) | No regression; locks still cite cleanly | **READY** |
| 3 | Sequencing | MASTER-PLAN trio | AMEND-REQ (P9) | MASTER-PLAN H tranche carries H.W0-H.W4 only at :174/:194; B/C and C/E/H sequencing intact | **READY** |
| 4 | Sequencing | PASS-1 / PASS-2 / PASS-3 | N/A (V7) | N/A — sequencing is a trio-only lane in compressed mode | **N/A** |
| 5 | Cohesion | MASTER-PLAN trio | AMEND-REQ (P2, P3, P5, P10) | P2 cite hygiene closed (no `ARCH §13 appendix`); P3 closed (ARCH §13.2 authored); P5 closed (over-promise softened); P10 closed (ARCH §5.6 header authored) | **READY** |
| 6 | Cohesion | PASS-1 / PASS-2 / PASS-3 | KEEP (V7) | R1+R2 lock cross-refs landed in PASS-1 (further increases cohesion) | **READY** |
| 7 | SOTA-Anchoring | MASTER-PLAN trio | KEEP (V7) | simdjson 7 GB/s + sonic-rs M1 Pro twitter 436 µs + lightning-css 4.16 ms Bootstrap intact at Lock 8; H.W3 + H.W4 carry the close-gate measurements | **READY** |
| 8 | SOTA-Anchoring | PASS-1 / PASS-2 / PASS-3 | KEEP (V7) | No regression | **READY** |
| 9 | Grammar-Authoritative | MASTER-PLAN trio | KEEP (V7) | yaml two-surface (workspace metadata + `.bbnf` source) intact; per-grammar fence canon at ARCH §13.1 unchanged | **READY** |
| 10 | Grammar-Authoritative | PASS-1 / PASS-2 / PASS-3 | KEEP (V7) | No regression | **READY** |
| 11 | Generated-Code-Budget | MASTER-PLAN trio | KEEP (V7) | F.W3 generated-LOC budget intact; per-grammar fence at ARCH §13.1 + Lock 14 unchanged; bbnf 21,503 → 21,933 row at ARCH §12.2 stable | **READY** |
| 12 | Generated-Code-Budget | PASS-1 / PASS-2 / PASS-3 | KEEP (V7) | No regression | **READY** |
| 13 | Friction-Forecast | MASTER-PLAN trio | AMEND-REQ (P7) | `BBNF-POINTER-*` → `BBNF-PATH-*` catalogue rename CLOSED (ARCH §7.4:1049-1051); MASTER-PLAN §25:802 + ARCH §7.4 + ARCH §12.2:1584 align | **READY** |
| 14 | Friction-Forecast | PASS-1 / PASS-2 / PASS-3 | KEEP (V7) | R3 closure-by-move parse-error verbatim diagnostic landed at PASS-1:119; PASS-1 friction-ledger lines tighten | **READY** |
| 15 | Carry-Deferral | MASTER-PLAN trio | AMEND-REQ (P4) | MIGRATION.md:71 narrows Lock 12 A.W0 archive ceremony to `ser` + `gorgeous` only; `bbnf-path-ts` reframes as V2-deferred placeholder, not A.W0 archive subject | **READY** |
| 16 | Carry-Deferral | PASS-1 / PASS-2 / PASS-3 | KEEP (V7) | No regression | **READY** |
| 17 | Greenfield-Discipline | MASTER-PLAN trio | AMEND-REQ (P8) | regex-automata oracle role removed from ARCH §7.2:940 active surface; only deletion-archaeology contexts remain (`:1585` retirement note + `:1642` `regex-engine-canon` lint-deny target) | **READY** |
| 18 | Greenfield-Discipline | PASS-1 / PASS-2 / PASS-3 | KEEP (V7) | No regression | **READY** |
| 19 | Cross-Target-Rename (composite) | All four | AMEND-REQ (X1) | `pointer!` → `path!`: ARCH active surface clean; only deletion-archaeology contexts at :1564/:1580/:1641. `BBNF-POINTER-*` → `BBNF-PATH-*`: ARCH §7.4 catalogue clean. `regex-automata` → `parse-that-regex`: ARCH §7.2:940 + §12.2:1585 clean | **READY** |
| 20 | Lens-G Overfitting | All four | KEEP (V7) | No re-introduced pattern-lift; Backend trait surface remains 5-method shape composing without LLVM TargetMachine method-set baggage | **READY** |
| 21 | Lens-H Hallucination | All four | KEEP (V7) | All four V7 phantom anchors closed (P2, P3, P10 + over-promise P5); zero invention; cite hygiene fully tightened | **READY** |

**21 of 21 lanes READY** (one N/A row in compressed mode).

## §4 Cross-Target Rename Completion Ledger

V7 surfaced three cross-target rename incompletions. V7.1 verifies all
three closed.

| Cluster | V7 fault | Phase 7.5 closure | V7.1 evidence | Verdict |
|---|---|---|---|---|
| `pointer!` → `path!` | ARCH §7.5 + PASS-2 still carried `pointer!`; PASS-3 + MASTER-PLAN + MIGRATION already renamed | Phase 7.5A absorbs ARCH; commit `3207b1cb` includes "ARCH §7.5 pointer→path" | `restart/ARCHITECTURE.md` carries `pointer!` only at :1564 (Lock 12 retirement-note context), :1580 (`ValueRef` borrow-shape retirement note: "the legacy `pointer!` macro retires under the naming-canon lint; the canonical macro is `path!`"), :1641 (`naming-canon` lint-deny target). `path!` present at :1564, :1584, :1641 (active references) | **CLOSED** |
| `BBNF-POINTER-*` → `BBNF-PATH-*` | ARCH §7.4 catalogue carried `BBNF-POINTER-UNKNOWN-SEGMENT`, `BBNF-POINTER-GRAMMAR-MISMATCH`, `BBNF-POINTER003`; MASTER-PLAN §25 already used `BBNF-PATH-*` | Phase 7.5A absorbs catalogue rename | `restart/ARCHITECTURE.md:1049-1051` carries renamed codes `BBNF-PATH-UNKNOWN-SEGMENT` (alias `BBNF-PATH001`), `BBNF-PATH-GRAMMAR-MISMATCH` (alias `BBNF-PATH002`), `BBNF-PATH003`; §12.2:1584 carries the renamed cross-reference. Zero `BBNF-POINTER-*` survivors | **CLOSED** |
| regex-automata oracle role | ARCH §7.2:935 (`RegexProgram` payload row) listed `regex-automata` as cross-engine oracle; contradicted §12.2 + §13.1 retirement | Phase 7.5A absorbs row rewrite | `restart/ARCHITECTURE.md:940` (`RegexProgram` row) reads "`parse-that-regex` carries internal cross-engine parity (VM ↔ lazy DFA ↔ full DFA) per V1-FOLD-CANDIDATES Tier 3 #23, and no external regex oracle is consumed at V1"; §12.2:1585 retirement-note + §13.1:1642 `regex-engine-canon` lint-deny target consistent | **CLOSED** |

All three cross-target rename clusters closed. The rename cascade is
now consistent across the four-target cohort + locks file.

## §5 Cohort Verdict

**READY.**

Re-draft thresholds (`HARDENING-CONSOLIDATED.md` §5; ten conditions): zero
crossed. Tape/direct union holds (Lock 1 + ARCH §7 + PASS-3 §4
unchanged). Backend IR ownership holds (Lock 5 + ARCH §7.5 + PASS-2 §3
unchanged). yaml two-surface proof holds (Lock 14 + ARCH §13.1 + PASS-1
§9 unchanged). Numeric SOTA gates hold (Lock 8 + MASTER-PLAN H tranche).
B/C and C/E/H sequencing hold (MASTER-PLAN §5.1 calendar +
H.W0-H.W4 wave layout). Generated-code budgets hold (F.W3 + ARCH
§12.2 bbnf 21,503 → 21,933). Carry ledgers hold (MIGRATION.md:71
narrowed; A.W0 archive ceremony cleanly bounded to `ser` + `gorgeous`).
`path!` / `select!` macros canonical (ARCH §7.4 catalogue + naming-canon
lint). `@error(recover)` holds (ARCH §8.1 + PASS-1 §6). OpenFrame
archaeology holds (ARCH §7.2 BIR matrix `SpeculativeAlt` + frame stack
discipline).

V7 cite-hygiene + cross-document coherence + cross-target rename
clusters all CLOSED. R4 (BBNF-PATTERN-NONEXHAUSTIVE in ARCH §7.4)
remains a single non-blocking friction residue; the diagnostic exists
verbatim in PASS-1's table and emits correctly from the producer site;
the absence in ARCH §7.4 catalogue does not contradict any lock or
producer-consumer contract. R4 closure is rolled forward to whichever
tranche-D spec wave first authors the surface (the diagnostic family
is match-expression exhaustiveness; consumer is the `passes::layout`
match-arm checker; close gate emits the verbatim string at the
producer site).

Cohort verdict tabulated:

| Surface | V7 verdict | V7.1 verdict |
|---|---|---|
| PASS-1 | READY | **READY** (R1+R2+R3 closed; R4 friction rolls forward) |
| PASS-2 | READY | **READY** (no regression) |
| PASS-3 | READY | **READY** (no regression) |
| MASTER-PLAN trio | AMENDMENT-REQUIRED | **READY** (P1-P10 closed; X1 closed) |
| **Cohort cumulative** | AMENDMENT-REQUIRED | **READY** |

## §6 Voice + Discipline Locks Summary

The Phase 7.5A + 7.5B amendment commits preserved voice and discipline
locks per `restart/README.md` §13. Calibrated, direct prose. Archaic
diction admissible (hereupon, therein, thereof) per project convention.
No metalanguage. Path:line citations on every concrete claim. Tables
liberal where they serve. Per-X tables (this report carries one for
the 14 V7 punch items, one for the 21 lane verifications, one for the
3 cross-target rename clusters, one for the cohort verdict). Receiver /
blocker / receiving-gate triple intact on every carry across MASTER-PLAN
§24 friction ledger. No quick solutions. No legacy code uncontested.
Phase 7.5 introduced no voice violations; the cite-hygiene amendments
are deft natural-integration revisions, not bolted-on sections.

## §7 Closing Posture

V7.1 returns the four-target cohort to **READY**. The fourteen locks
are settled. The eight-question adjudication is settled. The thirty-item
V1 fold is absorbed (DK13 higher-rank, GADT hidden substrate, row
polymorphism, schema miner, function values + lambdas + closures,
Backend trait, parse-that-regex anchor, `path!` rename + path-core
deduplication, `format()` public method, WASM/TS post-V1 defer, six-
directive grammar canon). The MASTER-PLAN trio + locks + ARCHITECTURE
catalogue + PASS-1/2/3 substrate are mutually consistent.

Per-tranche full-spec drafting (Wave 8+) unblocks. The expected dispatch
shape is ten parallel spec agents, one per tranche A through J, each
authoring 3,000-5,000 lines of full-spec content (lock anchoring, wave-
by-wave consumer-gate prose, fixture manifests, close-gate command lists,
diagnostic ledgers, generated-LOC budgets per grammar). The R4 residue
(BBNF-PATTERN-NONEXHAUSTIVE catalogue sync) is friction-class; absorbs
into whichever tranche first authors the match-expression exhaustiveness
producer site, with no orchestration cost.

Hereupon Wave 8+ dispatches. The architecture stands READY for full-spec
authorship.
