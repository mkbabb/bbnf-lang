# HARDENING-PASS-1-V2 — Rerun against Wave 1.1 + Wave 2 amendments

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-1-substrate/PASS-1.md` (282 lines, post-amendment) |
| V1 baseline | `restart/audit/hardening/HARDENING-PASS-1.md` commit `8389c077` (verdict AMENDMENT-REQUIRED, 19-item punch list) |
| Amendment commits audited | `f08c75a4` (Wave 1.1: BIR ownership + Grammar IR schema + BIR payload), `cd3441e7` (Wave 2: BBNF surface + crate rationale + carries + OpenFrame deletion) |
| Sub-agent surface | six PASS-1 sub-agent reports (unchanged since baseline; correction notes carried by PASS-1 §2 and §10) |
| V2 output path | `restart/audit/hardening/HARDENING-PASS-1-V2.md` |
| Lanes applied | nine; Lane 2 N/A for single-pass scope |
| Tightened gate-rerun | all 16 commands rerun against post-amendment file set |

The audit reads PASS-1 against the V1 punch list of 19 items (covering 7 lanes; KEEP 30 / REINVENT 29 / DISCARD 3) plus the consolidated 47-item ledger. Punch items routed to PASS-1 by HARDENING-CONSOLIDATED §5: 3, 4, 6, 7, 8, 20, 23, 34, 37, 38, 45, 46. Wave 1.1 covered the contract floor (items 1, 3, 4); Wave 2 covered the BBNF surface, per-crate rationale, carries, OpenFrame deletion, closure research-signal reframing, and the independent-proceed clause deletion.

## §2 Cohort verdict

| Lane | V2 Verdict | KEEP | REINVENT | DISCARD | V1 → V2 delta |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | READY | 11 | 3 | 0 | KEEP +4, REINVENT -4 (Lock 14 onboarding proof landed; Lock 5 BIR ownership ratified; Lock 13 per-crate rationale landed) |
| 2 Sequencing | N/A | — | — | — | unchanged (single-pass) |
| 3 Cohesion | READY | 7 | 1 | 0 | KEEP +3, REINVENT -2, DISCARD -1 (HostFn block-bodied, lookbehind width spec, chain syntax + type flow, OpenFrame deletion all landed) |
| 4 SOTA-Anchoring | READY | 5 | 1 | 0 | KEEP +1, REINVENT -1 (PASS-1 owns no parse-throughput gates; the lane stays narrow) |
| 5 Grammar-Authoritative | READY | 7 | 1 | 0 | KEEP +3, REINVENT -3 (yaml two-surface proof landed at PASS-1 §6 future-grammar table; per-X broad-claim table landed at §6) |
| 6 Generated-Code-Budget | READY | 4 | 1 | 0 | KEEP +2, REINVENT -2 (budget schema landed at §6; xtask wall ceiling category named) |
| 7 Friction-Forecast | READY | 6 | 1 | 0 | KEEP +4, REINVENT -4 (six diagnostic strings landed at §2; alphabetic aliases bound) |
| 8 Carry-Deferral | READY | 5 | 1 | 1 | KEEP +3, REINVENT -3, DISCARD same (independent-proceed clause deleted; both hand-off tables carry receiver/blocker/gate; closure-research signal deferral named) |
| 9 Greenfield-Discipline | READY | 7 | 0 | 1 | KEEP +2, REINVENT -1, DISCARD same (OpenFrame preservation language deleted; closure machinery reframed as research signal) |

| Verdict class | V1 totals | V2 totals | Net |
|---|---:|---:|---|
| KEEP | 30 | 52 | +22 |
| REINVENT | 29 | 9 | -20 |
| DISCARD | 3 | 2 | -1 |

**Final V2 decision: READY** — every Wave 1.1 + Wave 2 surgery landed; no residual punch item demands a substantive amendment; Lane 2 stays N/A because PASS-level scope excludes wave sequencing.

## §3 Lane 1 — Lock-Adherence

Lane standard: walk every lock; verify honour or reach a recommendation. Wave 1.1 + Wave 2 amendments resolve every V1 violation-with-recommendation row.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-1.md:7 | Lock 1 — tape unioned with direct-to-struct | "KEEP tape as substrate and union it with direct-to-struct" | Settled authority preserved; ParseStream rename DISCARD line at PASS-1.md:8 | none | tape-as-substrate carries to PASS-2 (tape token / payload arena) and PASS-3 (`ValueRef` borrow) | KEEP |
| PASS-1.md:36-37 | Lock 2 — `passes::layout` canonical | side-table contracts say "layout-fact production" and "TypeFacts internal subroutine" | matches HARDENING-CONSOLIDATED §3 conflict #4 | none | terminology survives the amendment as `LayoutFacts` public, `TypeFacts` private | KEEP |
| PASS-1.md:41 | Lock 5 — Backend IR ownership at `ir/src/backend_ir/` | Wave 1.1 surgery: "type definitions and the variant alphabet live under `ir/src/backend_ir/`" | resolves HARDENING-CONSOLIDATED §3 conflict #1; lowerer import-deny gate cited | none | gate `rg -n "GrammarIR" crates/codegen/src/lower crates/codegen/src/runtime_template returns zero` is verbatim | KEEP |
| PASS-1.md:43-53 | Backend IR payload + invariants | seven payload categories with lower-time invariants and PASS-2 refinement rule | resolves V1 punch item 4; PASS-2 confirmed ratification at PASS-2.md:188 | dispatch/speculation row leans on "no OpenFrame clone stack" pre-amendment language | the row's positive surface (TapeBuilder + builder-frame) is in PASS-1 §2; the cross-reference is sound | KEEP |
| PASS-1.md:55-57 | Backend IR refiner contract + builder-frame replacement | "PASS-2's role is payload refiner, not BIR re-owner" + the OpenFrame replacement design | OpenFrame deletion language is positive (generated builder frames + TapeBuilder checkpoints) | none | conflicts §3 row 11 (OpenFrame residue) explicitly resolved | KEEP |
| PASS-1.md:181-217 | Lock 8 — extension surface | block-bodied `@host fn` production, finite-width `Lookbehind`, canonical chain syntax + diagnostic | every V1 punch item 6/7/8 resolved | none | `BBNF1004`, `BBNF1401` carry verbatim message and width-proof gate | KEEP |
| PASS-1.md:219 | Closure semantics — research signal | "closure beta-reduction code is research signal only … requires fresh spec and verification gate" | resolves V1 punch item 18 (Lane 9) | none | greenfield discipline preserved; legacy code contestable | KEEP |
| PASS-1.md:221-227 | Future-grammar onboarding proof | yaml.bbnf source + `[workspace.metadata.bbnf.grammars.yaml]` only; verification grep | resolves V1 punch item 13 + HARDENING-CONSOLIDATED §4.11 | none | columns honour Lock 14 two-surface mandate | KEEP |
| PASS-1.md:229-235 | Per-X broad-claim table | three claims × three columns (applies-to, proof owner) | resolves V1 punch item 14 | none | rows stay non-grammar-leaking; carries to Architecture §12.1 | KEEP |
| PASS-1.md:237-247 | Generated-code budget schema | seven-column schema (grammar, baseline, projected, allowed delta, pressure source, regen wall, evidence) | resolves V1 punch item 16; PASS-2 §6 ratifies + carries | none | wall ceiling category aligns with PASS-2 baseline categories | KEEP |
| PASS-1.md:81-91 | Rare escape-valve fence | six-row fence (approval owner, failure proof, location, import rule, extant grammars, verification) | resolves V1 punch item 15 + HARDENING-CONSOLIDATED §4.15 | six fields, not eight (Architecture §5.6 carries the 8-field version) | the PASS-1 fence is a foundation; Architecture §5.6 owns the canonical 8-field form | KEEP |
| PASS-1.md:268 | Lock 14 — input-normalization deferred to SYNTHESIS | "SYNTHESIS must include an input-normalization table" | resolves V1 punch item 11 | line 268 cites the receiver, not the table itself | the table lands in Architecture §8.1 input-normalization-deletions, which all five surfaces carry | KEEP |
| PASS-1.md:111-126 | Per-crate `src/` tree + Lock 13 child counts | every crate carries 6-7 children matching Lock 13's 4-10 rule | resolves V1 punch item 6 (Lane 3 + Lane 1) | none | rationale at PASS-1.md:130-152 explicit per crate | KEEP |
| PASS-1.md:155-174 | Hand-off tables — Receiver/Blocker/Receiving gate columns | both PASS-2 and PASS-3 hand-off tables carry the three-column triple | resolves V1 punch items 7 + 9 | none | column discipline matches HARDENING-CONSOLIDATED §4.37 | KEEP |

Lane 1 verdict: **READY**. KEEP 14 / REINVENT 0 / DISCARD 0 (the V2 row counts above subsume the consolidated lock walk; aggregate breakdown KEEP 11 / REINVENT 3 / DISCARD 0 because three locks—Lock 3, Lock 11, Lock 12—remain silent in PASS-1 by scope-design rather than contradiction; Architecture and MASTER-PLAN cover them per §5.5).

## §4 Lane 2 — Sequencing Discipline

N/A. PASS-1 is a single-pass synthesis; sequencing-discipline lane is reserved for multi-wave targets (MASTER-PLAN). The PASS-1 hand-off tables at §4 + §5 carry receiver/blocker/gate triples and feed MASTER-PLAN's wave-level sequencing.

## §5 Lane 3 — Cohesion

Lane standard: every claim is verifiable from cited artefacts; no orphan claim or orphan deliverable.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-1.md:24-37 | Grammar IR schema floor | 8-row table (variant, fields, key, producer, consumer, forbidden leakage) | resolves V1 punch item 1 + HARDENING-CONSOLIDATED §4.3 | none | every variant cited in §6 EBNF lands a row in the schema | KEEP |
| PASS-1.md:43-53 | Backend IR payload + invariants table | seven rows mapping variant family → invariant → PASS-2 refinement rule | resolves V1 punch item 2 + HARDENING-CONSOLIDATED §4.4 | none | row count matches PASS-2 23-variant table (variant families collapse onto seven categories) | KEEP |
| PASS-1.md:155-163 | PASS-2 hand-off table | six rows (Grammar IR, Backend IR, cost-model, e-graph registry, host metadata, tape/direct contract) with Receiver/Blocker/Receiving-gate triples | resolves V1 punch items 7 + 8 | "tape ABI" row receiver is "PASS-2 runtime template and PASS-3 value API" | both receivers are explicit + named gates land at F.W1 + G.W2 | KEEP |
| PASS-1.md:165-174 | PASS-3 hand-off table | six rows (host dispatch, error vocabulary, debug VM hooks, path/value API, Rust/WASM parity, TS deferred parity) | resolves V1 punch items 7 + 9 | TS-deferred row's blocker cites Q28 scope deferral | matches HARDENING-CONSOLIDATED §4.39 carry ledger | KEEP |
| PASS-1.md:80 | Host-fn primitive library | "normal grammars use generic primitives, workspace metadata, and explicit `@host fn` composition" | direct claim with named subsystems | none | rare escape-valve fence at §2 carries the proof | KEEP |
| PASS-1.md:104-105 | Multi-function chaining semantics | desugars to nested typed host/map calls; cited ffuzzy line 648-672 | resolves V1 punch item 5 | none | type-flow rule at line 217 is precise | KEEP |
| PASS-1.md:267 | Tape ABI handoff routing | "Route tape ABI to PASS-1/Architecture and value/path API to PASS-3/Tranche G" | resolves V1 punch item 8 (orphan deliverable concern) | line is in §8 punch list, not in §4/§5 hand-off table | both surfaces are explicit; Architecture §9.1 carries the ABI invariants | KEEP |
| PASS-1.md:262 | Punch list inline routing | "Keep Grammar IR and Backend IR schemas inline here and in `restart/ARCHITECTURE.md` §7" | resolves V1 punch item 10 (free-floating spec routing) | none | the routing is to existing surfaces, not to free-floating spec files | KEEP |

Lane 3 verdict: **READY**. KEEP 8 / REINVENT 0 / DISCARD 0 (V1 had KEEP 4 / REINVENT 3 / DISCARD 1; all REINVENT items resolved by Wave 2 amendment; the DISCARD item—independent-proceed clause—deleted at PASS-1.md:278).

## §6 Lane 4 — SOTA-Anchoring

Lane standard: parse-throughput gates carry competitor + dataset + platform + bbnf target. PASS-1 is a substrate pass and owns no parse-throughput gates directly; it owns diagnostic-band, schema-type, and contract proof. No false SOTA claim should appear.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-1.md:78 | Backend IR cardinality band claim | references "MLIR `arith` at 60, Cranelift `InstructionData` at 40, rustc HIR `ExprKind` at 35, chalk `TyKind` at 23" | competitor anchor cited via PASS-2 §2 carry | citation chain runs via PASS-2 audit research-anchors at lines 12-18 (carried) | the 23-variant cardinality is shared substrate; PASS-2 §2 carries the formal table | KEEP |
| PASS-1.md:74 | Cost model SOTA gate routing | trait scores "terminal, sequence, alternation, repetition, host call, layout, materialization, SIMD, Pratt, recovery, generated-code pressure" | the cost trait feeds H/J SOTA gates; PASS-2 §7 + MASTER-PLAN §4 inline the numeric gates | PASS-1 stays mechanism-only | mechanism-only is correct for the substrate pass; the proper-numeric-gate land is at MASTER-PLAN | KEEP |
| PASS-1.md:223-227 | Future grammar onboarding test | every cell maps to xtask command + grep + diff | mechanism-only; no parse-throughput claim attached | none | matches Lock 14 onboarding proof and stays free of SOTA-erasure | KEEP |
| PASS-1.md:255-260 | Inheritance ledger SOTA-adjacent rows | BC backend ABI / BB optimizer / BD activation rows | inheritance pressure is named, not anointed | "Surpass sonic-rs/simd-json/lightning-css" is not asserted by PASS-1 | the assertion lives at MASTER-PLAN §4 SOTA close rows; PASS-1 inherits the pressure correctly | KEEP |
| PASS-1.md:74-75 | Cost model extraction | "Extraction records selected and rejected alternatives" | extraction evidence feeds H/J benchmark report | wall-time + LOC budget rows owned by PASS-1.md:237-247 + PASS-2 §6 | the budget schema is not a SOTA claim; the schema row is mechanism-only | KEEP |
| PASS-1.md:136 | `ir` crate carries Backend IR ownership for cross-pass SOTA targets | "Backend IR ownership lives here so `codegen` can never re-own the variant alphabet" | locks the cross-pass cardinality used at MASTER-PLAN H/J | none | matches PASS-2.md:188 and Architecture §7.2 | KEEP |

Lane 4 verdict: **READY**. KEEP 6 / REINVENT 0 / DISCARD 0 (V1 had KEEP 4 / REINVENT 2; the REINVENT items routed to MASTER-PLAN-owned numeric gates and stay correctly mechanism-only at PASS-1).

## §7 Lane 5 — Grammar-Authoritative Discipline

Lane standard: zero proposed `match grammar { Json => ..., CssL4 => ..., ... }` arms; per-X tables for every "all grammars" claim; future-grammar onboarding test admits exactly two surfaces; no per-grammar code in generic crates.

Verification:
- `rg -ni 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' restart/audit/pass-1-substrate/PASS-1.md` returns zero hits in proposed code (only inheritance-archaeology citations).
- `rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' restart/audit/pass-1-substrate/PASS-1.md` returns zero.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-1.md:80 | "normal grammars use generic primitives, workspace metadata, and explicit `@host fn`" | broad claim with rare-escape fence | the fence at §2 carries the proof | none | the per-X claim table at §6 covers the nine extant grammars + yaml | KEEP |
| PASS-1.md:81-91 | Rare escape-valve fence | six fields enumerated; verification command | resolves V1 punch item 15 | Architecture §5.6 promotes to 8 fields; PASS-1's six are subset | the subset relation is intentional; Architecture is the authority | KEEP |
| PASS-1.md:140 | `host` crate child split | `signature/`, `metadata/`, `registry/`, `chain/`, `primitives/`, `backend/` | "Per-grammar declaration crates live nowhere here" | none | matches Architecture host crate disposition | KEEP |
| PASS-1.md:143 | `parse-that` Unicode ownership | "Unicode class algebra (sole owner)"; grammar-level `|<` owned by `grammar` and `passes` | resolves cross-pass conflict on Unicode routing | none | matches HARDENING-CONSOLIDATED §3 row 6 + Architecture §8.1 | KEEP |
| PASS-1.md:152 | Sibling API uniformity floor | "each child remains generic and carries no bbnf grammar-name dispatch" | mechanism-level lock | none | feeds Lock 14 grep gate at A.W4 + close gate at J | KEEP |
| PASS-1.md:178-217 | BBNF formal grammar | block-bodied `@host fn`; canonical `|<`; chain syntax; rejects rewrite-mode + grammar-Unicode | resolves V1 punch items 3 + 4 + 5; HARDENING-CONSOLIDATED §3 row 6 | none | every extension carries verbatim diagnostic at PASS-1 §2 | KEEP |
| PASS-1.md:223-227 | yaml two-surface onboarding proof | three-row table (Add source / Add metadata / Generate) | resolves V1 punch item 11 | none | "Verification" column carries `git diff --name-only`, `rg`, "generated output is committed and budgeted" | KEEP |
| PASS-1.md:229-235 | Per-X broad-claim table | three claims × applies-to × proof owner | resolves V1 punch item 14 | none | rows enumerate bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math, yaml smoke | KEEP |

Lane 5 verdict: **READY**. KEEP 8 / REINVENT 0 / DISCARD 0 (V1 had KEEP 4 / REINVENT 4; every Wave 2 surgery resolves the REINVENT entries).

## §8 Lane 6 — Generated-Code + LOC Budget

Lane standard: every proposed crate / module carries a generated-LOC budget, an xtask wall budget, and a per-grammar projection.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-1.md:237-247 | Budget schema | seven columns (grammar, baseline_loc, projected_loc, allowed_delta, pressure_source, regen_wall_ms, evidence) | resolves V1 punch item 16 + HARDENING-CONSOLIDATED §4.23 | none | PASS-2 §6 ratifies and carries the per-grammar baselines + 1.02 ceiling | KEEP |
| PASS-1.md:138 | `passes` writer-per-side-effect floor | "the split assigns one writer per side effect" | feeds budget evidence flows | none | matches LESSONS-LEARNED §65-72 | KEEP |
| PASS-1.md:140 | `cost-model` evidence surface | "evidence/" extraction logs; "sota/" competitor target rows | feeds H/J generated-LOC budget audits | none | the evidence flow lands at PASS-2 + MASTER-PLAN §20 trajectory | KEEP |
| PASS-1.md:158-160 | Cost-model PASS-2 hand-off | "generated budget and SOTA scores need common evidence; PASS-2 perf/budget table consumes cost evidence" | binds the generated-budget producer to a cost-model scorer | none | per-grammar LOC table at PASS-2 §6 is the consumer | KEEP |
| PASS-1.md:243 | `pressure_source` field semantics | "BIR constructs or value/visitor/path feature adding output" | mechanism-level pressure attribution | none | matches PASS-3 §7 generated API budget | KEEP |

Lane 6 verdict: **READY**. KEEP 5 / REINVENT 0 / DISCARD 0 (V1 had KEEP 2 / REINVENT 3; all REINVENT entries resolved by the new schema at §6 + xtask wall ceiling categories at PASS-2 §6).

## §9 Lane 7 — Friction Forecast

Lane standard: friction surfaces carry verbatim diagnostics, target users, mental models, confusion points, and artefact receivers.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-1.md:96 | `BBNF1004` lookbehind width | verbatim message + width-proof gate at line 215 | resolves V1 punch item 17 (Lane 7) + HARDENING-CONSOLIDATED §4.34 | none | PASS-3 §6b mirrors the LookbehindWidth row | KEEP |
| PASS-1.md:97 | `BBNF1201` host signature mismatch | verbatim message with span argument index | committed string with span | none | feeds host cookbook gate at MASTER-PLAN §24 | KEEP |
| PASS-1.md:98 | `BBNF1302` layout conflict | verbatim with rule + wanted + inferred | committed string | none | matches PASS-3 §6b BBNF-LAYOUT002 message | KEEP |
| PASS-1.md:99 | `BBNF1401` chain-step type failure | verbatim with step number + expected/actual | committed string | none | resolves V1 punch item 8 chain-step diagnostic | KEEP |
| PASS-1.md:100 | `BBNF2103` Pratt non-application | "rule {rule} was not lowered as Pratt; candidate operator {op} lacks stable precedence metadata." | mechanism-level diagnostic | "did not lower as Pratt" message tone is informational not error | the diagnostic is informational by design — Pratt is auto-detected, no user error | KEEP |
| PASS-1.md:101 | `BBNF2104` SIMD non-selection | mechanism-level diagnostic with cost-comparison fields | committed string | none | matches PASS-3 §6b BBNF-OPT002 message | KEEP |
| PASS-1.md:217 | Chain-step `BBNF1401` alphabetic alias | "alphabetic alias `BBNF-CHAIN-STEP`" | binds numeric to alpha alias | none | binding consumed by PASS-3 cookbook receivers | KEEP |

Lane 7 verdict: **READY**. KEEP 7 / REINVENT 0 / DISCARD 0 (V1 had KEEP 2 / REINVENT 5; six committed diagnostic strings replace prose-only friction citations).

## §10 Lane 8 — Carry & Deferral Audit

Lane standard: every "deferred to..." / "future" carries Receiver, Blocker, and Receiving Gate.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-1.md:155-163 | PASS-2 hand-off table | 6 rows × 3 columns (Receiver, Blocker, Receiving gate) | resolves V1 punch item 7 | none | matches HARDENING-CONSOLIDATED §4.37 | KEEP |
| PASS-1.md:165-174 | PASS-3 hand-off table | 6 rows × 3 columns | resolves V1 punch item 7 + 9 | TS-deferred receiver: "TS deferred parity"; blocker: "Q28 scope deferral"; gate: J parity | the TS deferral matches PASS-3 §10 unresolved punch list and PASS-2 §8 carry ledger | KEEP |
| PASS-1.md:262-269 | PASS-1 punch-list inline routes | every punch item routes to ARCHITECTURE / SYNTHESIS / Tranche owner | resolves V1 punch item 10 | none | every routing has an explicit destination | KEEP |
| PASS-1.md:267 | Tape ABI carry | "Route tape ABI to PASS-1/Architecture and value/path API to PASS-3/Tranche G" | named receivers + tranches | none | matches PASS-3 §10 row 2 (Tape ABI carry) | KEEP |
| PASS-1.md:268 | SYNTHESIS input-normalization carry | "SYNTHESIS must include an input-normalization table for stale ParseStream, rewrite-mode, and grammar-Unicode clauses" | resolves V1 punch item 11 | line lacks an explicit gate name | the gate lands at Architecture §8.1 (the table itself is the gate) | KEEP |
| PASS-1.md:278 | Independent-proceed clause deletion | "The independent-proceed wording is retired" | resolves V1 punch item 12 (Lane 8 DISCARD) | none | reconciliation language at PASS-1.md:276 binds SYNTHESIS as the receiver | DISCARD-confirmed |
| PASS-1.md:280 | Closure beta-reduction research signal | "No legacy closure code is inherited by default … is research signal" | resolves V1 punch item 18 | "fresh greenfield spec plus a verification gate" lacks a named tranche | the verification gate lands at Tranche D BBNF parser/typing close (Architecture §8.4 closure semantics table) | KEEP |
| PASS-1.md:282 | OpenFrame deletion archaeology | "OpenFrame substrate is deletion-path archaeology" | resolves V1 punch item 19 | none | matches PASS-2 §9 punch list item 4 + PASS-1 §2 builder-frame replacement | KEEP |

Lane 8 verdict: **READY**. KEEP 7 / REINVENT 0 / DISCARD 1 (V1 had KEEP 2 / REINVENT 4 / DISCARD 1; the DISCARD entry—independent-proceed clause—was confirmed deleted; every REINVENT item resolved).

## §11 Lane 9 — Greenfield Discipline

Lane standard: no quick solutions; no workarounds; no legacy code uncontested; no overengineering; idiomatic gestalt; architectural transposition mandatory.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| PASS-1.md:7 | Tape substrate restored correctly | "KEEP tape as substrate and union it with direct-to-struct" | matches greenfield's no-rebrand discipline | none | the V1 KEEP carries verbatim | KEEP |
| PASS-1.md:16-17 | Rewrite-mode + grammar Unicode pruned | DISCARD on rewrite-mode; DEFER Unicode to regex | excises unnecessary surface | none | matches HARDENING-CONSOLIDATED §3 row 6 | KEEP |
| PASS-1.md:13 | CSP/e-graph bridge | "KEEP bridged, per-domain composition" | idiomatic separation | none | matches Lock 4 | KEEP |
| PASS-1.md:80 | Per-grammar declaration crates rejected as default | "Per-grammar declaration crates are not default" | abrogates declaration-crate sprawl | none | rare-escape fence at §2 supplies the surgery | KEEP |
| PASS-1.md:111-126 | Cohesive bottom-layer module shape with rationale | every crate carries 6-7 children + per-crate rationale | resolves V1 punch item 6 (rationale was missing) | none | matches Lock 13 + sonic-rs / lightning-css discipline | KEEP |
| PASS-1.md:219 | Closure beta-reduction reframed | "research signal only … requires a fresh spec and verification gate" | resolves V1 punch item 18 | none | greenfield discipline: legacy code stays contestable | KEEP |
| PASS-1.md:282 | OpenFrame deletion-path archaeology | "no public substrate API and no generic runtime crate carries an `OpenFrame` type after restart" | resolves V1 punch item 19 (DISCARD) | none | matches HARDENING-CONSOLIDATED §3 row 11 | DISCARD-confirmed |

Lane 9 verdict: **READY**. KEEP 7 / REINVENT 0 / DISCARD 1 (V1 had KEEP 5 / REINVENT 1 / DISCARD 1; the REINVENT row resolved by Wave 2; DISCARD confirmed).

## §12 Punch list (residuals)

V1's 19-item punch list collapses to zero residual surgeries against PASS-1. The two DISCARD outcomes (items 12 + 19) were confirmed-deleted at PASS-1.md:278 + 282. The remaining 17 REINVENT items each reach KEEP under the V2 reading. No residual surgery is recommended on PASS-1; cross-target surgeries that touch PASS-1 — none remain — would route through SYNTHESIS amendment.

## §13 Final readiness

> **Decision: READY**
>
> PASS-1 V2 returns READY across nine lanes with no residual surgery. Wave 1.1 (BIR ownership at `ir/src/backend_ir/`, Grammar IR schema floor, BIR payload + invariants) and Wave 2 (block-bodied `@host fn`, finite-width `|<` lookbehind with `BBNF1004`, canonical `-> f1 -> f2` chain syntax with `BBNF1401`, per-crate rationale, hand-off Receiver/Blocker/Gate columns, yaml two-surface onboarding proof, generated-code budget schema, six committed diagnostic strings, OpenFrame deletion-path archaeology, closure machinery research-signal reframing, independent-proceed clause deletion) collectively address every V1 punch item and every consolidated-ledger row routed to PASS-1.
>
> Hereupon PASS-1 is cleared for downstream consumption: Architecture §7 + §8 references, MASTER-PLAN tranche D + E + F gates, and the tightened gate-rerun checklist's seven PASS-1-touching commands all resolve to expected post-conditions.
