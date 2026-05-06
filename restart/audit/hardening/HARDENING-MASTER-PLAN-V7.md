# HARDENING-MASTER-PLAN-V7

## §1 Target Identification

V7 audit target: the post-Phase-7 MASTER-PLAN trio.

| Target | Path | HEAD line count | Phase landed |
|---|---|---:|---|
| ARCHITECTURE | `restart/ARCHITECTURE.md` | 1,675 | Phase 7.1 (`9cb92284`) |
| MASTER-PLAN | `restart/MASTER-PLAN.md` | 848 | Phase 7.2 (`c8fb1506`) |
| MIGRATION | `restart/MIGRATION.md` | 816 | Phase 7.2 (`c8fb1506`) |

Audit posture:

- Worker scope is audit only. The V7 worker reads the trio and writes one report path: `restart/audit/hardening/HARDENING-MASTER-PLAN-V7.md`.
- V6 baseline of record is `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md` (READY across PASS-1, PASS-2, PASS-3, SYNTHESIS) plus `restart/audit/hardening/HARDENING-MASTER-PLAN-V5.md` (the most recent target-specific report; V5 was AMENDMENT-REQUIRED, V5.1 closed it). No `HARDENING-MASTER-PLAN-V6.md` exists by that exact name; V6 closure for the trio rolls into `HARDENING-SYNTHESIS-V6.md` (commit `4fe06344`) and the consolidated V6 ledger.
- Phase 7.1 (`9cb92284`) landed seven lock amendments, ARCH §7.5 `Backend` trait, ARCH §8.1 six-directive grammar, ARCH §8.2 type-system §8 amendment, ARCH §10.1 rewrite-budget, ARCH §13.1 lint manifest, and the §5 declaration-crate review form template.
- Phase 7.2 (`c8fb1506`, with sister Phase-7.2 commits `c45d74ec` PASS-1, `3dc95460` PASS-2, `d9414a2f` PASS-3) cascaded into MASTER-PLAN + MIGRATION: `pointer!` → `path!` rename, `parse-that-regex` rename, TS/WASM defer post-V1, D wave grew 5 → 6, H wave drops 6 → 5, cookbook adds `format()` row.

Mandatory read set completed:

- `restart/ARCHITECTURE.md`
- `restart/MASTER-PLAN.md`
- `restart/MIGRATION.md`
- `restart/locks/14-LOCKS.md`
- `restart/audit/hardening/HARDENING-MASTER-PLAN-V5.md` (V5 carry baseline)
- `restart/audit/hardening/HARDENING-CONSOLIDATED-V6.md` (V6 baseline)
- `restart/research/V1-FOLD-CANDIDATES.md`
- `restart/research/PHASE-7.2-SYNTHESIS-CLASSIFICATION.md`
- `restart/prompts/HARDENING.md`

Verdict:

- **AMENDMENT-REQUIRED**

Reason for non-READY verdict:

- The fold absorbed the bulk of V1 fold candidates without ratifying every cross-reference it cited.
- Two cross-document anchors are phantoms: MASTER-PLAN cites `restart/ARCHITECTURE.md` §13 appendix (twice) for the declaration-crate review form template and the cookbook page contract template; the templates exist in ARCH §5, not in §13.
- One lock amendment carries a dead wave reference: Lock 8 (line 48) cites SOTA close gates at `H.W3, H.W4, and H.W5`; H.W5 does not exist. The H wave count dropped 6 → 5 in Phase 7.2 (the prior H.W3 WASM wave deferred), so Lock 8's amendment text fossilised the pre-fold wave numbering.
- One lock amendment violates Lock 12: MIGRATION.md:71 says `crates/bbnf-path-ts` archives at A.W0 alongside `ser`/`gorgeous`. Lock 12 (the canonical statement at line 56) names only `ser` and `gorgeous` for A.W0 archive; routing `bbnf-path-ts` into the same ceremony broadens Lock 12's contract without amendment.
- One C-wave cross-reference is fabricated: MASTER-PLAN C.W4 says "rewrite budget policy (consumed from `restart/ARCHITECTURE.md` §10.1 — categories, node/iteration ceilings, fail-closed posture, representative-stability protocol now landed at architecture level per Phase 7.1)". ARCH §10.1 carries categories and thresholds; it carries no "fail-closed posture" wording and no "representative-stability protocol" wording. The cite over-promises what §10.1 contains.
- One non-terminal naming drift survives: Lock 10 amendment text spells the production `Directive = ImportDecl | HostFn | ErrorDecl | LayoutDecl | PrettyDecl | TokenDecl ;`. ARCH §8.1 spells the same production `Item ::= ImportDecl | HostFn | RuleDecl | LayoutDecl | ErrorDecl | PrettyDecl | TokenDecl`. Lock 10's prose says "six-directive" but ARCH's `Item` is a seven-alternative production (RuleDecl is a non-directive item).
- BBNF-POINTER-* diagnostic codes survive in active surface even though Phase 7.2 §A renamed them. ARCH §7.4:1044-1046 still owns the canonical codes `BBNF-POINTER-UNKNOWN-SEGMENT`, `BBNF-POINTER-GRAMMAR-MISMATCH`, `BBNF-POINTER003`. ARCH §12.2:1579 keeps the `BBNF-POINTER-*` codes "until §7.4 catalogue renames"; MASTER-PLAN §25:802 already uses `BBNF-PATH-UNKNOWN-SEGMENT` / `BBNF-PATH-GRAMMAR-MISMATCH`. The ARCH catalogue retired by name in MASTER-PLAN but lives on in ARCH.

Redraft threshold:

- Not crossed.
- Tranche topology, lock ownership, migration routing, and close gates remain coherent.
- The Phase 7 fold absorbed thirty V1 candidates with intact greenfield architecture.
- The faults are surgical — phantom anchors, one wave-count fossil, one Lock 12 broadening, and one diagnostic-code cataloguing tail — not architectural reversals.

## §2 Carry-Aware Lens Table A-E

| ID | Lens | Verified anchors | Finding | Carry disposition |
|---|---|---|---|---|
| A1 | Inter-document narrative coherence | `restart/ARCHITECTURE.md:1067-1144`; `restart/MASTER-PLAN.md:60`, `restart/MASTER-PLAN.md:80`, `restart/MASTER-PLAN.md:174`; `restart/MIGRATION.md:71`, `restart/MIGRATION.md:660` | The Backend trait at ARCH §7.5 is consistently cited by MASTER-PLAN and MIGRATION as the V2 expansion contract. WasmBackend / TsBackend deferral is uniform. | KEEP |
| A2 | Inter-document narrative coherence | `restart/ARCHITECTURE.md:1146-1235`; `restart/MASTER-PLAN.md:170`; `restart/MASTER-PLAN.md:356` | The six-directive grammar amendment lands; ARCH §8.1 sketch agrees with PASS-1 §6 on block-bodied `@host fn`, `MapTail ::= "->" ChainExpr`, and `LambdaExpr` for V1. | KEEP |
| A3 | Inter-document narrative coherence | `restart/ARCHITECTURE.md:1273-1308`; `restart/MASTER-PLAN.md:318` | DK13 / Damas-Milner / Pierce-Turner composition prose is settled and load-bearing across both surfaces. C.W1 cites DK13 algorithmic completeness; ARCH §8.2 records the citation chain. | KEEP |
| A4 | Inter-document narrative coherence | `restart/ARCHITECTURE.md:1425-1448`; `restart/MASTER-PLAN.md:321` | C.W4 cite "fail-closed posture, representative-stability protocol now landed at architecture level per Phase 7.1" but §10.1 contains neither phrase. The §10.1 categories carry "abort on cycle detection" in the legality row; "fail-closed" is implied but not labelled, and "representative-stability" was a V6 R5 closure item that did not land in §10.1 text. | AMEND |
| A5 | Inter-document narrative coherence | `restart/ARCHITECTURE.md:739-770`; `restart/MASTER-PLAN.md:771`; `restart/MASTER-PLAN.md:797` | MASTER-PLAN cites "ARCH §13 appendix" for the declaration-crate review form template and for the cookbook page contract template. ARCH has no §13 appendix. The eight-field declaration-crate review form lives in §5 (after the canonical schema rules table). The cookbook page contract template does not exist as a section anywhere. | AMEND |
| B1 | Vocabulary drift | `restart/locks/14-LOCKS.md:52`; `restart/ARCHITECTURE.md:1158` | Lock 10 amendment names the production `Directive`; ARCH §8.1 names the production `Item` (which contains six directives plus `RuleDecl`). The two names refer to different productions: Lock 10's `Directive` is the directive set; ARCH's `Item` is the top-level grammar element including non-directive `RuleDecl`. The cross-reference is conflated in lock prose ("the V1 BBNF grammar formalises six directives: `Directive = ...`") and forces readers to reconcile the names. | AMEND |
| B2 | Vocabulary drift | `restart/ARCHITECTURE.md:1044-1046`; `restart/ARCHITECTURE.md:1579`; `restart/MASTER-PLAN.md:802` | `BBNF-POINTER-*` diagnostic codes survive as canonical at ARCH §7.4:1044-1046 (with `BBNF-POINTER-UNKNOWN-SEGMENT` / `BBNF-POINTER-GRAMMAR-MISMATCH` / `BBNF-POINTER003`). ARCH §12.2:1579 keeps them with a "legacy code names retained until §7.4 catalogue renames" deferral. MASTER-PLAN §25 row 1 already uses `BBNF-PATH-UNKNOWN-SEGMENT` / `BBNF-PATH-GRAMMAR-MISMATCH`. The two surfaces disagree on whether the rename has landed. | AMEND |
| B3 | Vocabulary drift | `restart/ARCHITECTURE.md:935`; `restart/ARCHITECTURE.md:1198`; `restart/ARCHITECTURE.md:1580`; `restart/ARCHITECTURE.md:1637` | `regex-automata` survives in three roles: (i) §7.2:935 says "Unicode stays below BBNF; `regex-automata` remains the oracle lane until parity is proven" (a positive surface); (ii) §12.2:1580 says oracle role "retires per V1-FOLD-CANDIDATES Tier 3 #23" (a retirement); (iii) §13.1:1637 lints `regex-automata` imports as `BBNF-REGEX-ENGINE-DRIFT`. Only (ii) and (iii) are consistent with Phase 7.1. (i) at line 935 is stale. | AMEND |
| B4 | Vocabulary drift | `restart/MIGRATION.md:71`; `restart/locks/14-LOCKS.md:56` | MIGRATION says `bbnf-path-ts` "archives at A.W0 alongside `ser`/`gorgeous` and is reconstituted as `path-ts` in V2". Lock 12 (line 56) names only `ser` and `gorgeous` for the A.W0 archive ceremony. Co-archiving `bbnf-path-ts` is a Lock 12 broadening; the amendment surface is not present in Lock 12. | AMEND |
| B5 | Vocabulary drift | `restart/locks/14-LOCKS.md:48`; `restart/MASTER-PLAN.md:174` | Lock 8 amendment cites SOTA close gates at "H.W3, H.W4, and H.W5". MASTER-PLAN H tranche has H.W0-H.W4 (five waves; H.W5 absent). Phase 7.2 dropped H wave count from 6 to 5; Lock 8's amendment text fossilised the pre-fold numbering. | AMEND |
| C1 | Worked-example scarcity | `restart/MASTER-PLAN.md:213-229` | The yaml A→F→J trajectory walks every tranche with two-surface proof; the trajectory now binds Phase 7.2 receivers (G `path!`, H WASM defer, I `DocumentSnapshot`/`ReparsePlan`) cleanly. | KEEP |
| C2 | Worked-example scarcity | `restart/MASTER-PLAN.md:802-810` | Eight-row cookbook table now anchors `path!`/`select!`, lifetime constructors, visitor mutation, layout, recognizers, crate-split migration, yaml onboarding, yaml syntax error, and the new `format()` row. The friction surface is materially closed. | KEEP |
| C3 | Worked-example scarcity | `restart/MASTER-PLAN.md:524`; `restart/audit/pass-3-runtime/PASS-3.md:181-190` | I.W1 binds incremental thresholds via `DocumentSnapshot`, snapshot-scoped `TapeId`, reuse maps, query invalidation, and reparse plans. The PASS-3 reuse percentages are not numerically inlined into MASTER-PLAN, but I.W1 names the whole fallback ledger, closing the V5 P11. | KEEP |
| C4 | Worked-example scarcity | `restart/MASTER-PLAN.md:355`; `restart/MASTER-PLAN.md:805` | `@error(recover = ...)` is gated at D.W4 and surfaces in the layout-cookbook row; the recovery cookbook row at §25:809 covers yaml syntax error with `BBNF-RECOVERY001`. | KEEP |
| C5 | Worked-example scarcity | `restart/MASTER-PLAN.md:213-229`; `restart/ARCHITECTURE.md:1546-1591` | The yaml A→F→J trajectory + the §12.2 per-grammar matrix together provide the single grammar trajectory through skeleton → BIR → publication readiness. V5 P14 is closed by yaml. | KEEP |
| D1 | Coverage gap | `restart/MASTER-PLAN.md:213-229`; `restart/ARCHITECTURE.md:1499-1544` | Two-surface yaml onboarding has authoring acceptance (commands, allowed/forbidden changes, A.W4 metadata gate, F.W5 generated runtime); V5 D1 closes. | KEEP |
| D2 | Coverage gap | `restart/MASTER-PLAN.md:524`; `restart/audit/pass-3-runtime/PASS-3.md:181-190` | Incremental fallback policy bound to I.W1 named ledger; PASS-3 owns numeric thresholds. | KEEP |
| D3 | Coverage gap | `restart/ARCHITECTURE.md:1015-1065` | Diagnostic vocabulary catalogue at §7.4 names producer sites for every code; runtime hooks land in B/I waves. The V5 D3 hook-schema gap closes via §7.4 producer table. | KEEP |
| D4 | Coverage gap | `restart/ARCHITECTURE.md:1051`; `restart/MASTER-PLAN.md:355` | Generic-rule typing × `@error(recover = ...)` intersection is gated by `BBNF-GENERIC-CYCLE` (§7.4:1051) plus D.W4 rewrite-rejection fixture; intersection coverage is structurally closed. | KEEP |
| D5 | Coverage gap | `restart/MASTER-PLAN.md:788`; `restart/ARCHITECTURE.md:1419` | WASM ABI matrix defers post-V1 alongside `WasmBackend: Backend`; the V5 D5 gap closes by deferral, not by V1 ABI matrix. | KEEP |
| D6 | Coverage gap | `restart/MASTER-PLAN.md:557`; `restart/locks/14-LOCKS.md:54` | Lock 11 publication gate at J.W3 splits stable surface (publishes unconditionally) from incubation-cleared sister crates (publish only after 2-tranche stability gate). The MASTER-PLAN J.W3 row is now full prose; V5 D6 closes. | KEEP |
| E1 | Cumulative lock tension | `restart/locks/14-LOCKS.md:34`; `restart/ARCHITECTURE.md:1351-1404`; `restart/MASTER-PLAN.md:282-294` | Lock 1 substrate honoured: tape + direct union, `(TapeId, node id, payload class)` identity smoke at ARCH §9.1, B-tranche substrate. | KEEP |
| E2 | Cumulative lock tension | `restart/locks/14-LOCKS.md:42`; `restart/ARCHITECTURE.md:1067-1144`; `restart/MASTER-PLAN.md:475` | Lock 5 amendment lands the `Backend` trait at ARCH §7.5 and the trait carries `RustBackend` V1 / `WasmBackend` V2 / `TsBackend` V2 surface. | KEEP |
| E3 | Cumulative lock tension | `restart/locks/14-LOCKS.md:48`; `restart/MASTER-PLAN.md:131-141` | Lock 8 SOTA gates carry M1 Pro Rust-line targets; competitor anchors agree with `restart/corpora/SOTA.md:50-89`. | KEEP-WITH-AMENDMENT (H.W5 wave fossil per B5) |
| E4 | Cumulative lock tension | `restart/locks/14-LOCKS.md:54`; `restart/MASTER-PLAN.md:557` | Lock 11 amendment landed both the parse-that-regex rename and the J.W3 stable-surface vs incubation-cleared split. | KEEP |
| E5 | Cumulative lock tension | `restart/locks/14-LOCKS.md:56`; `restart/MIGRATION.md:71` | Lock 12 amendment names `pre-restart-2026-05-04` tag and BA-/BD- slot-drift retirement. MIGRATION at line 71 broadens A.W0 archive to include `bbnf-path-ts`; this exceeds Lock 12's two-crate scope. | AMEND |
| E6 | Cumulative lock tension | `restart/locks/14-LOCKS.md:60`; `restart/ARCHITECTURE.md:1499-1544`; `restart/MASTER-PLAN.md:213-229` | Lock 14 onboarding two-surface proof preserved across yaml trajectory and the §12.1 walkthrough. | KEEP |

Lens A-E summary:

- The Phase 7 fold preserved the V6 broad coherence on substrate, BIR ownership, type system, lock ownership, migration routing, gate structure, and SOTA discipline.
- The faults are concentrated on cross-reference fidelity and one-wave-count fossil, not on architectural reversal.
- A4, A5, B1-B5, and E5 form the surgical amendment set.

## §3 LLM-Pathology Table F-H

| ID | Lens | Verified anchors | Pathology check | Finding |
|---|---|---|---|---|
| F1 | LLM bias | `restart/ARCHITECTURE.md:1273-1308` | Over-citation pile-up in §8.2 type system text | Damas-Milner 1982, Pierce 2002 ch.22, Pierce-Turner, Dunfield-Krishnaswami 2013 — all four are load-bearing. Damas-Milner names Algorithm-W, Pierce 2002 names HM equality, Pierce-Turner names bidirectional check/synth, DK13 names higher-rank algorithmic completeness. No DK19 or Pottier-Rémy citation appears. The four citations are functionally distinct, not stuffed. |
| F2 | LLM bias | `restart/MASTER-PLAN.md:487-488`; `restart/MASTER-PLAN.md:131-136` | Pseudo-precision in SOTA thresholds | Thresholds are sourced (`restart/corpora/SOTA.md:50-89`). Early H.W3/H.W4 thresholds are looser than J.W1 finals; relative looseness is justified by cost-evidence margin. No pseudo-precision detected. |
| F3 | LLM bias | `restart/MASTER-PLAN.md:321` | Confident prose hiding cross-reference contradiction | Present at C.W4: "rewrite budget policy (consumed from `restart/ARCHITECTURE.md` §10.1 — categories, node/iteration ceilings, fail-closed posture, representative-stability protocol now landed at architecture level per Phase 7.1)". §10.1 carries categories and node/iteration thresholds; "fail-closed posture" and "representative-stability protocol" do not appear as named protocols at §10.1. The cite over-promises §10.1. |
| F4 | LLM bias | `restart/MASTER-PLAN.md:771`; `restart/MASTER-PLAN.md:797` | Phantom anchor | Present. Two MASTER-PLAN rows cite "ARCH §13 appendix" for templates that live in ARCH §5 (declaration-crate review form, eight-field) and that are absent (cookbook page contract template). The first cite is an anchor mis-pointer; the second cite is a phantom artefact. |
| F5 | LLM bias | `restart/locks/14-LOCKS.md:48` | Wave-count fossil after fold | Present. Lock 8 amendment text says "H.W3, H.W4, and H.W5" but Phase 7.2 dropped H from six waves to five (H.W5 retired with WASM defer). Lock prose lags the wave-count cascade. |
| G1 | Overfitting | `restart/ARCHITECTURE.md:1546-1591` | Per-grammar matrix could freeze the seed grammar set | The matrix lists ten rows (nine seed grammars plus yaml probe). The yaml row is explicitly "onboarding probe" with declaration-crate forbidden at onboarding. The matrix is producer-side schema, not dispatch logic; column semantics live in the table immediately below. No overfitting. |
| G2 | Overfitting | `restart/MASTER-PLAN.md:170`; `restart/MASTER-PLAN.md:356` | D wave growth could be pseudo-precision | D wave count grew 5 → 6 to absorb function-value lowering at D.W5. The wave content is named (function values + lambda + closure capture by `&'i` + match + tuple + function-typed `@host fn` parameters + closure environment frame). The growth is justified work, not pseudo-growth. |
| G3 | Overfitting | `restart/MASTER-PLAN.md:557` | Sister-crate publication split could overfit a specific roadmap | The Lock 11 J.W3 split (stable surface vs incubation-cleared sister crates after 2-tranche stability gate) is generic over crate identity; any sister crate that fails the stability gate carries dry-run results and remains a path-dep until the next J cycle. The mechanism is principled. |
| G4 | Overfitting | `restart/MASTER-PLAN.md:131-136`; `restart/ARCHITECTURE.md:1483-1488` | SOTA rows could bias only JSON/CSS hot fixtures | The six SOTA rows cover JSON twitter/citm/canada, CSS bootstrap/animate, and SIMD structural-scan; yaml is explicitly excluded from seed-grammar SOTA budget. The fixture set is intentionally hot-path; the grammar trajectory at §5.3 carries yaml through the same gates. No overfitting. |
| H1 | Hallucination/provenance | `restart/MASTER-PLAN.md:557`; `restart/MIGRATION.md:595`; `restart/ARCHITECTURE.md:1589` | Lock 11 line citation | All three sites cite `restart/locks/14-LOCKS.md:54` — Lock 11 is at line 54. Citation correct. |
| H2 | Hallucination/provenance | `restart/MASTER-PLAN.md:121`; `restart/MASTER-PLAN.md:122` | Lock 8 / Lock 13 citations | MASTER-PLAN line 121 cites `restart/locks/14-LOCKS.md:58` for Lock 13. Lock 13 at line 58 — correct. MASTER-PLAN line 122 cites `restart/locks/14-LOCKS.md:48` for Lock 8. Lock 8 at line 48 — correct. The V5 wrong-line citations have closed. |
| H3 | Hallucination/provenance | `restart/MASTER-PLAN.md:133`; `restart/corpora/SOTA.md:56` | json/canada simd-json comparator | MASTER-PLAN row carries both sonic-rs and simd-json with SOTA citation; ARCH §11:1485 mirrors the pair. The V5 H3 provenance asymmetry has closed. |
| H4 | Hallucination/provenance | `restart/MASTER-PLAN.md:802`; `restart/audit/pass-3-runtime/PASS-3.md:359` | Path DSL canonical macro spelling | MASTER-PLAN row 1 in §25 names canonical Rust spelling `path!(Json => "/...")` and bracket form `path!(Json, ["a", "b", 0])`; PASS-3 owns diagnostic surface. The grammar-prefix rule is explicit. The V5 H4 ambiguity has closed. |
| H5 | Hallucination/provenance | `restart/MASTER-PLAN.md:769-791`; `restart/MIGRATION.md:791-804` | Carry routing without owner | MASTER-PLAN §24 owns the carry ledger; MIGRATION §20 retains the heading but routes to MASTER-PLAN §24. The two-surface ledger principle holds. |
| H6 | Hallucination/provenance | `restart/MASTER-PLAN.md:771`; `restart/ARCHITECTURE.md:739-770` | Eight-field declaration-crate review form template anchor | MASTER-PLAN cites ARCH "§13 appendix"; the template lives in ARCH §5 (after the canonical schema rules table at line 738). The template content is correct; the anchor is wrong. |
| H7 | Hallucination/provenance | `restart/MASTER-PLAN.md:797` | Cookbook page contract template anchor | MASTER-PLAN cites "ARCH §13 appendix (landed Phase 7.1)" for the page-contract template (audience + mental model, minimum running example, diagnostic codes table, close-gate command). No such section exists in ARCH. The template is implied by the cookbook §25 column structure but is not authored anywhere. |
| H8 | Hallucination/provenance | `restart/locks/14-LOCKS.md:48`; `restart/MASTER-PLAN.md:484-488` | H.W5 wave reference | Lock 8 cites H.W5; H tranche has H.W0-H.W4 only after Phase 7.2 fold. Lock prose fossilised pre-fold wave numbering. |

Pathology summary:

- The principal V7 pathology is anchor + wave-fossil drift after a multi-target rename cascade.
- Lock 10 directive-canon prose vs ARCH §8.1 `Item` production drift is a vocabulary tail; the Lock 10 amendment "six directives" comment is correct as a count but the production name disagreement leaves readers reconciling.
- The Lock 12 broadening at MIGRATION:71 is a content drift, not an anchor drift.
- No new architecture is invented in response to fold pressure.

## §4 Compressed 9-Lane Verification

| Row | Lane | Surface | Verification | Status |
|---|---|---|---|---|
| 1 | Lane 1 - Lock coverage | Lock 1 | Tape/direct substrate at ARCH §9 + MASTER B/F/H gates + Lock 1 amendment text. | honored |
| 2 | Lane 1 - Lock coverage | Lock 4 amendment | DK13 + GADT hidden + closure-by-`&'i` at Lock 4 (line 40); cited by ARCH §8.2 + C.W1. | honored |
| 3 | Lane 1 - Lock coverage | Lock 5 amendment | Backend trait at ARCH §7.5; V1 RustBackend / V2 WasmBackend + TsBackend; cited at MASTER-PLAN §60, §80, §174, §475. | honored |
| 4 | Lane 1 - Lock coverage | Lock 6 amendment | egraph-csp-solver decoupling at `passes::bridge`; named at Lock 6 line 44. | honored |
| 5 | Lane 1 - Lock coverage | Lock 7 amendment | path-core "exists" not "may exist"; ARCH §3.4 split surfaces. | honored |
| 6 | Lane 1 - Lock coverage | Lock 8 amendment | H.W5 dead reference. | violated-with-recommendation |
| 7 | Lane 1 - Lock coverage | Lock 10 amendment | Six-directive grammar; ARCH §8.1 `Item` production agrees on six directives plus `RuleDecl`; lock production name is `Directive`, ARCH name is `Item`. | violated-with-recommendation |
| 8 | Lane 1 - Lock coverage | Lock 11 amendment | parse-that-regex rename + J.W3 publication split; cited consistently. | honored |
| 9 | Lane 1 - Lock coverage | Lock 12 amendment | A.W0 archive ceremony + `pre-restart-2026-05-04` tag + BA-/BD- retirement; MIGRATION broadens to `bbnf-path-ts`. | violated-with-recommendation |
| 10 | Lane 1 - Lock coverage | Lock 14 | YAML onboarding two-surface proof preserved across §12.1 + MASTER-PLAN §5.3. | honored |
| 11 | Lane 2 - Sequencing, Lock 1 | B before F/H | MASTER B establishes substrate; F lowers; H optimises. | honored |
| 12 | Lane 2 - Sequencing, Lock 5 | IR before codegen | C/E before F/H. | honored |
| 13 | Lane 2 - Sequencing, Lock 7 | path DSL | A.W1 names `path` + `path-core` skeletons (path-ts deferred); G owns implementation. | honored |
| 14 | Lane 2 - Sequencing, Lock 8 | SOTA | H.W3/H.W4 + J.W1 close gates; Rust-line only per Lock 8 amendment. | honored-with-amendment-on-lock-text |
| 15 | Lane 2 - Sequencing, Lock 10 | recognizer auto-detect | Pratt/SIMD shape-mined at C.W3; ARCH §8.2 forbids `@pratt`/`@simd` directives. | honored |
| 16 | Lane 2 - Sequencing, Lock 11 | path-dep incubation | J.W3 stability split; sister crates publish only after 2-tranche stability. | honored |
| 17 | Lane 2 - Sequencing, Lock 12 | archive before A | A.W0 archives ser/gorgeous; MIGRATION row broadens to bbnf-path-ts (Lock 12 fault per E5). | violated-with-recommendation |
| 18 | Lane 2 - Sequencing, Lock 13 | tree shape | A.W1 + Lock 13 verification rows at MASTER-PLAN §21. | honored |
| 19 | Lane 2 - Sequencing, Lock 14 | future grammar | A/G/F/J yaml gates + ARCH §12.1 walkthrough. | honored |
| 20 | Lane 3 - Carry chain | V5 carry | V5.1 narrow amendments (lookbehind drift, host-decl drift, chain syntax drift, YAML row, lock citations) closed; V6 returned READY. V7 reopens narrow new faults. | honored-with-amendment |
| 21 | Lane 3 - Carry chain | V6 carry | V6 R5 (rewrite-budget categories) + R7 (per-wave specs) closed via Phase 7.1 §10.1 + Phase 7.2 D.W5 + H.W3/H.W4. | honored |
| 22 | Lane 4 - Evidence | Backend trait completeness | ARCH §7.5 trait carries five methods (`lower`, `emit_runtime_template`, `emit_value_api`, `emit_visitor`, `emit_path_schema`); each method has V1/V2/V2 row. | honored |
| 23 | Lane 4 - Evidence | Per-grammar matrix | §12.2 ten-row matrix carries every column (typed root, ValueRef, runtime files, visitor, path schema, path! typing, regex engine, fixture manifest, host route, generated LOC, declaration-crate status). | honored |
| 24 | Lane 4 - Evidence | Cookbook §25 | Eight rows; every row binds target user, mental model, confusion point, artefact, diagnostic; format() row added. | honored |
| 25 | Lane 4 - Evidence | Cookbook page contract template anchor | MASTER-PLAN cites ARCH §13 appendix; no such section exists. | failed |
| 26 | Lane 4 - Evidence | Declaration-crate review form anchor | MASTER-PLAN cites ARCH §13 appendix; lives in §5 (lines 738-770). | failed |
| 27 | Lane 5 - Migration | Migration routing | MIGRATION §20 routes carry to MASTER-PLAN §24; single-ledger principle holds. | honored |
| 28 | Lane 5 - Migration | path-ts disposition | MIGRATION:71 broadens A.W0 archive ceremony to `bbnf-path-ts`. | failed |
| 29 | Lane 6 - Public surface | `path!`/`select!` | Phase 7.2 rename cascade landed; cookbook + macro typing + diagnostics aligned in MASTER-PLAN; ARCH §7.4 catalogue retains BBNF-POINTER-* codes; ARCH §12.2 retains them with deferral note. | violated-with-recommendation |
| 30 | Lane 6 - Public surface | `parse-that-regex` | Rename landed; archaeology only at §13.1 lint targets (`bbnf-regex` references retire) and §0 stale ARCH §7.2:935 oracle row. | violated-with-recommendation |
| 31 | Lane 6 - Public surface | `format()` | New cookbook row at §25:810 names public method on DocumentView and OwnedDocument; metadata-driven dispatch reading `@layout` + `@pretty`. | honored |
| 32 | Lane 7 - Overfit | yaml two-surface | Onboarding admits exactly two surfaces; declaration crate forbidden at onboarding. | honored |
| 33 | Lane 7 - Overfit | generated LOC | Per-grammar baselines at §12.2; yaml provisional ≤ 4,000; all numerics are not pseudo-precise. | honored |
| 34 | Lane 8 - LLM pathology | citations | Lock 8/11/13 line citations are correct; ARCH §13 appendix anchor is fabricated; ARCH §10.1 fail-closed-posture cite is over-promise. | failed |
| 35 | Lane 8 - LLM pathology | wave-count fossil | Lock 8 amendment text references H.W5 after H wave count drops 6 → 5. | failed |
| 36 | Lane 9 - Verdict | Trio readiness | Narrow amendments required; no redraft. | AMENDMENT-REQUIRED |

Lane 2 full conclusion:

- All fourteen locks have sequencing coverage. Lock 8, Lock 10, and Lock 12 carry text-level amendment recommendations (H.W5 fossil; Directive vs Item production-name drift; A.W0 archive broadening); the sequencing itself is sound.

## §5 16-Command Gate Rerun

| Gate | Command | Observed result | V7 read |
|---|---|---|---|
| G01 | `rg -n 'pointer!' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md` | 9 hits in ARCH (multiple §3.4, §3.5, §4, §7.5, §12.2 sites and §13.1 lint target); 0 in MASTER-PLAN; 0 in MIGRATION. | PARTIAL. MASTER-PLAN + MIGRATION clean per Phase 7.2; ARCH still owns the bulk of `pointer!` mentions, two of which (§7.4:1044-1046 BBNF-POINTER-* codes; §12.2:1579 column-name rename deferral) are stale per Lane 8. |
| G02 | `rg -n 'path!' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md` | 12+ positive citations across MASTER-PLAN §25 + ARCH §12.2 per-grammar matrix + ARCH §13.1. | PASS. |
| G03 | `rg -n 'BBNF-POINTER' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md` | 4 hits (all in ARCH §7.4 + §12.2). | PARTIAL. Catalogue codes still own BBNF-POINTER-*; rename promised at §7.4 catalogue but not landed. |
| G04 | `rg -n 'BBNF-PATH' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md` | 1 hit (MASTER-PLAN §25 cookbook row). | PARTIAL. The new code names live only in MASTER-PLAN; ARCH catalogue still retired-name-only. |
| G05 | `rg -n 'parse-that-regex' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md` | 25+ positive citations (ARCH §0 +§7.1 + §7.2 + §8 + §11 + §12.2 + §13.1 + §15; MIGRATION §6 + §17; MASTER-PLAN A.W1 + D.W4 + H.W1 + J.W3 + cookbook). | PASS. |
| G06 | `rg -n 'bbnf-regex' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md` | 2 hits, both archaeology-tagged (ARCH §13.1 naming-canon lint target; MIGRATION §13 rename annotation). | PASS. |
| G07 | `rg -n 'regex-automata' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md` | 3 hits — ARCH §7.2:935 ("regex-automata remains the oracle lane until parity is proven"); ARCH §12.2:1580 (oracle role retires per V1-FOLD); ARCH §13.1:1637 (lint target). | PARTIAL. §7.2:935 is positive surface that contradicts the §12.2 + §13.1 retirement. |
| G08 | `rg -n 'BA\.W\|BB\.W\|BC\.W\|BD\.W' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/MIGRATION.md` | 0 hits. | PASS. Lock 12 amendment cleanly retired BA-BD slot drift. |
| G09 | `rg -n 'D\.W5\|D\.W6' restart/MASTER-PLAN.md` | D.W5 hits at line 170 (D tranche header) and line 356 (D.W5 wave row); 0 D.W6 hits. | PASS. D wave count grew 5 → 6 with new D.W5; D.W0-D.W5 enumerate the six waves. |
| G10 | `rg -nC2 'path-ts.*defer\|path-ts.*post-V1' restart/MASTER-PLAN.md` | 4 positive routing rows: §60 (synthesis verdict), §80 (workspace layer), §454 (G.W4), §557 (J.W3), §705 (Lock 7 ownership), §787 (carry). | PASS. |
| G11 | `rg -nC2 'WasmBackend.*V2\|TsBackend.*V2\|WASM.*post-V1' restart/MASTER-PLAN.md` | 6+ V2 routing rows. | PASS. |
| G12 | `rg -nC2 'H\.W3\|H\.W4\|H\.W5' restart/MASTER-PLAN.md` | H.W3/H.W4 in H tranche §13 + LOC trajectory §20; H.W5 absent. | PARTIAL. Lock 8 cites H.W5; MASTER-PLAN H tranche has H.W0-H.W4 (five waves). |
| G13 | `rg -n '§13 appendix\|13 appendix' restart/MASTER-PLAN.md restart/MIGRATION.md restart/ARCHITECTURE.md` | 2 hits in MASTER-PLAN (§24:771 declaration-crate carry; §24 prose:797 cookbook contract); 0 hits in ARCH. | FAILED. The cited appendix does not exist. |
| G14 | `rg -n 'fail-closed posture\|representative-stability protocol' restart/ARCHITECTURE.md` | 0 hits. | FAILED. C.W4 cite at MASTER-PLAN:321 over-promises §10.1. |
| G15 | `rg -n 'eight.field\|8.field' restart/ARCHITECTURE.md restart/MASTER-PLAN.md` | 1 hit in ARCH §5 (eight-field review form text); 1 hit in MASTER-PLAN §24 (eight-field cite to §13 appendix). | PARTIAL. Template content exists at §5; cite anchor wrong. |
| G16 | `rg -n 'CPU model\|compiler flags\|input hash\|competitor version\|warmup\|sample' restart/MASTER-PLAN.md restart/MIGRATION.md` | 8+ hits in MASTER-PLAN §4 benchmark reproducibility schema; 0 in MIGRATION (carry routing). | PASS. |

Gate summary:

- PASS: G02, G05, G06, G08, G09, G10, G11, G16.
- PARTIAL: G01, G03, G04, G07, G12, G15.
- FAILED: G13, G14.

Targeted verification notes:

- `rg -n 'fn lower\|fn emit_runtime_template\|fn emit_value_api\|fn emit_visitor\|fn emit_path_schema' restart/ARCHITECTURE.md`
  - Result: ARCH §7.5 trait carries all five method signatures; the per-method obligations table at lines 1113-1119 maps inputs and outputs across V1/V2/V2 backends.
  - V7 read: Backend trait fidelity is sound. The method names match LLVM/Cranelift/swc precedent in spirit (per-target lower step + per-artefact emit steps); the `emit_*` family is a superset of swc's per-target emit and a subset of LLVM's per-target codegen pipeline. The methods are not invented.

- `nl -ba restart/locks/14-LOCKS.md | sed -n '46p;48p;52p;54p;56p'`
  - Result: Lock 7 line 46; Lock 8 line 48; Lock 10 line 52; Lock 11 line 54; Lock 12 line 56.
  - V7 read: Line citations across MASTER-PLAN + MIGRATION + ARCHITECTURE consistently resolve. The V5 P6 + P7 wrong-line citations have closed and no new wrong lines emerge.

- `rg -n 'Damas-Milner\|Pierce 2002\|Pierce-Turner\|Dunfield-Krishnaswami' restart/ARCHITECTURE.md`
  - Result: §8.2 names four citations in one paragraph (1273-1284); each is functionally distinct (Algorithm-W naming; HM equality; bidirectional check/synth; DK13 higher-rank).
  - V7 read: Citation density is intentional and accurate; not pile-up.

- `rg -n '^| H\.W' restart/MASTER-PLAN.md`
  - Result: H tranche enumerates H.W0-H.W4 (five waves) in §13 stub waves table and §20 LOC trajectory.
  - V7 read: H.W5 does not exist; Lock 8 amendment text at line 48 refers to a wave that never landed in the post-Phase-7.2 trio.

- `rg -nC2 'review form\|page contract' restart/MASTER-PLAN.md`
  - Result: Two MASTER-PLAN rows cite "ARCH §13 appendix" — the declaration-crate carry row at §24:771, and the cookbook contract template at §24:797.
  - V7 read: The eight-field declaration-crate review form template exists in ARCH §5 (lines 738-770) but not in §13 appendix. The cookbook page contract template (audience + mental model, minimum running example, diagnostic codes table, close-gate command) is not authored in ARCH at all; it is implied by the cookbook §25 column structure.

Local edit hygiene:

- Only this report path was written.
- Target surfaces were not patched.
- No lock, prompt, research, pass, crate, corpus, inheritance, archive, README, or other hardening report was edited.

## §6 Cross-Document Binding Ledger

| Claim | ARCHITECTURE binding | MIGRATION binding | MASTER binding | External binding | V7 result |
|---|---|---|---|---|---|
| `Backend` trait at ARCH §7.5 is the per-backend boundary contract. | `restart/ARCHITECTURE.md:1067-1144` | Absent (cite-only via `restart/MIGRATION.md:71`, §17:681) | `restart/MASTER-PLAN.md:60`, `:80`, `:174`, `:475`, `:705`, `:782`, `:787`, `:788` | `restart/locks/14-LOCKS.md:42` | KEEP |
| RustBackend ships V1; WasmBackend / TsBackend defer V2. | `restart/ARCHITECTURE.md:1072-1074`, `:1419-1420` | `restart/MIGRATION.md:660`, `:681` | `restart/MASTER-PLAN.md:60`, `:174`, `:475`, `:557`, `:705`, `:787`, `:788` | Lock 5 line 42; Lock 8 line 48 | KEEP |
| `pointer!` retires; `path!` is canonical. | `restart/ARCHITECTURE.md:1575`, `:1636` (lint target) | Absent (no macro mentions) | `restart/MASTER-PLAN.md:173`, `:226`, `:442`, `:451`, `:802` | Lock 7 line 46 + naming-canon lint | AMEND-CATALOGUE |
| `BBNF-POINTER-*` codes retire to `BBNF-PATH-*`. | `restart/ARCHITECTURE.md:1044-1046` (still BBNF-POINTER-*); `:1579` (column rename deferred) | Absent | `restart/MASTER-PLAN.md:802` (already BBNF-PATH-*) | Phase 7.2 §A rename ledger | AMEND |
| `parse-that-regex` is canonical regex sub-crate. | `restart/ARCHITECTURE.md:25`, `:849`, `:869`, `:1198`, `:1244-1245`, `:1559-1568`, `:1580`, `:1637`, `:1669` | `restart/MIGRATION.md:506`, `:595`, `:681` | `restart/MASTER-PLAN.md:250`, `:355`, `:485`, `:501`, `:557`, `:777` | Lock 11 line 54 | KEEP |
| `regex-automata` retires (oracle role gone). | `restart/ARCHITECTURE.md:935` (positive — stale); `:1580` (retired); `:1637` (lint) | Absent | Absent | V1-FOLD-CANDIDATES Tier 3 #23 | AMEND |
| `bbnf-regex` is archaeology only. | `restart/ARCHITECTURE.md:1636` (lint target only) | `restart/MIGRATION.md:595` (rename annotation) | Absent | Lock 11 line 54 | KEEP |
| BA-BD slot drift retires. | `restart/ARCHITECTURE.md:14` (citation) | `restart/MIGRATION.md:650-660` (legacy mining) | `restart/MASTER-PLAN.md:13`, `:196`, `:546`, `:641`, `:782` | Lock 12 line 56 | KEEP |
| D wave count grows 5 → 6 with D.W5 function-value lowering. | Absent (no tranche table in ARCH) | Absent | `restart/MASTER-PLAN.md:170`, `:356` | V1-FOLD §5 | KEEP |
| H wave count drops 6 → 5 with WASM defer. | Absent | Absent | `restart/MASTER-PLAN.md:174`, `:484-493` | Lock 8 (line 48 still cites H.W5) | AMEND-LOCK |
| `path-ts` defers post-V1. | `restart/ARCHITECTURE.md:63`, `:1420` | `restart/MIGRATION.md:71` (broadens A.W0 archive ceremony) | `restart/MASTER-PLAN.md:60`, `:80`, `:454`, `:557`, `:705`, `:787`, `:790` | Lock 7/11 amendments | AMEND-MIGRATION |
| WASM SOTA defers post-V1. | `restart/ARCHITECTURE.md:1419`, `:1497` | `restart/MIGRATION.md:660`, `:681` | `restart/MASTER-PLAN.md:138-141`, `:174`, `:209`, `:227`, `:475-476`, `:557`, `:629`, `:782`, `:788` | Lock 8 line 48 | KEEP-WITH-AMENDMENT |
| Six-directive grammar lands. | `restart/ARCHITECTURE.md:1158`, `:1210-1235` | Absent | `restart/MASTER-PLAN.md:170` (hint via D wave grow), `:355` (D.W4 routing) | Lock 10 line 52 | AMEND-NAMING |
| DK13 + GADT hidden + closure-by-`&'i`. | `restart/ARCHITECTURE.md:1273-1308` | Absent | `restart/MASTER-PLAN.md:318` (C.W1 binding), `:352` (D.W1), `:356` (D.W5) | Lock 4 line 40 | KEEP |
| egraph-csp-solver decoupling at `passes::bridge`. | Implicit at §7 + §10.1 | Absent | `restart/MASTER-PLAN.md:321` (C.W4 bridge tables) | Lock 6 line 44 | KEEP |
| `path-core` exists (not "may exist"). | `restart/ARCHITECTURE.md:62-63` | `restart/MIGRATION.md:594` | `restart/MASTER-PLAN.md:80`, `:250`, `:705` | Lock 7 line 46 | KEEP |
| `@pretty` directive vocabulary preserved verbatim. | `restart/ARCHITECTURE.md:1167-1168`, `:1216-1219` | Absent | `restart/MASTER-PLAN.md:810` (cookbook row references) | Lock 10 line 52 | KEEP |
| `@token` directive lands. | `restart/ARCHITECTURE.md:1169`, `:1219` | Absent | Absent (carries in §3 IR contract) | Lock 10 line 52 | KEEP |
| Function values + types first-class. | `restart/ARCHITECTURE.md:1185`, `:1190`, `:1194-1195`, `:1227-1235`, `:1293-1300` | Absent | `restart/MASTER-PLAN.md:170`, `:352`, `:356` | Lock 4 + Lock 10 amendments | KEEP |
| Schema-mining miner. | `restart/ARCHITECTURE.md:1302-1308` | Absent | `restart/MASTER-PLAN.md:319` (C.W2) | V1-FOLD #7 | KEEP |
| Rewrite-budget categories at §10.1. | `restart/ARCHITECTURE.md:1425-1448` | Absent | `restart/MASTER-PLAN.md:321` (C.W4 cite) | V1-FOLD #26 | AMEND-OVERPROMISE |
| Lint manifest at §13.1. | `restart/ARCHITECTURE.md:1624-1643` | Absent | `restart/MASTER-PLAN.md:253` (A.W4 cite) | V1-FOLD #27 | KEEP |
| Eight-field declaration-crate review form. | `restart/ARCHITECTURE.md:739-770` (in §5) | Absent | `restart/MASTER-PLAN.md:771` (cites "§13 appendix") | V1-FOLD #28 | AMEND-ANCHOR |
| Cookbook page contract template. | Absent (no §13 appendix) | Absent | `restart/MASTER-PLAN.md:797` (cites "§13 appendix landed Phase 7.1") | V1-FOLD #29 | AMEND-PHANTOM |
| `format()` public method. | Absent (Lock 10 + ARCH §8.1 retain `@pretty` directive only) | Absent | `restart/MASTER-PLAN.md:810` (cookbook row) | V1-FOLD #4 (audit #5 F4) | KEEP |

Ledger conclusion:

- The binding ledger is strong on architecture, lock coverage, and tranche routing.
- The amendment surface is concentrated on three classes: (i) BBNF-POINTER-* catalogue rename trailing the cookbook rename; (ii) ARCH §13 appendix anchor for two templates that live elsewhere or do not exist; (iii) Lock 8 H.W5 wave-count fossil after H wave count dropped to five.
- The ledger is amendment-grade, not redraft-grade.

## §7 Deduped Punch List

| ID | Path:line | Surgery | Acceptance gate | Lens origin |
|---|---|---|---|---|
| P1 | `restart/locks/14-LOCKS.md:48` | Replace "H.W3, H.W4, and H.W5" with "H.W3 and H.W4". The post-Phase-7.2 H tranche has H.W0-H.W4 (five waves); H.W5 retired with WASM defer. | `rg -n 'H\.W5' restart/locks/14-LOCKS.md restart/MASTER-PLAN.md` returns zero outside legitimate WASM-V2 deferral notes. | B5, F5, H8 |
| P2 | `restart/MASTER-PLAN.md:771` | Change cite "template at `restart/ARCHITECTURE.md` §13 appendix" to "template at `restart/ARCHITECTURE.md` §5 (lines 738-770)" or to a new §5.6 sub-section header that the architecture amendment can introduce. | `rg -n '§13 appendix' restart/MASTER-PLAN.md` returns zero; the cite resolves to a real section in ARCH. | A5, F4, H6 |
| P3 | `restart/MASTER-PLAN.md:797` | Either author the cookbook page contract template as ARCH §13 appendix (audience + mental model, minimum running example, diagnostic codes table, close-gate command), or remove the "(landed Phase 7.1)" claim and route the template to a new tranche slot that owns the work. The current cite is a phantom artefact. | The cited section exists in ARCH; the cite resolves; J.W2 cookbook gate consumes the named contract. | A5, F4, H7 |
| P4 | `restart/MIGRATION.md:71` | Remove the "archives at A.W0 alongside `ser`/`gorgeous`" clause. Lock 12 names only `ser` and `gorgeous` for A.W0 ceremony. Replace with "deletion or replacement scheduled at the V2 amendment when `TsBackend: Backend` lands; V1 carries no `bbnf-path-ts` archive ceremony at A.W0". | Lock 12 line 56 reads identical to the archived-crate set; no Lock 12 broadening. | B4, E5 |
| P5 | `restart/MASTER-PLAN.md:321` | Remove the "fail-closed posture, representative-stability protocol now landed at architecture level per Phase 7.1" clause from C.W4, or amend ARCH §10.1 to author both protocols verbatim. The current cite over-promises §10.1. | `rg -n 'fail-closed posture\|representative-stability protocol' restart/ARCHITECTURE.md` returns positive matches if the protocols are authored, or the C.W4 row no longer makes the claim. | A4, F3, G14 |
| P6 | `restart/locks/14-LOCKS.md:52`; `restart/ARCHITECTURE.md:1158` | Reconcile production naming: Lock 10 amendment says `Directive = ImportDecl | HostFn | ErrorDecl | LayoutDecl | PrettyDecl | TokenDecl ;`; ARCH §8.1 says `Item ::= ImportDecl | HostFn | RuleDecl | LayoutDecl | ErrorDecl | PrettyDecl | TokenDecl`. Two surgical options: (a) rename Lock 10's production to `Item` and make Lock 10's count "seven items including six directives plus one rule declaration"; (b) add a sub-production `Directive ::= ImportDecl | HostFn | ErrorDecl | LayoutDecl | PrettyDecl | TokenDecl` to ARCH §8.1 and amend `Item` to `Item ::= Directive | RuleDecl`. Option (b) preserves the Lock 10 spelling and matches the lint manifest's `directive-canon` lint name. | ARCH §8.1 production names match Lock 10 amendment text; the lint manifest `directive-canon` lint at §13.1 retains its target identity. | B1 |
| P7 | `restart/ARCHITECTURE.md:1044-1046`; `restart/ARCHITECTURE.md:1579` | Either rename `BBNF-POINTER-*` catalogue codes to `BBNF-PATH-*` in §7.4 + §12.2 alongside MASTER-PLAN §25, or commit the deferral schedule (which tranche owns the catalogue rename, gated when). The current state has MASTER-PLAN §25 row already on `BBNF-PATH-*` while ARCH §7.4 catalogue still owns `BBNF-POINTER-*`. | Either ARCH catalogue + MASTER-PLAN cookbook row agree on `BBNF-PATH-*`, or the deferral row appears in MASTER-PLAN §24 carry ledger with named receiver and gate. | A1, B2 |
| P8 | `restart/ARCHITECTURE.md:935` | Rewrite the §7.2 BIR variant note that says "regex-automata remains the oracle lane until parity is proven" to align with the §12.2 + §13.1 retirement of the oracle role. The current line 935 is a positive surface that contradicts the §12.2:1580 retirement and the §13.1:1637 lint target. Per V1-FOLD-CANDIDATES Tier 3 #23 and the audit #4 + #6 user-mandate forbid, regex-automata oracle citation should be removed entirely from §7.2. | `rg -n 'regex-automata' restart/ARCHITECTURE.md` returns hits only at §12.2 retirement note and §13.1 lint target. | B3, G07 |
| P9 | `restart/MASTER-PLAN.md:174`; `restart/MASTER-PLAN.md:194` | Update H tranche close gate prose at line 174 to read "Auto-detected Pratt/SIMD pass early SOTA gates on the Rust line at H.W3/H.W4. WASM defers ..." (drop H.W5 implication). Calendar slot row at line 194 already says "Pratt, SIMD, early perf on Rust line"; verify no H.W5 reference survives in ARCH §11 or MIGRATION §17 either. | The H tranche prose nowhere references H.W5; the Rust-line SOTA waves are H.W3 and H.W4 only. | B5, F5 |
| P10 | `restart/ARCHITECTURE.md:1247`; `restart/ARCHITECTURE.md:1559`; `restart/ARCHITECTURE.md:1584` | Define §5.6 as a sub-section header for the declaration-crate review form template, or rewrite the §5.6 references at §8.1:1247, §12.2:1559, §12.2:1584 to point to the actual line range (§5 fence around lines 739-770). The current state has three "§5.6 fence" references but no §5.6 header anywhere in §5. | `rg -n '^### 5\.6' restart/ARCHITECTURE.md` returns one match, or the §5.6 references are removed and replaced with an inline cite. | A5, H6 |

Punch-list priority:

- P1, P2, P3 are amendment blockers because they are LLM-pathology artefacts with concrete consequence: a dead wave reference in a lock, a wrong anchor cited twice, and a phantom artefact cited as "landed".
- P4 is a Lock 12 broadening that exceeds the lock's two-crate amendment surface; not a redraft, but a contract-text surgery.
- P5 is an LLM over-promise that misrepresents §10.1 content; not load-bearing, but the cite is wrong.
- P6 is a vocabulary-drift amendment that touches Lock 10 + ARCH §8.1 in concert.
- P7, P8 are catalogue-rename trailing edges (V6 R1/R2-style hygiene) that the Phase 7 fold did not finish.
- P9, P10 are anchor + reference cleanups that follow from P1, P2, P3.

## §8 V5->V6 History Note

V5 result:

- `HARDENING-MASTER-PLAN-V5.md` (the 419-line target-specific report) found AMENDMENT-REQUIRED.
- V5 themes: HostDecl declaration-only grammar drift (P1), MapExpr `=> TypeExpr` chain syntax drift (P2), method-chain rule-level drift (P3), `@host fn declarations` wave text drift (P4), YAML row pipe-cell mismatch (P5), Lock 14 wrong-line citation (P6), Lock 11 wrong-line citation (P7), json/canada SOTA provenance asymmetry (P8), missing public-query worked example (P9), missing yaml onboarding worked example (P10), missing incremental-thresholds binding (P11), missing `@error(recover)` worked example (P12), missing WASM host primitive ABI matrix (P13), missing A→F→J grammar trajectory (P14), Lock 11 ownership polish (P15).

V5.1 result (closed by `4fe06344` and predecessors):

- All P1-P14 closed via narrow amendments.
- The trio returned to READY before V6 launched.

V6 result:

- `HARDENING-CONSOLIDATED-V6.md` returned READY across the cohort (PASS-1, PASS-2, PASS-3, SYNTHESIS).
- V6 R1-R7 residue routed: research-index hygiene; README precision; harness rerun checklist; Lock 4 egglog rationale; rewrite-budget tests; H.W3 WASM placeholders; full per-wave tranche specs.
- V6 found no blocking amendment row; the trio was structurally ready for tranche drafting.

V7 delta:

- V7 audits the post-Phase-7 trio against the amended locks + the V1-FOLD synthesis.
- The Phase 7.1 amendments (Backend trait at §7.5, six-directive grammar at §8.1, type-system §8.2, rewrite-budget §10.1, lint manifest §13.1, declaration-crate review form in §5) all landed.
- The Phase 7.2 cascade (`pointer!` → `path!` rename in MASTER-PLAN; parse-that-regex cascade; TS/WASM defer; D wave grow 5 → 6; H wave drop 6 → 5; format() cookbook row) all landed in MASTER-PLAN + MIGRATION.
- V7 finds residue at four anchor sites (P1, P2, P3, P5), one Lock 12 broadening (P4), one production-name drift (P6), and two catalogue trailing edges (P7, P8) plus surface clean-up (P9, P10).
- The carry lesson: a multi-target rename cascade that touches both producer (ARCH catalogue) and consumer (MASTER-PLAN cookbook + lock prose) requires the cascade to finish on the producer side; trailing producer surfaces invite drift.

## §9 LLM-Pathology Summary

LLM bias subclasses observed:

- Phantom anchor: MASTER-PLAN cites "ARCH §13 appendix" twice; the appendix does not exist as a section. The eight-field template lives at ARCH §5; the cookbook page contract template is unauthored. The cites are plausible-sounding fabrications dressed as Phase 7.1 deliverables.
- Wave-count fossil: Lock 8 amendment text says "H.W3, H.W4, and H.W5" but H wave dropped from six to five during Phase 7.2 cascade. Lock prose lagged the wave-count change by one fold step.
- Cite over-promise: MASTER-PLAN C.W4 cites §10.1 for "fail-closed posture, representative-stability protocol now landed at architecture level per Phase 7.1". §10.1 carries categories and thresholds; "fail-closed posture" and "representative-stability protocol" are missing from the architecture text. The cite confidently claims content that did not land.

Overfitting subclasses observed:

- None new. The yaml two-surface proof, generated LOC budgets, Backend trait per-grammar audit, lint manifest fence canon, and SOTA fixture set are principled, not overfit.

Hallucination/provenance subclasses observed:

- Two anchor mis-pointers (§13 appendix → §5).
- One missing artefact (cookbook page contract template).
- One wave-count fossil (H.W5 in Lock 8).
- One cite over-promise (§10.1 protocols).
- One Lock 12 broadening (bbnf-path-ts in A.W0 archive ceremony).
- Two catalogue trailing edges (BBNF-POINTER-* codes; regex-automata §7.2:935).

Pathology boundary:

- No new architecture is invented in response.
- The amendment set is text-surgical: rewrite three cite anchors, retire one wave reference, narrow Lock 12 contract to its two-crate scope, complete two catalogue renames.
- Research fold work can proceed from a cleaner trio after these surgeries.

## §10 Verdict

Verdict:

- **AMENDMENT-REQUIRED**

Not READY because:

- P1 is a hard lock-prose fault (H.W5 dead reference).
- P2 + P3 are anchor faults that misroute readers searching for templates.
- P4 broadens Lock 12 contract scope without amendment.
- P5 over-promises ARCH §10.1 content with confident "now landed at architecture level per Phase 7.1" prose.
- P6 leaves vocabulary drift between lock prose and architecture production names.
- P7, P8, P9, P10 are tail-end hygiene that the Phase 7 fold cascade did not close.

Not RE-DRAFT because:

- Tranche topology, lock ownership, migration routing, and gate structure remain coherent.
- The Phase 7.1 architectural prerequisites (Backend trait, six-directive grammar, type-system §8, rewrite-budget §10.1, lint manifest §13.1, declaration-crate review form in §5) all landed.
- The Phase 7.2 cascade (rename ledger, TS/WASM defer, D wave growth, H wave drop, cookbook expansion) is materially complete.
- The amendment set is surgical and document-local.

Acceptance standard for re-verdict:

- P1-P10 resolved or explicitly routed with receiver, blocker, and gate.
- The 16-command gate rerun shows all PARTIAL gates close (G01, G03, G04, G07, G12, G15) and both FAILED gates close (G13, G14).
- Lock 8 amendment text matches the post-Phase-7.2 H wave count.
- Lock 12 amendment scope remains the original two crates plus archive-tag citation.
- ARCH §13 appendix anchor either resolves to a real sub-section or is removed from MASTER-PLAN cites.
- ARCH §10.1 protocol prose either lands the referenced concepts or the C.W4 cite drops the claim.

## §11 Closing Posture

Recommended posture:

- Apply a narrow trio amendment (P1-P10).
- Do not redraft tranche sequence.
- Do not reopen closed V1-V6 items unless one of the P1-P10 fixes touches them.
- Keep PASS files as ground truth for grammar IR, BIR, and runtime contracts; they are sister Phase-7.2 fold targets and were audited separately.

Estimated amendment wall time:

- 60 to 90 minutes for a careful amendment pass.
- 15 minutes for Lock 8 H.W5 retirement (P1) and §13 appendix anchor surgery (P2, P3).
- 15 minutes for Lock 12 narrowing (P4) and §10.1 cite reconciliation (P5).
- 15 minutes for Item / Directive production-name reconciliation (P6).
- 15 to 45 minutes for BBNF-POINTER-* catalogue rename (P7) and §7.2:935 regex-automata cleanup (P8) plus §5.6 anchor surgery (P10).

Stop condition for the amendment:

- The trio should become READY after P1-P10 are resolved.
- Any attempt to redesign the Backend trait, the type system, the rewrite-budget categories, or the lock amendment surface is outside the V7 finding set.
- Any new section authoring (cookbook page contract template at §13 appendix, §5.6 sub-section) is a one-paragraph addition, not a structural change.

Hereupon the V7 audit closes. The Phase 7 fold preserved V6's READY across most of the trio; the residue is text-surgical and concentrated on cite hygiene plus one wave-count fossil. The trio is one narrow amendment cycle away from V7 READY.
