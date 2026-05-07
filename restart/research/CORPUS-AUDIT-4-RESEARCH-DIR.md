# CORPUS AUDIT 4 — research/ directory

This is the fourth corpus audit of the bbnf-lang greenfield restart, scoped to `restart/research/`. The directory carries 22 markdown files / 10,486 lines / ~840K of SOTA grounding split across five strata: 1 INDEX (Phase 5 catalogue), 8 topic deep-dives (Phase 5 SOTA artefacts), 4 fold reports (Phase 5 fold record), 8 deferral audits (Phase 7 input cohort), 1 V1-FOLD-CANDIDATES synthesis (Phase 7 contract). The audit classifies each file under EXPLICATE / UPDATE / PRUNE / KEEP-AS-SEALED-RESEARCH and recommends a pruning path.

## §1 — Audit scope

| Stratum | Files | Lines | Phase |
|---|---:|---:|---|
| INDEX | 1 | 183 | Phase 5 catalogue (cited by `RESEARCH-FOLD-ORCHESTRATOR.md:130-137` + HANDOFF) |
| Topic deep-dives | 8 | 5,765 | Phase 5 SOTA grounding |
| Fold reports | 4 | 969 | Phase 5 fold record (cited only by V6 hardening) |
| Deferral audits | 8 | 3,245 | Phase 7 input cohort (one-shot inputs to V1-FOLD-CANDIDATES) |
| V1-FOLD-CANDIDATES | 1 | 221 | Phase 7 contract (cited by V8 + ARCH + Lock 8) |
| **Total** | **22** | **10,383** | |

Citation footprint by phase (rg per file pattern, restart-wide):

| File class | V6 cites | V7 cites | V8 cites | Live cites (ARCH/MIGRATION/MASTER-PLAN/locks) |
|---|---:|---:|---:|---:|
| `INDEX.md` | 6 | 0 | 0 | 1 (HANDOFF reference) |
| `topic-{1..8}-*.md` | 25 | 1 | 0 | 0 |
| `fold-{pass-1,pass-2,pass-3,synthesis}.md` | 25 | 0 | 0 | 0 |
| `deferral-audit-{1..8}-*.md` | 0 | 0 | 0 | 1 (HANDOFF reference) |
| `V1-FOLD-CANDIDATES.md` | n/a | 5 | 3 | 4 (ARCH:940, ARCH:1585, ARCH:1642, locks:48) |

The pattern is sharp: INDEX + topics + fold reports were Phase 5 evidence sources for V6 hardening; deferral audits were one-shot inputs to V1-FOLD-CANDIDATES; V1-FOLD-CANDIDATES is the only live Phase 7 contract carrying load-bearing citations through V8.

## §2 — Topic deep-dives (8 files; 5,765 lines)

Each topic carries §1 settled position / §2 SOTA literature / §3 convergence / §4 divergence / §5 refinements / §6 adversarial findings / §7 surgery proposals (per `INDEX.md:20-34` output contract).

| # | File | Lines | Anchor locks | §6 finding count | Absorption status | Disposition |
|---:|---|---:|---|---:|---|---|
| 1 | `topic-1-hm-foundations.md` | 737 | Lock 2, Lock 4 | 3 (A1-A3) | All absorbed at Phase 5 fold (`fold-pass-1.md:67-77`, `fold-synthesis.md:60-79`) → V6 hardening (`HARDENING-PASS-1-V6.md:79`) → Phase 7 fold (V7.1 READY); HM-decomposition wording lives at ARCH §8.2 lines 1117-1132 | **KEEP-AS-SEALED-RESEARCH** |
| 2 | `topic-2-bidirectional.md` | 532 | Lock 2, Lock 4 | 5 (A-E; lock-numbering drift, "full HM" overstrength, DK13 cite, coercion examples, Roc role) | A absorbed (lock numbering corrected V6); B-D absorbed via PASS-1 §3 chain rule + ARCH §8.2; E (Roc role) absorbed at INDEX §2.1 hygiene table | **KEEP-AS-SEALED-RESEARCH** |
| 3 | `topic-3-csp-gadts.md` | 594 | Lock 2, Lock 4, Lock 14 | (engagement: HM(X) vs OutsideIn vs orthogonal CSP) | Phase 7 fold landed CSP/HM split + GADT-substrate-hidden + OutsideIn-deferred per `V1-FOLD-CANDIDATES.md:54` Tier 1 #9 | **KEEP-AS-SEALED-RESEARCH** |
| 4 | `topic-4-egraphs.md` | 926 | Lock 4 | 5 (A1-A5; representative promotion, Lock 4 egglog argument, rewrite categories, bridge proofs, source provenance) | A1-A4 absorbed at Lock 6 amendment + ARCH §10 + V1-FOLD-CANDIDATES Tier 1 #10; A5 (egglog provenance) corrected at `INDEX.md:107` | **KEEP-AS-SEALED-RESEARCH** |
| 5 | `topic-5-cost-models.md` | 895 | Lock 4, Lock 8 | (engagement: shared trait shape vs shared semantics) | Tier α2 V8 simplification carry (`HARDENING-CONSOLIDATED-V8.md:36`) acknowledges trait-shared-shape-not-semantics finding; cost-model crate hygiene at V1-FOLD-CANDIDATES Tier 3 | **KEEP-AS-SEALED-RESEARCH** |
| 6 | `topic-6-tape.md` | 504 | Lock 1, Lock 8 | (engagement: union vs co-existence, simdjson two-stage, sonic-rs tape) | All A/S items absorbed at PASS-2 §B + PASS-3 §3 + ARCH §11 per `fold-synthesis.md:46-50`; tape/direct union locked at Lock 1 amendment | **KEEP-AS-SEALED-RESEARCH** |
| 7 | `topic-7-green-red-incremental.md` | 749 | Lock 1, Lock 14 | (engagement: substrate vs rust-analyzer green/red separation) | All A/S items absorbed at PASS-3 §3 ReparsePlan + recovery node shape per `fold-pass-3.md:60-68`; salsa lean cited as design language at γ7 V8 | **KEEP-AS-SEALED-RESEARCH** |
| 8 | `topic-8-simd-dfa.md` | 931 | Lock 1, Lock 8, Lock 10 | (engagement: parse-that vs regex-automata, SIMD prefilter contract) | All §5/§7 items absorbed at PASS-2 detection + PASS-3 §16 + ARCH §7.2 RegexProgram per V1-FOLD-CANDIDATES Tier 3 #23; oracle role retired | **KEEP-AS-SEALED-RESEARCH** |

**Topic verdict — KEEP-AS-SEALED-RESEARCH for all 8.**

The §6 adversarial findings + §7 surgery proposals were absorbed across two cycles: Phase 5 fold (4 fold reports) + Phase 7 fold (V1-FOLD-CANDIDATES). V6/V7/V8 hardening verifies the absorption; no §6 finding remains unrouted. The topics retain value as the **primary-literature provenance trail** for every load-bearing claim in the 14 locks; pruning would orphan citations like `INDEX.md:46-53` (verified-source-slot table) and `HARDENING-PASS-1-V6.md:79-97` (Topic-by-Topic routing audit).

Topic engagement quality verifies as load-bearing — every topic has at least one V6-cited row, the §6 findings are non-trivial (Topic 4 surfaces representative promotion + egglog counterargument; Topic 1 surfaces HM-subsumption-overstrength), and the SOTA-source-slot ledger is INDEX's authority on provenance hygiene.

## §3 — Fold reports (4 files; 969 lines)

| # | File | Lines | Phase | Cited by | Disposition |
|---:|---|---:|---|---|---|
| 1 | `fold-pass-1.md` | 235 | Phase 5 fold record (PASS-1 worker classification) | `HARDENING-PASS-1-V6.md` 8 cites; `HARDENING-PASS-2-V6.md` ; `HARDENING-PASS-3-V6.md` ; `HARDENING-SYNTHESIS-V6.md` ; `deferral-audit-3:196`, `deferral-audit-1:269` | **KEEP-AS-SEALED-RESEARCH** (V6 verification trail; not cited V7+) |
| 2 | `fold-pass-2.md` | 216 | Phase 5 fold record (PASS-2 worker classification) | `HARDENING-PASS-2-V6.md`; `deferral-audit-3:195`, `deferral-audit-6:144,165,195,411` | **KEEP-AS-SEALED-RESEARCH** |
| 3 | `fold-pass-3.md` | 161 | Phase 5 fold record (PASS-3 worker classification) | `HARDENING-PASS-3-V6.md` 12 cites; `deferral-audit-3:194` | **KEEP-AS-SEALED-RESEARCH** |
| 4 | `fold-synthesis.md` | 357 | Phase 5 fold record (SYNTHESIS worker classification) | `HARDENING-PASS-3-V6.md`; `HARDENING-SYNTHESIS-V6.md`; `deferral-audit-7:168` | **KEEP-AS-SEALED-RESEARCH** |

**Fold reports verdict — KEEP-AS-SEALED-RESEARCH for all 4.**

The fold reports are Phase 5 worker classification artefacts: each names the §5/§7 items folded vs deferred per the four amendment surfaces (PASS-1, PASS-2, PASS-3, SYNTHESIS trio). They were the V6 hardening verification trail — V6 reports cite specific fold-report:line anchors to verify "topic finding X is routed away from PASS-Y" claims (`HARDENING-PASS-1-V6.md:79-97` is wall-to-wall fold-pass-1 routing citations).

V7 onward did not cite the fold reports (zero cites in V7 + V8 hardening reports). Phase 7 introduced V1-FOLD-CANDIDATES as a different artefact class (synthesised fold contract, not per-worker classification). The fold reports are not architecturally superseded — they are still load-bearing for V6 verification — but their forward cite count is zero.

The four reports do not merit PRUNE: they are the deserialised audit trail proving Phase 5 fold did not silently drop topic findings. If V6 ever needs to be re-audited (e.g., during a future hardening cycle that questions topic absorption), these reports are the discriminator. The MINIMAL pruning path (§7) reconsiders this; the recommendation below preserves them.

## §4 — Deferral audits (8 files; 3,245 lines)

Per `V1-FOLD-CANDIDATES.md:9-19` cohort overview — these were Phase 7 inputs that synthesised into the 30-item V1 fold list.

| # | File | Lines | Scope | Folds proposed | Citation trail | Disposition |
|---:|---|---:|---|---:|---|---|
| 1 | `deferral-audit-1-type-system.md` | 393 | HM, bidirectional, CSP, generics, GADTs, row-poly, refinement | 4 high-value + 13 catalogued | Cited by V1-FOLD-CANDIDATES §1 + §3 Tier 1 items 2-9; HANDOFF reference row | **KEEP-AS-SEALED-INPUT** |
| 2 | `deferral-audit-2-function-value-system.md` | 442 | Function values, lambdas, closures, composition, match, tuples | 9 (all FOLD V1) | V1-FOLD-CANDIDATES §1 + Tier 1 items 3-6, Tier 2 items 19-20, Tier 3 item 25 | **KEEP-AS-SEALED-INPUT** |
| 3 | `deferral-audit-3-bbnf-surface-directives.md` | 359 | BBNF surface + @directives + `pointer!` → `path!` rename | 8 (rename ledger + directive lean) | V1-FOLD-CANDIDATES §1 + Tier 2 items 11-17 | **KEEP-AS-SEALED-INPUT** |
| 4 | `deferral-audit-4-sibling-crates.md` | 335 | csp-solver, parse-that, egraph, simd-scan | parse-that build + egraph decoupling + 4 hygiene | V1-FOLD-CANDIDATES §1 + Tier 1 items 1, 10; Tier 3 items 21-23 | **KEEP-AS-SEALED-INPUT** |
| 5 | `deferral-audit-5-runtime-pass3.md` | 270 | Runtime / PASS-3 / public API / TS bindings / WASM ABI / pretty | 7 V1 folds + 4 escalations | V1-FOLD-CANDIDATES §1 + Tier 1 item 7; conflict resolutions 9 (path-ts) | **KEEP-AS-SEALED-INPUT** |
| 6 | `deferral-audit-6-codegen-pass2.md` | 458 | Codegen / PASS-2 / lowering / regex-automata / function-value lowering | 1 fold (regex-automata oracle decision) | V1-FOLD-CANDIDATES §1 + conflict resolutions 6 (oracle); Tier 3 item 23 | **KEEP-AS-SEALED-INPUT** |
| 7 | `deferral-audit-7-locks-architecture.md` | 415 | 14 locks + ARCHITECTURE deferral ledger | 5 lock amendments + 2 hygiene | V1-FOLD-CANDIDATES §1 + Tier 4 item 30 (5 lock amendments) | **KEEP-AS-SEALED-INPUT** |
| 8 | `deferral-audit-8-migration-tranche.md` | 573 | Migration + MASTER-PLAN + tranche residue | 8 V1 folds (ARCH/PLAN amendments + templates) | V1-FOLD-CANDIDATES §1 + §5 tranche impact + Tier 4 items 26-29 | **KEEP-AS-SEALED-INPUT** |

**Deferral audit verdict — KEEP-AS-SEALED-INPUT for all 8** under the recommended NONE pruning path; AGGRESSIVE path keeps audit #4 as exemplar and prunes 7.

The deferral audits were one-shot Phase 7 inputs. Their purpose was to surface ~50 distinct fold candidates across 8 cohorts; V1-FOLD-CANDIDATES de-duplicated to 30 candidates with cross-target conflict resolution (`V1-FOLD-CANDIDATES.md:23-39`). Once V1-FOLD-CANDIDATES landed and Phase 7.1/7.2/7.5 absorbed the 30 candidates, the audits' forward role collapsed: zero citations in V7 + V8 hardening reports; ARCH/MIGRATION/MASTER-PLAN/locks do not cite them.

The audits retain provenance value: they are the per-cohort detail behind V1-FOLD-CANDIDATES tier classifications. If a tier-1 fold is ever questioned (e.g., "why does V1 fold DK13 higher-rank when the orig V6 baseline deferred?"), the answer lives at `deferral-audit-1-type-system.md:264-288` (Tier 1 fold rationale) + `V1-FOLD-CANDIDATES.md:30-31` (resolution row 2) + Phase 7.1 lock amendment commit. The audits ground the synthesis in primary-cohort detail.

## §5 — V1-FOLD-CANDIDATES + INDEX disposition

| File | Lines | Status | Citation footprint | Disposition |
|---|---:|---|---|---|
| `V1-FOLD-CANDIDATES.md` | 221 | Live (Phase 7 contract) | V8 hardening 3 reports; ARCH 3 sites (`ARCHITECTURE.md:940`, `:1585`, `:1642`); Lock 8 (`14-LOCKS.md:48` "Tier 4 wave-count drop"); HANDOFF Live row | **EXPLICATE** |
| `INDEX.md` | 183 | Live (research catalogue) | RESEARCH-FOLD-ORCHESTRATOR §3.4 + per-topic dispatch; HANDOFF Live row; V6 hardening Phase-2 routing | **EXPLICATE** |

**V1-FOLD-CANDIDATES disposition — EXPLICATE.** The 30-item synthesis carries forward as the Phase 7 contract; ARCHITECTURE retires regex-automata oracle "per V1-FOLD-CANDIDATES Tier 3 #23" three times; Lock 8 binds H tranche wave count to "the V1-FOLD-CANDIDATES Tier 4 wave-count drop"; V8 simplification carries cite Tier 3 deferrals (#24 CHR-improvement, #25 function composition) verbatim. The artefact is load-bearing; it does not need updating but does need explication as the binding contract for Phase 7 absorption verification.

**INDEX.md disposition — EXPLICATE.** The catalogue defines the 8 topics + verified-source-slot hygiene + per-topic anchor lock. RESEARCH-FOLD-ORCHESTRATOR cites it; HANDOFF marks it Live. The §2.1 source-slot table is the canonical authority for "is this citation primary-source-verified or provenance-gap?" — provenance-gap entries (Hubbard, Almomany, Deb 2014, Ungar/Adams, HelpMate) are what the topic artefacts route to non-evidence status. Pruning INDEX would break that authority chain.

INDEX could merit a content UPDATE (mark Phase 5 + Phase 7 cycles as closed, point to V1-FOLD-CANDIDATES as the active contract) but this is non-blocking — the index reads cleanly as Phase-5 dispatch authority and the absorbed-topic status is verifiable from `HARDENING-CONSOLIDATED-V6.md:§3` + V8 carry tables.

## §6 — V2-deferral occurrence ledger

The user retires V2 deferrals. Search across `restart/research/` for "V2 amendment", "post-V1 amendment", "deferred V2", "post-V1 fork", "post-V1 deferral" patterns:

| Path:line | Phrase | Classification |
|---|---|---|
| `V1-FOLD-CANDIDATES.md:5` | "fold post-V1 deferrals into V1 where the architecture-nailing value justifies V1 cost" | **acceptable-as-historical-record** — describes the user mandate that motivated Phase 7 |
| `V1-FOLD-CANDIDATES.md:37` | "TS-native runtime as principled (not pragmatic) post-V1 fork" | **load-bearing** — confirmed conflict resolution row 9; `path-ts` ships J.W3, TS-native deferred. Per Lock 5 amendment this is V1-correct (not retired by user). |
| `V1-FOLD-CANDIDATES.md:204` | "DK13 higher-rank — fold V1 (audit #1 recommendation) or defer post-V1 (audit #6 V6-baseline reading)?" | **acceptable-as-historical-record** — open question already resolved (FOLD V1 per Phase 7.1) |
| `deferral-audit-1-type-system.md:32` | "explicit deferral language (post-V1, future amendment, reserved, out of scope)" | **acceptable-as-historical-record** — describes audit method, not a live deferral |
| `deferral-audit-1-type-system.md:377` | "post-V1, the algorithm absorbs it without re-architect" | **acceptable-as-historical-record** — sealed input prose |
| `deferral-audit-3-bbnf-surface-directives.md:40` | "Receiver: post-V1 amendment if higher-rank surface lands" (function-type productions) | **historical** (resolved — Phase 7 folded function types V1 per `V1-FOLD-CANDIDATES.md:48`) |
| `deferral-audit-3-bbnf-surface-directives.md:43` | "Receiver: post-V1 amendment if the visitor surface proves insufficient" (`match` expression) | **historical** (resolved — `match` folded V1 per V1-FOLD-CANDIDATES Tier 2 item 19) |
| `deferral-audit-3-bbnf-surface-directives.md:56` | "Deferrals above are not waiting on tranches — they are post-V1 amendment gates" | **historical** (sealed-input prose; subsequently reversed) |
| `deferral-audit-3-bbnf-surface-directives.md:268` | "The cookbook itself is post-V1 doc work" | **historical** (sealed-input prose) |
| `deferral-audit-4-sibling-crates.md:170` | "Document `egglog::compat::Bridge = future research target` so post-V1 reopen has a pointer" | **load-bearing** — egglog fusion correctly remains a post-V1 research comparison per `INDEX.md:49`, Lock 4, Topic 4 §6 A2 |
| `deferral-audit-4-sibling-crates.md:302` | "reopen-pointer hygiene for post-V1 work" | **historical** (sealed-input prose) |
| `deferral-audit-5-runtime-pass3.md:32` | "Item is post-V1, receiver and blocker named, gate routed" (CARRY-OK definition) | **acceptable-as-historical-record** — audit method classifier |
| `deferral-audit-5-runtime-pass3.md:49` | "Streaming parsing ... Carry to post-V1 if user-mandated" | **load-bearing** — streaming forks Lock 9 lifetime story; correctly post-V1 |
| `deferral-audit-5-runtime-pass3.md:101` | "runtime parity with Rust is post-V1 architectural work" | **load-bearing** — TS-native runtime fork; same as `V1-FOLD-CANDIDATES.md:37` |
| `deferral-audit-5-runtime-pass3.md:199` | "DK-style higher-rank is explicitly post-V1 per `restart/ARCHITECTURE.md:1161-1166`" | **historical** (Phase 7.1 reversed; DK13 folded V1 per V1-FOLD-CANDIDATES Tier 1 #2) |
| `deferral-audit-5-runtime-pass3.md:258` | "runtime is post-V1 architectural fork, not pragmatic deferral" | **load-bearing** — TS-native runtime |
| `deferral-audit-6-codegen-pass2.md:203` | "Profile-guided specialization ... DEFER post-V1" | **load-bearing** — PGO requires runtime feedback loop absent V1; correctly post-V1 |
| `deferral-audit-6-codegen-pass2.md:364` | "If post-V1 work introduces general fn ..." | **historical** (sealed-input prose) |
| `deferral-audit-6-codegen-pass2.md:419` | "Profile-guided specialization ... DEFER post-V1" | **load-bearing** — same as :203 |
| `deferral-audit-7-locks-architecture.md:8` | "the cost of moving lock text post-V1" | **acceptable-as-historical-record** — audit method |
| `deferral-audit-7-locks-architecture.md:33` | "post-V1 unless folded; MEDIUM = lock text holds but downstream specs migrate" | **acceptable-as-historical-record** — audit classifier |
| `deferral-audit-7-locks-architecture.md:45` | "Lock 1 carries no post-V1 hook" | **acceptable-as-historical-record** |
| `deferral-audit-7-locks-architecture.md:81` | "fusion remains a post-V1 research comparison" (egglog) | **load-bearing** — Lock 4 egglog rationale per Topic 4 §6 A2 |
| `deferral-audit-7-locks-architecture.md:105` | "removing the post-V1 ..." | **historical** (sealed-input prose; Lock 5 amendment per Phase 7.1) |
| `deferral-audit-7-locks-architecture.md:184` | "A staged-publication post-V1 hook" (Lock 11) | **load-bearing** — Lock 11 publication gate |
| `deferral-audit-7-locks-architecture.md:252,254,264,298,309,371,376,394,404,410` | various post-V1 references | **load-bearing or historical** — mostly audit ledger rows |
| `topic-4-egraphs.md:758` | "egglog-style fusion remains a post-V1 research comparison, not V1 architecture" | **load-bearing** — Topic 4 §6 A2 surgery (folded into Lock 4 explanation per `fold-synthesis.md:281-282`) |

**Ledger summary**: 23 occurrences of post-V1 / V2 phrasing across the research dir. Every load-bearing occurrence has a verifiable receiver — `path-ts` deferral (Lock 5), egglog fusion (Lock 4), streaming (Lock 9), PGO (no V1 runtime feedback loop), TS-native runtime (architectural fork). Every historical occurrence is sealed-input prose superseded by Phase 7 fold (most prominently DK13 fold-V1 reverses two `deferral-audit-5:199` post-V1 references). **Zero occurrences require Phase 8.4+ surgery to retire** — they are either load-bearing-correct or sealed-input-historical.

**No load-bearing-research-finding-must-fold-V1 occurrences detected.** The user's V2-deferral-retirement instruction surfaces no work in the research dir.

## §7 — Pruning recommendation

Three paths surveyed:

| Path | Files dropped | Bytes dropped | Risk |
|---|---:|---:|---|
| **AGGRESSIVE** | 4 fold reports + 7 of 8 deferral audits (keep audit #4 as exemplar) | ~10 files / ~300K | Loses V6 verification trail (fold reports) + per-cohort Phase 7 detail (audits 1-3, 5-8) |
| **MINIMAL** | 4 fold reports only | 4 files / ~80K | Loses V6 verification trail |
| **NONE** | zero | 0 | None |

**Recommendation: NONE.**

Rationale:

1. **Fold reports retain V6 verification value**. `HARDENING-PASS-1-V6.md:79-97` cites specific fold-pass-1.md lines as the discriminator for "topic-X finding routed away from PASS-Y" claims. Pruning the reports orphans those citations. V6 was never re-run; V7/V8 verified V6 carries forward without re-touching the trail. The reports are sealed but their cited-line stability is load-bearing.

2. **Deferral audits retain per-cohort provenance value**. V1-FOLD-CANDIDATES synthesises 30 items across 4 tiers but compresses cohort detail (5,800 LOC across 8 audits → 221 LOC synthesis). When a Tier 1 fold is questioned (Phase 8.4+ or per-tranche drafting), the answer requires per-cohort detail (e.g., `deferral-audit-1:264-288` for DK13 fold rationale, `deferral-audit-2:336-356` for function-value V1 fold rationale). Pruning the cohort orphans the synthesis.

3. **The cohort is sealed**. None of the 22 files require update; none carry V2 deferrals that retire under user mandate (per §6); no §6 finding remains unrouted. The corpus does not gain by trimming what is already settled — it gains by leaving the trail intact for the next hardening or per-tranche audit cycle.

4. **Cost of retention is low**. ~840K of static markdown carries zero forward maintenance burden; the only forward-maintenance file is V1-FOLD-CANDIDATES itself, which is Live.

**MINIMAL is the secondary recommendation** if disk pressure or repository-size hygiene is invoked: fold reports drop ~80K and break only V6 verification (V6 itself remains Live as a hardening predecessor but is not the operating baseline). The user's KISS-DRY discipline (`feedback_system_cohesion`) could plausibly motivate MINIMAL — Phase 5 fold records are subsumed by Phase 7 V1-FOLD-CANDIDATES in the operating baseline.

**AGGRESSIVE is rejected**. The 8 deferral audits underwrite V1-FOLD-CANDIDATES; trimming 7 of 8 leaves the synthesis hovering above its evidence and would break the per-cohort traceability for any future tier-questioning.

## §8 — Open questions

These route to the synthesis cohort coordinator (sibling corpus-audit reports landing concurrently):

1. **Should V1-FOLD-CANDIDATES carry an "absorption-status" appendix?** The 30 items have all landed (Phase 7.1 + 7.2 + 7.5 + V8 carries); an appendix `V1-FOLD-CANDIDATES.md §11 Absorption Ledger` mapping each item to its absorption commit + verifying hardening report would make the artefact self-contained vs. requiring HANDOFF + V8 cross-references. Disposition: **UPDATE candidate** if the synthesis cohort recommends it.

2. **Should INDEX.md gain a Phase-status header?** "Phase 5 dispatch authority; Phase 7 inputs subsume Phase 5 fold artefacts; per-topic absorption verified at HARDENING-CONSOLIDATED-V6.md §3 + V8 carries." Currently the reader infers this from HANDOFF; the index could state it directly. Disposition: **non-blocking; UPDATE if the corpus-audit cohort recommends**.

3. **Should the 22 research artefacts move to a sealed sub-directory** (e.g., `restart/research/sealed-phase-5/` for topics + folds; `restart/research/sealed-phase-7/` for deferral audits)? This makes the live vs sealed distinction physical rather than HANDOFF-table-only. Trade-off: rename ledger spans 50+ external citations (fold-pass-* in V6 reports + topic-* in V6 reports). **Recommend rejecting** — the static-cite-line stability is more valuable than directory hygiene.

4. **Does V1-FOLD-CANDIDATES.md §9 "Open questions for synthesis" still represent unresolved work?** Items 1, 2, 3, 4, 6, 8 were resolved at Phase 7.1; items 5, 7 are H.W3/J.W3-receiver-confirmed. Disposition: **non-blocking; the §9 questions are sealed alongside the document**. An UPDATE could add "RESOLVED: ..." annotations but this gilds the lily.

5. **Is the AGGRESSIVE path ever justified?** Only under repo-size pressure. The bbnf-lang corpus is markdown-and-source; ~840K is negligible vs. generated bbnf assets. The AGGRESSIVE path's only motivation is ideological (KISS) and the cost (orphaned synthesis evidence) is not worth it.

## §9 — Voice + discipline locks

Per `restart/README.md` §13. Calibrated, direct, archaic-permissive. Path:line citations on every concrete claim. Tables liberal where they serve the audit. Per-file disposition tables for the 8 topics + 4 folds + 8 audits. No metalanguage; no "the prompt asked"; no filler.

## §10 — Closing posture

The research dir carries 10,486 lines of SOTA grounding across 22 artefacts in five strata. Two are Live (V1-FOLD-CANDIDATES, INDEX); twenty are sealed evidence trails (8 topic deep-dives + 4 fold reports + 8 deferral audits). Citation footprint verifies the strata: V1-FOLD-CANDIDATES alone carries forward through V8 + ARCH + Lock 8; topics + folds peak at V6 verification; deferral audits compress into V1-FOLD-CANDIDATES at Phase 7.

Recommended pruning path: **NONE**. The cohort is settled; pruning either fold reports or deferral audits orphans verification or evidence trails. MINIMAL stands as secondary if disk hygiene is invoked (drops 4 files / ~80K of fold-pass records). AGGRESSIVE is rejected.

V2-deferral occurrences total 23 across the dir; every load-bearing occurrence has a verifiable receiver (Lock 4 egglog, Lock 5 path-ts, Lock 9 streaming, no-V1-runtime PGO, TS-native architectural fork). Zero occurrences require Phase 8.4+ surgery; the user's V2-deferral retirement instruction surfaces no work in research/.

Two non-blocking UPDATE candidates: V1-FOLD-CANDIDATES absorption-ledger appendix + INDEX phase-status header. Both await synthesis-cohort coordination with the three sibling corpus-audit reports landing concurrently.
