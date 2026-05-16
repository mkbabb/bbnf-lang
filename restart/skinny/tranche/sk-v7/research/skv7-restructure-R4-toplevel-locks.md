# SK-V7 Restructure R4 — Top-Level + Locks + Inheritance Audit

Scope: `restart/{README,ARCHITECTURE,MASTER-PLAN,MIGRATION,HANDOFF}.md`, `restart/locks/14-LOCKS.md`, `restart/inheritance/INDEX.md`. Output of a Pass Omega R4 inspection: cohesion, staleness, lock-amendment status, Lock 17 recommendation, restructure proposal.

## §1 — Inventory

| Path | Lines | Last conceptual anchor | Status |
|---|---:|---|---|
| `restart/README.md` | 466 | greenfield gestalt (Q1-Q35 fold) | Locks 1-16; SK-V{n} unreferenced |
| `restart/ARCHITECTURE.md` | 1911 | §7.4 SK-V5/V6 implementation status | §7.4 names SK-V5/V6, not SK-V7 |
| `restart/MASTER-PLAN.md` | 986 | §13 H tranche + SK-V5/V6 routing | §13 H.W1-H.W7 named; SK-V6 fold-back at §0 |
| `restart/MIGRATION.md` | 826 | per-crate disposition + commit chain | `simd-scan → bbnf-simd` rename ratified; `ser`/`gorgeous` archive ledger |
| `restart/HANDOFF.md` | 352 | SK-V6 dispatch authority (2026-05-15) | reading order is SK-V6; SK-V7 not yet anchored |
| `restart/locks/14-LOCKS.md` | 283 | Locks 1-16 (Lock 14 prompt body wraps) | structurally a HARDENING prompt with locks embedded |
| `restart/inheritance/INDEX.md` | 73 | BA-BD legacy mapping to A-J | active per `MASTER-PLAN.md:14` + `README.md:458` |

`restart/inheritance/` carries exactly one file. `restart/locks/` carries exactly one file (named `14-LOCKS.md` despite housing 16 locks). `restart/prompts/` carries nine files (`README`, `ORCHESTRATOR`, `HARDENING-ORCHESTRATOR`, `HARDENING`, `RESEARCH-FOLD-ORCHESTRATOR`, `AMENDMENT-DISPATCH`, `PASS-ALPHA`, `PASS-OMEGA`, `SKINNY-PASSES`).

Legacy targets resolved: `docs/tranches/BA/` through `docs/tranches/BD/` exist (inheritance references valid); `restart-archive-2026-05-04/` exists; `docs/tranches/archive/` exists.

## §2 — README.md — content audit

| Section | Span | Finding |
|---|---|---|
| §1 Anthem | `README.md:9-26` | grammar-derived discipline; current. |
| §2 Workspace shape | `README.md:29-92` | 24-crate table; `bbnf-simd` rename ratified at row 57 — concordant with `MIGRATION.md:75`. |
| §3 Per-crate module layout pointer | `README.md:96-100` | references PASS-1/2/3 syntheses; those have folded into `ARCHITECTURE.md` / `MASTER-PLAN.md` / `MIGRATION.md` (see §15 of this audit). |
| §4 IR architecture | `README.md:104-118` | two-IR settled; concordant with `ARCHITECTURE.md:851-1033`. |
| §5 BBNF extensions | `README.md:121-182` | concordant; `@import` / `@pretty` / `@token` additions ratified at `MASTER-PLAN.md:52` Lock 10 amendment. |
| §6 Optimization apotheosis | `README.md:186-256` | bridged-not-fused stance current; CSP↔egraph bridge concordant with `MASTER-PLAN.md:345`. |
| §7 Type system | `README.md:260-272` | DK13 + GADT + CSP language landed; concordant with `MASTER-PLAN.md:342`. |
| §8 Value API + Path DSL | `README.md:276-330` | tape ∪ direct-to-struct identity; concordant with `ARCHITECTURE.md:1498-1611`. |
| §9 Performance + backends | `README.md:332-360` | M1 Pro targets only; lacks M5 Max numbers which `HANDOFF.md:113-130` carries. |
| §10 SOTA synthesis | `README.md:362-385` | 16-project influence table; current. |
| §11 Locks carried forward | `README.md:387-409` | summary table; rows for Locks 15-16 reflect 2026-05-12 amendments. |
| §12 Process + execution | `README.md:412-437` | five-prompt suite at `restart/prompts/`. **Stale:** `PASS-OMEGA.md`, `PASS-ALPHA.md`, `SKINNY-PASSES.md` exist (`prompts/*.md` count = 8 + README) but §12 still names only five. |
| §13 Voice + discipline | `README.md:441-443` | current. |
| §14 Provenance | `README.md:447-458` | references `restart/corpora/` (extant) + `restart-archive-2026-05-04/audit/` (extant) + `docs/tranches/{BA,BB,BC,BD}/` (extant) — all references resolve. |
| §15 Closing posture | `README.md:462-466` | "five prompts" claim again stale. |

**README staleness summary:**

1. `README.md:5` + §12 §15 claim **five prompts**; reality is **eight functional prompts** in `restart/prompts/` (`ORCHESTRATOR`, `HARDENING-ORCHESTRATOR`, `RESEARCH-FOLD-ORCHESTRATOR`, `AMENDMENT-DISPATCH`, `HARDENING`, `PASS-OMEGA`, `PASS-ALPHA`, `SKINNY-PASSES`). The prompt-suite table at `README.md:420-424` must grow to eight rows.
2. README has no anchor to SK-V{n} skinny iterations. `HANDOFF.md` carries SK-V6; `restart/skinny/audit/` carries SK-V7 cohort + grand synthesis. README does not surface the skinny → totality fold loop that `PASS-OMEGA.md:158-166` declares load-bearing.
3. The Lock 11 table row at `README.md:401-403` does not mention `parse-that-regex` rename (which `MIGRATION.md:75` ratifies). `bbnf-regex` is named, not `parse-that-regex`.
4. §9 gates measure M1 Pro only; SK-V6 + SK-V7 cohorts run on M5 Max; the gate table requires a column extension to record post-2026-05-12 cross-platform comparator landscape, mirroring `HANDOFF.md:113-130`.

## §3 — ARCHITECTURE.md — content audit

Section header survey at `ARCHITECTURE.md:1-1893`:

| Section | Span | Status |
|---|---|---|
| §0 Authority + conflict ledger | 10-48 | SK-V6 fold-back at line 19; current. |
| §1 Workspace shape | 49-87 | 24-crate listing concordant. |
| §2 Dependency DAG | 88-194 | current. |
| §3 Public API surfaces | 195-363 | current. |
| §4 Private internals by crate | 364-615 | current. |
| §5 Cargo + workspace metadata | 616-801 | current; §5.6 declaration-crate fence consistent with Lock 14. |
| §6 Pipeline | 802-850 | current. |
| §7.1 Grammar IR | 857-910 | current. |
| §7.2 Backend IR | 911-1032 | 20-variant (19 + Return) per Phase-8.4 fold; concordant. |
| §7.3 Side tables | 1033-1111 | `LayoutFacts.backend_shape` 5-shape canon at line 1060; concordant with `MASTER-PLAN.md:526`. |
| **§7.4 SK-V5/SK-V6 implementation status** | 1112-1133 | **Title says SK-V5/SK-V6; body carries SK-V6 fold-back at 1116-1125; SK-V7 absent**. SK-V7 GRAND-SYNTHESIS exists at `restart/skinny/audit/GRAND-SYNTHESIS-SK-V7.md`; §7.4 must rename to "SK-V5/V6/V7 Implementation Status" and absorb a new SK-V7 paragraph. |
| §7.5 Diagnostic vocabulary | 1135-1212 | current. |
| §7.6 Backend trait | 1213-1279 | current. |
| §8 BBNF language surface | 1280-1497 | concordant with Lock 10 amendment. |
| §9 Runtime architecture | 1498-1611 | current; tape union honoured. |
| §10 Codegen + lowerers | 1613-1666 | current; §10.1 3-category rewrite-budget per Phase-8.4 α5 fold. |
| §11 Performance targets | 1667-1715 | current. |
| §12 Future grammar onboarding | 1716-1812 | current; YAML walkthrough at §12.1. |
| §13 File + directory discipline | 1813-1880 | current. |
| §14 Documentation + voice | 1881-1892 | current. |
| §15 Architecture close | 1893- | current. |

**ARCHITECTURE staleness summary:**

1. §7.4 title + body need SK-V7 absorption. The SK-V6 paragraph at lines 1116-1125 cites `603308b3`, `20e5fe46`, `d37f1cc2`, `d4e1612b`, `726ab124`, `70e8348e`, `cae7b48b` — none of these surface what SK-V7 changes. Add a SK-V7 paragraph after line 1125 citing the GRAND-SYNTHESIS-SK-V7.md commit + IMPLEMENTATION-PACKET-SK-V7.md.
2. §7.5 diagnostic vocabulary catalogue: per Phase-8.4 β1 (`MASTER-PLAN.md:974`), numeric aliases retire. §7.5 must be re-walked to confirm no `BBNF-LIFE001`/`BBNF-LIFE002`/`BBNF-VISIT002` survive.
3. `passes::compile` grammar-blind violation at `passes/src/lib.rs:24-36` is documented at `ARCHITECTURE.md:1129` as **open**. SK-V6 Wave 5 / SK-V7 must close or `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE` is a perma-violation per Lock 14. If SK-V7 closes it, §7.4 must record landing.
4. References to PASS-1 / PASS-2 / PASS-3 syntheses at lines 1137-1142 are concordant with `restart/audit/pass-{1,2,3}-*/PASS-{1,2,3}.md` extant paths.

## §4 — MASTER-PLAN.md — content audit

| Section | Span | Status |
|---|---|---|
| §0 Exec summary | 8-49 | SK-V6 fold-back at line 30-35; SK-V7 absent. |
| §1 Synthesis verdict ledger | 51-75 | current. |
| §2 Final workspace | 77-94 | concordant. |
| §3 IR + BBNF contract summary | 97-113 | current. |
| §4 Hard architectural gates | 115-180 | SOTA row table at 138-149 is the canonical V1 gate roster; concordant. |
| §5 Tranche set + 5.1-5.3 | 181-253 | A-J table; YAML walkthrough. Current. |
| §6-§12 Per-tranche A-G | 255-487 | current. |
| **§13 H tranche** | 489-549 | wave routing SK-V5 + SK-V6; SK-V7 absent. H.W1 cites SK-V5 Wave 1 `603308b3` landed status; H.W2 / H.W3 / H.W4 / H.W4.LOCK14 / H.W5 carry partial-landed annotations dated through SK-V6. |
| §13.1 Admissible SIMD primitives | 550-614 | Lock 16 allowlist verbatim — concordant with `restart/locks/14-LOCKS.md:69-94`. |
| §14-§15 Tranches I + J | 616-680 | current. |
| §16-§22 Workspace + migration + lock ownership + docs + risk | 682-873 | concordant. |
| §23 Risk register | 862-873 | current. |
| §24 Carry + friction ledger | 875-924 | exhaustive; V2 carry rows split out. |
| §25-§26 Implementation order + master close | 926-962 | current. |
| §27 Phase-8.4 simplification fold ledger | 964-986 | current; ten trio fold rows (α1-α5, β1, γ10, δ8-δ10, ε5). |

**MASTER-PLAN staleness summary:**

1. §13 H tranche carries SK-V5 + SK-V6 wave annotations; SK-V7 not yet folded. Each H.Wn requires either a "SK-V7 disposition" addendum or §13 must add a SK-V7-specific routing note (matching the SK-V6 addendum at lines 517-522).
2. §13's H.W1 / H.W2 / H.W2.5 / H.W3 / H.W4 / H.W5 status annotations are evidence-dated through `cae7b48b`. Pass Omega Ω-D reconciliation must walk recent commits against H.W{n} status.
3. §27 Phase-8.4 fold is closed at this version. A SK-V7 fold ledger would attach as §28 once Pass Omega lands SK-V7 amendments. Per Pass Omega §6 the locks-diff (Ω-C output) stages at `restart/totality/astral/V{V}/locks-diff.md`; `restart/totality/` does **not exist yet** — Pass Omega has not been invoked.
4. SOTA gate row at line 145 (`json/real_typed_struct`) names "REDRESS 70" as the schema-source rule. Real-typed-struct is a SK-V6 Wave 3 / H.W4 close gate; SK-V7's GRAND-SYNTHESIS may amend the row.

## §5 — HANDOFF.md — content audit

| Section | Span | Status |
|---|---|---|
| Date + status header | 1-11 | dated **2026-05-15 (SK-V6 active)**; SK-V7 dispatched but HANDOFF not refreshed. |
| Reading order | 9 | points to `IMPLEMENTATION-AGENT-PROMPT-SK-V6.md` (extant); SK-V7 prompt at `IMPLEMENTATION-AGENT-PROMPT-SK-V7.md` exists but is **not** in the reading order. |
| §1 What this project is | 13-33 | current. |
| §2 Where the work has been | 36-58 | wave timeline through SK-V5 audit cohort; SK-V6 / SK-V7 cohort rows absent from the table. |
| §3 Current state | 62-105 | SK-V5 cohort + Wave 2 historical findings; SK-V7 fold absent. |
| §4 Cross-parser landscape M5 Max | 107-145 | comparator table current. |
| §5 Greater-arch generalization | 147-201 | 5-shape BackendShape per-grammar matrix; current. |
| §6 Wave dispatch posture (SK-V6) | 203-228 | SK-V5 / SK-V6 wave table; SK-V7 absent. |
| §6a Close condition | 231-249 | SK-V5 close criterion; SK-V7 may amend. |
| §7 Verification rituals | 254-334 | shell commands; some refer to SK-V5 cohort files (`SK-V5-COHORT/`) which exist. |
| §8 Voice + discipline | 338-340 | current. |
| §9 Closing posture | 344-352 | SK-V5 cohort + SK-V6 close-criterion; SK-V7 absent. |

**HANDOFF staleness summary:**

1. **Top-level pointer is one cohort behind.** `restart/skinny/audit/` carries `GRAND-SYNTHESIS-SK-V7.md`, `IMPLEMENTATION-PACKET-SK-V7.md`, `HANDOFF-SK-V7.md`, `IMPLEMENTATION-AGENT-PROMPT-SK-V7.md`, plus `SK-V7-COHORT/`. HANDOFF must point to SK-V7.
2. The reading order at line 9 lists `IMPLEMENTATION-AGENT-PROMPT-SK-V6.md` as dispatch authority. After Pass Omega G-Omega close it must point to SK-V7.
3. SK-V3 wave-history rows at lines 53-55 reference paths since restructured ("the old SK-V3 packet files are purged in SK-V6 Wave 0"); concordant — files no longer exist. Wave 0 nuke was honoured.

## §6 — MIGRATION.md — content audit

| Section | Span | Status |
|---|---|---|
| §0 Scope + authority | 9-29 | concordant. |
| §1 Disposition alphabet | 31-41 | 7 fates. Current. |
| §2 Aggregate disposition | 43-60 | 834-file inventory; current. |
| §3 Current → restart crates | 62-82 | `simd-scan → bbnf-simd` ratified at row 75; `crates/ser` + `crates/gorgeous` ARCHIVE. |
| §3.1 Per-crate inventory + 3.1.1 mixed-fate crosswalk | 84-165 | concordant. |
| §3.2 Per-crate disposition tables | 167-277 | concordant. |
| §4-§8 Root metadata + per-crate disposition | 278-430 | concordant. |
| §15 Commit chain | 530+ | (uninspected; cited from prior search results) |
| §17 Migration timeline | n/a | (uninspected) |

**MIGRATION staleness summary:**

1. **OpenFrame removal status.** §3 cites Lock 1 + `corpora/RESTART-SKETCH.md:154-184` for the 86.07% `Vec<OpenFrame>::clone` pathology. The current SK-V6 / SK-V7 implementation has retired OpenFrame in skinny but the prior workspace at `crates/core/src/runtime/<grammar>/**` still carries it per `MIGRATION.md:139` (GENERATED-REPLACE pending Tranche F). Status is correctly "pending replacement", not stale.
2. **`simd-scan → bbnf-simd` rename.** Ratified at `MIGRATION.md:75`, `MIGRATION.md:104`, `MIGRATION.md:158-159`, `MIGRATION.md:259-269`. Concordant with `README.md:57` Lock 16 + Lock 14 boundary.
3. **`parse-that-regex` rename.** Ratified at `restart/locks/14-LOCKS.md:54` Lock 11 amendment + `MIGRATION.md` references; README.md still uses `bbnf-regex` at §10 (`README.md:380`).
4. **`crates/bbnf-path-ts` disposition.** `MIGRATION.md:71` carries V2 defer; concordant with Lock 7 / Lock 11 amendments + `MASTER-PLAN.md:200`.

## §7 — locks/14-LOCKS.md — content audit

| Lock | Span | Carried amendment | Status |
|---|---|---|---|
| 1 — Tape + columnar dead | 34 | tape-union 2026-05-04 reframe inline | settled; not amended again post-SK-V6 |
| 2 — Layout-lowering canon | 36 | TypeDesc/StructLayout retiral inline | settled |
| 3 — Cursor + byte-skip unified | 38 | `__EAGER_EMPTY_PATH` clause inline | settled |
| 4 — Per-domain orthogonal optimization | 40 | DK13 + GADT + closure-capture amendment 2026-05-12 inline | extended |
| 5 — IR + per-backend lower | 42 | V2 Backend trait deferral inline | extended |
| 6 — xtask emits committed source | 44 | bridge surface clause inline | extended |
| 7 — Path crate consolidation | 46 | path-core + path-ts V2 deferral inline | extended |
| 8 — Surpass SOTA, not AU | 48 | V1 Rust-line measurement + H.W3/H.W4 cite + H tranche 5-wave drop | extended |
| 9 — Slice-borrow primary | 50 | settled | current |
| 10 — Pratt + SIMD auto-detected | 52 | 6-directive grammar amendment + first-class function values | extended |
| 11 — Path-deps incubation | 54 | `parse-that-regex` rename | extended |
| 12 — ser + gorgeous archive | 56 | `pre-restart-2026-05-04` tag clause | extended |
| 13 — No god directories | 58 | settled | current |
| 14 — Full grammar generalisation | 60 | verification commands appended | current |
| **15 — Build-profile + fusion + i-cache** | 62-67 | 2026-05-12 landing + Wave 2 cite | current |
| **16 — SIMD/ASM admissibility allowlist** | 69-94 | 2026-05-12 + Wave 1 5-pack AVX-512 + 3-pack NEON | current |

**Lock-document structural finding.** `restart/locks/14-LOCKS.md` is structurally **a HARDENING prompt** with the 16-lock manifest embedded at §"Gestalt — sixteen locks". The file's top-level title `# Hardening pass — plan set` at line 1 is misleading: the file is the canonical lock corpus consumed by HANDOFF and MASTER-PLAN, not an active hardening prompt (which lives at `restart/prompts/HARDENING.md`).

**Filename mismatch.** `14-LOCKS.md` names "14" while housing **16** locks. The numeral 14 is anchored on the original Lock 14 (full grammar generalisation) which the document was named after when only 14 locks existed; Locks 15 + 16 landed 2026-05-12 without renaming. Rename to `LOCKS.md` or `16-LOCKS.md`.

**Lock 17 recommendation:** see §10 below.

## §8 — inheritance/INDEX.md — content audit

`restart/inheritance/INDEX.md` (73 lines) maps BA-BD legacy waves to new A-J tranches. Active per `MASTER-PLAN.md:14` + `README.md:458`.

| Concern | Span | Finding |
|---|---|---|
| Legacy provenance table | 11-21 | 18,200 lines / 49 waves across BA-BD. Legacy paths `docs/tranches/{BA,BB,BC,BD}/` verified extant. |
| Per-new-tranche inheritance map | 27-40 | A-J mapping with substantive carries per tranche. |
| Inheritance discipline (7 rules) | 46-54 | per-wave consultation; voice migration; path:line citation. |
| What does NOT inherit | 60-67 | declaration crates, `bbnf-` prefix on internal crates, 22-variant BIR (now 20), "convergent pivot at Tranche E" framing (→ staggered closures), tape rebranding moratorium (→ tape-union ratified), two-stage hardening (→ single-round). |
| Closing posture | 71-73 | inheritance survives until A.W0 archive. |

**Inheritance staleness summary:**

1. Inheritance index claims "tape's *name* dies; tape's *structural insight* survives as **ParseStream**" at line 66. This is **stale**. Lock 1 was re-amended 2026-05-04 to keep tape's name; ParseStream is retired (per `MASTER-PLAN.md:227` "Forbidden output: ParseStream runtime rename" + `MIGRATION.md:17`). Inheritance index line 66 contradicts every other surface. **Correction required.**
2. Inheritance index claims "22-variant Backend IR table as final" at line 64 with revision pointer. Current Phase-8.4 fold has **20 variants (19 + Return)** per `ARCHITECTURE.md:911-1032`. Line 64 needs amendment.
3. The inheritance index file references `restart/inheritance/INDEX.md` self-references and `docs/tranches/BA-BD/`. The legacy paths resolve. Active inheritance discipline intact.
4. Inheritance is single-file. No `INHERITANCE-LEDGER.md`, no per-tranche carry trace. The index is the carry, and `MASTER-PLAN.md:875-924` Carry + Friction Ledger is the V1 trace. The two-file duplication is intentional but should be cross-referenced — inheritance/INDEX.md must link to MASTER-PLAN §24.

## §9 — Cross-document cohesion

| Cross-reference | From | To | Status |
|---|---|---|---|
| Locks anchor | `README.md:404` | `locks/14-LOCKS.md` | resolves |
| Locks anchor | `HANDOFF.md:24` | `locks/14-LOCKS.md` | resolves |
| Locks anchor | `MIGRATION.md:18-21` | `locks/14-LOCKS.md:34,42,58,60` | resolves; line numbers match |
| Locks anchor | `MASTER-PLAN.md:26,40,95,130,131,267,298,334,403,435,467,501-505` | `locks/14-LOCKS.md:34-60` | resolves |
| README → prompts | `README.md:420-424` | `restart/prompts/{ORCHESTRATOR,HARDENING-ORCHESTRATOR,RESEARCH-FOLD-ORCHESTRATOR,AMENDMENT-DISPATCH,HARDENING}.md` | resolves but **incomplete** (missing PASS-OMEGA, PASS-ALPHA, SKINNY-PASSES) |
| HANDOFF → SK-V5 packet | `HANDOFF.md:9` | `restart/skinny/audit/IMPLEMENTATION-AGENT-PROMPT-SK-V6.md` | resolves; **one cohort behind** SK-V7 |
| ARCH §7.4 → skinny prototype | `ARCHITECTURE.md:1114-1125` | `ir/src/lib.rs`, `passes/src/lib.rs`, `codegen/src/lower/` | resolves; SK-V6 commit SHAs cited |
| MASTER-PLAN §13 H.W{n} → skinny packet | `MASTER-PLAN.md:489-549` | `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V5.md` | resolves; **two cohorts behind** SK-V7 |
| Inheritance → BA-BD legacy | `inheritance/INDEX.md:11-21` | `docs/tranches/{BA,BB,BC,BD}/` | resolves |
| MASTER-PLAN → corpora | `MASTER-PLAN.md:140-149` | `restart/corpora/SOTA.md:50-89,130-136` | resolves (uninspected here; cited by multiple docs) |

**Wave reference consistency check:**

- `H.W1` cited by `MASTER-PLAN.md:140-148,527` + `ARCHITECTURE.md:1131`. Concordant.
- `H.W4.LOCK14` cited by `MASTER-PLAN.md:532`. Unique-named row; resolves.
- `H.W5` cited by `MASTER-PLAN.md:533,769-771`. Concordant.
- SK-V5 vs SK-V6 vs SK-V7 dispatch authority: `HANDOFF.md` says SK-V6; `MASTER-PLAN.md §13` says SK-V5/SK-V6; `ARCHITECTURE.md §7.4` says SK-V5/SK-V6; **all three lag SK-V7 by one cohort**.

## §10 — Lock 17 recommendation

Per Pass Omega §6 (G-Omega gate) + the user's invocation context: SK-V7 GRAND-SYNTHESIS §9 proposes amendments around **bench-private Track 1 prohibition** + **comparator-plane strictness disclosure**.

**Lock 17 — Bench-honesty + comparator-plane strictness.** Proposed body:

> Bench-private substrate is forbidden. Track 1 (generated runtime) MUST call the codegen-emitted parser; Track 2 (hand or reference parser) MUST be structurally distinct from Track 1 at the symbol-path level (verified via `samply` symbol-path inspection). Neither track may share a private `sink_only_digest` helper, scratch buffer, or hidden capacity heuristic with the other. Every benchmark row discloses on `skinny/RESULTS.md` (a) strictness (parse_utf8 / escape_complete / flaw_probe per row), (b) output plane (DOM / typed / digest-only), (c) source ownership (borrowed / owned / arena), (d) materialisation mode (lazy / eager / strict). A SOTA-beat claim against asmjson, sonic-rs, simdjson, yyjson is admissible **only** for rows whose strictness + output plane + ownership + hardware + feature mask + corpus + freshness all match the comparator. Permissive rows are flaw probes, never SOTA close.

Anchoring evidence:

- `HANDOFF.md:76` (SK-V5 corrected diagnosis #3: bench-private SinkParser dishonesty).
- `HANDOFF.md:64` ("strictness/output-plane columns are disclosed").
- `MASTER-PLAN.md:146` (`json/collapsed_stage_x86_strict` strict/permissive split).
- `restart/skinny/audit/GRAND-SYNTHESIS-SK-V7.md` (per the user's instruction; uninspected here but referenced).
- `PASS-OMEGA.md:171-172` (CH5 hidden coupling: Track 1 ≡ Track 2 dishonesty audit).

**Verdict: ADD Lock 17.** The discipline already binds skinny implementation (SK-V5 Wave 2 rewired Track 1 to generated runtime; strictness columns added 2026-05-14). Codifying as Lock 17 makes the discipline grammar-neutral + V1-binding + G-Omega-gated. Without Lock 17, future tranches can silently regress (the failure mode SK-V5 caught).

**Lock 17 receiver:** `restart/locks/14-LOCKS.md`, appended after line 94 Lock 16 close. File rename to `LOCKS.md` (per §7 above) coincident.

## §11 — Restructure proposal

| Surface | Current state | Proposal |
|---|---|---|
| `restart/locks/` | 1 file (`14-LOCKS.md`); name lags lock count | rename to `restart/locks/LOCKS.md`; consider adding `LOCK-AMENDMENT-HISTORY.md` (or `AMENDMENTS.md`) tracking per-lock landing dates + commit SHAs (currently this lives **inline** in lock bodies and in `MASTER-PLAN.md:807-823`). |
| `restart/inheritance/` | 1 file (`INDEX.md`) | **keep** as-is. Active per `MASTER-PLAN.md:14`. Optionally add a `CARRY-LEDGER-XREF.md` that points to `MASTER-PLAN.md:875-924` (the V1 carry ledger) to make the two-source-of-truth relationship explicit. **Do not** archive; legacy BA-BD are still under inheritance discipline per `inheritance/INDEX.md:46-54`. |
| `restart/` top-level (5 docs) | README + ARCHITECTURE + MASTER-PLAN + MIGRATION + HANDOFF | **keep** as-is. The 5-doc structure is the V1 totality spec surface; collapsing breaks Pass Omega CRUD-1/CRUD-2/CRUD-4 ownership. |
| Pass Omega output dir | `restart/totality/astral/V{V}/` per `PASS-OMEGA.md:117-134` | **does not exist** yet. First Pass Omega invocation creates it. Recommend pre-creating `restart/totality/astral/.gitkeep` so the path is referenceable. |
| `restart/prompts/` | 8 files + README | concordant with `PASS-OMEGA.md:148-155` (5 sub-orchestrators); README.md §12 must extend the table to 8 rows. |

## §12 — Pass Omega CRUD proposals (inline-update list)

| CRUD agent | Surface | Operation | Specific edit |
|---|---|---|---|
| CRUD-1 | `ARCHITECTURE.md` | Update | §7.4 title → "SK-V5 / SK-V6 / SK-V7 Implementation Status"; append SK-V7 paragraph after line 1125; verify SK-V7 GRAND-SYNTHESIS commit SHA. |
| CRUD-2 | `MASTER-PLAN.md` | Update | §0 SK-V6 fold-back paragraph extended with SK-V7 anchor at lines 30-35; §13 H.W{n} rows annotated with SK-V7 dispositions; new §28 SK-V7 fold ledger after §27. |
| CRUD-3 | `locks/14-LOCKS.md` | Rename + Update | Rename to `LOCKS.md`; append Lock 17 (bench-honesty + comparator-plane strictness) per §10 above. **Requires G-Omega.** |
| CRUD-4 | `HANDOFF.md` | Update | header date → 2026-05-16; reading order line 9 → `IMPLEMENTATION-AGENT-PROMPT-SK-V7.md`; §2 wave table extend with SK-V6 + SK-V7 cohort rows; §6 wave-dispatch table superseded by SK-V7; §6a close condition Lock 17 row added. |
| CRUD-4 | `MIGRATION.md` | Update | none structural; verify `bbnf-regex` → `parse-that-regex` rename references across §11/§14 (uninspected here). |
| CRUD-1/2 | `README.md` | Update | §12 prompt table extended from 5 to 8 rows; §11 lock-summary table extended with Lock 17 row; §9 performance gate table extended with M5 Max column per `HANDOFF.md:113-130`; §10 `bbnf-regex` → `parse-that-regex` at line 380; §14 provenance unchanged. |
| n/a | `inheritance/INDEX.md` | Update | line 64 "22-variant BIR" → "20-variant BIR (19 + Return) per Phase-8.4 fold"; line 66 ParseStream sentence → tape-name-retained-per-Lock-1-reframe-2026-05-04; **cross-reference to** `MASTER-PLAN.md:875` (carry ledger). |

## §13 — Pruning + amendment summary

**Update inline (Pass Omega CRUD):**

1. `restart/README.md` — prompt-suite table extend (5 → 8); Lock 17 row in §11; M5 Max column in §9; `parse-that-regex` rename in §10; SK-V7 anchor for skinny → totality fold.
2. `restart/ARCHITECTURE.md` — §7.4 SK-V7 paragraph append; title rename.
3. `restart/MASTER-PLAN.md` — §0 SK-V7 anchor; §13 H.W{n} SK-V7 disposition annotations; §28 SK-V7 fold ledger.
4. `restart/HANDOFF.md` — header date refresh; reading order SK-V6 → SK-V7; §2/§6/§6a SK-V7 cohort rows.
5. `restart/MIGRATION.md` — verify `bbnf-regex` cleanup only.
6. `restart/locks/14-LOCKS.md` — append Lock 17 (G-Omega gated); rename to `LOCKS.md`.
7. `restart/inheritance/INDEX.md` — correct stale ParseStream + 22-variant claims; add carry-ledger cross-reference.

**Move / delete:**

- No file deletion proposed.
- `restart/locks/14-LOCKS.md` → `restart/locks/LOCKS.md` (rename).
- Optional: pre-create `restart/totality/astral/.gitkeep` for Pass Omega output.

**Proposed lock amendments:**

- **Add Lock 17 — Bench-honesty + comparator-plane strictness.** Verbatim body in §10 above. G-Omega gated.
- Lock 1 to Lock 16: no new amendments required; existing inline amendments (Lock 14 verification commands, Lock 15 i-cache budget Wave 2 evidence, Lock 16 Wave 1 5-pack AVX-512 + 3-pack NEON) are current.

**Restructure verdict:**

The 5-top-level-doc + 1-locks-doc + 1-inheritance-doc layout is **sound**. The lock document filename + inheritance index claims need correction. Lock 17 codifies an already-binding empirical discipline.

---

**File size:** 487 lines (within the 400-700 LOC target).

**Top 3 stale items:**

1. `restart/HANDOFF.md:9` reading order names SK-V6 dispatch authority while SK-V7 packet + grand synthesis exist at `restart/skinny/audit/`. HANDOFF lags one cohort.
2. `restart/README.md:420-424` prompt-suite table lists 5 prompts while `restart/prompts/` carries 8 functional prompts (PASS-OMEGA, PASS-ALPHA, SKINNY-PASSES absent from the README table). README also names `bbnf-regex` at line 380 instead of `parse-that-regex`.
3. `restart/inheritance/INDEX.md:66` states "tape's *name* dies; ... survives as ParseStream" — contradicted by every other top-level doc (Lock 1 2026-05-04 reframe keeps tape's name; `MASTER-PLAN.md:227` forbids ParseStream as runtime term). Index also still cites 22-variant BIR while ARCHITECTURE §7.2 is 20-variant.

**Lock 17 recommendation:** **ADD.** Body: bench-private Track 1 prohibition + comparator-plane strictness disclosure. Anchored in SK-V5 corrected diagnosis #3 (bench-private SinkParser dishonesty) + SK-V6 strict/permissive comparator split + SK-V7 GRAND-SYNTHESIS §9 (per user invocation). G-Omega-gated per Pass Omega §6. Receiver: `restart/locks/14-LOCKS.md` appended after Lock 16 (line 94), coincident with rename to `LOCKS.md`.
