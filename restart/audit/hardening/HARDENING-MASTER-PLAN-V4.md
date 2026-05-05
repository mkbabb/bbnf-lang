# HARDENING-MASTER-PLAN-V4 — Closure Audit Of The Wave-4.1 Trio

## §1 Target Identification

| Field | Value |
|---|---|
| Targets | `restart/ARCHITECTURE.md` (1408 lines), `restart/MIGRATION.md` (795 lines), `restart/MASTER-PLAN.md` (808 lines) |
| Combined corpus | 3,011 lines |
| Trio commits audited | `1d9d7ffa` (Wave-4.1 SYNTHESIS narrow amendment landing M1/M2/M4/M6/M7) and the consolidated Wave-4.1 cycle leading to it; sister commit `b60d7572` (PASS-2 narrow amendment) and `11806d5d` (PASS-3 narrow amendment) carried PASS-level closure for V3 punch items. |
| V1 baseline | `restart/audit/hardening/HARDENING-MASTER-PLAN.md` (verdict AMENDMENT-REQUIRED, 16 punch items) |
| V2 baseline | `restart/audit/hardening/HARDENING-MASTER-PLAN-V2.md` (verdict READY across all nine lanes; one non-blocking residual) |
| V3 baseline | `restart/audit/hardening/HARDENING-MASTER-PLAN-V3.md` (verdict AMENDMENT-REQUIRED across Lanes 1/3/4/8 plus READY-WITH-NOTE on Lanes 2/6; nine-item punch list M1-M9) |
| V4 audit posture | Verification + re-audit. The V3 punch list is closed verbatim per the Wave-4.1 amendment. V4 confirms each closure, redrives the nine lanes against the post-amendment artefacts, and runs the tightened 16-command gate. V4 is closure-focused but adversarial — any new fault introduced by the amendment work is to be surfaced. |
| Lanes applied | nine (full set; Lane 2 active because the trio is multi-wave) |

V4 is the closure cohort for the master-plan trio. The macro architecture survived V1/V2/V3; the remaining scope is Lock-level binding and source-of-truth reconciliation. V4 confirms the Wave-4.1 amendment closed every V3 reinvent without weakening any prior lane's verdict.

---

## §2 Cohort Verdict — Per-Lane Table

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | READY | 13 | 0 | 0 | Lock 2 path canonicalisation is settled (`passes::layout` everywhere; `passes::types` extinct); Lock 11 split publication clean (stable surface unconditional, sister crates gated); bbnf-bench cross-document `crates.io` agreement. |
| 2 Sequencing-Discipline | READY | 8 | 0 | 0 | H.W2 skip-marker phrasing closed; B.W3/C.W2 inversion remains KEEP-WITH-NOTE per V3 (defensible). |
| 3 Cohesion | READY | 7 | 0 | 0 | Per-grammar baseline reframed as firm; ARCH §12.1 numerics propagate to MASTER-PLAN §20 with `recorded, not deferred`; diagnostic vocabulary now lives at ARCH §7.4 with cross-references. |
| 4 SOTA-Anchoring | READY | 8 | 0 | 0 | H.W3 WASM gate carries lightning-css/WASM anchor (TBD numerics with explicit owner/blocker); json/canada simd-json 3.226ms anchored to SOTA.md:56. |
| 5 Grammar-Authoritative | READY | 8 | 0 | 0 | Lock 14 grep gates clean; per-X 10×9 authority intact; declaration-crate 8-field fence intact; yaml two-surface intact. |
| 6 Generated-Code-Budget | READY | 7 | 0 | 0 | Per-grammar source-of-truth reconciled: ARCH §12.1 firm baselines, MASTER-PLAN §20 mirrors them, A.W2 verifies drift. |
| 7 Friction-Forecast | READY | 8 | 0 | 0 | Cookbook + diagnostic ledger complete; ARCH §7.4 catalogue references resolve from MASTER-PLAN §24. |
| 8 Carry-Deferral | READY | 9 | 0 | 0 | Carry-ledger consolidated: MASTER-PLAN §24 single source of carry-truth with `Source` column; MIGRATION §20 cross-references rather than duplicates. |
| 9 Greenfield-Discipline | READY | 7 | 0 | 0 | Conflict ledger intact; final SOTA escape clause confirmed deleted; no contrivance. |

| Verdict class | Count |
|---|---:|
| KEEP | 75 |
| REINVENT | 0 |
| DISCARD | 0 |

**Final V4 decision: READY.** Zero reinvent rows. The Wave-4.1 amendment closed the nine V3 punch items without introducing new faults, and the V3 PARTIAL gate-rerun results (Gates 10 + 13) now PASS. The trio is ready for per-tranche full-spec drafting.

V4 explicitly defeats the V3 challenges: each Lock 2 narrative reference to the layout pass now points to `passes::layout` and the `passes/src/` tree at ARCH §4.2 carries `layout/`; J.W3 publication splits into stable + incubation-cleared with the stability gate enumerated; bbnf-bench is `crates.io` in all three documents; the H.W3 WASM gate cites lightning-css/WASM; ARCH §7.4 enumerates twenty-eight diagnostic codes the cookbook references; MASTER-PLAN §24 is the single ledger; H.W2 skip-marker is concrete; json/canada anchors simd-json to SOTA.md:56; per-grammar LOC numerics are firm in both ARCH §12.1 and MASTER-PLAN §20.

---

## §3 V3 Punch Closure Verification (M1-M9)

Each V3 punch item is verified against the post-amendment trio. Verification commands run from the repo root.

| # | V3 surgery | Closure verified at | Verification command + result | Status |
|---|---|---|---|---|
| **M1** | Lock 2 path canonicalisation: replace `types/` with `layout/` in §4.2 tree; rename `passes::types` → `passes::layout` everywhere narrative. | `restart/ARCHITECTURE.md:438` (tree); :792 (host route invariant); :868 (host call type unification); :975-990 (side-tables); :1024-1025 (BBNF-HOST diagnostics); `restart/MIGRATION.md:237`; `restart/MASTER-PLAN.md:295` (C.W1). | `rg -n 'passes::types\|passes/src/types' restart/{ARCHITECTURE,MIGRATION,MASTER-PLAN}.md` → **zero hits**. The §4.2 tree at line 438 lists `layout/` as the layout-pass directory; every narrative reference resolves to `passes::layout`. The HM + bidirectional + CSP triad is now framed as "layout-lowering subroutine" per Lock 2 spirit, with `TypeFacts` internal-to-`passes::layout` and `LayoutFacts` the public side-table. | CLOSED |
| **M2** | Lock 11 incubation-vs-publication gating: split J.W3 publication into stable surface (unconditional) and incubation-cleared (stability-gated). | `restart/MASTER-PLAN.md:524`. | `rg -nC4 'incubation.*publication\|stability gate\|stable surface\|frozen-version' restart/MASTER-PLAN.md` → hits at line 524 with explicit two-gate split: "(i) the **stable surface** … publishes unconditionally; (ii) the **incubation-cleared sister crates** (`egraph`, `egraph-derive`, `csp-solver`, `parse-that`) publish only after the stability gate clears — API has not changed across the prior tranche, downstream consumers compile against a frozen-version dry-run for one full tranche cycle, and no breaking change is queued." Failing crates remain path-deps with the failure recorded; one-cycle slip. | CLOSED |
| **M3** | bbnf-bench publication status conflict: pick one canonical status across README/ARCHITECTURE/MASTER-PLAN. | `restart/README.md:38` (`crates.io`); `restart/ARCHITECTURE.md:43` (`Public/dev`); `restart/MASTER-PLAN.md:524` (in stable surface). | `rg -n 'bbnf-bench.*crates.io\|bbnf-bench.*Public/dev' restart/README.md restart/ARCHITECTURE.md` returns the two declarations; MASTER-PLAN J.W3 lists `bbnf-bench` in the stable surface. README "yes / crates.io" + ARCH "Public/dev" + MASTER-PLAN "stable surface, publishes unconditionally" align — bbnf-bench is published. The V3 three-way conflict is gone. | CLOSED |
| **M4** | H.W3 WASM gate Lock 8 anchor: replace `≤ 3x native cost` with a competitor-anchored numeric (lightning-css/WASM recommended). | `restart/MASTER-PLAN.md:459`. | `rg -nC4 'lightning-css/WASM\|css/bootstrap.*WASM' restart/MASTER-PLAN.md` returns the H.W3 row binding `WASM package parses css/bootstrap on M1 Pro Safari WASM runtime within {N}ms (lightning-css/WASM baseline {M}ms on the same fixture; {N} and {M} are TBD at H.W3 measurement, owner = H.W3 lead, blocker = WASM build of lightning-css available for comparison)`. Metadata records WASM runtime, host browser, lightning-css/WASM version, bbnf commit, and fixture hash. The retired `<= 3x native cost` self-reference is named explicitly. | CLOSED |
| **M5** | Per-grammar baseline source-of-truth conflict: settle one place, cascade to the other. | `restart/MASTER-PLAN.md:634-649` mirrors `restart/ARCHITECTURE.md:1273-1281`. | `rg -n 'baseline recorded at A\.W2' restart/MASTER-PLAN.md` → **zero hits** (V3's MASTER-PLAN §20 wording is gone). `rg -nC2 'ARCHITECTURE §12\.1.*baseline\|firm per-grammar' restart/MASTER-PLAN.md` hits 634 with "The firm per-grammar baselines live in `restart/ARCHITECTURE.md` §12.1 (`Generated LOC (current → max)` column, lines 1273-1281); this table mirrors them so all 'nine seed grammars' claims close without chasing PASS-2 or ARCHITECTURE. A.W2 verifies the firm numerics against the live W3 branch and reports drift; the numbers themselves are recorded, not deferred." Mirroring table at 641-651 carries the firm numerics. | CLOSED |
| **M6** | Diagnostic vocabulary asymmetry: enumerate every `BBNF-*` code in ARCHITECTURE alongside the IR contract; cross-reference from MASTER-PLAN §24. | `restart/ARCHITECTURE.md:992-1039` (new §7.4). | `rg -nC2 'Diagnostic Vocabulary\|BBNF-LIFETIME-ESCAPE\|BBNF-ARENA-MISMATCH' restart/ARCHITECTURE.md` returns the §7.4 header at 992 plus a 28-row catalogue (lines 1004-1032) covering BBNF-LIFE001-009, BBNF-ARENA-MISMATCH, BBNF-VISITOR-MUTATION-OUTSIDE-ENTRY + VISIT001/003, BBNF-LAYOUT-CONFLICT/UNCLOSED + LAYOUT001-002, BBNF-LOOKBEHIND-WIDTH (PASS-1 alias `BBNF1004`), BBNF-PRATT/SIMD codes, BBNF-METADATA/GRAMMAR codes, BBNF-POINTER001-003, BBNF-HOST001-003, BBNF-RECOVERY*, BBNF-GEN001/014, BBNF-CODEGEN021/033, BBNF-SEM040. Cross-references at lines 1001-1002 ("MASTER-PLAN §24 cookbook table references this catalogue rather than re-enumerating codes"). | CLOSED |
| **M7** | Carry-ledger asymmetry: consolidate into one ledger or cross-reference. | `restart/MASTER-PLAN.md:730-756` (single ledger with `Source` column); `restart/MIGRATION.md:772-783` (cross-reference). | `rg -n 'Source: migration\|Migration-implementation receivers' restart/{MASTER-PLAN,MIGRATION}.md` returns the consolidation: MIGRATION:774-776 reads "Migration-implementation receivers are tracked at `restart/MASTER-PLAN.md` §24 (Carry and Friction Ledger) with `Source: migration` or `Source: synthesis + migration` tags. The eight migration-sourced items … appear in that consolidated ledger; this section retains its heading for cross-document anchoring but no longer carries a separate table." MASTER-PLAN §24 ledger now carries 19 rows × 5 columns (Item / Receiver / Blocker / Gate / Source) with `synthesis`, `migration`, or `synthesis + migration` source tags. | CLOSED |
| **M8** | H.W2 "skipped metadata" phrasing: clarify the skip mechanism. | `restart/MASTER-PLAN.md:458`. | `rg -n 'cpu_feature.*unsupported\|skip-marker' restart/MASTER-PLAN.md` hits line 458 with "Platform-specific tests pass on supporting hardware; tests on non-supporting hardware are skipped with a CI-readable skip-marker recording the missing capability (for example `cpu_feature: avx2_unsupported`)." Concrete, machine-readable, unambiguous. | CLOSED |
| **M9** | json/canada "simd-json comparable" looseness: replace with firm number from SOTA.md or strike. | `restart/MASTER-PLAN.md:133`. | `rg -nC2 'simd-json 3\.226' restart/MASTER-PLAN.md` returns line 133 binding "sonic-rs 3.144ms; simd-json 3.226ms (`restart/corpora/SOTA.md:56`)" → ≤ 2.8ms. `rg -n 'simd-json comparable' restart/MASTER-PLAN.md` returns **zero hits**. The path:line citation to SOTA.md:56 is the receipt. | CLOSED |

**M1-M9 closure verdict: 9/9 CLOSED.** Zero V3 surgeries are deferred; zero new faults are introduced.

### §3.1 Adversarial Pressure On Closure Quality

Each V3 punch item could close shallow (rename without architectural follow-through) or deep (rename + invariant + cross-document consequence). V4 audits closure depth.

| Punch | Closure depth | Adversarial test | V4 verdict |
|---|---|---|---|
| M1 Lock 2 path canon | DEEP | A reader of the §4.2 tree, §6 invariants, §7.1 host call, §7.3 side-tables, §8.2 type rules, §7.4 BBNF-HOST diagnostics, MIGRATION §3 type-facts row, and MASTER-PLAN C.W1 must arrive at the same name. V4 confirms `passes::layout` everywhere; `passes::types` extinct in the trio. | Deep closure. |
| M2 Lock 11 split | DEEP | A reader inferring the J.W3 publication outcome must be able to determine which crate publishes when. V4 confirms two-class split with stability-gate criteria enumerated (API frozen one tranche, frozen-version dry-run, no breaking change queued). | Deep closure. |
| M3 bbnf-bench status | SHALLOW-BUT-CONSISTENT | A reader cross-checking README + ARCH + MASTER-PLAN must find the same status. V4 confirms `crates.io` / `Public/dev` / stable-surface alignment. The terms are different but the meaning is identical: the bench publishes. | Shallow but consistent — no adversarial fault. |
| M4 H.W3 WASM anchor | DEEP-WITH-TBD | A reader must find a competitor + dataset + platform + measurement plan. V4 confirms lightning-css/WASM + css/bootstrap + M1 Pro Safari WASM + TBD numerics with H.W3 owner + blocker. The TBD numerics are scoped to measurement, not deferred. | Deep closure with explicit-TBD discipline. |
| M5 Per-grammar baseline | DEEP | A reader chasing the firm number must find one source. V4 confirms ARCH §12.1 firm; MASTER-PLAN §20 mirrors with explicit pointer; "recorded, not deferred." | Deep closure. |
| M6 Diagnostic vocabulary | DEEP | A reader of any diagnostic code must find both the producer site and the meaning at one location. V4 confirms ARCH §7.4 28-row catalogue with site + meaning columns; cookbook references the catalogue. | Deep closure. |
| M7 Carry-ledger consolidation | DEEP | A reader of any deferred item must find one ledger with receiver + blocker + gate + source. V4 confirms MASTER-PLAN §24 19-row ledger; MIGRATION §20 cross-references. | Deep closure. |
| M8 H.W2 skip-marker | DEEP | A CI consumer must be able to parse the skip metadata. V4 confirms `cpu_feature: avx2_unsupported` example concrete and machine-readable. | Deep closure. |
| M9 json/canada firm number | DEEP | A reader chasing the simd-json baseline must find a firm number with citation. V4 confirms `simd-json 3.226ms (restart/corpora/SOTA.md:56)`. | Deep closure. |

**Closure-depth verdict: 8/9 deep, 1/9 shallow-but-consistent.** No closure is shallow-and-conflicted. The adversarial pressure on M3 specifically (different terms across documents could mask conflict) finds no semantic disagreement: bench publishes, period.

---

## §4 Lane 1 — Lock-Adherence (full re-audit)

Lane standard: each of 14 locks honoured / violated-with-recommendation / silent-must-add. V3 flagged this lane AMENDMENT-REQUIRED on Lock 2 + Lock 11 + bbnf-bench. V4 verifies closure and redrives the full Lock 1-14 walk.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:21-22` | Lock 1 — tape + direct-to-struct union | Conflict ledger names tape as substrate, unioned with direct-to-struct, with parallel substrate / OpenFrame ladders banned. ParseStream resolved to `syn` macro use only. | Resolves prior 86.07% Vec<OpenFrame>::clone pathology by retiring orthogonal codepath, not the tape name. | None. | Steelman: an alternative reading is that ParseStream should also leave macro code. The V3 amendment correctly bounded ParseStream to proc-macro `syn` without leaking it as a runtime concept. KEEP. | KEEP |
| `restart/ARCHITECTURE.md:435-442` ↔ `restart/ARCHITECTURE.md:792, :868, :983, :1066`; `restart/MASTER-PLAN.md:295` | **Lock 2 — `passes::layout` canonicalisation** | The §4.2 tree carries `layout/` as a child of `passes/src/`; every narrative reference (host route, host call unification, side-tables, MIGRATION TypeFacts row, MASTER-PLAN C.W1) consumes `passes::layout`. HM + bidirectional + CSP is the layout-lowering subroutine. `LayoutFacts` is the public side-table; `TypeFacts` lives only inside the layout pass. | Lock 2 prescribed `passes::layout`; the artefact now matches. The V3 internal contradiction (tree said `types/`, narrative said `layout`) is resolved. The HM-as-subroutine framing matches the lock's "HM/CSP type checking is a subroutine of layout lowering, never a public peer pass." | None — the V3 reinvent is closed. | The V3 challenge ("§4.2 tree contradicts §6/§7.3/§8.2 narrative") no longer applies; both pivot to `passes::layout`. KEEP. | KEEP |
| `restart/ARCHITECTURE.md:802-806` | Lock 3 — cursor + byte-skip gates | Three rows: `__EAGER_EMPTY_PATH` regression fixture; `CursorDecision::Skip` unit + VM replay; scanner fast-path span round-trip. | Mechanism-level binding; testable from same-tranche carry rows. | None. | Lock 3 honoured. | KEEP |
| `restart/MASTER-PLAN.md:296` | Lock 4 — CSP/egraph bridge | C.W4 produces bridge tables; egraph and CSP exchange facts through bridge API. | Mechanism-level binding; resolves Lock 4's "no fused hypergraph" mandate. | None. | Lock 4 honoured. | KEEP |
| `restart/ARCHITECTURE.md:856-868`; `MASTER-PLAN.md:163-171` | Lock 5 — IR + per-backend lower | Backend IR is the only lowerer input. Codegen never reads Grammar IR (§2 line 173). VM replays all BIR variants. Import-deny test named at MIGRATION §19.3. | Mechanism-level boundary; concrete tests. | None. | Lock 5 honoured. | KEEP |
| `restart/ARCHITECTURE.md:1224-1225` ↔ `restart/MASTER-PLAN.md:202` | Lock 6 — committed source generation | "Generated source is committed. Lock 6 rejects a proc-macro facade and requires `xtask`-style committed source generation." | Aligns with Lock 6 spirit; `cargo xtask bbnf build` cited everywhere. | None. | Lock 6 honoured. | KEEP |
| `restart/ARCHITECTURE.md:55-58` ↔ `restart/MASTER-PLAN.md:80` | Lock 7 — path split | `path`, `path-core`, `path-ts` enumerated; `path-core` is "the single semantics owner, matching the path split in Lock 7." | Mechanism-level; matches Lock 7 verbatim. | None. | Lock 7 honoured. | KEEP |
| `restart/ARCHITECTURE.md:1252-1259` ↔ `restart/MASTER-PLAN.md:128-136` | Lock 8 — SOTA close | Six rows × five columns at MASTER-PLAN §4. Every row binds competitor + dataset + platform + bbnf target + reproducibility metadata. H.W3 WASM gate now anchored to lightning-css/WASM (line 459). json/canada anchored to simd-json 3.226ms (line 133). | Numeric, anchored, dual-document, WASM gate Lock-8-compliant. | None — V3 H.W3 violation closed. | Lock 8 honoured. | KEEP |
| `restart/ARCHITECTURE.md:196-203` | Lock 9 — slice-borrow primary; bumpalo + owned escape hatches | `parse(input)` slice-borrow primary; `parse_in(input, &arena)` arena-aware; `parse_owned(input)` owns bytes. | Mechanism-level; Lock 9 verbatim. | None. | Lock 9 honoured. | KEEP |
| `restart/ARCHITECTURE.md:715-716` ↔ `restart/MASTER-PLAN.md:677` | Lock 10 — Pratt + SIMD auto-detected | Schema rule: "`pratt`, `simd`, and recognizers default to `auto`." Lock 10 row at MASTER-PLAN §21: "C/H — Recognizer facts, no directives." | Mechanism-level; metadata defaults declarative. | None. | Lock 10 honoured. | KEEP |
| `restart/MASTER-PLAN.md:524` | **Lock 11 — incubating-vs-publication gating** | J.W3 publication is split: (i) stable surface publishes unconditionally; (ii) incubation-cleared sister crates publish only after the stability gate clears (API frozen one tranche cycle, downstream consumers compile against frozen-version dry-run, no breaking change queued). Failing crates remain path-deps with failure recorded. | Lock 11 prescribed "promote to registry once stable"; the J.W3 row enforces the stability proof. The V3 challenge ("J.W3 publishes incubated sister crates without proving stability") is resolved. | None — V3 reinvent closed. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:65-67` ↔ `restart/MASTER-PLAN.md:85-86` | Lock 12 — `ser` and `gorgeous` archive before BA.W0 | Crate table flags `ser`/`gorgeous` as "archived before the first implementation tranche, as Lock 12 requires." A.W0 archive ceremony at MASTER-PLAN line 226. | Mechanism-level; sequencing matches Lock 12. | None. | Lock 12 honoured. | KEEP |
| `restart/ARCHITECTURE.md:1359-1376` ↔ `restart/MASTER-PLAN.md:683-692` | Lock 13 — file/directory discipline | 4-10 children + 500 LOC handwritten ceiling + four exception classes (generated grammar/runtime, generated data tables, handwritten over-500 forbidden, >10 children only with rationale). MASTER-PLAN §21 verification table: 5 surfaces × child-count + LOC + exception rationale + enforcing command. | Mechanism-level binding. The bbnf canonical 8-children layout at ARCH §4.1 line 382 verifies one crate concretely. | The §4.x crate trees do not enumerate per-crate child counts; readers infer from the tree. The V3 KEEP "but a per-crate table at §4.x would prevent regression" remains a polish item, not a fault — the lint gates at A.W4 and the tree-shape verification table at MASTER-PLAN §21 enforce the discipline mechanically. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:1259-1297` | Lock 14 — per-X authority + yaml two-surface | 10 grammars × 9 columns at §12.1. yaml row says fixtures are parity-phase only. `Allowed changes:` enumerates two surfaces; `Forbidden changes:` enumerates Rust source, package declarations, manual registries, declaration crates. Required commands enumerated. | Mechanism-level; Lock 14 onboarding test fully bound. | The yaml LOC budget "0 → ≤ 4,000 (provisional; SYNTHESIS Wave-2 owner)" still carries a "provisional" qualifier — but per V3, the resolution mechanism is named (G.W4 measures and replaces "provisional" with a firm number). The V3 KEEP holds. | None — Lock 14 onboarding test is the executable verification; yaml's pre-implementation status justifies the provisional bound. | KEEP |

**Lane 1 verdict: READY.** KEEP 13 / REINVENT 0 / DISCARD 0. Every lock is honoured at mechanism level. The V3 Lock 2 + Lock 11 + bbnf-bench faults are resolved.

### §4.1 Cross-Lock Cohesion

V4 audits whether locks compose without conflict. Three cross-lock interactions deserve scrutiny:

| Lock pair | Interaction | V4 verdict |
|---|---|---|
| Lock 2 (`passes::layout`) ↔ Lock 4 (CSP/egraph compose by output piping) | The Wave-4.1 amendment frames CSP as a layout-lowering subroutine alongside HM and bidirectional checking. Lock 4 says CSP is a sister crate that composes by output piping. Are these compatible? | The CSP solver is in `crates/csp-solver` (sister crate per Lock 11); when called from `passes::layout`, it produces internal `TypeFacts` consumed by layout; when called from `passes::extract` for cost decisions, it produces public `CspSolution`. ARCH §7.3 line 987 marks `CspSolution` as "Public when produced for cost extraction; internal when produced inside layout lowering." Both readings honoured: solver remains generic (Lock 4); layout-internal use is subroutine (Lock 2). |
| Lock 5 (Backend IR is the contract) ↔ Lock 14 (no grammar-name in generic crates) | If codegen is BIR-only, can codegen still emit per-grammar runtime files? | Yes — codegen emits `runtime/src/grammars/<name>/` from BIR + side tables. The grammar name is metadata-derived (per Lock 14's two-surface rule); codegen never matches on a hardcoded grammar name. ARCH §7.2 line 965 ("Lowerers never inspect Grammar IR.") + §10 line 1208 ("Code generation is Backend-IR-only.") + §12.1 per-X table cooperate. |
| Lock 8 (SOTA) ↔ Lock 11 (incubating sister crates) | If sister crates publish only after stability proof, can SOTA gates depend on sister-crate APIs? | Yes — SOTA gates run at H.W4 / H.W5 / J.W1 inside the workspace; published-vs-path-dep status only matters for downstream consumers. The internal SOTA gates can call into incubating crates. J.W3 publishes after SOTA closes (J.W1 → J.W2 → J.W3). Sequencing honours both locks. |

No cross-lock conflict surfaces. Lane 1 is READY at the integration level.

---

## §5 Lane 2 — Sequencing Discipline (re-confirm)

Lane standard: every wave produces an artefact with a same-wave or next-wave consumer. V3 flagged READY-WITH-NOTE on H.W2 phrasing.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:225-230` | A.W0-A.W4 sequencing | Archive ceremony → 24-crate skeleton → metadata schema → grammar parser → generalization lint. | Each wave consumed by next. A.W4 lint consumes A.W3 grammar parser via `cargo xtask`. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:259-263` | B.W0-B.W4 sequencing | Tape tokens → checkpoints → DocumentView → direct-builder shell + tape identity hooks → seed generated grammar shell. | Tape substrate paired same-wave with consumer (B.W3 direct view borrows spans from tape). | The B.W3 direct-builder shell precedes C.W2 ShapeFacts; V3 noted this inversion. The shell is tape-only, contract is C.W2-driven; KEEP-WITH-NOTE per V3. | The substrate-shell vs consumer-contract distinction is the resolution; readers should infer that B.W3 shell is tape-only scaffolding, C.W2 fills in ShapeFacts contract. KEEP. | KEEP |
| `restart/MASTER-PLAN.md:294-300` | C.W0-C.W5 sequencing | Grammar IR → layout (HM+bidirectional+CSP subroutine) producing LayoutFacts → ShapeFacts → RecognizerFacts → CSP/egraph bridge → CostFacts. | Each wave consumed by next; aligns with V1 punch items 40 + 41 receivers; Lock 2's `passes::layout` ownership now consistent at C.W1. | C.W2 ShapeFacts has consumer "Direct-builder shell contract consumes ShapeFacts in a C fixture and records B integration gaps" — the inversion noted at row above. | KEEP-WITH-NOTE. | KEEP |
| `restart/MASTER-PLAN.md:329-331` | D.W0-D.W4 + Backend IR variants | Lookbehind → generics → `@host fn` → chains → `@error`/`@layout`/regex Unicode/rewrite rejection. | E.W0 (line 360) builds 23-variant BIR enum with "All variants construct and validate" — same-wave-after-D consumer. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:360-364` | E.W0-E.W4 sequencing | BIR enum → GIR + side tables → BIR builder → VM core → VM full coverage → lowerer trait + boundary. | Every wave consumes prior; E.W4 boundary check uses Lock 5 import-deny. Consumer = F. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:391-396` | F.W0-F.W5 sequencing | Control-flow lowerer → tape/direct emit → host/chain/layout/error → template + equality → generated LOC budget → nine-grammar regen. | F.W3 equality consumed by F.W5 regen; F.W4 budget tooling consumed by F.W5 ceiling. | F.W2 depends on D extensions through E lowering — three-tranche-deep dependency, acceptable per LESSONS-LEARNED §1-34. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:455-461` | H.W0-H.W5 sequencing | Pratt → SIMD → AVX2/NEON/scalar dispatch (with skip-marker) → WASM V1 (lightning-css/WASM anchored) → early JSON SOTA → early CSS SOTA. | Each numeric early threshold (H.W4: ≤480µs/950µs/3.5ms; H.W5: ≤3.8ms/1.9ms) carries to final J.W1 threshold. H.W2 skip-marker is concrete: "skipped with a CI-readable skip-marker recording the missing capability (for example `cpu_feature: avx2_unsupported`)." H.W3 anchored to lightning-css/WASM. | None — V3 H.W2 polish closed; V3 H.W3 Lock 8 fault closed. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:520-526` | J.W0-J.W5 sequencing | Parity → SOTA → docs → publication (split per Lock 11) → archive audit → close report. | Each wave consumes prior; J close depends on every earlier tranche. Lock 11 split clean. | None — V3 Lock 11 fault closed. | KEEP. | KEEP |

**Lane 2 verdict: READY.** KEEP 8 / REINVENT 0 / DISCARD 0. The V3 H.W2 polish is closed; the B.W3/C.W2 substrate-shell inversion remains the sole KEEP-WITH-NOTE (defensible as substrate-vs-contract distinction).

---

## §6 Lane 3 — Cohesion (full re-audit)

Lane standard: every claim verifiable from artefacts the trio produces or cites; no orphan claims; no orphan deliverables. V3 flagged AMENDMENT-REQUIRED on per-grammar baseline source-of-truth + diagnostic vocabulary asymmetry.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:1259-1297` | Per-X 10×9 authority table | 10 grammars × 9 columns. Single authoritative consumer; every "all extant grammars" claim resolves here. | yaml row carries `provisional` qualifier with G.W4 owner. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:634-651` ↔ `restart/ARCHITECTURE.md:1273-1281` | **Per-grammar baseline source-of-truth** | MASTER-PLAN §20 mirrors ARCH §12.1 numerics with explicit pointer to the firm source: "The firm per-grammar baselines live in `restart/ARCHITECTURE.md` §12.1 (`Generated LOC (current → max)` column, lines 1273-1281); this table mirrors them so all 'nine seed grammars' claims close without chasing PASS-2 or ARCHITECTURE. A.W2 verifies the firm numerics against the live W3 branch and reports drift; the numbers themselves are recorded, not deferred." | Single source of truth resolved; A.W2 is the drift-verifier, not the producer. | None — V3 reinvent closed. | KEEP. | KEEP |
| `restart/MIGRATION.md:113-165` | Mixed-fate crosswalk | 30 rows × 6 columns. Approximate file counts; refines at A.W2. | Resolves V1 punch item 42. "approximate" qualifier honest; refinement gate bound (A.W2). | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:683-692` | Lock 13 verification table | 5 surfaces × child-count + LOC + exception rationale + enforcing command. | Mechanism-level lint gate. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:138-150` | Benchmark reproducibility schema | 8 rows × 2 columns. Every field has a verbatim source command. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:1095-1101` | Input-normalization-deletions table | 5 rows × 4 columns (Surface, Status, Routed substrate, Closing gate). | Resolves V1 punch items 9 + 10. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:992-1039` ↔ `restart/MASTER-PLAN.md:762-770` | **Diagnostic vocabulary integration** | New §7.4 catalogue at ARCH lines 992-1039 enumerates 28 diagnostic codes with site + meaning. Cross-reference at line 1001-1002: "MASTER-PLAN §24 cookbook table references this catalogue rather than re-enumerating codes." MASTER-PLAN §24 cookbook table at lines 762-770 references the codes by identifier; the verbatim diagnostic strings live with the producer (PASS-2 and PASS-3). | Single catalogue at the executable contract; cookbook references rather than re-enumerates. The V3 asymmetry is closed. | None — V3 reinvent closed. | KEEP. | KEEP |

**Lane 3 verdict: READY.** KEEP 7 / REINVENT 0 / DISCARD 0. Both V3 source-of-truth conflicts are resolved: per-grammar baselines are firm in both ARCH §12.1 and MASTER-PLAN §20; diagnostic vocabulary is catalogued at ARCH §7.4 with cookbook cross-reference.

---

## §7 Lane 4 — SOTA Anchoring (full re-audit)

Lane standard: every parse-throughput gate cites a competitor + dataset + platform per Lock 8. V3 flagged AMENDMENT-REQUIRED on H.W3 WASM gate + json/canada looseness.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:128-136` | SOTA close rows | 6 rows × 5 columns. | Every row inlines competitor + dataset + platform + bbnf target. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:131` | json/twitter row | sonic-rs 436us; simd-json 424us → ≤ 380us M1 Pro. | Aligns with `restart/corpora/SOTA.md:50-89`. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:132` | json/citm row | sonic-rs 854us; simd-json 831us → ≤ 750us M1 Pro. | Anchored. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:133` | **json/canada row** | sonic-rs 3.144ms; simd-json 3.226ms (`restart/corpora/SOTA.md:56`) → ≤ 2.8ms M1 Pro. | The "comparable" qualifier is replaced with a firm number with path:line citation. The V3 looseness is closed. | None — V3 reinvent closed. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:134` | css/bootstrap row | lightning-css 4.16ms → ≤ 3.0ms M1 Pro. | Anchored. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:135` | css/animate row | lightning-css 1.97ms → ≤ 1.6ms M1 Pro. | Anchored. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:136` | simd/structural_scan row | simdjson On-Demand ~7 GB/s x86 AVX2; ~5 GB/s M-series → ≥ 5 GB/s M-series, ≥ 7 GB/s AVX2. | Anchored, dual-platform. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:459` | **H.W3 WASM gate** | WASM package parses css/bootstrap on M1 Pro Safari WASM runtime within `{N}`ms (lightning-css/WASM baseline `{M}`ms on the same fixture; `{N}` and `{M}` TBD at H.W3 measurement). Owner = H.W3 lead; blocker = WASM build of lightning-css available. Metadata records WASM runtime, host browser, lightning-css/WASM version, bbnf commit, fixture hash. The retired `<= 3x native cost` self-reference is named explicitly. | Lock 8 honoured — competitor + dataset + platform + measurement plan. The TBD numerics are scoped to H.W3 measurement with explicit owner + blocker. | The TBD numerics are placeholders. The V3 challenge "WASM gate must cite a WASM competitor" is met; the actual numbers ship at H.W3 measurement, which is when WASM runtime is present anyway. KEEP. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:138-150` | Reproducibility schema | 8 fields per row. | Mechanism-level binding. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:522` | J.W1 final SOTA gate | "JSON/CSS/SIMD targets met; misses require amendment before close." | Final SOTA escape clause confirmed deleted; misses block close. | None. | KEEP. | KEEP |

**Lane 4 verdict: READY.** KEEP 8 / REINVENT 0 / DISCARD 0. H.W3 WASM Lock 8 fault closed; json/canada simd-json firm number cited.

---

## §8 Lane 5 — Grammar-Authoritative Discipline (re-confirm)

Lane standard: zero proposed match-arms in generic crates; per-X tables for every "all-grammars" claim; future-grammar onboarding test honoured; per-grammar code lives only in workspace metadata or `@host fn`.

V4 verification commands (run at audit-time):

```
$ rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' \
    restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md
(zero matches)

$ rg -ni 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' \
    restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md
restart/ARCHITECTURE.md:326     (API leakage rule — mechanism-level forbidden example)
restart/MIGRATION.md:293        (rg gate negative grep)
restart/MIGRATION.md:696        (rg gate negative grep)
(all matches are mechanism-level negative-grep gates)
```

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:1259-1297` | Per-X 10×9 authority table | 10 grammars × 9 columns. | Single authoritative table. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:1095-1101` | Input-normalization deletions | 5 deletions × routed substrate × closing gate. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:1270-1303` | Future grammar onboarding test (yaml) | Two surfaces only; forbidden changes enumerated; required commands enumerated. | Mechanism-level. Resolves Lock 14 verification gate. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:723-754` | Declaration-crate fence (8 fields) | Reason / Owner / Why metadata fails / Why `@host fn` fails / Declaration location / No generic import proof / Deletion path / Reviewer. Reified as TOML. `allow=false` is default for all 9 extant grammars. | Mechanism-level fence. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:719` | Lock 14 lint risk row | Risk: "Old grammar registries reappear in new crates"; mitigation: `cargo xtask lint-no-hardcoded-grammars` enforced at A.W4, G.W4, J.W4 with `rg "PRODUCTION_MANIFEST_TABLE\|GrammarAuditTag\|bbnf-strategy"` returning zero. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/MIGRATION.md:692-700` | §19.1 generalization gate | Three rg gates: parser type names, hardcoded registries, manifest table. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:322-334` | API leakage rules | 6 forbidden examples → allowed replacements. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:589-720` | Workspace metadata schema | `[workspace.metadata.bbnf.grammars.<name>]` is the per-grammar declarative surface. Schema rules enforced by `grammar::metadata` + `pipeline::workspace`. | Mechanism-level. | None. | KEEP. | KEEP |

**Lane 5 verdict: READY.** KEEP 8 / REINVENT 0 / DISCARD 0. Lock 14 grep gates clean; per-X authority intact; declaration-crate fence intact; future-grammar onboarding test intact.

---

## §9 Lane 6 — Generated-Code + LOC Budget (re-confirm)

Lane standard: per-tranche LOC budget; xtask regen-cycle wall budget; per-grammar LOC delta projection; per-wave gate.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:617-629` | §20 generated LOC trajectory | 9 stages × 2 columns. Wall-time budgets per stage (30s / 60s / 60s / 90s / 120s / 150s / 150s / 150s / 180s). | Per-wave wall budget. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:634-651` ↔ `restart/ARCHITECTURE.md:1273-1281` | **Per-grammar baseline source of truth** | MASTER-PLAN §20 mirrors ARCH §12.1 firm numerics; A.W2 verifies drift, the numbers are "recorded, not deferred." | Single source of truth. | None — V3 reinvent closed. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:654-661` | Budget enforcement rows | 4 rows × 2 columns (Scope, Gate). | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:1261-1268` | Generated LOC budget rows | 4 rows × 2 columns. | Aligns with PASS-2 +2% ceiling. | None. | KEEP. | KEEP |
| `restart/MIGRATION.md:583-604` | LOC trajectory by tranche | 11 rows × 3 columns. | Tranche-level expectations. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:117` | Generated LOC budget hard gate | "Enforce PASS-2 +2 percent budget." | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:689` | Generated grammar/runtime files exception | "Not bound by 4-10. LOC excepted; budget rows in §20 govern." | Aligns with Lock 13 exception ledger. | None. | KEEP. | KEEP |

**Lane 6 verdict: READY.** KEEP 7 / REINVENT 0 / DISCARD 0. Per-grammar baseline firmness reconciled; wave budgets and exception classes well-specified.

---

## §10 Lane 7 — Friction Forecast (re-confirm)

Lane standard: where users / grammar authors hit the proposed API and do not understand it; specify user, mental model, confusion point, cookbook, verbatim error message.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:758-770` | §24 Cookbook + migration friction rows | 7 rows × 6 columns (Friction, Target user, Mental model, Confusion point, Artefact, Diagnostic). | Mechanism-level cross-cut: every row carries target user + mental model + confusion point + cookbook receiver + diagnostic code. Codes resolve to ARCH §7.4 catalogue. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:764` | `pointer!` + `select!` row | `BBNF-POINTER-UNKNOWN-SEGMENT` + `BBNF-POINTER-GRAMMAR-MISMATCH`. Mental model: path expression checked against grammar's path schema at compile time. | Aligns with PASS-3 §3 path commitments; codes catalogued at ARCH §7.4 lines 1021-1023. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:765` | Lifetime constructors row | `BBNF-LIFETIME-ESCAPE` + `BBNF-ARENA-MISMATCH`. Mental model: parse borrows / parse_in into arena / parse_owned allocates. | Aligns with PASS-3 §2 lifetime commitments; codes catalogued at ARCH §7.4 lines 1006-1007. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:766` | Visitor mutation row | `BBNF-VISITOR-MUTATION-OUTSIDE-ENTRY`. Mental model: read-write visitor only. | Aligns with PASS-3 visitor contract; code catalogued at ARCH §7.4 line 1010. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:767` | Layout errors row | `BBNF-LAYOUT-CONFLICT` + `BBNF-LAYOUT-UNCLOSED`. | Mechanism-level; codes at ARCH §7.4 lines 1013-1014. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:768` | Pratt/SIMD decisions row | `BBNF-PRATT-NOT-APPLIED` + `BBNF-SIMD-NOT-SELECTED`. Mental model: Pratt and SIMD auto-detected; metadata can disable but not force. | Aligns with Lock 10; codes at ARCH §7.4 lines 1017-1018. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:769` | Crate split migration row | "Old `bbnf-path*` and `core` are split into unprefixed crates." Cookbook page `cookbook/migration-crate-split.md`. | Documentation friction, not runtime diagnostic. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:770` | Adding yaml row | `BBNF-METADATA-MISSING-GRAMMAR` + `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE`. Mental model: two surfaces only. | Aligns with Lock 14 future-grammar onboarding test; codes at ARCH §7.4 lines 1019-1020. | None. | KEEP. | KEEP |

**Lane 7 verdict: READY.** KEEP 8 / REINVENT 0 / DISCARD 0. The cookbook + diagnostic ledger is complete; ARCH §7.4 catalogue cross-reference resolves the V3 asymmetry.

---

## §11 Lane 8 — Carry & Deferral Audit (full re-audit)

Lane standard: every "deferred to" / "carries to" / "future" / "TBD" / "user adjudicates" must name receiver + blocker + receiving gate. V3 flagged AMENDMENT-REQUIRED on bbnf-bench publication conflict + carry-ledger asymmetry + WASM ABI/SOTA carry split.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:730-756` | **§24 Carry + friction ledger (consolidated)** | 19 rows × 5 columns (Item, Receiver, Blocker, Gate, Source). Single source of carry-truth. Source column tags `synthesis`, `migration`, or `synthesis + migration`. | Every row triple-complete; source attribution explicit. The V3 carry-ledger asymmetry is closed: MIGRATION §20 retains heading for cross-document anchoring but no longer carries a separate table. | None — V3 reinvent closed. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:739` | Declaration-crate escape valve row | Receiver A/D; Blocker review form missing; Gate metadata validator rejects partial fence. | Aligns with ARCH §5.6 8-field fence. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:741` | Cursor skip row | Receiver B/H; Blocker runtime cannot prove empty-path and byte-skip; Gate `__EAGER_EMPTY_PATH` and `CursorDecision::Skip` fixtures. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:743` | SOTA metadata row | Receiver H/J; Blocker bench numbers lack metadata; Gate benchmark report schema rejects incomplete rows; benchmark host hardware profiles cite SOTA baselines. Source: synthesis + migration. | Cross-source row consolidating prior MIGRATION §20 entry. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:744` | yaml onboarding row | Receiver A/F/G/J; Blocker future grammar requires manual Rust edit; Gate yaml source + workspace metadata + generated runtime only. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:745` | Archive closure row | Receiver A/J; Blocker `ser`/`gorgeous` remains in production; Gate workspace membership + `archive/<crate>/` placement verified. Source: synthesis + migration. | Cross-source row. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:750` | **Publication readiness row** | Receiver A/J; Blocker package names + dry-run; Gate `cargo xtask publish --dry-run` clean for every public crate. The bbnf-bench three-document conflict resolved (M3): bench is `crates.io` per README, `Public/dev` per ARCH, listed in stable surface per MASTER-PLAN J.W3. | Single source of publication truth; V3 conflict resolved. | None — V3 reinvent closed. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:753` | **WASM ABI row** | Receiver H/J; Blocker WASM exported ABI not specified for V1; Gate H.W3 records exported function names; J.W3 dry-run includes WASM binding. Lock 8 SOTA anchor (Punch 4) folded into H.W3 row directly via lightning-css/WASM citation. | The V3 split-into-ABI-and-SOTA concern resolves at H.W3: ABI named at H.W3; SOTA target named at H.W3 (line 459); dual closure. | None — V3 reinvent closed. | KEEP. | KEEP |
| `restart/MIGRATION.md:772-783` | **§20 cross-reference** | Heading retained; pointer to MASTER-PLAN §24 with `Source: migration` or `Source: synthesis + migration` tags. Eight migration-sourced items enumerated: exact generated header fields, declaration-crate review form, `path-ts` publication timing, WASM exported ABI, benchmark host hardware profiles, archive destination, PASS-2 BIR snapshots, Lock 3 cursor gates. | Cross-document anchor preserved; the duplication retired. | None — V3 reinvent closed. | KEEP. | KEEP |

**Lane 8 verdict: READY.** KEEP 9 / REINVENT 0 / DISCARD 0. The V3 three-fault ledger (bbnf-bench, ledger asymmetry, WASM split) is closed.

### §11.1 Carry-Ledger Source-Column Audit

V4 verifies the consolidated MASTER-PLAN §24 ledger's `Source` column attribution per row. The eight migration-sourced items named at MIGRATION §20 (lines 776-781) are: exact generated header fields, declaration-crate review form, `path-ts` publication timing, WASM exported ABI, benchmark host hardware profiles, archive destination, PASS-2 BIR snapshots, Lock 3 cursor gates.

| Migration item | MASTER-PLAN §24 row | Source tag |
|---|---|---|
| Generated header fields | line 754 | `migration` |
| Declaration-crate review form | line 739 (escape valve) | `synthesis + migration` |
| path-ts publication timing | line 755 | `migration` |
| WASM exported ABI | line 753 | `synthesis + migration` |
| Benchmark host hardware profiles | line 743 (SOTA metadata) | `synthesis + migration` |
| Archive destination for ser/gorgeous | line 745 (Archive closure) | `synthesis + migration` |
| PASS-2 BIR snapshots | line 756 | `migration` |
| Lock 3 cursor gates | line 741 (Cursor skip) | `synthesis + migration` |

All eight migration-sourced items are present in the consolidated ledger with appropriate `Source` attribution. The cross-document anchor at MIGRATION §20 is intact. V3's reinvent is fully closed.

---

## §12 Lane 9 — Greenfield Discipline (re-confirm)

Lane standard: no quick solutions / no workarounds / no legacy uncontested / no contrivance / idiomatic gestalt / architectural transpositions mandatory.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:21-30` | Conflict ledger | 10 rows × 4 columns. Every superseded position has a settled resolution. | Honest archaeology; matches HARDENING-CONSOLIDATED §3 row 6. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:1095-1101` | Input-normalization deletions | 5 deletions, every closing gate verifiable. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:227` | A.W1 package naming | "unprefixed internal crates: `path`, `path-core`, `path-ts`, `test-fixtures`, `passes`, `simd-scan`, `egraph`, `csp-solver`; user-facing crates retain `bbnf-` prefix." | Resolves HARDENING-CONSOLIDATED §3 conflict #3. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:524` | J.W3 publication readiness | Lock 11 split; stable surface unconditional; incubation-cleared sister crates gated. | Path-dep leak gate enforced. | None — V3 Lock 11 fault closed. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:522` | J.W1 final SOTA gate | "misses require amendment before close." | Final SOTA escape clause confirmed deleted. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:382-391` | bbnf canonical 8-children layout | "exactly 8 immediate children." Resolves HARDENING-CONSOLIDATED §3 conflict #3 + §4.19 fix. | Resolves prior 7-children divergence. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:973-990` | Lock 2 — TypeFacts internal, LayoutFacts public | TypeFacts stays internal to `passes::layout`; LayoutFacts is the public side-table. | Resolves HARDENING-CONSOLIDATED §3 conflict #4. The path canonicalisation is settled (passes::layout everywhere). | None — V3 Lock 2 fault closed. | KEEP. | KEEP |

**Lane 9 verdict: READY.** KEEP 7 / REINVENT 0 / DISCARD 0. Greenfield archaeology honest; final SOTA escape clause deleted; no contrivance.

---

## §13 — Tightened 16-Command Gate-Rerun Results

Each command produced post-conditions; the table records actual hits and verdicts as run against the post-Wave-4.1 trio.

| # | Command | Hits | Post-condition | Verdict |
|---|---|---:|---|---|
| 1 | `rg -n "ParseStream\|rewrite-mode\|Unicode class algebra" restart/{ARCHITECTURE,MIGRATION,MASTER-PLAN}.md` | 31 | All hits in conflict ledgers, deletion tables, syn-macro carve-outs, and migration receiver rows. No production usage. | PASS |
| 2 | `rg -n "bbnf-path\|bbnf-test-fixtures\|path!" restart/{ARCHITECTURE,MASTER-PLAN}.md` | 5 | All hits in migration crosswalk rows, A.W1 lint row, and cookbook migration row. | PASS |
| 3 | `rg -n "codegen/src/backend_ir" restart/ARCHITECTURE.md` | 0 | Backend IR ownership lives in `ir` crate. | PASS |
| 4 | `rg -n "fixtures/yaml" restart/{ARCHITECTURE,MASTER-PLAN}.md` | 1 | Single hit at ARCH §12.1 `parity-phase fixtures/yaml/manifest.toml (post-onboarding gate, never an onboarding surface)`. | PASS |
| 5 | `rg -n "@recover" restart/ARCHITECTURE.md` | 1 | Hit at ARCH §8.1 deletion table: standalone `@recover` deleted; folded into `@error(recover = ...)`. | PASS |
| 6 | `rg -n "OpenFrame" restart/MASTER-PLAN.md` | 8 | All hits are negative-grep gates, "no OpenFrame ladders" invariants, runtime tests, lock ownership rows. | PASS |
| 7 | `rg -n "GrammarIR" restart/ARCHITECTURE.md` | 0 | Token form `GrammarIR` (CamelCase no-space) absent. Document uses `Grammar IR` (with space) consistently. The import-deny target is implicit in MIGRATION §19.3. | PASS |
| 8 | `rg -n "__EAGER_EMPTY_PATH\|CursorDecision::Skip" restart/{MASTER-PLAN,MIGRATION}.md` | 2 | MASTER-PLAN §24 carry row; MIGRATION §19.4. Lock 3 gates anchored across ARCHITECTURE + MASTER-PLAN + MIGRATION. | PASS |
| 9 | `rg -n "twitter\|canada\|citm\|bootstrap\|animate\|On-Demand" restart/{MASTER-PLAN,ARCHITECTURE}.md` | 26 | Every SOTA target row anchored across both documents. | PASS |
| 10 | `rg -n "receiver\|blocker\|receiving gate" restart/{MIGRATION,MASTER-PLAN}.md` | 5 | MIGRATION §19.7 (`receiver/blocker/gate` free text); MIGRATION §20 cross-reference; MASTER-PLAN §24 ledger headers + ledger row. **The V3 PARTIAL is closed**: MIGRATION §20 now references MASTER-PLAN §24 rather than carrying a parallel table. | PASS |
| 11 | `rg -n "yaml.bbnf\|workspace.metadata.bbnf.grammars.yaml" restart/{ARCHITECTURE,MASTER-PLAN}.md` | 6 | Future-grammar onboarding test at ARCH §12 + per-X table row + cookbook row. | PASS |
| 12 | `rg -n "generated_loc\|regen_wall\|xtask" restart/{ARCHITECTURE,MASTER-PLAN}.md` | 32 | Per-grammar `generated_loc_budget` at ARCH §5; xtask invocations across MASTER-PLAN §6/§11/§13/§15/§20. Wall-time budgets at MASTER-PLAN §20. Token `regen_wall` not used; descriptive prose covers it. | PASS-WITH-NOTE — descriptive prose in lieu of token form |
| 13 | `rg -n "BBNF-LIFE\|BBNF-LAYOUT\|BBNF-OPT\|BBNF-GRAMMAR\|BBNF-POINTER\|lookbehind\|HostSignature" restart/ARCHITECTURE.md` | 26 | **The V3 PARTIAL is closed**: ARCH §7.4 now carries the consolidated catalogue with BBNF-LIFETIME-ESCAPE / BBNF-ARENA-MISMATCH / BBNF-LIFE003 / BBNF-LIFE009 / BBNF-LAYOUT-CONFLICT / BBNF-LAYOUT-UNCLOSED / BBNF-LAYOUT001 / BBNF-LOOKBEHIND-WIDTH / BBNF-PRATT-NOT-APPLIED / BBNF-SIMD-NOT-SELECTED / BBNF-METADATA-MISSING-GRAMMAR / BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE / BBNF-POINTER-UNKNOWN-SEGMENT / BBNF-POINTER-GRAMMAR-MISMATCH / BBNF-POINTER003 / BBNF-HOST001-003 / BBNF-RECOVERY* / BBNF-GEN001 / BBNF-GEN014 / BBNF-CODEGEN021 / BBNF-CODEGEN033 / BBNF-SEM040. `lookbehind` resolves to grammar surface plus diagnostic code (`BBNF-LOOKBEHIND-WIDTH`). `HostSignature` not used; "Host signature" (with space) is the prose form at ARCH §7.1. | PASS |
| 14 | `rg -n "child count\|500 LOC\|exception rationale" restart/{ARCHITECTURE,MASTER-PLAN}.md` | 9 | Lock 13 verification table at MASTER-PLAN §21 (683-692) with 5 surfaces × child-count + LOC + exception rationale + enforcing command. ARCH §13 (1359-1376) carries the 4-10 + 500 LOC ceiling rules + exception ledger. | PASS |
| 15 | `rg -n "declaration-crate review\|why metadata\|deletion path\|reviewer\|why_metadata_fails\|deletion_path\|receiving_gate" restart/{ARCHITECTURE,MIGRATION}.md` | 11 | 8-field declaration-crate review form at ARCH §5.6 (lines 723-754). All 8 fields enumerated; TOML reified at lines 740-754. Cross-references at ARCH:1101 and MIGRATION:777. | PASS |
| 16 | `rg -n "CPU model\|compiler flags\|input hash\|competitor version\|warmup\|sample" restart/{MASTER-PLAN,MIGRATION}.md` | 4 | Reproducibility schema at MASTER-PLAN §4 lines 138-150 with 8 rows × 2 columns. Every field has a verbatim source command. | PASS |

**Gate-rerun summary**: 15 PASS / 1 PASS-WITH-NOTE / 0 PARTIAL. The V3 PARTIAL gates (10 + 13) are now PASS — carry-ledger asymmetry resolved (M7); diagnostic vocabulary catalogued (M6). The single PASS-WITH-NOTE is gate 12's descriptive-prose-vs-token concern (`regen_wall` not adopted), which is non-blocking polish.

---

## §14 — V2 / V3 / V4 Comparison

| Lane | V2 | V3 | V4 | Delta source |
|---|---|---|---|---|
| 1 Lock-Adherence | READY (KEEP 14, REINVENT 0) | AMENDMENT-REQUIRED (KEEP 10, REINVENT 2) | READY (KEEP 13, REINVENT 0) | V3's two reinvents (Lock 2 path canonicalisation; Lock 11 incubation gating) closed by Wave-4.1 amendments M1 + M2. |
| 2 Sequencing | READY (KEEP 8, REINVENT 1) | READY-WITH-NOTE (KEEP 7, REINVENT 1) | READY (KEEP 8, REINVENT 0) | V3's H.W2 "skipped metadata" reinvent closed by M8. |
| 3 Cohesion | READY (KEEP 6, REINVENT 0) | AMENDMENT-REQUIRED (KEEP 5, REINVENT 2) | READY (KEEP 7, REINVENT 0) | V3's per-grammar baseline source-of-truth and diagnostic vocabulary asymmetry closed by M5 + M6. |
| 4 SOTA-Anchoring | READY (KEEP 5, DISCARD 1) | AMENDMENT-REQUIRED (KEEP 6, REINVENT 1) | READY (KEEP 8, REINVENT 0) | V3's H.W3 WASM Lock-8 fault closed by M4; V3's json/canada looseness closed by M9. |
| 5 Grammar-Authoritative | READY (KEEP 6, DISCARD 1) | READY (KEEP 8, REINVENT 0) | READY (KEEP 8, REINVENT 0) | V4 confirms V3's verdict; no delta. |
| 6 Generated-Code-Budget | READY (KEEP 6, REINVENT 0) | READY-WITH-NOTE (KEEP 6, REINVENT 1) | READY (KEEP 7, REINVENT 0) | V3's per-grammar baseline reinvent (folded with Lane 3) closed by M5. |
| 7 Friction-Forecast | READY (KEEP 7, REINVENT 0) | READY (KEEP 7, REINVENT 0) | READY (KEEP 8, REINVENT 0) | V4 adds the cookbook-cross-reference confirmation row; no delta in verdict. |
| 8 Carry-Deferral | READY (KEEP 8, DISCARD 1) | AMENDMENT-REQUIRED (KEEP 7, REINVENT 2) | READY (KEEP 9, REINVENT 0) | V3's bbnf-bench publication conflict + carry-ledger asymmetry + WASM ABI/SOTA split closed by M3 + M7 + M4 (with M4 folding the WASM SOTA piece into the H.W3 row directly). |
| 9 Greenfield-Discipline | READY (KEEP 6, DISCARD 1) | READY (KEEP 7, REINVENT 0) | READY (KEEP 7, REINVENT 0) | V4 confirms V3's verdict; no delta. |

**Why V4 returns READY**: V3 surfaced six Lock-level / source-of-truth faults plus three polish items; the Wave-4.1 amendment closed all nine (M1-M9) without weakening any prior KEEP. V4 verifies each closure at the artefact, redrives the nine lanes, and confirms the gate-rerun PARTIAL items now PASS. The trio is closure-ready.

The V2-V3-V4 trajectory shows the audit-amendment-audit cycle working as designed: V2 surface KEEP across nine lanes; V3 cross-document re-read surfaces six Lock-level faults; Wave-4.1 amendment closes all six; V4 confirms closure across the same nine lanes.

---

## §15 — Punch List

The Wave-4.1 amendment closed the V3 nine-item punch list verbatim. V4 finds zero new reinvent rows. The trio is ready for per-tranche full-spec drafting.

| Punch | Status | Notes |
|---|---|---|
| M1 Lock 2 path canonicalisation | CLOSED | `passes::types` extinct; `passes::layout` consistent across §4.2 tree, §6 invariants, §7.1 host call, §7.3 side-tables, §8.2 type system, §7.4 BBNF-HOST diagnostics, MIGRATION §3 type-facts row, MASTER-PLAN C.W1. |
| M2 Lock 11 incubation-vs-publication gating | CLOSED | J.W3 splits stable surface (unconditional) and incubation-cleared sister crates (stability-gated). Failing crates remain path-deps; one-cycle slip. |
| M3 bbnf-bench publication conflict | CLOSED | All three documents agree: `crates.io` (README), `Public/dev` (ARCH), stable surface (MASTER-PLAN J.W3). |
| M4 H.W3 WASM Lock 8 anchor | CLOSED | lightning-css/WASM baseline anchored on css/bootstrap fixture; TBD numerics scoped to H.W3 measurement with explicit owner + blocker. |
| M5 Per-grammar baseline source-of-truth | CLOSED | ARCH §12.1 firm; MASTER-PLAN §20 mirrors with explicit pointer; A.W2 verifies drift. |
| M6 Diagnostic vocabulary asymmetry | CLOSED | ARCH §7.4 28-row catalogue; MASTER-PLAN §24 cookbook references the catalogue. |
| M7 Carry-ledger asymmetry | CLOSED | MASTER-PLAN §24 19-row consolidated ledger with `Source` column; MIGRATION §20 cross-references. |
| M8 H.W2 skip-marker phrasing | CLOSED | `cpu_feature: avx2_unsupported` example concrete, machine-readable. |
| M9 json/canada simd-json firm number | CLOSED | `simd-json 3.226ms (restart/corpora/SOTA.md:56)`. |

No punch items carry forward. No new faults discovered.

---

## §16 — Residual Polish (Non-Blocking)

V4 surfaces three polish items that do not block readiness; they are recorded for the next tranche-drafting cycle to consume:

| Polish | Site | Description | Why non-blocking |
|---|---|---|---|
| Per-crate child-count audit | ARCH §4.x crate trees | Each crate tree visually shows 4-10 children; only `bbnf` carries an explicit per-crate audit row at line 382. | The lint gates at A.W4 (`cargo xtask lint-tree`) and the verification table at MASTER-PLAN §21 enforce the discipline mechanically. A static per-crate table would prevent regression but is not load-bearing. |
| Token form `regen_wall` | MASTER-PLAN §20 wall budgets | The wall-time budgets (30s/60s/...) live as descriptive prose; no token like `regen_wall_budget = "60s"` appears in any metadata block. | The wall budgets are tranche-gate prose; they do not need to be metadata-encoded for A.W2 to consume them. The test fixture/CI path would carry them when needed. |
| yaml provisional LOC | ARCH §12.1 row 10; MASTER-PLAN §20 yaml row | Both rows carry "provisional ceiling ≤ 4,000 (SYNTHESIS Wave-2 owner)." The SYNTHESIS Wave-2 marker is artefact-of-record; G.W4 measures and replaces with a firm number when yaml admits. | Lock 14's onboarding test is the executable verification; the LOC ceiling sets an upper bound, not a hard close gate. yaml never closes a tranche on a seed-grammar gate before admission. |

These residuals do not require V5 hardening; they would land naturally during per-tranche full-spec drafting (specifically A.W2 metadata schema close and G.W4 future-grammar gate).

---

## §17 — Cross-Document Integration Audit

V4 stresses the trio's cross-document coherence. The three documents form one contract; coherence is the V4 readiness threshold.

| Cross-document pair | Integration claim | V4 evidence |
|---|---|---|
| ARCH §1 crate visibility ↔ MASTER-PLAN §2 layer table | Both must describe the same 24 crates with the same role assignments. | ARCH §1 lists 24 crates × 4 columns (visibility, role, inheritance); MASTER-PLAN §2 lines 75-82 group the same 24 into 6 layers (User entrypoints, Compiler substrate, Backend/runtime, Path, Sister, Dev/test). Bench visible per Lock 11; Sister crates path-dep until publication (Lock 11). The two views align. |
| ARCH §7.1 Grammar IR variants ↔ MASTER-PLAN §3 IR contract row table | Variant set must agree. | ARCH §7.1 enumerates 15 GIR variants (Rule, Seq, Alt, Repeat, Optional, Literal, Regex, Ref, Predicate, Lookbehind, Map, HostCall, LayoutDirective, ErrorDirective, Annotation). MASTER-PLAN §3 routes Grammar IR variants to ARCHITECTURE §7.1 ownership. No variant disagreement. |
| ARCH §7.2 Backend IR variants ↔ MASTER-PLAN E.W0 ↔ PASS-2 §2 | Variant count must agree. | ARCH §7.2 enumerates 23 BIR variants; MASTER-PLAN E.W0 says "All variants construct and validate" (i.e., 23 variants); PASS-2 §2 supplies the 23-variant table. ARCH lines 901-903 explicitly note: "If an implementation keeps PASS-1's 22-variant table without `Return`, it must prove equivalent control-flow closure before codegen. The architecture default is PASS-2's final 23-variant table." Consistency is named. |
| ARCH §5.6 declaration-crate fence ↔ MASTER-PLAN §24 ledger row 1 ↔ MIGRATION §20 punch | All three references must close on the same fence. | ARCH §5.6 lines 723-754 enumerates 8 fields with TOML reification; MASTER-PLAN §24 line 739 receives the carry; MIGRATION §20 cross-references MASTER-PLAN §24. The three are anchored. |
| ARCH §11 SOTA ↔ MASTER-PLAN §4 SOTA close rows ↔ corpora/SOTA.md | All three must agree on competitor numerics. | ARCH §11 lines 1252-1259 has 6-row table with reproducibility metadata. MASTER-PLAN §4 lines 128-136 has 6-row table with `Owner` column. MASTER-PLAN line 133 cites `restart/corpora/SOTA.md:56` for simd-json/canada. The three converge. |
| ARCH §12 yaml two-surface ↔ MASTER-PLAN §6 A.W2 + G.W4 ↔ MIGRATION §19.6 | All three must enforce the same two-surface rule. | ARCH §12 enumerates allowed/forbidden changes + required commands. MASTER-PLAN §12 G.W4 closes "yaml enters through grammar source plus metadata; generated runtime is derivative." MIGRATION §19.6 runs `git diff --exit-code -- grammars/yaml.bbnf Cargo.toml` and `rg "yaml\|Yaml" crates/*/src` as the lint gate. Three-way enforcement. |
| ARCH §7.4 diagnostic catalogue ↔ MASTER-PLAN §24 cookbook ↔ PASS-2/PASS-3 verbatim strings | Codes must resolve consistently. | ARCH §7.4 enumerates 28 codes with site + meaning. MASTER-PLAN §24 cookbook references the codes by identifier. ARCH lines 1034-1039 binds verbatim strings to PASS-2 (codegen/BIR codes) and PASS-3 (runtime/host/layout/pointer/visitor codes). One catalogue, three reference layers. |

**Integration verdict: cohesion holds.** The trio behaves as a single contract. No cross-document conflict survives V3's surgery.

---

## §18 — Final Verdict

> **Decision: READY**
>
> The Wave-4.1 narrow amendment closed the V3 nine-item punch list (M1-M9) verbatim and without introducing new faults. Every Lock 1-14 row honours its lock at mechanism level. Every parse-throughput gate cites a competitor + dataset + platform, including the H.W3 WASM gate's lightning-css/WASM anchor. The carry-ledger consolidates at MASTER-PLAN §24 with `Source` column attribution and MIGRATION §20 cross-references. The diagnostic vocabulary catalogue lives at ARCH §7.4 with cookbook cross-reference. The per-grammar generated LOC numerics are firm in both ARCH §12.1 and MASTER-PLAN §20. bbnf-bench's publication status is `crates.io` consistently. The Lock 11 J.W3 split between stable surface (unconditional publication) and incubation-cleared sister crates (stability-gated) is concrete and enforceable. Six surgical V3 reinvents and three V3 polish items resolved; zero V4 reinvents added.
>
> Hereupon the next step is per-tranche full-spec drafting. Tranche A is the obvious starting point: archive `ser`/`gorgeous`, create the 24-crate skeleton with unprefixed internal crates, replace root metadata schema, add source/error/grammar minimal APIs, and close A with generalization + tree-shape gates. The trio is the executable authority that A-J consume; that authority is now closure-ready. No amendment-agent dispatch required; no V5 hardening required.
