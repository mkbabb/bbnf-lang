# HARDENING-MASTER-PLAN-V3 — Independent Audit Of The Amended Trio

## §1 Target Identification

| Field | Value |
|---|---|
| Targets | `restart/ARCHITECTURE.md` (1359 lines), `restart/MIGRATION.md` (798 lines), `restart/MASTER-PLAN.md` (797 lines) |
| Combined corpus | 2,954 lines |
| Trio commits audited | `015317db` (Phase 2 synthesis baseline), `e4c4fee1` (Wave-2 baseline), `3a73f212` (Wave-2 amendment), `70378e46` (Wave-3 amendment) |
| V1 baseline | `restart/audit/hardening/HARDENING-MASTER-PLAN.md` (verdict AMENDMENT-REQUIRED, 16 punch items) |
| V2 baseline | `restart/audit/hardening/HARDENING-MASTER-PLAN-V2.md` (verdict READY across all nine lanes; 1 non-blocking residual) |
| V3 audit posture | Independent. The V2 readiness verdict carries no precedence; this audit redrives the nine lanes, runs the tightened 16-command gate-rerun, and only consults V2 in §6 for delta diagnosis. |
| Lanes applied | nine (full set; Lane 2 active because the trio is multi-wave) |

V3 is the most adversarial of the four cohort hardenings — the trio is the executable authority that downstream tranche full-spec drafting will consume. V2 is the prior reviewer of the same artefacts; consensus across V2/V3 is informative but neither is decisive against the locks.

---

## §2 Cohort Verdict — Per-Lane Table

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | AMENDMENT-REQUIRED | 10 | 2 | 0 | Lock 2 path naming is internally inconsistent — `passes::layout` is referenced as canonical but the tree at §4.2 lists `passes::types/` instead; Lock 11 incubation discipline at J.W3 publishes sister crates without API-stability gates. |
| 2 Sequencing-Discipline | READY-WITH-NOTE | 7 | 1 | 0 | B.W3 builds direct-builder shell before C.W2 produces ShapeFacts. Mitigated by C.W2's "B integration gap" recording, but the inversion deserves explicit naming. |
| 3 Cohesion | AMENDMENT-REQUIRED | 5 | 2 | 0 | MASTER-PLAN §20 says per-grammar baselines are "recorded at A.W2"; ARCHITECTURE §12.1 already declares numerics (21,503 / 107,138 / 14,088 / etc.). One source must give. Diagnostic vocabulary distributed: only MASTER-PLAN §24 carries cookbook codes; ARCHITECTURE owns the contract but enumerates none. |
| 4 SOTA-Anchoring | AMENDMENT-REQUIRED | 6 | 1 | 0 | H.W3 WASM gate "≤ 3x native cost" carries no competitor anchor. Lock 8 requires every parse-throughput gate to name a competitor. swc / lightning-css WASM / simd-json WASM are valid baselines. |
| 5 Grammar-Authoritative | READY | 8 | 0 | 0 | All Lock-14 greps return clean. Per-X 10×9 table at ARCHITECTURE §12.1 is the canonical authority. yaml two-surface proof intact. Declaration-crate fence intact (8 fields). |
| 6 Generated-Code-Budget | READY-WITH-NOTE | 6 | 1 | 0 | Wave-budget table at MASTER-PLAN §20 is solid; per-grammar baseline source-of-truth conflict (see Lane 3) carries here too. |
| 7 Friction-Forecast | READY | 7 | 0 | 0 | Cookbook + diagnostic ledger at MASTER-PLAN §24 is complete and well-cross-referenced. |
| 8 Carry-Deferral | AMENDMENT-REQUIRED | 7 | 2 | 0 | Three asymmetries: (a) carry-ledger lives only in MASTER-PLAN §24; MIGRATION §20 has parallel "Unresolved Migration Punch List" with disjoint columns and no cross-reference; (b) bbnf-bench publication status differs across README ("workspace-internal"), ARCHITECTURE ("Public/dev"), and MASTER-PLAN J.W3 (publish dry-run); (c) Lock 11 incubation-vs-publication gating is silent. |
| 9 Greenfield-Discipline | READY | 7 | 0 | 0 | Conflict ledger at ARCHITECTURE §0 is honest. OpenFrame archaeology cited. Registry deletion gate enforced. Final SOTA escape clause confirmed deleted. |

| Verdict class | Count |
|---|---:|
| KEEP | 63 |
| REINVENT | 9 |
| DISCARD | 0 |

**Final V3 decision: AMENDMENT-REQUIRED.** Six surgical punch items, none blocking the macro architecture, four blocking the locks. V2 returned READY across all nine lanes; V3 returns AMENDMENT-REQUIRED on four (Lanes 1, 3, 4, 8) and READY-WITH-NOTE on two (Lanes 2, 6). The deltas trace to specific locks V2 did not interrogate hard enough — Lock 2 (path canonicalisation), Lock 8 (every perf gate cites a competitor), Lock 11 (sister-crate incubation), and the source-of-truth discipline that no lock names but the precepts demand.

---

## §3 Lane 1 — Lock-Adherence

Lane standard: each of 14 locks honoured / violated-with-recommendation / silent-must-add. Particular foci per the V3 prompt: Lock 1 (tape + direct), Lock 2 (layout/LayoutFacts/passes::layout), Lock 3 (cursor + byte-skip), Lock 5 (IR + per-backend), Lock 8 (SOTA close), Lock 13 (child-count), Lock 14 (yaml two-surface).

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:21-22` | Lock 1 — tape + direct-to-struct union | Tape is the substrate, unioned with direct-to-struct. ParseStream is not a runtime concept; only `syn::ParseStream` survives in proc-macro code. | Resolves the prior 86.07% Vec<OpenFrame>::clone pathology by retiring the orthogonal codepath, not the tape name. Aligns with Lock 1's reframe at `restart/locks/14-LOCKS.md:34`. | None. | The amendment correctly distinguishes substrate-level identity from runtime concept. KEEP-confirmed. | KEEP |
| `restart/ARCHITECTURE.md:1133` | Lock 1 — OpenFrame absence | "Rollback is bounded and does not clone OpenFrame stacks." | Tape invariant binding; bench-testable (`restart/MIGRATION.md:727`). | Invariant table is in §9.1 only; no cross-link to MASTER-PLAN's tape consumer wave. | Cross-reference would tighten cohesion but is non-blocking. | KEEP |
| `restart/ARCHITECTURE.md:419-450, 435-442` ↔ `restart/ARCHITECTURE.md:983, 1066` | **Lock 2 — `passes::layout` canonicalisation** | Lock 2 names `passes::layout` as the canonical IR module. ARCHITECTURE §6 (line 792) routes type inference to `passes::types`. ARCHITECTURE §7.3 (line 983) routes `LayoutFacts` production to `passes::layout`. The `passes/src/` tree in §4.2 (lines 435-442) lists children `normalize/`, `types/`, `shapes/`, `recognizers/`, `extract/`, `bridge/` — **no `layout/` child**. | The narrative consistently names `passes::layout` as the public surface owning HM + bidirectional + CSP layout decisions. | The crate tree contradicts the narrative. Either `passes::types/` is the layout-pass directory (in which case Lock 2's prescribed name is violated) or `passes::layout/` must replace `passes::types/` in §4.2. | The challenge is not interpretive; the artefact is internally inconsistent. Tranche C cannot drop a `passes::layout/` directory without amending §4.2; cannot drop a `passes::types/` directory without amending §7.3 / §8.2. **REINVENT** at §4.2 — replace `types/` with `layout/`, or rename `layout` to `types` everywhere narrative; one source of truth. | REINVENT |
| `restart/ARCHITECTURE.md:792` | Lock 2 — passes::types vs passes::layout | "Type inference annotates Grammar IR; it does not mutate grammar syntax. `passes::types`" | Identifies the type-inference owner. | Conflicts with §7.3 / §8.2 which say `passes::layout` is the owner. | Lock 2 says HM/CSP is internal-subroutine to layout-lowering. Architecture §7.3 honours this; §6 does not. | REINVENT (folds into the §4.2 fix) |
| `restart/ARCHITECTURE.md:802-806` | Lock 3 — cursor + byte-skip gates | Three rows: `__EAGER_EMPTY_PATH` regression fixture; `CursorDecision::Skip` unit + VM replay; scanner fast-path span round-trip. | Mechanism-level binding; testable. Aligns with `restart/MASTER-PLAN.md:733` carry-ledger row. | None. | Lock 3 honoured. | KEEP |
| `restart/ARCHITECTURE.md:856` ↔ `MASTER-PLAN.md:163-171` | Lock 5 — IR + per-backend lower | Backend IR is the only lowerer input. Codegen never reads Grammar IR (§2 line 173). VM replays all BIR variants (`MASTER-PLAN.md:170`). | Mechanism-level boundary. Import-deny tests named at `ARCHITECTURE.md:965`. | The import-deny mechanism is named but not specified — what does the test actually check? `MIGRATION.md:715-718` uses `rg "GrammarIr\|GrammarIR\|grammar_ir" crates/codegen/src` plus `cargo test -p codegen backend_ir_only`. That is concrete enough. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:1201-1210` ↔ `MASTER-PLAN.md:128-136` | Lock 8 — SOTA close | Six rows: json/twitter (≤380µs), json/citm (≤750µs), json/canada (≤2.8ms), css/bootstrap (≤3.0ms), css/animate (≤1.6ms), simd/structural_scan (≥5 GB/s NEON, ≥7 GB/s AVX2). Each row binds competitor + dataset + platform + metadata. | Numeric, anchored, dual-document. | The H.W3 WASM row at `MASTER-PLAN.md:459` names "<= 3x native cost on M1 Pro Safari WASM runtime" — **no competitor anchor**. Lock 8 requires every parse-throughput gate to name a competitor's number on a specific platform. swc/WASM, lightning-css/WASM, simd-json/WASM are candidate anchors. | The Lock 8 violation is structural: a WASM gate must cite a WASM competitor. **REINVENT** — replace "≤ 3x native cost" with a WASM competitor's number on the same input fixture. | REINVENT |
| `restart/ARCHITECTURE.md:1305-1328` | Lock 13 — file/directory discipline | 4-10 children + 500 LOC handwritten ceiling + four exception classes (generated grammar/runtime, generated data tables, handwritten parser/lowerer/runtime over 500 LOC FORBIDDEN, >10 children only with tranche-local rationale). | Mechanism-level binding. Lint commands enumerated. | The §4.x crate trees do not carry a per-crate child count audit. bbnf has 8 (verified at §4.1 line 382). Other crates carry between 4 and 8 children visually but no table proves conformance. | The verification table at `MASTER-PLAN.md:683-689` covers the lint surface. Per-crate child-count audit can fold into A.W4 lint-tree, not the static doc. KEEP — but a per-crate table at §4.x would prevent regression. | KEEP |
| `restart/ARCHITECTURE.md:1259-1297` | Lock 14 — per-X authority + yaml two-surface | 10 grammars × 9 columns (Typed root, ValueRef shape, runtime files, Visitor + VisitTypes, path schema, fixture manifest, host route, generated LOC, declaration-crate status). | Single authoritative table; every "all extant grammars" claim resolves here. yaml row says fixtures are parity-phase only (post-onboarding gate). | The yaml LOC budget "0 → ≤ 4,000 (provisional; SYNTHESIS Wave-2 owner)" carries an unresolved-receiver pointer. The amendment-receiver is named ("SYNTHESIS Wave-2 owner"), but the resolution mechanism is silent: who measures? When does the provisional become firm? | The provisional clause is honest given pre-implementation status, but Lock 14's "future-grammar onboarding test" is the verification gate; the LOC ceiling sets the upper bound. KEEP — but G.W4's gate must measure and replace "provisional" with a firm number. | KEEP |
| `restart/ARCHITECTURE.md:1043-1052` | Lock 14 — input-normalization deletions | Five rows × four columns (rewrite-mode, Unicode set algebra, grammar-level `(?<=...)`, standalone `@recover`, per-grammar declaration crates). Every deletion has a closing gate. | Mechanism-level normalisation; matches PASS-1 §6 + PASS-3 §0 settled DISCARDs. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:524` ↔ `restart/README.md:31` | **Lock 11 — incubating-vs-publication gating** | J.W3 dry-runs `cargo publish` for `bbnf, bbnf-cli, bbnf-language-server, bbnf-bench, path, path-core, path-ts, parse-that, egraph, egraph-derive, csp-solver`. README §11 says "Path-deps for incubating sister crates"; egraph + csp-solver + parse-that are incubated until API stabilises. | Publication-readiness binding; matches README's listed crates.io candidates. | J.W3 publishes incubated sister crates without first proving API stability. Lock 11 explicitly gates publication on stability ("promote to registry once stable"). MASTER-PLAN does not name the stability proof. | The Lock-11 challenge is real: J.W3 should require an incubation-stability gate (e.g., "API has not changed across the prior tranche; downstream consumers compile against a frozen-version dry-run") before publishing egraph/csp-solver/parse-that. **REINVENT** at MASTER-PLAN.md:524 — split publication into "stable" (bbnf user-surface) and "incubation-cleared" (egraph/csp-solver/parse-that subject to stability gate). | REINVENT |
| `restart/README.md:38` ↔ `restart/ARCHITECTURE.md:43` ↔ `restart/MASTER-PLAN.md:524` | **bbnf-bench publication conflict** | README §2 line 38 lists `bbnf-bench` as `workspace-internal`. ARCHITECTURE §1 line 43 lists it as `Public/dev`. MASTER-PLAN J.W3 line 524 includes it in publish dry-run. Three documents disagree. | All three are aware of bbnf-bench. | The disagreement is not nominal — MASTER-PLAN J.W3 will exit gating if bbnf-bench cannot publish per README's "workspace-internal" status. | Resolution: pick one. If bench harness publishes, README §2 is wrong; if not, ARCHITECTURE/MASTER-PLAN are wrong. **REINVENT** — settle the bench publication status at one source. (Lane 8 carries this too as a carry-discipline item.) | REINVENT (folds with Lane 8) |
| `restart/MASTER-PLAN.md:660-678` | Lock 1-14 ownership table | 14 rows × 3 columns (Lock, Owner tranche, Close proof). | Every lock has a close proof. Aligns with HARDENING-CONSOLIDATED §3. | The Lock 11 row says "Sister crates remain generic and publishable" but does not say the sister-crate stability gate. | Folds with the Lock 11 fault above. | KEEP (with Lock 11 amendment) |

**Lane 1 verdict: AMENDMENT-REQUIRED.** KEEP 10 / REINVENT 2 / DISCARD 0. Two faults: Lock 2 path canonicalisation (passes::layout vs passes::types — §4.2 tree contradicts §6/§7.3/§8.2 narrative), Lock 11 incubation-vs-publication gating (J.W3 publishes without stability proof). The bbnf-bench cross-document conflict surfaces here too as a stability-of-publication issue.

---

## §4 Lane 2 — Sequencing Discipline

Lane standard: every wave produces an artefact with a same-wave or next-wave consumer. Substrate-first / consumer-later is the Era V failure mode; it is fault.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:294-300` | C.W0-C.W5 sequencing | C.W0 Grammar IR; C.W1 layout-internal TypeFacts → LayoutFacts; C.W2 ShapeFacts; C.W3 RecognizerFacts; C.W4 CSP/egraph bridge; C.W5 CostFacts → BIR builder. | Every wave's deliverable carries a consumer (C.W2 → direct-builder; C.W3 → E-owned BIR snapshots; C.W5 → E.W1 Backend IR). Aligns with V1 punch items 40 + 41 receivers. | C.W2 ShapeFacts has consumer "Direct-builder shell contract consumes ShapeFacts in a C fixture and records B integration gaps." But the direct-builder shell is built at B.W3 — earlier. | The inversion is mitigated: B.W3 substrate, C.W2 consumer-validation, C.W2 records B integration gaps. The Era V failure mode (substrate without consumer) is averted because C.W2 IS the consumer. **KEEP-WITH-NOTE** — the inversion is unusual but defensible. | KEEP |
| `restart/MASTER-PLAN.md:262` | B.W3 direct-builder shell | "Direct builder shell and tape identity hooks. Direct view borrows spans from tape." | B-tranche substrate; same-wave consumer = "Direct view borrows spans from tape" (i.e., B itself validates). | The shell is built before ShapeFacts (C.W2) exist. Direct-builder *contract* depends on ShapeFacts, but the *shell* (typed-borrow scaffolding) depends only on tape. | If the shell is purely tape-borrow scaffolding without ShapeFacts dependence, it stands alone. C.W2 then fills in the contract via fixture. **KEEP-WITH-NOTE** — but the trio should explicitly distinguish "shell" (B.W3, tape-only) from "contract" (C.W2, ShapeFacts-driven). Otherwise readers may infer C.W2 retrofits B.W3. | KEEP |
| `restart/MASTER-PLAN.md:329-331` | D.W0-D.W4 + Backend IR variants | D.W0 lookbehind; D.W1 generics; D.W2 `@host fn`; D.W3 chains; D.W4 `@error`/`@layout`/regex Unicode/rewrite rejection. | D produces extension parser + types. E.W0 (line 360) builds Backend IR enum "All variants construct and validate" — same-wave-after-D. | D extensions need BIR variants. The sequencing is D → E.W0 → E.W1 (BIR builder) → F (Rust lowerer). Three tranches deep, but ARCHITECTURE §7.1 lowering matrix shows lookbehind → SpeculativeAlt, etc. | E.W0's "all variants construct and validate" must include the variants D extensions need. Confirmed at `restart/ARCHITECTURE.md:874-900` (23 variants). KEEP. | KEEP |
| `restart/MASTER-PLAN.md:360-364` | E.W0-E.W4 sequencing | E.W0 BIR enum + validation; E.W1 GIR + side tables → BIR builder; E.W2 VM core; E.W3 VM full coverage; E.W4 lowerer trait + boundary. | Every wave consumes prior. | E.W4's "Codegen cannot import Grammar IR emitter logic" is a boundary check — uses Lock 5's import-deny. Consumer = F. | Clean. KEEP. | KEEP |
| `restart/MASTER-PLAN.md:391-396` | F.W0-F.W5 sequencing | F.W0 control-flow lowerer; F.W1 tape/direct emit; F.W2 host/chain/layout/error; F.W3 template + equality; F.W4 generated LOC budget; F.W5 nine-grammar regen. | F.W3 equality gate consumed by F.W5 regen. F.W4 budget tooling consumed by F.W5 ceiling check. | F.W2 says "Extension seed grammar compiles and runs" — depends on D extensions through E lowering. Three-tranche-deep dependency. | Acceptable per LESSONS-LEARNED §1-34 (wave boundaries by dependency). KEEP. | KEEP |
| `restart/MASTER-PLAN.md:455-461` | H.W0-H.W5 sequencing | H.W0 Pratt facts + PrattSpine; H.W1 SIMD facts; H.W2 platform dispatch; H.W3 WASM V1; H.W4 early JSON SOTA; H.W5 early CSS SOTA. | Each numeric early threshold (H.W4: ≤480µs/950µs/3.5ms; H.W5: ≤3.8ms/1.9ms) carries to final J.W1 threshold. | H.W3 WASM gate "≤ 3x native cost on M1 Pro Safari WASM runtime" — Lock 8 fault (covered in Lane 4). H.W2 "Platform-specific tests or skipped metadata" — what does "skipped metadata" mean? | "Skipped metadata" is unclear: does it mean the test is skipped on platforms without SIMD support, with metadata explaining the skip? If so, the wording is loose. **REINVENT** at H.W2 — clarify the skip mechanism. | REINVENT |
| `restart/MASTER-PLAN.md:520-526` | J.W0-J.W5 sequencing | Parity → SOTA → docs → publish → archive audit → close report. | Each wave consumes prior; J close depends on every earlier tranche. | J.W3 publishes incubated sister crates without stability gate (Lock 11 fault, Lane 1). | Folds with Lane 1 Lock 11. | KEEP (with Lock 11 amendment) |
| `restart/MASTER-PLAN.md:225-230` | A.W0-A.W4 sequencing | A.W0 archive ceremony; A.W1 24 crates; A.W2 metadata schema; A.W3 grammar parser; A.W4 generalization + tree-shape lint. | Each wave consumed by next. | V2 noted A.W4 "no hardcoded grammar dispatch" is a close gate, not a same-wave consumer for A.W3. | A.W4's lint gate consumes A.W3's grammar parser through cargo xtask binding. KEEP. | KEEP |

**Lane 2 verdict: READY-WITH-NOTE.** KEEP 7 / REINVENT 1 / DISCARD 0. The sole REINVENT is H.W2's loose "skipped metadata" phrasing. The B.W3 / C.W2 substrate-shell vs consumer-contract inversion is mitigated and defensible.

---

## §5 Lane 3 — Cohesion

Lane standard: every claim verifiable from artefacts the trio produces or cites; no orphan claims; no orphan deliverables.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:1259-1297` | Per-X 10×9 authority table | 10 grammars × 9 columns. | Single authoritative consumer; every "all extant grammars" claim resolves here. | yaml LOC "provisional" pointer is honest given pre-implementation status. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:1273-1281` ↔ `restart/MASTER-PLAN.md:638-649` | **Per-grammar generated LOC source-of-truth conflict** | ARCHITECTURE §12.1 carries numerics (`bbnf` 21,503 → 21,933; `css_l4` 107,138 → 109,281; `google_sheets` 14,088 → 14,370; `json` 3,500 → 3,570; `math` 871 → 888; etc.). MASTER-PLAN §20 says "baseline recorded at A.W2" with no numerics. | ARCHITECTURE provides the contract; MASTER-PLAN provides the receiving wave. | The two contradict. Either the numerics are firm (ARCHITECTURE is right; MASTER-PLAN is descriptively misleading) or the numerics are projections (MASTER-PLAN's A.W2 measurement is the actual ground truth; ARCHITECTURE's numerics are forecasts). | The reader cannot tell. **REINVENT** at MASTER-PLAN §20 — either drop "recorded at A.W2" and reference ARCHITECTURE §12.1 as the baseline source, or flag ARCHITECTURE §12.1 numerics as "projected; firm baseline at A.W2." Source-of-truth must be unambiguous. | REINVENT |
| `restart/MIGRATION.md:113-165` | Mixed-fate crosswalk | 30 rows × 6 columns. Approximate file counts; refines at A.W2. | Resolves V1 punch item 42. | "approximate" qualifier is honest; refinement gate bound (A.W2). | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:683-689` | Lock 13 verification table | 5 surfaces × child-count + LOC + exception rationale + enforcing command. | Mechanism-level lint gate. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:140-150` | Benchmark reproducibility schema | 8 rows × 2 columns. Every field has a verbatim source command. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:1042-1052` | Input-normalization-deletions table | 5 rows × 4 columns (Surface, Status, Routed substrate, Closing gate). | Resolves V1 punch items 9 + 10. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:751-759` ↔ `restart/ARCHITECTURE.md` (no enumeration) | **Diagnostic vocabulary asymmetry** | MASTER-PLAN §24 cookbook table enumerates `BBNF-POINTER-UNKNOWN-SEGMENT`, `BBNF-POINTER-GRAMMAR-MISMATCH`, `BBNF-LIFETIME-ESCAPE`, `BBNF-ARENA-MISMATCH`, `BBNF-VISITOR-MUTATION-OUTSIDE-ENTRY`, `BBNF-LAYOUT-CONFLICT`, `BBNF-LAYOUT-UNCLOSED`, `BBNF-PRATT-NOT-APPLIED`, `BBNF-SIMD-NOT-SELECTED`, `BBNF-METADATA-MISSING-GRAMMAR`, `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE`. ARCHITECTURE.md carries only `BBNF-LOOKBEHIND-WIDTH` (line 1050). | MASTER-PLAN's cookbook surface is complete. | ARCHITECTURE is the executable contract. The diagnostic vocabulary that ARCHITECTURE §7.1 produces (HostCall, LayoutDirective, ErrorDirective) and §7.2 consumes (CallHost, LayoutPush/Pop, ErrorRecover) does not enumerate the diagnostic codes the cookbook references. | A reader of ARCHITECTURE alone learns the IR contract but not the diagnostic vocabulary. ARCHITECTURE should enumerate the diagnostic codes alongside the IR contract — at least as a §7.4 or §10.4 table. **REINVENT** at ARCHITECTURE — add a diagnostic-vocabulary table that the cookbook references. | REINVENT |

**Lane 3 verdict: AMENDMENT-REQUIRED.** KEEP 5 / REINVENT 2 / DISCARD 0. Two source-of-truth issues: per-grammar LOC numerics (ARCHITECTURE has firm, MASTER-PLAN says "to be measured"); diagnostic vocabulary (MASTER-PLAN has 11 codes, ARCHITECTURE has 1).

---

## §6 Lane 4 — SOTA Anchoring

Lane standard: every parse-throughput gate cites a competitor + dataset + platform per Lock 8. Non-throughput engineering gates must NOT claim Lock 8 honour.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:128-136` | SOTA close rows | 6 rows × 5 columns (Row, Competitor baseline, bbnf target, Platform, Owner). | Every row inlines competitor + dataset + platform + bbnf target. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:131` | json/twitter row | sonic-rs 436µs; simd-json 424µs → ≤ 380µs M1 Pro. | Aligns with `restart/corpora/SOTA.md:50-89`. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:133` | json/canada row | sonic-rs 3.144ms; simd-json comparable → ≤ 2.8ms M1 Pro. | "comparable" is loose for simd-json; actual SOTA.md may have specific number. | The "comparable" qualifier weakens the row. SOTA.md has the firm number. | **REINVENT** — replace "simd-json comparable" with the actual measurement, or strike the simd-json side from the row if it's not anchorable. | REINVENT |
| `restart/MASTER-PLAN.md:135` | css/animate row | lightning-css 1.97ms → ≤ 1.6ms M1 Pro. | Anchored. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:136` | simd/structural_scan row | simdjson On-Demand ~7 GB/s x86 AVX2; ~5 GB/s M-series → ≥ 5 GB/s M-series, ≥ 7 GB/s AVX2. | Anchored, dual-platform. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:459` | **H.W3 WASM gate** | "WASM package parses seed grammar at <= 3x native cost on M1 Pro Safari WASM runtime; metadata records WASM runtime, host browser, and bbnf commit." | Numeric mechanism + metadata. | **No competitor anchor.** Lock 8 requires every parse-throughput gate to name a competitor's number. swc/WASM, lightning-css/WASM, simd-json/WASM are valid baselines. "≤ 3x native cost" is self-referential — bbnf's own native parse drives the ceiling. | The WASM gate must cite a WASM competitor. **REINVENT** — replace "≤ 3x native cost" with a specific WASM competitor's number (e.g., "≤ swc/WASM bootstrap parse on M1 Pro Safari" with concrete µs / ms). | REINVENT |
| `restart/MASTER-PLAN.md:138-150` | Reproducibility schema | 8 fields per row. | Mechanism-level binding. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:522` | J.W1 final SOTA gate | "JSON/CSS/SIMD targets met; misses require amendment before close." | Final SOTA escape clause deleted; misses block close. | None. | KEEP. | KEEP |

**Lane 4 verdict: AMENDMENT-REQUIRED.** KEEP 6 / REINVENT 1 / DISCARD 0. Two faults: H.W3 WASM gate carries no competitor anchor (Lock 8 violation); json/canada row's "simd-json comparable" is loose (could resolve to a firm number from SOTA.md).

---

## §7 Lane 5 — Grammar-Authoritative Discipline

Lane standard: zero proposed match-arms in generic crates; per-X tables for every "all-grammars" claim; future-grammar onboarding test honoured; per-grammar code lives only in workspace metadata or `@host fn`.

Verification commands (run as part of the V3 audit):

```
$ rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' \
    restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md
(zero matches)

$ rg -ni 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' \
    restart/ARCHITECTURE.md restart/MIGRATION.md restart/MASTER-PLAN.md
restart/ARCHITECTURE.md:326     (API leakage rule, mechanism-level forbidden example)
restart/MIGRATION.md:293        (rg gate negative grep)
restart/MIGRATION.md:696        (rg gate negative grep)
(all matches are mechanism-level negative-grep gates, not match-arm hardcoding)
```

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:1259-1297` | Per-X 10×9 authority table | 10 grammars × 9 columns. | Single authoritative table; resolves every "all extant grammars" claim. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:1043-1052` | Input-normalization deletions | 5 deletions × routed substrate × closing gate. | Mechanism-level deletions. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:1221-1257` | Future grammar onboarding test (yaml) | Two surfaces only: `grammars/yaml.bbnf` + `[workspace.metadata.bbnf.grammars.yaml]`. Forbidden changes enumerated. Required commands enumerated. | Mechanism-level. Resolves Lock 14 verification gate. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:723-754` | Declaration-crate fence (8 fields) | Reason / Owner / Why metadata fails / Why `@host fn` fails / Declaration location / No generic import proof / Deletion path / Reviewer. Reified as TOML. `allow=false` is default for all 9 extant grammars. | Mechanism-level fence. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:716` | Lock 14 lint risk row | Risk: "Old grammar registries reappear in new crates"; mitigation: `cargo xtask lint-no-hardcoded-grammars` enforced at A.W4, G.W4, J.W4 with rg gate. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/MIGRATION.md:692-700` | §19.1 generalization gate | Three rg gates: parser type names, hardcoded registries, manifest table. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:322-334` | API leakage rules | 6 forbidden examples → allowed replacements. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:589-720` | Workspace metadata schema | `[workspace.metadata.bbnf.grammars.<name>]` is the per-grammar declarative surface. Schema rules enforced by `grammar::metadata` + `pipeline::workspace`. | Mechanism-level. | The `[workspace.metadata.bbnf.grammars.json.host]` block carries only `registry = "default"` and `allow_declaration_crate = false`. The host route in §12.1 says "metadata + numeric/string host fns from `host::primitives`." How does metadata declare which primitives? | The metadata schema may need enrichment, but the `@host fn` directive plus `[workspace.metadata.bbnf.host_fns]` covers the surface. KEEP — but the host metadata details are underspecified for grammars without `@host fn` blocks. | KEEP |

**Lane 5 verdict: READY.** KEEP 8 / REINVENT 0 / DISCARD 0. Lock 14 grep gates clean. Per-X authority intact. Declaration-crate fence intact. Future-grammar onboarding test intact.

---

## §8 Lane 6 — Generated-Code + LOC Budget

Lane standard: per-tranche LOC budget; xtask regen-cycle wall budget; per-grammar LOC delta projection; per-wave gate.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:617-629` | §20 generated LOC trajectory | 9 stages × 2 columns. Wall-time budgets per stage (30s / 60s / 60s / 90s / 120s / 150s / 150s / 180s). | Per-wave wall budget. | F.W4's "wall under 60s" is below the F.W5 90s — descending then ascending. Why? | F.W4 is the budget tooling (no regen yet); F.W5 regenerates all 9 grammars. The numbers are defensible. KEEP. | KEEP |
| `restart/MASTER-PLAN.md:638-649` ↔ `restart/ARCHITECTURE.md:1273-1282` | **Per-grammar baseline source of truth** | MASTER-PLAN says "baseline recorded at A.W2" with no numerics. ARCHITECTURE §12.1 has firm numerics (21,503 / 107,138 / etc.). | Both reference the same per-grammar set. | The baseline is either firm (ARCH numerics) or to-be-measured (MP A.W2). One source must give. | **REINVENT** — fold with Lane 3's source-of-truth amendment. | REINVENT (folds with Lane 3) |
| `restart/MASTER-PLAN.md:650-658` | Budget enforcement rows | 4 rows × 2 columns (Scope, Gate). | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:1212-1219` | Generated LOC budget rows | 4 rows × 2 columns. | Aligns with PASS-2 +2% ceiling. | None. | KEEP. | KEEP |
| `restart/MIGRATION.md:583-604` | LOC trajectory by tranche | 11 rows × 3 columns (Phase, Expected movement, Gate). | Tranche-level expectations. | "The steady-state goal is not 'least LOC.'" — strong precept, but not a measurable gate. | KEEP — precept is correct; gates are elsewhere. | KEEP |
| `restart/MASTER-PLAN.md:115` | Generated LOC budget hard gate | "Enforce PASS-2 +2 percent budget. F/H/J." | Mechanism-level binding. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:686` | Generated grammar/runtime files exception | "Not bound by 4-10. LOC excepted; budget rows in §20 govern." | Aligns with Lock 13 exception ledger. | None. | KEEP. | KEEP |

**Lane 6 verdict: READY-WITH-NOTE.** KEEP 6 / REINVENT 1 / DISCARD 0. Per-grammar baseline source-of-truth conflict (Lane 3) carries here; otherwise the wave budgets and exception classes are well-specified.

---

## §9 Lane 7 — Friction Forecast

Lane standard: where users / grammar authors hit the proposed API and do not understand it; specify user, mental model, confusion point, cookbook, verbatim error message.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:751-759` | §24 Cookbook + migration friction rows | 7 rows × 6 columns (Friction, Target user, Mental model, Confusion point, Artefact, Diagnostic). | Mechanism-level cross-cut: every row carries target user + mental model + confusion point + cookbook receiver + diagnostic code. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:753` | `pointer!` + `select!` row | `BBNF-POINTER-UNKNOWN-SEGMENT` + `BBNF-POINTER-GRAMMAR-MISMATCH`. Mental model: path expression is checked against grammar's path schema at compile time. | Aligns with PASS-3 §3 path commitments. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:754` | Lifetime constructors row | `BBNF-LIFETIME-ESCAPE` + `BBNF-ARENA-MISMATCH`. Mental model: parse borrows / parse_in into arena / parse_owned allocates. | Aligns with PASS-3 §2 lifetime commitments. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:755` | Visitor mutation row | `BBNF-VISITOR-MUTATION-OUTSIDE-ENTRY`. Mental model: read-write visitor only; direct field writes forbidden. | Aligns with PASS-3 visitor contract. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:756` | Layout errors row | `BBNF-LAYOUT-CONFLICT` + `BBNF-LAYOUT-UNCLOSED`. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:757` | Pratt/SIMD decisions row | `BBNF-PRATT-NOT-APPLIED` + `BBNF-SIMD-NOT-SELECTED`. Mental model: Pratt and SIMD are auto-detected; metadata can disable but not force. | Aligns with Lock 10. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:759` | Adding yaml row | `BBNF-METADATA-MISSING-GRAMMAR` + `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE`. Mental model: two surfaces only. | Aligns with Lock 14 future-grammar onboarding test. | None. | KEEP. | KEEP |

**Lane 7 verdict: READY.** KEEP 7 / REINVENT 0 / DISCARD 0. The cookbook + diagnostic ledger is complete. The diagnostic vocabulary asymmetry surfaced in Lane 3 (codes only enumerated in MASTER-PLAN, not ARCHITECTURE) does not reduce the friction-forecast surface; it is a cohesion fault, not a friction fault.

---

## §10 Lane 8 — Carry & Deferral Audit

Lane standard: every "deferred to" / "carries to" / "future" / "TBD" / "user adjudicates" must name receiver + blocker + receiving gate.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/MASTER-PLAN.md:729-745` | §24 Carry + friction ledger | 14 rows × 4 columns (Item, Receiver, Blocker, Gate). | Every row triple-complete. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:740` | PASS-1 reconciliation row | Receiver C/D; Blocker GIR/side-tables/BBNF drift; Gate "Architecture §7 schema matches PASS-1 §2 enum; reconciliation noted in close report." | Cross-pass reconciliation explicit. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:742` | Publication readiness row | Receiver A/J; Blocker package names + dry-run; Gate `cargo xtask publish --dry-run` clean. | Mechanism-level binding. | bbnf-bench publication status conflict (Lane 1) — README "workspace-internal" vs ARCHITECTURE "Public/dev" vs MASTER-PLAN "publish dry-run." | The carry ledger inherits the conflict. **REINVENT** — settle bbnf-bench status one place; carry ledger then aligns. | REINVENT (folds with Lane 1) |
| `restart/MASTER-PLAN.md:739` | BD parity row | Receiver F/J; Blocker BD parity matrix not run; Gate `cargo xtask parity --all` matrix passes for nine seed grammars. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:744` | path-ts schema row | Receiver G; Blocker schema does not derive from path-core; Gate path-ts and path consume identical path-core AST. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:745` | WASM ABI row | Receiver H/J; Blocker WASM exported ABI not specified for V1; Gate H.W3 records exported function names; J.W3 dry-run includes WASM binding. | Mechanism-level. | Folds with Lane 4 H.W3 Lock 8 fault — the carry says "WASM ABI not specified" but does not require a WASM competitor anchor. | The ABI specification is one carry; the SOTA anchor is another. **REINVENT** — split the WASM carry into ABI (already covered) and SOTA (currently missing). | REINVENT |
| `restart/MIGRATION.md:777-786` ↔ `restart/MASTER-PLAN.md:729-745` | **Carry-ledger asymmetry** | MASTER-PLAN §24 has 14-row carry ledger with Receiver/Blocker/Gate columns. MIGRATION §20 has 8-row "Unresolved Migration Punch List" with Item/Owner tranche/Constraint columns. **No cross-reference between the two.** | Two separate ledgers cover different concerns: MASTER-PLAN tracks cross-tranche carries; MIGRATION tracks migration-implementation punch items. | The two have overlapping items: declaration-crate review form (MP §24 row 1; MIG §20 row 2); WASM exported ABI (MP §24 row 14; MIG §20 row 4); benchmark host hardware profiles (MP §24 row 5; MIG §20 row 5); Lock 3 cursor gates (MP §24 row 3; MIG §20 row 8). The duplication is not flagged; the two could drift. | **REINVENT** — consolidate the two ledgers OR cross-reference them explicitly. The reader should see one source of carry-truth, or two with explicit pointers. | REINVENT |
| `restart/MIGRATION.md:626-633` | Branch/tag operation routing floor | 6 rows × 4 columns. | Resolves V1 punch item 43. | None. | KEEP. | KEEP |
| `restart/MIGRATION.md:761-769` | §19.7 Diagnostic and carry proof | "migration-carry --check" — does the tool exist? | Mechanism named. | The xtask `cargo xtask migration-carry --check` is referenced; nowhere else specified. The tool's contract is silent. | The tool is implementation-detail for the migration phase but the mechanism is opaque. KEEP-WITH-NOTE — A.W4 must specify what the tool checks. | KEEP |
| `restart/MASTER-PLAN.md:730` | Declaration-crate escape valve row | Receiver A/D; Blocker review form missing; Gate metadata validator rejects partial fence. | Aligns with ARCH §5.6 8-field fence. | None. | KEEP. | KEEP |

**Lane 8 verdict: AMENDMENT-REQUIRED.** KEEP 7 / REINVENT 2 / DISCARD 0. Two faults: bbnf-bench publication conflict (carries from Lane 1); carry-ledger asymmetry between MASTER-PLAN §24 and MIGRATION §20.

---

## §11 Lane 9 — Greenfield Discipline

Lane standard: no quick solutions / no workarounds / no legacy uncontested / no contrivance / idiomatic gestalt / architectural transpositions mandatory.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `restart/ARCHITECTURE.md:21-30` | Conflict ledger | 10 rows × 4 columns. Every superseded position has a settled resolution. | Honest archaeology; matches HARDENING-CONSOLIDATED §3 row 6. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:1043-1052` | Input-normalization deletions | 5 deletions, every closing gate verifiable. | Mechanism-level. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:227` | A.W1 package naming | "unprefixed internal crates: `path`, `path-core`, `path-ts`, `test-fixtures`, `passes`, `simd-scan`, `egraph`, `csp-solver`; user-facing crates retain `bbnf-` prefix." | Resolves HARDENING-CONSOLIDATED §3 conflict #3. | None. | KEEP. | KEEP |
| `restart/MASTER-PLAN.md:524` | J.W3 publication readiness | "confirm publication-name plan, validate `[workspace.package]` defaults, dry-run `cargo publish` for every public crate, and verify path-dep incubation does not leak to `crates.io`." | Path-dep leak gate. | Lock 11 incubation-vs-publication gating silent (Lane 1). bbnf-bench inclusion conflicts with README §2 (Lane 1). | Folds with Lane 1. KEEP (with Lock 11 + bbnf-bench amendments). | KEEP |
| `restart/MASTER-PLAN.md:522` | J.W1 final SOTA gate | "misses require amendment before close." | Final SOTA escape clause confirmed deleted. | None. | KEEP. | KEEP |
| `restart/ARCHITECTURE.md:382-391` | bbnf canonical 8-children layout | "exactly 8 immediate children — `lib.rs`, `prelude.rs`, `parse/`, `document/`, `query/`, `visitor/`, `diagnostics/`, `metadata/`." Resolves HARDENING-CONSOLIDATED §3 conflict #3 + §4.19 fix. | Resolves prior 7-children divergence. | The §4.x crate trees do not carry per-crate child-count audit (covered in Lane 1 Lock 13). | Folds with Lane 1 Lock 13. KEEP. | KEEP |
| `restart/ARCHITECTURE.md:973-990` | Lock 2 + side tables — TypeFacts internal | "TypeFacts is an internal scratch artefact … never appears as a public side table." | Resolves HARDENING-CONSOLIDATED §3 conflict #4. | The path naming conflict (passes::types vs passes::layout) at Lane 1 Lock 2 still stands. | Folds with Lane 1 Lock 2. | KEEP |

**Lane 9 verdict: READY.** KEEP 7 / REINVENT 0 / DISCARD 0. Greenfield archaeology is honest; final SOTA escape clause deleted; no contrivance.

---

## §12 — Tightened 16-Command Gate-Rerun Results

Each command produced post-conditions; the table below records actual hits and verdicts.

| # | Command | Hits | Post-condition | Verdict |
|---|---|---:|---|---|
| 1 | `rg -n "ParseStream\|rewrite-mode\|Unicode class algebra" restart/{ARCHITECTURE,MIGRATION,MASTER-PLAN}.md` | 28 | All hits in conflict ledgers, deletion tables, syn-macro carve-outs, and migration receiver rows. No production usage. | PASS |
| 2 | `rg -n "bbnf-path\|bbnf-test-fixtures\|path!" restart/{ARCHITECTURE,MASTER-PLAN}.md` | 4 | All hits in migration crosswalk rows ("renames current bbnf-path") + A.W1 lint row + cookbook migration row. | PASS |
| 3 | `rg -n "codegen/src/backend_ir" restart/ARCHITECTURE.md` | 0 | Backend IR ownership lives in `ir` crate (not `codegen`); per `restart/ARCHITECTURE.md:419-423`. | PASS |
| 4 | `rg -n "fixtures/yaml" restart/{ARCHITECTURE,MASTER-PLAN}.md` | 1 | Single hit at ARCHITECTURE §12.1 `parity-phase fixtures/yaml/manifest.toml (post-onboarding gate, never an onboarding surface)`. yaml fixtures deferred to post-onboarding. | PASS |
| 5 | `rg -n "@recover" restart/ARCHITECTURE.md` | 1 | Hit at ARCHITECTURE §8.1 deletion table: standalone `@recover` deleted; folded into `@error(recover = ...)`. | PASS |
| 6 | `rg -n "OpenFrame" restart/MASTER-PLAN.md` | 8 | All hits are negative-grep gates, "no OpenFrame ladders" invariants, runtime tests, lock ownership rows. | PASS |
| 7 | `rg -n "GrammarIR" restart/ARCHITECTURE.md` | 0 | Token form `GrammarIR` (CamelCase no-space) absent. Document uses `Grammar IR` (with space) consistently. The import-deny target is implicit in `MIGRATION.md:715-718` `rg "GrammarIr\|GrammarIR\|grammar_ir"`. | PASS |
| 8 | `rg -n "__EAGER_EMPTY_PATH\|CursorDecision::Skip" restart/{MASTER-PLAN,MIGRATION}.md` | 3 | MASTER-PLAN §24 carry row + MIGRATION §19.4 + MIGRATION §20 punch list. Lock 3 gates anchored in two of three documents (ARCHITECTURE §6.1 line 802-806 covers the third). | PASS |
| 9 | `rg -n "twitter\|canada\|citm\|bootstrap\|animate\|On-Demand" restart/{MASTER-PLAN,ARCHITECTURE}.md` | 25 | Every SOTA target row anchored across both documents. | PASS |
| 10 | `rg -n "receiver\|blocker\|receiving gate" restart/{MIGRATION,MASTER-PLAN}.md` | 1 (free-text) + table headers | MIGRATION.md:769 mentions "receiver/blocker/gate" in §19.7 free text. Table-header form lives at MASTER-PLAN §24 (`Item / Receiver / Blocker / Gate`). MIGRATION §20 punch list uses different columns (`Item / Owner tranche / Constraint`) — **carry-ledger asymmetry surfaced (Lane 8)**. | PARTIAL — surfaces Lane 8 amendment |
| 11 | `rg -n "yaml.bbnf\|workspace.metadata.bbnf.grammars.yaml" restart/{ARCHITECTURE,MASTER-PLAN}.md` | 5 | Future-grammar onboarding test at ARCH §12 + per-X table row + cookbook row. | PASS |
| 12 | `rg -n "generated_loc\|regen_wall\|xtask" restart/{ARCHITECTURE,MASTER-PLAN}.md` | 21 | Per-grammar generated_loc_budget at ARCH §5; xtask invocations across MASTER-PLAN §6/§11/§13/§15/§20. Wall-time budgets at MASTER-PLAN §20. Token `regen_wall` not used; xtask wall budgets are inline ("under 30s/60s/90s..."). | PASS-WITH-NOTE — token form `regen_wall` not adopted; descriptive prose covers it. |
| 13 | `rg -n "BBNF-LIFE\|BBNF-LAYOUT\|BBNF-OPT\|BBNF-GRAMMAR\|BBNF-POINTER\|lookbehind\|HostSignature" restart/ARCHITECTURE.md` | 8 (lookbehind) + 1 (BBNF-LOOKBEHIND-WIDTH) | **Diagnostic vocabulary asymmetry surfaced (Lane 3).** ARCHITECTURE carries `BBNF-LOOKBEHIND-WIDTH` only. The remaining diagnostic codes (`BBNF-POINTER-*`, `BBNF-LIFETIME-*`, `BBNF-LAYOUT-*`, `BBNF-PRATT-*`, `BBNF-SIMD-*`, `BBNF-METADATA-*`, `BBNF-GRAMMAR-*`, `BBNF-VISITOR-*`, `BBNF-ARENA-*`) live only in MASTER-PLAN §24. `HostSignature` appears nowhere — the host-fn signature concept is named `Host signature` (with space) at ARCH §7.1 line 855. | PARTIAL — surfaces Lane 3 amendment |
| 14 | `rg -n "child count\|500 LOC\|exception rationale" restart/{ARCHITECTURE,MASTER-PLAN}.md` | 8 | Lock 13 verification table at MASTER-PLAN §21 (683-689) with 5 surfaces × child-count + LOC + exception rationale + enforcing command. ARCHITECTURE §13 carries the 4-10 children + 500 LOC ceiling rules. | PASS |
| 15 | `rg -n "declaration-crate review\|why metadata\|deletion path\|reviewer\|why_metadata_fails\|deletion_path\|receiving_gate" restart/{ARCHITECTURE,MIGRATION}.md` | 9 | 8-field declaration-crate review form at ARCH §5.6 (lines 723-754). All 8 fields enumerated; TOML reified at lines 740-754. | PASS |
| 16 | `rg -n "CPU model\|compiler flags\|input hash\|competitor version\|warmup\|sample" restart/{MASTER-PLAN,MIGRATION}.md` | 5 | Reproducibility schema at MASTER-PLAN §4 lines 138-150 with 8 rows × 2 columns. Every field has a verbatim source command. | PASS |

**Gate-rerun summary**: 13 PASS / 1 PASS-WITH-NOTE / 2 PARTIAL (gates 10 + 13). The two PARTIAL gates surface real amendments (carry-ledger asymmetry and diagnostic vocabulary asymmetry); they are not blocking the macro architecture but are blocking lane-cohesion verdicts.

---

## §13 — Punch List

Surgical edits to apply BEFORE the trio advances. Each entry: target file:line / verbatim edit (or surgery description) / source verdict / lane(s) producing the surgery.

### Punch 1 — Lock 2 path canonicalisation (REINVENT)

**Sites**: `restart/ARCHITECTURE.md:435-442` (the `passes/src/` tree); cross-references at `restart/ARCHITECTURE.md:792`, `restart/ARCHITECTURE.md:983`, `restart/ARCHITECTURE.md:1066`, `restart/MASTER-PLAN.md:295`.

**Surgery**: Pick one canonical name. Either:
- (a) Replace `types/` with `layout/` in §4.2's `passes/src/` tree, AND rename `passes::types` to `passes::layout` everywhere in narrative (§6 line 792 host route; §7.3 line 990 TypeFacts owner). Aligns with Lock 2's prescribed `passes::layout`.
- (b) Replace every `passes::layout` reference with `passes::types` in §6/§7.3/§8.2/MASTER-PLAN C.W1, AND amend Lock 2 at `restart/locks/14-LOCKS.md:36` to read `passes::types` instead of `passes::layout`. (Higher friction; Lock 2 is settled.)

**Recommendation**: option (a). Lock 2 names `passes::layout`; the §4.2 tree must reflect it.

**Owner**: Phase-2 trio amendment authority. **Lane**: 1.

### Punch 2 — Lock 11 incubation-vs-publication gating (REINVENT)

**Site**: `restart/MASTER-PLAN.md:524` J.W3 publication readiness.

**Surgery**: Split publication into two gates:
- **Stable surface** (`bbnf`, `bbnf-cli`, `bbnf-language-server`, `path`, `path-core`, `path-ts`): publish at J.W3.
- **Incubation-cleared sister crates** (`egraph`, `egraph-derive`, `csp-solver`, `parse-that`): publish at J.W3 only after a stability gate ("API has not changed across the prior tranche; downstream consumers compile against a frozen-version dry-run for K days/cycles").

Aligns with Lock 11 ("promote to registry once stable"). The current J.W3 wording publishes incubated crates without proving stability.

**Owner**: Phase-2 trio amendment authority. **Lane**: 1.

### Punch 3 — bbnf-bench publication status conflict (REINVENT)

**Sites**: `restart/README.md:38`, `restart/ARCHITECTURE.md:43`, `restart/MASTER-PLAN.md:524`.

**Surgery**: Settle bbnf-bench's publication status one place; cascade to the others.
- If publishable: amend README §2 line 38's "workspace-internal" to "crates.io".
- If not publishable: drop bbnf-bench from MASTER-PLAN J.W3's publish dry-run list; amend ARCHITECTURE §1 line 43's "Public/dev" to "Workspace-internal/dev".

**Recommendation**: bbnf-bench is a SOTA gate runner consumed by integrators reproducing benchmarks; it should publish. Adopt option (a).

**Owner**: Phase-2 trio amendment authority. **Lanes**: 1, 8.

### Punch 4 — H.W3 WASM gate Lock 8 anchor (REINVENT)

**Site**: `restart/MASTER-PLAN.md:459`.

**Surgery**: Replace the self-referential `≤ 3x native cost` ceiling with a competitor-anchored numeric. Candidate anchors:
- swc/WASM (TypeScript+JSX parse) — known WASM published harness.
- lightning-css/WASM — directly comparable to css/bootstrap row.
- simd-json/WASM — directly comparable to json/twitter row.

Recommended re-write:

> | H.W3 | WASM V1 via wasm32 Rust binding. | WASM package parses css/bootstrap on M1 Pro Safari WASM runtime ≤ {N}ms (lightning-css/WASM baseline {M}ms on the same fixture); metadata records WASM runtime, host browser, lightning-css/WASM version, and bbnf commit. |

**Owner**: Phase-2 trio amendment authority + measurement at H.W3. **Lanes**: 1, 4, 8.

### Punch 5 — Per-grammar baseline source-of-truth conflict (REINVENT)

**Sites**: `restart/ARCHITECTURE.md:1273-1281`, `restart/MASTER-PLAN.md:638-649`.

**Surgery**: Reconcile.
- **Option (a)**: ARCHITECTURE §12.1 numerics are firm. Amend MASTER-PLAN §20 lines 640-648 to drop "baseline recorded at A.W2"; reference ARCHITECTURE §12.1 as the baseline source.
- **Option (b)**: ARCHITECTURE §12.1 numerics are projections. Re-label them ("projected; firm baseline at A.W2"). MASTER-PLAN §20 keeps "recorded at A.W2."

**Recommendation**: option (a). The 9 extant grammars exist with measurable LOC today; tracking them through the migration as firm baselines is more honest than projecting.

**Owner**: Phase-2 trio amendment authority. **Lanes**: 3, 6.

### Punch 6 — Diagnostic vocabulary asymmetry (REINVENT)

**Site**: `restart/ARCHITECTURE.md` (no current location); recommend a new §7.4 or §10.5.

**Surgery**: Add a diagnostic-vocabulary table to ARCHITECTURE.md enumerating every `BBNF-*` code the codebase commits to. Cross-reference it from ARCHITECTURE §7.1 (HostCall produces `BBNF-HOST-*`), §7.2 (LayoutPush/Pop produces `BBNF-LAYOUT-CONFLICT`/`BBNF-LAYOUT-UNCLOSED`), §7.2 (ErrorRecover produces `BBNF-RECOVERY-*`). MASTER-PLAN §24 cookbook table then references this catalogue.

Suggested form:

> ### 7.4 Diagnostic Vocabulary
>
> | Code | Site | Meaning |
> |---|---|---|
> | `BBNF-LIFETIME-ESCAPE` | `bbnf` parse API | Borrow lifetime exceeds source. |
> | `BBNF-ARENA-MISMATCH` | `parse_in` | Caller-provided arena lifetime does not match. |
> | `BBNF-VISITOR-MUTATION-OUTSIDE-ENTRY` | `runtime/visitor` | Direct field mutation rejected. |
> | `BBNF-LAYOUT-CONFLICT` | BIR `LayoutPush`/`LayoutPop` | Conflicting layout policy. |
> | `BBNF-LAYOUT-UNCLOSED` | BIR `LayoutPop` | Unclosed layout scope. |
> | `BBNF-LOOKBEHIND-WIDTH` | Grammar IR `Lookbehind` | Unbounded lookbehind. |
> | `BBNF-PRATT-NOT-APPLIED` | `passes::recognizers` | Pratt detection ran but rejected. |
> | `BBNF-SIMD-NOT-SELECTED` | `passes::recognizers` | SIMD detection ran but rejected. |
> | `BBNF-METADATA-MISSING-GRAMMAR` | `pipeline::workspace` | Grammar source declared but no metadata block. |
> | `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE` | Lock 14 lint | A generic crate hardcodes a grammar name. |
> | `BBNF-POINTER-UNKNOWN-SEGMENT` | `path` macro | Path segment does not match grammar schema. |
> | `BBNF-POINTER-GRAMMAR-MISMATCH` | `path` macro | Path expression refers to a different grammar. |

**Owner**: Phase-2 trio amendment authority. **Lanes**: 3, 7.

### Punch 7 — Carry-ledger asymmetry (REINVENT)

**Sites**: `restart/MASTER-PLAN.md:729-745` (§24 14-row carry ledger); `restart/MIGRATION.md:777-786` (§20 8-row punch list).

**Surgery**: Either (a) consolidate into one ledger or (b) cross-reference each from the other.

**Option (a) — consolidation**: move MIGRATION §20 punch items into MASTER-PLAN §24 with a `Source: migration` column tag. MIGRATION §20 then links: "Migration-implementation receivers tracked at MASTER-PLAN §24."

**Option (b) — cross-reference**: amend MIGRATION §20 to point to MASTER-PLAN §24 for cross-tranche carries; amend MASTER-PLAN §24 to point to MIGRATION §20 for migration-implementation receivers. Eliminate duplication.

**Recommendation**: option (a). One source of carry-truth. The MIGRATION §20 items (declaration-crate review form, WASM exported ABI, benchmark host hardware profiles, archive destination, PASS-2 BIR snapshots, Lock 3 cursor gates, generated header fields, path-ts publication timing) overlap MASTER-PLAN §24 items (declaration-crate escape valve, WASM ABI, SOTA metadata, archive closure, Lock 3 cursor skip).

**Owner**: Phase-2 trio amendment authority. **Lane**: 8.

### Punch 8 — H.W2 "skipped metadata" phrasing (REINVENT)

**Site**: `restart/MASTER-PLAN.md:458`.

**Surgery**: Clarify "Platform-specific tests or skipped metadata." 

Recommended re-write:

> | H.W2 | AVX2/NEON/scalar dispatch gates. | Platform-specific tests pass on supporting hardware; tests on non-supporting hardware are skipped with a CI-readable skip-marker recording the missing capability (e.g., `cpu_feature: avx2_unsupported`). |

**Owner**: Phase-2 trio amendment authority. **Lane**: 2.

### Punch 9 — json/canada "simd-json comparable" (REINVENT)

**Site**: `restart/MASTER-PLAN.md:133`.

**Surgery**: Replace "simd-json comparable" with the firm number from `restart/corpora/SOTA.md` or strike the simd-json side.

Recommended re-write (assuming SOTA.md contains the simd-json/canada number — which V3 cannot verify without reading SOTA.md):

> | `json/canada` | sonic-rs 3.144ms; simd-json {Mms}. | <= 2.8ms. | M1 Pro macOS, native Rust release with `target-cpu=native`. | H.W4, J.W1. |

If SOTA.md does not measure simd-json on canada, drop the simd-json reference entirely.

**Owner**: Phase-2 trio amendment authority + verification at SOTA.md. **Lane**: 4.

---

## §14 V2 vs V3 Comparison

V2 returned READY across all nine lanes with one non-blocking residual. V3 returns AMENDMENT-REQUIRED on four lanes and READY-WITH-NOTE on two. The deltas:

| Lane | V2 | V3 | Delta source |
|---|---|---|---|
| 1 Lock-Adherence | READY (KEEP 14, REINVENT 0) | AMENDMENT-REQUIRED (KEEP 10, REINVENT 2) | V2 did not interrogate the §4.2 `passes/src/` tree against the §7.3 `passes::layout` narrative. V3 surfaced the contradiction. V2 did not interrogate Lock 11 ("Path-deps for incubating sister crates") against J.W3's blanket `cargo publish --dry-run` list. V3 surfaced the missing stability gate. |
| 2 Sequencing | READY (KEEP 8, REINVENT 1 — A.W4 phrasing) | READY-WITH-NOTE (KEEP 7, REINVENT 1 — H.W2 "skipped metadata") | V2's REINVENT (A.W4 binding refinement) overlaps V3's general close-gate analysis but the specific concern differs. V3 dropped V2's A.W4 critique (the close gate IS the consumer); V3 added H.W2 "skipped metadata" phrasing. The B.W3/C.W2 substrate-shell vs consumer-contract inversion is noted by V3 as KEEP-WITH-NOTE; V2 did not surface it explicitly. |
| 3 Cohesion | READY (KEEP 6, REINVENT 0) | AMENDMENT-REQUIRED (KEEP 5, REINVENT 2) | V2 did not interrogate the per-grammar LOC source-of-truth conflict (ARCH §12.1 numerics vs MP §20 "recorded at A.W2"). V2 did not interrogate the diagnostic vocabulary asymmetry (codes only in MASTER-PLAN §24, not in ARCHITECTURE). V3 surfaced both. |
| 4 SOTA-Anchoring | READY (KEEP 5, REINVENT 0, DISCARD 1 — final escape clause confirmed) | AMENDMENT-REQUIRED (KEEP 6, REINVENT 1) | V2 noted H.W3 "WASM <= 3x native cost" as KEEP ("numeric mechanism + metadata"). V3 reads it against Lock 8: every parse-throughput gate must cite a competitor. V3 surfaces the Lock 8 fault. V3 also notes "simd-json comparable" looseness at json/canada. |
| 5 Grammar-Authoritative | READY (KEEP 6, REINVENT 0, DISCARD 1) | READY (KEEP 8, REINVENT 0) | V3 confirms V2's verdict; gives more rows. |
| 6 Generated-Code-Budget | READY (KEEP 6, REINVENT 0) | READY-WITH-NOTE (KEEP 6, REINVENT 1) | V3's REINVENT folds with Lane 3's per-grammar baseline source-of-truth fault. |
| 7 Friction-Forecast | READY (KEEP 7, REINVENT 0) | READY (KEEP 7, REINVENT 0) | V2 and V3 align. |
| 8 Carry-Deferral | READY (KEEP 8, REINVENT 0, DISCARD 1) | AMENDMENT-REQUIRED (KEEP 7, REINVENT 2) | V2 did not interrogate the carry-ledger asymmetry between MASTER-PLAN §24 and MIGRATION §20. V2 did not surface the bbnf-bench publication conflict. V3 surfaces both. |
| 9 Greenfield-Discipline | READY (KEEP 6, REINVENT 0, DISCARD 1) | READY (KEEP 7, REINVENT 0) | V3 confirms V2's verdict. |

**Why V3 disagrees with V2**: V2 ran the gate-rerun at the surface and accepted the lane-by-lane "post-condition met." V3 ran the gates and then read across documents — the carry-ledger asymmetry surfaces only when MASTER-PLAN §24 and MIGRATION §20 are read together; the diagnostic vocabulary asymmetry surfaces only when the gate-13 result is interpreted against Lock 8 / cohesion principles; the Lock 2 fault surfaces only when §4.2 tree is read against §7.3 narrative; the Lock 11 fault surfaces only when J.W3 list is read against README §2's incubation classification. V2's READY verdict is defensible at the surface but not at the cross-document level. V3 takes the cross-document level as the auditable surface because the trio is a single contract.

The reverse direction also holds: V3 confirms the bulk of V2's KEEP verdicts. The Wave-2 + Wave-3 amendments did land. The trio is closer to ready than V1's 16-item punch list suggested. But six surgical amendments remain.

---

## §15 — Final Verdict

> **Decision: AMENDMENT-REQUIRED**
>
> The trio carries the macro architecture cleanly: tape + direct-to-struct union; 24-crate workspace with prefix discipline; two-IR + side-tables; Lock 14 grammar generalisation with per-X 10×9 authority table; declaration-crate fence with 8 fields; future-grammar onboarding test with two-surface proof; SOTA close rows with per-row metadata schema; benchmark reproducibility schema; mixed-fate crosswalk; branch/tag operation routing; greenfield archaeology with conflict ledger.
>
> Six surgical amendments remain, none touching the macro shape: Lock 2 path canonicalisation (`passes::layout` vs `passes::types` — internal contradiction); Lock 11 incubation-vs-publication gating (J.W3 publishes sister crates without stability proof); bbnf-bench publication status (3-document conflict); H.W3 WASM gate Lock 8 anchor (`≤ 3x native cost` carries no competitor); per-grammar baseline source-of-truth (ARCH numerics vs MP A.W2 measurement); diagnostic vocabulary asymmetry (codes only in MASTER-PLAN §24, not ARCHITECTURE). Three additional polish edits: H.W2 "skipped metadata" phrasing, json/canada "simd-json comparable" looseness, carry-ledger asymmetry between MP §24 and MIG §20.
>
> Hereupon the next step is amendment-agent dispatch with a punch list of nine items (six REINVENT, three polish). After amendment, a quick V4 confirmation gate-rerun should clear the trio for per-tranche full-spec drafting. The macro architecture survives; the remaining work is truth-table reconciliation and Lock-level binding.
