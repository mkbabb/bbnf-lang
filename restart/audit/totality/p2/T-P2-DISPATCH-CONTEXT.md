# T-P2 Dispatch Context — SK-V14 Totality Research Pass

Authored by SK-V14 orchestrator after T-P1 §3Z COHORT LOCK at commit `0a9c0fe65d7d80277bb56c32796c5ae6126d1052`. T-P2 dispatches next per `restart/prompts/totality/PASS-2-RESEARCH.md` + the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP. Six parallel sub-agents 2A..2F fan out per §2 scope matrix.

Each T-P2 agent reads §0 — §3 + own per-agent section.

## §0 — Authority

1. `restart/prompts/totality/PASS-2-RESEARCH.md` — your contract; §2 scope matrix; §2.1 frontmatter (NB: lowered into the prompt body); §3 CH1-CH6 lens overlay; §7 hard caps; §8 bbnf-lang specifics.
2. `restart/prompts/ORCHESTRATOR.md` §3W + §3Z; §8 non-negotiables.
3. `restart/audit/totality/p1/{1A,1B,1C,1D,1E,1F-coherence-scan,1F-anti-pattern,1F-past-corpora}.md` — converged T-P1 inventories (5-cycle V≤5 LOCKED).
4. `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md` — T-P1 §3Z LOCK declaration + 5-item T-P3 §3C carry-forward packet.
5. `restart/ARCHITECTURE.md` + `restart/MASTER-PLAN.md` + `restart/locks/LOCKS.md` (read-only governance surfaces; T-P2 emits LOCKS amendment **candidates only**).
6. `restart/HANDOFF.md` — totality handoff.
7. `skinny/REDRESS.md` (5041 lines; grep + offset) + `skinny/RESULTS.md` (185 lines) — empirical floor; 160-entry REDRESS pre-block ledger per S-P3 P3-E enumeration.
8. `restart/skinny/tranches/sk-v{1..14}/research/` (prior literature digests to extend, not re-derive).
9. SK-V14 SYNTHESIS.md §0 goalset + R-target acceptance criteria (informs T-P2 candidate dossier shape).
10. S-P2 V3 CONSOLIDATED at `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` — skinny S-P2 LOCKED candidate pool; T-P2 grounds the literature underneath the candidates.
11. S-P3 V1 CONSOLIDATED at `restart/skinny/tranches/sk-v14/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md` + SK-V14 SPEC at `restart/skinny/tranches/sk-v14/SPEC.md` — skinny wave plan (cross-reference; T-P2 grounds the SOTA + architectural primitives the SPEC waves consume).

## §1 — T-P1 V5 LOCKED ground truth (binding empirical floor)

T-P1 §3Z COHORT LOCK at V5 close (commit `0a9c0fe65d`); 5-cycle journey closed (sub-axis 68.6% → 94.8% → 97.3% → 100% → 100%; orphan REVISEs 22 → 6 → 2 → 0 → 0; REJECTs 0/0/0/0/0).

**Bound T-P1 evidence T-P2 must absorb:**
- **Lock 1 honoured-with-caveat** per 1A; 8 substrate divergences including 1A-DIV-008 two-cursor structural split at HEAD (parser.rs:7-12 ParserState.cursor over TapeBuilder vs codegen/src/json_typed_direct.rs:518-522 DirectParser.cursor raw usize); BIR 13/20 variants verified.
- **Codegen 1/5 BackendShape lowerers carries real logic** per 1B; 20 divergences; pass-layer D8 (passes/src/lib.rs:331 JSON-byte recognizer whitelist) + D10 (passes/src/lib.rs:1300-1391 JSON-literal role mining) are upstream Sheets/BBNF-self generalization blockers.
- **Pattern H = 67 hand-written per-grammar runtime files** per 1C (V13 baseline 64; +3 from css_pretty); 30 parser-name leak matches across 15 files; 127 grammar-named reexports at mod.rs:25-71 (47 lines hold 127 symbols).
- **5-cycle CHALLENGE journey** per 1D digest (SK-V14 audit-zero baseline; 74-finding S-P0 prune list ratified; CH2 13/17+14/17 envelope mis-attribution census).
- **16 LACs** per 1E (11 V4-carried + 5 SK-V14 NEW); LAC-1E-12 promoted to T-P3 §3C-priority.
- **1F coherence-scan 12 drifts + anti-pattern 19 + past-corpora 17** (AP-020 NEW: CSS source-sidecar comparator plane per CH5-004).

**T-P3 §3C carry-forward packet (5 items T-P2 must consume + extend):**
1. LAC-1E-12 procedural addendum — institutionalize executable verification on cite-carry (V→V+1).
2. NEW-CH2-V2-03 — any "N grammar-named X" subtract-from-K cite MUST enumerate K neutrals with path:line.
3. NEW-CH2-V3-02 — orphan-cell propagation guard: `rg -n <old-figure>` verify-before-commit on every count-cite fold.
4. CH4 cite-rebind cost-neutrality discipline (5 classes: cite-rebind / cite-cosmetic / REJECT-label-refinement / anti-paper-close-paragraph-insertion / anchor-refresh).
5. Substrate-union T-P3 §3C ratify-or-unify rule (1A-DIV-008 + 1D row 117 + 1E:35 sustained-UNKNOWN paragraph (iv) all bind).

**SK-V14 skinny-track carry-forward (informs T-P2 grounding scope):**
- 9-grammar census: bbnf/json/css_l4/css_pretty/google_sheets/ebnf/bnf/csv/math.
- S-P2 LOCKED candidate pool: 7 SOTA candidates + 5-stage admission process + 5 active arch esoterica + 2 active substrate + 9 parse-that gaps + 13 active grammar-neutral candidates (P2-A/B/C/D/E/F at V3 LOCK).
- S-P3 SPEC 12-wave plan W0..W11; canonical primitive `long_string_body_simd_scan` consolidates 3 axes (P2-A C2 ∪ P2-E Gap 1 ∪ P2-F C1+C2).
- F-V2-P1ABC-RERECORD Stage-0 binds W10 unconditionally per S-P3 V2 SPEC fold.

## §2 — Discipline (binding)

- HARD CAP 45 min per agent per PASS-2-RESEARCH.md §7.
- WRITE-ONLY for docs. Do NOT `git add`/`git commit`. Orchestrator commits all 6 P2 outputs atomically.
- T-P2 is read-only against `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md` (LOCKS amendment **candidates only**; Pass Omega disposes post-G-Omega).
- Cite primary sources verbatim (paper title + venue/year, post URL + named technique, or library source path:line). Confabulated citation = CH1 REJECT.
- Frontmatter per PASS-2-RESEARCH.md §2.1 (lowered into prompt body); body sections per §2 (Exec Summary + Technique Grounding Table + Architectural Assertions Defended + Architectural Assertions Refuted + Open Research Questions + LOCKS-AMENDMENTS-CANDIDATE).
- **Executable verification mandate institutionalized** per T-P1 V5 LOCK §6 carry-forward item 1 (LAC-1E-12): every cited path:line MUST be re-executed at HEAD before commit.
- aarch64 / Apple M5 Max primary per user pin (T-P2 2E primary architecture; x86 secondary).
- DAV1D/FFmpeg/VLC process discipline is spine of 2A (NOT pixel-domain kernels; the *process* — scalar oracle + checkasm differential + same-wave consumer).
- Lock 16 admissibility governs every primitive 2B + 2E ground (published citation + abstract-primitive name + hardware gate).
- Refutation is first-class output (a spec assumption literature does NOT support is the most load-bearing dossier row).

## §3 — Output structure

Each agent writes ONE dossier at `restart/audit/totality/p2/2{A..F}-{topic}.md` per PASS-2-RESEARCH.md §5. Frontmatter mandatory per §2.1; body sections per §2.

## §4 — Per-agent scope

Per PASS-2-RESEARCH.md §2 (read your row verbatim before writing):

- **2A — SOTA parsing landscape:** simdjson (stage1/stage2, On-Demand), sonic-rs (lazy-value, M1 Pro twitter anchor), yyjson (no-SIMD always_inline reference), asmjson (AVX-512 DOM kernel). DAV1D/FFmpeg/VLC process discipline (scalar oracle + checkasm differential + same-wave consumer). Output `2A-sota-landscape.md`.
- **2B — Primitive-vocabulary research:** Two-layer reusable SIMD/ASM primitive layer. Layer 0 (vendored x86inc.asm / dav1d macro corpus) + Layer 1 (bbnf.asm primitive vocabulary). Admission discipline per Lock 16. Output `2B-primitive-vocabulary.md`.
- **2C — Grammar-neutrality / generalisation research:** Primitive vocabulary + 5-shape BackendShape generalise beyond JSON to CSS L4 / Sheets / BBNF-self / arbitrary user grammars (Lock 14). Abstract-primitive-lift discipline. Output `2C-grammar-neutrality.md`.
- **2D — Cost-model + 5-shape BackendShape research:** Ground cost model (Lock 10) + 5-shape BackendShape derivation in literature. 8-step derive_backend_shape algorithm defensibility. CollapsedStage AVX-512-FSM design against asmjson + Sneller. Output `2D-cost-model.md`.
- **2E — Host-arch ASM/SIMD esoterica:** aarch64 PRIMARY (M5 Max target: PMULL/VPCLMUL lineage, CSSC, UDOT/DotProd, LD4-interleaved classify, BCAX/EOR3, NEON svmatch_u8 port). x86 SECONDARY (AVX2/AVX-512 VBMI2/GFNI/VPCLMUL, k-mask arithmetic, AVX-IFMA, VNNI, BITALG). Each entry: published citation + abstract-primitive name + hardware gate. Output `2E-host-arch-esoterica.md`.
- **2F — parse-that primitive gaps:** Audit parse-that / parse-that-regex crates for primitives V1 spec depends on but lack (regex/HIR, SIMD scan, string, float — Eisel-Lemire / Clinger). Per gap: published primitive + upstream-or-vendor decision + bbnf-specific need. Output `2F-parse-that-gaps.md`.

## §5 — Post-T-P2

After 6 P2 outputs commit + CHALLENGE V1 (CH1-CH6 + aggregator) per PASS-2-RESEARCH.md §3, §3Z convergence (≥95% × 2 cycles, zero orphan REVISEs, V≤5 ceiling) gates **T-P3 dispatch** per `restart/prompts/totality/PASS-3-SYNTHESIS.md`. G2 user gate is OPTIONAL per ORCHESTRATOR.md §6 — per the SK-V14 ORCHESTRATOR-PROMPT pin "do not relinquish except at G-Omega", G2 auto-passes and T-P3 fires.

## §6 — Cross-track cross-reference

S-P3 V2 CHALLENGE wave is converging on SK-V14 SPEC + DISPATCH-PROMPT in parallel. T-P2 must remain consistent with S-P3 LOCKED outputs (SPEC W0..W11 plan + 8-candidate shortlist + Stage-0 W10 unconditional binding); where T-P2 surfaces refuted assertions that contradict SPEC primitives, the refutation is the load-bearing dossier row routing to T-P3 §3C disposition (and possibly Pass Omega CRUD re-synthesis after G-Omega).
