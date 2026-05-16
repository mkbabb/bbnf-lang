# bbnf-lang Prompt Suite — Iterative Auto-Convergent Multi-Pass Framework

Reading order:

1. This README (framework gestalt).
2. `ORCHESTRATOR.md` (phase identification + dispatch matrix + iteration governance).
3. `PASS-ALPHA.md` (skinny astral synthesis — the SK-n cycle creation contract, the most user-load-bearing artefact).
4. `PASS-OMEGA.md` (totality astral synthesis — V1 spec cohesion + skinny lessons fold-in).
5. The four per-pass contracts: `TOTALITY-PASS-1-RESEARCH.md`, `TOTALITY-PASS-2-PROFILE.md`, `TOTALITY-PASS-3-HARDENING.md`, `SKINNY-PASSES.md`.

## Framework gestalt

bbnf-lang has two architectural tracks: the **V1 totality spec** (greater architecture; grammar-neutral; targets JSON + CSS L4 + BBNF-self + Sheets + arbitrary user grammars) and the **skinny spec subset** (JSON-focused implementation that serves as feedback loop for the greater spec). The skinny iterations (SK-V1 through SK-V7+) are the empirical engine; the totality spec is the durable target.

The prompt suite formalises this duality. Two parallel pass tracks exist:

- **Totality passes (1, 2, 3)** drive V1 spec evolution. Three substantive passes (research / profile / hardening) iterate autonomously until convergence, then **Pass Omega** (totality astral synthesis) folds findings + skinny lessons into V1 spec surfaces (ARCHITECTURE / MASTER-PLAN / locks / HANDOFF / MIGRATION / SUBSTRATE / COMPILER / BENCH / INDEX / WORKSPACE / HARDENING / SOTA-BEAT-DESIGN).
- **Skinny passes (1-n)** drive SK-V{N} evolution. Skinny pass count is unbounded because each SK iteration may require a different number of research/implement/redress sub-cycles. **Pass Alpha** (skinny astral synthesis) closes each cycle by producing the next SK-V{N+1} contract with detailed goalset + precisely-defined telemetry.

The pattern (per hassio-config `restart/prompts/`):

1. **Parallel sub-agents per pass.** Six (or more) read-only research / profile / hardening agents fan out on disjoint scope rows. Each commits one artefact under `restart/skinny/audit/SK-V{N}-COHORT/` (skinny track) or `restart/totality/{pass}/V{V}/` (totality track).
2. **Six-lens CHALLENGE hardening pass** follows each substantive pass. Adversarial review across six universal lenses (correctness / generality / regression / cost / hidden coupling / next-tranche-impact). Dispositions fold into v+1 of the substantive artefacts.
3. **N-iteration auto-converge.** Each pass repeats V1, V2, V3, … until the convergence criterion in `ORCHESTRATOR.md` §iteration-governance holds (95%+ ACCEPT on the challenge pass + zero open critical defects).
4. **User sign-off gates** at named boundaries (G1 = totality research close, G2 = totality profile close, G3 = totality hardening + locks crystallisation, G4 = master-plan locked, G5 = per-skinny-iteration sign-off, G-omega = totality astral synthesis sign-off, G-alpha = next-skinny-cycle goalset sign-off).
5. **Pass Omega + Pass Alpha** are themselves iterative + auto-convergent. Astral synthesis is not a one-shot fold — it iterates over challenge waves (claim-validation against antecedent passes) + CRUD waves (Create/Read/Update/Delete operations on the document corpus). Both passes converge against the locked locks + the empirical telemetry from the skinny track.

## Why two astral passes (Omega vs Alpha)

**Pass Omega (totality astral)** consumes:
- Totality passes 1-3 artefacts (research / profile / hardening) for the current cycle.
- All prior skinny iterations' lessons (REDRESS rejections, RESULTS deltas, validated/invalidated ledger from each SK-V{N}).
- Existing V1 spec surfaces (ARCHITECTURE, MASTER-PLAN, locks, HANDOFF, MIGRATION, SUBSTRATE, COMPILER, BENCH, INDEX, WORKSPACE, HARDENING, SOTA-BEAT-DESIGN).

Pass Omega produces:
- v+1 of every V1 spec surface (CRUD operations: Create new sections, Update stale, Delete superseded, Read for cohesion check).
- A `restart/HANDOFF.md` top-level state update.
- A locks amendment proposal (subject to G-omega sign-off before merge to `restart/locks/`).
- The directive set for the NEXT totality pass cycle (V2 of passes 1-3).

**Pass Alpha (skinny astral)** consumes:
- The most recent skinny pass cycle's artefacts.
- The current `skinny/RESULTS.md` + `skinny/REDRESS.md`.
- Pass Omega's latest output (so skinny stays subset of totality).
- All competitor benchmark data (sonic-rs, simdjson, yyjson, asmjson, RapidJSON, serde_json, plus future grammar-domain competitors for CSS / Sheets / BBNF-self).

Pass Alpha produces:
- The next `IMPLEMENTATION-PACKET-SK-V{N+1}.md` with **detailed goalset + precisely-defined telemetry** per Pass Alpha §goalset-template.
- The next `HANDOFF-SK-V{N+1}.md`.
- The empirical-correction ledger merged into `skinny/REDRESS.md`.
- A `skinny/RESULTS.md` schema check (must carry: per-corpus per-workload Mbps, c/B, strictness plane, output plane, hot-leaf attribution, delta-vs-previous-SK, delta-vs-every-competitor).

## Telemetry and goalset discipline

Every SK-V{N} iteration must publish:

- **Per-dataset benchmark items**: all 17 (or larger) corpora × all admitted workloads (parse_only, direct_to_struct, real_typed_struct, parse_full_traversal, path_lookup, unicode_string_float, memory, cycles_per_byte).
- **Delta vs previous SK**: per-row Mbps delta + per-row outcome classification change.
- **Delta vs every competitor**: per-row Track 1 vs sonic-rs (strict) / simdjson / yyjson / asmjson / RapidJSON / serde_json + any grammar-domain comparator (CSS parsers, Sheets parsers, etc. as the scope expands).
- **Strictness disclosure** (post SK-V6 finding): `Strictness | parse_utf8 | escape_complete | flaw_probe` columns on every row.
- **Hot-leaf attribution**: top-3 self-time symbols per row at PC level via `parse-attribution` feature.

The goalset for SK-V{N+1} carries:

- Named falsifiability gates per intervention (e.g. "row X must cross threshold Y or REJECT").
- Hard caps per wave (research / plan / redress minutes).
- Pre-blocked routes (every previously-rejected route from prior SK ledgers cited).
- Same-wave consumer requirement (no orphan kernels).
- Strict-vs-strict comparator plane (no flaw-probe baselines).
- Explicit success / partial / failure criteria mapping to commit / revert / re-research.

## Non-negotiables (apply across all passes)

| Rule | Enforcement |
|---|---|
| No new BBNF directives | Grep grammars/ + restart/skinny/ for new `@` directives between SK-V{N} and SK-V{N+1}. |
| No new BIR variant | Grep `ir/src/` for variant additions; existing alphabet (Alt{Dispatch}, TapeEmit, DirectBuild, CallHost) is closed. |
| No new substrate | tape ≡ structural projection union (Lock 1); no parallel scanners, sidecar producers, or refuted-prepass shapes. |
| No JSON code in generic crates | bbnf-simd / parse-that-regex / codegen/lower / runtime/tape / passes are grammar-neutral; per-grammar lives in runtime/grammars/{grammar}/ + codegen-emitted .data tables. |
| Scalar reference per primitive | Every SIMD/ASM primitive ships with scalar executable spec + checkasm parity BEFORE wiring. |
| Same-wave consumer | Primitive lands only with the generated/runtime consumer that exercises it on the hot path. |
| Profile-first prescription | No kernel intervention proposed without fresh PC-level profile of the NEW Track 1 (generated runtime) baseline naming the kernel boundary. Hypothesis transfer between SK iterations is forbidden. |
| Strict-vs-strict comparisons | Sidecar rows match strictness plane. Permissive-asmjson "beats" do not count. |
| Triumvirate discipline | Each wave separates research → plan → redress in distinct commits. No single commit merges roles. |
| Hard cap per dispatch | Every research / profile / implementation dispatch carries an explicit minute cap. At 0.9× cap commit; at cap halt. |
| Same-row falsification gate | A kernel that does not lift a previously-named row is rejected; record in REDRESS with measurements + the next candidate shape. |
| No deferrals | Wave N closes on measurement, not "future phase will fix it." |

## Phase glossary (cross-pass shared vocabulary)

- **CHALLENGE pass**: Adversarial review wave. Sub-agents take antecedent artefacts and produce dispositions (ACCEPT / REVISE / REJECT) across the six universal lenses.
- **CRUD wave**: Document maintenance wave under Pass Omega + Alpha. Creates new sections, Reads for cohesion, Updates stale text, Deletes superseded artefacts.
- **v+1 fold**: After a CHALLENGE pass closes, the original sub-agent author re-runs with the dispositions in hand and produces v+1 of the artefact. v+1 must address every disposition or fail explicitly.
- **Convergence criterion**: ≥95% ACCEPT on the most recent CHALLENGE pass + zero open critical defects + no orphan unresolved REVISE.
- **Triumvirate**: Research → Plan → Redress. Three distinct commits. Research = read-only diagnosis. Plan = synthesis of an intervention. Redress = the implementation commit + measurement + REDRESS entry on success or revert + REDRESS entry on failure.
- **SK-V{N}**: Skinny iteration N. Each SK-V{N} has its own cohort directory at `restart/skinny/audit/SK-V{N}-COHORT/` and its own master docs at `restart/skinny/audit/GRAND-SYNTHESIS-SK-V{N}.md`, `IMPLEMENTATION-PACKET-SK-V{N}.md`, `HANDOFF-SK-V{N}.md`.
- **V{V}**: Pass iteration version within a single pass (V1, V2, V3, …). Auto-incremented per `ORCHESTRATOR.md` §iteration-governance.

## Repository layout

```
restart/
├── prompts/                                 ← THIS DIRECTORY (the pass contracts)
│   ├── README.md                            ← framework gestalt
│   ├── ORCHESTRATOR.md                      ← dispatch + iteration governance
│   ├── PASS-ALPHA.md                        ← skinny astral synthesis (SK-n creation)
│   ├── PASS-OMEGA.md                        ← totality astral synthesis
│   ├── TOTALITY-PASS-1-RESEARCH.md          ← totality pass 1 contract
│   ├── TOTALITY-PASS-2-PROFILE.md           ← totality pass 2 contract
│   ├── TOTALITY-PASS-3-HARDENING.md         ← totality pass 3 contract
│   └── SKINNY-PASSES.md                     ← skinny passes 1-n contract
├── ARCHITECTURE.md
├── MASTER-PLAN.md
├── HANDOFF.md                               ← top-level state (Pass Omega owns)
├── MIGRATION.md
├── locks/
│   └── 14-LOCKS.md                          ← Lock 1-16 (Pass Omega proposes amendments; G-omega sign-off required)
├── skinny/                                  ← the skinny spec surfaces (Pass Alpha owns)
│   ├── BENCH.md
│   ├── COMPILER.md
│   ├── HARDENING.md
│   ├── INDEX.md
│   ├── SUBSTRATE.md
│   ├── WORKSPACE.md
│   └── audit/
│       ├── GRAND-SYNTHESIS-SK-V{N}.md       ← per-SK synthesis
│       ├── IMPLEMENTATION-PACKET-SK-V{N}.md ← per-SK implementation packet
│       ├── HANDOFF-SK-V{N}.md               ← per-SK packet handoff
│       ├── SOTA-BEAT-DESIGN.md              ← shared SOTA-beat design
│       └── SK-V{N}-COHORT/                  ← per-SK research + profile cohort reports
└── totality/                                ← totality track artefacts (Pass Omega owns)
    ├── pass-1-research/
    │   └── V{V}/                            ← per-iteration pass-1 outputs
    ├── pass-2-profile/
    │   └── V{V}/
    ├── pass-3-hardening/
    │   └── V{V}/
    └── astral/
        └── V{V}/                            ← per-omega-iteration synthesis outputs
```

## Dispatch invocation

The orchestrator-agent reads `ORCHESTRATOR.md` to identify the current pass + cycle from `restart/HANDOFF.md` + git state, then fans out parallel sub-agents per the phase table. Each sub-agent reads its assigned pass contract (`PASS-ALPHA.md` / `PASS-OMEGA.md` / `TOTALITY-PASS-{1,2,3}.md` / `SKINNY-PASSES.md`) and produces its committed artefact.

The user invokes the orchestrator by saying "dispatch P{N}" or "dispatch SK-V{N+1} alpha" or "dispatch omega". The orchestrator does not require user supervision between sub-agents within a pass — only at the named sign-off gates (G1, G2, G3, G4, G5, G-omega, G-alpha).

The user retains the right to override the orchestrator's phase identification + dispatch at any point. The orchestrator confirms the override and dispatches per the user's pinned phase.

## The bbnf-lang specific axes (vs hassio-config)

The hassio-config prompt suite was designed for a Home Assistant smart-home rearchitecture. The bbnf-lang adaptation differs on three axes:

1. **Bench gate is empirical, not declarative.** bbnf-lang's close condition is measured (no parse-G rows, no N-direct rows, strict-vs-strict competitor beat) rather than a paper sign-off. Every iteration produces real RESULTS.md rows; Pass Alpha verifies the goalset against those rows.
2. **Skinny ⊂ totality, with feedback loop.** Skinny iterations are not standalone; they are the empirical engine that informs V1 spec evolution. Pass Omega explicitly consumes skinny REDRESS entries to amend V1 locks / architecture. The skinny-to-totality direction is canonical.
3. **Grammar generalisation is non-negotiable.** Every primitive, kernel, and lowering pattern in skinny must be grammar-neutral; per-grammar variation lives in codegen-emitted .data + per-grammar wrapper directories. The Lock 14 audit lives inside the framework, not as an after-the-fact check.

## Closing posture

The prompt suite is the durable orchestrator. The skinny + totality content evolves through iteration. The framework gives the iteration a shape — auto-convergent, challenge-hardened, telemetry-bound. No SK-V{N+1} dispatches without a precise goalset from Pass Alpha. No V1 spec amendment without Pass Omega cohesion verification. No new pass cycle without convergence on the prior.

The work is bounded by the gates. The throughput is bounded by the bench. The architecture is bounded by the locks. The discipline is the suite.
