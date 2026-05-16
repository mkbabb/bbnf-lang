# PASS-ALPHA — Skinny Astral Synthesis (SK-V{N+1} Cycle Creation)

Pass Alpha is the **skinny astral synthesis pass**. It consumes the completed SK-V{N} cycle (research + planning + redress commits + measured RESULTS) and produces the next SK-V{N+1} contract: GRAND-SYNTHESIS-SK-V{N+1}.md + IMPLEMENTATION-PACKET-SK-V{N+1}.md + HANDOFF-SK-V{N+1}.md. The new contract carries a **detailed, precisely-defined goalset + telemetry binding**: per-corpus per-workload Mbps, c/B, strictness plane, output plane, hot-leaf attribution, delta-vs-previous-SK, delta-vs-every-competitor (sonic-rs / simdjson / yyjson / asmjson / RapidJSON / serde_json + future grammar-domain comparators for CSS / Sheets / BBNF-self).

The pass is **iterative + auto-convergent**. Six parallel sub-agents fan out per the scope matrix in §2. A six-lens CHALLENGE pass adversarially reviews the output. Dispositions fold into v+1. The loop terminates at convergence per `ORCHESTRATOR.md` §iteration-governance + user G-Alpha sign-off.

## §1 — Trigger + entry condition

Pass Alpha dispatches when:

- **SK-V{N} measured close**: the most recent skinny iteration has produced `skinny/RESULTS.md` with all admitted/rejected routes recorded.
- **SK-V{N} REDRESS entries are complete**: every rejected route in the SK-V{N} cycle has a numbered REDRESS entry with measurement evidence.
- **No open implementation work**: the SK-V{N} working tree is clean (no uncommitted source edits; only audit/doc deletions if any).
- **Pass Omega has run** for the current totality cycle (so V1 spec is current; skinny stays subset of totality).

Or the user explicitly invokes `dispatch alpha SK-V{N}→SK-V{N+1}`.

## §2 — Scope matrix (six parallel sub-agents)

| Agent | Scope | Output |
|---|---|---|
| **α-A SK-V{N} results extraction** | Extract every row of `skinny/RESULTS.md`: per-corpus per-workload (parse_only, direct_to_struct, real_typed_struct, parse_full_traversal, path_lookup, unicode_string_float, memory, cycles_per_byte) Mbps + c/B + strictness + output plane + hot leaf attribution. Compute delta vs SK-V{N-1} per row. | `restart/skinny/audit/SK-V{N+1}-COHORT/alpha/αA-results-extraction.md` |
| **α-B Competitor delta extraction** | For every comparator (sonic-rs strict / sonic-rs lossy / simdjson DOM / simdjson On Demand / yyjson default / yyjson minify / asmjson SWAR / asmjson AVX-512 / RapidJSON default / serde_json + any grammar-domain comparator if scope expands), compute per-corpus delta vs bbnf Track 1. Disclose strictness + output plane for each comparator row. | `restart/skinny/audit/SK-V{N+1}-COHORT/alpha/αB-competitor-deltas.md` |
| **α-C REDRESS digest** | Walk every REDRESS entry from SK-V{N}'s cycle. Classify: admitted (commit SHA) / rejected (measurement evidence) / partial. Identify routes that should pre-block SK-V{N+1} (avoid re-opening). Identify routes that may admit under different framing. | `restart/skinny/audit/SK-V{N+1}-COHORT/alpha/αC-redress-digest.md` |
| **α-D Validated/invalidated ledger** | Update the validated/invalidated/demoted/still-open ledger from prior SK iterations. Cite commit SHAs + RESULTS rows. Identify the load-bearing wins from SK-V{N} that carry forward; identify the still-open items that become SK-V{N+1} candidates. | `restart/skinny/audit/SK-V{N+1}-COHORT/alpha/αD-validated-invalidated.md` |
| **α-E Cohort candidate shortlist** | Synthesise the SK-V{N} cohort A/B/C reports (research + reinforcement + profile). Produce a shortlist of ≤5 candidate interventions for SK-V{N+1}. Each candidate carries: file path, scalar reference status, checkasm test status, same-wave consumer plan, falsifiability gate (named corpus rows + Mbps thresholds), LOC budget, risk classification. | `restart/skinny/audit/SK-V{N+1}-COHORT/alpha/αE-candidate-shortlist.md` |
| **α-F SK-V{N+1} contract draft** | Compose the SK-V{N+1} GRAND-SYNTHESIS + IMPLEMENTATION-PACKET + HANDOFF. Each carries: corrected diagnosis from α-A through α-E + the precisely-defined goalset (§4) + the wave structure with falsifiability gates per wave + the pre-blocked routes from α-C. Includes the telemetry-binding schema (§5). | `restart/skinny/audit/GRAND-SYNTHESIS-SK-V{N+1}.md` + `IMPLEMENTATION-PACKET-SK-V{N+1}.md` + `HANDOFF-SK-V{N+1}.md` |

Each agent reads the antecedent SK-V{N} cohort + the four canonical files (RESULTS.md / REDRESS.md / restart/HANDOFF.md / restart/skinny/audit/HANDOFF-SK-V{N}.md). Each writes ONE artefact at the assigned path. Hard cap 45 min per agent.

After all six commit, the **CHALLENGE pass** dispatches (§3).

## §3 — Six-lens CHALLENGE pass

Six lens agents (CH1-CH6 per `ORCHESTRATOR.md` §5) adversarially review the α-A through α-F artefacts. Each produces dispositions per sub-section: ACCEPT / REVISE / REJECT.

**CH1 Correctness** — does every claim cite RESULTS.md row, REDRESS entry, commit SHA, or measurement file? Are falsifiability gates measurable? Are competitor deltas computed against the correct strictness plane?

**CH2 Generality** — does the goalset respect Lock 14? Are the proposed interventions grammar-neutral? Will they work for non-JSON grammars (CSS L4 / Sheets / BBNF-self)?

**CH3 Regression** — does any proposed intervention re-open a route in REDRESS? Cross-check the shortlist against entries 1-N. Has α-C correctly identified the pre-block list?

**CH4 Cost** — what is the LOC budget per intervention? Risk classification? Wave alignment? Same-wave consumer present per intervention?

**CH5 Hidden Coupling** — does any proposed intervention introduce parallel substrate, sidecar producer, renamed-scanner Lock 1 violation, Track 1 ≡ Track 2 dishonesty? Are the typed product plane gates structurally honest (Track 2 ≠ Track 1)?

**CH6 Next-Tranche-Impact** — does the SK-V{N+1} contract specify revert protocol per intervention? Hard caps? Triumvirate discipline? Is the goalset measurable + verifiable from the bench gate?

The CHALLENGE pass produces `restart/skinny/audit/SK-V{N+1}-COHORT/alpha-hardening/V{V}/{CH1..CH6}.md` + `CONSOLIDATED.md`. Hard cap 90 min for the CHALLENGE wave.

## §4 — The SK-V{N+1} goalset template (the load-bearing artefact)

The IMPLEMENTATION-PACKET-SK-V{N+1}.md carries a **detailed, precisely-defined goalset** at §0 Close Condition. The goalset has four layers:

### §4.1 — Per-row close conditions

For every corpus × workload pair currently in N-direct / parse-G state, name:

- The current state: (Mbps, % sonic-strict, % yyjson, % simdjson, % asmjson, classification).
- The target state: minimum Mbps OR minimum %sonic-strict OR PASS classification.
- The expected intervention: which candidate from α-E moves this row.
- The fallback: if the named intervention does NOT move the row by X%, what's the alternative? (Or: row REJECTed and recorded in REDRESS.)

### §4.2 — Strict comparator gate

Every row's competitor comparison must be against:

- **sonic-rs strict** (rebuilt without `utf8_lossy`).
- **simdjson C++ DOM + On Demand** (both planes named).
- **yyjson default strict** (default strictness mode disclosed).
- **asmjson SWAR + AVX-512** (where applicable; asmjson SWAR is permissive flaw probe, asmjson AVX-512 is strict on Zen 4).
- **RapidJSON default** (permissive; flaw probe only).
- **serde_json** (strict reference baseline).

For each comparator on each row: Mbps + % delta + strictness plane + output plane + hot leaf.

### §4.3 — Telemetry binding

The `skinny/RESULTS.md` schema for SK-V{N+1} must carry the following per row:

| Column | Type | Required |
|---|---|---|
| Corpus | string | yes |
| Workload | string (parse_only / direct_to_struct / real_typed_struct / parse_full_traversal / path_lookup / unicode_string_float / memory / cycles_per_byte) | yes |
| Outcome | enum (A / C / G / L) | yes |
| Verdict | enum (GO / NO-GO) | yes |
| Strictness | enum (strict / permissive / deferred) | yes |
| parse_utf8 | enum (scan-boundary / view-boundary / none) | yes |
| escape_complete | enum (yes / no) | yes |
| flaw_probe | string (one-line summary) | yes |
| Output plane | string (DOM / typed direct / iterator / borrowed view / digest) | yes |
| Track 1 Mbps | number | yes |
| Track 2 Mbps | number | yes |
| sonic-rs strict Mbps | number | yes if comparator runnable |
| sonic-rs lossy Mbps | number | optional (flaw probe) |
| simdjson DOM Mbps | number | yes if comparator runnable |
| simdjson On Demand Mbps | number | optional |
| yyjson default Mbps | number | yes if comparator runnable |
| asmjson SWAR Mbps | number | optional (permissive flaw probe) |
| asmjson AVX-512 Mbps | number | optional (x86 only) |
| RapidJSON default Mbps | number | optional (flaw probe) |
| serde_json Mbps | number | yes (strict baseline) |
| Δ vs SK-V{N} | number (Mbps delta or %) | yes |
| Δ vs sonic-strict | % | yes |
| Δ vs simdjson DOM | % | yes |
| Δ vs yyjson | % | yes |
| Hot leaf | string (top symbol + % self-time) | yes |
| Signal | string (PASS / NO-GO with reason) | yes |

The SK-V{N+1} bench harness must emit this schema verbatim. The xtask gate-json command must reject any row missing required columns.

### §4.4 — Wave-by-wave falsifiability gates

Each wave in the IMPLEMENTATION-PACKET-SK-V{N+1}.md carries:

- **Owner paths**: file paths the wave is authorised to touch.
- **Entry gate**: what must be true before dispatch (e.g. "Wave 1 entry: comparator-plane repair landed; sonic-rs is strict; RESULTS.md re-baselined").
- **Exit gate**: measurable condition for closure (e.g. "Wave 1 exit: at least 2 of 4 named must-lift rows cross threshold X; if not, REJECT and document").
- **Hard cap**: minutes for research / plan / redress phases.
- **Revert protocol**: if the wave fails its exit gate, what's the rollback action? Does it block subsequent waves?
- **Same-wave consumer**: each primitive/kernel/intervention lands WITH its hot-path caller in the same commit.
- **Pre-blocked routes**: cite REDRESS entries this wave must NOT re-open.

## §5 — Telemetry binding (the auto-converge feedback signal)

Pass Alpha's convergence is bound to telemetry:

- The goalset (§4.1) names specific rows + thresholds.
- The implementation cycle (SK-V{N+1} waves) admits or rejects per the falsifiability gates.
- The bench harness emits the schema (§4.3) on every measurement.
- The xtask gate-json compares Mbps deltas against the goalset + emits per-row signals.

If the SK-V{N+1} cycle closes with the goalset met, Pass Alpha's V{V} was correct.
If the cycle closes with the goalset unmet, Pass Alpha's V{V+1} must revise the candidate shortlist (α-E) and produce a corrected goalset.
The cycle of Pass Alpha iterations is bounded by V5 per the orchestrator ceiling.

## §6 — Output structure

After Pass Alpha closes:

```
restart/skinny/audit/SK-V{N+1}-COHORT/
├── alpha/
│   ├── αA-results-extraction.md
│   ├── αB-competitor-deltas.md
│   ├── αC-redress-digest.md
│   ├── αD-validated-invalidated.md
│   ├── αE-candidate-shortlist.md
│   └── αF-contract-draft.md
├── alpha-hardening/
│   └── V{V}/
│       ├── CH1.md
│       ├── CH2.md
│       ├── CH3.md
│       ├── CH4.md
│       ├── CH5.md
│       ├── CH6.md
│       └── CONSOLIDATED.md
└── [post-G-Alpha: the SK-V{N+1} cycle's own research/profile/redress cohort will populate]

restart/skinny/audit/GRAND-SYNTHESIS-SK-V{N+1}.md   ← THE master synthesis
restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V{N+1}.md ← THE implementation packet with goalset §4
restart/skinny/audit/HANDOFF-SK-V{N+1}.md           ← THE packet handoff
```

## §7 — User sign-off (G-Alpha)

After CHALLENGE CONSOLIDATED returns ≥95% ACCEPT, the orchestrator presents the SK-V{N+1} contract to the user:

- Summary: rows targeted + interventions + LOC budget + hard caps + pre-blocked routes.
- The full GRAND-SYNTHESIS-SK-V{N+1}.md + IMPLEMENTATION-PACKET-SK-V{N+1}.md links.
- The goalset table (§4.1) + telemetry schema (§4.3).
- The predicted close state (per α-A delta projection + α-B competitor delta).

The user signs off with **G-Alpha closed** or **G-Alpha revise** (with specific revisions named).

After G-Alpha closed, the orchestrator can dispatch SK-V{N+1} P1 (the first skinny pass of the new iteration).

## §8 — Convergence + escalation

Pass Alpha converges per `ORCHESTRATOR.md` §iteration-governance: ≥95% ACCEPT on CHALLENGE + zero open critical defects + no orphan REVISE + user G-Alpha sign-off.

Escalation to user:

- If α-E shortlist contains zero ACCEPTED candidates → Pass Alpha cannot produce a goalset → escalate to user with `BLOCKED: no candidate intervention survives CHALLENGE`.
- If goalset (§4.1) cannot be specified because every row is currently PASS → escalate to user with `SUCCESS: SK-V{N} is the close; dispatch x86 successor or grammar-expansion phase`.
- If the predicted SK-V{N+1} close state shows fewer rows passing than SK-V{N} → escalate to user with `WARN: candidate shortlist would regress; revise candidates or accept`.
- If V > V5 without convergence → escalate to user with `BLOCKED: 5 iterations without convergence; named unresolved REVISE dispositions`.

## §9 — Bbnf-lang specific axes for Pass Alpha

Pass Alpha is the place where bbnf-lang's SOTA-beat discipline manifests:

1. **The strict-vs-strict comparator gate** (§4.2) prevents permissive-comparator wins from counting as SOTA-beat. This was the SK-V6 finding (sonic-rs utf8_lossy); Pass Alpha enforces it for every SK-V{N+1}.
2. **The telemetry schema** (§4.3) makes per-iteration progress measurable. No SK-V{N+1} ships without producing the full schema.
3. **The grammar-neutral goalset** (§4.1) routes interventions through the cost model + the 5-shape BackendShape taxonomy, not through grammar-specific patches.
4. **The same-wave consumer rule** in falsifiability gates (§4.4) prevents the V5 orphan-kernel pattern.
5. **The pre-blocked routes** (§4.4) prevent hypothesis transfer between SK iterations — every SK-V{N+1} starts from the new baseline empirical.

## §10 — Closing posture

Pass Alpha is the synthesis discipline. SK-V{N+1} is born from the empirical evidence of SK-V{N}. The goalset is precise + measurable + telemetry-bound. The CHALLENGE pass is adversarial. The G-Alpha gate is user-controlled.

No SK-V{N+1} dispatch without G-Alpha. No G-Alpha without CHALLENGE convergence. No CHALLENGE convergence without measurable falsifiability gates per row. No falsifiability gate without RESULTS.md row evidence.

The work is bounded by the bench. The bench is bound by the schema. The schema is the contract.
