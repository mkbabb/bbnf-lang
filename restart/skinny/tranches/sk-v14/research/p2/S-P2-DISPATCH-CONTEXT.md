# S-P2 Dispatch Context — SK-V14 Research Pass

Authored by the SK-V14 orchestrator after S-P1 closed G-S-P1-CONVERGED at `426d7771e`. SK-V14 contract durable; G-Alpha auto-signed; S-P2 dispatches next per `restart/prompts/skinny/PASS-2-RESEARCH.md` + the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP (concurrent with Pass Omega T-P1).

Each S-P2 agent reads §0 — §3 + own per-agent section (§P2-A..F per PASS-2-RESEARCH.md §2 scope matrix).

## §0 — Authority

1. `restart/prompts/skinny/PASS-2-RESEARCH.md` — your contract; §2 scope matrix; §2.1 frontmatter; §3 CH1-CH6 lens overlay; §7 hard caps; §8 bbnf-lang specifics.
2. `restart/prompts/ORCHESTRATOR.md` §3W + §3Z; §8 non-negotiables (Lock 1 substrate union; Lock 14 grammar-neutrality; scalar-reference + checkasm; same-wave consumer).
3. `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md` §CH7.
4. `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` + `SYNTHESIS.md` + `HANDOFF.md` — durable SK-V14 contract.
5. `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` — S-P0 prune list (74 findings; 3 architectural sequencing constraints).
6. `restart/locks/LOCKS.md` — 16 locks; **Lock 1 substrate union + Lock 14 grammar-neutrality are load-bearing**.

## §1 — S-P1 inheritance (empirical floor for primitive design)

S-P1 LOCKED at G-S-P1-CONVERGED (`426d7771e`); 99.43% V2 + 99.87% V3 aggregate. Six P1 profile artefacts at `restart/skinny/tranches/sk-v14/research/p1/`:
- `p1a-samply-mode-1.md` (parse_only × 17 corpora; atos -inlineFrames pipeline)
- `p1b-samply-mode-2.md` (direct × 17 + typed × 11; **DirectParser::skip_value dominates typed 72.5-76.1%** — typed plane is empirically structural-skip not typed-decode)
- `p1c-samply-mode-3.md` (mode-III × 17 × 4 probes; 8 ANOMs incl. alternate_scalar_plan misnaming)
- `p1d-pmu-cycles.md` (231 PMU rows; cycles+inst REACHABLE unprivileged; PMC counters UNREACHABLE)
- `p1e-hot-leaf-attribution.md` (**CH2 Lock-14 mis-attribution census: 13/17 + 14/17 envelope mis-attribution** — dispatch_value, parse_object_value_at_direct; S-P2 must enable `parse-attribution` Cargo feature to crack envelope)
- `p1f-results-delta.md` (75 rows; SK-V14 NEW columns 100% absent → R1/R2 wave; 4 typed admits have admit-entries but zero typed bytes)

**Inherited V2/V3 carry-forwards bound to S-P2:**
- **F-V2-P1ABC-RERECORD** (heavy deferred packet per V1 aggregator Option X): parse-attribution rebuild + samply re-record P1-A/B/C + github_events longer-iter. **This is wave-program work post G-Omega — S-P2 designs with the existing P1 profile; the re-record refines later. Document the envelope-folded `dispatch_value` masking in your candidate-primitive list and flag the re-record dependency.**
- **CH2 F1:** parse-attribution is `runtime`-crate-private; bench-harness must invoke `--features runtime/parse-attribution` (transitive), not bare `--features parse-attribution`. 14 functions gated at `runtime/src/grammars/json/generated.rs` (lines 33-237).
- **CH2 F2:** Zero CSS L4 grammar-neutral primitive evidence at SK-V14 (only `declaration_values` renders as parse-result row; 23/24 CSS features absent). S-P2 generalization argument made on JSON profile + CSS L4 spec evidence jointly, without CSS L4 profile corroboration.
- **Substrate-union finding (Lock 1):** DirectParser::cursor + parse_object_value_at_direct cursor are two structurally independent state machines (per CH5 V3 verification at HEAD); P2-D's tape interrogation must conclude the substrate union holds.
- **Cohort discovery:** {P1-A, P1-B} are RUSTFLAGS-unset; {P1-C, P1-D} are `-C target-cpu=native`. Cross-cohort c/B deltas refuse per `build_flags_regime` schema column.
- **Process observations:** cargo metadata cd-prefix to repo root; @generated CI lint distinguishes diagnostic vs load-bearing; /tmp non-volatile sidecar relocation.

## §2 — Discipline (binding)

- HARD CAP 45 min per agent per PASS-2-RESEARCH.md §7. At 40 min commit-equivalent (write file); at 45 halt.
- WRITE-ONLY for docs. Do NOT `git add`/`git commit`. Orchestrator commits all 6 P2 outputs atomically.
- S-P2 is read-only against `skinny/` source.
- Cite path:line on bbnf claims; cite external sources (asmjson/sonic-rs/simdjson/yyjson source files, ISA manuals, prior tranche evidence).
- §2.1 frontmatter mandatory; §2 candidate-primitive enumeration is load-bearing (prose without concrete candidate list fails CH1).
- Per `[no-warm-benches]` `[no-god-modules]` `[hybrid-grammar-host]`: candidate primitives are sub-modules of bbnf-simd, not god modules; JSON policy never enters generic crate; SOTA-beat against strict comparator only.
- aarch64 / Apple M5 Max binding per user pin (P2-C primary architecture).

## §3 — Output structure

Each agent writes ONE file at `restart/skinny/tranches/sk-v14/research/p2/p2{a..f}-{topic}.md` per PASS-2-RESEARCH.md §5. Frontmatter per §2.1; body sections §1 Findings + §2 Candidate primitives + §3 Grammar-neutrality + §4 Risks (REDRESS pre-block) + §5 Sources.

## §4 — Per-agent scope

Per PASS-2-RESEARCH.md §2 (read your row verbatim before writing):
- **P2-A:** SOTA comparator teardown — asmjson/sonic-rs/simdjson/yyjson architecture. Output `p2a-sota-teardown.md`. SK-V14 binding: each comparator's strict-vs-strict plane (per R1 of ORCHESTRATOR-PROMPT) keyed to S-P1 hot leaves.
- **P2-B:** DAV1D/FFmpeg ASM process — scalar-oracle-first + checkasm differential. Output `p2b-dav1d-process.md`. Map onto bbnf-simd `checkasm_*` tests; primitive-admission process S-P3 gates against.
- **P2-C:** Host-arch ASM/SIMD esoterica — aarch64 primary (PMULL, CSSC CTZ, UDOT/DotProd, TBL/TBX, wide-shift). Output `p2c-arch-esoterica.md`. Flag REDRESS-blocked routes (REDRESS 88 PMULL prefix-XOR hot body; 89 CSSC CTZ next-bit bulk).
- **P2-D:** Substrate + tape design — Lock 1 interrogation. Output `p2d-substrate-tape.md`. Verify P1 finding (DirectParser cursor separation) generalises; no parallel substrate proposals.
- **P2-E:** parse-that primitive gaps — which SIMD/string/float/regex primitives S-P1 hot leaves demand that parse-that does not yet expose. Output `p2e-parse-that-gaps.md`. Layer-0/Layer-1 placement.
- **P2-F:** Grammar-neutral abstraction — for every candidate primitive from P2-B/C/D/E, generalisation to CSS L4 / Sheets / BBNF-self (Lock 14). Output `p2f-grammar-neutral.md`. CSS L4 generalisation made from spec evidence (no profile corroboration available per CH2 F2).

P2-F depends on P2-B/C/D/E outputs. Parallel-dispatch: P2-F reads available outputs at dispatch + completes in CHALLENGE-fold cycle.

## §5 — Post-S-P2

After 6 P2 outputs commit + CHALLENGE V1 (CH1-CH7 + aggregator) per PASS-2-RESEARCH.md §3, §3Z convergence (≥95% × 2 cycles, zero orphan REVISEs) gates S-P3 dispatch per PASS-3-SYNTHESIS-PLAN.md. No user gate intervenes.
