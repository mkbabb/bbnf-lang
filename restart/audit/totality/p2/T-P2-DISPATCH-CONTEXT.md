# T-P2 Dispatch Context — SK-V15 Totality Research Pass

Authored by the SK-V15 orchestrator after T-P1 reached V5 clean-final at
commit `08609ab8e`. T-P1 did not close by the normal two-consecutive-clean
§3Z route: V4 was REVISE, V5 was the hard ceiling, and the active user pin
auto-passes optional G1 gates. T-P2 therefore dispatches under
`G1-AUTO-PINNED / CLEAN-FINAL`, with the governance fact carried forward
plainly into T-P2, T-P3, and Pass Omega.

The existing T-P2 dossiers and hardening files in this directory are SK-V14
history. SK-V15 V1 rewrites only the six assigned dossier paths below; git
history preserves the SK-V14 packet.

Each T-P2 agent reads §0 through §4 plus its own per-agent row, then writes
one dossier at its assigned path.

## §0 — Authority

1. `restart/prompts/totality/PASS-2-RESEARCH.md` — pass contract, §2 scope
   matrix, required frontmatter, dossier body sections, CHALLENGE contract,
   hard caps, and bbnf-specific axes.
2. `restart/prompts/ORCHESTRATOR.md` §3W and §3Z — universal hardening,
   convergence, V≤5 ceiling, and non-negotiables.
3. `restart/audit/totality/p1/{1A,1B,1C,1D,1E,1F-coherence-scan,1F-anti-pattern,1F-past-corpora}.md`
   — SK-V15 T-P1 inventories after the V5 fold.
4. `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`
   — V5 clean-final / G1-auto-pinned verdict.
5. `restart/skinny/tranches/sk-v15/SYNTHESIS.md`,
   `restart/skinny/tranches/sk-v15/SPEC.md`, and
   `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md` — locked SK-V15
   PRUNE-then-REBUILD wave contract.
6. `restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md`
   — S-P3 V4 §3Z LOCK declaration.
7. `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md` — implementation
   overfit audit that opened SK-V15.
8. `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`,
   `restart/locks/LOCKS.md`, `restart/HANDOFF.md`, and
   `restart/MIGRATION.md` — read-only governance surfaces. T-P2 emits
   amendment candidates only.
9. `skinny/REDRESS.md` and `skinny/RESULTS.md` — empirical ledger and
   falsification floor.
10. Prior research under `restart/skinny/tranches/sk-v{1..15}/research/`
    and historical T-P2 SK-V14 dossiers under this directory — extend or
    refute; do not copy forward unverified claims.

## §1 — SK-V15 empirical floor

The PASS-IMPL V1 audit at `cbafeb566` is dispositive for the current cycle:

| axis | SK-V15 floor |
|---|---|
| JSON | honest scanner and value API; 51/51 admit rows measurement-valid; M5 Max aarch64 measurements are the native close route. |
| CSS L4 | contrived: 24 row admits came from one measurement broadcast; generated modules were byte-identical copies; the generator hid a hand-written CSS tokeniser inside a string literal; `full_parse` was a brace-counter, not same-workload CSSOM comparison. |
| Pattern H | not collapsed: runtime Pattern H file count remains 67; generated headers absent; 4/9 grammars still bespoke. |
| Generic infrastructure | mixed: runtime provider enum retired, but leak axes and self-exempting Lock 14 scans remain. |
| Decision Engine | scaffold: zero e-graph rules, tautological CSP, and lowerer stubs. |
| FNV bench scheme | quarantined contrivance in bench scaffolding, not production runtime. |

The locked SK-V15 wave contract is PRUNE-then-REBUILD. T-P2 must ground or
refute the literature beneath that contract, not re-litigate whether the
contrivances exist.

## §2 — Current SK-V15 T-P1 carry-forward

T-P1 V5 closed all evidence defects after the V4 CH1 fold:

- root-resolving citations replaced shorthand path claims in live inventories;
- stale V3 wording was removed from 1A and 1B;
- the 1F FNV line-position transcript now resolves from repository root;
- no orphan REVISE or REJECT remained at V5.

Because this was a hard-ceiling clean-final rather than a normal two-clean
§3Z LOCK, T-P2 must carry the governance qualifier in its frontmatter or
executive summary:

`t_p1_entry_state: CLEAN-FINAL-G1-AUTO-PINNED-NOT-NORMAL-3Z`

## §3 — SK-V15 research obligations

T-P2 is literature grounding, not implementation. Every claim needs a
primary source, named technique post, or source path:line. The following
SK-V15 obligations are load-bearing:

1. **No CSS overfit revival.** CSS L4 claims must distinguish tokenisation,
   value API, rule/declaration AST, CSSOM, and same-workload comparator
   surfaces. A brace-counter cannot be cited as CSS parser parity.
2. **No broadcast admission.** A throughput datum reused across multiple
   conceptual rows must be named as a broadcast and rejected unless each row
   has row-local measurement and row-local equality.
3. **No gate that lies by omission.** Lock 14 / Lock 16 scans that exclude
   newly introduced leak paths are not evidence.
4. **Apple M5 Max / aarch64 primary.** aarch64 SIMD, PMU, and dispatch
   evidence are primary. x86 and AVX-512 are secondary diagnostic or
   future-backend material, never the SK-V15 close route.
5. **Deep SIMD is admissible only with process.** A primitive needs scalar
   reference, differential check, hardware gate, same-wave consumer, and
   row movement. Citation density alone admits nothing.
6. **Grammar-neutrality must be operational.** A primitive or generator
   route must explain transfer across JSON, CSS L4, Sheets, BBNF-self, and
   future user grammars without generic crate grammar switches.
7. **Abrogate before patch.** If the literature or implementation evidence
   shows a subsystem is intrinsically contrived, name the deletion route
   before proposing repair.

## §4 — Per-agent ownership

All six agents run in parallel and own disjoint outputs. Hard cap: 45 min
per agent. Do not stage or commit; the orchestrator commits the six outputs
atomically after review.

| Agent | Scope | Output |
|---|---|---|
| **2A — SOTA parsing landscape** | Ground same-workload parsing claims for JSON and CSS. Separate DOM/value/typed/lazy/fact-stream/CSSOM workloads. Reground simdjson, sonic-rs, yyjson, cssparser, lightningcss, and any same-workload comparator claims. Flag any workload mismatch that would make a >SOTA assertion invalid. | `restart/audit/totality/p2/2A-sota-landscape.md` |
| **2B — Primitive-vocabulary research** | Ground the reusable SIMD/ASM primitive vocabulary for aarch64-first bbnf: scalar oracle, checkasm-style differential, hardware gate, same-wave consumer, and row movement. Keep Layer 0 / Layer 1 separation clean. | `restart/audit/totality/p2/2B-primitive-vocabulary.md` |
| **2C — Grammar-neutrality / generalisation research** | Ground Lock 14 transfer: generator input surfaces, metadata/grammar-source ownership, Pattern H collapse, CSS Value API as a grammar-derived value surface, and future-grammar onboarding tests. | `restart/audit/totality/p2/2C-grammar-neutrality.md` |
| **2D — Cost-model + 5-shape BackendShape research** | Ground Decision Engine activation, cost model, e-graph/CSP use, and the 5-shape BackendShape derivation. Refute scaffold-only lowerers and no-op rules as evidence. | `restart/audit/totality/p2/2D-cost-model.md` |
| **2E — Host-arch ASM/SIMD esoterica** | aarch64 primary for M5 Max: NEON/AdvSIMD, PMULL lineage, DotProd/I8MM where applicable, LD4 classify, ternary bitwise, CSSC if host-gated, PMU validation. x86 only as secondary future-backend contrast. | `restart/audit/totality/p2/2E-host-arch-esoterica.md` |
| **2F — parse-that primitive gaps** | Audit `parse-that` / `parse-that-regex` gaps against the SK-V15 prune/rebuild contract: regex/HIR, SIMD scan, string, float, CSS value parsing, and generator integration. Decide upstream-or-vendor per gap. | `restart/audit/totality/p2/2F-parse-that-gaps.md` |

## §5 — Required frontmatter additions

Use the PASS-2 frontmatter, and add these SK-V15 fields:

```yaml
sk_cycle: SK-V15
t_p1_entry_state: CLEAN-FINAL-G1-AUTO-PINNED-NOT-NORMAL-3Z
implementation_floor: PASS-IMPL-V1-CSS-CONTRIVANCE-JSON-HONEST
host_close_route: Apple-M5-Max-aarch64
stale_sk_v14_material_reused: <none|list with reverified source ids>
```

## §6 — Challenge addenda for this cycle

T-P2's prompt names CH1-CH6. SK-V15 composes the current CH7 overfit-prune
guard as an additional lens in hardening. The later CH wave must therefore
write CH1-CH7. Three procedural addenda are binding:

- **NEW-CH3-V5-01 wave-graph cycle detection:** any proposed deletion or
  retirement route must prove the rebuild provider precedes the deletion
  consumer.
- **NEW-CH5-V5-02 broadcast-admission detection:** repeated throughput
  tuples across distinct row IDs are evidence of a broadcast unless each
  row has independent command, input, equality, and timing.
- **NEW-CH7-V5-03 gate-exclusion detection:** a grep/check gate that
  excludes files introduced by the same change is a contrivance, not a
  guard.

## §7 — Closure expectation

After the six dossiers land and commit, dispatch CH1-CH7 for V1 hardening.
Fold every REJECT/REVISE into V2. Converge per §3Z or reach V5 clean-final
only with the governance qualifier surfaced. Do not advance to T-P3 on
stale SK-V14 convergence records.
