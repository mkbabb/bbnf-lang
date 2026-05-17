# PASS-2-RESEARCH — T-P2 Totality Research (SOTA + Architectural Grounding In Primary Literature)

T-P2 is the **totality-track research pass**. It consumes the converged
T-P1 evidence base (`restart/audit/totality/p1/`) and grounds every SOTA
assertion and architectural design claim the V1 spec depends upon in
**primary literature** — papers, named-technique blog posts, established
library source. The pass converts T-P1's catalogued divergences and
amendment candidates from "the spec says X" into "X is defensible against
the published architecture, here cited" — or surfaces that it is not.

The pass is **iterative + auto-convergent**. Six parallel sub-agents 2A–2F
fan out per the scope matrix in §2; each writes one research dossier. A
six-lens CHALLENGE wave (§3) adversarially reviews the dossiers;
dispositions fold into v+1; the loop terminates at the convergence
criterion in §4. T-P2's converged output is what T-P3 Synthesis distils
into spec amendments. The pass is self-contained: an agentic system handed
only this prompt and `ORCHESTRATOR.md` runs it end-to-end.

T-P2 is forward-architecture grounding, not implementation. It cites
literature, names techniques, and states the bbnf-specific reason a
technique does or does not transfer. It holds the same locks-amendment
**candidate** authority as T-P1: where the literature reveals a previously
unstated invariant or refutes a settled one, the dossier emits an
amendment candidate. Disposition is T-P3's 3C; merge is Pass Omega's,
post-G-Omega.

## §1 — Trigger + entry condition

T-P2 dispatches when **T-P1 has converged** — CHALLENGE returned ≥95%
ACCEPT for two consecutive cycles or the user pinned G1 — and
`restart/HANDOFF.md` declares ready-for-T-P2.

**Entry artefacts** the orchestrator confirms present: the six (or more)
converged T-P1 inventories under `restart/audit/totality/p1/` + the
T-P1 CONSOLIDATED verdict; the V1 spec surfaces; `skinny/REDRESS.md` +
`skinny/RESULTS.md`; the skinny corpus surfaces; the live `skinny/crates/`
workspace; the SK-V{N} cohort research under
`restart/skinny/tranches/sk-v{1..N}/research/` (prior literature digests
to extend, not re-derive). If an entry artefact is absent the orchestrator
fails the dispatch loudly.

## §2 — Scope matrix (six parallel sub-agents)

Each row is one sub-agent. The agent reads the T-P1 evidence base + its
literature scope, then writes ONE research dossier at the assigned path.
All six own disjoint scopes and run in parallel. Hard cap 45 min per agent.

| Agent | Scope | Output |
|---|---|---|
| **2A — SOTA parsing landscape** | Ground the SOTA-parsing assertions: simdjson (stage1/stage2, On-Demand), sonic-rs (lazy-value, M1 Pro twitter anchor), yyjson (no-SIMD `always_inline` reference), asmjson (AVX-512 DOM kernel) architecture. The DAV1D / FFmpeg / VLC hand-written-ASM process discipline — scalar oracle + checkasm differential harness + same-wave consumer. Cite papers + library source per claim. | `restart/audit/totality/p2/2A-sota-landscape.md` |
| **2B — Primitive-vocabulary research** | The two-layer reusable SIMD/ASM primitive layer: Layer 0 (vendored `x86inc.asm` / dav1d macro corpus) + Layer 1 (`bbnf.asm` — the bbnf-authored primitive vocabulary). What primitives the layer must carry, how Layer 1 consumes Layer 0, the admission discipline (scalar reference + checkasm parity before wiring) per Lock 16. | `restart/audit/totality/p2/2B-primitive-vocabulary.md` |
| **2C — Grammar-neutrality / generalisation research** | How the primitive vocabulary + the 5-shape `BackendShape` generalise beyond JSON to CSS L4 / Sheets / BBNF-self / arbitrary user grammars (Lock 14). The abstract-primitive-lift discipline: which dav1d/ffmpeg primitives translate to byte-stream parsing for *any* grammar, and the cost-model-derived per-grammar selection. The future-grammar onboarding test. | `restart/audit/totality/p2/2C-grammar-neutrality.md` |
| **2D — Cost-model + 5-shape BackendShape research** | Ground the cost model (Lock 10) + the 5-shape `BackendShape` derivation in literature: how published parser generators / optimizers derive materialization strategy; the 8-step `derive_backend_shape` algorithm's defensibility; the `CollapsedStage` AVX-512-FSM design against asmjson + Sneller branchless-AVX-512. | `restart/audit/totality/p2/2D-cost-model.md` |
| **2E — Host-arch ASM/SIMD esoterica** | aarch64 **primary** (M5 Max target): PMULL/VPCLMUL lineage, CSSC, UDOT/DotProd, LD4-interleaved classify, BCAX/EOR3 ternary bitwise, NEON `svmatch_u8` port. x86 **secondary**: AVX2, AVX-512 (VBMI2, GFNI, VPCLMUL), k-mask arithmetic, AVX-IFMA, VNNI, BITALG. Each esoterica entry: published citation + abstract-primitive name + hardware gate. | `restart/audit/totality/p2/2E-host-arch-esoterica.md` |
| **2F — parse-that primitive gaps** | Audit the `parse-that` crate family (`parse-that`, `parse-that-regex`) for primitives the V1 spec depends on but the crate lacks: regex/HIR primitives, SIMD scan primitives, string primitives, float parsing (Eisel-Lemire / Clinger). Per gap: the published primitive, the upstream-or-vendor decision, the bbnf-specific need. | `restart/audit/totality/p2/2F-parse-that-gaps.md` |

### Output-schema frontmatter (every 2X dossier emits this block)

```yaml
---
agent: 2X
pass: T-P2-research
cycle: V{N}
generated_at: <ISO-8601>
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: <int>            # papers + named-technique posts + library source
techniques_grounded: <int>
techniques_refuted: <int>               # spec-assumed technique the literature does not support
prior_cycle_dispositions_folded:
  accepted: [<finding-id>, ...]
  rejected: [<finding-id>, ...]
  revised: [<finding-id>, ...]
  first_cycle_additions: [<finding-id>, ...]
locks_amendment_candidates: <int>
---
```

Body sections, every dossier: **Executive Summary** (≤200 words);
**Technique Grounding Table** (spec-claim or T-P1-divergence-id | published
source cited | grounded / refuted / partial | bbnf-specific note);
**Architectural Assertions Defended**; **Architectural Assertions
Refuted** (a spec assumption the literature does not support — the most
load-bearing dossier rows); **Open Research Questions** (UNKNOWN →
verify_action); **LOCKS-AMENDMENTS-CANDIDATE** table (or explicit
"no candidates; scanned axes: <list>" row). Every citation is a real,
verifiable source — paper title + venue/year, post URL + named technique,
or library source path:line. A confabulated citation is a CH1 REJECT.

## §3 — Six-lens CHALLENGE pass (CH1–CH6)

Every cycle closes with the six-lens CHALLENGE wave per `ORCHESTRATOR.md`
§3W. One lens, one agent; six agents; each writes
`restart/audit/totality/p2/hardening/V{N}/CH{n}.md`. One aggregator writes
`.../hardening/HARDENING-T-P2-V{N}-CONSOLIDATED.md` carrying the six
dispositions + the cycle verdict. Dispositions are **ACCEPT / REJECT /
REVISE**. Cycle V1 expects ≥30% REVISE; an all-ACCEPT wave is paper-close.

What each lens scans inside T-P2's output:

**CH1 CORRECTNESS** — every cited paper exists and carries the claimed
finding; every library-source citation resolves to the claimed path:line;
every benchmark number traces to a named corpus + platform. Provenance
gaps and confabulated citations are REJECT. Refuted-technique rows match
the literature's actual position.

**CH2 GENERALITY** — Lock 14 holds: every primitive or technique the
dossiers ground is grounded *grammar-neutrally*; 2C's generalisation story
shows the technique transferring to CSS L4 / Sheets / BBNF-self, not only
JSON. A technique grounded JSON-only that the spec uses fleet-wide is a
REVISE.

**CH3 REGRESSION** — no dossier grounds a route already refuted in
`skinny/REDRESS.md` as if it were viable; the skinny rejected-route ledger
is honoured; a "promising" research direction that REDRESS already
falsified is a REJECT.

**CH4 COST** — every grounded primitive carries an admission cost: scalar
reference + checkasm parity per Lock 16; a same-wave consumer is named;
LOC/risk for adoption is realistic; no orphan-kernel research.

**CH5 HIDDEN COUPLING** — no grounded design implies a parallel substrate,
a sidecar producer, or a Lock 1 violation; the `CollapsedStage` / FSM
research (2D) keeps the mask stream a transient producer, not a retained
sidecar; primitive-vocabulary research (2B) keeps Layer 0/Layer 1 a clean
two-layer dependency, not a coupling.

**CH6 ANTI-PAPER-CLOSE** — no dossier claims a technique "validated" or
"proven" on the strength of citation density alone; reference-stuffing
(N sources cited, none integrated) is flagged; every grounded technique
states the bbnf-specific reason it transfers, not merely "SOTA does it
this way". No deferral to "a later research pass".

The lens registry is monotonically extensible (CH7+); CH1–CH6 are never
renumbered. The A-K lens set (`audit-specs/HARDENING-LENS-SET.md`) — in
particular Lens G (overfitting) and Lens H (hallucination + provenance) —
may be composed by reference when a CHALLENGE agent audits dossier prose.

## §4 — Iteration + auto-convergence

T-P2 executes cycles V1, V2, … per `ORCHESTRATOR.md` §3Z; the cycle
counter is per-pass and independent.

Per cycle: **(1)** the six 2X agents fan out and write their dossiers;
**(2)** the pass output commits before CHALLENGE; **(3)** the six CH
agents fan out; **(4)** the aggregator writes the CONSOLIDATED verdict;
**(5)** dispositions fold into V{N+1} — each 2X author addresses every
REJECT with a corrected or removed grounding and every REVISE with a new
citation or measurement plan, citing the disposition source. Hardening
without folding is paper-hardening; the pass does not advance.

**Convergence criterion** (advances the pass to T-P3): CHALLENGE returns
**≥95% ACCEPT for two consecutive cycles**, with zero open critical
defects and no orphan unresolved REVISE; OR the user pins the cycle as
final at G2.

A research dossier that draws the same REVISE across three consecutive
cycles indicates the author is paper-folding; the orchestrator escalates
to the user before V4. **Hard ceiling V ≤ 5**; a V5 non-convergence
escalates with a `BLOCKED` verdict naming the unresolved REVISE set.

## §5 — Output structure

```
restart/audit/totality/p2/
├── 2A-sota-landscape.md
├── 2B-primitive-vocabulary.md
├── 2C-grammar-neutrality.md
├── 2D-cost-model.md
├── 2E-host-arch-esoterica.md
├── 2F-parse-that-gaps.md
└── hardening/
    ├── V{N}/
    │   ├── CH1.md   CH2.md   CH3.md
    │   ├── CH4.md   CH5.md   CH6.md
    └── HARDENING-T-P2-V{N}-CONSOLIDATED.md
```

Each cycle overwrites the 2X dossiers in place; git history preserves
V1, V2, … . The `hardening/V{N}/` directory is per-cycle.

## §6 — User sign-off gate (G2)

Per `ORCHESTRATOR.md` §6, T-P2 convergence reaches **G2** — an optional
convergence pin. The orchestrator presents at G2: the cycle's CONSOLIDATED
verdict; the technique-grounding census (grounded vs refuted vs partial);
the architectural assertions the literature *refuted* (these constrain
T-P3 hardest); the LOCKS-AMENDMENTS-CANDIDATE set. On G2 close — pinned or
auto — the orchestrator updates `restart/HANDOFF.md` to declare
ready-for-T-P3 and dispatches T-P3 per `totality/PASS-3-SYNTHESIS.md`.
T-P3 does not dispatch before G2.

## §7 — Hard caps

Per `ORCHESTRATOR.md` §9: substantive pass ~45 min per agent, ~60 min wall
incl. commit; CHALLENGE wave ~90 min wall. Every dispatch carries an
explicit minute cap. At 0.9× the cap the agent commits what it has; at the
cap it halts. An overrun surfaces to the user as an extension decision;
the orchestrator engineers no silent deferral.

## §8 — bbnf-lang specific axes for T-P2

1. **The DAV1D/FFmpeg/VLC process discipline** is the spine of 2A — not the pixel-domain kernels (motion compensation, IDCT, loop filter, film grain do not translate to JSON), but the *process*: scalar oracle, checkasm differential harness, same-wave consumer. 2A grounds the process; 2B grounds the abstract-primitive lifts underneath the kernels.
2. **The two-layer vocabulary** (2B): Layer 0 is vendored verbatim (`x86inc.asm`); Layer 1 is `bbnf.asm`, bbnf-authored, grammar-neutral. The dossier keeps the layers distinct and the dependency one-directional.
3. **aarch64 is primary, x86 is secondary** (2E). The M5 Max is the live comparator silicon; AVX-512 esoterica are grounded as the >SOTA x86 path, hardware-gated, never the M5 Max close route.
4. **Lock 16 admissibility** governs every primitive 2B + 2E ground: an admissible primitive carries a published citation and an abstract-primitive name; hand-tuned undocumented intrinsic loops are magic and inadmissible.
5. **Refutation is a first-class output.** A spec assumption the literature does not support is the most load-bearing dossier row — it constrains T-P3's amendment set. The skinny REDRESS ledger already refuted several routes; T-P2 grounds *why* the literature predicts those refutations, and predicts others.
6. **No new directive, no new BIR variant, no new substrate.** T-P2 grounds techniques that fit the existing surface; a technique that would require a new directive or BIR variant is flagged as such for T-P3 to disposition explicitly — never silently absorbed.

## §9 — Closing posture

T-P2 is the totality track's literature firewall. It is iterative +
auto-convergent. It converts the V1 spec's SOTA + architectural assertions
from claim into citation — or refutes them against the published record.
It grounds the primitive vocabulary, the cost model, the host-arch
esoterica, and the parse-that gaps. The CHALLENGE wave is the firewall
against confabulated citations and reference-stuffing. The G2 gate is the
user's optional pin.

No technique grounded without a primary source. No primitive admitted
without a scalar reference. No assertion defended without a bbnf-specific
reason. No pass advance without convergence on the prior cycle.

Hereupon the six 2X agents fan out per §2; the CHALLENGE wave hardens per
§3; the loop converges per §4; the orchestrator presents G2 per §6 and
dispatches T-P3.
