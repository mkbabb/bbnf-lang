# PASS-1-EXCAVATION — T-P1 Totality Excavation (Current-State Evidence For The V1 Spec)

T-P1 is the **totality-track excavation pass**. It excavates current-state
evidence against the V1 greater spec — `restart/ARCHITECTURE.md`,
`restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md` — and against the skinny
empirical corpus. The pass answers a single question per layer: what does
the V1 spec *claim*, and what is *implemented*, and where do the two
diverge. Every divergence is cited at path:line; no claim stands on recall.

The pass is **iterative + auto-convergent**. Six parallel sub-agents 1A–1F
fan out per the scope matrix in §2; each writes one evidence inventory. A
six-lens CHALLENGE wave (§3) adversarially reviews the inventories;
dispositions fold into v+1; the loop terminates at the convergence
criterion in §4. T-P1's converged output is the evidence base T-P2
Research grounds and T-P3 Synthesis distils. The pass is self-contained:
an agentic system handed only this prompt and `ORCHESTRATOR.md` runs it
end-to-end.

T-P1 holds **no amendment authority of its own**. It catalogues evidence
and, where the evidence warrants, surfaces LOCKS-amendment *candidates*
(1E is the dedicated candidate scanner). Crystallisation of those
candidates into a v+1 diff is T-P3's 3C task, G3-gated; Pass Omega merges,
post-G-Omega. Excavation proposes; synthesis disposes; the gate ratifies.

## §1 — Trigger + entry condition

T-P1 dispatches when:

- A totality cycle opens and `restart/HANDOFF.md` declares **ready-for-T-P1**; OR
- A major skinny iteration has closed (SK-V{N} REDRESS + RESULTS materially affect the V1 spec) and the orchestrator opens a totality cycle to absorb it; OR
- The user explicitly invokes `dispatch t-p1`.

**Entry artefacts** the orchestrator confirms present before dispatch:
`restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md`,
`restart/HANDOFF.md`, `skinny/REDRESS.md`, `skinny/RESULTS.md`, the skinny
corpus surfaces `restart/skinny/{INDEX,SUBSTRATE,COMPILER,BENCH,WORKSPACE,HARDENING}.md`,
and the live workspace under `skinny/crates/`. If an entry artefact is
absent the orchestrator fails the dispatch loudly; it does not proceed on
assumed content.

## §2 — Scope matrix (six parallel sub-agents)

Each row is one sub-agent. The agent reads its scope end-to-end against
both the V1 spec surfaces and the live code, then writes ONE evidence
inventory at the assigned path. Sub-agents 1A–1E own disjoint scopes and
run in parallel; 1F's corpus scan is read-only over all of them and runs
alongside. Hard cap 45 min per agent.

| Agent | Scope | Output |
|---|---|---|
| **1A — Substrate-layer evidence** | What `ARCHITECTURE.md` §1, §7.1, §9 + Lock 1 claim about the substrate (tape ∪ direct-to-struct union, the 20-variant BIR alphabet, `&'i Tape<'i>` borrow shape) vs what is implemented in `skinny/crates/runtime/src/tape/`, `ir/src/`, `runtime/src/grammars/`. Cite spec-claim path:line ↔ impl path:line per row; flag every divergence. | `restart/audit/totality/p1/1A-substrate-evidence.md` |
| **1B — Codegen-layer evidence** | What the spec claims about codegen, the `BackendShape` 5-shape canon (`ARCHITECTURE.md` §7.3), `derive_backend_shape`, the cost model (Lock 10), the lowerer hierarchy (`ARCHITECTURE.md` §10, Lock 5) vs `skinny/crates/codegen/src/lower/`, `codegen/src/lib.rs`, `passes/src/`. Cite each shape's claimed derivation step ↔ landed Rust state. | `restart/audit/totality/p1/1B-codegen-evidence.md` |
| **1C — Runtime-layer evidence** | The runtime crate, generated parsers, `runtime/src/grammars/<g>/`. What `ARCHITECTURE.md` §9 + §4.3 claim vs the generated `SinkOnly`/`OffsetTape`/`EventTape` consumption code actually emitted. Per-grammar runtime module census; hand-written-vs-generated audit (Lock 14). | `restart/audit/totality/p1/1C-runtime-evidence.md` |
| **1D — Skinny-track lessons digest** | What SK-V1…SK-V{N} iterations empirically proved or disproved that the totality spec must absorb. Read `skinny/REDRESS.md` (the rejected-route + admitted-win ledger) and `skinny/RESULTS.md` (the measured gate) as the evidence; cite every entry. Produce a proved / disproved / pending table the V1 spec must reflect. | `restart/audit/totality/p1/1D-skinny-lessons.md` |
| **1E — Locks evidence + amendment candidates** | Audit the 16 locks (`restart/locks/LOCKS.md`) against current code + the skinny REDRESS ledger. Per lock: honoured / drifted / over-stated / silent-must-add. Produce a LOCKS-AMENDMENTS-CANDIDATE table (additions / refinements / removals) with supporting path:line evidence. Candidates only — disposition is 3C. | `restart/audit/totality/p1/1E-locks-evidence.md` |
| **1F — Cross-corpus coherence + anti-pattern + past-corpora scan** | Cross-document coherence audit (spec-surface ↔ spec-surface drift); anti-pattern scan of the live code (god modules, parallel substrates, grammar-name leaks); past-corpora scan (`restart/skinny/tranches/sk-v{1..N}/research/`, prior audit reports) for findings the current cycle must not re-derive. Multi-output permitted. | `restart/audit/totality/p1/1F-coherence-scan.md` (+ `1F-anti-pattern.md`, `1F-past-corpora.md` if scope warrants) |

### Output-schema frontmatter (every 1X inventory emits this block)

```yaml
---
agent: 1X
pass: T-P1-excavation
cycle: V{N}
generated_at: <ISO-8601>
spec_surfaces_audited: [ARCHITECTURE.md, MASTER-PLAN.md, LOCKS.md, ...]
files_audited_count: <int>
live_truth_method: <how loc/paths/symbols were verified — e.g. "wc -l + rg + cargo asm">
prior_cycle_dispositions_folded:
  accepted: [<finding-id>, ...]
  rejected: [<finding-id>, ...]
  revised: [<finding-id>, ...]
  first_cycle_additions: [<finding-id>, ...]
divergence_count:
  spec_claims_implemented: <int>
  spec_claims_unimplemented: <int>
  impl_exceeds_spec: <int>
  unknown: <int>
locks_amendment_candidates: <int>   # 1E populates; others emit 0 or surface to 1E
---
```

Body sections, every inventory: **Executive Summary** (≤200 words);
**Spec-Claim ↔ Implementation Table** (claim path:line | impl path:line |
verdict | note); **Divergences Catalogued** (concrete, path:line);
**Gaps / Missing Primitives**; **Open Questions** (UNKNOWN →
verify_action). 1E additionally emits the LOCKS-AMENDMENTS-CANDIDATE
table; if it proposes none, the table carries an explicit
"no candidates; scanned axes: <list>" row — silent omission is forbidden.

## §3 — Six-lens CHALLENGE pass (CH1–CH6)

Every cycle closes with the six-lens CHALLENGE wave per `ORCHESTRATOR.md`
§3W. One lens, one agent; six agents; each writes
`restart/audit/totality/p1/hardening/V{N}/CH{n}.md`. One aggregator then
writes `.../hardening/HARDENING-T-P1-V{N}-CONSOLIDATED.md` carrying the six
dispositions + the cycle verdict (ACCEPT-rate + REJECT list + REVISE
list). Dispositions are exactly **ACCEPT / REJECT / REVISE**; an unlabelled
finding is malformed. A wave returning ACCEPT on every finding without
close reading is itself paper-close — cycle V1 expects ≥30% REVISE.

What each lens scans inside T-P1's output:

**CH1 CORRECTNESS** — every spec-claim ↔ implementation row resolves: the
spec path:line carries the claimed text; the impl path:line carries the
claimed symbol; the verdict matches the cited evidence. RESULTS-row and
REDRESS-entry citations in 1D resolve to real entries. No recalled LOC,
no recalled symbol path.

**CH2 GENERALITY** — Lock 14 holds across the inventories: no divergence is
catalogued as JSON-only when it is a grammar-neutral substrate fact; 1C's
runtime census flags every grammar-named module in a generic crate; the
skinny-lessons digest (1D) separates JSON-empirical findings from
grammar-neutral findings. No grammar-name leak passes uncited.

**CH3 REGRESSION** — no inventory re-opens a route already in
`skinny/REDRESS.md`; the rejected-route pre-block list is correctly
identified by 1D and 1E; no admitted REDRESS row is mis-catalogued as
unimplemented.

**CH4 COST** — every divergence carries a realistic LOC-delta estimate and
risk class; 1E's amendment candidates state a wave-alignment hint;
amendment candidates without supporting path:line evidence are REVISE.

**CH5 HIDDEN COUPLING** — the substrate inventory (1A) is audited for the
Lock 1 union: no catalogued state implies a parallel substrate, a sidecar
producer, or a renamed-scanner violation; 1F's anti-pattern scan caught
the live couplings. Track 1 ≡ Track 2 dishonesty surfaces if present.

**CH6 ANTI-PAPER-CLOSE** — no inventory self-reports a divergence as
"resolved" or "wired" without a live-evidence citation (cargo asm symbol,
bench row, checkasm pass, REDRESS admit). No divergence deferred to "a
later inventory". Every UNKNOWN carries a verify_action.

The lens registry is monotonically extensible: a failure mode the six
cannot disposition adds CH7+; CH1–CH6 are never renumbered. The A-K lens
set (`audit-specs/HARDENING-LENS-SET.md`) is the complementary scheme — a
CHALLENGE agent auditing inventory *prose* may compose A-K by reference;
the intervention-plan lens is CH1–CH6.

## §4 — Iteration + auto-convergence

T-P1 executes cycles V1, V2, V3, … per `ORCHESTRATOR.md` §3Z. The cycle
counter is per-pass and independent of any skinny counter.

Per cycle: **(1)** the six 1X agents fan out and write their inventories;
**(2)** the pass output commits before CHALLENGE dispatches; **(3)** the
six CH agents fan out and write `CH{n}.md`; **(4)** the aggregator writes
the CONSOLIDATED verdict; **(5)** dispositions fold into the V{N+1}
dispatch — each 1X author addresses every REJECT with a corrected entry
and every REVISE with new evidence, citing the disposition source.
Hardening without folding is paper-hardening; the pass does not advance
until folding is complete.

**Convergence criterion** (advances the pass to T-P2): CHALLENGE returns
**≥95% ACCEPT for two consecutive cycles**, with zero open critical
defects and no orphan unresolved REVISE; OR the user pins the cycle as
final at G1.

**Hard ceiling.** V ≤ 5. A pass that reaches V5 without convergence
escalates to the user with a `BLOCKED` verdict naming the unresolved
REVISE dispositions.

## §5 — Output structure

```
restart/audit/totality/p1/
├── 1A-substrate-evidence.md
├── 1B-codegen-evidence.md
├── 1C-runtime-evidence.md
├── 1D-skinny-lessons.md
├── 1E-locks-evidence.md            ← LOCKS-AMENDMENTS-CANDIDATE table
├── 1F-coherence-scan.md
├── 1F-anti-pattern.md              ← if scope warrants (1F multi-output)
├── 1F-past-corpora.md              ← if scope warrants (1F multi-output)
└── hardening/
    ├── V{N}/
    │   ├── CH1.md   CH2.md   CH3.md
    │   ├── CH4.md   CH5.md   CH6.md
    └── HARDENING-T-P1-V{N}-CONSOLIDATED.md
```

Each cycle overwrites the 1X inventories in place; git history preserves
V1, V2, … . The `hardening/V{N}/` directory is per-cycle.

## §6 — User sign-off gate (G1)

Per `ORCHESTRATOR.md` §6, T-P1 convergence reaches **G1**. G1 is an
**optional convergence pin** — the user may pin the cycle as final, or let
the auto-convergence criterion (§4) close the pass. The orchestrator
presents at G1: the cycle's CONSOLIDATED verdict; the divergence census
(spec-claims-implemented vs unimplemented vs impl-exceeds-spec); 1E's
LOCKS-AMENDMENTS-CANDIDATE table. On G1 close — pinned or auto — the
orchestrator updates `restart/HANDOFF.md` to declare ready-for-T-P2 and
dispatches T-P2 per `totality/PASS-2-RESEARCH.md`. T-P2 does not dispatch
before G1.

## §7 — Hard caps

Per `ORCHESTRATOR.md` §9: substantive pass ~45 min per agent, ~60 min wall
incl. commit; CHALLENGE wave ~90 min wall. Every dispatch carries an
explicit minute cap. At 0.9× the cap the agent commits what it has; at the
cap it halts. A pass that overruns surfaces the slip to the user as an
extension decision — the orchestrator engineers no silent deferral.

## §8 — bbnf-lang specific axes for T-P1

1. **The Lock 1 substrate-union audit** is 1A's spine and CH5's firewall. The structural projection IS the tape; every catalogued state that looks like a sidecar producer or a parallel substrate is a divergence, not a feature.
2. **The 5-shape `BackendShape` canon** (`ARCHITECTURE.md` §7.3) must be inventoried whole by 1B — all five shapes, the 8-step `derive_backend_shape` algorithm, the per-grammar matrix. A shape claimed by the spec but absent in `codegen/src/lower/` is a spec-claims-unimplemented row.
3. **Lock 14 grammar-neutrality** is excavated by 1C (runtime census) + 1F (anti-pattern scan). The generic crates carry ZERO grammar-named modules; any leak is catalogued at path:line.
4. **The 16-lock count** is 1E's fixed baseline. 1E catalogues evidence and proposes amendment *candidates*; it never amends `LOCKS.md` and never re-numbers a lock.
5. **The skinny → totality direction is monotonic.** 1D digests skinny lessons FOR the totality spec; T-P1 never dictates back to a live skinny iteration. The digest separates JSON-empirical from grammar-neutral so T-P3 folds only the durable findings.
6. **No new directive, no new BIR variant, no new substrate.** T-P1 catalogues what exists; a divergence that would require a new directive or BIR variant to close is flagged as such for T-P2/T-P3 to research and disposition — never silently absorbed.

## §9 — Closing posture

T-P1 is the totality track's evidence floor. It is iterative +
auto-convergent. It catalogues, at path:line, where the V1 spec and the
implemented system agree and where they diverge; it digests the skinny
empirical record; it surfaces locks-amendment candidates without
disposing them. The CHALLENGE wave is the firewall against recalled
evidence and paper-close. The G1 gate is the user's optional pin.

No claim without a citation. No divergence without a verdict. No
amendment candidate without supporting evidence. No pass advance without
convergence on the prior cycle.

Hereupon the six 1X agents fan out per §2; the CHALLENGE wave hardens per
§3; the loop converges per §4; the orchestrator presents G1 per §6 and
dispatches T-P2.
